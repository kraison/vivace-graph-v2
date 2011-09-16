(in-package #:vivace-graph-v2)

;; The foundation of the serialization code comes from Sonja Keene's "Object-Oriented 
;; Programming in Common Lisp."  Thanks Sonja!

(defgeneric serialize (thing stream))
(defgeneric serialize-action (action stream &rest args))

(defmethod serialize :around (thing stream)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'serialization-error :instance thing :reason condition))))

(defun serialize-integer (int stream)
  (let ((n-bytes (ceiling (integer-length int) 8)))
    (write-byte n-bytes stream)
    (dotimes (i n-bytes)
      (write-byte (ldb (byte 8 0) int) stream)
      (setq int (ash int -8)))))

(defmethod serialize ((int integer) (stream stream))
  "Encodes integers between (- (1- (expt 2 (* 8 255)))) and (1- (expt 2 (* 8 255)))"
  (if (minusp int)
      (progn
	(write-byte +negative-integer+ stream)
	(setq int (abs int)))
      (write-byte +positive-integer+ stream))
  (serialize-integer int stream))

(defmethod serialize ((ratio ratio) (stream stream))
  (let* ((numerator (numerator ratio)) (denominator (denominator ratio)))
    (write-byte +ratio+ stream)
    (serialize numerator stream)
    (serialize denominator stream)))

(defmethod serialize ((float single-float) (stream stream))
  (write-byte +single-float+ stream)
  (serialize-integer (ieee-floats:encode-float32 float) stream))

(defmethod serialize ((float double-float) (stream stream))
  (write-byte +single-float+ stream)
  (serialize-integer (ieee-floats:encode-float64 float) stream))

(defmethod serialize ((char character) (stream stream))
  (write-byte +character+ stream)
  (serialize-integer (char-code char) stream))

(defmethod serialize ((string string) (stream stream))
  ;; FIXME: what is the right length to enable compression?
  (if (and *compression-enabled?* (> (length string) 20))
      (let* ((comp (salza2:compress-data 
                    ;;  we just took above -- :end (length string)
                    ;; #+clisp (ext:convert-string-to-bytes string charset:utf-8)
                    ;; #+sbcl  (sb-ext:string-to-octets string :external-format :utf-8)
                    ;; #-(sbcl clisp) (flexi-streams:string-to-octets string :external-format :utf-8) 
		    (babel:string-to-octets string) 'salza2:zlib-compressor))
	     (length (length comp)))
	(write-byte +compressed-string+ stream)
	(serialize length stream)
	(dotimes (i length)
	  (write-byte (aref comp i) stream)))
      (let* 
          ;; #+sbcl  (unicode (sbcl:string-to-octets string :external-format :utf-8))
          ;; #+clisp (unicode (ext:convert-string-to-bytes string charset:utf-8))
          ;; #-(sbcl clisp) (unicode (flexi-streams:string-to-octets string :external-format :utf-8))
          ((unicode (babel:string-to-octets string))
           (length  (length unicode)))
	(write-byte +string+ stream)
	(serialize length stream)
	(dotimes (i length)
	  (write-byte (aref unicode i) stream)))))

(defmethod serialize ((symbol symbol) (stream stream))
  (cond ((null symbol)
	 (write-byte +null+ stream))
        ((eq symbol t)
	 (write-byte +t+ stream))
        (t
	 (write-byte +symbol+ stream)
	 (serialize (symbol-name symbol) stream)
	 (serialize (package-name (symbol-package symbol)) stream))))

(defmethod serialize ((uuid unicly:unique-universal-identifier) (stream stream))
  (write-byte +uuid+ stream)
  (vg-uuid:serialize-uuid uuid stream))

;; (defmethod serialize ((uuid vg-uuid) (stream stream))
;;   (write-byte +uuid+ stream)
;;   (vg-uuid:serialize-uuid uuid stream))

(defun serialize-sequence (seq stream code)
  (let ((length (length seq)))
    (write-byte code stream)
    (serialize length stream)
    (dotimes (i length)
      (serialize (elt seq i) stream))))

(defmethod serialize ((list list) (stream stream))
  (serialize-sequence list stream +list+))

(defmethod serialize ((vector vector) (stream stream))
  (serialize-sequence vector stream +vector+))

;; :NOTE why cl:find-package when *graph-words* is already bound in
;; vivace-graph-v2/globals.lisp?
(defun serialize-triple-help (triple stream)
  ;; (let ((graph-pkg *graph-words*))
  (let ((graph-pkg (find-package 'graph-words)))
    (if (and (symbolp (subject triple)) 
	     (eq (symbol-package (subject triple)) graph-pkg))
	(serialize (symbol-name (subject triple)) stream)
	(serialize (subject triple) stream))
    (if (and (symbolp (predicate triple)) 
	     (eq (symbol-package (predicate triple)) graph-pkg))
	(serialize (symbol-name (predicate triple)) stream)
	(serialize (predicate triple) stream))
    (if (and (symbolp (object triple)) 
	     (eq (symbol-package (object triple)) graph-pkg))
	(serialize (symbol-name (object triple)) stream)
	(serialize (object triple) stream))
    (if (and (symbolp (graph triple)) 
	     (eq (symbol-package (graph triple)) graph-pkg))
	(serialize (symbol-name (graph triple)) stream)
	(serialize (graph triple) stream))
    (serialize (id triple) stream)
    (serialize (deleted? triple) stream)
    (serialize (cf triple) stream)))

(defmethod serialize ((triple triple) (stream stream))
  (write-byte +triple+ stream)
  (serialize-triple-help triple stream))

;; :NOTE why cl:find-package when *graph-words* is already bound in
;; vivace-graph-v2/globals.lisp?
(defmethod serialize-action ((action (eql :add-triple)) stream &rest args)
  (logger :debug "Serialize-action ~A: ~A~%" action args)
  (write-byte +add-triple+ stream)
  (if (triple? (first args))  
      ;; We generally want to avoid this, as the triple could change between
      ;; requested serialization and actual serialization.
      (serialize-triple-help (first args) stream)
      (let ((subject   (nth 0 args)) 
            (predicate (nth 1 args))
            (object    (nth 2 args))
	    (graph     (nth 3 args)) 
            ;; (graph-pkg *graph-words*) 
            (graph-pkg (find-package 'graph-words)))
	(if (and (symbolp subject) (eq (symbol-package subject) graph-pkg))
	    (serialize (symbol-name subject) stream)
	    (serialize subject stream))
	(if (and (symbolp predicate) (eq (symbol-package predicate) graph-pkg))
	    (serialize (symbol-name predicate) stream)
	    (serialize predicate stream))
	(if (and (symbolp object) (eq (symbol-package object) graph-pkg))
	    (serialize (symbol-name object) stream)
	    (serialize object stream))
	(if (and (symbolp graph) (eq (symbol-package graph) graph-pkg))
	    (serialize (symbol-name graph) stream)
	    (serialize graph stream))
	(serialize (nth 4 args) stream)	    ;; id
	(serialize (nth 5 args) stream)	    ;; deleted?
	(serialize (nth 6 args) stream)))) ;; cf

(defmethod serialize-action ((action (eql :delete-triple)) stream &rest args)
  (write-byte +delete-triple+ stream)
  (serialize (nth 0 args) stream)  ;; id
  (serialize (nth 1 args) stream)) ;; timestamp

(defmethod serialize-action ((action (eql :undelete-triple)) stream &rest args)
  (write-byte +undelete-triple+ stream)
  (serialize (nth 0 args) stream)) ;; id

(defmethod serialize-action ((action (eql :set-cf)) stream &rest args)
  (write-byte +set-cf+ stream)
  (serialize (nth 0 args) stream)  ;; id
  (serialize (nth 1 args) stream)) ;; cf

(defmethod serialize-action ((action (eql :transaction)) stream &rest args)
  (write-byte +transaction+ stream)
  (let ((tx (nth 0 args)))
    ;;(serialize (length (tx-queue tx)) stream)  
    (dolist (a (reverse (tx-queue tx)))
      (logger :info "TX: serializing ~A / ~A~%" (first a) (rest a))
      (apply #'serialize-action 
	     (nconc (list (first a) stream) (rest a))))))

(defun test-serializer (file)
  (with-open-file (stream file
			  :direction :output
			  :element-type '(unsigned-byte 8)
			  :if-exists :overwrite
			  :if-does-not-exist :create)
    (let ((uuid (vg-uuid::make-v4-uuid))
	  (vec (make-array 5)))
      (setf (aref vec 0) 1)
      (setf (aref vec 1) #\a)
      (setf (aref vec 2) "string")
      (setf (aref vec 3) 'symbol)
      (setf (aref vec 4) uuid)
      (format t "UUID IS ~A~%" uuid)
      (serialize 123 stream)
      (serialize 123.123 stream)
      (serialize 123/555 stream)
      (serialize #\a stream)
      (serialize "string" stream)
      (serialize 'symbol stream)
      (serialize uuid stream)
      (serialize (list 1 #\a "string" 'symbol uuid) stream)
      (serialize vec stream)))
  (deserialize-file file))
