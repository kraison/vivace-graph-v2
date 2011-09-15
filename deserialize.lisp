(in-package #:vivace-graph-v2)

;; The foundation of the serialization code comes from Sonja Keene's "Object-Oriented 
;; Programming in Common Lisp."  Thanks Sonja!

(defgeneric deserialize (code stream))
(defgeneric deserialize-action (code stream))

(defun deserialize-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (do ((code (read-byte stream nil :eof) (read-byte stream nil :eof)))
	((eql code :eof))
      (format t "CODE ~A: ~A~%" code (deserialize code stream)))))

(defmethod deserialize :around (code stream)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'deserialization-error :instance stream :reason condition))))

(defun deserialize-integer (stream)
  (let ((int 0) 
        (n-bytes (read-byte stream)))
    (dotimes (i n-bytes)
      ;; (setq int (dpb (read-byte stream) (byte 8 (ash i 3)) int))
      (setq int (dpb (read-byte stream) (byte 8 (* i 8)) int)))
    int))

(defmethod deserialize ((code (eql +negative-integer+)) stream)
  (- (deserialize-integer stream)))

(defmethod deserialize ((code (eql +positive-integer+)) stream)
  (deserialize-integer stream))

(defmethod deserialize ((code (eql +ratio+)) stream)
  (let ((numerator (deserialize (read-byte stream) stream))
	(denominator (deserialize (read-byte stream) stream)))
    (/ numerator denominator)))

(defmethod deserialize ((code (eql +single-float+)) stream)
  (ieee-floats:decode-float32 (deserialize-integer stream)))

(defmethod deserialize ((code (eql +double-float+)) stream)
  (ieee-floats:decode-float64 (deserialize-integer stream)))

(defmethod deserialize ((code (eql +character+)) stream)
  (let ((char-code (deserialize-integer stream)))
    (code-char char-code)))

;;; ==============================
;; :NOTE each of the following test cases pass for Clisp-2.49 and SBCL-1.47
;; As does form using flexi-streams for both Clisp-2.49 and SBCL-1.47
;; #+clisp
;; (let* ((src-string "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ")
;;        (compd (salza2:compress-data (ext:convert-string-to-bytes src-string charset:utf-8)
;;                                     'salza2:zlib-compressor))
;;        (decompd (chipz:decompress nil 'chipz:zlib compd))
;;        (reconv  (ext:convert-string-from-bytes decompd charset:utf-8)))
;;   (list (string= src-string reconv) (cons src-string reconv)))
;; #+sbcl
;; (let* ((src-string "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ")
;;        (compd (salza2:compress-data (sb-ext:string-to-octets src-string :external-format :utf-8)
;;                                         'salza2:zlib-compressor))
;;        (decompd (chipz:decompress nil 'chipz:zlib compd))
;;        (reconv  (sb-ext:octets-to-string decompd :external-format :utf-8)))
;;   (list (string= src-string reconv) (cons src-string reconv)))
;;
;; #-(sbcl clisp)
;; (let* ((src-string "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ")
;;        (compd (salza2:compress-data (flexi-streams:string-to-octets src-string :external-format :utf-8)
;;                                     'salza2:zlib-compressor))
;;        (decompd (chipz:decompress nil 'chipz:zlib compd))
;;        (reconv  (flexi-streams:octets-to-string decompd :external-format :utf-8)))
;;   (list (string= src-string reconv) (cons src-string reconv)))
;;; ==============================

(defmethod deserialize ((code (eql +string+)) stream)
  (let* ((length (deserialize (read-byte stream) stream))
	 (array (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref array i) (read-byte stream)))
    ;; #+sbcl  (sb-ext:octets-to-string array :external-format :utf-8 :start 0 :end length))
    ;; #+clisp (ext:convert-string-from-bytes array charset:utf-8 :start 0 :end length)
    ;; #-(sbcl clisp) (flexi-streams:octets-to-string array  :start 0 :end length :external-format :utf-8)
    (babel:octets-to-string array)))

(defmethod deserialize ((code (eql +compressed-string+)) stream)
  (let* ((length (deserialize (read-byte stream) stream))
	 (array (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref array i) (read-byte stream)))
    ;; #+sbcl  (sb-ext:octets-to-string array :external-format :utf-8 :start 0 :end length))
    ;; #+clisp (ext:convert-string-from-bytes array charset:utf-8 :start 0 :end length)
    ;; #-(sbcl clisp) (flexi-streams:octets-to-string (chipz:decompress nil 'chipz:zlib array) :external-format :utf-8)
    (babel:octets-to-string (chipz:decompress nil 'chipz:zlib array))))

(defmethod deserialize ((code (eql +t+)) stream)
  t)

(defmethod deserialize ((code (eql +null+)) stream)
  nil)

(defmethod deserialize ((code (eql +symbol+)) stream)
  (let ((code (read-byte stream)))
    (when (and (/= +string+ code) (/= +compressed-string+ code))
      (error 'deserialization-error :instance code :reason 
	     "Symbol-name is not a string!"))
    (let ((symbol-name (deserialize code stream)))
      (setq code (read-byte stream))
      (when (and (/= +string+ code) (/= +compressed-string+ code))
	(error 'deserialization-error :instance code :reason 
	       "Symbol-package is not a string!"))
      (let* ((pkg-name (deserialize code stream))
	     (pkg (find-package pkg-name)))
	(when (null pkg)
	  (error 'deserialization-error :instance code :reason 
		 (format nil "Symbol-package ~A does not exist!" pkg-name)))
	(intern symbol-name pkg)))))

(defun deserialize-sequence (stream type)
  (let* ((length (deserialize (read-byte stream) stream))
	 (seq (make-sequence type length)))
    (dotimes (i length)
      (setf (elt seq i) (deserialize (read-byte stream) stream)))
    seq))

(defmethod deserialize ((code (eql +list+)) stream)
  (deserialize-sequence stream 'list))

(defmethod deserialize ((code (eql +vector+)) stream)
  (deserialize-sequence stream 'vector))

;; (vg-uuid::deserialize-uuid (stream))
;; (unicly::uuid-from-byte-array (unicly::uuid-deserialize-byte-array-bytes stream))
(defmethod deserialize ((code (eql +uuid+)) stream)
  ;; (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
  ;;   (dotimes (i 16)
  ;;     (let ((byte (read-byte stream)))
  ;;       (cond ((= i 4)  (setf (aref array 5) byte))
  ;;             ((= i 5)  (setf (aref array 4) byte))
  ;;             ((= i 6)  (setf (aref array 7) byte))
  ;;             ((= i 7)  (setf (aref array 6) byte))
  ;;             ((= i 10) (setf (aref array 15) byte))
  ;;             ((= i 11) (setf (aref array 14) byte))
  ;;             ((= i 12) (setf (aref array 13) byte))
  ;;             ((= i 13) (setf (aref array 12) byte))
  ;;             ((= i 14) (setf (aref array 11) byte))
  ;;             ((= i 15) (setf (aref array 10) byte))
  ;;             (t        (setf (aref array i) byte)))))
  ;;   (unicly::uuid-from-byte-array array)))
  (vg-uuid::deserialize-uuid (stream)))


(defun deserialize-triple-slot (stream)
  (let* ((type-byte (read-byte stream))
	 (value (deserialize type-byte stream)))
    (if (or (eq type-byte +string+) (eq type-byte +compressed-string+))
	(intern value :graph-words)
	value)))

(defmethod deserialize ((code (eql +triple+)) (stream stream))
  (let ((subject (deserialize-triple-slot stream))
	(predicate (deserialize-triple-slot stream))
	(object (deserialize-triple-slot stream))
	(graph (deserialize-triple-slot stream))
	(id (deserialize (read-byte stream) stream))
	(deleted? (deserialize (read-byte stream) stream))
	(cf (deserialize (read-byte stream) stream)))
    (%add-triple subject predicate object id graph cf deleted?)))

(defmethod deserialize-action ((code (eql +transaction+)) (stream stream))
  (do ((code (read-byte stream nil :eof) (read-byte stream nil :eof)))
      ((or (eql code :eof) (null code)))
    (deserialize-action code stream)))

(defmethod deserialize-action ((code (eql +add-triple+)) stream)
  (let ((subject (deserialize-triple-slot stream))
	(predicate (deserialize-triple-slot stream))
	(object (deserialize-triple-slot stream))
	(graph (deserialize-triple-slot stream))
	(id (deserialize (read-byte stream) stream))
	(deleted? (deserialize (read-byte stream) stream))
	(cf (deserialize (read-byte stream) stream)))
    (%add-triple subject predicate object id graph cf deleted?)))

(defmethod deserialize-action ((code (eql +delete-triple+)) stream)
  (let ((id (deserialize (read-byte stream) stream))
	(timestamp (deserialize (read-byte stream) stream)))
    (%delete-triple id timestamp)))

(defmethod deserialize-action ((code (eql +undelete-triple+)) stream)
  (let ((id (deserialize (read-byte stream) stream)))
    (%undelete-triple id)))

(defmethod deserialize-action ((code (eql +set-cf+)) stream)
  (let ((id (deserialize (read-byte stream) stream))
	(cf (deserialize (read-byte stream) stream)))
    (%set-triple-cf id cf)))
