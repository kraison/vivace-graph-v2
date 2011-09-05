(in-package #:uuid)

;; (export 'uuid::time-low)
;; (export 'time-mid)
;; (export 'time-high)
;; (export 'clock-seq-var)
;; (export 'clock-seq-low)
;; (export 'node)
;; (export 'time-high-and-version)
;; (export 'clock-seq-and-reserved)
(export 'uuid-eql)
(export 'uuid?)
(export 'serialize-uuid)

(defgeneric uuid? (thing)
  (:method ((thing uuid)) t)
  (:method (thing) nil)
  (:documentation "UUID type predicate."))

(defgeneric uuid-eql (uuid1 uuid2)
  (:method ((uuid1 uuid) (uuid2 uuid))
    (equalp (uuid-to-byte-array uuid1) (uuid-to-byte-array uuid2)))
  (:method ((uuid1 uuid) uuid2)
    nil)
  (:method (uuid1 (uuid2 uuid))
    nil)
  (:documentation "Equality check for UUIDs."))

(defun serialize-uuid (uuid stream)
  (with-slots 
	(uuid::time-low uuid::time-mid uuid::time-high-and-version 
         uuid::clock-seq-and-reserved uuid::clock-seq-low uuid::node)
      uuid
    (loop for i from 3 downto 0
       do (write-byte (ldb (byte 8 (* 8 i)) uuid::time-low) stream))
    (loop for i from 5 downto 4
       do (write-byte (ldb (byte 8 (* 8 (- 5 i))) uuid::time-mid) stream))
    (loop for i from 7 downto 6
       do (write-byte (ldb (byte 8 (* 8 (- 7 i))) uuid::time-high-and-version) stream))
    (write-byte (ldb (byte 8 0) uuid::clock-seq-and-reserved) stream)
    (write-byte (ldb (byte 8 0) uuid::clock-seq-low) stream)
    (loop for i from 15 downto 10
       do (write-byte (ldb (byte 8 (* 8 (- 15 i))) uuid::node) stream))))

;; I can't find any callers for `uuid-to-byte-array' outside this file and
;; def. not with the type-specifier non-nil
;; However, kyoto-persistence/uuid.lisp defines `uuid-to-pointer' which has a similar signature
;; It has one direct caller `serialize' in kyoto-persistence/serialize.lisp 
;; (defmethod serialize ((uuid uuid:uuid))
;;   "Encode a UUID."
;;   (uuid:uuid-to-pointer uuid +uuid+))
;; (defconstant +uuid+ 12)
(defun uuid-to-byte-array (uuid &optional (type-specifier nil))
  "Converts an uuid to byte-array"
  (if type-specifier
      (let ((array (make-array 18 :element-type '(unsigned-byte 8))))
	(setf (aref array 0) type-specifier)
	(setf (aref array 1) 16)
	(with-slots 
	      (uuid::time-low uuid::time-mid uuid::time-high-and-version uuid::clock-seq-and-reserved uuid::clock-seq-low uuid::node)
	    uuid
	  (loop for i from 3 downto 0
	     do (setf (aref array (+ 2 (- 3 i))) (ldb (byte 8 (* 8 i)) uuid::time-low)))
	  (loop for i from 5 downto 4
	     do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 5 i))) uuid::time-mid)))
	  (loop for i from 7 downto 6
	     do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 7 i))) 
						uuid::time-high-and-version)))
	  (setf (aref array (+ 2 8)) (ldb (byte 8 0) uuid::clock-seq-and-reserved))
	  (setf (aref array (+ 2 9)) (ldb (byte 8 0) uuid::clock-seq-low))
	  (loop for i from 15 downto 10
	     do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 15 i))) uuid::node)))
	  array))
      (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
	(with-slots 
	      (uuid::time-low uuid::time-mid uuid::time-high-and-version uuid::clock-seq-and-reserved 
			uuid::clock-seq-low uuid::node)
	    uuid
	  (loop for i from 3 downto 0
	     do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) uuid::time-low)))
	  (loop for i from 5 downto 4
	     do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) uuid::time-mid)))
	  (loop for i from 7 downto 6
	     do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) 
					  uuid::time-high-and-version)))
	  (setf (aref array 8) (ldb (byte 8 0) uuid::clock-seq-and-reserved))
	  (setf (aref array 9) (ldb (byte 8 0) uuid::clock-seq-low))
	  (loop for i from 15 downto 10
	     do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) uuid::node)))
	  array))))

