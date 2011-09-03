

;;; ==============================
;; (in-package #:uuid)
;;
;; (export 'uuid::time-low)
;; (export 'time-mid)
;; (export 'time-high)
;; (export 'clock-seq-var)
;; (export 'clock-seq-low)
;; (export 'node)
;; (export 'time-high-and-version)
;; (export 'clock-seq-and-reserved)
;; (export 'uuid-eql)
;; (export 'uuid?)
;; (export 'serialize-uuid)

;;; ==============================


(defpackage #:vg-uuid
  (:use :common-lisp)
  (:export #:uuid?
           #:uuid-eql
           #:serialize-uuid
           #:make-v1-uuid
           ))

(in-package #:vg-uuid)

(defgeneric uuid? (thing)
  (:method ((thing uuid:uuid)) t)
  (:method (thing) nil)
  (:documentation "UUID type predicate."))

(defgeneric uuid-eql (uuid1 uuid2)
  (:method ((uuid1 uuid:uuid) (uuid2 uuid:uuid))
    (equalp (uuid:uuid-to-byte-array uuid1) (uuid:uuid-to-byte-array uuid2)))
  (:method ((uuid1 uuid:uuid) uuid2)
    nil)
  (:method (uuid1 (uuid2 uuid:uuid))
    nil)
  (:documentation "Equality check for UUIDs."))

(defun serialize-uuid (uuid stream)
  (with-slots (uuid::time-low 
               uuid::time-mid
               uuid::time-high-and-version 
               uuid::clock-seq-and-reserved
               uuid::clock-seq-low 
               uuid::node)
      uuid
    (loop 
       for i from 3 downto 0
       do (write-byte (ldb (byte 8 (* 8 i)) uuid::time-low) stream))
    (loop 
       for i from 5 downto 4
       do (write-byte (ldb (byte 8 (* 8 (- 5 i))) uuid::time-mid) stream))
    (loop 
       for i from 7 downto 6
       do (write-byte (ldb (byte 8 (* 8 (- 7 i))) uuid::time-high-and-version) stream))
    (write-byte (ldb (byte 8 0) uuid::clock-seq-and-reserved) stream)
    (write-byte (ldb (byte 8 0) uuid::clock-seq-low) stream)
    (loop 
       for i from 15 downto 10
       do (write-byte (ldb (byte 8 (* 8 (- 15 i))) uuid::node) stream))))


;;; UUIDs
(defun make-v1-uuid ()
  "Create a new UUID."
  (uuid:make-v1-uuid))

(defun sxhash-uuid (uuid) (sxhash (uuid:print-bytes nil uuid)))

(sb-ext:define-hash-table-test vg-uuid:uuid-eql sxhash-uuid)

(defun make-uuid-table (&key synchronized) 
  (make-hash-table :test 'vg-uuid:uuid-eql :synchronized synchronized))

;; :NOTE without the TYPE-SPECIFIER arg this function is identical to `uuid:uuid-to-byte-array'
;; I can't find any callers for `uuid-to-byte-array' outside this file and
;; definitely not with TYPE-SPECIFIER non-nil.
;; However, kyoto-persistence/uuid.lisp defines `uuid-to-pointer' which has a similar signature
;; It has one direct caller `serialize' in kyoto-persistence/serialize.lisp 
;; (defmethod serialize ((uuid uuid:uuid))
;;   "Encode a UUID."
;;   (uuid:uuid-to-pointer uuid +uuid+))
;; (defconstant +uuid+ 12)
;;
;; IOW, I don't think the additional functionaliity is actually being used in
;; vivace-graph-v2 as currently provided...
;; (defun uuid-to-byte-array (uuid &optional (type-specifier nil))
;;   "Converts an uuid to byte-array"
;;   (if type-specifier
;;       (let ((array (make-array 18 :element-type '(unsigned-byte 8))))
;;         (setf (aref array 0) type-specifier)
;;         (setf (aref array 1) 16)
;;         (with-slots 
;;               (uuid::time-low uuid::time-mid uuid::time-high-and-version uuid::clock-seq-and-reserved uuid::clock-seq-low uuid::node)
;;             uuid
;;           (loop for i from 3 downto 0
;;              do (setf (aref array (+ 2 (- 3 i))) (ldb (byte 8 (* 8 i)) uuid::time-low)))
;;           (loop for i from 5 downto 4
;;              do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 5 i))) uuid::time-mid)))
;;           (loop for i from 7 downto 6
;;              do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 7 i))) 
;;         					uuid::time-high-and-version)))
;;           (setf (aref array (+ 2 8)) (ldb (byte 8 0) uuid::clock-seq-and-reserved))
;;           (setf (aref array (+ 2 9)) (ldb (byte 8 0) uuid::clock-seq-low))
;;           (loop for i from 15 downto 10
;;              do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 15 i))) uuid::node)))
;;           array))
;;       (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
;;         (with-slots (uuid::time-low 
;;                      uuid::time-mid 
;;                      uuid::time-high-and-version 
;;                      uuid::clock-seq-and-reserved 
;;                      uuid::clock-seq-low uuid::node)
;;             uuid
;;           (loop 
;;              for i from 3 downto 0
;;              do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) uuid::time-low)))
;;           (loop 
;;              for i from 5 downto 4
;;              do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) uuid::time-mid)))
;;           (loop 
;;              for i from 7 downto 6
;;              do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) uuid::time-high-and-version)))
;;           (setf (aref array 8) (ldb (byte 8 0) uuid::clock-seq-and-reserved))
;;           (setf (aref array 9) (ldb (byte 8 0) uuid::clock-seq-low))
;;           (loop 
;;              for i from 15 downto 10
;;              do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) uuid::node)))
;;           array))))

