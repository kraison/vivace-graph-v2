;;; :FILE-CREATED <Timestamp: #{2011-09-05T13:10:36-04:00Z}#{11361} - by MON>
;;; :FILE vivace-graph-v2-FORK/uuid-bridge.lisp
;;; ==============================

;;; ==============================
;; :NOTE vivace-graph-v2/uuid-bridge.lisp relocats symbols previously found in:
;;   vivace-graph-v2/uuid.lisp
;;   vivace-graph-v2/data-types.lisp
;; 
;;  symbols previously defined in vivace-graph-v2/uuid.lisp
;; `uuid?'
;; `uuid-eql'
;; `serialize-uuid'
;;
;;  symbols previously defined in vivace-graph-v2/data-types.lisp
;; `make-uuid'
;; `sxhash-uuid'
;; `make-uuid-table'
;;
;; :RENAMED `make-uuid' -> `make-v1-uuid'
;;
;; This file creates a new package "VG-UUID".
;; Orginally vivace-graph-v2/uuid.lisp operated inside the package "UUID" and in
;; so doing redefined symbols in a namespace that is not under our control. Not
;; only was this was ugly, but it makes experimentation with transitioning to
;; the Unicly interface more difficult.
;;
;; Originally vivace-graph-v2/uuid.lisp redefined `uuid-to-byte-array'.  This
;; does not appear needed b/c without the TYPE-SPECIFIER arg, the redefined
;; function was otherwise identical to `uuid:uuid-to-byte-array' and I can't
;; find any callers for `uuid-to-byte-array' outside this file and definitely
;; not with TYPE-SPECIFIER non-nil.
;;
;; It is likely that the rationale for the redefinition was included in lieu of
;; kyoto-persistence/uuid.lisp which defined `uuid-to-pointer' and which also
;; has a TYPE-SPECIFIER parameter in its signature.
;;
;; kyoto-persistence`s `uuid-to-pointer' has one direct caller `serialize' in
;; kyoto-persistence/serialize.lisp
;; (defmethod serialize ((uuid uuid:uuid))
;;   "Encode a UUID."
;;   (uuid:uuid-to-pointer uuid +uuid+))
;; (defconstant +uuid+ 12)
;;
;; IOW, I don't think the additional functionaliity is actually being used in
;; vivace-graph-v2 as currently provided...
;;
;;; ==============================


(defpackage #:vg-uuid
  (:use #:common-lisp)
  (:export #:uuid?
           #:uuid-eql
           #:serialize-uuid
           ;; #:make-v1-uuid
           ;; #:make-v4-uuid
           ))

(in-package #:vg-uuid)


(defgeneric uuid? (thing)
  (:method ((thing uuid:uuid)) t) ;; unicly:unique-universal-identifier
  (:method (thing) nil)
  (:documentation "UUID type predicate."))


;; (defgeneric uuid? (thing)
;;   ;; :NOTE unicly:unique-universal-identifier-p does the same with some
;;   ;; provision for indicating if a uuid-bit-vector-128 is potentially
;;   ;; coerce-able to a uuid
;;   (:method ((thing vg-uuid)) t) ;; unicly:unique-universal-identifier
;;   (:method (thing) nil)
;;   (:documentation "UUID type predicate."))

;; (defgeneric uuid-eql (uuid1 uuid2)
;;   (:method ((uuid1 vg-uuid) (uuid2 vg-uuid))
;;     (unicly:uuid-eql uuid1 uuid1))
;;   (:method ((uuid1 vg-uuid) uuid2)
;;     nil)
;;   (:method (uuid1 (uuid2 vg-uuid))
;;     nil)
;;   (:documentation "Equality check for UUIDs."))

;; (defclass vg-uuid (unicly:unique-universal-identifier)
;;   ())


;; prolog-equal -- safe 
;; triple-eql      -- should be safe 
;;                    this is specialized on triples by comparing triple identity per the uuid which dereferences them, 
;; triple-equal     -- Ths one is trickier -- it is as above but also requires that the subject, predicate, and object of two  triples satisfy cl:equal
;; triple-equalp    -- if two triples are triple-equal and there respective graphs are cl:equal
;; make-fresh-store --> (make-skip-list :key-equal 'equalp :value-equal 'vg-uuid:uuid-eql :duplicates-allowed? t)
;; we should prob. subclass unicly:unique-universal-identifier before using these.
(defgeneric uuid-eql (uuid1 uuid2)
  (:method ((uuid1 uuid:uuid) (uuid2 uuid:uuid))
    (equalp (uuid:uuid-to-byte-array uuid1) (uuid:uuid-to-byte-array uuid2)))
  (:method ((uuid1 uuid:uuid) uuid2)
    nil)
  (:method (uuid1 (uuid2 uuid:uuid))
    nil)
  (:documentation "Equality check for UUIDs."))

;; make-anonymous-node-name specialzed on uuid:uuid

;; load-triples     -- evaluates uuid:make-uuid-from-string
;; %set-triple-cf   -- evaluates uuid:make-uuid-from-string and vg-uuid:uuid?
;; %undelete-triple -- evaluates uuid:make-uuid-from-string and vg-uuid:uuid?
;; %delete-triple   -- evaluates uuid:make-uuid-from-string and vg-uuid:uuid?


;; lookup-triple    -- evaluates vg-uuid:uuid?
;; add-triple       -- evaluates vg-uuid::make-v4-uuid

;; (defun serialize-uuid (uuid stream)
;;  ;; uuid-serialize-bit-vector-bits
;;   (uuid-serialize-byte-array-bytes uuid stream))

;; serialize -- specializes on uuid:uuid and vg-uuid:serialize-uuid
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

;; :SEE deserialize method specialzed on +uuid+in vivace-graph-v2/deserialize.lisp
;; (defun deserialize-uuid (stream)
;;   (unicly::uuid-from-byte-array (unicly::uuid-deserialize-byte-array-bytes stream)))

(defun make-v1-uuid ()
  "Create a new version one UUID."
  (uuid:make-v1-uuid))



;; make-v4-uuid is used as the id slot of a transaction
(defun make-v4-uuid ()
  "Create a new version four UUID."
  (uuid:make-v4-uuid))

;; (defun make-v4-uuid ()
;;   "Create a new version four UUID."
;;   (unicly:make-v4-uuid))

(defun sxhash-uuid (uuid) 
  (sxhash (uuid:print-bytes nil uuid)))

;; (defun sxhash-uuid (uuid)
;;   (unicly:sxhash-uuid uuid))

(sb-ext:define-hash-table-test vg-uuid:uuid-eql sxhash-uuid)

;; make-fresh-store
(defun make-uuid-table (&key synchronized) 
  (make-hash-table :test 'vg-uuid:uuid-eql :synchronized synchronized))

;; (defun make-uuid-table (&key synchronized) 
;;   (unicly:make-hash-table-uuid :synchronized synchronized))



;;; ==============================
;;; ==============================
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


;;; ==============================
;;; EOF
