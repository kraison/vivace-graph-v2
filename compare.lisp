;;; :FILE-CREATED <Timestamp: #{2011-09-23T19:44:02-04:00Z}#{11385} - by MON>
;;; :FILE vivace-graph-v2-FORK/compare.lisp
;;; ==============================

(in-package #:vivace-graph-v2)

;; vg-less-than
(defgeneric vg-less-than (x y)
  (:documentation "Generic less-than operator.  Allows comparison of apples and oranges.")
  
  (:method ((x symbol) (y symbol))
    (string< (symbol-name x) (symbol-name y)))
  
  (:method ((x number) (y number))
    (< x y))
  
  (:method ((x string) (y string))
    (string< x y))

  (:method ((x symbol) (y string))
    (string< (symbol-name x) y))

  (:method ((x string) (y symbol))
    (string< x (symbol-name y)))

  (:method ((x symbol) (y number))
    (string< (symbol-name x) (write-to-string y)))
  
  (:method ((x number) (y symbol))
    (string< (write-to-string x) (symbol-name y)))
  
  (:method ((x number) (y string))
    (string< (write-to-string x) y))

  (:method ((x string) (y number))
    (string< x (write-to-string y)))

  (:method ((x timestamp) (y timestamp)) 
    (timestamp< x y))

  (:method ((x number) (y timestamp))
    (< (timestamp-to-universal x) y))
  
  (:method ((x timestamp) (y number))
    (< x (timestamp-to-universal y)))

  ;; (:method ((x puri:uri) y)
  ;;   (b-tree-impl::key< x (princ-to-string y)))
  ;;
  ;; (:method b-tree-impl::key< ((x puri:uri) y)
  ;;   (b-tree-impl::key< (princ-to-string x) y))

  ;; :WAS 
  ;; (:method ((x uuid:uuid) (y uuid:uuid)) 
  ;;        (string< (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
  (:method ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier)) 
    (< (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x))
       (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))

  ;; :WAS 
  ;; (:method ((x symbol) (y uuid:uuid))
  ;;   (string< (symbol-name x) (uuid:print-bytes nil y)))
  ;; (:method ((x uuid:uuid) (y symbol)) 
  ;;   (string< (uuid:print-bytes nil x) (symbol-name y)))
  (:method ((x symbol) (y unicly:unique-universal-identifier))
    (string< (symbol-name x) (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t)))

  (:method ((x unicly:unique-universal-identifier) (y symbol)) 
    (string< (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t) (symbol-name y)))

  ;; :WAS (:method ((x string) (y uuid:uuid))
  ;;        (string< x (uuid:print-bytes nil y)))
  ;;       (:method ((x uuid:uuid) (y string)) 
  ;;         (string< (uuid:print-bytes nil x) y))
  (:method ((x string) (y unicly:unique-universal-identifier))
    (string< x (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char)))

  (:method ((x unicly:unique-universal-identifier) (y string))
    (string< (unicly:uuid-print-bytes-to-string x :string-or-char-type 'base-char) y))

  ;; :WAS 
  ;; (:method ((x number) (y uuid:uuid)) 
  ;;   (string< (write-to-string x) (uuid:print-bytes nil y)))
  ;; (:method ((x uuid:uuid) (y number)) 
  ;;   (string< (uuid:print-bytes nil x) (write-to-string y)))
  (:method ((x number) (y unicly:unique-universal-identifier))
    (<  x (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))
             
  (:method ((x unicly:unique-universal-identifier) (y number))
    (< (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x)) y))
  )

(defgeneric vg-greater-than (x y)
  (:documentation "Generic greater-than operator.  Allows comparison of apples and oranges.")
  
  (:method ((x symbol) (y symbol))
    (string> (symbol-name x) (symbol-name y)))
  
  (:method ((x number) (y number))
    (> x y))

  (:method ((x timestamp) (y timestamp))
    (timestamp> x y))

  (:method ((x string) (y string))
    (string> x y))
  
  (:method ((x symbol) (y string))
    (string> (symbol-name x) y))
  
  (:method ((x string) (y symbol))
    (string> x (symbol-name y)))
  
  (:method ((x symbol) (y number))
    (string> (symbol-name x) (write-to-string y)))
  
  (:method ((x number) (y symbol)) 
    (string> (write-to-string x) (symbol-name y)))

  (:method ((x number) (y string))
    (string> (write-to-string x) y))

  (:method ((x string) (y number))
    (string> x (write-to-string y)))

  (:method ((x number) (y timestamp))
    (> (timestamp-to-universal x) y))
  
  (:method ((x timestamp) (y number))
    (> x (timestamp-to-universal y)))

  ;; :WAS
  ;; (:method ((x uuid:uuid) (y uuid:uuid)) 
  ;;   (string> (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
  (:method ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier)) 
    (> (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x))
       (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))
  
  ;; :WAS
  ;; (:method ((x string) (y uuid:uuid)) 
  ;;   (string> x (uuid:print-bytes nil y)))
  ;; (:method ((x uuid:uuid) (y string)) 
  ;;   (string> (uuid:print-bytes nil x) y))

  (:method ((x string) (y unicly:unique-universal-identifier))
    (string> x (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char)))

  (:method ((x unicly:unique-universal-identifier) (y string))
    (string> (unicly:uuid-print-bytes-to-string x :string-or-char-type 'base-char) y))

  ;; :WAS
  ;; (:method ((x uuid:uuid) (y symbol)) 
  ;;   (string> (uuid:print-bytes nil x) (symbol-name y)))
  ;; (:method ((x symbol) (y uuid:uuid))
  ;;   (string> (symbol-name x) (uuid:print-bytes nil y)))
  (:method ((x symbol) (y unicly:unique-universal-identifier))
    (string> (symbol-name x) (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t)))

  (:method ((x unicly:unique-universal-identifier) (y symbol)) 
    (string> (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t) (symbol-name y)))

  ;; :WAS
  ;; (:method ((x number) (y uuid:uuid))
  ;;   (string> (write-to-string x) (uuid:print-bytes nil y)))           
  ;; (:method ((x uuid:uuid) (y number)) 
  ;;   (string> (uuid:print-bytes nil x) (write-to-string y)))
  (:method ((x number) (y unicly:unique-universal-identifier))
    (> x (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))
             
  (:method ((x unicly:unique-universal-identifier) (y number))
    (> (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x)) y))
  )

(defgeneric vg-equal (x y)
  (:documentation "Generic equality operator for prolog unification. Specialize 
this for new types that will be stored in the db.")
  (:method ((x number) (y number))
    (= x y))
  (:method ((x string) (y string)) 
    (string= x y))
  (:method ((x character) (y character)) 
    (char= x y))
  (:method ((x timestamp) (y timestamp))
    (timestamp= x y))
  (:method ((x timestamp) (y integer))
    (= (timestamp-to-universal x) y))
  (:method ((x integer) (y timestamp))
    (= (timestamp-to-universal y) x))
  (:method ((x triple) (y triple))
    (triple-equal x y))
  ;; (:method ((x uuid:uuid) (y uuid:uuid)) (vg-uuid:uuid-eql x y))
  (:method ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier))
    (vg-uuid:uuid-eql x y))
  (:method (x y)
    (equal x y)))

;;; ==============================
;;; EOF
