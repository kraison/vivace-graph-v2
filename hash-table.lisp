;;; :FILE vivace-graph-v2-FORK/hash-table.lisp
;;; ==============================

;;; ==============================
;;
;; AFAICT It will be difficult to fully abstract away SBCL's spinlocks/mutex with bordeaux-threads
;; bordeaux-threads:release-lock   <-- sb-thread:release-mutex
;; (sb-thread::release-spinlock (sb-impl::hash-table-spinlock hash-table))
;;
;; SBCL's hash-table's are implemented as STRUCTURE-OBJECT with
;; sb-impl::hash-table-spinlock as one of its slots.
;; The spinlock is used for locking cl:gethash/(setf cl:gethash)/cl:remhash and
;; looks like this:
;;
;;  (spinlock (sb-thread::make-spinlock :name "hash-table lock")
;;            :type sb-thread::spinlock :read-only t)
;;
;; IOW, there doesn't appear to be MUTEX.
;;
;; In any event, I'm not comfortable understanding how to incorporate
;; bordeaux-threads MUTEX frobbing with SBCL's spinlocks... though it appears
;; that a spinlock not entirely unlike a MUTEX. Following from
;; :FILE sbcl/src/code/thread.lisp
;;
;; (defstruct spinlock
;;   "Spinlock type."
;;   (name  nil :type (or null thread-name))
;;   (value nil))
;;
;; (defstruct mutex
;;   "Mutex type."
;;   (name   nil :type (or null thread-name))
;;   (%owner nil :type (or null thread))
;;   #!+(and (not sb-lutex) sb-thread)
;;   (state    0 :type fixnum)
;;   #!+(and sb-lutex sb-thread)
;;   (lutex (make-lutex)))
;;
;;; ==============================
;;
;; (defstruct tt--spinlock
;;   "Spinlock type."
;;   (name  nil
;;          :type (or null vg-thread-name))
;;   (value nil))
;;
;; (let ((spin (make-tt--spinlock :name "my lock")))
;;   (cons (type-of spin)
;;         (typep spin 'tt--spinlock)))
;;
;; (let ((spin (make-tt--spinlock)))
;;   (cons (type-of spin)
;;         (typep spin 'tt--spinlock)))
;;
;; (let ((spin (make-tt--spinlock)))
;;   (tt--spinlock-p spin))
;;
;; (let ((spin (make-tt--spinlock :name "my lock")))
;;   (setf (tt--spinlock-value spin) 42)
;;   spin)
;; ;=> #S(TT--SPINLOCK :NAME "my lock" :VALUE 42)
;;
;;; ==============================
;;
;; :NOTE Following are LispWorks low level atomic operations defined in the package "SYSTEM":
;;  `system:atomic-push', `system:atomic-pop', `system:atomic-fixnum-incf',
;;  `system:atomic-fixnum-decf', `system:atomic-incf', `system:atomic-decf',
;;  `system:atomic-exchange', `system:compare-and-swap',
;;  `system:define-atomic-modify-macro', `system:setup-atomic-funcall',
;;  `system:low-level-atomic-place-p',
;;
;;; ==============================


(in-package #:vivace-graph-v2)
;; *package*

#+sbcl (declaim (inline vg-current-thread
                        vg-get-spinlock-value
                        vg-compare-and-swap-spinlock-value
                        vg-get-hash-table-spinlock
                        vg-get-hash-table-spinlock-value
                        vg-current-thread-spinlock-value-check
                        vg-allow-with-interrupts
                        vg-interrupts-not-enabled-check
                        vg-interrupts-allowed-and-not-enabled-check
                        vg-thread-yield-cas-spinlock
                        vg-unix-interrupts-check
                        vg-release-spinlock))

(deftype vg-thread ()
  #-sbcl 'bordeaux-threads:thread
  #+sbcl 'sb-thread:thread)

(deftype vg-thread-name ()
  #-sbcl 'simple-string   ; <-- classic unsubstantiated and erroneous assumption
  #+sbcl 'sb-thread:thread-name)

(deftype vg-spinlock ()
  #-sbcl 'boolean ;; what else might we specify here ???
  #+sbcl 'sb-thread::spinlock)

;; :TODO Lispworks provides a compare-and-swap -- what is it?
;; Make compare-and-swap shorter to call
(defmacro cas (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

#-sbcl
(defun vg-get-spinlock-value (spinlock)
  (declare (ignore spinlock))
  (error "not implemented -- what is the equivalent of sb-thread::spinlock-value"))

#-sbcl
(defun vg-compare-and-swap-spinlock-value (cas-spinlock cas-old cas-new)
  (error "not implemented -- what is the equivalent of sb-ext:compare-and-swap of sb-thread::spinlock-value?"))

#-sbcl
(defun vg-get-hash-table-spinlock (hash-table)
  (declare (ignore hash-table))
  (error "not implemented -- what is the equivalent of sb-impl:hash-table-spinlock?"))

#-sbcl
(defun vg-allow-with-interrupts ()
  (error "not implemented -- what is equivalent of sb-sys:*allow-with-interrupts*?"))

#-sbcl
(defun vg-interrupts-not-enabled-check ()
  (error "not implemented -- what is equivalent of (not sb-sys:*interrupts-enabled*)?"))

#-sbcl
(defun vg-thread-yield-cas-spinlock ()
  ;; :NOTE `bordeaux-threads:thread-yield' wraps `sb-thread:release-foreground'
  ;; whereas `sb-thread:thread-yield' is an alien-routine
  (error "not implemented -- verify that bordeaux-threads:thread-yield ~
         is applicably equivalent with sb-thread:thread-yield in this context"))

#-sbcl
(defun vg-unix-interrupts-check ()
  (error "not implemented -- what is equivalent of sb-unix::%check-interrupts?"))

#-sbcl
(defun vg-release-spinlock (spinlock)
  (declare (ignore spinlock))
  (error "not implemented -- what is equivalent of sb-thread::release-spinlock"))


;;; ==============================
;; :NOTE sb-thread::spinlock is a structure object.
;;
;; :FIXME sb-ext:compare-and-swap has setf-like semantics and places
;; restrictions on the types of place it will perform a C-A-S upon.
;;
;; ,----
;; | `place' must be an accessor form {... elided ...}  or the name of a
;; | `defstruct' created accessor for a slot whose declared type is either `fixnum'
;; | or `t'. Results are unspecified if the slot has a declared type other then
;; | `fixnum' or `t'.
;; `----
;;
;; The only way I can see to make vg-get-spinlock-value work as a half viable
;; abstraction is to cons up a list or simple-vector for return value of
;; `sb-thread::spinlock-value' e.g.:
;; (list (sb-thread::spinlock-value spinlock))
;; In which case we might as well not bother using `sb-ext:compare-and-swap'...
;;
;; (defun vg-get-spinlock-value (spinlock)
;;   #+sbcl (declare (sb-thread::spinlock spinlock)
;;                   (optimize (speed 3) (safety 1)))
;;   #-sbcl (error "not implemented -- what is the equivalent of sb-thread::spinlock-value")
;;   #+sbcl (if (sb-thread::spinlock-p spinlock)
;;              (list (sb-thread::spinlock-value spinlock))
;;              (error "Arg SPINLOCK not `sb-thread::spinlock-p'")))
;;
#+sbcl
(defun vg-get-spinlock-value (spinlock)
  (declare (vg-spinlock spinlock)
           (optimize (speed 3)))
  (sb-thread::spinlock-value spinlock))

#+sbcl
(defun vg-compare-and-swap-spinlock-value (cas-spinlock cas-old cas-new)
  (declare (vg-spinlock cas-spinlock)
           (optimize (speed 3)))
  (cas (sb-thread::spinlock-value cas-spinlock) cas-old cas-new))

#+sbcl
(defun vg-get-hash-table-spinlock (hash-table)
  (declare (hash-table hash-table)
           (optimize (speed 3)))
  (the vg-spinlock (sb-impl::hash-table-spinlock hash-table)))

(defun vg-get-hash-table-spinlock-value (hash-table)
  #+sbcl (declare (inline vg-get-hash-table-spinlock) ;vg-get-spinlock-value)
                  (hash-table hash-table)
                  (optimize (speed 3)))
  (vg-get-spinlock-value (the vg-spinlock (vg-get-hash-table-spinlock hash-table))))

(defun vg-current-thread ()
  (declare (optimize (speed 3)))
  #-sbcl (the vg-thread (bordeaux-threads:current-thread))
  ;; :NOTE bordeaux-threads:current-thread is a simple wrapper fun around sb-thread:*current-thread*
  #+sbcl (the vg-thread sb-thread:*current-thread*))

(defun vg-current-thread-spinlock-value-check (hash-table)
  #+sbcl (declare (inline vg-current-thread
                          vg-get-spinlock-value
                          vg-get-hash-table-spinlock
                          vg-get-hash-table-spinlock-value)
                  (hash-table hash-table)
                  (optimize (speed 3)))
  (the boolean
    (eq (the vg-thread (vg-current-thread))
        (vg-get-hash-table-spinlock-value hash-table))))

#+sbcl
(defun vg-allow-with-interrupts ()
  (declare (optimize (speed 3)))
  (the boolean (and sb-sys:*allow-with-interrupts* t)))

#+sbcl
(defun vg-interrupts-not-enabled-check ()
  (declare (optimize (speed 3)))
  (the boolean (not sb-sys:*interrupts-enabled*)))

(defun vg-interrupts-allowed-and-not-enabled-check ()
  #+sbcl (declare (inline vg-interrupts-not-enabled-check
                          vg-allow-with-interrupts)
                  (optimize (speed 3)))
  (the boolean
    (and (the boolean (vg-interrupts-not-enabled-check))
         (vg-allow-with-interrupts)
         t)))

#+sbcl
(defun vg-thread-yield-cas-spinlock ()
  (declare (optimize (speed 3)))
  (sb-thread:thread-yield))

#+sbcl
(defun vg-unix-interrupts-check ()
  (declare (optimize (speed 3)))
  (sb-unix::%check-interrupts))

#+sbcl
(defun vg-release-spinlock (spinlock)
  (declare (vg-spinlock spinlock)
           (optimize (speed 3)))
  (sb-thread::release-spinlock spinlock))

(defun vg-get-spinlock (spinlock)
  #+sbcl (declare (inline vg-current-thread
                          ;; vg-get-spinlock-value
                          vg-compare-and-swap-spinlock-value
                          vg-allow-with-interrupts
                          vg-interrupts-not-enabled-check
                          vg-interrupts-allowed-and-not-enabled-check
                          vg-unix-interrupts-check)
                  ;; (optimize (speed 3) (safety 1)))
                  (optimize (speed 3) (safety 0)))
  (let* ((new (the vg-thread (vg-current-thread)))
         ;; (old (sb-ext:compare-and-swap (vg-get-spinlock-value cas-spinlock) nil new)))
         (old (vg-compare-and-swap-spinlock-value spinlock nil (the vg-thread new))))
    (when old
      (when (eq old new)
        (error "Recursive lock attempt on ~S." spinlock))
      (flet ((cas ()
               (declare (inline vg-compare-and-swap-spinlock-value
                                vg-thread-yield-cas-spinlock)
                        (optimize (speed 3) (safety 0)))
               (if (vg-compare-and-swap-spinlock-value spinlock nil new)
                   (progn
                     (sleep 0.000000001)
                     (vg-thread-yield-cas-spinlock))
                   (return-from vg-get-spinlock t))))
        (if (the boolean (vg-interrupts-allowed-and-not-enabled-check))
            ;; If interrupts are disabled, but we are allowed to
            ;; enable them, check for pending interrupts every once
            ;; in a while. %CHECK-INTERRUPTS is taking shortcuts, make
            ;; sure that deferrables are unblocked by doing an empty
            ;; WITH-INTERRUPTS once.
            (progn
              ;; :NOTE Not currenlty trying to abstract this b/c not sure if
              ;; there is some environment object which might be in play...
              (sb-sys:with-interrupts)
              (loop
		 (loop repeat 128 do (cas)) ; 128 is arbitrary here
                 (vg-unix-interrupts-check)))
            (loop (cas)))))
    t))

(defun acquire-hash-table-lock (hash-table)
  (declare (hash-table hash-table))
  (if (vg-current-thread-spinlock-value-check hash-table)
      t
      (vg-get-spinlock ;; (the vg-spinlock
       (vg-get-hash-table-spinlock hash-table))))

(defun release-hash-table-lock (hash-table)
  (declare (hash-table hash-table))
  (if (vg-current-thread-spinlock-value-check hash-table)
      (vg-release-spinlock (vg-get-hash-table-spinlock hash-table))))

;; Keyword SIZE defaults to value of `SB-IMPL::+MIN-HASH-TABLE-SIZE+'.
;; Where the SIZE of the generated table is know It is likely that specifying a
;; value will enable the system to not require as much rehashing.
;; REHASH-SIZE defaults to 1.5. For large tables setting this value to
#+:sbcl
(defun vg-make-hash-table (&key
                           (size SB-IMPL::+MIN-HASH-TABLE-SIZE+)
                           (rehash-size 1.5)
                           (rehash-threshold 1)
                           (test 'eql)
                           (hash-function nil)
                           (weakness nil)
                           (synchronized nil)
                           ) ;; &allow-other-keys ??
  (declare
   (type unsigned-byte size)
   (type (or (integer 1) (single-float (1.0)) (double-float (1.0d0))) rehash-size)
   (type (or (single-float 0.0 1.0) (double-float 0.0d0 1.0d0) (rational 0 1)) rehash-threshold)
   (type (member nil :key :value :key-and-value :key-or-value) weakness)
   (type (or function symbol) hash-function)
   (type boolean synchronized))
  (make-hash-table :size             size
                   :rehash-size      rehash-size
                   :rehash-threshold rehash-threshold
                   :weakness         weakness
                   :test             test
                   :hash-function    hash-function
                   :synchronized     synchronized))

;; LispWorks hash-tables:
;; &key test size rehash-size rehash-threshold hash-function weak-kind single-thread free-function
;;
;; :NOTE regarding use of :TEST argument to LW's `make-hash-table'.
;; Paraphrasing LW manual:
;; ,----
;; | The standard definition of `cl:make-hash-table' is extended such that test can be any
;; | suitable user-defined function, except that it must not call process-wait or
;; | similar MP package functions which suspend the current process. If test is not
;; | one of the standard test functions:
;; |  `cl:eq', `cl:eql' `cl:equal' `cl:equalp')
;; | and if value of keyword HASH-FUNCTION is not supplied, then the hash value
;; | is the same as would be used if :TEST were `cl:equalp'.
;; `---- :SOURCE (URL `http://www.lispworks.com/documentation/lw60/LW/html/lw-608.htm#47359')
;;
;; Keyword SYNCHRONIZED is a boolean. When t this indicates that
;; hash-table will _only_ be used in a single-threaded context (e.g. it needn't
;; be thread-safe) and/or it is known that the hash-table will never be accessed
;; outside the scop of a lock. Default is nil
;;
;; Keyword WEAKNESS is one one of:
;;  t, nil :value :key :one :both :either
;;
;;  NIL            -- Make hash-table non-weak. All entries are kept.
;;  T or :VALUE    -- Entires persist when a pointer to hash-value from another object exists.
;; :KEY            -- Entries persist when a pointer to hash-key from another object exists.
;; :BOTH           -- Entries persist when pointers to both hash-key and hash-value exist.
;; :ONE or :EITHER -- Entries persist when a pointer to either hash-key or hash-value exist.
;;
;; Keyword FREE-FUNCTION is a function-designator for a function accepting two
;; arguments (a hash-key and hash-value).
;; :SEE (URL `http://www.lispworks.com/documentation/lw60/LW/html/lw-608.htm#47359')
;; :SEE-ALSO `lw:choose-unicode-string-hash-function',
;; `hcl:set-hash-table-weak'.  `hcl:modify-hash', `hcl:with-hash-table-locked'.
#+:lispworks
(defun vg-make-hash-table (&key size 
                           rehash-size 
                           rehash-threshold
                           (test 'eql)
                           hash-function
                           (weakness nil)
                           (synchronized nil)
                           free-function)  ;; &allow-other-keys ??
  (declare (type (or boolean (member :value :key :one :both :either)) weakness)
           (type boolean synchronized))
  (make-hash-table :size             size
                   :rehash-size      rehash-size
                   :rehash-threshold rehash-threshold
                   :test             test
                   :hash-function    hash-function
                   :weak-kind        weakness
                   :single-thread    synchronized
                   :free-function    free-function))

;; CLisp hash-tables
;; :NOTE currently we ingore any argument to keyword SYNCHRONIZED
;;  Do Clisp's hash-tables provide a locking mechanism --  ????
;;
;; (&key size rehash-size rehash-threshold initial-contents key-type value-type
;;       warn-if-needs-rehash-after-gc weak test)
;;
;; :TODO Figure out if our :test any vg functions can leverage
;; `ext:fasthash-<FOO>' or `ext:stablehash-<FOO>' particularly the `ext:stablehash-<FOO>'s.
;;
;;  :NOTE AFAICT default values for `custom:*eq-hashfunction*',
;;  `custom:*eql-hashfunction*', `custom:*equal-hashfunction*', default to
;;  `ext:fasthash-eq', `ext:fasthash-eql', `ext:fasthash-equal'
;;
#+:clisp
(defun vg-make-hash-table (&key size rehash-size rehash-threshold test initial-contents weakness synchronized)
  ;; Keyword INITIAL-CONTENTS when non-nil is a _proper_ alist (e.g. a list of
  ;; cons cells) used to populat the key/value pairs of returned hash-table.
  ;; Keyword WEAKNESS is one of:
  ;;  nil :key :value :key-and-value :key-or-value
  ;;  nil           -- the hash-table is not contained of weak relations
  ;; :key           -- an `ext:weak-mapping' from hash-key to hash-value
  ;; :value         -- an `ext:weak-mapping' from hash-value to hash-key
  ;; :key-and-value -- an `ext:weak-and-relation' of the key and the value,
  ;; :key-or-value  -- an `ext:weak-or-relation' of the key and the value.
  ;; :SEE (URL `http://clisp.cons.org/impnotes/make-hash.html')
  ;; :SEE (URL `http://www.clisp.org/impnotes/weak.html#weak-ht')
  ;; :SEE-ALSO `ext:standard-stablehash', `ext:structure-stablehash',
  ;; `custom:*warn-on-hashtable-needing-rehash-after-gc*', `ext:hash-table-weak-p'.
  (declare (ignore synchronized)
           (type (member nil :key :value :key-and-value :key-or-value) weak)) ;; &allow-other-keys ??
  (make-hash-table :size size
                   :rehash-size rehash-size
                   :rehash-threshold rehash-threshold
                   (test 'eql)
                   ;; :hash-function hash-function
                   :weak weakness
                   ;; Clisp specific but see the equivalent alist-hash-fu in Alexandria 
                   ;; or`sb-impl::%stuff-hash-table' in sbcl/src/code/target-hash-table.lisp 
                   :initial-contents initial-contents))


;;; ==============================
;;; EOF
