;;; :FILE vivace-graph-v2-FORK/hash-table.lisp
;;; ==============================

;;; ==============================
;;
;; AFAICT It will be difficult to fully abstract away SBCL's spinlocks/mutex with bordeaux-threads 
;; (bordeaux-threads:release-lock   <-- sb-thread:release-mutex
;; (sb-thread::release-spinlock (sb-impl::hash-table-spinlock hash-table))
;;
;; SBCL's hash-table's are implemented as STRUCTURE-OBJECT with and
;; sb-impl::hash-table-spinlock as an slot.  The spinlock is used for locking 
;; cl:gethash/(setf cl:gethash)/cl:remhash and looks like this:
;;
;; (spinlock (sb-thread::make-spinlock :name "hash-table lock")
;;           :type sb-thread::spinlock :read-only t)
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
;;=> #S(TT--SPINLOCK :NAME "my lock" :VALUE 42)
;;
;;; ==============================


(in-package #:vivace-graph-v2)

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

(deftype vg-thread-name ()
  #-sbcl 'simple-string   ; <-- classic unsubstantiated and erroneous assumption
  #+sbcl 'sb-thread::thread-name)

(defun vg-current-thread ()
  ;; bordeaux-threads:current-thread is a simple wrapper fun around sb-thread:*current-thread*
  #-sbcl (bordeaux-threads:current-thread)
  #+sbcl sb-thread:*current-thread*)

;; :NOTE sb-thread::spinlock is a structure object 
;;
;; (defun vg-get-spinlock-value (spinlock)
;;   (declare (sb-thread::spinlock spinlock)
;;            (optimize (speed 3) (safety 1)))
;;   (unless (sb-thread::spinlock-p spinlock)
;;     (error "Arg SPINLOCK not `sb-thread::spinlock-p'"))
;;   (sb-thread::spinlock-value spinlock))
(defun vg-get-spinlock-value (spinlock)
  (declare #+sbcl (sb-thread::spinlock spinlock))
  #-sbcl (error "not implemented -- what is the equivalent of sb-thread::spinlock-value")
  #+sbcl (sb-thread::spinlock-value spinlock))
;; :FIXME I don't think I've correctly implementated a correct abstraction around CAS!
(defun vg-compare-and-swap-spinlock-value (cas-spinlock cas-old cas-new)
  #+sbcl (declare (sb-thread::spinlock cas-spinlock)
                  (inline vg-get-spinlock-value))
  #-sbcl (error "not implemented -- what is the equivalent of sb-ext:compare-and-swap?")
  #+sbcl
  (sb-ext:compare-and-swap (vg-get-spinlock-value cas-spinlock) cas-old cas-new))

(defun vg-get-hash-table-spinlock (hash-table)
  (declare (hash-table hash-table))
  #-sbcl (error "not implemented -- what is the equivalent of sb-impl:hash-table-spinlock?")
  #+sbcl (sb-impl::hash-table-spinlock hash-table))

(defun vg-get-hash-table-spinlock-value (hash-table)
  (declare (hash-table hash-table)
           #+sbcl (inline vg-get-hash-table-spinlock
                          vg-get-spinlock-value))
  (vg-get-spinlock-value (vg-get-hash-table-spinlock hash-table)))

(defun vg-current-thread-spinlock-value-check (hash-table)
  (declare (hash-table hash-table)
           #+sbcl (inline vg-current-thread
                          vg-get-spinlock-value
                          vg-get-hash-table-spinlock
                          vg-get-hash-table-spinlock-value))
  (eq (vg-current-thread)  
      (vg-get-hash-table-spinlock-value hash-table)))

(defun vg-allow-with-interrupts ()
  #-sbcl (error "not implemented -- what is equivalent of sb-sys:*allow-with-interrupts*?")
  #+sbcl sb-sys:*allow-with-interrupts*)

(defun vg-interrupts-not-enabled-check ()
  #-sbcl (error "not implemented -- what is equivalent of (not sb-sys:*interrupts-enabled*)?")
  #+sbcl (not sb-sys:*interrupts-enabled*))

(defun vg-interrupts-allowed-and-not-enabled-check ()
  #+sbcl (declare (inline vg-interrupts-not-enabled-check
                          vg-allow-with-interrupts))
  (and (vg-interrups-not-enabled-check)
       (vg-allow-with-interrupts)))

;; :NOTE `bordeaux-threads:thread-yield' wraps `sb-thread:release-foreground'
;; whereas `sb-thread:thread-yield' is an alien-routine
(defun vg-thread-yield-cas-spinlock ()
  #-sbcl (error "not implemented -- verify that bordeaux-threads:thread-yield ~
                 is applicably equivalent with sb-thread:thread-yield in this context")
  #+sbcl (sb-thread:thread-yield))

(defun vg-unix-interrupts-check ()
  #-sbcl (error "not implemented -- what is equivalent of sb-unix::%check-interrupts?")
  #+sbcl (sb-unix::%check-interrupts))

(defun vg-release-spinlock (spinlock)
  #-sbcl (error "not implemented -- what is equivalent of sb-thread::release-spinlock")
  #+sbcl (sb-thread::release-spinlock spinlock))

(defun vg-get-spinlock (spinlock)
  #+sbcl (declare (inline vg-current-thread
                          vg-get-spinlock-value
                          ;; vg-compare-and-swap-spinlock-value
                          vg-allow-with-interrupts
                          vg-interrupts-not-enabled-check
                          vg-interrupts-allowed-and-not-enabled-check
                          vg-thread-yield-cas-spinlock
                          vg-unix-interrupts-check)
                  (optimize (speed 3) (safety 0)))
  (let* ((new (vg-current-thread))
         (old ;; (vg-compare-and-swap-spinlock-value spinlock nil new)))
          (sb-ext:compare-and-swap (vg-get-spinlock-value cas-spinlock) nil new)))
    (when old
      (when (eq old new)
        (error "Recursive lock attempt on ~S." spinlock))
      (flet ((cas ()
               (if 
                (sb-ext:compare-and-swap (vg-get-spinlock-value cas-spinlock) nil new)
                ;;  (vg-compare-and-swap-spinlock-value spinlock nil new)
                   ;; :NOTE I _really_ hope this `sleep'ing is for
                   ;; thread-interrupts and not just to accomodate 
                   ;; 'uuid:make-v1-uuid' while it bangs on the system clock!...
		   (progn
                     (sleep 0.000000001)
		     (vg-thread-yield-cas-spinlock))
                   (return-from vg-get-spinlock t))))
        (if (vg-interrupts-allowed-and-not-enabled-check)
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
      (vg-get-spinlock (vg-get-hash-table-spinlock hash-table))))

(defun release-hash-table-lock (hash-table)
  (declare (hash-table hash-table))
  (if (vg-current-thread-spinlock-value-check hash-table)
      (vg-release-spinlock (vg-get-hash-table-spinlock hash-table))))


;;; ==============================
;;; EOF
