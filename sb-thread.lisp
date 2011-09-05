
(in-package #:vivace-graph-v2)

;;; ==============================
;; :WAS
;; (in-package #:sb-thread)
;; (export 'get-spinlock     (find-package 'sb-thread))
;; (export 'release-spinlock (find-package 'sb-thread))
;; (export 'spinlock-value   (find-package 'sb-thread))
;; (export 'my-get-spinlock  (find-package 'sb-thread))
;;; ==============================

;; sb-thread::spinlock is a structure object 

(defun vg-current-thread ()
  ;; bordeaux-threads:current-thread is a simple wrapper fun around sb-thread:*current-thread*
  #-sbcl (bordeaux-threads:current-thread)
  #+sbcl sb-thread:*current-thread*)

(defun vg-current-thread-spinlock-value-check ()
  (eq (vg-current-thread)  
      (sb-thread::spinlock-value (sb-impl:hash-table-spinlock hash-table))))
  
;; (vg-get-spinlock (sb-impl:hash-table-spinlock hash-table))
;;
;; (defun my-get-spinlock (spinlock)
(defun vg-get-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0)))
  ;; :WAS 
  ;; (let* ((new sb-thread:*current-thread*)
  (let* ((new vg-current-thread) 
         (old (sb-ext:compare-and-swap (sb-thread::spinlock-value spinlock) nil new)))
    (when old
      (when (eq old new)
        (error "Recursive lock attempt on ~S." spinlock))
      (flet ((cas ()
               (if (sb-ext:compare-and-swap (sb-thread::spinlock-value spinlock) nil new)
		   (progn
		     (sleep 0.000000001)
                     ;; (bordeaux-threads:thread-yield) wraps `sb-thread:release-foreground'
                     ;; sb-thread:thread-yield is an alien-routine
		     (sb-thread:thread-yield))
                   (return-from vg-get-spinlock t))))
        (if ;; (and (not sb-thread::*interrupts-enabled*) sb-thread::*allow-with-interrupts*)
            (and (not sb-sys:*interrupts-enabled*) sb-sys:*allow-with-interrupts*)
            ;; If interrupts are disabled, but we are allowed to
            ;; enabled them, check for pending interrupts every once
            ;; in a while. %CHECK-INTERRUPTS is taking shortcuts, make
            ;; sure that deferrables are unblocked by doing an empty
            ;; WITH-INTERRUPTS once.
            (progn
              ;; (with-interrupts)
              (sb-sys:with-interrupts)
              (loop
		 (loop repeat 128 do (cas)) ; 128 is arbitrary here
		 (sb-unix::%check-interrupts)))
            (loop (cas)))))
    t))

;; I'm not comfortable understanding how to incorporate bordeaux-threads mutex
;; frobbing with sbcl's spinlocks...
;; SBCL's hash-table's are implemented as STRUCTURE-OBJECT with and
;; sb-impl:hash-table-spinlock as an accessor
;;
;; (bordeaux-threads:release-lock <-- sb-thread:release-mutex
;; (sb-thread::release-spinlock (sb-impl:hash-table-spinlock hash-table))


