(in-package #:sb-thread)

(export 'get-spinlock (find-package 'sb-thread))
(export 'my-get-spinlock (find-package 'sb-thread))
(export 'release-spinlock (find-package 'sb-thread))
(export 'spinlock-value (find-package 'sb-thread))

(defun my-get-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((new *current-thread*)
         (old (sb-ext:compare-and-swap (spinlock-value spinlock) nil new)))
    (when old
      (when (eq old new)
        (error "Recursive lock attempt on ~S." spinlock))
      (flet ((cas ()
               (if (sb-ext:compare-and-swap (spinlock-value spinlock) nil new)
		   (progn
		     (sleep 0.000000001)
		     (thread-yield))
                   (return-from my-get-spinlock t))))
        (if (and (not *interrupts-enabled*) *allow-with-interrupts*)
            ;; If interrupts are disabled, but we are allowed to
            ;; enabled them, check for pending interrupts every once
            ;; in a while. %CHECK-INTERRUPTS is taking shortcuts, make
            ;; sure that deferrables are unblocked by doing an empty
            ;; WITH-INTERRUPTS once.
            (progn
              (with-interrupts)
              (loop
		 (loop repeat 128 do (cas)) ; 128 is arbitrary here
		 (sb-unix::%check-interrupts)))
            (loop (cas)))))
    t))

