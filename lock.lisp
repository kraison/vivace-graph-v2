(in-package #:vivace-graph-v2)

(defun print-rw-lock (lock stream depth)
  (format stream "#<RW-LOCK, W: ~A, R: ~A>" (lock-writer lock) (lock-readers lock)))

(defstruct (rw-lock
	     (:conc-name lock-)
	     (:print-function print-rw-lock)
	     (:predicate rw-lock?))
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (readers 0 :type integer)
  (semaphore (sb-thread:make-semaphore) :type sb-thread:semaphore)
  (writer-queue (make-empty-queue) :type queue)
  (writer nil)
  (waitqueue (sb-thread:make-waitqueue) :type sb-thread:waitqueue))

(defun next-in-queue? (rw-lock thread)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (and (not (empty-queue? (lock-writer-queue rw-lock)))
	 (eq thread (queue-front (lock-writer-queue rw-lock))))))

(defun lock-unused? (rw-lock)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (and (= 0 (lock-readers rw-lock))
	 (= 0 (sb-thread:semaphore-count (lock-semaphore rw-lock)))
	 (null (lock-writer rw-lock))
	 (empty-queue? (lock-writer-queue rw-lock)))))

(defun release-read-lock (rw-lock)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (when (eql 0 (decf (lock-readers rw-lock)))
      (when (lock-writer rw-lock)
	(sb-thread:signal-semaphore (lock-semaphore rw-lock))))))

(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  (loop for tries from 0 to max-tries do
       (sb-thread:with-recursive-lock ((lock-lock rw-lock))
	 (if (lock-writer rw-lock)
	     (condition-wait (lock-waitqueue rw-lock) (lock-lock rw-lock))
	     (progn
	       (incf (lock-readers rw-lock))
	       (return-from acquire-read-lock rw-lock))))))

(defmacro with-read-lock ((rw-lock) &body body)
  `(unwind-protect
	(if (rw-lock? (acquire-read-lock ,rw-lock))
	    (progn ,@body)
	    (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-read-lock ,rw-lock)))

(defun release-write-lock (rw-lock &key reading-p)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (if (next-in-queue? rw-lock (vg-current-thread))
	(dequeue (lock-writer-queue rw-lock))
	(error "Cannot release lock I don't own!"))
    (if (next-in-queue? rw-lock (vg-current-thread))
	;;(format t "Not releasing lock;  recursive ownership detected!~%")
	nil
	(progn
	  (setf (lock-writer rw-lock) nil)
	  (when reading-p
	    (incf (lock-readers rw-lock)))
	  (sb-thread:condition-broadcast (lock-waitqueue rw-lock))))))

(defun acquire-write-lock (rw-lock &key (max-tries 1000) reading-p)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (if (and (next-in-queue? rw-lock (vg-current-thread))
	     (eq (lock-writer rw-lock) (vg-current-thread)))
	(progn
	  (enqueue-front (lock-writer-queue rw-lock) (vg-current-thread))
	  (return-from acquire-write-lock rw-lock))
	(enqueue (lock-writer-queue rw-lock) (vg-current-thread))))
  (loop for tries from 0 to max-tries do
       (if (eq (lock-writer rw-lock) (vg-current-thread))
	   (return-from acquire-write-lock rw-lock)
	   (let ((wait-p nil))
	     (handler-case
		 (sb-thread:with-recursive-lock ((lock-lock rw-lock))
		   (if (and (null (lock-writer rw-lock))
			    (next-in-queue? rw-lock (vg-current-thread)))
		       (progn
			 (setf (lock-writer rw-lock) (vg-current-thread))
			 (when reading-p
			   (decf (lock-readers rw-lock)))
			 (unless (eql 0 (lock-readers rw-lock))
			   (setf wait-p t)))
		       (sb-thread:condition-wait 
			(lock-waitqueue rw-lock) (lock-lock rw-lock))))
	       (error (c)
		 (format t "Got error ~A while acquiring write lock ~A" c rw-lock)))
	     (when wait-p
	       (sb-thread:wait-on-semaphore (lock-semaphore rw-lock)))))))

(defmacro with-write-lock ((rw-lock) &body body)
  `(unwind-protect
	(if (rw-lock? (acquire-write-lock ,rw-lock))
	    (progn ,@body)
	    (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-write-lock ,rw-lock)))

(defstruct (lock-pool
	     (:constructor %make-lock-pool)
	     (:predicate lock-pool?))
  (lock (make-recursive-lock))
  ;; (queue (sb-concurrency:make-queue))
  (queue (concurrent-make-queue))
  (acquired-locks (make-hash-table :synchronized t))
  (size 20))

(defun make-lock-pool (size)
  (let ((pool (%make-lock-pool :size size)))
    (dotimes (i size)
      ;; (sb-concurrency:enqueue (make-rw-lock) (lock-pool-queue pool)))
      (concurrent-enqueue (make-rw-lock) (lock-pool-queue pool)))
    pool))

(defun change-lock-pool-size (pool new-size)
  (cond ((> new-size (lock-pool-size pool))
	 (sb-thread:with-recursive-lock ((lock-pool-lock pool))
	   (cas (lock-pool-size pool) (lock-pool-size pool) new-size)
	   (dotimes (i (- new-size (lock-pool-size pool)))
	     ;; (sb-concurrency:enqueue (make-rw-lock) (lock-pool-queue pool)))))
	     (concurrent-enqueue (make-rw-lock) (lock-pool-queue pool)))))
	((< new-size (lock-pool-size pool))
	 (error "Cannot shrink lock pool size")))
  new-size)

(defun release-pool-lock (pool lock)
  (if (remhash lock (lock-pool-acquired-locks pool))
      ;; (sb-concurrency:enqueue lock (lock-pool-queue pool))
      (concurrent-enqueue lock (lock-pool-queue pool))
      (error "Lock ~A not in acquired-locks list" lock)))

(defun get-pool-lock (pool &key (wait-p t) timeout)
  (let ((start-time (gettimeofday)))
    (loop
       ;; (let ((lock (sb-concurrency:dequeue (lock-pool-queue pool))))
       (let ((lock (concurrent-dequeue (lock-pool-queue pool))))
	 (if (rw-lock? lock)
	     (progn
	       (setf (gethash lock (lock-pool-acquired-locks pool)) t)
	       (return-from get-pool-lock lock))
	     (if wait-p
		 (if (and timeout (> (gettimeofday) (+ start-time timeout)))
		     (return-from get-pool-lock nil)
		     (sleep 0.000000001))
		 (return-from get-pool-lock nil)))))))

#|
(defun test-rw-locks ()
  (let ((lock (make-rw-lock)))
    (make-thread
     #'(lambda () (with-write-lock (lock) 
		    (format t "1 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (with-write-lock (lock)
		      (format t "1 acquired recursive lock.~%")
		      (sleep 5)
		      (with-write-lock (lock)
			(format t "1 acquired recursive lock.~%")
			(sleep 5)
			(format t "1 releasing recursive write lock.~%"))
		      (format t "1 releasing recursive write lock.~%"))
		    (format t "1 releasing write lock.~%"))))
    (make-thread 
     #'(lambda () (with-read-lock (lock) (format t "2 got read lock~%") (sleep 5))))
    (make-thread 
     #'(lambda () (with-read-lock (lock) (format t "3 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-write-lock (lock) 
		    (format t "4 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (with-write-lock (lock)
		      (format t "4 acquired recursive lock.~%")
		      (sleep 5)
		      (with-write-lock (lock)
			(format t "4 acquired recursive lock.~%")
			(sleep 5)
			(format t "4 releasing recursive write lock.~%"))
		      (format t "4 releasing recursive write lock.~%"))
		    (format t "4 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-write-lock (lock) 
		    (format t "5 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (format t "5 releasing write lock.~%"))))
    (make-thread 
     #'(lambda () (with-read-lock (lock) (format t "6 got read lock~%") (sleep 5))))
    (make-thread 
     #'(lambda () (with-read-lock (lock) (format t "7 got read lock~%") (sleep 5))))))
|#
