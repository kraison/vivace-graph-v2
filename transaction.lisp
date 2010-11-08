(in-package #:vivace-graph-v2)

(defparameter *current-transaction* nil)
(defparameter *max-log-file-length* 10000000)

(defstruct (transaction
	     (:conc-name tx-)
	     (:predicate transaction?))
  (queue (sb-concurrency:make-queue))
  (thread (current-thread))
  store)

(defun dump-transaction (stream tx)
  )

(defun start-logger (store)
  (let ((mailbox (sb-concurrency:make-mailbox)))
    (setf (log-mailbox store) mailbox)
    (make-thread 
     #'(lambda ()
	 (let (stream)
	   (unwind-protect
		(progn
		  (setq stream (open (format nil "~A/tx-~A" 
					     (location store) (get-universal-time))
				     :direction :output 
				     :if-exists :append
				     :if-does-not-exist :create))
		  (loop
		     (multiple-value-bind (msg msg?) 
			 (sb-concurrency:receive-message-no-hang mailbox)
		       (when msg?
			 (typecase msg
			   (transaction (dump-transaction stream msg))
			   (keyword (case msg 
				      (:shutdown (quit))
				      (otherwise 
				       (logger :info 
					       "Unknown command to tx-log thread: ~A" 
					       msg)))))))
		     (when (>= *max-log-file-length* (file-length stream))
		       (close stream)
		       (setq stream 
			     (open (format nil "~A/tx-~A" 
					   (location store) (get-universal-time))
				   :direction :output 
				   :if-exists :append
				   :if-does-not-exist :create)))))
	     (when (and (streamp stream) (open-stream-p stream)) (close stream))))))))

(defmacro with-transaction ((store) &body body)
  (let ((success (gensym)))
    `(let ((,success nil))
       (flet ((atomic-op ()
                ,@body))
         (cond ((and (transaction? *current-transaction*)
		     (equal (name (tx-store *current-transaction*)) (name ,store)))
		(atomic-op))
	       ((transaction? *current-transaction*)
		(error "Transactions cannot currently span multiple stores."))
	       (t
		(unwind-protect
		     (let ((*current-transaction* (make-transaction :store ,store)))
		       (atomic-op)
		       (sb-concurrency:send-message (log-mailbox ,store) 
						    *current-transaction*)
		       (setf ,success t))
		  (if ,success
		      t
		      nil))))))))
