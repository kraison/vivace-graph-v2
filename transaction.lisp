(in-package #:vivace-graph-v2)

(defparameter *current-transaction* nil)
(defparameter *max-log-file-length* 10000000)

(defstruct (transaction
	     (:conc-name tx-)
	     (:predicate transaction?))
  (queue nil)
  (mailbox (sb-concurrency:make-mailbox))
  (thread (current-thread))
  (store nil))

(defun replay-transaction (store)
  )

(defun restore-triple-store (store)
  )

(defun snapshot (store)
  (dump-triples (format nil "~A/snap-~A" (location store) (get-universal-time)) store))

(defun dump-transaction (stream tx)
  ;; FIXME: write something more useful for replay;  serializer needed...
  (when tx-queue
    (logger :info "Dumping tx ~A to ~A" tx stream)
    (format stream "START-TX ~A~%" (gettimeofday))
    (dolist (op (reverse (tx-queue tx)))
      (write op :stream stream :pretty nil)
      (format stream "~%")) 
    (format stream "END-TX ~A~%" (gettimeofday))
    (force-output stream)))

(defun roll-logfile (store stream)
  (when (and (streamp stream) (open-stream-p stream)) (close stream))
  (open (format nil "~A/tx-~A" (location store) (get-universal-time))
	:element-type '(unsigned-byte 8)
	:direction :output 
	:if-exists :append
	:if-does-not-exist :create))

(defun set-dirty (store)
  (with-open-file (stream (format nil "~A/.dirty" (location store)) 
			  :direction :output :if-exists :overwrite 
			  :if-does-not-exist :create)
    (format stream "~A" (gettimeofday))))

(defun set-clean (store)
  (delete-file (format nil "~A/.dirty" (location store))))

(defun clear-tx-log (store)
  (dolist (file (directory 
		 (make-pathname :directory (location store) :name :wild :type :wild)))
    (when (pathname-match-p file "tx-*")
      (delete-file file))))

(defun clear-snapshots (store)
  (dolist (file (directory 
		 (make-pathname :directory (location store) :name :wild :type :wild)))
    (when (pathname-match-p file "snap-*")
      (delete-file file))))

(defun start-logger (store)
  (make-thread 
   #'(lambda ()
       (handler-case
	   (let ((mailbox (sb-concurrency:make-mailbox)) (last-snapshot (gettimeofday))
		 (stream (roll-logfile store nil)))
	     (setf (log-mailbox store) mailbox)
	     (unwind-protect
		  (loop
		     (let ((msg (sb-concurrency:receive-message mailbox)))
		       (logger :info "tx-log thread received message ~A" msg)
		       (typecase msg
			 (transaction 
			  (progn
			    (set-dirty store)
			    (dump-transaction stream msg)
			    (sb-concurrency:send-message mailbox :snapshot)))
			 (keyword 
			  (case msg 
			    (:shutdown-and-clear
			     (close stream)
			     (clear-tx-log store)
			     (clear-snapshots store)
			     (set-clean store)
			     (quit))
			    (:shutdown 
			     (dolist 
				 (msg 
				   (sb-concurrency:receive-pending-messages mailbox))
			       (when (transaction? msg)
				 (dump-transaction stream msg)))
			     (close stream)
			     (snapshot store)
			     (set-clean store)
			     (quit))
			    (:snapshot 
			     (when (> 60 (- (gettimeofday) last-snapshot))
			       (roll-logfile store stream)
			       (snapshot store)
			       (set-clean store)
			       (setq last-snapshot (gettimeofday))))
			    (otherwise 
			     (logger :info 
				     "Unknown command to tx-log thread: ~A" msg))))))
		     (when (>= (file-length stream) *max-log-file-length*)
		       (setq stream (roll-logfile store stream))))
	       (when (and (streamp stream) (open-stream-p stream)) (close stream))))
	 (error (condition)
	   (logger :err "Unhandled error in tx logger for ~A: ~A" store condition))))
   :name (format nil "tx-log thread for ~A" store)))

(defmacro with-graph-transaction ((store) &body body)
  (let ((success (gensym)))
    `(let ((,success nil))
       (flet ((atomic-op ()
                ,@body))
	 ;;(format t "STORE: ~A~%" ,store)
         (cond ((and (transaction? *current-transaction*)
		     (equal (name (tx-store *current-transaction*)) (name ,store)))
		(atomic-op))
	       ((transaction? *current-transaction*)
		(error "Transactions cannot currently span multiple stores."))
	       (t
		(unwind-protect
		     (let ((*current-transaction* (make-transaction :store ,store)))
		       ;; Global serialization is not ideal.
		       (with-recursive-lock-held ((store-lock ,store))
			 (atomic-op))
		       (sb-concurrency:send-message (log-mailbox ,store)
						    *current-transaction*)
		       (setf ,success t))
		  (if ,success
		      t
		      nil))))))))
