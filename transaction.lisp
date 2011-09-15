(in-package #:vivace-graph-v2)

(defparameter *current-transaction* nil)
(defparameter *max-log-file-length* 10000000)
(defparameter *file-counter* 0)

(defun print-transaction (tx stream depth)
  (declare (ignore depth))
  (format stream "#<TX-~A>" (tx-id tx)))

(defstruct (transaction
	     (:print-function print-transaction)
	     (:conc-name tx-)
	     (:predicate transaction?))
  (id       (vg-uuid::make-v4-uuid))
  (queue    nil)
  (rollback nil)
  ;; (mailbox  (sb-concurrency:make-mailbox))
  (mailbox  (concurrent-make-mailbox))
  (thread   (vg-current-thread))
  (store    nil)
  (locks    nil))

(defun find-newest-snapshot (store)
  (let ((snap nil) 
	(location (if (pathnamep (location store)) (namestring (location store))
		      (location store))))
    (dolist (file (directory (make-pathname :directory location
					    :name :wild :type :wild)))
      (when (and (pathname-match-p file "snap-*") 
		 (or (null snap)
		     (> (file-write-date file) (file-write-date snap))))
	(setq snap file)))
    (if snap
	(values snap (file-write-date snap))
	(values nil nil))))
  
(defun find-transactions (store timestamp)
  (let ((transaction-logs nil) 
	(location (if (pathnamep (location store)) 
		      (namestring (location store))
		      (location store))))
    (format t "Looking for transactions to restore...~%")
    (dolist (file (directory (make-pathname :directory location
					    :name :wild :type :wild)))
      (when (and (pathname-match-p file "tx-*") 
		 (or (null timestamp)
		     (and (numberp timestamp) 
			  (> (file-write-date file) timestamp))))
	(format t "Found transaction file ~A~%" file)
	(push file transaction-logs)))
    (sort transaction-logs
	  #'(lambda (x y)
	      (when (and (stringp x) (stringp y))
		(let ((pieces-x (cl-ppcre:split "\-" (pathname-name x)))
		      (pieces-y (cl-ppcre:split "\-" (pathname-name y))))
		  (or (< (parse-integer (nth 1 pieces-x))
			 (parse-integer (nth 1 pieces-y)))
		      (and (= (parse-integer (nth 1 pieces-y))
			      (parse-integer (nth 1 pieces-x)))
			   (< (parse-integer (nth 2 pieces-x))
			      (parse-integer (nth 2 pieces-y))))))))
	  :key #'namestring)))

(defun replay-transactions (file &optional (store *store*))
  (let ((*store* store))
    (with-open-file (stream file :element-type '(unsigned-byte 8))
      (let ((magic-byte (read-byte stream nil :eof)))
	(unless (= +transaction+ magic-byte)
	  (error 'transaction-error 
		 :reason (format nil "~A is not a tx file!" file)))
	(deserialize-action magic-byte stream)))))

(defun restore-triple-store (store)
  (let ((*store* store))
    (with-locked-index ((main-idx store))
      (multiple-value-bind (snapshot-file timestamp) (find-newest-snapshot store)
	(when snapshot-file
	  (format t "Restoring from snapshot file ~A~%" snapshot-file)
	  (with-open-file (stream snapshot-file :element-type '(unsigned-byte 8))
	    (do ((code (read-byte stream nil :eof) (read-byte stream nil :eof)))
		((or (eql code :eof) (null code) (= code 0)))
	      ;;(format t "GOT CODE 0x~X -> ~A~%" code (deserialize code stream))))
	      (deserialize code stream))))
	(dolist (file (find-transactions store timestamp))
	  (format t "REPLAYING TX ~A~%" file)
	  (replay-transactions file))
	(do-indexing store)
	store))))

(defun snapshot (store)
  (with-open-file 
      (stream 
       (format nil "~A/snap-~A" (location store) (get-universal-time))
       :direction :output 
       :element-type '(unsigned-byte 8)
       :if-exists :overwrite
       :if-does-not-exist :create)
    (with-locked-index ((main-idx store))
      (maphash #'(lambda (id triple)
		   (declare (ignore id))
		   (when (persistent? triple)
		     (logger :info "serializing ~A: ~A" 
			     (triple-id triple) triple)
		     (serialize triple stream)))
	       (gethash :id-idx (index-table (main-idx store)))))
    (logger :info "Recording null byte")
    (write-byte 0 stream)
    (force-output stream)))

(defun roll-logfile (store stream)
  (when (and (streamp stream) (open-stream-p stream)) (close stream))
  (open (format nil "~A/tx-~A" (location store) (get-universal-time))
	:element-type '(unsigned-byte 8)
	:direction :output 
	:if-exists :rename
	:if-does-not-exist :create))

(defun set-dirty (store)
  (with-open-file (stream (format nil "~A/.dirty" (location store)) 
			  :direction :output :if-exists :overwrite 
			  :if-does-not-exist :create)
    (format stream "~A" (gettimeofday))))

(defun set-clean (store)
  (let ((file (format nil "~A/.dirty" (location store))))  
    (when (probe-file file)
      (delete-file file))))

(defun clear-tx-log (store)
  (dolist (file (directory 
		 (make-pathname :directory (location store) 
				:name :wild :type :wild)))
    (when (pathname-match-p file "tx-*")
      (delete-file file))))

(defun clear-snapshots (store)
  (dolist (file (directory 
		 (make-pathname :directory (location store) 
				:name :wild :type :wild)))
    (when (pathname-match-p file "snap-*")
      (delete-file file))))

(defun dump-transaction (stream tx)
  (when (and (transaction? tx) (tx-queue tx))
    (logger :info "Dumping tx ~A to ~A" tx stream)
    (serialize-action :transaction stream tx)
    (force-output stream)))

(defun record-tx (tx store)
  (when (and (transaction? tx) (tx-queue tx))
    (logger :info "Recording tx ~A~%" (reverse (tx-queue tx)))
    (handler-case
	(with-open-file (stream 
			 (format nil "~A/tx-~A-~A" (location store) 
				 (get-universal-time) (incf *file-counter*))
			 :element-type '(unsigned-byte 8) :direction :output
			 :if-exists :rename :if-does-not-exist :create)
	  (set-dirty store)
	  (dump-transaction stream tx))
      (error (c)
	(logger :err "Unhandled error in record-tx: ~A" c)))))

(defun stop-logger (store)
  ;; (sb-concurrency:send-message (log-mailbox store) :shutdown)
  (concurrent-send-message (log-mailbox store) :shutdown)
  (bt:join-thread (logger-thread store)))

(defun start-logger (store)
  (make-thread 
   #'(lambda ()
       ;; (let ((mailbox (sb-concurrency:make-mailbox)) 
       (let ((mailbox (concurrent-make-mailbox))
             (*file-counter* 0)
	     (last-snapshot (gettimeofday)))
	 (setf (log-mailbox store) mailbox)
	 (loop
	    (handler-case
		;; (let ((msg (sb-concurrency:receive-message mailbox)))
                (let ((msg (concurrent-receive-message mailbox)))
		  (logger :info "tx-log thread received message ~A" msg)
		  (typecase msg
		    (transaction (record-tx msg store))
		    (keyword 
		     (case msg 
		       (:shutdown-and-clear
			(clear-tx-log store)
			(clear-snapshots store)
			(set-clean store)
			(quit))
		       (:shutdown 
			(logger :info "Processing all pending messages.")
                        ;; (dolist (msg (sb-concurrency:receive-pending-messages mailbox))
                        (dolist (msg (concurrent-receive-pending-messages mailbox))
			  (logger :info "Processing message ~A" msg)
			  (when (transaction? msg)
			    (record-tx msg store)))
			;;(logger :info "Snapshotting the store.")
			;;(snapshot store)
			;;(logger :info "Marking the store clean.")
			;;(set-clean store)
			(logger :info "Logger thread quitting.")
			(return t))
		       (:snapshot
			(logger :info "Snapshot commencing")
			(snapshot store)
			(logger :info "Snapshot complete.  Setting store CLEAN")
			(set-clean store)
			(logger :info "Store set CLEAN")
			(setq last-snapshot (gettimeofday))
			(logger :info "Snapshot finished"))
		       (otherwise 
			(logger :info "Unknown msg to tx-log thread: ~A" 
				msg))))))
	      (error (condition)
		(logger :err "Unhandled error in tx logger for ~A: ~A" 
			store condition))))))
   :name (format nil "tx-log thread for ~A" store)))

(defun release-all-locks (tx)
  ;; LispWorks hcl:with-hash-table-locked hash-table &body body => results
  (sb-ext:with-locked-hash-table ((locks *store*))
    (dolist (pair (tx-locks tx))
      (destructuring-bind (pattern-or-triple lock kind) pair
	(declare (ignore lock))
	(if (triple? pattern-or-triple)
	    (unlock-triple pattern-or-triple :kind kind)
	    (funcall #'unlock-pattern 
		     (nth 0 pattern-or-triple)
		     (nth 1 pattern-or-triple)
		     (nth 2 pattern-or-triple)
		     (nth 3 pattern-or-triple)
		     :kind kind))))))

(defun enqueue-lock (pattern lock kind)
  (push (list pattern lock kind) (tx-locks *current-transaction*)))

(defun rollback-tx (tx)
  (dolist (fn (reverse (tx-rollback tx)))
    (funcall fn)))

(defun execute-tx (store fn timeout max-tries retries)
  (if (>= retries max-tries)
      (error 'transaction-error
	     :reason 
	     (format nil "Unable to execute transaction. Too may retries (~A)."
		     retries))
      (let ((*current-transaction* (make-transaction :store store)))
	(logger :info "~A execute-tx starting" *current-transaction*)
	(handler-case
	    (sb-ext:with-timeout timeout
	      (funcall fn))
	  (sb-ext:timeout (condition)
	    (logger :info "~A execute-tx timeout ~A" 
		    *current-transaction* condition)
	    (rollback-tx *current-transaction*)
	    (release-all-locks *current-transaction*)
	    (execute-tx store fn timeout max-tries (1+ retries)))
	  (error (condition)
	    (logger :info "~A execute-tx error ~A" 
		    *current-transaction* condition)
	    (rollback-tx *current-transaction*)
	    (release-all-locks *current-transaction*)
	    (error 'transaction-error 
		   :reason 
		   (format nil "Unable to execute transaction: ~A" condition)))
	  (:no-error (result)
	    (logger :info "~A execute-tx success (~A)" 
		    *current-transaction* result)
	    (when (tx-queue *current-transaction*)
	      ;; (sb-concurrency:send-message (log-mailbox store) *current-transaction*))
              (concurrent-send-message (log-mailbox store) *current-transaction*))
	    (release-all-locks *current-transaction*)
	    result)))))

(defmacro with-graph-transaction ((store &key (timeout 10) (max-tries 10)) 
				  &body body)
  (with-gensyms (atomic-op)
    `(let ((,atomic-op #'(lambda () ,@body)))
       (cond ((and (transaction? *current-transaction*)
		   (equal (store-name (tx-store *current-transaction*)) 
			  (store-name ,store)))
	      (funcall ,atomic-op))
	     ((transaction? *current-transaction*)
	      (error 'transaction-error
		     :reason 
		     "Transactions cannot currently span multiple stores."))
	     (t
	      (execute-tx ,store ,atomic-op ,timeout ,max-tries 0))))))
