(in-package #:vivace-graph-v2)

(defparameter *current-transaction* nil)
(defparameter *max-log-file-length* 10000000)
(defparameter *file-counter* 1)

(defstruct (transaction
	     (:conc-name tx-)
	     (:predicate transaction?))
  (queue nil)
  (mailbox (sb-concurrency:make-mailbox))
  (thread (current-thread))
  (store nil))

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
	(location (if (pathnamep (location store)) (namestring (location store))
		      (location store))))
    (dolist (file (directory (make-pathname :directory location
					    :name :wild :type :wild)))
      (format t "Looking for transactions: ~A~%" file)
      (when (and (pathname-match-p file "tx-*") 
		 (> (file-write-date file) timestamp))
	(push file transaction-logs)))
    (when transaction-logs
      (sort transaction-logs #'string< 
	    :key #'(lambda (x y)
		     (let ((pieces-x (cl-ppcre:split "\-" x))
			   (pieces-y (cl-ppcre:split "\-" y))) 
		       (and (>= (parse-integer (nth 1 pieces-y))
				(parse-integer (nth 1 pieces-x)))
			    (> (parse-integer (nth 2 pieces-y))
			       (parse-integer (nth 2 pieces-x))))))))))

(defun replay-transactions (file &optional (store *store*))
  (let ((*store* store))
    (with-open-file (stream file :element-type '(unsigned-byte 8))
      (do ((code (read-byte stream nil :eof) (read-byte stream nil :eof)))
	  ((or (eql code :eof) (null code)))
	(format t "CODE ~A: ~A~%" code (deserialize-action code stream))))))

(defun restore-triple-store (store)
  (let ((*store* store))
    (with-locked-index ((main-idx store))
      (multiple-value-bind (snapshot-file timestamp) (find-newest-snapshot store)
	(format t "Restoring form snapshot file ~A~%" snapshot-file)
	(with-open-file (stream snapshot-file :element-type '(unsigned-byte 8))
	  (do ((code (read-byte stream nil :eof) (read-byte stream nil :eof)))
	      ((or (eql code :eof) (null code) (= code 0)))
	    (format t "GOT CODE 0x~X~%" code)
	    (format t "   -> ~A~%" (deserialize code stream))))
	(dolist (file (find-transactions store timestamp))
	  (format t "REPLAYING TX ~A~%" file)
	  (replay-transactions file))
	(do-indexing store)
	store))))

(defun snapshot (store)
  (with-open-file (stream 
		   (format nil "~A/snap-~A" (location store) (get-universal-time))
		   :direction :output 
		   :element-type '(unsigned-byte 8)
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (with-locked-index ((main-idx store))
      (maphash #'(lambda (id triple)
		   (declare (ignore id))
		   (when (persistent? triple)
		     (serialize triple stream)))
	       (gethash :id-idx (index-table (main-idx store)))))
    (write-byte 0 stream)
    (force-output stream)))

(defun dump-transaction (stream tx)
  (when (and (transaction? tx) (tx-queue tx))
    (logger :info "Dumping tx ~A to ~A" tx stream)
    (serialize-action :transaction stream tx)
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

(defun record-tx (tx store)
  (when (and (transaction? tx) (tx-queue tx))
    (with-open-file (stream (format nil "~A/tx-~A-~A" (location store) 
				    (get-universal-time) (incf *file-counter*))
			    :element-type '(unsigned-byte 8) :direction :output
			    :if-exists :rename :if-does-not-exist :create)
      (set-dirty store)
      (dump-transaction stream tx))))
    ;;(sb-concurrency:send-message mailbox :snapshot)))

(defun start-logger (store)
  (make-thread 
   #'(lambda ()
       (handler-case
	   (let ((mailbox (sb-concurrency:make-mailbox)) (*file-counter* 1)
		 (last-snapshot (gettimeofday)))
	     (setf (log-mailbox store) mailbox)
	     (loop
		(let ((msg (sb-concurrency:receive-message mailbox)))
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
			(dolist 
			    (msg 
			      (sb-concurrency:receive-pending-messages mailbox))
			  (when (transaction? msg)
			    (record-tx msg store)))
			(snapshot store)
			(set-clean store)
			(quit))
		       (:snapshot 
			;;(when (> 600 (- (gettimeofday) last-snapshot))
			(snapshot store)
			(set-clean store)
			(setq last-snapshot (gettimeofday)))
		       (otherwise 
			(logger :info "Unknown msg to tx-log thread: ~A" msg))))))))
	 (error (condition)
	   (logger :err "Unhandled error in tx logger for ~A: ~A" store condition))))
   :name (format nil "tx-log thread for ~A" store)))

(defmacro with-graph-transaction ((store) &body body)
  (let ((success (gensym)))
    `(let ((,success nil))
       (flet ((atomic-op ()
                ,@body))
         (cond ((and (transaction? *current-transaction*)
		     (equal (store-name (tx-store *current-transaction*)) 
			    (store-name ,store)))
		(atomic-op))
	       ((transaction? *current-transaction*)
		(error "Transactions cannot currently span multiple stores."))
	       (t
		(let ((*current-transaction* (make-transaction :store ,store)))
		  ;; Global serialization is not ideal.
		  (prog1
		      (with-locked-index ((main-idx ,store))
			(atomic-op))
		    (sb-concurrency:send-message (log-mailbox ,store)
						 *current-transaction*)
		    (setf ,success t)))))))))
