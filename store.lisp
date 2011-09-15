(in-package #:vivace-graph-v2)

(defclass triple-store ()
  ((name :initarg :name :accessor store-name)))

(defclass local-triple-store (triple-store)
  ((main-idx :initarg :main-idx :accessor main-idx)
   (text-idx :initarg :text-idx :accessor text-idx)
   (log-mailbox :initarg :log-mailbox :accessor log-mailbox)
   (index-queue :initarg :index-queue :accessor index-queue)
   (delete-queue :initarg :delete-queue :accessor delete-queue)
   (indexed-predicates :initarg :indexed-predicates :accessor indexed-predicates)
   (templates :initarg :templates :accessor templates)
   (location :initarg :location :accessor location)
   (lock-pool :initarg :lock-pool :accessor lock-pool)
   (locks :initarg :locks :accessor locks)
   (logger-thread :initarg :logger-thread :accessor logger-thread)))

(defclass remote-triple-store (triple-store)
  ((host :initarg :host :accessor remote-host)
   (port :initarg :port :accessor remote-port)
   (user :initarg :user :accessor user)
   (pass :initarg :pass :accessor pass)))

(defgeneric triple-store? (thing)
  (:method ((store triple-store)) t)
  (:method (thing) nil))

(defun index-predicate? (name)
  (gethash name (indexed-predicates *store*)))

(defun list-indexed-predicates (&optional (store *store*))
  (let ((result nil))
    (maphash #'(lambda (k v) 
		 (when v (push k result))) 
	     (indexed-predicates store))
    (sort result #'string>)))

(defun make-fresh-store (name location &key (num-locks 10000))
  (let ((store
	 (make-instance 'local-triple-store
 			:name name
			:location location
			:main-idx (make-hierarchical-index)
			:lock-pool (make-lock-pool num-locks)
			:locks (make-hash-table :synchronized t :test 'equal)
			:text-idx (make-skip-list :key-equal 'equalp
						  :value-equal 'vg-uuid:uuid-eql
						  :duplicates-allowed? t)
			;; :log-mailbox  (sb-concurrency:make-mailbox)
			;; :index-queue  (sb-concurrency:make-queue)
			;; :delete-queue (sb-concurrency:make-queue)
			:log-mailbox  (concurrent-make-mailbox)
			:index-queue  (concurrent-make-queue)
			:delete-queue (concurrent-make-queue)
			:templates (make-hash-table :synchronized t :test 'eql)
			:indexed-predicates (make-hash-table :synchronized t 
							     :test 'eql))))
    (add-to-index (main-idx store) (vg-uuid::make-uuid-table :synchronized t) :id-idx)
    (setf (logger-thread store) (start-logger store))
    store))

(defun make-local-triple-store (name location)
  (make-fresh-store name location))

(defun create-triple-store (&key name if-exists? location host port 
			    user password)
  (declare (ignore if-exists?))
  (setq *graph* (or name location (format nil "~A:~A" host port)))
  (if location
      (let ((store (make-local-triple-store *graph* location)))
	(if (triple-store? store)
	    (setf (gethash (store-name store) *store-table*) store
		  *store* store)
	      (error "Unknown error opening triple-store at ~A." location)))
      (setq *store* (make-instance 'remote-triple-store
				   :name *graph*
				   :host host
				   :port port
				   :user user
				   :password password))))

(defun change-store (name)
  (let ((store (gethash name *store-table*)))
    (if (triple-store? store)
	(setq *store* store)
	(error "Unknown triple-store requested: ~A" name))))

(defun close-triple-store (&key (store *store*))
  (remhash (store-name store) *store-table*)
  (if (eql store *store*) (setq *store* nil))
  (stop-logger store)
  nil)

(defun open-triple-store (&key name location host port user password)
  (let ((store (create-triple-store :name name
				    :location location
				    :if-exists? :open
				    :host host
				    :port port
				    :user user
				    :port port
				    :password password)))
    (restore-triple-store store)
    (setq *store* store)))

(defun clear-triple-store (&optional (store *store*))
  ;; (sb-concurrency:send-message (log-mailbox store) :shutdown-and-clear)
  (concurrent-send-message (log-mailbox store) :shutdown-and-clear)
  (bt:join-thread (logger-thread store))
  (make-fresh-store *graph* (location store)))
  
(defun use-graph (name)
  (setq *graph* name))

(defun add-to-index-queue (thing &optional (store *store*))
  ;; (sb-concurrency:enqueue thing (index-queue store))
  (concurrent-enqueue thing (index-queue store)))

(defun add-to-delete-queue (thing &optional (store *store*))
  ;; (sb-concurrency:enqueue thing (delete-queue store)))
  (concurrent-enqueue thing (delete-queue store)))

(defun intern-spog (s p o g)
  (values 
   (if (stringp s) (intern s :graph-words) s)
   (if (stringp p) (intern p :graph-words) p)
   (if (stringp o) (intern o :graph-words) o)
   (if (stringp g) (intern g :graph-words) g)))

(defun lock-pattern (subject predicate object graph &key (kind :write) 
		     (store *store*))
  (multiple-value-bind (subject predicate object graph) 
      (intern-spog subject predicate object graph)
    (let ((lock nil) (pattern (list subject predicate object graph)))
      (logger :info "~A: Locking pattern ~A~%" *current-transaction* pattern)
      ;; LispWorks hcl:with-hash-table-locked hash-table &body body => results
      (sb-ext:with-locked-hash-table ((locks store))
	(setq lock 
	      (or (gethash pattern (locks store))
		  (setf (gethash pattern (locks store)) 
			(get-pool-lock (lock-pool store))))))
      (if (rw-lock? lock)
	  (if (eq kind :write)
	      (acquire-write-lock lock)
	      (acquire-read-lock lock))
	  (error "Unable to get lock for ~A" pattern)))))

(defun lock-triple (triple &key (kind :write) (store *store*))
  (lock-pattern (triple-subject triple) 
		(triple-predicate triple) 
		(triple-object triple) 
		(triple-graph triple)
		:kind kind
		:store store))

(defun unlock-pattern (subject predicate object graph &key kind (store *store*))
  (multiple-value-bind (subject predicate object graph) 
      (intern-spog subject predicate object graph)
    (let ((pattern (list subject predicate object graph)))
      ;; LispWorks hcl:with-hash-table-locked hash-table &body body => results
      (sb-ext:with-locked-hash-table ((locks store))
	(let ((lock (gethash pattern (locks store))))
	  (when (rw-lock? lock)
	    (sb-thread:with-recursive-lock ((lock-lock lock))
	      (case kind
		(:write (release-write-lock lock))
		(:read  (release-read-lock lock)))
	      (when (lock-unused? lock)
		(remhash pattern (locks store))
		(release-pool-lock (lock-pool store) lock)))))))))

(defun unlock-triple (triple &key kind (store *store*))
  (funcall #'unlock-pattern 
	   (triple-subject triple) 
	   (triple-predicate triple) 
	   (triple-object triple) 
	   (triple-graph triple)
	   :kind kind
	   :store store))

(defmacro with-locked-pattern ((subject predicate object graph kind) &body body)
  (with-gensyms (s p o g k)
    `(let ((,s ,subject) (,p ,predicate) (,o ,object) (,g ,graph) (,k ,kind))
       (unwind-protect
	    (progn
	      (lock-pattern ,s ,p ,o ,g :kind ,k :store *store*)
	      ,@body)
       (unlock-pattern ,s ,p ,o ,g :kind ,k :store *store*)))))

