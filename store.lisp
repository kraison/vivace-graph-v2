(in-package #:vivace-graph-v2)

(defclass triple-store ()
  ((name :initarg :name :accessor name)))

(defclass local-triple-store (triple-store)
  (;;(spogi-idx :initarg :spogi-idx :accessor spogi-idx)
   ;;(posgi-idx :initarg :posgi-idx :accessor posgi-idx)
   ;;(ospgi-idx :initarg :ospgi-idx :accessor ospgi-idx)
   ;;(gspoi-idx :initarg :gspoi-idx :accessor gspoi-idx)
   ;;(gposi-idx :initarg :gposi-idx :accessor gposi-idx)
   ;;(gospi-idx :initarg :gospi-idx :accessor gospi-idx)
   ;;(id-idx :initarg :id-idx :accessor id-idx)
   (main-idx :initarg :main-idx :accessor main-idx)
   (text-idx :initarg :text-idx :accessor text-idx)
   (log-mailbox :initarg :log-mailbox :accessor log-mailbox)
   (index-queue :initarg :index-queue :accessor index-queue)
   (delete-queue :initarg :delete-queue :accessor delete-queue)
   (indexed-predicates :initarg :indexed-predicates :accessor indexed-predicates)
   (templates :initarg :templates :accessor templates)
   (location :initarg :location :accessor location)
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
    (maphash #'(lambda (k v) (when v (push k result))) (indexed-predicates store))
    (sort result #'string>)))

(defun make-fresh-store (name location)
  (let ((store
	 (make-instance 'local-triple-store
			:name name
			:location location
			:main-idx (make-hierarchical-index)
			;;:spogi-idx (make-hierarchical-index)
			;;:posgi-idx (make-hierarchical-index)
			;;:ospgi-idx (make-hierarchical-index)
			;;:gspoi-idx (make-hierarchical-index)
			;;:gposi-idx (make-hierarchical-index)
			;;:gospi-idx (make-hierarchical-index)
			;;:id-idx (make-uuid-table :synchronized t)
			:text-idx (make-skip-list :key-equal 'equalp
						  :value-equal 'uuid:uuid-eql
						  :duplicates-allowed? t)
			:log-mailbox (sb-concurrency:make-mailbox)
			:index-queue (sb-concurrency:make-queue)
			:delete-queue (sb-concurrency:make-queue)
			:templates (make-hash-table :synchronized t :test 'eql)
			:indexed-predicates (make-hash-table :synchronized t 
							     :test 'eql))))
    (add-to-index (main-idx store) (make-uuid-table :synchronized t) :id-idx)
    (setf (logger-thread store) (start-logger store))
    store))

(defun make-local-triple-store (name location)
  (make-fresh-store name location))

(defun create-triple-store (&key name if-exists? location host port user password)
  (declare (ignore if-exists?))
  (setq *graph* (or name location (format nil "~A:~A" host port)))
  (if location
      (let ((store (make-local-triple-store *graph* location)))
	(if (triple-store? store)
	    (setf (gethash (name store) *store-table*) store
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
  (remhash (name store) *store-table*)
  (if (eql store *store*) (setq *store* nil))
  (sb-concurrency:send-message (log-mailbox store) :shutdown)
  (join-thread (logger-thread store))
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
  (sb-concurrency:send-message (log-mailbox store) :shutdown-and-clear)
  (join-thread (logger-thread store))
  (make-fresh-store *graph* (location store)))
  
(defun use-graph (name)
  (setq *graph* name))

(defun add-to-index-queue (thing &optional (store *store*))
  (sb-concurrency:enqueue thing (index-queue store)))

(defun add-to-delete-queue (thing &optional (store *store*))
  (sb-concurrency:enqueue thing (delete-queue store)))

