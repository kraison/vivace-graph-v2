(in-package #:vivace-graph-v2)

(defgeneric triple-eql (t1 t2)
  (:method ((t1 triple) (t2 triple)) 
    (vg-uuid:uuid-eql (id t1) (id t2)))
  (:method (t1 t2) nil))

(defgeneric triple-equal (t1 t2)
  (:method ((t1 triple) (t2 triple)) 
    (and (vg-uuid:uuid-eql (id t1) (id t2))
	 (equal (triple-subject t1) (triple-subject t2))
	 (equal (triple-predicate t1) (triple-predicate t2))
	 (equal (triple-object t1) (triple-object t2))))
  (:method (t1 t2) nil))

(defgeneric triple-equalp (t1 t2)
  (:method ((t1 triple) (t2 triple)) 
    (and (triple-equal t1 t2)
	 (equal (triple-graph t1) (triple-graph t2))))
  (:method (t1 t2) nil))

(defmethod deleted? ((triple triple))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
	(triple-deleted? triple))	
      (triple-deleted? triple)))

(defmethod subject ((triple triple))
  (flet ((get-value ()
	   (if (and (symbolp (triple-subject triple)) 
		    (eq *graph-words* (symbol-package (triple-subject triple))))
	       (symbol-name (triple-subject triple))
	       (triple-subject triple))))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
 	(get-value))
      (get-value))))

(defmethod subject ((list list))
  (second list))

(defmethod predicate ((triple triple))
  (flet ((get-value ()
	   (if (and (symbolp (triple-predicate triple)) 
		    (eq *graph-words* 
			(symbol-package (triple-predicate triple))))
	       (symbol-name (triple-predicate triple))
	       (triple-predicate triple))))
    (if (not *read-uncommitted*)
	(with-graph-transaction (*store*)
	  (enqueue-lock triple (lock-triple triple :kind :read) :read)
	  (get-value))
	(get-value))))

(defmethod predicate ((list list))
  (first list))

(defmethod object ((triple triple))
  (flet ((get-value ()
	   (if (and (symbolp (triple-object triple)) 
		    (eq *graph-words* (symbol-package (triple-object triple))))
	       (symbol-name (triple-object triple))
	       (triple-object triple))))
    (if (not *read-uncommitted*)
	(with-graph-transaction (*store*)
	  (enqueue-lock triple (lock-triple triple :kind :read) :read)
	  (get-value))
	(get-value))))

(defmethod object ((list list))
  (third list))

(defmethod graph ((triple triple))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
	(triple-graph triple))
      (triple-graph triple)))

(defmethod graph ((list list))
  (fourth list))

(defmethod id ((triple triple))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
	(triple-id triple))
      (triple-id triple)))

(defmethod id ((list list))
  (fifth list))

(defmethod cf ((triple triple))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
	(triple-cf triple))
      (triple-cf triple)))

(defmethod belief-factor ((triple triple))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
	(triple-cf triple))
      (triple-cf triple)))

(defmethod persistent? ((triple triple))
  (if (not *read-uncommitted*)
      (with-graph-transaction (*store*)
	(enqueue-lock triple (lock-triple triple :kind :read) :read)
	(triple-persistent? triple))
      (triple-persistent? triple)))

;; :NOTE Our initial naive assumption is that it may be faster to use
;; `make-v4-uuid' esp. as long as the rest of the system continues using
;; `vg-uuid:make-v1-uuid', using a v4-uuid we could then check `anonymous?' by
;; examining if the version 4 bit is set. This said, it would be _much_ cleaner
;; to eschew v1 uuids completely int the the UUID reliant portions of the
;; system by using v3 or v5 UUIDs instead.
(defun make-anonymous-node ()
  "Create a unique anonymous node.
:SEE-ALSO `deftemplate'"
  (format nil "_anon:~A" (vg-uuid:make-v1-uuid)))

;; If the uuid library were more like Unicly it would do "the right thing" per
;; the RFC by case-sensitively printing hex chars of UUID objects in lower
;; case...  There is a minor performance optimization to be had by avoiding
;; having to match [a-fA-F] and instead matching only [a-f]{4}
;;
;; (let ((regex (cl-ppcre:create-scanner "^_anon\:[0-9a-f]{8}(\-[0-9a-f]{4}){3}-[0-9a-f]{12}$")))
;;   (cl-ppcre:scan regex (concatenate 'string "_anon:" (unicly::princ-to-string (unicly:make-v4-uuid)))))
;; => 0, 42, #(24), #(29)
;;
;; (let ((regex (cl-ppcre:create-scanner "^_anon\:[0-9a-f]{8}(\-[0-9a-f]{4}){3}-[0-9a-f]{12}$")))
;;   (cl-ppcre:scan regex (concatenate 'string "_anon:" (format nil "~S" (uuid:make-v4-uuid)))))
;; => NIL
(let ((regex 
       ;; (cl-ppcre:create-scanner "^_anon\:[0-9a-f]{8}(\-[0-9a-f]{4}){3}-[0-9a-f]{12}$"))
       "^_anon\:[0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}$"))
  (defun anonymous? (node)
    (and (stringp node)
	 (cl-ppcre:scan regex node))))

(defun make-text-idx-key (g s p o)
  (string-downcase (format nil "~A~A~A~A~A~A~A" g #\Nul s #\Nul p #\Nul o)))

(defun index-predicate (name-string)
  (setf (gethash name-string (indexed-predicates *store*)) t))
	
(defun unindex-predicate (name-string)
  (setf (gethash name-string (indexed-predicates *store*)) nil))

;; This method appears to be unused.
(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun set-triple-cf (triple new-value)
  (with-graph-transaction (*store*)
    (enqueue-lock triple (lock-triple triple :kind :write) :write)
    (let ((old-cf (triple-cf triple)))
      (when (persistent? triple)
	(push (list :set-cf triple) (tx-queue *current-transaction*)))
      (push (lambda () (setf (triple-cf triple) old-cf))
	    (tx-rollback *current-transaction*))
      (cas (triple-cf triple) (triple-cf triple) new-value))))

(defun undelete-triple (triple &key (persistent? t))
  (with-graph-transaction (*store*)
    (enqueue-lock triple (lock-triple triple :kind :write) :write)
    (when persistent?
      (push (list :undelete-triple triple) (tx-queue *current-transaction*)))
    (let ((old-value (triple-deleted? triple)))
      (push (lambda () (setf (triple-deleted? triple) old-value))
	    (tx-rollback *current-transaction*)))
    (cas (triple-deleted? triple) (triple-deleted? triple) nil))
  triple)

(defun delete-triple (triple &key (persistent? t))
  (with-graph-transaction (*store*)
    (enqueue-lock triple (lock-triple triple :kind :write) :write)
    (when persistent?
      (push (list :delete-triple triple) (tx-queue *current-transaction*)))
    (let ((old-value (triple-deleted? triple)))
      (push (lambda () (setf (triple-deleted? triple) old-value))
	    (tx-rollback *current-transaction*)))
    (cas (triple-deleted? triple) nil (gettimeofday))))

(defun truly-delete-triple (triple &key (persistent? t))
  ;; FIXME: create method for truly deleting triples, rather than marking them deleted.
  (with-graph-transaction (*store*)
    (enqueue-lock triple (lock-triple triple :kind :write) :write)
    (when persistent?
      (push (list :delete-triple triple) (tx-queue *current-transaction*)))
    (cas (triple-deleted? triple) nil (gettimeofday))))

(defun %deindex-triple (triple &optional (store *store*))
  (delete-from-index (main-idx store) (id triple) :gspoi-idx
		     (triple-graph triple) (triple-subject triple) 
		     (triple-predicate triple) (triple-object triple))
  (delete-from-index (main-idx store) (id triple) :spogi-idx
		     (triple-subject triple) (triple-predicate triple) 
		     (triple-object triple) (triple-graph triple))
  (delete-from-index (main-idx store) (id triple) :posgi-idx
		     (triple-predicate triple) (triple-object triple) 
		     (triple-subject triple) (triple-graph triple))
  (delete-from-index (main-idx store) (id triple) :ospgi-idx
		     (triple-object triple) (triple-subject triple) 
		     (triple-predicate triple) (triple-graph triple))
  (delete-from-index (main-idx store) (id triple) :gposi-idx
		     (triple-graph triple) (triple-predicate triple) 
		     (triple-object triple) (triple-subject triple))
  (delete-from-index (main-idx store) (id triple) :gospi-idx
		     (triple-graph triple) (triple-object triple) 
		     (triple-subject triple) (triple-predicate triple))
  (when (index-predicate? (predicate triple))
    (remove-from-text-index (text-idx *store*)
			    (make-text-idx-key (graph triple) (subject triple) 
					       (predicate triple) (object triple))))
  t)
  
(defun index-triple (triple &optional (store *store*))
  (with-graph-transaction (store)
    (enqueue-lock triple (lock-triple triple :kind :write) :write)
    (push (lambda () (%deindex-triple triple)) (tx-rollback *current-transaction*))
    (add-to-index (main-idx store) (id triple) :gspoi-idx
		  (triple-graph triple) (triple-subject triple) 
		  (triple-predicate triple) (triple-object triple))
    (add-to-index (main-idx store) (id triple) :spogi-idx
		  (triple-subject triple) (triple-predicate triple) 
		  (triple-object triple) (triple-graph triple))
    (add-to-index (main-idx store) (id triple) :posgi-idx
		  (triple-predicate triple) (triple-object triple) 
		  (triple-subject triple) (triple-graph triple))
    (add-to-index (main-idx store) (id triple) :ospgi-idx
		  (triple-object triple) (triple-subject triple) 
		  (triple-predicate triple) (triple-graph triple))
    (add-to-index (main-idx store) (id triple) :gposi-idx
		  (triple-graph triple) (triple-predicate triple) 
		  (triple-object triple) (triple-subject triple))
    (add-to-index (main-idx store) (id triple) :gospi-idx
		  (triple-graph triple) (triple-object triple) 
		  (triple-subject triple) (triple-predicate triple))
    (when (index-predicate? (predicate triple))
      (add-to-text-index (text-idx *store*)
			 (make-text-idx-key (graph triple) (subject triple) 
					    (predicate triple) (object triple))
			 (id triple)))
    triple))

(defun do-indexing (&optional (store *store*))
  (with-graph-transaction (store)
    (loop for triple = (sb-concurrency:dequeue (index-queue store)) do
	 (when (triple? triple)
	   (index-triple triple *store*))
	 (when (sb-concurrency:queue-empty-p (index-queue store))
	   (return)))))

(defun enqueue-triple-for-indexing (triple)
  (add-to-index-queue triple))

(defun lookup-triple (subject predicate object graph &key retrieve-deleted? 
		      already-locked?)
  (multiple-value-bind (subject predicate object graph) 
      (intern-spog subject predicate object graph)
    (flet ((lookup (s p o g)
	     (let ((cursor (get-from-index (main-idx *store*) :gspoi-idx g s p o)))
	       (if (vg-uuid:uuid? (cursor-value cursor))
		   (let ((triple (cursor-value
				  (get-from-index (main-idx *store*) 
						  :id-idx 
						  (cursor-value cursor)))))
		     (when (triple? triple)
		       (if (deleted? triple) 
			   (when retrieve-deleted? 
			     triple)
			   triple)))))))
      (if (or *read-uncommitted* already-locked?)
	  (lookup subject predicate object graph)
	  (with-graph-transaction (*store*)
	    (enqueue-lock (list subject predicate object graph)
			  (lock-pattern subject predicate object graph :kind :read) 
			  :read)
	    (lookup subject predicate object graph))))))

(defun add-triple (subject predicate object &key (graph *graph*) (index-immediate? t) 
		   cf (persistent? t))
  (multiple-value-bind (subject predicate object graph) 
      (intern-spog subject predicate object graph)
    (with-graph-transaction (*store*)
      (let ((lock (lock-pattern subject predicate object graph :kind :write)))
	(enqueue-lock (list subject predicate object graph) lock :write)
	(or
	 (let ((triple (lookup-triple subject predicate object graph 
				      :retrieve-deleted? t
				      :already-locked? t)))
	   (when (triple? triple)
	     (when cf
	       (set-triple-cf triple cf))
	     (when (deleted? triple)
	       (undelete-triple triple :persistent? persistent?))
	     triple))
	 (let ((id (vg-uuid:make-v1-uuid)))  
	   (let ((triple (make-triple :subject subject
				      :predicate predicate
				      :object object 
				      :graph graph
				      :cf (or cf +cf-true+)
				      :persistent? persistent?
				      :id id)))
	     (when persistent?
	       (push (list :add-triple subject predicate object graph id nil 
			   (cf triple))
		     (tx-queue *current-transaction*)))
	     (push (lambda () (delete-from-index (main-idx *store*) triple :id-idx id))
		   (tx-rollback *current-transaction*))
	     (add-to-index (main-idx *store*) triple :id-idx id)
	     (if index-immediate?
		 (index-triple triple *store*)
		 (enqueue-triple-for-indexing triple))
	     triple)))))))

(defun get-triple-by-id (id &optional (store *store*))
  (cursor-value (get-from-index (main-idx store) :id-idx id)))

(defun list-triples (&optional (store *store*))
  (let ((triples nil))
    (with-locked-index ((main-idx store))
      (maphash #'(lambda (id triple)
		   (declare (ignore id))
		   (when (not (deleted? triple)) (push triple triples)))
	       (gethash :id-idx (index-table (main-idx store)))))
    triples))

(defun triple-count (&optional (store *store*))
  (let ((triple-count 0))
    (with-locked-index ((main-idx store))
      (maphash #'(lambda (id triple)
                   ;; (declare (ignorable id))
		   (when (not (deleted? triple)) (incf triple-count)))
	       (gethash :id-idx (index-table (main-idx store)))))
    triple-count))

(defun get-triples (&key s p o (g *graph*) (store *store*))
  "Returns a cursor to the results."
  (flet ((get-them ()
	   (multiple-value-bind (s p o g) (intern-spog s p o g)
	     (cond ((and g s p o)
		    (if (consp o)
			(get-index-range (text-idx store) 
					 (make-text-idx-key g s p (nth 0 o))
					 (make-text-idx-key g s p (nth 1 o)))
			(get-from-index (main-idx store) :gspoi-idx g s p o)))
		   ((and g p s)
		    (get-from-index (main-idx store) :gspoi-idx g s p))
		   ((and g p o)
		    (get-from-index (main-idx store) :gposi-idx g p o))
		   ((and g p)
		    (get-from-index (main-idx store) :gposi-idx g p))
		   ((and g s)
		    (get-from-index (main-idx store) :gspoi-idx g s))
		   ((and g o)
		    (get-from-index (main-idx store) :gospi-idx g o))
		   (g
		    (get-from-index (main-idx store) :gospi-idx g))
		   (s
		    (get-from-index (main-idx store) :spogi-idx s))
		   (o
		    (get-from-index (main-idx store) :ospgi-idx o))
		   (p
		    (get-from-index (main-idx store) :posgi-idx p))
		   ((and (null s) (null p) (null o) (null g))
		    (get-from-index (main-idx store) :gspoi-idx))
		   (t 
		    (error "Other combinations of spogi to be implemented later."))))))
    (if *read-uncommitted*
	(get-them)
	(with-locked-pattern (s p o g :read)
	  (get-them)))))

(defun get-triples-list (&key s p o (g *graph*) (store *store*) retrieve-deleted? 
			 limit)
  (let ((triples (map 'list 
		      #'get-triple-by-id 
		      (index-cursor-vector 
		       (get-triples :s s :p p :o o :g g :store store)))))
    (if retrieve-deleted?
	(if limit
	    (subseq triples 0 (if (> (length triples) limit) limit))
	    triples)
	(if limit
	    (let ((triples (remove-if #'deleted? triples)))
	      (subseq triples 0 (if (> (length triples) limit) limit)))
	    (remove-if #'deleted? triples)))))

(defun clear-graph (&optional (name *graph*))
  (with-graph-transaction (*store*)
    (map-cursor #'(lambda (id)
		    (delete-triple (get-triple-by-id id)))
		(get-from-index (main-idx *store*) :gspoi-idx name))))

(defun %set-triple-cf (id cf)
  (let ((triple (get-triple-by-id (if (vg-uuid:uuid? id) 
				      id 
				      (uuid:make-uuid-from-string id)))))
    (when (triple? triple)
      (cas (triple-cf triple) (triple-cf triple) cf))))

(defun %undelete-triple (id)
  (let ((triple (get-triple-by-id (if (vg-uuid:uuid? id) 
				      id 
				      (uuid:make-uuid-from-string id)))))
    (when (triple? triple)
      (cas (triple-deleted? triple) (triple-deleted? triple) nil))))

(defun %delete-triple (id timestamp)
  (let ((triple (get-triple-by-id (if (vg-uuid:uuid? id) 
				      id 
				      (uuid:make-uuid-from-string id)))))
    (when (triple? triple)
      (cas (triple-deleted? triple) (triple-deleted? triple) timestamp))))

(defun %index-triple (triple &optional (store *store*))
  (add-to-index (main-idx store) triple :id-idx (id triple))
  (add-to-index (main-idx store) (id triple) :gspoi-idx
		(triple-graph triple) (triple-subject triple) 
		(triple-predicate triple) (triple-object triple))
  (add-to-index (main-idx store) (id triple) :spogi-idx
		(triple-subject triple) (triple-predicate triple) 
		(triple-object triple) (triple-graph triple))
  (add-to-index (main-idx store) (id triple) :posgi-idx
		(triple-predicate triple) (triple-object triple) 
		(triple-subject triple) (triple-graph triple))
  (add-to-index (main-idx store) (id triple) :ospgi-idx
		(triple-object triple) (triple-subject triple) 
		(triple-predicate triple) (triple-graph triple))
  (add-to-index (main-idx store) (id triple) :gposi-idx
		(triple-graph triple) (triple-predicate triple) 
		(triple-object triple) (triple-subject triple))
  (add-to-index (main-idx store) (id triple) :gospi-idx
		(triple-graph triple) (triple-object triple) 
		(triple-subject triple) (triple-predicate triple))
  (when (index-predicate? (predicate triple))
    (add-to-text-index (text-idx *store*)
		       (make-text-idx-key (graph triple) (subject triple) 
					  (predicate triple) (object triple))
		       (id triple)))
  triple)

(defun %add-triple (subject predicate object id graph cf deleted?)
  (let ((triple (make-triple :subject subject
			     :predicate predicate
			     :object object 
			     :graph graph
			     :cf cf
			     :id id
			     :persistent? t
			     :deleted? deleted?)))
    (%index-triple triple)
    triple))
  
(defun dump-triples (file &optional (store *store*))
  (with-open-file (stream file 
			  :direction :output 
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (with-graph-transaction (store)
      (maphash #'(lambda (id triple)
		   (when (persistent? triple)
		     (write `(,(subject triple)
			       ,(predicate triple)
			       ,(object triple)
			       ,(format nil "~A" id)
			       ,(graph triple)
			       ,(cf triple)
			       ,(deleted? triple))
			    :stream stream :pretty nil)
		     (format stream "~%")))
	       (gethash :id-idx (index-table (main-idx store)))))))

(defun load-triples (file)
  (with-open-file (stream file)
    (let ((count 0))
      (handler-case
	  (loop
	     (let ((triple (read stream)))
	       (%add-triple (nth 0 triple)
			    (nth 1 triple)
			    (nth 2 triple)
			    (uuid:make-uuid-from-string (nth 3 triple))
			    (nth 4 triple)
			    (nth 5 triple)
			    (nth 6 triple))
	       (incf count)))
	(end-of-file (condition)
	  (declare (ignore condition))
	  (do-indexing)
	  (format t "Loaded ~A triples~%" count))
	(error (condition)
	  (format t "Error loading triples: ~A / ~A~%" 
		  (type-of condition) condition))))))

