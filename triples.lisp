(in-package #:vivace-graph-v2)

(defparameter *print-triple-details* nil)

(defun print-triple (triple stream depth)
  (declare (ignore depth))
  (if *print-triple-details* 
      (format stream "<'~A' '~A' '~A' {~F:~A:~A}>" 
	      (subject triple) (predicate triple) (object triple) 
	      (cf triple) (graph triple) (id triple))
      (format stream "<'~A' '~A' '~A'>" 
	      (subject triple) (predicate triple) (object triple))))

(defstruct (triple
	     (:print-function print-triple)
	     (:conc-name triple-)
	     (:predicate triple?))
  subject predicate object graph id (deleted? nil) (cf +cf-true+) (persistent? t))

(defgeneric triple-equal (t1 t2)
  (:method ((t1 triple) (t2 triple)) (uuid:uuid-eql (id t1) (id t2)))
  (:method (t1 t2) nil))

(defmethod deleted? ((triple triple))
  (triple-deleted? triple))

(defmethod subject ((triple triple))
  (triple-subject triple))

(defmethod subject ((list list))
  (second list))

(defmethod predicate ((triple triple))
  (triple-predicate triple))

(defmethod predicate ((list list))
  (first list))

(defmethod object ((triple triple))
  (triple-object triple))

(defmethod object ((list list))
  (third list))

(defmethod graph ((triple triple))
  (triple-graph triple))

(defmethod graph ((list list))
  (fourth list))

(defmethod id ((triple triple))
  (triple-id triple))

(defmethod id ((list list))
  (fifth list))

(defmethod cf ((triple triple))
  (triple-cf triple))

(defmethod belief-factor ((triple triple))
  (triple-cf triple))

(defun index-predicate (name-string)
  (setf (gethash name-string (indexed-predicates *store*)) t))
	
(defun unindex-predicate (name-string)
  (setf (gethash name-string (indexed-predicates *store*)) nil))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun make-anonymous-node ()
  "Create a unique anonymous node."
  (format nil "_anon:~A" (make-uuid)))

(defun lookup-triple (subject predicate object graph &key retrieve-deleted?)
  (let ((cursor (get-from-index (gspoi-idx *store*) graph subject predicate object)))
    (if (cursor-value cursor)
	(let ((triple (gethash (cursor-value cursor) (id-idx *store*))))
	  (when (triple? triple)
	    (if (deleted? triple) 
		(when retrieve-deleted? 
		  triple)
		triple))))))

(defun add-to-text-index (triple)
  (skip-list-add (text-idx *store*) (object triple) (id triple)))

(defun index-triple (triple &optional (store *store*))
  (prog1
      (setf (gethash (id triple) (id-idx store)) triple)
    (add-to-index (spogi-idx store) (id triple) 
		  (subject triple) (predicate triple) (object triple) (graph triple))
    (add-to-index (posgi-idx store) (id triple) 
		  (predicate triple) (object triple) (subject triple) (graph triple))
    (add-to-index (ospgi-idx store) (id triple) 
		  (object triple) (subject triple) (predicate triple) (graph triple))
    (add-to-index (gspoi-idx store) (id triple) 
		  (graph triple) (subject triple) (predicate triple) (object triple))
    (add-to-index (gposi-idx store) (id triple) 
		  (graph triple) (predicate triple) (object triple) (subject triple))
    (add-to-index (gospi-idx store) (id triple) 
		  (graph triple) (object triple) (subject triple) (predicate triple))
    (when (index-predicate? (predicate triple))
      (add-to-text-index triple))))

(defun do-indexing (&optional (store *store*))
  (loop for triple = (sb-concurrency:dequeue (index-queue store)) do
       (if (not (triple? triple))
	   (return)
	   (with-transaction (*store*)
	     (index-triple triple *store*)))))

(defun enqueue-triple-for-indexing (triple)
  (add-to-index-queue triple))

(defun undelete-triple (triple)
  (cas (triple-deleted? triple) (triple-deleted? triple) nil)
  triple)

(defun delete-triple (triple)
  (cas (triple-deleted? triple) nil (gettimeofday)))
  ;;(add-to-delete-queue triple)))

(defun add-triple (subject predicate object &key (graph *graph*) (index-immediate? t) 
		   cf)
  (or (let ((triple 
	     (lookup-triple subject predicate object graph :retrieve-deleted? t)))
	(when (triple? triple)
	  (when cf
	    (cas (triple-cf triple) (triple-cf triple) cf))
	  (if (deleted? triple)
	      (undelete-triple triple)
	      triple)))
      (let ((id (uuid:make-v1-uuid)))  
	(let ((triple (make-triple :subject subject
				   :predicate predicate
				   :object object 
				   :graph graph
				   :cf (or cf +cf-true+)
				   :id id)))
	  (if index-immediate?
	      (index-triple triple *store*)
	      (enqueue-triple-for-indexing triple))
	  triple))))

(defun get-triple-by-id (id &optional (store *store*))
  (gethash id (id-idx store)))

(defun get-triples (&key s p o (g *graph*) (store *store*))
  "This needs to be updated to a cursor-based retrieval engine when we have persistent
storage."
  (cond ((and g s p o)
	 (get-from-index (gspoi-idx store) g s p o))
	((and g p s)
	 (get-from-index (gspoi-idx store) g s p))
	((and g p o)
	 (get-from-index (gposi-idx store) g p o))
	((and g p)
	 (get-from-index (gposi-idx store) g p))
	((and g s)
	 (get-from-index (gspoi-idx store) g s))
	((and g o)
	 (get-from-index (gospi-idx store) g o))
	(t (error "Other combinations to be implemented later."))))

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
  (with-transaction (*store*)
    (map-cursor #'(lambda (id)
		    (delete-triple (get-triple-by-id id)))
		(get-from-index (gspoi-idx *store*) name))))

(defun %delete-triple (id timestamp)
  (let ((triple (get-triple-by-id (if (uuid:uuid? id) 
				      id 
				      (uuid:make-uuid-from-string id)))))
    (when (triple? triple)
      (cas (triple-deleted? triple) (triple-deleted? triple) timestamp))))

(defun %add-triple (subject predicate object id graph cf deleted?)
  (let ((triple (make-triple :subject subject
			     :predicate predicate
			     :object object 
			     :graph graph
			     :cf cf
			     :id id
			     :deleted? deleted?)))
    (enqueue-triple-for-indexing triple)
    triple))
  
(defun dump-triples (file &optional (store *store*))
  (with-open-file (stream file 
			  :direction :output 
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (maphash #'(lambda (id triple)
		 (write `(,(subject triple)
			   ,(predicate triple)
			   ,(object triple)
			   ,(format nil "~A" id)
			   ,(graph triple)
			   ,(cf triple)
			   ,(deleted? triple))
			:stream stream :pretty nil)
		 (format stream "~%"))
	     (id-idx store))))

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
