(in-package #:vivace-graph-v2)

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

(defmethod persistent? ((triple triple))
  (triple-persistent? triple))

(defun index-predicate (name-string)
  (setf (gethash name-string (indexed-predicates *store*)) t))
	
(defun unindex-predicate (name-string)
  (setf (gethash name-string (indexed-predicates *store*)) nil))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun set-triple-cf (triple new-value)
  (cas (triple-cf triple) (triple-cf triple) new-value))

(defun make-anonymous-node ()
  "Create a unique anonymous node."
  (format nil "_anon:~A" (make-uuid)))

(let ((regex 
       "^_anon\:[0-9abcdefABCEDF]{8}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{12}$"))
  (defun anonymous? (node)
    (cl-ppcre:scan regex node)))

(defmethod reify (node)
  (declare (special node))
  (select (?p ?o)
	  (lisp ?s node)
	  (q- ?s ?p ?o)))

(defun reify-recursive (node &key (max-levels 2) (level 0))
  (unless (>= level max-levels)
    (let ((relations (reify node)))
      (list node
	    (mapcar #'(lambda (relation)
			(if (anonymous? (second relation))
			    (list relation 
				  (reify-recursive (second relation)
						   :max-levels max-levels
						   :level (1+ level)))
			    relation))
		    relations)))))

(defun make-text-idx-key (g s p o)
  (string-downcase (format nil "~A~A~A~A~A~A~A" g #\Nul s #\Nul p #\Nul o)))

(defun index-triple (triple &optional (store *store*))
  (prog1
      (setf (gethash (id triple) (id-idx store)) triple)
    (add-to-index (gspoi-idx store) (id triple) 
		  (graph triple) (subject triple) (predicate triple) (object triple))
    (add-to-index (spogi-idx store) (id triple) 
		  (subject triple) (predicate triple) (object triple) (graph triple))
    (add-to-index (posgi-idx store) (id triple) 
		  (predicate triple) (object triple) (subject triple) (graph triple))
    (add-to-index (ospgi-idx store) (id triple) 
		  (object triple) (subject triple) (predicate triple) (graph triple))
    (add-to-index (gposi-idx store) (id triple) 
		  (graph triple) (predicate triple) (object triple) (subject triple))
    (add-to-index (gospi-idx store) (id triple) 
		  (graph triple) (object triple) (subject triple) (predicate triple))
    (when (index-predicate? (predicate triple))
      (add-to-text-index (text-idx *store*)
			 (make-text-idx-key (graph triple) (subject triple) 
					    (predicate triple) (object triple))
			 (id triple)))))

(defun do-indexing (&optional (store *store*))
  (loop for triple = (sb-concurrency:dequeue (index-queue store)) do
       (if (not (triple? triple))
	   (return)
	   (with-graph-transaction (*store*)
	     (index-triple triple *store*)))))

(defun enqueue-triple-for-indexing (triple)
  (add-to-index-queue triple))

(defun undelete-triple (triple &key (persistent? t))
  (if persistent?
      (with-graph-transaction (*store*)
	(push (list :undelete-triple triple) (tx-queue *current-transaction*))
	(cas (triple-deleted? triple) (triple-deleted? triple) nil))
      (cas (triple-deleted? triple) (triple-deleted? triple) nil))
  triple)

(defun delete-triple (triple &key (persistent? t))
  (if persistent?
      (with-graph-transaction (*store*)
	(push (list :delete-triple triple) (tx-queue *current-transaction*))
	(cas (triple-deleted? triple) nil (gettimeofday)))
      (cas (triple-deleted? triple) nil (gettimeofday))))
  ;;(add-to-delete-queue triple)))

(defun lookup-triple (subject predicate object graph &key retrieve-deleted?)
  (multiple-value-bind (subject predicate object graph) 
      (intern-spog subject predicate object graph)
    (let ((cursor (get-from-index (gspoi-idx *store*) graph subject predicate object)))
      (if (cursor-value cursor)
	  (let ((triple (gethash (cursor-value cursor) (id-idx *store*))))
	    (when (triple? triple)
	      (if (deleted? triple) 
		  (when retrieve-deleted? 
		    triple)
		  triple)))))))

(defpackage #:graph-words)

(defun intern-spog (s p o g)
  (values 
   (if (stringp s) (intern s :graph-words) s)
   (if (stringp p) (intern p :graph-words) p)
   (if (stringp o) (intern o :graph-words) o)
   (if (stringp g) (intern g :graph-words) g)))

(defun add-triple (subject predicate object &key (graph *graph*) (index-immediate? t) 
		   cf (persistent? t))
  (multiple-value-bind (subject predicate object graph) 
      (intern-spog subject predicate object graph)
    (with-locked-index ((gspoi-idx *store*) graph subject predicate object)
      (or
       (let ((triple 
	      (lookup-triple subject predicate object graph :retrieve-deleted? t)))
	 (when (triple? triple)
	   (when (or cf (deleted? triple))
	     (with-graph-transaction (*store*)
	       (when cf
		 (when persistent?
		   (push (list :set-cf triple) (tx-queue *current-transaction*)))
		 (set-triple-cf triple cf))
	       (if (deleted? triple)
		   (undelete-triple triple :persistent? persistent?)
		   triple)))))
       (let ((id (uuid:make-v1-uuid)))  
	 (let ((triple (make-triple :subject subject
				    :predicate predicate
				    :object object 
				    :graph graph
				    :cf (or cf +cf-true+)
				    :persistent? persistent?
				    :id id)))
	   (with-graph-transaction (*store*)
	     (push (list :add-triple triple) (tx-queue *current-transaction*))
	     (add-to-index (gspoi-idx *store*) (id triple) 
			   (graph triple) (subject triple) 
			   (predicate triple) (object triple))
	     (if index-immediate?
		 (index-triple triple *store*)
		 (enqueue-triple-for-indexing triple))
	     triple)))))))

(defun get-triple-by-id (id &optional (store *store*))
  (gethash id (id-idx store)))

(defun get-triples (&key s p o (g *graph*) (store *store*))
  "Returns a cursor to the results."
  (multiple-value-bind (s p o g) (intern-spog s p o g)
    (cond ((and g s p o)
	   (if (consp o)
	       (get-index-range (text-idx store) 
				(make-text-idx-key g s p (nth 0 o))
				(make-text-idx-key g s p (nth 1 o)))
	       (get-from-index (gspoi-idx store) g s p o)))
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
	  (g
	   (get-from-index (gospi-idx store) g))
	  (s
	   (get-from-index (spogi-idx store) s))
	  (o
	   (get-from-index (ospgi-idx store) o))
	  (p
	   (get-from-index (posgi-idx store) p))
	  (t 
	   (error "Other combinations of spogi to be implemented later.")))))

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
		(get-from-index (gspoi-idx *store*) name))))

(defun %set-triple-cf (id cf)
  (let ((triple (get-triple-by-id (if (uuid:uuid? id) 
				      id 
				      (uuid:make-uuid-from-string id)))))
    (when (triple? triple)
      (cas (triple-cf triple) (triple-cf triple) cf))))

(defun %undelete-triple (id)
  (let ((triple (get-triple-by-id (if (uuid:uuid? id) 
				      id 
				      (uuid:make-uuid-from-string id)))))
    (when (triple? triple)
      (cas (triple-deleted? triple) (triple-deleted? triple) nil))))

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
			     :persistent? t
			     :deleted? deleted?)))
    (setf (gethash (id triple) (id-idx *store*)) triple)
    (enqueue-triple-for-indexing triple)
    triple))
  
(defun dump-triples (file &optional (store *store*))
  (with-open-file (stream file 
			  :direction :output 
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (sb-ext:with-locked-hash-table ((id-idx store))
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
	       (id-idx store)))))

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

#|
(defmethod deserialize-help ((become (eql +triple+)) bytes)
  "Decode a triple."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (declare (type integer become))
  (destructuring-bind (subject predicate object belief id timestamp derived? deleted?)
      (extract-all-subseqs bytes)
    (declare (type (simple-array (unsigned-byte 8))
                   subject predicate object belief id timestamp derived? deleted?))
    (make-triple
     :uuid (deserialize id)
     :belief-factor (deserialize belief)
     :derived? (deserialize derived?)
     :subject (lookup-node subject *graph* t)
     :predicate (lookup-predicate predicate *graph*)
     :deleted? (deserialize deleted?)
     :timestamp (deserialize timestamp)
     :object (lookup-node object *graph* t))))

(defmethod serialize ((triple triple))
  "Encode a triple for storage."
  (serialize-multiple +triple+
                      (node-value (triple-subject triple))
                      (make-serialized-key (triple-predicate triple))
                      (node-value (triple-object triple))
                      (triple-belief-factor triple)
                      (triple-uuid triple)
                      (triple-timestamp triple)
                      (triple-derived? triple)
                      (triple-deleted? triple)))
|#
