;;;; This is Kevin Raison's customization of Mr. Norvig's PAIP Prolog.  
;;;; Thanks Mr. Norvig!
;;;; Copyright (c) 1991 Peter Norvig, (c) 2010 Kevin Raison
(in-package #:vivace-graph-v2)

(defun trace-prolog () 
  (setq *prolog-trace* t))

(defun untrace-prolog () 
  (setq *prolog-trace* nil))

(defstruct (var (:constructor ? ())     ; var?
                (:print-function print-var)) 
  (name                                 ; var-name
   (incf *var-counter*))
  (binding                              ; var-binding
   +unbound+))

(defmacro var-deref (exp)
  "Follow pointers for bound variables."
  `(progn 
     (loop 
        while (and (var-p ,exp) (bound-p ,exp))
        do (setf ,exp (var-binding ,exp)))
     ,exp))

(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (var-deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

(defun bound-p (var) 
  (not (eq (var-binding var) +unbound+)))

(defun unify (x y)
  "Destructively unify two expressions."
  (cond ((vg-equal (var-deref x) (var-deref y)) t)
        ((var-p x) (set-binding x y))
        ((var-p y) (set-binding y x))
        ((and (consp x) (consp y))
         (and (unify (first x) (first y))
              (unify (rest x) (rest y))))
        (t nil)))

(defun set-binding (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop 
     until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) +unbound+)))

(defmethod clause-head ((triple triple))
  (list (predicate triple) (subject triple) (object triple) (graph triple)))

(defmethod clause-head ((list list))
  (first list))

(defun prolog-compile-help (functor clauses)
  (let ((arity (relation-arity (clause-head (first clauses)))))
    (compile-functor functor arity (clauses-with-arity clauses #'= arity))
    (prolog-compile-help functor (clauses-with-arity clauses #'/= arity))))

(defmethod prolog-compile ((functor functor))
  (if (null (functor-clauses functor))
      (prolog-compile-null functor)
      (prolog-compile-help functor (functor-clauses functor))))

(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key #'(lambda (clause) (relation-arity (clause-head clause)))
            :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation" (rest x))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop 
     for i from 1 to arity
     collect (new-interned-symbol '?arg i)))

(defun make-functor-symbol (symbol arity)
  (new-interned-symbol symbol '/ arity))

(defun make-= (x y) `(= ,x ,y))

(defun compile-call (predicate arity args cont)
  "Compile a call to a prolog predicate."
  (let ((functor (make-functor-symbol predicate arity)))
    `(let ((func (or (get-functor-fn ',functor) 
		     (gethash ',functor *prolog-global-functors*))))
       (when *prolog-trace*
	 (format t "TRACE: ~A/~A~A~%" ',predicate ',arity ',args))
       (if (functionp func)
	   (funcall func ,@args ,cont)))))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (typecase name
    (string (get (intern (string-upcase name)) 'prolog-compiler-macro))
    (symbol (get name 'prolog-compiler-macro))
    (otherwise nil)))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         #'(lambda ,arglist .,body)))

(defun binding-val (binding)
  (cdr binding))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun variable-p (x)
  ;;(and (symbolp x) (not (eq x '??)) (equal (char (symbol-name x) 0) #\?)))
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
             (compile-arg (binding-val binding) bindings)
             arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'(lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop 
               for exp in (rest compiled-exps)
               collect '(undo-bindings old-trail)
               collect exp)))))

(defmacro with-undo-bindings (&body body)
  (if (length=1 body)
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
	 ,(first body)
	 ,@(loop 
              for exp in (rest body)
	      collect '(undo-bindings old-trail)
	      collect exp))))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun unbound-var-p (exp)
  (and (var-p exp) (not (bound-p exp))))

(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (remove '? (set-difference (variables-in exp)
					     parameters))))
    (if exp-vars
        `(let ,(mapcar #'(lambda (var) `(,var (?)))
                       exp-vars)
           ,exp)
        exp)))

(defun make-anonymous (exp &optional (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))
 
(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
     (multiple-value-bind (new-seen-once new-seen-more)
         (anon-vars-in (first tree) seen-once seen-more)
       (anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (variable-p tree)) (values seen-once seen-more))
    ((member tree seen-once)
     (values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
     (values seen-once seen-more))
    (t (values (cons tree seen-once) seen-more))))

(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
    ;; Unify constants and conses:                       ; Case
    ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
     (values (vg-equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (multiple-value-bind (code1 bindings1)
         (compile-unify (first x) (first y) bindings)
       (multiple-value-bind (code2 bindings2)
           (compile-unify (rest x) (rest y) bindings1)
         (values (compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((variable-p x) (compile-unify-variable x y bindings))
    (t              (compile-unify-variable y x bindings))))

(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings +no-bindings+)
	    nil
	    bindings)))

(defun compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                  ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))       ; 12
      ((not (and (vg-equal x x1) (vg-equal y y1))) ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))        ; 11
      ((consp y1)                                          ; 7,10
       (values `(unify ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))    ; 4
           (values `(unify ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))     ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings))))))  ; 8,9

(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))

(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x)
  (declare (optimize (speed 3)))
  (cons x x))

(def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
            (compile-unify (first args) (second args) bindings)
          (compile-if
            code1
            (compile-body body cont bindings1))))))

(def-prolog-compiler-macro true (goal body cont bindings)
  (declare (ignore goal))
  (compile-body body cont bindings))

(def-prolog-compiler-macro fail (goal body cont bindings)
  (declare (ignore goal body cont bindings))
  nil)

(def-prolog-compiler-macro and (goal body cont bindings)
  (compile-body (append (args goal) body) cont bindings))

(def-prolog-compiler-macro or (goal body cont bindings)
  (let ((disjuncts (args goal)))
    (case (length disjuncts)
      (0 +fail+)
      (1 (compile-body (cons (first disjuncts) body) cont bindings))
      (t (let ((fn (gensym "F")))
	   `(flet ((,fn () ,(compile-body body cont bindings)))
	      .,(maybe-add-undo-bindings
		 (loop for g in disjuncts collect
		      (compile-body (list g) `#',fn bindings)))))))))

(defmethod clause-body ((triple triple))
  nil)

(defmethod clause-body ((list list))
  (rest list))

(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (let ((body
	 (bind-unbound-vars       
	  parms
	  (compile-body
	   (nconc
	    (mapcar #'make-= parms (args (clause-head clause)))
	    (clause-body clause))
	   cont
	   (mapcar #'self-cons parms)))))
    (when *prolog-trace* 
      (format t "TRACE: ~A BODY:~% ~A~%" (clause-head clause) body))
    body))

(defun add-clause (clause)
  "Add a user-defined functor"
  (let* ((functor-name (first (clause-head clause))))
    (when *prolog-trace* (format t "TRACE:  Adding clause ~A~%" clause))
    (assert (and (atom functor-name) (not (variable-p functor-name))))
    (let* ((arity (relation-arity (clause-head clause)))
	   (functor (make-functor-symbol functor-name arity)))
      (if (gethash functor *prolog-global-functors*)
	  (error 'prolog-error 
		 :reason 
		 (format nil "Cannot override default functor ~A." functor))
	  (let ((f (lookup-functor functor)))
	    (if (functor? f)
		(add-functor-clause f clause)
		(make-functor :name functor :clauses (list clause))))))))

;(defun deref-copy (exp)
;  (sublis (mapcar #'(lambda (var) (cons (var-deref var) (?)))
;		  (unique-find-anywhere-if #'var-p exp))
;	  exp))

(defun deref-copy (exp)
  (let ((var-alist nil))
    (labels ((walk (exp)
	       (deref-exp exp)
	       (cond ((consp exp)
		      (reuse-cons (walk (first exp))
				  (walk (rest exp))
				  exp))
		     ((var-p exp)
		      (let ((entry (assoc exp var-alist)))
			(if (not (null entry))
			    (cdr entry)
			    (let ((var-copy (?)))
			      (push (cons exp var-copy) var-alist)
			      var-copy))))
		     (t exp))))
      (walk exp))))

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (var-deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))

(defun deref-equal (x y)
  (or (vg-equal (var-deref x) (var-deref y))
      (and (consp x)
	   (consp y)
	   (deref-equal (first x) (first y))
	   (deref-equal (rest x) (rest y)))))

(defmethod prolog-compile-null ((functor functor))
  (let ((*functor* (functor-name functor)))
    (set-functor-fn *functor*
		    #'(lambda (&rest args) (declare (ignore args)) nil))))

(defun compile-functor (functor arity clauses)
  "Compile all the clauses for a given symbol/arity into a single LISP function."
  (let ((*functor* (functor-name functor)) 
	(parameters (make-parameters arity)))
    (let ((func `#'(lambda (,@parameters cont)
		     (block ,*functor*
		       .,(maybe-add-undo-bindings
			  (mapcar #'(lambda (clause)
				      (compile-clause parameters clause 'cont))
				  clauses))))))
      (when *prolog-trace* (format t "TRACE: Adding ~A to ~A~%" func *functor*))
      (set-functor-fn *functor* (eval func)))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((or (eq (first body) '!) (eq (first body) 'cut) (equalp (first body) "cut"))
     `(progn ,(compile-body (rest body) cont bindings)
             (return-from ,*functor* nil)))
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro 
			     (funcall macro goal (rest body) cont bindings))))
	 (if (and macro (not (eq macro-val :pass)))
	     macro-val
	     (compile-call (predicate goal) (relation-arity goal)
			   (mapcar #'(lambda (arg)
				       (compile-arg arg bindings))
				   (args goal))
			   (if (null (rest body))
			       cont
			       `#'(lambda ()
				    ,(compile-body 
				      (rest body) cont
				      (bind-new-variables bindings goal))))))))))

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (intern (symbol-name (gensym "?"))))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(defmacro <- (&rest clause)
  "Add a user-defined functor, or add clauses to an existing functor."
  `(let ((*functor* nil)) (add-clause ',(make-anonymous clause))))

(defmacro insert (&rest triples)
  "Add triples to the data base. Wraps all additions in a single transaction."
  `(let ((count 0))
     (with-graph-transaction (*store*)
       (dolist (triple ',triples)
	 (add-triple (first triple) (second triple) (third triple) 
		     :graph (or (fourth triple) *graph*))
	 (incf count))
       (do-indexing))
     count))

(defun prolog-ignore (&rest args)
  (declare (ignore args))
  nil)

(defmacro ?- (&rest goals)
  "Execute an interactive prolog query."
  (let* ((goals (replace-?-vars goals))
	 (vars (delete '? (variables-in goals)))
	 (top-level-query (gensym "PROVE"))
	 (*functor* (make-functor-symbol top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*functor* ',*functor*)
	    (functor (make-functor :name *functor* :clauses nil)))
       (unwind-protect
	    (catch 'top-level-prove
	      (let ((func #'(lambda (cont) 
			      (handler-case
				  (block ,*functor*
				    .,(maybe-add-undo-bindings
				       (mapcar 
					#'(lambda (clause)
					    (compile-clause nil clause 'cont))
					`(((,top-level-query)
					   ,@goals
					   (show-prolog-vars 
					    ,(mapcar #'symbol-name vars)
					    ,vars))))))
				(undefined-function (condition)
				  (error 'prolog-error :reason condition))))))
		(set-functor-fn *functor* func)
		(funcall func #'prolog-ignore)
		;;(setf (gethash ',*functor* *user-functors*) func)
		;;(funcall (gethash ',*functor* *user-functors*) #'prolog-ignore)
		(format t "~&No.~%")))
	 ;;(remhash ',*functor* *user-functors*))
	 (delete-functor functor))
       (values))))

(defmacro select (vars &rest goals)
  "Select specific variables as a list of lists using the following form:
 (select (?x ?y) (is-a ?x ?y)) could return ((Joe Human) (Spot Dog)) and
 (select ((:entity ?x) (:species ?y)) could return 
 (((:entity Joe) (:species Human)) 
  ((:entity Spot)(:species Dog)))"
  (let* ((top-level-query (gensym "PROVE"))
	 (goals (replace-?-vars goals))
	 (*functor* (make-functor-symbol top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*functor* ',*functor*)
	    (*select-list* nil)
	    (functor (make-functor :name *functor* :clauses nil)))
       (unwind-protect
	    (let ((func 
		   #'(lambda (cont) 
		       (handler-case
			   (block ,*functor*
			     .,(maybe-add-undo-bindings
				(mapcar #'(lambda (clause)
					    (compile-clause nil clause 'cont))
					`(((,top-level-query)
					   ,@goals
					   (select 
					    ,(mapcar 
					      #'(lambda (var)
						  (typecase var
						    (symbol (symbol-name var))
						    (list (first var))))
					      vars) ,vars))))))
			 (undefined-function (condition)
			   (error 'prolog-error :reason condition))))))
	      (set-functor-fn *functor* func)
	      (funcall func #'prolog-ignore))
	 (delete-functor functor))
       (nreverse *select-list*))))

(defmacro select-flat (vars &rest goals)
  `(flatten (select ,vars ,@goals)))

(defmacro select-first (vars &rest goals)
  `(first (select ,vars ,@goals !)))

(defmacro select-one (vars &rest goals)
  `(first (flatten (select ,vars ,@goals !))))

(defmacro do-query (&rest goals)
  `(select () ,@goals))

(defmacro map-query (fn query &key collect?)
  "Maps fn over the results of query. collect? will return a list of the results
of each application of fn."
  (with-gensyms (result)
    (if collect?
	`(mapcar #'(lambda (,result)
		     (apply ,fn ,result))
		 ,query)
	`(dolist (,result ,query)
	   (apply ,fn ,result)))))

(defun valid-prolog-query? (form)
  (case (first form)
    (select t)
    (select-one t)
    (select-flat t)
    (select-first t)
    (<- t)
    ;; (insertt)
    (insert t)
    (otherwise nil)))
