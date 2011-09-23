(in-package #:vivace-graph-v2)

(defun symbol-nstring-upcase (symbol-to-upcase)
  (declare (symbol symbol-to-upcase))
  #-:sbcl (assert (symbolp "string"))
  (nstring-upcase (string symbol-to-upcase)))

(defun ensure-internable (thing)
  (etypecase thing
    (symbol 
     (intern (symbol-nstring-upcase thing)))
    (string 
     (intern (string-upcase thing)))
    (number thing)
    (t 
     (error "THING must be of type cl:string, cl:symbol or cl:integer, not ~A" 
            (type-of thing)))))

#+sbcl
(defun quit () (sb-ext:quit))

(defmacro logger (level msg &rest args)
  "Syslogger"
  ;; `(osicat-posix:syslog (gethash ',level *syslog-priorities*) ,msg ,@args))
  `(funcall #'sb-posix:syslog (gethash ',level *syslog-priorities*) ,msg ,@args))

(defun ip-to-string (ip)
  (format nil "~A.~A.~A.~A" (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))

(defgeneric less-than (x y)
  (:documentation "Generic less-than operator.  Allows comparison of apples and oranges.")
  
  (:method ((x symbol) (y symbol))
    (string< (symbol-name x) (symbol-name y)))
  
  (:method ((x number) (y number))
    (< x y))
  
  (:method ((x string) (y string))
    (string< x y))

  (:method ((x symbol) (y string))
    (string< (symbol-name x) y))

  (:method ((x string) (y symbol))
    (string< x (symbol-name y)))

  (:method ((x symbol) (y number))
    (string< (symbol-name x) (write-to-string y)))
  
  (:method ((x number) (y symbol))
    (string< (write-to-string x) (symbol-name y)))
  
  (:method ((x number) (y string))
    (string< (write-to-string x) y))

  (:method ((x string) (y number))
    (string< x (write-to-string y)))

  (:method ((x timestamp) (y timestamp)) 
    (timestamp< x y))

  (:method ((x number) (y timestamp))
    (< (timestamp-to-universal x) y))
  
  (:method ((x timestamp) (y number))
    (< x (timestamp-to-universal y)))

  ;; :WAS 
  ;; (:method ((x uuid:uuid) (y uuid:uuid)) 
  ;;        (string< (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
  (:method ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier)) 
    (< (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x))
       (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))

  ;; :WAS 
  ;; (:method ((x symbol) (y uuid:uuid))
  ;;   (string< (symbol-name x) (uuid:print-bytes nil y)))
  ;; (:method ((x uuid:uuid) (y symbol)) 
  ;;   (string< (uuid:print-bytes nil x) (symbol-name y)))
  (:method ((x symbol) (y unicly:unique-universal-identifier))
    (string< (symbol-name x) (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t)))

  (:method ((x unicly:unique-universal-identifier) (y symbol)) 
    (string< (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t) (symbol-name y)))

  ;; :WAS (:method ((x string) (y uuid:uuid))
  ;;        (string< x (uuid:print-bytes nil y)))
  ;;       (:method ((x uuid:uuid) (y string)) 
  ;;         (string< (uuid:print-bytes nil x) y))
  (:method ((x string) (y unicly:unique-universal-identifier))
    (string< x (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char)))

  (:method ((x unicly:unique-universal-identifier) (y string))
    (string< (unicly:uuid-print-bytes-to-string x :string-or-char-type 'base-char) y))

  ;; :WAS 
  ;; (:method ((x number) (y uuid:uuid)) 
  ;;   (string< (write-to-string x) (uuid:print-bytes nil y)))
  ;; (:method ((x uuid:uuid) (y number)) 
  ;;   (string< (uuid:print-bytes nil x) (write-to-string y)))
  (:method ((x number) (y unicly:unique-universal-identifier))
    (<  x (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))
             
  (:method ((x unicly:unique-universal-identifier) (y number))
    (< (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x)) y))
  )

(defgeneric greater-than (x y)
  (:documentation "Generic greater-than operator.  Allows comparison of apples and oranges.")
  
  (:method ((x symbol) (y symbol))
    (string> (symbol-name x) (symbol-name y)))
  
  (:method ((x number) (y number))
    (> x y))

  (:method ((x timestamp) (y timestamp))
    (timestamp> x y))

  (:method ((x string) (y string))
    (string> x y))
  
  (:method ((x symbol) (y string))
    (string> (symbol-name x) y))
  
  (:method ((x string) (y symbol))
    (string> x (symbol-name y)))
  
  (:method ((x symbol) (y number))
    (string> (symbol-name x) (write-to-string y)))
  
  (:method ((x number) (y symbol)) 
    (string> (write-to-string x) (symbol-name y)))

  (:method ((x number) (y string))
    (string> (write-to-string x) y))

  (:method ((x string) (y number))
    (string> x (write-to-string y)))

  (:method ((x number) (y timestamp))
    (> (timestamp-to-universal x) y))
  
  (:method ((x timestamp) (y number))
    (> x (timestamp-to-universal y)))

  ;; :WAS
  ;; (:method ((x uuid:uuid) (y uuid:uuid)) 
  ;;   (string> (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
  (:method ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier)) 
    (> (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x))
       (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))
  
  ;; :WAS
  ;; (:method ((x string) (y uuid:uuid)) 
  ;;   (string> x (uuid:print-bytes nil y)))
  ;; (:method ((x uuid:uuid) (y string)) 
  ;;   (string> (uuid:print-bytes nil x) y))

  (:method ((x string) (y unicly:unique-universal-identifier))
    (string> x (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char)))

  (:method ((x unicly:unique-universal-identifier) (y string))
    (string> (unicly:uuid-print-bytes-to-string x :string-or-char-type 'base-char) y))

  ;; :WAS
  ;; (:method ((x uuid:uuid) (y symbol)) 
  ;;   (string> (uuid:print-bytes nil x) (symbol-name y)))
  ;; (:method ((x symbol) (y uuid:uuid))
  ;;   (string> (symbol-name x) (uuid:print-bytes nil y)))
  (:method ((x symbol) (y unicly:unique-universal-identifier))
    (string> (symbol-name x) (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t)))

  (:method ((x unicly:unique-universal-identifier) (y symbol)) 
    (string> (unicly:uuid-print-bytes-to-string y :string-or-char-type 'base-char :upcase t) (symbol-name y)))

  ;; :WAS
  ;; (:method ((x number) (y uuid:uuid))
  ;;   (string> (write-to-string x) (uuid:print-bytes nil y)))           
  ;; (:method ((x uuid:uuid) (y number)) 
  ;;   (string> (uuid:print-bytes nil x) (write-to-string y)))
  (:method ((x number) (y unicly:unique-universal-identifier))
    (> x (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector y))))
             
  (:method ((x unicly:unique-universal-identifier) (y number))
    (> (unicly::uuid-bit-vector-to-integer (unicly:uuid-to-bit-vector x)) y))
  )
             

(defun uri? (string)
  (cl-ppcre:scan "^(https?|ftp)\:\/\/[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(\/.*)?$" string))

(defun make-slot-key (id slot)
  (format nil "~A~A~A" id #\Nul slot))

;; String split without regexes.
(defun split (string &optional (ws '(#\Space #\Tab)) max)
  "Split STRING along whitespace as defined by the sequence WS.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless MAX is provided, in which case the
string will be split into MAX tokens at most, the last one
containing the whole rest of the given STRING, if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
  (when (and max (>= words (1- max)))
    (return (cons (subseq string start) list)))
  (setf end (position-if #'is-ws string :start start))
  (push (subseq string start end) list)
  (incf words)
  (unless end (return list))
  (setf start (1+ end)))))))

(defun print-hash (ht)
  "Dump the k-v pairs of a hash table to stdout."
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht))

;; Plists
(defun get-prop (plist prop)
  "Return the value of a property in a property list."
  (cond ((null plist) nil)
	((eql (car plist) prop)
	 (cadr plist))
	(t (get-prop (cddr plist) prop))))

;; Norvig utilities
(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-p))))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun new-interned-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "return a list of leaves of tree satisfying predicate, with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;; Borrowed from On Lisp by Graham
(defmacro while (test &rest body)
  `(loop until (not ,test) do
	,@body))

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))


;; The following queueing code was borrowed and adapted from Russell & Norvig's
;; "Introduction to AI"
(defun print-queue (q stream depth)
  (declare (ignore depth))
  (format stream "<QUEUE: ~a>" (queue-elements q)))

(defstruct (queue
             (:print-function print-queue))
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue? (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  (elt (queue-elements q) 0))

(defun dequeue (q)
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue-front (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-elements q) (nconc items (queue-elements q))
	       (queue-last q) (last (queue-elements q))))
        (t (setf (queue-elements q) (nconc items (queue-elements q))))))

(defun enqueue (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaphora
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))


(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro aprog1 (valform &body body)
  `(let ((it ,valform)) ,@body it))

(defmacro awhen-prog1 (test-form &body body)
  "A combination of AWHEN and PROG1; always returns the result of TEST-FORM."
  `(awhen ,test-form (prog1 it ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro anaphoric (op test &body body)
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro acond (&body clauses)
  "Like COND, except result of each test-form is bound to IT (via LET) for the
scope of the corresponding clause."
  (labels ((rec (clauses)
             (if clauses
                 (destructuring-bind ((test &body body) . rest)  clauses
                   (if body
                       `(anaphoric if ,test (progn ,@body) ,(rec rest))
                       `(anaphoric if ,test it ,(rec rest))))
                 nil)))
    (rec clauses)))

;; (defmacro aconsf (place key value &environment env)
;;   "CONS is to PUSH as ACONS is to ACONSF; it pushes (cons KEY VALUE) to the PLACE."
;;   (multiple-value-bind (temps vals stores set-value get-value)
;;       (get-setf-expansion place env)
;;     (unless (null (cdr stores))
;;       (error "ACONSF can't store to this form: ~:_~S" place))
;;     (once-only (key value)
;;       `(let* (,@(mapcar 'list temps vals)
;;               (,(car stores)
;;                (acons ,key ,value ,get-value)))
;;          ,set-value
;;          ,value))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alexandria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype string-designator ()
  "A string designator is either a string, a symbol, or a character."
  `(or symbol string character))

(defmacro fun (&body body)
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

;; ;;; this variant is from sheeple, which in-turn, was adapted from Alexandria
;; (defmacro with-gensyms (names &body forms)
;;   "Binds each variable named by a symbol in NAMES to a unique symbol around
;; FORMS. Each of NAMES must either be either a symbol, or of the form:

;;  (symbol string-designator)

;; Bare symbols appearing in NAMES are equivalent to:

;;  (symbol symbol)

;; The string-designator is used as the argument to GENSYM when constructing the
;; unique symbol the named variable will be bound to."
;;   `(let ,(mapcar (fun (multiple-value-bind (symbol string)
;;                           (etypecase _
;;                             (symbol
;;                              (values _ (symbol-name _)))
;;                             ((cons symbol (cons string-designator null))
;;                              (values (car _) (string (cadr _)))))
;;                         `(,symbol (gensym ,string))))
;;                  names)
;;      ,@forms))

;; ;;; this variant is from sheeple, which in-turn, was heavily adapted from Alexandria
;; (defmacro once-only (specs &body forms)
;;   "Each SPEC must be either a NAME, or a (NAME INITFORM), with plain
;; NAME using the named variable as initform.

;; Evaluates FORMS with names rebound to temporary variables, ensuring
;; that each is evaluated only once.

;; Example:
;;   (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
;;   (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
;;   (let ((gensyms (mapcar (fun (gensym "ONCE-ONLY")) specs))
;;         (real-specs (mapcar (fun (etypecase _
;;                                    (list (cons (car _) (cadr _)))
;;                                    (symbol (cons _ _))))
;;                             specs)))
;;     (flet ((mapcar-gars (thunction) (mapcar thunction gensyms real-specs)))
;;       `(let ,(mapcar-gars (lambda (g n) `(,g (gensym ,(string (car n))))))
;;          `(let (,,@(mapcar-gars (lambda (g n) ``(,,g ,,(cdr n)))))
;;             ,(let ,(mapcar-gars (lambda (g n) `(,(car n) ,g)))
;;                   ,@forms))))))

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(declaim (inline maphash-keys))
(defun maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(declaim (inline maphash-values))
(defun maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defun make-gensym-list (length &optional (x "G"))
  "Returns a list of LENGTH gensyms, each generated as if with a call to MAKE-GENSYM,
using the second (optional, defaulting to \"G\") argument."
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop repeat length
      collect (gensym g))))

(declaim (inline ensure-function))	; to propagate return type.

(declaim (ftype (function (t) (values function &optional))
           ensure-function))

(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(defun ensure-list (thing)
  (typecase thing
    (list thing)
    (vector (coerce thing 'list))
    (t    (list thing))))


(define-modify-macro ensure-functionf/1 () ensure-function)

(defmacro ensure-functionf (&rest places)
  `(progn ,@(mapcar (lambda (x) `(ensure-functionf/1 ,x)) places)))

(defun disjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning the primary value of the first
predicate that returns true, without calling the remaining predicates.
If none of the predicates returns true, NIL is returned."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate))
	(more-predicates (mapcar #'ensure-function more-predicates)))
    (lambda (&rest arguments)
      (or (apply predicate arguments)
	  (some (lambda (p)
		  (declare (type function p))
		  (apply p arguments))
		more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning NIL if any of the predicates
returns false, without calling the remaining predicates. If none of the
predicates returns false, returns the primary value of the last predicate."
  (lambda (&rest arguments)
    (and (apply predicate arguments)
	 ;; Cannot simply use CL:EVERY because we want to return the
	 ;; non-NIL value of the last predicate if all succeed.
         (do ((tail (cdr more-predicates) (cdr tail))
              (head (car more-predicates) (car tail)))
             ((not tail)
              (apply head arguments))
           (unless (apply head arguments)
             (return nil))))))


(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(funcall f (apply g arguments)))))
          more-functions
          :initial-value function))


(defun multiple-value-compose (function &rest more-functions)
    "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(multiple-value-call f (apply g arguments)))))
          more-functions
          :initial-value function))


(defun curry (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      ;; Using M-V-C we don't need to append the arguments.
      (multiple-value-call fn (values-list arguments) (values-list more)))))


(defun rcurry (function &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn (values-list more) (values-list arguments)))))


(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

(defmacro collect (collections &body body)
  (let (macros binds)
    (dolist (spec collections)
      (destructuring-bind (name &optional default (kind 'collect))
          (ensure-list spec)
        (let ((value (gensym (format nil "~A-VALUE-" name))))
          (push (if (null default) value `(,value ,default)) binds)
          (macrolet ((collect-macro (fun-form)
                       `(push `(,name (&rest forms)
                                      `(progn ,@(mapcar (fun ,,fun-form) forms) ,',value))
                              macros)))
            (if (eq kind 'collect)
                (let ((tail (gensym (format nil "~A-TAIL-" name))))
                  (if (null default) (push tail binds)
                      (push `(,tail (last ,value)) binds))
                  (collect-macro `(with-gensyms (n-res)
                                    `(let ((,n-res (list ,_)))
                                       (cond ((null ,',tail)
                                              (setf ,',tail  ,n-res ,',value ,n-res))
                                             (t (setf (cdr ,',tail) ,n-res ,',tail ,n-res)))))))
                (collect-macro ``(setf ,',value (,',kind ,',value ,_))))))))
    `(let* ,(nreverse binds) (macrolet ,macros ,@body))))


(defun symbolicate (&rest things &aux (index 0))
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let ((name (make-string (reduce #'+ things :key (compose 'length 'string)))))
    (dolist (thing things (values (intern name)))
      (let ((x (string thing)))
        (replace name x :start1 index)
        (incf index (length x))))))

;; End of adapted code
