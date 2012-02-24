;;; :FILE vivace-graph-v2/templates.lisp
;;; ==============================

#||
 
:NOTE Instead of using a v1 or v4 UUID, why not instead create the node as a v3
or v5 UUID using a namespace *anonymous-node-namespace* as created with
make-v4-uuid?

;; (string-downcase (princ-to-string (uuid:make-v4-uuid)))
;;=> "a8c5c06f-fbf7-4275-b2be-55907fbeb18e"
;; :NOTE Unicly does not require downcasing the uuid b/c it already DTRT :P

 (defconstant +mostly-immutable-anonymous-node-namespace+ "a8c5c06f-fbf7-4275-b2be-55907fbeb18e"
   "A string of hexadecimal characters delimited by #\\- with the form: 8-4-4-12.
Its value is the string representation of the UUID used as the value of the
variable `*anonymous-node-namespace*'.
:NOTE The intention is that this variable should remain constant across multiple
vivace-graph-v2 sessions. IOW its value should not be mutated!
Doing so may invalidate the identity of pre-existing persistent UUIDs instiated
as if by the macro `deftemplate'.")

 (defvar *anonymous-node-namespace* 
   (make-uuid-from-string +mostly-immutable-anonymous-node-namespace+)
   "Namespace used for creation of anonymous nodes with the macro `deftemplate'.
:SEE-ALSO `make-template-node-in-anonymouse-namespace'.")

 (defun make-template-node-in-anonymouse-namespace (stringed-node-name)
   "Helper function for the macro `deftemplate'.
STRINGED-NODE-NAME is a string to as the NAME argument to `make-v5-uuid'.
It is assumed that STRINGED-NODE-NAME has already processed with `string-downcase'."
   (declare ((or string symbol) stringed-node-name)
            (special *anonymous-node-namespace*)
            (uuid *anonymous-node-namespace*))
   (uuid:make-v5-uuid *anonymous-node-namespace* stringed-node-name))
`
 (defmacro tt--deftemplate (name &rest slots)
   (unless (triple-store? *store*)
     (error "deftemplate ~A: *store* is not bound to a triple store!" name))
   (let ((node            (gensym))
         (node-namestring (gensym)))
     (setf (gethash name (templates *store*))
           (eval
            `#'(lambda (&key ,@(mapcar #'second slots))
                 (with-graph-transaction (*store*)
                   (let* ( ;; it is acceptable to pass string-downcase a symbol
                          (,node-namestring (string-downcase name)) ;; (symbol-name name) ;
                          (,node (make-template-node-in-anonymouse-namespace ,node-namestring)))
                     (add-triple ,node "is-a" ,node-namestring)
                     ,@(mapcar 
                        #'(lambda (slot)
                            `(add-triple ,node 
                                         ,(string-downcase (symbol-name (second slot)))
                                         ,(second slot)))
                        slots)
                     ,node)))))))

||#



(in-package #:vivace-graph-v2)

(defmacro deftemplate (name &rest slots)
  (unless (triple-store? *store*)
    (error "deftemplate ~A: *store* is not bound to a triple store!" name))
  (let ((node (gensym)))
    (setf (gethash name (templates *store*))
          (eval
           `#'(lambda (&key ,@(mapcar #'second slots))
                (with-graph-transaction (*store*)
                  (let ((,node (vg-uuid::make-anonymous-node)))
                    (add-triple ,node "is-a" ,(string-downcase (symbol-name name)))
                    ,@(mapcar 
                       #'(lambda (slot)
                           `(add-triple ,node 
                                        ,(string-downcase (symbol-name (second slot)))
                                        ,(second slot)))
                       slots)
                    ,node)))))))

(defmacro fact (template)
  (let ((tmpl-name (first template)))
    `(funcall (gethash ',tmpl-name (templates *store*))
              ,@(flatten (mapcar #'(lambda (slot)
                                     `(,(intern (symbol-name (first slot)) 'keyword)
                                        ,(second slot)))
                                 (rest template))))))

(defmacro deffacts (&rest templates)
  (let ((template (gensym)))
    `(mapcar #'(lambda (,template)
                 (let ((tmpl-name (first ,template)))
                   (format t "tmpl-name is ~A~%" tmpl-name)
                   (apply (gethash tmpl-name (templates *store*))
                          (flatten
                           (mapcar #'(lambda (slot)
                                       (list (intern (symbol-name (first slot)) 'keyword)
                                             (second slot)))
                                   (rest ,template))))))
             ',templates)))

;;; ==============================
;;; EOF
