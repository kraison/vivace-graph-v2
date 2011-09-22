;;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;;; Licensed under Apache License 2.0 

(in-package :vivace-graph-v2)

(defun printv-minor-separator ()
  (format *trace-output* "~&;; ~60,,,'-<-~>~%")
  (force-output *trace-output*))

(defun printv-major-separator ()
  (format *trace-output* "~&;;~%")
  (princ
    (concatenate 'string ";; "
      (make-string (- *print-right-margin* 5) :initial-element #\=)) *trace-output*)
  (force-output *trace-output*))

(defun printv-form-printer (form)
  (typecase form
    ;; String (label):
    (string (format *trace-output* "~&;; ~a~%" form))
    ;; Evaluated form:
    ((or cons (and symbol (not keyword)))
      (format *trace-output* "~&;;   ~w =>" form))
    (vector (format *trace-output* "~&;;   ~s~%" form)) 
    ;; Self-evaluating form:
    (t (format *trace-output* "~&;;   ~s~%" form)))
  (force-output *trace-output*))

(defun printv-values-printer (values-list)
  (format *trace-output* "~:[ [returned 0 values]~;~:*~{ ~w~^;~}~]~%"  values-list)
  (force-output *trace-output*))

(defun printv-expander (forms &optional values-trans-fn) ;; Allow for customized printv'ers:
  (let ((result-sym (gensym)))
    `(let ((*print-readably* nil) ,result-sym)
       ,@(loop for form in forms nconcing
           (cond
             ;; Markup form:
             ((eq form ':ff) (list '(printv-major-separator)))
             ((eq form ':hr) (list '(printv-minor-separator)))
             ;; Evaluated form:
             ((or (consp form) (and (symbolp form) (not (keywordp form))))
               `((printv-form-printer ',form)
                  (printv-values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list ,form))
                                         `(multiple-value-list ,form))))))
             ;; Self-evaluating form:
             (t `((printv-form-printer 
                    (car (setf ,result-sym (list ,form))))))))
       (values-list ,result-sym))))

(defmacro printv (&rest forms)
  (printv-expander forms))

(defmacro :printv (&rest forms)
  (printv-expander forms))


#||

  (assert (equalp (list 7 5)
          (multiple-value-list 
            (printv :ff
              "some simple examples" :hr ""
              nil t  (list :s :p :o :c) #(1 2 3 4) :foo *package* (make-instance 'standard-object) 
              "" "multiple value examples" :hr ""
              (values) (gethash 'x (make-hash-table)) (values (+ 3 4) (+ 2 3)) :ff))))

;; ===============================================================================================
;; some simple examples
;; ------------------------------------------------------------
;; 
;;   NIL => NIL
;;   T => T
;;   (LIST :S :P :O :C) => (:S :P :O :C)
;;   #(1 2 3 4)
;;   :FOO
;;   *PACKAGE* => #<PACKAGE "VIVACE-GRAPH-V2">
;;   (MAKE-INSTANCE 'STANDARD-OBJECT) => #<STANDARD-OBJECT {100471CA71}>
;; 
;; multiple value examples
;; ------------------------------------------------------------
;; 
;;   (VALUES) => [returned 0 values]
;;   (GETHASH 'X (MAKE-HASH-TABLE)) => NIL; NIL
;;   (VALUES (+ 3 4) (+ 2 3)) => 7; 5
;;
;; ===============================================================================================

||#

