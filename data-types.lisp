(in-package #:vivace-graph-v2)

;;; Dates
;;; timestamps provided by local-time lib
(defgeneric timestamp? (thing)
  (:method ((thing timestamp)) t)
  (:method (thing) nil))

;;; Triple structure
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
  subject 
  predicate 
  object 
  graph 
  id 
  (deleted? nil) 
  (cf +cf-true+) 
  (persistent? t))

