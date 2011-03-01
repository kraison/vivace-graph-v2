(in-package #:vivace-graph-v2-test)

(defparameter *basic-concurrency-1* nil)

(defun basic-concurrency-1 (&optional (store *store*))
  (let ((*store* store))
    (let ((thr1 (make-thread 
		 #'(lambda ()
		     (with-graph-transaction (*store* :timeout 10)
		       (let ((triple (add-triple "This" "is-a" "test" :graph "VGT")))
			 (format t "~%basic-concurrency-1: ~A: ~A~%" 
				 (triple-id triple) triple)
			 (setq *basic-concurrency-1* triple)
			 (sleep 3))))))
	  (thr2 (make-thread 
		 #'(lambda ()
		     (sleep 1)
		     (let ((triple (add-triple
				    "This" "is-a" "test" :graph "VGT")))
		       (if (triple? triple)
			   (format t "basic-concurrency-1 lookup: ~A: ~A~%" 
				   (triple-id triple) triple)
			   (format t "basic-concurrency-1 lookup: ~A~%" triple))
			   (if (triple-equal triple *basic-concurrency-1*)
			       (setq *basic-concurrency-1* triple)
			       (setq *basic-concurrency-1* nil)))))))
      (join-thread thr1)
      (join-thread thr2)
      (format t "basic-concurrency-1: ~A~%" *basic-concurrency-1*)
      *basic-concurrency-1*)))
    
