;;; :FILE-CREATED <Timestamp: #{2011-09-23T18:54:38-04:00Z}#{11385} - by MON>
;;; :FILE vivace-graph-v2-FORK/queue.lisp
;;; ==============================

(in-package #:vivace-graph-v2)

;; The following queueing code was borrowed and adapted from Russell & Norvig's
;; "Introduction to AI"
(defun print-queue (q stream depth)
  (declare (ignore depth))
  (format stream "<QUEUE: ~a>" (queue-elements q)))

(defstruct (queue                       ; make-queue
             (:print-function print-queue))
  (key #'identity)                      ; queue-key
  (last nil)                            ; queue-last
  (elements nil))                       ; queue-elements

(defun make-empty-queue () 
  (make-queue))

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

;;; ==============================
;;; EOF
