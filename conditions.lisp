(in-package #:vivace-graph-v2)

(define-condition prolog-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
	     (with-slots (reason) error
	       (format stream "Prolog error: ~A." reason)))))

(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a." 
		       instance reason)))))

(define-condition deserialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Deserialization failed for ~a because of ~a." 
		       instance reason)))))

(define-condition transaction-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
	     (with-slots (reason) error
	       (format stream "Transaction error: ~A." reason)))))

