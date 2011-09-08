;;; :FILE-CREATED <Timestamp: #{2011-09-07T02:43:06-04:00Z}#{11363} - by MON>
;;; :FILE vivace-graph-v2-FORK/concurrency-bridge.lisp
;;; ==============================

(in-package #:vivace-graph-v2)


#-sbcl
(defun concurrent-make-mailbox (&key name initial-contents)
  (declare (ignore name initial-contents))
  (error "not implemented -- what is equivalent of sb-concurrency:make-mailbox?"))

#-sbcl
(defun concurrent-send-message (mailbox message)
  (declare (ignore mailbox message))
  (error "not implemented -- what is equivalent of sb-concurrency:send-message"))

#-sbcl
(defun concurrent-receive-message (mailbox &key)
  (declare (ignore mailbox))
  (error "not implemented -- what is equivalent of sb-concurrency:receive-message"))

#-sbcl
(defun concurrent-receive-pending-message (mailbox &optional n)
  (declare (ignore mailbox n))
  (error "not implemented -- what is equivalent of ~
          sb-concurrency:receive-pending-messages"))

#-sbcl
(defun concurrent-make-queue (&key name initial-contents)        
  (declare (ignore name initial-contents))
  (error "not implemented -- what is equivalent of sb-concurrency:make-queue")) 

#-sbcl
(defun concurrent-enqueue (value queue)
  (declare (ignore value queue))
  (error "not implemented -- what is equivalent of sb-concurrency:enqueue"))

#-sbcl
(defun concurrent-dequeue (queue)
  (declare (ignore value queue))
  (error "not implemented -- what is equivalent of sb-concurrency:dequeue"))

#-sbcl
(defun concurrent-queue-empty-p (queue)
  (declare (ignore value queue))
  (error "not implemented -- what is equivalent of sb-concurrency:queue-empty-p"))

;;; ==============================

#+sbcl
(defun concurrent-make-mailbox (&key name initial-contents)
  (sb-concurrency:make-mailbox :name name 
                               :initial-contents initial-contents))

#+sbcl
(defun concurrent-send-message (mailbox message)
  (sb-concurrency:send-message mailbox message))

#+sbcl
(defun concurrent-receive-message (mailbox &key)
  (sb-concurrency:receive-message mailbox))

#+sbcl
(defun concurrent-receive-pending-message (mailbox &optional n)
  (sb-concurrency:receive-pending-messages mailbox n))

#+sbcl
(defun concurrent-make-queue (&key name initial-contents)        
  (sb-concurrency:make-queue :name name                          
                             :initial-contents initial-contents))
#+sbcl
(defun concurrent-enqueue (value queue)
  (sb-concurrency:enqueue value queue))

#+sbcl
(defun concurrent-dequeue (queue)
  (sb-concurrency:dequeue queue))

#+sbcl
(defun concurrent-queue-empty-p (queue)
  (sb-concurrency:queue-empty-p queue))

;;; ==============================
;;; EOF
