;;; :FILE-CREATED <Timestamp: #{2011-09-07T02:43:06-04:00Z}#{11363} - by MON>
;;; :FILE vivace-graph-v2-FORK/concurrency-bridge.lisp
;;; ==============================

(in-package #:vivace-graph-v2)

(defun concurrent-make-mailbox (&key name initial-contents)
  (sb-concurrency:make-mailbox :name name 
                               :initial-contents initial-contents))

(defun concurrent-send-message (mailbox message)
  (sb-concurrency:send-message mailbox message))

(defun concurrent-receive-message (mailbox &key)
  (sb-concurrency:receive-message mailbox))

(defun concurrent-receive-pending-message (mailbox &optional n)
  (sb-concurrency:receive-pending-messages mailbox n))

(defun concurrent-make-queue (&key name initial-contents)        
  (sb-concurrency:make-queue :name name                          
                             :initial-contents initial-contents))

(defun concurrent-enqueue (value queue)
  (sb-concurrency:enqueue value queue))

(defun concurrent-dequeue (queue)
  (sb-concurrency:dequeue queue))


;;; ==============================
;;; EOF
