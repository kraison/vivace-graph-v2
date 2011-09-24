;;; :FILE-CREATED <Timestamp: #{2011-09-07T02:43:06-04:00Z}#{11363} - by MON>
;;; :FILE vivace-graph-v2-FORK/concurrency-bridge.lisp
;;; ==============================

;; :NOTE Following are symbols exported from LispWorks MP package:
;;  `allowing-block-interrupts', `barrier-arriver-count', `barrier-change-count',
;;  `barrier-count', `barrier-disable', `barrier-enable', `barrier-name',
;;  `barrier-pass-through', `barrier-unblock', `barrier-wait',
;;  `change-process-priority', `condition-variable-broadcast',
;;  `condition-variable-signal', `condition-variable-wait',
;;  `condition-variable-wait-count', `create-simple-process', `*current-process*',
;;  `current-process-block-interrupts', `current-process-in-cleanup-p',
;;  `current-process-pause', `current-process-unblock-interrupts',
;;  `debug-other-process', `*default-process-priority*',
;;  `*default-simple-process-priority*', `ensure-process-cleanup',
;;  `find-process-from-name', `general-handle-event', `get-current-process',
;;  `get-process', `get-process-private-property', `initialize-multiprocessing',
;;  `*initial-processes*', `last-callback-on-thread', `list-all-processes',
;;  `lock-locked-p', `lock-owned-by-current-process-p', `lock-recursive-p',
;;  `lock-recursively-locked-p', `lock-name', `lock-owner', `mailbox-empty-p',
;;  `mailbox-peek', `mailbox-read', `mailbox-reader-process', `mailbox-send',
;;  `mailbox-wait-for-event', `*main-process*', `make-barrier',
;;  `make-condition-variable', `make-lock', `make-mailbox', `make-named-timer',
;;  `make-semaphore', `make-timer', `map-all-processes',
;;  `map-all-processes-backtrace', `map-process-backtrace', `map-processes',
;;  `notice-fd', `process-alive-p', `process-all-events',
;;  `process-allow-scheduling', `process-arrest-reasons', `process-break',
;;  `process-continue', `process-exclusive-lock', `process-exclusive-unlock',
;;  `process-idle-time', `*process-initial-bindings*', `process-interrupt',
;;  `process-join', `process-kill', `process-lock', `process-mailbox',
;;  `process-name', `process-p', `process-plist', `process-poke',
;;  `process-priority', `process-private-property', `process-property',
;;  `process-reset', `process-run-function', `process-run-reasons',
;;  `process-run-time', `process-send', `process-sharing-lock',
;;  `process-sharing-unlock', `process-stop', `process-stopped-p', `process-unlock',
;;  `process-unstop', `process-wait', `process-wait-for-event',
;;  `process-wait-function', `process-wait-local',
;;  `process-wait-local-with-periodic-checks', `process-wait-local-with-timeout',
;;  `process-wait-local-with-timeout-and-periodic-checks',
;;  `process-wait-with-timeout', `process-whostate',
;;  `pushnew-to-process-private-property', `pushnew-to-process-property', `ps',
;;  `remove-from-process-private-property', `remove-from-process-property',
;;  `remove-process-private-property', `remove-process-property', `schedule-timer',
;;  `schedule-timer-milliseconds', `schedule-timer-relative',
;;  `schedule-timer-relative-milliseconds', `semaphore-acquire', `semaphore-count',
;;  `semaphore-name', `semaphore-release', `semaphore-wait-count',
;;  `simple-process-p', `symeval-in-process', `timer-expired-p', `timer-name',
;;  `unnotice-fd', `unschedule-timer', `wait-processing-events',
;;  `with-exclusive-lock', `with-interrupts-blocked', `with-lock',
;;  `with-sharing-lock', `without-interrupts', `without-preemption', `yield',
;; :SEE (URL `http://www.lispworks.com/documentation/lw60/LW/html/lw-949.htm#pgfId-886156')
;; :SEE (URL `http://www.lispworks.com/documentation/lw60/LW/html/lw-228.htm#69534')

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
(defun concurrent-receive-pending-messages (mailbox &optional n)
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

;; (bt:join-thread (logger-thread store))

;;; ==============================
;;; EOF
