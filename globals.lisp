(in-package #:vivace-graph-v2)

(defpackage #:graph-words)
(defparameter *graph-words* (find-package :graph-words))

(defparameter *store* nil)
(defparameter *store-table* (make-hash-table :synchronized t :test 'eql))
(defparameter *namespaces*  (make-hash-table :synchronized t :test 'equalp))

(defparameter *read-uncommitted* t)

(defparameter *compression-enabled?* t)

;; Graphs
(defvar *graph* nil)
(defvar *graph-table* nil)

;; Logging
;;
;; :NOTE Osicat provides a nice (and mostly SBCL) equivalent syslog interface
;; with `osicat-posix:syslog'
;;
;; (defvar *syslog-program* "vivace-graph-v2")
;; (defvar *syslog-facility* sb-posix:log-local7)
;; (progn 
;;   (defparameter *syslog-priorities* (make-hash-table :size 9))
;;   (loop 
;;      with vec = (vector :emerg :alert :crit :err :warning :notice :info :debug)
;;      for idx from 0 below 8
;;      do (setf (gethash (aref vec idx) *syslog-priorities*) idx)
;;      finally (setf (gethash :warn    *syslog-priorities*)
;;                    (gethash :warning *syslog-priorities*))))
;;
(defvar *syslog-program* "vivace-graph-v2")
(defvar *syslog-facility* sb-posix:log-local7)
(progn
  (defparameter *syslog-priorities* (make-hash-table))
  (setf (gethash :emerg   *syslog-priorities*) sb-posix:log-emerg)
  (setf (gethash :alert   *syslog-priorities*) sb-posix:log-alert)
  (setf (gethash :crit    *syslog-priorities*) sb-posix:log-crit)
  (setf (gethash :err     *syslog-priorities*) sb-posix:log-err)
  (setf (gethash :warning *syslog-priorities*) sb-posix:log-warning)
  (setf (gethash :warn    *syslog-priorities*) sb-posix:log-warning)
  (setf (gethash :notice  *syslog-priorities*) sb-posix:log-notice)
  (setf (gethash :info    *syslog-priorities*) sb-posix:log-info)
  (setf (gethash :debug   *syslog-priorities*) sb-posix:log-debug))

;; Prolog specials
(defparameter *occurs-check* t)

(defparameter *prolog-trace* nil)

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defvar *var-counter* 0)

(defvar *select-list* nil )

(defvar *cont* nil )

(defvar *functor* nil)

(defvar *prolog-global-functors* (make-hash-table :synchronized t))

(defvar *user-functors*          (make-hash-table :synchronized t :test 'eql))
