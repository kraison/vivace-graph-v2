(in-package #:vivace-graph-v2)

(defparameter *graph-words* (find-package "GRAPH-WORDS"))

(defparameter *literals*    (vg-make-hash-table :synchronized t :test 'equalp))
(defparameter *nodes*       (vg-make-hash-table :synchronized t :test 'equalp))

(defparameter *store* nil)
(defparameter *store-table* (vg-make-hash-table :synchronized t :test 'eql))

;; IMHO the null-uuid is basically a thing that exists b/c it has to otherwise
;; the UUID model falls over and unless there is a specific reason for using the
;; null-uuid we shouldn'? -- MON
;;
;; :WAS (defvar *default-context*   (unicly:make-null-uuid))
(defvar *default-vivace-graph-context* (unicly:make-v5-uuid unicly:*uuid-namespace-dns* "*default-context*"))

;; :FIXME Relying on asdf:<FOO> here is potentially a point of failure should
;; this variable ever change location. 
;; Also, in general, creating pathnames in someone elses tree is rude. 
;; Also, this is a bad idea w/r/t any potential Quicklisp dist b/c it will leave
;; a data/db somewhere underneath software/vivace-graph-v2 -- MON
(defvar *default-location-defaults* (ensure-directories-exist
                                     (asdf:system-relative-pathname
                                      (asdf:find-system :vivace-graph-v2)
                                      "data/" :type "db")))

(defvar *constituent* nil
  "dynamic indication of current node's statement constituent type")

(defvar *depth* nil
  "dynamic indication of depth during descent in hierarchical index")

 

(defparameter *namespaces* (vg-make-hash-table :synchronized t :test 'equalp))

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

(defvar *prolog-global-functors* (vg-make-hash-table :synchronized t))

(defvar *user-functors*          (vg-make-hash-table :synchronized t :test 'eql))
