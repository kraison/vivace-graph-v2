;; ASDF package description for vivace-graph-v2-test     -*- Lisp -*-

(defpackage #:vivace-graph-v2-test-system (:use #:common-llisp #:asdf))

(in-package #:vivace-graph-v2-test-system)

(defsystem vivace-graph-v2-test
  :name "Vivace Graph Tests"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.2"
  :description "Vivace Graph Version 2 Test Suite"
  :long-description "Vivace Graph Version 2 Test Suite."
  :depends-on (:vivace-graph-v2
	       :cl-fad
	       :fiveam)
  :components ((:file "vivace-graph-v2-test-package")
               (:file "test-aux"             :depends-on ("vivace-graph-v2-test-package"))
	       (:file "test-scenarios"       :depends-on ("vivace-graph-v2-test-package"))
	       (:file "vivace-graph-v2-test" :depends-on ("test-scenarios"))))



(defmethod asdf:perform :before ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  (declare (ignore op) (ignore sys))
  (asdf:load-system :vivace-graph-v2-test)
  (princ (make-string *print-right-margin* :initial-element #\=))
  (format t "~&~%Exectuting tests:~%"))
  
(defmethod asdf:perform ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  (declare (ignore op) (ignore sys))
  (format t "~%Journaling~%") 
  (funcall (read-from-string "wal-test::run-all-tests"))
  (format t "~%Binary IO~%") 
  (funcall (read-from-string "binary-file::run-all-tests"))
  (format t "~%Block IO~%") 
  (funcall (read-from-string "swap-file-test::run-all-tests"))
  (format t "~%BTree~%") 
  (funcall (read-from-string "b-tree-test-impl::run-all-tests")))

(defmethod asdf:perform :after ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  (declare (ignore op) (ignore sys))
  (format t "~%Done.~%")
  (princ (make-string *print-right-margin* :initial-element #\=))
  (terpri))

(defmethod asdf:operation-done-p ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  "testing is always performed on-demand"
  (declare (ignore op) (ignore sys))
  nil)

(defmethod asdf:perform ((op test-op)(sys (eql (find-system :vivace-graph-v2-test))))
  (declare (ignore op) (ignore sys))
  (asdf:test-system :vivace-graph-v2))

(defmethod asdf:operation-done-p ((op test-op)(sys (eql (find-system :vivace-graph-v2-test))))
  "testing is always performed on-demand"
  (declare (ignore op) (ignore sys))
  nil)
