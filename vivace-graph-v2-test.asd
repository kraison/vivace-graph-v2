;; ASDF package description for vivace-graph-v2-test     -*- Lisp -*-

(defpackage :vivace-graph-v2-test-system (:use :cl :asdf))
(in-package :vivace-graph-v2-test-system)

(defsystem vivace-graph-v2-test
  :name "Vivace Graph Tests"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.2"
  :description "Vivace Graph Version 2 Test Suite"
  :long-description "Vivace Graph Version 2 Test Suite."
  :depends-on (:vivace-graph-v2
	       :bordeaux-threads
	       :cl-fad
	       :fiveam)
  :components ((:file "vivace-graph-v2-test-package")
	       (:file "test-scenarios" :depends-on ("vivace-graph-v2-test-package"))
	       (:file "vivace-graph-v2-test" :depends-on ("test-scenarios"))))


