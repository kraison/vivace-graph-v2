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
	       :fiveam)
  :components ((:file "vivace-graph-v2-test")))

