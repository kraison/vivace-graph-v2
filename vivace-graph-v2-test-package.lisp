(in-package #:cl-user)

(defpackage #:vivace-graph-v2-test
  (:use #:cl #:vivace-graph-v2 #:bordeaux-threads)
  (:export #:run-all-tests
	   #:*test-db-dir*))

