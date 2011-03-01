(in-package #:vivace-graph-v2-test)

(defparameter *test-db-dir* #P"/var/tmp/vivace-graph-v2-test-db/")

(defun test-select (store)
  (let ((*store* store))
    (let ((triple (select-first (?s ?p ?o) (q- ?s ?p ?o))))
      (and (equal "VGT" (first triple))
           (equal "is-a" (second triple))
           (equal "thing" (third triple))))))

(defun run-all-tests ()
  (fiveam:def-suite vg-test-suite :description "VG Test Suite")
  (fiveam:in-suite vg-test-suite)
  (ensure-directories-exist *test-db-dir*)
  (format t "~%~%Preparing to run all VivaceGraph Tests.~%")
  (fiveam:test (vg-tests)
    ;; Basic tests of graph db
    (fiveam:is (triple-store? (create-triple-store 
			       :name "VGT" 
			       :location *test-db-dir*)))
    (fiveam:is (triple-store? *store*))
    (fiveam:is (equal "VGT" *graph*))
    (fiveam:is (triple? (add-triple "VGT" "is-a" "thing" :cf 1.0)))
    (fiveam:is (triple? (first (get-triples-list))))
    (fiveam:is (test-select *store*))
    (fiveam:is-false (close-triple-store))
    (fiveam:is (null *store*))
    (format t "~%")
    (fiveam:is (triple-store? (open-triple-store
			       :name "VGT"
			       :location *test-db-dir*)))
    (fiveam:is (triple-store? *store*))
    (fiveam:is (equal "VGT" *graph*))
    (fiveam:is (triple? (first (get-triples-list))))
    (fiveam:is (test-select *store*))
    ;; Concurrency tests
    (fiveam:is (triple? (basic-concurrency-1 *store*)))
    (fiveam:is (basic-concurrency-2 *store*))
    (fiveam:is (delete-undelete-test))
    (fiveam:is-false (close-triple-store))
    (fiveam:is (null *store*))
    (fiveam:is-false (progn
		       (cl-fad:delete-directory-and-files *test-db-dir*)
		       (probe-file *test-db-dir*))))
  (fiveam:run!))

