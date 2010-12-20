;; ASDF package description for vivace-graph-v2              -*- Lisp -*-

(defpackage :vivace-graph-v2-system (:use :cl :asdf))
(in-package :vivace-graph-v2-system)

(defsystem vivace-graph-v2
  :name "Vivace Graph"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.2"
  :description "Vivace Graph Version 2"
  :long-description "Vivace Graph Version 2."
  :depends-on (:sb-concurrency
	       :sb-posix
	       :cffi
	       :bordeaux-threads
	       :cl-skip-list
	       :hunchentoot
	       :uuid
	       :ieee-floats
	       :local-time
	       :date-calc
	       :parse-number
	       :split-sequence
	       :py-configparser
	       :cl-js
	       :cl-json)
  :components ((:file "uuid")
	       (:file "vivace-graph-v2-package" :depends-on ("uuid"))
	       (:file "gettimeofday" :depends-on ("vivace-graph-v2-package"))
	       (:file "conditions" :depends-on ("vivace-graph-v2-package"))
	       (:file "constants" :depends-on ("conditions"))
	       (:file "globals" :depends-on ("constants"))
	       (:file "utilities" :depends-on ("globals"))
	       (:file "data-types" :depends-on ("utilities"))
	       (:file "certainty-factors" :depends-on ("constants"))
	       (:file "transaction" :depends-on ("data-types"))
	       (:file "index" :depends-on ("transaction"))
	       (:file "full-text-index" :depends-on ("data-types"))
	       (:file "store" :depends-on ("index" "full-text-index"))
	       (:file "namespaces" :depends-on ("store"))
	       (:file "functor" :depends-on ("namespaces"))
	       (:file "triples" :depends-on ("functor" "gettimeofday"))
	       (:file "prologc" :depends-on ("triples"))
	       (:file "prolog-functors" :depends-on ("prologc"))
	       (:file "templates" :depends-on ("prolog-functors"))
	       ))

