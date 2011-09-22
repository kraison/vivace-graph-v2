;; ASDF package description for vivace-graph-v2              -*- Lisp -*-

(defpackage #:vivace-graph-v2-system (:use #:common-lisp #:asdf))

(in-package #:vivace-graph-v2-system)

(defsystem #:vivace-graph-v2
  :name "Vivace Graph"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.2"
  :description "Vivace Graph Version 2"
  :long-description "Vivace Graph Version 2."
  :depends-on (
	       #+sbcl :sb-concurrency
	       #+sbcl :sb-posix
               :babel  ;; cl-btree ???
               :cl-btree-0.5
	       :cffi
	       :bordeaux-threads
	       :cl-skip-list
	       :salza2
	       :chipz
	       ;; :hunchentoot    ;; not currently in use AFAICT
	       :uuid
	       :ieee-floats
	       :local-time
	       :date-calc
	       :parse-number
	       :split-sequence
	       ;; :py-configparser ;; not currently in use AFAICT
	       ;; :cl-js           ;; not currently in use AFAICT
	       ;; :cl-json         ;; not currently in use AFAICT 
               )
  :components ((:file "vivace-graph-v2-package")
               (:file "uuid-bridge"        :depends-on ("vivace-graph-v2-package"))
	       (:file "gettimeofday"       :depends-on ("vivace-graph-v2-package"))
	       (:file "hash-table"         :depends-on ("vivace-graph-v2-package"))
               (:file "concurrency-bridge" :depends-on ("vivace-graph-v2-package"))
	       (:file "conditions"         :depends-on ("vivace-graph-v2-package"))
	       (:file "constants"          :depends-on ("conditions"))
	       (:file "globals"            :depends-on ("constants"))
	       (:file "utilities"          :depends-on ("globals"))
	       (:file "lock"               :depends-on ("utilities" "hash-table"))
	       (:file "data-types"         :depends-on ("lock"))
	       (:file "certainty-factors"  :depends-on ("constants"))
	       (:file "serialize"          :depends-on ("data-types"))
	       (:file "deserialize"        :depends-on ("serialize"))
               (:file "btree"              :depends-on ("deserialize")) 
               (:file "table"              :depends-on ("btree")) 
	       (:file "index"              :depends-on ("deserialize"))
	       (:file "transaction"        :depends-on ("index"))
	       (:file "full-text-index"    :depends-on ("transaction"))
	       (:file "store"              :depends-on ("transaction" "full-text-index"))
	       (:file "namespaces"         :depends-on ("store"))
	       (:file "functor"            :depends-on ("namespaces"))
	       (:file "triples"            :depends-on ("functor" "gettimeofday"))
	       (:file "prologc"            :depends-on ("triples"))
	       (:file "prolog-functors"    :depends-on ("prologc"))
	       (:file "templates"          :depends-on ("prolog-functors"))
               (:file "vivace-graph-v2-doc-utils" :depends-on ("vivace-graph-v2-package"))
               (:file "vivace-graph-v2-docs"      :depends-on ("vivace-graph-v2-doc-utils"))))
