;; (in-package #:cl-user)

;; :NOTE I personally think the "NODE" nickname is a bad idea. 
;; Who knows what other packages use this nickname already. -- MON
(defpackage #:graph-words
  (:nicknames :node)
  (:use))

(defpackage #:vg-get-time-of-day
  (:use #:common-lisp #:cffi)
  (:export #:gettimeofday))

;; :NOTE If possible, I'd like to _not_ ":use" some of these packages.
;; AFAICT the only feature which directly depends on CFFI is gettimeofday and
;; that is now in its own package.
;; Also, I've made some effort to package qualify the bordeaux-threads symbols
;; b/c it is most prone to namespace conflicts. -- MON

(defpackage #:vivace-graph-v2
  (:nicknames :vg)
  (:use #:common-lisp
        #:cffi
	#:bordeaux-threads 
	#:cl-skip-list
	#:local-time)
  (:import-from #:vg-get-time-of-day #:gettimeofday)
  (:export #:*store*
	   #:create-triple-store
	   #:open-triple-store
	   #:close-triple-store
	   #:clear-triple-store
	   #:*store*
	   #:*graph*
	   #:use-graph
	   #:clear-graph
	   #:triple-store?
	   #:needs-indexing?
	   #:do-indexing
	   #:with-graph-transaction
	   #:with-locked-index
	   #:get-table-to-lock
	   #:*current-transaction*
	   #:*in-transaction-p
	   #:main-idx
	   #:log-mailbox
	   #:tx-store
	   #:transaction?
	   #:dbm-rollback
	   #:dbm-commit
	   #:dbm-begin
	   #:triple-db
	   #:functors
	   #:enable-!-reader
	   #:disable-namespace-reader
	   #:register-namespace
	   #:display-namespaces

	   #:triple?
	   #:triple-subject
	   #:triple-predicate
	   #:triple-object
	   #:triple-id
	   #:triple-cf
	   #:subject
	   #:predicate
	   #:object
	   #:id
	   #:cf
	   #:triple-eql
	   #:triple-equal
	   #:make-anonymous-node
	   #:add-triple
	   #:get-triples
	   #:get-triples-list
	   #:list-triples
	   #:triple-count
	   #:-o
	   #:lookup-triple
	   #:bulk-add-triples
	   #:index-predicate?
	   #:list-indexed-predicates
	   #:index-predicate
	   #:unindex-predicate
	   #:map-text-search
	   #:delete-triple
	   #:undelete-triple
	   #:triple-deleted?
	   #:erase-triple
	   #:dump-triples
	   #:load-triples
	   #:reify
	   #:reify-recursive
	   #:anonymous?

	   #:rule?
	   #:defrule
	   #:get-rule
	   #:retract-rule
	   #:deftemplate
	   #:fact
	   #:deffacts

	   #:certainty-factor-p
	   #:belief-factor
	   #:true-p
	   #:false-p
	   #:unknown-p
	   #:cf-or
	   #:cf-and
	   #:cf-combine
	   #:conjunct-cf
	   #:recalculate-cf
	   #:adjust-belief
	   #:set-triple-cf

	   #:!
	   #:<-
	   #:?-
	   #:q-
	   #:cut
	   #:prolog
	   #:def-global-prolog-functor
	   #:*prolog-global-functors*
	   #:unify
	   #:var-deref
	   #:insert
	   #:select
	   #:select-flat
	   #:select-first
	   #:select-one
	   #:do-query
	   #:map-query
	   #:read/1
	   #:wrte/1
	   #:nl/0
	   #:repeat/0
	   #:fail/0
	   #:=/2
	   #:==/2
	   #:/=/2
	   #:>/2
	   #:</2
	   #:>=/2
	   #:<=/2
	   #:??
	   #:lisp/2
	   #:regex-match/2
	   #:var/1
	   #:is/2
	   #:call/1
	   #:not/1
	   #:bagof/3
	   #:setof/3
	   #:if/2
	   #:if/3
	   #:is-valid/1
	   #:is-valid?/1
	   #:is-invalid/1
	   #:is-invalid?/1
	   #:valid-date?/1
	   #:trigger/1
	   #:assert/1
	   #:retract/1
	   #:show-prolog-vars/2
	   #:select/2
	   #:select-as-bind-alist/2
	   #:triple-search/3
	   #:valid-date?
	   #:*trail*
	   #:*var-counter*
	   #:*predicate*
	   #:*select-list*
	   #:*prolog-global-functors*
	   #:*prolog-trace*
	   #:trace-prolog
	   #:untrace-prolog
	   #:prolog-error

	   #:timestamp?
	   #:universal-to-timestamp
	   #:timestamp-to-universal
	   #:decode-timestamp

	   #:flatten
	   ))


