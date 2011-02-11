(in-package #:vivace-graph-v2)

(cffi:defctype size :unsigned-int)

;; Prolog constants and specials
(defconstant +unbound+ :unbound)
(ignore-errors (defconstant +no-bindings+ '((t . t))))
(defconstant +fail+ nil)

;; Certainty factors
(defconstant +cf-true+ 1.0)
(defconstant +cf-false+ -1.0)
(defconstant +cf-unknown+ 0.0)

;; Shortened slot identifiers for slot keys
(defparameter +predicate-slot+ #x00)
(defparameter +subject-slot+ #x01)
(defparameter +object-slot+ #x02)
(defparameter +timestamp-slot+ #x03)
(defparameter +belief-factor-slot+ #x04)
(defparameter +deleted?-slot+ #x04)
(defparameter +derived?-slot+ #x05)
(defparameter +uuid-slot+ #x06)
(defparameter +name-slot+ #x07)
(defparameter +clauses-slot+ #x08)
(defparameter +premises-slot+ #x09)
(defparameter +conclusions-slot+ #x0a)
(defparameter +cf-slot+ #x0b)

;; Action identifiers for serialization
(defparameter +transaction+ #x00)
(defparameter +add-triple+ #x01)
(defparameter +delete-triple+ #x02)
(defparameter +undelete-triple+ #X03)
(defparameter +set-cf+ #x04)

;; Built-in type identifiers for serializing
(defconstant +needs-lookup+ :needs-lookup)
(defconstant +negative-integer+ 1)
(defconstant +positive-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5)
(defconstant +list+ 6)
(defconstant +vector+ 7)
(defconstant +single-float+ 8)
(defconstant +double-float+ 9)
(defconstant +ratio+ 10)
(defconstant +t+ 11)
(defconstant +null+ 12)
(defconstant +blob+ 13) ;; Uninterpreted octets
(defconstant +dotted-list+ 14)
(defconstant +compressed-string+ 15)
;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+ 100)
(defconstant +triple+ 101)
(defconstant +predicate+ 102)
(defconstant +timestamp+ 103)
(defconstant +rule+ 104)
