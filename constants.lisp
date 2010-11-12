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

(defconstant +needs-lookup+ :needs-lookup)

;; Built-in type identifiers for serializing
(defconstant +needs-lookup+ :needs-lookup)
(defconstant +unknown+ 0)
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

;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+ 100)
(defconstant +triple+ 101)
(defconstant +predicate+ 102)
(defconstant +timestamp+ 103)
(defconstant +rule+ 104)
