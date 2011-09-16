;;; :FILE-CREATED <Timestamp: #{2011-09-05T15:03:38-04:00Z}#{11361} - by MON>
;;; :FILE vivace-graph-v2/vivace-graph-v2-docs.lisp
;;; ==============================

(in-package #:vivace-graph-v2)

;;; ==============================
;;;  vivace-graph-v2/utilities.lisp
;;; ==============================

(fundoc 'symbol-nstring-upcase
"Normalize SYMBOL-TO-UPCASE to the string representation of its cannonicaly
upcased symbol-name.~%~@
If SYMBOL-TO-UPCASE is not of type cl:symbol an error is signaled.~%~@
:EXAMPLE~%
 \(symbol-nstring-upcase '|BubBa|\)~%
 \(symbol-nstring-upcase 'bubba\)~%
 \(symbol-nstring-upcase \"string-fails-succesfully\"\)~%~@
:SEE-ALSO `ensure-internable'.~%")

(fundoc 'ensure-internable
"When THING is of type cl:symbol or cl:string intern it as a canonical upcased symbol.~%~@
When THING is of type cl:number return THING.~%~@
When THING is of some other type an error is signaled.~%~@
:EXAMPLE~%
 \(ensure-internable 88\)~%
 \(ensure-internable '|BubBa|\)~%
 \(ensure-internable 'bubba\)~%
 \(ensure-internable \"bubba\"\)~%
 \(ensure-internable \"BubBa\"\)~%~@
:SEE-ALSO `symbol-nstring-upcase'.~%")



;;; ==============================
;;; vivace-graph-v2/globals.lisp
;;; ==============================

(vardoc '*cont*
"Continuation container for Prolog step-wise queries.")

(vardoc '*select-list*
        "Accumulator for Prolog selects.")

(vardoc '*var-counter*
 "Counter for generating Prolog variable names.")

(vardoc '*functor*
  "The Prolog functor currently being compiled.")



;;; ==============================
;;; vivace-graph-v2/triples.lisp
;;; ==============================

(fundoc 'make-anonymous-node
  "Create a unique anonymous node identifier.~%~@
Return value is a string of the form:~%
 \"_anon:<UUID>\"~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `deftemplate'.~%")

(fundoc 'get-triples
        "Return a cursor to the result.~%~@
Keyword G defaults to `*graph*'.~%~@
Keyword STORE defaults to `*store*'.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'print-triple
"Printer function for instances of the structure triple.
Print output dependent on value of `*print-triple-details*'.~%")

(fundoc 'triple?
"Whether object is an instance of the structure triple.~%")

(fundoc 'triple-subject
 "Return the SPOG subject associated with an INSTANCE of the structure triple.~%")

(fundoc 'triple-predicate
 "Return the SPOG predicate associated with an INSTANCE of the structure triple.~%")

(fundoc 'triple-object   
"Return the SPOG object associated with an INSTANCE of the structure triple.~%")

(fundoc 'triple-graph
"Return the SPOG graph associated with an INSTANCE of the structure triple.~%")

(fundoc 'triple-id 
"Return the SPOGI id associated with an INSTANCE of the structure triple.~%")

(fundoc 'triple-deleted?
"Whether INSTANCE of the structure triple is deleted from the triple-store.~%")

(fundoc 'triple-cf
"Return certainty factor for an INSTANCE of the structure triple.~%")

(fundoc 'triple-persistent?
"Whether INSTANCE of the structure triple is persitent in the triple-store.~%")





;;; ==============================
;;; vivace-graph-v2/templates.lisp
;;; ==============================

(fundoc 'deftemplate
        "Define a template with NAME and slots SLOTS.~%~@
A function with name NAME is added to the template table of `*store*'.~%~@
The function so created may then be used to create sets of triples conforming to
template's format.~%~@
The SLOTS of template may be used as \"predicates\" \(slot readers\) of NAME.~%~@
The arguments to `deftemplate' should be of the form:~%
 \(deftemplate <NAME>
   \(slot <PREDICATE>\)
    {...}   
   \(slot <PREDICATE>\)\)
:EXAMPLE~%
 \(deftemplate person
   \(slot has-name\)
   \(slot has-age\)
   \(slot has-eye-color\)
   \(slot has-hair-color\)\)~%~@
:SEE-ALSO `fact', `deffacts'.~%")

(fundoc 'fact
        "Create a triple-set using the named template TEMPLATE.~%~@
TEMPLATE is an object as defined with the macro `deftemplate'.~%~@
The range of possible triple-sets which may be created from TEMPLATE are
constrained by the slots \(reader predicates\) defined of TEMPLATE.~%~@
The arguments to `fact' should be of the form:~%
 \(fact 
  \(<TEMPLATE-NAME>
   \(<PREDICATE> <VALUE>\)
    {...}   
   \(<PREDICATE> <VALUE>\)\)\)~%~@
:EXAMPLE~%~@
 \(fact 
  \(person \(has-name      \"John Q. Public\"\)
          \(has-age        23\)
          \(has-eye-color  blue\)
          \(has-hair-color black\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'deffacts
        "Create a grouping of triple-sets from TEMPLATES.~%~@
Each triple-set in grouping has the same form as the TEMPLATE argument to the
macro `fact' where a fact is a set of assertions conforming to a named template
as defined by the macro `deftemplate'.~%~@
The arguments to `deffacts' should be of the form:~%
 \(deffacts
     \(<TEMPLATE-NAME>
      \(<PREDICATE> <VALUE>\)
      {...}   
      \(<PREDICATE> <VALUE>\)\)
     \(<TEMPLATE-NAME>
      \(<PREDICATE> <VALUE>\)
      {...}   
      \(<PREDICATE> <VALUE>\)\)\)~%~@
:EXAMPLE~%
 \(deffacts
     \(person \(has-name       \"John Q. Public\"\) 
             \(has-age        23\) 
             \(has-eye-color  blue\) 
             \(has-hair-color black\)\)
     \(pet    \(has-owner      \"John Q. Public\"\) 
             \(has-name       \"Fido\"\)
             \(has-species    \"canine\"\) 
             \(has-eye-color  brown\) 
             \(has-hair-color black\)\)\)
:SEE-ALSO `<XREF>'.~%")

;;; ==============================
;;; EOF
