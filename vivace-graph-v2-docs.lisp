;;; :FILE-CREATED <Timestamp: #{2011-09-05T15:03:38-04:00Z}#{11361} - by MON>
;;; :FILE vivace-graph-v2/vivace-graph-v2-docs.lisp
;;; ==============================

(in-package #:vivace-graph-v2)


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
