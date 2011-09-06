;;; :FILE-CREATED <Timestamp: #{2011-09-05T15:06:17-04:00Z}#{11361} - by MON>
;;; :FILE vivace-graph-v2/vivace-graph-v2-doc-utils.lisp
;;; ==============================

;;; ==============================
;;; Utility functions useful for maintaing documentation of functions, macros,
;;; variables, types seperate from the locus of the source defining them.
;;; If we ever incorporate the Unicly system this file can be dropped as it
;;; duplicates existing functionality already provided by that system.
;;;
;;; :NOTE Source for originally taken from mon-systems/specials.lisp
;;; :SEE (URL `https://raw.github.com/mon-key/mon-systems-cl/master/specials.lisp')
;;;
;;; The stuff in mon-systems was is in turn derivative of source from Robert
;;; Strandh's SICL.
;;; :SEE (URL `git://common-lisp.net/projects/sicl/SICL.git')
;;; 
;;; ==============================
(in-package #:vivace-graph-v2)


;;; ==============================
;;; documentation fun
;;; ==============================  

;;; :SOURCE mcclim/Apps/Scigraph/dwim/extensions.lisp
;;; Which noted: "A somewhat consful implementation, but entirely portable."
(defun type-specifier-p (object)
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))

(defun doc-set (name object-type string args);&rest args)
  (declare (type symbol name) 
           ((member variable type function) object-type)
           ((or null string) string))
  (let ((doc-or-null 
         (if (null string)
             string
             (apply #'format nil `(,string ,@args)))))
        (ecase object-type
          (function
           (setf (documentation (fdefinition name) object-type) 
                 (setf (documentation name object-type) doc-or-null)))
      (variable 
       (locally (declare (special name))
         (setf (documentation name object-type) doc-or-null)))
      (type 
       (setf (documentation name object-type) doc-or-null)))))

(defun fundoc (name &optional string &rest args)
  (declare (type symbol name) ((or null string) string))
  (doc-set name 'function string args))

(defun vardoc (name &optional string &rest args)
  (declare (type symbol name)
           (special name) 
           ((or null string) string))
  (doc-set name 'variable string args))

(defun typedoc (name &optional string &rest args)
  (declare (type symbol name) 
           ((or null string) string))
  (when (type-specifier-p name)
    (doc-set name 'type string args)))

;;; ==============================
;;; EOF
