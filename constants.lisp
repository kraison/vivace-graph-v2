(in-package #:vivace-graph-v2)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro defconst (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
)

(cffi:defctype size :unsigned-int)

;; Prolog constants and specials
(defconstant +unbound+ :unbound)
(ignore-errors (defconstant +no-bindings+ '((t . t))))
(defconstant +fail+ nil)

;; Certainty factors
(defconstant +cf-true+    1.0)
(defconstant +cf-false+  -1.0)
(defconstant +cf-unknown+ 0.0)

;; :NOTE The `defcons'd variables are treated as constants, e.g. with the
;; +<FOO>+ convention but defined with `cl:defparameter'.
;; Unless there is some reason why these shouldn't be def i'd perfer to define
;; the as such -- MON
;;
;; :NOTE Changed radix of following constants/variables to hex just in case
;; someone has an insane value for `cl:*read-base*' -- MON

;; :NOTE these were `cl:defparameter'd -- MON
;; Shortened slot identifiers for slot keys
(defconst +predicate-slot+         #x00)
(defconst +subject-slot+           #x01)
(defconst +object-slot+            #x02)
(defconst +timestamp-slot+         #x03)
(defconst +belief-factor-slot+     #x04)
(defconst +deleted?-slot+          #x04)
(defconst +derived?-slot+          #x05)
(defconst +uuid-slot+              #x06)
(defconst +name-slot+              #x07)
(defconst +clauses-slot+           #x08)
(defconst +premises-slot+          #x09)
(defconst +conclusions-slot+       #x0a)
(defconst +cf-slot+                #x0b)

;; :NOTE These were `cl:defparameter'd -- MON
;; Action identifiers for serialization
(defconst +transaction+            #x00)
(defconst +add-triple+             #x01)
(defconst +delete-triple+          #x02)
(defconst +undelete-triple+        #X03)
(defconst +set-cf+                 #x04)

;; Built-in type identifiers for serializing
(defconstant +needs-lookup+        :needs-lookup)
(defconstant +negative-integer+    #x01)
(defconstant +positive-integer+    #x02)
(defconstant +character+           #x03)
(defconstant +symbol+              #x04)
(defconstant +string+              #x05)
(defconstant +list+                #x06)
(defconstant +vector+              #x07)
(defconstant +single-float+        #x08)
(defconstant +double-float+        #x09)
(defconstant +ratio+               #x0A) ;10
(defconstant +t+                   #x0B) ;11
(defconstant +null+                #x0C) ;12
(defconstant +blob+                #x0D) ;13 ;; A blob is a block of uninterpreted octets 
(defconstant +dotted-list+         #x0E) ;14 
(defconstant +compressed-string+   #x0F) ;15

;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+                #x64) ;100
(defconstant +triple+              #x65) ;101
(defconstant +predicate+           #x66) ;102
(defconstant +timestamp+           #x67) ;103
(defconstant +rule+                #x68) ;104

;; Type identifiers for index persistence
(defconstant +table+               #x6E) ;110
(defconstant +uri+                 #x6F) ;111
(defconstant +pathname+            #x70) ;112
(defconstant +package+             #x71) ;113

;; :NOTE Why the gap from #x72 - #x77? -- MON

;; Type identifiers for rdfs semantics
(defconstant +literal+            #x78) ;120
(defconstant +plain-literal+      #x79) ;121
(defconstant +typed-literal+      #x7A) ;122
(defconstant +xml-literal+        #x7B) ;123
(defconstant +seq+                #x7C) ;124
(defconstant +bag+                #x7D) ;125
(defconstant +alt+                #x7E) ;126
(defconstant +nil+                #x7F) ;127

