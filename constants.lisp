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

;; :TODO These variables are treated as constants, e.g. with the +<FOO>+ convention
;; Unless there is some reason why these shouldn't be def i'd perfer to define the as such -- MON
;;
;; :NOTE Changed radix of following constants/variables to hex just in case
;; someone has an insane value for `cl:*read-base*'

;; :NOTE these were `cl:defparameter'd
;; Shortened slot identifiers for slot keys
(defconst +predicate-slot+     #x00)
(defconst +subject-slot+       #x01)
(defconst +object-slot+        #x02)
(defconst +timestamp-slot+     #x03)
(defconst +belief-factor-slot+ #x04)
(defconst +deleted?-slot+      #x04)
(defconst +derived?-slot+      #x05)
(defconst +uuid-slot+          #x06)
(defconst +name-slot+          #x07)
(defconst +clauses-slot+       #x08)
(defconst +premises-slot+      #x09)
(defconst +conclusions-slot+   #x0a)
(defconst +cf-slot+            #x0b)

;; :NOTE these were `cl:defparameter'd
;; Action identifiers for serialization
(defconst +transaction+        #x00)
(defconst +add-triple+         #x01)
(defconst +delete-triple+      #x02)
(defconst +undelete-triple+    #X03)
(defconst +set-cf+             #x04)

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
(defconstant +ratio+               #x0A)
(defconstant +t+                   #x0B)
(defconstant +null+                #x0C)
(defconstant +blob+                #x0D) ;; Uninterpreted octets
(defconstant +dotted-list+         #x0E) 
(defconstant +compressed-string+   #x0F)

;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+                #x64) 
(defconstant +triple+              #x65) 
(defconstant +predicate+           #x66) 
(defconstant +timestamp+           #x67) 
(defconstant +rule+                #x68) 
