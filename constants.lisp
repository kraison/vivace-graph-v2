(in-package #:vivace-graph-v2)

(cffi:defctype size :unsigned-int)

;; Prolog constants and specials
(defconstant +unbound+ :unbound)
(ignore-errors (defconstant +no-bindings+ '((t . t))))
(defconstant +fail+ nil)

;; Certainty factors
(defconstant +cf-true+    1.0)
(defconstant +cf-false+  -1.0)
(defconstant +cf-unknown+ 0.0)

;; Shortened slot identifiers for slot keys
(defparameter +predicate-slot+     #x00)
(defparameter +subject-slot+       #x01)
(defparameter +object-slot+        #x02)
(defparameter +timestamp-slot+     #x03)
(defparameter +belief-factor-slot+ #x04)
(defparameter +deleted?-slot+      #x04)
(defparameter +derived?-slot+      #x05)
(defparameter +uuid-slot+          #x06)
(defparameter +name-slot+          #x07)
(defparameter +clauses-slot+       #x08)
(defparameter +premises-slot+      #x09)
(defparameter +conclusions-slot+   #x0a)
(defparameter +cf-slot+            #x0b)

;; Action identifiers for serialization
(defparameter +transaction+        #x00)
(defparameter +add-triple+         #x01)
(defparameter +delete-triple+      #x02)
(defparameter +undelete-triple+    #X03)
(defparameter +set-cf+             #x04)

;; Built-in type identifiers for serializing
(defconstant +needs-lookup+        :needs-lookup)
(defconstant +negative-integer+    #x00)
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
