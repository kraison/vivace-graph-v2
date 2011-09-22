;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :vivace-graph-v2)

(defmethod b-tree-impl::|KEY>| (x y)
  (b-tree-impl::key< y x))

(defmethod b-tree-impl::|KEY>=| (x y)
  (or
    (b-tree-impl::key= x y)
    (b-tree-impl::key< y x)))




(defmethod b-tree-impl::key< ((x puri:uri) y)
  (b-tree-impl::key< (princ-to-string x) y))

(defmethod b-tree-impl::key< (x (y puri:uri))
  (b-tree-impl::key< x (princ-to-string y)))

(defmethod b-tree-impl::key= ((x puri:uri) y)
  (b-tree-impl::key= (puri::uri-string x) y))

(defmethod b-tree-impl::key= (x (y puri:uri))
  (b-tree-impl::key= x (puri::uri-string y)))




(defmethod b-tree-impl::key< ((x symbol) y)
  (string< (symbol-name x) y))

(defmethod b-tree-impl::key< ((x symbol) (y number))
  (string< (symbol-name x) (write-to-string y)))

(defmethod b-tree-impl::key< ((x number) (y symbol))
  (string< (write-to-string x) (symbol-name y)))

(defmethod b-tree-impl::key< ((x number) (y string))
  (string< (write-to-string x) y))

(defmethod b-tree-impl::key< ((x number) (y uuid:uuid))
  (string< (write-to-string x) (uuid:print-bytes nil y)))

(defmethod b-tree-impl::key< ((x string) (y symbol))
  (string< x (symbol-name y)))

(defmethod b-tree-impl::key< ((x string) (y number))
  (string< x (write-to-string y)))

(defmethod b-tree-impl::key< ((x string) (y uuid:uuid))
  (string< x (uuid:print-bytes nil y)))

(defmethod b-tree-impl::key< ((x timestamp) (y timestamp))
  (timestamp< x y))

(defmethod b-tree-impl::key< ((x number) (y timestamp))
  (< (timestamp-to-universal x) y))

(defmethod b-tree-impl::key< ((x timestamp) (y number))
  (< x (timestamp-to-universal y)))

(defmethod b-tree-impl::key< ((x uuid:uuid) (y uuid:uuid))
    (string< (uuid:print-bytes nil x) (uuid:print-bytes nil y)))

(defmethod b-tree-impl::key< ((x uuid:uuid) (y string))
  (string< (uuid:print-bytes nil x) y))

(defmethod b-tree-impl::key< ((x uuid:uuid) (y symbol))
  (string< (uuid:print-bytes nil x) (symbol-name y)))

(defmethod b-tree-impl::key< ((x uuid:uuid) (y number))
  (string< (uuid:print-bytes nil x) (write-to-string y))))

