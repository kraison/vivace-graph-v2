(in-package #:vivace-graph-v2)

(defun add-to-text-index (idx key value)
  (skip-list-add idx key value))

(defun remove-from-text-index (idx key)
  (skip-list-delete idx key))

(defun get-index-range (index start end)
  "This is wildly inefficient;  we need to eventually coalesce the two cursor types."
  (let ((result (make-array 0 :fill-pointer t :adjustable t)))
    (let ((cursor (skip-list-range-cursor index start end)))
      (do ((kv (sl-cursor-next cursor) (sl-cursor-next cursor)))
	  ((null kv))
	(vector-push-extend (second kv) result)))
    (make-index-cursor :index index :vector result :pointer 0)))
