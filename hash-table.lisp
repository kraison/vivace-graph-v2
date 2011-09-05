(in-package #:vivace-graph-v2)

(defun acquire-hash-table-lock (hash-table)
    ;; (if (eq sb-thread:*current-thread* 
  (if (vg-current-thread-spinlock-value-check)
      t
      ;; :WAS (sb-thread:my-get-spinlock (sb-impl:hash-table-spinlock hash-table))))
      (vg-get-spinlock (sb-impl:hash-table-spinlock hash-table))))

(defun release-hash-table-lock (hash-table)
  ;; (if (eq sb-thread:*current-thread* 
  (if (vg-current-thread-spinlock-value-check)
      (sb-thread::release-spinlock (sb-impl:hash-table-spinlock hash-table))))
