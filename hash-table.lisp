(in-package #:vivace-graph-v2)

(defun acquire-hash-table-lock (hash-table)
  (if (eq sb-thread:*current-thread* 
	  (sb-thread:spinlock-value (sb-impl:hash-table-spinlock hash-table)))
      t
      (sb-thread:my-get-spinlock (sb-impl:hash-table-spinlock hash-table))))

(defun release-hash-table-lock (hash-table)
  (if (eq sb-thread:*current-thread* 
	  (sb-thread:spinlock-value (sb-impl:hash-table-spinlock hash-table)))
      (sb-thread:release-spinlock (sb-impl:hash-table-spinlock hash-table))))

