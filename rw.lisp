(defun do-write ()
  (with-open-file (stream "/var/tmp/test" 
			  :direction :output 
                          :if-exists :overwrite 
                          :if-does-not-exist :create)
    (time
     (dotimes (i 1000)
       (dotimes (j 1000)
	 (write `(+ ,i ,j) :stream stream)
	 (format stream "~%"))))))

(defun do-read ()
  (with-open-file (stream "/var/tmp/test")
    (time
     (handler-case
	 (loop
	    (eval (read stream)))
       (error (c)
	 (format t "Got error ~A~%" c))))))

(let ((ht (make-hash-table :test 'equal :synchronized t)))
  (dotimes (i 10000)
    (let ((ht1 (make-hash-table :test 'equal :synchronized t)))
      (setf (gethash (format nil "~A" i) ht) ht1)
      (dotimes (j 100)
	(setf (gethash (format nil "~A" j) ht1) (random 10000)))))    
  (defun cl-store-write ()
    (time (cl-store:store ht "/var/tmp/ht.dat"))))
