(in-package #:vivace-graph-v2)

(defun register-namespace (short-name uri &key errorp)
  (declare (ignore errorp))
  (setf (gethash uri *namespaces*) short-name)
  (setf (gethash short-name *namespaces*) uri))

(defun display-namespaces ()
  (maphash #'(lambda (k v)
               (unless (uri? k)
                 (format t "~A~A=> ~A~%" k #\Tab v)))
           *namespaces*))

(defun get-namespace (short-name)
  (gethash short-name *namespaces*))

(defun read-node (stream)
  (with-output-to-string (out)
    (loop
       for c = (read-char stream nil :eof)
       do
       (if (member c '(#\Space #\Newline #\Tab #\Return #\)))
           (progn
             (unread-char c stream)
             (return))
           (format out "~A" c)))))

(defun read-namespace (stream char)
  (declare (ignore char))
  (let ((c (read-char stream nil :eof)))
    (cond ((eql c #\") ;; This is a string, treat it as such
           (funcall (get-macro-character #\") stream #\"))
          ((eql c #\<) ;; This is a URI, treat it as such
           (format nil "<~A>" (funcall (get-macro-character #\") stream #\>)))
          (t
           (let ((uri (get-namespace
                       (with-output-to-string (key-stream)
                         (loop until (or (eq :eof c) (eql c #\:)) do
                              (format key-stream "~A" c)
                              (setq c (read-char stream nil :eof)))))))
             (if uri
                 (format nil "<~A~A>" uri (read-node stream))
                 nil))))))

(defun enable-!-reader ()
  (set-macro-character #\! #'read-namespace))

(defun disable-namespace-reader ()
  (set-macro-character #\! nil))

(defun namespace-reader-enabled? ()
  (get-macro-character #\!))

(defun shorten-namespace (thing)
  thing)
