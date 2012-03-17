(in-package #:vivace-graph-v2)

(defun add-to-text-index (idx triple)
  ;; FIXME: we need our own tokenizer that does some stemming and such
  (format t "Adding ~A to full text idx~%" triple)
  (let ((doc (make-instance 'montezuma:document)))
    (montezuma:add-field doc
                         (montezuma:make-field
                          "triple-id" (format nil "~A" (id triple))
                          :stored t :index :untokenized))
     (montezuma:add-field doc
                          (montezuma:make-field
                           "subject" (format nil "~A" (subject triple))
                           :stored nil :index :tokenized))
     (montezuma:add-field doc
                          (montezuma:make-field
                           "object" (format nil "~A" (object triple))
                           :stored nil :index :tokenized))
     (montezuma:add-field doc
                          (montezuma:make-field
                           "graph" (format nil "~A" (graph triple))
                           :stored nil :index :untokenized))
     (montezuma:add-field doc
                          (montezuma:make-field
                           "predicate" (format nil "~A" (predicate triple))
                           :stored nil :index :untokenized))
     (montezuma:add-document-to-index idx doc)
     doc))

(defun remove-from-text-index (idx triple)
  (montezuma:delete-document
   idx (montezuma:make-term "triple-id" (format nil "~A" (id triple)))))

(defun full-text-search (index search-string &key g s p)
  (let ((result (make-array 0 :fill-pointer t :adjustable t)))
    (montezuma:search-each
     index
     (with-output-to-string (stream)
       (format stream "object:\"~A\"" search-string)
       (when g (format stream " graph:\"~A\"" g))
       (when g (format stream " subject:\"~A\"" s))
       (when g (format stream " predicate:\"~A\"" p)))
     #'(lambda (doc-id score)
         (let ((doc (montezuma:get-document index doc-id)))
           (vector-push-extend
            (uuid:make-uuid-from-string
             (montezuma:document-value doc "triple-id"))
          result))))
    (make-index-cursor :index index :vector result :pointer 0)))
