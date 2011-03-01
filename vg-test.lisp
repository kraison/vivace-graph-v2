(require 'asdf)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(asdf:oos 'asdf:load-op 'VIVACE-GRAPH-V2-TEST)
(in-package #:VIVACE-GRAPH-V2-TEST)
(run-all-tests)
