(in-package :asdf-user)

(defsystem "trivial-pretty-function"
  :version "0.1"
  :depends-on '("trivial-garbage")
  :components ((:file "package")
               (:file "pretty-function.lisp")))
