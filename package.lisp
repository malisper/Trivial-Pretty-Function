(defpackage :trivial-pretty-function
  (:nicknames :tpf)
  (:use :cl :trivial-garbage)
  (:export :enable-pretty-function-printing
           :named-lambda
           :named-lambda*
           :with-function-printer

           :clear-pretty-function-table
           :function-printer))
