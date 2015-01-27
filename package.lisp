(defpackage :trivial-pretty-function
  (:nicknames :tpf)
  (:use :cl :trivial-garbage)
  (:export :print-functions-pretty
           :named-lambda
           :named-lambda*
           :with-function-printer

           :clear-pretty-function-table
           :function-printer))
