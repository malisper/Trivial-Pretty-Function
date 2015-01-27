(defpackage :trivial-pretty-function
  (:nicknames :tpf)
  (:use :cl :trivial-garbage)
  (:export :enable-pretty-function-printing
           :named-lambda
           :named-lambda*
           :with-function-printer

           :*pretty-function-printing-supported-p*
           :print-pretty-function-table
           :clear-pretty-function-table
           :get-function-printer))
