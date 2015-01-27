(in-package :trivial-pretty-function)

(defvar *print-functions-pretty* t
  "Is pretty function printing enabled?")

(defvar *fn-table* (make-weak-hash-table :weakness :key)
  "A hash table containing all of the functions and their associated
   printers.")

(defun function-printer (fn)
  "Look up the printer for the given function. Return nil if none was
   found."
  (values (gethash fn *fn-table*)))

(defun (setf function-printer) (printer fn)
  "Set the printer for the given function."
  (setf (gethash fn *fn-table*) printer))

(defun print-pretty-function (s fn)
  "Print the function FN to the stream S in a pretty way."
  (let ((printer (function-printer fn)))
    (if (and printer *print-functions-pretty*)
        (funcall printer s)
        ;; If there is no printer associated with this function,
        ;; print it normally.
        (let ((*print-pretty* nil))
          (write fn :stream s)))))

(set-pprint-dispatch 'function 'print-pretty-function)

(defmacro with-function-printer (printer fn)
  "Assign the printer that results from evaluating PRINTER, to the
   function that results from evaluating FN."
  (let ((gfn (gensym)))
    `(let ((,gfn ,fn))
       (setf (function-printer ,gfn) ,printer)
       ,gfn)))

(defmacro named-lambda (name lambda-list &body body)
  "Create a lambda function that is associated with NAME. Whenever the
   pretty function printing is enabled, a function declared with
   named-lambda will be printed with its name. NAME isn't evaluated."
  `(named-lambda* ',name ,lambda-list ,@body))

(defmacro named-lambda* (name-form lambda-list &body body)
  "Create a lambda function that is associated with NAME. Whenever the
   pretty function printing is enabled, a function declared with
   named-lambda will be printed with its name. NAME is repeatedly
   evaluated every time the function is printed."
  (let ((gstream (gensym)))
    `(with-function-printer (lambda (,gstream)
                              (format ,gstream "#<named-lambda ~A>" ,name-form))
       (lambda ,lambda-list ,@body))))

(defun clear-pretty-function-table ()
  "Remove all printers associated with pretty functions."
  (clrhash *fn-table*))
