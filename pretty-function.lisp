(in-package :trivial-pretty-function)

(defvar *pretty-function-printing-enabled-p* nil
  "Is pretty function printing enabled?")

(defvar *fn-table* (make-weak-hash-table :weakness :value)
  "A hash table containing all of the functions and their associated
   printers.")

(defun function-printer (fn)
  "Look up the printer for the given function. Return nil if none was
   found."
  (values (gethash fn *fn-table*)))

(defun (setf function-printer) (printer fn)
  "Set the printer for the given function."
  (setf (gethash fn *fn-table*) printer))

(defun enable-pretty-function-printing (&optional (priority 0) (table *print-pprint-dispatch*))
  "Enable pretty function printing. Return true if pretty function
   printing was previously disabled."
  (prog2
    (set-pprint-dispatch 'function 'print-pretty-function priority table)
    (not *pretty-function-printing-enabled-p*)
    (setf *pretty-function-printing-enabled-p* t)))

(defun disable-pretty-function-printing ()
  "Disables pretty function printing. Returns true if pretty function
   printing was previously enabled."
  (prog1 *pretty-function-printing-enabled-p*
    (setf *pretty-function-printing-enabled-p* nil)))

(defun print-pretty-function (s fn)
  (let ((printer (function-printer fn)))
    (if (and printer *pretty-function-printing-enabled-p*)
        (funcall printer s)
        ;; If there is no printer associated with this function,
        ;; print it normally.
        (let ((*print-pretty* nil))
          (write fn :stream s)))))

(defmacro with-function-printer (printer fn)
  "Assign the printer that results from evaluating PRINTER, to the
   function that results from evaluating FN."
  `(let ((f ,fn))
     (setf (gethash f *fn-table*) ,printer)
     f))

(defmacro named-lambda (name lambda-list &body body)
  "Create a lambda function that is associated with NAME. Whenever the
   pretty function printing is enabled, a function declared with
   named-lambda will be printed with its name. NAME isn't evaluated."
  `(named-lambda* ',name ,lambda-list ,@body))

(defmacro named-lambda* (name-form lambda-list &body body)
  "Create a lambda function that is associated with NAME. Whenever the
   pretty function printing is enabled, a function declared with
   named-lambda will be printed with its name. NAME is evaluated."
  `(with-function-printer (lambda (s)
                            (format s "#<named-lambda ~A>" ,name-form))
     (lambda ,lambda-list ,@body)))

(defun clear-pretty-function-table ()
  "Remove all printers associated with pretty functions."
  (clrhash *fn-table*))

(provide :pretty-function)
