(in-package :lispmake)

;; lispmake, written by Matthew Veety, et al.
;; (c) Matthew Veety 2012-2021. Under BSD License.

(defun loadfile (outstream fname)
  (format outstream "(load #P\"~A\")~%" fname))

(defun quickloads (outstream library)
  (format outstream "(ql:quickload '~A)~%" library))

(defun pl-package (args)
  (setf *lm-package* args)
  (set-var 'package (car args)))

(defun pl-toplevel (args)
  (setf *toplevel* args)
  (set-var 'toplevel (car args)))

(defun pl-file (args)
  (if (listp args)
      (dolist (x args)
		(let ((var (varhdl x)))
		  (if (not (nilp var))
			  (setf *sources* (append *sources* (list var))))))
      (lm-error "file" "invalid arguments")))

(defun pl-output (args)
  (setf *outfile* args)
  (set-var 'outfile (car args)))

(defun pl-quicklisp (args)
  (if (listp args)
      (dolist (x args)
		(setf *quickloads* (append *quickloads* (list x))))
      (lm-error "quicklisp" "invalid arguments")))
