(in-package :lispmake)

;; lispmake, written by Matthew Veety, et al.
;; (c) Matthew Veety 2012,2013,2014. Under BSD License.
;; 
;; This is a hacked together program designed to generate the build
;; files that are required for a client's lisp set up. If you find
;; bugs or better ways to do things then send in patches.
;; 
;; Things that need doing:
;;     * Support for clisp, cmucl, ccl, and I guess others
;;     * build targets (ala make) (could be a plugin)
;;     * testing. It works for me, it might not for you

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
  (if (stringp args)
      (setf *sources* (append *sources* (list args)))
      (if (listp args)
	  (dolist (x args)
	    (setf *sources* (append *sources* x))))))

(defun pl-output (args)
  (setf *outfile* args)
  (set-var 'outfile (car args)))

(defun pl-quicklisp (args)
  (if (symbolp args)
      (setf *quickloads* (append *quickloads* (list args)))
      (if (listp args)
	  (dolist (x args)
	    (setf *quickloads* (append *quickloads* x))))))
