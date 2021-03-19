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

(defun pl-exec (args)
  (if (listp args)
      (let* ((cmd (car args))
			 (cmd-args (cdr args)))
		(oth-run-executable cmd cmd-args))))

(defun pl-install (args)
  (if (listp args)
      (let* ((filename (varhdl (getf args :file)))
			 (target (varhdl (getf args :to)))
			 (mode (varhdl (getf args :mode)))
			 destpath
			 cleantgt
			 ;; (options (varhdl (getf args :options)))
			 )
		(if (nilp filename)
			(lm-abort "filename equals nil"))
		(if (nilp target)
			(setf target (get-var 'bindir)))
		(if (nilp mode)
			(setf mode (get-var 'default-mode)))
		(if (stringp mode)
			(setf mode (number-to-perms mode)))
		(setf cleantgt (subseq target 1))
		(setf destpath (cl-fad:canonical-pathname
						(make-pathname :directory (list :absolute cleantgt) :name filename)))
		(if (cl-fad:directory-exists-p target)
			(progn
			  (format t ":install ~A -> ~A (~A)~%"
					  filename
					  destpath
					  (perms-to-number mode))
			  (cl-fad:copy-file
			   filename
			   (concatenate 'string
							target
							"/"
							filename)
			   :overwrite t)
			  (setf (osicat:file-permissions destpath) mode))
			(lm-abort "target directory doesn't exist")))
      (lm-error "install" "invalid arguments")))

(defun lm-delete-file (fname)
  (if (probe-file fname)
      (progn
		(format t ":delete ~A~%" fname)
		(delete-file fname))))

(defun pl-delete (args)
  (if (not (listp args))
      (lm-error "delete" "invalid arguments"))
  (dolist (x args)
    (if (stringp x)
		(lm-delete-file x)
		(let ((var (varhdl x)))
		  (if (stringp var)
			  (lm-delete-file var))))))

(defun pl-define (args)
  (if (and (listp args)
		   (equal (length args) 2))
      (let* ((varname (car args))
			 (value (cadr args)))
		(set-var varname value))))

(defun pl-require-file (args)
  (if (not (cl-fad:file-exists-p (car args)))
      (lm-error "require-file" (format nil "~A is missing" (car args)))))
