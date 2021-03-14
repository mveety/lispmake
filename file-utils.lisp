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

(defun perms-to-number (perms-list)
  (let ((user 0)
	(group 0)
	(other 0))
    (dolist (x perms-list)
      (cond
	((equal x :user-read) (incf user 4))
	((equal x :user-write) (incf user 2))
	((equal x :user-exec) (incf user 1))
	((equal x :group-read) (incf group 4))
	((equal x :group-write) (incf group 2))
	((equal x :group-exec) (incf group 1))
	((equal x :other-read) (incf other 4))
	((equal x :other-write) (incf other 2))
	((equal x :other-exec) (incf other 1))))
    (format nil "~A~A~A" user group other)))

(defun number-to-perms (perms-str)
  (let ((rperms (list (parse-integer (subseq perms-str 0 1) :radix 8)
		      (parse-integer (subseq perms-str 1 2) :radix 8)
		      (parse-integer (subseq perms-str 2 3) :radix 8)))
	(state 0)
	(unixread 4)
	(unixwrite 2)
	(unixexec 1)
	(perms nil))
    (dolist (x rperms)
      (if (not (< (- x unixread) 0))
	  (setf perms (append perms
			      (case state
				(0 (list :user-read))
				(1 (list :group-read))
				(2 (list :other-read))))
		x (- x unixread)))
      (if (not (< (- x unixwrite) 0))
	  (setf perms (append perms
			      (case state
				(0 (list :user-write))
				(1 (list :group-write))
				(2 (list :other-write))))
		x (- x unixwrite)))
      (if (equal x unixexec)
	  (setf perms (append perms
			      (case state
				(0 (list :user-exec))
				(1 (list :group-exec))
				(2 (list :other-exec))))))
      (incf state))
    perms))

(defun pl-install (args)
  (if (listp args)
      (let* ((filename (varhdl (getf args :file)))
	     (target (varhdl (getf args :to)))
	     (mode (varhdl (getf args :mode)))
	     destpath
	     cleantgt
	     ; (options (varhdl (getf args :options)))
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
