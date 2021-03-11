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

(defvar *cmd-options* nil)
(defvar *lmfname* *lmakefile*)
(defparameter *variables* '(prefix "/usr/local"
			    bindir "/usr/local/bin"
			    libdir "/usr/local/lib"
			    etcdir "/usr/local/etc"
			    default-mode 555))

(defun split-equal (string)
  (let* ((bf (split-sequence:split-sequence #\= string)))
    (if (equal (length bf) 2)
	bf
	nil)))

(defun parg (arg)
  (if (symbolp arg)
      (getf *variables* arg)
      arg))

(defun string-to-symbol (string)
  (if (stringp string)
      (intern (string-upcase string))
      nil))

(defun string-to-keyword (string)
  (if (stringp string)
      (intern (string-upcase string) "KEYWORD")
      nil))

(defun symbol-to-keyword (symbol)
  (if (symbolp symbol)
      (let* ((s (string symbol)))
	(intern (string-upcase s) "KEYWORD"))
      nil))

(defun get-var (varname)
  (getf *variables* varname))

(defun set-var (varname value)
  (let* ((vstat (getf *variables* varname)))
    (if (equal vstat nil)
	(progn
	  (pushnew value *variables*)
	  (pushnew varname *variables*))
	(setf (getf *variables* varname) value))))

(defun handle-options ()
  (setf *cmd-options* (unix-options:cli-options))
  (dolist (x *cmd-options*)
    (let* ((bf (split-equal x)))
      (if (not (equal bf nil))
	  (let* ((var (car bf))
		 (value (cadr bf)))
	    (cond
	      ((equal var "target")
	       (setf *target* value))
	      ((equal var "file")
	       (setf *lmfname* value))
	      ((equal var "debug")
	       (if (or  ;; This is a shitshow. I should fix this.
		    (equal value "true")
		    (equal value "TRUE")
		    (equal value "yes")
		    (equal value "YES")
		    (equal value "y")
		    (equal value "t")
		    (equal value "T")
		    (equal value "Y")
		    (equal value "1"))
		   (setf *debugging* T)
		   (setf *debugging* nil)))
	      (T (set-var (string-to-symbol var) value)))))))
  (if (equal *target* nil)
      (setf *lmakefile* *lmfname*)
      (setf *lmakefile*
	    (concatenate 'string *lmakefile* "." *target*)))
  (if *debugging*
      (progn
	(format
	 t
	 "lispmake: debug: handle-options: lmakefile=~A~%"
	 *lmakefile*)
	(format t "lmakefile=~A~%target=~A~%file=~A~%"
		*lmakefile* *target* *lmfname*)))
  nil)

(defun varhdl (form)
  (if (symbolp form)
      (let ((bf (getf *variables* form)))
	(if (not (equal bf nil))
	    bf
	    form))
      form))

(defmacro nilp (form)
  `(equal ,form nil))

(defun pl-fn (args)
  (if (not (listp args))
      (lm-abort "fn args should by type list"))
  (if (not (keywordp (car args)))
      (lm-abort "fn name needs to be type keyword"))
  (install-fn-plugin (car args) (cdr args)))

(defun configure-string (varname)
  (tagbody
   start
     (format t "lispmake: configure: Input value for (string) ~A: " varname)
     (force-output)
     (let* ((bf nil))
       (setf bf (read))
       (if (not (stringp bf))
	   (progn
	     (format t "lispmake: configure: error: invalid type~%")
	     (force-output)
	     (go start)))
       (set-var varname bf))))

(defun configure-boolean (varname)
  (tagbody
   start
     (format t "lispmake: configure: Enable option (y/n)~A" varname)
     (force-output)
     (let* ((bf nil))
       (setf bf (read))
       (setf bf (string-upcase bf))
       (if (or
	    (equal bf "Y")
	    (equal bf "YES")
	    (equal bf "TRUE"))
	   (set-var varname (not nil))
	   (if (or
		(equal bf "N")
		(equal bf "NO")
		(equal bf "FALSE"))
	       (set-var varname nil)
	       (progn
		 (format t "lispmake: configure: error: invalid input~%")
		 (force-output)
		 (go start)))))))

(defun configure-number (varname)
  (tagbody
   start
     (format t "lispmake: configure: Input value for (number)~A: " varname)
     (force-output)
     (let* ((bf nil))
       (setf bf (read))
       (if (not (numberp bf))
	   (progn
	     (format t "lispmake: configure: error: invalid input~%")
	     (force-output)
	     (go start)))
       (set-var varname bf))))

(defun pl-configure (args)
  (dolist (x args)
    (if (not (listp x))
	(lm-abort "must be a list of lists"))
    (let* ((type (cadr x))
	   (name (car x)))
      (cond
	((equal type :string) (configure-string name))
	((equal type :boolean) (configure-boolean name))
	((equal type :number) (configure-number name))))))

