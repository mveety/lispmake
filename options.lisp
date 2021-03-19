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
(defparameter *variables*
  (list 'prefix "/usr/local"
		'bindir ""
		'libdir ""
		'etcdir ""
		'target ""
		'cwd ""
		'default-mode '(:user-read :user-exec :user-write
						:group-read :group-exec
						:other-read :other-exec)))

(defparameter *varhdlrs* nil)

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

(defun get-var (varname)
  (getf *variables* varname))

(defun set-var (varname value)
  (let* ((vstat (getf *variables* varname)))
    (if (equal vstat nil)
	(progn
	  (pushnew value *variables*)
	  (pushnew varname *variables*))
	(progn
	  (if (getf *varhdlrs* varname)
	      (funcall (getf *varhdlrs* varname) value)
	      (setf (getf *variables* varname) value))))))

(defun set-var-handler (varname handler)
  (setf (getf *varhdlrs* varname) handler))

(defun var-defined-p (varname)
  (if (find varname *variables*)
      t
      nil))

(defmacro true-mess (value)
  ;; this is still a shitshow, but a bit more contained.
  `(or (equal ,value "true")
       (equal ,value "TRUE")
       (equal ,value "yes")
       (equal ,value "YES")
       (equal ,value "y")
       (equal ,value "t")
       (equal ,value "T")
       (equal ,value "Y")
       (equal ,value "1")))

(defun handle-options ()
  (setf *cmd-options* (unix-options:cli-options))
  (dolist (x *cmd-options*)
    (let* ((bf (split-equal x)))
      (if (not (equal bf nil))
		  (let* ((var (car bf))
				 (value (cadr bf)))
			(cond
			  ((equal var "target")
			   (setf *target* value)
			   (set-var 'target *target*))
			  ((equal var "lisp")
			   (setf *lisp-executable* value))
			  ((equal var "file")
			   (setf *lmfname* value))
			  ((equal var "dobuild")
			   (setf *do-build-override* t)
			   (if (true-mess value)
				   (setf *do-build* T)
				   (setf *do-build* nil)))
			  ((equal var "debug")
			   (if (true-mess value)
				   (setf *debugging* T)
				   (setf *debugging* nil)))
			  (T (set-var (string-to-symbol var) value))))
	  ;;; if the arg doesn't have an = sign, then assume it's a target
	  (progn
	    (set-var 'target x)
	    (setf *target* x)))))
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

(defun set-default-mode (value)
  (cond
    ((typep value 'cons)
     (setf (getf *variables* 'default-mode) value))
    ((or (typep value 'string)
		 (typep value 'number))
     (let ((tmpperm nil))
       (if (typep value 'number)
		   (setf tmpperm (format nil "~A" value))
		   (setf tmpperm value))
       (setf (getf *variables* 'default-mode) (number-to-perms tmpperm))))
    (t (lm-error "handler: default-mode" "invalid type"))))

(defun initialize-vars ()
  (let* ((prefix (get-var 'prefix))
		 (prefix-changed nil)
		 (cwdstr (format nil "~A" (osicat:current-directory)))
		 bindir libdir etcdir)
    ;; remove trailing slash from prefix if it exists
    (if (equal (subseq prefix (1- (length prefix))) "/")
		(setf prefix (subseq prefix 0 (1- (length prefix)))
			  prefix-changed t))
	(if (equal (subseq cwdstr (1- (length cwdstr))) "/")
		(setf cwdstr (subseq cwdstr 0 (1- (length cwdstr)))))
    (setf bindir (concatenate 'string prefix "/bin")
		  libdir (concatenate 'string prefix "/lib")
		  etcdir (concatenate 'string prefix "/etc"))
    (if prefix-changed
		(set-var 'prefix prefix))
    (if (equal (get-var 'bindir) "")
		(set-var 'bindir bindir))
    (if (equal (get-var 'libdir) "")
		(set-var 'libdir libdir))
    (if (equal (get-var 'etcdir) "")
		(set-var 'etcdir etcdir))
    (if (equal (get-var 'target) "")
		(set-var 'target "build"))
	(set-var 'cwd cwdstr))
  (set-var-handler 'default-mode #'set-default-mode))

(defun pl-apply-prefix ()
  (initialize-vars))
