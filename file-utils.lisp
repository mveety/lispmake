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

(defparameter *variables* '(prefix "/usr/local"
			    bindir "/usr/local/bin"
			    libdir "/usr/local/lib"
			    etcdir "/usr/local/etc"
			    default-mode 555))

(defun get-var (varname)
  (getf *variables* varname))

(defun set-var (varname value)
  (let* ((vstat (getf *variables* varname)))
    (if (equal vstat nil)
	(progn
	  (pushnew value *variables*)
	  (pushnew varname *variables*))
	(setf (getf *variables* varname) value))))

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
