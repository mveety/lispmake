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
(defvar *target* nil)

(defun split-equal (string)
  (let* ((bf (split-sequence:split-sequence #\= string)))
    (if (equal (length bf) 2)
	bf
	nil)))

(defun handle-options ()
  (setf *cmd-options* (unix-options:cli-options))
  (format t "options: ~A~%" *cmd-options*)
  (format t "real options: ~A~%" (unix-options:cli-options))
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
		   (setf *debugging* nil))))))))
  (if (equal *target* nil)
      (setf *lmakefile* *lmfname*)
      (setf *lmakefile*
	    (concatenate 'string *lmakefile* "." *target*)))
  (format
   t
   "lmakefile=~A~%target=~A~%file=~A~%"
   *lmakefile* *target* *lmfname*)
  (if *debugging*
      (format
       t
       "lispmake: debug: handle-options: lmakefile=~A~%"
       *lmakefile*))
  nil)
