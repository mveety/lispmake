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

(defparameter *generate-asdf* nil)
(defparameter *central-registry-additions* nil)
(defparameter *asdf-systems* nil)

(defun initialize-asdf-vars ()
  (set-var 'asdf-verbose nil))

(defun enable-asdf ()
  (setf *generate-asdf* t))

(defun pl-asdf-add-registry (args)
  (if (not (listp args))
	  (lm-error "pl-asdf-add-registry" "invalid arguments"))
  (let ((tmp nil))
	(dolist (x args)
	  (setf tmp (varhdl x))
	  (if (not (nilp tmp))
		  (push tmp *central-registry-additions*)))))

(defun pl-asdf-load-system (args)
  (if (not (listp args))
	  (lm-error "pl-asdf-system" "invalid arguments"))
  (enable-asdf)
  (dolist (x args)
	(push x *asdf-systems*)))

(defun pl-asdf-pregen ()
  (if *generate-asdf*
	  (format t "(require 'asdf)~%")))

(defun pl-asdf-postgen ()
  (if *generate-asdf*
	  (progn
		(dolist (x *central-registry-additions*)
		  (format t "(push \"~A\" asdf:*central-registry*)~%" x))
		(dolist (x *asdf-systems*)
		  (format t "(asdf:load-system '~A :verbose ~A)~%" x
				  (if (get-var 'asdf-verbose) "t" "nil"))))))
