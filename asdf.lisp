(in-package :lispmake)

;; lispmake, written by Matthew Veety, et al.
;; (c) Matthew Veety 2012-2021. Under BSD License.

(defparameter *generate-asdf* nil)
(defparameter *central-registry-additions* nil)
(defparameter *asdf-systems* nil)
(defparameter *asdf-make* nil)
(defparameter *asdf-track-ql-deps* nil)

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
		  (progn
			(push tmp asdf:*central-registry*)
			(push tmp *central-registry-additions*))))))

(defun pl-asdf-load-system (args)
  (if (not (listp args))
	  (lm-error "pl-asdf-system" "invalid arguments"))
  (enable-asdf)
  (dolist (x args)
	(push (string x) *asdf-systems*)))

(defun pl-asdf-make (args)
  (if (not (listp args))
	  (lm-error "pl-asdf-make" "invalid arguments"))
  (enable-asdf)
  (setf *asdf-make* (car args)))

(defun pl-asdf-pregen ()
  (if *generate-asdf*
	  (format t "(require 'asdf)~%")))

(defun pl-asdf-postgen ()
  (if *generate-asdf*
	  (progn
		(dolist (x *central-registry-additions*)
		  (format t "(push \"~A\" asdf:*central-registry*)~%" x))
		(dolist (x *asdf-systems*)
		  (format t "(asdf:load-system \"~A\" :verbose ~A)~%" x
				  (if (get-var 'asdf-verbose) "t" "nil")))
		(if *asdf-make*
			(format t "(asdf:make :~A)~%" *asdf-make*)))))
