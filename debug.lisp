(in-package :lispmake)

;; lispmake, written by Matthew Veety, et al.
;; (c) Matthew Veety 2012-2021. Under BSD License.

(defun quit-lisp ()
  #+sbcl (sb-ext:exit)
  #+ccl (ccl:quit)
  #-(or ccl sbcl) (lm-abort "unable to exit cleanly in your lisp"))

(defun lm-error (function explain)
  (format t "lispmake: error: ~A: ~A~%" function explain)
  (if *debugging*
      (abort)
      (quit-lisp)))

(defun lm-abort (explain)
  (format t "lispmake: abort: ~A~%" explain)
  (if *debugging*
      (abort)
      (quit-lisp)))

(defun lm-warning (function explain)
  (format t "lispmake: warning: ~A: ~A~%" function explain)
  nil)

(defun lm-debug (function explain)
  (if *debugging*
	  (progn
		(format t "lispmake: debug: ~A: ~A~%" function explain)
		(force-output))
	  nil))

(defmacro lm-advdebug (function fmt &rest forms)
  (if *debugging*
      (progn
		(format t "lispmake: debug: ~A: " function)
		`(format t ,fmt ,@forms)
		(format t "~%"))))
