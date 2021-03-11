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
      (format t "lispmake: debug: ~A: ~A~%" function explain))
  nil)

(defmacro lm-advdebug (function fmt &rest forms)
  (if *debugging*
      (progn
	(format t "lispmake: debug: ~A: " function)
	`(format t ,fmt ,@forms)
	(format t "~%"))))
