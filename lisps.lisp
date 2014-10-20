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

#+ccl (defvar *lisp-type* 'ccl)
#+sbcl (defvar *lisp-type* 'sbcl)
#+clisp (defvar *lisp-type* 'clisp)
#-(or ccl sbcl clisp) (lm-error "lisps.lisp" "unable to support this lisp")

(defun buildexe (outstream fname package toplevel &optional (lisp 'default))
  (if *debugging*
      (format t "lispmake: debug: lisp=~A~%" lisp))
  (if (or (not lisp) (equal lisp 'default))
      (setf lisp *lisp-type*))
  (if *debugging*
      (lm-debug "buildexe" "generating output"))
  (if *debugging*
      (format t "lispmake: debug: lisp=~A~%" lisp))
  (format outstream
	  "#+sbcl (sb-ext:save-lisp-and-die \"~A\" :executable t :toplevel #'~A:~A)~%"
	  (car fname) (car package) (car toplevel))
  (format outstream
	  "#+ccl (ccl:save-application ~A :toplevel-function #'~A:~A :prepend-kernel t)~%"
	  (car fname) (car package) (car toplevel))
  (format outstream
	  "#+clisp (ext:saveinitmem ~A :init-function #'~A:~A :executable t :norc t)~%"
	  (car fname) (car package) (car toplevel)))
