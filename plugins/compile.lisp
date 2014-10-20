(in-package :lispmake)

;; plugin install:
;; (:plugin :compile-file "compile.lisp" pl-compile-file pl-init-compile-file) 

(defvar *compile-files* nil)

(defun pl-compile-file (args)
  (lm-debug "pl-compile-file" "compiling file")
  (if (not (equal (length args) 1))
      (lm-error "pl-compile-file" "args length incorrect")
      (setf *compile-files* (append *compile-files* args))))

(defun pl-init-compile-file ()
  (lm-debug "pl-init-compile-file" "installing plugin")
  (install-pregen-hook 'pl-compile-file-pregen))

(defun pl-compile-file-pregen ()
  (lm-debug "pl-compile-file-pregen" "running pregen")
  (if (not *compile-files*)
      (dolist (x *compile-files*)
	(lm-debug "pl-compile-file-pregen" "adding file")
	(format t "(compile-file ~A :output-file ~A)~%" x (concatenate 'string x ".fasl")))))
