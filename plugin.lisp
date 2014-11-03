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

(defun load-plugin (name file toplevel init-toplevel)
  (setf toplevel (car toplevel))
  (setf init-toplevel (car init-toplevel))
  (lm-debug "load-plugin" "loading a plugin")
  (format t "debug: toplevel=~A init-toplevel=~A~%"
	  (type-of toplevel) (type-of init-toplevel))
  (format t "debug: toplevel=~A init-toplevel=~A~%"
	  toplevel init-toplevel)
  (if *debugging*
      (format t 
	      "lispmake: debug: installing plugin ~A from file ~A with 
toplevel ~A and running ~A~%" 
	      name file toplevel init-toplevel))
  (if (equal (type-of name) 'keyword)
      (progn
	(load file)
	(setf *plugins* (append *plugins* (list (list name toplevel))))
	(lm-debug "load-plugin" "running toplevel function")
	(funcall init-toplevel))
      (lm-error "load-plugin" "arg name should be type keyword")))

(defun install-plugin (name toplevel)
  (lm-debug "install-plugin" "installing a plugin")
  (if *debugging*
      (format t "lispmake: debug: installing plugin ~A with toplevel ~A~%"
	      name toplevel))
  (if (equal (type-of toplevel) 'symbol)
      (if (equal (type-of name) 'keyword)
	  (setf *plugins* (append *plugins* (list (list name toplevel))))
	  (lm-error "install-plugin" "arg name should by type keyword"))
      (lm-error "install-plugin" "arg toplevel should be type symbol")))

(defun run-plugin-pregen (x)
  (lm-debug "run-plugin-pregen" "running pregeneration hooks")
  (let ((*standard-output* x))
    (if (not (equal *pregen-hooks* nil))
	(dolist (y *pregen-hooks*)
	  (funcall y))
	nil)))

(defun run-plugin-postgen (x)
  (lm-debug "run-plugin-pregen" "running postgeneration hooks")
  (let ((*standard-output* x))
    (if (not (equal *postgen-hooks* nil))
	(dolist (y *postgen-hooks*)
	  (funcall y))
	nil)))

(defun install-pregen-hook (fname)
  (lm-debug "install-pregen-hook" "adding pregeneration hook")
  (if (not (equal (type-of fname) 'symbol))
      (lm-warning "install-pregen-hook" "fname is not of type symbol")
      (setf *pregen-hooks* (append *pregen-hooks* (list fname)))))

(defun install-postgen-hook (fname)
  (lm-debug "install-postgen-hook" "adding postgeneration hook")
  (if (not (equal (type-of fname) 'symbol))
      (lm-warning "install-postgen-hook" "fname is not of type symbol")
      (setf *postgen-hooks* (append *postgen-hooks* (list fname)))))

(defun pl-plugin (args)
  (if (not (equal (length args) 4))
      (lm-error "pl-plugin" "error parsing plugin def")
      (load-plugin (car args) (cadr args) (caddr args) (cadddr args))))

