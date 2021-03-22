(in-package :lispmake)

;; lispmake, written by Matthew Veety, et al.
;; (c) Matthew Veety 2012-2021. Under BSD License.

(defvar *debugging* nil)
(defvar *lispmake-version* 14)
(defvar *sources* nil)
(defvar *outfile* nil)
(defvar *lm-package* nil)
(defvar *toplevel* nil)
(defvar *quickloads* nil)
(defvar *generate* nil)
(defvar *compile-files* nil)
#+FreeBSD (defvar *lisp-executable* "/usr/local/bin/sbcl")
#+Linux (defvar *lisp-executable* "/usr/bin/sbcl")
(defvar *do-build* nil)
(defvar *do-build-override* nil)
(defvar *lisp-target* 'default)
(defvar *plugins* nil)
(defvar *pregen-hooks* nil)
(defvar *postgen-hooks* nil)
(defvar *lmakefile* "LMakefile")
(defvar *target* nil)

(defun output-fname ()
  (if (eq *target* nil)
      "run-build.lisp"
      (format nil "run-~A.lisp" *target*)))
