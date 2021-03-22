(in-package :lispmake)

;; lispmake, written by Matthew Veety, et al.
;; (c) Matthew Veety 2012-2021. Under BSD License.

(defmacro append-to-list (lst elem)
  `(setf ,lst (append ,lst (list ,elem))))

(defun split-package-list (lst prefix)
  (let ((lista nil)
		(listb nil)
		(prefix-len (length prefix)))
	(dolist (n lst)
	  (if (if (>= (length n) prefix-len)
			  (string= prefix n :end2 prefix-len)
			  nil)
		  (append-to-list listb n)
		  (append-to-list lista n)))
	(list lista listb)))

(defun get-depends-list (package)
  (asdf:system-depends-on (asdf:find-system package)))

(defun sym-exists-in-list (lst sym)
  (if (equal lst nil)
	  nil
	  (if (equal (car lst) sym)
		  t
		  (sym-exists-in-list (cdr lst) sym))))

(defmacro add-unique-to-list (lst sym)
  `(if (not (sym-exists-in-list ,lst ,sym))
	   (append-to-list ,lst ,sym)))

(defun get-system-depends-1 (sysname prefix &optional (found-list nil) (step 0) (parent "root"))
  (let* ((deps nil)
		 (deps-lists (split-package-list
					  (get-depends-list sysname)
					  prefix))
		 (global-deps (car deps-lists))
		 (local-deps (cadr deps-lists))
		 (returned-deps nil))
	(lm-debug "get-system-depends"
			  (format nil "~%  ~A: looking at ~A from parent ~A:~%found-list = ~A"
					  step sysname parent found-list))
	(dolist (x global-deps)
	  (if (equal found-list nil)
		  (append-to-list returned-deps x)
		  (add-unique-to-list returned-deps x)))
	(if (not (equal local-deps nil))
		(dolist (sys local-deps)
		  (setf deps (get-system-depends-1 sys prefix returned-deps (+ step 1) sysname))
		  (if (not (equal deps nil))
				(dolist (y deps)
				  (add-unique-to-list returned-deps y)))))
	returned-deps))

(defun get-system-depends (sysname)
  (get-system-depends-1 sysname sysname))

(defun pl-asdf-quickload (args)
  (let* ((sysname (car args))
		 (systems (get-system-depends sysname)))
	(if (not (nilp systems))
		(dolist (sys systems)
		  (add-unique-to-list *quickloads* (string-to-symbol sys))))))
