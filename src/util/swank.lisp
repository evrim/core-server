(in-package :swank)

(defun external-indentation-package-p (package)
  (reduce (lambda (acc atom)
	    (cond
	      (acc acc)
	      ((char= (aref atom 0) #\<) t)
	      (t acc)))
	  (package-names package) :initial-value nil))

(defun external-indentation-packages ()
  (reduce (lambda (acc package)
	    (if (external-indentation-package-p package)
		(cons package acc)
		acc))
	  (list-all-packages) :initial-value nil))

(defvar +external-indentation-packages+ (external-indentation-packages))

(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL.
The form is to be used as the `common-lisp-indent-function' property
in Emacs."
  (cond
    ((and (member (symbol-package symbol) +external-indentation-packages+)
	  (symbol-external-p symbol))
     (list (format nil "~A:~A"
		   (string-downcase (package-name (symbol-package symbol)))
		   (string-downcase symbol))
	   "tag-indent-function"))
    ((and (macro-function symbol) (not (known-to-emacs-p symbol)))
     (let ((arglist (arglist symbol)))
       (etypecase arglist
	 ((member :not-available) nil)
	 (list (list (string-downcase symbol) (macro-indentation arglist))))))
    (t nil)))

(defun update-indentation/delta-for-emacs (cache force package)
  "Update the cache and return the changes in a (SYMBOL INDENT PACKAGES) list.
If FORCE is true then check all symbols, otherwise only check symbols
belonging to PACKAGE."
  (let ((alist '()))
    (flet ((consider (symbol)
             (let ((result (symbol-indentation symbol)))
               (when result
		 (destructuring-bind (name indent) result
		   (unless (equal (gethash symbol cache) indent)
		     (setf (gethash symbol cache) indent)
		     (let ((pkgs (mapcar #'package-name (symbol-packages symbol))))
		       (push (list name indent pkgs) alist))))))))
      (cond (force
             (do-all-symbols (symbol)
               (consider symbol)))
            (t
             (do-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (consider symbol)))))
      alist)))


(defun print-indentation-lossage (&optional (stream *standard-output*))
  "Return the list of symbols whose indentation styles collide incompatibly.
Collisions are caused because package information is ignored."
  (let ((table (make-hash-table :test 'equal)))
    (flet ((name (s) (string-downcase (symbol-name s))))
      (do-all-symbols (s)
        (setf (gethash (name s) table)
              (cons s (cadr (symbol-indentation s)))))
      (let ((collisions '()))
        (do-all-symbols (s)
          (let* ((entry (gethash (name s) table))
                 (owner (car entry))
                 (indent (cdr entry)))
            (unless (or (eq s owner)
                        (equal (cadr (symbol-indentation s)) indent)
                        (and (not (fboundp s))
                             (null (macro-function s))))
              (pushnew owner collisions)
              (pushnew s collisions))))
        (if (null collisions)
            (format stream "~&No worries!~%")
            (format stream "~&Symbols with collisions:~%~{  ~S~%~}"
                    collisions))))))