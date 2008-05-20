(in-package :tr.gen.core.server)

;;;
;;; Filesystem abstraction as a service
;;;
;;; Usage: create a filesystem service,
;;;
;;;   (defparameter *fs* (make-instance 'filesystem :root *project-docs-root* :label *project-name*))
;;;
;;; Second parameter must be a relative pathname,
;;;
;;;   (writefile *fs* #P"staff/introduction.txt" "Here we go...")
;;;   (readfile  *fs* #P"staff/introduction.txt")
;;;   (list-directory *fs* #P"pictures/" :recursive t)

;; http://www.emmett.ca/~sabetts/slurp.html
(defun slurp-stream5 (stream) 
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t))) 
    (setf (fill-pointer seq) (read-sequence seq stream)) 
    seq))

(defclass filesystem (local-unit)
  ((label :accessor filesystem.label :initarg :label :initform "")
   (root :accessor filesystem.root :initarg :root :initform (error "Filesystem root must be set!"))))

;; security & validator layer
(defmacro with-guard ((fs filepath) &body body)
  (let* ((errors (gensym)))
    `(let ((,errors (list #'(lambda () (error "Filesystem null!"))
			  #'(lambda () (error "You must use relative paths!"))))
	   (ret (cond
		  ((null ,fs)
		   (function car))
		  ((and (pathname-directory ,filepath)
			(not (eq :RELATIVE (car (pathname-directory ,filepath)))))
		   (function cadr)) 
		  (t (let ((res (progn ,@body)))
		       #'(lambda (x) (declare (ignore x)) #'(lambda () res)))))))
       (funcall (funcall ret ,errors)))))

;; filesystem -> pathname -> IO String
(defmethod/unit readfile :async-no-return ((self filesystem) filepath)
  (with-guard (self filepath)
    (with-open-file (s (merge-pathnames filepath (filesystem.root self)))
      (slurp-stream5 s))))

;; filesystem -> pathname -> data -> IO String
(defmethod/unit writefile :async-no-return ((self filesystem) filepath data)
  (with-guard (self filepath)
    (with-output-to-file (s (merge-pathnames filepath
					     (filesystem.root self)))
      :if-exists :overwrite :if-does-not-exist :create
      (write-sequence data s))))

;; filesystem -> pathname -> IO Bool
(defmethod/unit deletefile :async-no-return ((self filesystem) filepath)
  (with-guard (self filepath)
    (delete-file (merge-pathnames filepath (filesystem.root self)))))

;; filesystem -> pathname -> pathname -> IO Bool
(defmethod/unit movefile :async-no-return ((self filesystem) src dst)
  (error "Not implemented yet."))

;; filesystem -> pathname -> IO [pathname]
(defmethod/unit ls :async-no-return ((self filesystem) filepath)
  (with-guard (self filepath)
    (cl-fad::list-directory (merge-pathnames filepath (filesystem.root self)))))

(defmethod/unit fold-directory :async-no-return ((self filesystem) filepath fun)
  (with-guard (self filepath)
    (cl-fad:walk-directory (merge-pathnames filepath (filesystem.root self))
			   fun
			   :directories t
			   :if-does-not-exist :error)))