(in-package :tr.gen.core.server)

(defun load-file-into-string (pathname)
  (iter (for l in-file pathname using #'read-line)
	(reducing l by #'(lambda (acc elem)
			   (concatenate 'string acc elem (format nil "~%"))))))

(defun fix-apache-permissions (pathname)
  (sb-ext:run-program +sudo+ (cons (namestring +chown+)
				   (list (format nil "~A:~A" +apache-user+ +apache-group+)					   
					 (namestring pathname))))
  (sb-ext:run-program +sudo+ (cons (namestring +chmod+)
				   (list "660" (namestring pathname)))))


;;; http://paste.lisp.org/display/9527
;;; greetz goes to andreas
(defmacro with-current-directory (dir &body body)
  `(unwind-protect (progn
                     (sb-posix:chdir ,dir)
                     (let ((*default-pathname-defaults* ,dir))
                       ,@body))
     (sb-posix:chdir *default-pathname-defaults*)))

(defmacro make-project-path (system path)
  `(merge-pathnames (make-pathname :directory '(:relative ,path))
                    (asdf:component-pathname (asdf:find-system ,system))))

;; DNS aids
(defun host-part (fqdn)
  (awhen (position #\. fqdn)
    (subseq fqdn 0 it)))

(defun domain-part (fqdn)
  (awhen (position #\. fqdn)
    (subseq fqdn (+ 1 it))))