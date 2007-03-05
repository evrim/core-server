(in-package :core-server)

(defun make-darcs-application (fqdn project-name admin-email project-pathname &optional use depends-on)
  (let ((params (list :fqdn fqdn
		      :project-name project-name
		      :admin-email admin-email
		      :project-pathname project-pathname)))
    (and use (nconc params (list :use use)))
    (and depends-on (nconc params (list :depends-on depends-on)))
    (apply #'make-instance
	   'darcs-application
	   params)))

(defun darcs (&rest args)
  (unwind-protect
       (sb-ext::process-exit-code
	(sb-ext:run-program +darcs+ args :output *standard-output*))))

(defmethod package-keyword ((self darcs-application) &optional long)
  (or (and long (make-keyword (strcat "tr.gen.core." (web-application.project-name self))))
      (make-keyword (web-application.project-name self))))

(defmethod package-test-keyword ((self darcs-application))
  (make-keyword (format nil "~A.test" (package-keyword self))))

(defmethod model-class ((self darcs-application))  
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-model")))))

(defmethod application-class ((self darcs-application))
  (intern (format nil "~A" (string-upcase (strcat (web-application.project-name self) "-application")))))

(defmethod darcs-directory ((self darcs-application))
  (merge-pathnames (make-pathname :directory '(:relative "_darcs"))
		   (web-application.project-pathname self)))

(defmethod darcs-author-file ((self darcs-application))
  (merge-pathnames (make-pathname :directory '(:relative "prefs") :name "author") (darcs-directory self)))
  
(defmethod serialize ((self darcs-application))  
  ;; Error if exists
  (when (probe-file (darcs-directory self))
    (error "Project already has _darcs folder."))
  (call-next-method))

(defmethod share ((self darcs-application))
  (init self)
  (record self)
  (put self))

(defmethod evaluate ((self darcs-application))
  (pushnew (web-application.project-pathname self)
	   asdf:*central-registry*)
  (asdf:oos 'asdf:load-op (package-keyword self)))

(defmethod init ((self darcs-application))
  (if (zerop (with-current-directory (web-application.project-pathname self)
	       (darcs "init")))
      (progn
	(with-open-file (out (darcs-author-file self) :direction :output :if-does-not-exist :create)
	  (format out " <~A>" "bilgi@core.gen.tr")))
      (error "darcs init failed.")))

(defmethod record ((self darcs-application) &optional patch-name)
  (with-current-directory (web-application.project-pathname self)
    (darcs "record" "-m" (or patch-name "core-server checkpoint")
	   "--all" "--author" "bilgi@core.gen.tr" "--skip-long-comment" "--look-for-adds")))

(defmethod put ((self darcs-application) &optional (remote-repo (format nil "~A@node2:/home/projects/~A" 
									+remote-user+ (web-application.project-name self))))
  (with-current-directory (web-application.project-pathname self)
    (darcs "put" remote-repo)))

(defmethod push-all ((self darcs-application))
  (with-current-directory (web-application.project-pathname self)    
    (darcs "push" "--all")))

