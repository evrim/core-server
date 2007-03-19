(in-package :core-server)

(defun make-git-application (fqdn project-name admin-email project-pathname &optional use depends-on)
  (let ((params (list :fqdn fqdn
		      :project-name project-name
		      :admin-email admin-email
		      :project-pathname project-pathname)))
    (and use (nconc params (list :use use)))
    (and depends-on (nconc params (list :depends-on depends-on)))
    (apply #'make-instance
	   'git-application
	   params)))

(defun git (&rest args)
  (unwind-protect
       (sb-ext::process-exit-code
	(sb-ext:run-program +git+ args :output *standard-output*))))

(defmethod git-directory ((self git-application))
  (merge-pathnames (make-pathname :directory '(:relative ".git"))
		   (web-application.project-name self)))

(defmethod serialize ((self git-application))
  ;;error if already exists
  (when (probe-file (git-directory self))
    (error "Project already has .git folder."))
  (call-next-method))

(defmethod init ((self git-application))
  (unless (zerop (with-current-directory (web-application.project-pathname self)
		   (git "init")
		   (git "add" "."))) 
    (error "git init failed.")))

(defmethod share ((self git-application))
  (init self))