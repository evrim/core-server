(in-package :core-server)

;; aycan: go functional
(defun darcs-directory (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "_darcs")) project-pathname))

(defun darcs-author-file (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "prefs") :name "author") (darcs-directory project-pathname)))

(defun darcs-init (project-pathname admin-email)
  (if (zerop (with-current-directory project-pathname
	       (sb-ext::process-exit-code
		(sb-ext::run-program "/usr/bin/darcs" '("init")))))
      (progn
	(with-output-to-file (s (darcs-author-file project-pathname))
	  (format s " <~A>" admin-email)))
      (error "darcs init failed.")))

;; <project-pathname>/project-name.asd (systems source)
(defun systems-source (project-pathname project-name)
  (merge-pathnames (make-pathname :name (string-downcase project-name) :type "asd")
		   project-pathname))

;; <project-pathname>/project-name.lisp (main source)
(defun main-source (project-pathname project-name)
  (merge-pathnames (make-pathname :name (string-downcase project-name) :type "lisp")
		   project-pathname))

;; <project-pathname>/src/model.lisp (model source)
(defun model-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name "model" :type "lisp")
		   project-pathname))

;; <project-pathname>/src/packages.lisp (packages source)
(defun packages-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name "packages" :type "lisp")
		   project-pathname))

(defun touch-file (pathname)
  (sb-ext::run-program "/usr/bin/touch" (list (format nil "~A" pathname))))

(defmethod serialize ((self darcs-web-application)) 
  (let ((ppath (make-pathname :directory (darcs-web-application.source-pathname self)))
	(pname (darcs-web-application.project-name self)))
    ;; Create project folder
    (ensure-directories-exist ppath)

    ;; Error if exists
    (when (probe-file (darcs-directory ppath))
      (error "Project already has _darcs folder."))
    
    ;; Create template folders
    (mapcar #'(lambda (dir)
		(ensure-directories-exist (merge-pathnames dir ppath)))
	    (darcs-web-application.directory-list self))

    ;; Create standard files
    (with-open-file (s (systems-source ppath pname) :if-does-not-exist :create)
      )

    (with-open-file (s (main-source ppath pname) :if-does-not-exist :create)
      )

    (with-open-file (s (model-source ppath) :if-does-not-exist :create)
      )
    
    (with-open-file (s (packages-source ppath) :if-does-not-exist :create)
      )

    ;; Initiate darcs repo and set author 
    (darcs-init ppath (web-application.admin-email self))))