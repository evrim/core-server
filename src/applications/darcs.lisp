(in-package :core-server)

;; aycan: go functional
(defun darcs-directory (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "_darcs")) project-pathname))

(defun darcs-author-file (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "prefs") :name "author") (darcs-directory project-pathname)))

(defun darcs-init (project-pathname admin-email)
  (if (zerop (with-current-directory project-pathname
	       (sb-ext::process-exit-code
		(sb-ext::run-program +darcs+ '("init")))))
      (progn
	(with-open-file (out (darcs-author-file project-pathname) :direction :output :if-does-not-exist :create)
	  (format out " <~A>" admin-email)))
      (error "darcs init failed.")))

;; <project-pathname>/project-name.asd (systems source)
(defun systems-source (project-pathname project-name)
  (merge-pathnames (make-pathname :name (string-downcase project-name) :type "asd")
		   project-pathname))

;; <project-pathname>/src/project-name.lisp (main source)
(defun main-source (project-pathname project-name)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name (string-downcase project-name) :type "lisp")
		   project-pathname))

;; <project-pathname>/src/model.lisp (model source)
(defun model-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name "model" :type "lisp")
		   project-pathname))

;; <project-pathname>/src/packages.lisp (packages source)
(defun packages-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name "packages" :type "lisp")
		   project-pathname))

;; <project-pathname>/src/transactions.lisp (model transactions)
(defun transactions-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name "transactions" :type "lisp")
		   project-pathname))

;; <project-pathname>/src/interfaces.lisp (model interfaces)
(defun interfaces-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src") :name "interfaces" :type "lisp")
		   project-pathname))

;; <project-pathname>/src/ui/main.lisp (main user interface)
(defun main-ui-source (project-pathname)
  (merge-pathnames (make-pathname :directory '(:relative "src" "ui") :name "main" :type "lisp")
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
    (with-open-file (out (systems-source ppath pname) :direction :output :if-does-not-exist :create) 
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/project-name.asd" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g ; s/admin-email/~A/g}"
			     (string-downcase (darcs-web-application.project-name self))
			     (web-application.admin-email self)))
	 :input in
	 :output out)))

    (with-open-file (out (main-source ppath pname) :direction :output :if-does-not-exist :create)
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/src/project-name.lisp" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g ; s/project-admin-email/~S/g ; s/project-path\\//~A/g ; s/project-fqdn/~S/g}"
			     (string-downcase (darcs-web-application.project-name self))
			     (web-application.admin-email self)
			     (string-replace-all "/" "\\/" (darcs-web-application.source-pathname self))
			     (web-application.fqdn self)))
	 :input in
	 :output out)))

    (with-open-file (out (model-source ppath) :direction :output :if-does-not-exist :create)
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/src/model.lisp" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g}"
			     (string-downcase (darcs-web-application.project-name self))))
	 :input in
	 :output out)))
    
    (with-open-file (out (packages-source ppath) :direction :output :if-does-not-exist :create)
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/src/packages.lisp" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g}"
			     (string-downcase (darcs-web-application.project-name self))))
	 :input in
	 :output out)))

    (with-open-file (out (transactions-source ppath) :direction :output :if-does-not-exist :create)
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/src/transactions.lisp" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g}"
			     (string-downcase (darcs-web-application.project-name self))))
	 :input in
	 :output out)))

    (with-open-file (out (interfaces-source ppath) :direction :output :if-does-not-exist :create)
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/src/interfaces.lisp" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g}"
			     (string-downcase (darcs-web-application.project-name self))))
	 :input in
	 :output out)))

    (with-open-file (out (main-ui-source ppath) :direction :output :if-does-not-exist :create)
      (with-open-file (in #P"/home/aycan/lisp/core-server/etc/darcs/src/ui/main.lisp" :direction :input)
	(sb-ext::run-program
	 +sed+ (list (format nil "-e {s/project-name/~A/g}"
			     (string-downcase (darcs-web-application.project-name self))))
	 :input in
	 :output out)))
    
    ;; Initiate darcs repo and set author 
    (darcs-init ppath (web-application.admin-email self))))