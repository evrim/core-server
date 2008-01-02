(in-package :tr.gen.core.server)

(eval-always
  (when (null (sb-posix:getenv "CORESERVER_HOME"))
    (error "CORESERVER_HOME environment variable is unset. Can't continue.")))

;; +Global Configurations

;;;; you must add your use to your /etc/sudoers
;;;; file with visudo like:
;;;; evrim   ALL= NOPASSWD: ALL
(defvar +sudo+ (whereis "sudo"))
(defvar *apache-default-config-extenstion* "conf")
(defvar +cp+ (whereis "cp"))
(defvar +chown+ (whereis "chown"))
(defvar +chmod+ (whereis "chmod"))
(defvar +apache-user+ #-debian "apache" #+debian "www-data")
(defvar +apache-group+ #-debian "apache" #+debian "www-data")
(defvar +find+ (whereis "find"))
(defvar +rm+ (whereis "rm"))
(defvar +mkdir+ (whereis "mkdir"))
(defvar +postmap+ #P"/usr/sbin/postmap") ;;can't be found on all.
(defvar +sed+ (whereis "sed"))
(defvar +darcs+ (whereis "darcs"))
(defvar +git+ #P"/usr/bin/git") ;; can't be found on all
(defvar +remote-user+ "evrim.ulu")

;; +Web Related Configurations
(defvar +dojo-path+ "/dojo/")
(defvar +fckeditor-path+ "/fckeditor/")
(defvar +default-extension+ ".core")

;;; below images are inserted when a field is valid/invalid which
;;; is decided by series of form-field validators.
(defparameter *valid-field-image* "/dojo/demos/widget/Mail/ok.gif")
(defparameter *invalid-field-image* "/dojo/demos/widget/Mail/cancel.gif")
(defparameter *validation-css* "/* Validator Stylesheets */
span.valid {color: #000;} 
span.invalid {color: #cf2229; font-weight: bold;} 
span.invalid img {height:20px; width: 20px;}
span.valid img {height:20px; width: 20px;} 
span.valid, span.invalid {margin-left: 1px;}
")

(defun load-end.lisp (&rest args)
  (declare (ignore args))
  (load (merge-pathnames "etc/end.lisp"
			 (sb-posix:getenv "CORESERVER_HOME"))))

#+sbcl
(progn
  (require :sb-posix)
  (sb-unix::enable-interrupt sb-posix:sigterm #'core-server::load-end.lisp)
  (with-open-file (s (merge-pathnames #P"var/core-server.pid"
				      (pathname (sb-posix:getenv "CORESERVER_HOME")))
		     :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (format s "~D" (sb-posix:getpid))))