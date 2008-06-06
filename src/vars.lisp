(in-package :tr.gen.core.server)

;;+----------------------------------------------------------------------------
;;| Standard Global Variables
;;+----------------------------------------------------------------------------
;; 
;; This file contains system specific global variables.


;; TODO: move them to etc/start.lisp -evrim
;; FIXmE: how does core server behave in the abcense of envvar?
(eval-always
 (if (null (sb-posix:getenv "CORESERVER_HOME"))
     (error "CORESERVER_HOME environment variable is unset. Please set.")))

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

;;-----------------------------------------------------------------------------
;; Unix Commands
;;-----------------------------------------------------------------------------
;;
;; You must add your use to your /etc/sudoers file with visudo like:
;; evrim:   ALL= NOPASSWD: ALL
;;
(defvar +sudo+ (whereis "sudo") "Sudo Pathname")
(defvar +cp+ (whereis "cp") "cp Pathname")
(defvar +chown+ (whereis "chown") "chown Pathname")
(defvar +chmod+ (whereis "chmod") "chmod Pathname")
(defvar +find+ (whereis "find") "find Pathname")
(defvar +rm+ (whereis "rm") "rm Pathname")
(defvar +mkdir+ (whereis "mkdir") "mkdir Pathname")
(defvar +sed+ (whereis "sed") "sed Pathname")

;;-----------------------------------------------------------------------------
;; Apache Specific Variables
;;-----------------------------------------------------------------------------
(defvar *apache-default-config-extenstion* "conf"
  "Apache Configuration File Extension")
(defvar +apache-user+ #-debian "apache" #+debian "www-data"
	"The user that Apache runs as")
(defvar +apache-group+ #-debian "apache" #+debian "www-data"
	"The group that Apache runs as")

;;-----------------------------------------------------------------------------
;; Postfix Specific Variables
;;-----------------------------------------------------------------------------
(defvar +postmap+ #P"/usr/sbin/postmap") ;;can't be found on all.

;;-----------------------------------------------------------------------------
;; SCm Specific Variables
;;-----------------------------------------------------------------------------
(defvar +darcs+ (whereis "darcs") "darcs Pathname")
(defvar +git+ #P"/usr/bin/git" "git Pathname") ;; can't be found on all

(defvar +remote-user+ "evrim.ulu"
  "Default ssh username for application sharing, see darcs-application.share")

;;-----------------------------------------------------------------------------
;; Web Specific Variables
;;-----------------------------------------------------------------------------
(defvar +default-extension+ ".core" "Web application default extension")
(defvar +dojo-path+ "/dojo/" "Dojo Pathname")
(defvar +fckeditor-path+ "/fckeditor/" "Fckeditor Pathname")

