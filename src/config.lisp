(in-package :tr.gen.core.server)

;;;; you must add your use to your /etc/sudoers
;;;; file with visudo like:
;;;; evrim   ALL= NOPASSWD: ALL
(defvar +sudo+ #P"/usr/bin/sudo")
(defvar *apache-default-config-extenstion* "conf")
(defvar +cp+ #-debian #P"/usr/bin/cp" #+debian #P"/bin/cp")
(defvar +chown+ #-debian #P"/usr/bin/chown" #+debian #P"/bin/chown")
(defvar +chmod+ #-debian #P"/usr/bin/chmod" #+debian #P"/bin/chmod")
(defvar +apache-user+ #-debian "apache" #+debian "www-data")
(defvar +apache-group+ #-debian "apache" #+debian "www-data")
(defvar +find+ #P"/usr/bin/find")
(defvar +rm+ #-debian #P"/usr/bin/rm" #+debian #P"/bin/rm")
(defvar +mkdir+ #-debian #P"/usr/bin/mkdir" #+debian #P"/bin/mkdir")
(defvar +postmap+ #P"/usr/sbin/postmap")
(defvar +sed+ #-debian #P"/usr/bin/sed" #+debian #P"/bin/sed")
(defvar +darcs+ #P"/usr/bin/darcs")
(defvar +git+ #P"/usr/bin/git")
(defvar +remote-user+ "evrim.ulu")

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