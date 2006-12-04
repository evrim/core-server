(in-package :tr.gen.core.server)

;;;; you must add your use to your /etc/sudoers
;;;; file with visudo like:
;;;; evrim   ALL= NOPASSWD: ALL
(defvar +sudo+ #P"/usr/bin/sudo")
(defvar *apache-default-config-extenstion* "conf")
(defvar +cp+ #P"/usr/bin/cp")
(defvar +chown+ #P"/usr/bin/chown")
(defvar +chmod+ #P"/usr/bin/chmod")
(defvar +apache-user+ "apache")
(defvar +apache-group+ "apache")
(defvar +find+ #P"/usr/bin/find")
(defvar +rm+ #P"/usr/bin/rm")
(defvar +mkdir+ #P"/usr/bin/mkdir")
(defvar +postmap+ #P"/usr/sbin/postmap")