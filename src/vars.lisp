;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :tr.gen.core.server)

;;+--------------------------------------------------------------------------
;;| Standard Global Variables
;;+--------------------------------------------------------------------------
;; 
;; This file contains system specific global variables.


(defun load-end.lisp (&rest args)
  (declare (ignore args))
  (load (merge-pathnames "etc/end.lisp"
			 (bootstrap:home))))

#+sbcl
(progn
  (require :sb-posix)
  (sb-unix::enable-interrupt sb-posix:sigterm #'core-server::load-end.lisp)
  (with-open-file (s (merge-pathnames #P"var/core-server.pid"
				      (bootstrap:home))
		     :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (format s "~D" (sb-posix:getpid))))

;; --------------------------------------------------------------------------
;; Temporary Directory
;; --------------------------------------------------------------------------
(defparameter +tmp+ (make-pathname :directory '(:absolute "tmp"))
  "Temporary directory")

;;---------------------------------------------------------------------------
;; Unix Commands
;;---------------------------------------------------------------------------
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

;;---------------------------------------------------------------------------
;; Apache Specific Variables
;;---------------------------------------------------------------------------
(defvar *apache-default-config-extenstion* "conf"
  "Apache Configuration File Extension")
(defvar +apache-user+ #-debian "apache" #+debian "www-data"
	"The user that Apache runs as")
(defvar +apache-group+ #-debian "apache" #+debian "www-data"
	"The group that Apache runs as")

;;---------------------------------------------------------------------------
;; Postfix Specific Variables
;;---------------------------------------------------------------------------
(defvar +postmap+ (which :name "postmap" :errorp nil))

;;---------------------------------------------------------------------------
;; DNS Specific Variables
;;---------------------------------------------------------------------------
(defvar +ns1+ "139.179.139.251"
  "IP address of the first nameserver")

(defvar +ns2+ "212.175.40.11"
  "IP address of the second nameserver")

(defvar +mx+ "212.175.40.55"
  "IP address of the mail exchanger")

;;---------------------------------------------------------------------------
;; SCM Specific Variables
;;---------------------------------------------------------------------------
(defvar +darcs+ (whereis "darcs") "darcs Pathname")
(defvar +git+ #P"/usr/bin/git" "git Pathname") ;; can't be found on all

(defvar +remote-user+ "evrim.ulu"
  "Default ssh username for application sharing, see darcs-application.share")

;;---------------------------------------------------------------------------
;; Web Specific Variables
;;---------------------------------------------------------------------------
(defvar +loading-gif+ "style/images/loading.gif")
(defvar +default-extension+ ".core" "Web application default extension")

;; -------------------------------------------------------------------------
;; JQuery Variables
;; -------------------------------------------------------------------------
(defvar +jquery.js+ "/js/jquery.min.js")
(defvar +jquery-ui.js+ "/js/jquery-ui.min.js")
(defvar +jquery-ui.css+ "/js/jquery-ui/css/blitzer/jquery-ui.custom.css")
(defvar +jquery-lightbox.js+ "/js/lightbox/js/jquery.lightbox-0.5.min.js")
(defvar +jquery-lightbox.css+ "/js/lightbox/css/jquery.lightbox-0.5.css")

(defvar +jquery-carousel.js+ "/js/jcarousel/lib/jquery.jcarousel.min.js")
(defvar +jquery-carousel.css+ "/js/jcarousel/skins/tango/skin.css")

(defvar +jquery-nested-sortable.js+ "/js/nested-sortable/jquery.ui.nestedSortable.js")
(defvar +jquery-newsticker.js+ "/js/jquery.newsTicker.js")
(defvar +jquery-slider.js+ "/js/slider/slider.js")
(defvar +jquery-slider.css+ "/js/slider/slider.css")
(defvar +jquery-text-effects.js+ "/js/slider/jquery.text-effects.js")

(defvar +jquery-tree.js+ "/js/jquery-tree/jquery.tree.js")
(defvar +jquery-cookie.js+ "/js/jquery-tree/jquery.cookie.js")

(defvar +jquery-date-time-picker.js+ "/js/jquery-ui-timepicker-addon.js")
(defvar +jquery-date-time-picker.css+ "/js/jquery-ui-timepicker-addon.css")

(defvar +ckeditor.js+ "/js/ckeditor/ckeditor.js")
(defvar +ckeditor-source.js+ "/js/ckeditor/ckeditor_source.js")
(defvar +ckeditor.css+ "/style/ckeditor.css")

;; -------------------------------------------------------------------------
;; Component Specific Stylesheets
;; -------------------------------------------------------------------------
(defvar +theme-root+ "style/themes/")
(defvar +default-theme+ "default")
(defvar +core.css+ "style/core.css")
(defvar +tab.css+ "style/tab.css")
(defvar +console.css+ "style/console.css")
(defvar +toaster.css+ "style/toaster.css")
(defvar +taskbar.css+ "style/taskbar.css")
(defvar +sidebar.css+ "style/sidebar.css")
(defvar +page-plugin.css+ "style/page.css")

(defvar +table.css+ "style/table.css")
(defvar +dialog.css+ "/style/dialog/dialog.css")
(defvar +crud.css+ "style/crud.css")

;; -------------------------------------------------------------------------
;; Picasa Variables
;; -------------------------------------------------------------------------
(defvar +picasa-user-format-string+
  "http://picasaweb.google.com/data/feed/api/user/~A")
(defparameter +picasa-album-format-string+
  "http://picasaweb.google.com/data/feed/api/user/~A/albumid/~A?imgmax=640")

;;--------------------------------------------------------------------------
;; Mail Service Variables
;;--------------------------------------------------------------------------
(defvar +x-mailer+ "[Core-serveR] (http://labs.core.gen.tr)")
(defvar +x-http-client+ +x-mailer+)