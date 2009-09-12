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

(in-package :core-server)

;;+----------------------------------------------------------------------------
;;| Git Application
;;+----------------------------------------------------------------------------
;;
;; This application is the extension of serializable-application to be used
;; along with SCM Git (http://git.or.cz)
;;
(defclass+ git-application (serializable-web-application)
  ()
  (:documentation "Git Application Class - A serializable-application
that uses GIT (http://git.or.cz) as SCM"))

(defun make-git-application (fqdn project-name admin-email project-pathname
			     &optional use depends-on)
  "Returns a new git-application having parameters provided"
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