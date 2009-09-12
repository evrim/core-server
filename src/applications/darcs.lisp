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
;;| Darcs Application
;;+----------------------------------------------------------------------------
;;
;; This application is the extension of serializable-application to be used
;; along with SCM Darcs (http://darcs.net)
;;
(defclass darcs-application (serializable-web-application)
  ()
  (:documentation "Darcs Application Class - A
serializable-application that uses Darcs (http://darcs.net) as SCM"))

(defun make-darcs-application (fqdn project-name admin-email project-pathname
			       &optional use depends-on)
  "Returns a new darcs-aplication having parameters provided"
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

(defmethod darcs-directory ((self darcs-application))
  (merge-pathnames (make-pathname :directory '(:relative "_darcs"))
		   (web-application.project-pathname self)))

(defmethod darcs-author-file ((self darcs-application))
  (merge-pathnames (make-pathname :directory '(:relative "prefs") :name "author")
		   (darcs-directory self)))
  
(defmethod serialize ((self darcs-application))  
  ;; Error if exists
  (when (probe-file (darcs-directory self))
    (error "Project already has _darcs folder."))
  (call-next-method))

(defmethod share ((self darcs-application))
  (init self)
  (record self)
  (put self))

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

(defmethod put ((self darcs-application)
		&optional (remote-repo (format nil "~A@node2:/home/projects/~A" 
					       +remote-user+ (web-application.project-name self))))
  (with-current-directory (web-application.project-pathname self)
    (darcs "put" remote-repo)))

(defmethod push-all ((self darcs-application))
  (with-current-directory (web-application.project-pathname self)    
    (darcs "push" "--all")))

