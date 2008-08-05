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

(in-package :cl-user)
(require :sb-posix)
(require :asdf)
(setf asdf::*central-registry* nil)
(defpackage :tr.gen.core.server.bootstrap
  (:nicknames :bootstrap)
  (:use :cl)
  (:export
   #:home
   #:in-home
   #:register-libs
   #:register-projects
   #:environment-variable-not-found))

(in-package :bootstrap)

(defparameter +verbose+ t)

(defmacro msg (&body body)
  `(when +verbose+
     ,@body))

(define-condition environment-variable-not-found (error)
  ((var :initarg :var :reader var))
  ;; (:report (lambda (c s)
;; 	     (format s "Environment variable ~A not set." (var c))))
  )

(defmethod print-object ((o environment-variable-not-found) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "var:~A" (var o))))

;; (defun set-environment-variable (c)
;;   (declare (ignorable c))
;;   (let ((restart (find-restart 'set-environment-variable)))
;;     (when restart (invoke-restart restart))))

(defun getenv (var)
  (let ((var #+sbcl (sb-posix:getenv var)
	     #+cmucl (cdr (assoc var ext:*environment-list*))))
    (if (or (null var) (equal var "")) nil var)))

(defun setenv (var val)
  #+sbcl
  (sb-posix:putenv (format nil "~A=~A" var val))
  #+cmucl
  (setf ext:*environment-list* (cons (cons var val) ext:*environment-list*)))

(defun coreserver-home-aux (var)
  (restart-case (or (getenv var)
		    (error 'environment-variable-not-found :var var))
    (set-value (val)
      :report (lambda (s) (format s "Specify a value for ~A" var))
      :interactive (lambda ()
		     (format t "Enter value:")
		     (list (read)))
      (setenv var val)
      val)
    (use-nil () 
      :report (lambda (s) (format s "Use NIL as a return value"))
      nil)))

(defun home (&optional (var "CORESERVER_HOME"))
  "Get the value of the environment variable CORESERVER_HOME"
  (coreserver-home-aux var))

(defun in-home (pathname)
  "Prepends (coreserver-home) to the given pathname"
  (merge-pathnames pathname (home)))

(defun register-libs (&optional (path #P"lib/systems/"))
  (msg (format t "Registering Libraries to asdf::*central-registry*.~%"))
  (pushnew (in-home path) asdf:*central-registry* :test #'equal))

(defun register-projects (&optional (path #P"projects/"))
  (msg (format t "Registering Projects to asdf::*central-registry*.~%"))
  (flet ((push-all (systems-dir)
	   (dolist
	       (dir-candidate
		 (directory (concatenate 'string (namestring systems-dir) "*/")))
	     (let ((name (car (last (pathname-directory dir-candidate)))))
	       (unless (equal #\_ (elt name 0))
		 (pushnew dir-candidate asdf:*central-registry* :test 'equal))))))
    (push-all (in-home path))))