;; A simple example to demonstrate the performance of prevalence
;; database

(defpackage :poleposition
  (:use :cl :core-server :arnesi))

(in-package :poleposition)

(defclass+ pilot ()
  ((name)
   (firstname)
   (points :index t)
   (licenseid)))

(defcrud pilot)

(defparameter *db*
  (make-instance 'database-server
		 :database-directory #P"/tmp/poleposition/"
		 :auto-start t))

;;;; Executing body in one transaction
(defun pp-write (count)
  (with-transaction (*db*)
    (loop
       for i from 1 to count
       do (pilot-add *db* :name (format nil "Pilot_~D" i) :firstname "Herkules" :points i :licenseid i))))

(defun pp-read (count)
  (loop
     for i from 1 to count
     do (find-pilot *db* :points i)))

(defun pp-delete (count)
  (let ((sid (get-id (find-pilot *db* :points 1))))
   (with-transaction (*db*)
     (loop
	for i from sid to count
	do (pilot-delete *db* i)))))

(defun test ()
  (totally-destroy *db*)
  (start *db*)
  (format t "* Creating 30000 objects~%")
  (time (pp-write 30000))
  (format t "* Reading 30000 objects~%")
  (time (pp-read 30000))
  (format t "* Deleting 30000 objects~%")
  (time (pp-delete 30000)))
