;; A simple example to demonstrate the performance of prevalence
;; database

(defpackage :poleposition
  (:use :cl :core-server :arnesi))

(in-package :poleposition)

(defclass+ pilot (object-with-id)
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
       do (pilot.add *db* :name (format nil "Pilot_~D" i) :firstname "Herkules" :points i :licenseid i))))

(defun pp-read (count)
  (loop
     for i from 1 to count
     do (pilot.find *db* :points i)))

(defun pp-delete (count)
  (with-transaction (*db*)
    (mapcar (arnesi::curry #'pilot.delete *db*) (take count (pilot.list *db*)))))

(defun test (&optional (count 3000))
  (start *db*)
  (format t "* Creating ~A objects~%" count)
  (time (pp-write count))
  (format t "* Reading ~A objects~%" count)
  (time (pp-read count))
  (format t "* Deleting ~A objects~%" count)
  (time (pp-delete count))
  nil)
