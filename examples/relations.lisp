;; A simple example demonstrating relations between objects
;;
;;
;; (progn (test *db*) (list *db*))
(defpackage :relations
  (:use :cl :core-server :arnesi))

(in-package :relations)

(defclass+ category (object-with-id)
  ((objects :type cobject* :relation category)
   (label :print t)))

(defclass+ cobject (object-with-id)
  ((category :type category :relation objects)
   (name :print t)))

(defcrud category)
(defcrud cobject)

(defparameter *db*
  (make-instance 'database-server
		 :database-directory #P"/tmp/relations/"
		 :auto-start t))

(defun make-new-category (db label objs)
  (let ((cat (category.add db :label label)))
    (mapcar #'(lambda (c)
                (cobject.add db :name c :category cat))
            objs)))

(defun test (db)
  (start db)
  (make-new-category db "Alphabet" (list #\A #\B #\C #\D #\E #\F))
  (make-new-category db "Numbers" (list 1 2 3 4 5 6))
  (stop db))

(defun list-db (db)
  (start db)
  (mapcar #'describe (cobject.list db))
  (stop db))

