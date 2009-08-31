;; Address Book Example
;; ------------------------------------
;; Sample Database Server Demonstration

(defpackage :addressbook
  (:use :cl :core-server :arnesi))

(in-package :addressbook)

(defclass+ person ()
  ((fullname :index t)
   (fields)
   (ctime :initform (get-universal-time))))

(defcrud person)

(defparameter *addressbook*
  (make-instance 'database-server
		 :database-directory #P"/tmp/addressbook/"
		 :auto-start t))

;; Test
(defun test-book-1 (book)
  (let ((person (person.add book
			    :fullname "Johny Doe"
			    :fields '((home-phone "555-1212")
				      (mobile-phone "555-1313")
				      (address "Acme St. NY, USA")
				      (email "johndoe@example.com")))))
    (describe (person.find book :fullname "Johny Doe"))
    (setq person (person.update book person :fullname "John Doe"))
    (describe (person.find book :fullname "John Doe")))
  (snapshot book))
