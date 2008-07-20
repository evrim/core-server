;; Address Book Example
;; ------------------------------------
;; Sample Database Server Demonstration

(defpackage :addressbook
  (:use :cl :core-server :arnesi :cl-prevalence))

(in-package :addressbook)

(defclass+ person (object-with-id)
  ((fullname :host local :initform nil)
   (fields :host local :initform nil)))

(defclass+ addressbook-model ()
  ((people :host local :type list :initform nil))
  (:ctor (&optional people)))

(defclass addressbook (database-server)
  ()
  (:default-initargs :directory #P"/tmp/addressbook/"
		     :db-auto-start nil
		     :model-class 'addressbook-model))


(defparameter *addressbook*
  (make-instance 'addressbook))

;; Queries
(defun people (addressbook)
  (addressbook-model.people (model addressbook)))

(defun find-person (addressbook fullname-or-id)  
  (find-if (curry #'equal fullname-or-id) (people addressbook)
	   :key (if (numberp fullname-or-id)
		    #'get-id
		    #'person.fullname)))

;; Transactions
(defun tx-person-add (addressbook &rest args)
  (let ((person (apply #'make-instance 'person `(:id ,(next-id addressbook) ,@args))))
    (push person (addressbook-model.people (model addressbook)))
    person))

(defun tx-person-update (addressbook fullname-or-id &rest args)
  (aif (find-person addressbook fullname-or-id)
       (prog1 it (update-slots it args))))

(defun tx-person-del (addressbook fullname-or-id)
  (aif (find-person addressbook fullname-or-id)
       (prog1 it
	 (setf (addressbook-model.people (model addressbook))
	       (remove it (addressbook-model.people (model addressbook)))))))

;; Interface
(defmethod person-add ((book addressbook) slot-vals)
  (execute book (apply #'make-tx 'tx-person-add slot-vals)))

(defmethod person-del ((book addressbook) (person person))
  (execute book (apply #'make-tx 'tx-person-del (list (person.fullname person)))))

(defmethod person-update ((book addressbook) (person person) slot-vals)
  (execute book (apply #'make-tx 'tx-person-update (person.fullname person) slot-vals)))


;; Test
(defun test-book-1 (book)
  (start book)
  (let ((person (person-add book (list :fullname "Johny Doe"
				       :fields '((home-phone "555-1212")
						 (mobile-phone "555-1313")
						 (address "Acme St. NY, USA")
						 (email "johndoe@example.com"))))))
    (describe (find-person book "Johny Doe"))
    (setq person (person-update book person (list :fullname "John Doe")))
    (describe (find-person book "John Doe")))
  (snapshot book)
  (stop book))


