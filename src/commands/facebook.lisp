(in-package :core-server)

;; -------------------------------------------------------------------------
;; Facebook Graph HTTP Client
;; -------------------------------------------------------------------------
;; https://developers.facebook.com/docs/reference/api/
(defvar +fb-me+ "https://graph.facebook.com/me")
(defun make-fb-me-uri (name) (format nil "~A/~A" +fb-me+ name))

(define-condition <fb:exception (error)
  ((code :initarg :code :reader <fb:exception.code)
   (type :initarg :type :reader <fb:exception.type)
   (message :initarg :message :reader <fb:exception.message))
  (:report (lambda (condition stream)
	     (with-slots (code type message) condition
	       (format stream "~A:~A:~A" code type message))))
  (:documentation "Facebook style conditions"))

;; -------------------------------------------------------------------------
;; Facebook Command
;; -------------------------------------------------------------------------
(defcommand <fb:funkall (http)
  ((token :host local :initform nil)
   (meta-data :host local :initform t))
  (:default-initargs :url "http://graph.facebook.com/"))

(defmethod run ((self <fb:funkall))
  (with-slots (token meta-data) self
    (if token
	(http.add-query self "access_token" token))

    (if meta-data
	(http.add-query self "metadata" "1"))
    
    (multiple-value-bind (result response) (call-next-method self)
      (if (eq 200 (http-response.status-code response))
	  (return-from run (values result response)))
    
      (let ((error (get-attribute result :error)))
	(if error
	    (values (make-condition '<fb:exception
				    :code (get-attribute error :code)
				    :type (get-attribute error :type)
				    :message (get-attribute error :message))
		    response)
	    (values result response))))))

;; -------------------------------------------------------------------------
;; Facebook Me Command
;; -------------------------------------------------------------------------
;; + Access to current user resources
(defcommand <fb:me (<fb:funkall)
  ()
  (:default-initargs :url +fb-me+))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +fb-me-commands+ '(friends home feed likes movies music
			     books notes permissions photos albums
			     videos events groups checkins)))

(defmacro deffb-me-commands (lst)
  (let ((lst (if (listp lst) lst (eval lst))))
    `(progn
       ,@(mapcar
	  (lambda (command)
	    (let ((name (intern (string-upcase command) (find-package :<fb))))
	      `(defcommand ,name (<fb:me)
		   ()
		 (:default-initargs :url (make-fb-me-uri '(,(symbol-name command)))))))
	  lst))))

(deffb-me-commands +fb-me-commands+)

;; one more extra, videos-uploaded.
(defcommand <fb:videos-uploaded (<fb:me)
  ()
  (:default-initargs :url (make-fb-me-uri '("videos") '("uploaded"))))

(defcommand <fb:fetch (<fb:funkall)
  ((id :host local :initform (error "Provide :id") :initarg :id)))

(defmethod http.setup-uri ((self <fb:fetch))
  (let ((uri (call-next-method self)))
    (setf (uri.paths uri) (list (list (s-v 'id))))
    uri))

(defun <fb:authenticate (token)
  (let ((user (<fb:me :token token)))
    (list :email (format nil "~A@facebook.com"
			 (get-attribute user :username))
	  :name (get-attribute user :name))))


;; (defvar +fb-me+ "https://graph.facebook.com/me")
;; (defun make-fb-me-uri (name) (format nil "~A/~A" +fb-me+ name))
