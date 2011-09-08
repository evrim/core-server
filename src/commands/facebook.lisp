(in-package :core-server)

;; https://graph.facebook.com/me/accounts?access_token=TOKEN_FROM_ABOVE
(defcommand <fb:me (http)
  ((access_token :host local :initform (error "Provide :access_token")))
  (:default-initargs :url "https://graph.facebook.com/me"))

(defmethod run ((self <fb:me))
  (http.add-query self "access_token" (s-v 'access_token))
  (call-next-method self))

(defun <fb:authenticate (token)
  (let ((user (apply #'jobject (<fb:me :access_token token))))
    (list :email (format nil "~A@facebook.com"
			 (get-attribute user :username))
	  :name (get-attribute user :name))))
