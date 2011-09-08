(in-package :core-server)

;; https://graph.facebook.com/me/accounts?access_token=TOKEN_FROM_ABOVE
(defcommand <fb:me (http)
  ((access_token :host local :initform (error "Provide :access_token")))
  (:default-initargs :url "https://graph.facebook.com/me"))

(defmethod run ((self <fb:me))
  (http.add-query self "access_token" (s-v 'access_token))
  (call-next-method self))
