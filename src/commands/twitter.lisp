(in-package :core-server)

;; https://dev.twitter.com/docs/api
(defcommand <twitter:funkall (http)
  ((cmd :host local :initform (error "Provide :cmd")))
  (:default-initargs :url "http://api.twitter.com/1/"))


(defcommand <twitter:get-user-lists (<twitter:funkall)
  ((username :host local :initform (error "Provide :username")))
  (:default-initargs
      :cmd t
      :url "http://twitter.com/goodies/list_of_lists"))

(defmethod run ((self <twitter:get-user-lists))
  (http.add-query self "screen_name" (s-v 'username))
  (let ((result (call-next-method self)))
    (awhen result
      (getf (json-deserialize (octets-to-string result :utf-8)) :lists))))