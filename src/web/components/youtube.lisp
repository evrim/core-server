(in-package :core-server)
;;;;
;;;; Youtube
;;;;

(defparameter +youtube-rest-url+ "http://gdata.youtube.com/feeds/api/")

(defcomponent youtube (ajax-mixin)
  ((format :host remote :initform "json" :initarg :format
	   :documentation "response format")))

(defmethod/remote send-request ((self youtube) method params)
  (let ((url (+ +youtube-rest-url+))
	(req (make-request self)))
    (let ((resp ((send-request self) req url)))
      (return
	(if (eql (format self) "json")
	    (eval (+ "(" resp.response-text ")"))
	    resp.response-x-m-l)))))

(defmethod/remote get-videos ((self youtube) username)
  (send-request ""))

;; Ref: http://code.google.com/apis/youtube/developers_guide_protocol.html#User_Uploaded_Videos
;;
;; To request a feed of all videos uploaded by another user, send an
;; HTTP GET request to the following URL. Note that this request does
;; not require authentication.

;; http://gdata.youtube.com/feeds/api/users/username/uploads

;; In the URL above, you must replace the text username with the
;; user's YouTube username. You can extract the username from the
;; <name> tag in an API response as shown in the following example.

;; <author>
;;   <name>andyland74</name>
;;   <uri>http://gdata.youtube.com/feeds/api/users/andyland74</uri>
;; </author>

;; Ref: http://code.google.com/apis/gdata/json.html
;;
;; For example, to request Google's developer calendar feed in JSON
;; format, send the following query:
;; http://www.google.com/calendar/feeds/developer-calendar@google.com/public/full?alt=json
