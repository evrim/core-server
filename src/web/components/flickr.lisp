(in-package :core-server)
;;;;
;;;; Flickr
;;;;

;; Ref: http://www.flickr.com/services/api/request.rest.html
;;
;; The REST Endpoint URL is http://api.flickr.com/services/rest/
;;
;; http://api.flickr.com/services/rest/?method=flickr.test.echo&name=value
;;
;; To return an API response in JSON format, send a parameter "format"
;; in the request with a value of "json".

(defparameter +flickr-auth-url+ "http://flickr.com/services/auth/?")
(defparameter +flickr-rest-url+ "http://api.flickr.com/services/rest/?")
(defparameter +flickr-static-url-part+ "static.flickr.com/")

(defcomponent flicr (ajax-mixin)
  ((key :host local :initform "" :initarg :key
	:documentation "api key")
   (secret :host local :initform "" :initarg :secret
	   :document "secret")
   (format :host remote :initform "json" :initarg :format
	   :documentation "response format")))

;; here implement them as javascript.
;;  (define (foldr f z xs)
;;    (if (null? xs)
;;        z
;;        (f (car xs) (foldr f z (cdr xs)))))

;;  (define (foldl f z xs)
;;    (if (null? xs)
;;        z
;;        (foldl f (f z (car xs)) (cdr xs))))

(defmethod/remote send-query ((self flickr) method params)
  (let ((url (+ +flickr-rest-url+
		"&method=" method
		"&api_key=" (key self)
		"&format=" (format self)
		(if (equal (format self) "json") "&nojsoncallback=1")
		(foldr (lambda (acc key)
			 (+ acc "&" key "=" (aref params key)))
		       ""
		       params)))
	(req (make-request self)))
    (let ((resp ((send-request self) req url)))
      (return
	(if (eql (format self) "json")
	    (eval (+ "(" resp.response-text ")"))
	    resp.response-x-m-l)))))

(defmethod/remote nsid ((self flickr) username)
  (send-query "flickr.people.findByUsername" (+ "username=" username)))

(defmethod/remote search ((self flickr) nsid tags)
  (send-query "flickr.photos.search" (+ "nsid=" nsid "&tags=" tags)))

(defmethod/remote get-photo-url ((self flickr) photo size type)
  (return (+ "http://farm" photo.farm "." +flickr-static-url-part+ photo.server "/" photo.id "_" photo.secret "_" size ".jpg")))

(defmethod/remote get-photo-url-original ((self flickr) photo type)
  (return (+ "http://farm" photo.farm "." +flickr-static-url-part+ photo.server "/" photo.id "_o-" photo.secret "_o." type)))

;; "http://api.flickr.com/services/rest/?api_key=~A&format=~A&method=~A"
;; http://api.flickr.com/services/rest/?method=flickr.people.findByUsername&api_key=9d8b5b16b8b7ae6d0060f3cc24c6868f&username=c0r3&format=json


;; bir kullanıcının resimleri için önce o kullanıcının nsid'sini almamız lazım:
;; http://api.flickr.com/services/rest/?api_key=9d8b5b16b8b7ae6d0060f3cc24c6868f&format=json&method=flickr.people.findByUsername&username=~A&
;;; jsonFlickrApi({"user":{"id":"26659297@N03", "nsid":"26659297@N03", "username":{"_content":"c0r3"}}, "stat":"ok"})

;; daha sonra bu nsid'yi kaydedeceğiz ve bununla bir arama sorgusu yapacağız.
;; http://api.flickr.com/services/rest/?api_key=9d8b5b16b8b7ae6d0060f3cc24c6868f&format=json&method=flickr.photos.search&user_id=8714164@N03

;; jsonFlickrApi({"photos":{"page":1, "pages":1, "perpage":100, "total":"27", "photo":[{"id":"1607497377", "owner":"8714164@N03", "secret":"e86b25917f", "server":"2005", "farm":3, "title":"6649235-Full", "ispublic":1, "isfriend":0, "isfamily":0},
;; {"id":"1607496947", "owner":"8714164@N03", "secret":"efacb636b9", "server":"2209", "farm":3, "title":"See
;; You in Hell (postcard)", "ispublic":1, "isfriend":0, "isfamily":0},
;; {"id":"1607927086", "owner":"8714164@N03", "secret":"d5167dde4a", "server":"2264", "farm":3, "title":"6318226-Full", "ispublic":1, "isfriend":0, "isfamily":0},
;; {"id":"1591179736", "owner":"8714164@N03", "secret":"063f6ceb5c", "server":"2008", "farm":3, "title":"6140418-Full", "ispublic":1, "isfriend":0, "isfamily":0},
;; ...]}, "stat":"ok"})

;; daha sonra bunları resim url'lerine çeviricez
;; http://www.flickr.com/services/api/misc.urls.html

;; http://farm1.static.flickr.com/2/1418878_1e92283336_m.jpg
;; farm-id: 1
;; server-id: 2
;; photo-id: 1418878
;; secret: 1e92283336
;; size: m
