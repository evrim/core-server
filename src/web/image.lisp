;; +----------------------------------------------------------------------------
;; | Image Related
;; +----------------------------------------------------------------------------
(in-package :core-server)

;; ----------------------------------------------------------------------------
;; Hedee Image Gallery
;; ----------------------------------------------------------------------------
(defun name-images-in-folder (pathname &optional (starting-from 0))
  (mapcar #'(lambda (path seq)
              (shell :cmd (whereis "mv")
                     :args (list path (make-pathname :directory (pathname-directory path)
                                                     :type (string-downcase (pathname-type path))
                                                     :name (format nil "~2,'0D" seq)))))
          (sort (cl-fad:list-directory pathname) #'string< :key (compose #'string-downcase #'namestring))
          (mapcar (curry #'+ starting-from) (seq (length (cl-fad:list-directory pathname))))))

(defun create-thumbnails (pathname &optional (height 150) (width 150))
  (let ((thumbs-path (merge-pathnames #P"thumbs/" pathname)))
    (ensure-directories-exist thumbs-path)
    (mapcar #'(lambda (path)
		(when (or (equal (pathname-type path) "jpg")
			  (equal (pathname-type path) "png")
			  (equal (pathname-type path) "gif"))
		  (shell :cmd (whereis "convert")
			 :args (list "-thumbnail"
				     (format nil "~Dx~D>" width height)
				     path
				     (make-pathname :name (pathname-name path)
						    :type "jpg"
						    :directory (pathname-directory thumbs-path)
						    :defaults path)))))
	    (cl-fad:list-directory pathname))))

(defcomponent hedee-component (toaster-component)
  ((root :host local :accessor hedee.root
         :initform (error "please specify hedee path") :initarg :root)
   (content-id :host remote :initform (error "please specify content id") :initarg :content-id)
   (offset :host remote :initform 0 :initarg :offset)
   (len :host remote :initform -1 :initarg :len)))

(defmethod images-pathname ((self hedee-component))
  (merge-pathnames (hedee.root self)
		   (apache-web-application.docroot-pathname (application self))))

(defmethod filtered-list-directory ((self hedee-component))
  (reduce (lambda (acc atom)
	    (if (or (equal (string-downcase (pathname-type atom)) "jpg")
		    (equal (string-downcase (pathname-type atom)) "jpeg")
		    (equal (string-downcase (pathname-type atom)) "png")
		    (equal (string-downcase (pathname-type atom)) "gif"))
		(cons atom acc)
		acc))
	  (cl-fad:list-directory (images-pathname self))
	  :initial-value nil))

(defmethod/local get-total ((self hedee-component))
  (length (filtered-list-directory self)))

(defmethod list-directory ((self hedee-component) offset length)
  (mapcar (lambda (atom seq) (cons seq atom))
          (nthcdr offset (filtered-list-directory self))
          (seq length)))

(defmethod/local template ((self hedee-component) offset length)
  (flet ((image-src (path)
	   (let ((absolute-path (merge-pathnames
				 (make-pathname
				  :directory (cons :absolute
						   (cdr
						    (pathname-directory
						     (pathname (hedee.root self))))))
				 path)))
	     (namestring (make-pathname :directory (cons :relative (nreverse (cons "thumbs" (nreverse (cdr (pathname-directory absolute-path))))))
					:name (pathname-name absolute-path)
					:type (pathname-type absolute-path))))))
    (apply (curry #'<:div :class "hedee-images")         	 
	   (reduce (lambda (acc atom)
		     (let ((seq (car atom))
			   (path (cdr atom)))
		       (cons (<:div :id (format nil "hedee-div-~D" (+ offset seq))
				    :class "hedee-div"
				    (<:a :id (format nil "hedee-a-~D" (+ offset seq))
					 :class "hedee-a"					 
					 (<:img :id (format nil "~A/~A.~A" (hedee.root self)
							    (pathname-name (cdr atom))
							    (pathname-type (cdr atom))) ;;(format nil "hedee-img-~D" (+ offset seq))			
						:src (image-src (cdr atom))
						:class "hedee-img")))
			     acc)))
		   (list-directory self offset length)
		   :initial-value nil))))

(defmethod/remote hide-image ((self hedee-component))
  (let ((d (dijit.by-id "hedee-dialog")))
    (if d (.hide d)))
  (return false))

(defmethod/remote show-image ((self hedee-component) image-source)
  (this.toast "Image loading, please hold...")
  (dojo.require "dijit.Dialog")
  (let ((div (document.create-element "DIV"))
	(img (document.create-element "IMG")))
    (setf div.id "hedee-dialog"
	  img.src image-source
	  img.onclick (dojo.hitch this (lambda (e) (return (this.hide-image))))
	  img.title image-source
	  img.onload (lambda (e)
		       (if (dijit.by-id "hedee-dialog")
			   (.destroy (dijit.by-id "hedee-dialog")))
		       
		       (.show (new (dijit.*dialog (create :title "Hedee Image Gallery") div)))
		       (return false)))
    (div.append-child img))
  (return false))

(defmethod/remote setup ((self hedee-component))
  (if (= "undefined" (typeof ($ this.content-id)))
      (return (this.toast (+ "Sorry, " this.content-id " not found, aborting hedee..."))))

  (.append-child ($ this.content-id) (this.template this.offset (if (> 0 this.len)
								    (this.get-total)
								    this.len)))
  (dolist (a (dojo.query (+ "#" this.content-id " a.hedee-a")))
    (dojo.connect a "onclick" this (lambda (e) (return (this.show-image e.target.id)))))
  (return t))

(defun make-hedee (path id)
  (lambda (context)
    (with-context context
      (javascript/suspend
       (lambda (stream)
	 (ctor! stream (make-instance 'hedee-component :root path :content-id id))
	 (with-js () stream	   
	   (setf hedee (new (hedee-component)))
	   (dojo.add-on-load (lambda () (hedee.setup)))))))))

;; ----------------------------------------------------------------------------
;; Flickr
;; ----------------------------------------------------------------------------
;; FIXME: Fix flickr component

;; Ref: http://www.flickr.com/services/api/request.rest.html
;;
;; The REST Endpoint URL is http://api.flickr.com/services/rest/
;;
;; http://api.flickr.com/services/rest/?method=flickr.test.echo&name=value
;;
;; To return an API response in JSON format, send a parameter "format"
;; in the request with a value of "json".

;; (defparameter +flickr-auth-url+ "http://flickr.com/services/auth/?")
;; (defparameter +flickr-rest-url+ "http://api.flickr.com/services/rest/?")
;; (defparameter +flickr-static-url-part+ "static.flickr.com/")

;; (defcomponent flicr (ajax-mixin)
;;   ((key :host local :initform "" :initarg :key
;; 	:documentation "api key")
;;    (secret :host local :initform "" :initarg :secret
;; 	   :document "secret")
;;    (format :host remote :initform "json" :initarg :format
;; 	   :documentation "response format")))

;; ;; here implement them as javascript.
;; ;;  (define (foldr f z xs)
;; ;;    (if (null? xs)
;; ;;        z
;; ;;        (f (car xs) (foldr f z (cdr xs)))))

;; ;;  (define (foldl f z xs)
;; ;;    (if (null? xs)
;; ;;        z
;; ;;        (foldl f (f z (car xs)) (cdr xs))))

;; (defmethod/remote send-query ((self flickr) method params)
;;   (let ((url (+ +flickr-rest-url+
;; 		"&method=" method
;; 		"&api_key=" (key self)
;; 		"&format=" (format self)
;; 		(if (equal (format self) "json") "&nojsoncallback=1")
;; 		(foldr (lambda (acc key)
;; 			 (+ acc "&" key "=" (aref params key)))
;; 		       ""
;; 		       params)))
;; 	(req (make-request self)))
;;     (let ((resp ((send-request self) req url)))
;;       (return
;; 	(if (eql (format self) "json")
;; 	    (eval (+ "(" resp.response-text ")"))
;; 	    resp.response-x-m-l)))))

;; (defmethod/remote nsid ((self flickr) username)
;;   (send-query "flickr.people.findByUsername" (+ "username=" username)))

;; (defmethod/remote search ((self flickr) nsid tags)
;;   (send-query "flickr.photos.search" (+ "nsid=" nsid "&tags=" tags)))

;; (defmethod/remote get-photo-url ((self flickr) photo size type)
;;   (return (+ "http://farm" photo.farm "." +flickr-static-url-part+ photo.server "/" photo.id "_" photo.secret "_" size ".jpg")))

;; (defmethod/remote get-photo-url-original ((self flickr) photo type)
;;   (return (+ "http://farm" photo.farm "." +flickr-static-url-part+ photo.server "/" photo.id "_o-" photo.secret "_o." type)))

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


;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
