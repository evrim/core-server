(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Dojo Extension
;; +----------------------------------------------------------------------------
(defpackage :tr.gen.core.server.dojo
  (:nicknames :dojo)
  (:use :common-lisp :core-server))

(in-package :dojo)

;;-----------------------------------------------------------------------------
;; Dojo Macros
;;-----------------------------------------------------------------------------
(defjsmacro $ (id)
  `(dojo.by-id ,id))

(defjsmacro $$ (id)
  `(dojo.widget.by-id ,id))

(defjsmacro debug (&rest rest)
  `(console.debug ,@rest))

(defvar +dojo-path+ "/dojo/")

;; ----------------------------------------------------------------------------
;; Dojo Stack
;; ----------------------------------------------------------------------------
(defrender/js dojo (&optional
		    base-url (debug nil) (prevent-back-button 'false)
		    (css (reduce (lambda (acc a)
				   (cons (concatenate
					  'string +dojo-path+ ".." a)
					 acc))
				 (reverse
				  '("/dijit/themes/dijit.css"
				    "/dijit/themes/tundra/tundra.css"
				    "/dojox/widget/Toaster/Toaster.css"))
				 :initial-value '("http://node1.core.gen.tr/coretal/style/coretal.css")))
		    &aux (base-url (if (and +context+ base-url)
				       (format nil "/~A/~A"
					       (web-application.fqdn (context.application +context+))
					       base-url)
				       base-url)))
  (defun load-javascript (url)
    (let ((request nil)
	  (base-url base-url))
      (cond
	(window.*x-m-l-http-request ;; Gecko
	 (setf request (new (*x-m-l-http-request))))
	(window.*active-x-object ;; Internettin Explorer
	 (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
      (if (= null request)
	  (throw (new (*error "Cannot Load Javascript, -core-server 1.0"))))
      (setf req request)
      (request.open "GET" url false)
      (request.send null)
      (if (= 200 request.status)
	  (return (eval (+ "{" request.response-text "}"))))
      (throw (new (*error (+ "Cannot load javascript:" url " -core-server 1.0"))))))
  
  (defun load-css (url)
    (let ((link (document.create-element "link")))
      (setf link.href url
	    link.rel "stylesheet"
	    link.type "text/css")
      (prepend (aref (document.get-elements-by-tag-name "head") 0) link)
      (return link)))
  
  (defun init-core-server ()
    (when (= "undefined" (typeof dojo))
      (setf dj-config (create :base-url +dojo-path+
			      :is-debug debug
			      :prevent-back-button-fix prevent-back-button
			      ;;				  :dojo-iframe-history-url "./resources/iframe_history.html"
			      ))      
      (dolist (src (array "bootstrap.js" "loader.js" "hostenv_browser.js" "loader_xd.js"))	
	(load-javascript (+ +dojo-path+ "_base/_loader/" src)))
      (load-javascript (+ +dojo-path+ "_base.js")))
    (setf base-url base-url)
    (dojo.require "dojo.back")
    (dojo.back.init)
    (mapcar (lambda (c) (load-css c)) css)
    (dojo.add-on-load
     (lambda ()
       (setf document.body.class-name (+ document.body.class-name " tundra")))))
  (defun serialize (value) (return (dojo.to-json value)))
  (defun funcall (url parameters retry-count)
    (let (result)
      (debug "server.funcall " url)
      (when (dojo.is-object parameters)
	(doeach (param parameters)
		(setf (slot-value parameters param)
		      (serialize (slot-value parameters param)))))
      (dojo.xhr-post
       (create :url (+ base-url url)
	       :handle-as "text"
	       :sync t
	       :timeout 10
	       :content parameters
	       :load (lambda (json args)
		       ;;			   (debug json)
		       (setf result (eval (+ "{" json "}"))))
	       :error (lambda (err args)
			(if (= err.status 500)				
			    (if (= "undefined" (typeof retry-count))
				(return (funcall url parameters 5))
				(if (> retry-count 0)
				    (return (funcall url parameters (- retry-count 1)))))
			    (throw (new (*error (+ "Funcall error: " url ", " err))))))))
      (return result)))
  (defun get-parameter (name)
    (debug "get-param:" name)
    (let ((params (+ (.substr window.location.hash 1) "&" (.substr window.location.search 1)))
	  (arr (params.split "&")))
      (dolist (a arr)
	(let ((key (aref (a.split "=") 0))
	      (value (aref (a.split "=") 1)))
	  (debug (+ "key:" key " val:" value))
	  (if (= (key.to-lower-case) (name.to-lower-case))
	      (return value))))))
  (defun set-parameter (name new-value)
    (let ((params (.substr window.location.hash 1))
	  (arr (params.split "&"))
	  (hash "")
	  (found nil))
      (dolist (a arr)
	(let ((key (aref (a.split "=") 0))
	      (value (aref (a.split "=") 1)))
	  (debug key value)
	  (if (not (= "undefined" (typeof key)))		  
	      (if (= (key.to-lower-case) (name.to-lower-case))
		  (setf hash (+ hash (+ key "=" new-value "&"))
			found t)
		  (setf hash (+ hash (+ key "=" (if (= "undefined" (typeof value))
						    "" value) "&")))))))
      (if (not found) (setf hash (+ hash (+ name "=" new-value))))
      (setf window.location.hash hash)
      (return new-value)))
  (init-core-server))

;; +----------------------------------------------------------------------------
;; | Dojo Components
;; +----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Toaster Component
;; ----------------------------------------------------------------------------
(defcomponent dojo-toaster-component ()
  ((toaster :host remote :initform nil)))

(defmethod/remote toast ((self dojo-toaster-component) message)
  (if (= null this.toaster)
      (progn
	(dojo.require "dojox.widget.Toaster")
	(let ((div (document.create-element "div")))
	  (document.body.append-child div)
	  (setf this.toaster (new (dojox.widget.*toaster
				   (create :position-direction "br-left"
					   :message-topic "toasterTopic"
					   :duration 1000)
				   div))))))

  (dojo.publish "toasterTopic" (array message)))


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
