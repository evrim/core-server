(in-package :core-server)
;; +----------------------------------------------------------------------------
;; | Component Framework
;; +----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; defmethod/local macro: Defines a local method
;; ----------------------------------------------------------------------------
(defmacro defmethod/local (name ((self class-name) &rest args) &body body)
  "Defines a local method and corresponding remote (javascript) proxy named 'name!'"
  (assert (not (null (find-class+ class-name))) nil
	  "Class+ ~A not found while defining defmethod/local ~A." class-name name)
  (with-unique-names (stream k)
    (let ((proxy (intern (format nil "~A!" name))))    
      `(progn
	 (class+.add-method (find-class+ ',class-name) ',name 'local '((,self ,class-name) ,@args))
	 (defmethod ,proxy ((,stream core-stream) (,self ,class-name) ,k)
	   (with-js (,k) ,stream
	     (lambda ,args
	       (funcall ,k (create
			    ,@(nreverse
			       (reduce0 (lambda (acc arg)
					  (cons arg (cons (make-keyword arg) acc)))
					(extract-argument-names args :allow-specializers t))))))))
	 (defmethod/cc ,name ((,self ,class-name) ,@args) ,@body)))))

;; ----------------------------------------------------------------------------
;; defmethod/remote macro: Defines a remote method
;; ----------------------------------------------------------------------------
(defmacro defmethod/remote (name ((self class-name) &rest args) &body body)
  "Defines a remote (javascript) method and corresponding local (lisp) proxy"
  (assert (not (null (find-class+ class-name))) nil
	  "Class+ ~A not found while defining defmethod/remote ~A." class-name name)
  (with-unique-names (stream hash)
    (let ((proxy (intern (format nil "~A!" name))))
      `(progn
	 (class+.add-method (find-class+ ',class-name) ',name 'remote '((,self ,class-name) ,@args))
	 (defmethod ,proxy ((,stream core-stream) (,self ,class-name))
	   (with-js () ,stream
	     (lambda ,args
	       ,@body)))
	 (defmethod/cc ,name ((,self ,class-name) ,@args)
	   (let ((,hash (action/url ((result "result"))
			  (answer (json-deserialize result)))))
	     (javascript/suspend
	      (lambda (,stream)
		(with-js (,hash) ,stream
		  (funcall ,hash (create :result (serialize (,name self ,@args)))))))))))))

;; +----------------------------------------------------------------------------
;; | Component Metaclass
;; +----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass component+ (class+)
    ((%ctor-timestamp :initform 0))))

;; +----------------------------------------------------------------------------
;; | Component Class
;; +----------------------------------------------------------------------------
(defclass+ component ()
  ()
  (:metaclass component+)
  (:documentation "Base component class"))

;; ----------------------------------------------------------------------------
;; defcomponent-accessors Macro: Defines remote and local accessors
;; for slots of a component
;; ----------------------------------------------------------------------------
(defmacro defcomponent-accessors (class-name)
  (flet ((reader (name) (intern (format nil "GET-~A" name)))
	 (writer (name) (intern (format nil "SET-~A" name))))
    (let ((class+ (find-class+ class-name)))
      `(progn
	 ,@(mapcar (lambda (slot)
		     (with-slotdef (name) slot
		       `(progn
			  (defmethod/local ,(reader name) ((self ,class-name))
			    (slot-value self ',name))
			  (defmethod/local ,(writer name) ((self ,class-name) value)
			    (setf (slot-value self ',name) value)))))
		   (filter (lambda (slot) (not (string= (slot-definition-name slot) 'id)))
			   (class+.local-slots class+)))
	 ,@(mapcar (lambda (slot)
		     (with-slotdef (name) slot
		       `(progn
			  (defmethod/remote ,(reader name) ((self ,class-name))
			    (slot-value self ',name))
			  (defmethod/remote ,(writer name) ((self ,class-name) value)
			    (setf (slot-value self ',name) value)))))
		   (class+.remote-slots class+))))))

;; ----------------------------------------------------------------------------
;; defcomponent-ctor Macro: Defines remote constructor for a component
;; ----------------------------------------------------------------------------
(defmacro defcomponent-ctor (class-name)
  (let* ((class+ (find-class+ class-name))
	 (k-urls (mapcar (lambda (m) (declare (ignore m)) (gensym))
			 (class+.local-methods class+)))
	 (remote-slots (mapcar (lambda (slot)
				 (with-slotdef (name reader initarg) slot
				   (list name (intern (symbol-name (gensym)))
					 reader (intern (symbol-name initarg)))))
			       (class+.remote-slots class+)))
	 (prototype `(slot-value ,class-name 'prototype)))
    `(progn
       ;; ----------------------------------------------------------------------------
       ;; Component Internal Render Method 
       ;; ----------------------------------------------------------------------------
       (defmethod %ctor! ((stream core-stream) (component ,class-name) &rest k-urls)
	 (destructuring-bind (,@k-urls) k-urls
	   (with-accessors (,@(mapcar (lambda (slot) (list (cadr slot) (caddr slot)))
				      remote-slots)) component
	     (with-js (,@k-urls ,@(mapcar #'cadr remote-slots)) stream
	       ;; ----------------------------------------------------------------------------
	       ;; Constructor
	       ;; ----------------------------------------------------------------------------
	       (defun ,class-name (,@(mapcar #'cadddr remote-slots))
		 ,@(mapcar (lambda (slot)
			     `(if (not (typep ,(cadddr slot) 'undefined))
				  (setf (slot-value this ',(car slot)) ,(cadddr slot))))
			   remote-slots)
		 this.prototype)
	       
	       ;; ----------------------------------------------------------------------------
	       ;; Prototype
	       ;; ----------------------------------------------------------------------------
	       (setf ,prototype (new (*object)))
	       
	       ;; ----------------------------------------------------------------------------
	       ;; Remote Slot Initial Values
	       ;; ----------------------------------------------------------------------------
	       ,@(mapcar (lambda (slot)
			   `(setf (slot-value ,prototype ',(car slot)) ,(cadr slot)))
			 remote-slots)
	       
	       ;; ----------------------------------------------------------------------------
	       ;; Remote Methods
	       ;; ----------------------------------------------------------------------------
	       ,@(mapcar (lambda (method)
			   (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
			     `(setf (slot-value ,prototype ',(car method))
				    (stream-escape (,proxy component)))))
			 (class+.remote-methods class+))
	       
	       ;; ----------------------------------------------------------------------------
	       ;; Local Methods
	       ;; ----------------------------------------------------------------------------
	       ,@(mapcar (lambda (method k-url)
			   (let ((proxy (intern (format nil "~A!" (car method)) :keyword)))
			     `(setf (slot-value ,prototype ',(car method))
				    (stream-escape (,proxy component ,k-url)))))
			 (class+.local-methods class+) k-urls)))))
       
       ;; ----------------------------------------------------------------------------
       ;; Component Constructor Renderer
       ;; ----------------------------------------------------------------------------
       (defmethod/cc ctor! ((stream core-stream) (component ,class-name))
	 (let ,(mapcar (lambda (method k-url)
			 (let ((method-args (extract-argument-names (cdddr method)
								    :allow-specializers t)))
			   `(,k-url (action/url ,(reduce0 (lambda (acc arg)
							    (cons (list arg (symbol-name arg))
								  acc))
							  method-args)				       
				      (json/suspend
					(lambda (stream)
					  (json! stream
						 (apply (function ,(car method))
							(cons component
							 (list ,@(mapcar (lambda (arg)
									   `(json-deserialize ,arg))
									 method-args)))))))))))
		       (class+.local-methods class+) k-urls)
	   (apply #'%ctor! (cons stream (cons component (list ,@k-urls)))))))))

;; ----------------------------------------------------------------------------
;; This around method allows us to compile constructor evertime class
;; definition changed.
;; ----------------------------------------------------------------------------
(defmethod/cc ctor! :around ((stream core-stream) (component component))
  (if  (> (slot-value (class-of component) '%timestamp)
	  (slot-value (class-of component) '%ctor-timestamp))      
       (let ((name (class-name (class-of component))))
	 (format *standard-output* "Compiling constructor for ~A.~%" name)
	 (setf (slot-value (class-of component) '%ctor-timestamp)
	       (get-universal-time))
	 (eval `(defcomponent-ctor ,name))
	 (ctor! stream component))
       (call-next-method stream component)))

(defmethod/cc ctor! ((stream core-stream) (component component))
  (error "This ctor! method should not be called."))

;; +----------------------------------------------------------------------------
;; | defcomponent Macro: Defines a new component
;; +----------------------------------------------------------------------------
(defmacro defcomponent (name supers slots &rest rest)
  `(progn     
     (defclass+ ,name (,@supers component)
       ,slots
       (:metaclass component+)
       ,@rest)
     (defcomponent-accessors ,name)
     (find-class+ ',name)))

;;-----------------------------------------------------------------------------
;; Component Protocol
;;-----------------------------------------------------------------------------
(defmethod component.application ((self component))
  "Returns application associated with this component."
  (context.application +context+))

(defmethod/remote to-json ((self component) object)
  (labels ((serialize (object)
	     (cond
	       ((typep object 'number)
		(+ "\"" object "\""))
	       ((typep object 'string)
		(+ "\"" (encode-u-r-i-component object) "\""))
	       ((typep object 'array)
		(+ "["		   
		   (.join (mapcar (lambda (item) (serialize item))
				  array)
			  ",")
		   "]"))
	       ((type object 'object)
		(doeach (i object)
			()))
	       (t
		(throw (new (*error (+ "Could not serialize " object))))))))
    (serialize object)))

(defmethod/remote funkall ((self component) action arguments)
  (let ((xhr (or (and window.*active-x-object
		      (new (*active-x-object "Microsoft.XMLHTTP")))
		 (new (*x-m-l-http-request)))))
    (xhr.open "POST" action false)
    (xhr.send (to-json self arguments))
    (if (= 200 xhr.status)
	(eval (+"{" xhr.response-text "}"))
	(throw (new (*error xhr.status))))))

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
      (.append-child (aref (document.get-elements-by-tag-name "head") 0) link)
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

;; ;;;;
;; ;;;; Interface for remote services
;; ;;;;
;; (defcomponent ajax-mixin ()
;;   ())

;; ;; TODO: first create activexobject, catch exception then create xmlhttprequest.
;; (defmethod/remote make-request ((self ajax-mixin))
;;   ;; (cond
;;   ;;       (window.*x-m-l-http-request ;; Gecko
;;   ;;        (setf request (new (*x-m-l-http-request))))
;;   ;;       (window.*active-x-object ;; Internettin Explorer
;;   ;;        (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
;;   ;;     (if (= null request)
;;   ;; 	(throw (new (*error "Exception: Cannot find usable XmlHttpRequest method, -core-server 1.0")))
;;   ;; 	(return request))
;;   (let ((req null))
;;     (try (setf req (new (*active-x-object "Msxml2.XMLHTTP")))
;; 	 (:catch (e1)
;; 	   (try (setf req (new (*active-x-object "Microsoft.XMLHTTP")))
;; 		(:catch (e2)
;; 		  (setf req null)))))
;;     (if (and (not req) (not (= (typeof *x-m-l-http-request) "undefined")))
;; 	(setf req (new (*x-m-l-http-request))))
;;     (return req)))

;; ;; return response directly, don't eval (text? xml?).
;; (defmethod/remote send-request ((self ajax-mixin) request url)
;;   (request.open "GET" url false)
;;   (request.send null)
;;   (if (= 200 request.status)
;;       (return request)
;;       (throw (new (*error (+ "Exception: Cannot send XmlHttpRequest: " url " -core-server 1.0"))))))

;; (defcomponent jqueryapi (ajax-mixin)
;;   ((script-location :host remote
;; 		    :initform "jquery-latest.min.js"
;; 		    :initarg :script-location
;; 		    :documentation "jQuery script location as url")))

;; (defmethod/remote init ((self jqueryapi))
;;   (when (= "undefined" (typeof j-query))
;;     (let ((req (this.make-request))
;; 	  (resp (this.send-request req this.script-location)))
;;       (return (eval (+ "{" resp.response-text "}"))))))

;; ;; TODO: implement retrycount, possibly using $.ajax.
;; (defmethod/remote jqueryfuncall ((self jqueryapi) url parameters retry-count)
;;   (let (result)
;;     (debug "server.funcall " url)
;;     ($.post url
;; 	    parameters
;; 	    (lambda (data textstatus)
;; 	      (setf result (eval (+ "{" data "}"))))
;; 	    "json")
;;     (return result)))

;; (defun/cc jquery (&optional scriptlocation)  
;;   (send/component (make-instance 'jqueryapi :script-location scriptlocation))
;;   ;; (<:js
;; ;;     `(progn
;; ;;        (setf jqueryapi (new (jqueryapi)))
;; ;;        (defun funcall (url parameters retry-count)
;; ;; 	 (return (jqueryapi.jqueryfuncall url parameters retry-count)))
;; ;;        (jqueryapi.init)))
;;   (error "fix jquery")
;;   )

;; (defun dojo2 (&optional base-url (debug nil) (prevent-back-button 'false)
;; 	      (css (reduce (lambda (acc a)
;; 			     (cons (concatenate
;; 				    'string +dojo-path+ ".." a)
;; 				   acc))
;; 			   (reverse
;; 			    '("/dijit/themes/dijit.css"
;; 			      "/dijit/themes/tundra/tundra.css"
;; 			      "/dojox/widget/Toaster/Toaster.css"))
;; 			   :initial-value '("http://node1.core.gen.tr/coretal/style/coretal.css"))))
;;   (js*
;;    `(progn
;;       (defun load-javascript (url)
;; 	(let ((request nil))
;; 	  (cond
;; 	    (window.*x-m-l-http-request ;; Gecko
;; 	     (setf request (new (*x-m-l-http-request))))
;; 	    (window.*active-x-object ;; Internettin Explorer
;; 	     (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
;; 	  (if (= null request)
;; 	      (throw (new (*error "Cannot Load Javascript, -core-server 1.0"))))
;; 	  (setf req request)
;; 	  (request.open "GET" url false)
;; 	  (request.send null)
;; 	  (if (= 200 request.status)
;; 	      (return (eval (+ "{" request.response-text "}"))))
;; 	  (throw (new (*error (+ "Cannot load javascript:" url " -core-server 1.0"))))))
;;       (defun load-css (url)
;; 	(let ((link (document.create-element "link")))
;; 	  (setf link.href url
;; 		link.rel "stylesheet"
;; 		link.type "text/css")
;; 	  (.append-child (aref (document.get-elements-by-tag-name "head") 0)
;; 			 link)
;; 	  (return link)))
;;       (defun init-core-server ()
;; 	(when (= "undefined" (typeof dojo))
;; 	  (setf dj-config (create :base-url ,+dojo-path+ :is-debug ,debug
;; 				  :prevent-back-button-fix ,prevent-back-button
;; 				  ;;				  :dojo-iframe-history-url "./resources/iframe_history.html"
;; 				  ))      
;; 	  (dolist (src (array "bootstrap.js" "loader.js" "hostenv_browser.js" "loader_xd.js"))	
;; 	    (load-javascript (+ ,+dojo-path+ "_base/_loader/" src)))
;; 	  (load-javascript (+ ,+dojo-path+ "_base.js")))
;; 	(setf base-url ,(if (and +context+ base-url)
;; 			    (format nil "/~A/~A"
;; 				    (web-application.fqdn (application +context+))
;; 				    base-url)))
;; 	(dojo.require "dojo.back")
;; 	(dojo.back.init)
;; 	,@(mapcar (lambda (c) `(load-css ,c)) css)
;; 	(dojo.add-on-load
;; 	 (lambda ()
;; 	   (setf document.body.class-name (+ document.body.class-name " tundra")))))
;;       (defun serialize (value) (return (dojo.to-json value)))
;;       (defun funcall (url parameters retry-count)
;; 	(let (result)
;; 	  (debug "server.funcall " url)
;; 	  (when (dojo.is-object parameters)
;; 	    (doeach (param parameters)
;; 		    (setf (slot-value parameters param)
;; 			  (serialize (slot-value parameters param)))))
;; 	  (dojo.xhr-post
;; 	   (create :url (+ base-url url)
;; 		   :handle-as "text"
;; 		   :sync t
;; 		   :timeout 10
;; 		   :content parameters
;; 		   :load (lambda (json args)
;; 			   ;;			   (debug json)
;; 			   (setf result (eval (+ "{" json "}"))))
;; 		   :error (lambda (err args)
;; 			    (if (= err.status 500)				
;; 				(if (= "undefined" (typeof retry-count))
;; 				    (return (funcall url parameters 5))
;; 				    (if (> retry-count 0)
;; 					(return (funcall url parameters (- retry-count 1)))))
;; 				(throw (new (*error (+ "Funcall error: " url ", " err))))))))
;; 	  (return result)))
;;       (defun get-parameter (name)
;; 	(debug "get-param:" name)
;; 	(let ((params (+ (.substr window.location.hash 1) "&" (.substr window.location.search 1)))
;; 	      (arr (params.split "&")))
;; 	  (dolist (a arr)
;; 	    (let ((key (aref (a.split "=") 0))
;; 		  (value (aref (a.split "=") 1)))
;; 	      (debug (+ "key:" key " val:" value))
;; 	      (if (= (key.to-lower-case) (name.to-lower-case))
;; 		  (return value))))))
;;       (defun set-parameter (name new-value)
;; 	(let ((params (.substr window.location.hash 1))
;; 	      (arr (params.split "&"))
;; 	      (hash "")
;; 	      (found nil))
;; 	  (dolist (a arr)
;; 	    (let ((key (aref (a.split "=") 0))
;; 		  (value (aref (a.split "=") 1)))
;; 	      (debug key value)
;; 	      (if (not (= "undefined" (typeof key)))		  
;; 		  (if (= (key.to-lower-case) (name.to-lower-case))
;; 		      (setf hash (+ hash (+ key "=" new-value "&"))
;; 			    found t)
;; 		      (setf hash (+ hash (+ key "=" (if (= "undefined" (typeof value))
;; 							"" value) "&")))))))
;; 	  (if (not found) (setf hash (+ hash (+ name "=" new-value))))
;;  	  (setf window.location.hash hash)
;;  	  (return new-value)))
;;       (init-core-server))))

;; (defun dojo-old (&optional base-url (debug nil) (prevent-back-button 'false)
;; 		 (css (reduce (lambda (acc a)
;; 				(cons (concatenate
;; 				       'string +dojo-path+ ".." a)
;; 				      acc))
;; 			      (reverse
;; 			       '("/dijit/themes/dijit.css"
;; 				 "/dijit/themes/tundra/tundra.css"
;; 				 "/dojox/widget/Toaster/Toaster.css"))
;; 			      :initial-value '("http://node1.core.gen.tr/coretal/style/coretal.css"))))
;;   (js:js*
;;    `(progn
;;       (defun load-javascript (url)
;; 	(let ((request nil))
;; 	  (cond
;; 	    (window.*x-m-l-http-request ;; Gecko
;; 	     (setf request (new (*x-m-l-http-request))))
;; 	    (window.*active-x-object ;; Internettin Explorer
;; 	     (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
;; 	  (if (= null request)
;; 	      (throw (new (*error "Cannot Load Javascript, -core-server 1.0"))))
;; 	  (setf req request)
;; 	  (request.open "GET" url false)
;; 	  (request.send null)
;; 	  (if (= 200 request.status)
;; 	      (return (eval (+ "{" request.response-text "}"))))
;; 	  (throw (new (*error (+ "Cannot load javascript:" url " -core-server 1.0"))))))
;;       (defun load-css (url)
;; 	(let ((link (document.create-element "link")))
;; 	  (setf link.href url
;; 		link.rel "stylesheet"
;; 		link.type "text/css")
;; 	  (.append-child (aref (document.get-elements-by-tag-name "head") 0)
;; 			 link)
;; 	  (return link)))
;;       (defun init-core-server ()
;; 	(when (= "undefined" (typeof dojo))
;; 	  (setf dj-config (create :base-url ,+dojo-path+ :is-debug ,debug
;; 				  :prevent-back-button-fix ,prevent-back-button
;; 				  ;;				  :dojo-iframe-history-url "./resources/iframe_history.html"
;; 				  ))      
;; 	  (dolist (src (array "bootstrap.js" "loader.js" "hostenv_browser.js" "loader_xd.js"))	
;; 	    (load-javascript (+ ,+dojo-path+ "_base/_loader/" src)))
;; 	  (load-javascript (+ ,+dojo-path+ "_base.js")))
;; 	(setf base-url ,(if (and +context+ base-url)
;; 			    (format nil "/~A/~A"
;; 				    (web-application.fqdn (application +context+))
;; 				    base-url)))
;; 	(dojo.require "dojo.back")
;; 	(dojo.back.init)
;; 	,@(mapcar (lambda (c) `(load-css ,c)) css)
;; 	(dojo.add-on-load
;; 	 (lambda ()
;; 	   (setf document.body.class-name (+ document.body.class-name " tundra")))))
;;       (defun serialize (value) (return (dojo.to-json value)))
;;       (defun funcall (url parameters retry-count)
;; 	(let (result)
;; 	  (debug "server.funcall " url)
;; 	  (when (dojo.is-object parameters)
;; 	    (doeach (param parameters)
;; 		    (setf (slot-value parameters param)
;; 			  (serialize (slot-value parameters param)))))
;; 	  (dojo.xhr-post
;; 	   (create :url (+ base-url url)
;; 		   :handle-as "text"
;; 		   :sync t
;; 		   :timeout 10
;; 		   :content parameters
;; 		   :load (lambda (json args)
;; 			   ;;			   (debug json)
;; 			   (setf result (eval (+ "{" json "}"))))
;; 		   :error (lambda (err args)
;; 			    (if (= err.status 500)				
;; 				(if (= "undefined" (typeof retry-count))
;; 				    (return (funcall url parameters 5))
;; 				    (if (> retry-count 0)
;; 					(return (funcall url parameters (- retry-count 1)))))
;; 				(throw (new (*error (+ "Funcall error: " url ", " err))))))))
;; 	  (return result)))
;;       (defun get-parameter (name)
;; 	(debug "get-param:" name)
;; 	(let ((params (+ (.substr window.location.hash 1) "&" (.substr window.location.search 1)))
;; 	      (arr (params.split "&")))
;; 	  (dolist (a arr)
;; 	    (let ((key (aref (a.split "=") 0))
;; 		  (value (aref (a.split "=") 1)))
;; 	      (debug (+ "key:" key " val:" value))
;; 	      (if (= (key.to-lower-case) (name.to-lower-case))
;; 		  (return value))))))
;;       (defun set-parameter (name new-value)
;; 	(let ((params (.substr window.location.hash 1))
;; 	      (arr (params.split "&"))
;; 	      (hash "")
;; 	      (found nil))
;; 	  (dolist (a arr)
;; 	    (let ((key (aref (a.split "=") 0))
;; 		  (value (aref (a.split "=") 1)))
;; 	      (debug key value)
;; 	      (if (not (= "undefined" (typeof key)))		  
;; 		  (if (= (key.to-lower-case) (name.to-lower-case))
;; 		      (setf hash (+ hash (+ key "=" new-value "&"))
;; 			    found t)
;; 		      (setf hash (+ hash (+ key "=" (if (= "undefined" (typeof value))
;; 							"" value) "&")))))))
;; 	  (if (not found) (setf hash (+ hash (+ name "=" new-value))))
;;  	  (setf window.location.hash hash)
;;  	  (return new-value)))
;;       (init-core-server))))

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
