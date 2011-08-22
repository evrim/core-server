(in-package :core-server)

;; -------------------------------------------------------------------------
;; Supply JQuery
;; -------------------------------------------------------------------------
(defcomponent supply-jquery ()
  ((jquery-uri :host remote :initform +jquery-uri+)))

(defmethod/remote load-jquery ((self supply-jquery))
  (load-javascript (jquery-uri self) (lambda () (not (null j-query)))))

;; -------------------------------------------------------------------------
;; Supply Jquery UI
;; -------------------------------------------------------------------------
(defcomponent supply-jquery-ui (supply-jquery)
  ((jquery-ui-uri :host remote :initform +jquery-ui-uri+)))

(defmethod/remote load-jquery-ui ((self supply-jquery-ui))
  (load-jquery self)
  (load-javascript (jquery-ui-uri self)
		   (lambda () (not (null j-query.fn.accordion)))))

;; -------------------------------------------------------------------------
;; Supply LightBox
;; -------------------------------------------------------------------------
(defvar +jquery-lightbox-config+
  (jobject :image-loading "/js/lightbox/images/lightbox-ico-loading.gif"
	   :image-btn-prev "/js/lightbox/images/lightbox-btn-prev.gif"
	   :image-btn-next "/js/lightbox/images/lightbox-btn-next.gif"
	   :image-btn-close "/js/lightbox/images/lightbox-btn-close.gif"
	   :image-blank "/js/lightbox/images/lightbox-blank.gif"))

(defcomponent supply-jquery-lightbox (supply-jquery)
  ((lightbox-uri :host remote :Initform +jquery-lightbox-uri+)
   (lightbox-css-uri :host remote :initform +jquery-lightbox-css-uri+)
   (lightbox-config :host remote :initform +jquery-lightbox-config+)))

(defmethod/remote load-jquery-lightbox ((self supply-jquery-lightbox))
  (load-jquery self)
  (load-javascript (lightbox-uri self) (lambda ()
					 (not (null j-query.fn.light-box))))
  (load-css (lightbox-css-uri self)))


;; -------------------------------------------------------------------------
;; Supply Nested Sortable
;; -------------------------------------------------------------------------
(defcomponent supply-jquery-nested-sortable (supply-jquery-ui)
  ((nested-sortable-uri :host remote :initform +jquery-nested-sortable-uri+)))

(defmethod/remote load-jquery-nested-sortable ((self supply-jquery-nested-sortable))
  (load-jquery-ui self)
  (load-javascript (nested-sortable-uri self)
		   (lambda () (not (null j-query.ui.nested-sortable)))))

;; -------------------------------------------------------------------------
;; Supply Newsticker
;; -------------------------------------------------------------------------
(defcomponent supply-jquery-newsticker (supply-jquery)
  ((newsticker-uri :host remote :initform +jquery-newsticker-uri+)))

(defmethod/remote load-jquery-newsticker ((self supply-jquery-newsticker))
  (load-jquery self)
  (load-javascript (newsticker-uri self)
		   (lambda () (not (null j-query.fn.news-ticker)))))

;; -------------------------------------------------------------------------
;; Supply Slider
;; -------------------------------------------------------------------------
(defcomponent supply-jquery-slider (supply-jquery)
  ((slider-uri :host remote :initform +jquery-slider-uri+)
   (slider-css :host remote :initform +jquery-slider-css+)))

(defmethod/remote load-jquery-slider ((self supply-jquery-newsticker))
  (load-jquery self)
  (load-css (slider-css self))
  (load-javascript (slider-uri self)
		   (lambda () (not (null j-query.fn.slider1)))))

;; -------------------------------------------------------------------------
;; Supply Jquery Text Effects
;; -------------------------------------------------------------------------
(defcomponent supply-jquery-text-effects (supply-jquery)
  ((text-effects-uri :host remote :initform +jquery-text-effects-uri+)))

(defmethod/remote load-jquery-text-effects ((self supply-jquery-text-effects))
  (load-jquery self)
  (load-javascript (text-effects-uri self)
		   (lambda () (not (null j-query.fn.unscramble)))))

;; ;; +----------------------------------------------------------------------------
;; ;; | Jquery Extension
;; ;; +----------------------------------------------------------------------------
;; (defpackage :tr.gen.core.server.jquery
;;   (:nicknames :jquery :<jquery)
;;   (:use :common-lisp :core-server)
;; ;;;   (:import-from #:core-server #:get-model #:set-model #:login)
;;   (:export
;;    #:jquery
;;    #:+jquery+
;;    #:+jquery-ui+
;;    #:dialog))

;; (in-package :jquery)

;; (defparameter +jquery+ "http://code.jquery.com/jquery-1.2.6.js")
;; (defparameter +jquery-ui+ "http://localhost/jquery/jquery-ui-personalized-1.6rc2.packed.js")

;; ;; ----------------------------------------------------------------------------
;; ;; Jquery Stack 
;; ;; ----------------------------------------------------------------------------
;; (defcomponent jquery ()
;;   ())

;; ;; (defmethod/remote to-json ((self jquery) object)
;; ;;   object)

;; (defmethod/remote funkall ((self jquery) action arguments)
;;   (let ((result)
;; 	(target this))
;;     (j-query.ajax (create :url action
;; 			  :type "POST"
;; 			  :data (mapobject (lambda (k v)
;; 					     (target.to-json v))
;; 					    arguments)
;; 			  :async false
;; 			  :success (lambda (xhr status)
;; 				     (setf result xhr))
;; 			  :error (lambda (xhr status e)
;; 				   (throw e))))
;;     ;; (return (eval result))
;;     (try
;;      (setf result (eval result))
;;      (:catch (e) (throw e)))
;;     (return result)))


;; ;; ----------------------------------------------------------------------------
;; ;; Jquery FlexiGrid Component
;; ;; ----------------------------------------------------------------------------
;; (defcomponent flexi-grid (jquery)
;;   ((instances :initarg :instances :initform nil)
;;    (dom-id :host remote :initarg :id)
;;    (grid :host remote :initform nil)
;;    (slots :host none :initarg :slots :initform nil)
;;    (col-model :host remote :initform nil)
;;    (actions :host remote :initarg :actions :initform nil)
;;    (height :host remote :initform 200)
;;    (width :host remote :initform "auto")
;;    (striped :host remote :initform t)
;;    (novstripe :host remote :initform nil)
;;    (min-width :host remote :initform 30)
;;    (max-height :host remote :initform 80)
;;    (resizable :host remote :initform t)
;;    (use-pager :host remote :initform t)
;;    (nowrap :host remote :initform t)
;;    (rp :host remote :initform 15) ;; Results per page
;;    (rp-options :host remote :initform (list 10 15 20 25 40))
;;    (title :host remote :initform nil)
;;    (autoload :host remote :initform t)
;;    (sortname :host remote :initform nil)
;;    (sortorder :host remote :initform "asc")))

;; (defmethod shared-initialize :after ((grid flexi-grid) slot-names
;; 				     &key &allow-other-keys)
;;   (setf (flexi-grid.col-model grid)
;; 	(mapcar (lambda (slot)
;; 		  (let* ((slot (copy-list slot))
;; 			 (label (getf (cdr slot) :label)))
;; 		    (remf (cdr slot) :label)
;; 		    (setf (getf (cdr slot) :display) label
;; 			  (getf (cdr slot) :name) (core-server::symbol-to-js (car slot)))
;; 		    (remf (cdr slot) :reader)
;; 		    (apply #'jobject (cdr slot))))
;; 		(flexi-grid.slots grid))))

;; (defcomponent-ctor flexi-grid)

;; ;; ----------------------------------------------------------------------------
;; ;; Flexigrid Data Format
;; ;; ----------------------------------------------------------------------------

;; ;; {
;; ;; page: 1,
;; ;; total: 239,
;; ;; rows: [
;; ;; {id:'ZW',cell:['ZW','ZIMBABWE','Zimbabwe','ZWE','716']},
;; ;; {id:'VE',cell:['VE','VENEZUELA','Venezuela','VEN','862']},
;; ;; {id:'VU',cell:['VU','VANUATU','Vanuatu','VUT','548']}]
;; ;; }

;; (defmethod flexi-grid.find-slot ((self flexi-grid) slot-namestring)
;;   (find (string-upcase slot-namestring) (flexi-grid.slots self)
;; 	:test #'string=
;; 	:key (arnesi::compose #'symbol-name #'car)))

;; (defmethod flexi-grid.sort ((self flexi-grid) data sortname sortorder)
;;   (let ((slot (flexi-grid.find-slot self sortname)))
;;     (if slot
;; 	(sort (copy-list data)
;; 	      (lambda (a b)
;; 		(cond
;; 		  ((equal sortorder "asc")		   
;; 		   (typecase a
;; 		     (number
;; 		      (< (or a 0) (or b 0)))
;; 		     (string
;; 		      (string< a b))
;; 		     (t t)))
;; 		  ((equal sortorder "desc")
;; 		   (typecase a
;; 		     (number
;; 		      (> (or a 0) (or b 0)))
;; 		     (string
;; 		      (string> a b))
;; 		     (t t)))
;; 		  (t t)))
;; 	      :key (getf (cdr slot) :reader))
;; 	data)))

;; (defmethod flexi-grid.query ((self flexi-grid) query slot)
;;   (let* ((slot (flexi-grid.find-slot self slot))
;; 	 (reader (getf (cdr slot) :reader)))
;;     (when reader
;;       (labels ((predicate (instance)
;; 		 (let ((value (funcall reader instance)))
;; 		   (equal value query))))
;; 	(core-server::filter #'predicate (flexi-grid.instances self))))))

;; (defmethod/local grid-data ((self flexi-grid) page rp sortname sortorder query qtype)
;;   (let ((data (flexi-grid.sort self
;; 			       (if (and query qtype)
;; 				   (flexi-grid.query self query qtype)
;; 				   (flexi-grid.instances self))
;; 			       sortname sortorder)))
;;     (describe (list page rp sortname sortorder query qtype))
;;     (jobject :page page
;; 	     :total (length data)
;; 	     :rows (or (mapcar (lambda (instance)
;; 				 (jobject :id (core-server::get-id instance)
;; 					  :cell (mapcar (lambda (slot)
;; 							  (funcall (getf (cdr slot) :reader) instance))
;; 							(flexi-grid.slots self))))
;; 			       (take rp (drop (* page rp) data)))
;; 		       (jobject)))))

;; (defmethod/local handler ((self flexi-grid))
;;   (action/url ((page "page") (rp "rp") (sortname "sortname")
;; 	       (sortorder "sortorder") (query "query") (qtype "qtype"))
;;     (json/suspend
;;       (core-server::json!
;;        (http-response.stream (context.response +context+))
;;        (apply 'grid-data (cons self
;; 				(mapcar 'core-server::json-deserialize
;; 					(list page rp sortname sortorder query qtype))))))))

;; (core-server::defctor (self flexi-grid)
;;   (let ((target this))
;;     (.flexigrid ($ (+ "#" (this.get-dom-id)))
;; 		(create
;; 		 :url (this.handler)
;; 		 :data-type "json"
;; 		 :col-model (this.get-col-model)
;; 		 :sortname "name"
;; 		 :sortorder "asc"
;; 		 :height (this.get-height)
;; 		 :width (this.get-width)
;; 		 :striped (this.get-striped)
;; 		 :novstripe (this.get-novstripe)
;; 		 :minwidth (this.get-min-width)
;; 		 :maxheight (this.get-max-height)
;; 		 :resizable (this.get-resizable)
;; 		 :usepager (this.get-use-pager)
;; 		 :nowrap (this.get-nowrap)
;; 		 :rp (this.get-rp)
;; 		 :rp-options (this.get-rp-options)
;; 		 :title (this.get-title)
;; 		 :autoload (this.get-autoload)
;; 		 :searchitems (filter (lambda (item)
;; 					(if (aref item "searchable")
;; 					    true
;; 					    nil))
;; 				      (this.get-col-model))
;; 		 :buttons (mapcar (lambda (action)
;; 				    (let ((object (new (*object action)))
;; 					  (onpress (aref action "onpress")))
;; 				      (setf (slot-value object 'onpress)
;; 					    (lambda (com grid)
;; 					      (let ((fun (aref target onpress)))
;; 						(if fun
;; 						    (fun.call target grid)
;; 						    (alert (+ "Sorry, " onpress " is undefined."))))))
;; 				      object))
;; 				  (this.get-actions)))))
;;   (this.set-grid (slot-value (aref ($ (+ "#" (this.get-dom-id))) 0) 'grid)))

;; ;; ----------------------------------------------------------------------------
;; ;; defflexi-grid Macro: Define a table view
;; ;; ----------------------------------------------------------------------------
;; (defmacro defflexi-grid (name supers slots &rest rest)
;;   `(progn
;;      (defcomponent ,name (flexi-grid ,@supers)
;;        ()
;;        (:default-initargs :slots ',slots ,@(flatten1 rest)))))


;; ;; ----------------------------------------------------------------------------
;; ;; Jquery CRUD
;; ;; ----------------------------------------------------------------------------
;; (defcomponent jquery-crud (jquery)
;;   ((fields :initarg :fields :host local :initform nil)
;;    (dom-id :initarg :id :host remote :initform "crud")
;;    (instance :initarg :instance :host local :initform nil)
;;    (model :host remote :initform nil)
;;    (view-title :initarg :view-title :initform nil)
;;    (edit-title :initarg :edit-title :initform nil)
;;    (new-title :initarg :new-title :initform nil)))

;; (defmethod shared-initialize :after ((crud jquery-crud) slot-names
;; 				     &key &allow-other-keys)
;;   (setf (jquery-crud.model crud)
;; 	(mapcar (lambda (field)
;; 		  (let ((field (copy-list field)))
;; 		    (remf (cdr field) :reader)
;; 		    (remf (cdr field) :type)
;; 		    (apply #'jobject (cons :name
;; 					   (cons (symbol-name (car field))
;; 						 (cdr field))))))
;; 		(jquery-crud.fields crud))))

;; (defmethod/local crud-template ((crud jquery-crud))  
;;   (<:form :action "#"
;; 	  (mapcar (lambda (field)
;; 		    (<:div :class "field"
;; 			   (<:div :class "name" (getf (cdr field) :label))
;; 			   (<:div :class "value"
;; 				  (case (getf (cdr field) :type)
;; 				    (t
;; 				     (<:input :type "text" :name (symbol-name (car field))
;; 					      :value (funcall
;; 						      (getf (cdr field) :reader)
;; 						      (jquery-crud.instance crud))))))))
;; 		  (jquery-crud.fields crud))
;; 	  (<:div :class "field"
;; 		 (<:div :class "name")
;; 		 (<:div :class "value"
;; 			(<:input :type "submit" :value "Save")))))

;; (defmethod/local save-instance ((crud jquery-crud) values)
;;   ;; (break values)
;; ;;   (mapcar #'describe values)
;;   (describe values)
;;   )

;; (defmethod/remote save ((crud jquery-crud) e)
;;   (let* ((model (this.get-model))
;; 	 (form e.target)
;; 	 (values (mapcar (lambda (m)
;; 			   (cons (slot-value m 'name)
;; 				 (slot-value (slot-value form (slot-value m 'name))
;; 					     'value)))
;; 			 model)))
;; ;;    (this.save-instance.apply values)
;;     (this.save-instance values)))

;; (core-server::defctor (component jquery-crud)
;;   (let ((div (aref ($ (+ "#" (this.get-dom-id))) 0))
;; 	(form (this.crud-template))
;; 	(self this))
;;     (setf div.inner-h-t-m-l "")
;;     (div.append-child form)
;;     (setf form.onsubmit (lambda (e)
;; 			  (self.save.call self e)))))

;; (defcomponent-ctor jquery-crud)
;; ;; ----------------------------------------------------------------------------
;; ;; defjquery-crud Macro: Define a form view
;; ;; ----------------------------------------------------------------------------
;; (defmacro defjquery-crud (name supers slots &rest rest)
;;   `(progn
;;      (defcomponent ,name (jquery-crud ,@supers)
;;        ()
;;        (:default-initargs :fields ',slots
;; 	 ,@(flatten1 rest)))))


;; ;; +----------------------------------------------------------------------------
;; ;; | Jquery Dialog
;; ;; +----------------------------------------------------------------------------
;; (defhtml-component dialog (<:div)
;;   ((showp :initarg :showp :initform nil :host local)
;;    (eben :initarg :eben :host remote :initform "eben")))

;; (defmethod/remote template ((self dialog))
;;   (<:div :class "flora":title "eben"
;; 	 "I am Jquery Dialog, override my template method."))

;; (defmethod/remote showingp ((self dialog))
;;   (.dialog ($ (+ "#" self.id)) "isOpen"))

;; (defmethod/remote show ((self dialog))
;;   (unless (showingp self)
;; ;;;     (let ((template (template self)))
;; ;;; ;;;       (.hide ($ template))
;; ;;; ;;;       (document.body.append-child template)
;; ;;;       (.dialog ($ template))
;; ;;;       (.show ($ template)))
;;     (.dialog ($ self))
;;     ))

;; (defmethod/remote init ((self dialog))
;;   (set-eben self "test")
;;   (if (get-showp self)
;;       (show self)))

;; (core-server::defhtmlcomponent-ctor dialog)

;; ;; +----------------------------------------------------------------------------
;; ;; | Jquery Login Link & Dialog
;; ;; +----------------------------------------------------------------------------
;; (defhtml-component login-dialog (<:a)
;;   ())

;; (defmethod/local auth ((self login-dialog) username password)
;;   (answer/dispatch 'login username password))

;; (defmethod/remote template ((self login-dialog))
;;   (<:div :class "gee"
;; 	 (<:form
;; 	  :action "#"
;; 	  :onsubmit (lambda (e)
;; 		      (let ((form e.target))
;; 			(self.auth form.username.value form.password.value)))
;; 	  (with-field "Username:" (<:input :type "text" :name "username"))
;; 	  (with-field "Password:" (<:input :type "password" :name "password"))
;; 	  (with-field "" (<:input :type "submit" :value "Login")))))

;; (defmethod/remote init ((self login-dialog))
;;   (setf this.onclick (lambda (e)
;; 		       (.dialog ($ (template self)))
;; 		       (return false))))

;; (core-server::defhtmlcomponent-ctor login-dialog)

;; ;; ----------------------------------------------------------------------------
;; ;; Jquery Macros
;; ;; ----------------------------------------------------------------------------

;; ;; ----------------------------------------------------------------------------
;; ;; Jquery Stack
;; ;; ----------------------------------------------------------------------------
;; ;; ;;;;
;; ;; ;;;; Interface for remote services
;; ;; ;;;;
;; ;; (defcomponent ajax-mixin ()
;; ;;   ())

;; ;; ;; TODO: first create activexobject, catch exception then create xmlhttprequest.
;; ;; (defmethod/remote make-request ((self ajax-mixin))
;; ;;   ;; (cond
;; ;;   ;;       (window.*x-m-l-http-request ;; Gecko
;; ;;   ;;        (setf request (new (*x-m-l-http-request))))
;; ;;   ;;       (window.*active-x-object ;; Internettin Explorer
;; ;;   ;;        (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
;; ;;   ;;     (if (= null request)
;; ;;   ;; 	(throw (new (*error "Exception: Cannot find usable XmlHttpRequest method, -core-server 1.0")))
;; ;;   ;; 	(return request))
;; ;;   (let ((req null))
;; ;;     (try (setf req (new (*active-x-object "Msxml2.XMLHTTP")))
;; ;; 	 (:catch (e1)
;; ;; 	   (try (setf req (new (*active-x-object "Microsoft.XMLHTTP")))
;; ;; 		(:catch (e2)
;; ;; 		  (setf req null)))))
;; ;;     (if (and (not req) (not (= (typeof *x-m-l-http-request) "undefined")))
;; ;; 	(setf req (new (*x-m-l-http-request))))
;; ;;     (return req)))

;; ;; ;; return response directly, don't eval (text? xml?).
;; ;; (defmethod/remote send-request ((self ajax-mixin) request url)
;; ;;   (request.open "GET" url false)
;; ;;   (request.send null)
;; ;;   (if (= 200 request.status)
;; ;;       (return request)
;; ;;       (throw (new (*error (+ "Exception: Cannot send XmlHttpRequest: " url " -core-server 1.0"))))))

;; ;; (defcomponent jqueryapi (ajax-mixin)
;; ;;   ((script-location :host remote
;; ;; 		    :initform "jquery-latest.min.js"
;; ;; 		    :initarg :script-location
;; ;; 		    :documentation "jQuery script location as url")))

;; ;; (defmethod/remote init ((self jqueryapi))
;; ;;   (when (= "undefined" (typeof j-query))
;; ;;     (let ((req (this.make-request))
;; ;; 	  (resp (this.send-request req this.script-location)))
;; ;;       (return (eval (+ "{" resp.response-text "}"))))))

;; ;; ;; TODO: implement retrycount, possibly using $.ajax.
;; ;; (defmethod/remote jqueryfuncall ((self jqueryapi) url parameters retry-count)
;; ;;   (let (result)
;; ;;     (debug "server.funcall " url)
;; ;;     ($.post url
;; ;; 	    parameters
;; ;; 	    (lambda (data textstatus)
;; ;; 	      (setf result (eval (+ "{" data "}"))))
;; ;; 	    "json")
;; ;;     (return result)))

;; ;; (defun/cc jquery (&optional scriptlocation)  
;; ;;   (send/component (make-instance 'jqueryapi :script-location scriptlocation))
;; ;;   ;; (<:js
;; ;; ;;     `(progn
;; ;; ;;        (setf jqueryapi (new (jqueryapi)))
;; ;; ;;        (defun funcall (url parameters retry-count)
;; ;; ;; 	 (return (jqueryapi.jqueryfuncall url parameters retry-count)))
;; ;; ;;        (jqueryapi.init)))
;; ;;   (error "fix jquery")
;; ;;   )
