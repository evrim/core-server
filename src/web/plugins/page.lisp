(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Page Plugin
;; +-------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Template
;; -------------------------------------------------------------------------
(defcomponent <core:template (secure-object)
  ((name :host local)
   (widget-maps :host local))
  (:default-initargs
   :levels '(<core:template/anonymous <core:template/owner)
   :permissions '((owner . 1) (group . 0) (other . 0) (anonymous . 0)
		  (unauthorized . -1))
   :owner +system+ :group (user.group +system+)))

(defcomponent <core:template/anonymous (secure-object/authorized)
  ((secure-object :host lift :type <core:template)
   (widget-maps :host remote :lift t)
   (name :host remote :lift t)
   (_controller :host remote)))

(defmethod/remote destroy ((self <core:template/anonymous))
  (mapcar-cc (lambda (a) (if a (destroy a))) (widget-maps self))
  (call-next-method self))

(defmethod/remote init ((self <core:template/anonymous))
  (setf (widget-maps self)
	(mapcar-cc (lambda (m) (make-component m :_controller (_controller self)))
		   (widget-maps self))))

(defcomponent <core:template/owner (<core:template/anonymous)
  ())

;; -------------------------------------------------------------------------
;; Page
;; -------------------------------------------------------------------------
(defcomponent <core:page (secure-object)
  ((template :host local :initform (error "Provide :template"))
   (name :host local)
   (widget-maps :host local))
  (:default-initargs
   :levels '(<core:page/anonymous <core:page/owner)
   :permissions '((owner . 1) (group . 0) (other . 0) (anonymous . 0)
		  (unauthorized . -1))
   :owner +system+ :group (user.group +system+)))

(defcomponent <core:page/anonymous (secure-object/authorized)
  ((secure-object :host lift :type <core:page)
   (name :host remote :lift t)
   (widget-maps :host remote :lift t)
   (_controller :host remote)))

(defmethod/remote destroy ((self <core:page/anonymous))
  (mapcar-cc (lambda (a) (if a (destroy a)))
	     (widget-maps self))
  (call-next-method self))

(defmethod/remote init ((self <core:simple-page/anonymous))
  (setf (widget-maps self)
	(mapcar-cc (lambda (m)
		     (make-component m :_controller (_controller self)))
		   (widget-maps self))))

(defcomponent <core:page/owner (<core:page/anonymous)
  ())

;; -------------------------------------------------------------------------
;; Page Plugin
;; -------------------------------------------------------------------------
(defcomponent <plugin:page (secure-object)
  ((default-page-name :host local :initform "index")
   (v2-compat-mode :host local :initform nil)
   (page-css :host local :initform +page-plugin.css+))
  (:default-initargs
   :levels '(<plugin:page/anonymous <plugin:page/registered <plugin:page/owner)
   :permissions '((owner . 2) (group . 2) (other . 1) (anonymous . 0))
   :owner +system+ :group (user.group +system+)))

(defmethod <plugin:page.get ((self <plugin:page) name)
  (error "Please implement (<plugin:page.get ~A ~A)" (class-name (class-of self)) name))

;; -------------------------------------------------------------------------
;; Page Plugin/Anonymous
;; -------------------------------------------------------------------------
(defplugin <plugin:page/anonymous (secure-object/authorized history-mixin)
  ((secure-object :host lift :type <plugin:page)
   (default-page-name :host none :lift t)
   (v2-compat-mode :host remote :lift t)
   (page-css :host remote :lift t)
   (current-page :host remote)
   (current-template :host remote)
   (on-load-hooks :host remote)))

(defmethod/lift <plugin:page.get ((self <plugin:page/anonymous) name))

(defmethod shared-initialize :after ((self <plugin:page/anonymous) slots &rest args)
  (declare (ignore args))
  (with-slots (default-page-name) self
    (when default-page-name
      (let ((page (<plugin:page.get self default-page-name)))
	(when page
	  (setf (current-page self) page
		(current-template self) (template page)))))))

(defmethod/remote register-on-load ((self <plugin:page/anonymous) fun)
  (setf (on-load-hooks self) (cons fun (on-load-hooks self))))

(defmethod/remote unregister-on-load ((self <plugin:page/anonymous) fun)
  (setf (on-load-hooks self) (remove fun (on-load-hooks self))))

(defmethod/remote get-page-in-the-url ((self <plugin:page/anonymous))
  (if (v2-compat-mode self)
      (or (get-parameter "page") (.substr window.location.hash 1))
      (get-parameter "page")))

(defmethod/remote set-page-in-the-url ((self <plugin:page/anonymous) _name)
  (unless (eq _name (get-page-in-the-url self))
    (if (v2-compat-mode self)
	(setf window.location.hash (encode-u-r-i-component _name))
	(set-parameter "page" _name))))

(defmethod/remote on-page-load ((self <plugin:page/anonymous) page-name)
  (mapcar (lambda (fun) (make-web-thread (lambda () (call/cc fun page-name))))
	  (on-load-hooks self)))

;; (defun/cc %relative-pathname (pathname &optional (context +context+))
;;   (let ((request (context.request context)))
;;     (cond
;;       ((and (query-session :referer) (http-request.header request 'referer))
;;        ;; Case Demo loaded from a Local Fille
;;        (let* ((pathname (uri? (make-core-stream pathname)))
;; 	      (paths (reverse
;; 		      (set-difference
;; 		       (uri.paths pathname)
;; 		       (uri.paths (query-session :referer))
;; 		       :test #'equal))))
;; ;;; 	 (describe (list 'pathname pathname 'paths paths
;; ;;; 			 'referer (query-session :referer)))
;; 	 (if paths
;; 	     (uri->string (make-uri :paths paths))
;; 	     "/")))
;;       ((query-session :referer)
;;        ;; Case Demo loaded from a Hosting
;;        (let* ((pathname (uri? (make-core-stream pathname)))
;; 	      (paths (reverse
;; 		      (set-difference (uri.paths pathname)
;; 				      (uri.paths (query-session :referer))
;; 				      :test #'equal))))
;; ;;; 	 (describe (list 'pathname pathname 'paths paths
;; ;;; 			 'referer (query-session :referer)))
;; 	 (if paths
;; 	     (uri->string (make-uri :paths paths))
;; 	     "/")))
;;       ((http-request.header request 'referer)
;;        ;; Deployed Instance
;;        (uri->string
;; 	(make-uri
;; 	 :paths (uri.paths (http-request.header request 'referer)))))
;;       (t
;;        ;; None of the above
;;        nil))))

;; (defmethod/cc relative-pathname ((self page-plugin/anonymous) pathname)
;;   (%relative-pathname pathname +context+))

;; (defun %absolute-pathname1 (location template-name
;; 			    &optional (context +context+))
;;   (cond
;;     ((and context (query-session :referer (context.session context))
;; 	  (http-request.header (context.request context) 'referer))
;;      ;; Case Demo loaded from a Hosting
;;      (let ((session (context.session context)))
;;        (format nil "~{~{/~A~}~}~A"
;; 	       (uri.paths (query-session :referer session))
;; 	       template-name)))
;;     ((and context (query-session :referer (context.session context)))
;;      ;; Case Demo loaded from a Local File
;;      ;; location: file://bidir/bdiir/products/index.html
;;      ;; template-name /index2.html
;;      ;; session-rererer: file://bidir/bidir/index.html
;;      (let* ((location (uri? (make-core-stream location)))
;; 	    (session (context.session context))
;; 	    (paths (reverse
;; 		    (set-difference
;; 		     (uri.paths location)
;; 		     (uri.paths (query-session :referer session))
;; 		     :test #'equal))))
;;        (format nil "./~{~{/..~}~}~A" paths template-name)))
;;     ((and context (http-request.header (context.request context) 'referer))
;;      ;; Deployed Instance
;;      template-name)
;;     (t
;;      ;; None of the above
;;      (warn "Error in page-plugin.absolute-pathname ~A ~A"
;; 	   location
;; 	   template-name))))

;; (defmethod/cc absolute-pathname1 ((self page-plugin/anonymous)
;; 				  location template-name)
;;   (%absolute-pathname1 location template-name +context+))

;; ;; To be overriden by subclasses. -evrim.
;; (defmethod authorize-page ((self page-plugin/anonymous) (page page))
;;   (if (published-p page)
;;       (authorize page nil) ;; (make-page/anonymous page)
;;       ))

;; (defmethod/local _get-page1 ((self page-plugin/anonymous) page-name location)
;;   (aif (page.find application :name page-name)
;;        (let* ((template (template it))
;; 	      (relative (relative-pathname self location)))
;; 	 ;; (describe (list 'relative relative))
;; 	 (if (handles-p (template it) relative)
;; 	     (authorize-page self it)
;; 	     (redirect-page self
;; 	       (absolute-pathname1 self location
;; 				   (format nil "~A#page:~A" (name template)
;; 					   page-name)))))))

;; (defmethod/cc _make-template ((self page-plugin/anonymous)
;; 			      (template template))
;;   (make-template/anonymous template))

;; (defmethod/local _get-template1 ((self page-plugin/anonymous) name)
;;   (aif (template.find application :name (relative-pathname self name))
;;        (_make-template self it)))

;; (defmethod/remote load-template ((self page-plugin/anonymous))
;;   (let ((_template-ctor (_get-template1 self window.location.href)))
;;     (when _template-ctor
;;       (setf (current-template self)
;; 	    (make-component _template-ctor :taskbar self)))))

;; (defmethod/remote load-page-from-ctor ((self page-plugin/anonymous) ctor)
;;   (when (current-page self)	       
;;     (remove-class document.body
;; 		  (+ (encode-u-r-i-component
;; 		      (name (current-page self)))
;; 		     "_page"))
;;     (destroy (current-page self)))

;;   (if (null (current-template self))
;;       (load-template self))
       
;;   (let* ((page (make-component ctor :taskbar self))
;; 	 (_name (name page)))
;;     (setf (current-page self) page)

;;     ;; If we'r loading first page, do not change anchor, break
;;     ;; back button bad. -evrim.
;;     (if (not (and (eq _name (first-page self)) (not (loaded-p self))))
;; 	(set-page-in-the-url self _name))
	 
;;     (make-web-thread (lambda () (on-page-load self _name)))
;;     (add-class document.body
;; 	       (+ (encode-u-r-i-component _name) "_page"))
;;     (toast self (_ "Page %1 is loaded." _name))
;;     (make-web-thread gc)))

;; (defmethod/remote load-page ((self page-plugin/anonymous) page-name)
;;   (let ((start (new (*date))))
;;     (prog1
;; 	(let ((page-ctor (_get-page1 self page-name window.location.href)))
;; 	  (cond
;; 	    (page-ctor
;; 	     (load-page-from-ctor self page-ctor)
;; 	     t)
;; 	    (t
;; 	     (toast self (_ "Sorry, page %1 is not found." page-name))
;; 	     nil)))
;;       (_debug (list "Page loaded in:" (- (new (*date)) start))))))


;; (defmethod/remote on-history-change ((self page-plugin/anonymous))
;;   (_debug (list "on-history-change" self))
;;   (let ((page (current-page self))
;; 	(anchor (or (get-page-in-the-url self) (first-page self))))
;;     (if page
;; 	(let ((name (name page)))
;; 	  (if (and (not (eq name anchor))
;; 		   (not (eq name window.location.pathname)))
;; 	      (load-page self anchor)
;; 	      (call-next-method self)))
;; 	(load-page self anchor))))

;; (defmethod/remote destroy ((self <plugin:page/anonymous))
;;   (remove-css (page-css self))
;;   (awhen (current-page self) (destroy it))
;;   (call-next-method self))

;; (defmethod/remote init ((self page-plugin/anonymous))
;;   (load-css (page-css self))

;;   (when (eq "function" (typeof (slot-value window 'coretal-on-page-load)))
;;     (register-on-load self (slot-value window 'coretal-on-page-load)))
  
;;   (load-page self (or (get-page-in-the-url self) (first-page self)))
;;   (start-history-timeout self))

;; ;; -------------------------------------------------------------------------
;; ;; Page Plugin/Registered
;; ;; -------------------------------------------------------------------------
;; (defcomponent page-plugin/registered (page-plugin/anonymous)
;;   ((_editor-top :host remote)
;;    (_editor-bottom :host remote)
;;    (_editor-count :host remote :initform 0)))

;; (defmethod/remote enable-editor ((self page-plugin/registered))
;;   (if (null (_editor-top self))
;;       (setf (_editor-top self)
;; 	    (<:div :class "ckeditor-top" :id "ckeditor-top")))
;;   (if (null (_editor-bottom self))
;;       (setf (_editor-bottom self)
;; 	    (<:div :class "ckeditor-bottom" :id "ckeditor-bottom")))

;;   (cond
;;     ((eq 0 (_editor-count self))
;;      (prepend document.body (_editor-top self))
;;      (append document.body (_editor-bottom self))
;;      (setf (_editor-count self) 1)
;;      (let ((m (parse-int
;; 	       (slot-value (slot-value document.body 'style) 'margin-top))))
;;        (if (is-na-n m)
;; 	   (setf (slot-value (slot-value document.body 'style) 'margin-top)
;; 		 "140px")
;; 	   (setf (slot-value (slot-value document.body 'style) 'margin-top)
;; 		 (+ (+ m 140) "px")))))
;;     (t (setf (_editor-count self) (+ 1 (_editor-count self))))))

;; (defmethod/remote disable-editor ((self page-plugin/registered))
;;   (cond
;;     ((eq 1 (_editor-count self))
;;      (setf (_editor-count self) 0)
;;      (.remove-child document.body (_editor-top self))
;;      (.remove-child document.body (_editor-bottom self))
;;      (setf (_editor-top self) nil (_editor-bottom self) nil)
;;      (let ((m (parse-int
;; 	       (slot-value (slot-value document.body 'style) 'margin-top))))
;;        (if (is-na-n m)
;; 	   (setf (slot-value (slot-value document.body 'style) 'margin-top)
;; 		 "0px")
;; 	   (setf (slot-value (slot-value document.body 'style) 'margin-top)
;; 		 (+ (- m 140) "px")))))
;;     (t
;;      (setf (_editor-count self) (- (_editor-count self) 1)))))

;; (defmethod/remote page-menu ((self page-plugin/registered)) nil)
;; ;; FIXME: Optimization, call/cc is slow -evrim.
;; (defun %page-list (pages) (sort (mapcar #'name pages) #'string<))
;; (defmethod/local page-list ((self page-plugin/registered))
;;   (%page-list (filter #'published-p (page.list application))))

;; (defmethod/remote destroy ((self page-plugin/registered))
;;   (disable-editor self)
;;   (delete-slots self '_editor-top '_editor-bottom '_editor-count)
;;   (call-next-method self))

;; (defmethod authorize-page ((self page-plugin/registered) (page page))
;;   (if (published-p page)
;;       (authorize page (user self)) ;; (make-page/registered page (user self))
;;       ))

;; ;; -------------------------------------------------------------------------
;; ;; Page Plugin/Editor
;; ;; -------------------------------------------------------------------------
;; (defcomponent page-plugin/editor (page-plugin/registered
;; 				  supply-prompt-dialog supply-dialog
;; 				  supply-new-page-dialog)
;;   ((_page-menu :host remote)))

;; (defmethod authorize-page ((self page-plugin/editor) (page page))
;;   (authorize page (user self)) ;; (make-page/editor page (user self))
;;   )

;; (defmethod/cc _make-template ((self page-plugin/editor)
;; 			      (template template))
;;   (make-latest-template/anonymous template))

;; (defmethod/local page-list ((self page-plugin/editor))
;;   (%page-list (page.list application)))

;; ;; (defmethod/local new-page ((self page-plugin/editor) new-name pathname)
;; ;;   (let ((pathname (relative-pathname self pathname)))
;; ;;     (flet ((create-new-template ()
;; ;; 	     (template.add application :name pathname)))
;; ;;       (let* ((_template (or (template.find application :name pathname)
;; ;; 			    (create-new-template)))
;; ;; 	     (_page (page.add application :name new-name
;; ;; 			      :template _template)))
;; ;; 	(name _page)))))

;; (defmethod/remote load-page ((self page-plugin/editor) page-name)
;;   (let ((page (call-next-method self page-name)))
;;     (cond
;;       (page
;;        (append (current-page self)
;; 	       (<:li
;; 		(<:a :onclick (lifte (do-configure-template self))
;; 		     (_ "Configure Template")
;; 		     (<:span :class "subtitle"
;; 			     (_"Configure common widgets"))))))
;;       (t
;;        (make-web-thread
;; 	(lambda ()
;; 	  (if (confirm (+ (_"Page %1 is not found." page-name)
;; 			  " " (_"Do you want to create?")))
;; 	      (let* ((example (call-component
;; 			       (make-component
;; 				(_make-new-page-dialog self))))
;; 		     (_new-page (new-page-from-example
;; 				 self page-name
;; 				 window.location.pathname
;; 				 (slot-value example 'name))))
;; 		(page-menu self)
;; 		(load-page self _new-page)))))))))

;; (defmethod/local new-page-from-example ((self page-plugin/editor)
;; 					new-name pathname example)
;;   (let ((pathname (relative-pathname self pathname))
;; 	(example (find example (application.examples application)
;; 		       :key #'example.name :test #'equal)))
;;     (assert (not (null example)))
;;     (flet ((create-new-template ()
;; 	     (template.add application :name pathname)))
;;       (let* ((_template (or (template.find application :name pathname)
;; 			    (create-new-template))))
;; 	(name (page.add-from-example
;; 	       application new-name (name _template) example))))))

;; (defmethod/remote do-new-page ((self page-plugin/editor))
;;   (let* ((example (call-component
;; 		   (make-component (_make-new-page-dialog self))))
;; 	 (name (call-component
;; 		(make-component (make-prompt-dialog self)
;; 				:title (_"new page")
;; 				:message (_ (+ "Please enter a name for"
;; 					       " the new page:")))))
;; 	 (name (new-page-from-example self name window.location.pathname
;; 				      (slot-value example 'name))))
;;     (page-menu self)
;;     (load-page self name)))

;; (defmethod/remote page-menu ((self page-plugin/editor))
;;   (let ((menu (<:ul :class "page-list"
;; 	       (cons
;; 		(<:li
;; 		 (<:a :onclick (lifte self.do-new-page) (_"New Page")
;; 		      (<:div :class "subtitle" (_"Create a new page"))))
;; 		(mapcar-cc
;; 		 (lambda (page-name)
;; 		   (<:li
;; 		    (<:a :onclick (lifte self.load-page page-name)
;; 			 page-name)))
;; 		 (page-list self))))))
;;     (if (_page-menu self) (replace-node (_page-menu self) menu))
;;     (setf (_page-menu self) menu)
;;     menu))

;; (defmethod/local _get-template-editor ((self page-plugin/editor) name)
;;   (aif (template.find application :name name)
;;        (make-template/editor it (user self))))

;; (defmethod/remote do-configure-template ((self page-plugin/editor))
;;   (let* ((_editor (_get-template-editor self (name (current-template self))))
;; 	 (_page (current-page self))
;; 	 (_name (name _page)))
;;     (+ "" _name) ;; do not inline
;;     (destroy _page)
;;     (destroy (current-template self))
;;     (call-component
;;      (call/cc _editor (extend (jobject :taskbar self) _page)))
;;     (setf (current-page self) nil
;; 	  (current-template self) nil)
;;     (load-page self _name)))

;; (defmethod/remote destroy ((self page-plugin/editor))
;;   (delete-slots self '_page-menu)
;;   (call-next-method self))

;; (defmethod/remote init ((self page-plugin/editor))
;;   (add-menu self
;; 	    (<:a (_"Pages")
;; 		 (<:span :class "subtitle" (_"Open a page on this site"))
;; 		 (<:div :class "taskbar-content" (page-menu self))))  
;;   (call-next-method self))

