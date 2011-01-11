(defpackage :crud
  (:use :cl :core-server :arnesi))

(in-package :crud)

(defclass+ crud-application (http-application database)
  ()
  (:metaclass http-application+)
  (:default-initargs :auto-start t))

(defclass+ user ()
  ((username :host both)
   (email :host both)
   (password :host local)))

(defcrud user)

(defcomponent user-proxy ()
  ((username :host both)
   (email :host both)
   (password :host remote)))

(defcomponent crud-application-component ()
  ((crud :initform (<core:crud :template-class (find-class 'data-proxy-template)))
   (table :initform (<core:table :template-class (find-class 'data-proxy-template)
				 :primary-field "slot1"))
   (local-cache :initform (make-hash-table :test #'string=)
		:host none)))

(defmethod/local get-instances ((self crud-application-component))
  (mapcar (lambda (user)
	    (let ((random-id (random-string 5)))
	      (setf (gethash random-id (local-cache self)) random-id)
	      (let ((obj (object->jobject user)))
		(set-attribute obj "ref-id" random-id)
		obj)))
	  (user.list (component.application self))))

(defmethod/local save-it ((self crud-application-component) instance-id slots)
  (let ((instance (gethash instance-id (local-cache self))))
    (break (list 'save-it instance slots))))

(defmethod/local add-it ((self crud-application-component) slots)
  (break (list 'add-it slots)))

(defmethod/remote add-link ((self crud-application-component) e)
  (let ((slots (call-component (call/cc (crud self) (.get-element-by-id docuent "crud")))))
    (let ((instance (add-it self slots)))
      (init self))))

;; data JSComponent :: Object -> IO Object
;; call/cc :: JSComponent -> 
;; -------------------------------------------------------------------------
;; Javascript call/cc: (f &rest args) -> (apply #'f (append args (list k)))
;; -------------------------------------------------------------------------
;; CRUD> (js 
;; 	(progn
;; 	  (defun usual-function () 1)
;; 	  (defun/cc callcc-function () 2)
;; 	  (defun manual-continuation () (k 3))
;; 	  (with-call/cc (usual-function))
;; 	  (with-call/cc (callcc-function))
;; 	  (with-call/cc (call/cc manual-continuation alert))))
;; "function usualFunction() {
;;   return 1;
;; };
;; function callccFunction(k11991) {
;;   k11991 = k11991 || window.k;
;;   return k11991(2);
;; };
;; function manualContinuation() {
;;   return k(3);
;; };
;; k(usualFunction());
;; callccFunction(k);
;; manualContinuation(alert, k);"
(defmethod/remote init ((self crud-application-component))
  (setf window.core self)
  (let ((instances (get-instances self)))
    (debug instances)
    (make-web-thread
     (lambda ()
       (let ((table (call/cc (table self)
			     (extend (jobject :instances instances)
				     (.get-element-by-id document "table")))))
	 (let ((selected (call-component table)))
	   (debug selected)
	   (let ((crud (call/cc (crud self)
				(extend (jobject :instance selected)
					(.get-element-by-id docuent "crud")))))
	     (let ((slots (call-component crud)))
	       (save-it self selected slots)))))))))

(defhandler "index2.html" ((self crud-application))
  (<:html (<:head (<:script :type "text/javascript" :src "library.core") (crud-application-component))
	  (<:body (<:h1 "Data Crud:")
		  (<:table :id "table")
		  (<:a :id "add" :onclick "javascript:core.addLink()" "Add")
		  (<:div :id "crud"))))

;; (defhandler "index.html" ((self crud-application))
;;   (let* ((crud (<core:crud :template-class (find-class 'data-proxy-template)))
;; 	 (table (<core:table
;; 		 :instances (data.list self)
;; 		 :template-class (find-class 'data-proxy-template)
;; 		 :primary-field "slot1"
;; 		 :id "table")))
;;     (destructuring-bind (slot . value)
;; 	(send/suspend
;; 	  (<:html (<:head (<:script :type "text/javascript" :src "library.core"))
;; 		  (<:body (<:h1 "Data Crud:") table (<:div :id "crud") (<:a :id "add" "Add")
;; 		   (<:script :type "text/javascript"
;; 			     (lambda (stream)
;; 			       (with-js (crud) stream
;; 				 (let ((crud crud))
;; 				   (with-call/cc
;; 				     (let  ((table (.get-element-by-id document "table")))
;; 				       (let ((instance (call-component table)))
;; 					 (let ((crud (call/cc crud
;; 							      (extend (jobject :instance instance)
;; 								      (.get-element-by-id document "crud")))))
;; 					   (call-component crud)
;; 					   (init table)))))
;; 				   (connect (.get-element-by-id document "add") "onclick"
;; 					    (lambda (e)
;; 					      (with-call/cc
;; 						(let ((instance (jobject :instance
;; 									 (jobject :class
;; 										  (jobject :slot1
;; 											   (jobject :label "Slot1"
;; 												    :name "slot1"
;; 												    :type "primitive"))
;; 										  :slot1 nil))))
;; 						  (let ((crud (call/cc crud
;; 							       (extend instance
;; 								(.get-element-by-id document "crud")))))
;; 						    (debug instance)
;; 						    (debug crud)
;; 						    (call-component crud))))
;; 					      false)))))))))
;;       (let ((instance (slot-value table 'core-server::selected)))
;; 	(case slot
;; 	  (slot1 (data.update self instance :slot1 value))
;; 	  (slot2 (data.update self instance :slot2 (update-session 'slot2 value)))))
;;       (setf (slot-value table 'instances) (data.list self))
;;       (continue-component crud t))))

(deftransaction init-database ((self crud-application))
  (user.add self :username "evrim" :email "evrim@core.gen.tr")
  (user.add self :username "aycan" :email "aycan@core.gen.tr")
  self)

(defparameter *crud
  (make-instance 'crud-application
		 :fqdn "crud"
		 :admin-email "evrim@core.gen.tr"))

(register *server* *crud)
