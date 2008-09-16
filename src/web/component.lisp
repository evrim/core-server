;; +----------------------------------------------------------------------------
;; | Component Framework
;; +----------------------------------------------------------------------------
(in-package :core-server)

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
	       (this.funkall ,k
		(create
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
		  (this.funkall ,hash
		   (create :result (serialize (,name self ,@args)))))))))))))

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

;; +----------------------------------------------------------------------------
;; | Basic Components
;; +----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Debug Component
;; ----------------------------------------------------------------------------
;; FIXME: Fix debug component
(defcomponent debug-component ()
  ())

(defmethod/local get-source-code ((self debug-component) function)
  (funcall (function (intern o)))) => "function () { return 1; }" ;

(defmethod/local set-result ((self debug-component) function parameters result)
  (setf (gethash function +function-table+) (list (cons funciton parameters) result)))

(defmethod/remote run-test ((self debug-component) function)
  (let ((fun (this.get-source-code(function))))
    (if fun
	(this.set-result function nil (funcall fun)))))

(defun/javascript denemeA (str num) ("aycan" 1) ()
  (denemeB str (incf num)))

(defun/javascript denemeB (str num)
  (list str num))

;; record tanimla
;; record query, update
;; IPC

;; ----------------------------------------------------------------------------
;; HTML Component
;; ----------------------------------------------------------------------------
;; (defcomponent html-element (dom-element)
;;   ())

;; (defmethod/local render ((self html-element))
;;   nil)

;; ;;; (with-yaclml-output-to-string
;; ;;;     (<:div :id "56" :style "background-color:#FFF; color:#000"
;; ;;; 	   (<:p "This is render of html-element")
;; ;;; 	   (<:p "Aytek maraba!")))

;; (defmethod/cc send/ctor ((self html-element) remote-slots local-methods remote-methods)
;;   ;; (<:js
;; ;;    `(defun ,(class-name (class-of self)) ()
;; ;; 	(let ((o (create ,@remote-slots ,@local-methods ,@remote-methods))
;; ;; 	      (p (document.create-element o.tag)))
;; ;; 	  (doeach (property o)
;; ;; 	    (setf (slot-value p property) (slot-value o property)))
	  
;; ;; 	  (if (= "function" (typeof o.render))		
;; ;; 	      (setf p.inner-h-t-m-l (o.render)))

;; ;; 	  (setf this.prototype p)
;; ;; 	  (return p))))
;;   (error "fixme"))

;; (defcomponent div-element (html-element)
;;   ()
;;   (:default-initargs :tag "div"))

;; (defmethod/local render ((self div-element))
;;   (with-html-output (http-response.stream (response +context+))
;;     (<:div "hobaaa")))

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
