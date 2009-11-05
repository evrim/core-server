;; Two different methods for component usage
;;
;; Page Dependent:
;; http://localhost:8080/dict/dict.html
;;
;; Modular:
;; http://localhost:8080/dict/dict2.html
(defpackage :dict
  (:use :cl :core-server :arnesi))

(in-package :dict)

(defapplication dictionary-application (http-application database-server)
  ()
  (:default-initargs :fqdn "dict"
    :auto-start t
    :admin-email "tahsin.pehlivan@core.gen.tr"))

(defvar *app* (make-instance 'dictionary-application))

;; ----------------------------------------------------------------------------
;; Dictionary of (key,val)
;; ----------------------------------------------------------------------------
(defclass+ dictionary (object-with-id)
  ((key :print t :host both)
   (val :print t :host both)))

(defcrud dictionary)

;; +----------------------------------------------------------------------------
;; | Javascript Component for Dictionary
;; | * Page Dependent Version (see modular version)
;; +----------------------------------------------------------------------------
(defcomponent dictionary-component (<:div)
  ())

(defmethod/local lookup ((self dictionary-component) key)
  (dictionary.find (component.application self) :key key))

(defmethod/local add ((self dictionary-component) key value)
  (dictionary.add (component.application self) :key key :val value))

;; html response for testing dictionary
(defhandler "dict.html" ((self dictionary-application))
  (<:html
   (<:head
    (<:script :type "text/javascript" :src "library.core"))
   (<:body
    (dictionary-component :id "dict")
    (<:p "Add dictionary element")
    (<:form :id "dict-add"
            :action "#"
            :onsubmit (js
                        (let ((comp (document.get-element-by-id "dict")))
                          (add comp this.key.value this.val.value)
                          (return false)))
            (<:input :type "text" :name "key")
            (<:input :type "text" :name "val")
            (<:input :type "submit" :value "Add"))
    
    (<:p "Lookup dictionary element")
    (<:form :id "dict-lookup"
            :action "#"
            :onsubmit (js
			(progn
			  (lookup (document.get-element-by-id "dict") this.key.value
				  (lambda (val)
				    (setf (slot-value (document.get-element-by-id "result") 'inner-h-t-m-l)
					  (slot-value val 'val))))
			  (return false)))
            (<:input :type "text" :name "key")
            (<:input :type "submit" :value "Lookup"))
    (<:p "Value:" (<:span :id "result")))))

;; +----------------------------------------------------------------------------
;; | Another Javascript Component for Dictionary
;; | * Modular Javascript Version
;; +----------------------------------------------------------------------------
(defcomponent dictionary-component2 (<:div)
  ())

(defmethod/local lookup ((self dictionary-component2) key)
  (dictionary.find (component.application self) :key key))

(defmethod/local add ((self dictionary-component2) key value)
  (dictionary.add (component.application self) :key key :val value))

;; this returns a form for adding key,value pairs to the database
(defmethod/remote add-form ((self dictionary-component2))
  (let ((key (<:input :type "text" :name "key"))
	(val (<:input :type "text" :name "val")))
    (<:form :id "dict-add"
	    :action "#"
	    :onsubmit (lambda (e)
			(make-web-thread
			 (lambda () (add self (slot-value key 'value) (slot-value val 'value))))
			false)
	    key val (<:input :type "submit" :value "Add"))))

;; this returns a form for querying keys
(defmethod/remote find-form ((self dictionary-component2))
  (let ((key (<:input :type "text" :name "key")))
    (<:form :id "dict-lookup"
	    :action "#"
	    :onsubmit (lambda (e)
			(let ((key (slot-value key 'value)))
			  (make-web-thread
			   (lambda ()
			     (setf (slot-value (document.get-element-by-id "result") 'inner-h-t-m-l)
				   (slot-value (lookup self key) 'val)))))
			false)
	    key
	    (<:input :type "submit" :value "Lookup"))))

;; organization of elements in a page
(defmethod/remote template ((self dictionary-component2))
  (list (<:p "Add dictionary element")
        (add-form self)
        (<:p "Lookup dictionary element")
        (find-form self)
        (<:p "Value:" (<:span :id "result"))))

;; automatically executed upon initialization of the component
(defmethod/remote init ((self dictionary-component2))
  (mapcar (lambda (a) (.append-child self a)) (template self)))

(defhandler "dict2.html" ((self dictionary-application))
  (<:html
   (<:head
    (<:script :type "text/javascript" :src "library.core"))
   (<:body
    (dictionary-component2 :id "dict"))))

(register *server* *app*)