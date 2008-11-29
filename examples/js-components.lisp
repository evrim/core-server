(defpackage :jscomps
  (:use :cl :core-server :arnesi)
  (:export #:dictcomp))

(in-package :jscomps)

(defapplication jscomps-app (http-application database-server)
  ()
  (:default-initargs :fqdn "localhost"
    :database-directory #P"/tmp/jscomps/"
    :htdocs-pathname #P"/tmp/jscomps/"
    :admin-email "tahsin.pehlivan@core.gen.tr"))

(defvar *app* (make-instance 'jscomps-app))

;; ----------------------------------------------------------------------------
;; Simple Demonstration of Core Server Javascript Framework
;; ----------------------------------------------------------------------------
(defcomponent component1 (<:div)
  ())

;; this defines a local (running on the server) method: remoteCall()
;; function will be generated automatically
(defmethod/local remote-call ((self component1))
  (get-universal-time))

;; this defines a remote (running on the browser) method: localCall()
(defmethod/remote local-call ((self component1))
  (alert "Here we called a javascript function."))

;; this defines a remote (running on the browser) method which also
;; uses a local method: combinedCall()
(defmethod/remote combined-call ((self component1))
  (alert (+ "Universal time is: " (self.remote-call))))

(defmethod/remote init ((self component1))
  (self.append-child
   (<:p "Use firebug and test component1.remoteCall(), component1.localCall() and component1.combinedCall() methods."))
  (setf component1 self))

(defhandler "component1.html" ((self jscomps-app))
  (<:html
   (<:head
    (<:script :type "text/javascript" :src "library.core"))
   (<:body
    (component1 :id "component1"))))

;; javascript output if you want to distribute the component
(defhandler "component1.js" ((self jscomps-app))
  (javascript/suspend
   (lambda (stream)
     (component! stream (component1)))))


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
  (find-dictionary *app* :key key))

(defmethod/local add ((self dictionary-component) key value)
  (dictionary-add *app* :key key :val value))

;; html response for testing dictionary
(defhandler "dict.html" ((self jscomps-app))
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
                        (let ((comp (document.get-element-by-id "dict")))
                          (setf (slot-value (document.get-element-by-id "result") 'inner-h-t-m-l)
                                (slot-value (slot-value (lookup comp this.key.value) 'val) 'value))
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
  (find-dictionary *app* :key key))

(defmethod/local add ((self dictionary-component2) key value)
  (dictionary-add *app* :key key :val value))

;; this returns a form for adding key,value pairs to the database
(defmethod/remote add-form ((self dictionary-component2))
  (<:form :id "dict-add"
          :action "#"
          :onsubmit (lambda (e)                      
                      (add self this.key.value this.val.value)
                      (return false))
          (<:input :type "text" :name "key")
          (<:input :type "text" :name "val")
          (<:input :type "submit" :value "Add")))

;; this returns a form for querying keys
(defmethod/remote find-form ((self dictionary-component2))
  (<:form :id "dict-lookup"
          :action "#"
          :onsubmit (lambda (e)                      
                      (setf (slot-value (document.get-element-by-id "result") 'inner-h-t-m-l)
                            (slot-value (slot-value (lookup self this.key.value) 'val) 'value))
                      (return false))
          (<:input :type "text" :name "key")
          (<:input :type "submit" :value "Lookup")))

;; organization of elements in a page
(defmethod/remote template ((self dictionary-component2))
  (list (<:p "Add dictionary element")
        (self.add-form)
        (<:p "Lookup dictionary element")
        (self.find-form)
        (<:p "Value:" (<:span :id "result"))))

;; automatically executed upon initialization of the component
(defmethod/remote init ((self dictionary-component2))
  (mapcar (lambda (a) (self.append-child a)) (template self)))

(defhandler "dict2.html" ((self jscomps-app))
  (<:html
   (<:head
    (<:script :type "text/javascript" :src "library.core"))
   (<:body
    (dictionary-component2 :id "dict"))))

(register *server* *app*)