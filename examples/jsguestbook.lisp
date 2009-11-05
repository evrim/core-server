;; A persistent guestbook component
;; Just compile it with C-c C-k or load it with C-c C-l and then visit
;; http://localhost:8080/jsguestbook/index.html

(defpackage :jsguestbook
  (:use :cl :core-server :arnesi))

(in-package :jsguestbook)

(defapplication jsguestbook-app (http-application database-server)
  ()
  (:default-initargs :fqdn "jsguestbook"
    :admin-email "aycan@core.gen.tr"
    :database-directory #P"/tmp/jsguestbook/"
    :auto-start t))

(defparameter *app* (make-instance 'jsguestbook-app))

;; a class for messages
(defclass+ message (object-with-id)
  ((sender :print t :host both)
   (subject :print t :host both)
   (text :host both)
   (timestamp :initform (get-universal-time))))

(defcrud message)

;; Our component will be in a div element and it has no slots
(defcomponent guestbook-component (<:div)
  ())

(defmethod/local send-message ((self guestbook-component) sender subject text)
  (message.add (component.application self) :sender sender :subject subject :text text))

(defmethod/local get-messages ((self guestbook-component))
  (message.list (component.application self)))

(defmethod/remote add-form ((self guestbook-component))
  (<:div :id "add-form"
    (<:h1 "Sign the guestbook:")
    (<:form :id "send-message"
            :action "#"
            :onsubmit (event (ev)
			(with-call/cc
			  (send-message self this.sender.value this.subject.value this.text.value)
			  (replace-node (document.get-element-by-id "show-guestbook")
					(show-guestbook self)))
                        false)
            (<:p "Sender: " (<:input :type "text" :name "sender"))
            (<:p "Subject: " (<:input :type "text" :name "subject"))
            (<:p "Message: " (<:textarea :rows "10" :cols "40" :name "text"))
            (<:input :type "submit" :value "Sign Guestbook"))))

(defmethod/remote show-guestbook ((self guestbook-component))
  (<:div :id "show-guestbook"
    (<:h1 "Guestbook Messages:")
    (<:table :id "messages"
             (mapcar (lambda (m)
                       (list
                        (<:tr (<:td "From: ")
                              (<:td (unescape (slot-value m 'sender))))
                        (<:tr (<:td "Subject: ")
                              (<:td (unescape (slot-value m 'subject))))
                        (<:tr (<:td :colspan "2" (unescape (slot-value m 'text))))
                        (<:tr (<:td :colspan "2" (<:hr)))))
                     (get-messages self)))))

(defmethod/remote init ((self guestbook-component))
  (mapcar (lambda (e) (self.append-child e))
          (list (add-form self)
                (show-guestbook self))))

;; http://localhost:8080/guestbook/guestbook
(defhandler "jsguestbook.html" ((self jsguestbook-app))
  (<:html
   (<:head
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
    (<:script :type "text/javascript" :src "library.core"))
   (<:body
    (guestbook-component :id "guestbook"))))

(register *server* *app*)