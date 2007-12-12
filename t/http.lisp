(in-package :core-server)

(defvar *dojo-url* "/dojo/dojo.js")
(js:defjsmacro $ (name)
  `(document.get-element-by-id ,name))

(eval-always
  (defclass test-application (http-application)
    ()))

(unregister core-server::*server* *app*)

(eval-always
  (defparameter *app*
    (make-instance 'test-application
		   :fqdn "test"
		   :admin-email "evrim@core.gen.tr")))

(register core-server::*server* *app*)

(defun/cc header () (<:div "I'm a header, jo!"))
(defun/cc greeting () (<:div "Greeting, lets do it!"))
(defun/cc footer () (<:div "say, don't make me foot, i'll make you a footer."))

(defun/cc navigation ()
  (<:ul
   (<:li (<:a :href (action/url ()
		      (answer :menu1)) "menu1"))
   (<:li (<:a :href (action/url ()
		      (answer :menu2)) "menu2"))))

(defun/cc settings (name)
  (<:div "Here is the settings:")
  (<:form :action "index.core" :method "GET"
	  :onsubmit
	  (js:js*
	   `(progn
	      (dojo.io.bind
	       (create :url ,(function/url ((dekst "dekst") (mekst "mekst"))
					   (answer dekst))
		       :mimetype "text/javascript"
		       :form-node this))
	      (return false)))
	  (<:ah "Neym:") (<:input :type "text" :value name :name "dekst")
	  (<:input :type "submit" :value "kaydit")))

(defun/cc javascript (lambda)
  (send/suspend    
    (setf (cdr (assoc 'content-type (http-response.entity-headers (response +context+))))
	  '("text" "javascript" ("charset" "UTF-8")))
    (funcall lambda)))

(defun/cc window (&optional (b0dy #'greeting))
  (format t "into the window~%")
  (prog1
      (send/suspend
	(<:html
	 (<:head (<:script :src *dojo-url* :type "text/javascript"))
	 (<:body
	  (<:div :class "container"
		 (header)
		 (navigation)
		 (<:div :id "body" :class "wrapper" (funcall b0dy))
		 (footer)))))  
    (format t "out to window~%")))


(defun/cc coretal (app &optional (b0dy #'greeting))
  (let ((it (window b0dy)))
    (format t "window ret:~A~%" it)
    (cond
      ((eq it :menu1)
       (format t "Do menu1~%")
       (let ((r (window (lambda () (settings "neym")))))
	 (format t "r:~A" r)
	 (javascript
	  (lambda ()
	    (format t "Javascript~%")
	    (<:js
	      `(setf (slot-value ($ "body") 'inner-h-t-m-l) (+ "Result:" ,r)))))))
      ((eq it :menu2)
       (format t "Do menu2~%")
       (coretal app (lambda () (<:ah "menu2"))))
      (t
       (break it)))))

(defurl *app* "index.core" ((command "cmd"))
;;  (describe command)
;;  (describe *context*)
  (loop
     (progn
       (format t "looping~%")
       (coretal *app*))))

(defun/cc get-number ()
  (send/suspend
    (<:html
;;     (<:head (<:script :src *dojo-url* :type "text/javascript"))
     (<:body
      (<:form :action "number.core" :method "GET"
	      (<:input :type "hidden" :name "k" :value (function/hash ((number "number"))
							 (answer (parse-integer number :junk-allowed t))))
	      (<:input :type "hidden" :name "s" :value (id (session +context+)))
	      (<:ah "Enter Number:") (<:input :type "text" :name "number")
	      (<:input :type "submit" :value "Tamam"))))))

(defun/cc print-number (number)
  (send/suspend
    (<:html
;     (<:head (<:script :src *dojo-url* :type "text/javascript"))
     (<:body
      (<:div
       (<:p (<:ah number)))))))

(defun add-numbers (num1 num2)
  (print (+ num1 num2)))

(defurl *app* "number.core" ()
  (print-number (+ (get-number) (get-number))))

(defun/cc test-fun (acc)
  (send/suspend
    (<:html
     (<:head)
     (<:body
      (<:a :href (action/url ()
		   (test-fun (1+ acc))) (<:ah "Acc(action):" acc))
      (<:br)
      (<:a :href (function/url ()
		   (test-fun (1+ acc))) (<:ah "Acc(function):" acc))))))

(defurl *app* "gee.core" ()
  (test-fun 0))


(defun/cc render-node (name)
  (send/suspend
    (<:html
     (<:body
      (dotimes (i 3)
	(<:a :href (let ((i i))
		     (function/url ()
		       (render-node (format nil "~A.~D" name i))))
	     (format nil "~A.~D" name i))	
	(<:br))))))

(defurl *app* "tree.core" ()
  (render-node "ahmet"))

(defun/cc render-graph-node (name)
  (send/suspend
    (<:html
     (<:body
      (<:p (format nil "~A" name))      
      (dotimes (i 3)
	(<:a :href (let ((i i))
		     (function/url ()
		       (render-graph-node (format nil "~A.~D" name i))))
	     (format nil "~A.~D" name i))	
	(<:br))
      (<:a :href (action/url ()
		     (ana-sayfa))
	   "Ana Sayfa")))))

(defun/cc ana-sayfa ()
  (<:html
   (<:body
    (render-graph-node "mehmet"))))

(defurl *app* "graph.core" ()
  (ana-sayfa))
