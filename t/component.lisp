(in-package :core-server)

(eval-always
  (defclass test-application (http-application)
    ())
  (when (boundp '*app1*)
    (unregister core-server::*server* *app1*))
  (defparameter *app1*
    (make-instance 'test-application
		   :fqdn "test"
		   :admin-email "evrim@core.gen.tr"))
  (register core-server::*server* *app1*))

(defcomponent test-component ()
  ((local-slot :host local :client-type array)
   (mocal-slot :host local :client-type object)
   (remote-slot :host remote :initform (list "removt" "defim" "mesine") :client-type array))
  (:default-initargs :local-slot nil :mocal-slot (list "core" "cok" "pis" "core")))

(defmethod/local mocal-method ((self test-component))
  (list 1 2 (list 3 "hobaa")))

(defmethod/local local-method ((self test-component) local-arg1)
  (list "local-method-result" local-arg1))

(defmethod/remote remote-method ((self test-component) remote-arg1)
  (return (list "remote-method-result" remote-arg1)))

(defurl *app1* "test.core" ()
  (send/suspend
    (<:html
     (<:head
      (dojo-javascript-stack)      
      (<:script :type "text/javascript"
       (send/component (make-instance 'test-component
				      :local-slot (list 1 2 3)
				      ;; :remote-slot (list "a" "b" "c")
				      ))))
     (<:body
      "body"))))

;;(with-call/cc (apply (function bir) (list (make-instance 'window))))