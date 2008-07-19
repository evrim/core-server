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
   (remote-slot :host remote :initform (list "value0" "value1" "value2") :client-type array))
  (:default-initargs :local-slot nil))

(defmethod/local local-method ((self test-component) local-arg1)
  (list "local-method-result" local-arg1))

(defmethod/remote remote-method ((self test-component) remote-arg1)
  (return (list "remote-method-result" remote-arg1)))

(defurl *app1* "test.core" ()
  (send/suspend
    (<:html
     (<:head
      (<:script :src "/dojo/dojo/dojo.js" :type "text/javascript")
      (<:script :type "text/javascript"
	 (dojo-1.0 "test.core")
         (send/component (make-instance 'test-component
					:local-slot (list 1 2 3)))))
     (<:body
      (<:div :id "hobaa"
	     (<:script :type "text/javascript"
		(<:js
		 `(progn
		    (dojo.add-on-load
		     (lambda ()
		       (debug "Starting.." this)
		       (let ((component (new (test-component))))
			 (debug (+ "Result of local-method:" (component.local-method "local-method-arg1")))
			 (debug (+ "Result of remote-method:" (component.remote-method "remote-method-arg1"))))))))))))))


(defcomponent test1 ()
  ())

(defmethod/local test1-local-method ((self test1) arg)
  (list arg))

(defmethod/remote test1-remote-method ((self test1) arg)
  (list arg))

(defcomponent-ctor test1)


;; SERVER> (with-test-context (+context+ "blog" blog::*app*)
;; 	  (with-call/cc
;; 	    (ctor! *core-output* (make-instance 'test1))))
;; TEST1-LOCAL-METHOD is
;; an internal symbol
;; in #<PACKAGE "TR.GEN.CORE.SERVER">.
;; #<STANDARD-GENERIC-FUNCTION TEST1-LOCAL-METHOD (1)> is a generic function.
;; Its lambda-list is:
;;   (SELF ARG)
;; Its method-combination is:
;;   #<SB-PCL::LONG-METHOD-COMBINATION IT.BESE.ARNESI::CC-STANDARD NIL {1005292A21}>
;; Its methods are:
;;   (TEST1-LOCAL-METHOD :PRIMARY (TEST1 T))

;; test1 = function () {
;; this.prototype = {
;; test1LocalMethod: function (arg) {
;; return funcall("?s=QrpaIBkJ&k=act-ZpaMVnll");
;; }
;; };
;; return this.prototype;
;; };#<CORE-STANDARD-OUTPUT {1003575FB1}>