(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Jquery Extension
;; +----------------------------------------------------------------------------
(defpackage :tr.gen.core.server.jquery
  (:nicknames :jquery)
  (:use :common-lisp :core-server))

(in-package :jquery)

;; ----------------------------------------------------------------------------
;; Jquery Macros
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Jquery Stack
;; ----------------------------------------------------------------------------
;; ;;;;
;; ;;;; Interface for remote services
;; ;;;;
;; (defcomponent ajax-mixin ()
;;   ())

;; ;; TODO: first create activexobject, catch exception then create xmlhttprequest.
;; (defmethod/remote make-request ((self ajax-mixin))
;;   ;; (cond
;;   ;;       (window.*x-m-l-http-request ;; Gecko
;;   ;;        (setf request (new (*x-m-l-http-request))))
;;   ;;       (window.*active-x-object ;; Internettin Explorer
;;   ;;        (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
;;   ;;     (if (= null request)
;;   ;; 	(throw (new (*error "Exception: Cannot find usable XmlHttpRequest method, -core-server 1.0")))
;;   ;; 	(return request))
;;   (let ((req null))
;;     (try (setf req (new (*active-x-object "Msxml2.XMLHTTP")))
;; 	 (:catch (e1)
;; 	   (try (setf req (new (*active-x-object "Microsoft.XMLHTTP")))
;; 		(:catch (e2)
;; 		  (setf req null)))))
;;     (if (and (not req) (not (= (typeof *x-m-l-http-request) "undefined")))
;; 	(setf req (new (*x-m-l-http-request))))
;;     (return req)))

;; ;; return response directly, don't eval (text? xml?).
;; (defmethod/remote send-request ((self ajax-mixin) request url)
;;   (request.open "GET" url false)
;;   (request.send null)
;;   (if (= 200 request.status)
;;       (return request)
;;       (throw (new (*error (+ "Exception: Cannot send XmlHttpRequest: " url " -core-server 1.0"))))))

;; (defcomponent jqueryapi (ajax-mixin)
;;   ((script-location :host remote
;; 		    :initform "jquery-latest.min.js"
;; 		    :initarg :script-location
;; 		    :documentation "jQuery script location as url")))

;; (defmethod/remote init ((self jqueryapi))
;;   (when (= "undefined" (typeof j-query))
;;     (let ((req (this.make-request))
;; 	  (resp (this.send-request req this.script-location)))
;;       (return (eval (+ "{" resp.response-text "}"))))))

;; ;; TODO: implement retrycount, possibly using $.ajax.
;; (defmethod/remote jqueryfuncall ((self jqueryapi) url parameters retry-count)
;;   (let (result)
;;     (debug "server.funcall " url)
;;     ($.post url
;; 	    parameters
;; 	    (lambda (data textstatus)
;; 	      (setf result (eval (+ "{" data "}"))))
;; 	    "json")
;;     (return result)))

;; (defun/cc jquery (&optional scriptlocation)  
;;   (send/component (make-instance 'jqueryapi :script-location scriptlocation))
;;   ;; (<:js
;; ;;     `(progn
;; ;;        (setf jqueryapi (new (jqueryapi)))
;; ;;        (defun funcall (url parameters retry-count)
;; ;; 	 (return (jqueryapi.jqueryfuncall url parameters retry-count)))
;; ;;        (jqueryapi.init)))
;;   (error "fix jquery")
;;   )
