(in-package :core-server)
;;;;
;;;; Interface for remote services
;;;;
(defcomponent ajax-mixin ()
  ())

;; TODO: first create activexobject, catch exception then create xmlhttprequest.
(defmethod/remote make-request ((self ajax-mixin))
  ;; (cond
  ;;       (window.*x-m-l-http-request ;; Gecko
  ;;        (setf request (new (*x-m-l-http-request))))
  ;;       (window.*active-x-object ;; Internettin Explorer
  ;;        (setf request (new (*active-x-object "Microsoft.XMLHTTP")))))
  ;;     (if (= null request)
  ;; 	(throw (new (*error "Exception: Cannot find usable XmlHttpRequest method, -core-server 1.0")))
  ;; 	(return request))
  (let ((req null))
    (try (setf req (new (*active-x-object "Msxml2.XMLHTTP")))
	 (:catch (e)
	   (try (setf req (new (*active-x-object "Microsoft.XMLHTTP")))
		(:catch (e)
		  (setf req null)))))
    (if (and (not req) (not (eq (typeof *x-m-l-http-request) "undefined")))
	(setf req (new (*x-m-l-http-request))))
    (return req)))

;; return response directly, don't eval (text? xml?).
(defmethod/remote send-request ((self ajax-mixin) request url)
  (request.open "GET" url false)
  (request.send null)
  (if (= 200 request.status)
      (return request)
      (throw (new (*error (+ "Exception: Cannot send XmlHttpRequest: " url " -core-server 1.0"))))))
