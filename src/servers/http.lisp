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

(in-package :core-server)

(defclass custom-http-peer (http-peer)
  ())

(defmethod render-404 ((request http-request) (response http-response))
  (setf (http-response.status-code response) (make-status-code 404))
  (rewind-stream (http-response.stream response))
  (checkpoint-stream (http-response.stream response))
  (with-html-output (http-response.stream response)
    (<:html
     (<:body
      "Core-serveR - URL Not Found")))
  response)

;; (defmethod eval-request ((self custom-http-peer) (request http-request))
;;   (let ((response (call-next-method))
;; 	(path (caar (uri.paths (http-request.uri request)))))
;;     (if (any #'(lambda (app)
;; 		 (when (string= path (web-application.fqdn app))
;; 		   (with-html-output (http-response.stream response)
;; 		     (dispatch app request response))))
;; 	     (server.applications (peer.server self)))
;; 	response
;; 	(render-404 self request response))))

(defmethod eval-request ((self custom-http-peer) (request http-request))
  (let ((response (call-next-method))
	(host (caadr (assoc 'HOST (http-request.headers request))))
	(app-name (caar (uri.paths (http-request.uri request)))))
    (cond
      ;; dispatch by app-name like http://servername/app-fqdn/js.core
      ((any #'(lambda (app)
		(when (string= app-name (web-application.fqdn app)) 
		  (with-html-output (http-response.stream response)
		    (pop (uri.paths (http-request.uri request)))
		    (dispatch app request response))))
	    (server.applications (peer.server self)))
       response)
      ;; dispatch by hostname like http://servername/js.core
      ((and (stringp host)
	    (any #'(lambda (app)
		     (when (string= host (web-application.fqdn app)) 
		       (with-html-output (http-response.stream response)
			 (dispatch app request response))))
		 (server.applications (peer.server self))))
       response)
      ;; catch-all via 404
      (t (render-404 request response)))))

(defclass http-server (socket-server)
  ((applications :accessor server.applications :initform '()))
  (:default-initargs :port 3001 :peer-class '(custom-http-peer)))

(defmethod register ((self http-server) (app http-application))
  (setf (server.applications self)
	(sort (cons app
		    (remove (web-application.fqdn app)
			    (server.applications self)
			    :test #'equal :key #'web-application.fqdn)) #'>
	      :key (lambda (app) (length (web-application.fqdn app)))))
  (setf (application.server app) self))

(defmethod unregister ((self http-server) (app http-application))
  (setf (server.applications self)
	(remove (web-application.fqdn app)
		(server.applications self) :test #'equal :key #'web-application.fqdn)))

(defmethod find-application ((self http-server) fqdn)
  (find fqdn (server.applications self) :key #'web-application.fqdn :test #'equal))

