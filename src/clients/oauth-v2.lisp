;; -------------------------------------------------------------------------
;; OAuth 2.0 Protocol Implementation
;; -------------------------------------------------------------------------
;; Date: Aug 2012
;; Author: Evrim Ulu <evrim@core.gen.tr>
(in-package :core-server)

;; No signatures in this protocol,
;; See this discussion here,
;; http://hueniverse.com/2010/09/oauth-2-0-without-signatures-is-bad-for-the-web/

;; 1. Pop-up a dialog using <oauth2:oauth-uri
;; 2. Get *code* parameter in the return-uri <not here>
;; 3. Get an access token
;; 4. Use <oauth2: commands and make api calls to service.

;; -------------------------------------------------------------------------
;; OAuth Dialog
;; -------------------------------------------------------------------------
;; 1. Generate an OAuth Dialog URI
(defmethod <oauth2:oauth-uri ((base-url uri)
			      &key
				(client-id (error "Provide :client-id"))
				(redirect-uri (error "Provide :redirect-uri"))
				(scope "email")
				(response-type "code")
				(state nil))
  (setf (uri.queries base-url)
	`(("client_id" . ,client-id)
	  ("response_type" . ,response-type)
	  ("scope" . ,scope)
	  ("redirect_uri" . ,(typecase redirect-uri
			       (uri (uri->string redirect-uri))
			       (string redirect-uri)))))

  (if state
      (uri.add-query base-url "state" state)
      base-url))


;; -------------------------------------------------------------------------
;; OAuth Exception
;; -------------------------------------------------------------------------
(define-condition <oauth2:exception (error)
  ((code :initarg :code :reader <oauth2:exception.code)
   (type :initarg :type :reader <oauth2:exception.type)
   (message :initarg :message :reader <oauth2:exception.message))
  (:report (lambda (condition stream)
	     (with-slots (code type message) condition
	       (format stream "~A:~A:~A" code type message))))
  (:documentation "OAuth Style Conditions"))

;; -------------------------------------------------------------------------
;; OAuth Command
;; -------------------------------------------------------------------------
(defcommand <oauth2:funkall (http)
  ())

(defmethod http.evaluate ((self <oauth2:funkall) result response)
  (if (eq 200 (http-response.status-code response))
      (return-from http.evaluate (values result response)))
    
  (let ((error (get-attribute result :error)))
    (if error
	(values (make-condition '<oauth2:exception
				:code (get-attribute error :code)
				:type (get-attribute error :type)
				:message (get-attribute error :message))
		response)
	(values result response))))

;; -------------------------------------------------------------------------
;; Access Token
;; -------------------------------------------------------------------------
(defclass+ <oauth2:access-token ()
  ((timestamp :host local :accessor <oauth2:access-token.timestamp
	      :initform (get-universal-time))
   (expires :host local :accessor <oauth2:access-token.expires)
   (token :host local :accessor <oauth2:access-token.token)
   (id-token :host local :accessor <oauth2:access-token.id-token)
   (token-type :host local :accessor <oauth2:access-token.token-type))
  (:ctor <oauth2:%make-access-token))

;; 3. Get Access Token
;; (<oauth2:get-access-token :client-id ".." :client-secret "..."
;;                          :code *code :redirect-uri *uri)
;; ((timestamp . 3187371)
;;  ("access_token" . "AAACT3RIWEo0BANaEBCQDRBb.. ..glX8ZC6iZBpNue3uWRBiubZBdYMtQZDZD")
;;  ("expires" . "4889"))
(defcommand <oauth2:get-access-token (<oauth2:funkall)
  ((client-id :initform (error "Provide :client-id") :host local)
   (client-secret :initform (error "Provide :client-secret") :host local)
   (code :initform (error "Provide :code") :host local)
   (redirect-uri :initform (error "Provide :redirect-uri") :host local))
  (:default-initargs
   :url (make-google-oauth-uri :paths `(("oauth") ("access_token")))))

(defmethod http.setup-uri ((self <oauth2:get-access-token))
  (with-slots (client-id client-secret code redirect-uri) self
    (let ((uri (call-next-method self)))
      (setf (uri.queries uri)
	    `(("client_id" . ,client-id)
	      ("client_secret" . ,client-secret)
	      ("code" . ,code)
	      ("redirect_uri" . ,(typecase redirect-uri
				   (string redirect-uri)
				   (uri (uri->string redirect-uri))))))
      uri)))

(defmethod http.evaluate ((self <oauth2:get-access-token) result response)
  (let ((content-type (http-response.get-content-type response)))
    (cond
      ((and (equal "application" (car content-type))
	    (equal "json" (cadr content-type)))
       (with-attributes (id_token expires_in token_type
				  access_token) result
	 (values (<oauth2:%make-access-token
		  :expires expires_in :token access_token
		  :id-token id_token :token-type token_type))))
      (t
       (flet ((get-key (key result) (cdr (assoc key result :test #'string=))))
	 (let* ((result (query? (make-core-stream result))))
	   (values (<oauth2:%make-access-token
		    :timestamp (get-universal-time)
		    :expires (parse-integer (get-key "expires" result))
		    :token (get-key "access_token" result))
		   response)))))))

;; -------------------------------------------------------------------------
;; OAuth Authorized Funkall
;; -------------------------------------------------------------------------
(defcommand <oauth2:authorized-funkall (<oauth2:funkall)
  ((token :host local :initform (error "Provide :token"))))

(defmethod http.setup-uri ((self <oauth2:authorized-funkall))
  (with-slots (token) self
    (let ((uri (call-next-method self)))
      (typecase token
	(string
	 (http.add-query self "access_token" token))
	(<oauth2:access-token
	 (http.add-query self "access_token" (<oauth2:access-token.token token))))      
      uri)))
