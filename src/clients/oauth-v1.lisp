;; -------------------------------------------------------------------------
;; OAuth 1.0 Protocol Implementation aka RFC 5849
;; -------------------------------------------------------------------------
;; http://tools.ietf.org/html/rfc5849
;; Date: Aug 2012
;; Author: Evrim Ulu <evrim@core.gen.tr>
(in-package :core-server)

;; OAuth 1.0 Flow:
;; http://developer.yahoo.com/oauth/guide/oauth-auth-flow.html
;; Similar to openID spec with signatures, assumed to be CCA-2 compliant.

;; -------------------------------------------------------------------------
;; OAuth Request Token
;; -------------------------------------------------------------------------
(defclass+ <oauth1:request-token ()
  ((token :host local :initform (error "Provide :token")
	  :accessor <oauth1:request-token.token)
   (token-secret :host local :initform (error "Provide :token-secret")
		 :accessor <oauth1:request-token.token-secret)
   (callback-confirmed :host local :initform nil
		       :accessor <oauth1:request-token.callback-confirmed))
  (:ctor <oauth1:%make-request-token))

;; -------------------------------------------------------------------------
;; OAuth Funkall
;; -------------------------------------------------------------------------
(defcommand <oauth1:funkall (http)
  ((mode :host local :initform :query :documentation "Can be :header or :query")
   (consumer-key :host local :initform (error "Provide :consumer-key"))
   (consumer-secret :host local :initform (error "Provide :consumer-secret"))
   (version :host local :initform "1.0")
   (signature-method :host local :initform "HMAC-SHA1"))
  (:default-initargs :method 'post))

(defmethod <oauth1:funkall.signature-key ((self <oauth1:funkall))
  (concat (escape-as-uri (slot-value self 'consumer-secret)) "&"))

(defmethod <oauth1:funkall.sign ((self <oauth1:funkall) (string string))
  (with-slots (signature-method) self
    ;; http://oauth.net/core/1.0/#sig_base_example 9.2
    (let ((key (<oauth1:funkall.signature-key self)))
      (cond
	((equal "HMAC-SHA1" signature-method)
	 (let ((mac (cadr (multiple-value-list (hmac key string :sha1)))))
	   (with-core-stream (s "")
	     (base64! s mac)
	     (return-stream s))))
	;; ((equal "PLAINTEXT" signature-method) consumer-secret)
	(t (error "Unsupported signature-method ~A" signature-method))))))

(defmethod <oauth1:funkall.build-signature ((self <oauth1:funkall) parameters)
  (with-slots (url method) self
    (with-slots (scheme server port paths) url
      (flet ((one (a)
	       (destructuring-bind (key . value) a
		 (escape-as-uri (concat key "=" (escape-as-uri value))))))
	(let ((parameters (sort (copy-list (append (uri.queries url) parameters))
				#'string< :key #'car)))
	  (reduce (lambda (acc parameter)
		    (concat acc (escape-as-uri "&") (one parameter)))
		  (cdr parameters)
		  :initial-value
		  (concat (symbol-name method) "&"
			  (escape-as-uri
			   (uri->string (make-uri :scheme scheme :server server
						  :port port :paths paths)))
			  "&" (one (car parameters)))))))))

(defmethod <oauth1:funkall.parameters ((self <oauth1:funkall))
  (with-slots (consumer-key callback version signature-method url) self
    (let ((nonce (random-string 32))
  	  (timestamp (format nil "~A" (get-unix-time))))
      `(("oauth_consumer_key" . ,consumer-key)
	("oauth_nonce" . ,nonce)
	("oauth_signature_method" . ,signature-method)
	("oauth_timestamp" .  ,timestamp)
	("oauth_version" . ,version)))))

(defmethod <oauth1:funkall.header ((self <oauth1:funkall))
  (let* ((parameters (<oauth1:funkall.parameters self))
	 (str-to-sign (<oauth1:funkall.build-signature self parameters)))
    (cons `("oauth_signature" . ,(<oauth1:funkall.sign self str-to-sign))
	  parameters)))

;; If we are in :header mode, then add an Authorization: OAuth header
(defmethod http.make-request ((self <oauth1:funkall))
  (let ((req (call-next-method self)))
    (when (eq :header (slot-value self 'mode))
      (let ((parameters (mapcar (lambda (a)
				  (destructuring-bind (key . value) a
				    `(,key . ,(escape-as-uri value))))
				(<oauth1:funkall.header self))))
	(http-request.add-request-header req 'authorization
					 `(oauth ,@parameters))))
    req))

;; Otherwise, we are in :query mode, add query parameters
(defmethod http.setup-uri ((self <oauth1:funkall))
  (let ((url (call-next-method self)))
    (when (eq :query (slot-value self 'mode))
      (mapcar (lambda (a)
		(destructuring-bind (key . value) a
		  (uri.add-query url key value)))
	      (reverse (<oauth1:funkall.header self))))
    url))

;; -------------------------------------------------------------------------
;; OAuth Get Request Token
;; -------------------------------------------------------------------------
;; Stage 1. Obtain a REQUEST TOKEN
(defcommand <oauth1:get-request-token (<oauth1:funkall)
  ((callback :host local :initform (error "Provide :callback"))))

(defmethod <oauth1:funkall.parameters ((self <oauth1:get-request-token))
  (with-slots (callback) self
    (let ((parameters (call-next-method self)))
      (cons `("oauth_callback" . ,callback) parameters))))

(defmethod http.parse-response ((self <oauth1:get-request-token)
				(stream core-fd-io-stream))
  (let ((response (call-next-method self stream)))
    (prog1 response
      (setf (http-response.entities response)
	    (query? (http-response.stream response))))))

(defmethod http.evaluate ((self <oauth1:get-request-token) result response)
  (flet ((get-key (name) (cdr (assoc name result :test #'equal))))
    (values (<oauth1:%make-request-token
	     :token (get-key "oauth_token")
	     :token-secret (get-key "oauth_token_secret")
	     :callback-confirmed (json-deserialize
				  (get-key "oauth_callback_confirmed")))
	    response)))

;; -------------------------------------------------------------------------
;; OAuth Authorize Url
;; -------------------------------------------------------------------------
;; STAGE 2. Redirect user to AUTHORIZATION URL
(defmethod <oauth1:authorize-url ((base-url uri)
				  &key (token (error "Provide request :token")))
  
  (prog1 base-url
    (uri.add-query base-url "oauth_token"
		   (typecase token
		     (string token)
		     (<oauth1:request-token (slot-value token 'token))))))

;; -------------------------------------------------------------------------
;; OAuth Access Token
;; -------------------------------------------------------------------------
(defclass+ <oauth1:access-token ()
  ((token :host local)
   (token-secret :host local))
  (:ctor <oauth1:%make-access-token))

;; -------------------------------------------------------------------------
;; OAuth Get Access Token
;; -------------------------------------------------------------------------
;; StAGE 3. Get an ACCESS TOKEN
(defcommand <oauth1:get-access-token (<oauth1:funkall)
  ((verifier :host local :initform (error "Provide :verifier"))
   (request-token :host local :initform (error "Provide :request-token"))))

(defmethod <oauth1:funkall.signature-key ((self <oauth1:get-access-token))
  (with-slots (consumer-secret request-token) self
    (concat (escape-as-uri consumer-secret) "&"
	    (escape-as-uri (slot-value request-token 'token-secret)))))

(defmethod <oauth1:funkall.parameters ((self <oauth1:get-access-token))
  (with-slots (request-token) self
    (with-slots (token) request-token
      (let ((parameters (call-next-method self)))
	(cons `("oauth_token" . ,token) parameters)))))

(defmethod http.setup-uri ((self <oauth1:get-access-token))
  (let ((url (call-next-method self)))
    (with-slots (verifier) self
      (prog1 url (http.add-post self "oauth_verifier" verifier)))))

(defmethod http.parse-response ((self <oauth1:get-access-token)
				(stream core-fd-io-stream))
  (let ((response (call-next-method self stream)))
    (prog1 response
      (setf (http-response.entities response)
	    (query? (http-response.stream response))))))

(defmethod http.evaluate ((self <oauth1:get-access-token) result response)
  (flet ((get-key (name) (cdr (assoc name result :test #'equal))))
    (values (<oauth1:%make-access-token
	     :token (get-key "oauth_token")
	     :token-secret (get-key "oauth_token_secret"))
	    response)))

(deftrace oauth1 '(<oauth1:funkall.header <oauth1:funkall.build-signature
		   <oauth1:funkall.sign <oauth1:funkall.signature-key
		   <oauth1:funkall.parameters
		   <oauth1:authorize-url <oauth1:get-request-token
		   <oauth1:%make-request-token <oauth1:get-access-token))

;; https://dev.twitter.com/docs/api/1/post/oauth/request_token
;; Authorization Header:
;; OAuth oauth_nonce= "K7ny27JTpKVsTgdyLdDfmQQWVLERj2zAK5BslRsqyw", oauth_callback= "http%3A%2F%2Fmyapp.com%3A3005%2Ftwitter%2Fprocess_callback", oauth_signature_method= "HMAC-SHA1", oauth_timestamp= "1300228849", oauth_consumer_key= "OqEqJeafRSF11jBMStrZz", oauth_signature= "Pc%2BMLdv028fxCErFyi8KXFM%2BddU%3D", oauth_version= "1.0"

;; AUTHORIZATION: oauth oauth_callback="http%3A%2F%2Fnode1.coretal.net%3A8080%2Fauth.html%3Faction%3Danswer%26provider%3Dtwitter",oauth_consumer_key="RLdbWgOhvyLsL7x1Lxryw",oauth_nonce="lkqBmezOneWUjvkbWSEXcxtWwmnzNDxq",oauth_signature="ZjJhOGJlNDYwNzhlYjc2YjYxYmVhMmU5YzVjZGNiOGU3ZmVkNDQ4OQ%3D%3D",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1344310881",oauth_version="1.0"
;; USER-AGENT: [Core-serveR] (http://labs.core.gen.tr)

;; (defparameter +timestamp+ "1318467427")
;; (defparameter +nonce+ "ea9ec8429b68d6b77cd5600adbbb0456")
;; (in-package :manager)
;; (defparameter *key "cChZNFj6T5R0TigYB9yd1w")
;; (defparameter *secret "L8qq9PZyRg6ieKGEKhZolGC0vJWLw8iEJ88DRdyOg")
;; (defparameter *callback (unescape-as-uri"http%3A%2F%2Flocalhost%2Fsign-in-with-twitter%2F"))
;; (defparameter *result "F1Li3tvehgcraF8DMJ7OyxO4w9Y%3D")

;; http://tools.ietf.org/html/rfc5849#section-3.4
;; POST&http%3A%2F%2Fexample.com%2Frequest&a2%3Dr%2520b%26a3%3D2%2520q
;; %26a3%3Da%26b5%3D%253D%25253D%26c%2540%3D%26c2%3D%26oauth_consumer_
;; key%3D9djdj82h48djs9d2%26oauth_nonce%3D7d8f3e4a%26oauth_signature_m
;; ethod%3DHMAC-SHA1%26oauth_timestamp%3D137131201%26oauth_token%3Dkkk
;; 9d7dh3k39sjv7


;; https://photos.example.net/request_token?oauth_consumer_key=dpf43f3p2l4k3l03&oauth_signature_method=PLAINTEXT&oauth_signature=kd94hf93k423kf44%26&oauth_timestamp=1191242090&oauth_nonce=hsu94j3884jdopsl&oauth_version=1.0
;; #<URI https://photos.example.net/(request_token)?
;; (oauth_consumer_key . dpf43f3p2l4k3l03)
;; (oauth_signature_method . PLAINTEXT)
;; (oauth_signature . kd94hf93k423kf44&)
;; (oauth_timestamp . 1191242090)
;; (oauth_nonce . hsu94j3884jdopsl)
;; (oauth_version . 1.0)# {101371CEB3}>

;; (uri->string (uri? (make-core-stream "https://photos.example.net/request_token?oauth_consumer_key=dpf43f3p2l4k3l03&oauth_signature_method=PLAINTEXT&oauth_signature=kd94hf93k423kf44%26&oauth_timestamp=1191242090&oauth_nonce=hsu94j3884jdopsl&oauth_version=1.0")))

;; https://api.twitter.com/oauth/request_token?oauth_consumer_key=RLdbWgOhvyLsL7x1Lxryw&oauth_nonce=UDxrfQCZZMLMUKraCiHaliXVnSLplcjn&oauth_signature_method=PLAINTEXT&oauth_timestamp=1344373681&oauth_version=1.0&oauth_signature=dsMKwAquRgGYog72YXvJqRDOYlWjXpHJWZa4u6yKU%26

;; #<URI https://api.twitter.com/(oauth)/(request_token)?
;; (oauth_consumer_key . RLdbWgOhvyLsL7x1Lxryw)
;; (oauth_signature_method . PLAINTEXT)
;; (oauth_signature . dsMKwAquRgGYog72YXvJqRDOYlWjXpHJWZa4u6yKU&)
;; (oauth_timestamp . 1344373681)
;; (oauth_nonce . UDxrfQCZZMLMUKraCiHaliXVnSLplcjn)
;; (oauth_version . 1.0)
;; # {1013A433B3}>

;; (with-slots (consumer-key consumer-secret) (database.get *app* :twitter)
;;   (octets-to-string 
;;    (<oauth1:get-request-token :callback "https://node1.coretal.net/auth.html"
;; 			      :consumer-key consumer-key
;; 			      :consumer-secret consumer-secret
;; 			      :debug-p t :parse-p nil)
;;    :utf-8))

;; "oauth_token=v13hTVvdzG84dlHvllb07igPkf3FgPTJAKRHrBPO06s&oauth_token_secret=KzhK8dkVMdFOXM5ObqTEY5D9YVtfdxNU54ORmhEaQY&oauth_callback_confirmed=true"
