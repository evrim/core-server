(in-package :core-server)

;; Depreciated, Aug 2012
;; Google and fellows use OAuth 2.0 now. -evrim.

;; -------------------------------------------------------------------------
;; OpenID Funkall
;; -------------------------------------------------------------------------
;; http://openid.net/specs/openid-authentication-2_0.html
(defcommand <openid:funkall (http)
  ((mode :host local :initform (error "Provide :mode")))
  (:default-initargs :url
           "https://www.google.com/accounts/o8/ud"
      ;; "http://localhost:9000/accounts/o8/ud"
    ;; :debug t
    :method 'post))

(defparser parse-openid-key-value? (c (key (make-accumulator))
				      (value (make-accumulator))
				      lst)
  (:oom (:oom (:not #\:) (:type visible-char? c) (:collect c key))
	(:oom (:type visible-char? c) (:collect c value))	
	(:do (setq lst (cons (cons key value) lst)
		   key (make-accumulator)
		   value (make-accumulator)))
	(:lwsp?))
  (:return (cons (cons "timestamp" (get-universal-time)) (nreverse lst))))

(defmethod %%encode ((self <openid:funkall) params)
  (flet ((one (stream key-value)
	   (destructuring-bind (key . value) key-value
	     (string! stream key)
	     (char! stream #\:)
	     (string! stream value)
	     (char! stream #\Newline))))
    (let* ((zipped params))
      (with-core-stream (s "")
	(reduce #'one (cdr zipped) :initial-value (one s (car zipped)))
	(return-stream s)))))

(defmethod %%decode-base64 ((self <openid:funkall) str)
  (base64? (make-core-stream str)))

(defmethod run ((self <openid:funkall))
  (http.add-post self "openid.ns" "http://specs.openid.net/auth/2.0")
  (http.add-post self "openid.mode" (s-v 'mode))
  (parse-openid-key-value? (make-core-stream (call-next-method self))))

;; -------------------------------------------------------------------------
;; OpenID Associate
;; -------------------------------------------------------------------------
(defcommand <openid:associate (<openid:funkall)
  ((type :host local :initform "HMAC-SHA256")
   (session-type :host local :initform "no-encryption"))
  (:default-initargs :mode "associate"))

(defmethod run ((self <openid:associate))
  (http.add-post self "openid.assoc_type" (s-v 'type))
  (http.add-post self "openid.session_type" (s-v 'session-type))  
  (call-next-method self))

;; -------------------------------------------------------------------------
;; OpenID Request Authentication
;; -------------------------------------------------------------------------
(defcommand <openid:request-authentication (<openid:funkall)
  ((association :host local :initform (error "Provide :association"))
   (return-to :host local :initform (error "Provide :return-to")))
  (:default-initargs :mode "checkid_setup" ;; or "checkid_immediate"
    ))

(defmethod run ((self <openid:request-authentication))
  (http.add-post self "openid.ns" "http://specs.openid.net/auth/2.0")
  (http.add-post self "openid.mode" (s-v 'mode))
  (http.add-post self "openid.assoc_handle"
		 (cdr (assoc "assoc_handle" (s-v 'association)
			     :test #'equal)))
  (http.add-post self "openid.claimed_id"
		 "http://specs.openid.net/auth/2.0/identifier_select")
  (http.add-post self "openid.identity"
		 "http://specs.openid.net/auth/2.0/identifier_select")
  (http.add-post self "openid.return_to" (s-v 'return-to))
  (format nil "~A?~A" (uri->string (s-v 'url)) (s-v 'post-data)))

;; -------------------------------------------------------------------------
;; OpenID Verify Authentication
;; -------------------------------------------------------------------------
(defcommand <openid:verify-authentication (<openid:funkall)
  ((association :host local :initform (error "Provide :association")))
  (:default-initargs :url (error "Provide result :url")
    :mode "verify"))

(defparser comma-seperated? (c (token (make-accumulator)) lst)
  (:oom (:oom (:not #\,) (:type visible-char? c) (:collect c token))
	(:do (setq lst (cons token lst) token (make-accumulator))))
  (:return (nreverse lst)))

(defmethod run ((self <openid:verify-authentication))
  (with-slots (url) self
    (assert (equal (uri.query url "openid.assoc_handle")
    		   (cdr (assoc "assoc_handle" (s-v 'association)
    			       :test #'equal)))
    	    nil "Associations are not the same: ~A ~A"
    	    (uri.query url "openid.assoc_handle")
    	    (cdr (assoc "assoc_handle" (s-v 'association) :test #'equal)))
    (cond
      ((not (equal "id_res" (uri.query url "openid.mode")))
       (warn "Openid Mode is different than id_res mode:~A "
	     (uri.query url "openid.mode"))
       nil)
      (t
       (let* ((signed-keys (comma-seperated?
			    (make-core-stream
			     (uri.query url "openid.signed"))))
	      (signed-values (mapcar
			      (lambda (a)
				(uri.query url (format nil "openid.~A" a)))
			      signed-keys))
	      (str (%%encode self (mapcar #'cons signed-keys signed-values)))
	      (key (%%decode-base64 self
				    (cdr (assoc "mac_key" (s-v 'association)
						:test #'equal))))
	      (type (let ((type (cdr (assoc "assoc_type" (s-v 'association)
		    			    :test #'equal))))
		      (cond
		    	((equal type "HMAC-SHA256") :sha256)
		    	(t :sha1)))))
	 (when (s-v 'debug)
	   (describe (list 'signed-keys signed-keys))
	   (describe (list 'signed-values signed-values))
	   (format t "str-to-sign:~%~A~%" str)
	   (describe (list 'mac-key key))
	   (describe (list 'mac-type type))
	   (describe (list 'hmac-result (hmac key str type)))
	   (describe (list 'signature
			   (%%decode-base64 self
					    (uri.query url "openid.sig")))))
	 (if (equal (hmac key str type)
		    (crypto:byte-array-to-hex-string
		     (%%decode-base64 self (uri.query url "openid.sig"))))
	     t
	     nil))))))
