(in-package :core-server)

;; -------------------------------------------------------------------------
;; Facebook Graph HTTP Client
;; -------------------------------------------------------------------------
;; https://developers.facebook.com/docs/reference/api/
;; https://developers.facebook.com/docs/authentication/server-side/
;; https://developers.facebook.com/docs/authentication/permissions/
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2012
;; 
;; 1. Pop-up a dialog using <fb:oauth-uri
;; 2. Get *code* parameter in the return-uri <not here>
;; 3. Get an access token
;; 4. Use <fb:me and fellows.

(defun make-fb-uri (&key paths queries)
  (make-uri :scheme "https" :server "www.facebook.com" :paths paths :queries queries))

(defun make-fb-graph-uri (&key paths queries)
  (make-uri :scheme "https" :server "graph.facebook.com" :paths paths :queries queries))

;; -------------------------------------------------------------------------
;; Oauth Dialog
;; -------------------------------------------------------------------------
;; 1. First of all, generate an OAuth Dialog URI

;; https://www.facebook.com/dialog/oauth?client-id=APP_ID&response_type=token
;; &display=popup&redirect_uri=REDIRECT_URI

;; http://node1.coretal.net:8080/auth.core?mode=return&type=facebook&code=CODE#_=_
(defun <fb:oauth-uri (&key (client-id (error "Provide :client-id"))
			   (redirect-uri (error "Provide :redirect-uri"))
			   (scope "email")
			   (response-type "code")
			   (state nil))
  (let ((url (<oauth2:oauth-uri (make-fb-uri :paths '(("dialog") ("oauth")))
				:client-id client-id :redirect-uri redirect-uri
				:scope scope :response-type response-type
				:state state)))
    (setf (uri.queries url) (cons `("display" . "popup") (uri.queries url)))
    url))

;; 3. Get Access Token
;; (<fb:get-access-token :client-id ".." :client-secret "..."
;;                       :code *code :redirect-uri *uri)
;; ((timestamp . 3187371)
;;  ("access_token" . "AAACT3RIWEo0BANaEBCQDRBb.. ..glX8ZC6iZBpNue3uWRBiubZBdYMtQZDZD")
;;  ("expires" . "4889"))
(defcommand <fb:get-access-token (<oauth2:get-access-token)
  ()
  (:default-initargs
   :url (make-fb-graph-uri :paths '(("oauth") ("access_token")))))

;; -------------------------------------------------------------------------
;; Facebook Authorized Funkall
;; -------------------------------------------------------------------------
(defcommand <fb:authorized-funkall (<oauth2:authorized-funkall)
  ((meta-data :host local :initform t))
  (:default-initargs :url (make-fb-graph-uri)))

(defmethod http.setup-uri ((self <fb:authorized-funkall))
  (with-slots (meta-data) self
    (let ((uri (call-next-method self)))
      (when meta-data
	(http.add-query self "metadata" "1"))
      uri)))

;; -------------------------------------------------------------------------
;; Facebook Me Command
;; -------------------------------------------------------------------------
;; + Access to current user resources
;; MANAGER> (<fb:me :token *token)
;; #<JOBJECT {1004D1BA23}>
;; #<HTTP-RESPONSE (200 . OK) {10041C7D03}>
;; MANAGER> (describe *)
;; #<JOBJECT {1004D1BA23}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   ATTRIBUTES  = (:METADATA #<JOBJECT {1004D1A643}> :UPDATED_TIME
;;  "2012-06-25T18:36:31+0000" :VERIFIED TRUE :LOCALE "en_US" :TIMEZONE 3
;;  :EMAIL "evrimulu@gmail.com" :GENDER "male" :FAVORITE_ATHLETES
;;  (#<JOBJECT {10041EA0C3}>) :LOCATION #<JOBJECT {10041E1D83}> :USERNAME
;;  "eevrimulu" :LINK "http://www.facebook.com/eevrimulu" :LAST_NAME "Ulu"
;;  :FIRST_NAME "Evrim" :NAME "Evrim Ulu" :ID "700518347")

(defcommand <fb:me (<fb:authorized-funkall)
  ()
  (:default-initargs :url (make-fb-graph-uri :paths '(("me")))))

(defparser facebook-date-time? (year day month second hour minute gmt)
  (:fixnum? year) #\- (:fixnum? month) #\- (:fixnum? day)
  #\T (:fixnum? hour) #\: (:fixnum? minute) #\: (:fixnum? second)
  #\+ (:fixnum? gmt)
  (:return (encode-universal-time second minute hour day month year gmt)))

(defmethod http.evaluate ((self <fb:me) result response)
  (let ((result (call-next-method self result response)))
    (setf (get-attribute result :updated_time)
	  (facebook-date-time?
	   (make-core-stream (get-attribute result :updated_time))))
    (values result response)))

;; -------------------------------------------------------------------------
;; Other Facebook Commands
;; -------------------------------------------------------------------------
(defun make-fb-me-uri (&rest paths)
  (make-fb-graph-uri :paths `(("me") ,@paths)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar +fb-me-commands+ '(friends home feed likes movies music
			     books notes permissions photos albums
			     videos events groups checkins)))

(defmacro deffb-me-commands (lst)
  (let ((lst (if (listp lst) lst (eval lst))))
    `(progn
       ,@(mapcar
	  (lambda (command)
	    (let ((name (intern (string-upcase command) (find-package :<fb))))
	      `(defcommand ,name (<fb:me)
		   ()
		 (:default-initargs :url (make-fb-me-uri '(,(symbol-name command)))))))
	  lst))))

(deffb-me-commands +fb-me-commands+)

;; one more extra, videos-uploaded.
(defcommand <fb:videos-uploaded (<fb:me)
  ()
  (:default-initargs :url (make-fb-me-uri '("videos") '("uploaded"))))

(defcommand <fb:fetch (<fb:authorized-funkall)
  ((id :host local :initform (error "Provide :id") :initarg :id)))

(defmethod http.setup-uri ((self <fb:fetch))
  (let ((uri (call-next-method self)))
    (setf (uri.paths uri) (list (list (s-v 'id))))
    uri))

(defun <fb:authenticate (token)
  (let ((user (<fb:me :token token :debug-p t)))
    (list :email (format nil "~A@facebook.com"
			 (get-attribute user :username))
	  :name (get-attribute user :name))))


;; Access Token Debug Info

;; Working
;; https://graph.facebook.com:443/oauth/access_token?client%5Fid=162577700491917&client%5Fsecret=51b3bdef9fdf2e4175db9595f1fc3c89&code=AQDZYnYRBJRBz8pKc3PqsXYaXN0S5jj8tNxd%5FtwrDu5XIv0NwaJqjQV1y%5F0%5FOYcByxljiV452ukXccbikLn1%5Fw4C%2Di18goMNilQo%5Fna8U%5FhPar6ajCkHwZOyMUmo9jShB0rF9TCXQVLe0QxkWyAYtKzqLvJRg3Hy3T8vi0nMhc%2DyEU%2DmcrH2Qw8PybgKPea7pxc&redirect%5Furi=http%3A%2F%2Fnode1%2Ecoretal%2Enet%3A8080%2Fauth%2Ecore%3Faction%3Danswer%26provider%3Dfacebook%26return%2Dto%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth%2Ecore
;; Not working
;; https://graph.facebook.com:443/oauth/access_token?client%5Fid=162577700491917&client%5Fsecret=51b3bdef9fdf2e4175db9595f1fc3c89&code=AQCbh95Dr6ciqyIx0JnU%2D4iQ%2DFf5qZ%5FatPfgqriUeAmawvTLI9%5FYxN%2D5tvsv5q7b1ENz1PATSL9W%5FVzu%5FB0WVSfoN01bCahCjPj4e%2DwU79ojKtB1Tf5O%2D8vssKdNJiz%5FnxtdTRZfx1QFMt7S1Vd2uYrX%2DwS4GV0OTumSb7YXaT6kUAeeQxOsFcpM9PoQmAUQ248&redirect%5Furi=http%253A%252F%252Fnode1%252Ecoretal%252Enet%253A8080%252Fauth%252Ecore%253Faction%253Danswer%2526provider%253Dfacebook%2526return%25252Dto%253Dhttp%25253A%25252F%25252Flocalhost%25253A8080%25252Fauthentication%25252Fauth%25252Ecore
;; https://graph.facebook.com:443/oauth/access_token?client%5Fid=162577700491917&client%5Fsecret=51b3bdef9fdf2e4175db9595f1fc3c89&code=AQC8jTg8k9exmdSQBUh0uFy%5F4kLXj9OwaUpSmPaiagTutbQngNEvYhdy6QcMRXTjWTRhSGe4cptcqzq%5F4DTG0aAAedCWJnbouROjKNwUXVFyVpA1b8B8FCDxygcWhtj2lLG2YCnqFH5ibwksI5%5F%5FLNV2rni9c8NdUgWKxpvLjiRx%5F8wFhLqk1Ra%5FEmz%5FGz6%2D4ng&redirect%5Furi=http%3A%2F%2Fnode1%2Ecoretal%2Enet%3A8080%2Fauth%2Ecore%3Faction%3Danswer%26provider%3Dfacebook%26return%252Dto%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth%252Ecore
;; Not working
;; https://graph.facebook.com:443/oauth/access_token?client_id=162577700491917&client_secret=51b3bdef9fdf2e4175db9595f1fc3c89&code=AQBEwOjX8eOpcse-L0yuTtOj-fWJyLeNvUpc1srzTnxxbOAaoNWptww_ZxmIGPceScKlMneMJ1FHoXwIrcom3vOop5rYkG_En7pkUNvZFg3e32njPkT2mPZjv3Iv9dRAMxEXwv9DP1X3QcbCIvPMkAaL3Atx6j-qMgdCPHtISl0-LD5oYQ0Lz2luUYg4Iuvd_zs&redirect_uri=http%3A%2F%2Fnode1%2Ecoretal%2Enet%3A8080%2Fauth%2Ecore%3Faction%3Danswer%26provider%3Dfacebook%26return-to%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth%252Ecore
;; Working
;; https://graph.facebook.com:443/oauth/access_token?client_id=162577700491917&client_secret=51b3bdef9fdf2e4175db9595f1fc3c89&code=AQAu5wxOSY-AC8DOcoD8qn5YQ1-07FPggnuAvKhAgv7qOkqse0ARiqdxxA8eyb9sO2kuCR6zlYAYrWUxCUkmns8SBlZiBJYYtu36JChBWZIVQv1ZurcrEKSG1TIsOZrxBL1OUAkxNieleAxEFf36VzXdy53cRr9GwI6dCgb9eEV-aeHZFWQXWBj1smK_RPeoER8&redirect_uri=http%3A%2F%2Fnode1.coretal.net%3A8080%2Fauth.core%3Faction%3Danswer%26provider%3Dfacebook%26return-to%3Dhttp%253A%252F%252Flocalhost%253A8080%252Fauthentication%252Fauth.core
