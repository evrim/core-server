(in-package :core-server)

(defpackage :core-server.examples.web-handlers
  (:nicknames :handlers)
  (:use :cl :core-server :arnesi))

(in-package :handlers)

;; Note C-c C-k to load file.

(defapplication web-handlers-example (http-application)
  ()
  (:default-initargs :fqdn "web-handlers"
    :admin-email "evrim@core.gen.tr"
    :htdocs-pathname "~/core-server/src/manager/wwwroot/"))

(defparameter *app* (make-instance 'web-handlers-example))
(register *server* *app*)

;; C-c M-p to change package.
;; C-x 1 to maximize buffer.

;; C-x C-f to open a file.
;; Open index.html.

;; Back to previous buffer C-x b RET

;; Define static handler
;; (defhandler/static #P "~/core-server/src/manager/wwwroot/index.html" "index.core")

;; Expand via C-c RET
;; Return via q

;; M-x slime-macroexpand-1-inplace
;; C-x u to undo.
;; C-M-6 to move up.
;; C-c C-c to compile.
;; C-k to delete line.
(defun make-index ()
  (<:HTML
   (HEAD (<:TITLE "Core Server - http://labs.core.gen.tr/")
         (<:META :HTTP--EQUIV "Content-Type" :CONTENT
                 "text/html; charset=utf-8")
         (<:LINK :REL "stylesheet" :HREF "style/reset.css")
         (<:LINK :REL "stylesheet" :HREF "style/common.css")
         (<:LINK :REL "stylesheet" :HREF "style/core.css")
         (<:LINK :REL "stylesheet" :HREF "style/style.css")
         (<:SCRIPT :SRC "library.core" :TYPE "text/javascript")
         (<:SCRIPT :SRC "index.core" :TYPE "text/javascript")
         (<:STYLE :TYPE "text/css" "body {background-color:#eee;}
      .container { 
      padding-top:65px;
      padding-bottom:35px;
      background:url(../images/index-bg.jpg);
      color:#fff;
      }
      .index-wrapper { color:#fff; padding-top:20px; }
      .index-wrapper h3 { font-size:18px; margin-bottom:20px; }
      .core-clock-widget { margin-top:15px; text-align:right; color:#000; }
      .footer {background:transparent;}
      .field .value span.validation { display:block; padding:5px 0; }
      .field .value span.invalid {  color:#ce0000; }
      .field .value span.valid {  color:#fff; }
    "))
   (<:BODY :CLASS "text-center"
           (<:DIV :CLASS "container"
                  (<:DIV :CLASS "center max-width text-left header"
                         (<:DIV :CLASS "left" (<:H1 "[Core-serveR]")
                                (<:H2 "A Common-Lisp Application Server"))
                         (<:DIV :CLASS "right"
				;; Add Clock
				(<core:simple-clock :ID "clock"))
                         (<:DIV :CLASS "clear"))
                  (<:DIV :CLASS "pad10")
                  (<:DIV :CLASS "index-wrapper"
                         (<:DIV :CLASS "center max-width text-left"
				;; Add Login Box
				(<core:login :CLASS "text-left right" :ID "login")
                                (<:DIV :CLASS "left" (<:H3 "Welcome!")
                                       (<:P
                                        "Server documentation is located at: ")
                                       (<:P
                                        (<:A :TARGET "_blank" :HREF
                                             "http://labs.core.gen.tr/"
                                             "http://labs.core.gen.tr/ "))
                                       (<:BR) (<:P "Follow us on Twitter!")
                                       (<:P
                                        (<:A :TARGET "_blank" :HREF
                                             "http://twitter.com/core_server"
                                             "http://twitter.com/core_server "))))
                         (<:DIV :CLASS "clear")))
           (<:DIV :CLASS "max-width center text-center footer clear"
                  (<:DIV :CLASS "left" "&copy; 2006-2012, 
      Kor Information Technologies Ltd.")
                  (<:DIV :CLASS "right"
                         (<:A :HREF "http://www.core.gen.tr/"
                              "http://www.core.gen.tr"))))))

;; Remember to compile C-c C-c
;; C-x C-s to save.

;; C-x 2 to divide.
;; C-x o to switch
;; C-c s r to REPL

;; C-c C-c
(defparameter +users+ '(("evrim". "core-server")
			("aycan". "core-server")))

(defparameter +page+ (make-index))
;; C-c C-c

(defhandler "index.core" ((self web-handlers-example))
  (destructuring-bind (username password) (send/suspend +page+)
    (flet ((get-user ()
	     (let ((user (assoc username +users+ :test #'equal)))
	       (and user (equal password (cdr user))
		    user))))
      (aif (get-user)
	   (continue/js
	    (progn
	      (update-session :user it)
	      (lambda (self k)
		(k
		 (setf window.location.href "homepage.html")))))
	   (continue/js
	    (lambda (self k) (k nil)))))))

;; Navigate to your browser and open
;; http://localhost:8080/web-handlers/index.core

;; MARK 2

;; Core Server -- http://labs.core.gen.tr/
;; Documentation: http://labs.core.gen.tr/#web-handlers
;; Github https://github.com/evrim/core-server/


;; M-> to go to end.
;; C-c C-c to compile.
;; C-c s d to return to debugger.
;; 0 to exit debugger.
;; 1 to retry.

;; C-x C-s to save.

(defhandler "homepage.html" ((self web-handlers-example))
  (<:html
   (<:body
    (<:h1 "Hello " (car (query-session :user)) "!"))))



;; PART 2
;; http://labs.core.gen.tr/#web-handlers-2

(defun/cc %write-page (stream)
  (let ((clock (<core:simple-clock))
	(login (<core:login)))
    (with-js (clock login) stream
      (let ((clock clock) (login login))
	(add-on-load
	 (lambda ()
	   (clock (document.get-element-by-id "clock")
		  (lambda (clock)
		    (login (document.get-element-by-id "login")
			   (lambda (login)
			     (_debug "starting")))))))))))

;; See src/javascript/macro.lisp
;;
;; (defmacro rebinding-js (vars stream &body body)
;;   (let ((vars (mapcar #'ensure-list vars)))
;;     `(let (,@(reduce0 (lambda (acc a)
;; 			(if (null (cdr a))
;; 			    acc
;; 			    (cons a acc)))
;; 		      (reverse vars)))
;;        (with-js ,(mapcar #'car vars) ,stream
;; 	 (let ,(mapcar (lambda (a) (list (car a) (car a))) vars)
;; 	   (add-on-load
;; 	    (lambda ()
;; 	      ,@body)))))))

(defun/cc %%write-page (stream)
  (rebinding-js ((clock (<core:simple-clock)) (login (<core:login))) stream
      (clock (document.get-element-by-id "clock")
	     (lambda (clock)
	       (login (document.get-element-by-id "login")
		      (lambda (login)
			(_debug "starting")))))))

;; See src/javascript/macro.lisp
;; (defmacro rebinding-js/cc (vars stream &body body)
;;   `(rebinding-js ,vars ,stream
;;      (with-call/cc ,@body)))

(defun/cc %%%write-page (stream)
  (let ((clock (<core:simple-clock)))
    (rebinding-js/cc (clock (login (<core:login))) stream
      (call/cc clock (document.get-element-by-id "clock"))
      (call/cc login (document.get-element-by-id "login")))))

(defhandler "index\.core" ((self web-handlers-example))
  (destructuring-bind (username password) (javascript/suspend #'%%%write-page)
    (flet ((get-user ()
	     (let ((user (assoc username +users+ :test #'equal)))
	       (and user (equal password (cdr user))
		    user))))
      (aif (get-user)
	   (continue/js
	    (progn
	      (update-session :user it)
	      (lambda (self k)
		(k
		 (setf window.location.href "homepage.html")))))
	   (continue/js
	    (lambda (self k) (k nil)))))))