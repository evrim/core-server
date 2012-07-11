;; -------------------------------------------------------------------------
;; [Core-serveR] Quiz Example
;; -------------------------------------------------------------------------
;; Load the file with C-c C-l and visit, 
;; http://localhost:8080/quiz/

;; -------------------------------------------------------------------------
;; Define a new namespace
;; -------------------------------------------------------------------------
(defpackage :quiz
  (:use :cl :core-server :arnesi))

;; -------------------------------------------------------------------------
;; Switch to new namespace
;; -------------------------------------------------------------------------
(in-package :quiz)

;; -------------------------------------------------------------------------
;; Quiz Application Definition
;; -------------------------------------------------------------------------
(defapplication quiz-application (http-application)
  ()
  (:default-initargs :fqdn "quiz" :admin-email "aycan@core.gen.tr"))

;; -------------------------------------------------------------------------
;; Question Data as Lists
;; -------------------------------------------------------------------------
(defparameter *questions*
  '(("First Question?" ("1" "2" "3" "4" "5") 2)
    ("Second Question?" ("1" "2" "3" "4" "5") 3)
    ("Third Question?" ("1" "2" "3" "4" "5") 4)))

;; -------------------------------------------------------------------------
;; Question Data Accessors
;; -------------------------------------------------------------------------
(defun text (question)
  (car question))

(defun options (question)
  (cadr question))

(defun answer-index (question)
  (last1 question))

(defun right-option (question)
  (nth (- (answer-index question) 1) (options question)))

(defun right? (question answer)
  (equal (right-option question) answer))

;; -------------------------------------------------------------------------
;; Define 'page' function which gets body as a parameter
;; -------------------------------------------------------------------------
(defun/cc page (body)
  (<:html (<:head
	   (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8")
	   (<:title "Core Server - Quiz Example"))
	  (<:body
	   (<:h1 "[Core-serveR] - Quiz Example")
	   body)))

;; -------------------------------------------------------------------------
;; Display the result
;; -------------------------------------------------------------------------
(defun/cc send-result (wrongs)
  (send/forward
   (page
    (<:div
     (if wrongs
	 (<:div
	  (<:p "You answered wrong to some of the questions.")
	  (mapcar #'(lambda (w)
		      (<:div
		       (<:p "Question: " (text (car w)))
		       (<:p "The Answer should be: " (right-option (car w)))
		       (<:p "You answered: " (cadr w))))
		  wrongs))
	 (<:p "Congrats!"))
     (<:a :href "index" "Restart")))))

;; -------------------------------------------------------------------------
;; Ask a single question
;; -------------------------------------------------------------------------
(defun/cc ask-question (question)
  (send/suspend
   (page
    (<:form :method "POST"
	    :action (action/url ((ans "ans"))
		      (answer ans))
	    (<:p (text question))
	    (mapcar #'(lambda (o) 
			(<:span (<:input :type "radio" :name "ans" :value o) o))
		    (options question))
	    (<:br)
	    (<:p (<:input :type "submit" :value "Next"))))))

;; -------------------------------------------------------------------------
;; Ask Questions
;; -------------------------------------------------------------------------
(defun/cc ask-questions (questions)
  (let ((answers))
    (dolist (q questions)
      (let ((ans (ask-question q)))
	(unless (right? q ans)
	  (push (list q ans) answers))))
    answers))

;; -------------------------------------------------------------------------
;; Begin Quiz
;; -------------------------------------------------------------------------
(defun/cc begin-quiz ()
  (send/suspend
    (page
     (<:div
      (<:p "Welcome to the Quiz Example.")
      (<:p "Click to start your quiz.")
      (<:form :method "POST"
	      :action (action/url ()
			(answer nil))
	      (<:input :type "submit" :value "Begin"))))))

;; -------------------------------------------------------------------------
;; Register a handler
;; -------------------------------------------------------------------------
(defhandler "index" ((self quiz-application))
  (begin-quiz)
  (let ((wrongs (reverse (ask-questions *questions*))))
    (send-result wrongs)))

;; -------------------------------------------------------------------------
;; Create an application instance
;; -------------------------------------------------------------------------
(defparameter *quiz* (make-instance 'quiz-application))

;; -------------------------------------------------------------------------
;; Register our application to the server
;; -------------------------------------------------------------------------
;; (register *server* *quiz*)

;; EoF