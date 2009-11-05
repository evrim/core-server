(defpackage :quiz
  (:use :cl :core-server :arnesi))

(in-package :quiz)

;; Create an application
(defparameter *quiz-app*
  (make-instance 'http-application
		 :fqdn "quiz"
		 :admin-email "aycan@core.gen.tr"))

(defparameter *questions* '(("First Question?" ("1" "2" "3" "4" "5") 2)
			    ("Second Question?" ("1" "2" "3" "4" "5") 3)
			    ("Third Question?" ("1" "2" "3" "4" "5") 4)
			    ("Fourth Question?" ("1" "2" "3" "4" "5") 1)))

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

;; Render a page
(defun/cc page (body)
  (<:html
   (<:head
    (<:meta :http--equiv "Content-Type" :content "text/html; charset=utf-8"))
   (<:body
    body)))

;; Display the result
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
     (<:a :href "begin" "Restart")))))

;; Read an integer
(defun/cc send-question (question)
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
	    (<:input :type "submit" :value "Next")))))

(defun/cc ask-questions (questions)
  (let ((answers))
    (dolist (q questions)
      (let ((ans (send-question q)))
	(unless (right? q ans)
	  (push (list q ans) answers))))
    answers))

(defun/cc begin-quiz ()
  (send/suspend
    (page
     (<:div
      (<:p "Welcome to the Quiz Example.")
      (<:form :method "POST"
	      :action (action/url ()
			(answer nil))
	      (<:input :type "submit" :value "Begin"))))))

;; Register a handler
(defhandler "begin" ((self http-application))
  (begin-quiz)
  (let ((wrongs (reverse (ask-questions *questions*))))
    (send-result wrongs)))

;; Register our application to the server
(register *server* *quiz-app*)