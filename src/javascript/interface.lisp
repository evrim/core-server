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

;;+-----------------------------------------------------------------------------
;;| Javascript Interface
;;+-----------------------------------------------------------------------------
;;
;; This file contains interface function to use Javascript library.
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +indent-javascript+ t))

(defmacro js (body)
  "Converts unevaluated body to javascript and returns it as a string"
  `(let ((s ,(if +indent-javascript+
		 `(make-indented-stream "" 2)
		 `(make-compressed-stream ""))))
     (funcall (lambda ()
		(block rule-block		  
		  ,(expand-render
		    (walk-grammar
		     (expand-javascript (walk-form-no-expand body)
					#'expand-javascript))
		    #'expand-render 's))))
     (return-stream s)))

(defun js* (&rest body)
  "Converts evaluated body to javascript and returns it as a string"
  (eval `(js ,@body)))

(defmacro defun/javascript (name params &body body)
  "Defun a function in both worlds (lisp/javascript)"
  `(prog1
     (defun ,name ,params ,@body)
     (defun ,(intern (string-upcase (format nil "~A!" name))) (s)
       (block rule-block		  
	 ,(expand-render (walk-grammar
			  (expand-javascript
			   (make-instance 'setq-form
					  :var name
					  :value (fix-javascript-returns
						  (walk-form-no-expand
						   `(lambda ,params
						      ,@body))))
			   #'expand-javascript))
			 #'expand-render 's)))))

;; (defun/javascript fun1 (a)
;;   (let ((a (lambda (a b c)
;; 	     (list a b c))))
;;     (return a)))

;; (defmacro defun/parenscript (name params &body body)
;;   `(prog1
;;        (defun ,name ,params ,@body)
;;      (defun ,(intern (string-upcase (format nil "~A!" name))) (s)
;;        (write-stream s
;; 		     ,(js:js* `(lambda ,params
;; 				 ,@body))))))

;; (defun/parenscript fun2 (a)
;;   (let ((a (lambda (a b c)
;; 	     (list a b c))))
;;     (return a)))

;; (js+ 
;;   (+ 1 1)
;;   (+ 2 2))

;; (defun <:js (&rest body)
;;   "Convert body to javascript and return it as a string, this is an ucw+
;; backward compatiblity macro, and should not be used."
;;   (with-unique-names (output)
;;     (eval
;;      `(let ((,output (if +context+
;; 			 (http-response.stream (response +context+))
;; 			 *core-output*)))
;; 	(funcall (lambda ()
;; 		   (block rule-block
;; 		     ,(expand-render
;; 		       (walk-grammar
;; 			`(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
;; 					 (mapcar #'walk-form-no-expand body))
;; 			       #\Newline))
;; 		       #'expand-render output)
;; 		     nil)))))))
