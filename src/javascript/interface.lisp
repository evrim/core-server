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

(defmacro js (&body body)
  "Converts unevaluated body to javascript and returns it as a string"
  (let ((render (expand-render
		 (optimize-render-1
		  (walk-grammar
		   `(:and
		     ,@(mapcar (compose
				(rcurry #'expand-javascript #'expand-javascript)
				#'fix-javascript-returns
				#'walk-js-form)
			       body))))
		 #'expand-render 's)))
    `(let ((s ,(if +indent-javascript+
		   `(make-indented-stream (make-core-stream ""))
		   `(make-compressed-stream (make-core-stream "")))))
       (funcall (lambda ()
		  (block rule-block
		    ,render)))
       (return-stream s))))

(defmacro js* (&body body)
  `(eval `(js ,,@body)))

(defmacro with-js (vars stream &body body)
  (with-unique-names (s)
    (let ((+js-free-variables+ vars))
      (labels ((expander (form expander)
		 (expand-render (optimize-render-1 (expand-javascript form expander))
				#'expand-render
				s)))
	`(block rule-block
	   (let ((,s ,stream))
	     ,(expand-render
	       (optimize-render-1
		(walk-grammar
		 (expand-javascript
		  (fix-javascript-returns
		   (walk-js-form (if (= 1 (length body)) (car body) `(progn ,@body))))
		  #'expand-javascript)))
	       #'expander s))
	   (char! ,stream #\Newline)
	   ,stream)))))

(defmacro defrender/js (name args &body body)
  (with-unique-names (stream)
    `(eval-when (:load-toplevel :compile-toplevel :execute)
       (defun ,name (,stream ,@args)
	 (with-js ,(extract-argument-names args) ,stream
	   ,@body)
	 (char! ,stream #\Newline)
	 ,stream))))

(defmacro defun/javascript (name params &body body)
  "Defun a function in both worlds (lisp/javascript)"
  `(prog1 (defun ,name ,params ,@body)
     (defrender/js ,(intern (string-upcase (format nil "~A/JS" name))) ()
       (setq ,name (lambda ,params
		     ,@body)))))

(defmacro jambda (arguments &body body)
  (with-unique-names (stream)
    (let ((stream (intern (symbol-name stream))))
      `(with-core-stream/cc (,stream "")
	 (with-js ,arguments ,stream
	   ,@body)
	 (return-stream ,stream)))))
