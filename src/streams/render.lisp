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
;;;-----------------------------------------------------------------------------
;;; Render
;;;-----------------------------------------------------------------------------
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defgeneric expand-render (form expander stream &optional continue checkpoint)
    (:documentation "Special dsl for rendering."))

  (defmethod expand-render ((form form) expander (stream symbol) &optional continue checkpoint)
    (expand-grammar form #'expand-render stream continue checkpoint))

  (defmethod expand-render ((form t) expander (stream symbol) &optional continue checkpoint)
    `(write-stream ,stream ,form))

  (defmethod expand-render ((form bind-form) expander (stream symbol) &optional continue checpoint)
    `(,(func form) ,stream ,@(args form)))

  (defmethod expand-render ((form sep-form) expander (stream symbol) &optional continue checkpoint)
    (if (symbolp (children form))
	`(prog1 ,continue
	   ,(funcall expander (walk-grammar `(car ,(children form)))
		     expander stream continue checkpoint)
	   (mapcar (lambda (atom)		 
		     ,(funcall expander (walk-grammar `(:and ,(seperator form) atom))
			       expander stream continue checkpoint))
		   (cdr ,(children form))))
	(progn
	  (if (eq 'quote (car (children form))) (setf (children form) (cadr (children form))))
	  `(prog1 ,continue
	     ,(funcall expander (walk-grammar (car (children form)))
		       expander stream continue checkpoint)
	     ,@(mapcar (lambda (atom)
			 (funcall expander (walk-grammar `(:and ,(seperator form) ,atom))
				  expander stream continue checkpoint))
		       (cdr (children form))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defrender (name args &rest body)
    "Rule definition macro that provides a common lexical environment for rules."
    (with-unique-names (stream)      
      `(defun ,name ,(cons stream args)
	 (block rule-block
	   (let ((cp (current-checkpoint ,stream)))
	     (declare (ignorable cp))
	     ,(expand-render (walk-grammar `(:checkpoint ,@body (:commit))) #'expand-render stream)
	     ,stream))))))

(defun byte! (stream byte)
  (write-stream stream byte))

(defun char! (stream char)
  (let ((code (char-code char)))
    (if (> code 127) 
	(reduce #'(lambda (acc atom)
		    (declare (ignore acc))
		    (byte! stream atom)
		    nil)
		(string-to-octets (string char) :utf-8) :initial-value nil)
	(byte! stream code))))

(defun string! (stream string)
  (write-stream stream (string-to-octets string :utf-8)))

(defun version! (stream version)
  (fixnum! stream (car version))
  (reduce #'(lambda (acc atom)
	      (declare (ignore acc))
	      (char! stream #\.)
	      (fixnum! stream atom)
	      nil)
	  (cdr version) :initial-value nil))

(defun symbol! (stream symbol)
  (string! stream (string-downcase (symbol-name symbol))))

(defun fixnum! (stream number)
  (string! stream (format nil "~D" number)))

(defun quoted! (stream string)
  (char! stream #\")
  (reduce #'(lambda (acc atom)
	      (declare (ignore acc))
	      (cond
		((or (char= #\" atom) (char= #\\ atom))
		 (char! stream #\\) (char! stream atom))
		((or (char= #\Newline atom) (char= #\Linefeed atom))
		 )
		(t (char! stream atom)))
	      nil)
	  string
	  :initial-value nil)
  (char! stream #\"))

(defun quoted-fixnum! (stream number)
  (quoted! stream (format nil "~D" number)))

(defvar +hex-alphabet+
  #.(reduce #'(lambda (acc atom) (push-atom (char-code atom) acc) acc)
	    "0123456789ABCDEF" :initial-value (make-accumulator :byte)))

(defun hex-value! (stream hex)
  (byte! stream (aref +hex-alphabet+ (floor (/ hex 16))))
  (byte! stream (aref +hex-alphabet+ (rem hex 16))))

;;; sugars
(defmacro with-separator ((var lst sep stream) &body body) 
  `(let ((,var (car ,lst)))
     ,@body
     (reduce #'(lambda (acc ,var)
		 (declare (ignore acc))
		 (char! ,stream ,sep)
		 (char! ,stream #\ )
		 ,@body)
	     (cdr ,lst) :initial-value nil)))
