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

;;; Got from parenscript.
;;; This is just an effort to fix http-equiv conversion problem
;;; ecmascript standard:
;;; http://www.ecma-international.org/publications/standards/Ecma-262.htm

;;; javascript name conversion

(defparameter *special-chars*
  '((#\! . "Bang")
    (#\? . "What")
    (#\# . "Hash")
    (#\@ . "At")
    (#\% . "Percent")
    (#\+ . "Plus")
    (#\* . "Star")
    (#\/ . "Slash")))

;;; wie herrlich effizient
(defun list-to-string (list)
  (reduce #'(lambda (str1 &optional (str2 ""))
              (concatenate 'string str1 str2))
          list))

(defun list-join (list elt)
  (let (res)
    (dolist (i list)
      (push i res)
      (push elt res))
    (pop res)
    (nreverse res)))

(defun string-join (strings elt)
  (list-to-string (list-join strings elt)))

(defun string-split (string separators)
  (do ((len (length string))
       (i 0 (1+ i))
       (last 0)
       res)
      ((= i len)
       (nreverse (if (> i last)
		     (cons (subseq string last i) res)
		     res)))
    (when (member (char string i) separators)
      (push (subseq string last i) res)
      (setf last (1+ i)))))

(defun string-chars (string)
  (coerce string 'list))

(defun constant-string-p (string)
  (let ((len (length string))
        (constant-chars '(#\+ #\*)))
    (and (> len 2)
         (member (char string 0) constant-chars)
         (member (char string (1- len)) constant-chars))))

(defun first-uppercase-p (string)
  (and (> (length string) 1)
       (member (char string 0) '(#\+ #\*))))

(defun untouchable-string-p (string)
  (and (> (length string) 1)
       (char= #\: (char string 0))))

(defun symbol-to-js (symbol)
  (when (symbolp symbol)
    (setf symbol (symbol-name symbol)))
  (let ((symbols (string-split symbol '(#\.))))
    (cond ((null symbols) "")
	  ((= (length symbols) 1)
	   (let (res
                 (do-not-touch nil)
		 (lowercase t)
		 (all-uppercase nil))
	     (cond ((constant-string-p symbol)
		    (setf all-uppercase t
			  symbol (subseq symbol 1 (1- (length symbol)))))
		   ((first-uppercase-p symbol)
		    (setf lowercase nil
			  symbol (subseq symbol 1)))
                   ((untouchable-string-p symbol)
                    (setf do-not-touch t
                          symbol (subseq symbol 1))))
	     (flet ((reschar (c)
		      (push (cond
                              (do-not-touch c)
                              ((and lowercase (not all-uppercase))
                               (char-downcase c))
                              (t (char-upcase c)))
                            res)
		      (setf lowercase t)))
	       (dotimes (i (length symbol))
		 (let ((c (char symbol i)))
		   (cond
		     ;; :http--equiv => "http-equiv"
		     ((and (not lowercase) (eql c #\-))		      
		      (reschar #\-))
		     ((eql c #\-)
		      (setf lowercase (not lowercase)))		     
		     ((assoc c *special-chars*)
		      (dolist (i (coerce (cdr (assoc c *special-chars*)) 'list))
			(reschar i)))
		     (t (reschar c))))))
	     (coerce (nreverse res) 'string)))
	  (t (string-join (mapcar #'symbol-to-js symbols) ".")))))
