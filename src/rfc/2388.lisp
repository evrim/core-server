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

(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; RFC 2388 - Returning Values from Forms:  multipart/form-data
;;; http://www.ietf.org/rfc/rfc2388.txt
;;;-----------------------------------------------------------------------------

;; 3. Definition of multipart/form-data
;; content-disposition: form-data; name=\"field1\"
;; Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
(defatom rfc2388-parameter-value? ()
  (and (not (eq #.(char-code #\;) c))
       (or (visible-char? c)
	   (space? c))))

(defrule rfc2388-content-disposition? ((parameters '()) (type (make-accumulator)) c
					    (key (make-accumulator)) value)
  (:lwsp?)
  (:type http-header-name? c) (:collect c type)
  (:zom (:not #\;) (:type (or visible-char? space?) c) (:collect c type))
  (:lwsp?)
  (:zom (:type http-header-name? c) (:collect c key)
	(:zom (:not #\=) (:type http-header-name? c) (:collect c key))
	(:or (:and (:quoted? value))
	     (:and (:do (setq value (make-accumulator :byte)))
		   (:type http-header-value? c) (:collect c value)
		   (:zom (:type rfc2388-parameter-value? c) (:collect c value))))
	(:do (push (cons key value) parameters)
	     (setq key (make-accumulator)))
	#\;
	(:lwsp?))
  (:return (list type (nreverse parameters))))

(defun rfc2388-mimes? (stream &optional (boundary nil))
  (let ((mimes (mimes? stream boundary)))
    (when mimes
      (mapc #'(lambda (mime)
		(mime-search mime
			     #'(lambda (mime)
				 (setf (mime.header mime 'disposition)
				       (rfc2388-content-disposition?
					(make-core-stream (mime.header mime 'disposition))))
				 nil)))
	    mimes)
      mimes)))
