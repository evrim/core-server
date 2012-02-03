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
;;; RFC 2109 - HTTP State Management Mechanism (aka Cookies)
;;; http://www.ietf.org/rfc/rfc2109.txt
;;;-----------------------------------------------------------------------------
(defclass cookie ()
  ((name :accessor cookie.name :initarg :name :initform nil)
   (value :accessor cookie.value :initarg :value :initform nil)
   (version :accessor cookie.version :initarg :version :initform nil
	    :type integer)
   (comment :accessor cookie.comment :initarg :comment :initform nil)
   (domain :accessor cookie.domain :initarg :domain :initform nil)
   (max-age :accessor cookie.max-age :initarg :max-age :initform nil
	    :type integer)
   (expires :accessor cookie.expires :initarg :expires :initform nil
	    :type integer)
   (path :accessor cookie.path :initarg :path :initform nil)
   (secure :accessor cookie.secure :initarg :secure :initform nil
	   :type boolean)))

(defprint-object (self cookie)
  (format t "~A:~A" (slot-value self 'name) (slot-value self 'value)))

(defgeneric cookiep (cookie)
  (:method ((cookie cookie)) t)
  (:method ((cookie t)) nil))

(defun make-cookie (name value &key (version 1) (comment nil) (domain nil)
		                    (max-age nil) (path nil) (secure nil)
		    (expires nil))
  (make-instance 'cookie
		 :name name
		 :value value
		 :version version
		 :comment comment
		 :domain domain
		 :max-age max-age
		 :path path
		 :expires expires
		 :secure secure))

;; Set-Cookie: Part_Number="Riding_Rocket_0023"; Version="1"; Path="/acme/ammo"
(defmethod cookie! ((stream core-stream) (cookie cookie))
  (with-slots (name value version comment domain max-age path secure) cookie    
    (when name
      (string! stream (cookie.name cookie))  
      (char! stream #\=)
      (quoted! stream value)
      (when version
	(string! stream "; Version=")
	(fixnum! stream version))
      (when comment
	(string! stream "; Comment=")
	(quoted! stream comment))
      (when domain
	(string! stream "; Domain=")
	(string! stream domain))
      (when max-age
	(string! stream "; Max-age=")
	(fixnum! stream max-age))
      (when path
	(string! stream "; Path=")
	(string! stream path))
      (when secure
	(string! stream "; Secure")))))

(defatom rfc2109-cookie-header? ()
  (and (visible-char? c) (not (eq #.(char-code #\=) c))))

(defatom rfc2109-cookie-value? ()
  (and (visible-char? c) (not (eq #.(char-code #\;) c))
       (not (eq #.(char-code #\,) c))))

(defrule rfc2109-quoted-value? ((value (make-accumulator)) c)
  (:or (:and #\"
	     (:zom (:or (:checkpoint
			 (:and #\\ #\"
			       (:do (push-atom #\" value))
			       (:commit)))
			(:and #\" (:return value))
			(:and (:type (or visible-char? space?) c)
			      (:collect c value)))))
       (:and (:zom (:type rfc2109-cookie-value? c)
		   (:collect c value))
	     (:return value))))

;; Cookie: $Version="1"; Customer="WILE_E_COYOTE"; $Path="/acme"; $Domain=".core.gen.tr"
(defrule cookie? ((key (make-accumulator))
		  value version path domain c max-age 
		  comment expires secure)
  (:optional
   (:sci "$Version=")
   (:rfc2109-quoted-value? version)
   (:or #\; #\,)
   (:do (setq version (parse-integer version))))
  (:oom (:type rfc2109-cookie-header? c) (:collect c key))
  #\=
  (:rfc2109-quoted-value? value)
  (:zom #\;
	;; (:not (or #\Return #\Newline))
	(:lwsp?)
	(:or (:and (:sci "path=") (:rfc2109-quoted-value? path))
	     (:and (:sci "domain=") (:rfc2109-quoted-value? domain))
	     (:and (:sci "expires=") (:http-date? expires))
	     (:and (:sci "max-age=") (:rfc2109-quoted-value? max-age))
	     (:and (:sci "version=") (:rfc2109-quoted-value? version))
	     (:and (:sci "comment=") (:rfc2109-quoted-value? comment))
	     (:and (:sci "secure=") (:rfc2109-quoted-value? secure)))
	#\; (:lwsp?))
  (:return (make-cookie key value :version version 
			:domain domain 
			:path path
			:max-age max-age
			:comment comment
			:expires expires
			:secure secure)))

(defrule cookies? (cookie acc)
  (:oom (:cookie? cookie) (:do (push cookie acc)))
  (:return (nreverse acc)))


#|
(defparameter *c (make-cookie "neym" "valyu"
				      :version 2 :comment "coment"
				      :domain "domeyn" :max-age 0
				      :path "/acme" :secure t))
(cookiep *c)
(describe *c)
(defparameter *s (make-core-stream ""))
(time (cookie! *s *c))
(octets-to-string (slot-value *s '%octets) :utf-8)
=> "neym=\"valyu\"; Version=\"2\"; Comment=\"coment\"; Domain=\"domeyn\"; Max-age=\"0\"; Path=\"/acme\"; Secure"
SERVER> (cookie? (make-core-stream "$Version=\"1\";
                 Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"; $Domain=\"gee\";
                 Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
                 Shipping=\"FedEx\"; $Path=/acme;"))
(#<COOKIE {BA88419}> #<COOKIE {BA89B09}> #<COOKIE {BA8A669}>)
SERVER> (mapcar #'describe *)
#<COOKIE {BA88419}> is an instance of class #<STANDARD-CLASS COOKIE>.
The following slots have :INSTANCE allocation:
 NAME       "Customer"
 VALUE      "WILE_E_COYOTE"
 VERSION    1
 COMMENT    NIL
 DOMAIN     "gee"
 MAX-AGE    NIL
 PATH       "/acme"
 SECURE     NIL
#<COOKIE {BA89B09}> is an instance of class #<STANDARD-CLASS COOKIE>.
The following slots have :INSTANCE allocation:
 NAME       "Part_Number"
 VALUE      "Rocket_Launcher_0001"
 VERSION    NIL
 COMMENT    NIL
 DOMAIN     NIL
 MAX-AGE    NIL
 PATH       "/acme"
 SECURE     NIL
#<COOKIE {BA8A669}> is an instance of class #<STANDARD-CLASS COOKIE>.
The following slots have :INSTANCE allocation:
 NAME       "Shipping"
 VALUE      "FedEx"
 VERSION    NIL
 COMMENT    NIL
 DOMAIN     NIL
 MAX-AGE    NIL
 PATH       "/acme"
 SECURE     NIL
(NIL NIL NIL)
|#

