;; +----------------------------------------------------------------------------
;; | Tag Definitions
;; +----------------------------------------------------------------------------
(in-package :core-server)

(defun <core:redirect (&key href (seconds 0))
  (<:html
   (<:head
    (<:meta :http--equiv "Refresh" :content (format nil "~D; ~A" seconds href)))
   (<:body)))

(defun <core:input (&key CLASS ID STYLE TITLE ONCLICK ONDBLCLICK DIR
		    LANG ACCEPT ACCESSKEY ALT CHECKED DISABLED
		    MAXLENGTH NAME ONBLUR ONCHANGE ONFOCUS ONSELECT
		    READONLY SIZE SRC TABINDEX (TYPE "text") USEMAP
		    VALUE WIDTH HEIGHT ONMOUSEOVER ONMOUSEOUT)
  (<:input :STYLE (concat "opacity: 0.5;" style)
	   :ONFOCUS (if (and value onfocus)
			onfocus
			(jambda (value)			  
			  (when (equal value this.value)
			    (setf this.value ""
				  this.style.opacity 1.0))))
	   :ONBLUR (if (and value ONBLUR)
		       onblur
		       (jambda (value)
			 (when (equal "" this.value)
			   (setf this.value value)
			   (setf this.style.opacity 0.5))))
	   :CLASS CLASS :ID ID :TITLE TITLE :ONCLICK
	   ONCLICK :ONDBLCLICK ONDBLCLICK :DIR DIR :LANG LANG :ACCEPT
	   ACCEPT :ACCESSKEY ACCESSKEY :ALT ALT :CHECKED
	   CHECKED :DISABLED DISABLED :MAXLENGTH MAXLENGTH :NAME
	   NAME :ONCHANGE ONCHANGE :ONSELECT ONSELECT :READONLY
	   READONLY :SIZE SIZE :SRC SRC :TABINDEX TABINDEX :TYPE
	   TYPE :USEMAP USEMAP :VALUE VALUE :WIDTH WIDTH :HEIGHT
	   HEIGHT :ONMOUSEOVER ONMOUSEOVER :ONMOUSEOUT ONMOUSEOUT))

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
