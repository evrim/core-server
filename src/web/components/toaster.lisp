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

(defcomponent toaster-component ()
  ((toaster :host remote :initform nil)))

(defmethod/remote toast ((self toaster-component) message)
  (if (= null this.toaster)
      (progn
	(dojo.require "dojox.widget.Toaster")
	(let ((div (document.create-element "div")))
	  (document.body.append-child div)
	  (setf this.toaster (new (dojox.widget.*toaster
				   (create :position-direction "br-left"
					   :message-topic "toasterTopic"
					   :duration 1000)
				   div))))))

  (dojo.publish "toasterTopic" (array message)))
