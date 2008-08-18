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

(defcommand thumbnail (shell)
  ((option :initform "-thumbnail" :host local)
   (width :initform "150" :host local)
   (height :initform "150" :host local)
   (source :initform (error "Please supply source pathname") :host local)
   (target :initform (error "Please supply target pathname") :host local))
  (:default-initargs :cmd (whereis "convert")))

(defmethod run ((self thumbnail))
  (setf (args self)
	(list (thumbnail.option self)
	      (format nil "~Dx~D>" (thumbnail.width self) (thumbnail.height self))
	      (thumbnail.source self)
	      (thumbnail.target self)))
  (call-next-method))