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

(defclass peer ()
  ((server :accessor peer.server :initarg :server :initform nil)))

(defclass stream-peer (peer local-unit)
  ()
  (:default-initargs :name "Stream Peer Handling Unit"))

;; (defmethod/unit handle-stream :async-no-return ((self stream-peer) stream address)
;;   (format t "1.Peer address:~A~%" address)
;;   (let ((acc (make-accumulator :byte)))
;;     (iterate (for str = (read-stream stream))
;; 	     (when str (push-atom str acc))
;; 	     (until (null str))
;; 	     (finally  
;; 	      (close-stream stream)
;; 	      (format t "GEE:~A~%" (octets-to-string acc :utf-8)))))
;;   (sb-int::flush-standard-output-streams))