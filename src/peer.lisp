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

;;+----------------------------------------------------------------------------
;;| Peer Base Classes
;;+----------------------------------------------------------------------------
;;
;; This file contains Peer Base Class.
;;
(defclass peer (unit)
  ((server :accessor peer.server :initarg :server :initform nil
	   :documentation "The server that this peer belongs to"))
  (:documentation "Peer Base Class - Peers are worker units of
 servers. The server when started creates N peers to handle incoming
 requests. These requests can come from very different sources like
 sockets, unix pipes etc."))

(defclass stream-peer (peer local-unit)
  ()
  (:default-initargs :name "Stream Peer Handling Unit")
  (:documentation "Stream Peer Class - This is the base class for
  core-stream handling peer"))


;; FIXme: Do we have a protocol for stream-peer having below method? -evrim

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