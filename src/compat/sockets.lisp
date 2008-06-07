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
;;| BSD Sockets Compatibilty Functions
;;+----------------------------------------------------------------------------
;;
;; This file contains compat layer for BSD Sockets

(defun resolve-hostname (name)
  "Resolves the host 'name' by using gethostbyname(3)"
  (cond
   ((typep name '(vector * 4)) name)
   (t (car (host-ent-addresses (get-host-by-name name))))))

(defun make-server (&key (host (vector 0 0 0 0)) (port 0) (reuse-address t)
		    (backlog 1) (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (let ((socket (make-instance 'inet-socket :type :stream :protocol protocol)))
    (when reuse-address
      (setf (sockopt-reuse-address socket) t))
    (socket-bind socket (resolve-hostname host) port)
    (socket-listen socket backlog)
    (apply #'values socket
	    (multiple-value-list (socket-name socket)))))

(defun close-server (server)
  "Closes a server socket created by make-server"
  (socket-close server))

(defun accept (socket &key (element-type '(unsigned-byte 8)))
  "Returns a new client core-stream that is just connected to 'socket'"
  (multiple-value-bind (s peer) (socket-accept socket)
    (values (make-core-stream (socket-make-stream s
						  :input t :output t
						  :element-type element-type
						  :buffering :full))
            peer)))

(defun connect (server-host server-port
		&key (element-type '(unsigned-byte 8)) (protocol :tcp))
  "Connects to the specified 'server-host' 'server-port' and returns a new
core-stream"
  (let ((socket (make-instance 'inet-socket :type :stream :protocol protocol)))
    (socket-connect socket (resolve-hostname server-host) server-port)
    (make-core-stream (socket-make-stream socket :input t :output t
					  :element-type element-type						 
					  :buffering :full))))
