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
;;| Server Protocols
;;+----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Generic Server Protocol
;;-----------------------------------------------------------------------------
(defgeneric start (server)
  (:documentation "Starts the server. This method is surrounded by server.mutex")
  (:method-combination sysv-standard :type :start)
  (:method ((self null)) t)
  (:method ((self server)) t))

(defgeneric stop (server)
  (:documentation "Stops the server. This method is surrounded by server.mutex")
  (:method-combination sysv-standard :type :stop)
  (:method ((self null)) t)
  (:method ((self server)) t))

(defgeneric status (server)
  (:documentation "Returns t if server is running, nil otherwise.")
  (:method-combination sysv-standard :type :status)
  (:method ((self null)) nil)
  (:method ((self server)) t))

(defgeneric register (server app)
  (:documentation "Deploys an application to server. This method
  is surrounded by server.mutex")
  (:method-combination sysv-standard :type :start)
  (:method ((self null) app) nil)
  (:method ((self web-server) app) nil))

(defgeneric unregister (server app)
  (:documentation "Undeploys an application from a server. This
  method is surrounded by server.mutex")
  (:method-combination sysv-standard :type :stop)
  (:method ((self null) app) nil)
  (:method ((self web-server) app) nil))

;;-----------------------------------------------------------------------------
;; Name-server Protocol
;;-----------------------------------------------------------------------------
(defgeneric tinydns-server.domains (server)
  (:documentation "Returns raw domain data"))

(defgeneric find-record (server fqdn)
  (:documentation "Finds any record relating to fqdn"))

(defgeneric find-a (server fqdn)
  (:documentation "Finds A type records for fqdn"))

(defgeneric find-ns (server fqdn)
  (:documentation "Finds NS type records for fqdn"))

(defgeneric find-mx (server fqdn)
  (:documentation "Find mX type records for fqdn"))

(defgeneric add-mx (server fqdn &optional ip)
  (:documentation "Adds new mX type record to the database"))

(defgeneric add-ns (server fqdn ip)
  (:documentation "Adds new NS type record to the database"))

(defgeneric add-host (server fqdn ip)
  (:documentation "Adds new A type record to the database"))

(defgeneric add-alias (server fqdn ip)
  (:documentation "Adds new ALIAS type record to the database"))

(defgeneric find-a (server fqdn)
  (:documentation "Returns A records of 'fqdn'"))

(defgeneric find-alias (server fqdn)
  (:documentation "Returns ALIAS records of 'fqdn'"))

(defgeneric find-ns (server fqdn)
  (:documentation "Returns NS records of 'fqdn'"))

(defgeneric find-mx (server fqdn)
  (:documentation "Returns MX records of 'fqdn'"))

;;-----------------------------------------------------------------------------
;; Logger Server Protocol
;;-----------------------------------------------------------------------------
(defgeneric log-me (server tag message)
  (:documentation "Log messages as '<time> <tag> <message>'"))

(defgeneric log-me-raw (server message)
  (:documentation "Log messages as '<message>'"))

;;-----------------------------------------------------------------------------
;; Ticket Server Protocol
;;-----------------------------------------------------------------------------
(defgeneric add-ticket (server hash type &optional used)
  (:documentation "Add ticket to the server"))

(defgeneric generate-tickets (server amount type)
  (:documentation "Generate given amount of tickets with random hash"))

;;-----------------------------------------------------------------------------
;; Socket Server Protocol
;;-----------------------------------------------------------------------------
(defgeneric handle-stream (unit core-stream address)
  (:documentation "Handle the incoming remote socket request"))

;;+----------------------------------------------------------------------------
;;| Application Protocol
;;+----------------------------------------------------------------------------
;;
;; Definition of generic application protocol
;;
(defgeneric serialize-source (app symbol)
  (:documentation "Serialize a new source for application")
  (:method ((self null) symbol) t))

(defgeneric serialize-asd (app)
  (:documentation "Serialize system definition"))

(defgeneric source-to-pathname (app symbol)
  (:documentation "todo"))

(defgeneric serialize (app)
  (:documentation "Serialize application to project path"))

(defgeneric package-keyword (app &optional long)
  (:documentation "Package name for our new application"))

(defgeneric src/packages (app)
  (:documentation "Returns src/packages.lisp sexp"))

(defgeneric src/model (app)
  (:documentation "Returns src/model.lisp sexp"))

(defgeneric src/tx (app)
  (:documentation "Returns src/tx.lisp sexp"))

(defgeneric src/interfaces (app)
  (:documentation "Returns src/interfaces.lisp sexp"))

(defgeneric src/application (app)
  (:documentation "Returns src/application.lisp sexp"))

(defgeneric src/security (app)
  (:documentation "Returns src/security.lisp sexp"))

(defgeneric src/ui/main (app)
  (:documentation "Returns src/ui/main.lisp sexp"))