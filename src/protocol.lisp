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
;;| Server Protocol
;;+----------------------------------------------------------------------------
;;
;; Definition of generic server protocol.
;;
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
  (:documentation "Deploys an application to web server. This method
  is surrounded by server.mutex")
  (:method-combination sysv-standard :type :start)
  (:method ((self null) app) nil)
  (:method ((self web-server) app) nil))

(defgeneric unregister (server app)
  (:documentation "Undeploys an application from a web server. This
  method is surrounded by server.mutex")
  (:method-combination sysv-standard :type :stop)
  (:method ((self null) app) nil)
  (:method ((self web-server) app) nil))

;;; Name-server Protocol
(defgeneric add-mx (server fqdn &optional ip)
  (:documentation "Adds an mx record"))

(defgeneric add-ns (server fqdn ip)
  (:documentation "Adds a nameserver"))

(defgeneric add-host (server fqdn ip)
  (:documentation "Adds a host"))

(defgeneric add-alias (server source target)
  (:documentation "Adds an alias from source fqdn to target "))

(defgeneric find-domain-records (server domain-name)
  (:documentation "Return the list of dns records of the associated
  domain identified by name."))

;;; Ticket Server Protocol

(defgeneric add-ticket (server hash type &optional used)
  (:documentation "Add ticket to the server"))

(defgeneric generate-tickets (server amount type)
  (:documentation "Generate given amount of tickets with random hash"))

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