(in-package :tr.gen.core.server)

;; Server Protocol
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
  (:documentation "Deploys an application to web server. This method is surrounded by server.mutex")
  (:method-combination sysv-standard :type :start)
  (:method ((self null) app) nil)
  (:method ((self web-server) app) nil))

(defgeneric unregister (server app)
  (:documentation "Undeploys an application from a web server. This method is surrounded by server.mutex")
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
  (:documentation "Return the list of dns records of the associated domain identified by name."))

;;; Ticket Server Protocol

(defgeneric add-ticket (server hash type &optional used)
  (:documentation "Add ticket to the server"))

(defgeneric generate-tickets (server amount type)
  (:documentation "Generate given amount of tickets with random hash"))