(in-package :tr.gen.core.server)

;;;;
;;;; Usage:
;;;;
;;
;; for console output:
;; (defparameter *logger* (make-instance 'logger-server :log-stream *standard-output*))
;;
;; for file output (to $CORESERVER_HOME/var/log/core-server.log):
;; (defparameter *logger* (make-instance 'logger-server :log-stream nil))

(defparameter +default-log-path+
  (merge-pathnames (make-pathname :directory '(:relative "var" "log"))
		   (sb-posix:getenv "CORESERVER_HOME")))

;; this is a thread that logs messages to a stream
(defclass logger-server (local-unit)
  ((log-stream :accessor log-stream :initarg :log-stream :initform *core-output*)
   (log-path :accessor log-path :initarg :log-path
	     :initform +default-log-path+)))

;; log lines are like that:
;; <time> <tag> <message>
(defmethod/unit log-me :async-no-return ((self logger-server) tag message)
  (string! (log-stream self) (time->string (get-universal-time) :short))
  (char! (log-stream self) #\Space)
  (string! (log-stream self) (symbol-name tag))
  (char! (log-stream self) #\Space)
  (string! (log-stream self) message)
  (char! (log-stream self) #\Newline)
  (when (typep (log-stream self) 'core-fd-io-stream)
    (force-output (slot-value (log-stream self) '%stream))))

;; raw logging with newline at the end of the message
(defmethod/unit log-me-raw :async-no-return ((self logger-server) message)
  (string! (log-stream self) message)
  (char! (log-stream self) #\Newline)
  (when (typep (log-stream self) 'core-fd-io-stream)
    (force-output (slot-value (log-stream self) '%stream))))

;; Start logger unit
(defmethod start ((self logger-server))
  (unless (log-stream self)
    (setf (log-stream self)
	  (make-core-stream
	   (open (make-pathname :directory (pathname-directory (log-path self))
				:name "core-server"
				:type "log")
		 :direction :output
		 :element-type '(unsigned-byte 8)
		 :if-exists :append
		 :if-does-not-exist :create
		 :external-format :utf8)))))

;; Stop logger unit
(defmethod stop ((self logger-server))
  (when (and (log-stream self) 
	     (typep (log-stream self) 'core-fd-io-stream))
    (close-stream (log-stream self))
    (setf (log-stream self) nil)))

(defmethod status ((self logger-server))
  (if (log-stream self)
      t))
