(in-package :core-server)

;; +----------------------------------------------------------------------------
;; | Shell Commands
;; +----------------------------------------------------------------------------
(defcommand shell ()
  ((cmd :host local :initform nil)
   (args :host local :initform nil)
   (wait :host local :initform t)
   (exit-code :host remote :initform -1)
   (errorp :host local :initform t)
   (process :initform nil)))

;; ----------------------------------------------------------------------------
;; Protocol
;; ----------------------------------------------------------------------------
(defgeneric run (shell)
  (:documentation "Run this command with given args."))

(defgeneric render-arguments (shell)
  (:documentation "Render arguments that will be given to the shell command."))

(defgeneric check-exit-code (shell)
  (:documentation "Checks the return value of the command executed. If
  it returns a non-zero value, prompts for retrying the command."))

(defgeneric wait-process (shell)
  (:documentation "Waits for the command to terminate, sets the
  exit-code and invokes check-exit-code."))

;; ----------------------------------------------------------------------------
;; Implementation
;; ----------------------------------------------------------------------------
(defmethod command.input-stream ((self shell))
  (sb-ext:process-input (shell.process self)))

(defmethod command.output-stream ((self shell))
  (sb-ext:process-output (shell.process self)))

(defmethod render-arguments ((self shell))
  (s-v 'args))

(defmethod render-arguments :around ((self shell))  
  (flet ((filter-arg (arg)
	   (cond 
	     ((or (keywordp arg) (symbolp arg))
	      (string-downcase (symbol-name arg)))
	     ((pathnamep arg)
	      (namestring arg))
	     (t arg))))
    (mapcar #'filter-arg (ensure-list (call-next-method)))))

(defmethod check-exit-code ((self shell))
  (if (s-v 'errorp)
      (restart-case (if (not (eq 0 (shell.exit-code self)))
			(error "Execution of \"~A ~{~A~^ ~}\" failed with error code: ~D"
			       (s-v 'cmd) (render-arguments self) (shell.exit-code self))
			(shell.exit-code self)) 
	(try-again ()
	  :report "Re-run this command."
	  (run self)))
      (shell.exit-code self)))

(defmethod wait-process ((self shell))
  (progn
    (sb-ext:process-wait (shell.process self))
    (setf (shell.exit-code self)
	  (sb-ext::process-exit-code (shell.process self)))
    (check-exit-code self)))

(defmethod run ((self shell))
  (flet ((ensure-cmd (cmd)
	   (cond
	     ((pathnamep cmd) (namestring cmd))
	     (t (error "Please provide cmd slot")))))
    (if (s-v 'verbose)
	(format t "Executing command: ~A ~{~A~^ ~}~%"
		(s-v 'cmd) (render-arguments self)))
    (setf (shell.process self)
	  (sb-ext:run-program (ensure-cmd (shell.cmd self))
			      (mapcar (lambda (a) (format nil "~A" a))
				      (render-arguments self))
			      :wait nil
			      :input (if (s-v 'verbose) t :stream)
			      :output (if (s-v 'verbose)
					  *standard-output*
					  :stream)
			      :error :output))
    (if (shell.wait self)
	(wait-process self) 
	(values))))

;; ----------------------------------------------------------------------------
;; Which & Whereis
;; ----------------------------------------------------------------------------
(defcommand which (shell)
  ((name :host local :initform nil))
  (:default-initargs :cmd #P"/usr/bin/which" :verbose nil))

(defmethod render-arguments ((self which))
  (list (s-v 'name)))

(defmethod run :around ((self which))
  (call-next-method)
  (when (zerop (shell.exit-code self))
    (let ((result (read-line (command.output-stream self))))
      (pathname result))))

(defmethod run-sudo :around ((self which))
  (call-next-method)
  (when (zerop (shell.exit-code self))
    (let ((result (read-line (command.output-stream self))))
      (pathname result))))

(defun whereis (name)
  (which :name name))

;; ----------------------------------------------------------------------------
;; Copy
;; ----------------------------------------------------------------------------
(defcommand cp (shell)
  ((from :host local :initform nil)
   (to :host local :initform nil)
   (recursive :host local :initform nil))
  (:default-initargs :cmd (whereis "cp")))

(defmethod render-arguments ((self cp))
  (let ((args (list (s-v 'from) (s-v 'to))))
    (if (s-v 'recursive)
	(cons "-r" args)
	args)))

;; ----------------------------------------------------------------------------
;; Remove
;; ----------------------------------------------------------------------------
(defcommand rm (shell)
  ((path :host local :initform nil))
  (:default-initargs :cmd (whereis "rm")))

(defmethod render-arguments ((self rm))
  (append (ensure-list (shell.args self)) (list (s-v 'path))))

;; ----------------------------------------------------------------------------
;; Find
;; ----------------------------------------------------------------------------
(defcommand find-file (shell)
  ((pattern :host local :initform (error "must specify a filename pattern.")))
  (:default-initargs :cmd (whereis "find") :verbose nil))

(defmethod render-arguments ((self find-file))
  (list "-name" (s-v 'pattern) "-type" "f"))

(defmethod run ((self find-file))
  (call-next-method)
  (loop for line = (read-line (command.output-stream self) nil nil)
     while line
     collect line))

;; ----------------------------------------------------------------------------
;; Link
;; ----------------------------------------------------------------------------
(defcommand ln (shell)
  ((source :host local :initform (error "please specify source"))
   (target :host local :initform (error "please specify target")))
  (:default-initargs :cmd (whereis "ln")))

(defmethod render-arguments ((self ln))
  (list "-sf" (s-v 'source) (s-v 'target)))

;; ----------------------------------------------------------------------------
;; Chown
;; ----------------------------------------------------------------------------
(defcommand chown (shell)
  ((user :host local :initform nil)
   (group :host local :initform nil)
   (recursive :host local :initform nil)
   (path :host local :initform (error "Pathname must be specified.")))  
  (:default-initargs :cmd (whereis "chown")))

(defmethod render-arguments ((self chown))
  (let ((ug (concatenate 'string (s-v 'user) ":" (s-v 'group))))
    (if (s-v 'recursive)
	(list ug "-R" (s-v 'path))
	(list ug (s-v 'path)))))

(defmethod run ((self chown))
  (when (and (null (s-v 'user)) (null (s-v 'group)))
    (error "Username and group can't be empty at the same time."))
  (call-next-method))

;; ----------------------------------------------------------------------------
;; Chmod
;; ----------------------------------------------------------------------------
(defcommand chmod (shell)
  ((mode :host local :initform (error "Please specify mode."))
   (path :host local :initform (error "Please specify path."))
   (recursive :host local :initform nil))
  (:default-initargs :cmd (whereis "chmod")))

(defmethod render-arguments ((self chmod))
  (if (s-v 'recursive)
      (list (s-v 'mode) "-R" (s-v 'path))
      (list (s-v 'mode) (s-v 'path))))

;; -------------------------------------------------------------------------
;; Mkdir
;; -------------------------------------------------------------------------
(defcommand mkdir (shell)
  ((path :host local :initform (error "Please specify :path")))
  (:default-initargs :cmd (whereis "mkdir")))

(defmethod render-arguments ((self mkdir))
  (append (s-v 'args) (list (s-v 'path))))

;; -------------------------------------------------------------------------
;; Mv
;; -------------------------------------------------------------------------
(defcommand mv (shell)
  ((source :host local :initform (error "Please specify :source"))
   (target :host local :initform (error "Please specify :target")))
  (:default-initargs :cmd (whereis "mv")))

(defmethod render-arguments ((self mv))
  (append (s-v 'args) (list (s-v 'source) (s-v 'target))))

;; ----------------------------------------------------------------------------
;; Wget
;; ----------------------------------------------------------------------------
(defcommand wget (shell)
  ((source :host local :initform (error "please specify source"))
   (target :host local :initform (error "please specify target")))
  (:default-initargs :cmd (whereis "wget") :wait t))

(defmethod render-arguments ((self wget))
  (list "--output-document" (s-v 'target) (s-v 'source)))

;; ----------------------------------------------------------------------------
;; Extra
;; ----------------------------------------------------------------------------
;; Generate unique filename for temporary usage
(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

;; ----------------------------------------------------------------------------
;; Remove Directory
;; ----------------------------------------------------------------------------
(defun rmdir (pathname)
  (sb-posix::rmdir pathname))

;; +-------------------------------------------------------------------------
;; | Mighty Sudo
;; +-------------------------------------------------------------------------
;; Use m version of commands along with sudo.
;;
;; SERVER> (sudo (postfix-sysv-scriptm :args '("start")))
;; 0
;; SERVER> (sudo (postfix-sysv-scriptm :args '("start") :verbose t))
;; Executing command: sudo /etc/init.d/postfix start
;; Starting Postfix Mail Transport Agent: postfix.
;; 0
(defmethod run-sudo ((self shell))
  (flet ((ensure-cmd (cmd)
	   (cond
	     ((pathnamep cmd) (namestring cmd))
	     (t (error "Please provide cmd slot")))))
    (if (s-v 'verbose)
	(format t "Executing command: sudo ~A ~{~A~^ ~}~%" (s-v 'cmd) (render-arguments self)))
    (setf (shell.process self)
	  (sb-ext:run-program (ensure-cmd +sudo+)
			      (cons (namestring (shell.cmd self)) (render-arguments self))
			      :wait nil
			      :input (if (s-v 'verbose) t :stream)
			      :output (if (s-v 'verbose)
					  *standard-output*
					  :stream)
			      :error :output))
    (if (shell.wait self)
	(wait-process self) 
	(values))))

(defun sudo (command)
  (run-sudo command))

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
