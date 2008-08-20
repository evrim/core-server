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

(defvar +verbose+ t "make command executions verbose during installation.")
(defvar +which+ #P"/usr/bin/which")

;; whereis utility cannot find those utilities
(defvar +useradd+ #P"/usr/sbin/useradd")
(defvar +groupadd+ #P"/usr/sbin/groupadd")

(defparameter +tmp+ (make-pathname :directory '(:absolute "tmp")) "Temporary directory")

(defun rmdir (pathname) (sb-posix::rmdir pathname))

;; Generate unique filename for temporary usage
(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defun filter-arguments (args)
  (flet ((filter-arg (arg)
	   (cond 
	    ((or (keywordp arg) (symbolp arg)) (string-downcase (symbol-name arg)))
	    ((pathnamep arg) (namestring arg))
	    (t arg))))
    (mapcar #'filter-arg args)))

(defclass+ command ()
  ((input-stream :accessor command.input-stream :initarg :input-stream :initform nil)
   (output-stream :accessor command.output-stream :initarg :output-stream :initform nil)
   (verbose :accessor command.verbose :initarg :verbose :initform +verbose+)
   (verbose-stream :accessor command.verbose-stream :initarg :verbose :initform *standard-output*)
   (local-args :accessor command.local-args :initarg :local-args :initform '())
   (remote-args :accessor command.remote-args :initarg :remote-args :initform '())))

(defgeneric render (command)
  (:documentation "Send the command to remote."))

(defgeneric parser (command)
  (:documentation "Parse the answer and call writers."))

(defgeneric run (command)
  (:documentation "Run this command with given args."))

(defgeneric render-arguments (shell)
  (:documentation "Render arguments that will be given to the shell command."))

(defgeneric check-exit-code (shell)
  (:documentation "Checks the return value of the command executed. If
  it returns a non-zero value, prompts for retrying the command."))

(defgeneric wait-process (shell)
  (:documentation "Waits for the command to terminate, sets the
  exit-code and invokes check-exit-code."))

(defmacro defcommand (name supers slots &rest rest)
  (let ((supers (nreverse (cons 'command (nreverse supers)))))
    (multiple-value-bind (class+ new-slots new-rest) (class+.register (class+ name supers slots rest))
      `(progn
	 (eval-when (:load-toplevel)
	   (class+.register (class+ ',name ',supers ',slots ',rest)))
	 ,(aif (cdr (assoc :ctor rest))
	       `(progn
		  (defun ,name ,@it
		    (run (make-instance ',name ,@(class+.ctor-arguments class+ (car it)))))
		  (defun ,(intern (format nil "~AM" name) (symbol-package name)) ,@it
		    (make-instance ',name ,@(class+.ctor-arguments class+ (car it)))))
	       `(progn
		  (defun ,name (&key ,@(class+.ctor-keywords class+))
		    (run (make-instance ',name ,@(class+.ctor-arguments class+))))
		  (defun ,(intern (format nil "~AM" name) (symbol-package name))
		      (&key ,@(class+.ctor-keywords class+))
		    (make-instance ',name ,@(class+.ctor-arguments class+)))))
	 (defclass ,name (,@supers) ,new-slots ,@new-rest)))))

(defcommand shell ()
  ((cmd :host local :initform nil)
   (args :host local :initform nil)
   (wait :host local :initform t)
   (exit-code :host remote :initform -1)
   (errorp :host local :initform t)
   (process :initform nil)))

(defmethod command.input-stream ((self shell))
  (sb-ext:process-input (shell.process self)))

(defmethod command.output-stream ((self shell))
  (sb-ext:process-output (shell.process self)))

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
    (setf (shell.exit-code self) (sb-ext::process-exit-code (shell.process self)))
    (check-exit-code self)))

(defmethod render-arguments :around ((self shell))
  (filter-arguments (ensure-list (call-next-method))))

(defmethod render-arguments ((self shell))
  (s-v 'args))

(defmethod run ((self shell))
  (flet ((ensure-cmd (cmd)
	   (cond
	     ((pathnamep cmd) (namestring cmd))
	     (t (error "Please provide cmd slot")))))
    (if (s-v 'verbose)
	(format t "Executing command: ~A ~{~A~^ ~}~%" (s-v 'cmd) (render-arguments self)))
    (setf (shell.process self)
	  (sb-ext:run-program (ensure-cmd (shell.cmd self))
			      (render-arguments self)
			      :wait nil
			      :input (if (s-v 'verbose) t :stream)
			      :output (if (s-v 'verbose)
					  *standard-output*
					  :stream)
			      :error :output))
    (if (shell.wait self)
	(wait-process self) 
	(values))))

(defcommand which (shell)
  ((name :host local :initform nil))
  (:default-initargs :cmd +which+ :verbose nil))

(defmethod render-arguments ((self which))
  (list (s-v 'name)))

(defmethod run :around ((self which))
  (call-next-method)
  (when (zerop (shell.exit-code self))
    (let ((result (read-line (command.output-stream self))))
      (pathname result))))

(defun whereis (name)
  (which :name name))

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

(defcommand rm (shell)
  ((path :host local :initform nil))
  (:default-initargs :cmd (whereis "rm")))

(defmethod render-arguments ((self rm))
  (list (s-v 'path)))

(defvar +darcs+ (whereis "darcs"))
(defvar +wget+ (whereis "wget"))
(defvar +tar+ (whereis "tar"))
(defvar +mv+ (whereis "mv"))
(defvar +ln+ (whereis "ln"))
(defvar +find+ (whereis "find"))
(defvar +chown+ (whereis "chown"))
(defvar +chmod+ (whereis "chmod"))

(defcommand useradd (shell)
  ((username :host local :initform (error "Username must be provided."))
   (group :host local :initform nil)
   (extra-groups :host local :initform '())
   (home-directory :host local :initform nil)
   (user-group :host local :initform nil)
   (create-home :host local :initform nil)
   (comment :host local :initform nil))
  (:default-initargs :cmd +useradd+))

(defmethod render-arguments ((self useradd))
  (cons (s-v 'username)
	(reduce
	 #'(lambda (acc item)
	     (if (s-v (car item))
		 (cons (cadr item) acc)
		 acc))	  
	 '((user-group "-n") (create-home "-m"))
	 :initial-value (reduce
			 #'(lambda (acc item)
			     (if (s-v (car item))
				 (cons (cadr item)
				       (if (listp (s-v (car item)))
					   (cons (reduce
						  #'(lambda (acc i)
						      (format nil "~A,~A" acc i))
						  (s-v (car item))) acc)
					   (cons (s-v (car item)) acc)))
				 acc))
			 '((group "-g") (extra-groups "-G") (home-directory "-d")
			   (comment "-c")) :initial-value nil))))

(defcommand groupadd (shell)
  ((groupname :host local :initform (error "Group name must be provided.")))
  (:default-initargs :cmd +groupadd+ :errorp nil))

(defmethod render-arguments ((self groupadd))
  (list (s-v 'groupname)))

(defcommand find-file (shell)
  ((pattern :host local :initform (error "must specify a filename pattern.")))
  (:default-initargs :cmd +find+ :verbose nil))

(defmethod render-arguments ((self find-file))
  (list "-name" (s-v 'pattern) "-type" "f"))

(defmethod run ((self find-file))
  (call-next-method)
  (loop for line = (read-line (command.output-stream self) nil nil)
     while line
     collect line))

(defcommand ln (shell)
  ((source :host local :initform (error "please specify source"))
   (target :host local :initform (error "please specify target")))
  (:default-initargs :cmd +ln+))

(defmethod render-arguments ((self ln))
  (list "-sf" (s-v 'source) (s-v 'target)))

(defcommand chown (shell)
  ((user :host local :initform nil)
   (group :host local :initform nil)
   (recursive :host local :initform nil)
   (path :host local :initform (error "Pathname must be specified.")))  
  (:default-initargs :cmd +chown+))

(defmethod render-arguments ((self chown))
  (let ((ug (concatenate 'string (s-v 'user) ":" (s-v 'group))))
    (if (s-v 'recursive)
	(list ug "-R" (s-v 'path))
	(list ug (s-v 'path)))))

(defmethod run ((self chown))
  (when (and (null (s-v 'user)) (null (s-v 'group)))
    (error "Username and group can't be empty at the same time."))
  (call-next-method))

(defcommand chmod (shell)
  ((mode :host local :initform (error "Please specify mode."))
   (path :host local :initform (error "Please specify path."))
   (recursive :host local :initform nil))
  (:default-initargs :cmd +chmod+))

(defmethod render-arguments ((self chmod))
  (if (s-v 'recursive)
      (list (s-v 'mode) "-R" (s-v 'path))
      (list (s-v 'mode) (s-v 'path))))

(defcommand wget (shell)
  ((source :host local :initform (error "please specify source"))
   (target :host local :initform (error "please specify target")))
  (:default-initargs :cmd +wget+ :wait t))

(defmethod render-arguments ((self wget))
  (list "--output-document" (s-v 'target) (s-v 'source)))

(defcommand cvs (shell)
  ((op :host local :initform "co")
   (repo-type :host local :initform "pserver")
   (username :host local :initform "anonymous")
   (password :host local :initform "anonymous")
   (repo :host local :initform (error "repo can't be nil"))
   (module :host local)
   (target :host local :initform (error "target can't be nil")))
  (:default-initargs :cmd (whereis "cvs")))

(defmethod render-arguments ((self cvs))
  (list (format nil "-d:~A:~A:~A@~A" (cvs.repo-type self) (cvs.username self)
		(cvs.password self) (cvs.repo self))
	(cvs.op self) "-d" (cvs.target self) (cvs.module self)))

(defcommand darcs (shell)
  ((op :host local :initform "get")
   (repo :host local :initform nil)
   (target :host local :initform nil)
   (partial :host local :initform t))
  (:default-initargs :cmd +darcs+))

(defmethod render-arguments ((self darcs))
  (cons (darcs.op self)
	(cond
	  ((equal (darcs.op self) "get")
	   (if (or (null (darcs.repo self)) (null (darcs.target self)))
	       (error "Please specify repo/target."))
	   (if (s-v 'partial)
	       (list "--partial" (darcs.repo self) (darcs.target self))
	       (list (darcs.repo self) (darcs.target self))))
	  ((equal (darcs.op self) "unpull")
	   nil))))

(defcommand svn (shell)
  ((op :host local :initform "co")
   (repo :host local :initform (error "repo can't be nil"))
   (target :host local :initform (error "target can't be nil")))
  (:default-initargs :cmd (whereis "svn")))

(defmethod render-arguments ((self svn))
  (list (svn.op self) (svn.repo self) (svn.target self)))

(defcommand tarball (shell)
  ((repo :host local :initform (error "repo can't be nil"))
   (temp-fname :host none :initform (make-pathname :name (tmpnam nil)))
   (target :host local :initform (error "target can't be nil"))
   (sandbox :host none :initform nil))
  (:default-initargs :cmd +tar+ :wait t))

(defmethod render-arguments ((self tarball))
  (cond
    ((search ".gz" (s-v 'repo)) (list "zxvf" (s-v 'temp-fname)))
    ((search ".bz2" (s-v 'repo)) (list "jxvf" (s-v 'temp-fname)))))

(defmethod run ((self tarball))
  (wget :source (s-v 'repo) :target (s-v 'temp-fname))
  (let ((sandbox-path (make-pathname :directory (tmpnam nil))))
    (ensure-directories-exist sandbox-path)
    (setf (tarball.sandbox self) sandbox-path)
    (with-current-directory sandbox-path
      (call-next-method))))

(defmethod run :after ((self tarball))
  (let ((package-directory (car (directory
				 (make-pathname :name :wild
						:type :wild
						:defaults (s-v 'sandbox))))))
    (if package-directory
	;; STUPID Debian and gnuutils this one should be simply:
	;; (shell :cmd +mv+ :args (list package-directory (target self)))	    
	(shell :cmd +mv+ :args (list package-directory
				     (subseq (format nil "~A" (tarball.target self))
					     0
					     (1- (length (namestring (tarball.target self)))))))
	(error "package tarball is bogus."))
    (when (tarball.temp-fname self)
      (delete-file (namestring (tarball.temp-fname self))))))
