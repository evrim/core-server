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
(defparameter +tmp+ (make-pathname :directory '(:absolute "tmp"))
  "Temporary directory")

(defclass command ()
  ((input-stream :accessor command.input-stream :initarg :input-stream
		 :initform nil)
   (output-stream :accessor command.output-stream :initarg :output-stream
		  :initform nil)
   (verbose :accessor command.verbose :initarg :verbose :initform +verbose+)
   (verbose-stream :accessor command.verbose-stream :initarg :verbose
		   :initform *standard-output*)
   (local-args :accessor command.local-args :initarg :local-args :initform '())
   (remote-args :accessor command.remote-args :initarg :remote-args :initform '())))

(defgeneric render (command)
  (:documentation "Send the command to remote."))

(defgeneric parser (command)
  (:documentation "Parse the answer and call writers."))

(defgeneric run (command)
  (:documentation "Run this command with the instance of the command class."))

(defmacro defcommand (name supers slots &rest rest)
  (multiple-value-bind (new-slots new-rest) (register-class name supers slots rest)
    `(prog1 (defclass ,name (,@supers command)
	      ,new-slots
	      (:default-initargs ,@(alist-to-plist (default-initargs-of-class name)))
	      ,@(remove :default-initargs new-rest :key #'car))
       (eval-when (:load-toplevel :compile-toplevel :execute)
	 (register-class ',name ',supers ',slots ',rest))
       ,(aif (cdr (assoc :ctor rest))
	     `(defun ,name ,@it
		(run (make-instance ',name ,@(ctor-arguments name (car it)))))
	     `(defun ,name (&key ,@(ctor-keywords name))
		(run (make-instance ',name ,@(ctor-arguments name))))))))
  
(defcommand shell ()
  ((cmd :host local :initform (error "State shell cmd."))
   (args :host local :initform '())
   (wait :host none :initform t :initarg :wait)
   (exit-code :host remote :initform -1)
   (errorp :host local :initform t)
   (process :accessor process :initform nil)))

(defmethod command.input-stream ((self shell))
  (sb-ext:process-input (process self)))

(defmethod command.output-stream ((self shell))
  (sb-ext:process-output (process self)))

(defmethod check-exit-code ((self shell))
  (if (s-v 'errorp)
      (restart-case
	  (if (not (eq 0 (exit-code self)))
	      (error "Execution failed with error code: ~D" (exit-code self))
	      (exit-code self))
	(try-again () 
	  :report "Re-run this command."
	  (run self)))
      (exit-code self)))

(defmethod wait-process ((self shell))
  (progn
    (sb-ext:process-wait (process self))
    (setf (exit-code self) (sb-ext::process-exit-code (process self)))
    (check-exit-code self)))

(defmethod run ((self shell))
  (flet ((filter-arg (arg)
	   (cond
	     ((null arg) nil)
	     ((or (keywordp arg) (symbolp arg)) (string-downcase (symbol-name arg)))
	     ((pathnamep arg) (namestring arg))
	     (t arg))))
    (if (s-v 'verbose)
	(format t "Executing command: ~A ~{ ~A~}~%" (s-v 'cmd) (s-v 'args)))
    (setf (process self)
	  (sb-ext:run-program (filter-arg (cmd self))
			      (mapcar #'filter-arg (args self))
			      :wait nil
			      :input (if (s-v 'verbose) t :stream)
			      :output (if (s-v 'verbose) t :stream)
			      :error :output))
    (if (wait self)
	(wait-process self) 
	(values))))

(defcommand which (shell)
  ()
  (:default-initargs :cmd +which+ :verbose nil))

(defmethod run :around ((self which))
  (call-next-method)
  (when (zerop (exit-code self))
    (let ((result (read-line (command.output-stream self))))
      (pathname result))))

(defun whereis (arg)
  (which :args (list arg)))

(defparameter +darcs+ (whereis "darcs"))
(defparameter +svn+ (whereis "svn"))
(defparameter +cvs+ (whereis "cvs"))
(defparameter +wget+ (whereis "wget"))
(defparameter +tar+ (whereis "tar"))
(defparameter +mv+ (whereis "mv"))
(defparameter +rm+ (whereis "rm"))
(defparameter +ln+ (whereis "ln"))
(defparameter +find+ (whereis "find"))
(defparameter +cp+ (whereis "cp"))

(defcommand find-file (shell)
  ((name :host local :initform (error "must specify a filename pattern.")))
  (:default-initargs :cmd +find+ :verbose nil))

(defmethod run ((self find-file))
  (setf (args self) (list "-name" (name self) "-type" "f"))
  (call-next-method)
  (loop for line = (read-line (command.output-stream self) nil nil)
     while line
     collect line))

(defcommand ln (shell)
  ((source :host local :initform (error "please specify source"))
   (target :host local :initform (error "please specify target")))
  (:default-initargs :cmd +ln+ :args '("-sf")))

(defmethod run ((self ln))
  (setf (args self)
	(append (args self)
		(list (s-v 'source) (s-v 'target))))
  (call-next-method))

(defvar +chown+ (whereis "chown"))
(defcommand chown (shell)
  ((user :host local :initarg :user :initform nil)
   (group :host local :initarg :group :initform nil)
   (recursive :host local :initarg :recursive :initform nil)
   (path :host local :initarg :path :initform (error "Pathname must be specified.")))  
  (:default-initargs :cmd +chown+))

(defmethod run ((self chown))
  (when (and (null (s-v 'user)) (null (s-v 'group)))
    (error "Username and group can't be empty at the same time."))
  (setf (s-v 'args)
	(cons (concatenate 'string (s-v 'user) ":" (s-v 'group))
	      (if (s-v 'recursive)
		  (cons "-R" (cons (s-v 'path) (s-v 'args)))
		  (cons (s-v 'path) (s-v 'args)))))
  (call-next-method))

(defvar +chmod+ (whereis "chmod"))
(defcommand chmod (shell)
  ((mode :host local :initarg :mode :initform (error "Please specify mode."))
   (path :host local :initarg :path :initform (error "Please specify path."))
   (recursive :host local :initarg :recursive :initform nil))
  (:default-initargs :cmd +chmod+))

(defmethod run ((self chmod))
  (setf (s-v 'args)
	(cons (s-v 'mode)
	      (if (s-v 'recursive)
		  (cons "-R" (cons (s-v 'path) (s-v 'args)))
		  (cons (s-v 'path) (s-v 'args)))))
  (call-next-method))

(defcommand cvs (shell)
  ((op :host local :initform "co")
   (repo-type :host local :initform "pserver")
   (username :host local :initform "anonymous")
   (password :host local :initform "anonymous")
   (repo :host local :initform (error "repo can't be nil"))
   (module :host local)
   (target :host local :initform (error "target can't be nil")))
  (:default-initargs :cmd +cvs+))

(defmethod run ((self cvs))
  (setf (args self)
	(list (format nil "-d:~A:~A:~A@~A" (repo-type self)
		      (username self) (password self)
		      (repo self))
	      (op self) "-d" (target self) (module self)))
  (call-next-method))

(defcommand darcs (shell)
  ((op :host local :initform "get")
   (repo :host local :initform nil)
   (target :host local :initform nil))
  (:default-initargs :cmd +darcs+))

(defmethod run ((self darcs))
  (cond
    ((equal (op self) "get")
     (if (or (null (repo self)) (null (target self)))
	 (error "Please specify repo/target."))
     (setf (args self) (append (cons (op self) (or (args self) '()))
			       (list (repo self) (target self)))))
    ((equal (op self) "unpull")
     (setf (args self) (cons (op self) (args self)))))
  (call-next-method))

(defcommand svn (shell)
  ((op :host local :initform "co")
   (repo :host local :initform (error "repo can't be nil"))
   (target :host local :initform (error "target can't be nil")))
  (:default-initargs :cmd +svn+))

(defmethod run ((self svn))  
  (setf (args self) (list (op self) (repo self) (target self)))
  (call-next-method))

(defcommand tarball (shell)
  ((repo :host local :initform (error "repo can't be nil"))
   (temp-fname :host none :initform nil)
   (target :host local :initform (error "target can't be nil"))
   (sandbox :host none :accessor sandbox :initform nil))
  (:default-initargs :cmd +tar+ :wait t))

(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defmethod run ((self tarball))
  (let ((ftype (cond
		 ((search ".gz" (repo self)) 'gz)
		 ((search ".bz2" (repo self)) 'bz2)
		 (t (error "only bz2 or gz tarballs supported."))))
	(fname (make-pathname :name (tmpnam nil))))
    (shell :cmd +wget+ :args (list "--output-document" fname (repo self)))
    (let ((sandbox (make-pathname :directory (tmpnam nil))))
      (ensure-directories-exist sandbox)
      (setf (sandbox self) sandbox)
      (with-current-directory sandbox
	(case ftype
	  (gz
	   (setf (args self) (list "zxvf" fname))
	   (call-next-method))
	  (bz2
	   (setf (args self) (list "jxvf" fname))
	   (call-next-method)))))
    (setf (temp-fname self) fname)))

(defmethod run :after ((self tarball))
  (let ((package-directory (car (directory
				 (make-pathname :name :wild
						:type :wild
						:defaults (sandbox self))))))
    (if package-directory
	;; STUPID Debian and gnuutils this one should be simply:
	;; (shell :cmd +mv+ :args (list package-directory (target self)))	    
	(shell :cmd +mv+ :args (list package-directory
				     (subseq (format nil "~A" (target self))
					     0
					     (1- (length (namestring (target self)))))))
	(error "package tarball is bogus."))
    (when (temp-fname self)
      (delete-file (namestring (temp-fname self))))))
