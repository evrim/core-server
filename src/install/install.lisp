;; -----------------------------------------------------------------------------
;; Copyright (c) Core-serveR 2006-2007
;; Installation Script for Core Server Project at http://www.core.gen.tr
;; Author: Evrim Ulu <evrim@core.gen.tr>
;;
;; Trademarks for the externals systems do belong to their owners.
;; Use Google Search to contact the owners of that specific trademark.
;; -----------------------------------------------------------------------------

(in-package :cl-user)
(require :sb-posix)
(defpackage :tr.gen.core.install
  (:use :cl)
  (:nicknames :core-server.install))

(in-package #:tr.gen.core.install)
(defvar +verbose+ t "make command executions verbose during installation.")
(defvar +which+ #P"/usr/bin/which")
(defparameter +tmp+ (make-pathname :directory '(:absolute "tmp")))

(defmacro s-v (slot-name)
  "(slot-value self slot-name)"
  `(slot-value self ,slot-name))

(defmacro with-current-directory (dir &body body)
  `(unwind-protect (progn
		     (sb-posix:chdir ,dir)
		     (let ((*default-pathname-defaults* ,dir))
		       ,@body))
     (sb-posix:chdir *default-pathname-defaults*)))

(defmacro with-package (package &body body)
  `(let ((*package* (find-package ,package)))
     ,@body))

(defmacro ->keyword (symbol)
  (let ((sym (gensym)))
    `(let ((,sym ,symbol))       
       (intern
	(typecase ,sym
	  (symbol (string-upcase (symbol-name ,sym)))
	  (string (string-upcase ,sym))
	  (t ,sym))
	:keyword))))

(defun uniq (lst &optional (key #'identity) &aux new)  
  (mapcar #'(lambda (atom) (pushnew atom new :key key)) lst)
  (nreverse new))

(defun flatten (lst)
  "Removes nestings from a list."
  (cond ((atom lst) lst)
	((listp (car lst))
	 (append (flatten (car lst)) (flatten (cdr lst))))
	(t (append (list (car lst)) (flatten (cdr lst))))))

;; This is the generic search algorithm which is given in PAIP and
;; AIMA books.
(defun core-search (states goal-p successors combiner)
  "Find a state that satisfies the goal-p. Start with states, and
  search according to successors and combiner"
  (cond ((null states) 'fail)
	((funcall goal-p (car states)) (car states))
	(t (core-search
	    (funcall combiner
		     (funcall successors (car states))
		     (rest states))
	    goal-p successors combiner))))

;; Standard successors
(defun class-successors (class)
  (if (eq class (find-class 'command))
      nil
      (sb-mop:class-direct-superclasses class)))

;; INSTALL> (class-superclasses 'c)
;; (#<STANDARD-CLASS C> #<STANDARD-CLASS B> #<STANDARD-CLASS A>
;;  #<STANDARD-CLASS COMMAND>)
(defun class-superclasses (class &aux lst)  
  (core-search (cons (find-class class)
		     (copy-list
		      (sb-mop:class-direct-superclasses (find-class class))))
	       #'(lambda (atom)
		   (pushnew atom lst)
		   nil) 
	       #'class-successors
	       #'append)
  (nreverse lst))

;; INSTALL> (class-default-initargs 'c)
;; ((:ARG-B 'ARG-B-OVERRIDE-BY-C #<FUNCTION {BC06125}>)
;;  (:ARG-A 'ARG-A-OVERRIDEN-BY-C #<FUNCTION {BC06195}>))
(defun class-default-initargs (class &aux lst)
  (core-search (cons (find-class class)
		     (copy-list
		      (sb-mop:class-direct-superclasses (find-class class))))
	       #'(lambda (atom)
		   (let ((args (copy-list
				(sb-mop:class-direct-default-initargs atom))))
		     (when args (setf lst (append args lst))))
		   nil)
	       #'class-successors
	       #'append)
  lst)

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

(eval-when (:execute)
  (defvar +command-registry+ (make-hash-table :test #'equal)))

(defmacro defcommand (name supers slots &rest default-initargs)
  (labels ((clazz-name (name)
	     (intern (string-upcase (format nil "~A" name))))
	   (gen-class (name &optional direction)
	     (case direction
	       ((to view send)
		(clazz-name (format nil "~A-~A" name 'send)))
	       ((from form receive)
		(clazz-name (format nil "~A-~A" name 'receive)))
	       (t
		(clazz-name (format nil "~A" name)))))
	   (filter-slot (slot-def)
	     (when (or (eq 'local (getf (cdr slot-def) :host))
		       (eq 'both (getf (cdr slot-def) :host)))	       
	       (unless (getf (cdr slot-def) :initarg)
		 (setf (getf (cdr slot-def) :initarg)
		       (->keyword (car slot-def)))))
	     (unless (getf (cdr slot-def) :accessor)
	       (setf (getf (cdr slot-def) :accessor)
		     (car slot-def)))
	     (remf (cdr slot-def) :host)
	     slot-def)
	   (local-slot (acc slot-def)
	     (if (or (eq (getf (cdr slot-def) :host) 'local)
		     (eq (getf (cdr slot-def) :host) 'both))
		 (cons (list (car slot-def) (getf (cdr slot-def) :initform)) acc)
		 acc))
;; 	   (remote-slot (acc slot-def)
;; 	     (if (or (eq (getf (cdr slot-def) :host) 'remote)
;; 		     (eq (getf (cdr slot-def) :host) 'both))
;; 		 (cons (list (car slot-def) (getf (cdr slot-def) :initform)) acc)
;; 		 acc))
	   (local-args (slotz)
	     (let ((args (append
			  (nreverse (reduce #'local-slot slotz :initial-value nil))
			  (reduce #'(lambda (acc super)
				      (append acc (gethash super +command-registry+)))
				  supers :initial-value nil)))
		   (super-args
		    (reduce #'append (mapcar #'class-default-initargs supers))))
 	       (setf args		     
		     (reduce
		      #'(lambda (acc arg)
			  (let ((value (cadr (assoc (car arg) super-args
						    :test #'string=))))
			    (if value
				(cons (list (car arg) value) acc)
				(cons arg acc))))
			     args :initial-value nil))
	       (reduce #'(lambda (acc arg)
			   (let ((value (getf (cdar default-initargs)
					      (->keyword (car arg)))))
			     (if value
				 (cons (list (car arg) value) acc)
				 (cons arg acc))))
		       args :initial-value nil)))
;; 	   (remote-args (slotz)
;; 	     (nreverse (reduce #'remote-slot slotz :initial-value nil)))
	   (function-key-args (slotz)
	     (reduce #'(lambda (acc slot-def)			 
			 (cons (->keyword (car slot-def))
			       (cons (car slot-def) acc)))
		     (local-args slotz) :initial-value nil))
	   (filter-default-initargs (lst)
	     (nreverse (reduce #'(lambda (acc item)
				   (if (or (eq item :default-initargs)
					   (eq item :local-args)
					   (eq item :remote-args))
				       acc
				       (cons item acc)))
			       lst :initial-value nil))))
    (setf (gethash name +command-registry+) (local-args slots))
    `(progn       
       (defclass ,(gen-class name t) (,@supers command)
	 ,(mapcar #'filter-slot (copy-tree slots))
	 (:default-initargs ,@(filter-default-initargs (car default-initargs)))
	 ,@(cdr default-initargs))
       (defun ,(intern (string-upcase name)) (&key ,@(local-args slots))
	 (run (apply #'make-instance ',(gen-class name t)
		     (list ,@(function-key-args slots))))))))

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
	(progn
	  (sb-ext:process-wait (process self))
	  (setf (exit-code self) (sb-ext::process-exit-code (process self)))
	  (check-exit-code self))
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
     (setf (args self) (list (op self) (repo self) (target self))))
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

(defclass sys ()
  ((name :accessor name :initarg :name)
   (repo-type :accessor repo-type :initarg :repo-type
	      :initform (error "specify repo-type"))
   (repo :accessor repo :initarg :repo)
   (homepage :accessor homepage :initarg :homepage)
   (module :accessor module :initarg :module :initform nil)))

(defmethod print-object ((self sys) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A is a ~A sys."
	    (name self) (repo-type self))))

(defun make-system (name type repo &key homepage module)
  (make-instance 'sys
		 :name (if (stringp name)
			   (intern (string-upcase name))
			   name)
		 :repo-type (if (stringp type)
				(intern (string-upcase type))
				type)
		 :repo repo
		 :homepage homepage
		 :module module))

(defgeneric target-directory (sys))
(defmethod target-directory ((self sys))
  (make-pathname :directory (list :relative
				  (string-downcase (symbol-name (name self))))))

(defgeneric fetch (sys &optional path))

(defmethod fetch :around ((sys sys) &optional path)
  (let ((path (or path *default-pathname-defaults*)))
    (with-current-directory path
      (call-next-method))))

(defmethod fetch ((self sys) &optional path)
  (declare (ignorable path))
  (format t "+-------------------------------------------------------------------------------+~%")
  (format t "| Checking out system: ~A~3,80@T|~%"
	  (name self))
  (format t "+-------------------------------------------------------------------------------+~%")
  (ecase (repo-type self)
    (cvs (cvs :repo (repo self) :module (module self) :target (name self)))
    (darcs (darcs :repo (repo self) :target (target-directory self)))
    (svn (svn :repo (repo self) :target (target-directory self)))
    (tar (tarball :repo (repo self) :target (target-directory self)))))

(defclass layout ()
  ((bin :accessor layout.bin :initarg :bin :initform #P"bin/")
   (etc :accessor layout.etc :initarg :etc :initform #P"etc/")
   (projects :accessor layout.projects :initarg :projects :initform #P"projects/")
   (lib :accessor layout.lib :initarg :lib :initform #P"lib/")
   (systems :accessor layout.systems :initarg :systems :initform #P"systems/")
   (lib.conf :accessor layout.lib.conf :initarg :lib.conf :initform #P"lib.conf")
   (var :accessor layout.var :initarg :var :initform #P"var/")
   (log :accessor layout.log :initarg :log :initform #P"var/log/")
   (doc :accessor layout.doc :initarg :doc :initform #P"doc/")
   (server-type :accessor layout.server-type :initform :httpd :initarg :server-type)
   (server-address :accessor layout.server-address :initform "0.0.0.0"
		   :initarg :server-address)
   (server-port :accessor layout.server-port :initform 8080 :initarg :server-port)
   (swank-port :accessor layout.swank-port :initform 4005 :initarg :swank-port)
   (swank-encoding :accessor layout.swank-encoding :initform "utf-8-unix"
		   :initarg :swank-encoding)
   (start.lisp :accessor layout.start.lisp :initarg :start.lisp
	       :initform #P"start.lisp")
   (core-server.sh :accessor layout.core-server.sh :initarg :core-server.sh
		   :initform #P"core-server")
   (registry :initform '() :documentation "Systems registry.")
   (root :initarg :root :initform (error "One must specify a root directory"))))

(defun make-layout (root)
  (make-instance 'layout
		 :root (if (pathnamep root)
			   root
			   (make-pathname :directory root))))

(defmethod unregister-system ((self layout) (sys sys))
  (setf (s-v 'registry)
	(delete sys (s-v 'registry) :key #'name :test #'equal)))

(defmethod register-system ((self layout) (sys sys))
  (unregister-system self sys)
  (pushnew sys (s-v 'registry) :key #'name))

(defmethod find-system ((self layout) sysname)
  (find-if #'(lambda (s) (string= (name s) sysname)) (s-v 'registry)))

(defmethod layout.root ((self layout))
  (if (sb-posix:getenv "CORESERVER_HOME")
      (pathname (sb-posix:getenv "CORESERVER_HOME"))
      (s-v 'root)))

(defmethod layout.lib.conf ((self layout))
  (if (sb-posix:getenv "CORESERVER_HOME")
      (merge-pathnames (s-v 'lib.conf) (layout.root self))
      #P"lib.conf"))

(defmethod layout.etc ((self layout))
  (merge-pathnames (s-v 'etc) (layout.root self)))

(defmethod layout.bin ((self layout))
  (merge-pathnames (s-v 'bin) (layout.root self)))

(defmethod layout.lib ((self layout))
  (merge-pathnames (s-v 'lib) (layout.root self)))

(defmethod layout.start.lisp ((self layout))
  (merge-pathnames (s-v 'start.lisp) (layout.etc self)))

(defmethod layout.core-server.sh ((self layout))
  (merge-pathnames (s-v 'core-server.sh) (layout.bin self)))

(defmethod layout.systems ((self layout))
  (merge-pathnames (s-v 'systems) (layout.lib self)))

(defun tokenize (string)
  (let ((pos (min (or (position #\Space string) (1- (length string)))
		  (or (position #\Tab string) (1- (length string))))))
    (cond
      ((zerop (length string)) nil)
      ((or (char= #\Space (aref string 0)) (char= #\Tab (aref string 0)))     
       (tokenize (subseq string 1)))
      ((and (numberp pos) (plusp pos))
       (cons (string-trim '(#\Space #\Tab) (subseq string 0 (1+ pos)))
	     (tokenize (subseq string (1+ pos)))))      
      (t (list (string-trim '(#\Space #\Tab) string))))))

(defmethod read-systems ((self layout))  
  (with-open-file (stream (layout.lib.conf self))
    (loop for line = (read-line stream nil nil)
       while line
       when (not (char= #\# (aref line 0)))       
       do (let ((tokens (tokenize line)))
	    (register-system self (apply #'make-system tokens))))))

(defmethod write-systems ((self layout))
  (with-open-file (stream (layout.lib.conf self) :direction :output
			  :if-exists :supersede :if-does-not-exist :create)    
    (format stream "# Core-server 1.0 Dependencies~%")
    (loop for sys in (reverse (s-v 'registry))
       do (format stream "~A~0,18@T~A~0,8@T~A~%"
		  (string-downcase (name sys)) (string-downcase (repo-type sys))
		  (repo sys)))))

(defmethod checkout-system ((self layout) (sys sys))
  (fetch sys (layout.lib self)))

(defmethod checkout-systems ((self layout))  
  (mapcar #'(lambda (r) (checkout-system self r)) (s-v 'registry)))

(defmethod link-systems ((self layout))
  (with-current-directory  (layout.lib self)
    (let ((systems (find-file :name "*.asd")))
      (mapcar #'(lambda (sys)
		  (unless (search "_darcs" sys)
		    (ln :source (merge-pathnames (pathname sys) (layout.lib self))
			:target (layout.systems self))))
	      systems))))

;; Templates
(defun write-template-sexp (template pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (mapcar #'(lambda (line) (format s "~W~%" line)) (cdr template))))

(defun write-template-string (template pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (format s "~A~%" template)))

(defmethod start.lisp ((self layout))
  `(progn     
     (in-package :cl-user)
     (require :sb-posix)
     (sb-posix:putenv ,(format nil "CORESERVER_HOME=~S" (layout.root self)))
     (require :asdf)
     (pushnew ,(layout.systems self) asdf:*central-registry* :test #'equal)     
     (asdf:oos 'asdf:load-op :asdf-binary-locations)
     (setf (symbol-value (find-symbol "*CENTRALIZE-LISP-BINARIES*" (find-package 'asdf)))
	   t)
     ;;     (setf asdf:*source-to-target-mappings* '((#p"/opt/sbcl/lib/sbcl/" nil)))
     ;;     /usr/share/sbcl-source/-> debian     
     (defun build-core-server ()
       (require :ucw+)
       (require :core-server)
       (require :core)
       (require :swank)
       ;;       (require :dojo-stub)
       (values))

     (build-core-server)
     (in-package :core-server)
     
     (defun swank ()
       (setf (symbol-value (find-symbol "*CODING-SYSTEM*" (find-package 'swank)))
	     ,(layout.swank-encoding self))
       (funcall (find-symbol "CREATE-SERVER" (find-package 'swank))
		:port ,(layout.swank-port self) :dont-close t)
       (values))

     (defun load-core-server ()
       (defclass core-server ,(if (eq (layout.server-type self) :mod-lisp)
				  `(apache-server ucw-server)
				  `(ucw-server))
	 ()
	 (:default-initargs :name "Core-serveR"
	   :backend (funcall (find-symbol "MAKE-BACKEND" (find-package 'ucw))
			     ,(layout.server-type self)
			     :host ,(layout.server-address self)
			     :port ,(layout.server-port self))))

       (defvar *server* (make-instance 'core-server))
       (start *server*)
       (if (status *server*)
	   (progn
	     ;;	     (funcall (find-symbol "REGISTER-ME" (find-package 'dojo-stub)) *server*)
	     (terpri)
	     (describe *server*)
	     (write-line "Server started!")
	     (terpri))
	   (progn
	     (terpri)
	     (write-line "Unable to start server.")
	     (terpri))))
     
     (swank)
     (load-core-server)))

(defmethod core-server.sh ((self layout))
  (format nil "#!/bin/bash
help ()
{
    cat <<EOF
 Usage: core-server.sh command

Commands:

  start              Start core server.
  stop               Shutdown core server.
  attach             Attach to screen instance

EOF
}
unset CORESERVER_HOME
CORESERVER_HOME=\"~A\"
SBCL=`which sbcl`
SCREEN=`which screen`
MEMSIZE=\"1024\"
CONFIGFILE=\"~A\"
PID=\"~~/core-server.pid\"

## go to home directory
OLDPWD=`pwd`
cd ~~
case \"$1\" in
    start)
        echo \"Trying to start core-server..\"
        export LANG=tr_TR.UTF-8 LC_ALL=tr_TR.UTF-8
        export CORESERVER_HOME=\"$CORESERVER_HOME\"
        $SCREEN -c /dev/null -dmS core-server $SBCL --dynamic-space-size $MEMSIZE --load $CONFIGFILE
        ;;
    stop)
        echo \"Trying to stop core-server..\"
        kill `cat $PID`
        wait `cat $PID`
        ;;
    attach)
        screen -x core-server
        ;;
    *)
        help
        ;;
esac
cd $OLDPWD
exit 0
" (layout.root self) (layout.start.lisp self)))

(defmethod emacs.sh ((self layout))
  (format nil "
#!/bin/sh
if [ -z $CORESERVER_HOME ]; then
  export CORESERVER_HOME=\"~A\"
fi
emacs -l $CORESERVER_HOME/etc/emacs/core-server.el
"
	  (layout.root self)))

(defmethod make-installer.sh ((self layout))
  (format nil "
#!/bin/sh
if [ -z $CORESERVER_HOME ]; then
   export CORESERVER_HOME=\"~A\"
fi

TAR=`which tar`
TEMP=`which mktemp`
MKDIR=`which mkdir`
CP=`which cp`
DIR=`$TEMP -d`
TARBALL=\"core-server-installer-`date + \"%d-%m-%Y\"`.tar.gz\"

$MKDIR -p $DIR/core-server-installer;
cd $DIR;
$CP $CORESERVER_HOME/src/install/* core-server-installer;
$TAR zcf $TARBALL *
mv $TARBALL /tmp/
echo \"Core Server Installer tarball is ready: /tmp/$TARBALL \"
" (layout.root self)))

(defmethod write-templates ((self layout))
  (write-template-sexp (start.lisp self) (layout.start.lisp self)) 		       
  (write-template-string (core-server.sh self) (layout.core-server.sh self))
  (write-template-string (emacs.sh self)
			 (merge-pathnames #P"emacs.sh"
					  (layout.bin self)))
  (write-template-string (make-installer.sh self)
			 (merge-pathnames #P"make-installer.sh"
					  (layout.bin self))))

(defmethod install :before ((self layout))
  (mapcar #'(lambda (slot)
	      (ensure-directories-exist (merge-pathnames (s-v slot) (layout.root self))))
	  '(bin projects lib var log))
  (ensure-directories-exist (layout.systems self)))

(defmethod install ((self layout)) 
  (read-systems self)
  (checkout-systems self)
  (link-systems self)
  (ln :source (merge-pathnames #P"core-server/etc" (layout.lib self))
      :target (layout.root self))
  (ln :source (merge-pathnames #P"core-server/doc" (layout.lib self))
      :target (layout.root self))
  (write-templates self)
  (chmod :mode "+x" :path (layout.core-server.sh self))
  (ln :source (merge-pathnames #P"core-server/etc" (layout.lib self))
      :target (layout.root self))
  (ln :source (merge-pathnames #P"core-server/src" (layout.lib self))
      :target (layout.root self))
  (ln :source (merge-pathnames #P"core-server/doc" (layout.lib self))
      :target (layout.root self)))

(defcommand useradd (shell)
  ((username :host local :initarg :username
				  :initform (error "Username must be provided."))
   (group :host local :initarg :group :initform nil)
   (extra-groups :host local :initarg :extra-groups :initform '())
   (home-directory :host local :initarg :home-directory :initform nil)
   (user-group :host local :initarg :user-group :initform nil)
   (create-home :host local :initarg :create-home :initform nil)
   (comment :host local :initarg :comment :initform nil))
  (:default-initargs :cmd (whereis "useradd")))

(defmethod run ((self useradd))
  (setf (s-v 'args)
	  (append
	   (append
	    (s-v 'args)
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
			       (comment "-c")) :initial-value nil)))
	   (list (s-v 'username))))
  (call-next-method))

(defclass server-layout (layout)
  ()
  (:default-initargs :server-type :mod-lisp
    :server-port 3001
    :server-address "127.0.0.1"))

(defun make-server-layout (root)
  (make-instance 'server-layout
		 :root (if (pathnamep root)
			   root
			   (make-pathname :directory root))))

;; chown :apache /var/www
;; chmod g+w /var/www
;; chown :apache /etc/apache2/vhosts.d
;; chmod g+w /etc/apache2/vhosts.d
(defvar +apache-config+ "
<IfDefine LISP>
        <IfModule !mod_lisp2.c>
                LoadModule lisp_module    modules/mod_lisp2.so
        </IfModule>
</IfDefine>
<IfModule mod_lisp2.c>
        <LocationMatch \"\\.core$\">
                LispServer  127.0.0.1 3001 \"core-server\"
                SetHandler core-server
        </LocationMatch>
</IfModule>
")
(defvar +apache-options+ "-D PROXY -D DAV -D DAV_FS -D LISP -D SSL")
(defvar +sudoers+ "core   ALL= NOPASSWD: /usr/sbin/apache2ctl, /etc/init.d/apache2, /etc/init.d/postfix, /etc/init.d/svscan")
(defmethod write-templates ((self server-layout))
  (write-template-sexp (start.lisp self) (layout.start.lisp self)) 		       
  (write-template-string (core-server.sh self) (layout.core-server.sh self))
  (write-template-string (emacs.sh self)
			 (merge-pathnames #P"emacs.sh"
					  (layout.bin self)))
  (write-template-string (make-installer.sh self)
			 (merge-pathnames #P"make-installer.sh"
					  (layout.bin self))))

(defmethod install ((self server-layout))
  ;; FIXmE: debian'da www-data olmali extra-group
  (unless (zerop (shell :cmd (whereis "id") :args '("core") :errorp nil))
    (useradd :username "core" :extra-groups '("apache")
	     :user-group t :create-home t))
  (chown :user "core" :group "core" :path (layout.root self) :recursive t)
  (chown :group "apache" :path "/var/www" :recursive t)
  (chmod :mode "g+w" :path "/var/www" :recursive t)
  (chown :group "apache" :path "/etc/apache2/vhosts.d" :recursive t)
  (chmod :mode "g+w" :path "/etc/apache2/vhosts.d" :recursive t)
  (shell :cmd (whereis "apxs2") :args '("-i" "-c" "mod_lisp2.c"))
  (handler-bind ((error #'(lambda (e)
			    (declare (ignorable e))
			    (shell :cmd (whereis "rm") :args '("/etc/apache2/.core-server")))))
    (unless (probe-file #P"/etc/apache2/.core-server")
      (shell :cmd (whereis "touch") :args '("/etc/apache2/.core-server"))
      (with-open-file (s #P"/etc/conf.d/apache2" :direction :output
			 :if-exists :append)
	(format s "APACHE2_OPTS+=\" ~A\"~%" +apache-options+))
      (with-open-file (s #P"/etc/apache2/modules.d/666_mod_lisp.conf" :direction :output
			 :if-exists :supersede)
	(format s "~A~%" +apache-config+))
      (with-open-file (s #P"/etc/sudoers" :direction :output
			 :if-exists :append)
	(format s "~A~%" +sudoers+))))
  (call-next-method))
