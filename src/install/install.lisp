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

;; ----------------------------------------------------------------------------
;; Installation Script for Core Server Project at http://www.core.gen.tr
;; Author: Evrim Ulu <evrim@core.gen.tr>
;;
;; Trademarks for the externals systems do belong to their owners.
;; Use Google(tm) Search (http://www.google.com) to contact the owners
;; of that specific trademark.
;; ----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Functions that installer needs starts here
;; There are not loaded during normal Core-Server Startup
;;-----------------------------------------------------------------------------
#-core-server
(progn
  (in-package :cl-user)
  (require :sb-posix)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (if (null (find-package :core-server))
	(defpackage :tr.gen.core.server
	  (:use :cl)
	  (:nicknames :core-server)
	  (:export
	   #:command
	   #:shell
					;   #:darcs
	   #:svn
	   #:tarball
	   #:defcommand
	   #:find-file
	   #:ln
	   #:chmod
	   #:cvs
	   #:useradd)))))

(in-package :core-server)

;; Add distribution based features
#-core-server
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((probe-file "/etc/pardus-release")
     (pushnew :pardus *features*))
    ((probe-file "/etc/gentoo-release")
     (pushnew :gentoo *features*))
    ((probe-file "/etc/debian_version")
     (pushnew :debian *features*))))

#-core-server
(progn
  (defun ensure-list (atom-or-list)
    (if (atom atom-or-list)
	(list atom-or-list)
	atom-or-list))

  (defun extract-argument-names (lambda-list)
    "Returns a list of symbols representing the names of the
  variables bound by the lambda list LAMBDA-LIST."
    (nreverse
     (reduce #'(lambda (acc atom)
		 (if (eq #\& (aref (symbol-name atom) 0))
		     acc
		     (cons atom acc)))
	     lambda-list :initial-value nil)))

  (defmacro deftrace (name methods)
    "Defines +name-methods+ variable, trace-name, untrace-name functions
for traceing a closed system"
    (let ((var-symbol (intern (string-upcase (format nil "+~A-methods+" name))))
	  (trace-symbol (intern (string-upcase (format nil "trace-~A" name))))
	  (untrace-symbol (intern (string-upcase (format nil "untrace-~A" name)))))
      `(progn
	 (defparameter ,var-symbol ,methods)
	 (defun ,trace-symbol (&optional (methods ,var-symbol))
	   (mapcar (lambda (e) (eval `(trace ,e))) methods))
	 (defun ,untrace-symbol (&optional (methods ,var-symbol))
	   (mapcar (lambda (e) (eval `(untrace ,e))) methods)))))

  (defmacro aif (consequent then &optional else)
    "Special if that binds 'consequent' to 'it'"
    `(let ((it ,consequent))
       (if it
	   ,then
	   ,(if else
		else))))

  (defmacro awhen (consequent &body body)
    "Special when that binds 'consequent' to 'it'"
    `(let ((it ,consequent))
       (when it
	 ,@body)))


  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defmethod make-keyword ((str string))
      "Returns keyword for the string 'str'"
      (intern (string-upcase str) :keyword))

    (defmethod make-keyword ((sym symbol))
      "Returns keyword for the symbol 'sym'"
      (intern (symbol-name sym) :keyword)))

  (defun plist-to-alist (plist)
    "Transforms a plist to an alist, keywords are transformed into symbols"
    (let (key)
      (nreverse
       (reduce #'(lambda (acc atom)
		   (if (and (null key) (keywordp atom))
		       (prog1 acc (setf key atom))
		       (prog1 (cons (cons (intern (symbol-name key)) atom) acc)
			 (setf key nil))))
	       plist :initial-value nil))))

  (defun alist-to-plist (alist)
    "Transforms an alist to a plist, key symbols are transformed into keywords"
    (reduce #'(lambda (acc atom)
		(nreverse (cons (cdr atom) (cons (make-keyword (car atom)) (nreverse acc)))))
	    alist :initial-value nil))

  (defmacro s-v (slot-name)
    "Expands to (slot-value self slot-name)"
    `(slot-value self ,slot-name))

  (defmacro with-current-directory (directory &body body)
    "Executes body while setting current directory to 'directory'"
    `(unwind-protect
	  (progn
	    (sb-posix:chdir ,directory)
	    (let ((*default-pathname-defaults* ,directory))
	      ,@body))
       (sb-posix:chdir *default-pathname-defaults*))))

#-core-server
(mapc #'(lambda (lisp)
	  (if (null (load lisp))
	      (error "Failed to load ~A" lisp)))
      '("search.lisp" "mop.lisp" "class+.lisp" "command.lisp"))
;;-----------------------------------------------------------------------------
;; Functions that installer needs ends here
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; System Definition
;;-----------------------------------------------------------------------------
(defclass sys ()
  ((name :accessor name :initarg :name)
   (repo-type :accessor repo-type :initarg :repo-type
	      :initform (error "specify repo-type"))
   (repo :accessor repo :initarg :repo)
   (homepage :accessor homepage :initarg :homepage)
   (module :accessor module :initarg :module :initform nil))
  (:documentation "Represents an undivisible part of a layout. It can
  be library, or a bunch of static files or other external system."))

(defmethod print-object ((self sys) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A is a ~A sys."
	    (name self) (repo-type self))))

;;-----------------------------------------------------------------------------
;; System Constructor
;;-----------------------------------------------------------------------------
(defun make-system (name type repo &key homepage module)
  "Return a new system having 'name', 'type' and 'repo'
location. 'Homepage' and 'module' for systems are optional. 'module'
is needed in some systems like CVS"
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

;;-----------------------------------------------------------------------------
;; System Protocol
;;-----------------------------------------------------------------------------
(defgeneric target-directory (sys)
  (:documentation "Getter for target-directory of this system"))

(defgeneric fetch (sys &optional path)
  (:documentation "Fetchs this system from its 'repository'/'module'"))

(defmethod target-directory ((self sys))
  (make-pathname :directory (list :relative
				  (string-downcase (symbol-name (name self))))))


(defmethod fetch :around ((sys sys) &optional path)  
  (with-current-directory (or path *default-pathname-defaults*)
    (call-next-method)))

(defmethod fetch ((self sys) &optional path)
  (declare (ignorable path))
  (format t "+-------------------------------------------------+~%")
  (format t "| Checking out system: ~A~3,50@T|~%"
	  (name self))
  (format t "+-------------------------------------------------+~%")
  (ecase (repo-type self)
    (cvs (cvs :repo (repo self) :module (module self) :target (name self)))
    (darcs (darcs :repo (repo self) :target (target-directory self)
		  :args '("--partial")))
    (svn (svn :repo (repo self) :target (target-directory self)))
    (tar (tarball :repo (repo self) :target (target-directory self)))))

;;-----------------------------------------------------------------------------
;; System Layout
;;-----------------------------------------------------------------------------
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
   (end.lisp :accessor layout.end.lisp :initarg :end.lisp
	     :initform #P"end.lisp")
   (core-server.sh :accessor layout.core-server.sh :initarg :core-server.sh
		   :initform #P"core-server")
   (registry :initform '() :documentation "Systems registry.")
   (root :initarg :root :initform (error "One must specify a root directory")))
  (:documentation "Represents a Core Server layout"))
  
(defun normalize-target-directory (dir)
  "Return a normalized version of 'dir' for example:
 #P\"/home/gee -> #P\"/home/gee/"
  (let ((dir (pathname dir)))
    (cond
      ((and (null (pathname-name dir))
	    (not (null (pathname-directory dir))))
       dir)
      (t
       (make-pathname :directory (append (pathname-directory dir)
					 (if (pathname-type dir)
					     (list (format nil "~A.~A"
							   (pathname-name dir)
							   (pathname-type dir)))
					     (list (pathname-name dir)))))))))

;;-----------------------------------------------------------------------------
;; System Layout Constructor
;;-----------------------------------------------------------------------------
(defun make-layout (root)
  (make-instance 'layout :root (normalize-target-directory root)))

;;-----------------------------------------------------------------------------
;; System Layout Accessors
;;-----------------------------------------------------------------------------
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

(defmethod layout.end.lisp ((self layout))
  (merge-pathnames (s-v 'end.lisp) (layout.etc self)))

(defmethod layout.core-server.sh ((self layout))
  (merge-pathnames (s-v 'core-server.sh) (layout.bin self)))

(defmethod layout.systems ((self layout))
  (merge-pathnames (s-v 'systems) (layout.lib self)))

;;-----------------------------------------------------------------------------
;; System Layout Protocol
;;-----------------------------------------------------------------------------
(defgeneric unregister-system (layout system)
  (:documentation "Unregisters a 'system' from a 'layout'"))

(defgeneric register-system (layout system)
  (:documentation "Registers a 'system' to a 'layout'"))

(defgeneric find-system (layout system-name)
  (:documentation "Returns the system associated with 'system-name' in
  'layout'"))

(defgeneric read-systems (layout)
  (:documentation "Reads system definitions from 
'(layout.lib.conf layout)'"))

(defgeneric write-systems (layout)
  (:documentation "Writes current system definitions to
 '(layout.lib.conf layout)'"))

(defgeneric checkout-system (layout system)
  (:documentation "Checks out 'system' of 'layout'"))

(defgeneric checkout-systems (layout)
  (:documentation "Checks out all systems in the registry of 'layout'"))

(defgeneric link-systems (layout)
  (:documentation "Creates links for every system in the registry of
this 'layout' to '(layout.systems self)'"))

;;-----------------------------------------------------------------------------
;; System Layout Protocol Implementation
;;-----------------------------------------------------------------------------
(defmethod unregister-system ((self layout) (sys sys))
  (setf (s-v 'registry)
	(delete sys (s-v 'registry) :key #'name :test #'equal)))

(defmethod register-system ((self layout) (sys sys))
  (unregister-system self sys)
  (pushnew sys (s-v 'registry) :key #'name))

(defmethod find-system ((self layout) sysname)
  (find-if #'(lambda (s) (string= (name s) sysname)) (s-v 'registry)))

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

;;-----------------------------------------------------------------------------
;; System Layout Template Helpers
;;-----------------------------------------------------------------------------
(defun write-template-sexp (template pathname)
  "Writes sexp (cdr 'template') to 'pathname'"
  (with-open-file (s pathname :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (mapcar #'(lambda (line) (format s "~W~%" line)) (cdr template))))

(defun write-template-string (template pathname)
  "Writes string 'template' to 'pathname'"
  (with-open-file (s pathname :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (format s "~A~%" template)))

;;-----------------------------------------------------------------------------
;; System Layout Template Protocol
;;-----------------------------------------------------------------------------
(defgeneric end.lisp (layout)
  (:documentation "Returns etc/end.lisp sexp"))

(defgeneric start.lisp (layout)
  (:documentation "Returns etc/start.lisp sexp"))

(defgeneric core-server.sh (layout)
  (:documentation "Returns contents of bin/core-server.sh"))

(defgeneric emacs.sh (layout)
  (:documentation "Returns contents of bin/emacs.sh"))

(defgeneric make-installer.sh (layout)
  (:documentation "Returns contents of bin/make-installer.sh"))

(defgeneric write-templates (layout)
  (:documentation "Writes all templates to associated pathnames"))

(defgeneric create-directories (layout)
  (:documentation "Creates all directories that this 'layout' needs"))

(defgeneric install (layout)
  (:documentation "Installs this 'layout' to '(layout.target-directory layout)'"))

;;-----------------------------------------------------------------------------
;; System Layout Template Implementation
;;-----------------------------------------------------------------------------
(defmethod end.lisp ((self layout))
  `(progn
     (in-package :core-server)
     (stop *server*)
     (quit 0)))

(defmethod start.lisp ((self layout))
  `(progn     
     (in-package :cl-user)
     (require :sb-posix)
     (require :asdf)
     (pushnew ,(layout.systems self) asdf:*central-registry* :test #'equal)

     (defun scan-projects (systems-dir)
       (dolist (dir-candidate
		 (directory (concatenate 'string (namestring systems-dir) "*/")))
	 ;; skip dirs starting with a _
	 (let ((name (car (last (pathname-directory dir-candidate)))))
	   (unless (equal #\_ (elt name 0))
	     (pushnew dir-candidate asdf:*central-registry* :test 'equal)))))
     
     ;; add projects
     (scan-projects ,(merge-pathnames (layout.projects self) (layout.root self)))

     (asdf:oos 'asdf:load-op :asdf-binary-locations)
     (setf (symbol-value (find-symbol "*CENTRALIZE-LISP-BINARIES*" (find-package 'asdf)))
	   t)
     ;;     (setf asdf:*source-to-target-mappings* '((#p"/opt/sbcl/lib/sbcl/" nil)))
     ;;     /usr/share/sbcl-source/-> debian

     ;; Set Environment
     (if (null (sb-posix:getenv "CORESERVER_HOME"))
	 (sb-posix:putenv ,(format nil "CORESERVER_HOME=~A" (layout.root self))))
     
     (defun build-core-server ()
       (require :swank)
       (require :core-server)
       (require :core)
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
				  `(apache-server http-server)
				  `(http-server))
	 ()
	 (:default-initargs :name "Core-serveR" :port 8080))

       (defvar *server* (make-instance 'core-server))
       (start *server*)
       (if (status *server*)
	   (progn
	     (terpri)
	     (describe *server*)
	     (format t "Core Server Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

This program comes with ABSOLUTELY NO WARRANTY; for details type
`(show-license-warranty)'.  This is free software, and you are welcome
to redistribute it under certain conditions; type
`(show-license-conditions)' for details.~%~%")
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
# Core Server: Web Application Server
#
# Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


help ()
{
    cat <<EOF
 Usage: $0 command

Commands:

  start              Start core server
  stop               Shutdown core server
  status             Query for existence
  attach             Attach to screen instance

EOF
}

# lookup utility using which, exit if not found
lookup () {
  local ret=`which $1`
  if [ -z $ret ]; then
      echo \"I couldn't find the utility: $1. Exiting...\"
      exit 1
  else
      echo $ret
  fi
}

# run utility using lookup
runX () { 
  `lookup $1`
}

unset CORESERVER_HOME
CORESERVER_HOME=\"~A\"
MEMSIZE=\"1024\"
CONFIGFILE=\"~A\"
PID=\"~Avar/core-server.pid\"

## go to home directory
OLDPWD=`pwd`
cd ~~
case \"$1\" in
    start)
        echo -n \"[ Core-serveR ] starting \"        
        export LANG=tr_TR.UTF-8 LC_ALL=tr_TR.UTF-8
        export CORESERVER_HOME=\"$CORESERVER_HOME\"
        sleep 1
        echo \"now!\"
        $(lookup screen) -c /dev/null -dmS core-server \\
        $(lookup sbcl) --dynamic-space-size $MEMSIZE \\
        --load $CONFIGFILE
        ;;
    stop)
        echo \"[ Core-serveR ] stopping \"
        kill `cat $PID`
        ;;
    attach)
        screen -x core-server
        ;;
    status)
        PP=`cat $PID`
        if [ -z \"`/bin/cat /proc/$PP/status 2&> /dev/null`\" ]; then
            echo \"[ Core-server ] *not* running\"
            exit 1
        else 
            echo \"[ Core-server ] running\"
            exit 0 
        fi
        ;;
    *)
        help
        ;;
esac
cd $OLDPWD
exit 0
" (layout.root self) (layout.start.lisp self) (layout.root self)))

(defmethod emacs.sh ((self layout))
  (format nil "
#!/bin/sh
if [ -z $CORESERVER_HOME ]; then
  export CORESERVER_HOME=\"~A\"
fi
emacs -q -l $CORESERVER_HOME/etc/emacs/core-server.el
" (layout.root self)))

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
TARBALL=\"core-server-installer-`date +\"%Y-%m-%d\"`.tar.gz\"

$MKDIR -p $DIR/core-server-installer;
cd $DIR;
$CP $CORESERVER_HOME/src/util/search.lisp core-server-installer;
$CP $CORESERVER_HOME/src/util/mop.lisp core-server-installer;
$CP $CORESERVER_HOME/src/util/class+.lisp core-server-installer;
$CP $CORESERVER_HOME/src/install/* core-server-installer;
$CP $CORESERVER_HOME/src/commands/command.lisp core-server-installer;
$CP $CORESERVER_HOME/doc/README core-server-installer;
$TAR zcf $TARBALL *
mv $TARBALL /tmp/
echo \"[Core serveR] Installer tarball is ready: /tmp/$TARBALL \"
" (layout.root self)))

(defmethod write-templates ((self layout))
  (write-template-sexp (start.lisp self) (layout.start.lisp self))
  (write-template-sexp (end.lisp self) (layout.end.lisp self))
  (write-template-string (core-server.sh self) (layout.core-server.sh self))
  (write-template-string (emacs.sh self)
			 (merge-pathnames #P"emacs.sh"
					  (layout.bin self)))
  (write-template-string (make-installer.sh self)
			 (merge-pathnames #P"make-installer.sh"
					  (layout.bin self))))

(defmethod create-directories ((self layout))
  (mapcar #'(lambda (slot)
	      (ensure-directories-exist (merge-pathnames (s-v slot) (layout.root self))))
	  '(bin projects lib var log))
  (ensure-directories-exist (layout.systems self)))

(defmethod install ((self layout)) 
  (create-directories self)
  (read-systems self)
  (checkout-systems self)
  (link-systems self)
  (ln :source (merge-pathnames #P"core-server/etc" (layout.lib self))
      :target (layout.root self))
  (ln :source (merge-pathnames #P"core-server/doc" (layout.lib self))
      :target (layout.root self))
  (write-templates self)
  (chmod :mode "+x" :path (layout.core-server.sh self))
  (shell :cmd +cp+ :args (list "lib.conf" (layout.etc self)))
  (ln :source (merge-pathnames #P"core-server/src" (layout.lib self))
      :target (layout.root self))
  (ln :source (merge-pathnames #P"core-server/t" (layout.lib self))
      :target (layout.root self))
  (ln :source (merge-pathnames #P"core-server/examples" (layout.lib self))
      :target (layout.root self)))

;;-----------------------------------------------------------------------------
;; Server System Layout
;;-----------------------------------------------------------------------------
;;
;; This layout is used to install/manage Core Server and severeal unix servers
;;
(defclass server-layout (layout)
  ()
  (:default-initargs :server-type :mod-lisp
    :server-port 3001
    :server-address "127.0.0.1"))

;;-----------------------------------------------------------------------------
;; Server System Layout Constructor
;;-----------------------------------------------------------------------------
(defun make-server-layout (root)
  (make-instance 'server-layout :root (normalize-target-directory root)))

;;-----------------------------------------------------------------------------
;; Server System Layout Implementation
;;-----------------------------------------------------------------------------
(defmethod core-server.sh ((self server-layout))
  (format nil "#!/bin/bash
help ()
{
    cat <<EOF
 Usage: $0 command

Commands:

  start              Start core server
  stop               Shutdown core server
  status             Query for existence
  attach             Attach to screen instance

EOF
}

# lookup utility using which, exit if not found
lookup () {
  local ret=`which $1`
  if [ -z $ret ]; then
      echo \"I couldn't find the utility: $1. Exiting...\"
      exit 1
  else
      echo $ret
  fi
}

# run utility using lookup
runX () { 
  `lookup $1`
}

# Indefinitely try to run as core.
# Recursive.
CORE=`id core 2&> /dev/null`
CORESERVER_HOME=\"~A\"

# if the current user is NOT core and the core id IS present
if [ ! $(runX whoami) = \"core\" ] && [ -n \"$CORE\"]; then
        $(lookup chmod) g+rw $(runX tty)
        $(lookup su) core -c \"$0 $@\"
        exit $?
fi

unset CORESERVER_HOME
CORESERVER_HOME=\"~A\"
MEMSIZE=\"1024\"
CONFIGFILE=\"~A\"
PID=\"~Avar/core-server.pid\"

## go to home directory
OLDPWD=`pwd`
cd ~~
case \"$1\" in
    start)
        echo -n \"[ Core-serveR ] starting \"        
        export LANG=tr_TR.UTF-8 LC_ALL=tr_TR.UTF-8
        export CORESERVER_HOME=\"$CORESERVER_HOME\"
        sleep 1
        echo \"now!\"
        $(lookup screen) -c /dev/null -dmS core-server \\
        $(lookup sbcl) --dynamic-space-size $MEMSIZE \\
        --load $CONFIGFILE
        ;;
    stop)
        echo \"[ Core-serveR ] stopping \"
        kill `cat $PID`
        ;;
    attach)
        screen -x core-server
        ;;
    status)
        PP=`cat $PID`
        if [ -z \"`/bin/cat /proc/$PP/status 2&> /dev/null`\" ]; then
            echo \"[ Core-server ] *not* running\"
            exit 1
        else 
            echo \"[ Core-server ] running\"
            exit 0 
        fi
        ;;
    *)
        help
        ;;
esac
cd $OLDPWD
exit 0
" (layout.root self) (layout.root self) (layout.start.lisp self) (layout.root self)))

(defmethod start.lisp ((self server-layout))
  `(progn     
     (in-package :cl-user)
     (require :sb-posix)
     (require :asdf)
     (pushnew ,(layout.systems self) asdf:*central-registry* :test #'equal)

     (defun scan-projects (systems-dir)
       (dolist (dir-candidate
		 (directory (concatenate 'string (namestring systems-dir) "*/")))
	 ;; skip dirs starting with a _  (ie _darcs)
	 (let ((name (car (last (pathname-directory dir-candidate)))))
	   (unless (equal #\_ (elt name 0))
	     (pushnew dir-candidate asdf:*central-registry* :test 'equal)))))
     
     ;; add projects
     (scan-projects ,(merge-pathnames (layout.projects self) (layout.root self)))

     (asdf:oos 'asdf:load-op :asdf-binary-locations)
     (setf (symbol-value (find-symbol "*CENTRALIZE-LISP-BINARIES*" (find-package 'asdf)))
	   t)
     ;;     (setf asdf:*source-to-target-mappings* '((#p"/opt/sbcl/lib/sbcl/" nil)))
     ;;     /usr/share/sbcl-source/-> debian

     ;; Set Environment
     (if (null (sb-posix:getenv "CORESERVER_HOME"))
	 (sb-posix:putenv ,(format nil "CORESERVER_HOME=~A" (layout.root self))))
     
     (defun build-core-server ()
       (require :swank)
       (require :core-server)
       (require :core)
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
				  `(apache-server http-server)
				  `(http-server))
	 ()
	 (:default-initargs :name "Core-serveR"))

       (defvar *server* (make-instance 'core-server))
       (start *server*)
       (if (status *server*)
	   (progn
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

;; chown :apache /var/www
;; chmod g+w /var/www
;; chown :apache /etc/apache2/vhosts.d
;; chmod g+w /etc/apache2/vhosts.d
(defvar +sudoers+ "core   ALL= NOPASSWD: /usr/sbin/apache2ctl, /etc/init.d/apache2, /etc/init.d/postfix, /etc/init.d/svscan, /usr/bin/find, /bin/chmod, /bin/chown")
(defmethod write-templates ((self server-layout))
  (write-template-sexp (start.lisp self) (layout.start.lisp self))
  (write-template-sexp (end.lisp self) (layout.end.lisp self))
  (write-template-string (core-server.sh self) (layout.core-server.sh self))
  (write-template-string (emacs.sh self)
			 (merge-pathnames #P"emacs.sh"
					  (layout.bin self)))
  (write-template-string (make-installer.sh self)
			 (merge-pathnames #P"make-installer.sh"
					  (layout.bin self))))


(defmethod configure-debian-apache ((self server-layout))
  (with-open-file (s #P"/etc/apache2/mods-enabled/mod_lisp2.load"
		     :direction :output :if-exists :supersede)
    (format s "LoadModule lisp_module /usr/lib/apache2/modules/mod_lisp2.so"))
  (with-open-file (s #P"/etc/apache2/mods-enabled/mod_lisp2.conf"
		     :direction :output :if-exists :supersede)
    (format s "
        <LocationMatch \"\\.core$\">
                LispServer  127.0.0.1 3001 \"core-server\"
                SetHandler lisp-handler
        </LocationMatch>"))
  (with-current-directory #P"/etc/apache2/mods-enabled/"
    (mapcar #'(lambda (atom)
		(let ((load-file (format nil "../mods-available/~A.load" atom))
		      (conf-file (format nil "../mods-available/~A.conf" atom)))
		  (if (probe-file load-file)
		      (ln :source load-file :target "."))
		  (if (probe-file conf-file)
		      (ln :source conf-file  :target "."))))
	    '("dav" "dav_fs" "proxy" "proxy_http"))))

(defvar +gentoo-apache-config+ "
<IfDefine LISP>
        <IfModule !mod_lisp2.c>
                LoadModule lisp_module    modules/mod_lisp2.so
        </IfModule>
</IfDefine>
<IfModule mod_lisp2.c>
        <LocationMatch \"\\.core$\">
                LispServer  127.0.0.1 3001 \"core-server\"
                SetHandler lisp-handler
        </LocationMatch>
</IfModule>
")

(defvar +gentoo-apache-options+ "-D PROXY -D DAV -D DAV_FS -D LISP -D SSL")
(defmethod configure-gentoo-apache ((self server-layout))
  (with-open-file (s #P"/etc/conf.d/apache2" :direction :output :if-exists :append)
    (format s "APACHE2_OPTS+=\" ~A\"~%" +gentoo-apache-options+))
  (with-open-file (s #P"/etc/apache2/modules.d/666_mod_lisp.conf"
		     :direction :output :if-exists :supersede)
    (format s "~A~%" +gentoo-apache-config+)))

(defvar +pardus-mod-proxy-config+ "
<IfDefine PROXY_HTML>
  <IfModule !mod_proxy_html.c>
    LoadFile /usr/lib/libxml2.so
    LoadModule proxy_html_module    modules/mod_proxy_html.so
  </IfModule>
</IfDefine>

<IfModule mod_proxy_html.c>

# See http://apache.webthing.com/mod_proxy_html/ for now :/

</IfModule>
")

(defmethod configure-pardus-apache ((self server-layout))
  (with-open-file (s #P"/etc/conf.d/apache2" :direction :output :if-exists :append)
    (format s "APACHE2_OPTS+=\" ~A\"~%" +gentoo-apache-options+))
  (with-open-file (s #P"/etc/apache2/modules.d/666_mod_lisp.conf"
		     :direction :output :if-exists :supersede)
    (format s "~A~%" +gentoo-apache-config+))
  (with-open-file (s #P"/etc/apache2/modules.d/27_mod_proxy_html.conf"
		     :direction :output :if-exists :supersede)
    (format s "~A~%" +pardus-mod-proxy-config+)))

;; FIXME: this does add more lines to sudoers when re-run.
(defmethod install ((self server-layout))
  (let ((apache-group #+debian "www-data"
		      #+(or gentoo pardus) "apache")
	(apache-vhosts (merge-pathnames
			#+debian #P"sites-enabled/"
			#+(or gentoo pardus) #P"vhosts.d/"
			#P"/etc/apache2/")))			
    (unless (zerop (shell :cmd (whereis "id") :args '("core") :errorp nil))
      (groupadd :groupname "core")
      (useradd :username "core" :extra-groups apache-group
	       :group "core" :create-home t))
    (ensure-directories-exist (layout.root self))
    (chown :user "core" :group "core" :path (layout.root self) :recursive t)
    (chown :group apache-group :path "/var/www" :recursive t)
    (chmod :mode "g+w" :path "/var/www" :recursive t)
    (chown :group apache-group :path apache-vhosts :recursive t)  
    (chmod :mode "g+w" :path apache-vhosts :recursive t)
    (shell :cmd (whereis "apxs2") :args '("-i" "-c" "mod_lisp2.c"))
    (handler-bind ((error #'(lambda (e)
			      (declare (ignorable e))
			      (shell :cmd (whereis "rm") :args '("/etc/apache2/.core-server")))))
      (unless (probe-file #P"/etc/apache2/.core-server")
	(shell :cmd (whereis "touch") :args '("/etc/apache2/.core-server"))
	#+debian (configure-debian-apache self)
	#+gentoo (configure-gentoo-apache self)
	#+pardus (configure-pardus-apache self)))
    (with-open-file (s #P"/etc/sudoers" :direction :output :if-exists :append)
      (format s "~A~%" +sudoers+)))
  (call-next-method)
  (chown :user "core" :group "core" :recursive t
	 :path (layout.root self)))
