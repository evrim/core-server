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
;;| Postfix Server Implementation
;;+----------------------------------------------------------------------------
(defclass+ postfix-server (mail-server)
  ((postfix-script-pathname :accessor postfix-server.postfix-script-pathname
			    :initarg postfix-script-pathname
			    :initform (make-pathname :directory '(:absolute "etc" "init.d")
						     :name "postfix"))
   (virtual-domains-pathname :accessor postfix-server.virtual-domains-pathname
			     :initarg :virtual-domains-pathname
			     :initform #P"/etc/postfix/virtual_domains")
   (amavis-conf-pathname :accessor postfix-server.amavis-conf-pathname
			 :initarg :amavis-conf-pathname
			 :initform #P"/etc/amavis/conf.d/50-user")
   (dovecot-root-pathname :accessor postfix-server.dovecot-root-pathname
			  :initarg :dovecot-root-pathname
			  :initform (make-pathname :directory '(:absolute "home"))))
  (:default-initargs :name "Postfix Mail Server - Mix this class with
your server to manage Postfix server. See src/servers/postfix.lisp for
implementation"))

(defmethod postfix-server.backup-root-pathname ((self postfix-server))
  (let ((backup-root (merge-pathnames #P"expired-domains/"
				      (s-v 'dovecot-root-pathname))))
    (prog1 backup-root
      (unless (probe-file backup-root)
	(sudo (mkdirm :args '("-p") :path backup-root))
	(sudo (chmodm :mode "771" :recursive t :path backup-root))
	(sudo (chownm :group "core" :recursive t :path backup-root))))))

;; -------------------------------------------------------------------------
;; Postfix init.d command definition
;; -------------------------------------------------------------------------
(defcommand postfix-sysv-script (shell)
  ((parameter :host local :initform nil))
  (:default-initargs :cmd #P"/etc/init.d/postfix" :verbose t :errorp nil))

(defmethod render-arguments ((self postfix-sysv-script))
  (cons (s-v 'parameter) (s-v 'args)))

(defcommand amavis-sysv-script (shell)
  ((parameter :host local :initform nil))
  (:default-initargs :cmd #P"/etc/init.d/amavis" :verbose t :errorp nil))

(defmethod render-arguments ((self amavis-sysv-script))
  (cons (s-v 'parameter) (s-v 'args)))

;; -------------------------------------------------------------------------
;; Server Protocol Implementation
;; -------------------------------------------------------------------------
(defmethod start ((self postfix-server))
  (sudo (chownm :group "core" :path (postfix-server.virtual-domains-pathname self)))
  (sudo (chmodm :mode "660" :path (postfix-server.virtual-domains-pathname self)))
  (sudo (chownm :group "core" :path (postfix-server.amavis-conf-pathname self)))
  (sudo (chmodm :mode "660" :path (postfix-server.amavis-conf-pathname self)))
  (sudo (amavis-sysv-scriptm :parameter "start"))
  (sudo (postfix-sysv-scriptm :parameter "start")))

(defmethod stop ((self postfix-server))
  (sudo (postfix-sysv-scriptm :parameter "stop"))
  (sudo (amavis-sysv-scriptm :parameter "stop")))

;; FIXME: How to see whether amavis is running? -evrim
(defmethod status ((self postfix-server))
  (eq 0 (sudo (postfix-sysv-scriptm :parameter "status"))))

;; -------------------------------------------------------------------------
;; Postmap Command Definition
;; -------------------------------------------------------------------------
(defcommand postmap (shell)
  ()
  (:default-initargs :cmd +postmap+ :verbose nil))

;; -------------------------------------------------------------------------
;; Postfix Protocol
;; -------------------------------------------------------------------------

(defparameter +amavis-conf+ "use strict;
$final_spam_destiny       = D_DISCARD;
@local_domains_acl = ( ~{\".~A\"~^, ~} );
@local_domains_maps = ( [qw( ~{.~A~^, ~} )] );
1;
")

;; -------------------------------------------------------------------------
;; Data Homomorphism of Postfix Virtual Domains Config File (one by line)
;; -------------------------------------------------------------------------
(defrule postfix-virtual-domains? (c (acc (make-accumulator)) res)
  (:zom  (:or (:and #\Newline
		    (:do (when (> (length acc) 0)
			   (push acc res)
			   (setq acc (make-accumulator)))))
	      (:or (:and #\# (:zom (:not #\Newline) (:type octet?)))
		   (:and (:type (or visible-char? space?) c) (:collect c acc)))))
  (:return (nreverse res)))

(defrender postfix-virtual-domains! (lst)
  (:sep #\Newline lst)
  #\Newline)

(defmethod postfix-server.domains ((self postfix-server))
  (let ((input (make-core-file-input-stream
		(postfix-server.virtual-domains-pathname self))))
    (unwind-protect (postfix-virtual-domains? input)
      (close-stream input))))

(defsynhronized handle-domain ((self postfix-server) (domain string))
  (let* ((path (postfix-server.virtual-domains-pathname self))
	 (domains (cons domain (remove domain (postfix-server.domains self)
				       :test #'equal)))
	 (postfix-output (make-core-file-output-stream path))
	 (amavis-output (make-core-file-output-stream
			 (postfix-server.amavis-conf-pathname self))))
    (unwind-protect
	 (progn
	   (postfix-virtual-domains! postfix-output domains)
	   (string! amavis-output (format nil +amavis-conf+ domains domains)))
      (progn
	(close-stream postfix-output)
	(close-stream amavis-output))))
  
  (with-current-directory #P"/etc/postfix/"
    (sudo (postmapm :args (postfix-server.virtual-domains-pathname self))))

  (let ((dovecot-root (merge-pathnames (make-pathname :directory (list :relative domain))
				       (postfix-server.dovecot-root-pathname self))))
    (unless (probe-file dovecot-root)
      (sudo (mkdirm :args '("-p") :path (merge-pathnames #P"etc/" dovecot-root)))
      (sudo (chmodm :mode "771" :recursive t :path dovecot-root))
      (sudo (chownm :user "dovecot" :group "core" :recursive t :path dovecot-root)))))

(defsynhronized unhandle-domain ((self postfix-server) (domain string))
  (let* ((path (postfix-server.virtual-domains-pathname self))
	 (domains (remove domain (postfix-server.domains self)
			  :test #'equal))
	 (postfix-output (make-core-file-output-stream path))
	 (amavis-output (make-core-file-output-stream
			 (postfix-server.amavis-conf-pathname self))))
    (unwind-protect
	 (progn
	   (postfix-virtual-domains! postfix-output domains)
	   (string! amavis-output (format nil +amavis-conf+ domains domains)))
      (progn
	(close-stream postfix-output)
	(close-stream amavis-output)))
    
    (with-current-directory #P"/etc/postfix/"
      (sudo (postmapm :args (list path))))

    (let ((dovecot-root (merge-pathnames (make-pathname :directory (list :relative domain))
					 (postfix-server.dovecot-root-pathname self))))
      (when (probe-file dovecot-root)
	(sudo
	 (mvm :source dovecot-root
	      :target (merge-pathnames (make-pathname :directory
						      (list :relative
							    (format nil "~A.~A"
								    domain (get-universal-time))))
				       (postfix-server.backup-root-pathname self))))))))

;; -------------------------------------------------------------------------
;; Dovecot Protocol
;; -------------------------------------------------------------------------
(defparser dovecot-etc-passwd-line? (c (acc (make-accumulator)) res)
  (:zom (:not #\Newline)
	(:or (:and #\: (:do (push acc res)
			    (setq acc (make-accumulator))))
	     (:and (:type (or visible-char? space?) c)
		   (:collect c acc))))
  (:do (if (> (length acc) 0)
	   (push acc res)))
  (:return (nreverse res)))

(defparser dovecot-etc-passwd? (res line)
  (:zom (:dovecot-etc-passwd-line? line)
	(:do (push line res)))
  (:return res))

(defrender dovecot-etc-passwd-line! (user)
  (:sep #\: user))

(defun dovecot-etc-passwd! (stream users)
  (reduce (lambda (stream user)
	    (dovecot-etc-passwd-line! stream user)
	    (char! stream #\Newline))
	  users :initial-value stream))

(defmethod dovecot-domain-passwd-pathname ((self postfix-server) (domain string))
  (merge-pathnames (make-pathname :directory (list :relative domain "etc")
				  :name "passwd")
		   (postfix-server.dovecot-root-pathname self)))

(defmethod domain-users ((self postfix-server) (domain string))
  (if (probe-file (dovecot-domain-passwd-pathname self domain))
      (with-core-stream (s (dovecot-domain-passwd-pathname self domain))
	(dovecot-etc-passwd? s))))

(defmethod next-domain-user-id ((self postfix-server) (domain string))
  (1+ (reduce (lambda (uid user)
		(max uid (parse-integer (nth 2 user))))
	      (domain-users self domain) :initial-value 0)))

(defmethod make-dovecot-user ((self postfix-server) (domain string)
			      (username string) (password string) (uid string))
  (list username (format nil "{plain}~A" password)
	uid "core"
	"" (namestring
	    (merge-pathnames (make-pathname :directory (list :relative domain username))
			     (postfix-server.dovecot-root-pathname self)))
	"" "quota=maildir"
	"storage=10240000 userdb_mail=maildir"))

(defmethod add-email ((self postfix-server) (domain string) (username string)
		      (password string) &optional uid)
  (let* ((user (make-dovecot-user self domain username password				  
				  (format nil "~A" (or uid (next-domain-user-id self domain)))))
	 (users (domain-users self domain))
	 (output (make-core-file-output-stream (dovecot-domain-passwd-pathname self domain))))
    (unwind-protect (dovecot-etc-passwd! output
		      (cons user (remove username users :test #'equal :key #'car)))
      (close-stream output))
    (mkdir :path (merge-pathnames (make-pathname :directory (list :relative domain username))
				  (postfix-server.dovecot-root-pathname self))
	   :args '("-p"))))

(defmethod delete-email ((self postfix-server) (domain string) (username string))
  (let* ((users (domain-users self domain))
	 (output (make-core-file-output-stream (dovecot-domain-passwd-pathname self domain))))
    (unwind-protect (dovecot-etc-passwd! output (remove username users :test #'equal :key #'car))
	   
      (close-stream output))
    (when (probe-file (merge-pathnames (make-pathname :directory (list :relative domain username))
				       (postfix-server.dovecot-root-pathname self)))
      (prog1 t
	(sudo
	 (mvm :source (merge-pathnames (make-pathname :directory (list :relative domain username))
				       (postfix-server.dovecot-root-pathname self))
	      :target (merge-pathnames (make-pathname :directory
						      (list :relative domain
							    (format nil "~A.~A.old" username
								    (get-universal-time))))
				       (postfix-server.dovecot-root-pathname self))))))))

;; (defmethod add-email ((self postfix-server) (email string) (maildir string))
;;   (unwind-protect
;;        (sb-impl::process-exit-code 
;; 	(with-input-from-string (in (concatenate 'string email " " maildir))
;; 	  (sb-ext:run-program +sudo+
;; 			      (list (namestring +postmap+) "-i" "-r"
;; 				    (format nil "hash:~A" (postfix-server.virtual-mailbox-maps self)))
;; 			      :input in
;; 			      :output *standard-output*)))))

;; (defmethod del-email ((self postfix-server) (email string) &optional delete-maildir)
;;   (unwind-protect
;;        (sb-impl::process-exit-code 
;; 	(sb-ext:run-program +sudo+
;; 			    (list (namestring +postmap+)
;; 				  "-d" email
;; 				  (format nil "hash:~A" (postfix-server.virtual-mailbox-maps self)))
;; 			    :output *standard-output*)))
;;   (when delete-maildir
;;     (error "Removing maildirs not implemented yet")))