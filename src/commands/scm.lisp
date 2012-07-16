(in-package :core-server)
;; +----------------------------------------------------------------------------
;; | SCM Commands
;; +----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; CVS
;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
;; Darcs
;; ----------------------------------------------------------------------------
(defcommand darcs (shell)
  ((op :host local :initform "get")
   (repo :host local :initform nil)
   (target :host local :initform nil)
   (lazy :host local :initform t))
  (:default-initargs :cmd (whereis "darcs")))

(defmethod render-arguments ((self darcs))
  (cons (darcs.op self)
	(cond
	  ((equal (darcs.op self) "get")
	   (if (or (null (darcs.repo self)) (null (darcs.target self)))
	       (error "Please specify repo/target."))
	   (if (s-v 'lazy)
	       (list "--lazy" (darcs.repo self) (darcs.target self))
	       (list (darcs.repo self) (darcs.target self))))
	  ((equal (darcs.op self) "unpull")
	   nil))))

;; ----------------------------------------------------------------------------
;; SVN
;; ----------------------------------------------------------------------------
(defcommand svn (shell)
  ((op :host local :initform "co")
   (repo :host local :initform (error "repo can't be nil"))
   (target :host local :initform (error "target can't be nil")))
  (:default-initargs :cmd (whereis "svn")))

(defmethod render-arguments ((self svn))
  (list (svn.op self) (svn.repo self) (svn.target self)))

;; ----------------------------------------------------------------------------
;; Tarball
;; ----------------------------------------------------------------------------
(defcommand tarball (shell)
  ((repo :host local :initform (error "repo can't be nil"))
   (temp-fname :host none :initform (make-pathname :name (tmpnam nil)))
   (target :host local :initform (error "target can't be nil"))
   (sandbox :host none :initform nil))
  (:default-initargs :cmd (whereis "tar") :wait t))

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
