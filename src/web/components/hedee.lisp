(in-package :core-server)

(defun name-images-in-folder (pathname &optional (starting-from 0))
  (mapcar #'(lambda (path seq)
              (shell :cmd (whereis "mv")
                     :args (list path (make-pathname :directory (pathname-directory path)
                                                     :type (string-downcase (pathname-type path))
                                                     :name (format nil "~A" seq)))))
          (cl-fad:list-directory pathname)
          (mapcar (curry #'+ starting-from) (seq (length (cl-fad:list-directory pathname))))))

(defun create-thumbnails (pathname &optional (height 150) (width 150))
  (let ((thumbs-path (merge-pathnames #P"thumbs/" pathname)))
    (ensure-directories-exist thumbs-path)
    (mapcar #'(lambda (path)
		(when (or (equal (pathname-type path) "jpg")
			  (equal (pathname-type path) "png")
			  (equal (pathname-type path) "gif"))
		  (shell :cmd (whereis "convert")
			 :args (list "-thumbnail"
				     (format nil "~Dx~D>" width height)
				     path
				     (make-pathname :name (pathname-name path)
						    :type "jpg"
						    :directory (pathname-directory thumbs-path)
						    :defaults path)))))
	    (cl-fad:list-directory pathname))))

(defcomponent hedee-component (toaster-component)
  ((root :host local :accessor hedee.root
         :initform (error "please specify hedee path") :initarg :root)
   (content-id :host remote :initform (error "please specify content id") :initarg :content-id)
   (offset :host remote :initform 0 :initarg :offset)
   (len :host remote :initform -1 :initarg :len)))

(defmethod images-pathname ((self hedee-component))
  (merge-pathnames (hedee.root self)
		   (apache-web-application.docroot-pathname (application self))))

(defmethod filtered-list-directory ((self hedee-component))
  (reduce (lambda (acc atom)
	    (if (or (equal (string-downcase (pathname-type atom)) "jpg")
		    (equal (string-downcase (pathname-type atom)) "jpeg")
		    (equal (string-downcase (pathname-type atom)) "png")
		    (equal (string-downcase (pathname-type atom)) "gif"))
		(cons atom acc)
		acc))
	  (cl-fad:list-directory (images-pathname self))
	  :initial-value nil))

(defmethod/local get-total ((self hedee-component))
  (length (filtered-list-directory self)))

(defmethod list-directory ((self hedee-component) offset length)
  (mapcar (lambda (atom seq) (cons seq atom))
          (nthcdr offset (filtered-list-directory self))
          (seq length)))

(defmethod/local template ((self hedee-component) offset length)
  (flet ((image-src (path)
	   (let ((absolute-path (merge-pathnames
				 (make-pathname
				  :directory (cons :absolute
						   (cdr
						    (pathname-directory
						     (pathname (hedee.root self))))))
				 path)))
	     (namestring (make-pathname :directory (cons :relative (nreverse (cons "thumbs" (nreverse (cdr (pathname-directory absolute-path))))))
					:name (pathname-name absolute-path)
					:type (pathname-type absolute-path))))))
    (apply (curry #'<:div :class "hedee-images")         	 
	   (reduce (lambda (acc atom)
		     (let ((seq (car atom))
			   (path (cdr atom)))
		       (cons (<:div :id (format nil "hedee-div-~D" (+ offset seq))
				    :class "hedee-div"
				    (<:a :id (format nil "hedee-a-~D" (+ offset seq))
					 :class "hedee-a"					 
					 (<:img :id (format nil "~A/~A.~A" (hedee.root self)
							    (pathname-name (cdr atom))
							    (pathname-type (cdr atom))) ;;(format nil "hedee-img-~D" (+ offset seq))
						:src (image-src (cdr atom))
						:class "hedee-img")))
			     acc)))
		   (list-directory self offset length)
		   :initial-value nil))))

(defmethod/remote hide-image ((self hedee-component))
  (let ((d (dijit.by-id "hedee-dialog")))
    (if d (.hide d)))
  (return false))

(defmethod/remote show-image ((self hedee-component) image-source)
  (this.toast "Image loading, please hold...")
  (dojo.require "dijit.Dialog")
  (let ((div (document.create-element "DIV"))
	(img (document.create-element "IMG")))
    (setf div.id "hedee-dialog"
	  img.src image-source
	  img.onclick (dojo.hitch this (lambda (e) (return (this.hide-image))))
	  img.onload (lambda (e)
		       (if (dijit.by-id "hedee-dialog")
			   (.destroy (dijit.by-id "hedee-dialog")))
		       
		       (.show (new (dijit.*dialog (create :title "Hedee Image Gallery") div)))
		       (return false)))
    (div.append-child img))
  (return false))

(defmethod/remote setup ((self hedee-component))
  (if (= "undefined" (typeof ($ this.content-id)))
      (return (this.toast (+ "Sorry, " this.content-id " not found, aborting hedee..."))))

  (.append-child ($ this.content-id) (this.template this.offset (if (> 0 this.len)
								    (this.get-total)
								    this.len)))
  (dolist (a (dojo.query (+ "#" this.content-id " a.hedee-a")))
    (dojo.connect a "onclick" this (lambda (e) (return (this.show-image e.target.id)))))
  (return t))


