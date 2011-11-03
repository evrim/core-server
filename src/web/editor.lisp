;; +-------------------------------------------------------------------------
;; | HTML Editors
;; +-------------------------------------------------------------------------
(in-package :core-server)

(defvar +ckeditor-toolbar+
  (list
   (list "Source" "-" "Save" ;; "NewPage" "Preview" "-" "Templates" 
	 )
   (list "Cut" "Copy" "Paste" "PasteText" "PasteFromWord" "-" "Print"
	 "SpellChecker" "Scayt")
   (list "Undo" "Redo" "-" "Find" "Replace" "-" "SelectAll" "RemoveFormat")
   (list "Form" "Checkbox" "Radio" "TextField" "Textarea" "Select" "Button"
	 "ImageButton" "HiddenField")
   "/"
   (list "Bold" "Italic" "Underline" "Strike" "-" "Subscript" "Superscript")
   (list "NumberedList" "BulletedList" "-" "Outdent" "Indent" "Blockquote"
	 "CreateDiv")
   (list "JustifyLeft" "JustifyCenter" "JustifyRight" "JustifyBlock")
   (list "BidiLtr" "BidiRtl")
   (list "Link" "Unlink" "Anchor")
   (list "Image" "Flash" "MediaEmbed" "Table" "HorizontalRule" "Smiley"
	 "SpecialChar" "PageBreak" "Iframe")
   "/"
   (list "Styles" "Format" "Font" "FontSize")
   (list "TextColor" "BGColor")
   (list "Maximize" "ShowBlocks" "-" "About")))

(defparameter +ckeditor-simple-toolbar+
  (list
   (list "Cut" "Copy" "Paste" "PasteText")
   (list "Undo" "Redo" "-" "")
   (list "Bold" "Italic" "Underline" "Strike")
   ;; "/"
   (list "NumberedList" "BulletedList" "-" "Outdent" "Indent" "Blockquote")
   (list "JustifyLeft" "JustifyCenter" "JustifyRight" "JustifyBlock")
   (list "Link" "Unlink")

   ;; (list "TextColor" "BGColor")
   ))

(defparameter +ckeditor-config+
  (jobject :base-path "/js/ckeditor/"
	   :extra-plugins "autogrow,mediaembed"
	   :remove-plugins "maximize,resize"
	   :toolbar +ckeditor-toolbar+
	   :toolbar-can-collapse nil))

(defparameter +ckeditor-simple-config+
  (jobject :base-path "/js/ckeditor/"
	   :extra-plugins "autogrow,mediaembed"
	   :remove-plugins "maximize,resize"
	   :skin "office2003"
	   :toolbar-can-collapse nil
	   :startup-focus t
	   :toolbar +ckeditor-simple-toolbar+))

(defvar +ckeditor-uri+ "/js/ckeditor/ckeditor.js")
(defvar +ckeditor-source-uri+ "/js/ckeditor/ckeditor_source.js")
(defvar +ckeditor-css+ "/style/ckeditor.css")

;; -------------------------------------------------------------------------
;; Ck Editor Form Field
;; -------------------------------------------------------------------------
(defcomponent <core:ckeditor (<core:validating-input)
  ((ckeditor-uri :host remote :initform +ckeditor-uri+)
   (ckeditor-css :host remote :initform +ckeditor-css+)
   (config :host remote :initform +ckeditor-config+)
   (instance :host remote))
  (:tag . "textarea"))

(defmethod/remote get-input-value ((self <core:ckeditor))
  (let ((instance (instance self)))
    (with-slots (get-snapshot get-data) instance
      (let* ((value1 (.apply get-snapshot instance (list)))
	     (value2 (.apply get-data instance (list)))
	     (value (if (and (not (eq value2 ""))
			     (not (null value2)))
			value2
			value1)))
	(if (or (null value) (eq "" value))
	    (throw (new (*error (+ "Editor " (slot-value self 'id)
				   "is empty."))))
	    value)))))

;; CKEDITOR.instances[i].on ('click', function ()
;; 				      {alert ('test 1 2 3')
;; 				      })
(defmethod/remote validate ((self <core:ckeditor))  
  (let ((instance (instance self)))
    (if instance
	(with-slots (get-snapshot get-data) instance
	  (let* ((value1 (.apply get-snapshot instance (list)))
		 (value2 (.apply get-data instance (list)))
		 (value (if (and (not (eq value2 ""))
				 (not (null value2)))
			    value2
			    value1) ))
	    (if (or (null value) (eq "" value))
		"This field is empty."
		t)))
	"This field is empty.")))

(defmethod/remote destroy ((self <core:ckeditor))
  (remove-css (ckeditor-css self))
  (with-slots (instance) self
    (if instance
	(.apply (slot-value instance 'destroy) instance (list))))  

  (delete-slots self 'ckeditor-uri 'ckeditor-css 'config 'instance)
  (call-next-method self))

(defmethod/remote load-ckeditor ((self <core:ckeditor))
  (load-css (ckeditor-css self))
  (load-javascript (ckeditor-uri self)
    (lambda ()
      (and (not (null -c-k-e-d-i-t-o-r))
	   (not (null (slot-value -c-k-e-d-i-t-o-r 'replace)))))))

(defmethod/remote onchange ((self <core:ckeditor) e)
  (if (instance self)
      (.update-element (instance self)))
  
  (run-validator self)
  t)

(defmethod/remote bind-editor ((self <core:ckeditor))
  (load-ckeditor self)

  (if (slot-value *c-k-e-d-i-t-o-r.instances (slot-value self 'id))
      (setf (slot-value self 'id) (random-string 5)))
  
  (with-slots (parent-node) self
    (cond
      ((or (null parent-node) (null (slot-value parent-node 'tag-name)))
       (make-web-thread (lambda () (bind-editor self))))
      (t
       (let ((instance (-c-k-e-d-i-t-o-r.replace self (config self))))
	 (setf (instance self) instance)
	 (.on instance "key" (event (e) (onchange self e)))
	 (.on instance "blur" (event (e) (onchange self e)))
	 (with-slots (form) self
	   (when form
	     (let ((onsubmit (slot-value form 'onsubmit)))
	       (+ " " onsubmit)
	       (setf (slot-value form 'onsubmit)
		     (event (e)
			    (make-web-thread (lambda () (destroy self window.k)))
			    (.apply onsubmit this (list e))))))))))))

(defmethod/remote init ((self <core:ckeditor))
  (bind-editor self)
  (call-next-method self))

;; -------------------------------------------------------------------------
;; Lazy Ck Editor Form Field
;; -------------------------------------------------------------------------
(defcomponent <core:lazy-ckeditor (<core:ckeditor)
  ((default-value :host remote))
  (:default-initargs :config +ckeditor-simple-config+))

(defmethod/remote onfocus ((self <core:lazy-ckeditor) e)
  (when (null (instance self))
    (with-slots (default-value value) self
      (if (eq default-value value)
	  (setf (slot-value self 'value) "")))
    (bind-editor self)))

(defmethod/remote init ((self <core:lazy-ckeditor))
  (with-slots (default-value value) self
    (if (null default-value)
	(setf (slot-value self 'default-value) value))

    (if (or (null value) (eq "" value))
	(setf (slot-value self 'value) (slot-value self 'default-value))))
  self)

;; --------------------------------------------------------------------------
;; Ck Editor
;; --------------------------------------------------------------------------
;; (defcomponent ckeditor-component (callable-component)
;;   ((instance :host remote)
;;    (target :host remote)
;;    (ckeditor-uri :host remote :initform +ckeditor-uri+)
;;    (ckeditor-css :host remote :initform +ckeditor-css+)
;;    (config :host remote :initform +ckeditor-config+)))

;; (defmethod/remote get-data ((self ckeditor-component))
;;   (let ((instance (instance self)))
;;     (if instance
;; 	(.apply (slot-value instance 'get-snapshot) instance (list)))))

;; (defmethod/remote call-component ((self ckeditor-component))
;;   (_debug (list "ckeditor-config" (config self)))
;;   (let* ((textarea (target self))
;; 	 (editor (-c-k-e-d-i-t-o-r.replace textarea (config self)))
;; 	 (form (slot-value textarea 'form)))
;;     (setf (instance self) editor)
;;     (setf (slot-value form 'submit)
;; 	  (event (e)
;; 	    (let ((data (.get-snapshot editor)))
;; 	      (try (.destroy editor) (:catch (err) nil))
;; 	      (with-call/cc
;; 		(make-web-thread
;; 		 (lambda ()
;; 		   (answer-component self (list "save" data))))))
;; 	    (return false)))
;;     (call-next-method self)))

;; (defmethod/remote destroy ((self ckeditor-component))  
;;   (let ((_foo (event (e) (try (.destroy e) (:catch (err) nil)))))
;;     (_foo (instance self))
;;     (delete-slot self 'instance)
;;     (remove-css "http://www.coretal.net/style/ckeditor.css")
;;     (call-next-method self)))

;; (defmethod/remote init ((self ckeditor-component))
;;   (load-css (ckeditor-css self))
;;   (load-javascript (ckeditor-uri self)
;;    (lambda ()
;;      (and (not (null -c-k-e-d-i-t-o-r))
;; 	  (not (null (slot-value -c-k-e-d-i-t-o-r 'replace)))))))

;; ;; -------------------------------------------------------------------------
;; ;; Supply CkEditor Mixin
;; ;; -------------------------------------------------------------------------
;; (defcomponent supply-ckeditor ()
;;   ())

;; (defmethod/local make-ckeditor ((self supply-ckeditor))
;;   (ckeditor-component))

;; (defvar +fck-image-extensions+ '("bmp" "gif" "jpeg" "jpg" "png" "psd" "tif" "tiff"))
;; (defvar +fck-flash-extensions+ '("swf" "fla"))
;; (defvar +fck-media-extensions+
;;   '("aiff" "asf" "avi" "bmp" "fla" "flv" "gif" "jpeg" "jpg"
;;     "mid" "mov" "mp3" "mp4" "mpc" "mpeg" "mpg" "png" "qt"
;;     "ram" "rm" "rmi" "rmvb" "swf" "tif" "tiff" "wav" "wma" "wmv"))

;; ;; TODO: fix this
;; (defun/cc handle-fck-browse (path publish-path)
;;   (let ((stream (http-response.stream (context.response +context+))))
;;     (labels ((send-error (number &optional message)
;; 	       (string! stream (format nil "<Error number=\"~A\"" number))
;; 	       (if message (format nil "text=\"~A\"" message))
;; 	       (string! stream (format nil "/>~%")))
;; 	     (folder-pathname (folder)
;; 	       (merge-pathnames			      
;; 		(make-pathname :directory (list :relative folder))
;; 		(make-pathname :directory (pathname-directory (pathname path)))))
;; 	     (files-by-extensions (folder-pathname exts)
;; 	       (reduce #'(lambda (acc atom)
;; 			   (append
;; 			    (directory
;; 			     (pathname
;; 			      (format nil "~A*.~A" folder-pathname atom)))
;; 			    acc))
;; 		       exts :initial-value nil)) 
;; 	     (files (folder-pathname type)
;; 	       (string! stream (format nil "<Files>~%"))
;; 	       (mapcar #'(lambda (file)
;; 			   (when (pathname-name file)
;; 			     (string! stream
;; 				      (concatenate 'string
;; 						   "<File name=\""
;; 						   (pathname-name file) "." (pathname-type file)
;; 						   "\" size=\"" (format nil "~D"
;; 									(round
;; 									 (/ (sb-posix:stat-size (sb-posix:stat file))
;; 									    1000)))
;; 						   "\" url=\"" publish-path))
;; 			     (mapcar #'(lambda (path)
;; 					 (string! stream (concatenate 'string path "/")))
;; 				     (reverse
;; 				      (set-difference
;; 				       (pathname-directory file)
;; 				       (pathname-directory (pathname path))
;; 				       :test #'equal)))
;; 			     (string! stream
;; 				      (concatenate 'string (pathname-name file) "." (pathname-type file) "\"/>" ~%))))
;; 		       (cond
;; 			 ((equal type "Flash") (files-by-extensions folder-pathname +fck-flash-extensions+))
;; 			 ((equal type "Image") (files-by-extensions folder-pathname +fck-image-extensions+))
;; 			 ((equal type "Media") (files-by-extensions folder-pathname +fck-media-extensions+))
;; 			 (t (directory (pathname (format nil "~A*.*" folder-pathname))))))
;; 	       (string! stream (concatenate 'string "</Files>" ~%)))
;; 	     (folders (folder-pathname)	       
;; 	       (string! stream (concatenate 'string "<Folders>" ~%))
;; 	       (mapcar #'(lambda (dir)
;; 			   (string! stream (concatenate 'string "<Folder name=\""
;; 							(car (reverse (pathname-directory dir))) "\"/>" ~%)))
;; 		       (directory
;; 			(make-pathname :directory (pathname-directory folder-pathname)
;; 				       :name :wild)))
;; 	       (string! stream (concatenate 'string "</Folders>" ~%)))
;; 	     (create-folder (folder new-folder)
;; 	       (let ((new-folder (make-pathname :directory (append (pathname-directory folder)
;; 								   (list new-folder)))))
;; 		 (if (probe-file new-folder)
;; 		     (send-error 101)
;; 		     (progn
;; 		       (ensure-directories-exist new-folder)
;; 		       (if (not (probe-file new-folder))
;; 			   (send-error 102))))))
;; 	     (create-file (folder file)
;; 	       ;; (setf (get-header (context.response *context*) "Content-Type") "text/html; charset=utf-8")
;; 	       ;; 	     (if (rfc2388:mime-part-p file)		 
;; 	       ;; 		 (progn
;; 	       ;; 		   (save-mime-file
;; 	       ;; 		    file
;; 	       ;; 		    (make-pathname :directory (pathname-directory folder)
;; 	       ;; 				   :name (pathname-name (pathname (get-mime-filename file)))
;; 	       ;; 				   :type (pathname-type (pathname (get-mime-filename file)))))
;; 	       ;; 		   (<:script
;; 	       ;; 		    (<:js `(window.parent.*on-upload-completed 0 "" ,(get-mime-filename file) ""))))
;; 	       ;; 		 (<:script
;; 	       ;; 		  (<:js `(window.parent.*on-upload-completed 1 "" "" "Error."))))
;; 	       ;; 	     (flush-request-response *context*)
;; 	       (format t "creating file:~A in folder:~A" file folder)
;; 	       (return-from handle-fck-browse nil)))
;;       (with-query ((command "Command") (type "Type")) (context.request +context+)      
;; 	(if (equal command "FileUpload")
;; 	    (with-query ((folder "CurrentFolder") (file "NewFile")) (context.request +context+)
;; 	      (create-file (folder-pathname folder) file)))
      
;; 	(xml/suspend
;; 	 (lambda (stream)	   
;; 	   (string! stream (format nil "<?xml version=\"1.0\" encoding=\"utf-8\" ?>~%
;; <Connector command=\"~A\" resourceType=\"~A\">~%" command type))

;; 	   ;;      (describe (context.request *context*))
	     
;; 	   (with-query ((folder "CurrentFolder")) (context.request +context+)
;; 	     (string! stream (format nil "<CurrentFolder path=\"~A\" url=\"\"/>~%" folder))
;; 	     (cond
;; 	       ((equal command "GetFolders")	   
;; 		(folders (folder-pathname folder)))
;; 	       ((equal command "GetFoldersAndFiles")	   
;; 		(folders (folder-pathname folder))
;; 		(files (folder-pathname folder) type))
;; 	       ((equal command "CreateFolder")
;; 		(with-query ((new-folder "NewFolderName")) (context.request +context+)
;; 		  (create-folder (folder-pathname folder) new-folder)))
;; 	       (t (send-error 1 "Error.")))
;; 	     (string! stream "</Connector>"))))))))

;; (defcomponent fckeditor-component ()
;;   ())

;; (defmethod/local fck-editor-config-url ((self fckeditor-component))
;;   (action/url ()
;;     (javascript/suspend
;;      (lambda (stream)
;;        (let ((path (format nil "~Aeditor/skins/silver/" +fckeditor-path+)))
;; 	 (with-js (path) stream
;; 	   (setf (aref *f-c-k-config.*toolbar-sets "CoreDefault")
;; 		 (array
;; 		  (array "Save" "Undo" "Redo" "-")
;; 		  (array "Cut" "Copy" "Paste" "PasteText" "PasteWord")
;; 		  (array "Bold" "Italic" "Underline" "StrikeThrough")
;; 		  (array "OrderedList" "UnorderedList" "-" "Outdent" "Indent" "Blockquote")
;; 		  (array "JustifyLeft" "JustifyCenter" "JustifyRight" "JustifyFull")
;; 		  (array "TextColor" "BGColor")
;; 		  (array "Image" "Flash" "Table" "Rule")
;; 		  (array "Link" "Unlink" "Anchor")
;; 		  (array "SpecialChar" "ShowBlocks" "-" "Source")
;; 		  "/"
;; 		  (array "FontFormat" "Style" "FontName" "FontSize"))
;; 		 *f-c-k-config.*skin-path path
;; 		 *f-c-k-config.*toolbar-can-collapse false)))))))

;; (defmethod/local fck-editor-browser-url ((self fckeditor-component))
;;   (action/url ()
;;    (handle-fck-browse (format nil "/var/www/~A/" (web-application.fqdn (application self)))    
;; 		      (format nil "/~A/" (web-application.fqdn (application self))))))

;; (defjsmacro fckeditor-path ()
;;   +fckeditor-path+)

;; (defjsmacro fckeditor-url ()
;;   `(+ ,+fckeditor-path+ "fckeditor.js"))

;; (defjsmacro fckeditor-browser-url ()
;;   `(+ ,+fckeditor-path+ "editor/filemanager/browser/default/browser.html?Connector="))

;; (defmethod/remote create-fck-editor ((self fckeditor-component) id width height)  
;;   (if (= "undefined" (typeof *f-c-keditor))
;;       (dojo.xhr-get (create :url (fckeditor-url)
;; 			    :sync t
;; 			    :handle-as "javascript")))
;;   (if (= "undefined" (typeof *f-c-k-editor))
;;       (let ((script (document.create-element "script")))
;; 	(setf script.src (fckeditor-url))
;; 	(.append-child (aref (document.get-elements-by-tag-name "head") 0) script)))
;;   (let ((fck (new (*f-c-keditor id width height)))
;; 	(config (slot-value fck '*config))
;; 	(browse-path (+ (fckeditor-browser-url)
;; 			base-url
;; 			(.replace
;; 			 (.replace (.replace (this.fck-editor-browser-url) "&" "%26") "&" "%26")
;; 			 "?" "%3F"))))
	  
;;     (setf fck.*toolbar-set "CoreDefault"
;; 	  fck.*base-path (fckeditor-path)
;; 	  config.*custom-configurations-path (+ base-url (this.fck-editor-config-url))
;; 	  config.*link-browser-u-r-l browse-path
;; 	  config.*image-browser-u-r-l browse-path
;; 	  config.*flash-browser-r-u-l browse-path
;; 	  config.*link-upload false
;; 	  config.*image-upload false
;; 	  config.*flash-upload false)
;;     (return fck)))

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
