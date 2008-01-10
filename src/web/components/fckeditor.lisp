(in-package :core-server)

(defcomponent fckeditor-component ()
  ())

(defmethod/local fck-editor-config-url ((self fckeditor-component))
  (action/url ()
    (<:js
     `(setf (aref *f-c-k-config.*toolbar-sets "CoreDefault")
	    (array
	     (array "Save" "Undo" "Redo" "-")
	     (array "Cut" "Copy" "Paste" "PasteText" "PasteWord")
	     (array "Bold" "Italic" "Underline" "StrikeThrough")
	     (array "OrderedList" "UnorderedList" "-" "Outdent" "Indent" "Blockquote")
	     (array "JustifyLeft" "JustifyCenter" "JustifyRight" "JustifyFull")
	     (array "TextColor" "BGColor")
	     (array "Image" "Flash" "Table" "Rule")
	     (array "Link" "Unlink" "Anchor")
	     (array "SpecialChar" "ShowBlocks" "-" "Source")
	     "/"
	     (array "FontFormat" "Style" "FontName" "FontSize"))
	    *f-c-k-config.*skin-path "/fckeditor/editor/skins/silver/"
	    *f-c-k-config.*toolbar-can-collapse false))))

(defjsmacro fckeditor-url ()
  `(+ ,+fckeditor-path+ "fckeditor.js"))

(defmethod/remote create-fck-editor ((self fckeditor-component) id width height)  
  (if (= "undefined" (typeof *f-c-keditor))
      (dojo.xhr-get (create :url (fckeditor-url)
			    :sync t
			    :handle-as "javascript")))
  (if (= "undefined" (typeof *f-c-k-editor))
      (let ((script (document.create-element "script")))
	(setf script.src (fckeditor-url))
	(.append-child (aref (document.get-elements-by-tag-name "head") 0) script)))
  (let ((fck (new (*f-c-keditor id width height)))
	(config (slot-value fck '*config)))
	  
    (setf fck.*toolbar-set "CoreDefault"
	  config.*custom-configurations-path (+ base-url (this.fck-editor-config-url)))
    (return fck)))
