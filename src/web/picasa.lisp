(in-package :core-server)

(defcomponent supply-picasa ()
  ())

(defmethod %get-albums ((self supply-picasa) user)
  (awhen (http :url (format nil +picasa-user-format-string+ user))
    (let* ((entity it)
	   (albums (xml-search entity (lambda (a) (typep a '<atom:entry)))))
      (mapcar
       (lambda (album)
	 (flet ((foo (a) (car (xml.children (car a)))))
	   (cons
	    (foo (xml-search album (make-xml-type-matcher '<atom:title)))
	    (foo (xml-search album (make-xml-type-matcher  '<gphoto:id))))))
       albums))))

(defmethod/cc get-albums ((self supply-picasa) user)
  (%get-albums self user))

(defmethod %get-photos ((self supply-picasa) user album)
  (flet ((search-for (root type)
	   (xml-search root (make-xml-type-matcher type))))
    (flet ((get-url (photo)
	     (slot-value (car (search-for photo '<media:content))
			 'core-server::url))
	   (get-name (photo)
	     (aif (car (search-for photo '<media:description))
		  (car (xml.children it))
		  (car (xml.children
			(car (search-for photo '<media:title))))))
	   (get-date (photo)
	     (car (xml.children (car (search-for photo '<atom:published)))))
	   (get-thumbnails (photo)
	     (nreverse
	      (mapcar (lambda (thumb)
			(jobject
			 :width (slot-value thumb 'core-server::width)
			 :height (slot-value thumb 'core-server::height)
			 :url (slot-value thumb 'core-server::url)))
		      (search-for photo '<media:thumbnail)))))
      (awhen (http :url (format nil +picasa-album-format-string+
				user album)
		   :cache-p t)
	(let* ((entity it)
	       (photos (xml-search entity
				   (make-xml-type-matcher '<atom:entry))))
	  (nreverse
	   (mapcar (lambda (photo)
	  	     (jobject :thumbnails (get-thumbnails photo)
	  		      :url (get-url photo)
	  		      :name (get-name photo)
	  		      :date (get-date photo)))
	  	   photos)))))))

(defmethod/cc get-photos ((self supply-picasa) user album)
  (%get-photos self user album))
