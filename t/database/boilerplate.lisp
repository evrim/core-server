(in-package :core-server.test)

(defclass+ object-with-id ()
  ((id :reader get-id :initarg :id)))

(defclass+ object-with-timestamp ()
  ((timestamp :reader get-timestamp :initarg :timestamp)))

(defclass+ tree-node ()
  ((parent)
   (children)))

(defclass+ folder (tree-node object-with-timestamp)
  ((pathname)
   (parent :accessor folder.parent :type folder)
   (children :accessor folder.files :type (* file))))

(defclass+ file (folder)
  ())

(defclass+ image (file)
  ((user :type user)))

(defclass+ blog (file)
  ((user :type user)
   (title :type string)
   (comments :type (* comment))))

(defclass+ user (folder)
  ((name :type string)
   (password :type string)
   (email :type string)))

(defclass+ comment (tree-node object-with-timestamp)
  ((user :type user)
   (text :type string)))

(defclass+ model ()
  ((users :type user :index t)
   (blogs :type blogs :index t)
   (images :type :index t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ user ()
  ((name)
   (password)
   (blogs :type blog* :relation user)))

(defclass+ blog ()
  ((user :type user :relation blogs)
   (title)
   (text)))

(defclass+ model ()
  ((users)
   (blogs)))

(defcrud user)
(defcrud blog)

(defmethod find-user ((self database) &key name password)
  (with-transaction (self)
    (reduce0 (lambda (acc atom)
	       (pushnew atom acc)
	      acc)
	     (append (find-object-with-slot self 'user 'name name)
		     (find-object-with-slot self 'user 'password password)))))

(defmethod user-add ((self database) &key (name (error "specify name")) (password nil) (blogs nil))
  (with-transaction (self)
    (let* ((id (next-id self))
	   (object (make-instance 'user :id id :name name :password password)))
      (push object (get-object-index self 'user))
      object)))

(defmethod user-delete ((self database) (user user))
  (let ((id (get-id user)))
    (with-transaction (self)
      (let ((object (find-object-with-id self id)))
	(delete object (get-object-index self 'user))
	object))))

(defmethod user-update ((self database) (user user) name)
  (let ((id (get-id user)))
    (with-transaction (self)
      (let ((object (find-object-with-id self id)))
	(update-slots object (list :name name))
	object))))

(defmethod blog-add ((self database) (user user)
		     &key (title (error "specify title")) (text (error "specify text")))
  (let ((user-id (get-id user)))
    (with-transaction (self)
      (let* ((id (next-id self))
	     (object (make-instance 'blog
				    :title title :text text
				    :user (find-object-with-id self id))))
	(push object (get-object-index self 'blog))
	(push object (blogs.user user))
	object))))

(defmethod blog-delete ((self database) (blog blog))
  (let ((id (get-id blog)))
    (with-transaction (self)
      (let ((object (find-object-with-id self id)))
	(delete object (get-object-index self 'blog))
	object))))

(defmethod blog-update ((self database) (blog blog) &key title text)
  (let ((id (get-id blog)))
    (with-transaction (self)
      (let ((object (find-object-with-id self id)))
	(update-slots object (lsit :title title :text text))
	object))))

(blogs *user*)

(blogs-of-user *app* *user*)
(user *blog*) => 10 => (find-user *app* 10)

(defmethod find-user ((self blog-application) ..)
  (call-next-method)..)