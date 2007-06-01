(in-package :tr.gen.core.server)
(declaim (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))

;;;-----------------------------------------------------------------------------
;;; IMPLEMENTATION OF SEVERAL BOGUS TURING MACHINES (Left is broken)
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Utils
;;;-----------------------------------------------------------------------------
(defun char->utf8 (char acc)
  (sb-impl::char->utf8 char acc))

;;;-----------------------------------------------------------------------------
;;; Simple Accumulator
;;;-----------------------------------------------------------------------------
(defun make-accumulator (&optional (type :character) (size 0))  
  "Creates an adjustable string with a fill pointer."
  (declare (type fixnum size))
  (make-array size
	      :element-type (cond
			      ((eq type :character)
			       'character)
			      ((eq type :integer)
			       'integer)
			      (t
			       '(unsigned-byte 8)))
	      :adjustable t
	      :fill-pointer 0))

(declaim (inline push-atom))
(defun push-atom (atom accumulator)
  (cond
    ((and (characterp atom) (not (eq 'character (array-element-type accumulator))))
     (char->utf8 atom accumulator))
    ((and (typep atom '(unsigned-byte 8)) (eq 'character (array-element-type accumulator)))
     (vector-push-extend (code-char atom) accumulator))
    (t
     (vector-push-extend atom accumulator))))

(defun pop-atom (accumulator)
  (vector-pop accumulator))

;;;-----------------------------------------------------------------------------
;;; Stream Definitions
;;;-----------------------------------------------------------------------------
(defclass core-stream ()
  ((%checkpoints :initform '()) ;; checkpoints list as (index #(buffer))
   (%current :initform -1 :type fixnum))) ;; current checkpoint

(defgeneric read-stream (core-stream)
  (:documentation "read byte"))

(defgeneric peek-stream (core-stream)
  (:documentation "peek byte"))

(defgeneric checkpoint-stream (core-stream)
  (:documentation "start tx"))

(defgeneric commit-stream (core-stream)
  (:documentation "commit"))

(defgeneric rewind-stream (core-stream)
  (:documentation "rollback"))

(defgeneric write-stream (core-stream atom)
  (:documentation "write byte"))

(defgeneric close-stream (core-stream)
  (:documentation "close stream, clear buffers"))

(defgeneric core-streamp (core-stream)
  (:method ((stream core-stream)) t)
  (:method ((stream t)) nil))

(defmethod transactionalp ((self core-stream))
  (> (the fixnum (s-v '%current)) -1))

(defmethod current-checkpoint ((self core-stream))
  (if (> (the fixnum (s-v '%current)) -1)
      (length (s-v '%checkpoints))
      -1))

(defmethod rewind-stream :around ((self core-stream))
  (if (not (transactionalp self))
      -1
      (call-next-method)))

(defmethod commit-stream :around ((self core-stream))
  (if (not (transactionalp self))
      -1
      (call-next-method)))

;;;-----------------------------------------------------------------------------
;;; Vector Stream
;;;-----------------------------------------------------------------------------
(defclass %core-vector-stream (core-stream)
  ((%octets :initarg :octets :initform nil :type (vector (unsigned-byte 8))) ;; underlying octets
   (%index :initform 0 :type fixnum) ;; current index   
   (%buffer :initform (make-accumulator :byte) :type (vector (unsigned-byte 8)))))

(defmethod close-stream ((self %core-vector-stream))
  (setf (s-v '%octets) nil (s-v '%index) 0 (s-v '%buffer) nil
	(s-v '%checkpoints) '() (s-v '%current) -1)
  t)

(defclass core-vector-io-stream (%core-vector-stream)
  ())

(defmethod stream-data ((self core-vector-io-stream))
  (s-v '%octets))

(defmethod peek-stream ((self core-vector-io-stream))
  (if (>= (the fixnum (s-v '%index))
	  (length (the (vector (unsigned-byte 8)) (s-v '%octets))))
      nil
      (aref (the (vector (unsigned-byte 8)) (s-v '%octets))
	    (the fixnum (s-v '%index)))))

(defmethod read-stream ((self core-vector-io-stream))  
  (let ((c (peek-stream self)))
    (when c
      (if (transactionalp self)
	  (push-atom c (s-v '%buffer)))
      (incf (the fixnum (s-v '%index)))
      c)))

(defmethod write-stream ((self core-vector-io-stream) atom)
  (if (transactionalp self)
      (push-atom atom (s-v '%buffer))      
      (if (> (length (the vector (s-v '%octets))) (the fixnum (s-v '%index)))
	  (setf (aref (the (vector (unsigned-byte 8)) (s-v '%octets))
		      (s-v '%index)) atom)
	  (push-atom atom (s-v '%octets))))
  
  (incf (the fixnum (s-v '%index))) ;; paradoxal
  )

(defmethod checkpoint-stream ((self core-vector-io-stream))
  (if (transactionalp self)
      (push (list (s-v '%current) (s-v '%buffer))
	    (s-v '%checkpoints)))

  (setf (s-v '%current) (s-v '%index)
	(s-v '%buffer) (make-accumulator :byte))
  (length (s-v '%checkpoints)))

(defmethod commit-stream ((self core-vector-io-stream))
  (let ((buffer (s-v '%buffer)))
    (prog1 (rewind-stream self)
      (reduce #'(lambda (acc item)
		  (declare (ignore acc))
		  (write-stream self item)
		  nil)
	      buffer :initial-value nil))))

(defmethod rewind-stream ((self core-vector-io-stream))
  (setf (s-v '%index) (s-v '%current))
  (prog1 (length (s-v '%checkpoints))
      (let ((previous-checkpoint (pop (s-v '%checkpoints))))
	(if previous-checkpoint
	    (setf (s-v '%current) (car previous-checkpoint)
		  (s-v '%buffer) (cadr previous-checkpoint))
	    (setf (s-v '%current) -1
		  (s-v '%buffer) (make-accumulator :byte))))))

;;;-----------------------------------------------------------------------------
;;; String Stream
;;;-----------------------------------------------------------------------------
(defclass core-string-io-stream (core-vector-io-stream)
  ((external-format :initarg :external-format :initform :utf-8)))

(defmethod shared-initialize :after ((self core-string-io-stream) slot-name
				     &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (when (getf initargs :string)
    (let ((octets (string-to-octets (getf initargs :string) (s-v 'external-format))))
      (setf (s-v '%octets)
	    (make-array (length (the vector octets))
			:fill-pointer (length (the vector octets))
			:adjustable t
			:element-type '(unsigned-byte 8)
			:displaced-to octets)))))

(defmethod stream-data ((self core-string-io-stream))
  (octets-to-string (s-v '%octets) (s-v 'external-format)))

;;;-----------------------------------------------------------------------------
;;; Socket Stream
;;;-----------------------------------------------------------------------------
(defclass %core-fd-stream (core-stream)
  ((%stream :initarg :stream :initform nil)))

(defmethod close-stream ((self %core-fd-stream))
  (close (s-v '%stream))
  (setf (s-v '%checkpoints) '() (s-v '%current) -1)
  t)

(defclass core-fd-io-stream (%core-fd-stream)
  ((%peek :initform -1 :type fixnum)
   (%read-buffer :initform (make-accumulator :byte) :type (vector (unsigned-byte 8)))
   (%read-index :initform 0 :type fixnum)
   (%write-buffer :initform (make-accumulator :byte) :type (vector (unsigned-byte 8)))))

(declaim (inline stream-using-cache?))
(defun stream-using-cache? (self)
  (< (the fixnum (s-v '%read-index))
     (length (the (vector (unsigned-byte 8)) (s-v '%read-buffer)))))

(defmethod %peek-stream ((self core-fd-io-stream))
  (if (< (the fixnum (s-v '%peek)) 0)
      (let ((peeked (read-byte (s-v '%stream) nil nil)))	
	(setf (s-v '%peek) (or peeked -1))
	peeked)
      (s-v '%peek)))

(defmethod %read-stream ((self core-fd-io-stream))
  (let ((peeked (%peek-stream self)))
    (if peeked
	(setf (s-v '%peek) -1))
    peeked))

(defmethod peek-stream ((self core-fd-io-stream))
  (if (stream-using-cache? self)
      (aref (the (vector (unsigned-byte 8)) (s-v '%read-buffer))
	    (the fixnum (s-v '%read-index)))
      (%peek-stream self)))

(defmethod read-stream ((self core-fd-io-stream))
  (if (stream-using-cache? self)
      (prog1 (aref (the (vector (unsigned-byte 8)) (s-v '%read-buffer)) (s-v '%read-index))
	(incf (the fixnum (s-v '%read-index)))
	(if (= (s-v '%read-index) (length (the (vector (unsigned-byte 8)) (s-v '%read-buffer))))
	    (setf (s-v '%read-index) 0
		  (s-v '%read-buffer) (make-accumulator :byte))))
      (let ((read (%read-stream self)))
	(when read
	  (when (transactionalp self)	      
	    (push-atom read (the (vector (unsigned-byte 8)) (s-v '%read-buffer)))
	    (incf (the fixnum (s-v '%read-index))))
	  read))))

(defmethod write-stream ((self core-fd-io-stream) atom)
  (if (transactionalp self)
      (push-atom atom (s-v '%write-buffer))      
      (prog1 (write-byte atom (s-v '%stream))
;;	(sb-impl::flush-output-buffer (s-v '%stream))
	nil))
  ;;  (incf (s-v '%read-index)) ;; paradoxal
  )

(defmethod checkpoint-stream ((self core-fd-io-stream))
  (if (transactionalp self)
      (push (list (s-v '%current) (s-v '%write-buffer))
	    (s-v '%checkpoints)))

  (setf (s-v '%current) (s-v '%read-index)
	(s-v '%write-buffer) (make-accumulator :byte))
  (length (s-v '%checkpoints)))

(defmethod %rewind-checkpoint ((self core-fd-io-stream))
  (prog1 (length (s-v '%checkpoints))
    (let ((previous-checkpoint (pop (s-v '%checkpoints))))
      (if previous-checkpoint
	  (setf (s-v '%current) (car previous-checkpoint)
		(s-v '%write-buffer) (cadr previous-checkpoint))	  
	  (setf (s-v '%current) -1
		(s-v '%write-buffer) (make-accumulator :byte))))))

(defmethod rewind-stream ((self core-fd-io-stream))
  (setf (s-v '%read-index) (s-v '%current))
  (%rewind-checkpoint self))

(defmethod commit-stream ((self core-fd-io-stream))
  (let ((buffer (s-v '%write-buffer)))
    (prog1 (%rewind-checkpoint self)
      (reduce #'(lambda (acc item)
		  (declare (ignore acc))
		  (write-stream self item)
		  nil)
	      buffer :initial-value nil))))

;;;-----------------------------------------------------------------------------
;;; File Stream
;;;-----------------------------------------------------------------------------
(defclass core-file-io-stream (core-fd-io-stream)
  ())

(defmethod shared-initialize :after ((self core-file-io-stream) slot-name
				     &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (when (getf initargs :file)    
    (setf (s-v '%stream)
	  (open (getf initargs :file)
		:direction :io
		:element-type '(unsigned-byte 8)
		:if-exists :overwrite
		:if-does-not-exist :create
		:external-format :utf8))))

;;;-----------------------------------------------------------------------------
;;; List Stream
;;;-----------------------------------------------------------------------------
(defclass core-list-io-stream (core-stream)
  ((%list :initarg :list :initform nil) ;; underlying list
   (%index :initform 0 :type fixnum)	;; current index   
   (%buffer :initform '())))

(defmethod stream-data ((self core-list-io-stream))
  (s-v '%list))

(defmethod close-stream ((self core-list-io-stream))
  (setf (s-v '%list) nil (s-v '%index) 0 (s-v '%buffer) 0
	(s-v '%checkpoints) '() (s-v '%current) -1)
  t)

(defmethod peek-stream ((self core-list-io-stream))
  (if (>= (the fixnum (s-v '%index)) (length (s-v '%list)))
      nil
      (nth (the fixnum (s-v '%index)) (s-v '%list))))

(defmethod read-stream ((self core-list-io-stream))  
  (let ((c (peek-stream self)))
    (when c
      (if (transactionalp self)
	  (push c (s-v '%buffer)))
      (incf (the fixnum (s-v '%index)))
      c)))

(defmethod write-stream ((self core-list-io-stream) atom)
  (if (transactionalp self)
      (push atom (s-v '%buffer))      
      (if (> (length (s-v '%list)) (s-v '%index))
	  (setf (nth (the fixnum (s-v '%index)) (s-v '%list)) atom)
	  (push atom (s-v '%list))))
  
  (incf (the fixnum (s-v '%index))) ;; paradoxal
  )

(defmethod checkpoint-stream ((self core-list-io-stream))
  (if (transactionalp self)
      (push (list (s-v '%current) (s-v '%buffer))
	    (s-v '%checkpoints)))

  (setf (s-v '%current) (s-v '%index)
	(s-v '%buffer) '())
  (length (s-v '%checkpoints)))

(defmethod commit-stream ((self core-list-io-stream))
  (let ((buffer (s-v '%buffer)))
    (prog1 (rewind-stream self)
      (dotimes (i (length buffer))
	(write-stream self (nth i buffer))))))

(defmethod rewind-stream ((self core-list-io-stream))
  (setf (s-v '%index) (s-v '%current))
  (prog1 (length (s-v '%checkpoints))
    (let ((previous-checkpoint (pop (s-v '%checkpoints))))
      (if previous-checkpoint
	  (setf (s-v '%current) (car previous-checkpoint)
		(s-v '%buffer) (cadr previous-checkpoint))
	  (setf (s-v '%current) -1
		(s-v '%buffer) '())))))

;;;-----------------------------------------------------------------------------
;;; Object Stream
;;;-----------------------------------------------------------------------------
(defclass core-object-io-stream (core-list-io-stream)
  ((%clazz :initarg :clazz :initform nil))) ;; underlyng object clazz

(defmethod close-stream ((self core-object-io-stream))
  (setf (s-v '%clazz) nil)
  (call-next-method))

(declaim (inline object-to-list))
(defun object-to-list (object)
  (reduce #'(lambda (acc item)
	      (let ((name (sb-mop:slot-definition-name item)))
		(if (slot-boundp object name)
		    (append acc (list (cons name (slot-value object name))))
		    acc)))
	  (sb-mop:class-slots (class-of object))
	  :initial-value nil))

(defmethod shared-initialize :after ((self core-object-io-stream) slot-name
				     &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (let ((o (getf initargs :object)))
    (when o
      (setf (s-v '%clazz) (class-name (class-of o))
	    (s-v '%list) (object-to-list o)))))

(declaim (inline list-to-object))
(defun list-to-object (list clazz)
  (let ((o (make-instance clazz)))
    (reduce #'(lambda (acc slot)
		(declare (ignore acc))
		(let ((name (sb-mop:slot-definition-name slot)))
		  (setf (slot-value o name) (cdr (assoc name list))))
		nil)
	    (sb-mop:class-slots (find-class clazz))
	    :initial-value nil)
    o))

(defmethod stream-data ((self core-object-io-stream))
  (list-to-object (s-v '%list) (s-v '%clazz)))

(defun make-core-stream (target &rest args)
  (apply #'make-instance
	 (append
	  (etypecase target
	    (string (list 'core-string-io-stream :string target))
	    (pathname (list 'core-file-io-stream :file target))
	    (array (list 'core-vector-io-stream :octets target))
	    (sb-sys::fd-stream (list 'core-fd-io-stream :stream target))
	    (list (list 'core-list-io-stream :list target))
	    (standard-object (list 'core-object-io-stream :object target)))
	  args)))

;; (defmethod rewind-stream ((self core-fd-input-stream))
;;   (setf (s-v '%read-index) (s-v '%current))
;;   (commit-stream self))

;; (defmethod commit-stream ((self core-fd-input-stream))
;;   (prog1 (length (s-v '%checkpoints))
;;     (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;;       (if previous-checkpoint
;; 	  (setf (s-v '%current) (car previous-checkpoint))
;; 	  (progn
;; 	    (setf (s-v '%current) -1))))))

;; (defmethod commit-stream ((self core-fd-output-stream))
;;   (let ((buffer (s-v '%write-buffer)))
;;     (prog1 (rewind-stream self)
;;       (dotimes (i (length buffer))
;; 	(write-stream self (aref buffer i))))))

;; (defmethod rewind-stream ((self core-fd-output-stream))
;; ;;  (setf (s-v '%index) (s-v '%current))
;;   (prog1 (length (s-v '%checkpoints))
;;       (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;; 	(if previous-checkpoint
;; 	    (setf (s-v '%current) (car previous-checkpoint)
;; 		  (s-v '%write-buffer) (cadr previous-checkpoint))
;; 	    (setf (s-v '%current) -1
;; 		  (s-v '%write-buffer) (make-accumulator :byte))))))

;; (defclass core-fd-input-stream (%core-fd-stream)
;;   ((%peek :initform -1 :type integer)
;;    (%read-buffer :initform (make-accumulator :byte) :type (array (unsigned-byte 8)))
;;    (%read-index :initform 0 :type integer)))

;; (defmethod %peek-stream ((self core-fd-input-stream))
;;   (if (< (s-v '%peek) 0)
;;       (let ((peeked (read-byte (s-v '%stream) nil nil)))	
;; 	(setf (s-v '%peek) (or peeked -1))
;; 	peeked)
;;       (s-v '%peek)))

;; (defmethod %read-stream ((self core-fd-input-stream))
;;   (let ((peeked (%peek-stream self)))
;;     (if peeked
;; 	(setf (s-v '%peek) -1))
;;     peeked))

;; ;; (declaim (inline stream-using-cache?))
;; ;; (defun stream-using-cache? (self)
;; ;;   (< (s-v '%read-index) (length (s-v '%read-buffer))))

;; (defmethod peek-stream ((self core-fd-input-stream))
;;   (if (stream-using-cache? self)
;;       (aref (s-v '%read-buffer) (s-v '%read-index))
;;       (%peek-stream self)))

;; (defmethod read-stream ((self core-fd-input-stream))
;;   (if (stream-using-cache? self)
;;       (prog1 (aref (s-v '%read-buffer) (s-v '%read-index))
;; 	(incf (s-v '%read-index))
;; 	(if (= (s-v '%read-index) (length (s-v '%read-buffer)))
;; 	    (setf (s-v '%read-index) 0
;; 		  (s-v '%read-buffer) (make-accumulator :byte))))
;;       (let ((read (%read-stream self)))
;; 	(when read
;; 	  (when (transactionalp self)	      
;; 	    (push-atom read (s-v '%read-buffer))
;; 	    (incf (s-v '%read-index)))
;; 	  read))))

;; (defmethod checkpoint-stream ((self core-fd-input-stream))
;;   (if (transactionalp self)
;;       (push (list (s-v '%current) nil) (s-v '%checkpoints)))

;;   (setf (s-v '%current) (s-v '%read-index))
;;   (length (s-v '%checkpoints)))

;; (defmethod rewind-stream ((self core-fd-input-stream))
;;   (setf (s-v '%read-index) (s-v '%current))
;;   (commit-stream self))

;; (defmethod commit-stream ((self core-fd-input-stream))
;;   (prog1 (length (s-v '%checkpoints))
;;     (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;;       (if previous-checkpoint
;; 	  (setf (s-v '%current) (car previous-checkpoint))
;; 	  (progn
;; 	    (setf (s-v '%current) -1))))))

;; (defclass core-fd-output-stream (%core-fd-stream)
;;   ((%write-buffer :initform (make-accumulator :byte) :type (array (unsigned-byte 8)))))

;; (defmethod write-stream ((self core-fd-output-stream) atom)
;;   (if (transactionalp self)
;;       (push-atom atom (s-v '%write-buffer))      
;;       (prog1 (write-byte atom (s-v '%stream))
;; 	(sb-impl::flush-output-buffer (s-v '%stream))))
;;   ;;  (incf (s-v '%read-index)) ;; paradoxal
;;   )

;; (defmethod checkpoint-stream ((self core-fd-output-stream))
;;   (if (transactionalp self)
;;       (push (list (s-v '%current) (s-v '%write-buffer))
;; 	    (s-v '%checkpoints)))

;;   (setf (s-v '%current) (length (s-v '%write-buffer))
;; 	(s-v '%write-buffer) (make-accumulator :byte))
;;   (length (s-v '%checkpoints)))

;; (defmethod commit-stream ((self core-fd-output-stream))
;;   (let ((buffer (s-v '%write-buffer)))
;;     (prog1 (rewind-stream self)
;;       (dotimes (i (length buffer))
;; 	(write-stream self (aref buffer i))))))

;; (defmethod rewind-stream ((self core-fd-output-stream))
;; ;;  (setf (s-v '%index) (s-v '%current))
;;   (prog1 (length (s-v '%checkpoints))
;;       (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;; 	(if previous-checkpoint
;; 	    (setf (s-v '%current) (car previous-checkpoint)
;; 		  (s-v '%write-buffer) (cadr previous-checkpoint))
;; 	    (setf (s-v '%current) -1
;; 		  (s-v '%write-buffer) (make-accumulator :byte))))))

;; ;;;-----------------------------------------------------------------------------
;; ;;; File Stream
;; ;;;-----------------------------------------------------------------------------
;; (defclass core-file-stream (core-stream)
;;   ((%pathname :initarg :file :initform nil)
;;    (%stream :initarg :stream :initform nil)
;;    (%checkpoint :initform -1 :type integer)
;;    (%backtrack :initform #() :type (array (unsigned-byte 8)))))

;; (defmethod shared-initialize :after ((self core-file-stream) slot-name
;; 				     &rest initargs &key &allow-other-keys)
;;   (declare (ignore initargs))
;;   (if (and (null (s-v '%stream)) (s-v '%pathname) (pathnamep (s-v '%pathname)))
;;       (setf (s-v '%stream)
;; 	    (open (s-v '%pathname) :element-type '(unsigned-byte 8)))))

;; (defmethod peek-stream ((self core-file-stream))
;;   (if (null (s-v '%octet))
;;       (read-stream self)
;;       (s-v '%octet)))

;; (defmethod read-stream ((self core-file-stream))
;;   (if (null (s-v '%octet))
;;       (setf (s-v '%octet)
;; 	    (read-byte (s-v '%stream) nil nil))
;;       (prog1 (s-v '%octet)
;; 	(setf (s-v '%octet)
;; 	      (read-byte (s-v '%stream) nil nil)))))

;; (defmethod checkpoint-stream ((self core-file-stream))
;;   (setf (s-v '%checkpoint) 0
;; 	(s-v '%backtrack) (make-accumulator :byte)))

;; (defmethod continue-stream ((self core-file-stream))
;;   )

;; (defmethod rewind-stream ((self core-file-stream))
;;   )

;; (defmethod stream-type ((stream flexi-stream))
;;   (cond
;;     ((equal 'character (flexi-stream-element-type stream))       
;;      :character)
;;     ((equal '(unsigned-byte 8) (flexi-stream-element-type stream))
;;      :byte)))

;; (defmethod stream-type ((stream vector-stream))
;;   :byte)

;; (defmethod flexi-streams::flexi-stream-last-octet ((stream vector-stream))
;;   (aref (flexi-streams::vector-stream-vector stream) (flexi-streams::vector-stream-index stream)))

;; (defmethod stream-type ((stream sb-impl::string-input-stream))
;;   :character)

;; (defmethod stream-type ((stream sb-impl::string-output-stream))
;;   :character)

;; ;;;-----------------------------------------------------------------------------
;; ;;; Stream Read/Peek Definitions
;; ;;;-----------------------------------------------------------------------------
;; (defvar +max-read-length+ nil)

;; (declaim (inline peek-stream))
;; (defun peek-stream (stream)
;;   (if (and +max-read-length+ (< +max-read-length+ 1))
;;       (return-from peek-stream nil))
     
;;   (ecase (stream-type stream)
;;     (:character (peek-char nil stream nil))
;;     (:byte (peek-byte stream))))

;; (declaim (inline read-stream))
;; (defun read-stream (stream)
;;   (when +max-read-length+
;;     (if (< +max-read-length+ 1)
;; 	(return-from read-stream nil)
;; 	(decf +max-read-length+)))
  
;;   (ecase (stream-type stream)
;;     (:character (read-char stream))
;;     (:byte (read-byte stream))))

;; (defclass core-vector-input-stream (%core-vector-stream)
;;   ())

;; (defmethod peek-stream ((self core-vector-input-stream))
;;   (if (>= (s-v '%index) (length (s-v '%octets)))
;;       nil
;;       (aref (s-v '%octets) (s-v '%index))))

;; (defmethod read-stream ((self core-vector-input-stream))
;;   (let ((c (peek-stream self)))
;;     (when c
;;       (incf (s-v '%index))
;;       c)))

;; (defmethod checkpoint-stream ((self core-vector-input-stream))
;;   "Save current transaction, create a new one having empty
;; read-buffer and position as current-index, otherwise
;; save index in offset."
;;   (if (transactionalp self)
;;       (push (cons (s-v '%current) nil)
;; 	    (s-v '%checkpoints)))
  
;;   (setf (s-v '%current) (s-v '%index))    
;;   (length (s-v '%checkpoints)))

;; (defmethod rewind-stream ((self core-vector-input-stream))
;;   (setf (s-v '%index) (s-v '%current))
;;   (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;;     (if previous-checkpoint
;; 	(setf (s-v '%current) (car previous-checkpoint))
;; 	(setf (s-v '%current) -1)))
;;   (length (s-v '%checkpoints)))

;; (defmethod commit-stream ((self core-vector-input-stream))
;;   (rewind-stream self))

;; (defclass core-vector-output-stream (%core-vector-stream)
;;   ((%write-buffer :initform (make-accumulator :byte)
;; 		  :type (array (unsigned-byte 8)))))

;; (defmethod write-stream ((self core-vector-output-stream) atom)
;;   (if (transactionalp self)
;;       (push-atom atom (s-v '%write-buffer))      
;;       (if (> (length (s-v '%octets)) (s-v '%index))
;; 	  (setf (aref (s-v '%octets) (s-v '%index)) atom)
;; 	  (push-atom atom (s-v '%octets))))
  
;;   (incf (s-v '%index)))

;; (defmethod checkpoint-stream ((self core-vector-output-stream))
;;   (if (transactionalp self)
;;       (push (list (s-v '%current) (s-v '%write-buffer))
;; 	    (s-v '%checkpoints)))

;;   (setf (s-v '%current) (s-v '%index)
;; 	(s-v '%write-buffer) (make-accumulator :byte))
;;   (length (s-v '%checkpoints)))

;; (defmethod rewind-stream ((self core-vector-output-stream))
;;   (setf (s-v '%index) (s-v '%current))
;;   (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;;     (if previous-checkpoint
;; 	(setf (s-v '%current) (car previous-checkpoint)
;; 	      (s-v '%write-buffer) (cadr previous-checkpoint))
;; 	(setf (s-v '%current) -1
;; 	      (s-v '%write-buffer) (make-accumulator :byte))))
;;   (length (s-v '%checkpoints)))

;; (defmethod commit-stream ((self core-vector-output-stream)) 
;;   (let ((buffer (s-v '%write-buffer)))
;;     (rewind-stream self)
;;     (dotimes (i (length buffer))
;;       (write-stream self (aref buffer i))))
;;   (length (s-v '%checkpoints)))

;; (defclass core-vector-stream (core-stream)
;;   ((%octets :initarg :octets :initform nil) 
;;    (%index :initform 0 :type integer)
;;    (%length :initform 0 :type integer)
;;    (%checkpoints :initform '())   
;;    (%read-buffer :initform (make-accumulator :byte)
;; 		 :type (array (unsigned-byte 8)))
;;    (%current-checkpoint :initform -1
;; 			:type unsigned-byte)
;;    (%write-buffer :initform (make-accumulator :byte)
;; 		  :type (array (unsigned-byte 8)))
;;    (%writtenp :initform nil :type boolean)))

;; (defmethod peek-stream ((self core-vector-stream))
;;   (if (>= (s-v '%index) (s-v '%length))
;;       nil
;;       (aref (s-v '%octets) (s-v '%index))))

;; (defmethod read-stream ((self core-vector-stream))
;; ;;   (if (and (transactionalp self) (s-v '%writtenp))
;; ;;       (error "Can't read, commit first."))
  
;;   (let ((c (peek-stream self)))
;;     (when c
;;       (incf (s-v '%index))
;;       c)))

;; (defmethod write-stream ((self core-vector-stream) atom)
;;   (setf (aref (s-v '%octets) (s-v '%index)) atom)
;;   (incf (s-v '%index))
;;   atom)

;; (defvar *transactional* t)
;; (defmethod transactionalp ((self core-vector-stream))
;;   (and *transactional* (> (s-v '%current-checkpoint) -1)))

;; ;; (defmethod ensure-write-state ((self core-vector-stream))
;; ;;   (mapc #'(lambda (checkpoint)
;; ;; 	    (when (and (cadr checkpoint) (> (length (cadr checkpoint)) 0))
;; ;; 	      (setf (s-v '%writtenp) t)
;; ;; 	      (return-from ensure-write-state t)))
;; ;; 	(cons (list (s-v '%current-checkpoint)
;; ;; 		    (s-v '%write-buffer))
;; ;; 	      (s-v '%checkpoints)))
;; ;;   (setf (s-v '%writtenp) nil))

;; (defmethod checkpoint-stream ((self core-vector-stream))
;;   (if (transactionalp self)
;;       (push (cons (s-v '%index)
;; 		  (s-v '%write-buffer))
;; 	    (s-v '%checkpoints)))
;;   (setf (s-v '%current-checkpoint)
;; 	(- (s-v '%index) (or (caar (s-v '%checkpoints)) 0)))
;; ;;  (ensure-write-state self)
;;   (length (s-v '%checkpoints)))

;; (defmethod read-stream :around ((self core-vector-stream))  
;;   (let ((atom (call-next-method)))
;;     ;; cache byte if we'r in a transaction
;;     (when (and (transactionalp self) atom)
;;       (push-atom atom (s-v '%read-buffer)))
;;     atom))

;; (defmethod write-stream :around ((self core-vector-stream) atom)
;;   (if (transactionalp self)
;;       (progn
;; 	(push-atom atom (cadr (s-v '%write-buffer)))
;; 	(read-stream self)
;; 	atom)
;;       (call-next-method)))

;; (defmethod commit-stream ((self core-vector-stream))
;;   (if (not (transactionalp self))
;;       (return-from commit-stream -1))

;;   (if (> (length (s-v '%write-buffer)))
;;       (dotimes (i (length (s-v '%write-buffer)))
;; 	(let ((*transactional* nil))
;; 	  (write-stream self (aref (s-v '%write-buffer) i)))))
;;   (setf (s-v '%read-buffer)
;; 	(make-array (- (length (s-v '%read-buffer))
;; 		       (s-v '%current-checkpoint))
;; 		    :element-type '(unsigned-byte 8)
;; 		    :fill-pointer 0
;; 		    :displaced-to (s-v '%read-buffer)
;; 		    :displaced-index-offset (s-v '%current-checkpoint)))
;;   (let ((last-checkpoint (pop (s-v '%checkpoints))))
;;     (when last-checkpoint
;;       (setf (s-v '%current-checkpoint) (car last-checkpoint)
;; 	    (s-v '%write-buffer) (cadr last-checkpoint)))))

;; (defmethod rewind-stream ((self core-vector-stream))
;;   (let ((last-checkpoint (pop (s-v '%checkpoints))))
;;     (when last-checkpoint
;;       (setf (s-v '%current-checkpoint) (car last-checkpoint)
;; 	    (s-v '%write-buffer) (cadr last-checkpoint)))))

;; (defmethod checkpointp ((self core-vector-stream) checkpoint)
;;   (and (integerp checkpoint) (> checkpoint -1)
;;        (< checkpoint (length (s-v '%checkpoints)))))

;; (defmethod checkpoint-index ((self core-vector-stream) &optional (checkpoint 0))
;;   (if (checkpointp checkpoint)
;;       (let ((checkpoint (aref (s-v '%checkpoints) checkpoint)))
;; 	(if (atom checkpoint)
;; 	    checkpoint
;; 	    (car checkpoint)))
;;       (error "Not a checkpoint.")))


;; (defmethod writtenp ((self core-vector-stream))
;;   (mapc #'(lambda (checkpoint)
;; 	    (if (cadr checkpoint)
;; 		(return-from writtenp t)))
;; 	(s-v '%checkpoints))
;;   nil)

;; (defmethod read-stream :around ((self core-vector-stream))
;;   (if (writtenp self)
;;       (error "Can't read, commit written checkpoints first.")
;;       (let ((atom (call-next-method)))
;; 	;; cache byte if we'r in a transaction
;; 	(when (and (transactionalp self) atom)
;; 	  (push-atom atom (s-v '%buffer)))
;; 	atom)))

;; (defmethod write-stream :around ((self core-vector-stream) atom)
;;   (if (transactionalp self)
;;       (progn
;; 	(if (listp (car (reverse (s-v '%checkpoints))))
;; 	    (push-atom atom (cadr (reverse (s-v '%checkpoints))))
;; 	    (progn
;; 	      (setf ())))
;; 	(push-atom atom (cadr (s-v '%write-buffers)))
;; ;;	(incf (s-v '%index))
;; 	atom)
;;       (call-next-method)))

;; (defmethod commit-stream ((self core-vector-stream) &optional (checkpoint 0))
;;   (if (not (transactionalp self))
;;       (return-from commit-stream nil))
  
;;   (let ((checkpoint-index (checkpoint-index self checkpoint))
;; 	(found nil))
;;     (do ((c (pop (s-v '%checkpoints)) (pop (s-v '%checkpoints))))
;; 	((or (null c) found) nil)
;;       (when (listp c)
;; 	(dotimes (i (length (cadr c)))
;; 	  (let ((*transactional* nil))
;; 	    (write-stream self (aref (cadr c) i)))))
;;       (if (or (and (listp c) (= checkpoint c)) (= checkpoint (car c)))
;; 	  (setf found t)))
    
;;     (let ((current-checkpoint (car (s-v '%checkpoints))))
;;       (when current-checkpoint
;; 	(if (listp current-checkpoint)
;; 	    (setq current-checkpoint (car current-checkpoint)))	
;; 	(setf (s-v '%buffer)
;; 	      (make-array (- (length (s-v '%buffer)) current-checkpoint)
;; 			  :adjustable t
;; 			  :displaced-to (s-v '%buffer)
;; 			  :displaced-index-offset current-checkpoint
;; 			  :fill-pointer 0))))))

;; (defmethod rewind-stream ((self core-vector-stream) &optional (checkpoint 0))
;;   (let* ((checkpoint-index (checkpoint-index self (find-checkpoint self checkpoint)))
;; 	 (offset (- (length (s-v '%buffer)) checkpoint-index)))
;;     (decf (s-v '%index) offset)
;;     (do ((i (pop-atom (s-v '%checkpoints)) (pop-atom (s-v '%checkpoints))))
;; 	((equal i checkpoint-index)))))
