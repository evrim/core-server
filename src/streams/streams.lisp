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
;;(declaim (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))

;;;-----------------------------------------------------------------------------
;;; IMPLEMENTATION OF SEVERAL BOGUS TURING MACHINES (Left is broken)
;;;-----------------------------------------------------------------------------

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
     (reduce #'(lambda (acc atom)
		 (declare (ignore acc))
		 (vector-push-extend atom accumulator)
		 nil)
	     (string-to-octets (string atom) :utf8) :initial-value nil))
    ((and (typep atom '(unsigned-byte 8)) (eq 'character (array-element-type accumulator)))
     (vector-push-extend (code-char atom) accumulator))
    (t
     (vector-push-extend atom accumulator))))

(defun pop-atom (accumulator)
  (vector-pop accumulator))

;;;-----------------------------------------------------------------------------
;;; Stream Definitions
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defclass core-stream ()
    ((%checkpoints :initform '()) ;; checkpoints list as (index #(buffer))
     (%current :initform -1 :type fixnum) ;; current checkpoint
     (%max-read :initform nil))))

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

(defgeneric return-stream (core-stream)
  (:documentation "stagnate data out of the stream"))

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

(defmethod close-stream :around ((self core-stream))
  (when (transactionalp self)
    (do ((i (current-checkpoint self) (current-checkpoint self)))
	((= -1 i) nil)
      (commit-stream self)))
  (call-next-method))

(defmethod write-stream ((self core-stream) (c null))
  self)

;;;-----------------------------------------------------------------------------
;;; Standard Output Workarounds
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defclass core-standard-output (core-stream)
    ())

  (defmethod write-stream ((self core-standard-output) (vector vector))
    (reduce #'write-stream vector :initial-value self))
  
  (defmethod write-stream ((self core-standard-output) atom)
    (prog1 self (princ (code-char atom) *standard-output*)))

  (defmethod write-stream ((self core-standard-output) (a character))
    (prog1 self (write-stream self (char-code a))))

  (defmethod write-stream ((self core-standard-output) (a null))
    nil)
  
  (defvar *core-output* (make-instance 'core-standard-output)))

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

(defmethod return-stream ((self core-vector-io-stream))  
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

(defmethod write-stream ((self core-vector-io-stream) (val null))
  self)

;; SERVER> (time 
;; 	 (let ((a (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
;; 	  (loop for i from 0 upto 10000000
;; 	       do (vector-push-extend #\A a))))
;; Evaluation took:
;;   0.667 seconds of real time
;;   0.620038 seconds of user run time
;;   0.052003 seconds of system run time
;;   [Run times include 0.056 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   268,434,512 bytes consed.
;; NIL
;; SERVER> (time 
;; 	 (let ((a (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
;; 	   (adjust-array a 10000000)
;; 	   (loop for i from 0 upto 10000000
;; 	       do (vector-push #\A a))))
;; Evaluation took:
;;   0.304 seconds of real time
;;   0.288018 seconds of user run time
;;   0.016001 seconds of system run time
;;   0 calls to %EVAL
;;   0 page faults and
;;   80,000,032 bytes consed.
;; NIL
(defmethod write-stream ((self core-vector-io-stream) (vector vector))  
  (prog1 self (reduce #'write-stream vector :initial-value self)))

(defmethod write-stream ((self core-vector-io-stream) atom)
  (if (transactionalp self)
      (push-atom atom (s-v '%buffer))
      (if (> (length (the vector (s-v '%octets))) (the fixnum (s-v '%index)))
	  (setf (aref (the (vector (unsigned-byte 8)) (s-v '%octets))
		      (s-v '%index)) atom)
	  (push-atom atom (s-v '%octets))))
  
  (incf (the fixnum (s-v '%index))) ;; paradoxal
  self)

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

(defmethod return-stream ((self core-string-io-stream))
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
     (length (s-v '%read-buffer))))

(defmethod %peek-stream ((self core-fd-io-stream))
  (flet ((peek ()
	   (let ((peeked (read-byte (s-v '%stream) nil nil)))	
	     (setf (s-v '%peek) (or peeked -1))
	     peeked)))
    (if (< (the fixnum (s-v '%peek)) 0)
	(cond
	  ((and (s-v '%max-read) (> (the fixnum (s-v '%max-read)) 0))
	   (prog1 (peek) (decf (s-v '%max-read))))
	  ((s-v '%max-read) nil)	
	  (t (peek)))      
	(s-v '%peek))))

(defmethod %read-stream ((self core-fd-io-stream))
  (let ((peeked (%peek-stream self)))
    (if peeked
	(setf (s-v '%peek) -1))
    peeked))

(defmethod peek-stream ((self core-fd-io-stream))
  (if (stream-using-cache? self)
      (aref (s-v '%read-buffer)
	    (the fixnum (s-v '%read-index)))
      (%peek-stream self)))

(defmethod read-stream ((self core-fd-io-stream))
  (if (stream-using-cache? self)
      (prog1 (aref (s-v '%read-buffer) (s-v '%read-index))
	(incf (the fixnum (s-v '%read-index)))
	(when (not (transactionalp self))
	  (if (= (s-v '%read-index) (length (s-v '%read-buffer)))
	      (setf (s-v '%read-index) 0
		    (s-v '%read-buffer) (make-accumulator :byte)))))
      (let ((read (%read-stream self)))
	(when read
	  (when (transactionalp self)	      
	    (push-atom read (s-v '%read-buffer))
	    (incf (the fixnum (s-v '%read-index))))
	  read))))

(defmethod write-stream ((self core-fd-io-stream) (c (eql nil)))
  self)

(defmethod write-stream ((self core-fd-io-stream) (vector vector))
  (prog1 self
    (if (transactionalp self)
	(reduce #'write-stream vector :initial-value self)		
	(write-sequence vector (s-v '%stream)))))

(defmethod write-stream ((self core-fd-io-stream) atom)
  (prog1 self
    (if (transactionalp self)
	(push-atom atom (s-v '%write-buffer))
	(write-byte atom (s-v '%stream))	
	;;	(sb-impl::flush-output-buffer (s-v '%stream))
	))
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
      (write-stream self buffer)
      (if (not (transactionalp self))
	  (finish-output (s-v '%stream))))))

;;;-----------------------------------------------------------------------------
;;; File Stream
;;;-----------------------------------------------------------------------------
(defclass core-file-io-stream (core-fd-io-stream)
  ())

(defmethod shared-initialize :after ((self core-file-io-stream) slot-name
				     &rest initargs &key &allow-other-keys)
  (when (getf initargs :file)    
    (setf (s-v '%stream)
	  (open (getf initargs :file)
		:direction :io
		:element-type '(unsigned-byte 8)
		:if-exists :supersede
		:if-does-not-exist :create
		:external-format :utf8))))

(defclass core-file-input-stream (core-fd-io-stream)
  ())

(defmethod shared-initialize :after ((self core-file-input-stream) slot-names
				     &rest initargs &key &allow-other-keys)
  
  (when (getf initargs :file)
    (setf (s-v '%stream)
	  (open (getf initargs :file)
		:direction :input
		:element-type '(unsigned-byte 8)
		:if-does-not-exist :error
		:external-format :utf8))))

(defun make-core-file-input-stream (pathname)
  (make-instance 'core-file-input-stream :file pathname))

(defclass core-file-output-stream (core-fd-io-stream)
  ())

(defmethod shared-initialize :after ((self core-file-output-stream) slot-names
				     &rest initargs &key &allow-other-keys)
  (when (getf initargs :file)
    (setf (s-v '%stream)
	  (open (getf initargs :file)
		:direction :output
		:element-type '(unsigned-byte 8)
		:if-does-not-exist :create
		:if-exists :supersede
		:external-format :utf8))))

(defun make-core-file-output-stream (pathname)
  (make-instance 'core-file-output-stream :file pathname))

;;;-----------------------------------------------------------------------------
;;; List Stream
;;;-----------------------------------------------------------------------------
(defclass core-list-io-stream (core-stream)
  ((%list :initarg :list :type list :initform nil) ;; underlying list
   (%index :initform 0 :type fixnum) ;; current index
   (%buffer :initform '()))) ;; checkpoint buffer

(defmethod return-stream ((self core-list-io-stream))
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

(defmethod write-stream ((self core-list-io-stream) (c (eql nil)))
  self)

(defmethod write-stream ((self core-list-io-stream) (list list))
  (if (transactionalp self)
      (setf (s-v '%buffer) (append (s-v '%buffer) (list list)))
      (if (> (length (s-v '%list)) (s-v '%index))
	  (setf (nth (the fixnum (s-v '%index)) (s-v '%list)) list)
	  (setf (s-v '%list) (append (s-v '%list) (list list)))))
  
  (incf (the fixnum (s-v '%index))) ;; paradoxal
  self)

(defmethod write-stream ((self core-list-io-stream) atom)
  (if (transactionalp self)
      (setf (s-v '%buffer)
	    (nreverse (cons atom
			    (nreverse (s-v '%buffer)))))
      (if (> (length (s-v '%list)) (s-v '%index))
	  (setf (nth (the fixnum (s-v '%index)) (s-v '%list)) atom)
	  (setf (s-v '%list) (nreverse
			      (cons atom
				    (nreverse (s-v '%list)))))))
  
  (incf (the fixnum (s-v '%index))) ;; paradoxal
  self)

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
	      (let ((name (mopp::slot-definition-name item)))
		(if (slot-boundp object name)
		    (append acc (list (cons name (slot-value object name))))
		    acc)))
	  (mopp:class-slots (class-of object))
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
		(let ((name (mopp:slot-definition-name slot)))
		  (setf (slot-value o name) (cdr (assoc name list))))
		nil)
	    (mopp:class-slots (find-class clazz))
	    :initial-value nil)
    o))

(defmethod return-stream ((self core-object-io-stream))
  (list-to-object (s-v '%list) (s-v '%clazz)))

;; (defmacro make-core-stream (target &rest args)
;;   `(etypecase ,target
;;      (string (make-instance 'core-string-io-stream :string ,target ,@args))
;;      (pathname (make-instance 'core-file-io-stream :file ,target))
;;      (array (make-instance 'core-vector-io-stream :octets ,target))
;;      (sb-sys::fd-stream (make-instance 'core-fd-io-stream :stream ,target))
;;      (list (make-instance 'core-list-io-stream :list ,target))
;;      (standard-object (make-instance 'core-object-io-stream :object ,target))))

(defun make-core-stream (target)
  (etypecase target
    (string (make-instance 'core-string-io-stream :string target))
    (pathname (make-instance 'core-file-input-stream :file target))
    (array (make-instance 'core-vector-io-stream :octets target))
    (sb-sys::fd-stream (make-instance 'core-fd-io-stream :stream target))
    (list (make-instance 'core-list-io-stream :list target))
    (standard-object (make-instance 'core-object-io-stream :object target))))

;; HELPERS

;; (with-core-stream (s "abc")
;;   (read-stream s))
(defmacro with-core-stream ((var val) &body body)
  `(let ((,var (make-core-stream ,val)))
     (unwind-protect
	  (progn ,@body)
       (close-stream ,var))))

;;;-----------------------------------------------------------------------------
;;; Indent Stream Mixin
;;;-----------------------------------------------------------------------------
;;; This is used to indent outputs like javascript render.
;;;
(defclass core-stream-indent-mixin ()
  ((%indent :initform 0 :initarg :indent)
   (%increment :initform 2 :initarg :increment)))

(defmethod increase-indent ((self core-stream) &optional n)
  self)

(defmethod increase-indent ((self core-stream-indent-mixin) &optional (n nil))  
  (let ((n (or n (s-v '%increment))))
    (aif (< (incf (s-v '%indent) n) 0)
	 (setf (s-v '%indent) 0)
	 it)
    self))

(defmethod decrease-indent ((self core-stream) &optional n)
  self)

(defmethod decrease-indent ((self core-stream-indent-mixin) &optional (n nil))
  (let ((n (or n (s-v '%increment))))  
    (aif (< (decf (s-v '%indent) n) 0)
	 (setf (s-v '%indent) 0)
	 it)
    self))

(defmethod write-stream ((self core-stream-indent-mixin) atom)
  (prog1 (call-next-method self atom)
    (if (or (eq #.(char-code #\Newline) atom) (eq #\Newline atom))
	(dotimes (i (s-v '%indent))
	  (call-next-method self #.(char-code #\Space))))))

(defclass core-indented-string-stream (core-stream-indent-mixin core-string-io-stream)
  ())

(defclass core-indented-file-io-stream (core-stream-indent-mixin core-string-io-stream)
  ())

(defun make-indented-stream (target &optional (increment 0) (initial 0))
  (etypecase target
    (string (make-instance 'core-indented-string-stream :string target
			   :indent initial :increment increment))
    (sb-sys::fd-stream (make-instance 'core-indented-fd-io-stream :stream target
				      ::indent initial :increment increment))))

;;;-----------------------------------------------------------------------------
;;; Compressed Stream Mixin
;;;-----------------------------------------------------------------------------
;;; This is used to compressed outputs like javascript render.
;;;
(defclass core-stream-compress-mixin ()
  ())

(defmethod write-stream ((self core-stream-compress-mixin) atom)  
  (if (or (eq #.(char-code #\Newline) atom) (eq #\Newline atom))
      self
      (call-next-method self atom)))

(defclass core-compressed-string-stream (core-stream-compress-mixin core-string-io-stream)
  ())

(defclass core-compressed-file-io-stream (core-stream-compress-mixin core-string-io-stream)
  ())

(defun make-compressed-stream (target)
  (etypecase target
    (string (make-instance 'core-compressed-string-stream :string target))
    (sb-sys::fd-stream (make-instance 'core-compressed-fd-io-stream :stream target))))

(defclass pipe-stream (core-stream)
  ((%input :accessor pipe-stream.input
	   :initform (error "Please specify \":input\" stream")
	   :initarg :input :type core-stream)
   (%output :accessor pipe-stream.output
	    :initform (error "Please specify \":output\" stream ")
	    :initarg :output :type core-stream)))

(defmethod peek-stream ((self pipe-stream))
  (peek-stream (s-v '%input)))

(defmethod read-stream ((self pipe-stream))
  (read-stream (s-v '%input)))

(defmethod write-stream ((self pipe-stream) c)
  (write-stream (s-v '%output) c))

(defmethod checkpoint-stream ((self pipe-stream))
  (if (transactionalp self)
      (push (list (s-v '%current)) (s-v '%checkpoints)))
  (checkpoint-stream (s-v '%input))
  (checkpoint-stream (s-v '%output))
  (setf (s-v '%current) (length (s-v '%checkpoints)))
  (length (s-v '%checkpoints)))

(defmethod rewind-stream ((self pipe-stream))
  (prog1 (length (s-v '%checkpoints))
    (setf (s-v '%current)
	  (or (pop (s-v '%checkpoints))
	      -1))
    (rewind-stream (s-v '%input))
    (rewind-stream (s-v '%output))))

(defmethod commit-stream ((self pipe-stream))
  (prog1 (length (s-v '%checkpoints))
    (setf (s-v '%current) (or (pop (s-v '%checkpoints))
			      -1))
    (commit-stream (s-v '%input))
    (commit-stream (s-v '%output))))

(defmethod close-stream ((self pipe-stream))
  (close-stream (s-v '%output))
  (close-stream (s-v '%input)))

(defmethod return-stream ((self pipe-stream))
  (return-stream (s-v '%output)))

(defun make-pipe-stream (input output)
  (make-instance 'pipe-stream :input input :output output))

(defclass core-transformer-stream (local-unit pipe-stream)
  ((%decoder :accessor transformer.decoder :initarg :decoder
	     :initform (error "Please specify \":decoder\" function"))
   (%encoder :accessor transformer.encoder :initarg :encoder :initform nil)
   (%read-k :initform nil)
   (%write-k :initform nil))
  (:default-initargs :auto-start t :name "Transformer Stream"))

(defmethod/unit read-stream ((self core-transformer-stream))  
  (format t "read-stream transformer, thread:~A~%" (current-thread))
  (cond
    ((s-v '%write-k)
     (format t "gee:~A~%" (s-v '%write-k))
     (let ((write-k (s-v '%write-k)))
       (setf (s-v '%write-k) nil
	     (s-v '%read-k) +ret+)
       (funcall write-k t)))
    ((null (s-v '%read-k))
     (setf (s-v '%read-k) +ret+)
     (format t "funku dekoder ret:~A~%" +ret+)
     (prog1 (funcall (s-v '%decoder) self)
       (setf (s-v '%read-k) nil
	     (s-v '%write-k) nil)))
    (t
     (format t "funku default~%")
     (read-stream (s-v '%input)))))

(defmethod/unit write-stream ((self core-transformer-stream) c)
  (flet ((ret (val)
	   (format t "inside write-stream ret val:~A~%" val)
	   (return-from write-stream val)))
    (format t "write-stream transformer, thread:~A~%" (current-thread))
    (cond
      ((s-v '%read-k)	 
       (let ((read-k (s-v '%read-k)))
	 (setf (s-v '%write-k) #'ret)
	 (setf (s-v '%read-k) nil)
	 (funcall read-k c)))
      (t
       (write-stream (s-v '%output) c)))))

(defun make-transformer-stream (decoder input
				&optional (output (make-instance 'core-list-io-stream)))
  (make-instance 'core-transformer-stream :input input :output output
		 :decoder decoder))

(defclass core-cps-stream (core-stream)
  ((%k :initform nil)
   (%continuations :initform '())))

(defmethod copy-core-stream ((self core-cps-stream))
  (let ((s (make-instance 'core-cps-stream)))
    (setf (slot-value s '%k) (s-v '%k)
	  (slot-value s '%continuations) (copy-list (s-v '%continuations)))
    s))

(defmethod transactionalp ((self core-cps-stream))
  (not (null (s-v '%k))))

(defmethod current-k ((self core-cps-stream))
  (s-v '%k))

(defmethod/cc checkpoint-stream/cc ((self core-cps-stream) k)
  (if (transactionalp self)
      (push (s-v '%k) (s-v '%continuations)))

  (setf (s-v '%k) (lambda (&optional values)
		    (setf (s-v '%k) (pop (s-v '%continuations)))
		    (kall k values)))
  (format t "checkpoint:~D~%" (length (s-v '%continuations)))
  (length (s-v '%continuations)))

(defmethod/cc rewind-stream/cc ((self core-cps-stream) &optional values)
  (if (not (transactionalp self))
      -1
      (apply (s-v '%k) (ensure-list values))))

;; (defmethod/cc rewind-stream/cc ((self core-cps-stream) &optional values)
;;   (break "rewinding~%")
;;   (setf *konts* (s-v '%continuations))
;;   (if (not (transactionalp self))
;;       -1
;;       (progn
;; 	(format t "rewind:~D~%" (1- (length (s-v '%continuations))))
;; 	(acond	 
;; 	 ((pop (s-v '%continuations))
;;  	  (let ((current (s-v '%k)))
;;  	    (setf (s-v '%k) it)
;; 	    (format t "kalling:~A, k is :~A" current (s-v '%k))
;;  	    (apply #'kall current values)))
;; 	 ((s-v '%k)
;; 	  (setf (s-v '%k) nil)
;; 	  (format t "K is :~A" (s-v '%k))
;; 	  (apply #'kall it values))
;; 	 (t
;; 	  (break "y00"))
;; 	 ))
;;       ))

(defun escape (&rest values)
  (funcall (apply (arnesi::toplevel-k) values))
  (break "You should not see this, call/cc must be escaped already."))

(defmethod/cc commit-stream/cc ((self core-cps-stream) &optional value)
  (describe self)
  (if (not (transactionalp self))
      -1
      (progn
	(format t "commit:~D~%" (length (s-v '%continuations)))
	;;	(setf (s-v '%k) (pop (s-v '%continuations)))
	(escape value))))

(defclass core-cps-io-stream (core-cps-stream core-vector-io-stream)
  ())

;; (defmethod/cc checkpoint-stream/cc ((self core-cps-stream))
;;   (let/cc k
;;     (if (transactionalp self)
;; 	(push k (s-v '%continuations)))
    
;;     (setf (s-v '%k) k)
;;     (kall k (checkpoint-stream self))))

;; (defmethod/cc rewind-stream/cc ((self core-cps-stream))
;;   (if (not (transactionalp self))
;;       -1
;;       (progn
;; 	(acond
;; 	 ((pop (s-v '%continuations))
;; 	  (setf (s-v '%k) it)	  
;; 	  (kall it (rewind-stream self)))
;; 	 ((s-v '%k)
;; 	  (setf (s-v '%k) nil)	  
;; 	  (kall it (rewind-stream self)))))))

;; (defmethod/cc commit-stream/cc ((self core-cps-stream))
;;   (if (not (transactionalp self))
;;       -1
;;       (prog1 (commit-stream self)
;; 	(setf (s-v '%k) (pop (s-v '%continuations))))))

(defclass core-cps-string-io-stream (core-string-io-stream core-cps-stream)
  ())

(defclass core-cps-fd-io-stream (core-cps-stream core-fd-io-stream)
  ())

(defclass core-cps-file-io-stream (core-cps-stream core-file-io-stream)
  ())

(defclass core-cps-list-io-stream (core-cps-stream core-list-io-stream)
  ())

(defclass core-cps-object-io-stream (core-cps-stream core-object-io-stream)
  ())

(defun make-core-cps-stream (target &rest args)
  (apply #'make-instance
	 (append
	  (etypecase target
	    (string (list 'core-cps-string-io-stream :string target))
	    (pathname (list 'core-cps-file-io-stream :file target))
	    (array (list 'core-cps-vector-io-stream :octets target))
	    (sb-sys::fd-stream (list 'core-cps-fd-io-stream :stream target))
	    (list (list 'core-cps-list-io-stream :list target))
	    (standard-object (list 'core-cps-object-io-stream :object target)))
	  args)))

(deftrace streams
    '(read-stream write-stream close-stream)) ;;current-checkpoint

(deftrace streams-tx
    '(peek-stream checkpoint-stream rewind-stream commit-stream close-stream
      current-checkpoint))
