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

;;;-------------------------------------------------------------------------
;;; IMPLEMENTATION OF SEVERAL BOGUS TURING MACHINES (Left is broken)
;;;-------------------------------------------------------------------------

;;;-------------------------------------------------------------------------
;;; Simple Accumulator
;;;--------------------------------------------------------------------------
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
    ((and (characterp atom)
	  (not (eq 'character (array-element-type accumulator))))
     (reduce #'(lambda (acc atom)
		 (declare (ignore acc))
		 (vector-push-extend atom accumulator)
		 nil)
	     (string-to-octets (string atom) :utf8) :initial-value nil))
    ((and (typep atom '(unsigned-byte 8))
	  (eq 'character (array-element-type accumulator)))
     (vector-push-extend (code-char atom) accumulator))
    (t
     (vector-push-extend atom accumulator))))

(defun pop-atom (accumulator)
  (vector-pop accumulator))

;;;-------------------------------------------------------------------------
;;; Stream Definitions
;;;-------------------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defclass core-stream ()
    ((%checkpoints :initform '()) ;; checkpoints list as (index #(buffer))
     (%current :initform -1 :type fixnum)))) ;; current checkpoint

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

(defgeneric transactionalp (core-stream)
  (:documentation "transactional predicate"))

(defgeneric current-checkpoint (core-stream)
  (:documentation "id for current checkpoint"))

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
      (call-next-method self)))

(defmethod commit-stream :around ((self core-stream))
  (if (not (transactionalp self))
      -1
      (call-next-method self)))

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

  (defmethod rewind-stream ((self core-standard-output))
    nil)

  (defmethod checkpoint-stream ((self core-standard-output))
    nil)
  
  (defmethod commit-stream ((self core-standard-output))
    nil)

  (defmethod write-stream ((self core-standard-output) (vector vector))
    (reduce #'write-stream vector :initial-value self))
  
  (defmethod write-stream ((self core-standard-output) atom)
    (prog1 self (princ (code-char atom) *standard-output*)))

  (defmethod write-stream ((self core-standard-output) (a character))
    (prog1 self (write-stream self (char-code a))))

  (defmethod write-stream ((self core-standard-output) (a null))
    self)
  
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

(defmethod write-stream ((self core-vector-io-stream) (vector vector))  
  (prog1 self      
    (reduce #'write-stream vector :initial-value self)))

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
   (%read-buffer :initform (make-accumulator :byte)
		 :type (vector (unsigned-byte 8)))
   (%read-index :initform 0 :type fixnum)
   (%write-buffer :initform (make-accumulator :byte)
		  :type (vector (unsigned-byte 8)))
   (%transactional :initform nil :reader transactionalp)))

;; (defmethod transactionalp ((self core-fd-io-stream))
;;   (s-v '%transactional))

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
	(peek)
	(s-v '%peek))))

(defmethod %read-stream ((self core-fd-io-stream))
  (let ((peeked (%peek-stream self)))
    (if peeked
	(setf (s-v '%peek) -1))
    peeked))

(defmethod peek-stream ((self core-fd-io-stream))
  (if (stream-using-cache? self)
      (aref (s-v '%read-buffer) (the fixnum (s-v '%read-index)))
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
	(progn
	  (if (null (s-v '%write-buffer))
	      (setf (s-v '%write-buffer) (make-accumulator :byte)))
	  (push-atom atom (s-v '%write-buffer)))
	(write-byte atom (s-v '%stream))	
	;;	(sb-impl::flush-output-buffer (s-v '%stream))
	))
  ;;  (incf (s-v '%read-index)) ;; paradoxal
  )

(defmethod write-stream ((self core-fd-io-stream) (atom character))
  (write-stream self (char-code atom)))

(defmethod checkpoint-stream ((self core-fd-io-stream))
  (if (transactionalp self)
      (push (cons (s-v '%current) (s-v '%write-buffer))
	    (s-v '%checkpoints)))

  (setf (s-v '%current) (s-v '%read-index)
	(s-v '%write-buffer) nil ;; (make-accumulator :byte)
	(s-v '%transactional) t)
  (length (s-v '%checkpoints)))

(defmethod %rewind-checkpoint ((self core-fd-io-stream))
  (prog1 (length (s-v '%checkpoints))
    (let ((previous-checkpoint (pop (s-v '%checkpoints))))
      (if previous-checkpoint
	  (setf (s-v '%current) (car previous-checkpoint)
		(s-v '%write-buffer) (cdr previous-checkpoint))
	  (setf (s-v '%current) -1
		(s-v '%write-buffer) nil ;; (make-accumulator :byte)
		(s-v '%transactional) nil)))))

(defmethod rewind-stream ((self core-fd-io-stream))
  (setf (s-v '%read-index) (s-v '%current))
  (%rewind-checkpoint self))

(defmethod commit-stream ((self core-fd-io-stream))
  (let ((buffer (s-v '%write-buffer)))
    (prog1 (%rewind-checkpoint self) 
      (write-stream self buffer)
      (if (not (transactionalp self))
	  (finish-output (s-v '%stream))))))

;;;-------------------------------------------------------------------------
;;; File Stream
;;;-------------------------------------------------------------------------
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

(defun make-core-file-io-stream (file)
  (make-instance 'core-file-io-stream :file file))

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
  ((%clazz :accessor clazz :initarg :clazz :initform nil))) ;; underlyng object clazz

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

(defun make-core-stream (target)
  (etypecase target
    (string (make-instance 'core-string-io-stream :string target))
    (pathname (make-instance 'core-file-input-stream :file target))
    (array (make-instance 'core-vector-io-stream :octets target))
    (sb-sys::fd-stream (make-instance 'core-fd-io-stream :stream target))
#+ssl (cl+ssl::ssl-stream (make-instance 'core-fd-io-stream :stream target)) 
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

(defmacro with-core-stream/cc ((var val) &body body)
  `(let ((,var (make-core-stream ,val)))
     (prog1 (progn ,@body)
       (close-stream ,var))))

;; +------------------------------------------------------------------------
;; | Semi Stream
;; +------------------------------------------------------------------------
(defclass wrapping-stream (core-stream)
  ((%stream :initform (error "Provide :stream") :initarg :stream)))

(defmethod transactionalp ((self wrapping-stream))
  (transactionalp (s-v '%stream)))

(defmethod current-checkpoint ((self wrapping-stream))
  (current-checkpoint (s-v '%stream)))

(defmethod close-stream ((stream wrapping-stream))
  (close-stream (slot-value stream '%stream)))

(defmethod checkpoint-stream ((stream wrapping-stream))
  (checkpoint-stream (slot-value stream '%stream)))

(defmethod rewind-stream ((stream wrapping-stream))
  (rewind-stream (slot-value stream '%stream)))

(defmethod commit-stream ((stream wrapping-stream))
  (commit-stream (slot-value stream '%stream)))

(defmethod read-stream ((stream wrapping-stream))
  (error "Could not read from ~A" stream))

(defmethod write-stream ((stream wrapping-stream) object)
  (error "Could not write ~A to ~A" object stream))

(defmethod write-stream ((stream wrapping-stream) (object function))
  (prog1 stream (funcall object (slot-value stream '%stream))))

(defmethod write-stream ((stream wrapping-stream) (object null))
  stream)

(defmethod return-stream ((stream wrapping-stream))
  (return-stream (slot-value stream '%stream)))

;; -------------------------------------------------------------------------
;; Bounded Stream - A Maximum Read Length Bounded Stream
;; -------------------------------------------------------------------------
(defclass bounded-stream (wrapping-stream)
  ((%max-read :initarg :max-read :initform 0)
   (%max-read-checkpoints :initform '())))

(defmethod read-allowed-p ((self bounded-stream))
  (and (> (s-v '%max-read) 0) t))

(defmethod peek-stream ((self bounded-stream))
  (if (read-allowed-p self)
      (peek-stream (s-v '%stream))))

(defmethod read-stream ((self bounded-stream))
  (if (read-allowed-p self)
      (prog1 (read-stream (s-v '%stream))
	(decf (s-v '%max-read)))))

(defmethod checkpoint-stream ((self bounded-stream))
  (push (s-v '%max-read) (s-v '%max-read-checkpoints))
  (call-next-method self))

(defmethod rewind-stream ((self bounded-stream))
  (if (transactionalp self)
      (setf (s-v '%max-read) (pop (S-v '%max-read-checkpoints))))
  (call-next-method self))

(defmethod commit-stream ((self bounded-stream))
  (if (transactionalp self)
      (pop (s-v '%max-read-checkpoints)))
  (call-next-method self))

(defun make-bounded-stream (stream max-read)
  (make-instance 'bounded-stream :stream stream :max-read max-read))

;;;-------------------------------------------------------------------------
;;; Core Pipe Stream
;;; ------------------------------------------------------------------------
(defclass pipe-stream (core-stream)
  ((%input :accessor pipe-stream.input :initarg :input :type core-stream
	   :initform (error "Provide :input stream"))
   (%output :accessor pipe-stream.output :initarg :output :type core-stream
	    :initform (error "Provide :output stream "))))

(defmethod transactionalp ((self pipe-stream))
  (transactionalp (s-v '%input)))

(defmethod current-checkpoint ((self pipe-stream))
  (current-checkpoint (s-v '%input)))

(defmethod peek-stream ((self pipe-stream))
  (peek-stream (s-v '%input)))

(defmethod read-stream ((self pipe-stream))
  (read-stream (s-v '%input)))

(defmethod write-stream ((self pipe-stream) c)
  (write-stream (s-v '%output) c))

(defmethod checkpoint-stream ((self pipe-stream))
  (if (transactionalp self)
      (push (s-v '%current) (s-v '%checkpoints)))
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

;; Object IO Extenstions
(defmethod clazz ((self pipe-stream))
  (clazz (s-v '%ouput)))
 
(defmethod (setf clazz) (value (self pipe-stream))
  (setf (clazz (s-v '%output)) value))

(defun make-pipe-stream (input output)
  (make-instance 'pipe-stream :input input :output output))

;;;------------------------------------------------------------------------
;;; Core Transformer Stream
;;; -----------------------------------------------------------------------
;;;
;;; Only output transforming is tested.
;;;
(defclass core-transformer-stream (core-stream)
  ((%output :initform (error "Please specify output stream")
	    :initarg :output :type (or null core-stream))
   (%encoder :initarg :encoder :initform
	     (error "Please specify encoder lambda"))))

;; (defmethod peek-stream ((self core-transformer-stream))
;;   (peek-stream (s-v '%input)))

;; (defmethod read-stream ((self core-transformer-stream))
;;   (read-stream (s-v '%input)))

;; (defmethod write-stream ((self core-transformer-stream) c)
;;   (write-stream (s-v '%output) c))

(defmethod checkpoint-stream ((self core-transformer-stream))
  (if (transactionalp self)
      (push (s-v '%current) (s-v '%checkpoints)))
;;  (checkpoint-stream (s-v '%input))
  (checkpoint-stream (s-v '%output))
  (setf (s-v '%current) (length (s-v '%checkpoints)))
  (length (s-v '%checkpoints)))

(defmethod rewind-stream ((self core-transformer-stream))
  (prog1 (length (s-v '%checkpoints))
    (setf (s-v '%current)
	  (or (pop (s-v '%checkpoints))
	      -1))
;;    (rewind-stream (s-v '%input))
    (rewind-stream (s-v '%output))))

(defmethod commit-stream ((self core-transformer-stream))
  (prog1 (length (s-v '%checkpoints))
    (setf (s-v '%current) (or (pop (s-v '%checkpoints))
			      -1))
;;    (commit-stream (s-v '%input))
    (commit-stream (s-v '%output))))

(defmethod close-stream ((self core-transformer-stream))
  (close-stream (s-v '%output))
;;  (close-stream (s-v '%input))
  )

(defmethod return-stream ((self core-transformer-stream))
  (return-stream (s-v '%output)))

(defmethod write-stream ((self core-transformer-stream) (vector vector))
  (prog1 self (reduce #'write-stream vector :initial-value self)))

(defmethod write-stream ((self core-transformer-stream) atom)
  (prog1 self
    (if (s-v '%encoder)
	(funcall (s-v '%encoder) (s-v '%output) atom)
	(call-next-method))))

(defun make-output-transformer (output encoder)
  (make-instance 'core-transformer-stream :output output :encoder encoder))


;; +-------------------------------------------------------------------------
;; | Http Chunck Dencoding Stream (Transfer-Encoding: chunked)
;; +-------------------------------------------------------------------------
(defclass core-chunked-stream (bounded-stream)
  ()
  (:default-initargs :max-read 0))

(defmethod chunk-readable-p ((self core-chunked-stream))
  (let ((max-read (s-v '%max-read)))
    (and max-read (> max-read 0) t)))

(defmethod chunk-setup ((self core-chunked-stream))
  (let* ((s (s-v '%stream))
	 (max-read (s-v '%max-read)))
    (setf (s-v '%max-read) nil)
    (crlf? s)
    (let ((num (hex-value*? s)))
      (if (and num (> num 0))
	  (prog1 t
	    (crlf? s)
	    (setf (s-v '%max-read) num))
	  (prog1 nil
	    (setf (s-v '%max-read) max-read))))))

(defmethod peek-stream ((self core-chunked-stream))
  (if (chunk-readable-p self)
      (call-next-method self)
      (if (chunk-setup self)
  	  (peek-stream self))))

(defmethod read-stream ((self core-chunked-stream))
  (if (chunk-readable-p self)
      (call-next-method self)
      (if (chunk-setup self)
  	  (read-stream self))))

(defun make-chunked-stream (input)
  (make-instance 'core-chunked-stream :stream input))

;;;-------------------------------------------------------------------------
;;; Core Indented Stream
;;; ------------------------------------------------------------------------
;;; This is used to indent outputs like javascript render. Writes
;;; %indent spaces after a #\Newline is written.
;;;
(defclass core-indented-stream (core-transformer-stream)
  ((%indent :initform 0 :initarg :indent)
   (%increment :initform 2 :initarg :increment)))

(defmethod increase-indent ((self core-stream) &optional n)
  (declare (ignore n))
  self)

(defmethod increase-indent ((self core-indented-stream) &optional (n nil))  
  (let ((n (or n (s-v '%increment))))
    (aif (< (incf (s-v '%indent) n) 0)
	 (setf (s-v '%indent) 0)
	 it)
    self))

(defmethod decrease-indent ((self core-stream) &optional n)
  (declare (ignore n))
  self)

(defmethod decrease-indent ((self core-indented-stream) &optional (n nil))
  (let ((n (or n (s-v '%increment))))  
    (aif (< (decf (s-v '%indent) n) 0)
	 (setf (s-v '%indent) 0)
	 it)
    self))

(defmethod write-stream ((self core-indented-stream) (vector vector))
  (prog1 self (reduce #'write-stream vector :initial-value self)))

(defmethod write-stream ((self core-indented-stream) atom)
  (prog1 self (funcall (s-v '%encoder) (s-v '%output) atom (s-v '%indent))))

(defun make-indented-stream (output &optional (increment 2) (initial 0))
  (make-instance 'core-indented-stream
		 :output output
		 :increment increment
		 :indent initial
		 :encoder #'(lambda (stream atom indent)
			      (write-stream stream atom)
			      (if (or (eq #.(char-code #\Newline) atom) (eq #\Newline atom))
				  (dotimes (i indent)
				    (write-stream stream #.(char-code #\Space)))))))

;;;-------------------------------------------------------------------------
;;; Core Compressed Stream
;;;-------------------------------------------------------------------------
;;; This is used to compressed outputs like javascript render. Eats all #\Newline
;;;
(defclass core-compressed-stream (core-transformer-stream)
  ())

(defun make-compressed-stream (output)
  (make-instance 'core-compressed-stream
		 :output output
		 :encoder #'(lambda (stream atom)
			      (if (or (eq #.(char-code #\Newline) atom)
				      (eq #\Newline atom))
				  stream
				  (write-stream stream atom)))))

;;;-----------------------------------------------------------------------------
;;; Core CPS Stream
;;;-----------------------------------------------------------------------------
(defclass core-cps-stream (core-stream)
  ((%stack :initarg :stack :initform (error "Stack should not be nil"))))

(defmethod read-stream ((self core-cps-stream))
  (pop (s-v '%stack)))

(defmethod write-stream ((self core-cps-stream) value)
  (push value (s-v '%stack)))

(defmethod checkpoint-stream2 ((self core-cps-stream) name)
  (push (cons name (s-v '%stack)) (s-v '%checkpoints))
  (setf (s-v '%stack) nil)
  name)

(defmethod rewind-stream2 ((self core-cps-stream) name)
  (do ((checkpoint (pop (s-v '%checkpoints)) (pop (s-v '%checkpoints))))
      ((null checkpoint) name)
    (setf (s-v '%stack) (cdr checkpoint))
    (cond
      ((null (symbol-package name))
       (if (string= name (car checkpoint))
	   (return-from rewind-stream2 name)))
      (t
       (if (equal name (car checkpoint))
	   (return-from rewind-stream2 name))))) 
  name)

(defmethod commit-stream2 ((self core-cps-stream) name)
  (rewind-stream2 self name))

(defmethod run ((self core-cps-stream))
  (let ((count 1000000))
    (catch 'done
      ;; (let ((values '(nil))
;; 	    (+stream+ self))
;; 	(declare (special +stream+))
;; 	(do ((k (read-stream self) (read-stream self)))
;; 	    ((null k) (apply #'values values))
;; 	  (setq values (multiple-value-list (apply k (or values (list nil)))))
;; ;;	  (describe values)
;; 	  (decf count)
;; 	  (if (zerop count)
;; 	      (progn
;; 		(warn "Executing limit exceeded.")
;; 		(throw 'done (apply #'values values))))))
      (let ((value nil)
	    (+stream+ self))
	(declare (special +stream+))
	(do ((k (read-stream self) (read-stream self)))
	    ((null k) value)
	  (setq value (funcall k value))))
      )))

(defclass core-dummy-cps-stream (core-stream)
  ())

(defmethod read-stream ((self core-dummy-cps-stream))
  (error "Please call inside with-call/cc2"))

(defmethod write-stream ((self core-dummy-cps-stream) value)
  (error "Please call inside with-call/cc2"))

(defmethod checkpoint-stream2 ((self core-dummy-cps-stream) name)
  (error "Please call inside with-call/cc2"))

(defmethod commit-stream2 ((self core-dummy-cps-stream) name)
  (error "Please call-inside with-call/cc2"))

(defvar +stream+ (make-instance 'core-dummy-cps-stream)
  "Dummy CPS Stream")


;;;-----------------------------------------------------------------------------
;;; Core FD NIO Stream
;;;-----------------------------------------------------------------------------
(defclass core-fd-nio-stream (core-fd-io-stream)
  ((unit :initarg :unit :initform nil :accessor unit)
   (%buffer-size :initarg :buffer-size :initform 4096)
   (%read-buffer :initform #())))

;; (defclass core-fd-io-stream (%core-fd-stream)
;;   ((%peek :initform -1 :type fixnum)
;;    (%read-buffer :initform (make-accumulator :byte) :type (vector (unsigned-byte 8)))
;;    (%read-index :initform 0 :type fixnum)
;;    (%write-buffer :initform (make-accumulator :byte) :type (vector (unsigned-byte 8)))))

(defun zoo ()
  (let ((s (core-ffi::connect "localhost" 7000)))
    (make-instance 'core-fd-nio-stream :stream s)))

;; burda connect sbclninkini kullanio ondan scio herhalde

(defmethod shared-initialize :after ((self core-fd-nio-stream) slots &key &allow-other-keys)
  (core-ffi::set-nonblock (s-v '%stream))
  self)

(defmethod/cc1 fill-read-buffer ((self core-fd-nio-stream))      
  (let/cc1 k
    (let ((kont (lambda (&rest values)
		  (declare (ignore values))
		  (funcall k (fill-read-buffer self)))))
      (with-foreign-object (buffer :uint8 (s-v '%buffer-size))
	(core-ffi::bzero buffer (s-v '%buffer-size))
	(let* ((ret (core-ffi::%read (s-v '%stream) buffer (s-v '%buffer-size)))
	       (errno (core-ffi::errno)))
	  (cond
	    ((and (s-v 'unit) (eq -1 ret) (eq 11 errno))
	     (format *standard-output* "Got failed to read: EAGAIN")
	     ;; save k
	     (add-fd (s-v 'unit) (s-v '%stream) (list core-ffi::epollin #.(ash 1 31))
		     kont)
	     ;; escape
	     (funcall +k+ nil))
	    ((and (eq -1 ret) (not (eq 11 errno)))
	     (warn "Read returned err:~D, fd: ~D" errno (s-v '%stream))
	     (core-ffi::%close (s-v '%stream)))
	    ((not (transactionalp self))
	     (setf (s-v '%read-buffer) (cffi::foreign-array-to-lisp buffer (list :array :uint8 ret))
		   (s-v '%read-index) 0))
	    (t
	     (let ((current-length (length (s-v '%read-buffer))))
	       (let ((array (cffi::foreign-array-to-lisp buffer (list :array :uint8 ret)))
		     (new-array (adjust-array (s-v '%read-buffer) (+ ret current-length))))
		 (reduce (lambda (len atom)
			   (setf (aref new-array len) atom)
			   (1+ len))
			 array :initial-value current-length)
		 (setf (s-v '%read-buffer) new-array)))))))))
  self)

(defmethod/cc1 peek-stream ((self core-fd-nio-stream))
  (if (stream-using-cache? self)
      (if (or (null (s-v '%max-read)) (> (s-v '%max-read) 0))
	  (aref (s-v '%read-buffer) (s-v '%read-index))
	  nil)
      (progn
	(fill-read-buffer self)
	(peek-stream self))))

(defmethod/cc1 read-stream ((self core-fd-nio-stream))
  (let ((peek (peek-stream self)))
    (when peek
      (when (s-v '%max-read) (decf (s-v '%max-read)))
      (incf (s-v '%read-index)))
    peek))

(defmethod/cc1 write-stream :around ((stream core-fd-nio-stream) (atom null))
  stream)

(defmethod/cc1 write-stream ((stream core-fd-nio-stream) (atom (eql 'nil)))
  stream)

(defmethod/cc1 write-stream :around ((stream core-fd-nio-stream) (list list))
  (cond
    ((transactionalp stream)
     (setf (slot-value stream '%write-buffer)
	   (append list (slot-value stream '%write-buffer)))
     stream)
    (t	       
     (write-stream stream (car list))
     (if (cdr list)
	 (write-stream stream (cdr list))
	 stream))))

(defmethod/cc1 write-stream :around ((stream core-fd-nio-stream) atom)
  (if (transactionalp stream)
      (prog1 stream
	(setf (slot-value stream '%write-buffer)
	      (cons atom (slot-value stream '%write-buffer))))
      (call-next-method)))

(defmethod/cc1 write-stream ((self core-fd-nio-stream) (atom character))
  (write-stream self (char-code atom)))

(defmethod/cc1 write-stream ((self core-fd-nio-stream) (string string))
  (write-stream self (string-to-octets string :utf-8)))

(defmethod/cc1 write-stream ((self core-fd-nio-stream) atom)
  (prog1 self
    (let/cc1 k
      (let ((kont (lambda (&rest values)
		    (declare (ignore values))
		    (funcall k (write-stream self atom)))))
	(unwind-protect
	     (cffi::with-foreign-array (buffer (make-array 1 :initial-element atom) (list :array :uint8 1))	
	       (let* ((ret (core-ffi::%write (s-v '%stream) buffer 1))
		      (errno (core-ffi::errno)))
		 (cond
		   ((and (eq ret -1) (eq 11 errno))
		    (funcall +k+ kont))
		   ((eq ret -1)
		    (warn "%write failed with errcode:~A, fd:~D, atom:~A" errno (s-v '%stream) atom)
		    (core-ffi::%close (s-v '%stream)))
		   (t
		    self)))))))))

(defparameter *static-5k* (foreign-alloc :string :count  5192))
;; (lisp-string-to-foreign *5k* *static-5k* 5192)

(defmethod/cc1 write-content ((self core-fd-nio-stream))
  (prog1 self
    (let/cc1 k
      (let ((kont (lambda (&rest values)
		    (declare (ignore values))
		    (funcall k (write-content self)))))
	(unwind-protect	     
	     (let* ((ret (core-ffi::%write (s-v '%stream) *static-5k* 5192))
		    (errno (core-ffi::errno)))
	       (cond
		 ((and (s-v 'unit) (eq ret -1) (eq 11 errno))
		  ;; save kont	       
		  (add-fd (s-v 'unit) (s-v '%stream) (list core-ffi::epollout #.(ash 1 31))
			  kont)
		  ;; escape
		  (funcall +k+ nil))
		 ((eq ret -1)
		  (warn "%write failed with errcode:~A, fd:~D, in write-content" errno (s-v '%stream))
		  (core-ffi::%close (s-v '%stream)))
		 (t
		  self))))))))

(defmethod/cc1 write-stream ((self core-fd-nio-stream) (vector vector))
  (prog1 self
    (let/cc1 k
      (let ((kont (lambda (&rest values)
		    (declare (ignore values))
		    (funcall k (write-stream self vector)))))
	(unwind-protect
	     (cffi::with-foreign-array (buffer vector (list :array :uint8 (length vector)))
	       (let* ((ret (core-ffi::%write (s-v '%stream) buffer (length vector)))
		      (errno (core-ffi::errno)))
		 (cond
		   ((and (s-v 'unit) (eq ret -1) (eq 11 errno))
		    ;; save kont	       
		    (add-fd (s-v 'unit) (s-v '%stream) (list core-ffi::epollout #.(ash 1 31))
			    kont)
		    ;; escape
		    (funcall +k+ nil))
		   ((eq ret -1)
		    (warn "%write failed with errcode:~A, fd:~D, length vector:~D" errno (s-v '%stream) (length vector))
		    (core-ffi::%close (s-v '%stream)))
		   (t
		    self)))))))))

(defmethod/cc1 checkpoint-stream ((self core-fd-nio-stream))
  (if (transactionalp self)
      (push (list (s-v '%current) (s-v '%write-buffer))
	    (s-v '%checkpoints)))

  (setf (s-v '%current) (s-v '%read-index)
	(s-v '%write-buffer) nil)
  (length (s-v '%checkpoints)))

(defmethod/cc1 %rewind-stream ((self core-fd-nio-stream))
  (prog1 (length (s-v '%checkpoints))
    (let ((previous-checkpoint (pop (s-v '%checkpoints))))
      (if previous-checkpoint
	  (setf (s-v '%current) (car previous-checkpoint)
		(s-v '%write-buffer) (cadr previous-checkpoint))	  
	  (setf (s-v '%current) -1
		(s-v '%write-buffer) nil)))))

(defmethod/cc1 rewind-stream ((self core-fd-nio-stream))
  (setf (s-v '%read-index) (s-v '%current))
  (%rewind-stream self))

(defmethod/cc1 commit-stream ((self core-fd-nio-stream))  
  (let ((buffer (s-v '%write-buffer)))
    (prog1 (%rewind-stream self)
      (write-stream self (nreverse buffer)))))

(defmethod/cc1 close-stream ((self core-fd-nio-stream))
  (when (transactionalp self)
    (do ((i (current-checkpoint self) (current-checkpoint self)))
	((< i 0) nil)
      (commit-stream self)))

  (when (s-v 'unit)
    (del-fd (s-v 'unit) (s-v '%stream)))
  
  (core-ffi::%close (s-v '%stream))
  self)

(deftrace streams
    '(read-stream write-stream close-stream)) ;;current-checkpoint

(deftrace streams-tx
    '(peek-stream checkpoint-stream rewind-stream commit-stream close-stream
      current-checkpoint))


;;string, null, symbol

;; (defmethod write-stream ((self core-fd-nio-stream) (c (eql nil)))
;;   self)

;; (defmethod write-stream ((self core-fd-io-stream) (vector vector))
;;   (prog1 self
;;     (if (transactionalp self)
;; 	(reduce #'write-stream vector :initial-value self)		
;; 	(write-sequence vector (s-v '%stream)))))


;; (defmethod checkpoint-stream ((self core-fd-io-stream))
;;   (if (transactionalp self)
;;       (push (list (s-v '%current) (s-v '%write-buffer))
;; 	    (s-v '%checkpoints)))

;;   (setf (s-v '%current) (s-v '%read-index)
;; 	(s-v '%write-buffer) (make-accumulator :byte))
;;   (length (s-v '%checkpoints)))

;; (defmethod %rewind-checkpoint ((self core-fd-io-stream))
;;   (prog1 (length (s-v '%checkpoints))
;;     (let ((previous-checkpoint (pop (s-v '%checkpoints))))
;;       (if previous-checkpoint
;; 	  (setf (s-v '%current) (car previous-checkpoint)
;; 		(s-v '%write-buffer) (cadr previous-checkpoint))	  
;; 	  (setf (s-v '%current) -1
;; 		(s-v '%write-buffer) (make-accumulator :byte))))))

;; (defmethod rewind-stream ((self core-fd-io-stream))
;;   (setf (s-v '%read-index) (s-v '%current))
;;   (%rewind-checkpoint self))

;; (defmethod commit-stream ((self core-fd-io-stream))
;;   (let ((buffer (s-v '%write-buffer)))
;;     (prog1 (%rewind-checkpoint self)
;;       (write-stream self buffer)
;;       (if (not (transactionalp self))
;; 	  (finish-output (s-v '%stream))))))

;; (defmethod/cc read-stream ((self core-fd-nio-stream))  
;;   ;; (let/cc k
;; ;;     (handler-bind ((kernel-error (lambda (c)
;; ;; 				   (if (eagain? c)
;; ;; 				       (queue-stream (worker self) 'read-only)))))
;; ;;       (call-next-method)))
;;   3
;;   )

;; (defclass core-cps-stream (core-stream)
;;   ((%k :initform nil)))


;;; This is used to compressed outputs like javascript render.
;;;
;; (defclass core-cps0-stream (core-stream)
;;   ((%k :initform nil)
;;    (%continuations :initform '())))

;; (defmethod copy-core-stream ((self core-cps0-stream))
;;   (let ((s (make-instance 'core-cps-stream)))
;;     (setf (slot-value s '%k) (s-v '%k)
;; 	  (slot-value s '%continuations) (copy-list (s-v '%continuations)))
;;     s))

;; (defmethod transactionalp ((self core-cps0-stream))
;;   (not (null (s-v '%k))))

;; (defmethod current-k ((self core-cps0-stream))
;;   (s-v '%k))

;; (defmethod/cc checkpoint-stream/cc ((self core-cps0-stream) k)
;;   (if (transactionalp self)
;;       (push (s-v '%k) (s-v '%continuations)))

;;   (setf (s-v '%k) (lambda (&optional values)
;; 		    (setf (s-v '%k) (pop (s-v '%continuations)))
;; 		    (kall k values)))
;;   (format t "checkpoint:~D~%" (length (s-v '%continuations)))
;;   (length (s-v '%continuations)))

;; (defmethod/cc rewind-stream/cc ((self core-cps0-stream) &optional values)
;;   (if (not (transactionalp self))
;;       -1
;;       (apply (s-v '%k) (ensure-list values))))

;; ;; (defmethod/cc rewind-stream/cc ((self core-cps-stream) &optional values)
;; ;;   (break "rewinding~%")
;; ;;   (setf *konts* (s-v '%continuations))
;; ;;   (if (not (transactionalp self))
;; ;;       -1
;; ;;       (progn
;; ;; 	(format t "rewind:~D~%" (1- (length (s-v '%continuations))))
;; ;; 	(acond	 
;; ;; 	 ((pop (s-v '%continuations))
;; ;;  	  (let ((current (s-v '%k)))
;; ;;  	    (setf (s-v '%k) it)
;; ;; 	    (format t "kalling:~A, k is :~A" current (s-v '%k))
;; ;;  	    (apply #'kall current values)))
;; ;; 	 ((s-v '%k)
;; ;; 	  (setf (s-v '%k) nil)
;; ;; 	  (format t "K is :~A" (s-v '%k))
;; ;; 	  (apply #'kall it values))
;; ;; 	 (t
; ;; 	  (break "y00"))
;; ;; 	 ))
;; ;;       ))

;; (defun escape (&rest values)
;;   (funcall (apply (arnesi::toplevel-k) values))
;;   (break "You should not see this, call/cc must be escaped already."))

;; (defmethod/cc commit-stream/cc ((self core-cps0-stream) &optional value)
;;   (describe self)
;;   (if (not (transactionalp self))
;;       -1
;;       (progn
;; 	(format t "commit:~D~%" (length (s-v '%continuations)))
;; 	;;	(setf (s-v '%k) (pop (s-v '%continuations)))
;; 	(escape value))))

;; (defclass core-cps0-io-stream (core-cps0-stream core-vector-io-stream)
;;   ())

;; ;; (defmethod/cc checkpoint-stream/cc ((self core-cps-stream))
;; ;;   (let/cc k
;; ;;     (if (transactionalp self)
;; ;; 	(push k (s-v '%continuations)))
    
;; ;;     (setf (s-v '%k) k)
;; ;;     (kall k (checkpoint-stream self))))

;; ;; (defmethod/cc rewind-stream/cc ((self core-cps-stream))
;; ;;   (if (not (transactionalp self))
;; ;;       -1
;; ;;       (progn
;; ;; 	(acond
;; ;; 	 ((pop (s-v '%continuations))
;; ;; 	  (setf (s-v '%k) it)	  
;; ;; 	  (kall it (rewind-stream self)))
;; ;; 	 ((s-v '%k)
;; ;; 	  (setf (s-v '%k) nil)	  
;; ;; 	  (kall it (rewind-stream self)))))))

;; ;; (defmethod/cc commit-stream/cc ((self core-cps-stream))
;; ;;   (if (not (transactionalp self))
;; ;;       -1
;; ;;       (prog1 (commit-stream self)
;; ;; 	(setf (s-v '%k) (pop (s-v '%continuations))))))

;; (defclass core-cps-string-io-stream (core-string-io-stream core-cps0-stream)
;;   ())

;; (defclass core-cps-fd-io-stream (core-cps0-stream core-fd-io-stream)
;;   ())

;; (defclass core-cps-file-io-stream (core-cps0-stream core-file-io-stream)
;;   ())

;; (defclass core-cps-list-io-stream (core-cps0-stream core-list-io-stream)
;;   ())

;; (defclass core-cps-object-io-stream (core-cps0-stream core-object-io-stream)
;;   ())

;; (defun make-core-cps-stream (target)  	 
;;   (etypecase target
;;     (string (make-instance 'core-cps-string-io-stream :string target))
;;     (pathname (make-instance 'core-cps-file-io-stream :file target))
;;     (array (make-instance 'core-cps-vector-io-stream :octets target))
;;     (sb-sys::fd-stream (make-instance 'core-cps-fd-io-stream :stream target))
;;     (list (make-instance 'core-cps-list-io-stream :list target))
;;     (standard-object (make-instance 'core-cps-object-io-stream :object target))))
