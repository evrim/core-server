(in-package :core-server)

;; The application/json dia Type for JavaScript Object Notation (JSON)
;; http://www.ietf.org/rfc/rfc4627.txt

;; JSon Protocol Data Types
(defrule json-string? (q c acc)
  (:or (:and (:seq "\"\"") (:return 'null)) ;; -hek.
       (:and (:quoted? q) (:return (if (> (length q) 0) q nil)))
       (:or (:and (:escaped-string? acc) (:return acc))
	    (:and (:do (setq acc (make-accumulator :byte)))
		  (:type (or visible-char? space?) c)
		  (:collect c acc)
		  (:zom (:type (or visible-char? space?) c)
			(:collect c acc))
		  (:return (if (> (length acc) 0)
			       (octets-to-string acc :utf-8)
			       nil))))))

(defmethod json! ((stream core-stream) (s string))
  (prog1 stream (quoted! stream s #\')))

;; TODO: Implement RFC to decode numbers. -evrim
;;
;; number = [ minus ] int [ frac ] [ exp ]
;; decimal-point = %x2E       ; .
;; digit1-9 = %x31-39         ; 1-9
;; e = %x65 / %x45            ; e E
;; exp = e [ minus / plus ] 1*DIGIT
;; frac = decimal-point 1*DIGIT
;; int = zero / ( digit1-9 *DIGIT)
;; minus = %x2D               ; -
;; plus = %x2B                ; +
;; zero = %x30                ; 0
(defrule json-number? ((sign 1) int frac exp (exp-sign 1))
  (:checkpoint #\- (:do (setq sign -1)) (:commit))
  (:fixnum? int)
  (:checkpoint #\.
	       (:fixnum? frac)
	       (:checkpoint
		#\e
		(:checkpoint
		 (:or (:and #\- (:do (setq exp-sign -1)))
		      #\+)
		 (:commit))
		(:fixnum? exp))
	       (:commit))
  (:return int))

(defmethod json! ((stream core-stream) (n number))
  (prog1 stream (fixnum! stream n)))

(defrule json-boolean? ()
  (:or (:and (:seq "true") (:return 'true))
       (:and (:seq "false") (:return 'false))))

(defmethod json! ((stream core-stream) (false (eql 'false)))
  (string! stream "false"))

(defmethod json! ((stream core-stream) (object null))
  (string! stream "null"))

(defmethod json! ((stream core-stream) (object (eql 't)))
  (string! stream "true"))

(defrule json-array? (val lst)
  (:lwsp?) #\[ (:lwsp?)
  (:zom (:not #\])
	(:json? val)
	(:do (push val lst))
	(:lwsp?)
	(:checkpoint #\, (:commit))
	(:lwsp?))
  (:return (nreverse lst)))

;; FIXmE: This accepts list not array? -evrim.
(defmethod json! ((stream core-stream) (sequence sequence))
  (flet ((primitive! (s p)
	   (prog1 s
	     (string! s " ,")
	     (char! s #\Space)
	     (json! s p))))
    (prog1 stream
      (string! stream "[ ")
      (reduce #'primitive! (rest sequence)
	      :initial-value (json! stream (first sequence)))
      (string! stream " ]"))))

(defrule json-key? (c (acc (make-accumulator)))
  (:checkpoint (:quoted? c) (:lwsp?) #\: (:return c))
  (:not #\:)
  (:type visible-char? c) (:collect c acc)
  (:zom (:not #\:) (:type visible-char? c) (:collect c acc))
  (:return acc))

(defmethod json-key! ((stream core-stream) (key string))
  (prog1 stream (quoted! stream key)))

(defmethod json-key! ((stream core-stream) (key symbol))
  (json-key! stream (symbol-to-js key)))

(defrule json-object? (key value (object (make-hash-table)))
  (:lwsp?) #\{ (:lwsp?)
  (:zom (:not #\})
	(:lwsp?) (:json-key? key) (:lwsp?) (:json? value) (:lwsp?)
	(:checkpoint #\, (:commit)) (:lwsp?)
	(:do (setf (gethash key object) value)))
  (:return object))

(defmethod json! ((stream core-stream) (hash-table hash-table))
  (prog1 stream
    (flet ((one (key value)
	     (when key
	       (json-key! stream key)
	       (string! stream ": ")
	       (json! stream value))))
      (let ((keys (hash-table-keys hash-table))
	    (values (hash-table-values hash-table)))
	(string! stream "{ ")
	(one (car keys) (car values))
	(mapcar (lambda (k v)
		  (string! stream ", ")
		  (one k v))
		(cdr keys) (cdr values))
	(string! stream " }")))))

(defclass jobject ()
  ((attributes :initarg :attributes :initform nil)))

(defun jobject (&rest attributes)
  (make-instance 'jobject :attributes attributes))

(defun convert-label-to-javascript (label)
  (let ((pos 1)
	(label (string-downcase label)))
    (do ((pos (search "-" label :start2 pos) (search "-" label :start2 pos)))
	((null pos) (string-capitalize label))
      (setf (aref label pos) #\Space))))

(defun slot->jobject (slot)
  (with-slotdef (name client-type label) slot
    (cond 
      ((string= 'password client-type)
       (jobject :name (symbol-to-js name)
		;; :value ""
		:type "password"
		:label (or label (convert-label-to-javascript name))))
      ((string= 'string client-type)
       (jobject :name (symbol-to-js name)
		;; :value (let ((value (slot-value object name)))
		;; 	 (typecase value
		;; 	   (symbol (symbol-to-js value))
		;; 	   (t value)))
		:type "string"
		:label (or label (convert-label-to-javascript name))))
      (t
       (jobject :name (symbol-to-js name)
		;; :value (slot-value object name)
		:type (symbol-to-js client-type)
		;; (typecase (slot-value object name)
		;;   (string "string")
		;;   (boolean "boolean")
		;;   (t (symbol-to-js client-type)))
		
		:label (or label (convert-label-to-javascript name)))))))

(defun class->jobject (class)
  (apply #'jobject
	 (reduce0 (lambda (acc slot)
				(cons (make-keyword (slot-definition-name slot))
				      (cons (slot->jobject slot)
					    acc)))
			      (class+.remote-slots class))
	 ;; (cons :class-name
	 ;;       (cons (string-downcase (class-name class))
	 ;; 	     (reduce0 (lambda (acc slot)
	 ;; 			(cons (make-keyword (slot-definition-name slot))
	 ;; 			      (cons (slot->jobject slot)
	 ;; 				    acc)))
	 ;; 		      (class+.remote-slots class))))
	 ))

(defun object->jobject (object &optional (template-class nil))
  (apply #'jobject
	 (append (list :class
		       (class->jobject (if template-class
					   (if (symbolp template-class)
					       (class+.find template-class)
					       template-class)
					   (class-of object))))
		 (reduce0 (lambda (acc slot)
			    (with-slotdef (name client-type) slot
			      (cons (make-keyword name)
				    (cons (if (not (string= 'password client-type))
					      (slot-value object name)) acc))))
			  (class+.remote-slots (or template-class (class-of object)))))))

(defmethod json! ((stream core-stream) (jobject jobject))
  (prog1 stream
    (let ((attributes (slot-value jobject 'attributes)))
      (flet ((one (key value)
	       (when key
		 (json-key! stream key)
		 (string! stream ": ")
		 (json! stream value))))
	(let ((keys (filter #'keywordp attributes))
	      (values (filter (compose #'not #'keywordp) attributes)))
	  (string! stream "{ ")
	  (one (car keys) (car values))
	  (mapcar (lambda (k v)
		    (string! stream ", ")
		    (one k v))
		  (cdr keys) (cdr values))
	  (string! stream " }"))))))

(defmethod json! ((stream core-stream) (object class+-instance))
  (json! stream (object->jobject object)))

(defrule json? (value)
  (:or (:and (:seq "undefined") (:return 'undefined))
       (:and (:seq "null") (:return nil))
       (:and (:or (:json-boolean? value)
		  (:json-number? value)
		  (:json-array? value)
		  (:json-object? value)
		  (:json-string? value))
	     (:return value))))

(defmethod json! ((stream core-stream) (undefined (eql 'undefined)))
  (string! stream "undefined"))

(defmethod json! ((stream core-stream) (symbol symbol))
  (string! stream (symbol-to-js symbol)))

(defmethod json! ((stream core-stream) (element xml))
  (typecase element
    (component
     (with-call/cc (component! (make-indented-stream stream)
			       element)))
    (t
     (string! stream (js* (dom2js element))))))

(defmethod json! ((stream core-stream) (closure arnesi::closure/cc))
  (string! stream (js* `((lambda ,(mapcar (lambda (arg)
					    (case (car arg)
					      (:let (cadr arg))
					      (t (prog1 nil
						   (warn "Fixme: serialize closure to json: ~A" arg)))))
					  (slot-value closure 'arnesi::env))
			   (return
			     ,(unwalk-form (slot-value closure 'arnesi::code))))
			 ,@(mapcar (lambda (arg)
				     (case (car arg)
				       (:let (cddr arg))
				       (t (prog1 nil
					    (warn "Fixme: serialize closure to json: ~A" arg)))))
				   (ast-search-type (slot-value closure 'arnesi::env)
						    'variable-reference))))))

(defun json-serialize (object)
  (let ((s (make-core-stream "")))
    (json! s object)
    (return-stream s)))

(defun json-deserialize (string)
  (typecase string
    (string
     (let ((s (make-core-stream string)))
       (json? s)))
    (t
     string)))

(deftrace json-parsers
    '(json? json-array? json-key? json-object? json-number?
      json-boolean? json-string?))

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
