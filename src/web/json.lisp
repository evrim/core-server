(in-package :core-server)

;; The application/json dia Type for JavaScript Object Notation (JSON)
;; http://www.ietf.org/rfc/rfc4627.txt

;; JSon Protocol Data Types
(defrule json-string? (q c acc)
  (:checkpoint (:seq "\"\"") (:return 'null)) ;; -hek.
  (:or (:and (:quoted-string? q) (:return q))
       (:and (:do (setq acc (make-accumulator)))
	     (:type (or visible-char? space?) c)
	     (:collect c acc)
	     (:zom (:type (or visible-char? space?) c)
		   (:collect c acc))
	     (:return acc))))

(defmethod json-string! ((stream core-stream) (s string))
  (prog1 stream (quoted! stream s)))

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

(defmethod json-number! ((stream core-stream) (n number))
  (prog1 stream (fixnum! stream n)))

(defrule json-boolean? ()
  (:or (:and (:seq "true") (:return 'true))
       (:and (:seq "false") (:return 'false))))

(defmethod json-boolean! ((stream core-stream) b)
  (prog1 stream
    (ecase b
      (true (string! stream "true"))
      (false (string! stream "false"))
      (t (string! stream "true")))))

(defrule json-array? (val lst)
  (:lwsp?) #\[ (:lwsp?)
  (:zom (:not #\])
	(:json? val)	
	(:do (push val lst))
	(:lwsp?) (:checkpoint #\, (:commit)) (:lwsp?))
  (:return (nreverse lst)))


;; FIXmE: This accepts list not array? -evrim.
(defmethod json-array! ((stream core-stream) sequence)
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
  (:checkpoint (:quoted-string? c) (:lwsp?) #\: (:return c))
  (:not #\:)
  (:type visible-char? c) (:collect c acc)
  (:zom (:not #\:) (:type visible-char? c) (:collect c acc))
  (:return acc))

(defmethod json-key! ((stream core-stream) (key string))
  (json-string! stream key))

(defmethod json-key! ((stream core-stream) (key symbol))
  (symbol! stream key))

(defrule json-object? (key value (object (make-hash-table)))
  (:lwsp?) #\{ (:lwsp?)
  (:zom (:not #\})
	(:lwsp?) (:json-key? key) (:lwsp?) (:json? value) (:lwsp?)
	(:checkpoint #\, (:commit)) (:lwsp?)
	(:do (setf (gethash key object) value)))
  (:return object))

(defmethod json-object! ((stream core-stream) hash-table)
  (prog1 stream
    (string! stream "{")
    (maphash (lambda (key value)
	       (char! stream #\Space)
	       (json-key! stream key)
	       (string! stream ": ")
	       (json! stream value))
	     hash-table)
    (string! stream " }")))


(defrule json? (value)
  (:or (:and (:seq "undefined") (:return 'undefined))
       (:and (:seq "null") (:return 'null))
       (:and (:or (:json-boolean? value)
		  (:json-number? value)
		  (:json-array? value)
		  (:json-object? value)
		  (:json-string? value))
	     (:return value))))

(defmethod json! ((stream core-stream) something)
  (etypecase something
    (null (string! stream "null"))
    (hash-table (json-object! stream something))
    (list (json-array! stream something))
    (string (json-string! stream something))
    (number (json-number! stream something))
    (symbol
     (ecase something
       ((or true false) (json-boolean! stream something))           
       (undefined (string! stream "undefined"))
       (null (string! stream "null"))))))


(defun json-serialize (object)
  (let ((s (make-core-stream "")))
    (json! object)
    (return-stream s)))

(defun json-deserialize (string)
  (let ((s (make-core-stream string)))
    (json? s)))

(defvar +json-parsers+ '(json? json-array? json-key? json-object?
			 json-number? json-boolean? json-string?))

(defun trace-json-parsers ()
  (mapcar (lambda (p)
	    (eval `(trace ,p))) +json-parsers+))

(defun untrace-json-parsers ()
  (mapcar (lambda (p)
	    (eval `(untrace ,p))) +json-parsers+))