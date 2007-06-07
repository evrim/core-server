(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; RFC 2388 - Returning Values from Forms:  multipart/form-data
;;; http://www.ietf.org/rfc/rfc2388.txt
;;;-----------------------------------------------------------------------------

;; 3. Definition of multipart/form-data
;; content-disposition: form-data; name=\"field1\"
;; Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
(defatom rfc2388-parameter-value? ()
  (and (not (eq #.(char-code #\;) c))
       (or (visible-char? c)
	   (space? c))))

(defrule rfc2388-content-disposition? ((parameters '()) (type (make-accumulator)) c
					    (key (make-accumulator)) value)
  (:lwsp?)
  (:type http-header-name? c) (:collect c type)
  (:zom (:not #\;) (:type (or visible-char? space?) c) (:collect c type))
  (:lwsp?)
  (:zom (:type http-header-name? c) (:collect c key)
	(:zom (:not #\=) (:type http-header-name? c) (:collect c key))
	(:or (:and (:quoted? value))
	     (:and (:do (setq value (make-accumulator :byte)))
		   (:type http-header-value? c) (:collect c value)
		   (:zom (:type rfc2388-parameter-value? c) (:collect c value))))
	(:do (push (cons key value) parameters)
	     (setq key (make-accumulator)))
	#\;
	(:debug)
	(:lwsp?))
  (:return (list type (nreverse parameters))))

(setf (gethash :rfc2388-mimes? +parser-rules+) 'rfc2388-mimes?)
(defun rfc2388-mimes? (stream &optional (boundary nil))
  (let ((mimes (mimes? stream boundary)))
    (when mimes
      (mapc #'(lambda (mime)
		(mime-search mime
			     #'(lambda (mime)
				 (setf (mime.header mime 'disposition)
				       (rfc2388-content-disposition?
					(make-core-stream (mime.header mime 'disposition))))
				 nil)))
	    mimes)
      mimes)))

;; (defclass mime-part ()
;;   ((name :accessor mime-part.name :initarg :name :initform nil)
;;    (filename :accessor mime-part.filename :initarg :filename :initform nil)
;;    (content-type :accessor mime-part.content-type :initarg :content-type
;; 		 :initform nil)
;;    (parameters :accessor mime-part.parameters :initarg :parameters :initform nil)
;;    (encoding :accessor mime-part.encoding :initarg :encoding :initform nil)
;;    (mixed-boundary :accessor mime-part.mixed-boundary :initarg :mixed-boundary :initform nil)
;;    (data :accessor mime-part.data :initarg :data :initform #()
;; 	 :type (array (unsigned-byte 8)))))

;; (defmacro length+ (var)
;;   `(if (> (length ,var) 0)
;;        ,var))

;; (defun make-mime-part (name data &key filename content-type parameters encoding mixed-boundary)
;;   (make-instance 'mime-part
;; 		 :name (length+ name)
;; 		 :data data
;; 		 :filename (length+ filename)
;; 		 :content-type (length+ content-type)
;; 		 :parameters (length+ parameters)
;; 		 :encoding (length+ encoding)
;; 		 :mixed-boundary (length+ mixed-boundary)))


;; ;; -----XXXXX
;; (defrule rfc2388-boundary? (c (boundary (make-accumulator)) (last nil))
;;   #\-
;;   (:zom #\-)
;;   (:type rfc2388-boundary-char? c)
;;   (:collect c boundary)
;;   (:zom (:type rfc2388-boundary-char? c) (:collect c boundary))
;;   (:zom #\- (:do (setq last t)))
;;   (:return (values boundary last)))

;; (defatom rfc2388-media-type? ()
;;   (and (not (eq c #.(char-code #\,)))
;;        (not (eq c #.(char-code #\;)))
;;        (not (eq c #.(char-code #\/)))
;;        (not (eq c #.(char-code #\=)))
;;        (visible-char? c)))

;; (defrule rfc2388-media-range? ((type (make-accumulator))
;; 			    (subtype (make-accumulator)) c)
;;   (:zom (:type rfc2388-media-type? c) (:collect c type))
;;   #\/
;;   (:zom (:type rfc2388-media-type? c) (:collect c subtype))  
;;   (:return (values type subtype)))

;; ;; content-type: text/plain;charset=windows-1250
;; (setf (gethash :rfc2388-content-type? +parser-rules+) 'rfc2388-content-type?)
;; (defrule rfc2388-content-type? (c type subtype
;; 				  (charset (make-accumulator))
;; 				  (boundary (make-accumulator)))
;;   (:sci "content-type:") (:lwsp?)
;;   (:rfc2388-media-range? type subtype)
;;   (:and (:lwsp?)
;; 	(:checkpoint
;; 	 (:lwsp?) (:sci "charset=")
;; 	 (:zom (:type visible-char? c)
;; 	       (:collect c charset))
;; 	 (:commit))
;; 	(:checkpoint
;; 	 (:lwsp?) (:sci "boundary=")
;; 	 (:zom (:type visible-char? c)
;; 	       (:collect c boundary))
;; 	 (:commit))
;; 	(:lwsp?)
;; 	(:return (values type subtype			
;; 			 (list (cons "charset" charset)
;; 			       (cons "boundary" charset))))))

;; ;;content-transfer-encoding: quoted-printable
;; (defrule rfc2388-content-transfer-encoding? (c (encoding (make-accumulator)))
;;   (:sci "content-transfer-encoding:")
;;   (:zom (:type space?))
;;   (:and (:zom (:type visible-char? c)
;; 	      (:collect c encoding))	
;; 	(:lwsp?)
;; 	(:if (> (length encoding) 0)
;; 	     (:return encoding)
;; 	     (:return "7bit"))))

;; (defrule rfc2388-mime-part? (boundary name filename type subtype parameters encoding
;; 			     temp1 temp2 temp3 mixed-boundary c (data (make-accumulator :byte))
;; 			     last)
;;   (:rfc2388-boundary? boundary last)
;;   (:if last (:return nil))
;;   (:lwsp?)
;;   (:zom (:not (:type (or linefeed? carriage-return?)))
;; 	(:or (:and (:rfc2388-content-disposition? temp1)
;; 		   (:do (setq name (car temp1) filename (cdr temp1))))
;; 	     (:and (:rfc2388-content-type? temp1 temp2 temp3)
;; 		   (:do (setq type temp1 subtype temp2 parameters temp3)))
;; 	     (:and (:rfc2388-content-transfer-encoding? temp1)
;; 		   (:do (setq encoding temp1)))))
;;   (:zom (:or (:checkpoint
;; ;;	      (:crlf?)
;; 	      (:rfc2388-boundary? temp1)
;; 	      (:if (string= temp1 boundary)		   
;; 		   (:rewind-return (values boundary
;; 					   (make-mime-part
;; 					    name data
;; 					    :filename filename
;; 					    :content-type (or (and type (list type subtype))
;; 							      (if filename
;; 								  (list "application" "octet-stream")
;; 								  (list "text" "plain")))
;; 					    :parameters parameters
;; 					    :encoding encoding
;; 					    :mixed-boundary mixed-boundary)))))
;; 	     (:zom (:not #\-) (:not (:type carriage-return?))
;; 		   (:not (:type linefeed?))		   
;; 		   (:type octet? c) (:collect c data))
;; 	     (:debug))))

;; (defrule mime-parts? ((mimes '()) (boundaries '()) boundary mime)
;;   (:lwsp?)
;;   (:zom (:rfc2388-mime-part? boundary mime)
;; 	(:do (push mime mimes)
;; 	     (push boundary boundaries)))
;;   (:do (when mimes
;; 	 (setq mimes
;; 	       (nreverse
;; 		(reduce #'(lambda (acc atom)
;; 			    (if (equal "mixed" (string-downcase (cadr (mime-part.content-type atom))))		
;; 				(let ((m (mime-parts?
;; 					  (make-core-stream
;; 					   (mime-part.data atom)))))
;; 				  (append m acc))
;; 				(cons atom acc)))
;; 			mimes
;; 			:initial-value nil)))))
;;   (:return (nreverse mimes)))

(defparameter *a-mime-part*
  "--AaB03x
content-disposition: form-data; name=\"field1\"
content-type: text/plain;charset=windows-1250
content-transfer-encoding: quoted-printable

Joe owes =80100.
--
--AaB03x")

(defparameter *b-mime-part*
  "-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"gee\"

gee123
-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"abuzer1\"

bas bakalim
-----------------------------180841493794515451098915474
Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
Content-Type: text/html

<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html
  ><body
      ><form action=\"http://localhost/gee.gee\" method=\"POST\"
	  enctype=\"multipart/form-data\" 
	        ><input name=\"gee\" type=\"text\" value=\"gee123\"
			      /><input type=\"submit\" name=\"abuzer1\" value=\"bas bakalim\">
				  <input type=\"file\" name=\"abuzer2\">
				  <input type=\"file\" name=\"abuzer3\">
				  </form
				      ></body
					    ></html
						>

-----------------------------180841493794515451098915474
")

(defvar *c-mime-part* "----AaB03x
Content-Disposition: form-data; name=\"submit-name\"

Larry
----AaB03x
Content-Disposition: form-data; name=\"files\"; filename=\"file1.txt\"
Content-Type: text/plain

file1.txt
----AaB03x--
")

(defparameter *d-mime-part* "--AaB03x
Content-Disposition: form-data; name=\"gee-name\"

Lari balki
--AaB03x
Content-Disposition: form-data; name=\"files\"
Content-Type: multipart/mixed; boundary=BbC04y

--BbC04y
Content-Disposition: file; filename=\"file1.txt\"
Content-Type: text/plain

file1.txt
--BbC04y
Content-Disposition: file; filename=\"file2.gif\"
Content-Type: image/gif
Content-Transfer-Encoding: binary

file2.gif
--BbC04y--
--AaB03x
Content-Disposition: form-data; name=\"submit-name\"

Larry
--AaB03x--
a")

(defparameter *e-mime-part* "--AaB03x
Content-Disposition: form-data; name=\"files\"

----AaB03
--AaB03x--
")