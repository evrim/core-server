(in-package :tr.gen.core.server)
;;;-----------------------------------------------------------------------------
;;; RFC 2388 - Returning Values from Forms:  multipart/form-data
;;; http://www.ietf.org/rfc/rfc2388.txt
;;;-----------------------------------------------------------------------------
(defclass mime-part ()
  ((name :accessor mime-part.name :initarg :name :initform nil)
   (filename :accessor mime-part.filename :initarg :filename :initform nil)
   (content-type :accessor mime-part.content-type :initarg :content-type
		 :initform nil)
   (charset :accessor mime-part.charset :initarg :charset :initform nil)
   (encoding :accessor mime-part.encoding :initarg :encoding :initform nil)
   (mixed-boundary :accessor mime-part.mixed-boundary :initarg :mixed-boundary :initform nil)
   (data :accessor mime-part.data :initarg :data :initform #()
	 :type (array (unsigned-byte 8)))))

(defmacro length+ (var)
  `(if (> (length ,var) 0)
       ,var))

(defun make-mime-part (name data &key filename content-type charset encoding mixed-boundary)
  (make-instance 'mime-part
		 :name (length+ name)
		 :data data
		 :filename (length+ filename)
		 :content-type (length+ content-type)
		 :charset (length+ charset)
		 :encoding (length+ encoding)
		 :mixed-boundary (length+ mixed-boundary)))

(defatom rfc2388-boundary-char? ()
  (and (visible-char? c) (not (eq c #.(char-code #\-)))))

;; -----XXXXX
(defrule rfc2388-boundary? (c (boundary (make-accumulator)))
  (:zom #\-)
  (:zom (:type rfc2388-boundary-char? c)
	(:collect c boundary))
  (:zom #\-) (:lwsp?)
  (:if (> (length boundary) 0)
       (:return boundary)))

;; content-disposition: form-data; name=\"field1\"
;; Content-Disposition: form-data; name=\"abuzer2\"; filename=\"a.html\"
(defrule rfc2388-content-disposition? (name filename (acc (make-accumulator))
				       c)
  (:sci "content-disposition:") (:lwsp?)
  (:or (:checkpoint
	(:sci "form-data;")
	(:commit))
       (:checkpoint
	(:sci "file;")
	(:commit)))
  (:lwsp?)
  (:zom (:and (:or (:and (:sci "name=\"")
			 (:zom (:not #\") (:type octet? c) (:collect c acc))
			 (:do (setq name acc acc (make-accumulator))))
		   (:and (:sci "filename=\"")
			 (:zom (:not #\") (:type octet? c) (:collect c acc))
			 (:do (setq filename acc acc (make-accumulator)))))
	      #\; (:lwsp?)))
  (:lwsp?)
  (:return (cons name filename)))

;; content-type: text/plain;charset=windows-1250
(defrule rfc2388-content-type? (c (type (make-accumulator))
				  (charset (make-accumulator))
				  (boundary (make-accumulator)))
  (:sci "content-type:") (:lwsp?)
  (:and (:zom (:not #\;) (:type visible-char? c) (:collect c type)) 
	(:checkpoint
	 (:lwsp?) (:sci "charset=")
	 (:zom (:type visible-char? c)
	       (:collect c charset))
	 (:commit))
	(:checkpoint
	 (:lwsp?) (:sci "boundary=")
	 (:zom (:type visible-char? c)
	       (:collect c boundary))
	 (:commit))
	(:lwsp?)
	(:return (values type charset boundary))))

;;content-transfer-encoding: quoted-printable
(defrule rfc2388-content-transfer-encoding? (c (encoding (make-accumulator)))
  (:sci "content-transfer-encoding:")
  (:zom (:type space?))
  (:and (:zom (:type visible-char? c)
	      (:collect c encoding))	
	(:lwsp?)
	(:if (> (length encoding) 0)
	     (:return encoding)
	     (:return "7bit"))))

(defrule rfc2388-mime-part? (boundary name filename type charset encoding
			     temp1 temp2 temp3 mixed-boundary c (data (make-accumulator :byte)))
  (:lwsp?)
  (:rfc2388-boundary? boundary)
  (:zom (:or (:and (:rfc2388-content-disposition? temp1)
		   (:do (setq name (car temp1) filename (cdr temp1))))
	     (:and (:rfc2388-content-type? temp1 temp2 temp3)
		   (:do (setq type temp1 charset temp2 mixed-boundary temp3)))
	     (:and (:rfc2388-content-transfer-encoding? temp1)
		   (:do (setq encoding temp1)))))
  (:lwsp?)
  (:zom (:or (:checkpoint
	      (:type linefeed?)
	      (:rfc2388-boundary? temp1)
	      (:if (string= temp1 boundary)
		   (:rewind-return (values boundary
					   (make-mime-part
					    name data
					    :filename filename
					    :content-type (or type
							      (if filename
								  "application/octet-stream"
								  "text/plain"))
					    :charset charset
					    :encoding encoding
					    :mixed-boundary mixed-boundary)
					   (list name filename
						 (or type
						     (if filename
							 "application/octet-stream"
							 "text/plain"))
						 charset
						 encoding
						 mixed-boundary
						 data)))))
	     (:and (:type octet? c)
		   (:collect c data)))))

(defrule mime-parts? ((mimes '()) (boundaries '()) boundary mime)
  (:zom (:and (:rfc2388-mime-part? boundary mime)
	      (:do (push mime mimes)
		   (push boundary boundaries))))
  (:do (when mimes
	 (setq mimes
	       (nreverse
		(reduce #'(lambda (acc atom)
			    (if (equal "multipart/mixed" (mime-part.content-type atom))		
				(let ((m (mime-parts?
					  (make-core-stream
					   (mime-part.data atom)))))
				  (append m acc))
				(cons atom acc)))
			mimes
			:initial-value nil)))))
  (:return (nreverse mimes)))

(defvar *a-mime-part*
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