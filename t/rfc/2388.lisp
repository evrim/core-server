(in-package :tr.gen.core.server.test)

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

(deftest mime-part-from-stream
    (let* ((cstream (make-core-stream *a-mime-part*))
	   (mps (mime-parts? cstream)))
      (and (equal 1 (length mps))
	   (equal (mime-part.name (car mps)) "field1")
	   (equal (mime-part.charset (car mps)) "windows-1250")
	   (equal (mime-part.encoding (car mps)) "quoted-printable")))
  t)

(deftest mime-parts-from-stream 
    (equal (list 3 2 4 1)
	   (list (let* ((cstream (make-core-stream *b-mime-part*))
			(mps (mime-parts? cstream)))
		   (length mps))
		 (let* ((cstream (make-core-stream *c-mime-part*))
			(mps (mime-parts? cstream)))
		   (length mps))
		 (let* ((cstream (make-core-stream *d-mime-part*))
			(mps (mime-parts? cstream)))
		   (length mps))
		 (let* ((cstream (make-core-stream *e-mime-part*))
			(mps (mime-parts? cstream)))
		   (length mps))))
    
  t)