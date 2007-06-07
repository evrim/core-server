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

;; A mime that has only one part
(deftest mime-part-from-stream
    (let* ((cstream (make-core-stream *a-mime-part*))
	   (mime (rfc2388-mimes? cstream)))
      (and (equal 1 (length mime))
	   (equal (mime.header (car mime) 'disposition)
		  `("form-data" (("name" . "field1"))))
	   (equal (mime.header (car mime) 'content-type)
		  `("text" "plain" (("charset" . "windows-1250"))))
	   (equal (mime.header (car mime) 'transfer-encoding)
		  'quoted-printable)))
  t)

(deftest mime-parts-from-stream
    (equal (list 3 2 3 1)
	   (list (let* ((cstream (make-core-stream *b-mime-part*))
			(mps (mimes? cstream)))
		   (length mps))
		 (let* ((cstream (make-core-stream *c-mime-part*))
			(mps (mimes? cstream)))
		   (length mps))
		 (let* ((cstream (make-core-stream *d-mime-part*))
			(mps (mimes? cstream)))
		   (length mps))
		 (let* ((cstream (make-core-stream *e-mime-part*))
			(mps (mimes? cstream)))
		   (length mps))))    
  t)

(defun vector= (v1 v2)
  (reduce #'(lambda (x y)
	      (and x y))
	  (map 'vector #'eq v1 v2)))

(deftest exampleb-mime-part
    (let* ((cstream (make-core-stream *b-mime-part*))
	   (mps (rfc2388-mimes? cstream)))
      (and
       ;; first part 
       (let ((mime (car mps)))
	 (and
	  (equal (mime.header mime 'disposition)
		 '("form-data" (("name" . "gee"))))
	  (vector= (mime.data mime)
		   #(103 101 101 49 50 51))))
       ;; third part
       (let ((mime (caddr mps)))
	 (and
	  (equal (mime.header mime 'content-type)
		 '("text" "html"))
	  (equal (mime.header mime "disposition")
		 '("form-data" (("name" . "abuzer2") ("filename" . "a.html"))))
	  (vector= (mime.data mime)
		   #(60 33 68 79 67 84 89 80 69 32 104 116 109 108 32 80 85 66 76
              73 67 32 34 45 47 47 87 51 67 47 47 68 84 68 32 88 72 84 77
              76 32 49 46 48 32 83 116 114 105 99 116 47 47 69 78 34 32 34
              104 116 116 112 58 47 47 119 119 119 46 119 51 46 111 114
              103 47 84 82 47 120 104 116 109 108 49 47 68 84 68 47 120
              104 116 109 108 49 45 115 116 114 105 99 116 46 100 116 100
              34 62 10 60 104 116 109 108 10 32 32 62 60 98 111 100 121 10
              32 32 32 32 32 32 62 60 102 111 114 109 32 97 99 116 105 111
              110 61 34 104 116 116 112 58 47 47 108 111 99 97 108 104 111
              115 116 47 103 101 101 46 103 101 101 34 32 109 101 116 104
              111 100 61 34 80 79 83 84 34 10 9 32 32 101 110 99 116 121
              112 101 61 34 109 117 108 116 105 112 97 114 116 47 102 111
              114 109 45 100 97 116 97 34 32 10 9 32 32 32 32 32 32 32 32
              62 60 105 110 112 117 116 32 110 97 109 101 61 34 103 101
              101 34 32 116 121 112 101 61 34 116 101 120 116 34 32 118 97
              108 117 101 61 34 103 101 101 49 50 51 34 10 9 9 9 32 32 32
              32 32 32 47 62 60 105 110 112 117 116 32 116 121 112 101 61
              34 115 117 98 109 105 116 34 32 110 97 109 101 61 34 97 98
              117 122 101 114 49 34 32 118 97 108 117 101 61 34 98 97 115
              32 98 97 107 97 108 105 109 34 62 10 9 9 9 9 32 32 60 105
              110 112 117 116 32 116 121 112 101 61 34 102 105 108 101 34
              32 110 97 109 101 61 34 97 98 117 122 101 114 50 34 62 10 9
              9 9 9 32 32 60 105 110 112 117 116 32 116 121 112 101 61 34
              102 105 108 101 34 32 110 97 109 101 61 34 97 98 117 122 101
              114 51 34 62 10 9 9 9 9 32 32 60 47 102 111 114 109 10 9 9 9
              9 32 32 32 32 32 32 62 60 47 98 111 100 121 10 9 9 9 9 9 32
              32 32 32 62 60 47 104 116 109 108 10 9 9 9 9 9 9 62 10))))))
    t)