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

;;;--------------------------------------------------------------------------
;;; RFC 2045 - Multipurpose Internet Mail Extensions (MIME)
;;; Part One: Format of Internet Message Bodies
;;;--------------------------------------------------------------------------

;;;--------------------------------------------------------------------------
;;; 6.7.  Quoted-Printable Content-Transfer-Encoding
;;;--------------------------------------------------------------------------

;; transport-padding := *LWSP-char
;; Composers MUST NOT generate non-zero length transport padding, but
;; receivers MUST be able to handle padding added by message
;; transports.
(defatom lwsp-char? ()
  (or (space? c) (tab? c) (carriage-return? c) (linefeed? c)))

(defrule transport-padding? ()
  (:zom (:type lwsp-char?))
  (:return t))

;; hex-octet := "=" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
;; Octet must be used for characters > 127, =, SPACEs or TABs at the
;; ends of lines, and is recommended for any character not listed in
;; RFC 2049 as "mail-safe".
(defrule hex-octet? (hex)
  (:and #\= (:hex-value? hex) (:return hex)))

;; safe-char := <any octet with decimal value of 33 through
;;               60 inclusive, and 62 through 126>
;; Characters not listed as "mail-safe" in RFC 2049 are also not recommended.
(defatom safe-char? ()
  (or (and (> c 32) (< c 61)) (and (> c 61) (< c 127))))

;; ptext := hex-octet / safe-char
(defrule ptext? (c)
  (:or (:and (:type safe-char? c) (:return c))
       (:and (:hex-octet? c) (:return c))))

;; qp-section := [*(ptext / SPACE / TAB) ptext]
(defrule qp-section? (c (acc (make-accumulator :byte)))
  (:zom (:or (:ptext? c) (:type (or space? tab?) c)) (:collect c acc)
	(:or (:ptext? c) (:type (or space? tab?) c)) (:collect c acc)
	(:zom (:type (or space? tab?))))
  (:return acc))

;; qp-segment := qp-section *(SPACE / TAB) "="
;; Maximum length of 76 characters
(defrule qp-segment? (qp)
  (:qp-section? qp) (:zom (:type (or space? tab?)))
  #\= (:return qp))

;; qp-part := qp-section ; Maximum length of 76 characters
(defrule qp-part? (qp)
  (:qp-section? qp) (:return qp))

;; qp-line := *(qp-segment transport-padding CRLF)
;;            qp-part transport-padding
(defrule qp-line? (segments qs)
  (:zom (:qp-segment? qs) (:do (push qs segments))
	(:crlf?))
  (:and (:qp-part? qs) (:do (push qs segments)))
  (:transport-padding?)
  (:return (apply #'concatenate '(vector (unsigned-byte 8))
		  (nreverse segments))))

;; quoted-printable := qp-line *(CRLF qp-line)
(defrule quoted-printable? (lines line)
  (:qp-line? line) (:do (push line lines))
  (:zom (:crlf?)
	(:qp-line? line)
	(:do (push line lines)))
  (:return (apply #'concatenate '(vector (unsigned-byte 8))
		  (nreverse lines))))

(defun hex-octet! (stream atom)
  (char! stream #\=)
  (hex-value! stream atom))

;; This is depreciated. Use make-quoted-printable-stream
;; instead. -evrim.
(defun quoted-printable! (stream array &aux (line-length 0))
  (etypecase array
     (string (setq array (string-to-octets array :utf-8)))
     (array t))
  (flet ((do-crlf ()	   
	   (byte! stream #.(char-code #\=))
;;         (byte! stream 13)
	   (byte! stream 10)
	   (setq line-length 0)))
    (reduce #'(lambda (acc atom)
		(declare (ignore acc))
		(cond
		  ((safe-char? atom)
		   (if (> line-length 74)
		       (do-crlf)
		       (incf line-length))
		   (byte! stream atom))
		  (t
		   (if (> line-length 72)
		       (do-crlf)
		       (incf line-length 3))
		   (hex-octet! stream atom)))
		nil)
	    array :initial-value nil))
  array)

;; #|

;; (defparameter *qptext-1* "abc def ghi=0A=
;; abc def ghi=0A=
;; ghi")
;; (defparameter *qptext* "abc def ghi
;; abc def ghi
;; ghi")
;; (equal (octets-to-string (quoted-printable? (make-core-stream *qptext-1*)) :utf-8)
;;        *qptext*)
;; => t
;; SERVER> (let ((stream (make-core-stream "")))
;; 	  (quoted-printable! stream "AAAAAAAAAAAAAAAAAAAAAAA
;;  A AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
;; 	  (stream-data stream))
;; "AAAAAAAAAAAAAAAAAAAAAAA=0A=20A=20AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
;; AAAAAAAAAAB"
;; SERVER> (length "AAAAAAAAAAAAAAAAAAAAAAA=0A=20A=20AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")
;; 76
;; SERVER> (let ((stream (make-core-stream "")))
;; 	  (quoted-printable! stream "AAAAAAAAAAAAAAAAAAAAAAA
;;  A AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
;; 	  (stream-data stream))
;; "AAAAAAAAAAAAAAAAAAAAAAA=0A=20A=20AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
;; AAAAAAAAAAB"
;; SERVER> (quoted-printable? (make-core-stream *))
;; "AAAAAAAAAAAAAAAAAAAAAAA
;;  A AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"
 
;; |#

;;;-----------------------------------------------------------------------------
;;; 6.8.  Base64 Content-Transfer-Encoding
;;;-----------------------------------------------------------------------------

;;; Got binary aritmetic functions from mr. Sven Van Caekenberghe
;;; http://cl-debian.alioth.debian.org/repository/pvaneynd/s-base64-upstream/src/base64.lisp
;;; Those have licenses at: http://opensource.franz.com/preamble.html

;;                     Table 1: The Base64 Alphabet
;;
;;      Value Encoding  Value Encoding  Value Encoding  Value Encoding
;;          0 A            17 R            34 i            51 z
;;          1 B            18 S            35 j            52 0
;;          2 C            19 T            36 k            53 1
;;          3 D            20 U            37 l            54 2
;;          4 E            21 V            38 m            55 3
;;          5 F            22 W            39 n            56 4
;;          6 G            23 X            40 o            57 5
;;          7 H            24 Y            41 p            58 6
;;          8 I            25 Z            42 q            59 7
;;          9 J            26 a            43 r            60 8
;;         10 K            27 b            44 s            61 9
;;         11 L            28 c            45 t            62 +
;;         12 M            29 d            46 u            63 /
;;         13 N            30 e            47 v
;;         14 O            31 f            48 w         (pad) =
;;         15 P            32 g            49 x
;;         16 Q            33 h            50 y
(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter +inverse-base64-alphabet+
  (let ((inverse-base64-alphabet (make-array 127)))
    (dotimes (i 127 inverse-base64-alphabet)
      (setf (aref inverse-base64-alphabet i)
	    (position (code-char i) +base64-alphabet+)))))

(defun base64-decode (byte1 byte2 byte3 byte4)
  (flet ((inverse (byte)
	   (if byte
	       (aref +inverse-base64-alphabet+ byte)
	       0)))
    (let ((v1 (inverse byte1)) (v2 (inverse byte2))
	  (v3 (inverse byte3)) (v4 (inverse byte4)))
      (apply #'values
	     (list (logior (ash v1 2) (ash v2 -4))
		   (if byte3
		       (logior (ash (logand v2 #B1111) 4) (ash v3 -2)))
		   (if (and byte3 byte4)
		       (logior (ash (logand v3 #B11) 6) v4)))))))

(defatom base64-char? ()
  (or (alphanum? c) (eq c #.(char-code #\+)) (eq c #.(char-code #\/))))

(defrule base64-block? (c1 c2 c3 c4)
 (:type base64-char? c1)
 (:type base64-char? c2)
 (:or (:type base64-char? c3)
      (:and #\= (:do (setq c3 nil))))
 (:or (:type base64-char? c4)
      (:and #\= (:do (setq c4 nil))))
 (:return (base64-decode c1 c2 c3 c4)))

(defrule base64? (v1 v2 v3 (acc (make-accumulator :byte)))
  (:zom (:base64-block? v1 v2 v3)
	(:if v1 (:collect v1 acc))
	(:if v2 (:collect v2 acc))
	(:if v3 (:collect v3 acc))
	(:lwsp?)
	(:if (null v3) (:return acc)))
  (:return acc))

(defun base64-encode (byte1 byte2 byte3)  
  (values (char +base64-alphabet+ (ash byte1 -2))
	  (char +base64-alphabet+		
		(logior (ash (logand byte1 #B11) 4)
			(ash (logand (or byte2 0) #B11110000) -4)))
	  (if byte2 
	      (char +base64-alphabet+
		    (logior (ash (logand byte2 #B00001111) 2)
			    (ash (logand (or byte3 0) #B11000000) -6)))
	      #\=)
	  (if byte3
	      (char +base64-alphabet+ (logand byte3 #B111111))
	      #\=)))

(defun base64! (stream array)
  (etypecase array
      (string (setq array (string-to-octets array :utf-8)))
      (array t))
  (let ((i 0) leak)
    (labels ((out (char)
	       (when (= (the fixnum i) 76)
		 (char! stream #\Newline)
		 (setq i 0))
	       (char! stream char)
	       (incf i))
	     (write-blocks ()
	       (reduce
		(lambda (acc atom)
		  (cond
		    ((= (length acc) 2)
		     (push-atom atom acc)
		     (multiple-value-bind (out1 out2 out3 out4)
			 (base64-encode (aref acc 0) (aref acc 1) (aref acc 2))
		       (out out1)
		       (out out2)
		       (out out3)
		       (out out4))
		     (make-accumulator :byte))
		    (t
		     (push-atom atom acc)
		     acc)))
		array :initial-value (make-accumulator :byte))))    
      (setq leak (write-blocks))
      (let ((len (length leak)))
	(if (> len 0)
	    (multiple-value-bind (out1 out2 out3 out4)
		(base64-encode (aref leak 0) (if (>= len 2) (aref leak 1))
			       (if (>= len 3) (aref leak 2)))
	      (out out1) (out out2) (out out3) (out out4))))))
  t)

;; #|

;; (defparameter +base64-encoded+ "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
;; IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
;; dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
;; dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
;; ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=sometrash")

;; (defparameter +base64-string+ "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

;; (equal (octets-to-string (base64? (make-core-stream +base64-encoded+))
;; 			 :utf-8) +base64-string+)

;; (equal (let ((stream (make-core-stream "")))
;; 	 (base64! stream +base64-string+)
;; 	 (return-strteam stream))
;;        +base64-encoded+)

;; |#

;;;-----------------------------------------------------------------------------
;;; 4.  MIME-Version Header Field
;;;-----------------------------------------------------------------------------
;; version := "MIME-Version" ":" 1*DIGIT "." 1*DIGIT
(defrule mime-version? (v)
  (:sci "mime-version:")
  (:lwsp?) (:version? v) (:return v))

;;;-----------------------------------------------------------------------------
;;; 5.  Content-Type Header Field
;;;-----------------------------------------------------------------------------
(defrule rfc2045-content-type? (content-type)
  (:sci "content-type:")
  (:lwsp?)
  (:http-content-type? content-type)
  (:return content-type))

;;;-----------------------------------------------------------------------------
;;; 6.  Content-Transfer-Encoding Header Field
;;;-----------------------------------------------------------------------------
;;  encoding := "Content-Transfer-Encoding" ":" mechanism
;;  mechanism := "7bit" / "8bit" / "binary" / "quoted-printable" / "base64" /
;;               ietf-token / x-token
(defrule rfc2045-content-transfer-encoding? ()
  (:sci "content-transfer-encoding:")
  (:lwsp?)
  (:or (:and (:sci "7bit") (:return '7bit))
       (:and (:sci "8bit") (:return '8bit))
       (:and (:sci "binary") (:return 'binary))
       (:and (:sci "quoted-printable") (:return 'quoted-printable))
       (:and (:sci "base64") (:return 'base64))))

;;;-----------------------------------------------------------------------------
;;; 7.  Content-ID Header Field
;;;-----------------------------------------------------------------------------
;; id := "Content-ID" ":" msg-id
;; fixme: implement msg-id
(defrule rfc2045-content-id? (c (acc (make-accumulator :byte)))
  (:sci "content-id:")
  (:lwsp?)
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

;;;-----------------------------------------------------------------------------
;;; 8.  Content-Description Header Field
;;;-----------------------------------------------------------------------------
;;; description := "Content-Description" ":" *text
(defrule rfc2045-content-description? (c (acc (make-accumulator :byte)))
  (:sci "content-description:") (:lwsp?)
  (:type http-header-value? c) (:collect c acc)
  (:zom (:type http-header-value? c) (:collect c acc))
  (:return acc))

;;;-----------------------------------------------------------------------------
;;; 9.  Additional  Header Fields
;;;-----------------------------------------------------------------------------
;;  MIME-extension-field := <Any RFC 822 header field which begins
;;                               with the string "Content-">
;; fixme: implement extenstion fields.


