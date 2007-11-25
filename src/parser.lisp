(in-package :tr.gen.core.server)
;;(declaim (optimize (speed 3) (space 2) (safety 0) (debug 0) (compilation-speed 0)))

;;;-----------------------------------------------------------------------------
;;; HENRY G. BAKER PARSER GENERATOR WITH EXTENDED GRAMMER
;;; http://linux.rice.edu/~rahul/hbaker/Prag-Parse.html
;;;-----------------------------------------------------------------------------
(declaim (inline match-atom))
(defun match-atom (stream atom)
  (let ((c (peek-stream stream)))    
    (cond
      ((and (characterp atom) (> (char-code atom) 127)) ;;utf-8
       (prog1 t
	 (checkpoint-stream stream) 
	 (prog1 t
	   (reduce #'(lambda (acc atom)
		       (declare (ignore acc))
		       (if (eq (the (unsigned-byte 8) atom) (the (unsigned-byte 8) c))
			   (progn
			     (read-stream c) (setq c (peek-stream stream)))
			   (progn
			     (rewind-stream stream)
			     (return-from match-atom nil)))
		       nil)
		   (string-to-octets (string atom) :utf-8) :initial-value nil))
	 (commit-stream stream)))
      ((characterp atom) (eq c (char-code atom)))
      (t (eq atom c)))))

(defmacro match/stream (stream atom)
  `(when (match-atom ,stream ,atom)
     (read-stream ,stream)))

(defmacro match-type/stream (stream type var)
  `(when (typep (peek-stream ,stream) ',type)
    ,(if (null var)
         `(read-stream ,stream)
         `(setq ,var (read-stream ,stream)))))

;;;-----------------------------------------------------------------------------
;;; Type Definitions - Hackish macros since CL doesn't have Mr. Curry.
;;;-----------------------------------------------------------------------------
(defmacro defatom (name args &rest body)
  "Match definition macro that provides a common lexical environment for matchers."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,(if args (list 'c '&aux args) '(c))
       (if c
	   (let ((c (if (characterp c) (char-code c) c)))
	     (declare (type (unsigned-byte 8) c))
	     ,@body)))
     (deftype ,name ()
       `(satisfies ,',name))))

;;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
(defatom bit? ()
  (or (= c 0)
      (= c 1)))

(defatom octet? ()
  (> c -1) (< c 256))

(defatom char? ()
  (and (> c -1) (< c 128)))

(defatom upalpha? ()
  (and (> c 64) (< c 91)))

(defatom loalpha? ()
  (and (> c 96) (< c 123)))

(defatom alpha? ()
  (or (loalpha? c)
      (upalpha? c)))

(defatom digit? ()
  (and (> c 47) (< c 58)))

(defatom alphanum? ()
  (or (alpha? c) (digit? c)))

(defatom control? ()
  (or (= c 127) (and (> c -1) (< c 32))))

(defatom carriage-return? ()
  (= c 13))

(defatom linefeed? ()
  (= c 10))

(defatom space? ()
  (= c 32))

(defatom tab? ()
  (= c 9))

(defatom double-quote? ()
  (= c 34))

(defatom return? ()
  (= c 13))

(defatom white-space? ()
  (or (space? c)
      (tab? c)))

(defatom visible-char? ()
  (and (> c 32)
       (< c 127)))

(defatom hex-upchar? ()
  (and (> c 64) (< c 71)))

(defatom hex-lochar? ()
  (and (> c 96) (< c 103)))

(defatom hex-char? ()
  (or (hex-upchar? c) (hex-lochar? c)))

(defatom hex? ()
  (or (digit? c) (hex-char? c)))

;;;-----------------------------------------------------------------------------
;;; Grammer
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +parser-rules+ (make-hash-table :test #'equal)) ;; Rule or not
  (defvar +checkpoint+ nil)) ;; used for backtracking
  
(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defun compile-parser-grammar (stream-var form
				 &optional (match-func 'match/stream)
				 (match-type-func 'match-type/stream))
    (labels
	((compile1 (form &optional (continue nil))
	   (cond
	     ((and (consp form) (keywordp (car form)))
	      (case (car form)
		((:and)
		 `(and ,@(compiled-subexprs form t)))
		((:or)
		 `(or ,@(compiled-subexprs form nil)))
		((:not)
		 `(not ,@(compiled-subexprs form continue))) 
		((:checkpoint)
		 (with-unique-names (checkpoint)
		   (let ((+checkpoint+ checkpoint)) 
		     `(block ,checkpoint
			(checkpoint-stream ,stream-var)
			,(compile1 `(:and ,@(cdr form)))
			(rewind-stream ,stream-var)
			,continue))))
		((:commit)
		 `(progn
		    (commit-stream ,stream-var)
		    (return-from ,+checkpoint+ ,continue)))
		((:rewind)
		 `(progn
		    (rewind-stream ,stream-var)
		    (return-from ,+checkpoint+ ,continue)))
		((:return)
		 `(let ((retval (multiple-value-list ,(cadr form))))
		    (cond
		      ((= (the fixnum (current-checkpoint ,stream-var)) (the fixnum cp)) nil)
		      ((< (the fixnum (current-checkpoint ,stream-var)) (the fixnum cp))
		       (error "This parser rule is not functional!"))
		      (t (do ((i (current-checkpoint ,stream-var)
				 (current-checkpoint ,stream-var)))
			     ((= (the fixnum i) (the fixnum cp)) nil)
			   (commit-stream ,stream-var))))
		    (return-from rule-block (apply #'values retval))))
		((:rewind-return)
		 `(progn (rewind-stream ,stream-var)
			 ,(compile1 `(:return ,(cadr form)))))
		((:zero-or-more :zom)
		 `(not (do () ((not ,(compile1 `(:and ,@(cdr form)))))))) 
		((:if)
		 `(prog1 ,continue
		    (if ,(cadr form) 
			,(compile1 (caddr form))			 
			,(if (cadddr form)
			     (compile1 (cadddr form))))))
		((:cond)
		 `(prog1 ,continue
		    (cond
		      ,@(mapcar #'(lambda (atom)
				    (list (car atom)
					  (compile1 `(:and ,@(cdr atom)))))
				(cdr form)))))
		((:sequence-case-sensitive :scs :sequence :seq)
		 (if (stringp (cadr form))
		     (compile1
		      `(:checkpoint
			,@(nreverse
			   (reduce #'(lambda (acc atom)
				       (cons atom acc))
				   (cadr form) :initial-value nil))
			(:commit)))
		     (compile1
		      `(:checkpoint
			(:do
			 (reduce #'(lambda (acc atom)
				     (declare (ignore acc))
				     (when (not (,match-func ,stream-var atom))
				       (rewind-stream ,stream-var)
				       (return-from ,+checkpoint+ nil)))
				 ,(cadr form) :initial-value nil))
			(:commit)))))
		((:sequence-case-insensitive :sci)
		 (if (stringp (cadr form))
		     (compile1
		      `(:checkpoint
			,@(nreverse
			   (reduce #'(lambda (acc atom)
				       (let ((upcase (char-upcase atom))
					     (downcase (char-downcase atom)))
					 (if (eq upcase downcase)
					     (cons atom acc)
					     (cons (list ':or upcase downcase)
						   acc))))
				   (cadr form) :initial-value nil))
			(:commit)))
		     (compile1
		      `(:checkpoint
			(:do
			 (reduce #'(lambda (acc atom)
				     (declare (ignore acc))
				     (when (not (or (,match-func ,stream-var (char-upcase atom))
						    (,match-func ,stream-var (char-downcase atom))))
				       (rewind-stream ,stream-var)
				       (return-from ,+checkpoint+ nil)))
				 ,(cadr form) :initial-value nil))
			(:commit)))))
		((:empty) t)
		((:type)
		 (assert (null (cdddr form)))	       
		 `(,match-type-func ,stream-var ,(cadr form) ,(caddr form)))
		((:do)
		 (assert (not (null (cdr form))))
		 `(progn ,@(cdr form)
			 ,continue))
		((:collect)
		 (assert (null (cdddr form)))
		 `(or (push-atom ,(cadr form) ,(caddr form)) t))				
		((:debug)
		 `(or (format t "current char-code:~D~%" (let ((c (peek-stream ,stream-var)))
							   (if (characterp c)
							       (char-code c)
							       c)))
		      ,continue))
		((:current)
		 `(progn (describe ,stream-var) ,continue))
		(t
		 (if (< (length (cdr form)) 1)
		     `(,(intern (symbol-name (car form))) ,stream-var)
		     `(multiple-value-setq ,(cdr form) 
			(,(intern (symbol-name (car form))) ,stream-var))))))
;; 		 (if (gethash (car form) +parser-rules+)
;; 		     (if (< (length (cdr form)) 1)
;; 			 `(,(gethash (car form) +parser-rules+) ,stream-var)
;; 			 `(multiple-value-setq ,(cdr form) (,(gethash (car form) +parser-rules+) ,stream-var)))
;; 		     (error "No rule associated with:~A" (car form))))))
	   (t
	    `(,match-func ,stream-var ,form))))
      (compiled-subexprs (form &optional (continue t))
			 (mapcar #'(lambda (f) (compile1 f continue)) (cdr form))))
    (compile1 form))))

;;;-----------------------------------------------------------------------------
;;; Rules
;;;-----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defrule (name args &rest body)
    "Rule definition macro that provides a common lexical environment for rules."
    (with-unique-names (stream)
      (flet ((rule-args ()
	       (if args
		   (list* stream '&aux args)
		   (list stream))))
	(setf (gethash (make-keyword name) +parser-rules+) name)
	`(defun ,name ,(rule-args)
	   (block rule-block
	     (let ((cp (current-checkpoint ,stream)))
	       (declare (ignorable cp))
	       ,(compile-parser-grammar stream `(:checkpoint ,@body))
	       nil)))))))

(defrule crlf? ()
  (:or (:and #\Return #\Newline)
       #\Newline)
  (:return t))

(defrule lwsp? ()  
  (:zom (:type (or space? tab? carriage-return? linefeed?)))
  (:return t))

(defrule hex-value? (a b)  
  (:or (:and (:type digit? a)
	     (:do (setq a (- (the (unsigned-byte 8) a) 48))))
       (:and (:type hex-upchar? a)
	     (:do (setq a (+ 10 (- (the (unsigned-byte 8) a) 65)))))
       (:and (:type hex-lochar? a)
	     (:do (setq a (+ 10 (- (the (unsigned-byte 8) a) 97))))))
  (:or (:and (:type digit? b)
	     (:do (setq b (- (the (unsigned-byte 8) b) 48))))
       (:and (:type hex-upchar? b)
	     (:do (setq b (+ 10 (- (the (unsigned-byte 8) b) 65)))))
       (:and (:type hex-lochar? b)
	     (:do (setq b (+ 10 (- (the (unsigned-byte 8) b) 97))))))
  (:return (+ (* (the (unsigned-byte 8) a) 16) (the (unsigned-byte 8) b))))

;;       escaped       = "%" hex hex
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defrule escaped? (hex)  
    (:and #\% (:hex-value? hex) (:return hex))))

(defrule digit-value? (d)
  (:and (:type digit? d)
	(:return (- (the (unsigned-byte 8) d) 48))))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defrule fixnum? ((acc (make-accumulator)) c)
    (:type digit? c) (:collect c acc)
    (:zom (:type digit? c)
	  (:collect c acc))
    (:return (parse-integer acc))))

(defrule float? ((acc (make-accumulator)) c)
  (:and (:zom (:type digit? c) (:collect c acc))
	(:and #\. (:collect #\. acc))
	(:and (:type digit? c) (:collect c acc))
	(:zom (:type digit? c) (:collect c acc)))
  (:return  acc))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defrule version? (version d)
    (:fixnum? d) (:do (push d version))
    (:zom #\. (:fixnum? d) (:do (push d version)))
    (:return (nreverse version))))

(defrule quoted? ((value (make-accumulator :byte)) c)
  (:or
   (:and
    #\"
    (:zom (:or
	   (:checkpoint
	    (:and #\\ #\"
		  (:do (push-atom #\" value))
		  (:commit)))
	   (:and #\" (:return (octets-to-string value :utf-8)))
	   (:and (:type (or visible-char? space?) c)
		 (:collect c value)))))
   (:and (:zom (:type (or visible-char? space?) c)
	       (:collect c value))
	 (:return (octets-to-string value :utf-8)))))

(defrule split-by-line (c (acc (make-accumulator)) lst)
  (:zom (:or (:and #\Newline (:do (push acc lst)
				  (setq acc (make-accumulator))))
	     (:and (:type octet? c) (:collect c acc))))
  (:return lst))

(defrule split-by-space (c (acc (make-accumulator)) lst)
  (:lwsp?)
  (:zom (:or (:and #\Space (:do (push acc lst)
				(setq acc (make-accumulator))))
	     (:and (:type octet? c) (:collect c acc))))
  (:do (if (> (length acc) 0 ) (push acc lst)))
  (:return (reverse lst)))

(defun byte! (stream byte)
  (write-stream stream byte))

(defun char! (stream char)
  (let ((code (char-code char)))
    (if (> code 127) 
	(reduce #'(lambda (acc atom)
		    (declare (ignore acc))
		    (byte! stream atom)
		    nil)
		(string-to-octets (string char) :utf-8) :initial-value nil)
	(byte! stream code))))

(defun string! (stream string)
  (reduce #'(lambda (acc atom)
	      (declare (ignore acc))
	      (byte! stream atom)
	      nil)
	  (string-to-octets string :utf-8) :initial-value nil))

(defun version! (stream version)
  (fixnum! stream (car version))
  (reduce #'(lambda (acc atom)
	      (declare (ignore acc))
	      (char! stream #\.)
	      (fixnum! stream atom)
	      nil)
	  (cdr version) :initial-value nil))

(defun symbol! (stream symbol)
  (string! stream (symbol-name symbol)))

(defun fixnum! (stream number)
  (string! stream (format nil "~D" number)))

(defun quoted! (stream string)
  (char! stream #\")
  (reduce #'(lambda (acc atom)
	      (declare (ignore acc))
	      (cond
		((or (char= #\" atom) (char= #\\ atom))
		 (char! stream #\\) (char! stream atom))
		((or (char= #\Newline atom) (char= #\Linefeed atom))
		 )
		(t (char! stream atom)))
	      nil)
	  string
	  :initial-value nil)
  (char! stream #\"))

(defun quoted-fixnum! (stream number)
  (quoted! stream (format nil "~D" number)))

(defvar +hex-alphabet+
  #.(reduce #'(lambda (acc atom) (push-atom (char-code atom) acc) acc)
	    "0123456789ABCDEF" :initial-value (make-accumulator :byte)))

(defun hex-value! (stream hex)
  (byte! stream (aref +hex-alphabet+ (floor (/ hex 16))))
  (byte! stream (aref +hex-alphabet+ (rem hex 16))))

;; (defrule abc? (header abc)
;;   (:cond
;;     ((string= "abc" header)
;;      (:zom (:type visible-char?))
;;      (:do (setq abc 1)))
;;     (t
;;      (:return 1))))
;; (defrule abc? ()
;;   #\1 #\2 #\6
;;   (:return (list 1 2)))

;;(defparameter *gee (format nil  "TEXTEE~CTEXTBB~C" #\Newline #\Newline))
;;(defparameter *gee (format nil  "~C~C~C~Ca" #\Return #\Linefeed #\Space #\Tab))
;;(defparameter *gee (format nil  "~C~C" #\Space #\Tab))
;;(defparameter *gee (format nil  "A7"))
;; (defrule text-newline (c (text (make-accumulator)) (text2 (make-accumulator)))
;;   (:and (:zom (:type alpha? c)
;; 	      (:collect c text))
;; 	(:type linefeed?))
;;   (:debug)
;;   (:and (:zom (:type alpha? c)
;; 	      (:collect c text2))
;; 	(:type linefeed?))
;;   (:return (values text text2)))

;; (defmacro match (&rest matchers)
;;   "Attempts to match the next input character with one of the supplied matchers."
;;   `(and
;;     (or ,@(loop for m in matchers
;; 	     collect `(match-atom ,m s)))
;;     ;; cheat here a little bit - eat entire char entity instead
;;     ;; of peeked char
;;     (eat)))

;; (defmacro match-seq (&rest sequence)
;;   "Tries to match the supplied matchers in sequence with characters in the input stream."
;;   `(and ,@(loop for s in sequence
;;                 collect `(match ,s))))

;; (defmacro match* (&rest sequence)
;;   "Matches any occurances of any of the supplied matchers."
;;   `(loop with data = (make-accumulator (stream-type s))
;;       for c = (match ,@sequence)
;;       while c
;;       do (push-atom c data)
;;       finally (return data)))

;; (defmacro peek (&rest matchers)
;;   "Looks ahead for an occurance of any of the supplied matchers."
;;   `(or ,@(loop for m in matchers
;; 	      collect `(match-atom ,m s))))

;; (defmacro match+ (&rest sequence)
;;   "Matches one or more occurances of any of the supplied matchers."
;;   `(and (peek ,@sequence)
;;     (match* ,@sequence)))

;; (defmacro must ((error-type &rest error-args) &rest body)
;;   "Throws a parse error if the supplied forms do not succeed."
;;   `(or (progn ,@body)
;;        (error ',error-type ,@error-args)))

;;;-----------------------------------------------------------------------------
;;; PUBLIC INTERFACE
;;;-----------------------------------------------------------------------------
;; (defun parse (s &key uri-to-package)
;;   "Parses the supplied stream or string into a lisp node tree."
;;   (declare (special uri-to-package))
;;   (let ((*uri-to-package* uri-to-package)
;; 	(stream (etypecase s
;; 		  (string (make-string-input-stream s))
;; 		  (stream s))))
;;     (declare (special *uri-to-package*))
;;     (handler-bind ((xml-parse-error (lambda (c)
;;                                       (unless (offset c)
;;                                         (setf (offset c) (file-position stream))))))
;;       (document (make-state :stream stream)))))
;; (defstruct state
;;   "Represents parser state.  Passed among rules to avoid threading issues."  
;;   stream)

;; ;;;-----------------------------------------------------------------------------
;; ;;; CONDITIONS
;; ;;;-----------------------------------------------------------------------------
;; (define-condition server-parse-error (error) 
;;   ((offset :accessor offset :initform nil :initarg :offset)
;;    (message :accessor message :initform nil :initarg :message))
;;   (:report (lambda (c s)
;;              (if (message c)
;;                  (format s (message c))
;;                  (format s "PARSE-ERROR at offset ~D." (offset c))))))

;; Copyright (c) 2003, Miles Egan
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;
;;     * The name of the author may not be used to endorse or promote
;;       products derived from this software without specific prior
;;       written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; $Id: xmls.lisp 447 2003-08-29 04:40:29Z miles $

;;;; * Miles Egan's XMLS

;; (defmacro matchfn (name)
;;   "Convenience macro for creating an anonymous function wrapper around a matcher macro."
;;   `(lambda (s) (match ,name)))

;; (defun zom (s func)
;;   "Collects any matches of the supplied rule with the input stream."
;;   (declare (type function func))
;;   (let ((val (funcall func s)))
;;     (if val
;;         (multiple-value-bind (res nextval)
;;             (none-or-more s func)
;;           (values res (cons val nextval)))
;;         (values t nil))))

;; (defun oom (s func)
;;   "Collects one or more matches of the supplied rule with the input stream."
;;   (declare (type function func))
;;   (let ((val (funcall func s)))
;;     (if val
;;         (multiple-value-bind (res nextval)
;;             (none-or-more s func)
;;           (declare (ignore res))
;;           (cons val nextval))
;;         nil)))

;;;-----------------------------------------------------------------------------
;;; MATCHERS
;;;-----------------------------------------------------------------------------
;; (defmatch digit ()
;;   (and c (digit-char-p c)))

;; (defmatch letter ()
;;   (and c (alpha-char-p c)))

;; (defmatch ws-char ()
;;   (case c
;;     ((#\Newline #\Space #\Tab #\Return . #.(unless (char= #\Newline #\Linefeed) (list #\Linefeed))) t)
;;     (t nil)))

;; (defmatch namechar ()
;;   (or 
;;    (and c (alpha-char-p c))
;;    (and c (digit-char-p c))
;;    (case c
;;      ((#\. #\- #\_ #\:) t))))

;; (defmatch ncname-char ()
;;   (or 
;;    (and c (alpha-char-p c))
;;    (and c (digit-char-p c))
;;    (case c
;;      ((#\. #\- #\_) t))))

;; (defmatch attr-text-dq ()
;;   (and c
;;        (case c
;;          ((#\< #\") nil)
;;          (t t))))

;; (defmatch attr-text-sq ()
;;   (and c
;;        (case c
;;          ((#\< #\') nil)
;;          (t t))))

;; (defmatch chardata ()
;;   (and c (not (char= c #\<))))

;; (defmatch comment-char ()
;;   (and c (not (eql c #\-))))

;;;-----------------------------------------------------------------------------
;;; RULES
;;;-----------------------------------------------------------------------------
;; (defrule ncname ()
;;   (and (peek letter #\_)
;;        (match+ ncname-char)))

;; (defrule qname ()
;;   (let (name suffix)
;;     (and
;;      (setf name (ncname s))
;;      (or
;;       (and
;;        (match #\:)
;;        (setf suffix (ncname s)))
;;       t))
;;     (values name suffix)))

;; (defrule attr-or-nsdecl ()
;;   (let (suffix name val)
;;     (and
;;      (setf (values name suffix) (qname s))
;;      (or
;;       (and
;;        (match #\=)
;;        (or
;;         (and
;;          (match #\")
;;          (setf val (match* attr-text-dq))
;;          (match #\"))
;;         (and
;;          (match #\')
;;          (setf val (match* attr-text-sq))
;;          (match #\'))))
;;       t)
;;      (if (string= "xmlns" name)
;;          (cons 'nsdecl (cons suffix val)) 
;;          (list
;;           'attr 
;;           (if suffix
;;               (cons suffix name)
;;               (cons name nil))
;;           val)))))

;; (defrule ws ()
;;   (and (match+ ws-char)
;;        (make-element :type 'whitespace :val nil)))

;; (defrule name ()
;;   (and
;;    (peek namechar #\_ #\:)
;;    (match* namechar)))

;; (defrule ws-attr-or-nsdecl ()
;;   (and
;;    (ws s)
;;    (attr-or-nsdecl s)))

;; (defrule start-tag ()
;;   (let (name suffix attrs nsdecls)
;;     (and
;;      (peek namechar)
;;      (setf (values name suffix) (qname s))
;;      (multiple-value-bind (res a)
;;          (none-or-more s #'ws-attr-or-nsdecl)
;;        (mapcar (lambda (x) (if (eq (car x) 'attr)
;;                                (setf attrs (nconc (cdr x) attrs))
;;                                (push (cdr x) nsdecls)))
;;                a)
;;        res)
;;      (or (ws s) t)
;;      (values
;;       (make-node
;;        :name (or suffix name)
;;        :ns (and suffix name)
;;        :attrs attrs)
;;       nsdecls))))

;; (defrule end-tag ()
;;   (let (name suffix)
;;     (and
;;      (match #\/)
;;      (setf (values name suffix) (qname s))
;;      (or (ws s) t)
;;      (match #\>)
;;      (make-element :type 'end-tag :val (intern-xml-name (if suffix
;;                                                             suffix
;;                                                             name)
;;                                                         (if suffix
;;                                                             name
;;                                                             nil)
;;                                                         (state-nsstack s))))))

;; (defrule comment ()
;;   (and
;;    (match-seq #\! #\- #\-)
;;    (progn
;;      (loop until (match-seq #\- #\- #\>)
;;            do (eat))
;;      t)
;;    (make-element :type 'comment)))

;; (defrule comment-or-cdata ()
;;   (and
;;    (peek #\!)
;;    (must (xml-parse-error :offset (file-position (state-stream s)))
;;          (or (comment s)
;;              (and
;;               (match-seq #\[ #\C #\D #\A #\T #\A #\[)
;;               (loop with data = (make-extendable-string 50)
;;                  with state = 0
;;                  do (case state
;;                       (0 (if (match #\])
;;                              (incf state)
;;                              (push-string (eat) data)))
;;                       (1 (if (match #\])
;;                              (incf state)
;;                              (progn 
;;                                (setf state 0)
;;                                (push-string #\] data)
;;                                (push-string (eat) data))))
;;                       (2 (if (match #\>)
;;                              (incf state)
;;                              (progn 
;;                                (setf state 0)
;;                                (push-string #\] data)
;;                                (push-string #\] data)
;;                                (push-string (eat) data)))))
;;                  until (eq state 3)
;;                  finally (return (make-element :type 'cdata :val data))))))))

;; (declaim (ftype function element))     ; forward decl for content rule
;; (defrule content ()
;;   (if (match #\<)
;;       (must (xml-parse-error :offset (file-position (state-stream s)))
;;             (or (comment-or-cdata s)
;;                 (element s)
;;                 (end-tag s)))
;;       (or (let (content)
;;             (and (setf content (match+ chardata))
;;                  (make-element :type 'data :val content))))))

;; (defrule element ()
;;   (let (elem children nsdecls end-name)
;;     (and
;;      ;; parse front end of tag
;;      (multiple-value-bind (e n)
;;          (start-tag s)
;;        (setf elem e)
;;        (setf nsdecls n)
;;        e)
;;      ;; resolve namespaces *before* parsing children
;;      (if nsdecls
;;          (push nsdecls (state-nsstack s))
;;          t)
;;      (resolve-namespace elem (state-nsstack s))
;;      ;; parse end-tag and children
;;      (or
;;       (match-seq #\/ #\>)
;;       (and
;;        (match #\>)
;;        (loop for c = (content s)
;;              while c
;;              do (etypecase c
;;                   (element (case (element-type c)
;;                              ('end-tag
;;                               (return (setf end-name (element-val c))))
;;                              (t (if (element-val c)
;;                                     (push (element-val c) children)))))))
;;        (or (eql (node-name elem) end-name)
;;            (error 'unmatched-end-tag :found end-name :expected (node-name elem)))))
;;      ;; package up new node
;;      (progn
;;        (setf (node-children elem) (nreverse children))
;;        (make-element :type 'elem :val elem)))))

;; (defrule processing-instruction-or-xmldecl ()
;;   (let (name)
;;     (and
;;      (match #\?)
;;      (setf name (name s))
;;      (none-or-more s #'ws-attr-or-nsdecl)
;;      (match-seq #\? #\>)
;;      (make-element :type 'pi :val name))))

;; (defrule processing-instruction ()
;;   (let ((p (processing-instruction-or-xmldecl s)))
;;     (and p
;;          (not (string= (element-val p) "xml"))
;;          p)))

;; (defrule xmldecl ()
;;   (let ((p (processing-instruction-or-xmldecl s)))
;;     (and p
;;          (string= (element-val p) "xml")
;;          p)))

;; (defrule comment-or-doctype ()
;;   ;; skip dtd - bail out to comment if it's a comment
;;   ;; only match doctype once
;;   (and
;;    (peek #\!)
;;    (or (comment s)
;;        (and (not (state-got-doctype s))
;;             (must (xml-parse-error :offset (file-position (state-stream s)))
;;                   (match-seq #\D #\O #\C #\T #\Y #\P #\E))
;;             (loop with level = 1
;;                   do (case (eat)
;;                        (#\> (decf level))
;;                        (#\< (incf level)))
;;                   until (eq level 0)
;;                   finally (return t))
;;             (setf (state-got-doctype s) t)
;;             (make-element :type 'doctype)))))

;; (defrule misc ()
;;   (or 
;;    (ws s)
;;    (and (match #\<) (must (xml-parse-error :offset (file-position (state-stream s)))
;;                           (or (processing-instruction s)
;;                               (comment-or-doctype s)
;;                               (element s))))))

;; (defrule document ()
;;   (let (elem)
;;     (if (match #\<)
;;         (must (xml-parse-error :offset (file-position (state-stream s)))
;;               (or (processing-instruction-or-xmldecl s)
;;                   (comment-or-doctype s)
;;                   (setf elem (element s)))))
;;     (unless elem
;;       (loop for c = (misc s)
;;             while c do (if (eql (element-type c) 'elem)
;;                            (return (setf elem c)))))
;;     (and elem (element-val elem))))
