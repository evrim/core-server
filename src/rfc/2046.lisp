(in-package :tr.gen.core.server)

;;;-----------------------------------------------------------------------------
;;; RFC 2046 - Multipurpose Internet Mail Extensions (MIME)
;;; Part Two: Media Types
;;;-----------------------------------------------------------------------------
(defclass mime ()
  ((headers :accessor mime.headers :initarg :headers :initform nil)))

(defmethod mime.header ((mime mime) name-or-symbol)
  (cdr
   (assoc (if (stringp name-or-symbol)
	      (intern (string-upcase name-or-symbol))
	      name-or-symbol)
	  (mime.headers mime) :test #'string=
	  :key #'(lambda (k) (if (symbolp k) k (string-upcase k))))))

(defmethod (setf mime.header) (value (mime mime) name-or-symbol)  
  (if (mime.header mime name-or-symbol)
      (setf (cdr
	     (assoc (if (stringp name-or-symbol)
			(intern (string-upcase name-or-symbol))
			name-or-symbol)
		    (mime.headers mime) :test #'string=
		    :key #'(lambda (k) (if (symbolp k) k (string-upcase k)))))
	    value)
      (setf (mime.headers mime) (cons (cons name-or-symbol value)
				      (mime.headers mime))))
  value)

(defclass top-level-media (mime)
  ((data :accessor mime.data :initarg :data :initform nil)))

(defmethod mime.children ((mime top-level-media)) nil)

(defun make-top-level-media (headers data)
  (make-instance 'top-level-media :headers headers :data data))

(defclass composite-level-media (mime)
  ((children :accessor mime.children :initarg :children :initform nil)))

(defun make-composite-level-media (headers children)
  (make-instance 'composite-level-media :headers headers :children children))

(defun mime-search (root-mime-or-mimes goal-p &key (succ #'mime.children))
  (core-search (if (listp root-mime-or-mimes)
		   root-mime-or-mimes
		   (list root-mime-or-mimes))
	       goal-p succ #'(lambda (x y) (append y x))))

(defatom mime-boundary-char? ()
  (and (or (visible-char? c) (space? c)) (not (eq c #.(char-code #\-)))))

(defvar +mime-boundary+ nil)

(defrule mime-boundary? (c (boundary (make-accumulator)) last)
  (:zom (:or (:checkpoint
	      #\- #\-
	      (:if +mime-boundary+
		   (:and (:seq +mime-boundary+) (:do (setq boundary +mime-boundary+)))
		   (:and
		    (:zom #\- (:collect #\- boundary))
		    (:type mime-boundary-char? c) (:collect c boundary)
		    (:zom (:type mime-boundary-char? c) (:collect c boundary))))
	      (:zom #\- (:do (setq last t)))
	      (:if (> (length boundary) 0)
		   (:return (values boundary last))
		   (:commit)))
	     (:and (:type octet?)))))

(defrule mime-headers? ((headers '()) stub c
		        (key (make-accumulator)) (value (make-accumulator)))
  (:zom (:or (:and (:rfc2045-content-type? stub)
		   (:do (push (cons 'content-type stub) headers)))
 	     (:and (:rfc2045-content-transfer-encoding? stub)
		   (:do (push (cons 'transfer-encoding stub) headers)))
 	     (:and (:rfc2045-content-id? stub)
		   (:do (push (cons 'id stub) headers)))
 	     (:and (:rfc2045-content-description? stub)
		   (:do (push (cons 'description stub) headers)))
	     (:checkpoint
	      (:sci "content-")
	      (:zom (:not #\:) (:type visible-char? c) (:collect c key))
	      (:lwsp?)
	      (:zom (:type (or visible-char? space?) c) (:collect c value))
	      (:do (push (cons (string-downcase key) value) headers)
		   (setq key (make-accumulator) value (make-accumulator)))
	      (:commit))) 
	(:crlf?))
  (:crlf?)
  (:return headers))

(defrule mime-binary-data? (c (acc (make-accumulator :byte)))
  (:if (null +mime-boundary+)
       (:return nil)) ;; must have some binary to match to end
  (:zom (:or (:checkpoint
	      (:crlf?) #\- #\-
	      (:seq +mime-boundary+)
	      (:rewind-return acc))
	     (:and (:type octet? c)
		   (:collect c acc)))))

(setf (gethash :mime? +parser-rules+) 'mime?)
(defun mime? (stream &aux data headers)
  (checkpoint-stream stream)
  (setq headers (mime-headers? stream))
  (cond
    ((and (string= "multipart" (cadr (assoc 'content-type headers)))
	  (or (string= "mixed" (caddr (assoc 'content-type headers)))
	      (string= "alternative" (caddr (assoc 'content-type headers)))))
     (make-composite-level-media headers
      (mimes? stream
	      (cdr (assoc "boundary"
			  (cadddr (assoc 'content-type headers :test #'string=))
			  :test #'string=)))))
    (t	   
     (case (cdr (assoc 'transfer-encoding headers))
       (quoted-printable (setq data (quoted-printable? stream)))
       (base64 (setq data (base64? stream)))
       (t (setq data (mime-binary-data? stream))))
     (if (and (null headers) (null data))
	 (progn
	   (rewind-stream stream)
	   nil)
	 (prog1 (make-top-level-media headers data)
	   (commit-stream stream))))))

(setf (gethash :mimes? +parser-rules+) 'mimes?)
(defun mimes? (stream &optional (boundary nil) &aux last mimes)
  (flet ((rew-ret (var)
	   (when (not var)
	     (rewind-stream stream)
	     (return-from mimes? nil))))
    (checkpoint-stream stream)
    (let ((+mime-boundary+ boundary))
      (multiple-value-setq (boundary last) (mime-boundary? stream))
      (rew-ret boundary) (rew-ret (not last))
      (crlf? stream)
      (let ((+mime-boundary+ boundary) (last nil))
	(do ((mime (mime? stream) (mime? stream)))
	    (nil nil)
	  (when (null mime)
	    (rew-ret mimes)
	    (commit-stream stream)
	    (return-from mimes? (nreverse mimes)))
	  (push mime mimes)
	  (lwsp? stream)
	  (multiple-value-setq (boundary last) (mime-boundary? stream))
	  (rew-ret boundary)
	  (when last
	    (commit-stream stream)
	    (return-from mimes? (nreverse mimes)))
	  (crlf? stream)))
      (rewind-stream stream)
      nil)))

;; tests are in rfc 2388.