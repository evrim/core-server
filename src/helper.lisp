(in-package :tr.gen.core.server)

(defmacro s-v (slot-name)
  "slot-value self slot-name"
  `(slot-value self ,slot-name))

(defmacro seq (how-many)
  "(seq how-many) => (0 1 2 .. how-many-1)"
  `(let ((result))
     (dotimes (n ,how-many (nreverse result))
       (push n result))))

(defun load-file-into-string (pathname)
  (iter (for l in-file pathname using #'read-line)
	(reducing l by #'(lambda (acc elem)
			   (concatenate 'string acc elem (format nil "~%"))))))

(defun fix-apache-permissions (pathname)
  (sb-ext:run-program +sudo+ (cons (namestring +chown+)
				   (list (format nil "~A:~A" +apache-user+ +apache-group+)					   
					 (namestring pathname))))
  (sb-ext:run-program +sudo+ (cons (namestring +chmod+)
				   (list "660" (namestring pathname)))))


;;; http://paste.lisp.org/display/9527
;;; greetz goes to andreas
(defmacro with-current-directory (dir &body body)
  `(unwind-protect (progn
                     (sb-posix:chdir ,dir)
                     (let ((*default-pathname-defaults* ,dir))
                       ,@body))
     (sb-posix:chdir *default-pathname-defaults*)))

(defmacro make-project-path (system path)
  `(merge-pathnames (make-pathname :directory '(:relative ,path))
                    (asdf:component-pathname (asdf:find-system ,system))))

;; DNS aids
(defun host-part (fqdn)
  (awhen (position #\. fqdn)
    (subseq fqdn 0 it)))

(defun domain-part (fqdn)
  (awhen (position #\. fqdn)
    (subseq fqdn (+ 1 it))))

(defmacro with-package (package &body b0dy)  
  `(let ((*package* (find-package ,package)))
     ,@b0dy))

(defparameter +day-names+ '((:tr "Pazartesi" "Salı" "Çarşamba" "Perşembe" "Cuma" "Cumartesi" "Pazar")))
(defparameter +month-names+ '((:tr "Ocak" "Şubat" "Mart" "Nisan" "Mayıs" "Haziran" "Temmuz"
			       "Ağustos" "Eylül" "Ekim" "Kasım" "Aralık")))

(defun time->string (time &optional mode (lang :tr))
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore tz dst-p))
    (case mode
      (:long (format nil "~2,'0d ~a ~d ~a, ~2,'0d:~2,'0d:~2,'0d"
		     day (nth (decf month) (rest (assoc lang +month-names+)))
		     year (nth day-of-week (rest (assoc lang +day-names+))) hour minute second))
      (t (format nil "~2,'0d/~2,'0d/~d ~2,'0d:~2,'0d" day month year hour minute)))))

(defun string-replace-all (old new big)
  "Replace all occurences of OLD string with NEW
string in BIG string."
  (do ((newlen (length new))
       (oldlen (length old))
       (i (search old big)
          (search old big :start2 (+ i newlen))))
      ((null i) big)
    (setq big
	  (concatenate 'string
		       (subseq big 0 i)
		       new
		       (subseq big (+ i oldlen))))))

(defmethod make-keyword ((str string))
  (intern (string-upcase str) :keyword))

(defmethod make-keyword ((sym symbol))
  (intern (symbol-name sym) :keyword))

#+nil
(progn
(defvar +tr-alphabet+
  "ABCÇDEFGĞHIİJKLMNOÖPQRSŞTUÜVWXYZabcçdefgğhıijklmnoöpqrsştuüvwxyz")

(defun tr-char-upcase (a)
  (case a
    ((#\ı #\i)
     (aref +tr-alphabet+ (- (position a +tr-alphabet+) 32)))
    (otherwise (char-upcase a))))

(defun tr-char-downcase (a)
  (case a
    ((#\I #\İ)
     (aref +tr-alphabet+ (+ (position a +tr-alphabet+) 32)))
    (otherwise (char-downcase a))))

(defun tr-string-upcase (str)
  (reduce #'(lambda (acc item) (vector-push-extend (tr-char-upcase item) acc) acc)
	  str
	  :initial-value (make-array 0 :fill-pointer 0 :element-type 'character)))

(defun tr-string-downcase (str)  
  (reduce #'(lambda (acc item) (vector-push-extend (tr-char-downcase item) acc) acc)
	  str
	  :initial-value (make-array 0 :fill-pointer 0 :element-type 'character)))

(defun tr-char< (a b) 
  (if (eq a b)
      nil
      (let* ((posa (position a +tr-alphabet+))
	     (posb (position b +tr-alphabet+)))
	(if (and posa posb)
	    (> posa posb)
	    (char< a b)))))

(defun tr-char> (a b)
  (if (eq a b)
      nil
      (not (tr-char< a b))))

;; (and
;;   (tr-string< "aycan" "AYCAN")
;;   (tr-string< "aYcaN" "Aycan")
;;   (tr-string< "aycan" "aycaN")
;;   (equal (sort (list "ABC" "Abc" "abc" "aBC") #'string<)
;;          (tr-sort (list "ABC" "Abc" "abc" "aBC") #'tr-string>)))

(defun tr-string< (a b) 
  (let ((len (min (length a) (length b)))
  	(result nil))
    (dotimes (i len)
      (if (tr-char< (aref a i) (aref b i))
	  (return-from tr-string< t))
      (if (tr-char< (aref b i) (aref a i))
  	  (return-from tr-string< nil)))
    nil))

(defun tr-string> (a b)
  (not (tr-string< a b)))

(defun tr-sort (list test &key (key #'identity)) 
  (sort (copy-list list)
	(lambda (a b)
	  (funcall test a b))
	:key key))
)