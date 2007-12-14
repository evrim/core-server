(in-package :tr.gen.core.server)

(defun any (lambda list)
  "i wonder why missing."
  (reduce #'(lambda (acc atom)
	      (aif (funcall lambda atom)
		   (return-from any it)
		   acc))
	  list :initial-value nil))

(defmacro s-v (slot-name)
  "slot-value self slot-name"
  `(slot-value self ,slot-name))

(defmacro seq (how-many)
  "(seq how-many) => (0 1 2 .. how-many-1)"
  `(let ((result))
     (dotimes (n ,how-many (nreverse result))
       (push n result))))

(defun drop (n lst)
  (subseq lst n))

(defun take (n lst)
  (subseq lst 0 n))

(defun load-file-into-string (pathname)
  (reduce #'(lambda (acc atom)
	      (concatenate 'string acc atom (format nil "~%")))
	  (with-open-file (s pathname :direction :input)
	    (loop
	       for line = (read-line s nil 'end)
	       until (eq line 'end)
	       collect line)) :initial-value "")
  ;; (iter (for l in-file pathname using #'read-line)
  ;; 	(reducing l by #'(lambda (acc elem)
  ;; 			   (concatenate 'string acc elem (format nil "~%")))))
  )

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
      (:short (format nil "~2,'0d/~2,'0d ~2,'0d:~2,'0d" month day hour minute))
      (:long (format nil "~2,'0d ~a ~d ~a, ~2,'0d:~2,'0d:~2,'0d"
		     day (nth (decf month) (rest (assoc lang +month-names+)))
		     year (nth day-of-week (rest (assoc lang +day-names+))) hour minute second))
      (t (format nil "~2,'0d/~2,'0d/~d ~2,'0d:~2,'0d" day month year hour minute)))))

(defun time->ymd (time)
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore tz dst-p))
    (format nil "~d-~2,'0d-~2,'0d" year month day)))

(defun time->dmy (time)
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore tz dst-p))
    (format nil "~2,'0d-~2,'0d-~d" day month year)))

(defun time->hm (time)
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore tz dst-p))
    (format nil "~2,'0d:~2,'0d" hour minute)))

(defun hm->time (hm-str)
  (cond
    ((null hm-str) nil)
    (t (let ((val (cl-ppcre:split #\: hm-str)))
	 (apply #'encode-universal-time
		(append (cons 0 (reverse (loop for i from 0 upto 1
					    collect (parse-integer (nth i val) :junk-allowed t))))
			(list 1 1 0)))))))

;; given seconds, return hours as string
(defun seconds->hours (seconds)
  (let* ((second-minute 60)
         (second-hour (* second-minute 60))
         (second-day (* second-hour 24))
         (second-month (* second-day 30.4368499))
         (second-year (* second-month 12))
         (year (truncate seconds second-year))
         (month (truncate (decf seconds (* year second-year)) second-month))
         (day (truncate (decf seconds (* month second-month)) second-day))
         (hour (truncate (decf seconds (* day second-day)) second-hour))
         (minute (truncate (decf seconds (* hour second-hour)) second-minute))
         (second (truncate (decf seconds (* minute second-minute)) 1)))
    (declare (ignorable second-minute) (ignorable second))
    (format nil "~D:~D:~D" day hour minute)))

(defun ymd->time (ymd)
  (cond
    ((null ymd) nil)
    (t (apply #'encode-universal-time
	      (append (list 0 0 0)
		      (mapcar #'(lambda (i)
				  (parse-integer i :junk-allowed t))
			      (reverse (cl-ppcre:split #\- ymd))))))))

(defun combine-date-time (date time)
  (multiple-value-bind (s1 min1 h1 d1 m1 y1 day-of-week-1 dst1 tz1) (decode-universal-time date)
    (multiple-value-bind (s2 min2 h2 d2 m2 y2 day-of-week-2 dst2 tz2) (decode-universal-time time)
      (encode-universal-time s2 min2 h2 d1 m1 y1))))

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

(defun escape-parenscript (string)
  (core-server::return-stream   
   (let ((input (make-core-stream string))
	 (output (make-core-stream "")))
     (do ((peek (read-stream input) (read-stream input)))
	 ((null peek) output)
       (cond
	 ((> peek 127) (core-server::byte! output peek))
	 ((not (alpha-char-p (code-char peek)))
	  (progn
	    (core-server::char! output #\%)
	    (core-server::hex-value! output peek)))	 	 
	 (t (core-server::byte! output peek)))))))

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