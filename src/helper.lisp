(in-package :tr.gen.core.server)

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