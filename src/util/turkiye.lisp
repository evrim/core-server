(in-package :core-server)

;;+----------------------------------------------------------------------------
;;| Utilities for Turkish Locale
;;+----------------------------------------------------------------------------
;;
;; These utilities are disabled by default. Remove #+nil if you need them.

#+nil
(progn
  (defvar +tr-alphabet+
    "ABCÇDEFGĞHIİJKLMNOÖPQRSŞTUÜVWXYZabcçdefgğhıijklmnoöpqrsştuüvwxyz")

  (defun tr2en (str)
    "Convert a turkish string to corresponding ascii string."
    (nsubstitute #\s #\ş str) (nsubstitute #\S #\Ş str)
    (nsubstitute #\i #\ı str) (nsubstitute #\I #\İ str)
    (nsubstitute #\g #\ğ str) (nsubstitute #\G #\Ğ str)
    (nsubstitute #\c #\ç str) (nsubstitute #\C #\Ç str)
    (nsubstitute #\u #\ü str) (nsubstitute #\U #\Ü str)
    (nsubstitute #\o #\ö str) (nsubstitute #\O #\Ö str)
    str)
  
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
