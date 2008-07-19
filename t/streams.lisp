(in-package :tr.gen.core.server.test)

(defun trace-core-streams ()
  (trace commit-stream rewind-stream read-stream peek-stream write-stream
	 checkpoint-stream))

(defun untrace-core-streams ()
  (untrace commit-stream rewind-stream read-stream peek-stream write-stream
	   checkpoint-stream))

;; test basic stream operations with a scenario
(deftest string-stream
    (with-core-stream (s "ABCDEF")
      (and (equal (peek-stream s) #.(char-code #\A))
	   (equal (peek-stream s) #.(char-code #\A))
	   (equal (read-stream s) #.(char-code #\A))
	   (equal (read-stream s) #.(char-code #\B))
	   (equal (checkpoint-stream s) 0) ;; first checkpoint
	   (equal (read-stream s) #.(char-code #\C))
	   (equal (checkpoint-stream s) 1) ;; second checkpoint
	   (equal (read-stream s) #.(char-code #\D))
	   (equal (rewind-stream s) 1) ;; rewind to second checkpoint
	   (equal (peek-stream s) #.(char-code #\D))
	   (equal (rewind-stream s) 0) ;; rewind to first checkpoint
	   (equal (peek-stream s) #.(char-code #\C))
	   (equal (checkpoint-stream s) 0) ;; first checkpoint
	   (equal (read-stream s) #.(char-code #\C))
	   (equal (commit-stream s) 0) ;; discard first checkpoint and resume
	   (equal (read-stream s) #.(char-code #\D))))
  t)

(deftest init-file-test
    (let ((file-data "ABCDEFGHIJKL")
	  (file-name (format nil "/tmp/~A" (symbol-name (gensym "stream-test-")))))
      (write-string-to-file file-data file-name :if-exists :supersede :if-does-not-exist :create)
      (and (equal file-data (read-string-from-file file-name))
	   (let ((s (make-core-stream (pathname file-name))))
	     (and (equal (checkpoint-stream s) 0)	   
		  (equal (read-stream s) #.(char-code #\A)) 	
		  (equal (rewind-stream s) 0) 
		  (equal (read-stream s) #.(char-code #\A))
		  (equal (checkpoint-stream s) 0)
		  (equal (checkpoint-stream s) 1)
		  (equal (read-stream s) #.(char-code #\B))
		  (equal (rewind-stream s) 1)
		  (equal (read-stream s) #.(char-code #\B))
		  (equal (rewind-stream s) 0)
		  (equal (read-stream s) #.(char-code #\B))))))
  t)

(deftest list-stream-test
    (let ((s (make-core-stream '(1 (2 3) (4 5)))))
      (list (checkpoint-stream s)
	    (read-stream s)
	    (rewind-stream s)
	    (checkpoint-stream s)
	    (read-stream s)
	    (progn
	      (write-stream s 6)
	      (commit-stream s))
	    (read-stream s)
	    (progn (write-stream s '(7 8))
		   (return-stream s))))
    (0 1 0 0 1 0 (4 5) (1 6 (4 5) (7 8))))

(deftest indented-stream-test
    (let ((s (make-indented-stream (make-core-stream ""))))
      (write-stream s #\a)
      (increase-indent s)
      (write-stream s #\Newline)
      (write-stream s #\b)
      (return-stream s))
  "a
  b")

(deftest compressed-stream-test
    (let ((s (make-compressed-stream (make-core-stream ""))))
	  (write-stream s #\a)
	  (increase-indent s)
	  (write-stream s #\Newline)
	  (write-stream s #\b)
	  (return-stream s))
  "ab")

;; (deftest test-cps-stream  
;;     (with-call/cc
;;       (let ((s (core-server::make-core-cps-stream ""))
;; 	    (a 0))
;; 	(checkpoint-stream/cc s)
;; 	(if (zerop a)
;; 	    (progn (setq a 1) (rewind-stream/cc s))
;; 	    (progn (core-server::string! s "test1") (commit-stream/cc s)))
;; 	(core-server::return-stream s)))
;;   "test1")
;; (defun test-cps-stream ()
;;   (let* ((ret1 123)
;; 	 (ret0 321)
;; 	 (s (with-call/cc
;; 	     (let ((s (make-instance 'core-cps-stream)))
;; 	       (if (not (eq ret0 (let/cc k
;; 				   (checkpoint-stream/cc s k)			 
;; 				   (if (not (eq ret1 (let/cc k
;; 						       (checkpoint-stream/cc s k)
;; 						       (describe s)
;; 						       (commit-stream/cc s s))))
;; 				       (error "failed cps stream")
;; 				       (commit-stream/cc s s)))))
;; 		   (error "failed cps stream")
;; 		   s)))))
;;     (describe (with-call/cc (rewind-stream/cc s ret1)))
;;     (describe (with-call/cc (rewind-stream/cc s ret0)))))

(defclass abc ()
  (gee eeg moo))

;; SERVER> (with-core-stream (s (make-instance 'standard-object))
;; 	  (render-object! s)
;; 	  (return-stream s))
;; #<ABC {10030A8021}>
;; SERVER> (describe *)
;; #<ABC {10030A8021}> is an instance of class #<STANDARD-CLASS ABC>.
;; The following slots have :INSTANCE allocation:
;;  GEE    1
;;  EEG    2
;;  MOO    3
(defrender render-object! ()
  (:class! 'abc)
  (:slot! 'gee 1)
  (:slot! 'eeg 2)
  (:slot! 'moo 3))

;; SERVER> (describe *abc)
;; #<ABC {100520E1C1}> is an instance of class #<STANDARD-CLASS ABC>.
;; The following slots have :INSTANCE allocation:
;;  GEE    1
;;  EEG    2
;;  MOO    3
;; SERVER> (parse-object? (make-core-stream *abc))
;; (1 2 3)
(defparser parse-object? (gee eeg moo)
  (:slot? 'gee gee)
  (:slot? 'eeg eeg)
  (:slot? 'moo moo)
  (:return (list gee eeg moo)))