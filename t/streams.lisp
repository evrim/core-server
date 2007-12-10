(in-package :tr.gen.core.server.test)

(defun trace-core-streams ()
  (trace commit-stream rewind-stream read-stream peek-stream write-stream
	 checkpoint-stream))

(defun untrace-core-streams ()
  (untrace commit-stream rewind-stream read-stream peek-stream write-stream
	   checkpoint-stream))

;; test basic stream operations with a scenario
(deftest string-stream
    (let ((s (make-instance 'core-server::core-string-io-stream :string "ABCDEF")))
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
		  (equal (read-stream s) #.(char-code #\B))
		  (equal (core-server::char! s #\X) #.(char-code #\X))))))
  t)

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
(defun test-cps-stream ()
  (let* ((ret1 123)
	 (ret0 321)
	 (s (with-call/cc
	     (let ((s (make-instance 'core-cps-stream)))
	       (if (not (eq ret0 (let/cc k
				   (checkpoint-stream/cc s k)			 
				   (if (not (eq ret1 (let/cc k
						       (checkpoint-stream/cc s k)
						       (describe s)
						       (commit-stream/cc s s))))
				       (error "failed cps stream")
				       (commit-stream/cc s s)))))
		   (error "failed cps stream")
		   s)))))
    (describe (with-call/cc (rewind-stream/cc s ret1)))
    (describe (with-call/cc (rewind-stream/cc s ret0)))))
