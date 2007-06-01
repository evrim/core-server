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