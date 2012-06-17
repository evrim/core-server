(in-package :Core-server)

(defun speed-test1 ()
  (time
   (let ((s (make-core-stream "")))
     (dotimes (i 10000)
       (checkpoint-stream s)
       (write-stream (make-html-stream s) (<:html (<:body "foo")))
       (commit-stream s))
     (length (return-stream s)))))

(defun speed-test2 ()
  (time
   (let ((s (make-core-list-output-stream)))
     (dotimes (i 10000)
       (checkpoint-stream s)
       (write-stream (make-html-stream s) (<:html (<:body "foo")))
       (commit-stream s))
     (length (return-stream2 s)))))

(defun speed-test3 ()
  (time
   (let ((s (make-core-stream "")))
     (dotimes (i 100000)
       (checkpoint-stream s)
       (with-js () s
	 (let ((a (<:html (<:body a))))
	   (append document.body a)))
       (commit-stream s))
     (length (return-stream s)))))

(defun speed-test4 ()
  (time
   (let ((s (make-core-list-output-stream)))
     (dotimes (i 100000)
       (checkpoint-stream s)
       (with-js () s
	 (let ((a (<:html (<:body a))))
	   (append document.body a)))       
       (commit-stream s))
     (length (return-stream2 s)))))