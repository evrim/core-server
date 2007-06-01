(in-package :tr.gen.core.server.test)

(defun trace-core-streams ()
  (trace commit-stream rewind-stream read-stream peek-stream write-stream
	 checkpoint-stream))

(defun untrace-core-streams ()
  (untrace commit-stream rewind-stream read-stream peek-stream write-stream
	   checkpoint-stream))

(defun test-string-stream ()
  (let ((s (make-instance 'core-server::core-string-io-stream :string "ABCDEF")))
    (trace-core-streams)
    (assert (equal (peek-stream s) #.(char-code #\A)))
    (assert (equal (read-stream s) #.(char-code #\A)))
    (assert (equal (read-stream s) #.(char-code #\B)))
    (assert (equal (checkpoint-stream s) 0)) ;; first checkpoint
    (assert (equal (read-stream s) #.(char-code #\C)))
    (assert (equal (checkpoint-stream s) 1)) ;; second checkpoint
    (assert (equal (read-stream s) #.(char-code #\D)))
    (assert (equal (rewind-stream s) 1)) ;; rewind to second checkpoint
    (assert (equal (peek-stream s) #.(char-code #\D)))
    (assert (equal (rewind-stream s) 0)) ;; rewind to first checkpoint
    (assert (equal (peek-stream s) #.(char-code #\C)))
    (assert (equal (checkpoint-stream s) 0)) ;; first checkpoint
    (assert (equal (read-stream s) #.(char-code #\C)))
    (assert (equal (commit-stream s) 0)) ;; discard first checkpoint and resume
    (assert (equal (read-stream s) #.(char-code #\D)))
    (untrace-core-streams)))

(defparameter *file-data* "ABCDEFGHIJKL")
(defun init-file-test ()
  (write-string-to-file *file-data* #P"/tmp/stream-test" :if-exists :supersede :if-does-not-exist :create)
  (assert (equal *file-data* (read-string-from-file #P"/tmp/stream-test"))))

(defun test-file-stream ()
  (init-file-test)
  (let ((s (make-core-stream #P"/tmp/stream-test")))
    (trace-core-streams)
    (unwind-protect
	 (progn
	   (assert (equal (checkpoint-stream s) 0))	   
	   (assert (equal (read-stream s) #.(char-code #\A))) 	
	   (assert (equal (rewind-stream s) 0)) 
	   (assert (equal (read-stream s) #.(char-code #\A)))
	   (assert (equal (checkpoint-stream s) 0))
	   (assert (equal (checkpoint-stream s) 1))
	   (assert (equal (read-stream s) #.(char-code #\B)))
	   (assert (equal (rewind-stream s) 1))
	   (assert (equal (read-stream s) #.(char-code #\B)))
	   (assert (equal (rewind-stream s) 0))
	   (assert (equal (read-stream s) #.(char-code #\B)))
	   (assert (equal (core-server::char! s #\X) #.(char-code #\X)))
	   (describe (read-stream s)))
      (return-from test-file-stream s))
    (untrace-core-streams)))

;; (defparameter *stream-data* "ABCDEF")
;; (defparameter *stream-octets* (arnesi::string-to-octets *stream-data* :utf-8))
;; (defun write-test-data (stream)
;;   (dotimes (i (length *stream-octets*))
;;     (write-byte (aref *stream-octets* i) stream))
;;   (sb-impl::flush-output-buffer stream)
;;   (format t "Written ~A bytes~%" (length *stream-octets*)))

;; (defun read-test-data (stream)
;;   (format t "read: ")
;;   (dotimes (i (length *stream-octets*))    
;;     (let ((a (read-byte stream)))
;;       (format t "~A " a)
;;       (sb-impl::flush-standard-output-streams)))  
;;   (format t "~%Read ~A bytes~%" (length *stream-octets*)))

;; (defclass test-socket-thread (core-server::standard-thread)
;;   ((server :initarg :server :initform nil)))

;; (defmethod core-server::run ((core-server::self test-socket-thread))
;;   (loop
;;        (multiple-value-bind (stream peer-address)
;; 	   (trivial-sockets:accept-connection (core-server::s-v 'server)
;; 					      :element-type '(unsigned-byte 8))
;; 	 (format t "Got peer:~A~%" peer-address)
;; 	 (write-test-data stream)
;; 	 (read-test-data stream)
;; 	 ;; (let ((acc (core-server::make-accumulator :byte)))
;; ;; 	   (dotimes (i 4)
;; ;; 	     (let ((atom (read-byte stream)))
;; ;; 	       (when atom
;; ;; 		 (core-server::push-atom atom acc))))
;; ;; 	   (format t "Read ~A bytes:~A~%" (length acc) (arnesi::octets-to-string acc :utf-8)))
;; 	 (write-test-data stream)
;; 	 (close stream)
;; 	 (format t "Stream closed~%"))))

;; (defparameter *port-start* 4002)
;; (defparameter *server* (trivial-sockets:open-server :port (incf *port-start*)))
;; (defparameter *thread (make-instance 'test-socket-thread :server *server*))
;; (defun get-core-fd-stream ()
;;   (if (not (status *thread)) (start *thread))
  
;;   (let ((stream (trivial-sockets::open-stream "127.0.0.1" *port-start*
;; 					       :element-type '(unsigned-byte 8))))
;;     (make-instance 'core-server::core-fd-io-stream :stream stream)))

;; (defun test-fd-stream ()
;;   (let ((s (get-core-fd-stream)))
;;     (unwind-protect
;; 	 (progn
;; 	   (assert (equal (peek-stream s) #.(char-code #\A)))
;; 	   (assert (equal (read-stream s) #.(char-code #\A)))
;; 	   (assert (equal (read-stream s) #.(char-code #\B)))
;; 	   (assert (equal (checkpoint-stream s) 0)) ;; first checkpoint
;; 	   (assert (equal (read-stream s) #.(char-code #\C)))
;; 	   (assert (equal (checkpoint-stream s) 1)) ;; second checkpoint
;; 	   (assert (equal (read-stream s) #.(char-code #\D)))
;; 	   (assert (equal (rewind-stream s) 1)) ;; rewind to second checkpoint
;; 	   (assert (equal (peek-stream s) #.(char-code #\D)))
;; 	   (assert (equal (rewind-stream s) 0)) ;; rewind to first checkpoint
;; 	   (assert (equal (peek-stream s) #.(char-code #\C)))
;; 	   (assert (equal (checkpoint-stream s) 0)) ;; first checkpoint
;; 	   (assert (equal (read-stream s) #.(char-code #\C)))
;; 	   (assert (equal (commit-stream s) 0)) ;; discard first checkpoint and resume
;; 	   (assert (equal (read-stream s) #.(char-code #\D))))
;;       (return-from test-fd-stream s))))