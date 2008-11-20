(in-package :core-server)
;; +----------------------------------------------------------------------------
;; | v2 - Core File IO Streams
;; +----------------------------------------------------------------------------
(defclass core-fd-io-stream-v2 (core-fd-io-stream)
  ((%write-buffer :initform nil :type (or null list))))

(defmethod write-stream :around ((stream core-fd-io-stream-v2) (atom null))
  stream)

(defmethod write-stream :around ((stream core-fd-io-stream-v2) atom)
  (if (transactionalp stream)
      (prog1 stream
	(setf (slot-value stream '%write-buffer)
	      (cons atom (slot-value stream '%write-buffer))))
      (call-next-method)))

(defmethod write-stream ((self core-fd-io-stream-v2) (atom character))
  (write-stream self (char-code atom)))

(defmethod write-stream ((self core-fd-io-stream-v2) (list list))
  (reduce #'write-stream list :initial-value self))

(defmethod write-stream ((self core-fd-io-stream-v2) (string string))
  (write-stream self (string-to-octets string :utf-8)))

(defmethod checkpoint-stream ((self core-fd-io-stream-v2))
  (if (transactionalp self)
      (push (list (s-v '%current) (s-v '%write-buffer))
	    (s-v '%checkpoints)))

  (setf (s-v '%current) (s-v '%read-index)
	(s-v '%write-buffer) nil)
  (length (s-v '%checkpoints)))

(defmethod %rewind-checkpoint ((self core-fd-io-stream-v2))
  (prog1 (length (s-v '%checkpoints))
    (let ((previous-checkpoint (pop (s-v '%checkpoints))))
      (if previous-checkpoint
	  (setf (s-v '%current) (car previous-checkpoint)
		(s-v '%write-buffer) (cadr previous-checkpoint))	  
	  (setf (s-v '%current) -1
		(s-v '%write-buffer) nil)))))

(defmethod commit-stream ((self core-fd-io-stream-v2))
  (let ((buffer (s-v '%write-buffer)))
    (prog1 (%rewind-checkpoint self)
      (write-stream self (nreverse buffer))
      (if (not (transactionalp self))
	  (finish-output (s-v '%stream))))))

(defclass core-character-io-stream-v2 (core-fd-io-stream-v2)
  ())

(defmethod %peek-stream ((self core-character-io-stream-v2))
  (flet ((peek ()
	   (let ((peeked (char-code (read-char (s-v '%stream) nil nil))))	
	     (setf (s-v '%peek) (or peeked -1))
	     peeked)))
    (if (< (the fixnum (s-v '%peek)) 0)
	(cond
	  ((and (s-v '%max-read) (> (the fixnum (s-v '%max-read)) 0))
	   (prog1 (peek) (decf (s-v '%max-read))))
	  ((s-v '%max-read) nil)	
	  (t (peek)))      
	(s-v '%peek))))

(defmethod write-stream ((self core-character-io-stream-v2) (string string))
  (write-sequence string (s-v '%stream))
  self)

(defmethod write-stream ((self core-character-io-stream-v2) (list list))
  (reduce #'write-stream list :initial-value self))

(defmethod write-stream ((self core-character-io-stream-v2) (char character))
  (write-char char (s-v '%stream))
  self)

(defmethod write-stream ((self core-character-io-stream-v2) (char integer))
  (write-stream self (code-char char)))

(defmethod write-stream ((self core-character-io-stream-v2) (char t))
  (write-char char (s-v '%stream))
  self)

(defun make-core-stream-v2 (source &rest args)
  (apply #'make-instance
	 (typecase source
	   (sb-sys:fd-stream
	    (list 'core-fd-io-stream-v2 :stream source)))))

(defun fd-test ()
  (let ((path1 (tmpnam nil))
	(path2 (tmpnam nil))
	(path3 (tmpnam nil)))
    (labels ((get-fd1 ()
	       (open path1
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :overwrite
		     :element-type '(unsigned-byte 8)		     
		     :external-format :utf8))
	     (get-fd2 ()
	       (open path2
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :overwrite
		     :element-type ;; '(unsigned-byte 8)
		     'character
		     :external-format :utf8))
	     (fd1 ()	   
	       (let ((s (make-instance 'core-fd-io-stream :stream (get-fd1))))
		 (checkpoint-stream s)
		 (core-library! (make-compressed-stream s))
		 (commit-stream s)
		 (close-stream s)
		 s))
	     (fd2 ()	     
	       (let ((s (make-instance 'core-fd-io-stream-v2 :stream (get-fd1))))
		 (checkpoint-stream s)
		 (core-library! s)
		 (commit-stream s)
		 (close-stream s)
		 s))
	     (fd3 ()	     
	       (let ((s (make-instance 'core-character-io-stream-v2 :stream (get-fd2))))
		 (checkpoint-stream s)
		 (core-library! s)
		 (commit-stream s)
		 (close-stream s)
		 s)))      
      (time (dotimes (i 5000)
	      (fd1)))      
      (time (dotimes (i 5000)
	      (fd2)))      
      (time (dotimes (i 5000)
	      (fd3)))
      (list path1 path2))))

(defclass server-unit (local-unit)
  ())

(defmethod/unit handle-stream1 :async ((unit server-unit) stream)
  (let ((s (make-instance 'core-fd-io-stream :stream stream)))
    (checkpoint-stream s)
    (core-library! (make-compressed-stream s))
    (commit-stream s)
    (close-stream s)
    s))

(defmethod/unit handle-stream2 ((unit server-unit) stream)
  (let ((s (make-instance 'core-fd-io-stream-v2 :stream stream)))
    (checkpoint-stream s)
    (core-library! s)
    (commit-stream s)
    (close-stream s)
    s))

(defmethod/unit handle-stream3 ((unit server-unit) stream)
  (let ((s (make-instance 'core-character-io-stream-v2 :stream stream)))
    (checkpoint-stream s)
    (core-library! s)
    (commit-stream s)
    (close-stream s)
    s))

;; (defparameter *units*
;;   (mapcar (lambda (seq)
;; 	    (let ((unit (make-instance 'server-unit)))
;; 	      (start unit)
;; 	      unit))
;; 	  (seq 5)))

;; (defparameter *paths*
;;   (mapcar (lambda (unit)
;; 	    (tmpnam nil))
;; 	  (seq 100)))

;; (defun get-fd1 (path)
;;   (open path
;; 	:direction :output
;; 	:if-does-not-exist :create
;; 	:if-exists :overwrite
;; 	:element-type '(unsigned-byte 8)		     
;; 	:external-format :utf8))

;; (defun run-units ()  		  
;;   (time
;;    (progn
;;      (mapcar (lambda (unit)
;; 	       (mapcar (lambda (path)
;; 			 (handle-stream1 unit (get-fd1 path)))
;; 		       *paths*))
;; 	     *units*)
;;      (funcall (handle-stream1 (car (reverse *units*)) (get-fd1 (car *paths*)))))))


;; (defapplication test-application (http-application)
;;   ())

;; (defparameter *5k*
;;   (let ((acc (make-accumulator)))
;;     (loop for i from 1 upto 5192
;;        do (push-atom #\A acc))
;;     acc))

;; (defhandler "test1.core" ((app test-application))
;;   (write-stream (http-response.stream (context.response +context+)) *5k*))

;; (defhandler "test2.core" ((app test-application))
;;   (core-library! (http-response.stream (context.response +context+)))
;;   nil)

;; (defhandler "test3.core" ((app test-application))
;;   (core-library! (http-response.stream (context.response +context+)))
;;   (core-library! (http-response.stream (context.response +context+)))
;;   (core-library! (http-response.stream (context.response +context+)))
;;   (core-library! (http-response.stream (context.response +context+)))
;;   (core-library! (http-response.stream (context.response +context+)))
;;   (core-library! (http-response.stream (context.response +context+)))  
;;   nil)

;; (defvar *app* (make-instance 'test-application :fqdn "zoo.com" :admin-email "test@core"))
;; (register *server* *app*)

;; sh-3.2$ siege -c100 -t20S http://localhost:3001/zoo.com/test1.core
;; ** SIEGE 2.66
;; ** Preparing 100 concurrent users for battle.
;; The server is now under siege...
;; Lifting the server siege..      done.                                                                                                                                                                                                                                              Transactions:                   16212 hits
;; Availability:                 100.00 %
;; Elapsed time:                  19.99 secs
;; Data transferred:              80.27 MB
;; Response time:                  0.10 secs
;; Transaction rate:             811.01 trans/sec
;; Throughput:                     4.02 MB/sec
;; Concurrency:                   83.51
;; Successful transactions:       16212
;; Failed transactions:               0
;; Longest transaction:            9.81
;; Shortest transaction:           0.00

;; sh-3.2$ siege -c100 -t20S http://localhost:3001/zoo.com/test2.core
;; ** SIEGE 2.66
;; ** Preparing 100 concurrent users for battle.
;; The server is now under siege...
;; Lifting the server siege..      done.                                                                                                                                                                                                                                              Transactions:                    9493 hits
;; Availability:                 100.00 %
;; Elapsed time:                  19.68 secs
;; Data transferred:              47.86 MB
;; Response time:                  0.14 secs
;; Transaction rate:             482.37 trans/sec
;; Throughput:                     2.43 MB/sec
;; Concurrency:                   66.12
;; Successful transactions:        9493
;; Failed transactions:               0
;; Longest transaction:           15.59
;; Shortest transaction:           0.00

;; sh-3.2$ siege -c100 -t20S http://localhost:3001/zoo.com/test3.core
;; ** SIEGE 2.66
;; ** Preparing 100 concurrent users for battle.
;; The server is now under siege...
;; Lifting the server siege..      done.                                                                                                                                                                                                                                              Transactions:                    6550 hits
;; Availability:                 100.00 %
;; Elapsed time:                  20.25 secs
;; Data transferred:             198.12 MB
;; Response time:                  0.25 secs
;; Transaction rate:             323.46 trans/sec
;; Throughput:                     9.78 MB/sec
;; Concurrency:                   81.03
;; Successful transactions:        6550
;; Failed transactions:               0
;; Longest transaction:           13.39
;; Shortest transaction:           0.00
