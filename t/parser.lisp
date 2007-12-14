(in-package :tr.gen.core.server.test)

(defmacro deftest-parse (name string result)
  `(deftest ,name
       (let ((cs (make-core-stream ,string)))
	 (equal ,result
		(funcall (function ,(intern (symbol-name name) :core-server)) cs)))
     t))

(defmacro deftest-write (writer reader value &key result)
  `(deftest ,writer
       (let ((cs (make-core-stream "")))
	 (funcall (function ,(intern (symbol-name writer) :core-server)) cs ,value)
	 (equal ,(if result result value) (funcall (function ,(intern (symbol-name reader) :core-server)) cs)))
     t))

(deftest-parse crlf? (format nil "~%") t)
(deftest-parse lwsp? (format nil "~t") t)
(deftest-parse hex-value? "FA" 250)
(deftest-parse escaped? "%fa" 250)
(deftest-parse digit-value? "4" 4)
(deftest-parse fixnum? "424142" 424142)
(deftest-parse version? "1.2.3.4" '(1 2 3 4))
(deftest-parse quoted? "\"Here we go\"" "Here we go")

(deftest-write char! return-stream #\A :result "A")
(deftest-write string! return-stream "Hello, world!")
(deftest-write fixnum! return-stream 343 :result "343")
(deftest-write quoted! return-stream "Hello, world!" :result "\"Hello, world!\"")
(deftest-write quoted-fixnum! return-stream 343 :result "\"343\"")

