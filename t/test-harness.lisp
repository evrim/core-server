(in-package :tr.gen.core.server.test)

(defmacro deftest-parser (test-name test-function string result)
  `(deftest ,test-name
       (let ((cs (make-core-stream ,string)))
	 (equal ,result
		(funcall (function ,test-function) cs)))
     t))

(defmacro deftest-render (test-name test-function value result)
  `(deftest ,test-name
       (let ((cs (make-core-stream "")))
	 (funcall (function ,test-function) cs ,value)
	 (equal ,result
		(return-stream cs)))
     t))
