(in-package :tr.gen.core.server.test)

(deftest uri-to-stream
    (let ((uri-s "http://evrim:password@nodeN.core.gen.tr:80/a/b/c?a:1;b:2#x:1;y:2")
	  (uri-o (make-uri :scheme "http" :username "evrim"
					  :password "password"
					  :server "nodeN.core.gen.tr"
					  :port 80
					  :paths '(("a") ("b") ("c"))
					  :queries (list (cons "a" "1")
							 (cons "b" "2"))
					  :fragments (list (cons "x" "1")
							   (cons "y" "2"))))
	  (cstream (make-core-stream "")))
      (uri! cstream uri-o)
      (equal uri-s  (core-server::stream-data cstream)))
  t)

(deftest stream-to-uri
    (let* ((uri-s "http://evrim:password@nodeN.core.gen.tr:80/a/b/c?a:1;b:2#x:1;y:2")
	   (uri-o (uri? (make-core-stream uri-s))))
      (and (equal (uri.scheme uri-o) "http")
	   (equal (uri.username uri-o) "evrim")
	   (equal (uri.password uri-o) "password")
	   (equal (uri.server uri-o) "nodeN.core.gen.tr")
	   (eq (uri.port uri-o) 80)
	   (equal (uri.paths uri-o) '(("a") ("b") ("c")))
	   (equal (uri.queries uri-o) (list (cons "a" "1") (cons "b" "2")))
	   (equal (uri.fragments uri-o) (list (cons "x" "1") (cons "y" "2")))))
  t)