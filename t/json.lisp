(in-package :core-server)
(defmacro test-return (stream &body body)
  `(let ((,stream (make-core-stream "")))
     ,@body
     (return-stream ,stream)))

;; JSON String
(assert (equal "hobaa" (json-string? (make-core-stream "hobaa"))))
(assert (equal "hobaa" (json-string? (make-core-stream "\"hobaa\""))))

(assert (equal "\"hobaa\"" (test-return s (json-string! s "hobaa"))))

;; JSON Number
(assert (equal 123 (json-number? (make-core-stream "123"))))
(assert (equal 37 (json-number? (make-core-stream "37.3000"))))
(assert (equal -37 (json-number? (make-core-stream "-37.3000"))))

(assert (equal "123" (test-return s (json-number! s 123))))
(assert (equal "1.23222e12" (test-return s (json-number! s 123.222e10))))

;; JSON Boolean
(assert (equal 'true (json-boolean? (make-core-stream "true"))))
(assert (equal 'false (json-boolean? (make-core-stream "false"))))

(assert (equal "true" (test-return s (json-boolean! s 'true))))
(assert (equal "true" (test-return s (json-boolean! s t))))
(assert (equal "false" (test-return s (json-boolean! s 'false))))

;; JSON Primitive
(assert (equal 12334 (json-primitive? (make-core-stream "12334"))))
(assert (equal 'true (json-primitive? (make-core-stream "true"))))
(assert (equal 'undefined (json-primitive? (make-core-stream "undefined"))))
(assert (equal 'null (json-primitive? (make-core-stream "null"))))
(assert (equal "hobaa" (json-primitive? (make-core-stream "hobaa"))))

(assert (equal "undefined" (test-return s (json-primitive! s 'undefined))))
(assert (equal "null" (test-return s (json-primitive! s 'null))))
(assert (equal "true" (test-return s (json-primitive! s 'true))))
(assert (equal "false" (test-return s (json-primitive! s 'false))))

;; JSON Array
(assert (equal (list 1 2 3) (json-array? (make-core-stream "[ 1,2,3 ]"))))
(assert (equal (list "a" "b" "c") (json-array? (make-core-stream "[ \"a\", \"b\", \"c\" ]"))))
(assert (equal (list 'true 'false 'undefined)
	       (json-array? (make-core-stream "[ true, false, undefined ]"))))

(assert (equal "[ 1 , 2 , 3 ]" (test-return s (json-array! s (list 1 2 3)))))

;; JSON Object
(assert (equal (list (cons 'a 1) (cons 'b 2) (cons 'c 3))
	       (json-object? (make-core-stream "{ a: 1, b:2,c: 3 }"))))

(assert (equal "{ \"abc\":1 \"def\":2 \"hij\":3 }"
	       (test-return s (json-object! s '(("abc" . 1) ("def" . 2) ("hij" . 3))))))

;; RFC Test Data
(defvar +json-test1+ "
{
      \"Image\": {
                   \"Width\":  800,
                   \"Height\": 600,
                   \"Title\":  \"View from 15th Floor\",
                   \"Thumbnail\": {
                                    \"Url\": \"http://www.example.com/image/481989943\",
                                    \"Height\": 125,
                                    \"Width\":  \"100\"
                                  },
                   \"IDs\": [116, 943, 234, 38793]
                }
}
")

(assert (equal '((IMAGE (WIDTH . 800) (HEIGHT . 600) (TITLE . "View from 15th Floor")
		  (THUMBNAIL (URL . "http://www.example.com/image/481989943") (HEIGHT . 125)
		   (WIDTH . "100"))
		  (IDS 116 943 234 38793)))
	       (json-object? (make-core-stream +json-test1+))))

(defvar +json-test2+
  "
   [
      {
         \"precision\": \"zip\",
         \"Latitude\":  37.7668,
         \"Longitude\": -122.3959,
         \"Address\":   \"\",
         \"City\":      \"SAN FRANCISCO\",
         \"State\":     \"CA\",
         \"Zip\":       \"94107\",
         \"Country\":   \"US\"
      },
      {
         \"precision\": \"zip\",
         \"Latitude\":  37.371991,
         \"Longitude\": -122.026020,
         \"Address\":   \"\",
         \"City\":      \"SUNNYVALE\",
         \"State\":     \"CA\",
         \"Zip\":       \"94085\",
         \"Country\":   \"US\"
      }
   ]")

(assert (equal '(((PRECISION . "zip") (LATITUDE . 37) (LONGITUDE . -122)
		 (ADDRESS . NULL) (CITY . "SAN FRANCISCO") (STATE . "CA")
		 (ZIP . "94107") (COUNTRY . "US"))
		((PRECISION . "zip") (LATITUDE . 37) (LONGITUDE . -122)
		 (ADDRESS . NULL) (CITY . "SUNNYVALE") (STATE . "CA") (ZIP . "94085")
		 (COUNTRY . "US")))
	       (json-array? (make-core-stream +json-test2+))))

(describe (json? (make-core-stream "{\"a\":1,\"b\":2,\"c\":3}")))