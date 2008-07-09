(in-package :tr.gen.core.server.test)

;; JSON String
(deftest-parser json-string?-plain core-server::json-string? "hobaa" "hobaa")
(deftest-parser json-string?-quoted core-server::json-string? "\"hobaa\"" "hobaa")

(deftest-render json-string!-plain core-server::json-string! "hobaa" "\"hobaa\"")

;; JSON Number
(deftest-parser json-number?-integer core-server::json-number? "123" 123)
(deftest-parser json-number?-integer core-server::json-number? "37.3000" 37)
(deftest-parser json-number?-integer core-server::json-number? "-37.3000" -37)
(deftest-parser json-number?-integer core-server::json-number? "123" 123)
(deftest-parser json-number?-integer core-server::json-number? "123" 123)

(deftest-render json-number!-integer core-server::json-number! 123 "123")
(deftest-render json-number!-float core-server::json-number! 123.2222e10 "1.232222e12")

;;; ;; JSON Boolean
(deftest-parser json-boolean?-true core-server::json-boolean? "true" 'true)
(deftest-parser json-boolean?-false core-server::json-boolean? "false" 'false)

(deftest-render json-boolean!-t core-server::json-boolean! t "true")
(deftest-render json-boolean!-true core-server::json-boolean! 'true "true")
(deftest-render json-boolean!-false core-server::json-boolean! 'false "false")

;;; ;; JSON Array
(deftest-parser json-array?-nums core-server::json-array? "[1,2,3]" '(1 2 3))
(deftest-parser json-array?-strings core-server::json-array? "[\"core\", \"gen\", \"tr\"]" '("core" "gen" "tr"))
(deftest-parser json-array?-bools core-server::json-array? "[true, false, undefined]" '(true false undefined))

(deftest-render json-array!-nums core-server::json-array! '(1 2 3) "[ 1 , 2 , 3 ]")
(deftest-render json-array!-strings core-server::json-array! '("core" "gen" "tr") "[ \"core\" , \"gen\" , \"tr\" ]")
(deftest-render json-array!-bools core-server::json-array! '(undefined true false) "[ undefined , true , false ]")

;;; ;; JSON Object
(deftest-parser json-object?-proplist (lambda (stream) (hash-to-alist (core-server::json-object? stream))) "{ a: 1, b:2, c: 3 }" '(("a" . 1) ("b" . 2) ("c" . 3)))

(deftest-render json-object!-proplist core-server::json-object!
  (arnesi::build-hash-table '(:test equal)
			    '(("abc" 1)
			      ("def" 2)
			      ("hij" 3)))
  "{ \"abc\": 1, \"def\": 2, \"hij\": 3 }")

;;; ;; RFC Test Data
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

(deftest-parser json-rfc-test1? (lambda (stream) (caar (hash-to-alist (core-server::json-object? stream))))
  +json-test1+
  "Image")

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

(deftest-parser json-rfc-test2? (lambda (stream)
				  (mapcar #'hash-to-alist (core-server::json-array? stream)))
  +json-test2+
  '((("precision" . "zip")
     ("Latitude" . 37)
     ("Longitude" . 122) 
     ("Address" . NULL)
     ("City" . "SAN FRANCISCO")
     ("State" . "CA") 
     ("Zip" . "94107")
     ("Country" . "US")) 
    (("precision" . "zip")
     ("Latitude" . 37)
     ("Longitude" . 122) 
     ("Address" . NULL)
     ("City" . "SUNNYVALE")
     ("State" . "CA")
     ("Zip" . "94085")
     ("Country" . "US"))))

(deftest json-dom2js!
    (with-core-stream (s "")
      (html! s (<:div :id "5" "GEE"))
      (return-stream s))
  "<DIV id=\"5\">GEE</DIV>")