(in-package :core-server.test)

(defmacro test-js (name statement result)
  `(deftest ,(intern (format nil "JS-~A" name))
       (core-server::js ,statement)
     ,result))

(test-js evrim-1
  (unless (blorg.is-correct) (alert "gee"))
  "if (!blorg.isCorrect()) {
  alert('gee');
}")

(test-js evrim-2
  (when (blorg.is-correct) (carry-on) (return i))
  "if (blorg.isCorrect()) {
  carryOn();
  return i;
}")

(test-js evrim-3
  (if (blorg.is-correct) (carry-on) (return i))
  "if (blorg.isCorrect()) {
  carryOn();
} else {
  return i;
}")

(test-js evrim-4
  (+ i (if (blorg.add-one) 1 2))
  "i + (blorg.addOne() ? 1 : 2)")

(test-js evrim-5
  (if (= (typeof blorg) *string)
      (alert (+ "blorg is a string: " blorg))
      (alert "blorg is not a string"))
  "if (typeof blorg == String) {
  alert('blorg is a string: ' + blorg);
} else {
  alert('blorg is not a string');
}")

(test-js evrim-6
 (* 3 5 (+ 1 2))
 "3 * 5 * (1 + 2)")

(test-js symbol-conversion-1
	 !?#@%
	 "bangwhathashatpercent")

(test-js symbol-conversion-2
  bla-foo-bar
  "blaFooBar")

(test-js symbol-conversion-3
  *array
  "Array")

(test-js symbol-conversion-4
  *global-array*
  "GLOBALARRAY")

(test-js symbol-conversion-5
  *global-array*.length
  "GLOBALARRAY.length")

(test-js number-literals-1
  1
  "1")

(test-js number-literals-2
  123.123
  "123.123")

(test-js number-literals-3
  #x10
  "16")

(test-js string-literals-1
  "foobar"
  "'foobar'")

(test-js string-literals-2
  "bratzel bub"
  "'bratzel bub'")

(test-js operator-expressions-1
  (* 1 2 3)
  "1 * 2 * 3")

(test-js operator-expressions-2
  (= 1 2)
  "1 == 2")

(test-js operator-expressions-3
  (eql 1 2)
  "1 === 2")

(test-js operator-expressions-5
  (* 1 (+ 2 3 4) 4 (/ 6 7))
  "1 * (2 + 3 + 4) * 4 * (6 / 7)")

(test-js operator-expressions-8
  (incf i)
  "++i")

(test-js operator-expressions-9
  (decf i)
  "--i")

;; (test-js operator-expressions-6
;;   (++ i)
;;   "i++")

;; (test-js operator-expressions-7
;;   (-- i)
;;   "i--")

;; (test-js operator-expressions-10
;;   (1- i)
;;   "i - 1")

;; (test-js operator-expressions-11
;;   (1+ i)
;;   "i + 1")

(test-js operator-expressions-12
  (not (< i 2))
  "!(i < 2)")

(test-js operator-expressions-13
  (not (eql i 2))
  "!(i === 2)")

(test-js literal-symbols-1
  T
  "true")

;; (test-js literal-symbols-2
;;   FALSE
;;   "false")

(test-js literal-symbols-3
  NIL
  "null")

;; (test-js literal-symbols-4
;;   UNDEFINED
;;   "undefined")

;; (test-js literal-symbols-5
;;   THIS
;;   "this")

(test-js variables-1
  variable
  "variable")

(test-js variables-2
  a-variable
  "aVariable")

(test-js variables-3
  *math
  "Math")

(test-js variables-4
  *math.floor
  "Math.floor")

(test-js assignment-1
  (setq a 1)
  "a = 1")

;; complete, no optimization though.
(test-js assignment-2
  (setf a 2 b 3 c 4 x (+ a b c))
  "a = 2;
b = 3;
c = 4;
x = a + b + c;")

(test-js assignment-3
  (setq a (1+ a))
  "a = a + 1")

(test-js assignment-4
  (setq a (+ a 2 3 4 a))
  "a = a + 2 + 3 + 4 + a")

(test-js assignment-5
  (setq a (- 1 a))
  "a = 1 - a")

(test-js single-argument-statements-1
	 (return 1)
	 "return 1")

(test-js single-argument-expression-2
	 (if (= (typeof blorg) *string)
	     (alert (+ "blorg is a string: " blorg))
	     (alert "blorg is not a string"))
	 "if (typeof blorg == String) {
  alert('blorg is a string: ' + blorg);
} else {
  alert('blorg is not a string');
}")

(test-js conditional-statements-2
  (+ i (if (blorg.add-one) 1 2))
  "i + (blorg.addOne() ? 1 : 2)")

(test-js conditional-statements-3
  (when (blorg.is-correct)
  (carry-on)
  (return i))
  "if (blorg.isCorrect()) {
  carryOn();
  return i;
}")

(test-js conditional-statements-4
  (unless (blorg.is-correct)
    (alert "blorg is not correct!"))
  "if (!blorg.isCorrect()) {
  alert('blorg is not correct!');
}")

(test-js conditional-statements-5
  (cond
    ((eq a 1)
     (log-me 'gee))
    (t 
     (log-me 'zee)))
  "if(a === 1) {
  logMe(gee);
} else {
  logMe(zee);
}")

(test-js function-definition-2
	 (lambda (a b) (return (+ a b)))
	 "function (a, b) {
  return a + b;
}")

(test-js conditional-statements-1
	 (if (blorg.is-correct)
	     (progn (carry-on) (return i))
	     (alert "blorg is not correct!"))
	 "if (blorg.isCorrect()) {
  carryOn();
  return i;
} else {
  alert('blorg is not correct!');
}")

(test-js variable-declaration-3
  (if (= i 1)
    (let ((blorg "hallo"))
      (alert blorg))
    (let ((blorg "blitzel"))
      (alert blorg)))
  "if (i == 1) {
  var blorg = 'hallo';
  alert(blorg);
} else {
  var blorg = 'blitzel';
  alert(blorg);
}")

;; if sorunlu
(test-js variable-declaration-2
	 (if (= i 1)
	     (progn (defvar blorg "hallo")
		    (alert blorg))
	     (progn (defvar blorg "blitzel")
		    (alert blorg)))
	 "if (i == 1) {
  var blorg = 'hallo';
  alert(blorg);
} else {
  var blorg = 'blitzel';
  alert(blorg);
}")

(test-js iteration-constructs-5
  (while (film.is-not-finished)
    (this.eat (new (*popcorn))))
  "while (film.isNotFinished()) {
  this.eat(new Popcorn());
}")

(test-js regular-expression-literals-2
  (regex "/foobar/i")
  "/foobar/i")

(test-js function-calls-and-method-calls-1
	 (blorg 1 2)
	 "blorg(1, 2)")

(test-js object-literals-4
	 an-object.foo
	 "anObject.foo")

;; can't happen in lisp2
;; (test-js function-calls-and-method-calls-3
;;   ((aref foo i) 1 2)
;;   "foo[i](1, 2)")

(test-js variable-declaration-1
	 (defvar *a* (array 1 2 3))
	 "var A = [ 1, 2, 3 ]")

(test-js statements-and-expressions-1
	 (+ i (if 1 2 3))
	 "i + (1 ? 2 : 3)")

(test-js statements-and-expressions-2
	 (if 1 2 3)
	 "if (1) {
  2;
} else {
  3;
}")

(test-js array-literals-1
	 (array)
	 "[  ]")

(test-js array-literals-2
	 (array 1 2 3)
	 "[ 1, 2, 3 ]")

(test-js body-forms-1
	 (progn (blorg i) (blafoo i))
	 "blorg(i);
blafoo(i);")

(test-js body-forms-2
  (+ i (progn (blorg i) (blafoo i)))
  "i + (blorg(i), blafoo(i))")

(test-js function-calls-and-method-calls-2
	 (foobar (blorg 1 2) (blabla 3 4) (array 2 3 4))
	 "foobar(blorg(1, 2), blabla(3, 4), [ 2, 3, 4 ])")

(test-js object-literals-1
	 (create :foo "bar" :blorg 1)
	 "{
  foo: 'bar',
  blorg: 1
}")

(test-js object-literals-2
  (create :foo "hihi"
        :blorg (array 1 2 3)
        :another-object (create :schtrunz 1))
  "{
  foo: 'hihi',
  blorg: [ 1, 2, 3 ],
  anotherObject: {
    schtrunz: 1
  }
}")

(test-js single-argument-expression-1
	 (delete (new (*foobar 2 3 4)))
	 "delete new Foobar(2, 3, 4)")

(test-js array-literals-4
	 (make-array)
	 "new Array()")

(test-js array-literals-5
  (make-array 1 2 3)
  "new Array(1, 2, 3)")

(test-js array-literals-6
	 (make-array
	  (make-array 2 3)
	  (make-array "foobar" "bratzel bub"))
	 "new Array(new Array(2, 3), new Array('foobar', 'bratzel bub'))")

(test-js single-argument-statements-2
	 (throw "foobar")
	 "throw 'foobar'")

(test-js object-literals-3
  (slot-value an-object 'foo)
  "anObject.foo")


(test-js the-with-statement-1
  (with (create :foo "foo" :i "i")
	(alert (+ "i is now intermediary scoped: " i))
	(alert "zoo"))
  "with ({
  foo: 'foo',
  i: 'i'
}) {
  alert('i is now intermediary scoped: ' + i);
  alert('zoo');
}")

(test-js iteration-constructs-4
  (doeach (i object)
	  (document.write (+ i " is " (aref object i))))
  "for (var i in object) {
  document.write(i + ' is ' + object[i]);
}")

(test-js iteration-constructs-2
  (dotimes (i blorg.length)
    (document.write (+ "L is " (aref blorg i)))
    (alert "zoo"))
  "for (var i = 0; i < blorg.length; i = i + 1) {
  document.write('L is ' + blorg[i]);
  alert('zoo');
}")

;; Could not test dolist since i consists tmp vars.
;; (test-js iteration-constructs-3
;;   (dolist (l blorg)
;;     (document.write (+ "L is " l))
;;     (alert "zoo"))
;;   "for (var l = 0; l < blorg.length; l = l + 1) {
;; document.write('L is ' + l);
;; alert('zoo');
;; }"
;; ;;;   "{
;; ;;;   var tmpArr1 = blorg;
;; ;;;   for (var tmpI2 = 0; tmpI2 < tmpArr1.length;
;; ;;;     tmpI2 = tmpI2 + 1) {
;; ;;;     var l = tmpArr1[tmpI2];
;; ;;;     document.write('L is ' + l);
;; ;;;   };
;; ;;; }"
;;   )

(test-js iteration-constructs-4
  (do ((i 0 (+ 1 i)))
      ((> i 5) i)
    (print i))
  "for (var i=0; !(i > 5); i=1 + i) {
  print(i);
}")

(test-js slot-val-1
  (slot-value a "abc")
  "a['abc']")

(test-js slot-val-2
  (slot-value a 'abc)
  "a.abc")

(test-js slot-val-3
  (slot-value a abc)
  "a[abc]")

(test-js the-try-statement-1
  (try (throw "i")
       (:catch (error)
	 (alert (+ "an error happened: " error)))
       (:finally
	(alert "Leaving the try form")))
  "try {
  throw 'i'
} catch (error) {
  alert('an error happened: ' + error);
} finally {
  alert('Leaving the try form');
}")

(test-js regular-expression-literals-1
  (regex "/foobar/")
  "/foobar/")

(test-js function-definition-1
  (defun a-function (a b)
    (return (+ a b)))
  "function aFunction(a, b) {
  return a + b;
}")

(test-js function-calls-and-method-calls-6
  (.blorg (aref foobar 1) NIL T)
  "foobar[1].blorg(null, true)")

(test-js function-calls-and-method-calls-4
  (.blorg this 1 2)
  "this.blorg(1, 2)")

(test-js function-calls-and-method-calls-5
  (this.blorg 1 2)
  "this.blorg(1, 2)")

(test-js iteration-constructs-1
  (do ((i 0 (1+ i))
       (l (aref blorg i) (aref blorg i)))
      ((or (= i blorg.length)
	   (eql l "Fumitastic")))
    (document.write (+ "L is " l)))
  "for (var i=0, l=blorg[i]; !((i == blorg.length) || (l === 'Fumitastic')); i=i + 1, l=blorg[i]) {
  document.write('L is ' + l);
}")

(test-js the-case-statement-2
  (switch (aref blorg i)
    (1 (alert "If I get here"))
    (2 (alert "I also get here"))
    (default (alert "I always get here")))
  "switch (blorg[i]) {
  case 1:
    alert('If I get here');
  case 2:
    alert('I also get here');
  default:
    alert('I always get here');
}")

(test-js the-case-statement-1
  (case (aref blorg i)
    ((1 "one") (alert "one"))
    (2 (alert "two"))
    (t (alert "default clause")))
	 "switch (blorg[i]) {
  case 1:
  case 'one':
    alert('one');
    break;
  case 2:
    alert('two');
    break;
  default:
    alert('default clause');
}")

(test-js object-literals-5
	 (with-slots (a b c) this
	   (+ a b c))
	 "var a = this.a;
var b = this.b;
var c = this.c;
a + b + c")
;;  "this.a + this.b + this.c;"

(test-js array-literals-3
  (array (array 2 3)
	 (array "foobar" "bratzel bub"))
  "[ [ 2, 3 ], [ 'foobar', 'bratzel bub' ] ]")



;; Speed Results


;; Case 1: With Core Server Renderer as Indented
;; SERVER> (let ((s (make-indented-stream "" 2)))
;; 	  (time 
;; 	   (dotimes (i 10000)
;; 	     (dojo s))))
;; Evaluation took:
;;   41.749 seconds of real time
;;   41.870616 seconds of total run time (41.174573 user, 0.696043 system)
;;   [ Run times consist of 3.480 seconds GC time, and 38.391 seconds non-GC time. ]
;;   100.29% CPU
;;   100,205,266,725 processor cycles
;;   1 page fault <- ????
;;   4,602,467,504 bytes consed
;; NIL

;; Case 2: With Parenscript Indented
;; SERVER> (time 
;; 	 (dotimes (i 10000)
;; 	   (dojo-old)))
;; Evaluation took:
;;   56.669 seconds of real time
;;   56.555535 seconds of total run time (55.123445 user, 1.432090 system)
;;   [ Run times consist of 6.396 seconds GC time, and 50.160 seconds non-GC time. ]
;;   99.80% CPU
;;   136,017,920,106 processor cycles
;;   9,494,900,816 bytes consed
;; NIL

;; Case 3: Core Streams Overhead With Parenscript
;; SERVER> (let ((s (make-core-stream "")))
;; 	  (time 
;; 	   (dotimes (i 10000)
;; 	     (string! s (dojo-old)))))
;; Evaluation took:
;;   68.424 seconds of real time
;;   68.764297 seconds of total run time (67.336208 user, 1.428089 system)
;;   [ Run times consist of 7.472 seconds GC time, and 61.293 seconds non-GC time. ]
;;   100.50% CPU
;;   164,231,866,818 processor cycles
;;   9,688,441,856 bytes consed
;; NIL

;; Case 4: Stream Overhead with Cached Response
;; SERVER> (defvar *g (dojo-old))
;; *G
;; SERVER> (let ((s (make-core-stream "")))
;; 	  (time 
;; 	   (dotimes (i 10000)
;; 	     (string! s *g))))
;; Evaluation took:
;;   11.012 seconds of real time
;;   11.016689 seconds of total run time (10.952685 user, 0.064004 system)
;;   [ Run times consist of 0.072 seconds GC time, and 10.945 seconds non-GC time. ]
;;   100.05% CPU
;;   26,430,148,782 processor cycles
;;   175,730,464 bytes consed
;; NIL

;; Case 5: Compressed Renderer without indentation
;; SERVER> (let ((s (make-compressed-stream "")))
;; 	  (time 
;; 	   (dotimes (i 10000)
;; 	     (dojo s))))
;; Evaluation took:
;;   36.728 seconds of real time
;;   36.946308 seconds of total run time (36.330270 user, 0.616038 system)
;;   [ Run times consist of 2.340 seconds GC time, and 34.607 seconds non-GC time. ]
;;   100.59% CPU
;;   88,156,341,468 processor cycles
;;   4,240,289,552 bytes consed
;; NIL

;; Case 6: After aggressive optimization (optimize-render-1)
;; SERVER> (let ((s (make-indented-stream "" 2)))
;; 	  (time
;; 	   (dotimes (i 10000)
;; 	     (dojo s))))
;; Evaluation took:
;;   40.387 seconds of real time
;;   40.546534 seconds of total run time (39.778486 user, 0.768048 system)
;;   [ Run times consist of 3.896 seconds GC time, and 36.651 seconds non-GC time. ]
;;   100.40% CPU
;;   96,936,993,369 processor cycles
;;   4,381,989,984 bytes consed
;; NIL

;; Case 7: After aggressive optimization (optimize-render-1)
;; SERVER> (let ((s (make-compressed-stream "")))
;; 	  (time
;; 	   (dotimes (i 10000)
;; 	     (dojo s))))
;; Evaluation took:
;;   36.379 seconds of real time
;;   36.046253 seconds of total run time (35.602225 user, 0.444028 system)
;;   [ Run times consist of 3.641 seconds GC time, and 32.406 seconds non-GC time. ]
;;   99.08% CPU
;;   87,315,012,855 processor cycles
;;   4,019,826,784 bytes consed
;; NIL


;; (defrender/js moo (&optional base-url (debug nil) (back 'false))
;;   (list base-url debug back)
;;   (lambda ()
;;     (list 1 2 3)))

;; (defun/javascript fun1 (a)
;;   (let ((b (lambda (b c)
;; 	     (list b c))))
;;     (return (b))))

;; (defmacro defun/parenscript (name params &body body)
;;   `(prog1
;;        (defun ,name ,params ,@body)
;;      (defun ,(intern (string-upcase (format nil "~A!" name))) (s)
;;        (write-stream s
;; 		     ,(js:js* `(lambda ,params
;; 				 ,@body))))))

;; (defun/parenscript fun2 (a)
;;   (let ((a (lambda (a b c)
;; 	     (list a b c))))
;;     (return a)))

;; (js+ 
;;   (+ 1 1)
;;   (+ 2 2))

;; (defun <:js (&rest body)
;;   "Convert body to javascript and return it as a string, this is an ucw+
;; backward compatiblity macro, and should not be used."
;;   (with-unique-names (output)
;;     (eval
;;      `(let ((,output (if +context+
;; 			 (http-response.stream (response +context+))
;; 			 *core-output*)))
;; 	(funcall (lambda ()
;; 		   (block rule-block
;; 		     ,(expand-render
;; 		       (walk-grammar
;; 			`(:and ,@(mapcar (rcurry #'expand-javascript #'expand-javascript)
;; 					 (mapcar #'walk-form-no-expand body))
;; 			       #\Newline))
;; 		       #'expand-render output)
;; 		     nil)))))))
