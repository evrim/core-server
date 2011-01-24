;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :core-server)

;;+-----------------------------------------------------------------------------
;;| Javascript Library
;;+-----------------------------------------------------------------------------
;;
;; This file contains javascript library functions.
;;

(defrender/js core-library! ()
  (defun atom (a)
    (cond
      ((null a) t)
      ((null a.length) t)
      ((typep a 'string) t)
      (t nil)))

  (defun listp (a)
    (not (atom a)))
  
  (defun/cc reduce-cc (fun lst initial-value)
    (if (or (null lst) (= 0 lst.length))
	initial-value
	(reduce-cc fun (cdr lst) (call/cc fun initial-value (car lst)))))
  
  (defun reduce (fun lst initial-value)
    (if (null lst)
    	nil    
    	(let ((result (or (and (not (typep initial-value 'undefined)) initial-value) nil)))
    	  (if (not (null lst.length))
	      (dolist (item lst)
		(setf result (fun result item)))
	      (doeach (item lst)
		(setf result (fun result (aref lst item)))))
    	  result)))

  (defun/cc reduce0-cc (fun lst)
    (reduce-cc fun lst nil))
  
  (defun reduce0 (fun lst)
    (reduce fun lst nil))

  (defun/cc reverse-cc (lst)
    (if (null lst)
	nil
	(reduce0-cc (lambda (acc a) (cons a acc)) lst)))
  
  (defun reverse (lst)
    (if (null lst)
	nil
	(reduce0 (lambda (acc a) (cons a acc)) lst)))

  (defun flatten (lst acc)
    (if (typep acc 'undefined)
	(setf acc (list)))
    
    (cond
      ((null lst) nil)
      ((not (instanceof lst *array)) (cons lst acc))
      ((not lst.length) acc)
      (t
       (flatten (car lst) (flatten (cdr lst) acc)))))

  (defun filter (fun lst)
    (reverse
     (reduce0 (lambda (acc atom)
		(if (fun atom)
		    (cons atom acc)
		    acc))
	      lst)))

  (defun/cc filter-cc (fun lst)
    (reverse-cc
     (reduce0-cc (lambda (acc a)
		  (if (call/cc fun a) (cons a acc) acc))
		 lst)))
  
  (defun cons (atom lst)
    (if (null lst)
	(array atom)
	(.concat (array atom)
		 (if (instanceof lst *array)
		     lst
		     (list lst)))))

  (defun car (lst)
    (cond
      ((null lst) nil)
      ((instanceof lst *array) (aref lst 0))
      ((and (typep lst 'object)
	    (not (null lst.length))
	    (> lst.length 0))
       (aref lst 0))
      ((typep lst 'object)
       (let ((result))
	 (doeach (i lst)
	   (setf result (aref lst i)))
	 result))
      (t nil)))

  (defun cdr (lst)
    (cond
      ((null lst) nil)
      ((atom lst) lst)
      ((and (or (typep lst '*array)
		(typep lst 'object))
	    (not (null (slot-value lst 'slice))))
       (.slice lst 1))
      (t
       (cdr (mapcar (lambda (a) (return a))
		    lst)))))

  (defun nth (seq lst)
    (if (= 0 seq)
	(car lst)
	(nth (1- seq) (cdr lst))))

  (defun/cc mapcar-cc (fun lst)
    (reverse-cc
     (reduce-cc (lambda (acc atom)
		  (cons (call/cc fun atom) acc))
		lst
		null)))
  
  (defun mapcar (fun lst)
    (reverse
     (reduce (lambda (acc atom)
	       (cons (fun atom) acc))
	     lst)))

  (defun mapcar2 (fun lst1 lst2)
    (mapcar (lambda (index)
	      (fun (nth index lst1) (nth index lst2)))
	    (seq (*math.min (slot-value lst1 'length)
			    (slot-value lst2 'length)))))

  (defun/cc mapcar2-cc (fun lst1 lst2)
    (mapcar-cc (lambda (index)
		 (call/cc fun (nth index lst1) (nth index lst2)))
	       (seq (*math.min (slot-value lst1 'length)
			       (slot-value lst2 'length)))))
  
  (defun any (fun lst)
    (dolist (i lst)
      (if (fun i)
	  (return i)))
    (return nil))
  
  (defun seq (num)
    (cond
      ((>= num 1)
       (reverse (cons (- num 1) (reverse (seq (- num 1))))))
      (t (array))))
  
  (defun mapobject (fun obj)
    (let ((result (new (*object))))
      (doeach (i obj)
	(setf (slot-value result i)
	      (fun i (slot-value obj i))))
      result))

  (defun member (obj lst)
    (reduce (lambda (acc atom)
	      (or acc (equal atom obj)))
	    lst))

  (defun sort (fun lst)
    (let ((first (car lst))
	  (rest (cdr lst)))
      (if first
	  (flet ((compare (a) (fun first a)))
	    (append (sort fun (filter compare rest))
		    (cons first
			  (sort fun
				(filter (lambda (a) (not (compare a)))
					rest))))))))

  (defun uniq (key-fun lst)
    (reverse
     (reduce (lambda (acc atom)
	       (if (eq (key-fun (car acc)) (key-fun atom))
		   acc
		   (cons atom acc)))
	     (sort (lambda (a b) (eq (key-fun a) (key-fun b))) lst))))
  
  (defun has-class (node class-name)
    (if (member class-name (node.class-name.split " "))
	t
	nil))
  
  (defun remove-class (node class-name)    
    (let ((classes (node.class-name.split " ")))
      (when (and classes classes.length)
	(let ((classes (filter (lambda (a)
				 (not (equal class-name a)))
			       classes)))
	  (if (and classes classes.length)
	      (setf node.class-name (.join classes " "))
	      (setf node.class-name "")))))
    node)

  (defun add-class (node class-name)
    (remove-class node class-name)
    (setf node.class-name (+ class-name " " node.class-name))
    node)

  (defun replace-node (old-node new-node)
    (old-node.parent-node.replace-child new-node old-node)
    new-node)

  (defun trim (str)
    (.replace (new (*string str)) (regex "/^\s+|\s+$/g") ""))
  
  (defun show (node)
    (when (and node (slot-value node 'style))	;; (instanceof node *h-t-m-l-element)
      (setf node.style.display "block"))
    node)

  (defun hide (node)
    (when (and node (slot-value node 'style)) ;; (instanceof node *h-t-m-l-element)
      (setf node.style.display "none"))
    node)

  (defun inline (node)
    (when (and (slot-value node 'style))	;; (instanceof node *h-t-m-l-element)
      (setf node.style.display "inline"))
    node)

  (defun coretal-on-load (fun)
    (if (and (not (null (slot-value window 'coretal)))
	     (not (null (slot-value coretal 'loaded-p))))
	(make-web-thread fun)
	(make-web-thread (lambda () (coretal-on-load fun)))))
  
  (defun add-on-load (fun)
    (if (or (eq "complete" document.ready-state)
	    (eq "interactive" document.ready-state)) ;; yalan bu ya
	(fun)
	(if (typep window.onload 'function)
	    (let ((current window.onload))
	      (setf window.onload
		    (lambda ()
		      (current)
		      (fun))))
	    (setf window.onload fun))))

  (defun connect (target event lambda)
    (if (typep target 'string)
	(setf (slot-value ($ target) event) lambda)
	(setf (slot-value target event) lambda)))

  (defun prepend (to item)
    (if to.first-child
	(to.insert-before item to.first-child)
	(to.append-child item)))

  (defun append (to item)
    (if (null to)
	item
	(if (null item)
	    to
	    (if (slot-value to 'node-name)
		(to.append-child item)
		(reduce (flip cons)
			(reduce (flip cons) to)
			(reverse (reduce (flip cons) item)))))))

  (defun flip (fun) (return (lambda (a b) (fun b a))))
  (defun flip-cc (fun k) (return (lambda (a b k) (fun b a k))))

  (defun node-search (goal-p context)
    (let ((context (or context document)))
      (filter goal-p (.get-elements-by-tag-name context "*"))))

  (defun find (goal-p lst)
    (reduce (lambda (acc atom)
	      (or acc (and (goal-p atom) atom)))
	    lst nil))

  (defun/cc find-cc (goal-p lst)
    (reduce-cc (lambda (acc atom)
		 (or acc (and (call/cc goal-p atom) atom)))
	       lst nil))
  
  (defun node2str (item)
    (try
     (return (.serialize-to-string (new (*x-m-l-serializer)) item))
     (:catch (e)
       (try (return item.xml)
	(:catch (e)
	  (throw
	      (new
	       (*error
		(+ "Node " item " cannot be serialized to string.")))))))))
  
  (defun serialize (object)
    (labels ((serialize (object)
	     (cond
	       ((typep object 'undefined)
		"{}")
	       ((typep object 'boolean)
		(if object "true" "false"))
	       ((null object)
		"null")
	       ((typep object 'number)
		object)
	       ((typep object 'string)
		(+ "\"" (encode-u-r-i-component object) "\""))
	       ((instanceof object '*array)
		(if (> (slot-value object 'length) 0)
		    (+ "["		   
		       (.join (mapcar (lambda (item)
					(return (serialize item)))
				      object)
			      ",")
		       "]")
		    "[]"))
	       ((typep object 'object)
		(let ((result (escape "{"))
		      (keys))
		  (labels ((one (key value)
			     (setf result (+ result (serialize key) ":"
					     (serialize value)))))
		    (doeach (i object) (setf keys (cons i keys)))
		    (when (car keys)
		      (one (car keys) (slot-value object (car keys))))
		    (mapcar (lambda (key)
			      (setf result (+ result ","))
			      (one key (slot-value object key))) 
			    (cdr keys))
		    (return (+ result (escape "}"))))))
	       (t
		(throw (new (*error (+ "Could not serialize " object))))
		nil))))
    (serialize object)))

  (defun funcall (action arguments)
    (if window.*active-x-object
	(setf xhr (new (*active-x-object "Microsoft.XMLHTTP")))
	(setf xhr (new (*x-m-l-http-request))))
    (xhr.open "POST" action false)
    (xhr.set-request-header "Content-Type" "text/json")
    (xhr.send (serialize arguments))

    (if (not (= 200 xhr.status))
	(throw (new (*error (+ "Server error occured: " xhr.status)))))
            
    (let ((content-type (xhr.get-response-header "Content-Type")))
      (if (null content-type)
	  (throw (new (*error "Content-Type of the response is not defined"))))

      (setf content-type (aref (content-type.split ";") 0))
      
      (cond
	((or (eq content-type "text/json") (eq content-type "text/javascript"))
	 (eval (+ "("  xhr.response-text ")")))
	((eq content-type "text/html")
	 (let ((div (document.create-element "div")))
	   (setf div.inner-h-t-m-l xhr.response-text)
	   (cond
	     ((eq 0 div.child-nodes.length)
	      nil)
	     ((eq 1 div.child-nodes.length)
	      (aref div.child-nodes 0))
	     (t
	      div)))))))

  (defun serialize-to-uri (arg)
    (let ((result ""))
      (mapobject (lambda (k v)
		   (setf result
			 (+ result k ":"
			    (encode-u-r-i-component (serialize v))
			    "$")))
		 arg)
      result))

  (defun _debug (what k)
    (if (and (not (null console))
    	     (not (null console.debug)))
    	(console.debug what))
    (if (typep k '*function)
    	(k what)
    	what))
  
  (defun/cc funcall-long-cc (action args)
    (labels ((do-parts (str acc)
	       (if (and str (> (slot-value str 'length) 0))
		   (call/cc do-parts (.substr str 1000)
			    (cons (.slice (new (*string str)) 0 1000) acc))
		   acc)))
      (funcall-cc (+ "multipart.core"
		     (reduce-cc
		      (lambda (acc atom)
			(funcall-cc (+ "multipart.core" acc "$")
				    (jobject :data atom)))
		      (reverse-cc (do-parts (serialize-to-uri args) nil))
		      (funcall-cc "multipart.core?" (jobject :action action)))
		     "$")
		  (jobject :commit t))))
  
  (defun/cc funcall-cc (action args)
    (_debug (list "funcall/cc" action args))
    (let/cc current-continuation
      (let ((hash (+ "__result"
		     (.get-time (new (*date)))
		     (.substr (.concat "" (*math.random 10)) 3 5)))
	    (img (make-dom-element "IMG"
				   (jobject :class-name "coretal-loading"
					    :src (+ "http://www.coretal.net/style/"
						    "login/loading.gif"))
				   nil))
	    (args (if (null args) (jobject) args)))
	(setf (slot-value args "__hash") hash)
	(let* ((a (serialize-to-uri args))
	       (script (make-dom-element "script" (jobject :src (+ "" action a))
					 nil))
	      (head (aref (.get-elements-by-tag-name document "HEAD") 0))
	      (body (slot-value document 'body)))
	  (cond
	    ((and a (slot-value a 'length) (> (slot-value a 'length) 2000))
	     (funcall-long-cc action args))
	    (t	     
	     (setf (slot-value window hash)
		   (event (val)
			  (when (not (null (slot-value script 'parent-node)))
			    (.remove-child head script)
			    (if body (.remove-child body img)))
			  (current-continuation val)))
	     (if body (append body img))
	     (append head script)
	     (suspend)))))))

  (defun make-dom-element (tag properties children)
    (let ((element (document.create-element tag))
	  (children (flatten children)))
      (when (slot-value properties 'type)
	(setf (slot-value element 'type) (slot-value properties 'type))
	(delete-slot properties 'type))
      (mapcar (lambda (i)
		(cond
		  ((not (null (slot-value i 'tag-name)))
		   (element.append-child i))
		  (t
		   (element.append-child (document.create-text-node i)))))
	      children)
      (extend properties element)
      element))
  
  (defun get-parameter (name href)
    (let ((data nil))
      (if (not (null href))
	  (setf data (.substr (.substr href (.search href "#")) 1))
	  (setf data (+ (.substr window.location.hash 1) "$"
			(.substr window.location.search 1))))
      
      (if (null name)
	  (let ((_value (decode-u-r-i-component (car (.split data "$")))))
	    (try (return (eval _value)) (:catch (e) (return _value))))
	  (let ((_value (decode-u-r-i-component
			 (car
			  (cdr
			   (car
			    (filter (lambda (a) (eq (car a) name))
			      (mapcar (lambda (a)
					(flatten
					 (mapcar (lambda (b) (.split b "="))
						 (.split a ":"))))
				      (.split data "$")))))))))
	    (try (return (eval _value))
		 (:catch (e) (return _value)))))))

  (defun set-parameter (name new-value)
    (let* ((new-value (encode-u-r-i-component (serialize new-value)))
	   (append2 (lambda (a b)
		      (if (null a) b (if (null b) "" (+ a b)))))
	   (one (lambda (a) (append2 (car a) (append2 ":" (car (cdr a))))))
	   (found nil)
	   (elements (reverse
		      (reduce
		       (lambda (acc a)
			 (destructuring-bind (key value) a
			   (cond
			     ((eq name key)
			      (setf found t)
			      (if (eq new-value "null")
				  acc
				  (cons (cons name new-value) acc)))
			     (t (cons a acc)))))
		       (mapcar (lambda (a) (.split a ":"))
			       (.split (.substr window.location.hash 1) "$"))))))
      (if (and (null found) name)
	  (setf elements (cons (cons name new-value) elements)))
      (setf window.location.hash
	    (reduce (lambda (acc a)
		      (append2 acc (append2 "$" (one a))))
		    (cdr elements)
 		    (if (and (null new-value)
			     (null (car (cdr (car elements)))))
			name
			(one (car elements)))))))

  (defun load-css (url)
    (let ((link (document.create-element "link")))
      (setf link.href url
	    link.rel "stylesheet"
	    link.type "text/css")
      (.append-child (aref (document.get-elements-by-tag-name "head") 0)
		     link)
      (return link)))

  (defun remove-css (url)
    (mapcar (lambda (link)
	      (when (eq link.href url)
		(link.parent-node.remove-child link)))
	    (document.get-elements-by-tag-name "LINK")))
  
  (defun/cc load-javascript (url loaded-p)
    (_debug (list "load-javascript" url loaded-p))
    (let/cc current-continuation
      (let* ((img (make-dom-element "IMG"
		   (jobject :class-name "coretal-loading"
			    :src "http://www.coretal.net/style/login/loading.gif")
		   nil))
	     (script (make-dom-element "SCRIPT"
				       (jobject :type "text/javascript"
						:src url)
				       nil))
	     (head (aref (.get-elements-by-tag-name document "HEAD") 0))
	     (body (slot-value document 'body))
	     (loaded-p (or loaded-p (lambda () t)))
	     (recurse
	      (lambda (r)
		(cond		   
		  ((loaded-p window.k)
		   (when (not (null (slot-value script 'parent-node)))
		     (.remove-child head script)
		     (if body (.remove-child body img)))
		   (current-continuation null))
		  (t
		   (make-web-thread (lambda () (Y r)))
		   (suspend))))))
	(when (not (loaded-p window.k))
	  (if body (append body img))
	  (append head script)
	  (Y recurse)))))
  
;; +-------------------------------------------------------------------------
;; | Identity Continuation
;; +-------------------------------------------------------------------------
  (defun k (value)
    value)

  (defun extend (source target)
    (let ((target (or target (new (*object)))))
      (mapobject (lambda (k v)
		   (try (setf (slot-value target k) v)
			(:catch (err) (_debug err))))
		 source)
      target))

  (defun apply (fun scope args kX)
    (fun.apply scope (reverse (cons kX (reverse args)))))
  
  (defun make-web-thread (fun)
    (window.set-timeout (lambda () (fun (lambda (a) a))) 0))

  ;; FIX IE stack overflow bug
  (defun make-method (method)
    (if (> (.search navigator.user-agent "MSIE") 0)
	(return
	  (lambda ()
	    (let ((args arguments)
		  (self this))
	      (make-web-thread
	       (lambda ()
		 (apply method self args))))))
	method))

  (defun compose-progn1 (fun1 fun2)
    (cond
      ((not (eq "function" (typeof fun2)))
       fun1)
      ((not (eq "function" (typeof fun1)))
       fun2)
      (t
       (return
	 (lambda ()	   
	   (let* ((args (reverse (reverse arguments)))
		  (k (car (reverse args)))
		  (args (reverse (cdr (reverse args))))
		  (self this))
	     (apply fun1 self args
		    (lambda (val) (apply fun2 self args k)))))))))
  
  (defun random-string (len)
    (let ((len (or len 8))
	  (alphabet (+  "0123456789" "ABCDEFGHIJKLMNOPQRSTUVWXTZ"
			"abcdefghiklmnopqrstuvwxyz")))
      (flet ((one ()
	       (aref alphabet
		     (*math.floor
		      (* (*math.random) (slot-value alphabet 'length))))))
	(reduce (lambda (acc atom) (+ acc (one)))
		(seq (- len 1)) (one)))))

  (defun lisp-date-to-javascript (universal-time)
    (let ((b 3155666400)
	  (a 946677600000))
      (new (*date (+ a (* 1000 (- universal-time b)))))))
  
  (defun date-to-string (date)
    (flet ((pad-me (foo)
	     (if (< foo 10)
		 (+ "0" foo)
		 foo)))
      (+
       (pad-me (date.get-day)) "/"
       (pad-me (date.get-month)) "/"
       (date.get-full-year) " - "
       (pad-me (date.get-hours)) ":"
       (pad-me (date.get-minutes)) ":"
       (pad-me (date.get-seconds)))))

  (defun/cc Y (f)
    (f f)
    (suspend))

  (defun/cc Y1 (arg1 f)
    (f arg1 f)
    (suspend)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf (gethash 'make-component +javascript-cps-functions+) t
	(gethash 'make-service +javascript-cps-functions+) t
	(gethash 'apply +javascript-cps-functions+) t
	(gethash 'funcall-cc +javascript-cps-functions+) t
	(gethash 'make-web-thread +javascript-cps-functions+) t
	(gethash 'mapcar-cc +javascript-cps-functions+) t
	(gethash 'reverse-cc +javascript-cps-functions+) t
	(gethash 'reduce0-cc +javascript-cps-functions+) t
	(gethash 'reduce-cc +javascript-cps-functions+) t
	(gethash 'filter-cc +javascript-cps-functions+) t
	(gethash 'load-javascript +javascript-cps-functions+) t
	(gethash 'find-cc +javascript-cps-functions+) t
	(gethash 'flip-cc +javascript-cps-functions+) t
	(gethash 'mapcar2-cc +javascript-cps-functions+) t))


  ;; (defvar *registry* (create))  
  ;; (defun/cc make-service (name properties)
  ;;   (let ((service (slot-value *registry* name)))
  ;;     (if (not (null service))
  ;; 	  service
  ;; 	  (let ((retval (funcall-cc "service.core?" (create :service name))))
  ;; 	    (let ((instance (call/cc retval properties null)))
  ;; 	      (setf (slot-value *registry* name) instance)
  ;; 	      instance)))))

;; (defun/cc make-component (name properties to-extend)
  ;;   (let ((retval (slot-value *registry* name)))
  ;;     (if (not (null retval))
  ;;  	  (call/cc retval properties to-extend)
  ;;  	  (progn
  ;;  	    (setf (slot-value *registry* name)
  ;;  		  (funcall-cc "component.core?" (create :component name)))
  ;;  	    (make-component name properties to-extend)))))

;; ;; +-------------------------------------------------------------------------
;; ;; | 'new' Operator replacement for Continuations
;; ;; +-------------------------------------------------------------------------
;;   (defun make-instance (k ctor arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
;;     (if (> arguments.length 9)
;; 	(throw (new (*error (+ "Cannot makeInstance, too many arguments:"
;; 			       arguments.length ", ctor:" ctor))))
;; 	(cond
;; 	  ((not (typep arg8 'undefined))
;; 	   (k (new (ctor arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8))))
;; 	  ((not (typep arg7 'undefined))
;; 	   (k (new (ctor arg1 arg2 arg3 arg4 arg5 arg6 arg7))))
;; 	  ((not (typep arg6 'undefined))
;; 	   (k (new (ctor arg1 arg2 arg3 arg4 arg5 arg6))))
;; 	  ((not (typep arg5 'undefined))
;; 	   (k (new (ctor arg1 arg2 arg3 arg4 arg5))))
;; 	  ((not (typep arg4 'undefined))
;; 	   (k (new (ctor arg1 arg2 arg3 arg4))))
;; 	  ((not (typep arg3 'undefined))
;; 	   (k (new (ctor arg1 arg2 arg3))))
;; 	  ((not (typep arg2 'undefined))
;; 	   (k (new (ctor arg1 arg2))))
;; 	  ((not (typep arg1 'undefined))
;; 	   (k (new (ctor arg1))))
;; 	  (t
;; 	   (k (new (ctor)))))))
