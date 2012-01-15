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

;;+--------------------------------------------------------------------------
;;| Javascript Library
;;+--------------------------------------------------------------------------
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

  (defun null-p (a)
    (or (null a) (eq "undefined" a) (eq "" a)))
  
  (defun/cc reduce-cc (fun lst initial-value)
    (if (or (null lst) (= 0 lst.length))
	initial-value
	(reduce-cc fun (cdr lst) (call/cc fun initial-value (car lst)))))
  
  (defun reduce (fun lst initial-value)
    (if (null lst)
    	nil    
    	(let ((result (or (and (not (typep initial-value 'undefined))
			       initial-value) nil)))
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
  
  (defun flatten1 (lst)
    (reduce0 (lambda (acc a)
	       (if acc
		   (.concat a acc)
		   (list a)))
	     lst))

  (defun filter (fun lst)
    (reverse
     (reduce0 (lambda (acc atom)
		(if (fun atom)
		    (cons atom acc)
		    acc))
	      lst)))

  (defun take (n lst)
    (if (typep lst 'string)
	(.join (take n (.split lst "")) "")
	(cond
	  ((null (car lst)) nil)
	  ((> n 0) (cons (car lst) (take (- n 1) (cdr lst))))
	  (t nil))))

  (defun drop (n lst)
    (if (typep lst 'string)
	(.join (drop n (.split lst "")) "")
	(cond
	  ((> n 0)
	   (drop (- n 1) (cdr lst)))
	  (t lst))))
  
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
      ((and (not (null lst.length)) (eq lst.length 0))
       nil)
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

  (defun object-to-list (obj)
    (let ((result))
      (mapobject (lambda (k v)
		   (setf result (cons (list k v) result)))
		 obj)
      (reverse result)))
  
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

  (defun remove (item lst)
    (reverse
     (reduce0 (lambda (acc atom)
		(if (eq item atom)
		    acc
		    (cons atom acc)))
	      lst)))
  
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
    (.replace (new (*string str)) (regex "/^\\s+|\\s+$/g") ""))
  
  (defun show (node)
    (when (and node (slot-value node 'style))
      (setf node.style.display "block"))
    node)

  (defun is-showing (node)
    (not (eq "none" node.style.display)))
  
  (defun hide (node)
    (when (and node (slot-value node 'style))
      (setf node.style.display "none"))
    node)

  (defun inline (node)
    (when (and (slot-value node 'style))
      (setf node.style.display "inline"))
    node)  

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

  (defun scroll-to-node (node)
    (labels ((parent-search (root acc)
	       (cond
		 ((and root (slot-value root 'offset-parent))
		  (parent-search (slot-value root 'offset-parent)
				 (cons root acc)))
		 (t acc))))
      (let* ((parents (parent-search node (list)))
	     (pos-x (reduce (lambda (acc atom)
			      (+ acc (slot-value atom 'offset-left)))
			    parents 0))
	     (pos-y (reduce (lambda (acc atom)
			      (+ acc (slot-value atom 'offset-top)))
			    parents 0)))
	(window.scroll-to pos-x (- pos-y 50)))))

  (defun flip (fun) (return (lambda (a b) (fun b a))))
  (defun flip-cc (fun k) (return (lambda (a b k) (fun b a k))))

  (defun node-search (goal-p context)
    (let ((context (or context document)))
      (filter goal-p (.get-elements-by-tag-name context "*"))))

  (defun parent-search (goal-p context)
    (cond
      ((null context)
       nil)
      ((goal-p context)
       context)
      (t
       (parent-search goal-p (slot-value context 'parent-node)))))
  
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
		  (+ "\""
		     ;; (.replace (.replace (new (*string object))
		     ;; 			 (regex "/\\\\/g")
		     ;; 			 "\\\\\\\\") 
		     ;; 	       (regex "/\"/g")
		     ;; 	       "\\\\\"")
		     (.replace (new (*string object))
			       (regex "/\"/g")
			       "\\\"")
		     "\""))
		 ((instanceof object '*array)
		  (if (> (slot-value object 'length) 0)
		      (+ "["		   
			 (.join (mapcar (lambda (item)
					  (return (serialize item)))
					object)
				",")
			 "]")
		      "[]"))
		 ((typep (slot-value object 'get-time) 'function)
		  (serialize (.get-time object)))
		 ((typep object 'object)
		  (let ((result "{")
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
		      (return (+ result "}")))))
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

  (defun local-url-exists-p (url)
    (try (progn (funcall url nil) (return t))
	 (:catch (e) (return nil))))

  (defun local-serialize-to-uri (arg)
    (let ((result ""))
      (mapobject (lambda (k v)
		   (setf result
			 (+ result k "=" (encode-u-r-i-component v)
			    "&")))
		 arg)
      result))
  
  (defun local-funcall (action arguments)
    (if window.*active-x-object
	(setf xhr (new (*active-x-object "Microsoft.XMLHTTP")))
	(setf xhr (new (*x-m-l-http-request))))
    (xhr.open "POST" action false)
    (xhr.set-request-header "Content-Type" "application/x-www-form-urlencoded")    
    (xhr.send (local-serialize-to-uri arguments))

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
	   xhr.response-text
	   ;; (setf div.inner-h-t-m-l xhr.response-text)
;; 	   (cond
;; 	     ((eq 0 div.child-nodes.length)
;; 	      nil)
;; 	     ((eq 1 div.child-nodes.length)
;; 	      (aref div.child-nodes 0))
;; 	     (t
;; 	      div))
	   )))))

  (defun local-funcall-head (action arguments)
    (if window.*active-x-object
	(setf xhr (new (*active-x-object "Microsoft.XMLHTTP")))
	(setf xhr (new (*x-m-l-http-request))))
    
    (xhr.open "HEAD" action false)
    (xhr.set-request-header "Content-Type"
			    "application/x-www-form-urlencoded")    
    (xhr.send (local-serialize-to-uri arguments))

    (return xhr))
  
  (defun serialize-to-uri (arg)
    (let ((arg (object-to-list arg)))
      (reduce (lambda (acc a)
		(+ acc "&" (car a) "=" (encode-u-r-i-component (serialize (car (cdr a))))))
	      (cdr arg)
	      (+ (car (car arg)) "=" (encode-u-r-i-component (serialize (car (cdr (car arg)))))))))

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
		   acc))
	     (get-multipart-url ()
	       (let* ((lst (reverse (.split (.substr action 7) "/")))
		      (name (car lst))
		      (dirs (reverse (cdr lst)))
		      (session (car (.split (car (reverse (.split name "?")))
					    "$"))))
		 (+ "http://" (reduce-cc (lambda (a b) (+ a "/" b))
					 (cdr dirs) (car dirs))
		    "/multipart.core?" session))))
      (let ((url (call/cc get-multipart-url))
	    (args (serialize-to-uri args)))
	(funcall-cc (+ url
		       (reduce-cc
			(lambda (acc atom)
			  (funcall-cc (+ url acc "$") (jobject :data atom)))
			(reverse-cc (do-parts args nil))
			(funcall-cc (+ url "$") (jobject :action action)))
		       "$")
		    (jobject :commit t)))))

  (defun/cc funcall2-cc (action args callback-name)
    (_debug (list "funcall/cc" action args))
    (let/cc current-continuation
      (let* ((args (if (null args) (jobject) args))
	     (hash (if (slot-value args '__hash)
		       (let ((__hash (slot-value args '__hash)))			 
			 (delete-slot args '__hash)
			 __hash)
		       (+ "__result"
			  (.get-time (new (*date)))
			  (.substr (.concat "" (*math.random 10)) 3 5)))))
	(let ((img (make-dom-element "IMG"
				     (jobject :class-name "coretal-loading"
					      :src (+ "http://www.coretal.net/style/"
						      "login/loading.gif"))
				     nil)))
	  (setf (slot-value args (or callback-name "__hash")) hash)
	  (let* ((a (serialize-to-uri args))
		 (script (make-dom-element "script" (jobject :type "text/javascript") nil))
		 (head (aref (.get-elements-by-tag-name document "HEAD") 0))
		 (body (slot-value document 'body)))
	    (cond
	      ((and a (slot-value a 'length) (> (slot-value a 'length) 2000))
	       (funcall-long-cc action args))
	      (t	     
	       (setf (slot-value window hash)
		     (event (val)
			    (when (not (null (slot-value script 'parent-node)))
			      ;; (.remove-child head script)
			      (if (and (slot-value img 'parent-node) body)
				  (.remove-child body img)))
			    (current-continuation val)))
	       (if body (append body img))
	       (append head script)
	       (setf (slot-value script 'src) (+ "" action a))
	       (suspend))))))))

  (defun/cc funcall-cc (action args)
    (funcall2-cc action args nil))

  (defun make-dom-element (tag properties children)
    (let ((element (document.create-element tag))
	  (children (flatten children)))
      (when (slot-value properties 'type)
	(setf (slot-value element 'type) (slot-value properties 'type))
	(delete-slot properties 'type)

	(when (or (eq "checkbox" (slot-value element 'type))
		  (eq "radio" (slot-value element 'type)))
	  (let ((fr (document.create-document-fragment)))
	    (fr.append-child element))))
      
      (mapcar (lambda (i)
		(cond
		  ((not (null (slot-value i 'tag-name)))
		   (element.append-child i))
		  (t
		   (element.append-child (document.create-text-node i)))))
	      children)
      (extend properties element)
      element))

  (defun get-cookie (name)
    (let* ((cookies (mapcar (lambda (a) (.split a "="))
			    (.split document.cookie ";")))
	   (found (find (lambda (a) (eq (trim (car a)) name)) cookies)))
      (if found
	  (return (car (cdr found)))
	  (return nil))))

  (defun set-cookie (name value)
    (setf document.cookie (+ name "=" value)))  

  (defun get-parameter (name href)
    ;; (_debug (list "get-parameter" name href))
    (let* ((href (or (and href (.substr (.substr href (.search href "#")) 1))
		     (+ (.substr (.substr window.location.href
					  (.search window.location.href "#"))
				 1) "$"
			(.substr window.location.search 1))))
	   (key-value (reduce (lambda (acc a)
				(let ((b (.split a "=")))
				  (if (eq (slot-value b 'length) 1)
				      (let ((b (.split a ":")))
					(if (eq (slot-value b 'length) 1)
					    acc
					    (cons b acc)))
				      (cons b acc))))
			      (reduce (lambda (acc a)
					(let ((b (.split a "&")))
					  (if (eq (slot-value b 'length) 1)
					      (cons a acc)
					      (.concat acc b))))
				      (.split href "$")
				      (list))
			      (list)))
	   (result (find (lambda (item) (if (eq name (car item)) t))
			 key-value)))
      (if result
	  (return (decode-u-r-i-component (car (cdr result))))
	  (return nil))))
  
  (defun set-parameter (name value)
    ;; (_debug (list "set-parameter" name value))
    (if (eq (get-parameter name) value)
	(return nil))
    
    (let* ((found nil)
	   (result (reduce
		    (lambda (acc atom)
		      (cond
			((or (null (car (cdr atom)))
			     (eq (car (cdr atom)) ""))			     
			 acc)
			((and (or (null value) (eq value ""))
			      (eq name (car atom)))
			 (setf found t)
			 acc)
			((eq name (car atom))			 
			 (setf found t)
			 (+ acc (car atom) ":"
			    (encode-u-r-i-component value) "$"))
			(t
			 (+ acc (car atom) ":" (car (cdr atom)) "$"))))
		    (mapcar (lambda (a) (.split a ":"))
			    (.split (.substr window.location.hash 1) "$"))
		    (new (*string "")))))
      (if (or found (or (null value) (eq "" value)))
	  (setf window.location.hash result)
	  (setf window.location.hash (+ result name ":" value)))))

  (defun get-style (node)
    (cond
      ((and document.default-view
	    document.default-view.get-computed-style)
       (document.default-view.get-computed-style node null))
      ((slot-value node 'current-style)
       (slot-value node 'current-style))
      (t
       (_debug (list "cant (get-style " node ")")))))
  
  (defvar *css-refcount-table* (jobject))
  (defun load-css (url)
    (flet ((_load-css ()
	     (let ((link (make-dom-element "LINK"
					   (jobject :href url
						    :rel "stylesheet"
						    :type "text/css"))))
	       (.append-child (aref (document.get-elements-by-tag-name "head") 0)
			      link)
	       (return link))))
      (cond
	((slot-value *css-refcount-table* url)
	 (setf (slot-value *css-refcount-table* url)
	       (+ 1 (slot-value *css-refcount-table* url))))
	(t
	 (setf (slot-value *css-refcount-table* url) 1)
	 (_load-css)))
      url))

  (defun remove-css (url)
    (flet ((_unload-css ()
	     (mapcar (lambda (link)
		       (when (eq link.href url)
			 (link.parent-node.remove-child link)))
		     (document.get-elements-by-tag-name "LINK"))))
      (cond
	((and (slot-value *css-refcount-table* url)
	      (eq (slot-value *css-refcount-table* url) 1))
	 (setf (slot-value *css-refcount-table* url) 0)
	 (_unload-css))
	((slot-value *css-refcount-table* url)
	 (setf (slot-value *css-refcount-table* url)
	       (- (slot-value *css-refcount-table* url) 1))))
      url))

  (defvar *loading-table* (jobject))
  
  (defun/cc load-javascript (url loaded-p)
    (when (slot-value *loading-table* url)
      (make-web-thread
       (lambda ()
	 (load-javascript url loaded-p)))
      (suspend))
    
    (let/cc current-continuation
      (let* ((img (make-dom-element "IMG"
		   (jobject :class-name "coretal-loading"
			    :src (+ "http://www.coretal.net/"
				    "style/login/loading.gif"))
		   nil))
	     (script (make-dom-element "SCRIPT"
				       (jobject :type "text/javascript"
						:src url)
				       nil))	     
	     (body (slot-value document 'body))
	     (head (aref (.get-elements-by-tag-name document "HEAD") 0))
	     (time (.get-time (new (*date))))
	     (recurse
	      (lambda (r)		
		(cond
		  ((loaded-p window.k)		   
		   (when (not (null (slot-value script 'parent-node)))
		     ;; (.remove-child head script)
		     (if body (.remove-child body img)))
		   (delete (slot-value *loading-table* url))
		   (current-continuation null))
		  (t
		   (make-web-thread (lambda () (Y r)))
		   (suspend))))))
	(cond
	  (loaded-p
	   (unless (call/cc loaded-p)
	     (_debug (list "load-javascript" url loaded-p))
	     (if body (append body img))
	     (append head script)
	     (setf (slot-value *loading-table* url) t)
	     (Y recurse)))
	  (t
	   (_debug (list "load-javascript" url loaded-p))
	   (append head script))))))
  
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

  (defun remove-slots (source slots)
    (mapcar (lambda (slot)
	      (if (and (slot-value source 'has-attribute)
		       (.has-attribute source slot))
		  (.remove-attribute source slot)
		  (try (delete (slot-value source slot))
		       (:catch (e)
			 (setf (slot-value source slot) undefined)))))
	    slots))

  (defun apply (fun scope args kX)
    (fun.apply scope (reverse (cons kX (reverse args)))))
  
  (defun make-web-thread (fun k)
    (let ((k (or k (lambda (a) a))))
      (k (window.set-timeout (lambda () (fun (lambda (a) a))) 0))))

  ;; FIX IE stack overflow bug
  (defvar *recursion-count* 0)
  (defun make-method (method)
    (if (> (.search navigator.user-agent "MSIE") 0)
	(return
	  (lambda ()
	    (let ((args arguments) (self this))
	      (cond
		((> *recursion-count* 11)
		 (setf *recursion-count* 0)
		 (make-web-thread
		  (lambda ()
		    (apply method self args))))
		(t
		 (setf *recursion-count* (+ *recursion-count* 1))
		 (apply method self args))))))
	method))

  (defun compose-prog1 (fun1 fun2)
    (cond
      ((not (eq "function" (typeof fun2)))
       fun1)
      ((not (eq "function" (typeof fun1)))
       fun2)
      (t
       (return
	 (lambda ()
	   (let* ((args (reverse (reverse arguments)))
		  (self this)
		  (result (apply fun1 self args window.k)))
	     (apply fun2 self args window.k)
	     result))))))

  (defun compose-prog1-cc (fun1 fun2)
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

  (defun coretal-on-load (fun)
    (if (and (not (null (slot-value window 'coretal)))
	     (not (null (slot-value coretal 'loaded-p))))
	(make-web-thread fun)
	(make-web-thread (lambda () (coretal-on-load fun)))))
  
  (defun add-on-load (fun)
    (if (eq "complete" document.ready-state)
	;; (or (eq "complete" document.ready-state)
	;;     (eq "interactive" document.ready-state))
	;; yalan bu ya
	(fun)
	(if (typep window.onload 'function)
	    (let ((current window.onload))
	      (setf window.onload
		    (lambda ()
		      (current)
		      (fun))))
	    (setf window.onload fun))))

  (defvar +default-language+ "en-us")
  (defvar +language-data+ (jobject))
  (defun gettext-lang (lang str args)
    (flet ((combine (str args)
	     (return
	       (.replace str (regex "/%[1-9]/gi")
			 (lambda (a)
			   (return (aref args
					 (- (parse-int (.substr a 1))
					    1))))))))
      (let ((lang (slot-value +language-data+ lang)))
	(if lang
	    (combine (or (slot-value lang str) str) args)
	    (combine str args)))))

  (defun gettext (str args)
    (gettext-lang +default-language+ str args))

  (defun pop-up (url name width height)
    (with-call/cc
      (let* ((width (if width
			width
			(if (> screen.width 0)
			    (*math.floor (/ (- screen.width 10) 2))
			    400)))
	     (height (if height
			 height
			 (if (> screen.height 0)
			     (*math.floor (/ (- screen.height 10) 2))
			     400)))
	     (left (*math.floor (/ (- screen.width width) 2)))
	     (top (*math.floor (/ (- screen.height height) 2)))
	     (params (+ "left=" left ", top=" top ",width=" width ", height=" height ", screenX=" left ", screenY=" top ", toolbar=no, status=yes, location=no, menubar=no, directories=no, scrollbars=yes, resizable=yes")))
	;; (_debug (list "he" height "wi" width "param" params))
	(window.open url "" ;; name
		     params))))
  
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
  
  (defvar *months*
    (array "January" "February" "March"
	   "April" "May" "June"
	   "July" "August" "September"
	   "October" "November" "December"))
  
  (defun date-to-string (date)
    (flet ((pad-me (foo)
	     (if (< foo 10)
		 (+ "0" foo)
		 foo)))
      (+
       (pad-me (date.get-date)) "/"
       (pad-me (+ 1 (date.get-month))) "/"
       (date.get-full-year) " - "
       (pad-me (date.get-hours)) ":"
       (pad-me (date.get-minutes)) ":"
       (pad-me (date.get-seconds)))))

  (defun date-to-string2 (date)
    (.join (take 10 (.split (date-to-string date) "")) ""))

  (defun capitalize (str)
    (+ (.to-upper-case (.char-at str 0))
       (.slice str 1)))
  
  (defun/cc Y (f)
    (f f)
    (suspend))

  (defun/cc Y1 (arg1 f)
    (f arg1 f)
    (suspend))

  (defvar *gc* (jobject))

  (defun/cc gc ()
    (mapobject (lambda (url destroy-list)
		 (funcall-cc (+ url "$") (jobject :objects destroy-list))
		 (delete (slot-value *gc* url)))
	       *gc*))

  (set-interval (lambda () (gc window.k)) 5000)
  
  (defun/cc add-to-gc (fun)
    (destructuring-bind (url id) (call/cc fun)
      (setf (slot-value *gc* url) (cons id (slot-value *gc* url)))))
  
  (setf (slot-value window 'core-server-library-loaded-p) t
	(slot-value window 'component-cache) (jobject)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf (gethash 'make-component +javascript-cps-functions+) t
	(gethash 'make-service +javascript-cps-functions+) t
	(gethash 'apply +javascript-cps-functions+) t
	(gethash 'funcall-cc +javascript-cps-functions+) t
	(gethash 'funcall2-cc +javascript-cps-functions+) t
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

(defun write-core-library-to-file (pathname &optional (indented t))
  (let ((s (make-core-file-output-stream pathname)))
    (checkpoint-stream s)
    (core-server::core-library! (if indented
				    (make-indented-stream s)
				    (make-compressed-stream s)))
    (commit-stream s)
    (close-stream s)))


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

;; (defun get-parameter (name href)
;;     (flet ((eval-item (_value)
;; 	     (cond
;; 	       ((or (eq _value "") (eq _value "null"))
;; 		(return nil))
;; 	       ((and (eq "string" (typeof _value))
;; 		     (.match _value (regex "/^\".*\"$/gi")))
;; 		(try
;; 		 (let ((val (eval _value)))
;; 		   ;; 		(_debug (list name val _value))
;; 		   (cond
;; 		     ((eq val nil)
;; 		      (return nil))
;; 		     ((or (eq (typeof val) "function")
;; 			  (eq (typeof val) "object")
;; 			  (eq (typeof val) "undefined"))
;; 		      (throw (new (*error))))
;; 		     (t (return val))))
;; 		 (:catch (e)
;; 		   (return _value))))
;; 	       (t
;; 		(return _value))))
;; 	   (_get-value (_value)
;; 	     (return
;; 	       (decode-u-r-i-component
;; 		(reduce (lambda (acc atom) (+ acc ":" atom)) (cdr _value) (car _value))))))
;;       (let ((data nil))
;; 	(if (not (null href))
;; 	    (setf data (.substr (.substr href (.search href "#")) 1))
;; 	    (setf data (+ (.substr window.location.hash 1) "$"
;; 			  (.substr window.location.search 1))))
      
;; 	(if (null name)
;; 	    (let ((_value (decode-u-r-i-component (car (.split data "$")))))
;; 	      (return (eval-item _value)))
;; 	    (let ((_value (car
;; 			   (filter (lambda (a) (eq (car a) name))
;; 			    (mapcar
;; 			     (lambda (a)
;; 			       (flatten (mapcar (lambda (b) (.split b "="))
;; 						(.split a ":"))))
;; 			     (.split data "$"))))))
;; 	      (return (eval-item (_get-value (cdr _value)))))))))

;; (defun set-parameter (name new-value)
;;     (if (null name)
;; 	(setf window.location.hash new-value)
;; 	(let* ((new-value (serialize new-value))
;; 	       (append2 (lambda (a b)
;; 			  (if (null a) b (if (null b) "" (+ a b)))))
;; 	       (one (lambda (a) (append2 (car a) (append2 ":" (car (cdr a))))))
;; 	       (found nil)
;; 	       (elements (reverse
;; 			  (reduce
;; 			   (lambda (acc a)
;; 			     (destructuring-bind (key value) a
;; 			       (cond
;; 				 ((eq name key)
;; 				  (setf found t)
;; 				  (if (eq new-value "null")
;; 				      acc
;; 				      (cons (cons name new-value) acc)))
;; 				 (t (cons a acc)))))
;; 			   (mapcar (lambda (a) (.split a ":"))
;; 				   (.split (.substr window.location.hash 1) "$"))))))

;; 	  (if (and (null found) name (not (eq new-value "null")))
;; 	      (setf elements (cons (cons name new-value) elements)))

;; 	  (let ((value (reduce (lambda (acc a)
;; 				 (append2 acc (append2 "$" (one a))))
;; 			       (cdr elements)
;; 			       (if (and (null new-value)
;; 					(null (car (cdr (car elements)))))
;; 				   name
;; 				   (one (car elements))))))
;; 	    (setf window.location.hash (or value ""))))))