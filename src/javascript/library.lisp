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
  (defun reduce (fun lst initial-value)
    (if (null lst)
	(return nil))
    
    (let ((result (or (and (not (typep initial-value 'undefined)) initial-value) nil)))
      (if (not (null lst.length))
	  (dolist (item lst)
	    (setf result (fun result item)))
	  (doeach (item lst)
	    (setf result (fun result (aref lst item)))))
      result))

  (defun reduce0 (fun lst)
    (reduce fun lst nil))

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

  (defun cons (atom lst)
    (if (null lst)
	(array atom)
	(.concat (array atom) lst)))

  (defun car (lst)
    (cond
      ((null lst) nil)
      ((instanceof lst *array) (aref lst 0))
      ((typep lst 'object)
       (let ((result))
	 (doeach (i lst)
	   (setf result (aref lst i)))
	 result))
      (t nil)))

  (defun cdr (lst)
    (cond
      ((null lst) nil)      
      ((or (typep lst '*array)
	   (typep lst 'object)) (.slice lst 1))
      (t (list lst))))
  
  (defun mapcar (fun lst)
    (reverse
     (reduce (lambda (acc atom)
	       (cons (fun atom) acc))
	     lst)))
  
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
  
  (defun show (node)
    (when (instanceof node *h-t-m-l-element)
      (setf node.style.display "block"))
    node)

  (defun hide (node)
    (when (instanceof node *h-t-m-l-element)
      (setf node.style.display "none"))
    node)

  (defun inline (node)
    (when (instanceof node *h-t-m-l-element)
      (setf node.style.display "inline"))
    node)
  
  (defun add-on-load (fun)
    (if (typep window.onload 'function)
	(let ((current window.onload))
	  (setf window.onload
		(lambda ()
		  (current)
		  (fun))))
	(setf window.onload fun)))

  (defun connect (target event lambda)
    (if (typep target 'string)
	(setf (slot-value ($ target) event) lambda)
	(setf (slot-value target event) lambda)))
  
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
;;     (xhr.set-request-header "Content-Type" "application/x-www-form-urlencoded")
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
  
  (defun/cc funcall-cc (action args)    
    (let/cc current-continuation
      (let ((hash (+ "__result" (.get-time (new (*date))))))
	(setf (slot-value args "__hash") hash)
	(let ((script (make-dom-element "script"
					(create :src
						(+ action (serialize-to-uri args)))
					nil)))
	  (setf (slot-value script 'onload)
		(event ()
		  (document.body.remove-child script)
		  (current-continuation (slot-value window hash))))
	  (document.body.append-child script)
	  (suspend)))))

  (defun make-dom-element (tag properties children)
    (let ((element (document.create-element tag)))
      (doeach (i properties)
	      (setf (aref element i) (aref properties i)))
      (let ((children (flatten children)))
	(mapcar (lambda (i)
		  (cond
		    ((not (null (slot-value i 'tag-name)))
		     (element.append-child i))
		    (t
		     (element.append-child (document.create-text-node i)))))
		children))
      element))
  
  (defun get-parameter (name)
;;     (debug "get-param:" name)
    (let ((params (+ (.substr window.location.hash 1)
		     "$"
		     (.substr window.location.search 1)))
	  (arr (params.split "$")))
      (try
       (dolist (a arr)
	 (let ((key (.substr a 0 (.search a ":")))
	       (value (.substr a (+ 1 (.search a ":")))))
	   ;; 	  (debug (+ "key:" key " val:" value))
	   (if (= (key.to-lower-case) (name.to-lower-case))
	       (return (eval (+ "(" (unescape value) ")"))))))
       (:catch (e)))
      (return nil)))

  (defun set-parameter (name new-value)
    (let ((params (.substr window.location.hash 1))
	  (arr (params.split "$"))
	  (hash "")
	  (found nil))
      (dolist (a arr)
	(let ((key (aref (a.split ":") 0))
	      (value (aref (a.split ":") 1)))
;;	  (debug key value)
	  (cond
	    ((or (null key) (null value))
	     hash)
	    ((= (key.to-lower-case) (name.to-lower-case))
	     (setf hash (+ hash (+ key ":"
				   (encode-u-r-i-component (serialize new-value)) "$"))
		   found t))
	    (t
	     (setf hash (+ hash (+ key ":" value "$")))))))
      (if (not found)
	  (setf hash (+ hash (+ name ":" (encode-u-r-i-component (serialize new-value))))))
      (setf window.location.hash hash)
      (return new-value)))

;; +----------------------------------------------------------------------------
;; | Identity Continuation
;; +----------------------------------------------------------------------------
  (defun k (value)
    value)

;; +----------------------------------------------------------------------------
;; | 'new' Operator replacement for Continuations
;; +----------------------------------------------------------------------------
  (defun make-instance (k ctor arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
    (if (> arguments.length 9)
	(throw (new (*error (+ "Cannot makeInstance, too many arguments:"
			       arguments.length ", ctor:" ctor))))
	(cond
	  ((not (typep arg8 'undefined))
	   (k (new (ctor arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8))))
	  ((not (typep arg7 'undefined))
	   (k (new (ctor arg1 arg2 arg3 arg4 arg5 arg6 arg7))))
	  ((not (typep arg6 'undefined))
	   (k (new (ctor arg1 arg2 arg3 arg4 arg5 arg6))))
	  ((not (typep arg5 'undefined))
	   (k (new (ctor arg1 arg2 arg3 arg4 arg5))))
	  ((not (typep arg4 'undefined))
	   (k (new (ctor arg1 arg2 arg3 arg4))))
	  ((not (typep arg3 'undefined))
	   (k (new (ctor arg1 arg2 arg3))))
	  ((not (typep arg2 'undefined))
	   (k (new (ctor arg1 arg2))))
	  ((not (typep arg1 'undefined))
	   (k (new (ctor arg1))))
	  (t
	   (k (new (ctor)))))))

  (defun extend (source target)
    (mapobject (lambda (k v)
		 (setf (slot-value target k) v))
	       source)
    target)  

  (defvar *registry* (create))
  
  (defun/cc make-service (name properties)    
    (let ((service (slot-value *registry* name)))
      (if (not (null service))
	  service
	  (let ((ctor (funcall-cc "service.core?" (create :service name))))
	    (let ((instance (new (ctor properties))))
	      (setf (slot-value *registry* name) instance)
	      instance)))))
  
  (defun/cc make-component (name properties)
    (let ((component (slot-value *registry* name)))
      (if (not (null component))
	  (new (component properties))
	  (progn
	    (setf (slot-value *registry* name)
		  (funcall-cc "component.core?" (create :component name)))
	    (make-component name))))))
