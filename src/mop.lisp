(in-package :core-server)

;; Standard successors
(defun class-successors (class &optional (base 'component))
  (if (eq class (find-class base))
      nil
      (sb-mop:class-direct-superclasses class)))

;; INSTALL> (class-superclasses 'c)
;; (#<STANDARD-CLASS C> #<STANDARD-CLASS B> #<STANDARD-CLASS A>
;;  #<STANDARD-CLASS COMMAND>)
(defun class-superclasses (class &aux lst)
  (let ((class (if (stringp class) (find-class class) class)))
    (core-search (cons class (copy-list (sb-mop:class-direct-superclasses class)))
		 #'(lambda (atom) (pushnew atom lst) nil) 
		 #'class-successors
		 #'append)
    (nreverse lst)))

;; INSTALL> (class-default-initargs 'c)
;; ((:ARG-B 'ARG-B-OVERRIDEN-BY-C #<FUNCTION {BC06125}>)
;;  (:ARG-A 'ARG-A-OVERRIDEN-BY-C #<FUNCTION {BC06195}>))
(defun class-default-initargs (class &aux lst)
  (core-search (cons (find-class class)
		     (copy-list
		      (sb-mop:class-direct-superclasses (find-class class))))
	       #'(lambda (atom)
		   (let ((args (copy-list
				(sb-mop:class-direct-default-initargs atom))))
		     (when args (setf lst (append args lst))))
		   nil)
	       #'class-successors
	       #'append)
  lst)