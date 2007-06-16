(in-package :tr.gen.core.server)

;; 1.2 Access Authentication Framework
;; auth-scheme    = token
;; auth-param     = token "=" ( token | quoted-string )
;; challenge      = auth-scheme 1*SP 1#auth-param
;; realm          = "realm" "=" realm-value
;; realm-value    = quoted-string
;; credentials = auth-scheme #auth-param

;; http-auth-scheme? :: stream -> string
(defrule http-auth-scheme? (c (acc (make-accumulator)))
  (:type alpha? c)
  (:collect c acc)
  (:zom (:type alpha? c) (:collect c acc))
  (:return acc))

;; http-auth-scheme! :: stream -> string -> string
(defun http-auth-scheme! (stream scheme)
  (string! stream scheme))

;; http-auth-param? :: stream -> (cons string string)
(defrule http-auth-param? (attr val)
  (:http-auth-scheme? attr)
  #\=
  (:or (:quoted? val)
       (:http-auth-scheme? val))
  (:return (cons attr val)))

(defun http-auth-param! (acons)
  (string! stream (car cons))
  (char! stream #\=)
  (quoted! stream (cdr cons)))

;; http-challenge? :: stream -> (cons string ((attrstr . valstr) ...))
(defrule http-challenge? (scheme params param)
  (:http-auth-scheme? scheme)
  (:lwsp?)
  (:http-auth-param? param)
  (:do (push param params))
  (:zom #\, (:lwsp?)
	(:http-auth-param? param)
	(:do (push param params)))
  (:return (cons scheme params)))

(defun http-challenge! (stream challenge)
  (string! stream (car challenge))
  (char! stream #\ )
  (let ((params (cdr challenge)))
    (when params
      (string! stream (caar params))
      (char! stream #\=)
      (quoted! stream (cdar params))
      (if (cdr params)
	  (reduce #'(lambda (acc item)
		      (char! stream #\,)
		      (char! stream #\Newline)
		      (string! stream (caar params))
		      (char! stream #\=)
		      (quoted! stream (cdar params)))
		  (cdr params) :initial-value nil)))))

(defrule http-realm-value? (c)
  (:quoted? c)
  (:return c))

(defrule http-realm? (realm)
  (:seq "realm=")
  (:http-realm-value? realm)
  (:return realm))

