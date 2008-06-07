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

(in-package :tr.gen.core.server)

;; FIXME: implement rest of rfc822 ;)

;; FIXME: optimize me

;; 3.3 LEXICAL TOKENS
;;      specials    =  "(" / ")" / "<" / ">" / "@"  ; Must be in quoted-
;;                  /  "," / ";" / ":" / "\" / <">  ;  string, to use
;;                  /  "." / "[" / "]"              ;  within a word.

(defatom special? ()
  (if (member c '#.(mapcar #'char-code '(#\( #\) #\< #\> #\@
					 #\, #\; #\: #\\ #\"
					 #\. #\[ #\])))
      t))

(defatom atomic? ()
  (and (not (special? c))
       (not (control? c))
       (not (white-space? c))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule quoted-pair? ((pair (make-accumulator)) c)
    (:and #\\
	  (:type visible-char? c)
	  (:collect c pair))
    (:return pair)))

;; 3.3 LEXICAL TOKENS

;; ctext       =  <any CHAR excluding "(",     ; => may be folded
;;                 ")", "\" & CR, & including
;;                 linear-white-space>
  
(defatom ctext? () 
  (or (white-space? c)
      (and (char? c)
	   (not (eq c #.(char-code #\()))
	   (not (eq c #.(char-code #\))))
	   (not (eq c #.(char-code #\\)))
	   (not (carriage-return? c)))))

;; qtext       =  <any CHAR excepting <">,     ; => may be folded
;;                 "\" & CR, and including
;;                 linear-white-space>
  
(defatom qtext? () 
  (or (white-space? c)
      (and (char? c)
	   (not (eq c #.(char-code #\")))
	   (not (eq c #.(char-code #\<)))
	   (not (eq c #.(char-code #\>)))
	   (not (eq c #.(char-code #\\)))
	   (not (carriage-return? c)))))

;; dtext       =  <any CHAR excluding "[",     ; => may be folded
;;                 "]", "\" & CR, & including
;;                 linear-white-space>
  
(defatom dtext? ()
  (or (white-space? c)
      (and (char? c)
	   (not (eq c #.(char-code #\[)))
	   (not (eq c #.(char-code #\])))
	   (not (eq c #.(char-code #\\)))
	   (not (carriage-return? c)))))

;; quoted-string = < "> *(qtext/quoted-pair) <">; Regular qtext or
;;                                              ;   quoted chars.
(defrule quoted-string? ((value (make-accumulator)) c)
  #\"
  (:or (:type qtext? c)
       (:quoted-pair? c))
  (:collect c value)
  (:zom (:or (:type qtext? c)
	     (:quoted-pair? c))
	(:collect c value))
  #\"
  (:return value))

;; domain-literal =  "[" *(dtext / quoted-pair) "]"
(defrule domain-literal? (c (dl (make-accumulator)))
  #\[
  (:or (:type dtext? c)
       (:quoted-pair? c))
  (:collect c dl)
  (:zom (:or (:type dtext? c)
	     (:quoted-pair? c))
	(:collect c dl))
  #\]
  (:return dl))

;; comment     =  "(" *(ctext / quoted-pair / comment) ")"
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule comment? ((text (make-accumulator)) c)
    #\(
    (:zom (:or (:type ctext? c)
	       (:quoted-pair? c)
	       ;; TODO: fix recursion
	       ;; (:comment? c)
	       )
	  (:collect c text))
    #\)
    (:return text)))

(defun comment! (stream text)
  (char! stream #\()
  (string! stream text)
  (char! stream #\)))

;; delimiters  =  specials / linear-white-space / comment
(defrule delimiter? (c)
  (:or (:type special? c)
       (:lwsp? c)
       (:comment? c))
  (:return c))

;; atom        =  1*<any CHAR except specials, SPACE and CTLs>
(defrule atom? ((atom (make-accumulator)) c)
  (:type atomic? c)
  (:collect c atom)
  (:zom (:type atomic? c)
	(:collect c atom))
  (:return atom))

(defrule word? (word)
  (:or
   (:atom? word)
   (:quoted-string? word))
  (:return word))

(defrule phrase? (words c)
  (:word? c)
  (:do (push c words))
  (:zom #\ 
	(:word? c)
	(:do (push c words)))
  (:return (nreverse words)))

;; local-part  =  word * ("." word) ; uninterpreted
;;                                  ; case-preserved
(defrule local-part? (c lp)
  (:word? c)
  (:do (push c lp))
  (:zom #\.
	(:word? c)
	(:do (push "." lp) (push c lp)))
  (:return (apply #'concatenate 'string (nreverse lp))))

;; domain-ref  =  atom
(defrule domain-ref? (c)
  (:atom? c)
  (:return c))

;; sub-domain  =  domain-ref / domain-literal
(defrule sub-domain? (sd)
  (:or (:domain-ref? sd)
       (:domain-literal? sd))
  (:return sd))

;; domain      =  sub-domain *("." sub-domain)
(defrule domain? (c d)
  (:sub-domain? c)
  (:do (push c d))
  (:zom #\.
	(:sub-domain? c)
	(:do (push c d)))
  (:return (nreverse d)))

;; addr-spec? :: stream -> (local-part . domain)
(defrule addr-spec? (local-part domain)
  (:local-part? local-part)
  #\@
  (:domain? domain)
  (:return (cons local-part domain)))


;; ex: @relay1,@relay2,@relay3:user@domain
;; See: RFC1123/5.2.18
;; route       =  1#("@" domain) ":"           ; path-relative
(defrule route? (c r)
  #\@
  (:domain? c)
  (:do (push c r))
  (:zom #\, (:lwsp?)
	(:domain? c)
	(:do (push c r)))
  #\:
  (:return r))

;; route-addr? :: stream -> (cons addr-spec route)
;; route-addr  =  "<" [route] addr-spec ">"
(defrule route-addr? (r as)
  #\<
  (:or (:route? r)
       (:addr-spec? as))
  #\>
  (:return (cons as r)))

;; mailbox     =  addr-spec                    ; simple address
;;             /  phrase route-addr            ; name & addr-spec
;; mailbox? :: stream -> (cons addr-spec|route-addr phrase) 
(defrule mailbox? (as p)
  (:or (:and (:addr-spec? as)
	     (:return (cons as p)))
       (:or (:phrase? p)
	    (:route-addr? as)))
  (:return (cons as p)))