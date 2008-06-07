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

;The default method-combination technique
(define-method-combination sysv-standard (&key (type :start))
  ((around (:around)) (primary () :required t))  
  (labels ((specializer (method)
	     (car (sb-mop:method-specializers method)))
	   (specializers (methods)
	     (mapcar #'(lambda (m) (cons (specializer m) m)) methods))
	   (sort-by-specializers (methods)
	     (mapcar #'cdr (sort (specializers methods) #'equal :key #'car)))
	   (call-methods (methods)
	     (mapcar #'(lambda (method) `(call-method ,method))  methods)))
    (let ((around (nreverse around)))
      (cond ((eq type :start)
	     (setf primary (cons 'progn
				 (call-methods (nreverse (sort-by-specializers primary))))))
	    ((eq type :stop)
	     (setf primary (cons 'progn 
				 (call-methods (sort-by-specializers primary)))))
	    ((eq type :status)
	     (setf primary (cons 'and 
				 (call-methods (nreverse (sort-by-specializers primary)))))))
      (if around
	  `(call-method ,(first around)
			(,@(rest around)
			   (make-method ,primary)))
	  primary))))

(defparameter *sysv* '(start stop status))
(defun trace-sysv (&optional (sysv *sysv*))
  (mapcar (lambda (s) (eval `(trace ,s))) sysv))
(defun untrace-sysv (&optional (sysv *sysv*))
  (mapcar (lambda (s) (eval `(untrace ,s))) sysv))

;;     (let ((form (if (or before after (rest primary))
;; 		    `(multiple-value-prog1
;; 			 (progn ,@(call-methods before)
;; 				(call-method ,(first primary)
;; 					     ,(rest primary)))
;; 		       ,@(call-methods (reverse after)))
;; 		    `(call-method ,(first primary)))))
;;       (if around
;; 	  `(call-method ,(first around)
;; 			(,@(rest around)
;; 			   (make-method ,form)))
;; 	  form))

;;    (format t "~A~%" primary)
;;      (format t "~A~%" (call-primaries primary))
;;      (format t "~A~%" primary-methods)
;;      (format t "~A~%" start-or-stop)


;; (define-method-combination wrapping-standard
;;     (&key (around-order :most-specific-first)
;;           (before-order :most-specific-first)
;;           (primary-order :most-specific-first)
;;           (after-order :most-specific-last)
;;           (wrapping-order :most-specific-last)
;;           (wrap-around-order :most-specific-last))
;;   ((wrap-around (:wrap-around))
;;    (around (:around))
;;    (before (:before))
;;    (wrapping (:wrapping))
;;    (primary () :required t)
;;    (after (:after)))
;;   "Same semantics as standard method combination but allows
;; \"wrapping\" methods. Ordering of methods:

;;  (wrap-around
;;    (around
;;      (before)
;;      (wrapping
;;        (primary))
;;      (after)))

;; :warp-around, :around, :wrapping and :primary methods call the
;; next least/most specific method via call-next-method (as in
;; standard method combination).

;; The various WHATEVER-order keyword arguments set the order in
;; which the methods are called and be set to either
;; :most-specific-last or :most-specific-first."
;;   (labels ((effective-order (methods order)
;;              (ecase order
;;                (:most-specific-first methods)
;;                (:most-specific-last (reverse methods))))
;;            (call-methods (methods)
;;              (mapcar (lambda (meth) `(call-method ,meth))
;;                      methods)))
;;     (let* (;; reorder the methods based on the -order arguments
;;            (wrap-around (effective-order wrap-around wrap-around-order))
;;            (around (effective-order around around-order))
;;            (wrapping (effective-order wrapping wrapping-order))
;;            (before (effective-order before before-order))
;;            (primary (effective-order primary primary-order))
;;            (after (effective-order after after-order))
;;            ;; inital value of the effective call is a call its primary
;;            ;; method(s)
;;            (form (case (length primary)
;;                    (1 `(call-method ,(first primary)))
;;                    (t `(call-method ,(first primary) ,(rest primary))))))
;;       (when wrapping
;;         ;; wrap form in call to the wrapping methods
;;         (setf form `(call-method ,(first wrapping)
;;                                  (,@(rest wrapping) (make-method ,form)))))
;;       (when before
;;         ;; wrap FORM in calls to its before methods
;;         (setf form `(progn
;;                       ,@(call-methods before)
;;                       ,form)))
;;       (when after
;;         ;; wrap FORM in calls to its after methods
;;         (setf form `(multiple-value-prog1
;;                         ,form
;;                       ,@(call-methods after))))
;;       (when around
;;         ;; wrap FORM in calls to its around methods
;;         (setf form `(call-method ,(first around)
;;                                  (,@(rest around)
;;                                     (make-method ,form)))))
;;       (when wrap-around
;;         (setf form `(call-method ,(first wrap-around)
;;                                  (,@(rest wrap-around)
;;                                     (make-method ,form)))))
;;       form)))
