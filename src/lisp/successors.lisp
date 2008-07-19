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
;;| Lisp2 AST Successor
;;+-----------------------------------------------------------------------------
;;

(defgeneric form-successor (form &optional successor)
  (:documentation "Returns successors of the 'form'. 'successor' is
  used to divert flow."))

(defmethod form-successor :around ((form t) &optional (successor #'form-successor))
  (declare (ignore successor))
  (nreverse (flatten (call-next-method))))

(defmethod form-successor ((form t) &optional (successor #'form-successor))
  (declare (ignore successor))
  nil)

(defmethod form-successor ((form implicit-progn-mixin) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (body form))

(defmethod form-successor ((form application-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (arguments form))

(defmethod form-successor ((form lambda-application-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (cons (operator form) (call-next-method)))

(defmethod form-successor ((form if-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (cons (consequent form) (cons (then form) (else form))))

(defmethod form-successor ((form variable-binding-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (append (mapcar #'cdr (binds form)) (body form)))

(defmethod form-successor ((form function-binding-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (append (mapcar #'cdr (binds form)) (body form)))

(defmethod form-successor ((form return-from-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (result form))

(defmethod form-successor ((form throw-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (value form))

(defmethod form-successor ((form macrolet-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (append (mapcar #'cdr (binds form)) (body form)))

(defmethod form-successor ((form setq-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (list (var form) (value form)))

(defmethod form-successor ((form symbol-macrolet-form) &optional (successor #'form-successor))
  (declare (ignorable successor))
  (append (mapcar #'cdr (binds form)) (body form)))



