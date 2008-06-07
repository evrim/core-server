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

(in-package :s-serialization)

;;+----------------------------------------------------------------------------
;;| Cl-Prevalence Compat Functions
;;+----------------------------------------------------------------------------
;;
;; This file overrides some properties of cl-prevalence database.

#+sbcl
(defmethod serialize-sexp-internal ((object pathname) stream serialization-state)
  "Serialize a pathname structure to prevalence log file"
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "(:REF . " stream)
	  (prin1 id stream)
	  (write-string ")" stream))
	(let ((serializable-slots (get-serializable-slots serialization-state object)))
	  (setf id (set-known-object serialization-state object))
	  (write-string "(:PATHNAME " stream)
	  (prin1 id stream)
	  (write-string " :DIRECTORY " stream)
	  (serialize-sexp-internal (pathname-directory object) stream serialization-state)
	  (write-string " :NAME " stream)
	  (prin1 (pathname-name object) stream)
	  (write-string " :TYPE " stream)
	  (prin1 (pathname-type object) stream)
	  (write-string " )" stream)))))

(defun deserialize-sexp-internal (sexp deserialized-objects)
  "Overrides the real function and adds de/serialization of pathname objects to
prevalence log file."
  (if (atom sexp) 
      sexp
      (ecase (first sexp)
        (:sequence
	 (destructuring-bind (id &key class size elements) (rest sexp)
	   (let ((sequence (make-sequence class size)))
	     (setf (gethash id deserialized-objects) sequence)
	     (map-into sequence 
		       #'(lambda (x) (deserialize-sexp-internal x deserialized-objects)) 
		       elements))))
        (:hash-table
	 (destructuring-bind (id &key test size rehash-size rehash-threshold entries) (rest sexp)
	   (let ((hash-table (make-hash-table :size size 
					      :test test 
					      :rehash-size rehash-size 
					      :rehash-threshold rehash-threshold)))
	     (setf (gethash id deserialized-objects) hash-table)
	     (dolist (entry entries)
	       (setf (gethash (deserialize-sexp-internal (first entry) deserialized-objects) hash-table)
		     (deserialize-sexp-internal (rest entry) deserialized-objects)))
	     hash-table)))
        (:object
	 (destructuring-bind (id &key class slots) (rest sexp)
	   (let ((object (make-instance class)))
	     (setf (gethash id deserialized-objects) object)
	     (dolist (slot slots)
	       (when (slot-exists-p object (first slot))
		 (setf (slot-value object (first slot)) 
		       (deserialize-sexp-internal (rest slot) deserialized-objects))))
	     object)))
        (:struct
	 (destructuring-bind (id &key class slots) (rest sexp)
	   (let ((object (funcall (intern (concatenate 'string "MAKE-" (symbol-name class)) 
					  (symbol-package class)))))
	     (setf (gethash id deserialized-objects) object)
	     (dolist (slot slots)
	       (when (slot-exists-p object (first slot))
		 (setf (slot-value object (first slot)) 
		       (deserialize-sexp-internal (rest slot) deserialized-objects))))
	     object)))
        (:cons
	 (destructuring-bind (id cons-car cons-cdr) (rest sexp)
	   (let ((conspair (cons nil nil)))
	     (setf (gethash id deserialized-objects)
		   conspair)                   
	     (rplaca conspair (deserialize-sexp-internal cons-car deserialized-objects))
	     (rplacd conspair (deserialize-sexp-internal cons-cdr deserialized-objects)))))
        (:ref (gethash (rest sexp) deserialized-objects))
	#+sbcl
	(:pathname
	 (destructuring-bind (id &key directory name type) (rest sexp) 
	   (let ((pathname (make-pathname :directory (deserialize-sexp-internal directory deserialized-objects)
					  :name (deserialize-sexp-internal name deserialized-objects)
					  :type (deserialize-sexp-internal type deserialized-objects))))
	     (setf (gethash id deserialized-objects) pathname)))))))