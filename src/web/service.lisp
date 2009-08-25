(in-package :core-server)

(defmacro defservice (name supers slots &rest rest)
  `(progn
     (defcomponent ,name ,supers ,slots ,@rest)
     (defmacro/js ,name (&rest properties)
       `(make-service ,',(symbol-to-js name) (jobject ,@properties)))))