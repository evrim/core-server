(in-package :tr.gen.core.server)

(defclass peer ()
  ((server :accessor peer.server :initarg :server :initform nil)))

(defclass stream-peer (peer local-unit)
  ()
  (:default-initargs :name "Stream Peer Handling Unit"))

;; (defmethod/unit handle-stream :async-no-return ((self stream-peer) stream address)
;;   (format t "1.Peer address:~A~%" address)
;;   (let ((acc (make-accumulator :byte)))
;;     (iterate (for str = (read-stream stream))
;; 	     (when str (push-atom str acc))
;; 	     (until (null str))
;; 	     (finally  
;; 	      (close-stream stream)
;; 	      (format t "GEE:~A~%" (octets-to-string acc :utf-8)))))
;;   (sb-int::flush-standard-output-streams))