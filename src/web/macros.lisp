(in-package :core-server)

;; JS Macros
(defjsmacro $ (id)
  `(document.get-element-by-id ,id))

(defjsmacro doc-body ()
  `(aref (document.get-elements-by-tag-name "body") 0))

(defjsmacro dj_debug (&rest rest)
  `(dojo.debug ,@rest))

;; Dojo 0.4
(defjsmacro debug (&rest rest)
  `(dojo.debug ,@rest))

;; Dojo 1.0
(defjsmacro debug (&rest rest)
  `(console.debug ,@rest))

(defjsmacro $$ (id)
  `(dojo.widget.by-id ,id))

(defjsmacro $fck (id)
  `(*f-c-keditor-a-p-i.*get-instance ,id))

(defjsmacro s-v (a b)
  `(slot-value ,a ,b))

(defjsmacro $idoc (iframe-id)
  `(s-v ($ ,iframe-id) 'content-document))

(defjsmacro mapcar (lambda lst)
  `(dojo.map ,lst ,lambda))

;; (defjsmacro null (arg)
;;   `(= 'null ,arg))

;; (defjsmacro undefined (arg)
;;   `(= "undefined" (typeof ,arg)))

(defjsmacro input-value (id)
  `(slot-value ($ ,id) 'value))

;; (defjsmacro wrap-on-load (&body body)
;;   (if (context.ajax-request *context*)
;;       `(progn
;; 	 ,@body)
;;       `(dojo.add-on-load
;; 	(lambda ()
;; 	  ,@body))))

;; (defmacro with-yaclml-output-to-string (&body body)
;;   `(let ((*yaclml-stream* (make-core-stream ""))
;; 	 (*yaclml-indent* nil))
;;      ,@body
;;      (return-stream *yaclml-stream*)))
