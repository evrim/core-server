(in-package :tr.gen.core.server.test)

;; http://www.oasis-open.org/committees/xml-conformance/suite-v1se/xmlconf-20010315.htm
;; test types are :valid :invalid :not-wf

(defun parse-xml (error "Not implemented yet!"))

(defun file-contents (pathname)
  (with-open-file (stream pathname)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t))) 
      (setf (fill-pointer seq)
	    (read-sequence seq stream)) 
      seq)))
