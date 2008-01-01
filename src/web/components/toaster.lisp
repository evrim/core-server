(in-package :core-server)

(defcomponent toaster-component ()
  ((toaster :host remote :initform nil)))

(defmethod/remote toast ((self toaster-component) message)
  (if (= null this.toaster)
      (progn
	(dojo.require "dojox.widget.Toaster")
	(let ((div (document.create-element "div")))
	  (document.body.append-child div)
	  (setf this.toaster (new (dojox.widget.*toaster
				   (create :position-direction "br-left"
					   :message-topic "toasterTopic"
					   :duration 1000)
				   div))))))

  (dojo.publish "toasterTopic" (array message)))
