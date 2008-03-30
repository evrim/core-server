(in-package :core-server)

(defcomponent socialshare-component (toaster-component)
  ((socialshare-id :host remote :initarg :socialshare-id :initform "socialshare"
		   :documentation "id of the socialshare div element")))

(defmethod/remote make-link ((self socialshare-component) href text)
  (let ((a (document.create-element "A")))
    (a.set-attribute "href" href)
    (setf a.inner-h-t-m-l text)
    (return a)))

;; reddit, delicious, stumbleupon, digg
(defmethod/remote make-type1-link ((self socialshare-component) base text url title)
  (return (this.make-link (+ base "?url=" url "&title=" title) text)))

;; yahoo, facebook
(defmethod/remote make-type2-link ((self socialshare-component) base text url title)
  (return (this.make-link (+ base "?u=" url "&t=" title) text)))

;; http://reddit.com/submit?url=...&title=...
(defmethod/remote make-reddit-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://reddit.com/submit" "reddit" url title)))

;; http://del.icio.us/post?url=...&title=...
(defmethod/remote make-delicious-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://del.icio.us/post" "del.icio.us" url title)))

;; http://www.stumbleupon.com/submit?url=...&title=...
(defmethod/remote make-stumbleupon-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://www.stumbleupon.com/submit" "stumbleupon" url title)))

;; http://digg.com/submit?url=...&title=...
(defmethod/remote make-digg-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://digg.com/submit" "digg" url title)))

;; http://www.facebook.com/sharer.php?u=...
(defmethod/remote make-facebook-link ((self socialshare-component) url title)
  (return (this.make-type2-link "http://www.facebook.com/sharer.php" "facebook" url title)))

;; http://myweb2.search.yahoo.com/myresults/bookmarklet?&u=...&t=....
(defmethod/remote make-yahoo-link ((self socialshare-component) url title)
  (return (this.make-type2-link "http://myweb2.search.yahoo.com/myresults/bookmarklet" "yahoo" url title)))

(defmethod/remote make-socialshare-box ((self socialshare-component))
  (let ((div (document.create-element "DIV")))
    (div.append-child (this.make-reddit-link window.location document.title))
    (div.append-child (this.make-delicious-link window.location document.title))
    (div.append-child (this.make-stumbleupon-link window.location document.title))
    (div.append-child (this.make-digg-link window.location document.title))
    (div.append-child (this.make-facebook-link window.location document.title))
    (div.append-child (this.make-yahoo-link window.location document.title))
    (return div)))

(defmethod/remote initialize ((self socialshare-component) obj)
  (aif ($ this.socialshare-id)
       (it.append-child (obj.make-socialshare-box))
       (obj.toast (+ "div id \"" obj.socialshare-id "\" not found."))))