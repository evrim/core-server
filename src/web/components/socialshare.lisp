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

(defcomponent socialshare-component (toaster-component)
  ((socialshare-id :host remote :initarg :socialshare-id :initform "socialshare"
		   :documentation "id of the socialshare div element")))

(defmethod/remote make-link ((self socialshare-component) href text icon-url)
  (let ((a (document.create-element "A"))
	(img (document.create-element "IMG")))
    (a.set-attribute "href" href)
    (a.set-attribute "title" text)
    (img.set-attribute "src" icon-url)
    (img.set-attribute "alt" text)
    (setf img.border "0")
    (a.append-child img)
    (return a)))

;; reddit, delicious, stumbleupon, digg, dzone
(defmethod/remote make-type1-link ((self socialshare-component) base text icon url title)
  (return (this.make-link (+ base "?url=" (encode-u-r-i-component url) "&title=" title) text icon)))

;; yahoo, facebook
(defmethod/remote make-type2-link ((self socialshare-component) base text icon url title)
  (return (this.make-link (+ base "?u=" (encode-u-r-i-component url) "&t=" title) text icon)))

;; google
(defmethod/remote make-type3-link ((self socialshare-component) base text icon url title)
  (return (this.make-link (+ base "?op=edit&bkmk=" (encode-u-r-i-component url) "&title=" title) text icon)))

;; http://reddit.com/submit?url=...&title=...
(defmethod/remote make-reddit-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://reddit.com/submit"
				"reddit"
				"http://www.core.gen.tr/images/sociallinks/reddit.gif"
				url title)))

;; http://www.google.com/bookmarks/mark?op=edit&bkmk=<url>&title=<title>
(defmethod/remote make-google-link ((self socialshare-component) url title)
  (return (this.make-type3-link "http://www.google.com/bookmarks/mark"
                                "google"
				"http://www.core.gen.tr/images/sociallinks/google.jpg"
				url title)))

;; http://del.icio.us/post?url=...&title=...
(defmethod/remote make-delicious-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://del.icio.us/post"
				"del.icio.us"
				"http://www.core.gen.tr/images/sociallinks/delicious.gif"
				url title)))

;; http://www.stumbleupon.com/submit?url=...&title=...
(defmethod/remote make-stumbleupon-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://www.stumbleupon.com/submit"
				"stumbleupon"
				"http://www.core.gen.tr/images/sociallinks/stumbleupon.gif"
				url title)))

;; http://digg.com/submit?url=...&title=...
(defmethod/remote make-digg-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://digg.com/submit"
				"digg"
				"http://l.yimg.com/us.yimg.com/i/us/pps/digg.png" url title)))

;; http://www.dzone.com/links/add.html?url=...&title=...
(defmethod/remote make-dzone-link ((self socialshare-component) url title)
  (return (this.make-type1-link "http://www.dzone.com/links/add.html"
				"dzone"
				"http://l.yimg.com/us.yimg.com/i/us/pps/dzone.png"
				url title)))

;; http://www.facebook.com/sharer.php?u=...
(defmethod/remote make-facebook-link ((self socialshare-component) url title)
  (return (this.make-type2-link "http://www.facebook.com/sharer.php"
				"facebook"
				"http://www.core.gen.tr/images/sociallinks/facebook.gif"
				url title)))

;; http://myweb2.search.yahoo.com/myresults/bookmarklet?&u=...&t=....
(defmethod/remote make-yahoo-link ((self socialshare-component) url title)
  (return (this.make-type2-link "http://myweb2.search.yahoo.com/myresults/bookmarklet"
				"yahoo"
				"http://www.core.gen.tr/images/sociallinks/yahoo.jpg"
				url title)))

(defmethod/remote make-socialshare-box ((self socialshare-component))
  (let ((div (document.create-element "DIV")))
    (div.append-child (this.make-google-link window.location document.title))
    (div.append-child (this.make-facebook-link window.location document.title))
    (div.append-child (this.make-delicious-link window.location document.title))
    (div.append-child (this.make-reddit-link window.location document.title))
    (div.append-child (this.make-stumbleupon-link window.location document.title))
    (div.append-child (this.make-digg-link window.location document.title))
    (div.append-child (this.make-dzone-link window.location document.title))

    (div.append-child (this.make-yahoo-link window.location document.title))
    (return div)))

(defmethod/remote initialize ((self socialshare-component) obj)
  (aif ($ this.socialshare-id)
       (it.append-child (obj.make-socialshare-box))
       (obj.toast (+ "div id \"" obj.socialshare-id "\" not found."))))