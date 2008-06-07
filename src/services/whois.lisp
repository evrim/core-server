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

(defparameter *default-whois-port* 43)

(defparameter *whois-servers*
  '(("com" . "whois.internic.net")
    ("net" . "whois.internic.net")
    ("org" . "whois.internic.net")
    ("edu" . "whois.internic.net")
    ("uk.com" . "whois.uk.com")
    ("eu.org" . "whois.eu.org")
    ("ac" . "whois.nic.ac")
    ("al" . "whois.ripe.net")
    ("am" . "whois.amnic.net")
    ("am" . "whois.amnic.net")
    ("as" . "whois.nic.as")
    ("at" . "whois.nic.at")
    ("au" . "whois.aunic.net")
    ("az" . "whois.ripe.net")
    ("ba" . "whois.ripe.net")
    ("be" . "whois.dns.be")
    ("bg" . "whois.ripe.net")
;;    ("bm" . "rwhois.ibl.bm:4321")
    ("biz" . "whois.biz")
    ("br" . "whois.nic.br")
    ("by" . "whois.ripe.net")
    ("ca" . "whois.cira.ca")
    ("cc" . "whois.nic.cc")
    ("ch" . "whois.nic.ch")
    ("cl" . "whois.nic.cl")
    ("edu.cn" . "whois.edu.cn")
    ("cn" . "whois.cnnic.cn")
    ("cx" . "whois.nic.cx")
    ("cy" . "whois.ripe.net")
    ("cz" . "whois.ripe.net")
    ("de" . "whois.ripe.net")
    ("dk" . "whois.ripe.net")
    ("dz" . "whois.ripe.net")
    ("ee" . "whois.ripe.net")
    ("eg" . "whois.ripe.net")
    ("es" . "whois.ripe.net")
    ("fi" . "whois.ripe.net")
    ("fo" . "whois.ripe.net")
    ("fr" . "whois.nic.fr")
    ("gov" . "whois.nic.gov")
    ("gr" . "whois.ripe.net")
    ("gs" . "whois.adamsnames.tc")
    ("hk" . "whois.hknic.net.hk")
    ("hm" . "webhost1.capital.hm")
    ("hr" . "whois.ripe.net")
    ("hu" . "whois.ripe.net")
    ("ie" . "whois.domainregistry.ie")
    ("il" . "whois.ripe.net")
    ("in" . "whois.ncst.ernet.in")
    ("info" . "whois.afilias.net")
    ("int" . "whois.isi.edu")
    ("is" . "whois.isnet.is")
    ("it" . "whois.nic.it")
    ("jp" . "whois.nic.ad.jp")
    ("kr" . "whois.krnic.net")
    ("kz" . "whois.domain.kz")
    ("li" . "whois.nic.li")
    ("lk" . "whois.nic.lk")
    ("lt" . "whois.ripe.net")
    ("lu" . "whois.ripe.net")
    ("lv" . "whois.ripe.net")
    ("ma" . "whois.ripe.net")
    ("md" . "whois.ripe.net")
    ("mil" . "whois.nic.mil")
    ("mk" . "whois.ripe.net")
    ("mm" . "whois.nic.mm")
    ("mobi" . "whois.dotmobiregistry.net")
    ("ms" . "whois.adamsnames.tc")
    ("mt" . "whois.ripe.net")
    ("mx" . "whois.nic.mx")
    ("my" . "whois.mynic.net")
    ("nl" . "whois.domain-registry.nl")
    ("no" . "whois.norid.no")
    ("nu" . "whois.nic.nu")
    ("pe" . "whois.rcp.net.pe")
    ("pk" . "whois.pknic.net.pk")
    ("pl" . "whois.ripe.net")
    ("pt" . "whois.dns.pt")
    ("ro" . "whois.ripe.net")
    ("ru" . "whois.ripn.net")
    ("se" . "whois.nic-se.se")
    ("sg" . "whois.nic.net.sg")
    ("sh" . "whois.nic.sh")
    ("si" . "whois.ripe.net")
    ("sk" . "whois.ripe.net")
    ("sm" . "whois.ripe.net")
    ("st" . "whois.nic.st")
    ("su" . "whois.ripe.net")
    ("tc" . "whois.adamsnames.tc")
    ("tf" . "whois.adamsnames.tc")
    ("th" . "whois.thnic.net")
    ("tj" . "whois.nic.tj")
    ("tm" . "whois.nic.tm")
    ("tn" . "whois.ripe.net")
    ("to" . "whois.tonic.to")
    ("tr" . "whois.metu.edu.tr")
    ("tw" . "whois.twnic.net")
    ("tv" . "tvwhois.verisign-grs.com")
    ("ua" . "whois.ripe.net")
    ("ac.uk" . "whois.ja.net")
    ("gov.uk" . "whois.ja.net")
    ("uk" . "whois.nic.uk")
    ("us" . "whois.isi.edu")
    ("va" . "whois.ripe.net")
    ("vg" . "whois.adamsnames.tc")
    ("yu" . "whois.ripe.net")
    ("gb.com" . "whois.nomination.net")
    ("gb.net" . "whois.nomination.net")
    ("za" . "whois.co.za")))

;; com, net, org, edu -> type1
;; info -> type2
;; tv -> type1
;; mobi -> type2
;; biz -> type3

(defun render-type1 (fqdn)
  (format nil "=~a~c~c" fqdn #\return #\linefeed))

(defun render-type2 (fqdn)
  (format nil "~a~c~c" fqdn #\return #\linefeed))

(defun render-type3 (fqdn)
  (format nil "~a~c~c" fqdn #\return #\linefeed))

(defun parser-type1 (text)
  (search "No match for" text))

(defun parser-type2 (text)
  (search "NOT FOUND" text))

(defun parser-type3 (text)
  (search "Not found:" text))

(defparameter +whois-map+
  (list (cons '("com" "net" "org" "edu" "tv")
	      '(render-type1 . parser-type1))
	(cons '("info" "mobi")
	      '(render-type2 . parser-type2))
	(cons '("biz")
	      '(render-type3 . parser-type3))))

(defun whois-map-lookup (dpart)
  (reduce (lambda (i a) (if (null i) a i))
    (mapcar (lambda (l)
	      (if (member dpart (car l) :test #'string=) (cdr l)))
	    +whois-map+)))

(defun root-domain-part (fqdn)
  (awhen (position #\. fqdn :from-end t)
    (subseq fqdn (1+ it))))

(defun whois-server (fqdn &optional (server-list *whois-servers*))
  (flet ((resolve (addr)
	  (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name addr))))
    (awhen (root-domain-part fqdn)
      (let ((s (assoc it server-list :test #'equal)))
	(aif (and s (cdr s) (position #\: (cdr s)))
	     (values (resolve (subseq (cdr s) 0 it))
		     (or (parse-integer (subseq (cdr s) (1+ it)) :junk-allowed t)
			 *default-whois-port*)
		     (subseq (cdr s) 0 it))
	     (values (resolve (cdr s))
		     *default-whois-port*
		     (cdr s)))))))

;; look for this top level domains: com info net org tv mobi biz
;; domain-availablep :: string -> bool
(defun domain-availablep (fqdn)
  (let ((res (whois fqdn)))
    (if (funcall (car res) (cdr res)) t nil)))

(defun whois (fqdn)
  (handler-bind ((error (lambda (condition)
			  (restart-case (swank::swank-debugger-hook condition nil)
			    (ignore-error ()
			      :report "ignore error"
			      (return-from whois nil))))))
    (multiple-value-bind (server port) (whois-server fqdn)
      (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol 6))
	    (out "")
	    (whois-map (whois-map-lookup (root-domain-part fqdn))))
	(sb-bsd-sockets:socket-connect s server port)
	(with-open-stream (s (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :none
								:external-format :iso-8859-9
								:element-type 'character))
	  (format s (funcall (car whois-map) fqdn))
	  (force-output s) 
	  ;; no need to read all output, just search every line
	  (do ((line (read-line s nil :eof)
		     (read-line s nil :eof)))
	      ((eq line :eof))
	    (setf out (concatenate 'string out (format nil "~A~%" line))))
	  (format t "~A" out)
	  (cons (cdr whois-map) out))))))
