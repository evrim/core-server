(in-package :core-server)

(defparameter *default-whois-port* 43)

(defparameter *whois-servers*
  '((".com" . "whois.internic.net")
    (".net" . "whois.internic.net")
    (".org" . "whois.internic.net")
    (".edu" . "hois.internic.net")
    (".uk.com" . "whois.uk.com")
    (".eu.org" . "whois.eu.org")
    (".ac" . "whois.nic.ac")
    (".al" . "whois.ripe.net")
    (".am" . "whois.amnic.net")
    (".am" . "whois.amnic.net")
    (".as" . "whois.nic.as")
    (".at" . "whois.nic.at")
    (".au" . "whois.aunic.net")
    (".az" . "whois.ripe.net")
    (".ba" . "whois.ripe.net")
    (".be" . "whois.dns.be")
    (".bg" . "whois.ripe.net")
    (".bm" . "rwhois.ibl.bm:4321")
    (".br" . "registro.fapesp.br")
    (".by" . "whois.ripe.net")
    (".ca" . "whois.cdnnet.ca")
    (".cc" . "whois.nic.cc")
    (".ch" . "whois.nic.ch")
    (".cl" . "whois.nic.cl")
    (".edu.cn" . "whois.edu.cn")
    (".cn" . "whois.cnnic.cn")
    (".cx" . "whois.nic.cx")
    (".cy" . "whois.ripe.net")
    (".cz" . "whois.ripe.net")
    (".de" . "whois.ripe.net")
    (".dk" . "whois.ripe.net")
    (".dz" . "whois.ripe.net")
    (".ee" . "whois.ripe.net")
    (".eg" . "whois.ripe.net")
    (".es" . "whois.ripe.net")
    (".fi" . "whois.ripe.net")
    (".fo" . "whois.ripe.net")
    (".fr" . "whois.nic.fr")
    (".gov" . "whois.nic.gov")
    (".gr" . "whois.ripe.net")
    (".gs" . "whois.adamsnames.tc")
    (".hk" . "whois.hknic.net.hk")
    (".hm" . "whois.nic.hm")
    (".hr" . "whois.ripe.net")
    (".hu" . "whois.ripe.net")
    (".ie" . "whois.ucd.ie")
    (".il" . "whois.ripe.net")
    (".in" . "whois.ncst.ernet.in")
    (".int" . "whois.isi.edu")
    (".is" . "whois.isnet.is")
    (".it" . "whois.nic.it")
    (".jp" . "whois.nic.ad.jp")
    (".kr" . "whois.krnic.net")
    (".kz" . "whois.domain.kz")
    (".li" . "whois.nic.li")
    (".lk" . "whois.nic.lk")
    (".lt" . "whois.ripe.net")
    (".lu" . "whois.ripe.net")
    (".lv" . "whois.ripe.net")
    (".ma" . "whois.ripe.net")
    (".md" . "whois.ripe.net")
    (".mil" . "whois.nic.mil")
    (".mk" . "whois.ripe.net")
    (".mm" . "whois.nic.mm")
    (".ms" . "whois.adamsnames.tc")
    (".mt" . "whois.ripe.net")
    (".mx" . "whois.nic.mx")
    (".my" . "whois.mynic.net")
    (".nl" . "whois.domain-registry.nl")
    (".no" . "whois.norid.no")
    (".nu" . "whois.nic.nu")
    (".pe" . "whois.rcp.net.pe")
    (".pk" . "whois.pknic.net.pk")
    (".pl" . "whois.ripe.net")
    (".pt" . "whois.dns.pt")
    (".ro" . "whois.ripe.net")
    (".ru" . "whois.ripn.net")
    (".se" . "whois.nic-se.se")
    (".sg" . "whois.nic.net.sg")
    (".sh" . "whois.nic.sh")
    (".si" . "whois.ripe.net")
    (".sk" . "whois.ripe.net")
    (".sm" . "whois.ripe.net")
    (".st" . "whois.nic.st")
    (".su" . "whois.ripe.net")
    (".tc" . "whois.adamsnames.tc")
    (".tf" . "whois.adamsnames.tc")
    (".th" . "whois.thnic.net")
    (".tj" . "whois.nic.tj")
    (".tm" . "whois.nic.tm")
    (".tn" . "whois.ripe.net")
    (".to" . "whois.tonic.to")
    (".tr" . "whois.metu.edu.tr")
    (".tw" . "whois.twnic.net")
    (".ua" . "whois.ripe.net")
    (".ac.uk" . "whois.ja.net")
    (".gov.uk" . "whois.ja.net")
    (".uk" . "whois.nic.uk")
    (".us" . "whois.isi.edu")
    (".va" . "whois.ripe.net")
    (".vg" . "whois.adamsnames.tc")
    (".yu" . "whois.ripe.net")
    (".gb.com" . "whois.nomination.net")
    (".gb.net" . "whois.nomination.net")
    (".za" . "whois.co.za")))

(defun root-domain-part (fqdn)
  (awhen (position #\. fqdn :from-end t)
    (subseq fqdn it)))

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

(defun whois (fqdn)
  (multiple-value-bind (server port) (whois-server fqdn)
    (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol 6)))
      (sb-bsd-sockets:socket-connect s server port)
      (with-open-stream (s (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :none))
	(format s "~a~c~c" fqdn #\return #\linefeed)
	(force-output s)
	(do ((line (read-line s nil :eof) (read-line s nil :eof)))
	    ((eq line :eof))
	  (princ line)
	  (terpri))))))


