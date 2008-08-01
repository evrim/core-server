;; Pinger example
;; Demonstrating core-server defcommand and parser

(defpackage :pinger
  (:use #:cl #:core-server #:arnesi))

(in-package :pinger)

;; path to ping executable
(defparameter +ping+ (core-server::whereis "ping"))

;; Sample output that we need to parse.
;; 
;; quad% ping -q node6 -c 3 
;; PING node6.core.gen.tr (213.232.33.242) 56(84) bytes of data.
;;
;; --- node6.core.gen.tr ping statistics ---
;; 3 packets transmitted, 3 received, 0% packet loss, time 2007ms
;; rtt min/avg/max/mdev = 10.106/10.439/10.620/0.235 ms

;; min,max,avg,mdev are temporary named variables of this parser
;;
;; we're trying to parse the sequence "rtt min/avg/max/mdev = ", if it
;; fails we parse an octet and loop until the end of the stream. If we
;; can parse the sequence, we'll parse floats and then return them as
;; a list of floats.
(defrule ping? (min max avg mdev)
  (:zom (:or (:and (:seq "rtt min/avg/max/mdev = ")
		   (:float? min)
		   #\/
		   (:float? avg)
		   #\/
		   (:float? max)
		   #\/
		   (:float? mdev)
		   (:seq " ms")
		   (:return (list (parse-float min)
				  (parse-float max)
				  (parse-float avg)
				  (parse-float mdev))))
	     (:type octet?))))

;; We're defining a command with two arguments.
;; We'll have a function like this:
;;
;; (ping &key (ping-host (error "Specify host")) (ping-count "1")

(defcommand ping (shell)
  ((ping-host :initform (error "Specify host") :host local)
   (ping-count :initform "1" :host local))
  (:default-initargs :cmd +ping+ :verbose nil))

(defmethod render-arguments ((self ping))
  (list "-q" "-c" (s-v 'ping-count) (s-v 'ping-host)))

;; In the run method, we're setting the command arguments according to
;; the commands protocol. And then parse output with ping?.
(defmethod run-command ((self ping) args)
  (call-next-method)
  (with-core-stream (s (command.output-stream self))
    (ping? s)))