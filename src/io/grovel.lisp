(in-package :tr.gen.core.ffi)

(ctype retval "int")

;;;-----------------------------------------------------------------------------
;;; GETPROTOBYNAME, GETHOSTBYNAME TYPE DEFINITIONS
;;;-----------------------------------------------------------------------------
(include "netdb.h")
(cstruct protoent "struct protoent"
	 (name "p_name" :type :string)
	 (aliases "p_aliases" :type :pointer)
	 (proto "p_proto" :type :int))

(cstruct hostent "struct hostent"
	 (name "h_name" :type :string)
	 (aliases "h_aliases" :type :pointer)
	 (type "h_addrtype" :type :int)
	 (len "h_length" :type :int)
	 (list "h_addr_list" :type :pointer))

(include "sys/socket.h")
(constant (af-inet "AF_INET" "PF_INET")
	  :documentation "IPv4 Protocol family")
(constant (af-unspec "AF_UNSPEC"))

;; possible values for "ai-flags"
(constant (flag-ai-passive "AI_PASSIVE"))
(constant (flag-ai-canonname "AI_CANONNAME"))
(constant (flag-ai-numerichost "AI_NUMERICHOST"))
(constant (flag-ai-v4mapped "AI_V4MAPPED"))
(constant (flag-ai-all "AI_ALL"))
(constant (flag-ai-addrconfig "AI_ADDRCONFIG"))
(constant (flag-ai-idn "AI_IDN"))
(constant (flag-ai-canonidn "AI_CANONIDN"))

;; +----------------------------------------------------------------------------
;; |IPV4 SOCKET ADDRESS
;; +----------------------------------------------------------------------------
(include "sys/socket.h")
(ctype socklen "socklen_t")
(ctype sa-family "sa_family_t")
;;; socket() - socket address family
(constant (af-inet "AF_INET" "PF_INET") :documentation "IPv4 Protocol family")
(constant (af-local "AF_UNIX" "AF_LOCAL" "PF_UNIX" "PF_LOCAL")
          :documentation "File domain sockets")

;;; socket() - socket type
(constant (sock-stream "SOCK_STREAM") :documentation "TCP")
(constant (sock-dgram "SOCK_DGRAM") :documentation "UDP")

;;; socket() - socket protocols
(constant (ipproto-tcp "IPPROTO_TCP"))
(constant (ipproto-udp "IPPROTO_UDP"))

(cstruct sockaddr "struct sockaddr"
  (family "sa_family" :type sa-family)
  (sa-data "sa_data" :type :char :count 14))

;; GETNAMEINFO, GETADDRINFO
(include "sys/types.h" "sys/socket.h" "netdb.h")
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")

(include "sys/sendfile.h")
(ctype off-t "off_t")

(constant (ni-numerichost "NI_NUMERICHOST"))
(constant (ni-numericserv "NI_NUMERICSERV"))
(constant (ni-maxhost "NI_MAXHOST"))
(constant (ni-maxserv "NI_MAXSERV"))

;; struct addrinfo {
;;   int              ai_flags;
;;   int              ai_family;
;;   int              ai_socktype;
;;   int              ai_protocol;
;;   size_t           ai_addrlen;
;;   struct sockaddr *ai_addr;
;;   char            *ai_canonname;
;;   struct addrinfo *ai_next;
;; };
(cstruct addrinfo "struct addrinfo"
  (ai-flags "ai_flags" :type :int)
  (ai-family "ai_family" :type :int)
  (ai-socktype "ai_socktype" :type :int)
  (ai-protocol "ai_protocol" :type :int)
  (ai-addrlen "ai_addrlen" :type size-t)
  (ai-addr "ai_addr" :type :pointer)
  (ai-canonname "ai_canonname" :type :pointer)
  (ai-next "ai_next" :type :pointer))

(cstruct linger "struct linger"
  (l-onoff "l_onoff" :type :int)
  (l-linger "l_linger" :type :int))

;; +----------------------------------------------------------------------------
;; |SOCKET ADDRESS
;; +----------------------------------------------------------------------------
(include "netinet/in.h")
(ctype in-port "in_port_t")
(ctype in-addr "in_addr_t")

(cstruct sockaddr-in "struct sockaddr_in"
  "An IPv4 socket address."
  (family "sin_family" :type sa-family)
  (port   "sin_port"   :type in-port)
  (addr   "sin_addr"   :type in-addr))

(cstruct in-addr-struct "struct in_addr"
  (addr "s_addr" :type :uint32))

(include "netinet/tcp.h")
(progn
  (constant (tcp-nodelay "TCP_NODELAY")))

;; +----------------------------------------------------------------------------
;; |SET/GETSOCKOPT
;; +----------------------------------------------------------------------------
(progn
  (constant (sol-socket "SOL_SOCKET"))
  (constant (so-debug "SO_DEBUG"))
  (constant (so-reuseaddr "SO_REUSEADDR"))
  (constant (so-type "SO_TYPE"))
  (constant (so-error "SO_ERROR"))
  (constant (so-keepalive "SO_KEEPALIVE"))
  (constant (so-rcvtimeo "SO_RCVTIMEO"))
  (constant (so-sndtimeo "SO_SNDTIMEO"))
  (constant (so-linger "SO_LINGER")))

;; +----------------------------------------------------------------------------
;; |SEND/RECV FLAGS
;; +----------------------------------------------------------------------------
(progn
  (constant (msg-dontwait "MSG_DONTWAIT")))

;; +----------------------------------------------------------------------------
;; |FCNTL
;; +----------------------------------------------------------------------------
(include "fcntl.h")
;; FCNTL FLAGS
(progn
  (constant (rdonly "O_RDONLY"))
  (constant (wdonly "O_WRONLY"))
  (constant (nonblock "O_NONBLOCK")))

;; FCNTL COMMANDS
(progn
  (constant (getfd "F_GETFD"))
  (constant (setfd "F_SETFD"))
  (constant (getfl "F_GETFL"))
  (constant (setfl "F_SETFL"))
  (constant (setown "F_SETOWN"))
  (constant (getown "F_GETOWN"))
  (constant (setsig "F_SETSIG"))
  (constant (getsig "F_GETSIG")))

;; +----------------------------------------------------------------------------
;; | TIME
;; +----------------------------------------------------------------------------
(include "time.h")
(ctype time_t "time_t")
(ctype suseconds "suseconds_t")

(cstruct timespec "struct timespec"
	 "UNIX time specification in seconds and nanoseconds."
	 (sec  "tv_sec"  :type time_t)
	 (nsec "tv_nsec" :type :long))


;;;-----------------------------------------------------------------------------
;;; EPOLL TYPE DEFINITIONS
;;;-----------------------------------------------------------------------------
(include "sys/epoll.h" "sys/ioctl.h")
(ctype uint32-t "uint32_t")
(progn
  ;;  (ctype sigset-t "sigset_t") ;; broken
  (cunion epoll-data "epoll_data_t"
          (ptr "ptr" :type :pointer)
          (fd  "fd"  :type :int)
          (u32 "u32" :type :uint32)
          (u64 "u64" :type :uint64))

  (cstruct epoll-event "struct epoll_event"
           (events "events" :type uint32-t)
           (data   "data"   :type epoll-data))


  ;;---------------------------------------------------------------------------
  ;; EPOLL EVENTS
  ;;---------------------------------------------------------------------------
  (constant (epollin "EPOLLIN"))
  (constant (epollrdnorm "EPOLLRDNORM"))
  (constant (epollrdband "EPOLLRDBAND"))
  (constant (epollpri "EPOLLPRI"))
  (constant (epollout "EPOLLOUT"))
  (constant (epollwrnorm "EPOLLWRNORM"))
  (constant (epollwrband "EPOLLWRBAND"))
  (constant (epollerr "EPOLLERR"))
  (constant (epollhup "EPOLLHUP"))
  (constant (epollmsg "EPOLLMSG"))

  ;;--------------------------------------------------------------------------
  ;; EPOLL OPERATIONS FOR EPOLL_CTL()
  ;;--------------------------------------------------------------------------
  (constant (epoll-ctl-add "EPOLL_CTL_ADD"))
  (constant (epoll-ctl-del "EPOLL_CTL_DEL"))
  (constant (epoll-ctl-mod "EPOLL_CTL_MOD")))

;;(include "uuid/uuid.h")
;;(ctype uuid-t "uuid_t")
