(in-package :tr.gen.core.ffi)

;; +----------------------------------------------------------------------------
;; | Helpers
;; +----------------------------------------------------------------------------

(defctype fd :int)

;;; void bzero(void *s, size_t n);
(defcfun ("bzero" %bzero) :void
  (s :pointer)
  (n size-t))

;; +----------------------------------------------------------------------------
;; | Name Resolution
;; +----------------------------------------------------------------------------

;;; int getnameinfo(const struct sockaddr *sa, socklen_t salen,
;;;                 char *host, size_t hostlen,
;;;                 char *serv, size_t servlen, int flags);
(declaim (inline %getnameinfo))
(defcfun ("getnameinfo" %getnameinfo) retval
  (sa :pointer)    ;; const struct sockaddr *sa
  (salen socklen)  ;; socklen_t salen
  (host :string)   ;; char *host
  (hostlen size-t) ;; size_t hostlen
  (serv :string)   ;; char* serv
  (servlen size-t) ;; size_t servlen
  (flags :int)     ;; int flags
  )

;;; int getaddrinfo(const char *node, const char *service,
;;;                 const struct addrinfo *hints,
;;;                 struct addrinfo **res);
(declaim (inline %getaddrinfo))
(defcfun ("getaddrinfo" %getaddrinfo) retval
  (node :string)
  (service :string)
  (hints :pointer)
  (res :pointer)
  )

;;; void freeaddrinfo(struct addrinfo *res);
(defcfun ("freeaddrinfo" %freeaddrinfo) retval
  (res :pointer))

;;; const char *gai_strerror(int errcode);
(defcfun ("gai_strerror" %gai_strerror) retval
  (errcode :int))

;; +----------------------------------------------------------------------------
;; | Sockets
;; +----------------------------------------------------------------------------

;; int inet_pton(int af, const char *src, void *dst);
(defcfun ("inet_pton" %inet-pton) retval
  (af :int)
  (src :string)
  (dst :pointer))

;; char *inet_ntop(int af, const void *src,char *dst, socklen_t cnt);
(defcfun ("inet_ntop" %inet-ntop) retval
  (af :int)
  (sockaddr :pointer)
  (dst :pointer)
  (len :int))

;; uint32_t htonl(uint32_t hostlong);
(defcfun ("htonl" %htonl) :uint32
  (hostlong :uint32))

;; uint32_t ntohl(uint32_t netlong);
(defcfun ("ntohl" %ntohl) :uint32
  (netlong :uint32))

;; uint16_t htons(uint16_t hostshort);
(defcfun ("htons" %htons) :uint16
  (hostshort :uint16))

;; uint16_t ntohs(uint16_t netshort);
(defcfun ("ntohs" %ntohs) :uint16
  (netshort :uint16))

;; int socket(int domain, int type, int protocol);
(declaim (inline %socket))
(defcfun ("socket" %socket) retval
  (domain :int)
  (type :int)
  (protocol :int))

;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
(declaim (inline %bind))
(defcfun ("bind" %bind) retval
  (sockfd :int)
  (addr :pointer)
  (addrlen socklen))

;; int listen(int sockfd, int backlog);
(declaim (inline %listen))
(defcfun ("listen" %listen) retval
  (sockfd :int)
  (backlog :int))

;; int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
(declaim (inline %accept))
(defcfun ("accept" %accept) retval
  (sockfd :int)
  (addr :pointer)
  (addrlen :pointer))

;; ssize_t recv(int socket, void *buffer, size_t length, int flags); 
(defcfun ("recv" %recv) retval
  (fd fd)
  (buffer :pointer)
  (len :int)
  (flags :int))

;; ssize_t send(int s, const void *buf, size_t len, int flags); 
(defcfun ("send" %send) retval
  (fd :int)
  (buffer :pointer)
  (len :int)
  (flags :int))

;; ssize_t read(int fd, void *buf, size_t count);
(defcfun ("read" %read) ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

;; ssize_t write(int fd, const void *buf, size_t count);
(defcfun ("write" %write) ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

;; int close(int fd);
(declaim (inline %close))
(defcfun ("close" %close) retval
  (fd :int))

;; ssize_t sendfile(int out_fd, int in_fd, off_t *offset, size_t count);
(defcfun ("sendfile" %sendfile) retval
  (out-fd :int)
  (in-fd :int)
  (offset off-t)
  (count size-t))

;; int setsockopt(int s,int level,int optname,void *optval,socklen_t optlen);
(defcfun ("setsockopt" %setsockopt) retval
  (fd fd)
  (level :int)
  (option :int)
  (value :pointer)
  (len :int))

;; int fcntl(int fd, int cmd, long arg); 
(defcfun ("fcntl" %fcntl) retval
  (fd :int)
  (cmd :int)
  (options :long))

(declaim (inline %getprotobyname))
(defcfun ("getprotobyname" %getprotobyname) protoent
  (name :string))

(declaim (inline %gethostbyname))
(defcfun ("gethostbyname" %gethostbyname) hostent
  (name :string))


;; +----------------------------------------------------------------------------
;; | EPOLLUTION
;; +----------------------------------------------------------------------------
;; (defbitfield epoll-events
;;   (:IN      #x001)
;;   (:PRI     #x002)
;;   (:OUT     #x004)
;;   (:ERR     #x008)
;;   (:HUP     #x010)
;;   (:RDNORM  #x040)
;;   (:RDBAND  #x080)
;;   (:WRNORM  #x100)
;;   (:RWBAND  #x200)
;;   (:MSG     #x400)
;;   (:ONESHOT #.(ash 1 30))
;;   (:ET      #.(ash 1 31)))

;; (defcenum :EPOLL-OP
;;   (:ADD 1)
;;   (:DEL 2)
;;   (:MOD 3))

;;  int epoll_create(int size);
(defcfun ("epoll_create" %epoll-create) retval
  (size :int))

;;  int epoll_ctl(int epfd, int op, int fd, struct epoll_event *event);
(defcfun ("epoll_ctl" %epoll-ctl) retval
  (epfd  :int)
  (op    :int)
  (fd    :int)
  (event :pointer))

;; int epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout);
(defcfun ("epoll_wait" %epoll-wait) retval
  (epfd      :int)
  (events    :pointer)
  (maxevents :int)
  (timeout   :int))

;; int epoll_pwait(int epfd, struct epoll_event *events, int maxevents, int timeout, const sigset_t *sigmask);
(defcfun ("epoll_pwait" %epoll-pwait) retval
  (epfd      :int)
  (events    :pointer)
  (maxevents :int)
  (timeout   :int)
  (sigmask   :pointer))

(defbitfield epoll-events
  :epollin epollin
  :epollrdnorm epollrdnorm
  :epollrdband epollrdband
  :epollpri epollpri
  :epollout epollout
  :epollwrnorm epollwrnorm
  :epollwrband epollwrband
  :epollerr epollerr
  :epollhup epollhup
  :epollmsg epollmsg
  :epolloneshot epolloneshot
  :epollet epollet
)
