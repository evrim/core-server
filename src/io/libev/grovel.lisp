(in-package :core-ffi)
(include "ev.h" "signal.h" "sys/stat.h")

(constant (sigint "SIGINT"))

;; (define "EV_MULTIPLICITY")

(constant (evflag-auto "EVFLAG_AUTO"))
(constant (evflag-noenv "EVFLAG_NOENV"))
(constant (evflag-forkcheck "EVFLAG_FORKCHECK"))
(constant (evbackend-select "EVBACKEND_SELECT"))
(constant (evbackend-poll "EVBACKEND_POLL"))
(constant (evbackend-epoll "EVBACKEND_EPOLL"))
(constant (evbackend-kqueue "EVBACKEND_KQUEUE"))

(ctype ev-tstamp "double")
(ctype nlink-t "__UWORD_TYPE")
(ctype off-t "__SLONGWORD_TYPE")
(ctype ev-atomic-t "EV_ATOMIC_T")

(constant (evloop-nonblock "EVLOOP_NONBLOCK"))
(constant (evloop-oneshot "EVLOOP_ONESHOT"))
(constant (evunloop-cancel "EVUNLOOP_CANCEL"))
(constant (evunloop-one "EVUNLOOP_ONE"))
(constant (evunloop-all "EVUNLOOP_ALL"))

(constant (ev-undef "EV_UNDEF"))    
(constant (ev-none "EV_NONE"))     
(constant (ev-read "EV_READ"))     
(constant (ev-write "EV_WRITE"))    
(constant (ev-iofdset "EV_IOFDSET"))  
(constant (ev-timeout "EV_TIMEOUT"))  
(constant (ev-periodic "EV_PERIODIC")) 
(constant (ev-signal "EV_SIGNAL"))   
(constant (ev-child "EV_CHILD"))    
(constant (ev-stat "EV_STAT"))     
(constant (ev-idle "EV_IDLE"))     
(constant (ev-prepare "EV_PREPARE"))  
(constant (ev-check "EV_CHECK"))    
(constant (ev-embed "EV_EMBED"))    
(constant (ev-fork "EV_FORK"))     
(constant (ev-async "EV_ASYNC"))    
(constant (ev-error "EV_ERROR"))    

(cstruct ev-watcher "struct ev_watcher"
  (active "active" :type :int)
  (pending "pending" :type :int)
  (priority "priority" :type :int)
  (data "data" :type :pointer)
  ;;  void (*cb)(EV_P_ struct type *w, int revents);
  (cb "cb" :type :pointer))

(cstruct ev-watcher-time "struct ev_watcher_time"
  (at "at" :type :double))

(cstruct ev-io "struct ev_io"
  (fd "fd" :type :int)
  (events "events" :type :int))

(cstruct ev-timer "struct ev_timer"
  (repeat "repeat" :type :double))

(cstruct ev-periodic "struct ev_periodic"
  (offset "offset" :type :double)
  (interval "interval" :type :double)
  ;; ev_tstamp (*reschedule_cb)(struct ev_periodic *w, ev_tstamp now);
  (reschedule-cb "reschedule_cb" :type :pointer)
  )

(cstruct ev-signal "struct ev_signal"
  (signum "signum" :type :int))

(cstruct ev-child "struct ev_child"
  (flags "flags" :type :int)
  (pid "pid" :type :int)
  (rpid "rpid" :type :int)
  (rstatus "rstatus" :type :int))

(cstruct ev-statdata "struct stat")

(cstruct ev-stat "struct ev_stat"
  (timer "timer" :type :double)
  (interval "interval" :type :double)
  (path "path" :type :string)
  (prev "prev" :type ev-statdata)
  (attr "attr" :type ev-statdata)
  (wd "wd" :type :int))

(cstruct ev-idle "struct ev_idle")
(cstruct ev-prepare "struct ev_prepare")
(cstruct ev-check "struct ev_check")
(cstruct ev-fork "struct ev_fork")
(cstruct ev-embed "struct ev_embed"
  (other "other" :type :pointer)
  (io "io" :type ev-io)
  (prepare "prepare" :type ev-prepare)
  (check "check" :type ev-check)
  (timer "timer" :type ev-timer)
  (periodic "periodic" :type ev-periodic)
  (idle "idle" :type ev-idle)
  (fork "fork" :type ev-fork))
(cstruct ev-async "struct ev_async"
  (sent "sent" :type ev-atomic-t))

;; stat
(ctype time_t "time_t")

(cstruct timespec "struct timespec"
  "UNIX time specification in seconds and nanoseconds."
  (sec  "tv_sec"  :type time_t)
  (nsec "tv_nsec" :type :long))

(cstruct stat "struct stat"
  (st-nlink "st_nlink" :type nlink-t)
  (st-size "st_size" :type off-t)
  (st-atim "st_atim" :type timespec)
  (st-mtim "st_mtim" :type timespec)
  (st-ctim "st_ctim" :type timespec))