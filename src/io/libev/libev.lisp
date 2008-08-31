(in-package :core-ffi)

;; helpers and reimplementations
(define-c-struct-wrapper ev-watcher ())

(defmacro defevfun (name return-type &body body)
  `(defcfun ,name ,return-type
     (loop :pointer)
     ,@body))

(defcfun "ev_default_loop_init" :pointer (flags :unsigned-int))
(defcvar "ev_default_loop_ptr" :pointer)

(defun ev-default-loop (flags)
  (let ((loop *ev-default-loop-ptr*))
    (if (null-pointer-p loop)
	(setf loop (ev-default-loop-init flags)))
    loop))

;; GENERAL FUNS
(defcfun "ev_time" ev-tstamp)
(defcfun "ev_sleep" :void (delay ev-tstamp))
(defcfun "ev_version_major" :int)
(defcfun "ev_version_minor" :int)
(defcfun "ev_supported_backends" :unsigned-int)

;; ev_set_allocator (void *(*cb)(void *ptr, long size))
(defcfun "ev_set_allocator" :void
  (cb :pointer))

;; ev_set_syserr_cb (void (*cb)(const char *msg));
(defcfun "ev_set_syserr_cb" :void
  (cb :pointer))

;; FUNCTIONS CONTROLLING THE EVENT LOOP
(defcfun "ev_loop_new" :pointer (flags :unsigned-int))
(defcfun "ev_default_destroy" :void)
(defevfun "ev_loop_destroy" :void)
(defevfun "ev_loop_fork" :void)
(defevfun "ev_loop_count" :unsigned-int)
(defevfun "ev_backend" :unsigned-int)
(defevfun "ev_now" ev-tstamp)
(defevfun "ev_loop" :void (flags :int))
(defevfun "ev_unloop" :void (how :int))
(defevfun "ev_ref" :void)
(defevfun "ev_unref" :void)

;; +----------------------------------------------------------------------------
;; | GENERIC WATCHER FUNCTIONS
;; +----------------------------------------------------------------------------
(defun ev-init (watcher cback)
  (with-foreign-slots ((active pending priority cb) watcher ev-watcher)
    (setf active 0
	  pending 0
	  priority 0
	  cb cback)))

(defun ev-is-active (ev)
  (foreign-slot-value ev 'ev-watcher 'active))

(defun ev-is-pending (ev)
  (foreign-slot-value ev 'ev-watcher 'pending))

(defun ev-cb (ev)
  (foreign-slot-value ev 'ev-watcher 'cb))

(defun ev-cb-set (ev cb)
  (setf (foreign-slot-value ev 'ev-watcher 'cb) cb))

(defun ev-priority (ev)
  (foreign-slot-value ev 'ev-watcher 'priority))

(defun ev-set-priority (ev priority)
  (setf (foreign-slot-value ev 'ev-watcher 'priority) priority))

(defevfun "ev_invoke" :void (watcher :pointer) (revents :int))
(defevfun "ev_clear_pending" :int (watcher :pointer))

;; ev_io - is this file descriptor readable or writable?
(defun ev-io-set (watcher fd_ events_)
  (with-foreign-slots ((fd events) watcher ev-io)
    (setf fd fd_
	  events (logior events_ ev-iofdset))))

(defun ev-io-init (watcher cb fd events)
  (ev-init watcher cb)
  (ev-io-set watcher fd events))

(defevfun "ev_io_start" :void (watcher :pointer))
(defevfun "ev_io_stop" :void (watcher :pointer))

;; ev_timer - relative and optionally repeating timeouts
(defun ev-timer-set (watcher after repeat_)
  (setf (foreign-slot-value watcher 'ev-watcher-time 'at) after)
  (setf (foreign-slot-value watcher 'ev-timer 'repeat) repeat_))

(defun ev-timer-init (watcher cb after repeat)
  (ev-init watcher cb)
  (ev-timer-set watcher after repeat))

(defevfun "ev_timer_start" :void (watcher :pointer))
(defevfun "ev_timer_stop" :void (watcher :pointer))
(defevfun "ev_timer_again" :void (watcher :pointer))

;; ev_periodic - to cron or not to cron?

(defun ev-periodic-set (watcher offset_ interval_ reschedule-cb_)
  (with-foreign-slots ((offset interval reschedule-cb) watcher ev-periodic)
    (setf offset offset_
	  interval interval_
	  reschedule-cb reschedule-cb_)))

(defun ev-periodic-init (watcher callback after repeat reschedule-cb)
  (ev-init watcher callback)
  (ev-periodic-set watcher after repeat reschedule-cb))

(defevfun "ev_periodic_start" :void (watcher :pointer))
(defevfun "ev_periodic_stop" :void (watcher :pointer))
(defevfun "ev_periodic_again" :void (watcher :pointer))

(defun ev-periodic-at (watcher)
  (foreign-slot-value watcher 'ev-watcher-time 'at))

;; ev_signal - signal me when a signal gets signalled!

(defun ev-signal-set (watcher signum)
  (setf (foreign-slot-value watcher 'ev-signal 'signum) signum))

(defun ev-signal-init (watcher callback signum)
  (ev-init watcher callback)
  (ev-signal-set watcher signum))

(defevfun "ev_signal_start" :void (watcher :pointer))
(defevfun "ev_signal_stop" :void (watcher :pointer))

;; ev_child - watch out for process status changes

(defun ev-child-set (watcher pid_ flags_)
  (with-foreign-slots ((pid flags) watcher ev-child)
    (setf pid pid_
	  flags flags_)))

(defun ev-child-init (watcher callback pid trace)
  (ev-init watcher callback)
  (ev-child-set watcher pid trace))

(defevfun "ev_child_start" :void (watcher :pointer))
(defevfun "ev_child_stop" :void (watcher :pointer))

;; ev_stat - did the file attributes just change?
(defun ev-stat-set (watcher path_ interval_)
  (with-foreign-slots ((path interval wd) watcher ev-stat)
    (setf path path_
	  interval interval_
	  wd -2)))

(defun ev-stat-init (watcher callback path interval)
  (ev-init watcher callback)
  (ev-stat-set watcher path interval))

(defevfun "ev_stat_start" :void (watcher :pointer))
(defevfun "ev_stat_stop" :void (watcher :pointer))
(defevfun "ev_stat_stat" :void (watcher :pointer))

;; ev_idle - when you've got nothing better to do...
(defun ev-idle-init (watcher callback)
  (ev-init watcher callback))

(defevfun "ev_idle_start" :void (watcher :pointer))
(defevfun "ev_idle_stop" :void (watcher :pointer))

;; ev_prepare and ev_check - customise your event loop!
(defun ev-prepare-init (watcher callback)
  (ev-init watcher callback))
(defevfun "ev_prepare_start" :void (watcher :pointer))
(defevfun "ev_prepare_stop" :void (watcher :pointer))

(defun ev-check-init (watcher callback)
  (ev-init watcher callback))
(defevfun "ev_check_start" :void (watcher :pointer))
(defevfun "ev_check_stop" :void (watcher :pointer))

;; ev_embed - when one backend isn't enough...
(defun ev-embed-set (watcher other)
  (setf (foreign-slot-value watcher 'ev-embed 'other) other))

(defun ev-embed-init (watcher callback other)
  (ev-init watcher callback)
  (ev-embed-set watcher other))

(defevfun "ev_embed_start" :void (watcher :pointer))
(defevfun "ev_embed_stop" :void (watcher :pointer))
(defevfun "ev_embed_sweep" :void (watcher :pointer))

;; ev_fork - the audacity to resume the event loop after a fork
(defun ev-fork-init (watcher callback)
  (ev-init watcher callback))
(defevfun "ev_fork_start" :void (watcher :pointer))
(defevfun "ev_fork_stop" :void (watcher :pointer))

;; ev_async - how to wake up another event loop
(defun ev-async-set (watcher)
  (setf (foreign-slot-value watcher 'ev-async 'sent) 0))

(defun ev-async-init (watcher callback)
  (ev-init watcher callback)
  (ev-async-set watcher))

(defevfun "ev_async_start" :void (watcher :pointer))
(defevfun "ev_async_stop" :void (watcher :pointer))
(defevfun "ev_async_send" :void (watcher :pointer))

;; +----------------------------------------------------------------------------
;; | Other functions
;; +----------------------------------------------------------------------------
(defevfun ev-once :void
  (fd :int)
  (events :int)
  (timeout ev-tstamp)
  (callback :pointer))

(defevfun ev-feed-event :void (watcher :pointer) (revents :int))
(defevfun ev-feed-fd-event :void (fd :int) (revents :int))
(defevfun ev-feed-signal-event :void (signum :int))

;; +----------------------------------------------------------------------------
;; | Examples from the docs
;; +----------------------------------------------------------------------------

;; Example: Call stdin_readable_cb when STDIN_FILENO has become, well
;; readable, but only once. Since it is likely line-buffered, you
;; could attempt to read a whole line in the callback.

(defcallback stdin-readable-cb :void ((loop :pointer) (watcher :pointer) (revents :int))
  (declare (ignorable revents))
  (format t "Callbacked")
  (ev-io-stop loop watcher)
  (ev-unloop loop evunloop-all))

(defun example1 (fd)
  (let ((loop (ev-default-loop 0))
	(cb (callback stdin-readable-cb)))
    (with-foreign-object (watcher 'ev-io)
      (bzero watcher size-of-ev-io) 
      (ev-io-init watcher cb fd ev-read)
      (ev-io-start loop watcher)
      (ev-loop loop 0))))

;; Example: Create a timer that fires after 60 seconds.
(defcallback one-minute-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable revents))
  (format t "One minute over.")
  (ev-timer-stop loop w)
  (ev-unloop loop evunloop-all))

(defun example2 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback one-minute-cb)))
    (with-foreign-object (watcher 'ev-timer)
      (bzero watcher size-of-ev-timer)
      (ev-timer-init watcher cb 60.0d0 0.0d0)
      (ev-timer-start loop watcher)
      (ev-loop loop 0))))

;; Example: Create a timeout timer that times out after 10 seconds of
;; inactivity.
(defcallback timeout-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable loop w revents))
  (format t "ten seconds without any activity"))

(defun example3 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback timeout-cb)))
    (with-foreign-object (watcher 'ev-timer)
      (bzero watcher size-of-ev-timer)
      (ev-timer-init watcher cb 0 10)
      (ev-timer-again loop watcher)
      (ev-loop loop 0))))

;; Example: Call a callback every hour, or, more precisely, whenever
;; the system clock is divisible by 3600. The callback invocation
;; times have potentially a lot of jitter, but good long-term
;; stability.

(defcallback every-hour-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable loop w revents))
  (format t "its now a full hour (UTC, or TAI or whatever your clock follows)"))

(defun example4 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback every-hour-cb)))
    (with-foreign-object (hourly-tick 'ev-periodic)
      (bzero hourly-tick size-of-ev-periodic)
      (ev-periodic-init hourly-tick cb 0.0d0 3600.0d0 0)
      (ev-periodic-start loop hourly-tick)
      (ev-loop loop 0))))

;; Example: Try to exit cleanly on SIGINT and SIGTERM.
(defcallback sigint-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable w revents))
  (ev-unloop loop evunloop-all))

(defun example5 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback sigint-cb)))
    (with-foreign-object (w 'ev-signal)
      (bzero w size-of-ev-signal)
      (ev-signal-init w cb sigint)
      (ev-signal-start loop w)
      (ev-loop loop 0))))

;; Example: fork() a new process and install a child handler to wait
;; for its completion.
(defcallback child-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable revents))
  (ev-child-stop loop w)
  ;; printf ("process %d exited with status %x\n", w->rpid, w->rstatus);
  (with-foreign-slots ((rpid rstatus) w ev-child)
    (format t "process ~D exited with status ~D" rpid rstatus)))

(defun example6 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback child-cb)))
    (with-foreign-object (w 'ev-child)
      (bzero w size-of-ev-child)
      (let ((pid (sb-posix::fork)))
	(cond 
	  ((= pid 0) ;;child here
	   (sb-unix:unix-exit 1))
	  (t ;; parent here
	   (ev-child-init w cb pid 0)
	   (ev-child-start loop w)
	   (ev-loop loop 0)))))))

;; Example: Watch /tmp/passwd for attribute changes.
(defcallback passwd-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable loop revents))
  (let ((attr (foreign-slot-value w 'ev-stat 'attr)))
    (with-foreign-slots ((st-nlink st-size st-atim st-mtim) attr stat)
      (if (< 0 st-nlink)
	  ;; file exists
	  (format t "size: ~f, atime: ~f, mtime:~f ~%" st-size st-atim st-mtim)
	  ;; file not found
	  (format t "file not found~%")))))

(defun example7 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback passwd-cb)))
    (with-foreign-object (w 'ev-stat)
      (bzero w size-of-ev-stat)
      (ev-stat-init w cb "/tmp/passwd" 0.0d0)
      (ev-stat-start loop w)
      (ev-loop loop 0))))

;; Example: Like above, but additionally use a one-second delay so we
;; do not miss updates (however, frequent updates will delay
;; processing, too, so one might do the work both on ev_stat callback
;; invocation and on ev_timer callback invocation).

(defparameter *example8-timer* (foreign-alloc 'ev-timer))

(defcallback timer-cb :void ((loop :pointer) (timer :pointer) (revents :int))
  (declare (ignorable revents))
  (ev-timer-stop loop timer)
  (format t "now it's one second after the most recent passwd change~%"))

(defcallback stat-cb :void ((loop :pointer) (stat :pointer) (revents :int))
  (declare (ignorable stat revents))
  (ev-timer-again loop *example8-timer*))

(defun example8 ()
  (let ((loop (ev-default-loop 0))
	(cb1 (callback timer-cb))
	(cb2 (callback stat-cb)))
    (with-foreign-object (passwd 'ev-stat)
      (bzero passwd size-of-ev-stat)
      (bzero *example8-timer* size-of-ev-timer)
      (ev-stat-init passwd cb2 "/tmp/passwd" 0.0d0)
      (ev-stat-start loop passwd)
      (ev-timer-init *example8-timer* cb1 0.0d0 1.02d0)
      (ev-loop loop 0))))

;; Example: Dynamically allocate an ev_idle watcher, start it, and in
;; the callback, free it. Also, use no error checking, as usual.

(defcallback idle-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (declare (ignorable loop revents))
  (format t "event loop is idle~%")
  (ev-idle-stop loop w)
  (foreign-free w))

(defun example9 ()
  (let ((loop (ev-default-loop 0))
	(cb (callback idle-cb))
	(idle-watcher (foreign-alloc 'ev-idle)))
    (bzero idle-watcher size-of-ev-idle)
    (ev-idle-init idle-watcher cb)
    (ev-idle-start loop idle-watcher)
    (ev-loop loop 0)))