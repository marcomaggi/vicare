;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: Linux platform API
;;;Date: Mon Nov  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (vicare linux)
  (export
    ;; process termination status
    waitid
    make-struct-siginfo_t		struct-siginfo_t?
    struct-siginfo_t-si_pid		struct-siginfo_t-si_uid
    struct-siginfo_t-si_signo		struct-siginfo_t-si_status
    struct-siginfo_t-si_code
    WIFCONTINUED

    ;; platform resources usage and limits
    prlimit

    (rename (px.make-struct-rlimit		make-struct-rlimit)
	    (px.struct-rlimit?			struct-rlimit?)
	    (px.struct-rlimit-rlim_cur		struct-rlimit-rlim_cur)
	    (px.struct-rlimit-rlim_max		struct-rlimit-rlim_max)
	    (px.set-struct-rlimit-rlim_cur!	set-struct-rlimit-rlim_cur!)
	    (px.set-struct-rlimit-rlim_max!	set-struct-rlimit-rlim_max!)
	    (px.RLIM_INFINITY			RLIM_INFINITY))

    ;; epoll
    epoll-create			epoll-create1
    epoll-ctl				epoll-wait
    epoll-event-alloc			epoll-event-size
    epoll-event-set-events!		epoll-event-ref-events
    epoll-event-set-data-ptr!		epoll-event-ref-data-ptr
    epoll-event-set-data-fd!		epoll-event-ref-data-fd
    epoll-event-set-data-u32!		epoll-event-ref-data-u32
    epoll-event-set-data-u64!		epoll-event-ref-data-u64

    ;; interprocess signals
    signalfd				read-signalfd-siginfo

    make-struct-signalfd-siginfo	struct-signalfd-siginfo?
    struct-signalfd-siginfo-ssi_signo	set-struct-signalfd-siginfo-ssi_signo!
    struct-signalfd-siginfo-ssi_errno	set-struct-signalfd-siginfo-ssi_errno!
    struct-signalfd-siginfo-ssi_code	set-struct-signalfd-siginfo-ssi_code!
    struct-signalfd-siginfo-ssi_pid	set-struct-signalfd-siginfo-ssi_pid!
    struct-signalfd-siginfo-ssi_uid	set-struct-signalfd-siginfo-ssi_uid!
    struct-signalfd-siginfo-ssi_fd	set-struct-signalfd-siginfo-ssi_fd!
    struct-signalfd-siginfo-ssi_tid	set-struct-signalfd-siginfo-ssi_tid!
    struct-signalfd-siginfo-ssi_band	set-struct-signalfd-siginfo-ssi_band!
    struct-signalfd-siginfo-ssi_overrun	set-struct-signalfd-siginfo-ssi_overrun!
    struct-signalfd-siginfo-ssi_trapno	set-struct-signalfd-siginfo-ssi_trapno!
    struct-signalfd-siginfo-ssi_status	set-struct-signalfd-siginfo-ssi_status!
    struct-signalfd-siginfo-ssi_int	set-struct-signalfd-siginfo-ssi_int!
    struct-signalfd-siginfo-ssi_ptr	set-struct-signalfd-siginfo-ssi_ptr!
    struct-signalfd-siginfo-ssi_utime	set-struct-signalfd-siginfo-ssi_utime!
    struct-signalfd-siginfo-ssi_stime	set-struct-signalfd-siginfo-ssi_stime!
    struct-signalfd-siginfo-ssi_addr	set-struct-signalfd-siginfo-ssi_addr!

    ;; timer event through file descriptors
    timerfd-create			timerfd-read
    timerfd-settime			timerfd-gettime
    (rename (px.make-struct-timespec		make-struct-timespec)
	    (px.struct-timespec?		struct-timespec?)
	    (px.struct-timespec-tv_sec		struct-timespec-tv_sec)
	    (px.struct-timespec-tv_nsec		struct-timespec-tv_nsec)
	    (px.set-struct-timespec-tv_sec!	set-struct-timespec-tv_sec!)
	    (px.set-struct-timespec-tv_nsec!	set-struct-timespec-tv_nsec!)

	    (px.make-struct-itimerspec			make-struct-itimerspec)
	    (px.struct-itimerspec?			struct-itimerspec?)
	    (px.struct-itimerspec-it_interval		struct-itimerspec-it_interval)
	    (px.struct-itimerspec-it_value		struct-itimerspec-it_value)
	    (px.set-struct-itimerspec-it_interval!	set-struct-itimerspec-it_interval!)
	    (px.set-struct-itimerspec-it_value!		set-struct-itimerspec-it_value!))

    ;; inotify, monitoring file system events
    inotify-init			inotify-init1
    inotify-add-watch			inotify-rm-watch
    inotify-read

    (rename (%make-struct-inotify-event		make-struct-inotify-event))
    struct-inotify-event?
    struct-inotify-event-wd		set-struct-inotify-event-wd!
    struct-inotify-event-mask		set-struct-inotify-event-mask!
    struct-inotify-event-cookie		set-struct-inotify-event-cookie!
    struct-inotify-event-len		set-struct-inotify-event-len!
    struct-inotify-event-name		set-struct-inotify-event-name!
    )
  (import (vicare)
    (vicare syntactic-extensions)
    (vicare platform constants)
    (vicare arguments validation)
    (prefix (vicare words)
	    words.)
    (prefix (vicare posix)
	    px.)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected fixnum index as argument" obj))

(define-argument-validation (pathname who obj)
  (or (bytevector? obj) (string? obj))
  (assertion-violation who "expected string or bytevector as pathname argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (signal who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))

(define-argument-validation (file-descriptor/-1 who obj)
  (or (px.file-descriptor? obj)
      (eqv? -1 obj))
  (assertion-violation who "expected -1 or file descriptor as argument" obj))

(define-argument-validation (vector-of-signums who obj)
  (and (vector? obj) (vector-for-all fixnum? obj))
  (assertion-violation who "expected vector of signums as arguments" obj))

(define-argument-validation (clockid who obj)
  (and (fixnum? obj)
       (or (unsafe.fx= obj CLOCK_REALTIME)
	   (unsafe.fx= obj CLOCK_MONOTONIC)))
  (assertion-violation who
    (string-append "expected fixnum " (number->string CLOCK_REALTIME)
		   " or " (number->string CLOCK_MONOTONIC)
		   " as clockid argument")
    obj))

(define-argument-validation (timerfd-settime-flags who obj)
  (and (fixnum? obj)
       (or (unsafe.fxzero? obj)
	   (unsafe.fx= obj TFD_TIMER_ABSTIME)))
  (assertion-violation who
    "expected the fixnum zero or TFD_TIMER_ABSTIME as flags argument"
    obj))

;;; --------------------------------------------------------------------

(define (%valid-itimerspec? obj)
  (and (px.struct-itimerspec? obj)
       (let ((T (px.struct-itimerspec-it_interval obj)))
	 (and (px.struct-timespec? T)
	      (words.signed-long? (px.struct-timespec-tv_sec  T))
	      (words.signed-long? (px.struct-timespec-tv_nsec T))))
       (let ((T (px.struct-itimerspec-it_value obj)))
	 (and (px.struct-timespec? T)
	      (words.signed-long? (px.struct-timespec-tv_sec  T))
	      (words.signed-long? (px.struct-timespec-tv_nsec T))))))

(define-argument-validation (itimerspec who obj)
  (%valid-itimerspec? obj)
  (assertion-violation who "expected struct-itimerspec as argument" obj))

(define-argument-validation (itimerspec/false who obj)
  (or (not obj) (%valid-itimerspec? obj))
  (assertion-violation who "expected false or struct-itimerspec as argument" obj))

(define-argument-validation (rlimit who obj)
  (%valid-struct-rlimit? obj)
  (assertion-violation who "expected struct-rlimit as argument" obj))

(define-argument-validation (rlimit/false who obj)
  (or (not obj)
      (%valid-struct-rlimit? obj))
  (assertion-violation who "expected false or struct-rlimit as argument" obj))

(define-argument-validation (inotify-event who obj)
  (%valid-struct-inotify-event? obj)
  (assertion-violation who "expected struct-inotify-event as argument" obj))

(define-argument-validation (inotify-watch-descriptor who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected C language \"int\" exact integer as inotify watch descriptor argument"
    obj))


;;;; helpers

(define (%raise-errno-error who errno . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-irritants-condition irritants))))

(define (%valid-struct-rlimit? obj)
  (and (px.struct-rlimit? obj)
       (words.word-s64? (px.struct-rlimit-rlim_cur obj))
       (words.word-s64? (px.struct-rlimit-rlim_max obj))))

(define (%valid-struct-inotify-event? obj)
  (and (struct-inotify-event? obj)
       (words.signed-int? (struct-inotify-event-wd obj))
       (words.word-u32? (struct-inotify-event-mask obj))
       (words.word-u32? (struct-inotify-event-cookie obj))
       (words.word-u32? (struct-inotify-event-len obj))
       (let ((name (struct-inotify-event-name obj)))
	 (or (not name) (bytevector? name)))))


;;;; process termination status

(define-struct struct-siginfo_t
  (si_pid si_uid si_signo si_status si_code))

(define (waitid idtype id options)
  (define who 'waitid)
  (with-arguments-validation (who)
      ((fixnum  idtype)
       (fixnum	id)
       (fixnum	options))
    (capi.linux-waitid idtype id (make-struct-siginfo_t #f #f #f #f #f) options)))

(define (WIFCONTINUED status)
  (define who 'WIFCONTINUED)
  (with-arguments-validation (who)
      ((fixnum  status))
    (capi.linux-WIFCONTINUED status)))


;;;; platform resources usage and limits

(define prlimit
  (case-lambda
   ((pid resource)
    (prlimit pid resource #f (px.make-struct-rlimit)))
   ((pid resource new-rlimit)
    (prlimit pid resource new-rlimit (px.make-struct-rlimit)))
   ((pid resource new-rlimit old-rlimit)
    (define who 'prlimit)
    (with-arguments-validation (who)
	((pid		pid)
	 (signed-int	resource)
	 (rlimit/false	new-rlimit)
	 (rlimit	old-rlimit))
      (let ((rv (capi.linux-prlimit pid resource new-rlimit old-rlimit)))
	(if (unsafe.fxzero? rv)
	    old-rlimit
	  (%raise-errno-error who rv pid resource new-rlimit old-rlimit)))))))


;;;; epoll

(define epoll-create
  (case-lambda
   (()
    (epoll-create 16))
   ((size)
    (define who 'epoll-create)
    (with-arguments-validation (who)
	((signed-int	size))
      (let ((rv (capi.linux-epoll-create size)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv size)))))))

(define (epoll-create1 flags)
  (define who 'epoll-create1)
  (with-arguments-validation (who)
      ((signed-int	flags))
    (let ((rv (capi.linux-epoll-create flags)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv flags)))))

(define epoll-ctl
  (case-lambda
   ((epfd op fd)
    (epoll-ctl epfd op fd #f))
   ((epfd op fd event)
    (define who 'epoll-ctl)
    (with-arguments-validation (who)
	((px.file-descriptor	epfd)
	 (fixnum		op)
	 (px.file-descriptor	fd)
	 (pointer/false		event))
      (let ((rv (capi.linux-epoll-ctl epfd op fd event)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error who rv epfd op fd event)))))))

(define (epoll-wait epfd event maxevents timeout-ms)
  (define who 'epoll-wait)
  (with-arguments-validation (who)
      ((px.file-descriptor	epfd)
       (pointer			event)
       (signed-int		maxevents)
       (signed-int		timeout-ms))
    (let ((rv (capi.linux-epoll-wait epfd event maxevents timeout-ms)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv epfd event maxevents timeout-ms)))))

;;; --------------------------------------------------------------------

(define (epoll-event-alloc number-of-entries)
  (define who 'epoll-event-alloc)
  (with-arguments-validation (who)
      ((positive-fixnum	number-of-entries))
    (let ((rv (capi.linux-epoll-event-alloc number-of-entries)))
      (or rv (%raise-errno-error who ENOMEM number-of-entries)))))

(define (epoll-event-size)
  (capi.linux-epoll-event-size))

;;; --------------------------------------------------------------------

(let-syntax
    ((define-epoll-event-field
       (syntax-rules ()
	 ((_ ?mutator ?accessor ?value-type ?mutator-func ?accessor-func)
	  (begin
	    (define (?mutator events-array index new-value)
	      (define who '?mutator)
	      (with-arguments-validation (who)
		  ((pointer		events-array)
		   (index		index)
		   (?value-type	new-value))
		(?mutator-func events-array index new-value)))
	    (define (?accessor events-array index)
	      (define who '?accessor)
	      (with-arguments-validation (who)
		  ((pointer		events-array)
		   (index		index))
		(?accessor-func events-array index))))))))

  (define-epoll-event-field
    epoll-event-set-events!
    epoll-event-ref-events
    word-u32
    capi.linux-epoll-event-set-events!
    capi.linux-epoll-event-ref-events)

  (define-epoll-event-field
    epoll-event-set-data-ptr!
    epoll-event-ref-data-ptr
    pointer
    capi.linux-epoll-event-set-data-ptr!
    capi.linux-epoll-event-ref-data-ptr)

  (define-epoll-event-field
    epoll-event-set-data-fd!
    epoll-event-ref-data-fd
    px.file-descriptor
    capi.linux-epoll-event-set-data-fd!
    capi.linux-epoll-event-ref-data-fd)

  (define-epoll-event-field
    epoll-event-set-data-u32!
    epoll-event-ref-data-u32
    word-u32
    capi.linux-epoll-event-set-data-u32!
    capi.linux-epoll-event-ref-data-u32)

  (define-epoll-event-field
    epoll-event-set-data-u64!
    epoll-event-ref-data-u64
    word-u64
    capi.linux-epoll-event-set-data-u64!
    capi.linux-epoll-event-ref-data-u64)
  )


;;;; interprocess signals

(define-struct struct-signalfd-siginfo
  (ssi_signo	; 0
   ssi_errno	; 1
   ssi_code	; 2
   ssi_pid	; 3
   ssi_uid	; 4
   ssi_fd	; 5
   ssi_tid	; 6
   ssi_band	; 7
   ssi_overrun	; 8
   ssi_trapno	; 9
   ssi_status	; 10
   ssi_int	; 11
   ssi_ptr	; 12
   ssi_utime	; 13
   ssi_stime	; 14
   ssi_addr))	; 15

(define (%struct-signalfd-siginfo-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[struct-signalfd-siginfo")
  (%display " ssi_signo=")	(%display (struct-signalfd-siginfo-ssi_signo S))
  (%display " ssi_errno=")	(%display (struct-signalfd-siginfo-ssi_errno S))
  (%display " ssi_code=")	(%display (struct-signalfd-siginfo-ssi_code S))
  (%display " ssi_pid=")	(%display (struct-signalfd-siginfo-ssi_pid S))
  (%display " ssi_uid=")	(%display (struct-signalfd-siginfo-ssi_uid S))
  (%display " ssi_fd=")		(%display (struct-signalfd-siginfo-ssi_fd S))
  (%display " ssi_tid=")	(%display (struct-signalfd-siginfo-ssi_tid S))
  (%display " ssi_band=")	(%display (struct-signalfd-siginfo-ssi_band S))
  (%display " ssi_overrun=")	(%display (struct-signalfd-siginfo-ssi_overrun S))
  (%display " ssi_trapno=")	(%display (struct-signalfd-siginfo-ssi_trapno S))
  (%display " ssi_status=")	(%display (struct-signalfd-siginfo-ssi_status S))
  (%display " ssi_int=")	(%display (struct-signalfd-siginfo-ssi_int S))
  (%display " ssi_ptr=")	(%display (struct-signalfd-siginfo-ssi_ptr S))
  (%display " ssi_utime=")	(%display (struct-signalfd-siginfo-ssi_utime S))
  (%display " ssi_stime=")	(%display (struct-signalfd-siginfo-ssi_stime S))
  (%display " ssi_addr=")	(%display (struct-signalfd-siginfo-ssi_addr S))
  (%display "]"))

(define (signalfd fd mask flags)
  (define who 'signalfd)
  (with-arguments-validation (who)
      ((file-descriptor/-1	fd)
       (vector-of-signums	mask)
       (fixnum			flags))
    (let ((rv (capi.linux-signalfd fd mask flags)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fd mask flags)))))

(define (read-signalfd-siginfo fd)
  (define who 'read-signalfd-siginfo)
  (with-arguments-validation (who)
      ((px.file-descriptor	fd))
    (let* ((info (make-struct-signalfd-siginfo #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
	   (rv   (capi.linux-read-signalfd-siginfo fd info)))
      (cond ((unsafe.fxzero? rv)
	     info)
	    ((unsafe.fx= rv EAGAIN)
	     #f)
	    (else
	     (%raise-errno-error who rv fd))))))


;;;; timer event through file descriptors

(define timerfd-create
  (case-lambda
   ((clockid)
    (timerfd-create clockid 0))
   ((clockid flags)
    (define who 'timerfd-create)
    (with-arguments-validation (who)
	((clockid		clockid)
	 (fixnum		flags))
      (let ((rv (capi.linux-timerfd-create clockid flags)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv clockid flags)))))))

(define timerfd-settime
  (case-lambda
   ((fd flags new)
    (timerfd-settime fd flags new #f))
   ((fd flags new old)
    (define who 'timerfd-settime)
    (with-arguments-validation (who)
	((px.file-descriptor	fd)
	 (timerfd-settime-flags	flags)
	 (itimerspec		new)
	 (itimerspec/false	old))
      (let ((rv (capi.linux-timerfd-settime fd flags new old)))
	(if (unsafe.fxzero? rv)
	    old
	  (%raise-errno-error who rv fd flags new old)))))))

(define timerfd-gettime
  (case-lambda
   ((fd)
    (timerfd-gettime fd (px.make-struct-itimerspec
			 (px.make-struct-timespec 0 0)
			 (px.make-struct-timespec 0 0))))
   ((fd curr)
    (define who 'timerfd-gettime)
    (with-arguments-validation (who)
	((px.file-descriptor	fd)
	 (itimerspec		curr))
      (let ((rv (capi.linux-timerfd-gettime fd curr)))
	(if (unsafe.fxzero? rv)
	    curr
	  (%raise-errno-error who rv fd curr)))))))

(define (timerfd-read fd)
  (define who 'timerfd-read)
  (with-arguments-validation (who)
      ((px.file-descriptor	fd))
    (let ((rv (capi.linux-timerfd-read fd)))
      (if (<= 0 rv)
	  rv
	(%raise-errno-error who rv fd)))))


;;;; inotify, monitoring file system events

(define-struct struct-inotify-event
  (wd mask cookie len name))

(define (%struct-inotify-event-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-inotify-event\"")
  (%display " wd=")	(%display (struct-inotify-event-wd	S))
  (%display " mask=")	(%display (struct-inotify-event-mask	S))
  (%display " cookie=")	(%display (struct-inotify-event-cookie	S))
  (%display " len=")	(%display (struct-inotify-event-len	S))
  (%display " name=")	(%display (struct-inotify-event-name	S))
  (%display "]"))

(define %make-struct-inotify-event
  (case-lambda
   (()
    (make-struct-inotify-event 0 0 0 0 #f))
   ((wd mask cookie len name)
    (make-struct-inotify-event wd mask cookie len name))))

;;; --------------------------------------------------------------------

(define (inotify-init)
  (define who 'inotify-init)
  (let ((rv (capi.linux-inotify-init)))
    (if (unsafe.fx< 0 rv)
	rv
      (%raise-errno-error who rv))))

(define (inotify-init1 flags)
  (define who 'inotify-init1)
  (with-arguments-validation (who)
      ((fixnum	flags))
    (let ((rv (capi.linux-inotify-init1 flags)))
      (if (unsafe.fx< 0 rv)
	  rv
	(%raise-errno-error who rv flags)))))

(define (inotify-add-watch fd pathname mask)
  (define who 'inotify-add-watch)
  (with-arguments-validation (who)
      ((px.file-descriptor	fd)
       (pathname	pathname)
       (word-u32	mask))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.linux-inotify-add-watch fd pathname.bv mask)))
	(if (not (negative? rv))
	    rv
	  (%raise-errno-error who rv fd pathname mask))))))

(define (inotify-rm-watch fd wd)
  (define who 'inotify-rm-watch)
  (with-arguments-validation (who)
      ((px.file-descriptor			fd)
       (inotify-watch-descriptor	wd))
    (let ((rv (capi.linux-inotify-rm-watch fd wd)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv fd wd)))))

(define inotify-read
  (case-lambda
   ((fd)
    (inotify-read fd (%make-struct-inotify-event)))
   ((fd event)
    (define who 'inotify-read)
    (with-arguments-validation (who)
	((px.file-descriptor	fd)
	 (inotify-event		event))
      (let ((rv (capi.linux-inotify-read fd event)))
	(cond ((struct-inotify-event? rv)
	       event)
	      ((unsafe.fxzero? rv)
	       rv)
	      (else
	       (%raise-errno-error who rv fd event))))))))


;;;; done

(set-rtd-printer! (type-descriptor struct-signalfd-siginfo)	%struct-signalfd-siginfo-printer)
(set-rtd-printer! (type-descriptor struct-inotify-event)	%struct-inotify-event-printer)

)

;;; end of file
