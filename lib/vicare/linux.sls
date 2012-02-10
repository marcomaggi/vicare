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
    make-struct-siginfo_t	struct-siginfo_t?
    struct-siginfo_t-si_pid	struct-siginfo_t-si_uid
    struct-siginfo_t-si_signo	struct-siginfo_t-si_status
    struct-siginfo_t-si_code
    WIFCONTINUED

    ;; epoll
    epoll-create		epoll-create1
    epoll-ctl			epoll-wait
    epoll-event-alloc		epoll-event-size
    epoll-event-set-events!	epoll-event-ref-events
    epoll-event-set-data-ptr!	epoll-event-ref-data-ptr
    epoll-event-set-data-fd!	epoll-event-ref-data-fd
    epoll-event-set-data-u32!	epoll-event-ref-data-u32
    epoll-event-set-data-u64!	epoll-event-ref-data-u64
    )
  (import (vicare)
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare words)
	    words.)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; helpers

(define-syntax define-for-linux
  (if #t
      (syntax-rules ()
	((_ (?who . ?args) . ?body)
	 (define (?who . ?args) . ?body)))
    (syntax-rules ()
      ((_ (?who . ?args) . ?body)
       (define (?who . ?args)
	 (assertion-violation '?who
	   "attempt to call unimplemented GNU+Linux function"))))))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (boolean who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as argument" obj))

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (positive-fixnum who obj)
  (and (fixnum? obj) (unsafe.fx< 0 obj))
  (assertion-violation who "expected positive fixnum as argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected fixnum index as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who "expected C language signed int as argument" obj))

(define-argument-validation (uint32 who obj)
  (words.word-u32? obj)
  (assertion-violation who "expected C language uint32 as argument" obj))

(define-argument-validation (uint64 who obj)
  (words.word-u64? obj)
  (assertion-violation who "expected C language uint64 as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (signal who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))

(define-argument-validation (file-descriptor who obj)
  (%file-descriptor? obj)
  (assertion-violation who "expected fixnum file descriptor as argument" obj))


;;;; helpers

(define (%raise-errno-error who errno . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-irritants-condition irritants))))

(define-inline (%file-descriptor? obj)
  ;;Do  what   is  possible  to  recognise   fixnums  representing  file
  ;;descriptors.
  ;;
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<  obj FD_SETSIZE)))


;;;; process termination status

(define-struct struct-siginfo_t
  (si_pid si_uid si_signo si_status si_code))

(define-for-linux (waitid idtype id options)
  (define who 'waitid)
  (with-arguments-validation (who)
      ((fixnum  idtype)
       (fixnum	id)
       (fixnum	options))
    (capi.linux-waitid idtype id (make-struct-siginfo_t #f #f #f #f #f) options)))

(define-for-linux (WIFCONTINUED status)
  (define who 'WIFCONTINUED)
  (with-arguments-validation (who)
      ((fixnum  status))
    (capi.linux-WIFCONTINUED status)))


;;;; epoll

(define epoll-create
  (case-lambda
   (()
    (epoll-create 0))
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

(define (epoll-ctl epfd op fd event)
  (define who 'epoll-ctl)
  (with-arguments-validation (who)
      ((file-descriptor		epfd)
       (fixnum			op)
       (file-descriptor		fd)
       (pointer			event))
    (let ((rv (capi.linux-epoll-ctl epfd op fd event)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv epfd op fd event)))))

(define (epoll-wait epfd event maxevents timeout-ms)
  (define who 'epoll-wait)
  (with-arguments-validation (who)
      ((file-descriptor		epfd)
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
    uint32
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
    file-descriptor
    capi.linux-epoll-event-set-data-fd!
    capi.linux-epoll-event-ref-data-fd)

  (define-epoll-event-field
    epoll-event-set-data-u32!
    epoll-event-ref-data-u32
    uint32
    capi.linux-epoll-event-set-data-u32!
    capi.linux-epoll-event-ref-data-u32)

  (define-epoll-event-field
    epoll-event-set-data-u64!
    epoll-event-ref-data-u64
    uint64
    capi.linux-epoll-event-set-data-u64!
    capi.linux-epoll-event-ref-data-u64)
  )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'define-for-linux 'scheme-indent-function 1)
;; End:
