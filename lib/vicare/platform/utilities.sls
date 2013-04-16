;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: utility functions related to hosting platform features
;;;Date: Tue Oct  2, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare platform utilities)
  (export
    errno-code->symbol
    posix-signal->symbol)
  (import (ikarus)
    (only (ikarus system $fx)
	  $fx=)
    (vicare platform constants))


;;;; helpers

(define-syntax case-fixnums
  (syntax-rules (else)
    ((_ ?expr
	((?fixnum0 ?fixnum ...)
	 ?fx-body0 ?fx-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((fx ?expr))
       (cond ((or ($fx= ?fixnum0 fx)
		  ($fx= ?fixnum  fx)
		  ...)
	      ?fx-body0 ?fx-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?fixnum0 ?fixnum ...)
	 ?fx-body0 ?fx-body ...)
	...)
     (let ((fx ?expr))
       (cond ((or ($fx= ?fixnum0 fx)
		  ($fx= ?fixnum  fx)
		  ...)
	      ?fx-body0 ?fx-body ...)
	     ...)))
    ))

(define-syntax case-integers/false
  (syntax-rules (else)
    ((_ ?expr
	((?integer0 ?integer ...)
	 ?body0 ?body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((int ?expr))
       (cond ((or (and ?integer0 (= ?integer0 int))
		  (and ?integer  (= ?integer  int))
		  ...)
	      ?body0 ?body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?integer0 ?integer ...)
	 ?body0 ?body ...)
	...)
     (let ((int ?expr))
       (cond ((or (and ?integer0 (= ?integer0 int))
		  (and ?integer  (= ?integer  int))
		  ...)
	      ?body0 ?body ...)
	     ...)))
    ))

;;; --------------------------------------------------------------------

(define-syntax define-fixnum->symbol-function
  (syntax-rules ()
    ((_ ?who (?code ...))
     (define (?who code)
       (define who '?who)
       (assert (fixnum? code))
       (case-fixnums code
	 ((?code)	'?code)
	 ...
	 (else #f))))))

(define-syntax define-integer->symbol-function
  (syntax-rules ()
    ((_ ?who (?code ...))
     (define (?who code)
       (define who '?who)
       (assert (integer? code))
       (assert (exact?   code))
       (case-integers/false code
	 ((?code)	'?code)
	 ...
	 (else #f))))))


;;;; code to symbol conversion

(define-fixnum->symbol-function errno-code->symbol
  (E2BIG		EACCES		EADDRINUSE
   EADDRNOTAVAIL	EADV		EAFNOSUPPORT
   EAGAIN		EALREADY	EBADE
   EBADF		EBADFD		EBADMSG
   EBADR		EBADRQC		EBADSLT
   EBFONT		EBUSY		ECANCELED
   ECHILD		ECHRNG		ECOMM
   ECONNABORTED		ECONNREFUSED	ECONNRESET
   EDEADLK		EDEADLOCK	EDESTADDRREQ
   EDOM			EDOTDOT		EDQUOT
   EEXIST		EFAULT		EFBIG
   EHOSTDOWN		EHOSTUNREACH	EIDRM
   EILSEQ		EINPROGRESS	EINTR
   EINVAL		EIO		EISCONN
   EISDIR		EISNAM		EKEYEXPIRED
   EKEYREJECTED		EKEYREVOKED	EL2HLT
   EL2NSYNC		EL3HLT		EL3RST
   ELIBACC		ELIBBAD		ELIBEXEC
   ELIBMAX		ELIBSCN		ELNRNG
   ELOOP		EMEDIUMTYPE	EMFILE
   EMLINK		EMSGSIZE	EMULTIHOP
   ENAMETOOLONG		ENAVAIL		ENETDOWN
   ENETRESET		ENETUNREACH	ENFILE
   ENOANO		ENOBUFS		ENOCSI
   ENODATA		ENODEV		ENOENT
   ENOEXEC		ENOKEY		ENOLCK
   ENOLINK		ENOMEDIUM	ENOMEM
   ENOMSG		ENONET		ENOPKG
   ENOPROTOOPT		ENOSPC		ENOSR
   ENOSTR		ENOSYS		ENOTBLK
   ENOTCONN		ENOTDIR		ENOTEMPTY
   ENOTNAM		ENOTRECOVERABLE	ENOTSOCK
   ENOTTY		ENOTUNIQ	ENXIO
   EOPNOTSUPP		EOVERFLOW	EOWNERDEAD
   EPERM		EPFNOSUPPORT	EPIPE
   EPROTO		EPROTONOSUPPORT	EPROTOTYPE
   ERANGE		EREMCHG		EREMOTE
   EREMOTEIO		ERESTART	EROFS
   ESHUTDOWN		ESOCKTNOSUPPORT	ESPIPE
   ESRCH		ESRMNT		ESTALE
   ESTRPIPE		ETIME		ETIMEDOUT
   ETOOMANYREFS		ETXTBSY		EUCLEAN
   EUNATCH		EUSERS		EWOULDBLOCK
   EXDEV		EXFULL))

(define-integer->symbol-function posix-signal->symbol
  (SIGFPE		SIGILL		SIGSEGV
   SIGBUS		SIGABRT		SIGIOT
   SIGTRAP		SIGEMT		SIGSYS
   SIGTERM		SIGINT		SIGQUIT
   SIGKILL		SIGHUP
   SIGALRM		SIGVRALRM	SIGPROF
   SIGIO		SIGURG		SIGPOLL
   SIGCHLD		SIGCLD		SIGCONT
   SIGSTOP		SIGTSTP		SIGTTIN
   SIGTTOU
   SIGPIPE		SIGLOST		SIGXCPU
   SIGXSFZ
   SIGUSR1		SIGUSR2		SIGWINCH
   SIGINFO))


;;;; done

)

;;; end of file
