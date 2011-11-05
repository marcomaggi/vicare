;;;
;;;Part of: Vicare
;;;Contents: errno handling
;;;Date: Fri Nov  4, 2011
;;;
;;;Abstract
;;;
;;;	Export a function to convert an errno code as represented by the
;;;	(vicare errno) library into a string representing the errno code
;;;	symbol.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

#!r6rs
(library (ikarus errno)
  (export errno->string)
  (import (except (ikarus)
		  errno->string)
    (vicare errno)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))


(define-syntax errno-vector
  (lambda (stx)
    (define (%mk-vector)
      (let* ((max	(fold-left (lambda (max pair)
				     (let ((code (cdr pair)))
				       (cond ((not code)		max)
					     ((< max (fx- code))	(fx- code))
					     (else			max))))
			  0 errno-alist))
	     (vec.len	(fx+ 1 max))
	     ;;All the unused positions are set to #f.
	     (vec	(make-vector vec.len #f)))
	(for-each (lambda (pair)
		    (when (cdr pair)
		      (vector-set! vec (fx- (cdr pair)) (car pair))))
	  errno-alist)
	vec))

    (define errno-alist
      `((E2BIG			. ,E2BIG)
	(EACCES			. ,EACCES)
	(EADDRINUSE		. ,EADDRINUSE)
	(EADDRNOTAVAIL		. ,EADDRNOTAVAIL)
	(EADV			. ,EADV)
	(EAFNOSUPPORT		. ,EAFNOSUPPORT)
	(EAGAIN			. ,EAGAIN)
	(EALREADY		. ,EALREADY)
	(EBADE			. ,EBADE)
	(EBADF			. ,EBADF)
	(EBADFD			. ,EBADFD)
	(EBADMSG		. ,EBADMSG)
	(EBADR			. ,EBADR)
	(EBADRQC		. ,EBADRQC)
	(EBADSLT		. ,EBADSLT)
	(EBFONT			. ,EBFONT)
	(EBUSY			. ,EBUSY)
	(ECANCELED		. ,ECANCELED)
	(ECHILD			. ,ECHILD)
	(ECHRNG			. ,ECHRNG)
	(ECOMM			. ,ECOMM)
	(ECONNABORTED		. ,ECONNABORTED)
	(ECONNREFUSED		. ,ECONNREFUSED)
	(ECONNRESET		. ,ECONNRESET)
	(EDEADLK		. ,EDEADLK)
	(EDEADLOCK		. ,EDEADLOCK)
	(EDESTADDRREQ		. ,EDESTADDRREQ)
	(EDOM			. ,EDOM)
	(EDOTDOT		. ,EDOTDOT)
	(EDQUOT			. ,EDQUOT)
	(EEXIST			. ,EEXIST)
	(EFAULT			. ,EFAULT)
	(EFBIG			. ,EFBIG)
	(EHOSTDOWN		. ,EHOSTDOWN)
	(EHOSTUNREACH		. ,EHOSTUNREACH)
	(EIDRM			. ,EIDRM)
	(EILSEQ			. ,EILSEQ)
	(EINPROGRESS		. ,EINPROGRESS)
	(EINTR			. ,EINTR)
	(EINVAL			. ,EINVAL)
	(EIO			. ,EIO)
	(EISCONN		. ,EISCONN)
	(EISDIR			. ,EISDIR)
	(EISNAM			. ,EISNAM)
	(EKEYEXPIRED		. ,EKEYEXPIRED)
	(EKEYREJECTED		. ,EKEYREJECTED)
	(EKEYREVOKED		. ,EKEYREVOKED)
	(EL2HLT			. ,EL2HLT)
	(EL2NSYNC		. ,EL2NSYNC)
	(EL3HLT			. ,EL3HLT)
	(EL3RST			. ,EL3RST)
	(ELIBACC		. ,ELIBACC)
	(ELIBBAD		. ,ELIBBAD)
	(ELIBEXEC		. ,ELIBEXEC)
	(ELIBMAX		. ,ELIBMAX)
	(ELIBSCN		. ,ELIBSCN)
	(ELNRNG			. ,ELNRNG)
	(ELOOP			. ,ELOOP)
	(EMEDIUMTYPE		. ,EMEDIUMTYPE)
	(EMFILE			. ,EMFILE)
	(EMLINK			. ,EMLINK)
	(EMSGSIZE		. ,EMSGSIZE)
	(EMULTIHOP		. ,EMULTIHOP)
	(ENAMETOOLONG		. ,ENAMETOOLONG)
	(ENAVAIL		. ,ENAVAIL)
	(ENETDOWN		. ,ENETDOWN)
	(ENETRESET		. ,ENETRESET)
	(ENETUNREACH		. ,ENETUNREACH)
	(ENFILE			. ,ENFILE)
	(ENOANO			. ,ENOANO)
	(ENOBUFS		. ,ENOBUFS)
	(ENOCSI			. ,ENOCSI)
	(ENODATA		. ,ENODATA)
	(ENODEV			. ,ENODEV)
	(ENOENT			. ,ENOENT)
	(ENOEXEC		. ,ENOEXEC)
	(ENOKEY			. ,ENOKEY)
	(ENOLCK			. ,ENOLCK)
	(ENOLINK		. ,ENOLINK)
	(ENOMEDIUM		. ,ENOMEDIUM)
	(ENOMEM			. ,ENOMEM)
	(ENOMSG			. ,ENOMSG)
	(ENONET			. ,ENONET)
	(ENOPKG			. ,ENOPKG)
	(ENOPROTOOPT		. ,ENOPROTOOPT)
	(ENOSPC			. ,ENOSPC)
	(ENOSR			. ,ENOSR)
	(ENOSTR			. ,ENOSTR)
	(ENOSYS			. ,ENOSYS)
	(ENOTBLK		. ,ENOTBLK)
	(ENOTCONN		. ,ENOTCONN)
	(ENOTDIR		. ,ENOTDIR)
	(ENOTEMPTY		. ,ENOTEMPTY)
	(ENOTNAM		. ,ENOTNAM)
	(ENOTRECOVERABLE	. ,ENOTRECOVERABLE)
	(ENOTSOCK		. ,ENOTSOCK)
	(ENOTTY			. ,ENOTTY)
	(ENOTUNIQ		. ,ENOTUNIQ)
	(ENXIO			. ,ENXIO)
	(EOPNOTSUPP		. ,EOPNOTSUPP)
	(EOVERFLOW		. ,EOVERFLOW)
	(EOWNERDEAD		. ,EOWNERDEAD)
	(EPERM			. ,EPERM)
	(EPFNOSUPPORT		. ,EPFNOSUPPORT)
	(EPIPE			. ,EPIPE)
	(EPROTO			. ,EPROTO)
	(EPROTONOSUPPORT	. ,EPROTONOSUPPORT)
	(EPROTOTYPE		. ,EPROTOTYPE)
	(ERANGE			. ,ERANGE)
	(EREMCHG		. ,EREMCHG)
	(EREMOTE		. ,EREMOTE)
	(EREMOTEIO		. ,EREMOTEIO)
	(ERESTART		. ,ERESTART)
	(EROFS			. ,EROFS)
	(ESHUTDOWN		. ,ESHUTDOWN)
	(ESOCKTNOSUPPORT	. ,ESOCKTNOSUPPORT)
	(ESPIPE			. ,ESPIPE)
	(ESRCH			. ,ESRCH)
	(ESRMNT			. ,ESRMNT)
	(ESTALE			. ,ESTALE)
	(ESTRPIPE		. ,ESTRPIPE)
	(ETIME			. ,ETIME)
	(ETIMEDOUT		. ,ETIMEDOUT)
	(ETOOMANYREFS		. ,ETOOMANYREFS)
	(ETXTBSY		. ,ETXTBSY)
	(EUCLEAN		. ,EUCLEAN)
	(EUNATCH		. ,EUNATCH)
	(EUSERS			. ,EUSERS)
	(EWOULDBLOCK		. ,EWOULDBLOCK)
	(EXDEV			. ,EXDEV)
	(EXFULL			. ,EXFULL)))

    (syntax-case stx ()
      ((?ctx)
       #`(quote #,(datum->syntax #'?ctx (%mk-vector)))))))


(define ERRNO-VECTOR (errno-vector))

(define (errno->string negated-errno-code)
  ;;Defined  by Vicare.   Convert an  errno code  as represented  by the
  ;;(vicare  errno) library into  a string  representing the  errno code
  ;;symbol.
  ;;
  (define who 'errno->string)
  (with-arguments-validation (who)
      ((fixnum negated-errno-code))
    (let ((errno-code (unsafe.fx- 0 negated-errno-code)))
      (and (unsafe.fx> errno-code 0)
	   (unsafe.fx< errno-code (vector-length ERRNO-VECTOR))
	   (vector-ref ERRNO-VECTOR errno-code)))))


;;;; done

)

;;; end of file
