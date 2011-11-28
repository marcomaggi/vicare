;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2008, 2009  Abdulaziz Ghuloum
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (vicare ffi)
  (export
    ;; pointer values
    pointer?
    null-pointer			pointer-null?
    pointer->integer			integer->pointer
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

    ;; shared libraries inteface
    dlopen				dlclose
    dlsym				dlerror

    ;; calling functions and callbacks
    make-c-callout			make-c-callback

    ;; raw memory allocation
    malloc				free
    memcpy

    ;; errno interface
    errno
    case-errno				errno-code
    &errno				make-errno-condition
    errno-condition?			condition-errno

    ;; memory accessors and mutators
    pointer-c-ref-uint8			pointer-c-ref-sint8
    pointer-c-ref-uint16		pointer-c-ref-sint16
    pointer-c-ref-uint32		pointer-c-ref-sint32
    pointer-c-ref-uint64		pointer-c-ref-sint64

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long

    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    pointer-c-set-uint8!		pointer-c-set-sint8!
    pointer-c-set-uint16!		pointer-c-set-sint16!
    pointer-c-set-uint32!		pointer-c-set-sint32!
    pointer-c-set-uint64!		pointer-c-set-sint64!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!

    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-pointer!)
  (import (vicare)
    (ikarus system $foreign)
    (vicare platform-constants)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; errno interface

(define-syntax errno-code
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?code)
       (identifier? #'?code)
       (let ((sym (syntax->datum #'?code)))
	 (define them
	   '(E2BIG		EACCES		EADDRINUSE
	     EADDRNOTAVAIL	EADV		EAFNOSUPPORT
	     EAGAIN		EALREADY	EBADE
	     EBADF		EBADFD		EBADMSG
	     EBADR		EBADRQC		EBADSLT
	     EBFONT		EBUSY		ECANCELED
	     ECHILD		ECHRNG		ECOMM
	     ECONNABORTED	ECONNREFUSED	ECONNRESET
	     EDEADLK		EDEADLOCK	EDESTADDRREQ
	     EDOM		EDOTDOT		EDQUOT
	     EEXIST		EFAULT		EFBIG
	     EHOSTDOWN		EHOSTUNREACH	EIDRM
	     EILSEQ		EINPROGRESS	EINTR
	     EINVAL		EIO		EISCONN
	     EISDIR		EISNAM		EKEYEXPIRED
	     EKEYREJECTED	EKEYREVOKED	EL2HLT
	     EL2NSYNC		EL3HLT		EL3RST
	     ELIBACC		ELIBBAD		ELIBEXEC
	     ELIBMAX		ELIBSCN		ELNRNG
	     ELOOP		EMEDIUMTYPE	EMFILE
	     EMLINK		EMSGSIZE	EMULTIHOP
	     ENAMETOOLONG	ENAVAIL		ENETDOWN
	     ENETRESET		ENETUNREACH	ENFILE
	     ENOANO		ENOBUFS		ENOCSI
	     ENODATA		ENODEV		ENOENT
	     ENOEXEC		ENOKEY		ENOLCK
	     ENOLINK		ENOMEDIUM	ENOMEM
	     ENOMSG		ENONET		ENOPKG
	     ENOPROTOOPT	ENOSPC		ENOSR
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
	     ETOOMANYREFS	ETXTBSY		EUCLEAN
	     EUNATCH		EUSERS		EWOULDBLOCK
	     EXDEV		EXFULL))
	 (if (memq sym them)
	     #'?code
	   (syntax-violation 'errno-code "invalid symbol as errno code" (syntax->datum stx) sym)))))))

(define-syntax case-errno
  (lambda (stx)
    (define who 'case-errno)
    (define them
      ;;I know that it is really  ugly to duplicate this list, but it is
      ;;also  much simpler  than  creating an  external  library for  it
      ;;(Marco Maggi; Nov 28, 2011).
      '( ;;
	E2BIG		EACCES		EADDRINUSE
	EADDRNOTAVAIL	EADV		EAFNOSUPPORT
	EAGAIN		EALREADY	EBADE
	EBADF		EBADFD		EBADMSG
	EBADR		EBADRQC		EBADSLT
	EBFONT		EBUSY		ECANCELED
	ECHILD		ECHRNG		ECOMM
	ECONNABORTED	ECONNREFUSED	ECONNRESET
	EDEADLK		EDEADLOCK	EDESTADDRREQ
	EDOM		EDOTDOT		EDQUOT
	EEXIST		EFAULT		EFBIG
	EHOSTDOWN	EHOSTUNREACH	EIDRM
	EILSEQ		EINPROGRESS	EINTR
	EINVAL		EIO		EISCONN
	EISDIR		EISNAM		EKEYEXPIRED
	EKEYREJECTED	EKEYREVOKED	EL2HLT
	EL2NSYNC	EL3HLT		EL3RST
	ELIBACC		ELIBBAD		ELIBEXEC
	ELIBMAX		ELIBSCN		ELNRNG
	ELOOP		EMEDIUMTYPE	EMFILE
	EMLINK		EMSGSIZE	EMULTIHOP
	ENAMETOOLONG	ENAVAIL		ENETDOWN
	ENETRESET	ENETUNREACH	ENFILE
	ENOANO		ENOBUFS		ENOCSI
	ENODATA		ENODEV		ENOENT
	ENOEXEC		ENOKEY		ENOLCK
	ENOLINK		ENOMEDIUM	ENOMEM
	ENOMSG		ENONET		ENOPKG
	ENOPROTOOPT	ENOSPC		ENOSR
	ENOSTR		ENOSYS		ENOTBLK
	ENOTCONN	ENOTDIR		ENOTEMPTY
	ENOTNAM		ENOTRECOVERABLE	ENOTSOCK
	ENOTTY		ENOTUNIQ	ENXIO
	EOPNOTSUPP	EOVERFLOW	EOWNERDEAD
	EPERM		EPFNOSUPPORT	EPIPE
	EPROTO		EPROTONOSUPPORT	EPROTOTYPE
	ERANGE		EREMCHG		EREMOTE
	EREMOTEIO	ERESTART	EROFS
	ESHUTDOWN	ESOCKTNOSUPPORT	ESPIPE
	ESRCH		ESRMNT		ESTALE
	ESTRPIPE	ETIME		ETIMEDOUT
	ETOOMANYREFS	ETXTBSY		EUCLEAN
	EUNATCH		EUSERS		EWOULDBLOCK
	EXDEV		EXFULL))
    (define (%codes->bindings x)
      (let ((context #'EPERM))
	(map (lambda (list-of-symbols)
	       (map (lambda (symbol)
		      (cond ((not (symbol? symbol))
			     (syntax-violation who "expected symbol as symbolic errno code" symbol))
			    ((memq symbol them)
			     (datum->syntax context symbol))
			    (else
			     (syntax-violation who "unknown symbolic errno code" symbol))))
		 list-of-symbols))
	  (syntax->datum x))))
    (syntax-case stx (else)
      ((_ ?errno ((?code0 ?code ...) . ?body) ... (else . ?else-body))
       (with-syntax ((((CODE0 CODE ...) ...) (%codes->bindings #'((?code0 ?code ...) ...))))
	 #'(let ((errno ?errno))
	     (cond ((or (and (fixnum? CODE0) (unsafe.fx= errno CODE0))
			(and (fixnum? CODE)  (unsafe.fx= errno CODE))
			...)
		    . ?body)
		   ...
		   (else . ?else-body)))))
      ((_ ?errno ((?code0 ?code ...) . ?body) ...)
       (with-syntax ((((CODE0 CODE ...) ...) (%codes->bindings #'((?code0 ?code ...) ...))))
	 #'(let ((errno ?errno))
	     (cond ((or (and (fixnum? CODE0) (unsafe.fx= errno CODE0))
			(and (fixnum? CODE)  (unsafe.fx= errno CODE))
			...)
		    . ?body)
		   ...
		   (else
		    (assertion-violation #f "unknown errno code" errno))))))
      )))


;;;; done

)

;;; end of file
