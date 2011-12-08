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
    make-c-callout-maker		make-c-callout-maker/with-errno
    make-c-callback-maker		free-c-callback
    with-local-storage

    ;; raw memory allocation
    malloc				guarded-malloc
    realloc				guarded-realloc
    calloc				guarded-calloc
    free				memcmp
    memcpy				memmove
    memset				memory-copy
    memory->bytevector			bytevector->memory
    bytevector->guarded-memory
    bytevectors->argv			argv->bytevectors
    strings->argv			argv->strings
    bytevectors->guarded-argv
    strings->guarded-argv
    argv-length

    ;; C strings
    strlen
    strcmp				strncmp
    strdup				strndup
    guarded-strdup			guarded-strndup
    cstring->bytevector
    bytevector->cstring			bytevector->guarded-cstring
    cstring->string
    string->cstring			string->guarded-cstring

    ;; errno interface
    errno
    case-errno				errno-code
    &errno				make-errno-condition
    errno-condition?			condition-errno
    strerror

    ;; memory accessors and mutators
    pointer-ref-c-uint8			pointer-ref-c-sint8
    pointer-ref-c-uint16		pointer-ref-c-sint16
    pointer-ref-c-uint32		pointer-ref-c-sint32
    pointer-ref-c-uint64		pointer-ref-c-sint64

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long

    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    pointer-set-c-uint8!		pointer-set-c-sint8!
    pointer-set-c-uint16!		pointer-set-c-sint16!
    pointer-set-c-uint32!		pointer-set-c-sint32!
    pointer-set-c-uint64!		pointer-set-c-sint64!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!

    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-pointer!)
  (import (vicare)
    (ikarus system $foreign)
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; raw memory allocation

(define %memory-guardian
  #;(case-lambda
   ((obj)
    obj)
   (()
    #f))
  (make-guardian))

(define (%free-allocated-memory)
  (do ((pointer (%memory-guardian) (%memory-guardian)))
      ((not pointer))
    (unless (pointer-null? pointer)
      (free pointer))))

(define (guarded-malloc number-of-bytes)
  (let ((rv (malloc number-of-bytes)))
    (and rv (%memory-guardian rv))))

(define (guarded-realloc pointer number-of-bytes)
  (let ((rv (realloc pointer number-of-bytes)))
    (and rv (begin
	      (set-pointer-null! pointer)
	      (%memory-guardian rv)))))

(define (guarded-calloc number-of-elements element-size)
  (let ((rv (calloc number-of-elements element-size)))
    (and rv (%memory-guardian rv))))

(define (bytevector->guarded-memory bv)
  (let-values (((ptr len) (bytevector->memory bv)))
    (if ptr
	(values (%memory-guardian ptr) len)
      (values #f #f))))


;;;; C strings

(define (guarded-strdup pointer)
  (let ((rv (strdup pointer)))
    (and rv (%memory-guardian rv))))

(define (guarded-strndup pointer count)
  (let ((rv (strndup pointer count)))
    (and rv (%memory-guardian rv))))

(define (bytevector->guarded-cstring bv)
  (let ((rv (bytevector->cstring bv)))
    (and rv (%memory-guardian rv))))

(define (string->guarded-cstring str)
  (let ((rv (bytevector->cstring (string->latin1 str))))
    (and rv (%memory-guardian rv))))

(define (bytevectors->guarded-argv bvs)
  (let ((rv (bytevectors->argv bvs)))
    (and rv (%memory-guardian rv))))

(define (strings->guarded-argv bvs)
  (let ((rv (strings->argv bvs)))
    (and rv (%memory-guardian rv))))


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
	     (datum->syntax #'%memory-guardian sym)
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

(post-gc-hooks (cons %free-allocated-memory (post-gc-hooks)))

)

;;; end of file
