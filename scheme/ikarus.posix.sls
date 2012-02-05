;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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


(library (ikarus.posix)
  (export
    strerror
    errno->string
    file-exists?
    delete-file
    mkdir
    mkdir/parents
    getenv
    split-file-name
    real-pathname
    file-modification-time)
  (import (except (ikarus)
		  file-exists?
		  delete-file
		  strerror
		  getenv)
    (rename (only (ikarus system $pointers)
		  $pointer?)
	    ($pointer? pointer?))
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (vicare words))


;;;; arguments validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (pathname who obj)
  (or (bytevector? obj) (string? obj))
  (assertion-violation who "expected string or bytevector as pathname argument" obj))

(define-argument-validation (boolean/fixnum who obj)
  (or (fixnum? obj) (boolean? obj))
  (assertion-violation who "expected boolean or fixnum as argument" obj))


;;;; errors handling

(define (strerror errno)
  (define who 'strerror)
  (with-arguments-validation (who)
      ((boolean/fixnum  errno))
    (if errno
	(if (boolean? errno)
	    "unknown errno code (#t)"
	  (let ((msg (capi.posix-strerror errno)))
	    (if msg
		(string-append (errno->string errno) ": " (utf8->string msg))
	      (string-append "unknown errno code " (number->string (- errno))))))
      "no error")))

(define (%raise-errno-error/filename who errno filename . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-i/o-filename-error filename)
	  (make-irritants-condition irritants))))


;;;; errno handling

(define (errno->string negated-errno-code)
  ;;Convert   an   errno   code    as   represented   by   the   (vicare
  ;;platform-constants)  library into  a string  representing  the errno
  ;;code symbol.
  ;;
  (define who 'errno->string)
  (with-arguments-validation (who)
      ((fixnum negated-errno-code))
    (let ((errno-code (unsafe.fx- 0 negated-errno-code)))
      (and (unsafe.fx> errno-code 0)
	   (unsafe.fx< errno-code (vector-length ERRNO-VECTOR))
	   (vector-ref ERRNO-VECTOR errno-code)))))

(let-syntax
    ((make-errno-vector
      (lambda (stx)
	(define (%mk-vector)
	  (let* ((max	(fold-left (lambda (max pair)
				     (let ((code (cdr pair)))
				       (cond ((not code)
					      max)
					     ((< max (fx- code))
					      (fx- code))
					     (else
					      max))))
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
	  `(("E2BIG"		. ,E2BIG)
	    ("EACCES"		. ,EACCES)
	    ("EADDRINUSE"	. ,EADDRINUSE)
	    ("EADDRNOTAVAIL"	. ,EADDRNOTAVAIL)
	    ("EADV"		. ,EADV)
	    ("EAFNOSUPPORT"	. ,EAFNOSUPPORT)
	    ("EAGAIN"		. ,EAGAIN)
	    ("EALREADY"		. ,EALREADY)
	    ("EBADE"		. ,EBADE)
	    ("EBADF"		. ,EBADF)
	    ("EBADFD"		. ,EBADFD)
	    ("EBADMSG"		. ,EBADMSG)
	    ("EBADR"		. ,EBADR)
	    ("EBADRQC"		. ,EBADRQC)
	    ("EBADSLT"		. ,EBADSLT)
	    ("EBFONT"		. ,EBFONT)
	    ("EBUSY"		. ,EBUSY)
	    ("ECANCELED"	. ,ECANCELED)
	    ("ECHILD"		. ,ECHILD)
	    ("ECHRNG"		. ,ECHRNG)
	    ("ECOMM"		. ,ECOMM)
	    ("ECONNABORTED"	. ,ECONNABORTED)
	    ("ECONNREFUSED"	. ,ECONNREFUSED)
	    ("ECONNRESET"	. ,ECONNRESET)
	    ("EDEADLK"		. ,EDEADLK)
	    ("EDEADLOCK"	. ,EDEADLOCK)
	    ("EDESTADDRREQ"	. ,EDESTADDRREQ)
	    ("EDOM"		. ,EDOM)
	    ("EDOTDOT"		. ,EDOTDOT)
	    ("EDQUOT"		. ,EDQUOT)
	    ("EEXIST"		. ,EEXIST)
	    ("EFAULT"		. ,EFAULT)
	    ("EFBIG"		. ,EFBIG)
	    ("EHOSTDOWN"	. ,EHOSTDOWN)
	    ("EHOSTUNREACH"	. ,EHOSTUNREACH)
	    ("EIDRM"		. ,EIDRM)
	    ("EILSEQ"		. ,EILSEQ)
	    ("EINPROGRESS"	. ,EINPROGRESS)
	    ("EINTR"		. ,EINTR)
	    ("EINVAL"		. ,EINVAL)
	    ("EIO"		. ,EIO)
	    ("EISCONN"		. ,EISCONN)
	    ("EISDIR"		. ,EISDIR)
	    ("EISNAM"		. ,EISNAM)
	    ("EKEYEXPIRED"	. ,EKEYEXPIRED)
	    ("EKEYREJECTED"	. ,EKEYREJECTED)
	    ("EKEYREVOKED"	. ,EKEYREVOKED)
	    ("EL2HLT"		. ,EL2HLT)
	    ("EL2NSYNC"		. ,EL2NSYNC)
	    ("EL3HLT"		. ,EL3HLT)
	    ("EL3RST"		. ,EL3RST)
	    ("ELIBACC"		. ,ELIBACC)
	    ("ELIBBAD"		. ,ELIBBAD)
	    ("ELIBEXEC"		. ,ELIBEXEC)
	    ("ELIBMAX"		. ,ELIBMAX)
	    ("ELIBSCN"		. ,ELIBSCN)
	    ("ELNRNG"		. ,ELNRNG)
	    ("ELOOP"		. ,ELOOP)
	    ("EMEDIUMTYPE"	. ,EMEDIUMTYPE)
	    ("EMFILE"		. ,EMFILE)
	    ("EMLINK"		. ,EMLINK)
	    ("EMSGSIZE"		. ,EMSGSIZE)
	    ("EMULTIHOP"	. ,EMULTIHOP)
	    ("ENAMETOOLONG"	. ,ENAMETOOLONG)
	    ("ENAVAIL"		. ,ENAVAIL)
	    ("ENETDOWN"		. ,ENETDOWN)
	    ("ENETRESET"	. ,ENETRESET)
	    ("ENETUNREACH"	. ,ENETUNREACH)
	    ("ENFILE"		. ,ENFILE)
	    ("ENOANO"		. ,ENOANO)
	    ("ENOBUFS"		. ,ENOBUFS)
	    ("ENOCSI"		. ,ENOCSI)
	    ("ENODATA"		. ,ENODATA)
	    ("ENODEV"		. ,ENODEV)
	    ("ENOENT"		. ,ENOENT)
	    ("ENOEXEC"		. ,ENOEXEC)
	    ("ENOKEY"		. ,ENOKEY)
	    ("ENOLCK"		. ,ENOLCK)
	    ("ENOLINK"		. ,ENOLINK)
	    ("ENOMEDIUM"	. ,ENOMEDIUM)
	    ("ENOMEM"		. ,ENOMEM)
	    ("ENOMSG"		. ,ENOMSG)
	    ("ENONET"		. ,ENONET)
	    ("ENOPKG"		. ,ENOPKG)
	    ("ENOPROTOOPT"	. ,ENOPROTOOPT)
	    ("ENOSPC"		. ,ENOSPC)
	    ("ENOSR"		. ,ENOSR)
	    ("ENOSTR"		. ,ENOSTR)
	    ("ENOSYS"		. ,ENOSYS)
	    ("ENOTBLK"		. ,ENOTBLK)
	    ("ENOTCONN"		. ,ENOTCONN)
	    ("ENOTDIR"		. ,ENOTDIR)
	    ("ENOTEMPTY"	. ,ENOTEMPTY)
	    ("ENOTNAM"		. ,ENOTNAM)
	    ("ENOTRECOVERABLE"	. ,ENOTRECOVERABLE)
	    ("ENOTSOCK"		. ,ENOTSOCK)
	    ("ENOTTY"		. ,ENOTTY)
	    ("ENOTUNIQ"		. ,ENOTUNIQ)
	    ("ENXIO"		. ,ENXIO)
	    ("EOPNOTSUPP"	. ,EOPNOTSUPP)
	    ("EOVERFLOW"	. ,EOVERFLOW)
	    ("EOWNERDEAD"	. ,EOWNERDEAD)
	    ("EPERM"		. ,EPERM)
	    ("EPFNOSUPPORT"	. ,EPFNOSUPPORT)
	    ("EPIPE"		. ,EPIPE)
	    ("EPROTO"		. ,EPROTO)
	    ("EPROTONOSUPPORT"	. ,EPROTONOSUPPORT)
	    ("EPROTOTYPE"	. ,EPROTOTYPE)
	    ("ERANGE"		. ,ERANGE)
	    ("EREMCHG"		. ,EREMCHG)
	    ("EREMOTE"		. ,EREMOTE)
	    ("EREMOTEIO"	. ,EREMOTEIO)
	    ("ERESTART"		. ,ERESTART)
	    ("EROFS"		. ,EROFS)
	    ("ESHUTDOWN"	. ,ESHUTDOWN)
	    ("ESOCKTNOSUPPORT"	. ,ESOCKTNOSUPPORT)
	    ("ESPIPE"		. ,ESPIPE)
	    ("ESRCH"		. ,ESRCH)
	    ("ESRMNT"		. ,ESRMNT)
	    ("ESTALE"		. ,ESTALE)
	    ("ESTRPIPE"		. ,ESTRPIPE)
	    ("ETIME"		. ,ETIME)
	    ("ETIMEDOUT"	. ,ETIMEDOUT)
	    ("ETOOMANYREFS"	. ,ETOOMANYREFS)
	    ("ETXTBSY"		. ,ETXTBSY)
	    ("EUCLEAN"		. ,EUCLEAN)
	    ("EUNATCH"		. ,EUNATCH)
	    ("EUSERS"		. ,EUSERS)
	    ("EWOULDBLOCK"	. ,EWOULDBLOCK)
	    ("EXDEV"		. ,EXDEV)
	    ("EXFULL"		. ,EXFULL)))
	(syntax-case stx ()
	  ((?ctx)
	   #`(quote #,(datum->syntax #'?ctx (%mk-vector))))))))

  (define ERRNO-VECTOR (make-errno-vector)))


;;;; creating directories

(define (mkdir pathname mode)
  (define who 'mkdir)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (fixnum	  mode))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-mkdir pathname.bv mode)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname mode))))))

(define (mkdir/parents pathname mode)
  (define who 'mkdir/parents)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (fixnum	  mode))
    (let next-component ((pathname pathname))
      (if (file-exists? pathname)
	  (unless (%file-is-directory? who pathname)
	    (error who "path component is not a directory" pathname))
	(let-values (((base suffix) (split-file-name pathname)))
	  (unless (unsafe.fxzero? (unsafe.string-length base))
	    (next-component base))
	  (unless (unsafe.fxzero? (unsafe.string-length suffix))
	    (mkdir pathname mode)))))))

(define (%file-is-directory? who pathname)
  (with-pathnames ((pathname.bv pathname))
    (let ((rv (capi.posix-file-is-directory? pathname.bv #f)))
      (if (boolean? rv)
	  rv
	(%raise-errno-error/filename who rv pathname)))))

(define (real-pathname pathname)
  (define who 'real-pathname)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-realpath pathname.bv)))
	(if (bytevector? rv)
	    ((filename->string-func) rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (getenv key)
  (define who 'getenv)
  (with-arguments-validation (who)
      ((string  key))
    (let ((rv (capi.posix-getenv (string->utf8 key))))
      (and rv (utf8->string rv)))))

(define (file-exists? pathname)
  ;;Defined by R6RS.
  ;;
  (define who 'file-exists?)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-file-exists? pathname.bv)))
	(if (boolean? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname))))))

(define (delete-file pathname)
  ;;Defined by R6RS.
  ;;
  (define who 'delete-file)
  (with-arguments-validation (who)
      ((pathname pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-unlink pathname.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (split-file-name str)
  (define who 'split-file-name)
  (define path-sep #\/)
  (define (find-last c str)
    (let f ((i (string-length str)))
      (if (fx=? i 0)
	  #f
	(let ((i (fx- i 1)))
	  (if (char=? (string-ref str i) c)
	      i
	    (f i))))))
  (with-arguments-validation (who)
      ((string  str))
    (cond ((find-last path-sep str)
	   => (lambda (i)
		(values
		 (substring str 0 i)
		 (let ((i (fx+ i 1)))
		   (substring str i (string-length str) )))))
	  (else
	   (values "" str)))))

(define (file-modification-time pathname)
  (define who 'file-modification-time)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv  pathname))
      (let* ((timespec (unsafe.make-clean-vector 2))
	     (rv       (capi.posix-file-mtime pathname.bv timespec)))
	(if (unsafe.fxzero? rv)
	    (+ (* #e1e9 (unsafe.vector-ref timespec 0))
	       (unsafe.vector-ref timespec 1))
	  (%raise-errno-error/filename who rv pathname))))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-pathnames 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors/or-false 'scheme-indent-function 1)
;; End:
