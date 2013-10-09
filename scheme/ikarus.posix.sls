;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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

    ;; errno handling
    strerror
    errno->string

    ;; file operations
    file-exists?
    delete-file
    real-pathname

    ;; file predicates
    file-pathname?
    file-string-pathname?
    file-bytevector-pathname?
    file-absolute-pathname?
    file-relative-pathname?
    file-colon-search-path?
    file-string-colon-search-path?
    file-bytevector-colon-search-path?

    ;; file attributes
    file-modification-time

    ;; string pathnames
    split-pathname-root-and-tail
    search-file-in-environment-path
    search-file-in-list-path
    split-pathname
    split-pathname-bytevector
    split-pathname-string
    split-search-path
    split-search-path-bytevector
    split-search-path-string

    ;; creating directories
    mkdir
    mkdir/parents

    ;; system environment variables
    getenv
    environ

    ;; program name
    vicare-argv0
    vicare-argv0-string)
  (import (except (ikarus)
		  ;; errno handling
		  strerror
		  errno->string

		  ;; file operations
		  file-exists?
		  delete-file
		  real-pathname

		  ;; file predicates
		  file-pathname?
		  file-string-pathname?
		  file-bytevector-pathname?
		  file-absolute-pathname?
		  file-relative-pathname?
		  file-colon-search-path?
		  file-string-colon-search-path?
		  file-bytevector-colon-search-path?

		  ;; file attributes
		  file-modification-time

		  ;; string pathnames
		  split-pathname-root-and-tail
		  search-file-in-environment-path
		  search-file-in-list-path
		  split-pathname
		  split-pathname-bytevector
		  split-pathname-string
		  split-search-path
		  split-search-path-bytevector
		  split-search-path-string

		  ;; creating directories
		  mkdir
		  mkdir/parents

		  ;; system environment variables
		  getenv
		  environ

		  ;; program name
		  vicare-argv0
		  vicare-argv0-string)
    (vicare platform constants)
    (prefix (vicare unsafe capi)
	    capi.)
    (vicare unsafe operations)
    (vicare language-extensions syntaxes)
    (except (vicare arguments validation)
	    ;;FIXME  This except  clause must  be removed  at the  next boot
	    ;;image rotation.  (Marco Maggi; Wed Oct 9, 2013)
	    file-pathname.vicare-arguments-validation
	    file-string-pathname.vicare-arguments-validation
	    file-bytevector-pathname.vicare-arguments-validation
	    file-colon-search-path.vicare-arguments-validation
	    file-string-colon-search-path.vicare-arguments-validation
	    file-bytevector-colon-search-path.vicare-arguments-validation))


;;;; arguments validation

(define-argument-validation (boolean/fixnum who obj)
  (or (fixnum? obj) (boolean? obj))
  (procedure-argument-violation who "expected boolean or fixnum as argument" obj))

;;; --------------------------------------------------------------------

;;;FIXME The following pathname validation clauses are duplicated in the
;;;library (vicare  arguments validation); they  must be removed  at the
;;;next boot image rotation.  (Marco Maggi; Wed Oct 9, 2013)

(define-argument-validation (file-pathname who obj)
  (file-pathname? obj)
  (procedure-argument-violation who "expected string or bytevector as pathname argument" obj))

(define-argument-validation (file-string-pathname who obj)
  (file-string-pathname? obj)
  (procedure-argument-violation who "expected file string pathname as argument" obj))

(define-argument-validation (file-bytevector-pathname who obj)
  (file-bytevector-pathname? obj)
  (procedure-argument-violation who "expected file bytevector pathname as argument" obj))

;;; --------------------------------------------------------------------

;;;FIXME The  following path  validation clauses  are duplicated  in the
;;;library (vicare  arguments validation); they  must be removed  at the
;;;next boot image rotation.  (Marco Maggi; Wed Oct 9, 2013)

(define-argument-validation (file-colon-search-path who obj)
  (file-colon-search-path? obj)
  (procedure-argument-violation who
    "expected valid string or bytevector as colon-separated file search path argument" obj))

(define-argument-validation (file-string-colon-search-path who obj)
  (file-string-colon-search-path? obj)
  (procedure-argument-violation who
    "expected valid string as colon-separated file search path argument" obj))

(define-argument-validation (file-bytevector-colon-search-path who obj)
  (file-bytevector-colon-search-path? obj)
  (procedure-argument-violation who
    "expected valid bytevector as colon-separated file search path argument" obj))


;;;; helpers

(define-inline-constant ASCII-COLON-FX
  58 #;(char->integer #\:))

(define-inline-constant ASCII-SLASH-FX
  47 #;(char->integer #\/))


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
  ;;platform constants)  library into  a string  representing  the errno
  ;;code symbol.
  ;;
  (define who 'errno->string)
  (with-arguments-validation (who)
      ((fixnum negated-errno-code))
    (let ((errno-code ($fx- 0 negated-errno-code)))
      (and ($fx> errno-code 0)
	   ($fx< errno-code (vector-length ERRNO-VECTOR))
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


;;;; system environment variables

(define (getenv key)
  (define who 'getenv)
  (with-arguments-validation (who)
      ((string  key))
    (let ((rv (capi.posix-getenv (string->utf8 key))))
      (and rv (utf8->string rv)))))

(define (environ)
  (define (%find-index-of-= str idx str.len)
    ;;Scan STR starint  at index IDX and up to  STR.LEN for the position
    ;;of the character #\=.  Return the index or STR.LEN.
    ;;
    (cond (($fx= idx str.len)
	   idx)
	  (($char= #\= ($string-ref str idx))
	   idx)
	  (else
	   (%find-index-of-= str ($fxadd1 idx) str.len))))
  (map (lambda (bv)
	 (let* ((str     (utf8->string bv))
		(str.len ($string-length str))
		(idx     (%find-index-of-= str 0 str.len)))
	   (cons (substring str 0 idx)
		 (if ($fx< ($fxadd1 idx) str.len)
		     (substring str ($fxadd1 idx) str.len)
		   ""))))
    (capi.posix-environ)))


;;;; creating directories

(define (mkdir pathname mode)
  (define who 'mkdir)
  (with-arguments-validation (who)
      ((file-pathname	pathname)
       (fixnum		mode))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-mkdir pathname.bv mode)))
	(unless ($fxzero? rv)
	  (%raise-errno-error/filename who rv pathname mode))))))

(module (mkdir/parents)
  (define (mkdir/parents pathname mode)
    (define who 'mkdir/parents)
    (with-arguments-validation (who)
	((file-pathname		pathname)
	 (fixnum		mode))
      (let next-component ((pathname pathname))
	(if (file-exists? pathname)
	    (unless (%file-is-directory? who pathname)
	      (error who "path component is not a directory" pathname))
	  (let-values (((base suffix) (split-pathname-root-and-tail pathname)))
	    (unless ($fxzero? ($string-length base))
	      (next-component base))
	    (unless ($fxzero? ($string-length suffix))
	      (mkdir pathname mode)))))))

  (define (%file-is-directory? who pathname)
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-file-is-directory? pathname.bv #f)))
	(if (boolean? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname)))))

  #| end of module |# )


;;;; file predicates

(define (file-exists? pathname)
  ;;Defined by R6RS.
  ;;
  (define who 'file-exists?)
  (with-arguments-validation (who)
      ((file-pathname	pathname))
    ($file-exists? pathname)))

(define ($file-exists? pathname)
  (define who 'file-exists?)
  (with-pathnames ((pathname.bv pathname))
    (let ((rv (capi.posix-file-exists? pathname.bv)))
      (if (boolean? rv)
	  rv
	(%raise-errno-error/filename who rv pathname)))))

;;; --------------------------------------------------------------------

(define (file-pathname? obj)
  ;;Return #t if OBJ is a string or bytevector, not empty, not including
  ;;a character whose ASCII representation is the null byte.
  ;;
  (or (file-string-pathname?     obj)
      (file-bytevector-pathname? obj)))

(define (file-string-pathname? obj)
  ;;Return #t if  OBJ is a string, not empty,  not including a character
  ;;whose ASCII representation is the null byte.
  ;;
  (and (string? obj)
       (not ($string-empty? obj))
       ;;Search for #\nul and return true if not found.
       (let loop ((i 0))
	 (or ($fx= i ($string-length obj))
	     (and (not ($char= #\nul ($string-ref obj i)))
		  (loop ($fxadd1 i)))))))

(define (file-bytevector-pathname? obj)
  ;;Return  #t if  OBJ  is  a bytevector,  not  empty,  not including  a
  ;;character whose ASCII representation is the null byte.
  ;;
  (and (bytevector? obj)
       (not ($bytevector-empty? obj))
       ;;Search for 0 and return true if not found.
       (let loop ((i 0))
	 (or ($fx= i ($bytevector-length obj))
	     (and (not ($fx= 0 ($bytevector-u8-ref obj i)))
		  (loop ($fxadd1 i)))))))

;;; --------------------------------------------------------------------

(define (file-absolute-pathname? pathname)
  ;;The argument PATHNAME must be a  string or bytevector.  Return #t if
  ;;PATHNAME starts  with a "/"  character, which  means it is  valid as
  ;;Unix-style absolute pathname; otherwise return #f.
  ;;
  ;;This function only acts upon  its argument, never accessing the file
  ;;system.
  ;;
  (define who 'file-absolute-pathname?)
  (with-arguments-validation (who)
      ((file-pathname	pathname))
    ($file-absolute-pathname? pathname)))

(define ($file-absolute-pathname? pathname)
  (with-pathnames ((pathname.bv pathname))
    ($fx= ASCII-SLASH-FX ($bytevector-u8-ref pathname.bv 0))))

(define (file-relative-pathname? pathname)
  ;;The argument PATHNAME must be a  string or bytevector.  Return #t if
  ;;PATHNAME does  not start  with a  "/" character,  which means  it is
  ;;valid as Unix-style relative pathname; otherwise return #f.
  ;;
  ;;This function only acts upon  its argument, never accessing the file
  ;;system.
  ;;
  (define who 'file-relative-pathname?)
  (with-arguments-validation (who)
      ((file-pathname	pathname))
    ($file-relative-pathname? pathname)))

(define ($file-relative-pathname? pathname)
  (with-pathnames ((pathname.bv pathname))
    (not ($fx= ASCII-SLASH-FX ($bytevector-u8-ref pathname.bv 0)))))

;;; --------------------------------------------------------------------

(define (file-colon-search-path? obj)
  ;;Return #t  if OBJ  is a  string or  bytevector, possibly  empty, not
  ;;including a character whose ASCII representation is the null byte.
  ;;
  (or (file-string-colon-search-path?     obj)
      (file-bytevector-colon-search-path? obj)))

(define (file-string-colon-search-path? obj)
  ;;Return  #t if  OBJ  is a  string, possibly  empty,  not including  a
  ;;character whose ASCII representation is the null byte.
  ;;
  (and (string? obj)
       ;;Search for #\nul and return true if not found.
       (let loop ((i 0))
	 (or ($fx= i ($string-length obj))
	     (and (not ($char= #\nul ($string-ref obj i)))
		  (loop ($fxadd1 i)))))))

(define (file-bytevector-colon-search-path? obj)
  ;;Return #t  if OBJ is a  bytevector, possibly empty, not  including a
  ;;character whose ASCII representation is the null byte.
  ;;
  (and (bytevector? obj)
       ;;Search for 0 and return true if not found.
       (let loop ((i 0))
	 (or ($fx= i ($bytevector-length obj))
	     (and (not ($fx= 0 ($bytevector-u8-ref obj i)))
		  (loop ($fxadd1 i)))))))


;;;; file operations

(define (real-pathname pathname)
  (define who 'real-pathname)
  (with-arguments-validation (who)
      ((file-pathname		pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-realpath pathname.bv)))
	(if (bytevector? rv)
	    ((filename->string-func) rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (delete-file pathname)
  ;;Defined by R6RS.
  ;;
  (define who 'delete-file)
  (with-arguments-validation (who)
      ((file-pathname	pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-unlink pathname.bv)))
	(unless ($fxzero? rv)
	  (%raise-errno-error/filename who rv pathname))))))


;;;; string pathnames

(module (split-pathname-root-and-tail)
  (define who 'split-pathname-root-and-tail)

  (define (split-pathname-root-and-tail pathname)
    ;;Split a string pathname into the directory part and the tail part.
    ;;Return 2  values: a string  representing the directory part  and a
    ;;string representing the  tail name part.  If PATHNAME  is just the
    ;;name  of the  tail:  the directory  part is  empty  and the  first
    ;;returned value is the empty string.
    ;;
    ;;Assume  the  pathname  components   separator  is  "/",  which  is
    ;;Unix-specific.
    ;;
    (with-arguments-validation (who)
	((string  pathname))
      (cond (($find-last PATH-SEPARATOR pathname)
	     => (lambda (i)
		  (values (substring pathname 0 i)
			  (let ((i (fx+ i 1)))
			    (substring pathname i (string-length pathname))))))
	    (else
	     (values "" pathname)))))

  (define ($find-last ch str)
    ;;Return a fixnum  representing the index of the  last occurrence of
    ;;the character CH in the string STR.
    ;;
    (let loop ((i ($string-length str)))
      (if ($fxzero? i)
	  #f
	(let ((i ($fxsub1 i)))
	  (if ($char= ch ($string-ref str i))
	      i
	    (loop i))))))

  (define-constant PATH-SEPARATOR
    #\/)

  #| end of module: split-tail-name |# )

;;; --------------------------------------------------------------------

(module (search-file-in-environment-path
	 search-file-in-list-path)

  (define (search-file-in-environment-path pathname environment-variable)
    (define who 'search-file-in-environment-path)
    (with-arguments-validation (who)
	((file-string-pathname	pathname)
	 (non-empty-string	environment-variable))
      (cond ((getenv environment-variable)
	     => (lambda (string-path)
		  ($search-file-in-list-path pathname (split-search-path-string string-path))))
	    (else
	     (search-file-in-list-path pathname '())))))

  (define (search-file-in-list-path pathname list-of-directories)
    (define who 'search-file-in-list-path)
    (with-arguments-validation (who)
	((file-string-pathname	pathname)
	 (list-of-strings	list-of-directories))
      ($search-file-in-list-path pathname list-of-directories)))

  (define ($search-file-in-list-path pathname list-of-directories)
    (if (file-absolute-pathname? pathname)
	pathname
      (let loop ((dirs list-of-directories))
	(if (null? dirs)
	    #f
	  (let* ((pathname    (string-append ($car dirs) pathname))
		 (pathname.bv (with-pathnames ((pathname.bv pathname))
				(capi.posix-realpath pathname.bv))))
	    (if (bytevector? pathname.bv)
		((pathname->string-func) pathname.bv)
	      (loop ($cdr dirs))))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (split-search-path
	 split-search-path-bytevector
	 split-search-path-string
	 split-pathname
	 split-pathname-bytevector
	 split-pathname-string)

  (define (split-search-path path)
    (define who 'split-search-path)
    (with-arguments-validation (who)
	((file-colon-search-path	path))
      (if (string? path)
	  (map ascii->string (split-search-path-bytevector (string->ascii path)))
	(split-search-path-bytevector path))))

  (define (split-search-path-string path)
    (define who 'split-search-path-string)
    (with-arguments-validation (who)
	((file-string-colon-search-path	path))
      (map ascii->string (split-search-path-bytevector (string->ascii path)))))

  (define (split-search-path-bytevector path)
    (define who 'split-search-path-bytevector)
    (with-arguments-validation (who)
	((file-bytevector-colon-search-path	path))
      (let ((path.len ($bytevector-length path)))
	(if ($fxzero? path.len)
	    '()
	  (let next-pathname ((path.index	0)
			      (pathnames	'()))
	    (if ($fx= path.index path.len)
		(reverse pathnames)
	      (let ((separator-index (%find-next-separator ASCII-COLON-FX
							   path path.index path.len)))
		(if separator-index
		    (next-pathname ($fxadd1 separator-index)
				   (if ($fx= path.index separator-index)
				       pathnames
				     (cons (%$subbytevector path path.index separator-index)
					   pathnames)))
		  (reverse (cons (%$subbytevector path path.index path.len)
				 pathnames))))))))))

  (define (split-pathname pathname)
    (define who 'split-pathname)
    (with-arguments-validation (who)
	((file-pathname		pathname))
      (if (string? pathname)
	  (split-pathname-string pathname)
	(split-pathname-bytevector pathname))))

  (define (split-pathname-string pathname)
    (define who 'split-pathname-string)
    (with-arguments-validation (who)
	((file-string-pathname	pathname))
      (let-values (((absolute? components)
		    (split-pathname-bytevector (string->ascii pathname))))
	(values absolute? (map ascii->string components)))))

  (define (split-pathname-bytevector pathname)
    (define who 'split-pathname-bytevector)
    (with-arguments-validation (who)
	((file-bytevector-pathname	pathname))
      (let* ((pathname.len	($bytevector-length pathname))
	     (components	(if ($fxzero? pathname.len)
				    '()
				  (%$bytevector-pathname-components pathname pathname.len))))
	(cond ((null? components)
	       (cond (($fxzero? pathname.len)
		      (values #f '()))
		     (($fx= ASCII-SLASH-FX ($bytevector-u8-ref pathname 0))
		      (values #t '()))
		     (else
		      (values #f '()))))
	      (($fx= ASCII-SLASH-FX ($bytevector-u8-ref pathname 0))
	       (values #t components))
	      (else
	       (values #f components))))))

  (define (%$bytevector-pathname-components pathname.bv pathname.len)
    (let next-component ((pathname.index	0)
			 (components		'()))
      (if ($fx= pathname.index pathname.len)
	  (reverse components)
	(let ((separator-index (%find-next-separator ASCII-SLASH-FX
						     pathname.bv pathname.index pathname.len)))
	  (if separator-index
	      (next-component ($fxadd1 separator-index)
			      (if ($fx= pathname.index separator-index)
				  components
				(cons (%$subbytevector pathname.bv pathname.index separator-index)
				      components)))
	    (reverse (cons (%$subbytevector pathname.bv pathname.index pathname.len)
			   components)))))))

  (define (%find-next-separator separator bv bv.start bv.len)
    ;;Scan BV, from BV.START included  to BV.LEN excluded, looking for a
    ;;byte representing a slash in  ASCII encoding.  When found return a
    ;;fixnum being the index of the slash, else return false.
    ;;
    (let next-byte ((bv.index bv.start))
      (if ($fx= bv.index bv.len)
	  #f
	(if ($fx= separator ($bytevector-u8-ref bv bv.index))
	    bv.index
	  (next-byte ($fxadd1 bv.index))))))

  (define-inline (%$subbytevector src.bv src.start src.end)
    (%$subbytevector-u8/count src.bv src.start ($fx- src.end src.start)))

  (define (%$subbytevector-u8/count src.bv src.start dst.len)
    (let ((dst.bv ($make-bytevector dst.len)))
      (do ((dst.index 0         ($fx+ 1 dst.index))
	   (src.index src.start ($fx+ 1 src.index)))
	  (($fx= dst.index dst.len)
	   dst.bv)
	($bytevector-u8-set! dst.bv dst.index ($bytevector-u8-ref src.bv src.index)))))

  #| end of module |# )


;;;; file attributes

(define (file-modification-time pathname)
  (define who 'file-modification-time)
  (with-arguments-validation (who)
      ((file-pathname	pathname))
    (with-pathnames ((pathname.bv  pathname))
      (let* ((timespec ($make-clean-vector 2))
	     (rv       (capi.posix-file-mtime pathname.bv timespec)))
	(if ($fxzero? rv)
	    (+ (* #e1e9 ($vector-ref timespec 0))
	       ($vector-ref timespec 1))
	  (%raise-errno-error/filename who rv pathname))))))


;;;; program name

(define (vicare-argv0)
  (foreign-call "ikrt_get_argv0_bytevector"))

(define (vicare-argv0-string)
  (foreign-call "ikrt_get_argv0_string"))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-pathnames 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors/or-false 'scheme-indent-function 1)
;; End:
