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
    file-modification-time
    (rename (file-modification-time	file-mtime)
	    (real-pathname		realpath/string)))
  (import (except (ikarus)
		  ;; errno and h_errno codes handling
		  errno->string			h_errno->string
		  strerror			h_strerror

		  ;; interprocess singnal codes handling
		  interprocess-signal->string

		  ;; system environment variables
		  getenv			setenv
		  unsetenv
		  environ			environ-table
		  environ->table		table->environ

		  ;; process identifier
		  getpid			getppid

		  ;; executing processes
		  fork				system
		  execv				execve
		  execl				execle
		  execvp			execlp

		  ;; process exit status
		  waitpid			wait
		  WIFEXITED			WEXITSTATUS
		  WIFSIGNALED			WTERMSIG
		  WCOREDUMP			WIFSTOPPED
		  WSTOPSIG

		  ;; interprocess signals
		  raise-signal			kill
		  pause

		  ;; file system inspection
		  stat				lstat
		  fstat
		  make-struct-stat		struct-stat?
		  struct-stat-st_mode		struct-stat-st_ino
		  struct-stat-st_dev		struct-stat-st_nlink
		  struct-stat-st_uid		struct-stat-st_gid
		  struct-stat-st_size
		  struct-stat-st_atime		struct-stat-st_atime_usec
		  struct-stat-st_mtime		struct-stat-st_mtime_usec
		  struct-stat-st_ctime		struct-stat-st_ctime_usec
		  struct-stat-st_blocks		struct-stat-st_blksize

		  file-is-directory?		file-is-char-device?
		  file-is-block-device?		file-is-regular-file?
		  file-is-symbolic-link?	file-is-socket?
		  file-is-fifo?			file-is-message-queue?
		  file-is-semaphore?		file-is-shared-memory?
		  access			file-readable?
		  file-writable?		file-executable?
		  file-ctime			file-mtime
		  file-atime
		  file-size			file-exists?

		  S_ISDIR			S_ISCHR
		  S_ISBLK			S_ISREG
		  S_ISLNK			S_ISSOCK
		  S_ISFIFO

		  ;; file system muators
		  chown				fchown
		  chmod				fchmod
		  umask				getumask
		  utime				utimes
		  lutimes			futimes

		  ;; hard and symbolic links
		  link				symlink
		  readlink			readlink/string
		  realpath			realpath/string
		  delete-file			unlink
		  posix-remove			rename

		  ;; file system directories
		  mkdir				mkdir/parents
		  rmdir
		  getcwd			getcwd/string
		  chdir				fchdir
		  opendir			fdopendir
		  readdir			readdir/string
		  closedir			rewinddir
		  telldir			seekdir

		  make-directory-stream		directory-stream?
		  directory-stream-pathname	directory-stream-pointer
		  directory-stream-fd		directory-stream-closed?

		  split-file-name

		  ;; file descriptors
		  open				close
		  posix-read			pread
		  posix-write			pwrite
		  lseek
		  readv				writev
		  select			select-fd
		  fcntl				ioctl
		  dup				dup2
		  pipe				mkfifo

		  ;; sockets
		  make-sockaddr_un
		  sockaddr_un.pathname		sockaddr_un.pathname/string
		  make-sockaddr_in		make-sockaddr_in6
		  sockaddr_in.in_addr		sockaddr_in6.in6_addr
		  sockaddr_in.in_port		sockaddr_in6.in6_port
		  in6addr_loopback		in6addr_any
		  inet-aton			inet-ntoa
		  inet-pton			inet-ntop
		  inet-ntoa/string		inet-ntop/string
		  gethostbyname			gethostbyname2
		  gethostbyaddr			host-entries
		  getaddrinfo			gai-strerror
		  getprotobyname		getprotobynumber
		  getservbyname			getservbyport
		  protocol-entries		service-entries
		  socket			shutdown
		  socketpair
		  connect			listen
		  accept			bind
		  getpeername			getsockname
		  send				recv
		  sendto			recvfrom
		  setsockopt			getsockopt
		  setsockopt/int		getsockopt/int
		  setsockopt/size_t		getsockopt/size_t
		  setsockopt/linger		getsockopt/linger
		  getnetbyname			getnetbyaddr
		  network-entries

		  make-struct-hostent		struct-hostent?
		  struct-hostent-h_name		struct-hostent-h_aliases
		  struct-hostent-h_addrtype	struct-hostent-h_length
		  struct-hostent-h_addr_list	struct-hostent-h_addr

		  make-struct-addrinfo		struct-addrinfo?
		  struct-addrinfo-ai_flags	struct-addrinfo-ai_family
		  struct-addrinfo-ai_socktype	struct-addrinfo-ai_protocol
		  struct-addrinfo-ai_addrlen	struct-addrinfo-ai_addr
		  struct-addrinfo-ai_canonname

		  make-struct-protoent		struct-protoent?
		  struct-protoent-p_name	struct-protoent-p_aliases
		  struct-protoent-p_proto

		  make-struct-servent		struct-servent?
		  struct-servent-s_name		struct-servent-s_aliases
		  struct-servent-s_port		struct-servent-s_proto

		  make-struct-netent		struct-netent?
		  struct-netent-n_name		struct-netent-n_aliases
		  struct-netent-n_addrtype	struct-netent-n_net

		  ;; users and groups
		  getuid			getgid
		  geteuid			getegid
		  getgroups
		  seteuid			setuid
		  setegid			setgid
		  setreuid			setregid
		  getlogin			getlogin/string
		  getpwuid			getpwnam
		  getgrgid			getgrnam
		  user-entries			group-entries

		  make-struct-passwd		struct-passwd?
		  struct-passwd-pw_name		struct-passwd-pw_passwd
		  struct-passwd-pw_uid		struct-passwd-pw_gid
		  struct-passwd-pw_gecos	struct-passwd-pw_dir
		  struct-passwd-pw_shell

		  make-struct-group		struct-group?
		  struct-group-gr_name		struct-group-gr_gid
		  struct-group-gr_mem

		  ;; job control
		  ctermid			ctermid/string
		  setsid			getsid
		  getpgrp			setpgid
		  tcgetpgrp			tcsetpgrp
		  tcgetsid

		  ;; date and time functions
		  clock				times
		  posix-time			gettimeofday
		  localtime			gmtime
		  timelocal			timegm
		  strftime			strftime/string
		  nanosleep

		  make-struct-timeval		struct-timeval?
		  struct-timeval-tv_sec		struct-timeval-tv_usec

		  make-struct-timespec		struct-timespec?
		  struct-timespec-tv_sec	struct-timespec-tv_nsec

		  make-struct-tms		struct-tms?
		  struct-tms-tms_utime		struct-tms-tms_stime
		  struct-tms-tms_cutime		struct-tms-tms_cstime

		  make-struct-tm		struct-tm?
		  struct-tm-tm_sec		struct-tm-tm_min
		  struct-tm-tm_hour		struct-tm-tm_mday
		  struct-tm-tm_mon		struct-tm-tm_year
		  struct-tm-tm_wday		struct-tm-tm_yday
		  struct-tm-tm_isdst		struct-tm-tm_gmtoff
		  struct-tm-tm_zone

		  ;; miscellaneous functions
		  file-descriptor?)
    ;;To be removed after the next boot image rotation (Marco Maggi; Feb
    ;;4, 2012).
    (only (ikarus strings)
	  ascii->string
	  string->ascii)
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
  (define who 'realpath)
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
