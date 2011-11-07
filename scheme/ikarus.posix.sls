;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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
    ;; errno codes handling
    errno->string

    ;; interprocess singnal codes handling
    interprocess-signal->string

    ;; error handling
    strerror

    ;; executing processes
    posix-fork			fork
    system

    ;; process exit status
    waitpid
    WIFEXITED			WEXITSTATUS
    WIFSIGNALED			WTERMSIG
    WCOREDUMP			WIFSTOPPED
    WSTOPSIG

    ;; process identifier
    getpid			getppid

    ;; system environment variables
    getenv			setenv
    unsetenv			env
    environ

    ;; interprocess signals
    kill

    ;; file system interface
    file-exists?		delete-file
    rename-file			split-file-name
    file-real-path

    current-directory		directory-list
    make-directory		make-directory*
    delete-directory

    file-ctime			file-mtime
    file-regular?		file-directory?
    file-readable?		file-writable?
    file-executable?		file-symbolic-link?
    file-size

    make-symbolic-link		make-hard-link

    change-mode

    nanosleep)
  (import (except (ikarus)
		  ;; errno handling
		  errno->string

		  ;; interprocess singnal codes handling
		  interprocess-signal->string

		  ;; executing processes
		  posix-fork			fork
		  system

		  ;; process exit status
		  waitpid
		  WIFEXITED			WEXITSTATUS
		  WIFSIGNALED			WTERMSIG
		  WCOREDUMP			WIFSTOPPED
		  WSTOPSIG

		  ;; process identifier
		  getpid			getppid

		  kill
		  nanosleep

		  getenv			setenv
		  unsetenv			env
		  environ

		  file-exists?		delete-file
		  rename-file			split-file-name
		  file-real-path

		  current-directory		directory-list
		  make-directory		make-directory*
		  delete-directory

		  file-ctime			file-mtime
		  file-regular?		file-directory?
		  file-readable?		file-writable?
		  file-executable?		file-symbolic-link?
		  file-size

		  make-symbolic-link		make-hard-link

		  change-mode

		  strerror)
    (vicare errno)
    (vicare interprocess-signals)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.))


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

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (signal who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))


;;;; errno handling

(define (errno->string negated-errno-code)
  ;;Convert an errno  code as represented by the  (vicare errno) library
  ;;into a string representing the errno code symbol.
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


;;;; interprocess singnal codes handling

(define (interprocess-signal->string interprocess-signal-code)
  ;;Convert an  interprocess signal code  as represented by  the (vicare
  ;;interprocess-signals)  library   into  a  string   representing  the
  ;;interprocess signal symbol.
  ;;
  (define who 'interprocess-signal->string)
  (with-arguments-validation (who)
      ((fixnum  interprocess-signal-code))
    (and (unsafe.fx> interprocess-signal-code 0)
	 (unsafe.fx< interprocess-signal-code (vector-length INTERPROCESS-SIGNAL-VECTOR))
	 (vector-ref INTERPROCESS-SIGNAL-VECTOR interprocess-signal-code))))

(let-syntax
    ((make-interprocess-signal-vector
      (lambda (stx)
	(define (%mk-vector)
	  (let* ((max	(fold-left (lambda (max pair)
				     (let ((code (cdr pair)))
				       (cond ((not code)	max)
					     ((< max code)	code)
					     (else		max))))
			  0 interprocess-signal-alist))
		 (vec.len	(fx+ 1 max))
		 ;;All the unused positions are set to #f.
		 (vec	(make-vector vec.len #f)))
	    (for-each (lambda (pair)
			(when (cdr pair)
			  (vector-set! vec (cdr pair) (car pair))))
	      interprocess-signal-alist)
	    vec))
	(define interprocess-signal-alist
	  `(("SIGFPE"		. ,SIGFPE)
	    ("SIGILL"		. ,SIGILL)
	    ("SIGSEGV"		. ,SIGSEGV)
	    ("SIGBUS"		. ,SIGBUS)
	    ("SIGABRT"		. ,SIGABRT)
	    ("SIGIOT"		. ,SIGIOT)
	    ("SIGTRAP"		. ,SIGTRAP)
	    ("SIGEMT"		. ,SIGEMT)
	    ("SIGSYS"		. ,SIGSYS)
	    ("SIGTERM"		. ,SIGTERM)
	    ("SIGINT"		. ,SIGINT)
	    ("SIGQUIT"		. ,SIGQUIT)
	    ("SIGKILL"		. ,SIGKILL)
	    ("SIGHUP"		. ,SIGHUP)
	    ("SIGALRM"		. ,SIGALRM)
	    ("SIGVRALRM"	. ,SIGVRALRM)
	    ("SIGPROF"		. ,SIGPROF)
	    ("SIGIO"		. ,SIGIO)
	    ("SIGURG"		. ,SIGURG)
	    ("SIGPOLL"		. ,SIGPOLL)
	    ("SIGCHLD"		. ,SIGCHLD)
	    ("SIGCLD"		. ,SIGCLD)
	    ("SIGCONT"		. ,SIGCONT)
	    ("SIGSTOP"		. ,SIGSTOP)
	    ("SIGTSTP"		. ,SIGTSTP)
	    ("SIGTTIN"		. ,SIGTTIN)
	    ("SIGTTOU"		. ,SIGTTOU)
	    ("SIGPIPE"		. ,SIGPIPE)
	    ("SIGLOST"		. ,SIGLOST)
	    ("SIGXCPU"		. ,SIGXCPU)
	    ("SIGXSFZ"		. ,SIGXSFZ)
	    ("SIGUSR1"		. ,SIGUSR1)
	    ("SIGUSR2"		. ,SIGUSR2)
	    ("SIGWINCH"		. ,SIGWINCH)
	    ("SIGINFO"		. ,SIGINFO)))
	(syntax-case stx ()
	  ((?ctx)
	   #`(quote #,(datum->syntax #'?ctx (%mk-vector))))))))
  (define INTERPROCESS-SIGNAL-VECTOR (make-interprocess-signal-vector)))


;;;; process identifiers

(define (getpid)
  (capi.posix-getpid))

(define (getppid)
  (capi.posix-getppid))


;;;; executing and forking processes

(define (system x)
  (define who 'system)
  (with-arguments-validation (who)
      ((string  x))
    (let ((rv (capi.posix-system (string->utf8 x))))
      (if (unsafe.fx< rv 0)
	  (raise/strerror who rv)
	rv))))

(define (posix-fork)
  (capi.posix-fork))

(define (fork parent-proc child-proc)
  (define who 'fork)
  (with-arguments-validation (who)
      ((procedure  parent-proc)
       (procedure  child-proc))
    (let ((pid (capi.posix-fork)))
      (cond ((unsafe.fxzero? pid)
	     (child-proc))
	    ((unsafe.fx< pid 0)
	     (raise/strerror who pid))
	    (else
	     (parent-proc pid))))))


;;;; process termination status

(define (waitpid pid block?)
  (define who 'waitpid)
  (with-arguments-validation (who)
      ((pid      pid)
       (boolean  block?))
    (capi.posix-waitpid pid block?)))

(let-syntax
    ((define-termination-status (syntax-rules ()
				  ((_ ?who ?primitive)
				   (define (?who status)
				     (define who '?who)
				     (with-arguments-validation (who)
					 ((fixnum  status))
				       (?primitive status)))))))
  (define-termination-status WIFEXITED		capi.posix-WIFEXITED)
  (define-termination-status WEXITSTATUS	capi.posix-WEXITSTATUS)
  (define-termination-status WIFSIGNALED	capi.posix-WIFSIGNALED)
  (define-termination-status WTERMSIG		capi.posix-WTERMSIG)
  (define-termination-status WCOREDUMP		capi.posix-WCOREDUMP)
  (define-termination-status WIFSTOPPED		capi.posix-WIFSTOPPED)
  (define-termination-status WSTOPSIG		capi.posix-WSTOPSIG))


;;;; interprocess signal handling

(define (kill pid signum)
  (define who 'kill)
  (with-arguments-validation (who)
      ((pid	pid)
       (signal	signum))
    (let ((r (foreign-call "ikrt_kill" pid signum)))
      (when (unsafe.fx< r 0)
	(error who (strerror r) pid signum
	       (interprocess-signal->string signum))))))


(define (stat path follow who)
  (unless (string? path)
    (die who "not a string" path))
  (let ((r (foreign-call "ikrt_stat" ((string->filename-func) path) follow)))
    (case r
      ((0) 'unknown)
      ((1) 'regular)
      ((2) 'directory)
      ((3) 'symlink)
      (else
       (case-errno r
	 ((ENOENT)  #f) ;; from ikarus-errno.c: ENOENT -- path does not exist
	 ((ENOTDIR) #f) ;; from ikarus-errno.c: ENOTDIR -- path does not exist
	 (else
	  (raise/strerror who r path)))))))

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
  (unless (string? str) (die who "not a string" str))
  (cond
   ((find-last path-sep str) =>
    (lambda (i)
      (values
       (substring str 0 i)
       (let ((i (fx+ i 1)))
	 (substring str i (string-length str) )))))
   (else (values "" str))))

(define (access path how who)
  (unless (string? path)
    (die who "filename not a string" path))
  (let ((r (foreign-call "ikrt_access" ((string->filename-func) path) how)))
    (unless (boolean? r) (raise/strerror who r path))
    r))

(define file-exists?
  (case-lambda
   ((path)
    (file-exists? path #t))
   ((path follow)
    (and (stat path follow 'file-exists?) #t))))

(define file-regular?
  (case-lambda
   ((path) (file-regular? path #t))
   ((path follow)
    (eq? 'regular (stat path follow 'file-regular?)))))

(define file-directory?
  (case-lambda
   ((path) (file-directory? path #t))
   ((path follow)
    (eq? 'directory (stat path follow 'file-directory?)))))

(define (file-symbolic-link? path)
  (eq? 'symlink (stat path #f 'file-symbolic-link?)))

(define file-readable?
  (lambda (path)
    (access path 1 'file-readable?)))

(define file-writable?
  (lambda (path)
    (access path 2 'file-writable?)))

(define file-executable?
  (lambda (path)
    (access path 4 'file-executable?)))

(define file-size
  (lambda (path)
    (define who 'file-size)
    (unless (string? path)
      (die who "filename is not a string" path))
    (let* ((v (foreign-call "ikrt_file_size" ((string->filename-func) path))))
      (if (>= v 0)
	  v
	(raise/strerror who v path)))))

(define delete-file
  (lambda (x)
    (define who 'delete-file)
    (unless (string? x)
      (die who "filename is not a string" x))
    (let ((v (foreign-call "ikrt_delete_file" ((string->filename-func) x))))
      (unless (eq? v #t)
	(raise/strerror who v x)))))

(define rename-file
  (lambda (src dst)
    (define who 'rename-file)
    (unless (string? src)
      (die who "source file name is not a string" src))
    (unless (string? dst)
      (die who "destination file name is not a string" dst))
    (let ((v (foreign-call "ikrt_rename_file"
			   ((string->filename-func) src)
			   ((string->filename-func) dst))))
      (unless (eq? v #t)
	(raise/strerror who v src)))))

(define directory-list
  (lambda (path)
    (define who 'directory-list)
    (unless (string? path)
      (die who "not a string" path))
    (let ((r (foreign-call "ikrt_directory_list" ((string->filename-func) path))))
      (if (fixnum? r)
	  (raise/strerror who r path)
	(map utf8->string (reverse r))))))

(define ($make-directory path mode who)
  (unless (string? path)
    (die who "not a string" path))
  (unless (fixnum? mode)
    (die who "not a fixnum" mode))
  (let ((r (foreign-call "ikrt_mkdir" ((string->filename-func) path) mode)))
    (unless (eq? r #t)
      (raise/strerror who r path))))

(define default-dir-mode #o755)

(define make-directory
  (case-lambda
   ((path) (make-directory path default-dir-mode))
   ((path mode) ($make-directory path mode 'make-directory))))

(module (make-directory*)
  (define who 'make-directory*)
  (define (mkdir* dirname0 mode)
    (unless (string? dirname0)
      (die who "not a string" dirname0))
    (let f ((dirname dirname0))
      (cond
       ((file-exists? dirname)
	(unless (file-directory? dirname)
	  (die who
               (format "path component ~a is not a directory" dirname)
               dirname0)))
       (else
	(let-values (((base suffix) (split-file-name dirname)))
	  (unless (string=? base "") (f base))
	  (unless (string=? suffix "")
	    ($make-directory dirname mode who)))))))
  (define make-directory*
    (case-lambda
     ((name) (mkdir* name default-dir-mode))
     ((name mode) (mkdir* name mode)))))

(define delete-directory
  (case-lambda
   ((path) (delete-directory path #f))
   ((path want-error?)
    (define who 'delete-directory)
    (unless (string? path)
      (die who "not a string" path))
    (let ((r (foreign-call "ikrt_rmdir" ((string->filename-func) path))))
      (if want-error?
	  (unless (eq? r #t) (raise/strerror who r path))
	(eq? r #t))))))

(define change-mode
  (lambda (path mode)
    (define who 'change-mode)
    (unless (string? path)
      (die who "not a string" path))
    (unless (fixnum? mode)
      (die who "not a fixnum" mode))
    (let ((r (foreign-call "ikrt_chmod" ((string->filename-func) path) mode)))
      (unless (eq? r #t)
	(raise/strerror who r path)))))

(define ($make-link to path who proc)
  (unless (and (string? to) (string? path))
    (die who "not a string" (if (string? to) path to)))
  (let ((r (proc ((string->filename-func) to) ((string->filename-func) path))))
    (unless (eq? r #t)
      (raise/strerror who r path))))

(define (make-symbolic-link to path)
  ($make-link to path 'make-symbolic-link
	      (lambda (u-to u-path)
		(foreign-call "ikrt_symlink" u-to u-path))))

(define (make-hard-link to path)
  ($make-link to path 'make-hard-link
	      (lambda (u-to u-path)
		(foreign-call "ikrt_link" u-to u-path))))

(define ($file-time x who proc)
  (unless (string? x)
    (die who "not a string" x))
  (let ((v (proc ((string->filename-func) x))))
    (cond
     ((bytevector? v)
      (let ((n0 (bytevector-u8-ref v 0))
	    (n1 (bytevector-u8-ref v 1)))
	(+ (* (bytevector-uint-ref v 2 (native-endianness) n0)
	      #e1e9)
	   (bytevector-uint-ref v (+ 2 n0) (native-endianness) n1))))
     (else (raise/strerror who v x)))))

(define (file-ctime x)
  ($file-time x 'file-ctime
	      (lambda (u) (foreign-call "ikrt_file_ctime2" u))))

(define (file-mtime x)
  ($file-time x 'file-mtime
	      (lambda (u) (foreign-call "ikrt_file_mtime2" u))))

(define (file-real-path x)
  (define who 'file-real-path)
  (unless (string? x)
    (die who "not a string" x))
  (let ((v (foreign-call "ikrt_realpath" ((string->filename-func) x))))
    (cond
     ((bytevector? v)
      (let ((s (utf8->string v)))
	(when (or (string=? s "")
		  (not (char=? (string-ref s 0) #\/)))
	  (error who "unexpected value returned from OS" s x))
	s))
     (else (raise/strerror who v x)))))

(define (getenv key)
  (define who 'getenv)
  (define ($getenv-str key)
    (define ($getenv-bv key)
      (foreign-call "ikrt_getenv" key))
    (let ((rv ($getenv-bv (string->utf8 key))))
      (and rv (utf8->string rv))))
  (if (string? key)
      ($getenv-str key)
    (die who "key is not a string" key)))

(define ($setenv key val overwrite)
  (foreign-call "ikrt_setenv"
		(string->utf8 key) (string->utf8 val) overwrite))

(define setenv
  (case-lambda
   ((key val overwrite)
    (define who 'setenv)
    (if (string? key)
	(if (string? val)
	    (unless ($setenv key val overwrite)
	      (error who "cannot setenv"))
	  (die who "invalid value" val))
      (die who "invalid key" key)))
   ((key val) (setenv key val #t))))

(define (unsetenv key)
  (define who 'unsetenv)
  (if (string? key)
      (foreign-call "ikrt_unsetenv" (string->utf8 key))
    (die who "invalid key" key)))

(define env
  (let ()
    (define env
      (case-lambda
       ((key)
	(if (string? key)
	    (foreign-call "ikrt_getenv" key)
	  (die 'env "the key is not a string" key)))
       ((key val) (env key val #t))
       ((key val overwrite?)
	(if (string? key)
	    (if (string? val)
		(unless (foreign-call "ikrt_setenv" key val overwrite?)
		  (die 'env "failed" key val))
	      (die 'env "the value is not a string" val))
	  (die 'env "the key is not a string" key)))))
    (define busted (lambda args (die 'env "BUG: busted!")))
    busted))


(define environ
  (lambda ()
    (map
        (lambda (bv)
          (let ((s (utf8->string bv)))
            (define (loc= s i n)
              (cond
	       ((fx= i n) i)
	       ((char=? (string-ref s i) #\=) i)
	       (else (loc= s (fx+ i 1) n))))
            (let ((n (string-length s)))
              (let ((i (loc= s 0 n)))
                (cons (substring s 0 i)
                      (if (fx< (fxadd1 i) n)
                          (substring s (fxadd1 i) n)
			""))))))
      (foreign-call "ikrt_environ"))))

(define (nanosleep secs nsecs)
  (import (ikarus system $fx))
  (unless (cond
	   ((fixnum? secs) ($fx>= secs 0))
	   ((bignum? secs) (<= 0 secs (- (expt 2 32) 1)))
	   (else (die 'nanosleep "not an exact integer" secs)))
    (die 'nanosleep "seconds must be a nonnegative integer <=" secs))
  (unless (cond
	   ((fixnum? nsecs) ($fx>= nsecs 0))
	   ((bignum? nsecs) (<= 0 nsecs 999999999))
	   (else (die 'nanosleep "not an exact integer" nsecs)))
    (die 'nanosleep "nanoseconds must be an integer \
                       in the range 0..999999999" nsecs))
  (let ((rv (foreign-call "ikrt_nanosleep" secs nsecs)))
    (unless (eq? rv 0)
      (error 'nanosleep "failed"))))


(define current-directory
  (case-lambda
   (()
    (let ((v (foreign-call "ikrt_getcwd")))
      (if (bytevector? v)
	  (utf8->string v)
	(raise/strerror 'current-directory v))))
   ((x)
    (if (string? x)
	(let ((rv (foreign-call "ikrt_chdir" ((string->filename-func) x))))
	  (unless (eq? rv #t)
	    (raise/strerror 'current-directory rv x)))
      (die 'current-directory "not a string" x)))))

(define raise/strerror
  (case-lambda
   ((who errno-code)
    (raise/strerror who errno-code #f))
   ((who errno-code filename)
    (raise
     (condition
      (make-error)
      (make-who-condition who)
      (make-message-condition (strerror errno-code))
      (if filename
	  (make-i/o-filename-error filename)
	(condition)))))))

(define strerror
  (lambda (errno-code)
    (define who 'strerror)
    (unless (fixnum? errno-code)
      (die who "not a fixnum" errno-code))
    (let ((emsg (foreign-call "ikrt_strerror" errno-code)))
      (if emsg
	  (let ((errno-name (errno->string errno-code)))
	    #;(assert errno-name)
	    (format "~a: ~a"
	      errno-name #;(utf8->string errno-name)
	      (utf8->string emsg)))
	(format "Ikarus's ~a: don't know Ikarus errno code ~s"
	  who errno-code)))))


;;;; done

)

;;; end of file
