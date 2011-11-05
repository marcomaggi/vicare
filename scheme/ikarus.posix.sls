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
    posix-fork			fork
    waitpid			kill
    getpid			getppid
    system
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

    strerror
    wstatus-pid			wstatus-exit-status
    wstatus-received-signal)
  (import (except (ikarus)
		  nanosleep posix-fork fork waitpid getpid getppid system file-exists?
		  delete-file getenv setenv unsetenv env environ split-file-name
		  file-ctime file-mtime file-real-path current-directory
		  file-regular? file-directory? file-readable? file-writable?
		  file-executable? file-size rename-file file-symbolic-link?
		  make-symbolic-link make-hard-link directory-list
		  make-directory make-directory* delete-directory change-mode
		  kill strerror wstatus-pid wstatus-exit-status
		  wstatus-received-signal)
    (only (ikarus errno)
	  errno->string)
    (vicare errno)
    (vicare syntactic-extensions)
    (vicare unsafe-capi)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))


(define (posix-fork)
  (platform-fork-process))

(define (fork parent-proc child-proc)
  (define who 'fork)
  (with-arguments-validation (who)
      ((procedure  parent-proc)
       (procedure  child-proc))
    (let ((pid (platform-fork-process)))
      (cond ((unsafe.fx= pid 0)
	     (child-proc))
	    ((unsafe.fx< pid 0)
	     (raise/strerror who pid))
	    (else
	     (parent-proc pid))))))


;;;; signal handling

(define SIGNAL-NAMES-ALIST
  ;; From ikarus-process.c
  '((1 . SIGABRT)
    (2 . SIGALRM)
    (3 . SIGBUS)
    (4 . SIGCHLD)
    (5 . SIGCONT)
    (6 . SIGFPE)
    (7 . SIGHUP)
    (8 . SIGILL)
    (9 . SIGINT)
    (10 . SIGKILL)
    (11 . SIGPIPE)
    (12 . SIGQUIT)
    (13 . SIGSEGV)
    (14 . SIGSTOP)
    (15 . SIGTERM)
    (16 . SIGTSTP)
    (17 . SIGTTIN)
    (18 . SIGTTOU)
    (19 . SIGUSR1)
    (20 . SIGUSR2)
    (21 . SIGPOLL)
    (22 . SIGPROF)
    (23 . SIGSYS)
    (24 . SIGTRAP)
    (25 . SIGURG)
    (26 . SIGVTALRM)
    (27 . SIGXCPU)
    (28 . SIGXFSZ)))

(define (signal-code->signal-name sigcode)
  (cond ((assv sigcode SIGNAL-NAMES-ALIST)
	 => cdr)
	(else sigcode)))

(define (signal-name->signal-code signame)
  (cond ((find (lambda (p)
		 (eqv? (cdr p) signame))
	   SIGNAL-NAMES-ALIST)
	 => car)
	(else #f)))


(define (kill pid signame)
  (define who 'kill)
  (unless (fixnum? pid) (die who "not a fixnum" pid))
  (unless (symbol? signame) (die who "not a symbol" signame))
  (let ((r (foreign-call "ikrt_kill" pid
			 (or (signal-name->signal-code signame)
			     (die who "invalid signal name" signame)))))
    (when (fx< r 0)
      (error who (strerror r) pid signame))))

(define-struct wstatus (pid exit-status received-signal))

(define waitpid
  ;; If block? is #f and waitpid() would have blocked,
  ;; or if want-error? is #f and there was an error,
  ;; the value returned is #f
  (case-lambda
   (() (waitpid -1 #t #t))
   ((pid) (waitpid pid #t #t))
   ((pid block?) (waitpid pid block? #t))
   ((pid block? want-error?)
    (define who 'waitpid)
    (unless (fixnum? pid) (die who "not a fixnum" pid))
    (unless (boolean? block?) (die who "not a boolean" block?))
    (let ((r (foreign-call "ikrt_waitpid" (make-wstatus #f #f #f) pid block?)))
      (cond
       ((wstatus? r)
	(set-wstatus-received-signal! r
				      (signal-code->signal-name
				       (wstatus-received-signal r)))
	r)
       ((and want-error? (not (eqv? r 0)))
	(error who (strerror r) pid))
       (else #f))))))

(define (getpid)
  (foreign-call "ikrt_getpid"))

(define (getppid)
  (foreign-call "ikrt_getppid"))

(define (system x)
  (unless (string? x)
    (die 'system "not a string" x))
  (let ((rv (foreign-call "ik_system" (string->utf8 x))))
    (if (fx< rv 0)
	(raise/strerror 'system rv)
      rv)))

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
