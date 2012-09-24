;;;Vicare Scheme -- A compiler for R6RS Scheme.
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


#!vicare
(library (vicare posix)
  (export
    ;; errno and h_errno codes handling
    (rename (posix.errno->string	errno->string))
    strerror
    h_errno->string			h_strerror

    ;; interprocess singnal codes handling
    interprocess-signal->string

    ;; system environment variables
    getenv				setenv
    unsetenv
    environ				environ-table
    environ->table			table->environ

    ;; process identifier
    getpid				getppid

    ;; executing processes
    fork				system
    execv				execve
    execl				execle
    execvp				execlp

    ;; process exit status
    waitpid				wait
    WIFEXITED				WEXITSTATUS
    WIFSIGNALED				WTERMSIG
    WCOREDUMP				WIFSTOPPED
    WSTOPSIG

    ;; interprocess signals
    (rename (raise-signal	raise))
    kill				pause
    sigwaitinfo				sigtimedwait

    signal-bub-init			signal-bub-final
    signal-bub-acquire
    signal-bub-delivered?		signal-bub-all-delivered

    (rename (%make-struct-siginfo_t make-struct-siginfo_t))
    struct-siginfo_t?
    struct-siginfo_t-si_signo		set-struct-siginfo_t-si_signo!
    struct-siginfo_t-si_errno		set-struct-siginfo_t-si_errno!
    struct-siginfo_t-si_code		set-struct-siginfo_t-si_code!
    struct-siginfo_t-si_trapno		set-struct-siginfo_t-si_trapno!
    struct-siginfo_t-si_pid		set-struct-siginfo_t-si_pid!
    struct-siginfo_t-si_uid		set-struct-siginfo_t-si_uid!
    struct-siginfo_t-si_status		set-struct-siginfo_t-si_status!
    struct-siginfo_t-si_utime		set-struct-siginfo_t-si_utime!
    struct-siginfo_t-si_stime		set-struct-siginfo_t-si_stime!
    struct-siginfo_t-si_value.sival_int	set-struct-siginfo_t-si_value.sival_int!
    struct-siginfo_t-si_value.sival_ptr	set-struct-siginfo_t-si_value.sival_ptr!
    struct-siginfo_t-si_int		set-struct-siginfo_t-si_int!
    struct-siginfo_t-si_ptr		set-struct-siginfo_t-si_ptr!
    struct-siginfo_t-si_overrun		set-struct-siginfo_t-si_overrun!
    struct-siginfo_t-si_timerid		set-struct-siginfo_t-si_timerid!
    struct-siginfo_t-si_addr		set-struct-siginfo_t-si_addr!
    struct-siginfo_t-si_band		set-struct-siginfo_t-si_band!
    struct-siginfo_t-si_fd		set-struct-siginfo_t-si_fd!
    struct-siginfo_t-si_addr_lsb	set-struct-siginfo_t-si_addr_lsb!

    ;; file system inspection
    stat				lstat
    fstat
    make-struct-stat			struct-stat?
    struct-stat-st_mode			struct-stat-st_ino
    struct-stat-st_dev			struct-stat-st_nlink
    struct-stat-st_uid			struct-stat-st_gid
    struct-stat-st_size
    struct-stat-st_atime		struct-stat-st_atime_usec
    struct-stat-st_mtime		struct-stat-st_mtime_usec
    struct-stat-st_ctime		struct-stat-st_ctime_usec
    struct-stat-st_blocks		struct-stat-st_blksize

    file-is-directory?			file-is-char-device?
    file-is-block-device?		file-is-regular-file?
    file-is-symbolic-link?		file-is-socket?
    file-is-fifo?			file-is-message-queue?
    file-is-semaphore?			file-is-shared-memory?

    access				file-readable?
    file-writable?			file-executable?
    file-atime				file-ctime
    file-mtime
    file-size

    S_ISDIR				S_ISCHR
    S_ISBLK				S_ISREG
    S_ISLNK				S_ISSOCK
    S_ISFIFO

    ;; file system muators
    chown				fchown
    chmod				fchmod
    umask				getumask
    utime				utimes
    lutimes				futimes

    ;; hard and symbolic links
    link				symlink
    readlink				readlink/string
    realpath				realpath/string
    unlink
    remove			rename

    ;; file system directories
    mkdir				mkdir/parents
    rmdir
    getcwd				getcwd/string
    chdir				fchdir
    opendir				fdopendir
    readdir				readdir/string
    closedir				rewinddir
    telldir				seekdir

    make-directory-stream		directory-stream?
    directory-stream-pathname		directory-stream-pointer
    directory-stream-fd			directory-stream-closed?

    split-file-name
    split-search-path			split-pathname
    split-search-path-bytevector	split-pathname-bytevector
    split-search-path-string		split-pathname-string

    ;; file descriptors
    open				close
    read				pread
    write				pwrite
    lseek
    readv				writev
    select				select-fd
    select-fd-readable?			select-fd-writable?
    select-fd-exceptional?
    poll
    fcntl				ioctl
    dup					dup2
    pipe				mkfifo
    truncate				ftruncate

    sizeof-fd-set			make-fd-set-bytevector
    make-fd-set-pointer			make-fd-set-memory-block
    FD_ZERO				FD_SET
    FD_CLR				FD_ISSET
    select-from-sets			select-from-sets-array
    fd-set-inspection

    ;; memory-mapped input/output
    mmap				munmap
    msync				mremap
    madvise				mprotect
    mlock				munlock
    mlockall				munlockall

    ;; POSIX shared memory
    shm-open				shm-unlink

    ;; POSIX semaphores
    sem-open				sem-close
    sem-unlink				sem-init
    sem-destroy				sem-post
    sem-wait				sem-trywait
    sem-timedwait			sem-getvalue
    sizeof-sem_t

    ;; POSIX message queues
    mq-open				mq-close
    mq-unlink
    mq-send				mq-receive
    mq-timedsend			mq-timedreceive
    mq-setattr				mq-getattr
    #;mq-notify

    make-struct-mq-attr
    (rename (%valid-struct-mq-attr?	struct-mq-attr?))
    struct-mq-attr-mq_flags		set-struct-mq-attr-mq_flags!
    struct-mq-attr-mq_maxmsg		set-struct-mq-attr-mq_maxmsg!
    struct-mq-attr-mq_msgsize		set-struct-mq-attr-mq_msgsize!
    struct-mq-attr-mq_curmsgs		set-struct-mq-attr-mq_curmsgs!

    ;; POSIX per-process timers
    timer-create			timer-delete
    timer-settime			timer-gettime
    timer-getoverrun

    (rename (%make-struct-itimerspec make-struct-itimerspec))
    struct-itimerspec?
    struct-itimerspec-it_interval	struct-itimerspec-it_value
    set-struct-itimerspec-it_interval!	set-struct-itimerspec-it_value!

    ;; POSIX realtime clock functions
    clock-getres			clock-getcpuclockid
    clock-gettime			clock-settime

    ;; sockets
    make-sockaddr_un
    sockaddr_un.pathname		sockaddr_un.pathname/string
    make-sockaddr_in			make-sockaddr_in6
    sockaddr_in.in_addr			sockaddr_in6.in6_addr
    sockaddr_in.in_port			sockaddr_in6.in6_port
    in6addr_loopback			in6addr_any
    inet-aton				inet-ntoa
    inet-pton				inet-ntop
    inet-ntoa/string			inet-ntop/string
    gethostbyname			gethostbyname2
    gethostbyaddr			host-entries
    getaddrinfo				gai-strerror
    getprotobyname			getprotobynumber
    getservbyname			getservbyport
    protocol-entries			service-entries
    socket				shutdown
    socketpair
    connect				listen
    accept				bind
    getpeername				getsockname
    send				recv
    sendto				recvfrom
    setsockopt				getsockopt
    setsockopt/int			getsockopt/int
    setsockopt/size_t			getsockopt/size_t
    setsockopt/linger			getsockopt/linger
    getnetbyname			getnetbyaddr
    network-entries

    make-struct-hostent			struct-hostent?
    struct-hostent-h_name		struct-hostent-h_aliases
    struct-hostent-h_addrtype		struct-hostent-h_length
    struct-hostent-h_addr_list		struct-hostent-h_addr

    make-struct-addrinfo		struct-addrinfo?
    struct-addrinfo-ai_flags		struct-addrinfo-ai_family
    struct-addrinfo-ai_socktype		struct-addrinfo-ai_protocol
    struct-addrinfo-ai_addrlen		struct-addrinfo-ai_addr
    struct-addrinfo-ai_canonname

    make-struct-protoent		struct-protoent?
    struct-protoent-p_name		struct-protoent-p_aliases
    struct-protoent-p_proto

    make-struct-servent			struct-servent?
    struct-servent-s_name		struct-servent-s_aliases
    struct-servent-s_port		struct-servent-s_proto

    make-struct-netent			struct-netent?
    struct-netent-n_name		struct-netent-n_aliases
    struct-netent-n_addrtype		struct-netent-n_net

    ;; users and groups
    getuid				getgid
    geteuid				getegid
    getgroups
    seteuid				setuid
    setegid				setgid
    setreuid				setregid
    getlogin				getlogin/string
    getpwuid				getpwnam
    getgrgid				getgrnam
    user-entries			group-entries

    make-struct-passwd			struct-passwd?
    struct-passwd-pw_name		struct-passwd-pw_passwd
    struct-passwd-pw_uid		struct-passwd-pw_gid
    struct-passwd-pw_gecos		struct-passwd-pw_dir
    struct-passwd-pw_shell

    make-struct-group			struct-group?
    struct-group-gr_name		struct-group-gr_gid
    struct-group-gr_mem

    ;; job control
    ctermid				ctermid/string
    setsid				getsid
    getpgrp				setpgid
    tcgetpgrp				tcsetpgrp
    tcgetsid

    ;; date and time functions
    clock				times
    time				gettimeofday
    localtime				gmtime
    timelocal				timegm
    strftime				strftime/string
    setitimer				getitimer
    alarm
    nanosleep

    make-struct-timeval			struct-timeval?
    struct-timeval-tv_sec		struct-timeval-tv_usec
    set-struct-timeval-tv_sec!		set-struct-timeval-tv_usec!

    make-struct-timespec		struct-timespec?
    struct-timespec-tv_sec		struct-timespec-tv_nsec
    set-struct-timespec-tv_sec!		set-struct-timespec-tv_nsec!

    make-struct-tms			struct-tms?
    struct-tms-tms_utime		struct-tms-tms_stime
    struct-tms-tms_cutime		struct-tms-tms_cstime
    set-struct-tms-tms_utime!		set-struct-tms-tms_stime!
    set-struct-tms-tms_cutime!		set-struct-tms-tms_cstime!

    make-struct-tm			struct-tm?
    struct-tm-tm_sec			struct-tm-tm_min
    struct-tm-tm_hour			struct-tm-tm_mday
    struct-tm-tm_mon			struct-tm-tm_year
    struct-tm-tm_wday			struct-tm-tm_yday
    struct-tm-tm_isdst			struct-tm-tm_gmtoff
    struct-tm-tm_zone
    set-struct-tm-tm_sec!		set-struct-tm-tm_min!
    set-struct-tm-tm_hour!		set-struct-tm-tm_mday!
    set-struct-tm-tm_mon!		set-struct-tm-tm_year!
    set-struct-tm-tm_wday!		set-struct-tm-tm_yday!
    set-struct-tm-tm_isdst!		set-struct-tm-tm_gmtoff!
    set-struct-tm-tm_zone!

    (rename (%make-struct-itimerval make-struct-itimerval))
    struct-itimerval?
    struct-itimerval-it_interval	struct-itimerval-it_value
    set-struct-itimerval-it_interval!	set-struct-itimerval-it_value!

    ;; resources limits
    getrlimit				setrlimit
    getrusage				RLIM_INFINITY

    (rename (%make-struct-rlimit make-struct-rlimit))
    struct-rlimit?
    struct-rlimit-rlim_cur		set-struct-rlimit-rlim_cur!
    struct-rlimit-rlim_max		set-struct-rlimit-rlim_max!

    (rename (%make-struct-rusage make-struct-rusage))
    struct-rusage?
    struct-rusage-ru_utime		set-struct-rusage-ru_utime!
    struct-rusage-ru_stime		set-struct-rusage-ru_stime!
    struct-rusage-ru_maxrss		set-struct-rusage-ru_maxrss!
    struct-rusage-ru_ixrss		set-struct-rusage-ru_ixrss!
    struct-rusage-ru_idrss		set-struct-rusage-ru_idrss!
    struct-rusage-ru_isrss		set-struct-rusage-ru_isrss!
    struct-rusage-ru_minflt		set-struct-rusage-ru_minflt!
    struct-rusage-ru_majflt		set-struct-rusage-ru_majflt!
    struct-rusage-ru_nswap		set-struct-rusage-ru_nswap!
    struct-rusage-ru_inblock		set-struct-rusage-ru_inblock!
    struct-rusage-ru_oublock		set-struct-rusage-ru_oublock!
    struct-rusage-ru_msgsnd		set-struct-rusage-ru_msgsnd!
    struct-rusage-ru_msgrcv		set-struct-rusage-ru_msgrcv!
    struct-rusage-ru_nsignals		set-struct-rusage-ru_nsignals!
    struct-rusage-ru_nvcsw		set-struct-rusage-ru_nvcsw!
    struct-rusage-ru_nivcsw		set-struct-rusage-ru_nivcsw!

    ;; system configuration
    sysconf
    pathconf		fpathconf
    confstr		confstr/string

    ;; executable pathname
    find-executable-as-bytevector	find-executable-as-string
    vicare-executable-as-bytevector	vicare-executable-as-string

    ;; miscellaneous functions
    file-descriptor?)
  (import (except (vicare)
		  strerror		getenv
		  remove		time
		  read			write
		  truncate)
    (prefix (only (vicare $posix)
		  errno->string)
	    posix.)
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (prefix (vicare words)
	    words.))


;;;; helpers

(define-inline (%file-descriptor? obj)
  ;;Do  what   is  possible  to  recognise   fixnums  representing  file
  ;;descriptors.
  ;;
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<  obj FD_SETSIZE)))

(define-inline (%message-queue-descriptor? obj)
  (%file-descriptor? obj))

(define-inline (%signal-fixnum? ?obj)
  (let ((obj ?obj))
    (and (fixnum? obj)
	 (unsafe.fx>= obj 0)
	 (unsafe.fx<= obj NSIG))))

(define (%struct-timespec? obj)
  (and (struct-timespec? obj)
       (let ((sec (struct-timespec-tv_sec obj)))
	 (and (words.signed-long? sec)
	      (<= 0 sec)))
       (let ((nsec (struct-timespec-tv_nsec obj)))
	 (and (words.signed-long? nsec)
	      (<= 0 nsec 999999999)))))
;;;                      876543210

(define (%struct-timeval? obj)
  (and (struct-timeval? obj)
       (let ((sec (struct-timeval-tv_sec obj)))
	 (and (words.signed-long? sec)
	      (<= 0 sec)))
       (let ((usec (struct-timeval-tv_usec obj)))
	 (and (words.signed-long? usec)
	      (<= 0 usec 999999)))))
;;;                   876543210

(define (%struct-itimerval? obj)
  (and (struct-itimerval? obj)
       (%struct-timeval? (struct-itimerval-it_interval obj))
       (%struct-timeval? (struct-itimerval-it_value    obj))))

(define (%valid-itimerspec? obj)
  (and (struct-itimerspec? obj)
       (let ((T (struct-itimerspec-it_interval obj)))
	 (and (struct-timespec? T)
	      (words.signed-long? (struct-timespec-tv_sec  T))
	      (words.signed-long? (struct-timespec-tv_nsec T))))
       (let ((T (struct-itimerspec-it_value obj)))
	 (and (struct-timespec? T)
	      (words.signed-long? (struct-timespec-tv_sec  T))
	      (words.signed-long? (struct-timespec-tv_nsec T))))))

(define (%valid-struct-mq-attr? obj)
  (and (struct-mq-attr? obj)
       (words.signed-long? (struct-mq-attr-mq_flags   obj))
       (words.signed-long? (struct-mq-attr-mq_maxmsg  obj))
       (words.signed-long? (struct-mq-attr-mq_msgsize obj))
       (words.signed-long? (struct-mq-attr-mq_curmsgs obj))))

(define (%valid-struct-rlimit? obj)
  (and (struct-rlimit? obj)
       (words.word-s64? (struct-rlimit-rlim_cur obj))
       (words.word-s64? (struct-rlimit-rlim_max obj))))

(define (%valid-struct-rusage? obj)
  (define-inline (field? ?val)
    (let ((val ?val))
      (or (not val) (words.signed-long? val))))
  (and (struct-rusage? obj)
       (%struct-timeval? (struct-rusage-ru_utime obj))
       (%struct-timeval? (struct-rusage-ru_stime obj))
       (field? (struct-rusage-ru_maxrss obj))
       (field? (struct-rusage-ru_ixrss obj))
       (field? (struct-rusage-ru_idrss obj))
       (field? (struct-rusage-ru_isrss obj))
       (field? (struct-rusage-ru_minflt obj))
       (field? (struct-rusage-ru_majflt obj))
       (field? (struct-rusage-ru_nswap obj))
       (field? (struct-rusage-ru_inblock obj))
       (field? (struct-rusage-ru_oublock obj))
       (field? (struct-rusage-ru_msgsnd obj))
       (field? (struct-rusage-ru_msgrcv obj))
       (field? (struct-rusage-ru_nsignals obj))
       (field? (struct-rusage-ru_nvcsw obj))
       (field? (struct-rusage-ru_nivcsw obj))))


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

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (string-or-bytevector who obj)
  (or (bytevector? obj) (string? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (fixnum/false who obj)
  (or (not obj) (fixnum? obj))
  (assertion-violation who "expected false or fixnum as argument" obj))

(define-argument-validation (positive-fixnum who obj)
  (and (fixnum? obj)
       (unsafe.fx< 0 obj))
  (assertion-violation who "expected positive fixnum as argument" obj))

(define-argument-validation (non-negative-fixnum who obj)
  (and (fixnum? obj)
       (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

(define-argument-validation (boolean/fixnum who obj)
  (or (fixnum? obj) (boolean? obj))
  (assertion-violation who "expected boolean or fixnum as argument" obj))

(define-argument-validation (pointer/false who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected false or pointer as argument" obj))

(define-argument-validation (fixnum/pointer/false who obj)
  (or (not obj) (fixnum? obj) (pointer? obj))
  (assertion-violation who "expected false, fixnum or pointer as argument" obj))

(define-argument-validation (exact-integer who obj)
  (or (fixnum? obj) (bignum? obj))
  (assertion-violation who "expected exact integer as argument" obj))

(define-argument-validation (list-of-strings who obj)
  (and (list? obj) (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))

(define-argument-validation (list-of-bytevectors who obj)
  (and (list? obj) (for-all bytevector? obj))
  (assertion-violation who "expected list of bytevectors as argument" obj))

(define-argument-validation (string/bytevector who obj)
  (or (string? obj) (bytevector? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (string/bytevector/false who obj)
  (or (not obj) (string? obj) (bytevector? obj))
  (assertion-violation who "expected false, string or bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (general-buffer who obj)
  (or (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected bytevector or pointer or memory-block as general buffer argument" obj))

(define-argument-validation (general-buffer/false who obj)
  (or (not obj)
      (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected false or bytevector or pointer or memory-block as general buffer argument"
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (gid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum gid as argument" obj))

(define-argument-validation (file-descriptor who obj)
  (%file-descriptor? obj)
  (assertion-violation who "expected fixnum as file descriptor argument" obj))

(define-argument-validation (message-queue-descriptor who obj)
  (%message-queue-descriptor? obj)
  (assertion-violation who "expected message queue descriptor as argument" obj))

(define-argument-validation (signal who obj)
  (%signal-fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))

(define-argument-validation (pathname who obj)
  (or (bytevector? obj) (string? obj))
  (assertion-violation who "expected string or bytevector as pathname argument" obj))

(define-argument-validation (struct-stat who obj)
  (struct-stat? obj)
  (assertion-violation who "expected struct stat instance as argument" obj))

(define-argument-validation (secs who obj)
  (words.word-u32? obj)
  (assertion-violation who
    "expected exact integer in the range [0, 2^32-1] as seconds count argument" obj))

(define-argument-validation (nsecs who obj)
  (and (words.word-u32? obj)
       (<= 0 obj 999999999))
;;;              987654321
  (assertion-violation who
    "expected exact integer in the range [0, 999999999] as nanoseconds count argument" obj))

(define-argument-validation (secfx who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as seconds count argument" obj))

(define-argument-validation (usecfx who obj)
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<= obj 999999))
  (assertion-violation who "expected non-negative fixnum as nanoseconds count argument" obj))

(define-argument-validation (directory-stream who obj)
  (directory-stream? obj)
  (assertion-violation who "expected directory stream as argument" obj))

(define-argument-validation (open-directory-stream who obj)
  (not (directory-stream-closed? obj))
  (assertion-violation who "expected open directory stream as argument" obj))

(define-argument-validation (dirpos who obj)
  (and (or (fixnum? obj) (bignum? obj))
       (<= 0 obj))
  (assertion-violation who
    "expected non-negative exact integer as directory stream position argument" obj))

(define-argument-validation (off_t who obj)
  (words.off_t? obj)
  (assertion-violation who
    "expected platform off_t exact integer as offset argument" obj))

(define-argument-validation (false/file-descriptor who obj)
  (or (not obj) (%file-descriptor? obj))
  (assertion-violation who "expected false or file descriptor as argument" obj))

(define-argument-validation (select-nfds who obj)
  (or (not obj)
      (%file-descriptor? obj)
      (unsafe.fx= obj FD_SETSIZE))
  (assertion-violation who "expected false or file descriptor as argument" obj))

(define-argument-validation (list-of-fds who obj)
  (and (list? obj)
       (for-all (lambda (fd)
		  (%file-descriptor? fd))
	 obj))
  (assertion-violation who "expected list of file descriptors as argument" obj))

(define-argument-validation (af-inet who obj)
  (and (fixnum? obj)
       (or (unsafe.fx= obj AF_INET)
	   (unsafe.fx= obj AF_INET6)))
  (assertion-violation who "expected a fixnum among AF_INET and AF_INET6 as argument" obj))

(define-argument-validation (addrinfo/false who obj)
  (or (not obj) (struct-addrinfo? obj))
  (assertion-violation who "expected an instance of struct-addrinfo as argument" obj))

(define-argument-validation (netaddr who obj)
  (or (fixnum? obj) (bignum? obj)
      (and (bytevector? obj)
	   (= 4 (bytevector-length obj))))
  (assertion-violation who
    "expected exact integer or 32-bit bytevector as network address argument" obj))

(define-argument-validation (platform-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer in platform's \"int\" range as argument" obj))

(define-argument-validation (platform-int/boolean who obj)
  (or (boolean? obj) (words.signed-int? obj))
  (assertion-violation who
    "expected boolean or exact integer in platform's \"int\" range as argument" obj))

(define-argument-validation (platform-size_t who obj)
  (words.size_t? obj)
  (assertion-violation who
    "expected exact integer in platform's \"size_t\" range as argument" obj))

(define-argument-validation (struct-tm who obj)
  (struct-tm? obj)
  (assertion-violation who "expected instance of struct-tm as argument" obj))

(define-argument-validation (poll-fds who obj)
  (and (vector? obj) (vector-for-all (lambda (vec)
				       (and (unsafe.fx= 3 (unsafe.vector-length vec))
					    (fixnum? (unsafe.vector-ref vec 0))
					    (fixnum? (unsafe.vector-ref vec 1))
					    (fixnum? (unsafe.vector-ref vec 2))))
				     obj))
  (assertion-violation who "expected vector of data for poll as argument" obj))

(define-argument-validation (itimerval who obj)
  (%struct-itimerval? obj)
  (assertion-violation who "expected struct-itimerval as argument" obj))

(define-argument-validation (timespec who obj)
  (%struct-timespec? obj)
  (assertion-violation who "expected struct-timespec as argument" obj))

(define-argument-validation (itimerspec who obj)
  (%valid-itimerspec? obj)
  (assertion-violation who "expected struct-itimerspec as argument" obj))

(define-argument-validation (itimerspec/false who obj)
  (or (not obj) (%valid-itimerspec? obj))
  (assertion-violation who "expected false or struct-itimerspec as argument" obj))

(define-argument-validation (mq-attr who obj)
  (%valid-struct-mq-attr? obj)
  (assertion-violation who "expected instance of struct-mq-attr as argument" obj))

(define-argument-validation (mq-attr/false who obj)
  (or (not obj) (%valid-struct-mq-attr? obj))
  (assertion-violation who "expected false or instance of struct-mq-attr as argument" obj))

(define-argument-validation (semaphore who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as semaphore argument" obj))

(define-argument-validation (clockid_t who obj)
  (words.signed-long? obj)
  (assertion-violation who
    "expected exact integer representing clockid_t as argument" obj))

(define-argument-validation (timer_t who obj)
  (words.signed-long? obj)
  (assertion-violation who
    "expected exact integer representing timer_t as argument" obj))

(define-argument-validation (sigevent who obj)
  (struct-sigevent? obj)
  (assertion-violation who "expected struct-sigevent as argument" obj))

(define-argument-validation (siginfo_t who obj)
  (struct-siginfo_t? obj)
  (assertion-violation who "expected struct-siginfo_t as argument" obj))

(define-argument-validation (rlimit who obj)
  (%valid-struct-rlimit? obj)
  (assertion-violation who "expected struct-rlimit as argument" obj))

(define-argument-validation (rusage who obj)
  (%valid-struct-rusage? obj)
  (assertion-violation who "expected struct-rusage as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-int who obj)
  (words.unsigned-int? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned int\" as argument" obj))

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed int\" as argument" obj))


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
		(string-append (posix.errno->string errno) ": " (utf8->string msg))
	      (string-append "unknown errno code " (number->string (- errno))))))
      "no error")))

(define (%raise-errno-error who errno . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-irritants-condition irritants))))

(define (%raise-errno-error/filename who errno filename . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-i/o-filename-error filename)
	  (make-irritants-condition irritants))))

(define (%raise-h_errno-error who h_errno . irritants)
  (raise (condition
	  (make-error)
	  (make-h_errno-condition h_errno)
	  (make-who-condition who)
	  (make-message-condition (h_strerror h_errno))
	  (make-irritants-condition irritants))))

(define (%raise-posix-error who message . irritants)
  (raise (condition
	  (make-error)
	  (make-who-condition who)
	  (make-message-condition message)
	  (make-irritants-condition irritants))))


;;;; h_errno handling

(define (h_errno->string negated-h_errno-code)
  ;;Convert   an   h_errno   code   as  represented   by   the   (vicare
  ;;platform-constants) library  into a string  representing the h_errno
  ;;code symbol.
  ;;
  (define who 'h_errno->string)
  (with-arguments-validation (who)
      ((fixnum negated-h_errno-code))
    (let ((h_errno-code (unsafe.fx- 0 negated-h_errno-code)))
      (and (unsafe.fx> h_errno-code 0)
	   (unsafe.fx< h_errno-code (vector-length H_ERRNO-VECTOR))
	   (vector-ref H_ERRNO-VECTOR h_errno-code)))))

(let-syntax
    ((make-h_errno-vector
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
			  0 h_errno-alist))
		 (vec.len	(fx+ 1 max))
		 ;;All the unused positions are set to #f.
		 (vec	(make-vector vec.len #f)))
	    (for-each (lambda (pair)
			(when (cdr pair)
			  (vector-set! vec (fx- (cdr pair)) (car pair))))
	      h_errno-alist)
	    vec))
	(define h_errno-alist
	  `(("HOST_NOT_FOUND"		. ,HOST_NOT_FOUND)
	    ("TRY_AGAIN"		. ,TRY_AGAIN)
	    ("NO_RECOVERY"		. ,NO_RECOVERY)
	    ("NO_ADDRESS"		. ,NO_ADDRESS)))
	(syntax-case stx ()
	  ((?ctx)
	   #`(quote #,(datum->syntax #'?ctx (%mk-vector))))))))

  (define H_ERRNO-VECTOR (make-h_errno-vector)))

(define (h_strerror h_errno)
  (define who 'h_strerror)
  (with-arguments-validation (who)
      ((fixnum  h_errno))
    (let ((msg (case-h_errno h_errno
		 ((HOST_NOT_FOUND)	"no such host is known in the database")
		 ((TRY_AGAIN)		"the server could not be contacted")
		 ((NO_RECOVERY)		"non-recoverable error")
		 ((NO_ADDRESS)		"host entry exists but without Internet address")
		 (else			#f))))
      (if msg
	  (string-append (h_errno->string h_errno) ": " msg)
	(string-append "unknown h_errno code " (number->string (- h_errno)))))))


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


;;;; operating system environment variables

(define (getenv key)
  (define who 'getenv)
  (with-arguments-validation (who)
      ((string  key))
    (let ((rv (capi.posix-getenv (string->utf8 key))))
      (and rv (utf8->string rv)))))

(define setenv
  (case-lambda
   ((key val)
    (setenv key val #t))
   ((key val overwrite)
    (define who 'setenv)
    (with-arguments-validation (who)
	((string  key)
	 (string  val))
      (unless (capi.posix-setenv (string->utf8 key)
				 (string->utf8 val)
				 overwrite)
	(error who "cannot setenv" key val overwrite))))))

(define (unsetenv key)
  (define who 'unsetenv)
  (with-arguments-validation (who)
      ((string  key))
    (capi.posix-unsetenv (string->utf8 key))))

(define (%find-index-of-= str idx str.len)
  ;;Scan STR starint at index IDX  and up to STR.LEN for the position of
  ;;the character #\=.  Return the index or STR.LEN.
  ;;
  (cond ((unsafe.fx= idx str.len)
	 idx)
	((unsafe.char= #\= (unsafe.string-ref str idx))
	 idx)
	(else
	 (%find-index-of-= str (unsafe.fxadd1 idx) str.len))))

(define (environ)
  (map (lambda (bv)
	 (let* ((str     (utf8->string bv))
		(str.len (unsafe.string-length str))
		(idx     (%find-index-of-= str 0 str.len)))
	   (cons (substring str 0 idx)
		 (if (unsafe.fx< (unsafe.fxadd1 idx) str.len)
		     (substring str (unsafe.fxadd1 idx) str.len)
		   ""))))
    (capi.posix-environ)))

(define (environ-table)
  (environ->table (environ)))

(define (environ->table environ)
  (begin0-let ((table (make-hashtable string-hash string=?)))
    (for-each (lambda (pair)
		(hashtable-set! table (unsafe.car pair) (unsafe.cdr pair)))
      environ)))

(define (table->environ table)
  (let-values (((names values) (hashtable-entries table)))
    (let ((len     (unsafe.vector-length names))
	  (environ '()))
      (let loop ((i       0)
		 (environ '()))
	(if (unsafe.fx= i len)
	    environ
	  (loop (unsafe.fxadd1 i)
		(cons (cons (unsafe.vector-ref names  i)
			    (unsafe.vector-ref values i))
		      environ)))))))


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
	  (%raise-errno-error who rv x)
	rv))))

(define fork
  (case-lambda
   (()
    (define who 'fork)
    (let ((rv (capi.posix-fork)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv))))
   ((parent-proc child-proc)
    (define who 'fork)
    (with-arguments-validation (who)
	((procedure  parent-proc)
	 (procedure  child-proc))
      (let ((rv (capi.posix-fork)))
	(cond ((unsafe.fxzero? rv)
	       (child-proc))
	      ((unsafe.fx< rv 0)
	       (%raise-errno-error who rv))
	      (else
	       (parent-proc rv))))))))

(define (execl filename . argv)
  (execv filename argv))

(define (execv filename argv)
  (define who 'execv)
  (with-arguments-validation (who)
      ((pathname	filename)
       (list-of-strings	argv))
    (with-pathnames ((filename.bv filename))
      (let ((rv (capi.posix-execv filename.bv (map string->utf8 argv))))
	(if (unsafe.fx< rv 0)
	    (%raise-errno-error who rv filename argv)
	  rv)))))

(define (execle filename argv . env)
  (execve filename argv env))

(define (execve filename argv env)
  (define who 'execve)
  (with-arguments-validation (who)
      ((pathname	filename)
       (list-of-strings	argv)
       (list-of-strings	env))
    (with-pathnames ((filename.bv filename))
      (let ((rv (capi.posix-execve filename.bv
				   (map string->utf8 argv)
				   (map string->utf8 env))))
	(if (unsafe.fx< rv 0)
	    (%raise-errno-error who rv filename argv env)
	  rv)))))

(define (execlp filename . argv)
  (execvp filename argv))

(define (execvp filename argv)
  (define who 'execvp)
  (with-arguments-validation (who)
      ((pathname	filename)
       (list-of-strings	argv))
    (with-pathnames ((filename.bv filename))
      (let ((rv (capi.posix-execvp filename.bv (map string->utf8 argv))))
	(if (unsafe.fx< rv 0)
	    (%raise-errno-error who rv filename argv)
	  rv)))))


;;;; process termination status

(define (waitpid pid options)
  (define who 'waitpid)
  (with-arguments-validation (who)
      ((pid	pid)
       (fixnum	options))
    (let ((rv (capi.posix-waitpid pid options)))
      (if (unsafe.fx< rv 0)
	  (%raise-errno-error who rv pid options)
	rv))))

(define (wait)
  (define who 'wait)
  (let ((rv (capi.posix-wait)))
    (if (unsafe.fx< rv 0)
	(%raise-errno-error who rv)
      rv)))

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

(define (raise-signal signum)
  (define who 'raise-signal)
  (with-arguments-validation (who)
      ((signal	signum))
    (let ((rv (capi.posix-raise signum)))
      (when (unsafe.fx< rv 0)
	(%raise-errno-error who rv signum (interprocess-signal->string signum))))))

(define (kill pid signum)
  (define who 'kill)
  (with-arguments-validation (who)
      ((pid	pid)
       (signal	signum))
    (let ((rv (capi.posix-kill pid signum)))
      (when (unsafe.fx< rv 0)
	(%raise-errno-error who rv signum (interprocess-signal->string signum))))))

(define (pause)
  (capi.posix-pause))

;;; --------------------------------------------------------------------

(define-struct struct-siginfo_t
  (si_signo	      ;0
   si_errno	      ;1
   si_code	      ;2
   si_trapno	      ;3
   si_pid	      ;4
   si_uid	      ;5
   si_status	      ;6
   si_utime	      ;7
   si_stime	      ;8
   si_value.sival_int ;9
   si_value.sival_ptr ;10
   si_int	      ;11
   si_ptr	      ;12
   si_overrun	      ;13
   si_timerid	      ;14
   si_addr	      ;15
   si_band	      ;16
   si_fd	      ;17
   si_addr_lsb))      ;18

(define (%struct-siginfo_t-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[struct-siginfo_t")
  (%display " st_signo=")		(%display (struct-siginfo_t-si_signo S))
  (%display " si_errno=")		(%display (struct-siginfo_t-si_errno S))
  (%display " si_code=")		(%display (struct-siginfo_t-si_code S))
  (%display " si_trapno=")		(%display (struct-siginfo_t-si_trapno S))
  (%display " si_pid=")			(%display (struct-siginfo_t-si_pid S))
  (%display " si_uid=")			(%display (struct-siginfo_t-si_uid S))
  (%display " si_status=")		(%display (struct-siginfo_t-si_status S))
  (%display " si_utime=")		(%display (struct-siginfo_t-si_utime S))
  (%display " si_stime=")		(%display (struct-siginfo_t-si_stime S))
  (%display " si_value.sival_int=")	(%display (struct-siginfo_t-si_value.sival_int S))
  (%display " si_value.sival_ptr=")	(%display (struct-siginfo_t-si_value.sival_ptr S))
  (%display " si_int=")			(%display (struct-siginfo_t-si_int S))
  (%display " si_ptr=")			(%display (struct-siginfo_t-si_ptr S))
  (%display " si_overrun=")		(%display (struct-siginfo_t-si_overrun S))
  (%display " si_timerid=")		(%display (struct-siginfo_t-si_timerid S))
  (%display " si_addr=")		(%display (struct-siginfo_t-si_addr S))
  (%display " si_band=")		(%display (struct-siginfo_t-si_band S))
  (%display " si_fd=")			(%display (struct-siginfo_t-si_fd S))
  (%display " si_addr_lsb=")		(%display (struct-siginfo_t-si_addr_lsb S))
  (%display "]"))

(define %make-struct-siginfo_t
  (case-lambda
   (()
    (make-struct-siginfo_t #f #f #f #f #f #f #f #f
			   #f #f #f #f #f #f #f #f #f #f #f))
   ((signo errno code trapno pid uid status
	   utime stime value.int value.ptr int ptr overrun timerid addr band fd addr_lsb)
    (make-struct-siginfo_t signo errno code trapno pid uid status
			   utime stime value.int value.ptr int ptr overrun timerid
			   addr band fd addr_lsb))))

;;; --------------------------------------------------------------------

(define sigwaitinfo
  (case-lambda
   ((signo)
    (sigwaitinfo signo (%make-struct-siginfo_t)))
   ((signo siginfo)
    (define who 'sigwaitinfo)
    (with-arguments-validation (who)
	((signal	signo)
	 (siginfo_t	siginfo))
      (let ((rv (capi.posix-sigwaitinfo signo siginfo)))
	(if (unsafe.fx<= 0 rv)
	    (values rv siginfo)
	  (%raise-errno-error who rv signo siginfo)))))))

(define sigtimedwait
  (case-lambda
   ((signo timeout)
    (sigtimedwait signo (%make-struct-siginfo_t) timeout))
   ((signo siginfo timeout)
    (define who 'sigtimedwait)
    (with-arguments-validation (who)
	((signal	signo)
	 (siginfo_t	siginfo)
	 (timespec	timeout))
      (let ((rv (capi.posix-sigtimedwait signo siginfo timeout)))
	(if (unsafe.fx<= 0 rv)
	    (values rv siginfo)
	  (%raise-errno-error who rv signo siginfo timeout)))))))

;;; --------------------------------------------------------------------

(define (signal-bub-init)
  (capi.posix-signal-bub-init))

(define (signal-bub-final)
  (capi.posix-signal-bub-final))

(define (signal-bub-acquire)
  (capi.posix-signal-bub-acquire))

(define (signal-bub-delivered? signum)
  (define who 'signal-bub-delivered?)
  (with-arguments-validation (who)
      ((signal	signum))
    (capi.posix-signal-bub-delivered? signum)))

(define (signal-bub-all-delivered)
  (let recur ((i 0))
    (cond ((= i NSIG)
	   '())
	  ((signal-bub-delivered? i)
	   (cons i (recur (+ 1 i))))
	  (else
	   (recur (+ 1 i))))))


;;;; file system inspection

(define-struct struct-stat
  ;;The  order of  the fields  must match  the order  in the  C function
  ;;"fill_stat_struct()".
  ;;
  (st_mode st_ino st_dev st_nlink
	   st_uid st_gid st_size
	   st_atime st_atime_usec
	   st_mtime st_mtime_usec
	   st_ctime st_ctime_usec
	   st_blocks st_blksize))

(define (%struct-stat-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[struct-stat")
  (%display " st_mode=#o")	(%display (number->string (struct-stat-st_mode S) 8))
  (%display " st_ino=")		(%display (struct-stat-st_ino S))
  (%display " st_dev=")		(%display (struct-stat-st_dev S))
  (%display " st_nlink=")	(%display (struct-stat-st_nlink S))
  (%display " st_uid=")		(%display (struct-stat-st_uid S))
  (%display " st_gid=")		(%display (struct-stat-st_gid S))
  (%display " st_size=")	(%display (struct-stat-st_size S))
  (%display " st_atime=")	(%display (struct-stat-st_atime S))
  (%display " st_atime_usec=")	(%display (struct-stat-st_atime_usec S))
  (%display " st_mtime=")	(%display (struct-stat-st_mtime S))
  (%display " st_mtime_usec=")	(%display (struct-stat-st_mtime_usec S))
  (%display " st_ctime=")	(%display (struct-stat-st_ctime S))
  (%display " st_ctime_usec=")	(%display (struct-stat-st_ctime_usec S))
  (%display " st_blocks=")	(%display (struct-stat-st_blocks S))
  (%display " st_blksize=")	(%display (struct-stat-st_blksize S))
  (%display "]"))

(define-inline (%make-stat)
  (make-struct-stat #f #f #f #f #f
		    #f #f #f #f #f
		    #f #f #f #f #f))

(define (stat pathname)
  (define who 'stat)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let* ((S  (%make-stat))
	     (rv (capi.posix-stat pathname.bv S)))
	(if (unsafe.fx< rv 0)
	    (%raise-errno-error/filename who rv pathname)
	  S)))))

(define (lstat pathname)
  (define who 'stat)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let* ((S  (%make-stat))
	     (rv (capi.posix-lstat pathname.bv S)))
	(if (unsafe.fx< rv 0)
	    (%raise-errno-error/filename who rv pathname)
	  S)))))

(define (fstat fd)
  (define who 'stat)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let* ((S  (%make-stat))
	   (rv (capi.posix-lstat fd S)))
      (if (unsafe.fx< rv 0)
	  (%raise-errno-error who rv fd)
	S))))

;;; --------------------------------------------------------------------

(let-syntax
    ((define-file-is (syntax-rules ()
		       ((_ ?who ?func)
			(define ?who
			  (case-lambda
			   ((pathname)
			    (?who pathname #f))
			   ((pathname follow-symlinks?)
			    (define who '?who)
			    (with-arguments-validation (who)
				((pathname  pathname))
			      (with-pathnames ((pathname.bv pathname))
				(let ((rv (?func pathname.bv follow-symlinks?)))
				  (if (boolean? rv)
				      rv
				    (%raise-errno-error/filename who rv pathname))))))))
			))))
  (define-file-is file-is-directory?		capi.posix-file-is-directory?)
  (define-file-is file-is-char-device?		capi.posix-file-is-char-device?)
  (define-file-is file-is-block-device?		capi.posix-file-is-block-device?)
  (define-file-is file-is-regular-file?		capi.posix-file-is-regular-file?)
  (define-file-is file-is-symbolic-link?	capi.posix-file-is-symbolic-link?)
  (define-file-is file-is-socket?		capi.posix-file-is-socket?)
  (define-file-is file-is-fifo?			capi.posix-file-is-fifo?)
  (define-file-is file-is-message-queue?	capi.posix-file-is-message-queue?)
  (define-file-is file-is-semaphore?		capi.posix-file-is-semaphore?)
  (define-file-is file-is-shared-memory?	capi.posix-file-is-shared-memory?))

(let-syntax
    ((define-file-is (syntax-rules ()
		       ((_ ?who ?flag)
			(define (?who mode)
			  (with-arguments-validation (?who)
			      ((fixnum  mode))
			    (unsafe.fx= ?flag (unsafe.fxand ?flag mode))))
			))))
  (define-file-is S_ISDIR	S_IFDIR)
  (define-file-is S_ISCHR	S_IFCHR)
  (define-file-is S_ISBLK	S_IFBLK)
  (define-file-is S_ISREG	S_IFREG)
  (define-file-is S_ISLNK	S_IFLNK)
  (define-file-is S_ISSOCK	S_IFSOCK)
  (define-file-is S_ISFIFO	S_IFIFO))

;;; --------------------------------------------------------------------

(define (access pathname how)
  (define who 'access)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (fixnum	  how))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-access pathname.bv how)))
	(if (boolean? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname how))))))

;;; --------------------------------------------------------------------

(define (file-readable? pathname)
  (access pathname R_OK))

(define (file-writable? pathname)
  (access pathname W_OK))

(define (file-executable? pathname)
  (access pathname X_OK))

;;; --------------------------------------------------------------------

(define (file-size pathname)
  (define who 'file-size)
  (with-arguments-validation (who)
      ((pathname pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((v (capi.posix-file-size pathname.bv)))
	(if (>= v 0)
	    v
	  (%raise-errno-error/filename who v pathname))))))

;;; --------------------------------------------------------------------

(let-syntax
    ((define-file-time (syntax-rules ()
			 ((_ ?who ?func)
			  (define (?who pathname)
			    (define who '?who)
			    (with-arguments-validation (who)
				((pathname  pathname))
			      (with-pathnames ((pathname.bv  pathname))
				(let* ((timespec (unsafe.make-clean-vector 2))
				       (rv       (?func pathname.bv timespec)))
				  (if (unsafe.fxzero? rv)
				      (+ (* #e1e9 (unsafe.vector-ref timespec 0))
					 (unsafe.vector-ref timespec 1))
				    (%raise-errno-error/filename who rv pathname))))))
			  ))))
  (define-file-time file-atime	capi.posix-file-atime)
  (define-file-time file-mtime	capi.posix-file-mtime)
  (define-file-time file-ctime	capi.posix-file-ctime))


;;;; file system attributes mutators

(define (chown pathname owner group)
  (define who 'chown)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (pid       owner)
       (gid	  group))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-chown pathname.bv owner group)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname owner group))))))

(define (fchown fd owner group)
  (define who 'fchown)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (pid		owner)
       (gid		group))
    (let ((rv (capi.posix-fchown fd owner group)))
      (if (unsafe.fxzero? rv)
	  rv
	(%raise-errno-error who rv fd owner group)))))

;;; --------------------------------------------------------------------

(define (chmod pathname mode)
  (define who 'chmod)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (fixnum    mode))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-chmod pathname.bv mode)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname mode))))))

(define (fchmod fd mode)
  (define who 'fchmod)
  (with-arguments-validation (who)
      ((file-descriptor fd)
       (fixnum		mode))
    (let ((rv (capi.posix-fchmod fd mode)))
      (if (unsafe.fxzero? rv)
	  rv
	(%raise-errno-error who rv fd mode)))))

;;; --------------------------------------------------------------------

(define (umask mask)
  (define who 'umask)
  (with-arguments-validation (who)
      ((fixnum  mask))
    (capi.posix-umask mask)))

(define (getumask)
  (capi.posix-getumask))

;;; --------------------------------------------------------------------

(define (utime pathname atime mtime)
  (define who 'utime)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (secfx     atime)
       (secfx     mtime))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-utime pathname.bv atime mtime)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname atime mtime))))))

(define (utimes pathname atime.sec atime.usec mtime.sec mtime.usec)
  (define who 'utimes)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (secfx     atime.sec)
       (usecfx    atime.usec)
       (secfx     mtime.sec)
       (usecfx    mtime.usec))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-utimes pathname.bv atime.sec atime.usec mtime.sec mtime.usec)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname atime.sec atime.usec mtime.sec mtime.usec))))))

(define (lutimes pathname atime.sec atime.usec mtime.sec mtime.usec)
  (define who 'lutimes)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (secfx     atime.sec)
       (usecfx    atime.usec)
       (secfx     mtime.sec)
       (usecfx    mtime.usec))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-lutimes pathname.bv atime.sec atime.usec mtime.sec mtime.usec)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname atime.sec atime.usec mtime.sec mtime.usec))))))

(define (futimes fd atime.sec atime.usec mtime.sec mtime.usec)
  (define who 'futimes)
  (with-arguments-validation (who)
      ((file-descriptor  fd)
       (secfx     atime.sec)
       (usecfx    atime.usec)
       (secfx     mtime.sec)
       (usecfx    mtime.usec))
    (let ((rv (capi.posix-futimes fd atime.sec atime.usec mtime.sec mtime.usec)))
      (if (unsafe.fxzero? rv)
	  rv
	(%raise-errno-error who rv fd atime.sec atime.usec mtime.sec mtime.usec)))))


;;;; hard and symbolic links

(define (link old-pathname new-pathname)
  (define who 'link)
  (with-arguments-validation (who)
      ((pathname  old-pathname)
       (pathname  new-pathname))
    (with-pathnames ((old-pathname.bv old-pathname)
		     (new-pathname.bv new-pathname))
      (let ((rv (capi.posix-link old-pathname.bv new-pathname.bv)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv old-pathname new-pathname))))))

(define (symlink file-pathname link-pathname)
  (define who 'symlink)
  (with-arguments-validation (who)
      ((pathname  file-pathname)
       (pathname  link-pathname))
    (with-pathnames ((file-pathname.bv file-pathname)
		     (link-pathname.bv link-pathname))
      (let ((rv (capi.posix-symlink file-pathname.bv link-pathname.bv)))
	(if (unsafe.fxzero? rv)
	    rv
	  (%raise-errno-error/filename who rv file-pathname link-pathname))))))

(define (readlink link-pathname)
  (define who 'readlink)
  (with-arguments-validation (who)
      ((pathname  link-pathname))
    (with-pathnames ((link-pathname.bv link-pathname))
      (let ((rv (capi.posix-readlink link-pathname.bv)))
	(if (bytevector? rv)
	    rv
	  (%raise-errno-error/filename who rv link-pathname))))))

(define (readlink/string link-pathname)
  ((filename->string-func) (readlink link-pathname)))

(define (realpath pathname)
  (define who 'realpath)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-realpath pathname.bv)))
	(if (bytevector? rv)
	    rv
	  (%raise-errno-error/filename who rv pathname))))))

(define (realpath/string pathname)
  ((filename->string-func) (realpath pathname)))

;;; --------------------------------------------------------------------

(define (unlink pathname)
  (define who 'unlink)
  (with-arguments-validation (who)
      ((pathname pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-unlink pathname.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (remove pathname)
  (define who 'remove)
  (with-arguments-validation (who)
      ((pathname pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-remove pathname.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (rename old-pathname new-pathname)
  (define who 'rename)
  (with-arguments-validation (who)
      ((pathname  old-pathname)
       (pathname  new-pathname))
    (with-pathnames ((old-pathname.bv old-pathname)
		     (new-pathname.bv new-pathname))
      (let ((rv (capi.posix-rename old-pathname.bv new-pathname.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv old-pathname new-pathname))))))


;;;; file system directories

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
	  (unless (file-is-directory? pathname #f)
	    (error who "path component is not a directory" pathname))
	(let-values (((base suffix) (split-file-name pathname)))
	  (unless (unsafe.fxzero? (unsafe.string-length base))
	    (next-component base))
	  (unless (unsafe.fxzero? (unsafe.string-length suffix))
	    (mkdir pathname mode)))))))

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

(define (rmdir pathname)
  (define who 'rmdir)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-rmdir pathname.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (getcwd)
  (define who 'getcwd)
  (let ((rv (capi.posix-getcwd)))
    (if (bytevector? rv)
	rv
      (%raise-errno-error who rv))))

(define (getcwd/string)
  (define who 'getcwd)
  (let ((rv (capi.posix-getcwd)))
    (if (bytevector? rv)
	((filename->string-func) rv)
      (%raise-errno-error who rv))))

(define (chdir pathname)
  (define who 'chdir)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-chdir pathname.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname))))))

(define (fchdir fd)
  (define who 'fchdir)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.posix-fchdir fd)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv fd)))))


;;;; inspecting file system directories

(define-struct directory-stream
  (pathname pointer fd closed?))

(define (%directory-stream-printer struct port sub-printer)
  (display "#[directory-stream" port)
  (display " pathname=\"" port) (display (directory-stream-pathname struct) port)
  (display "\"" port)
  (display " pointer="    port) (display (directory-stream-pointer  struct) port)
  (display " fd="         port) (display (directory-stream-fd       struct) port)
  (display " closed?="    port) (display (directory-stream-closed?  struct) port)
  (display "]" port))

(define directory-stream-guardian
  (let ((G (make-guardian)))
    (define (close-garbage-collected-directory-streams)
      (let ((stream (G)))
	(when stream
	  (unless (directory-stream-closed? stream)
	    (set-directory-stream-closed?! stream #t)
	    (capi.posix-closedir (directory-stream-pointer stream)))
	  (close-garbage-collected-directory-streams))))
    (post-gc-hooks (cons close-garbage-collected-directory-streams (post-gc-hooks)))
    G))

(define (opendir pathname)
  (define who 'opendir)
  (with-arguments-validation (who)
      ((pathname  pathname))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-opendir pathname.bv)))
	(if (fixnum? rv)
	    (%raise-errno-error/filename who rv pathname)
	  (begin0-let ((stream (make-directory-stream pathname rv #f #f)))
	    (directory-stream-guardian stream)))))))

(define (fdopendir fd)
  (define who 'fdopendir)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.posix-fdopendir fd)))
      (if (fixnum? rv)
	  (%raise-errno-error who rv fd)
	(begin0-let ((stream (make-directory-stream #f rv fd #f)))
	  (directory-stream-guardian stream))))))

(define (readdir stream)
  (define who 'readdir)
  (with-arguments-validation (who)
      ((directory-stream       stream)
       (open-directory-stream  stream))
    (let ((rv (capi.posix-readdir (directory-stream-pointer stream))))
      (cond ((fixnum? rv)
	     (set-directory-stream-closed?! stream #t)
	     (%raise-errno-error who rv stream))
	    ((not rv)
	     (set-directory-stream-closed?! stream #t)
	     #f)
	    (else rv)))))

(define (readdir/string stream)
  (let ((rv (readdir stream)))
    (and rv ((filename->string-func) rv))))

(define (closedir stream)
  (define who 'closedir)
  (with-arguments-validation (who)
      ((directory-stream stream))
    (unless (directory-stream-closed? stream)
      (set-directory-stream-closed?! stream #t)
      (let ((rv (capi.posix-closedir (directory-stream-pointer stream))))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error who rv stream))))))

;;; --------------------------------------------------------------------

(define (rewinddir stream)
  (define who 'rewinddir)
  (with-arguments-validation (who)
      ((directory-stream       stream)
       (open-directory-stream  stream))
    (capi.posix-rewinddir (directory-stream-pointer stream))))

(define (telldir stream)
  (define who 'telldir)
  (with-arguments-validation (who)
      ((directory-stream       stream)
       (open-directory-stream  stream))
    (capi.posix-telldir (directory-stream-pointer stream))))

(define (seekdir stream pos)
  (define who 'rewinddir)
  (with-arguments-validation (who)
      ((directory-stream	stream)
       (open-directory-stream	stream)
       (dirpos			pos))
    (capi.posix-seekdir (directory-stream-pointer stream) pos)))


;;;; file descriptors

(define (open pathname flags mode)
  (define who 'open)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (fixnum    flags)
       (fixnum    mode))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-open pathname.bv flags mode)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error/filename who rv pathname flags mode))))))

(define (close fd)
  (define who 'close)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.posix-close fd)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv fd)))))

(define read
  (case-lambda
   ((fd buffer)
    (read fd buffer #f))
   ((fd buffer size)
    (define who 'read)
    (with-arguments-validation (who)
	((file-descriptor  fd)
	 (bytevector	 buffer)
	 (fixnum/false	 size))
      (let ((rv (capi.posix-read fd buffer size)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv fd)))))))

(define (pread fd buffer size off)
  (define who 'pread)
  (with-arguments-validation (who)
      ((file-descriptor  fd)
       (bytevector	 buffer)
       (fixnum/false	 size)
       (off_t		 off))
    (let ((rv (capi.posix-pread fd buffer size off)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fd)))))

(define write
  (case-lambda
   ((fd buffer)
    (write fd buffer #f))
   ((fd buffer size)
    (define who 'write)
    (with-arguments-validation (who)
	((file-descriptor  fd)
	 (bytevector	 buffer)
	 (fixnum/false	 size))
      (let ((rv (capi.posix-write fd buffer size)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv fd)))))))

(define (pwrite fd buffer size off)
  (define who 'pwrite)
  (with-arguments-validation (who)
      ((file-descriptor  fd)
       (bytevector	 buffer)
       (fixnum/false	 size)
       (off_t		 off))
    (let ((rv (capi.posix-pwrite fd buffer size off)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fd)))))

(define (lseek fd off whence)
  (define who 'lseek)
  (with-arguments-validation (who)
      ((file-descriptor  fd)
       (off_t		 off)
       (fixnum		 whence))
    (let ((rv (capi.posix-lseek fd off whence)))
      (if (negative? rv)
	  (%raise-errno-error who rv fd off whence)
	rv))))

;;; --------------------------------------------------------------------

(define (readv fd buffers)
  (define who 'readv)
  (with-arguments-validation (who)
      ((file-descriptor		fd)
       (list-of-bytevectors	buffers))
    (let ((rv (capi.posix-readv fd buffers)))
      (if (negative? rv)
	  (%raise-errno-error who rv fd buffers)
	rv))))

(define (writev fd buffers)
  (define who 'writev)
  (with-arguments-validation (who)
      ((file-descriptor		fd)
       (list-of-bytevectors	buffers))
    (let ((rv (capi.posix-writev fd buffers)))
      (if (negative? rv)
	  (%raise-errno-error who rv fd buffers)
	rv))))

;;; --------------------------------------------------------------------

(define (select nfds read-fds write-fds except-fds sec usec)
  (define who 'select)
  (with-arguments-validation (who)
      ((select-nfds	nfds)
       (list-of-fds	read-fds)
       (list-of-fds	write-fds)
       (list-of-fds	except-fds)
       (secfx		sec)
       (usecfx		usec))
    (let ((rv (capi.posix-select nfds read-fds write-fds except-fds sec usec)))
      (if (fixnum? rv)
	  (if (unsafe.fxzero? rv)
	      (values '() '() '()) ;timeout expired
	    (%raise-errno-error who rv nfds read-fds write-fds except-fds sec usec))
	;; success, extract lists of ready fds
	(values (unsafe.vector-ref rv 0)
		(unsafe.vector-ref rv 1)
		(unsafe.vector-ref rv 2))))))

(define (select-fd fd sec usec)
  (define who 'select-fd)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (secfx		sec)
       (usecfx		usec))
    (let ((rv (capi.posix-select-fd fd sec usec)))
      (cond ((unsafe.fxzero? rv) ;timeout expired
	     (values #f #f #f))
	    ((unsafe.fx< 0 rv) ;success
	     (values (if (unsafe.fx= 1 (unsafe.fxlogand rv 1)) fd #f)
		     (if (unsafe.fx= 2 (unsafe.fxlogand rv 2)) fd #f)
		     (if (unsafe.fx= 4 (unsafe.fxlogand rv 4)) fd #f)))
	    (else
	     (%raise-errno-error who rv fd sec usec))))))

(define (select-fd-readable? fd sec usec)
  (define who 'select-fd-readable?)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (secfx		sec)
       (usecfx		usec))
    (let ((rv (capi.posix-select-fd-readable? fd sec usec)))
      (if (fixnum? rv)
	  (%raise-errno-error who rv fd sec usec)
	rv))))

(define (select-fd-writable? fd sec usec)
  (define who 'select-fd-writable?)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (secfx		sec)
       (usecfx		usec))
    (let ((rv (capi.posix-select-fd-writable? fd sec usec)))
      (if (fixnum? rv)
	  (%raise-errno-error who rv fd sec usec)
	rv))))

(define (select-fd-exceptional? fd sec usec)
  (define who 'select-fd-exceptional?)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (secfx		sec)
       (usecfx		usec))
    (let ((rv (capi.posix-select-fd-exceptional? fd sec usec)))
      (if (fixnum? rv)
	  (%raise-errno-error who rv fd sec usec)
	rv))))

(define (poll fds timeout)
  (define who 'poll)
  (with-arguments-validation (who)
      ((poll-fds	fds)
       (platform-int	timeout))
    (let ((rv (capi.posix-poll fds timeout)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fds timeout)))))

;;; --------------------------------------------------------------------

(define fcntl
  (case-lambda
   ((fd command)
    (fcntl fd command #f))
   ((fd command arg)
    (define who 'fcntl)
    (with-arguments-validation (who)
	((file-descriptor	fd)
	 (fixnum		command)
	 (fixnum/pointer/false	arg))
      (let ((rv (capi.posix-fcntl fd command arg)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv fd command arg)))))))

(define ioctl
  (case-lambda
   ((fd command)
    (ioctl fd command #f))
   ((fd command arg)
    (define who 'ioctl)
    (with-arguments-validation (who)
	((file-descriptor	fd)
	 (fixnum		command)
	 (fixnum/pointer/false	arg))
      (let ((rv (capi.posix-ioctl fd command arg)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv fd command arg)))))))

;;; --------------------------------------------------------------------

(define (dup fd)
  (define who 'dup)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.posix-dup fd)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fd)))))

(define (dup2 old new)
  (define who 'dup2)
  (with-arguments-validation (who)
      ((file-descriptor  old)
       (file-descriptor  new))
    (let ((rv (capi.posix-dup2 old new)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv old new)))))

;;; --------------------------------------------------------------------

(define (pipe)
  (define who 'pipe)
  (let ((rv (capi.posix-pipe)))
    (if (pair? rv)
	(values (unsafe.car rv) (unsafe.cdr rv))
      (%raise-errno-error who rv))))

(define (mkfifo pathname mode)
  (define who 'mkfifo)
  (with-arguments-validation (who)
      ((pathname  pathname)
       (fixnum    mode))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-mkfifo pathname.bv mode)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error/filename who rv pathname mode))))))

;;; --------------------------------------------------------------------

(define (truncate pathname length)
  (define who 'truncate)
  (with-arguments-validation (who)
      ((pathname	pathname)
       (off_t		length))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-truncate pathname.bv length)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error who rv pathname length))))))

(define (ftruncate fd length)
  (define who 'ftruncate)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (off_t		length))
    (let ((rv (capi.posix-ftruncate fd length)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv fd length)))))


;;;; file descriptor sets

(define sizeof-fd-set
  (case-lambda
   (()
    (capi.posix-sizeof-fd-set 1))
   ((count)
    (define who 'sizeof-fd-set)
    (with-arguments-validation (who)
	((positive-fixnum	count))
      (capi.posix-sizeof-fd-set count)))))

(define make-fd-set-bytevector
  (case-lambda
   (()
    (capi.posix-make-fd-set-bytevector 1))
   ((count)
    (define who 'make-fd-set-bytevector)
    (with-arguments-validation (who)
	((positive-fixnum	count))
      (capi.posix-make-fd-set-bytevector count)))))

(define make-fd-set-pointer
  (case-lambda
   (()
    (capi.posix-make-fd-set-pointer 1))
   ((count)
    (define who 'make-fd-set-pointer)
    (with-arguments-validation (who)
	((positive-fixnum	count))
      (capi.posix-make-fd-set-pointer count)))))

(define make-fd-set-memory-block
  (case-lambda
   (()
    (make-fd-set-memory-block 1))
   ((count)
    (define who 'make-fd-set-memory-block)
    (with-arguments-validation (who)
	((positive-fixnum	count))
      (let ((mb (make-memory-block (null-pointer) 0)))
	(if (capi.posix-make-fd-set-memory-block! mb count)
	    mb
	  #f))))))

;;; --------------------------------------------------------------------

(define FD_ZERO
  (case-lambda
   ((fd-set)
    (FD_ZERO fd-set 0))
   ((fd-set idx)
    (define who 'FD_ZERO)
    (with-arguments-validation (who)
	((general-buffer	fd-set)
	 (non-negative-fixnum	idx))
      (capi.posix-fd-zero fd-set idx)))))

(define FD_SET
  (case-lambda
   ((fd fd-set)
    (FD_SET fd fd-set 0))
   ((fd fd-set idx)
    (define who 'FD_SET)
    (with-arguments-validation (who)
	((file-descriptor	fd)
	 (general-buffer	fd-set)
	 (non-negative-fixnum	idx))
      (capi.posix-fd-set fd fd-set idx)))))

(define FD_CLR
  (case-lambda
   ((fd fd-set)
    (FD_CLR fd fd-set 0))
   ((fd fd-set idx)
    (define who 'FD_SET)
    (with-arguments-validation (who)
	((file-descriptor	fd)
	 (general-buffer	fd-set)
	 (non-negative-fixnum	idx))
      (capi.posix-fd-clr fd fd-set idx)))))

(define FD_ISSET
  (case-lambda
   ((fd fd-set)
    (FD_ISSET fd fd-set 0))
   ((fd fd-set idx)
    (define who 'FD_ISSET)
    (with-arguments-validation (who)
	((file-descriptor	fd)
	 (general-buffer	fd-set)
	 (non-negative-fixnum	idx))
      (capi.posix-fd-isset fd fd-set idx)))))

;;; --------------------------------------------------------------------

(define (select-from-sets nfds read-fds write-fds except-fds sec usec)
  (define who 'select-from-sets)
  (with-arguments-validation (who)
      ((select-nfds		nfds)
       (general-buffer/false	read-fds)
       (general-buffer/false	write-fds)
       (general-buffer/false	except-fds)
       (secfx			sec)
       (usecfx			usec))
    (let ((rv (capi.posix-select-from-sets nfds read-fds write-fds except-fds sec usec)))
      (if (fixnum? rv)
	  (if (unsafe.fxzero? rv)
	      (values #f #f #f) ;timeout expired
	    (%raise-errno-error who rv nfds read-fds write-fds except-fds sec usec))
	;; success
	(values read-fds write-fds except-fds)))))

(define (select-from-sets-array nfds fd-sets sec usec)
  (define who 'select-from-sets-array)
  (with-arguments-validation (who)
      ((select-nfds	nfds)
       (general-buffer	fd-sets)
       (secfx		sec)
       (usecfx		usec))
    (let ((rv (capi.posix-select-from-sets-array nfds fd-sets sec usec)))
      (if (fixnum? rv)
	  (if (unsafe.fxzero? rv)
	      #f ;timeout expired
	    (%raise-errno-error who rv nfds fd-sets sec usec))
	;; success
	fd-sets))))

;;; --------------------------------------------------------------------

(define fd-set-inspection
  (case-lambda
   ((fdsets)
    (fd-set-inspection fdsets 0))
   ((fdsets idx)
    (let loop ((i   0)
	       (set '()))
      (if (= i FD_SETSIZE)
	  (reverse set)
	(loop (+ 1 i)
	      (if (FD_ISSET i fdsets idx)
		  (cons i set)
		set)))))))


;;;; memory-mapped input/output

(define (mmap address length protect flags fd offset)
  (define who 'mmap)
  (with-arguments-validation (who)
      ((pointer/false	address)
       (platform-size_t	length)
       (fixnum		protect)
       (fixnum		flags)
       (file-descriptor	fd)
       (off_t		offset))
    (let ((rv (capi.posix-mmap address length protect flags fd offset)))
      (if (pointer? rv)
	  rv
	(%raise-errno-error who rv address length protect flags fd offset)))))

(define (munmap address length)
  (define who 'munmap)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length))
    (let ((rv (capi.posix-munmap address length)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv address length)))))

(define (msync address length flags)
  (define who 'msync)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length)
       (fixnum		flags))
    (let ((rv (capi.posix-msync address length flags)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv address length flags)))))

(define (mremap address length new-length flags)
  (define who 'mremap)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length)
       (platform-size_t	new-length)
       (fixnum		flags))
    (let ((rv (capi.posix-mremap address length new-length flags)))
      (if (pointer? rv)
	  rv
	(%raise-errno-error who rv address length new-length flags)))))

(define (madvise address length advice)
  (define who 'madvise)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length)
       (fixnum		advice))
    (let ((rv (capi.posix-madvise address length advice)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv address length advice)))))

(define (mprotect address length prot)
  (define who 'mprotect)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length)
       (fixnum		prot))
    (let ((rv (capi.posix-mprotect address length prot)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv address length)))))

(define (mlock address length)
  (define who 'mlock)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length))
    (let ((rv (capi.posix-mlock address length)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv address length)))))

(define (munlock address length)
  (define who 'munlock)
  (with-arguments-validation (who)
      ((pointer		address)
       (platform-size_t	length))
    (let ((rv (capi.posix-munlock address length)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv address length)))))

(define (mlockall flags)
  (define who 'mlock)
  (with-arguments-validation (who)
      ((fixnum		flags))
    (let ((rv (capi.posix-mlockall flags)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv flags)))))

(define (munlockall)
  (define who 'mlock)
  (let ((rv (capi.posix-munlockall)))
    (unless (unsafe.fxzero? rv)
      (%raise-errno-error who rv))))


;;;; POSIX shared memory

(define (shm-open name oflag mode)
  (define who 'shm-open)
  (with-arguments-validation (who)
      ((pathname	name)
       (fixnum		oflag)
       (fixnum		mode))
    (with-pathnames ((name.bv name))
      (let ((rv (capi.posix-shm-open name.bv oflag mode)))
	(if (unsafe.fx<= 0 rv)
	    rv
	  (%raise-errno-error who rv name oflag mode))))))

(define (shm-unlink name)
  (define who 'shm-unlink)
  (with-arguments-validation (who)
      ((pathname	name))
    (with-pathnames ((name.bv name))
      (let ((rv (capi.posix-shm-unlink name.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error who rv name))))))


;;;; POSIX semaphores

(define (sizeof-sem_t)
  (capi.posix-sizeof-sem_t))

;;; --------------------------------------------------------------------

(define sem-open
  (case-lambda
   ((name oflag mode)
    (sem-open name oflag mode 0))
   ((name oflag mode value)
    (define who 'sem-open)
    (with-arguments-validation (who)
	((pathname	name)
	 (fixnum		oflag)
	 (fixnum		mode)
	 (unsigned-int	value))
      (with-pathnames ((name.bv name))
	(let ((rv (capi.posix-sem-open name.bv oflag mode value)))
	  (if (pointer? rv)
	      rv
	    (%raise-errno-error who rv name oflag mode value))))))))

(define (sem-close sem)
  (define who 'sem-close)
  (with-arguments-validation (who)
      ((semaphore	sem))
    (let ((rv (capi.posix-sem-close sem)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sem)))))

(define (sem-unlink name)
  (define who 'sem-unlink)
  (with-arguments-validation (who)
      ((pathname	name))
    (with-pathnames ((name.bv name))
      (let ((rv (capi.posix-sem-unlink name.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error who rv name.bv))))))

;;; --------------------------------------------------------------------

(define sem-init
  (case-lambda
   ((sem pshared)
    (sem-init sem pshared 0))
   ((sem pshared value)
    (define who 'sem-init)
    (with-arguments-validation (who)
	((semaphore	sem)
	 (unsigned-int	value))
      (let ((rv (capi.posix-sem-init sem pshared value)))
	(if (pointer? rv)
	    rv
	  (%raise-errno-error who rv sem pshared value)))))))

(define (sem-destroy sem)
  (define who 'sem-destroy)
  (with-arguments-validation (who)
      ((semaphore	sem))
    (let ((rv (capi.posix-sem-destroy sem)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sem)))))

;;; --------------------------------------------------------------------

(define (sem-post sem)
  (define who 'sem-post)
  (with-arguments-validation (who)
      ((semaphore	sem))
    (let ((rv (capi.posix-sem-post sem)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sem)))))

(define (sem-wait sem)
  (define who 'sem-wait)
  (with-arguments-validation (who)
      ((semaphore	sem))
    (let ((rv (capi.posix-sem-wait sem)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sem)))))

(define (sem-trywait sem)
  (define who 'sem-trywait)
  (with-arguments-validation (who)
      ((semaphore	sem))
    (let ((rv (capi.posix-sem-close sem)))
      (if (boolean? rv)
	  rv
	(%raise-errno-error who rv sem)))))

(define (sem-timedwait sem abs-timeout)
  (define who 'sem-timedwait)
  (with-arguments-validation (who)
      ((semaphore	sem)
       (timespec	abs-timeout))
    (let ((rv (capi.posix-sem-timedwait sem abs-timeout)))
      (if (boolean? rv)
	  rv
	(%raise-errno-error who rv sem abs-timeout)))))

(define (sem-getvalue sem)
  (define who 'sem-getvalue)
  (with-arguments-validation (who)
      ((semaphore	sem))
    (let ((rv (capi.posix-sem-getvalue sem)))
      (if (pair? rv)
	  (car rv)
	(%raise-errno-error who rv sem)))))


;;;; message queues

(define-struct struct-mq-attr
  (mq_flags mq_maxmsg mq_msgsize mq_curmsgs))

(define (%struct-mq-attr-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[struct-mq-attr")
  (%display " mq_flags=")	(%display (struct-mq-attr-mq_flags   S))
  (%display " mq_maxmsg=")	(%display (struct-mq-attr-mq_maxmsg  S))
  (%display " mq_msgsize=")	(%display (struct-mq-attr-mq_msgsize S))
  (%display " mq_curmsgs=")	(%display (struct-mq-attr-mq_curmsgs S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define mq-open
  (case-lambda
   ((name oflag mode)
    (mq-open name oflag mode #f))
   ((name oflag mode attr)
    (define who 'mq-open)
    (with-arguments-validation (who)
	((pathname	name)
	 (fixnum	oflag)
	 (fixnum	mode)
	 (mq-attr/false	attr))
      (with-pathnames ((name.bv name))
	(let ((rv (capi.posix-mq-open name.bv oflag mode attr)))
	  (if (unsafe.fx<= 0 rv)
	      rv
	    (%raise-errno-error who rv name oflag mode attr))))))))

(define (mq-close mq)
  (define who 'mq-close)
  (with-arguments-validation (who)
      ((message-queue-descriptor	mq))
    (let ((rv (capi.posix-mq-close mq)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv mq)))))

(define (mq-unlink name)
  (define who 'mq-unlink)
  (with-arguments-validation (who)
      ((pathname name))
    (with-pathnames ((name.bv name))
      (let ((rv (capi.posix-mq-unlink name.bv)))
	(unless (unsafe.fxzero? rv)
	  (%raise-errno-error who rv name))))))

(define (mq-send mqd message priority)
  (define who 'mq-send)
  (with-arguments-validation (who)
      ((message-queue-descriptor	mqd)
       (bytevector			message)
       (unsigned-int			priority))
    (let ((rv (capi.posix-mq-send mqd message priority)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv mqd message priority)))))

(define (mq-timedsend mqd message priority epoch-timeout)
  (define who 'mq-timedsend)
  (with-arguments-validation (who)
      ((message-queue-descriptor	mqd)
       (bytevector			message)
       (unsigned-int			priority)
       (timespec			epoch-timeout))
    (let ((rv (capi.posix-mq-timedsend mqd message priority epoch-timeout)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv mqd message priority epoch-timeout)))))

(define (mq-receive mqd message)
  (define who 'mq-receive)
  (with-arguments-validation (who)
      ((message-queue-descriptor	mqd)
       (bytevector			message))
    (let ((rv (capi.posix-mq-receive mqd message)))
      (if (pair? rv)
	  (values (car rv) (cdr rv))
	(%raise-errno-error who rv mqd message)))))

(define (mq-timedreceive mqd message epoch-timeout)
  (define who 'mq-timedreceive)
  (with-arguments-validation (who)
      ((message-queue-descriptor	mqd)
       (bytevector			message)
       (timespec			epoch-timeout))
    (let ((rv (capi.posix-mq-timedreceive mqd message epoch-timeout)))
      (if (pair? rv)
	  (values (car rv) (cdr rv))
	(%raise-errno-error who rv mqd message epoch-timeout)))))

(define mq-setattr
  (case-lambda
   ((mqd new-attr)
    (mq-setattr mqd new-attr (make-struct-mq-attr 0 0 0 0)))
   ((mqd new-attr old-attr)
    (define who 'mq-setattr)
    (with-arguments-validation (who)
	((message-queue-descriptor	mqd)
	 (mq-attr			new-attr)
	 (mq-attr			old-attr))
      (let ((rv (capi.posix-mq-setattr mqd new-attr old-attr)))
	(if (unsafe.fxzero? rv)
	    old-attr
	  (%raise-errno-error who rv new-attr old-attr)))))))

(define mq-getattr
  (case-lambda
   ((mqd)
    (mq-getattr mqd (make-struct-mq-attr 0 0 0 0)))
   ((mqd attr)
    (define who 'mq-getattr)
    (with-arguments-validation (who)
	((message-queue-descriptor	mqd)
	 (mq-attr			attr))
      (let ((rv (capi.posix-mq-getattr mqd attr)))
	(if (unsafe.fxzero? rv)
	    attr
	  (%raise-errno-error who rv attr)))))))

;;At present this is not interfaced.
;;
;; (define (mq-notify)
;;   (define who 'mq-notify)
;;   #f)


;;;; POSIX timers

(define-struct struct-sigevent
  (sigev_notify sigev_signo))

(define (%struct-sigevent-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[struct-sigevent")
  (%display " sigev_notify=")	(%display (struct-sigevent-sigev_notify S))
  (%display " sigev_signo=")	(%display (struct-sigevent-sigev_signo  S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-struct struct-itimerspec
  (it_interval it_value))

(define (%struct-itimerspec-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-itimerspec\"")
  (%display " it_interval=")		(%display (struct-itimerspec-it_interval S))
  (%display " it_value=")		(%display (struct-itimerspec-it_value    S))
  (%display "]"))

(define %make-struct-itimerspec
  (case-lambda
   (()
    (make-struct-itimerspec (make-struct-timespec 0 0)
			    (make-struct-timespec 0 0)))
   ((it-interval it-value)
    (make-struct-itimerspec it-interval it-value))))

;;; --------------------------------------------------------------------

(define (timer-create clock-id #;sev)
  ;;At present  we only support setting  the SEV argument to  false.  In
  ;;future we may support the full "struct-sigevent" structure.
  ;;
  (define who 'timer-create)
  (with-arguments-validation (who)
      ((clockid_t		clock-id)
       #;(sigevent/false	sev))
    (let* ((sev #f)
	   (rv  (capi.posix-timer-create clock-id sev)))
      (if (pair? rv)
	  (unsafe.car rv)
	(%raise-errno-error who rv clock-id sev)))))

(define (timer-delete timer-id)
  (define who 'timer-delete)
  (with-arguments-validation (who)
      ((timer_t	timer-id))
    (let ((rv (capi.posix-timer-delete timer-id)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv timer-id)))))

(define timer-settime
  (case-lambda
   ((timer-id flags new-timer-spec)
    (timer-settime timer-id flags new-timer-spec #f))
   ((timer-id flags new-timer-spec old-timer-spec)
    (define who 'timer-settime)
    (with-arguments-validation (who)
	((timer_t		timer-id)
	 (fixnum		flags)
	 (itimerspec		new-timer-spec)
	 (itimerspec/false	old-timer-spec))
      (let ((rv (capi.posix-timer-settime timer-id flags new-timer-spec old-timer-spec)))
	(if (unsafe.fxzero? rv)
	    old-timer-spec
	  (%raise-errno-error who rv timer-id flags new-timer-spec old-timer-spec)))))))

(define timer-gettime
  (case-lambda
   ((timer-id)
    (timer-gettime timer-id (%make-struct-itimerspec)))
   ((timer-id curr-timer-spec)
    (define who 'timer-gettime)
    (with-arguments-validation (who)
	((timer_t	timer-id)
	 (itimerspec	curr-timer-spec))
      (let ((rv (capi.posix-timer-gettime timer-id curr-timer-spec)))
	(if (unsafe.fxzero? rv)
	    curr-timer-spec
	  (%raise-errno-error who rv timer-id curr-timer-spec)))))))

(define (timer-getoverrun timer-id)
  (define who 'timer-getoverrun)
  (with-arguments-validation (who)
      ((timer_t		timer-id))
    (let ((rv (capi.posix-timer-getoverrun timer-id)))
      (if (<= 0 rv)
	  rv
	(%raise-errno-error who rv timer-id)))))


;;;; clock functions

(define (clock-getres clock-id T)
  (define who 'clock-getres)
  (with-arguments-validation (who)
      ((clockid_t	clock-id)
       (timespec	T))
    (let ((rv (capi.posix-clock-getres clock-id T)))
      (if (unsafe.fxzero? rv)
	  T
	(%raise-errno-error who rv clock-id T)))))

(define (clock-gettime clock-id T)
  (define who 'clock-gettime)
  (with-arguments-validation (who)
      ((clockid_t	clock-id)
       (timespec	T))
    (let ((rv (capi.posix-clock-gettime clock-id T)))
      (if (unsafe.fxzero? rv)
	  T
	(%raise-errno-error who rv clock-id T)))))

(define (clock-settime clock-id T)
  (define who 'clock-settime)
  (with-arguments-validation (who)
      ((clockid_t	clock-id)
       (timespec	T))
    (let ((rv (capi.posix-clock-settime clock-id T)))
      (if (unsafe.fxzero? rv)
	  T
	(%raise-errno-error who rv clock-id T)))))

(define (clock-getcpuclockid pid)
  (define who 'clock-getcpuclockid)
  (with-arguments-validation (who)
      ((pid	pid))
    (let ((rv (capi.posix-clock-getcpuclockid pid)))
      (if (pair? rv)
	  (car rv)
	(%raise-errno-error who rv pid)))))


;;;; sockets

(define (make-sockaddr_un pathname)
  (define who 'make-sockaddr_un)
  (with-arguments-validation (who)
      ((pathname	pathname))
    (with-pathnames ((pathname.bv pathname))
      (capi.posix-make-sockaddr_un pathname.bv))))

(define (sockaddr_un.pathname addr)
  (define who 'sockaddr_un.pathname)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (let ((rv (capi.posix-sockaddr_un.pathname addr)))
      (if (bytevector? rv)
	  rv
	(error who "expected bytevector holding \"struct sockaddr_un\" as argument" addr)))))

(define (sockaddr_un.pathname/string addr)
  ((filename->string-func) (sockaddr_un.pathname addr)))

;;; --------------------------------------------------------------------

(define (make-sockaddr_in addr port)
  (define who 'make-sockaddr_in)
  (with-arguments-validation (who)
      ((bytevector	addr)
       (fixnum		port))
    (capi.posix-make-sockaddr_in addr port)))

(define (sockaddr_in.in_addr sockaddr)
  (define who 'sockaddr_in.in_addr)
  (with-arguments-validation (who)
      ((bytevector	sockaddr))
    (let ((rv (capi.posix-sockaddr_in.in_addr sockaddr)))
      (if (bytevector? rv)
	  rv
	(error who "expected bytevector holding \"struct sockaddr_in\" as argument" sockaddr)))))

(define (sockaddr_in.in_port sockaddr)
  (define who 'sockaddr_in.in_port)
  (with-arguments-validation (who)
      ((bytevector	sockaddr))
    (let ((rv (capi.posix-sockaddr_in.in_port sockaddr)))
      (if (fixnum? rv)
	  rv
	(error who "expected bytevector holding \"struct sockaddr_in\" as argument" sockaddr)))))

;;; --------------------------------------------------------------------

(define (make-sockaddr_in6 addr port)
  (define who 'make-sockaddr_in6)
  (with-arguments-validation (who)
      ((bytevector	addr)
       (fixnum		port))
    (capi.posix-make-sockaddr_in6 addr port)))

(define (sockaddr_in6.in6_addr sockaddr)
  (define who 'sockaddr_in6.in6_addr)
  (with-arguments-validation (who)
      ((bytevector	sockaddr))
    (let ((rv (capi.posix-sockaddr_in6.in6_addr sockaddr)))
      (if (bytevector? rv)
	  rv
	(error who "expected bytevector holding \"struct sockaddr_in6\" as argument" sockaddr)))))

(define (sockaddr_in6.in6_port sockaddr)
  (define who 'sockaddr_in6.in6_port)
  (with-arguments-validation (who)
      ((bytevector	sockaddr))
    (let ((rv (capi.posix-sockaddr_in6.in6_port sockaddr)))
      (if (fixnum? rv)
	  rv
	(error who "expected bytevector holding \"struct sockaddr_in6\" as argument" sockaddr)))))

;;; --------------------------------------------------------------------

(define (in6addr_loopback)
  (capi.posix-in6addr_loopback))

(define (in6addr_any)
  (capi.posix-in6addr_any))

;;; --------------------------------------------------------------------

(define (inet-aton dotted-quad)
  (define who 'inet-aton)
  (with-arguments-validation (who)
      ((string/bytevector  dotted-quad))
    (let ((rv (capi.posix-inet_aton (if (string? dotted-quad)
					(string->utf8 dotted-quad)
				      dotted-quad))))
      (if (bytevector? rv)
	  rv
	(error who
	  "expected string or bytevector holding an ASCII dotted quad as argument"
	  dotted-quad)))))

(define (inet-ntoa addr)
  (define who 'inet-ntoa)
  (with-arguments-validation (who)
      ((bytevector  addr))
    (capi.posix-inet_ntoa addr)))

(define (inet-ntoa/string addr)
  (utf8->string (inet-ntoa addr)))

;;; --------------------------------------------------------------------

(define (inet-pton af presentation)
  (define who 'inet-pton)
  (with-arguments-validation (who)
      ((af-inet		   af)
       (string/bytevector  presentation))
    (let ((rv (capi.posix-inet_pton af (if (string? presentation)
					   (string->utf8 presentation)
					 presentation))))
      (if (bytevector? rv)
	  rv
	(error who "invalid arguments" af presentation)))))

(define (inet-ntop af addr)
  (define who 'inet-ptoa)
  (with-arguments-validation (who)
      ((af-inet	    af)
       (bytevector  addr))
    (let ((rv (capi.posix-inet_ntop af addr)))
      (if (bytevector? rv)
	  rv
	(error who "invalid arguments" af addr)))))

(define (inet-ntop/string af addr)
  (utf8->string (inet-ntop af addr)))

;;; --------------------------------------------------------------------

(define-struct struct-hostent
  (h_name	;0, bytevector, official host name
   h_aliases	;1, list of bytevectors, host name aliases
   h_addrtype	;2, fixnum, AF_INET or AF_INET6
   h_length	;3, length of address bytevector
   h_addr_list	;4, list of bytevectors holding "struct in_addr" or "struct in6_addr"
   h_addr	;5, bytevector, first in the list of host addresses
   ))

(define (%struct-hostent-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-hostent\"")

  (%display " h_name=\"")
  (%display (utf8->string (struct-hostent-h_name S)))
  (%display "\"")

  (%display " h_aliases=")
  (%display (map utf8->string (struct-hostent-h_aliases S)))

  (%display " h_addrtype=")
  (%display (if (unsafe.fx= AF_INET (struct-hostent-h_addrtype S))
		"AF_INET" "AF_INET6"))

  (%display " h_length=")
  (%display (struct-hostent-h_length S))

  (%display " h_addr_list=")
  (%display (struct-hostent-h_addr_list S))

  (%display " h_addr=")
  (%display (struct-hostent-h_addr S))

  (%display "]"))

;;; --------------------------------------------------------------------

(define (gethostbyname hostname)
  (define who 'gethostbyname)
  (with-arguments-validation (who)
      ((string/bytevector  hostname))
    (let ((rv (capi.posix-gethostbyname (type-descriptor struct-hostent)
					(if (bytevector? hostname)
					    hostname
					  (string->utf8 hostname)))))
      (if (fixnum? rv)
	  (%raise-h_errno-error who rv hostname)
	(begin
	  (set-struct-hostent-h_addr_list! rv (reverse (struct-hostent-h_addr_list rv)))
	  rv)))))

(define (gethostbyname2 hostname addrtype)
  (define who 'gethostbyname2)
  (with-arguments-validation (who)
      ((string/bytevector  hostname)
       (af-inet		   addrtype))
    (let ((rv (capi.posix-gethostbyname2 (type-descriptor struct-hostent)
					 (if (bytevector? hostname)
					     hostname
					   (string->utf8 hostname))
					 addrtype)))
      (if (fixnum? rv)
	  (%raise-h_errno-error who rv hostname addrtype)
	(begin
	  (set-struct-hostent-h_addr_list! rv (reverse (struct-hostent-h_addr_list rv)))
	  rv)))))

(define (gethostbyaddr addr)
  (define who 'gethostbyaddr)
  (with-arguments-validation (who)
      ((bytevector  addr))
    (let ((rv (capi.posix-gethostbyaddr (type-descriptor struct-hostent) addr)))
      (if (fixnum? rv)
	  (%raise-h_errno-error who rv addr)
	(begin
	  (set-struct-hostent-h_addr_list! rv (reverse (struct-hostent-h_addr_list rv)))
	  rv)))))

(define (host-entries)
  (capi.posix-host-entries (type-descriptor struct-hostent)))

;;; --------------------------------------------------------------------

(define-struct struct-addrinfo
  (ai_flags		;0, fixnum
   ai_family		;1, fixnum
   ai_socktype		;2, fixnum
   ai_protocol		;3, fixnum
   ai_addrlen		;4, fixnum
   ai_addr		;5, bytevector
   ai_canonname))	;6, false or bytevector

(define (%struct-addrinfo-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-addrinfo\"")
  (%display " ai_flags=")	(%display (struct-addrinfo-ai_flags	S))
  (%display " ai_family=")
  (%display (let ((N (struct-addrinfo-ai_family S)))
	      (cond ((unsafe.fx= N AF_INET)	"AF_INET")
		    ((unsafe.fx= N AF_INET6)	"AF_INET6")
		    ((unsafe.fx= N AF_UNSPEC)	"AF_UNSPEC")
		    (else			N))))
  (%display " ai_socktype=")
  (%display (let ((N (struct-addrinfo-ai_socktype S)))
	      (cond ((unsafe.fx= N SOCK_STREAM)		"SOCK_STREAM")
		    ((unsafe.fx= N SOCK_DGRAM)		"SOCK_DGRAM")
		    ((unsafe.fx= N SOCK_RAW)		"SOCK_RAW")
		    ((unsafe.fx= N SOCK_RDM)		"SOCK_RDM")
		    ((unsafe.fx= N SOCK_SEQPACKET)	"SOCK_SEQPACKET")
		    ((unsafe.fx= N SOCK_DCCP)		"SOCK_DCCP")
		    (else				N))))
  (%display " ai_protocol=")	(%display (struct-addrinfo-ai_protocol	S))
  (%display " ai_addrlen=")	(%display (struct-addrinfo-ai_addrlen	S))
  (%display " ai_addr=")	(%display (struct-addrinfo-ai_addr	S))
  (%display " ai_canonname=")	(let ((name (struct-addrinfo-ai_canonname S)))
				  (if name
				      (begin
					(%display "\"")
					(%display (ascii->string name))
					(%display "\""))
				    (%display #f)))
  (%display "]"))

(define (getaddrinfo node service hints)
  (define who 'getaddrinfo)
  (with-arguments-validation (who)
      ((string/bytevector/false	node)
       (string/bytevector/false	service)
       (addrinfo/false		hints))
    (with-bytevectors/or-false ((node.bv	node)
				(service.bv	service))
      (let ((rv (capi.posix-getaddrinfo (type-descriptor struct-addrinfo)
					node.bv service.bv hints)))
	(if (fixnum? rv)
	    (raise
	     (condition (make-who-condition who)
			(make-message-condition (gai-strerror rv))
			(make-irritants-condition (list node service hints))))
	  rv)))))

(define (gai-strerror code)
  (define who 'gai-strerror)
  (with-arguments-validation (who)
      ((fixnum   code))
    (ascii->string (capi.posix-gai_strerror code))))

;;; --------------------------------------------------------------------

(define-struct struct-protoent
  (p_name p_aliases p_proto))

(define (%struct-protoent-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-protoent\"")
  (%display " p_name=\"")
  (%display (ascii->string (struct-protoent-p_name S)))
  (%display "\"")
  (%display " p_aliases=")
  (%display (map ascii->string (struct-protoent-p_aliases S)))
  (%display " p_proto=")
  (%display (struct-protoent-p_proto S))
  (%display "]"))

(define (getprotobyname name)
  (define who 'getprotobyname)
  (with-arguments-validation (who)
      ((string/bytevector	name))
    (with-bytevectors ((name.bv name))
      (let ((rv (capi.posix-getprotobyname (type-descriptor struct-protoent) name.bv)))
	(if (not rv)
	    (error who "unknown network protocol" name)
	  rv)))))

(define (getprotobynumber number)
  (define who 'getprotobynumber)
  (with-arguments-validation (who)
      ((fixnum	number))
    (let ((rv (capi.posix-getprotobynumber (type-descriptor struct-protoent) number)))
      (if (not rv)
	  (error who "unknown network protocol" number)
	rv))))

(define (protocol-entries)
  (capi.posix-protocol-entries (type-descriptor struct-protoent)))

;;; --------------------------------------------------------------------

(define-struct struct-servent
  (s_name s_aliases s_port s_proto))

(define (%struct-servent-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-servent\"")
  (%display " s_name=\"")
  (%display (ascii->string (struct-servent-s_name S)))
  (%display "\"")
  (%display " s_aliases=")
  (%display (map ascii->string (struct-servent-s_aliases S)))
  (%display " s_port=")
  (%display (struct-servent-s_port S))
  (%display " s_proto=")
  (%display (ascii->string (struct-servent-s_proto S)))
  (%display "]"))

(define servent-rtd
  (type-descriptor struct-servent))

(define (getservbyname name protocol)
  (define who 'getservbyname)
  (with-arguments-validation (who)
      ((string/bytevector	name)
       (string/bytevector	protocol))
    (with-bytevectors ((name.bv		name)
		       (protocol.bv	protocol))
      (let ((rv (capi.posix-getservbyname servent-rtd name.bv protocol.bv)))
	(if (not rv)
	    (error who "unknown network service" name protocol)
	  rv)))))

(define (getservbyport port protocol)
  (define who 'getservbyport)
  (with-arguments-validation (who)
      ((fixnum			port)
       (string/bytevector	protocol))
    (with-bytevectors ((protocol.bv protocol))
      (let ((rv (capi.posix-getservbyport servent-rtd port protocol.bv)))
	(if (not rv)
	    (error who "unknown network service" port protocol)
	  rv)))))

(define (service-entries)
  (capi.posix-service-entries servent-rtd))

;;; --------------------------------------------------------------------

(define-struct struct-netent
  (n_name n_aliases n_addrtype n_net))

(define (%struct-netent-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-netent\"")
  (%display " n_name=\"")
  (%display (ascii->string (struct-netent-n_name S)))
  (%display "\"")
  (%display " n_aliases=")
  (%display (map ascii->string (struct-netent-n_aliases S)))
  (%display " n_addrtype=")
  (%display (let ((type (struct-netent-n_addrtype S)))
	      (cond ((unsafe.fx= type AF_INET)
		     "AF_INET")
		    ((unsafe.fx= type AF_INET6)
		     "AF_INET6")
		    (else type))))
  (%display " n_net=")
  (%display (struct-netent-n_net S))
  (%display "]"))

(define netent-rtd
  (type-descriptor struct-netent))

(define (%netaddr->bytevector S)
  (let ((net (make-bytevector 4)))
    (bytevector-u32-set! net 0 (struct-netent-n_net S) (endianness big))
    (set-struct-netent-n_net! S net)
    S))

(define (getnetbyname name)
  (define who 'getnetbyname)
  (with-arguments-validation (who)
      ((string/bytevector  name))
    (with-bytevectors ((name.bv  name))
      (let ((rv (capi.posix-getnetbyname netent-rtd name.bv)))
	(if (not rv)
	    (error who "unknown network" name)
	  (%netaddr->bytevector rv))))))

(define (getnetbyaddr net type)
  (define who 'getnetbyaddr)
  (with-arguments-validation (who)
      ((netaddr  net)
       (fixnum   type))
    (let* ((net	(if (bytevector? net)
		    (bytevector-u32-ref net 0 (endianness big))
		  net))
	   (rv	(capi.posix-getnetbyaddr netent-rtd net type)))
      (if (not rv)
	  (error who "unknown network" net type)
	(%netaddr->bytevector rv)))))

(define (network-entries)
  (map %netaddr->bytevector (capi.posix-network-entries netent-rtd)))

;;; --------------------------------------------------------------------

(define (socket namespace style protocol)
  (define who 'socket)
  (with-arguments-validation (who)
      ((fixnum	namespace)
       (fixnum	style)
       (fixnum	protocol))
    (let ((rv (capi.posix-socket namespace style protocol)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv namespace style protocol)))))

(define (shutdown sock how)
  (define who 'shutdown)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		how))
    (let ((rv (capi.posix-shutdown sock how)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock how)))))

(define (socketpair namespace style protocol)
  (define who 'socketpair)
  (with-arguments-validation (who)
      ((fixnum	namespace)
       (fixnum	style)
       (fixnum	protocol))
    (let ((rv (capi.posix-socketpair namespace style protocol)))
      (if (pair? rv)
	  (values (unsafe.car rv) (unsafe.cdr rv))
	(%raise-errno-error who rv namespace style protocol)))))

;;; --------------------------------------------------------------------

(define (connect sock sockaddr)
  (define who 'connect)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (bytevector	sockaddr))
    (let ((rv (capi.posix-connect sock sockaddr)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock sockaddr)))))

(define (listen sock max-pending-conns)
  (define who 'listen)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		max-pending-conns))
    (let ((rv (capi.posix-listen sock max-pending-conns)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock max-pending-conns)))))

(define (accept sock)
  (define who 'accept)
  (with-arguments-validation (who)
      ((file-descriptor	sock))
    (let ((rv (capi.posix-accept sock)))
      (cond ((pair? rv)
	     (values (unsafe.car rv) (unsafe.cdr rv)))
	    ((unsafe.fx= rv EWOULDBLOCK)
	     (values #f #f))
	    (else
	     (%raise-errno-error who rv sock))))))

(define (bind sock sockaddr)
  (define who 'bind)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (bytevector	sockaddr))
    (let ((rv (capi.posix-bind sock sockaddr)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock sockaddr)))))

;;; --------------------------------------------------------------------

(define (getpeername sock)
  (define who 'getpeername)
  (with-arguments-validation (who)
      ((file-descriptor	sock))
    (let ((rv (capi.posix-getpeername sock)))
      (if (bytevector? rv)
	  rv
	(%raise-errno-error who rv sock)))))

(define (getsockname sock)
  (define who 'getsockname)
  (with-arguments-validation (who)
      ((file-descriptor	sock))
    (let ((rv (capi.posix-getsockname sock)))
      (if (bytevector? rv)
	  rv
	(%raise-errno-error who rv sock)))))

;;; --------------------------------------------------------------------

(define (send sock buffer size flags)
  (define who 'send)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (bytevector	buffer)
       (fixnum/false	size)
       (fixnum		flags))
    (let ((rv (capi.posix-send sock buffer size flags)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv sock buffer size flags)))))

(define (recv sock buffer size flags)
  (define who 'recv)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (bytevector	buffer)
       (fixnum/false	size)
       (fixnum		flags))
    (let ((rv (capi.posix-recv sock buffer size flags)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv sock buffer size flags)))))

;;; --------------------------------------------------------------------

(define (sendto sock buffer size flags addr)
  (define who 'sendto)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (bytevector	buffer)
       (fixnum/false	size)
       (fixnum		flags)
       (bytevector	addr))
    (let ((rv (capi.posix-sendto sock buffer size flags addr)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv sock buffer size flags addr)))))

(define (recvfrom sock buffer size flags)
  (define who 'recvfrom)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (bytevector	buffer)
       (fixnum/false	size)
       (fixnum		flags))
    (let ((rv (capi.posix-recvfrom sock buffer size flags)))
      (if (pair? rv)
	  (values (unsafe.car rv) (unsafe.cdr rv))
	(%raise-errno-error who rv sock buffer size flags)))))

;;; --------------------------------------------------------------------

(define (getsockopt sock level option optval)
  (define who 'getsockopt)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		level)
       (fixnum		option)
       (bytevector	optval))
    (let ((rv (capi.posix-getsockopt sock level option optval)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock level option optval)))))

(define (getsockopt/int sock level option)
  (define who 'getsockopt/int)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		level)
       (fixnum		option))
    (let ((rv (capi.posix-getsockopt/int sock level option)))
      (if (pair? rv)
	  (unsafe.car rv)
	(%raise-errno-error who rv sock level option)))))

(define (getsockopt/size_t sock level option)
  (define who 'getsockopt/size_t)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		level)
       (fixnum		option))
    (let ((rv (capi.posix-getsockopt/size_t sock level option)))
      (if (pair? rv)
	  (unsafe.car rv)
	(%raise-errno-error who rv sock level option)))))

;;; --------------------------------------------------------------------

(define (setsockopt sock level option optval)
  (define who 'setsockopt)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		level)
       (fixnum		option)
       (bytevector	optval))
    (let ((rv (capi.posix-setsockopt sock level option optval)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock level option optval)))))

(define (setsockopt/int sock level option optval)
  (define who 'setsockopt/int)
  (with-arguments-validation (who)
      ((file-descriptor		sock)
       (fixnum			level)
       (fixnum			option)
       (platform-int/boolean	optval))
    (let ((rv (capi.posix-setsockopt/int sock level option optval)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock level option optval)))))

(define (setsockopt/size_t sock level option optval)
  (define who 'setsockopt/size_t)
  (with-arguments-validation (who)
      ((file-descriptor	sock)
       (fixnum		level)
       (fixnum		option)
       (platform-size_t	optval))
    (let ((rv (capi.posix-setsockopt/size_t sock level option optval)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock level option optval)))))

;;; --------------------------------------------------------------------

(define (setsockopt/linger sock onoff linger)
  (define who 'setsockopt/linger)
  (with-arguments-validation (who)
      ((file-descriptor		sock)
       (boolean			onoff)
       (fixnum			linger))
    (let ((rv (capi.posix-setsockopt/linger sock onoff linger)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv sock onoff linger)))))

(define (getsockopt/linger sock)
  (define who 'getsockopt/linger)
  (with-arguments-validation (who)
      ((file-descriptor  sock))
    (let ((rv (capi.posix-getsockopt/linger sock)))
      (if (pair? rv)
	  (values (unsafe.car rv) (unsafe.cdr rv))
	(%raise-errno-error who rv sock)))))


;;;; users and groups

(define (getuid)
  (capi.posix-getuid))

(define (getgid)
  (capi.posix-getgid))

(define (geteuid)
  (capi.posix-geteuid))

(define (getegid)
  (capi.posix-getegid))

(define (getgroups)
  (let ((rv (capi.posix-getgroups)))
    (if (pair? rv)
	rv
      (%raise-errno-error 'getgroups rv))))

;;; --------------------------------------------------------------------

(define (seteuid uid)
  (define who 'seteuid)
  (with-arguments-validation (who)
      ((fixnum	uid))
    (let ((rv (capi.posix-seteuid uid)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv uid)))))

(define (setuid uid)
  (define who 'setuid)
  (with-arguments-validation (who)
      ((fixnum	uid))
    (let ((rv (capi.posix-setuid uid)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv uid)))))

(define (setreuid real-uid effective-uid)
  (define who 'setreuid)
  (with-arguments-validation (who)
      ((fixnum	real-uid)
       (fixnum	effective-uid))
    (let ((rv (capi.posix-setreuid real-uid effective-uid)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv real-uid effective-uid)))))

;;; --------------------------------------------------------------------

(define (setegid gid)
  (define who 'setegid)
  (with-arguments-validation (who)
      ((fixnum	gid))
    (let ((rv (capi.posix-setegid gid)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv gid)))))

(define (setgid gid)
  (define who 'setgid)
  (with-arguments-validation (who)
      ((fixnum	gid))
    (let ((rv (capi.posix-setgid gid)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv gid)))))

(define (setregid real-gid effective-gid)
  (define who 'setregid)
  (with-arguments-validation (who)
      ((fixnum	real-gid)
       (fixnum	effective-gid))
    (let ((rv (capi.posix-setregid real-gid effective-gid)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv real-gid effective-gid)))))

;;; --------------------------------------------------------------------

(define (getlogin)
  (capi.posix-getlogin))

(define (getlogin/string)
  (let ((rv (capi.posix-getlogin)))
    (and rv (ascii->string rv))))

;;; --------------------------------------------------------------------

(define-struct struct-passwd
  (pw_name	;0, bytevector, user login name
   pw_passwd	;1, bytevector, encrypted password
   pw_uid	;2, fixnum, user ID
   pw_gid	;3, fixnum, group ID
   pw_gecos	;4, bytevector, user data
   pw_dir	;5, bytevector, user's home directory
   pw_shell	;6, bytevector, user's default shell
   ))

(define (%struct-passwd-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-passwd\"")
  (%display " pw_name=\"")	(%display (ascii->string (struct-passwd-pw_name S)))  (%display "\"")
  (%display " pw_passwd=\"")	(%display (ascii->string (struct-passwd-pw_passwd S))) (%display "\"")
  (%display " pw_uid=")		(%display (struct-passwd-pw_uid S))
  (%display " pw_gid=")		(%display (struct-passwd-pw_gid S))
  (%display " pw_gecos=\"")	(%display (ascii->string (struct-passwd-pw_gecos S))) (%display "\"")
  (%display " pw_dir=\"")	(%display (ascii->string (struct-passwd-pw_dir S)))   (%display "\"")
  (%display " pw_shell=\"")	(%display (ascii->string (struct-passwd-pw_shell S))) (%display "\"")
  (%display "]"))

(define passwd-rtd
  (type-descriptor struct-passwd))

(define (getpwuid uid)
  (define who 'getpwuid)
  (with-arguments-validation (who)
      ((fixnum	uid))
    (capi.posix-getpwuid passwd-rtd uid)))

(define (getpwnam name)
  (define who 'getpwnam)
  (with-arguments-validation (who)
      ((string/bytevector  name))
    (with-bytevectors ((name.bv name))
      (capi.posix-getpwnam passwd-rtd name.bv))))

(define (user-entries)
  (capi.posix-user-entries passwd-rtd))

;;; --------------------------------------------------------------------

(define-struct struct-group
  (gr_name	;0, bytevector, group name
   gr_gid	;1, fixnum, group ID
   gr_mem	;2, list of bytevectors, user names
   ))

(define (%struct-group-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-group\"")
  (%display " gr_name=\"")	(%display (ascii->string (struct-group-gr_name S)))  (%display "\"")
  (%display " gr_gid=")		(%display (struct-group-gr_gid S))
  (%display " gr_mem=")
  (%display (map ascii->string (struct-group-gr_mem S)))
  (%display "]"))

(define group-rtd
  (type-descriptor struct-group))

(define (getgrgid gid)
  (define who 'getgrgid)
  (with-arguments-validation (who)
      ((fixnum	gid))
    (capi.posix-getgrgid group-rtd gid)))

(define (getgrnam name)
  (define who 'getgrnam)
  (with-arguments-validation (who)
      ((string/bytevector  name))
    (with-bytevectors ((name.bv name))
      (capi.posix-getgrnam group-rtd name.bv))))

(define (group-entries)
  (capi.posix-group-entries group-rtd))


;;;; job control

(define (ctermid)
  (capi.posix-ctermid))

(define (ctermid/string)
  (ascii->string (capi.posix-ctermid)))

;;; --------------------------------------------------------------------

(define (setsid)
  (define who 'setsid)
  (let ((rv (capi.posix-setsid)))
    (if (unsafe.fx<= 0 rv)
	rv
      (%raise-errno-error who rv))))

(define (getsid pid)
  (define who 'getsid)
  (with-arguments-validation (who)
      ((fixnum  pid))
    (let ((rv (capi.posix-getsid pid)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv pid)))))

(define (getpgrp)
  (define who 'getpgrp)
  (let ((rv (capi.posix-getpgrp)))
    (if (unsafe.fx<= 0 rv)
	rv
      (%raise-errno-error who rv))))

(define (setpgid pid pgid)
  (define who 'setpgid)
  (with-arguments-validation (who)
      ((fixnum  pid)
       (fixnum  pgid))
    (let ((rv (capi.posix-setpgid pid pgid)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv pid pgid)))))

;;; --------------------------------------------------------------------

(define (tcgetpgrp fd)
  (define who 'tcgetpgrp)
  (with-arguments-validation (who)
      ((file-descriptor	fd))
    (let ((rv (capi.posix-tcgetpgrp fd)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fd)))))

(define (tcsetpgrp fd pgid)
  (define who 'tcsetpgrp)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (fixnum		pgid))
    (let ((rv (capi.posix-tcsetpgrp fd pgid)))
      (unless (unsafe.fx<= 0 rv)
	(%raise-errno-error who rv fd pgid)))))

(define (tcgetsid fd)
  (define who 'tcgetsid)
  (with-arguments-validation (who)
      ((file-descriptor	fd))
    (let ((rv (capi.posix-tcgetsid fd)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(%raise-errno-error who rv fd)))))


;;;; time functions

(define-struct struct-timeval
  (tv_sec tv_usec))

(define (%struct-timeval-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-timeval\"")
  (%display " tv_sec=")		(%display (struct-timeval-tv_sec  S))
  (%display " tv_usec=")	(%display (struct-timeval-tv_usec S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-struct struct-timespec
  (tv_sec tv_nsec))

(define (%struct-timespec-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-timespec\"")
  (%display " tv_sec=")		(%display (struct-timespec-tv_sec  S))
  (%display " tv_nsec=")	(%display (struct-timespec-tv_nsec S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-struct struct-tms
  (tms_utime	;0, exact integer
   tms_stime	;1, exact integer
   tms_cutime	;2, exact integer
   tms_cstime	;3, exact integer
   ))

(define tms-rtd
  (type-descriptor struct-tms))

(define (%struct-tms-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-tms\"")
  (%display " tms_utime=")	(%display (struct-tms-tms_utime  S))
  (%display " tms_stime=")	(%display (struct-tms-tms_stime  S))
  (%display " tms_cutime=")	(%display (struct-tms-tms_cutime S))
  (%display " tms_cstime=")	(%display (struct-tms-tms_cstime S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-struct struct-tm
  (tm_sec	;0, exact integer
   tm_min	;1, exact integer
   tm_hour	;2, exact integer
   tm_mday	;3, exact integer
   tm_mon	;4, exact integer
   tm_year	;5, exact integer
   tm_wday	;6, exact integer
   tm_yday	;7, exact integer
   tm_isdst	;8, boolean
   tm_gmtoff	;9, exact integer
   tm_zone	;10, bytevector
   ))

(define (%struct-tm-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-tm\"")
  (%display " tm_sec=")		(%display (struct-tm-tm_sec    S))
  (%display " tm_min=")		(%display (struct-tm-tm_min    S))
  (%display " tm_hour=")	(%display (struct-tm-tm_hour   S))
  (%display " tm_mday=")	(%display (struct-tm-tm_mday   S))
  (%display " tm_mon=")		(%display (struct-tm-tm_mon    S))
  (%display " (normalised=")	(%display (+ 1 (struct-tm-tm_mon    S))) (%display ")")
  (%display " tm_year=")	(%display (struct-tm-tm_year   S))
  (%display " (normalised=")	(%display (+ 1900 (struct-tm-tm_year S))) (%display ")")
  (%display " tm_wday=")	(%display (struct-tm-tm_wday   S))
  (%display " tm_yday=")	(%display (struct-tm-tm_yday   S))
  (%display " tm_isdst=")	(%display (struct-tm-tm_isdst  S))
  (%display " tm_gmtoff=")	(%display (struct-tm-tm_gmtoff S))
  (%display " tm_zone=")	(%display (ascii->string (struct-tm-tm_zone S)))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-struct struct-itimerval
  (it_interval it_value))

(define (%struct-itimerval-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-itimerval\"")
  (%display " it_interval=")		(%display (struct-itimerval-it_interval S))
  (%display " it_value=")		(%display (struct-itimerval-it_value    S))
  (%display "]"))

(define %make-struct-itimerval
  (case-lambda
   (()
    (make-struct-itimerval (make-struct-timeval 0 0)
			   (make-struct-timeval 0 0)))
   ((interval value)
    (make-struct-itimerval interval value))))

;;; --------------------------------------------------------------------

(define (clock)
  (exact (capi.posix-clock)))

(define (times)
  (let ((S (capi.posix-times tms-rtd)))
    (set-struct-tms-tms_utime!  S (exact (struct-tms-tms_utime  S)))
    (set-struct-tms-tms_stime!  S (exact (struct-tms-tms_stime  S)))
    (set-struct-tms-tms_cutime! S (exact (struct-tms-tms_cutime S)))
    (set-struct-tms-tms_cstime! S (exact (struct-tms-tms_cstime S)))
    S))

;;; --------------------------------------------------------------------

(define (time)
  (exact (capi.posix-time)))

(define (gettimeofday)
  (define who 'gettimeofday)
  (let ((rv (capi.posix-gettimeofday (type-descriptor struct-timeval))))
    (if (struct-timeval? rv)
	rv
      (%raise-errno-error who rv))))

;;; --------------------------------------------------------------------

(define (localtime time)
  (define who 'localtime)
  (with-arguments-validation (who)
      ((exact-integer	time))
    (let ((rv (capi.posix-localtime (type-descriptor struct-tm) time)))
      (or rv
	  (%raise-posix-error who "invalid time specification" time)))))

(define (gmtime time)
  (define who 'gmtime)
  (with-arguments-validation (who)
      ((exact-integer	time))
    (let ((rv (capi.posix-gmtime (type-descriptor struct-tm) time)))
      (or rv
	  (%raise-posix-error who "invalid time specification" time)))))

(define (timelocal tm)
  (define who 'timelocal)
  (with-arguments-validation (who)
      ((struct-tm  tm))
    (let ((rv (capi.posix-timelocal tm)))
      (if rv
	  (exact rv)
	(%raise-posix-error who "invalid broken time specification" tm)))))

(define (timegm tm)
  (define who 'timegm)
  (with-arguments-validation (who)
      ((struct-tm  tm))
    (let ((rv (capi.posix-timegm tm)))
      (if rv
	  (exact rv)
	(%raise-posix-error who "invalid broken time specification" tm)))))

(define (strftime template tm)
  (define who 'strftime)
  (with-arguments-validation (who)
      ((string/bytevector  template)
       (struct-tm          tm))
    (with-bytevectors ((template.bv template))
      (let ((rv (capi.posix-strftime template.bv tm)))
	(or rv
	    (%raise-posix-error who "invalid time conversion request" template tm))))))

(define (strftime/string template tm)
  (let ((rv (strftime template tm)))
    (and rv (ascii->string rv))))

;;; --------------------------------------------------------------------

(define (nanosleep secs nsecs)
  (define who 'nanosleep)
  (with-arguments-validation (who)
      ((secs	secs)
       (nsecs	nsecs))
    (let ((rv (capi.posix-nanosleep secs nsecs)))
      (if (pair? rv)
	  (values (car rv) (cdr rv))
	(%raise-errno-error who rv secs nsecs)))))

;;; --------------------------------------------------------------------

(define (setitimer which new)
  (define who 'setitimer)
  (with-arguments-validation (who)
      ((fixnum		which)
       (itimerval	new))
    (let ((rv (capi.posix-setitimer which new)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv which new)))))

(define (getitimer which)
  (define who 'getitimer)
  (with-arguments-validation (who)
      ((fixnum		which))
    (let* ((old (make-struct-itimerval
		 (make-struct-timeval 0 0)
		 (make-struct-timeval 0 0)))
	   (rv  (capi.posix-getitimer which old)))
      (if (unsafe.fxzero? rv)
	  old
	(%raise-errno-error who rv which old)))))

(define (alarm seconds)
  (define who 'alarm)
  (with-arguments-validation (who)
      ((unsigned-int	seconds))
    (capi.posix-alarm seconds)))


;;;; system configuration

(define (sysconf parameter)
  (define who 'sysconf)
  (with-arguments-validation (who)
      ((signed-int	parameter))
    (let ((rv (capi.posix-sysconf parameter)))
      (if rv
	  (if (negative? rv)
	      (%raise-errno-error who rv parameter)
	    rv)
	rv))))

;;; --------------------------------------------------------------------

(define (pathconf pathname parameter)
  (define who 'pathconf)
  (with-arguments-validation (who)
      ((string/bytevector	pathname)
       (signed-int		parameter))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.posix-pathconf pathname.bv parameter)))
	(if rv
	    (if (negative? rv)
		(%raise-errno-error who rv pathname parameter)
	      rv)
	  rv)))))

(define (fpathconf fd parameter)
  (define who 'fpathconf)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (signed-int	parameter))
    (let ((rv (capi.posix-fpathconf fd parameter)))
      (if rv
	  (if (negative? rv)
	      (%raise-errno-error who rv fd parameter)
	    rv)
	rv))))

;;; --------------------------------------------------------------------

(define (confstr parameter)
  (define who 'confstr)
  (with-arguments-validation (who)
      ((signed-int	parameter))
    (let ((rv (capi.posix-confstr parameter)))
      (if (bytevector? rv)
	  rv
	(%raise-errno-error who rv parameter)))))

(define (confstr/string parameter)
  (latin1->string (confstr parameter)))


;;;; resources limits

(define-struct struct-rlimit
  (rlim_cur rlim_max))

(define (%struct-rlimit-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-rlimit\"")
  (%display " rlim_cur=")	(%display (struct-rlimit-rlim_cur  S))
  (let ((max (struct-rlimit-rlim_max S)))
    (%display " rlim_max=")	(%display max)
    (when (and RLIM_INFINITY (= max RLIM_INFINITY))
      (%display " (RLIM_INFINITY)")))
  (%display "]"))

(define %make-struct-rlimit
  (case-lambda
   (()
    (make-struct-rlimit 0 0))
   ((cur max)
    (make-struct-rlimit cur max))))

;;; --------------------------------------------------------------------

(define-struct struct-rusage
  (ru_utime	;0
   ru_stime	;1
   ru_maxrss	;2
   ru_ixrss	;3
   ru_idrss	;4
   ru_isrss	;5
   ru_minflt	;6
   ru_majflt	;7
   ru_nswap	;8
   ru_inblock	;9
   ru_oublock	;10
   ru_msgsnd	;11
   ru_msgrcv	;12
   ru_nsignals	;13
   ru_nvcsw	;14
   ru_nivcsw	;15
   ))

(define (%struct-rusage-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[\"struct-rusage\"")
  (%display " ru_utime=")	(%display (struct-rusage-ru_utime	S)) ;0
  (%display " ru_stime=")	(%display (struct-rusage-ru_stime	S)) ;1
  (%display " ru_maxrss=")	(%display (struct-rusage-ru_maxrss	S)) ;2
  (%display " ru_ixrss=")	(%display (struct-rusage-ru_ixrss	S)) ;3
  (%display " ru_idrss=")	(%display (struct-rusage-ru_idrss	S)) ;4
  (%display " ru_isrss=")	(%display (struct-rusage-ru_isrss	S)) ;5
  (%display " ru_minflt=")	(%display (struct-rusage-ru_minflt	S)) ;6
  (%display " ru_majflt=")	(%display (struct-rusage-ru_majflt	S)) ;7
  (%display " ru_nswap=")	(%display (struct-rusage-ru_nswap	S)) ;8
  (%display " ru_inblock=")	(%display (struct-rusage-ru_inblock	S)) ;9
  (%display " ru_oublock=")	(%display (struct-rusage-ru_oublock	S)) ;10
  (%display " ru_msgsnd=")	(%display (struct-rusage-ru_msgsnd	S)) ;11
  (%display " ru_msgrcv=")	(%display (struct-rusage-ru_msgrcv	S)) ;12
  (%display " ru_nsignals=")	(%display (struct-rusage-ru_nsignals	S)) ;13
  (%display " ru_nvcsw=")	(%display (struct-rusage-ru_nvcsw	S)) ;14
  (%display " ru_nivcsw=")	(%display (struct-rusage-ru_nivcsw	S)) ;15
  (%display "]"))

(define %make-struct-rusage
  (case-lambda
   (()
    (make-struct-rusage (make-struct-timeval 0 0) ;; ru_utime, 0
			(make-struct-timeval 0 0) ;; ru_stime, 1
			#f			  ;; ru_maxrss, 2
			#f			  ;; ru_ixrss, 3
			#f			  ;; ru_idrss, 4
			#f			  ;; ru_isrss, 5
			#f			  ;; ru_minflt, 6
			#f			  ;; ru_majflt, 7
			#f			  ;; ru_nswap, 8
			#f			  ;; ru_inblock, 9
			#f			  ;; ru_oublock, 10
			#f			  ;; ru_msgsnd, 11
			#f			  ;; ru_msgrcv, 12
			#f			  ;; ru_nsignals, 13
			#f			  ;; ru_nvcsw, 14
			#f			  ;; ru_nivcsw, 15
			))
   ((ru_utime			      ;0
     ru_stime			      ;1
     ru_maxrss			      ;2
     ru_ixrss			      ;3
     ru_idrss			      ;4
     ru_isrss			      ;5
     ru_minflt			      ;6
     ru_majflt			      ;7
     ru_nswap			      ;8
     ru_inblock			      ;9
     ru_oublock			      ;10
     ru_msgsnd			      ;11
     ru_msgrcv			      ;12
     ru_nsignals		      ;13
     ru_nvcsw			      ;14
     ru_nivcsw)			      ;15
    (make-struct-rusage ru_utime      ;0
			ru_stime      ;1
			ru_maxrss     ;2
			ru_ixrss      ;3
			ru_idrss      ;4
			ru_isrss      ;5
			ru_minflt     ;6
			ru_majflt     ;7
			ru_nswap      ;8
			ru_inblock    ;9
			ru_oublock    ;10
			ru_msgsnd     ;11
			ru_msgrcv     ;12
			ru_nsignals   ;13
			ru_nvcsw      ;14
			ru_nivcsw     ;15
			))))

;;; --------------------------------------------------------------------

(define-inline-constant RLIM_INFINITY
  (capi.posix-RLIM_INFINITY))

(define getrlimit
  (case-lambda
   ((resource)
    (getrlimit resource (%make-struct-rlimit)))
   ((resource rlimit)
    (define who 'getrlimit)
    (with-arguments-validation (who)
	((signed-int	resource)
	 (rlimit	rlimit))
      (let ((rv (capi.posix-getrlimit resource rlimit)))
	(if (unsafe.fxzero? rv)
	    rlimit
	  (%raise-errno-error who rv resource rlimit)))))))

(define (setrlimit resource rlimit)
  (define who 'setrlimit)
  (with-arguments-validation (who)
      ((signed-int	resource)
       (rlimit		rlimit))
    (let ((rv (capi.posix-setrlimit resource rlimit)))
      (unless (unsafe.fxzero? rv)
	(%raise-errno-error who rv resource rlimit)))))

;;; --------------------------------------------------------------------

(define getrusage
  (case-lambda
   ((processes)
    (getrusage processes (%make-struct-rusage)))
   ((processes rusage)
    (define who 'getrusage)
    (with-arguments-validation (who)
	((signed-int	processes)
	 (rusage	rusage))
      (let ((rv (capi.posix-getrusage processes rusage)))
	(if (unsafe.fxzero? rv)
	    rusage
	  (%raise-errno-error who rv processes rusage)))))))


;;;; splitting pathnames and search paths

(module (split-search-path
	 split-search-path-bytevector
	 split-search-path-string
	 split-pathname
	 split-pathname-bytevector
	 split-pathname-string)

  (define (split-search-path path)
    (define who 'split-search-path)
    (with-arguments-validation (who)
	((string-or-bytevector	path))
      (if (string? path)
	  (map ascii->string (split-search-path-bytevector (string->ascii path)))
	(split-search-path-bytevector path))))

  (define (split-search-path-string path)
    (define who 'split-search-path-string)
    (with-arguments-validation (who)
	((string	path))
      (map ascii->string (split-search-path-bytevector (string->ascii path)))))

  (define (split-search-path-bytevector path)
    (define who 'split-search-path-bytevector)
    (with-arguments-validation (who)
	((bytevector	path))
      (let ((path.len (unsafe.bytevector-length path)))
	(if (unsafe.fxzero? path.len)
	    '()
	  (let next-pathname ((path.index	0)
			      (pathnames	'()))
	    (if (unsafe.fx= path.index path.len)
		(reverse pathnames)
	      (let ((separator-index (%find-next-separator ASCII-COLON-FX
							   path path.index path.len)))
		(if separator-index
		    (next-pathname (unsafe.fxadd1 separator-index)
				   (if (unsafe.fx= path.index separator-index)
				       pathnames
				     (cons (%unsafe.subbytevector path path.index separator-index)
					   pathnames)))
		  (reverse (cons (%unsafe.subbytevector path path.index path.len)
				 pathnames))))))))))

  (define (split-pathname pathname)
    (define who 'split-pathname)
    (with-arguments-validation (who)
	((string-or-bytevector	pathname))
      (if (string? pathname)
	  (split-pathname-string pathname)
	(split-pathname-bytevector pathname))))

  (define (split-pathname-string pathname)
    (define who 'split-pathname-string)
    (with-arguments-validation (who)
	((string	pathname))
      (let-values (((absolute? components)
		    (split-pathname-bytevector (string->ascii pathname))))
	(values absolute? (map ascii->string components)))))

  (define (split-pathname-bytevector pathname)
    (define who 'split-pathname-bytevector)
    (with-arguments-validation (who)
	((bytevector	pathname))
      (let* ((pathname.len	(unsafe.bytevector-length pathname))
	     (components	(if (unsafe.fxzero? pathname.len)
				    '()
				  (%unsafe.bytevector-pathname-components pathname pathname.len))))
	(cond ((null? components)
	       (cond ((unsafe.fxzero? pathname.len)
		      (values #f '()))
		     ((unsafe.fx= ASCII-SLASH-FX (unsafe.bytevector-u8-ref pathname 0))
		      (values #t '()))
		     (else
		      (values #f '()))))
	      ((unsafe.fx= ASCII-SLASH-FX (unsafe.bytevector-u8-ref pathname 0))
	       (values #t components))
	      (else
	       (values #f components))))))

  (define (%unsafe.bytevector-pathname-components pathname.bv pathname.len)
    (let next-component ((pathname.index	0)
			 (components		'()))
      (if (unsafe.fx= pathname.index pathname.len)
	  (reverse components)
	(let ((separator-index (%find-next-separator ASCII-SLASH-FX
						     pathname.bv pathname.index pathname.len)))
	  (if separator-index
	      (next-component (unsafe.fxadd1 separator-index)
			      (if (unsafe.fx= pathname.index separator-index)
				  components
				(cons (%unsafe.subbytevector pathname.bv pathname.index separator-index)
				      components)))
	    (reverse (cons (%unsafe.subbytevector pathname.bv pathname.index pathname.len)
			   components)))))))

  (define (%find-next-separator separator bv bv.start bv.len)
    ;;Scan BV, from BV.START included  to BV.LEN excluded, looking for a
    ;;byte representing a slash in  ASCII encoding.  When found return a
    ;;fixnum being the index of the slash, else return false.
    ;;
    (let next-byte ((bv.index bv.start))
      (if (unsafe.fx= bv.index bv.len)
	  #f
	(if (unsafe.fx= separator (unsafe.bytevector-u8-ref bv bv.index))
	    bv.index
	  (next-byte (unsafe.fxadd1 bv.index))))))

  (define-inline (%unsafe.subbytevector src.bv src.start src.end)
    (%unsafe.subbytevector-u8/count src.bv src.start (unsafe.fx- src.end src.start)))

  (define (%unsafe.subbytevector-u8/count src.bv src.start dst.len)
    (let ((dst.bv (unsafe.make-bytevector dst.len)))
      (do ((dst.index 0         (unsafe.fx+ 1 dst.index))
	   (src.index src.start (unsafe.fx+ 1 src.index)))
	  ((unsafe.fx= dst.index dst.len)
	   dst.bv)
	(unsafe.bytevector-u8-set! dst.bv dst.index (unsafe.bytevector-u8-ref src.bv src.index)))))

  (define-inline-constant ASCII-COLON-FX
    58 #;(char->integer #\:))

  (define-inline-constant ASCII-SLASH-FX
    47 #;(char->integer #\/))

  #| end of module |# )


;;;; executable pathname

(module (vicare-executable-as-bytevector
	 vicare-executable-as-string)

  (define EXECUTABLE-BYTEVECTOR #f)
  (define EXECUTABLE-STRING	#f)

  (define (vicare-executable-as-string)
    (or EXECUTABLE-STRING
	(begin
	  (set! EXECUTABLE-STRING (let ((pathname (vicare-executable-as-bytevector)))
				    (and pathname (ascii->string pathname))))
	  EXECUTABLE-STRING)))

  (define (vicare-executable-as-bytevector)
    (or EXECUTABLE-BYTEVECTOR
	(begin
	  (set! EXECUTABLE-BYTEVECTOR (find-executable-as-bytevector (vicare-argv0)))
	  EXECUTABLE-BYTEVECTOR)))

  #| end of module |# )

(module (find-executable-as-bytevector
	 find-executable-as-string)

  (define (find-executable-as-string pathname.str)
    (define who 'find-executable-as-string)
    (with-arguments-validation (who)
	((string	pathname.str))
      (let ((pathname.bv (find-executable-as-bytevector (string->ascii pathname.str))))
	(and pathname.bv (ascii->string pathname.bv)))))

  (define (find-executable-as-bytevector pathname.bv)
    (define who 'find-executable-as-bytevector)
    (with-arguments-validation (who)
	((bytevector	pathname.bv))
      (let* ((pathname.len (unsafe.bytevector-length pathname.bv))
	     (pathname.bv  (if (%unsafe.first-char-is-slash? pathname.bv)
			       pathname.bv
			     (let ((name (%unsafe.name-if-slash-char-found pathname.bv 1 pathname.len)))
			       (if name
				   (bytevector-append (getcwd) SLASH-BV name)
				 (%unsafe.path-search pathname.bv))))))
	(and pathname.bv
	     (file-exists? pathname.bv)
	     (access pathname.bv X_OK)
	     (file-is-regular-file? pathname.bv)
	     pathname.bv))))

  (define-inline (%unsafe.first-char-is-slash? bv)
    (unsafe.fx= ASCII-SLASH-FX (unsafe.bytevector-u8-ref bv 0)))

  (define (%unsafe.name-if-slash-char-found bv bv.index bv.past)
    ;;Scan the bytes in BV from BV.INDEX included to BV.PAST excluded in
    ;;search of  one representing a  slash character in  ASCII encoding.
    ;;When found return BV itself; else return false.
    ;;
    (and (unsafe.fx< bv.index bv.past)
	 (if (unsafe.fx= ASCII-SLASH-FX
			 (unsafe.bytevector-u8-ref bv bv.index))
	     bv
	   (%unsafe.name-if-slash-char-found bv (unsafe.fxadd1 bv.index) bv.past))))

  (define (%unsafe.path-search bv)
    ;;
    ;;An unset PATH is equivalent to the search path "/bin:/usr/bin"; an
    ;;empty PATH is equivalent to the search path "."
    ;;
    (let* ((PATH	(capi.posix-getenv #ve(ascii "PATH")))
	   (PATH-LIST	(if PATH
			    (if (unsafe.fxzero? (unsafe.bytevector-length PATH))
				'(#ve(ascii "."))
			      (split-search-path-bytevector PATH))
			  DEFAULT-PATH-LIST)))
      (let next-directory ((PATH-LIST PATH-LIST))
	(if (null? PATH-LIST)
	    #f
	  (let ((pathname (bytevector-append (car PATH-LIST) SLASH-BV bv)))
	    (if (file-exists? pathname)
		pathname
	      (next-directory (cdr PATH-LIST))))))))

  (define-inline-constant DEFAULT-PATH-LIST
    '(#ve(ascii "/bin") #ve(ascii "/usr/bin")))

  (define-inline-constant ASCII-SLASH-FX
    47 #;(char->integer #\/))

  (define-inline-constant SLASH-BV
    '#vu8(47))

  #| end of module |# )


;;;; miscellaneous functions

(define (file-descriptor? obj)
  (%file-descriptor? obj))


;;;; done

(set-rtd-printer! (type-descriptor struct-stat)		%struct-stat-printer)
(set-rtd-printer! (type-descriptor directory-stream)	%directory-stream-printer)
(set-rtd-printer! (type-descriptor struct-hostent)	%struct-hostent-printer)
(set-rtd-printer! (type-descriptor struct-addrinfo)	%struct-addrinfo-printer)
(set-rtd-printer! (type-descriptor struct-protoent)	%struct-protoent-printer)
(set-rtd-printer! (type-descriptor struct-servent)	%struct-servent-printer)
(set-rtd-printer! (type-descriptor struct-netent)	%struct-netent-printer)
(set-rtd-printer! (type-descriptor struct-passwd)	%struct-passwd-printer)
(set-rtd-printer! (type-descriptor struct-group)	%struct-group-printer)
(set-rtd-printer! (type-descriptor struct-timeval)	%struct-timeval-printer)
(set-rtd-printer! (type-descriptor struct-timespec)	%struct-timespec-printer)
(set-rtd-printer! (type-descriptor struct-tms)		%struct-tms-printer)
(set-rtd-printer! (type-descriptor struct-tm)		%struct-tm-printer)
(set-rtd-printer! (type-descriptor struct-itimerval)	%struct-itimerval-printer)
(set-rtd-printer! (type-descriptor struct-mq-attr)	%struct-mq-attr-printer)
(set-rtd-printer! (type-descriptor struct-sigevent)	%struct-sigevent-printer)
(set-rtd-printer! (type-descriptor struct-itimerspec)	%struct-itimerspec-printer)
(set-rtd-printer! (type-descriptor struct-siginfo_t)	%struct-siginfo_t-printer)
(set-rtd-printer! (type-descriptor struct-rlimit)	%struct-rlimit-printer)
(set-rtd-printer! (type-descriptor struct-rusage)	%struct-rusage-printer)

(vicare-executable-as-string)

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-pathnames 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; eval: (put 'with-bytevectors/or-false 'scheme-indent-function 1)
;; End:
