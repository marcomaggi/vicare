;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: interface to C language level API
;;;Date: Fri Nov  4, 2011
;;;
;;;Abstract
;;;
;;;	For the full documentation  of the functions referenced here see
;;;	the Vicare  documentation.  This library  exports only syntaxes,
;;;	so it can be used in the source code of Vicare itself.
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare unsafe capi)
  (export

    ;; foreign functions interface
    ffi-dlopen				ffi-dlclose
    ffi-dlsym				ffi-dlerror

    ffi-malloc				ffi-free
    ffi-realloc				ffi-calloc
    ffi-with-local-storage

    ffi-memcpy				ffi-memmove
    ffi-memset				ffi-memcmp
    ffi-memory->bytevector		ffi-bytevector->memory

    ffi-strlen
    ffi-strcmp				ffi-strncmp
    ffi-strdup				ffi-strndup
    ffi-bytevector->cstring		ffi-cstring->bytevector
    ffi-cstring16->bytevector
    ffi-bytevectors->argv		ffi-argv->bytevectors
    ffi-argv-length

    ffi-pointer?			ffi-pointer-null?
    ffi-fixnum->pointer			ffi-bignum->pointer
    ffi-pointer->integer		ffi-pointer-clone
    ffi-pointer-add
    ffi-pointer-eq			ffi-pointer-neq
    ffi-pointer-lt			ffi-pointer-gt
    ffi-pointer-le			ffi-pointer-ge
    ffi-set-pointer-null!

    ffi-pointer-ref-c-uint8		ffi-pointer-ref-c-sint8
    ffi-pointer-ref-c-uint16		ffi-pointer-ref-c-sint16
    ffi-pointer-ref-c-uint32		ffi-pointer-ref-c-sint32
    ffi-pointer-ref-c-uint64		ffi-pointer-ref-c-sint64

    ffi-pointer-ref-c-float		ffi-pointer-ref-c-double
    ffi-pointer-ref-c-pointer

    ffi-pointer-ref-c-signed-char	ffi-pointer-ref-c-unsigned-char
    ffi-pointer-ref-c-signed-short	ffi-pointer-ref-c-unsigned-short
    ffi-pointer-ref-c-signed-int	ffi-pointer-ref-c-unsigned-int
    ffi-pointer-ref-c-signed-long	ffi-pointer-ref-c-unsigned-long
    ffi-pointer-ref-c-signed-long-long	ffi-pointer-ref-c-unsigned-long-long

    ffi-pointer-ref-c-size_t		ffi-pointer-ref-c-ssize_t
    ffi-pointer-ref-c-off_t		ffi-pointer-ref-c-ptrdiff_t

    ffi-pointer-set-c-uint8!		ffi-pointer-set-c-sint8!
    ffi-pointer-set-c-uint16!		ffi-pointer-set-c-sint16!
    ffi-pointer-set-c-uint32!		ffi-pointer-set-c-sint32!
    ffi-pointer-set-c-uint64!		ffi-pointer-set-c-sint64!

    ffi-pointer-set-c-float!		ffi-pointer-set-c-double!
    ffi-pointer-set-c-pointer!

    ffi-pointer-set-c-signed-char!	ffi-pointer-set-c-unsigned-char!
    ffi-pointer-set-c-signed-short!	ffi-pointer-set-c-unsigned-short!
    ffi-pointer-set-c-signed-int!	ffi-pointer-set-c-unsigned-int!
    ffi-pointer-set-c-signed-long!	ffi-pointer-set-c-unsigned-long!
    ffi-pointer-set-c-signed-long-long!	ffi-pointer-set-c-unsigned-long-long!

    ffi-pointer-set-c-size_t!		ffi-pointer-set-c-ssize_t!
    ffi-pointer-set-c-off_t!		ffi-pointer-set-c-ptrdiff_t!

    ffi-array-ref-c-uint8		ffi-array-ref-c-sint8
    ffi-array-ref-c-uint16		ffi-array-ref-c-sint16
    ffi-array-ref-c-uint32		ffi-array-ref-c-sint32
    ffi-array-ref-c-uint64		ffi-array-ref-c-sint64

    ffi-array-ref-c-float		ffi-array-ref-c-double
    ffi-array-ref-c-pointer

    ffi-array-ref-c-signed-char		ffi-array-ref-c-unsigned-char
    ffi-array-ref-c-signed-short	ffi-array-ref-c-unsigned-short
    ffi-array-ref-c-signed-int		ffi-array-ref-c-unsigned-int
    ffi-array-ref-c-signed-long		ffi-array-ref-c-unsigned-long
    ffi-array-ref-c-signed-long-long	ffi-array-ref-c-unsigned-long-long

    ffi-array-ref-c-size_t		ffi-array-ref-c-ssize_t
    ffi-array-ref-c-off_t		ffi-array-ref-c-ptrdiff_t

    ffi-array-set-c-uint8!		ffi-array-set-c-sint8!
    ffi-array-set-c-uint16!		ffi-array-set-c-sint16!
    ffi-array-set-c-uint32!		ffi-array-set-c-sint32!
    ffi-array-set-c-uint64!		ffi-array-set-c-sint64!

    ffi-array-set-c-float!		ffi-array-set-c-double!
    ffi-array-set-c-pointer!

    ffi-array-set-c-signed-char!	ffi-array-set-c-unsigned-char!
    ffi-array-set-c-signed-short!	ffi-array-set-c-unsigned-short!
    ffi-array-set-c-signed-int!		ffi-array-set-c-unsigned-int!
    ffi-array-set-c-signed-long!	ffi-array-set-c-unsigned-long!
    ffi-array-set-c-signed-long-long!	ffi-array-set-c-unsigned-long-long!

    ffi-array-set-c-size_t!		ffi-array-set-c-ssize_t!
    ffi-array-set-c-off_t!		ffi-array-set-c-ptrdiff_t!

    ;; error handling
    posix-strerror

    ;; operating system environment variables
    posix-getenv			posix-setenv
    posix-unsetenv			posix-environ
    glibc-clearenv

    ;; process identifiers
    posix-getpid			posix-getppid

    ;; executing and forking processes
    posix-fork				posix-system
    posix-execv				posix-execve
    posix-execvp

    ;; process termination status
    posix-waitpid			linux-waitid
    posix-wait
    posix-WIFEXITED			posix-WEXITSTATUS
    posix-WIFSIGNALED			posix-WTERMSIG
    posix-WCOREDUMP			posix-WIFSTOPPED
    posix-WSTOPSIG			linux-WIFCONTINUED

    ;; delivering interprocess signals
    posix-raise				posix-kill
    posix-pause
    posix-sigwaitinfo			posix-sigtimedwait
    posix-signal-bub-init		posix-signal-bub-final
    posix-signal-bub-acquire		posix-signal-bub-delivered?
    linux-signalfd			linux-read-signalfd-siginfo
    linux-timerfd-create		linux-timerfd-read
    linux-timerfd-settime		linux-timerfd-gettime

    ;; inotify
    linux-inotify-init			linux-inotify-init1
    linux-inotify-add-watch		linux-inotify-rm-watch
    linux-inotify-read

    ;; file system inspection
    posix-stat				posix-lstat
    posix-fstat
    posix-file-is-directory?		posix-file-is-char-device?
    posix-file-is-block-device?		posix-file-is-regular-file?
    posix-file-is-symbolic-link?	posix-file-is-socket?
    posix-file-is-fifo?			posix-file-is-message-queue?
    posix-file-is-semaphore?		posix-file-is-shared-memory?
    posix-file-exists?
    posix-file-size			posix-access
    posix-file-atime			posix-file-mtime
    posix-file-ctime

    ;; file system interface
    posix-chown				posix-fchown
    posix-chmod				posix-fchmod
    posix-umask				posix-getumask
    posix-utime				posix-utimes
    posix-lutimes			posix-futimes

    ;; hard and symbolic links
    posix-link				posix-symlink
    posix-readlink			posix-realpath
    posix-unlink			posix-remove
    posix-rename

    ;; file system directories
    posix-mkdir				posix-rmdir
    posix-getcwd			posix-chdir
    posix-fchdir
    posix-opendir			posix-fdopendir
    posix-readdir			posix-closedir
    posix-rewinddir			posix-telldir
    posix-seekdir			glibc-dirfd

    ;; temporary files and directories
    glibc-mkstemp			glibc-mkdtemp

    ;; file descriptors
    posix-open				posix-close
    posix-read				posix-pread
    posix-write				posix-pwrite
    posix-lseek
    posix-readv				posix-writev
    posix-select			posix-select-fd
    posix-select-fd-readable?		posix-select-fd-writable?
    posix-select-fd-exceptional?
    posix-poll
    posix-fcntl				posix-ioctl
    posix-fd-set-non-blocking
    posix-dup				posix-dup2
    posix-pipe				posix-mkfifo
    posix-truncate			posix-ftruncate
    posix-lockf

    posix-sizeof-fd-set			posix-make-fd-set-bytevector
    posix-make-fd-set-pointer		posix-make-fd-set-memory-block!
    posix-fd-zero			posix-fd-set
    posix-fd-clr			posix-fd-isset
    posix-select-from-sets		posix-select-from-sets-array

    linux-epoll-event-alloc		linux-epoll-event-size
    linux-epoll-create			linux-epoll-create1
    linux-epoll-ctl			linux-epoll-wait
    linux-epoll-event-set-events!	linux-epoll-event-ref-events
    linux-epoll-event-set-data-ptr!	linux-epoll-event-ref-data-ptr
    linux-epoll-event-set-data-fd!	linux-epoll-event-ref-data-fd
    linux-epoll-event-set-data-u32!	linux-epoll-event-ref-data-u32
    linux-epoll-event-set-data-u64!	linux-epoll-event-ref-data-u64

    ;; memory-mapped input/output
    posix-mmap				posix-munmap
    posix-msync				posix-mremap
    posix-madvise			posix-mprotect
    posix-mlock				posix-munlock
    posix-mlockall			posix-munlockall

    ;; POSIX message queues
    posix-mq-open			posix-mq-close
    posix-mq-unlink
    posix-mq-send			posix-mq-receive
    posix-mq-timedsend			posix-mq-timedreceive
    posix-mq-setattr			posix-mq-getattr
    #;posix-mq-notify

    ;; POSIX shared memory
    posix-shm-open			posix-shm-unlink

    ;; POSIX semahpores
    posix-sem-open			posix-sem-close
    posix-sem-unlink			posix-sem-init
    posix-sem-destroy			posix-sem-post
    posix-sem-wait			posix-sem-trywait
    posix-sem-timedwait			posix-sem-getvalue
    posix-sizeof-sem_t

    ;; POSIX per-process timers
    posix-timer-create			posix-timer-delete
    posix-timer-settime			posix-timer-gettime
    posix-timer-getoverrun

    ;; POSIX realtime clock functions
    posix-clock-getres			posix-clock-getcpuclockid
    posix-clock-gettime			posix-clock-settime

    ;; file system synchronisation
    glibc-sync				glibc-fsync
    glibc-fdatasync

    ;; network sockets
    glibc-if-nametoindex		glibc-if-indextoname
    glibc-if-nameindex
    posix-make-sockaddr_un		posix-sockaddr_un.pathname
    posix-make-sockaddr_in		posix-make-sockaddr_in6
    posix-sockaddr_in.in_addr		posix-sockaddr_in6.in6_addr
    posix-sockaddr_in.in_port		posix-sockaddr_in6.in6_port
    posix-in6addr_loopback		posix-in6addr_any
    posix-inet_aton			posix-inet_ntoa
    posix-inet_pton			posix-inet_ntop
    posix-htonl				posix-htons
    posix-ntohl				posix-ntohs
    posix-gethostbyname			posix-gethostbyname2
    posix-gethostbyaddr			posix-host-entries
    posix-getaddrinfo			posix-gai_strerror
    posix-getprotobyname		posix-getprotobynumber
    posix-getservbyname			posix-getservbyport
    posix-protocol-entries		posix-service-entries
    posix-getnetbyname			posix-getnetbyaddr
    posix-network-entries
    posix-socket			posix-shutdown
    posix-socketpair
    posix-connect			posix-listen
    posix-accept			posix-bind
    posix-getpeername			posix-getsockname
    posix-send				posix-recv
    posix-sendto			posix-recvfrom
    posix-setsockopt			posix-getsockopt
    posix-setsockopt/int		posix-getsockopt/int
    posix-setsockopt/size_t		posix-getsockopt/size_t
    posix-setsockopt/linger		posix-getsockopt/linger

    ;; platform API for file descriptors and Scheme ports
    platform-open-input-fd		platform-open-output-fd
    platform-open-input/output-fd	platform-close-fd
    platform-read-fd			platform-write-fd
    platform-set-position
    platform-fd-set-non-blocking-mode	platform-fd-ref-non-blocking-mode

    ;; users and groups
    posix-getuid			posix-getgid
    posix-geteuid			posix-getegid
    posix-getgroups
    posix-seteuid			posix-setuid
    posix-setegid			posix-setgid
    posix-setreuid			posix-setregid
    posix-getlogin
    posix-getpwuid			posix-getpwnam
    posix-getgrgid			posix-getgrnam
    posix-user-entries			posix-group-entries

    ;; job control
    posix-ctermid
    posix-setsid			posix-getsid
    posix-getpgrp			posix-setpgid
    posix-tcgetpgrp			posix-tcsetpgrp
    posix-tcgetsid

    ;; date and time
    posix-clock				posix-times
    posix-time				posix-gettimeofday
    posix-localtime			posix-gmtime
    posix-timelocal			posix-timegm
    posix-strftime			posix-nanosleep
    posix-setitimer			posix-getitimer
    posix-alarm

    ;; resource limits
    posix-getrlimit			posix-setrlimit
    posix-getrusage			posix-RLIM_INFINITY
    linux-prlimit

    ;; daemonisation
    linux-daemon

    ;; mathematics
    glibc-csin		glibc-ccos	glibc-ctan
    glibc-casin		glibc-cacos	glibc-catan
    glibc-cexp		glibc-clog	glibc-clog10
    glibc-csqrt		glibc-cpow
    glibc-sinh		glibc-cosh	glibc-tanh
    glibc-csinh		glibc-ccosh	glibc-ctanh
    glibc-asinh		glibc-acosh	glibc-atanh
    glibc-casinh	glibc-cacosh	glibc-catanh
    glibc-erf		glibc-erfc	glibc-tgamma	glibc-lgamma
    glibc-j0		glibc-j1	glibc-y0
    glibc-y1		glibc-jn	glibc-yn

    ;; random numbers
    glibc-rand		glibc-srand

    ;; pattern matching, globbing, regular expressions
    glibc-fnmatch	glibc-glob
    glibc-regcomp	glibc-regexec	glibc-regfree

    ;; word expansion
    glibc-wordexp

    ;; system configuration
    posix-sysconf	posix-confstr
    posix-pathconf	posix-fpathconf

    ;; iconv
    glibc-iconv-open	glibc-iconv-close
    glibc-iconv
    )
  (import (ikarus))


;;;; helpers

;; (define-syntax define-inline
;;   (syntax-rules ()
;;     ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
;;      (define-syntax ?name
;;        (syntax-rules ()
;; 	 ((_ ?arg ... . ?rest)
;; 	  (begin ?form0 ?form ...)))))))


;;;; foreign functions interface

(define-inline (ffi-dlerror)
  (foreign-call "ikrt_dlerror"))

(define-inline (ffi-dlopen libname lazy? global?)
  (foreign-call "ikrt_dlopen" libname lazy? global?))

(define-inline (ffi-dlclose ptr)
  (foreign-call "ikrt_dlclose" ptr))

(define-inline (ffi-dlsym handle name)
  (foreign-call "ikrt_dlsym" handle name))

;;; --------------------------------------------------------------------

(define-inline (ffi-malloc number-of-bytes)
  (foreign-call "ikrt_malloc" number-of-bytes))

(define-inline (ffi-realloc pointer number-of-bytes)
  (foreign-call "ikrt_realloc" pointer number-of-bytes))

(define-inline (ffi-calloc number-of-elements element-size)
  (foreign-call "ikrt_calloc" number-of-elements element-size))

(define-inline (ffi-free pointer)
  (foreign-call "ikrt_free" pointer))

(define-inline (ffi-with-local-storage lengths thunk)
  (foreign-call "ikrt_with_local_storage" lengths thunk))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer? obj)
  (foreign-call "ikrt_is_pointer" obj))

(define-inline (ffi-pointer-null? obj)
  (foreign-call "ikrt_pointer_is_null" obj))

(define-inline (ffi-fixnum->pointer obj)
  (foreign-call "ikrt_fx_to_pointer" obj))

(define-inline (ffi-bignum->pointer obj)
  (foreign-call "ikrt_bn_to_pointer" obj))

(define-inline (ffi-pointer->integer obj)
  (foreign-call "ikrt_pointer_to_int" obj))

(define-inline (ffi-pointer-clone obj)
  (foreign-call "ikrt_pointer_clone" obj))

(define-inline (ffi-pointer-add ptr delta)
  (foreign-call "ikrt_pointer_add" ptr delta))

(define-inline (ffi-pointer-eq ptr1 ptr2)
  (foreign-call "ikrt_pointer_eq" ptr1 ptr2))

(define-inline (ffi-pointer-neq ptr1 ptr2)
  (foreign-call "ikrt_pointer_neq" ptr1 ptr2))

(define-inline (ffi-pointer-lt ptr1 ptr2)
  (foreign-call "ikrt_pointer_lt" ptr1 ptr2))

(define-inline (ffi-pointer-gt ptr1 ptr2)
  (foreign-call "ikrt_pointer_gt" ptr1 ptr2))

(define-inline (ffi-pointer-le ptr1 ptr2)
  (foreign-call "ikrt_pointer_le" ptr1 ptr2))

(define-inline (ffi-pointer-ge ptr1 ptr2)
  (foreign-call "ikrt_pointer_ge" ptr1 ptr2))

(define-inline (ffi-set-pointer-null! ptr)
  (foreign-call "ikrt_pointer_set_null" ptr))

;;; --------------------------------------------------------------------

(define-inline (ffi-memcpy dst src size)
  (foreign-call "ikrt_memcpy" dst src size))

(define-inline (ffi-memcmp ptr1 ptr2 count)
  (foreign-call "ikrt_memcmp" ptr1 ptr2 count))

(define-inline (ffi-memmove dst src size)
  (foreign-call "ikrt_memmove" dst src size))

(define-inline (ffi-memset ptr byte size)
  (foreign-call "ikrt_memset" ptr byte size))

(define-inline (ffi-memory->bytevector pointer length)
  (foreign-call "ikrt_bytevector_from_memory" pointer length))

(define-inline (ffi-bytevector->memory bv)
  (foreign-call "ikrt_bytevector_to_memory" bv))

;;; --------------------------------------------------------------------

(define-inline (ffi-bytevector->cstring bv)
  (foreign-call "ikrt_bytevector_to_cstring" bv))

(define-inline (ffi-cstring->bytevector pointer count)
  (foreign-call "ikrt_bytevector_from_cstring" pointer count))

(define-inline (ffi-cstring16->bytevector pointer)
  (foreign-call "ikrt_bytevector_from_cstring16" pointer))

(define-inline (ffi-strlen pointer)
  (foreign-call "ikrt_strlen" pointer))

(define-inline (ffi-strcmp pointer1 pointer2)
  (foreign-call "ikrt_strcmp" pointer1 pointer2))

(define-inline (ffi-strncmp pointer1 pointer2 count)
  (foreign-call "ikrt_strncmp" pointer1 pointer2 count))

(define-inline (ffi-strdup pointer)
  (foreign-call "ikrt_strdup" pointer))

(define-inline (ffi-strndup pointer count)
  (foreign-call "ikrt_strndup" pointer count))

(define-inline (ffi-bytevectors->argv bvs)
  (foreign-call "ikrt_argv_from_bytevectors" bvs))

(define-inline (ffi-argv->bytevectors pointer)
  (foreign-call "ikrt_argv_to_bytevectors" pointer))

(define-inline (ffi-argv-length pointer)
  (foreign-call "ikrt_argv_length" pointer))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-ref-c-uint8 pointer offset)
  (foreign-call "ikrt_ref_uint8" pointer offset))

(define-inline (ffi-pointer-ref-c-sint8 pointer offset)
  (foreign-call "ikrt_ref_sint8" pointer offset))

(define-inline (ffi-pointer-ref-c-uint16 pointer offset)
  (foreign-call "ikrt_ref_uint16" pointer offset))

(define-inline (ffi-pointer-ref-c-sint16 pointer offset)
  (foreign-call "ikrt_ref_sint16" pointer offset))

(define-inline (ffi-pointer-ref-c-uint32 pointer offset)
  (foreign-call "ikrt_ref_uint32" pointer offset))

(define-inline (ffi-pointer-ref-c-sint32 pointer offset)
  (foreign-call "ikrt_ref_sint32" pointer offset))

(define-inline (ffi-pointer-ref-c-uint64 pointer offset)
  (foreign-call "ikrt_ref_uint64" pointer offset))

(define-inline (ffi-pointer-ref-c-sint64 pointer offset)
  (foreign-call "ikrt_ref_sint64" pointer offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-ref-c-float pointer offset)
  (foreign-call "ikrt_ref_float" pointer offset))

(define-inline (ffi-pointer-ref-c-double pointer offset)
  (foreign-call "ikrt_ref_double" pointer offset))

(define-inline (ffi-pointer-ref-c-pointer pointer offset)
  (foreign-call "ikrt_ref_pointer" pointer offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-ref-c-signed-char pointer offset)
  (foreign-call "ikrt_ref_char" pointer offset))

(define-inline (ffi-pointer-ref-c-signed-short pointer offset)
  (foreign-call "ikrt_ref_short" pointer offset))

(define-inline (ffi-pointer-ref-c-signed-int pointer offset)
  (foreign-call "ikrt_ref_int" pointer offset))

(define-inline (ffi-pointer-ref-c-signed-long pointer offset)
  (foreign-call "ikrt_ref_long" pointer offset))

(define-inline (ffi-pointer-ref-c-signed-long-long pointer offset)
  (foreign-call "ikrt_ref_longlong" pointer offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-ref-c-unsigned-char pointer offset)
  (foreign-call "ikrt_ref_uchar" pointer offset))

(define-inline (ffi-pointer-ref-c-unsigned-short pointer offset)
  (foreign-call "ikrt_ref_ushort" pointer offset))

(define-inline (ffi-pointer-ref-c-unsigned-int pointer offset)
  (foreign-call "ikrt_ref_uint" pointer offset))

(define-inline (ffi-pointer-ref-c-unsigned-long pointer offset)
  (foreign-call "ikrt_ref_ulong" pointer offset))

(define-inline (ffi-pointer-ref-c-unsigned-long-long pointer offset)
  (foreign-call "ikrt_ref_ulonglong" pointer offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-ref-c-size_t pointer offset)
  (foreign-call "ikrt_ref_size_t" pointer offset))

(define-inline (ffi-pointer-ref-c-ssize_t pointer offset)
  (foreign-call "ikrt_ref_ssize_t" pointer offset))

(define-inline (ffi-pointer-ref-c-off_t pointer offset)
  (foreign-call "ikrt_ref_off_t" pointer offset))

(define-inline (ffi-pointer-ref-c-ptrdiff_t pointer offset)
  (foreign-call "ikrt_ref_ptrdiff_t" pointer offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-set-c-uint8! pointer offset value)
  (foreign-call "ikrt_set_uint8" pointer offset value))

(define-inline (ffi-pointer-set-c-sint8! pointer offset value)
  (foreign-call "ikrt_set_sint8" pointer offset value))

(define-inline (ffi-pointer-set-c-uint16! pointer offset value)
  (foreign-call "ikrt_set_uint16" pointer offset value))

(define-inline (ffi-pointer-set-c-sint16! pointer offset value)
  (foreign-call "ikrt_set_sint16" pointer offset value))

(define-inline (ffi-pointer-set-c-uint32! pointer offset value)
  (foreign-call "ikrt_set_uint32" pointer offset value))

(define-inline (ffi-pointer-set-c-sint32! pointer offset value)
  (foreign-call "ikrt_set_sint32" pointer offset value))

(define-inline (ffi-pointer-set-c-uint64! pointer offset value)
  (foreign-call "ikrt_set_uint64" pointer offset value))

(define-inline (ffi-pointer-set-c-sint64! pointer offset value)
  (foreign-call "ikrt_set_sint64" pointer offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-set-c-float! pointer offset value)
  (foreign-call "ikrt_set_float" pointer offset value))

(define-inline (ffi-pointer-set-c-double! pointer offset value)
  (foreign-call "ikrt_set_double" pointer offset value))

(define-inline (ffi-pointer-set-c-pointer! pointer offset value)
  (foreign-call "ikrt_set_pointer" pointer offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-set-c-signed-char! pointer offset value)
  (foreign-call "ikrt_set_char" pointer offset value))

(define-inline (ffi-pointer-set-c-signed-short! pointer offset value)
  (foreign-call "ikrt_set_short" pointer offset value))

(define-inline (ffi-pointer-set-c-signed-int! pointer offset value)
  (foreign-call "ikrt_set_int" pointer offset value))

(define-inline (ffi-pointer-set-c-signed-long! pointer offset value)
  (foreign-call "ikrt_set_long" pointer offset value))

(define-inline (ffi-pointer-set-c-signed-long-long! pointer offset value)
  (foreign-call "ikrt_set_longlong" pointer offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-set-c-unsigned-char! pointer offset value)
  (foreign-call "ikrt_set_uchar" pointer offset value))

(define-inline (ffi-pointer-set-c-unsigned-short! pointer offset value)
  (foreign-call "ikrt_set_ushort" pointer offset value))

(define-inline (ffi-pointer-set-c-unsigned-int! pointer offset value)
  (foreign-call "ikrt_set_uint" pointer offset value))

(define-inline (ffi-pointer-set-c-unsigned-long! pointer offset value)
  (foreign-call "ikrt_set_ulong" pointer offset value))

(define-inline (ffi-pointer-set-c-unsigned-long-long! pointer offset value)
  (foreign-call "ikrt_set_ulonglong" pointer offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-pointer-set-c-size_t! pointer offset value)
  (foreign-call "ikrt_set_size_t" pointer offset value))

(define-inline (ffi-pointer-set-c-ssize_t! pointer offset value)
  (foreign-call "ikrt_set_ssize_t" pointer offset value))

(define-inline (ffi-pointer-set-c-off_t! pointer offset value)
  (foreign-call "ikrt_set_off_t" pointer offset value))

(define-inline (ffi-pointer-set-c-ptrdiff_t! pointer offset value)
  (foreign-call "ikrt_set_ptrdiff_t" pointer offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-ref-c-uint8 array offset)
  (foreign-call "ikrt_array_ref_uint8" array offset))

(define-inline (ffi-array-ref-c-sint8 array offset)
  (foreign-call "ikrt_array_ref_sint8" array offset))

(define-inline (ffi-array-ref-c-uint16 array offset)
  (foreign-call "ikrt_array_ref_uint16" array offset))

(define-inline (ffi-array-ref-c-sint16 array offset)
  (foreign-call "ikrt_array_ref_sint16" array offset))

(define-inline (ffi-array-ref-c-uint32 array offset)
  (foreign-call "ikrt_array_ref_uint32" array offset))

(define-inline (ffi-array-ref-c-sint32 array offset)
  (foreign-call "ikrt_array_ref_sint32" array offset))

(define-inline (ffi-array-ref-c-uint64 array offset)
  (foreign-call "ikrt_array_ref_uint64" array offset))

(define-inline (ffi-array-ref-c-sint64 array offset)
  (foreign-call "ikrt_array_ref_sint64" array offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-ref-c-float array offset)
  (foreign-call "ikrt_array_ref_float" array offset))

(define-inline (ffi-array-ref-c-double array offset)
  (foreign-call "ikrt_array_ref_double" array offset))

(define-inline (ffi-array-ref-c-pointer array offset)
  (foreign-call "ikrt_array_ref_pointer" array offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-ref-c-signed-char array offset)
  (foreign-call "ikrt_array_ref_char" array offset))

(define-inline (ffi-array-ref-c-signed-short array offset)
  (foreign-call "ikrt_array_ref_short" array offset))

(define-inline (ffi-array-ref-c-signed-int array offset)
  (foreign-call "ikrt_array_ref_int" array offset))

(define-inline (ffi-array-ref-c-signed-long array offset)
  (foreign-call "ikrt_array_ref_long" array offset))

(define-inline (ffi-array-ref-c-signed-long-long array offset)
  (foreign-call "ikrt_array_ref_longlong" array offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-ref-c-unsigned-char array offset)
  (foreign-call "ikrt_array_ref_uchar" array offset))

(define-inline (ffi-array-ref-c-unsigned-short array offset)
  (foreign-call "ikrt_array_ref_ushort" array offset))

(define-inline (ffi-array-ref-c-unsigned-int array offset)
  (foreign-call "ikrt_array_ref_uint" array offset))

(define-inline (ffi-array-ref-c-unsigned-long array offset)
  (foreign-call "ikrt_array_ref_ulong" array offset))

(define-inline (ffi-array-ref-c-unsigned-long-long array offset)
  (foreign-call "ikrt_array_ref_ulonglong" array offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-ref-c-size_t array offset)
  (foreign-call "ikrt_array_ref_size_t" array offset))

(define-inline (ffi-array-ref-c-ssize_t array offset)
  (foreign-call "ikrt_array_ref_ssize_t" array offset))

(define-inline (ffi-array-ref-c-off_t array offset)
  (foreign-call "ikrt_array_ref_off_t" array offset))

(define-inline (ffi-array-ref-c-ptrdiff_t array offset)
  (foreign-call "ikrt_array_ref_ptrdiff_t" array offset))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-set-c-uint8! array offset value)
  (foreign-call "ikrt_array_set_uint8" array offset value))

(define-inline (ffi-array-set-c-sint8! array offset value)
  (foreign-call "ikrt_array_set_sint8" array offset value))

(define-inline (ffi-array-set-c-uint16! array offset value)
  (foreign-call "ikrt_array_set_uint16" array offset value))

(define-inline (ffi-array-set-c-sint16! array offset value)
  (foreign-call "ikrt_array_set_sint16" array offset value))

(define-inline (ffi-array-set-c-uint32! array offset value)
  (foreign-call "ikrt_array_set_uint32" array offset value))

(define-inline (ffi-array-set-c-sint32! array offset value)
  (foreign-call "ikrt_array_set_sint32" array offset value))

(define-inline (ffi-array-set-c-uint64! array offset value)
  (foreign-call "ikrt_array_set_uint64" array offset value))

(define-inline (ffi-array-set-c-sint64! array offset value)
  (foreign-call "ikrt_array_set_sint64" array offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-set-c-float! array offset value)
  (foreign-call "ikrt_array_set_float" array offset value))

(define-inline (ffi-array-set-c-double! array offset value)
  (foreign-call "ikrt_array_set_double" array offset value))

(define-inline (ffi-array-set-c-pointer! array offset value)
  (foreign-call "ikrt_array_set_pointer" array offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-set-c-signed-char! array offset value)
  (foreign-call "ikrt_array_set_char" array offset value))

(define-inline (ffi-array-set-c-signed-short! array offset value)
  (foreign-call "ikrt_array_set_short" array offset value))

(define-inline (ffi-array-set-c-signed-int! array offset value)
  (foreign-call "ikrt_array_set_int" array offset value))

(define-inline (ffi-array-set-c-signed-long! array offset value)
  (foreign-call "ikrt_array_set_long" array offset value))

(define-inline (ffi-array-set-c-signed-long-long! array offset value)
  (foreign-call "ikrt_array_set_longlong" array offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-set-c-unsigned-char! array offset value)
  (foreign-call "ikrt_array_set_uchar" array offset value))

(define-inline (ffi-array-set-c-unsigned-short! array offset value)
  (foreign-call "ikrt_array_set_ushort" array offset value))

(define-inline (ffi-array-set-c-unsigned-int! array offset value)
  (foreign-call "ikrt_array_set_uint" array offset value))

(define-inline (ffi-array-set-c-unsigned-long! array offset value)
  (foreign-call "ikrt_array_set_ulong" array offset value))

(define-inline (ffi-array-set-c-unsigned-long-long! array offset value)
  (foreign-call "ikrt_array_set_ulonglong" array offset value))

;;; --------------------------------------------------------------------

(define-inline (ffi-array-set-c-size_t! array offset value)
  (foreign-call "ikrt_array_set_size_t" array offset value))

(define-inline (ffi-array-set-c-ssize_t! array offset value)
  (foreign-call "ikrt_array_set_ssize_t" array offset value))

(define-inline (ffi-array-set-c-off_t! array offset value)
  (foreign-call "ikrt_array_set_off_t" array offset value))

(define-inline (ffi-array-set-c-ptrdiff_t! array offset value)
  (foreign-call "ikrt_array_set_ptrdiff_t" array offset value))


;;;; error handling

(define-inline (posix-strerror errno)
  (foreign-call "ikrt_posix_strerror" errno))



;;;; operating system environment variables

(define-inline (posix-getenv varname-bv)
  (foreign-call "ikrt_posix_getenv" varname-bv))

(define-inline (posix-setenv varname-bv value-bv replace-bool)
  (foreign-call "ikrt_posix_setenv" varname-bv value-bv replace-bool))

(define-inline (posix-unsetenv varname-bv)
  (foreign-call "ikrt_posix_unsetenv" varname-bv))

(define-inline (glibc-clearenv)
  (foreign-call "ikrt_glibc_clearenv"))

(define-inline (posix-environ)
  (foreign-call "ikrt_posix_environ"))


;;;; process identifiers

(define-inline (posix-getpid)
  (foreign-call "ikrt_posix_getpid"))

(define-inline (posix-getppid)
  (foreign-call "ikrt_posix_getppid"))


;;;; executing and forking processes

(define-inline (posix-fork)
  (foreign-call "ikrt_posix_fork"))

(define-inline (posix-system command-bv)
  (foreign-call "ikrt_posix_system" command-bv))

(define-inline (posix-execv filename-bv argv-list)
  (foreign-call "ikrt_posix_execv" filename-bv argv-list))

(define-inline (posix-execve filename-bv argv-list env-list)
  (foreign-call "ikrt_posix_execve" filename-bv argv-list env-list))

(define-inline (posix-execvp filename-bv argv-list)
  (foreign-call "ikrt_posix_execvp" filename-bv argv-list))


;;;; porcess termination status

(define-inline (posix-waitpid pid options)
  (foreign-call "ikrt_posix_waitpid" pid options))

(define-inline (posix-wait)
  (foreign-call "ikrt_posix_wait"))

(define-inline (linux-waitid idtype id info options)
  (foreign-call "ikrt_linux_waitid" idtype id info options))

(define-inline (posix-WIFEXITED status)
  (foreign-call "ikrt_posix_WIFEXITED" status))

(define-inline (posix-WEXITSTATUS status)
  (foreign-call "ikrt_posix_WEXITSTATUS" status))

(define-inline (posix-WIFSIGNALED status)
  (foreign-call "ikrt_posix_WIFSIGNALED" status))

(define-inline (posix-WTERMSIG status)
  (foreign-call "ikrt_posix_WTERMSIG" status))

(define-inline (posix-WCOREDUMP status)
  (foreign-call "ikrt_posix_WCOREDUMP" status))

(define-inline (posix-WIFSTOPPED status)
  (foreign-call "ikrt_posix_WIFSTOPPED" status))

(define-inline (posix-WSTOPSIG status)
  (foreign-call "ikrt_posix_WSTOPSIG" status))

(define-inline (linux-WIFCONTINUED status)
  (foreign-call "ikrt_linux_WIFCONTINUED" status))


;;;; delivering interprocess signals

(define-inline (posix-raise signum)
  (foreign-call "ikrt_posix_raise" signum))

(define-inline (posix-kill pid signum)
  (foreign-call "ikrt_posix_kill" pid signum))

(define-inline (posix-pause)
  (foreign-call "ikrt_posix_pause"))

;;; --------------------------------------------------------------------

(define-inline (posix-sigwaitinfo signo siginfo)
  (foreign-call "ikrt_posix_sigwaitinfo" signo siginfo))

(define-inline (posix-sigtimedwait signo siginfo timeout)
  (foreign-call "ikrt_posix_sigtimedwait" signo siginfo timeout))

;;; --------------------------------------------------------------------

(define-inline (posix-signal-bub-init)
  (foreign-call "ikrt_posix_signal_bub_init"))

(define-inline (posix-signal-bub-final)
  (foreign-call "ikrt_posix_signal_bub_final"))

(define-inline (posix-signal-bub-acquire)
  (foreign-call "ikrt_posix_signal_bub_acquire"))

(define-inline (posix-signal-bub-delivered? signum)
  (foreign-call "ikrt_posix_signal_bub_delivered" signum))

;;; --------------------------------------------------------------------

(define-inline (linux-signalfd fd mask flags)
  (foreign-call "ikrt_linux_signalfd" fd mask flags))

(define-inline (linux-read-signalfd-siginfo fd info)
  (foreign-call "ikrt_linux_read_signalfd_siginfo" fd info))


;;;; timer file descriptors

(define-inline (linux-timerfd-create clockid flags)
  (foreign-call "ikrt_linux_timerfd_create" clockid flags))

(define-inline (linux-timerfd-settime fd flags new old)
  (foreign-call "ikrt_linux_timerfd_settime" fd flags new old))

(define-inline (linux-timerfd-gettime fd curr)
  (foreign-call "ikrt_linux_timerfd_gettime" fd curr))

(define-inline (linux-timerfd-read fd)
  (foreign-call "ikrt_linux_timerfd_read" fd))


;;;; inotify, monitoring file system events

(define-inline (linux-inotify-init)
  (foreign-call "ikrt_linux_inotify_init"))

(define-inline (linux-inotify-init1 flags)
  (foreign-call "ikrt_linux_inotify_init1" flags))

(define-inline (linux-inotify-add-watch fd pathname mask)
  (foreign-call "ikrt_linux_inotify_add_watch" fd pathname mask))

(define-inline (linux-inotify-rm-watch fd wd)
  (foreign-call "ikrt_linux_inotify_rm_watch" fd wd))

(define-inline (linux-inotify-read fd event)
  (foreign-call "ikrt_linux_inotify_read" fd event))


;;;; file system inspection

(define-inline (posix-stat filename-bv stat-struct)
  (foreign-call "ikrt_posix_stat" filename-bv stat-struct))

(define-inline (posix-lstat filename-bv stat-struct)
  (foreign-call "ikrt_posix_lstat" filename-bv stat-struct))

(define-inline (posix-fstat fd stat-struct)
  (foreign-call "ikrt_posix_fstat" fd stat-struct))

;;; --------------------------------------------------------------------

(define-inline (posix-file-is-directory? pathname-bv follow)
  (foreign-call "ikrt_file_is_directory" pathname-bv follow))

(define-inline (posix-file-is-char-device? pathname-bv follow)
  (foreign-call "ikrt_file_is_char_device" pathname-bv follow))

(define-inline (posix-file-is-block-device? pathname-bv follow)
  (foreign-call "ikrt_file_is_block_device" pathname-bv follow))

(define-inline (posix-file-is-regular-file? pathname-bv follow)
  (foreign-call "ikrt_file_is_regular_file" pathname-bv follow))

(define-inline (posix-file-is-symbolic-link? pathname-bv follow)
  (foreign-call "ikrt_file_is_symbolic_link" pathname-bv follow))

(define-inline (posix-file-is-socket? pathname-bv follow)
  (foreign-call "ikrt_file_is_socket" pathname-bv follow))

(define-inline (posix-file-is-fifo? pathname-bv follow)
  (foreign-call "ikrt_file_is_fifo" pathname-bv follow))

;;; --------------------------------------------------------------------

(define-inline (posix-file-is-message-queue? pathname-bv follow)
  (foreign-call "ikrt_file_is_message_queue" pathname-bv follow))

(define-inline (posix-file-is-semaphore? pathname-bv follow)
  (foreign-call "ikrt_file_is_semaphore" pathname-bv follow))

(define-inline (posix-file-is-shared-memory? pathname-bv follow)
  (foreign-call "ikrt_file_is_shared_memory" pathname-bv follow))

;;; --------------------------------------------------------------------

(define-inline (posix-file-exists? pathname-bv)
  (foreign-call "ikrt_posix_file_exists" pathname-bv))

(define-inline (posix-file-size pathname-bv)
  (foreign-call "ikrt_posix_file_size" pathname-bv))

(define-inline (posix-access pathname-bv how-fx)
  (foreign-call "ikrt_posix_access" pathname-bv how-fx))

;;; --------------------------------------------------------------------

(define-inline (posix-file-atime pathname-bv vector)
  (foreign-call "ikrt_posix_file_atime" pathname-bv vector))

(define-inline (posix-file-mtime pathname-bv vector)
  (foreign-call "ikrt_posix_file_mtime" pathname-bv vector))

(define-inline (posix-file-ctime pathname-bv vector)
  (foreign-call "ikrt_posix_file_ctime" pathname-bv vector))


;;;; file system interface

(define-inline (posix-chown pathname-bv owner-fx group-fx)
  (foreign-call "ikrt_posix_chown" pathname-bv owner-fx group-fx))

(define-inline (posix-fchown fd owner-fx group-fx)
  (foreign-call "ikrt_posix_fchown" fd owner-fx group-fx))

;;; --------------------------------------------------------------------

(define-inline (posix-chmod pathname-bv mode-fx)
  (foreign-call "ikrt_posix_chmod" pathname-bv mode-fx))

(define-inline (posix-fchmod pathname-bv mode-fx)
  (foreign-call "ikrt_posix_fchmod" pathname-bv mode-fx))

(define-inline (posix-umask mask-fx)
  (foreign-call "ikrt_posix_umask" mask-fx))

(define-inline (posix-getumask)
  (foreign-call "ikrt_posix_getumask"))

;;; --------------------------------------------------------------------

(define-inline (posix-utime pathname-bv atime-sec mtime-sec)
  (foreign-call "ikrt_posix_utime" pathname-bv atime-sec mtime-sec))

(define-inline (posix-utimes pathname-bv atime-sec atime-usec mtime-sec mtime-usec)
  (foreign-call "ikrt_posix_utimes" pathname-bv atime-sec atime-usec mtime-sec mtime-usec))

(define-inline (posix-lutimes pathname-bv atime-sec atime-usec mtime-sec mtime-usec)
  (foreign-call "ikrt_posix_lutimes" pathname-bv atime-sec atime-usec mtime-sec mtime-usec))

(define-inline (posix-futimes fd atime-sec atime-usec mtime-sec mtime-usec)
  (foreign-call "ikrt_posix_futimes" fd atime-sec atime-usec mtime-sec mtime-usec))


;;;; hard and symbolic links

(define-inline (posix-link old-pathname-bv new-pathname-bv)
  (foreign-call "ikrt_posix_link" old-pathname-bv new-pathname-bv))

(define-inline (posix-symlink file-pathname-bv link-pathname-bv)
  (foreign-call "ikrt_posix_symlink" file-pathname-bv link-pathname-bv))

(define-inline (posix-readlink link-pathname-bv)
  (foreign-call "ikrt_posix_readlink" link-pathname-bv))

(define-inline (posix-realpath pathname-bv)
  (foreign-call "ikrt_posix_realpath" pathname-bv))

(define-inline (posix-unlink pathname-bv)
  (foreign-call "ikrt_posix_unlink" pathname-bv))

(define-inline (posix-remove pathname-bv)
  (foreign-call "ikrt_posix_remove" pathname-bv))

(define-inline (posix-rename old-pathname-bv new-pathname-bv)
  (foreign-call "ikrt_posix_rename" old-pathname-bv new-pathname-bv))


;;;; file system directories

(define-inline (posix-mkdir pathname-bv mode)
  (foreign-call "ikrt_posix_mkdir" pathname-bv mode))

(define-inline (posix-rmdir pathname-bv)
  (foreign-call "ikrt_posix_rmdir" pathname-bv))

(define-inline (posix-getcwd)
  (foreign-call "ikrt_posix_getcwd"))

(define-inline (posix-chdir pathname-bv)
  (foreign-call "ikrt_posix_chdir" pathname-bv))

(define-inline (posix-fchdir fd)
  (foreign-call "ikrt_posix_fchdir" fd))

;;; --------------------------------------------------------------------

(define-inline (posix-opendir filename.bv)
  (foreign-call "ikrt_posix_opendir" filename.bv))

(define-inline (posix-fdopendir filename.bv)
  (foreign-call "ikrt_posix_fdopendir" filename.bv))

(define-inline (glibc-dirfd stream-ptr)
  (foreign-call "ikrt_glibc_dirfd" stream-ptr))

(define-inline (posix-readdir stream.ptr)
  (foreign-call "ikrt_posix_readdir" stream.ptr))

(define-inline (posix-closedir stream.ptr)
  (foreign-call "ikrt_posix_closedir" stream.ptr))

;;; --------------------------------------------------------------------

(define-inline (posix-rewinddir stream.ptr)
  (foreign-call "ikrt_posix_rewinddir" stream.ptr))

(define-inline (posix-telldir stream.ptr)
  (foreign-call "ikrt_posix_telldir" stream.ptr))

(define-inline (posix-seekdir stream.ptr pos.num)
  (foreign-call "ikrt_posix_seekdir" stream.ptr pos.num))


;;;; temporary files and directories

(define-inline (glibc-mkstemp template-bv)
  (foreign-call "ikrt_glibc_mkstemp" template-bv))

(define-inline (glibc-mkdtemp template-bv)
  (foreign-call "ikrt_glibc_mkdtemp" template-bv))


;;;; file descriptors

(define-inline (posix-open pathname flags mode)
  (foreign-call "ikrt_posix_open" pathname flags mode))

(define-inline (posix-close fd)
  (foreign-call "ikrt_posix_close" fd))

(define-inline (posix-read fd buffer size)
  (foreign-call "ikrt_posix_read" fd buffer size))

(define-inline (posix-pread fd buffer size off)
  (foreign-call "ikrt_posix_pread" fd buffer size off))

(define-inline (posix-write fd buffer size)
  (foreign-call "ikrt_posix_write" fd buffer size))

(define-inline (posix-pwrite fd buffer size off)
  (foreign-call "ikrt_posix_pwrite" fd buffer size off))

(define-inline (posix-lseek fd off whence)
  (foreign-call "ikrt_posix_lseek" fd off whence))

;;; --------------------------------------------------------------------

(define-inline (posix-readv fd buffers)
  (foreign-call "ikrt_posix_readv" fd buffers))

(define-inline (posix-writev fd buffers)
  (foreign-call "ikrt_posix_writev" fd buffers))

;;; --------------------------------------------------------------------

(define-inline (posix-select nfds read_fds write_fds except_fds sec usec)
  (foreign-call "ikrt_posix_select" nfds read_fds write_fds except_fds sec usec))

(define-inline (posix-select-fd fd sec usec)
  (foreign-call "ikrt_posix_select_fd" fd sec usec))

(define-inline (posix-select-fd-readable? fd sec usec)
  (foreign-call "ikrt_posix_select_is_readable" fd sec usec))

(define-inline (posix-select-fd-writable? fd sec usec)
  (foreign-call "ikrt_posix_select_is_writable" fd sec usec))

(define-inline (posix-select-fd-exceptional? fd sec usec)
  (foreign-call "ikrt_posix_select_is_exceptional" fd sec usec))

(define-inline (posix-poll fds timeout)
  (foreign-call "ikrt_posix_poll" fds timeout))

;;; --------------------------------------------------------------------

(define-inline (posix-fcntl fd command arg)
  (foreign-call "ikrt_posix_fcntl" fd command arg))

(define-inline (posix-ioctl fd command arg)
  (foreign-call "ikrt_posix_ioctl" fd command arg))

(define-inline (posix-fd-set-non-blocking fd)
  (foreign-call "ikptr_posix_fd_set_non_blocking" fd))

;;; --------------------------------------------------------------------

(define-inline (posix-dup fd)
  (foreign-call "ikrt_posix_dup" fd))

(define-inline (posix-dup2 old new)
  (foreign-call "ikrt_posix_dup2" old new))

;;; --------------------------------------------------------------------

(define-inline (posix-pipe)
  (foreign-call "ikrt_posix_pipe"))

(define-inline (posix-mkfifo pathname-bv mode)
  (foreign-call "ikrt_posix_mkfifo" pathname-bv mode))

;;; --------------------------------------------------------------------

(define-inline (posix-truncate name length)
  (foreign-call "ikrt_posix_truncate" name length))

(define-inline (posix-ftruncate fd length)
  (foreign-call "ikrt_posix_ftruncate" fd length))

;;; --------------------------------------------------------------------

(define-inline (posix-lockf fd cmd len)
  (foreign-call "ikrt_posix_lockf" fd cmd len))

;;; --------------------------------------------------------------------

(define-inline (linux-epoll-create size)
  (foreign-call "ikrt_linux_epoll_create" size))

(define-inline (linux-epoll-create1 flags)
  (foreign-call "ikrt_linux_epoll_create1" flags))

(define-inline (linux-epoll-ctl epfd op fd event)
  (foreign-call "ikrt_linux_epoll_ctl" epfd op fd event))

(define-inline (linux-epoll-wait epfd event maxevents timeout-ms)
  (foreign-call "ikrt_linux_epoll_wait" epfd event maxevents timeout-ms))

(define-inline (linux-epoll-event-alloc number-of-entries)
  (foreign-call "ikrt_linux_epoll_event_alloc" number-of-entries))

(define-inline (linux-epoll-event-size)
  (foreign-call "ikrt_linux_epoll_event_size"))

(define-inline (linux-epoll-event-set-events! events-array index field-events)
  (foreign-call "ikrt_linux_epoll_event_set_events" events-array index field-events))
(define-inline (linux-epoll-event-ref-events  events-array index)
  (foreign-call "ikrt_linux_epoll_event_ref_events" events-array index))

(define-inline (linux-epoll-event-set-data-ptr! events-array index field-ptr)
  (foreign-call "ikrt_linux_epoll_event_set_data_ptr" events-array index field-ptr))
(define-inline (linux-epoll-event-ref-data-ptr  events-array index)
  (foreign-call "ikrt_linux_epoll_event_ref_data_ptr" events-array index))

(define-inline (linux-epoll-event-set-data-fd! events-array index field-fd)
  (foreign-call "ikrt_linux_epoll_event_set_data_fd" events-array index field-fd))
(define-inline (linux-epoll-event-ref-data-fd  events-array index)
  (foreign-call "ikrt_linux_epoll_event_ref_data_fd" events-array index))

(define-inline (linux-epoll-event-set-data-u32! events-array index field-u32)
  (foreign-call "ikrt_linux_epoll_event_set_data_u32" events-array index field-u32))
(define-inline (linux-epoll-event-ref-data-u32  events-array index)
  (foreign-call "ikrt_linux_epoll_event_ref_data_u32" events-array index))

(define-inline (linux-epoll-event-set-data-u64! events-array index field-u64)
  (foreign-call "ikrt_linux_epoll_event_set_data_u64" events-array index field-u64))
(define-inline (linux-epoll-event-ref-data-u64  events-array index)
  (foreign-call "ikrt_linux_epoll_event_ref_data_u64" events-array index))


;;;; file descriptor sets

(define-inline (posix-sizeof-fd-set count)
  (foreign-call "ikrt_posix_sizeof_fd_set" count))

(define-inline (posix-make-fd-set-bytevector count)
  (foreign-call "ikrt_posix_make_fd_set_bytevector" count))

(define-inline (posix-make-fd-set-pointer count)
  (foreign-call "ikrt_posix_make_fd_set_pointer" count))

(define-inline (posix-make-fd-set-memory-block! mblock count)
  (foreign-call "ikrt_posix_make_fd_set_memory_block" mblock count))

;;; --------------------------------------------------------------------

(define-inline (posix-fd-zero fdset idx)
  (foreign-call "ikrt_posix_fd_zero" fdset idx))

(define-inline (posix-fd-set fd fdset idx)
  (foreign-call "ikrt_posix_fd_set" fd fdset idx))

(define-inline (posix-fd-clr fd fdset idx)
  (foreign-call "ikrt_posix_fd_clr" fd fdset idx))

(define-inline (posix-fd-isset fd fdset idx)
  (foreign-call "ikrt_posix_fd_isset" fd fdset idx))

;;; --------------------------------------------------------------------

(define-inline (posix-select-from-sets nfds read-fds write-fds except-fds sec usec)
  (foreign-call "ikrt_posix_select_from_sets" nfds read-fds write-fds except-fds sec usec))

(define-inline (posix-select-from-sets-array nfds fd-sets sec usec)
  (foreign-call "ikrt_posix_select_from_sets_array" nfds fd-sets sec usec))


;;;; memory-mapped input/output

(define-inline (posix-mmap address length protect flags fd offset)
  (foreign-call "ikrt_posix_mmap" address length protect flags fd offset))

(define-inline (posix-munmap address length)
  (foreign-call "ikrt_posix_munmap" address length))

(define-inline (posix-msync address length flags)
  (foreign-call "ikrt_posix_msync" address length flags))

(define-inline (posix-mremap address length new-length flags)
  (foreign-call "ikrt_posix_mremap"  address length new-length flags))

(define-inline (posix-madvise address length advice)
  (foreign-call "ikrt_posix_madvise" address length advice))

(define-inline (posix-mlock address length)
  (foreign-call "ikrt_posix_mlock" address length))

(define-inline (posix-munlock address length)
  (foreign-call "ikrt_posix_munlock" address length))

(define-inline (posix-mlockall flags)
  (foreign-call "ikrt_posix_mlockall" flags))

(define-inline (posix-munlockall)
  (foreign-call "ikrt_posix_munlockall"))

(define-inline (posix-mprotect address length prot)
  (foreign-call "ikrt_posix_mprotect" address length prot))


;;;; POSIX message queues

(define-inline (posix-mq-open name oflag mode attr)
  (foreign-call "ikrt_posix_mq_open" name oflag mode attr))

(define-inline (posix-mq-close mqd)
  (foreign-call "ikrt_posix_mq_close" mqd))

(define-inline (posix-mq-unlink name)
  (foreign-call "ikrt_posix_mq_unlink" name))

(define-inline (posix-mq-send mqd message priority)
  (foreign-call "ikrt_posix_mq_send" mqd message priority))

(define-inline (posix-mq-timedsend mqd message priority epoch-timeout)
  (foreign-call "ikrt_posix_mq_timedsend" mqd message priority epoch-timeout))

(define-inline (posix-mq-receive mqd message)
  (foreign-call "ikrt_posix_mq_receive" mqd message))

(define-inline (posix-mq-timedreceive mqd message epoch-timeout)
  (foreign-call "ikrt_posix_mq_timedreceive" mqd message epoch-timeout))

(define-inline (posix-mq-setattr mqd new-attr old-attr)
  (foreign-call "ikrt_posix_mq_setattr" mqd new-attr old-attr))

(define-inline (posix-mq-getattr mqd attr)
  (foreign-call "ikrt_posix_mq_getattr" mqd attr))

;;At present this is not interface.
;;
;; (define-inline (posix-mq-notify)
;;   (foreign-call "ikrt_posix_mq_notify"))


;;;; POSIX shared memory

(define-inline (posix-shm-open name oflag mode)
  (foreign-call "ikrt_posix_shm_open" name oflag mode))

(define-inline (posix-shm-unlink name)
  (foreign-call "ikrt_posix_shm_unlink" name))


;;;; POSIX semaphores

(define-inline (posix-sem-open name oflag mode value)
  (foreign-call "ikrt_posix_sem_open" name oflag mode value))

(define-inline (posix-sem-close sem)
  (foreign-call "ikrt_posix_sem_close" sem))

(define-inline (posix-sem-unlink name)
  (foreign-call "ikrt_posix_sem_unlink" name))

;;; --------------------------------------------------------------------

(define-inline (posix-sem-init sem pshared value)
  (foreign-call "ikrt_posix_sem_init" sem pshared value))

(define-inline (posix-sem-destroy sem)
  (foreign-call "ikrt_posix_sem_destroy" sem))

;;; --------------------------------------------------------------------

(define-inline (posix-sem-post sem)
  (foreign-call "ikrt_posix_sem_post" sem))

(define-inline (posix-sem-wait sem)
  (foreign-call "ikrt_posix_sem_wait" sem))

(define-inline (posix-sem-trywait sem)
  (foreign-call "ikrt_posix_sem_trywait" sem))

(define-inline (posix-sem-timedwait sem abs-timeout)
  (foreign-call "ikrt_posix_sem_timedwait" sem abs-timeout))

(define-inline (posix-sem-getvalue sem)
  (foreign-call "ikrt_posix_sem_getvalue" sem))

;;; --------------------------------------------------------------------

(define-inline (posix-sizeof-sem_t)
  (foreign-call "ikrt_posix_sizeof_sem_t"))


;;;; POSIX timers

(define-inline (posix-timer-create clock-id sigevent)
  (foreign-call "ikrt_posix_timer_create" clock-id sigevent))

(define-inline (posix-timer-delete timer-id)
  (foreign-call "ikrt_posix_timer_delete" timer-id))

(define-inline (posix-timer-settime timer-id flags new-timer-spec old-timer-spec)
  (foreign-call "ikrt_posix_timer_settime" timer-id flags new-timer-spec old-timer-spec))

(define-inline (posix-timer-gettime timer-id curr-timer-spec)
  (foreign-call "ikrt_posix_timer_gettime" timer-id curr-timer-spec))

(define-inline (posix-timer-getoverrun timer-id)
  (foreign-call "ikrt_posix_timer_getoverrun" timer-id))


;;;; POSIX realtime clock functions

(define-inline (posix-clock-getres clock-id struct-timespec)
  (foreign-call "ikrt_posix_clock_getres" clock-id struct-timespec))

(define-inline (posix-clock-gettime clock-id struct-timespec)
  (foreign-call "ikrt_posix_clock_gettime" clock-id struct-timespec))

(define-inline (posix-clock-settime clock-id struct-timespec)
  (foreign-call "ikrt_posix_clock_settime" clock-id struct-timespec))

(define-inline (posix-clock-getcpuclockid pid)
  (foreign-call "ikrt_posix_clock_getcpuclockid" pid))


;;;; file system synchronisation

(define-inline (glibc-sync)
  (foreign-call "ikrt_glibc_sync"))

(define-inline (glibc-fsync fd)
  (foreign-call "ikrt_glibc_fsync" fd))

(define-inline (glibc-fdatasync fd)
  (foreign-call "ikrt_glibc_fdatasync" fd))


;;;; network sockets

(define-inline (glibc-if-nametoindex name)
  (foreign-call "ikrt_glibc_if_nametoindex" name))

(define-inline (glibc-if-indextoname index)
  (foreign-call "ikrt_glibc_if_indextoname" index))

(define-inline (glibc-if-nameindex)
  (foreign-call "ikrt_glibc_if_nameindex"))

;;; --------------------------------------------------------------------

(define-inline (posix-make-sockaddr_un pathname-bv)
  (foreign-call "ikrt_posix_make_sockaddr_un" pathname-bv))

(define-inline (posix-sockaddr_un.pathname addr)
  (foreign-call "ikrt_posix_sockaddr_un_pathname" addr))

;;; --------------------------------------------------------------------

(define-inline (posix-make-sockaddr_in addr port)
  (foreign-call "ikrt_posix_make_sockaddr_in" addr port))

(define-inline (posix-sockaddr_in.in_addr sockaddr)
  (foreign-call "ikrt_posix_sockaddr_in_in_addr" sockaddr))

(define-inline (posix-sockaddr_in.in_port sockaddr)
  (foreign-call "ikrt_posix_sockaddr_in_in_port" sockaddr))

;;; --------------------------------------------------------------------

(define-inline (posix-make-sockaddr_in6 addr port)
  (foreign-call "ikrt_posix_make_sockaddr_in6" addr port))

(define-inline (posix-sockaddr_in6.in6_addr sockaddr)
  (foreign-call "ikrt_posix_sockaddr_in6_in6_addr" sockaddr))

(define-inline (posix-sockaddr_in6.in6_port sockaddr)
  (foreign-call "ikrt_posix_sockaddr_in6_in6_port" sockaddr))

;;; --------------------------------------------------------------------

(define-inline (posix-in6addr_loopback)
  (foreign-call "ikrt_posix_in6addr_loopback"))

(define-inline (posix-in6addr_any)
  (foreign-call "ikrt_posix_in6addr_any"))

;;; --------------------------------------------------------------------

(define-inline (posix-inet_aton dotted-quad)
  (foreign-call "ikrt_posix_inet_aton" dotted-quad))

(define-inline (posix-inet_ntoa addr)
  (foreign-call "ikrt_posix_inet_ntoa" addr))

;;; --------------------------------------------------------------------

(define-inline (posix-inet_pton af presentation)
  (foreign-call "ikrt_posix_inet_pton" af presentation))

(define-inline (posix-inet_ntop af addr)
  (foreign-call "ikrt_posix_inet_ntop" af addr))

;;; --------------------------------------------------------------------

(define-inline (posix-htonl host-long)
  (foreign-call "ikrt_posix_htonl" host-long))

(define-inline (posix-htons host-short)
  (foreign-call "ikrt_posix_htons" host-short))

(define-inline (posix-ntohl host-long)
  (foreign-call "ikrt_posix_ntohl" host-long))

(define-inline (posix-ntohs host-short)
  (foreign-call "ikrt_posix_ntohs" host-short))

;;; --------------------------------------------------------------------

(define-inline (posix-gethostbyname rtd hostname)
  (foreign-call "ikrt_posix_gethostbyname" rtd hostname))

(define-inline (posix-gethostbyname2 rtd hostname addrtype)
  (foreign-call "ikrt_posix_gethostbyname2" rtd hostname addrtype))

(define-inline (posix-gethostbyaddr rtd addr)
  (foreign-call "ikrt_posix_gethostbyaddr" rtd addr))

(define-inline (posix-host-entries rtd)
  (foreign-call "ikrt_posix_host_entries" rtd))

(define-inline (posix-getaddrinfo rtd node service hints)
  (foreign-call "ikrt_posix_getaddrinfo" rtd node service hints))

(define-inline (posix-gai_strerror code)
  (foreign-call "ikrt_posix_gai_strerror" code))

;;; --------------------------------------------------------------------

(define-inline (posix-getprotobyname rtd name)
  (foreign-call "ikrt_posix_getprotobyname" rtd name))

(define-inline (posix-getprotobynumber rtd num)
  (foreign-call "ikrt_posix_getprotobynumber" rtd num))

(define-inline (posix-protocol-entries rtd)
  (foreign-call "ikrt_posix_protocol_entries" rtd))

;;; --------------------------------------------------------------------

(define-inline (posix-getservbyname rtd name protocol)
  (foreign-call "ikrt_posix_getservbyname" rtd name protocol))

(define-inline (posix-getservbyport rtd port protocol)
  (foreign-call "ikrt_posix_getservbyport" rtd port protocol))

(define-inline (posix-service-entries rtd)
  (foreign-call "ikrt_posix_service_entries" rtd))

;;; --------------------------------------------------------------------

(define-inline (posix-getnetbyname rtd name)
  (foreign-call "ikrt_posix_getnetbyname" rtd name))

(define-inline (posix-getnetbyaddr rtd net type)
  (foreign-call "ikrt_posix_getnetbyaddr" rtd net type))

(define-inline (posix-network-entries rtd)
  (foreign-call "ikrt_posix_network_entries" rtd))

;;; --------------------------------------------------------------------

(define-inline (posix-socket namespace style protocol)
  (foreign-call "ikrt_posix_socket" namespace style protocol))

(define-inline (posix-shutdown sock how)
  (foreign-call "ikrt_posix_shutdown" sock how))

(define-inline (posix-socketpair namespace style protocol)
  (foreign-call "ikrt_posix_socketpair" namespace style protocol))

;;; --------------------------------------------------------------------

(define-inline (posix-connect sock sockaddr)
  (foreign-call "ikrt_posix_connect" sock sockaddr))

(define-inline (posix-listen sock max-pending-conns)
  (foreign-call "ikrt_posix_listen" sock max-pending-conns))

(define-inline (posix-accept sock)
  (foreign-call "ikrt_posix_accept" sock))

(define-inline (posix-bind sock sockaddr)
  (foreign-call "ikrt_posix_bind" sock sockaddr))

;;; --------------------------------------------------------------------

(define-inline (posix-getpeername sock)
  (foreign-call "ikrt_posix_getpeername" sock))

(define-inline (posix-getsockname sock)
  (foreign-call "ikrt_posix_getsockname" sock))

;;; --------------------------------------------------------------------

(define-inline (posix-send sock buffer size flags)
  (foreign-call "ikrt_posix_send" sock buffer size flags))

(define-inline (posix-recv sock buffer size flags)
  (foreign-call "ikrt_posix_recv" sock buffer size flags))

(define-inline (posix-sendto sock buffer size flags addr)
  (foreign-call "ikrt_posix_sendto" sock buffer size flags addr))

(define-inline (posix-recvfrom sock buffer size flags)
  (foreign-call "ikrt_posix_recvfrom" sock buffer size flags))

;;; --------------------------------------------------------------------

(define-inline (posix-setsockopt sock level option optval)
  (foreign-call "ikrt_posix_setsockopt" sock level option optval))

(define-inline (posix-setsockopt/int sock level option optval)
  (foreign-call "ikrt_posix_setsockopt_int" sock level option optval))

(define-inline (posix-setsockopt/size_t sock level option optval)
  (foreign-call "ikrt_posix_setsockopt_size_t" sock level option optval))

(define-inline (posix-getsockopt sock level option optval)
  (foreign-call "ikrt_posix_getsockopt" sock level option optval))

(define-inline (posix-getsockopt/int sock level option)
  (foreign-call "ikrt_posix_getsockopt_int" sock level option))

(define-inline (posix-getsockopt/size_t sock level option)
  (foreign-call "ikrt_posix_getsockopt_size_t" sock level option))

(define-inline (posix-setsockopt/linger sock onoff linger)
  (foreign-call "ikrt_posix_setsockopt_linger" sock onoff linger))

(define-inline (posix-getsockopt/linger sock)
  (foreign-call "ikrt_posix_getsockopt_linger" sock))


;;;; platform API for file descriptors
;;
;;See detailed documentation of the C functions in the Texinfo file.
;;

(define-inline (platform-open-input-fd pathname-bv open-options)
  ;;Interface  to "open()".   Open  a file  descriptor  for reading;  if
  ;;successful  return  a  non-negative  fixnum  representing  the  file
  ;;descriptor;  else return  a  negative fixnum  representing an  ERRNO
  ;;code.
  ;;
  (foreign-call "ikrt_open_input_fd" pathname-bv open-options))

(define-inline (platform-open-output-fd pathname-bv open-options)
  ;;Interface  to "open()".   Open  a file  descriptor  for writing;  if
  ;;successful  return  a  non-negative  fixnum  representing  the  file
  ;;descriptor;  else return  a  negative fixnum  representing an  ERRNO
  ;;code.
  ;;
  (foreign-call "ikrt_open_output_fd" pathname-bv open-options))

(define-inline (platform-open-input/output-fd pathname-bv open-options)
  ;;Interface to "open()".  Open  a file descriptor for reading writing;
  ;;if  successful return  a non-negative  fixnum representing  the file
  ;;descriptor;  else return  a  negative fixnum  representing an  ERRNO
  ;;code.
  ;;
  (foreign-call "ikrt_open_input_output_fd" pathname-bv open-options))

(define-inline (platform-read-fd fd dst.bv dst.start requested-count)
  ;;Interface to "read()".  Read data  from the file descriptor into the
  ;;supplied  bytevector;  if successful  return  a non-negative  fixnum
  ;;representind  the  number of  bytes  actually  read;  else return  a
  ;;negative fixnum representing an ERRNO code.
  ;;
  (foreign-call "ikrt_read_fd" fd dst.bv dst.start requested-count))

(define-inline (platform-write-fd fd src.bv src.start requested-count)
  ;;Interface to "write()".  Write  data from the supplied bytevector to
  ;;the  file descriptor;  if  successful return  a non-negative  fixnum
  ;;representind  the number of  bytes actually  written; else  return a
  ;;negative fixnum representing an ERRNO code.
  ;;
  (foreign-call "ikrt_write_fd" fd src.bv src.start requested-count))

(define-inline (platform-set-position fd position)
  ;;Interface to "lseek()".  Set  the cursor position.  POSITION must be
  ;;an  exact integer in  the range  of the  "off_t" platform  type.  If
  ;;successful return false; if an error occurs return a negative fixnum
  ;;representing an  ERRNO code.
  ;;
  (foreign-call "ikrt_set_position" fd position))

(define-inline (platform-close-fd fd)
  ;;Interface to "close()".  Close  the file descriptor and return false
  ;;or a fixnum representing an ERRNO code.
  ;;
  (foreign-call "ikrt_close_fd" fd))

(define-inline (platform-fd-set-non-blocking-mode fd)
  ;;Make  use  of  "fcntl()"  to   set  non-blocking  mode  for  a  file
  ;;descriptor.
  ;;
  (foreign-call "ikptr_fd_set_non_blocking_mode" fd))

(define-inline (platform-fd-ref-non-blocking-mode fd)
  ;;Make use  of "fcntl()" to  query a file descriptor  for non-blocking
  ;;mode.
  ;;
  (foreign-call "ikptr_fd_ref_non_blocking_mode" fd))


;;;; users and groups

(define-inline (posix-getuid)
  (foreign-call "ikrt_posix_getuid"))

(define-inline (posix-getgid)
  (foreign-call "ikrt_posix_getgid"))

(define-inline (posix-geteuid)
  (foreign-call "ikrt_posix_geteuid"))

(define-inline (posix-getegid)
  (foreign-call "ikrt_posix_getegid"))

(define-inline (posix-getgroups)
  (foreign-call "ikrt_posix_getgroups"))

;;; --------------------------------------------------------------------

(define-inline (posix-seteuid uid)
  (foreign-call "ikrt_posix_seteuid" uid))

(define-inline (posix-setuid uid)
  (foreign-call "ikrt_posix_setuid" uid))

(define-inline (posix-setreuid real-uid effective-uid)
  (foreign-call "ikrt_posix_setreuid" real-uid effective-uid))

;;; --------------------------------------------------------------------

(define-inline (posix-setegid gid)
  (foreign-call "ikrt_posix_setegid" gid))

(define-inline (posix-setgid gid)
  (foreign-call "ikrt_posix_setgid" gid))

(define-inline (posix-setregid real-gid effective-gid)
  (foreign-call "ikrt_posix_setregid" real-gid effective-gid))

;;; --------------------------------------------------------------------

(define-inline (posix-getlogin)
  (foreign-call "ikrt_posix_getlogin"))

;;; --------------------------------------------------------------------

(define-inline (posix-getpwuid rtd uid)
  (foreign-call "ikrt_posix_getpwuid" rtd uid))

(define-inline (posix-getpwnam rtd name)
  (foreign-call "ikrt_posix_getpwnam" rtd name))

(define-inline (posix-user-entries rtd)
  (foreign-call "ikrt_posix_user_entries" rtd))

;;; --------------------------------------------------------------------

(define-inline (posix-getgrgid rtd gid)
  (foreign-call "ikrt_posix_getgrgid" rtd gid))

(define-inline (posix-getgrnam rtd name)
  (foreign-call "ikrt_posix_getgrnam" rtd name))

(define-inline (posix-group-entries rtd)
  (foreign-call "ikrt_posix_group_entries" rtd))


;;;; job control

(define-inline (posix-ctermid)
  (foreign-call "ikrt_posix_ctermid"))

;;; --------------------------------------------------------------------

(define-inline (posix-setsid)
  (foreign-call "ikrt_posix_setsid"))

(define-inline (posix-getsid pid)
  (foreign-call "ikrt_posix_getsid" pid))

(define-inline (posix-getpgrp)
  (foreign-call "ikrt_posix_getpgrp"))

(define-inline (posix-setpgid pid pgid)
  (foreign-call "ikrt_posix_setpgid" pid pgid))

;;; --------------------------------------------------------------------

(define-inline (posix-tcgetpgrp fd)
  (foreign-call "ikrt_posix_tcgetpgrp" fd))

(define-inline (posix-tcsetpgrp fd pgid)
  (foreign-call "ikrt_posix_tcsetpgrp" fd pgid))

(define-inline (posix-tcgetsid fd)
  (foreign-call "ikrt_posix_tcgetsid" fd))


;;;; date and time

(define-inline (posix-nanosleep secs nsecs)
  (foreign-call "ikrt_posix_nanosleep" secs nsecs))

;;; --------------------------------------------------------------------

(define-inline (posix-clock)
  (foreign-call "ikrt_posix_clock"))

(define-inline (posix-times rtd)
  (foreign-call "ikrt_posix_times" rtd))

;;; --------------------------------------------------------------------

(define-inline (posix-time)
  (foreign-call "ikrt_posix_time"))

(define-inline (posix-gettimeofday rtd)
  (foreign-call "ikrt_posix_gettimeofday" rtd))

;;; --------------------------------------------------------------------

(define-inline (posix-localtime rtd time)
  (foreign-call "ikrt_posix_localtime" rtd time))

(define-inline (posix-gmtime rtd time)
  (foreign-call "ikrt_posix_gmtime" rtd time))

(define-inline (posix-timelocal tm)
  (foreign-call "ikrt_posix_timelocal" tm))

(define-inline (posix-timegm tm)
  (foreign-call "ikrt_posix_timegm" tm))

(define-inline (posix-strftime template tm)
  (foreign-call "ikrt_posix_strftime" template tm))

;;; --------------------------------------------------------------------

(define-inline (posix-setitimer which new)
  (foreign-call "ikrt_posix_setitimer" which new))

(define-inline (posix-getitimer which old)
  (foreign-call "ikrt_posix_getitimer" which old))

(define-inline (posix-alarm seconds)
  (foreign-call "ikrt_posix_alarm" seconds))


;;;; resource limits

(define-inline (posix-RLIM_INFINITY)
  (foreign-call "ikrt_posix_RLIM_INFINITY"))

(define-inline (posix-getrlimit resource rlimit)
  (foreign-call "ikrt_posix_getrlimit" resource rlimit))

(define-inline (posix-setrlimit resource rlimit)
  (foreign-call "ikrt_posix_setrlimit" resource rlimit))

(define-inline (posix-getrusage processes rusage)
  (foreign-call "ikrt_posix_getrusage" processes rusage))

(define-inline (linux-prlimit pid resource new-limit old-limit)
  (foreign-call "ikrt_linux_prlimit" pid resource new-limit old-limit))


;;;; daemonisation

(define-inline (linux-daemon nochdir noclose)
  (foreign-call "ikrt_linux_daemon" nochdir noclose))


;;;; mathematics

(define-inline (glibc-csin X)
  (foreign-call "ikrt_glibc_csin" X))

(define-inline (glibc-ccos X)
  (foreign-call "ikrt_glibc_ccos" X))

(define-inline (glibc-ctan X)
  (foreign-call "ikrt_glibc_ctan" X))

(define-inline (glibc-casin X)
  (foreign-call "ikrt_glibc_casin" X))

(define-inline (glibc-cacos X)
  (foreign-call "ikrt_glibc_cacos" X))

(define-inline (glibc-catan X)
  (foreign-call "ikrt_glibc_catan" X))

;;; --------------------------------------------------------------------

(define-inline (glibc-cexp X)
  (foreign-call "ikrt_glibc_cexp" X))

(define-inline (glibc-clog X)
  (foreign-call "ikrt_glibc_clog" X))

(define-inline (glibc-clog10 X)
  (foreign-call "ikrt_glibc_clog10" X))

(define-inline (glibc-csqrt X)
  (foreign-call "ikrt_glibc_csqrt" X))

(define-inline (glibc-cpow X Y)
  (foreign-call "ikrt_glibc_cpow" X Y))

;;; --------------------------------------------------------------------

(define-inline (glibc-sinh X)
  (foreign-call "ikrt_glibc_sinh" X))

(define-inline (glibc-cosh X)
  (foreign-call "ikrt_glibc_cosh" X))

(define-inline (glibc-tanh X)
  (foreign-call "ikrt_glibc_tanh" X))

(define-inline (glibc-csinh X)
  (foreign-call "ikrt_glibc_csinh" X))

(define-inline (glibc-ccosh X)
  (foreign-call "ikrt_glibc_ccosh" X))

(define-inline (glibc-ctanh X)
  (foreign-call "ikrt_glibc_ctanh" X))

(define-inline (glibc-asinh X)
  (foreign-call "ikrt_glibc_asinh" X))

(define-inline (glibc-acosh X)
  (foreign-call "ikrt_glibc_acosh" X))

(define-inline (glibc-atanh X)
  (foreign-call "ikrt_glibc_atanh" X))

(define-inline (glibc-casinh X)
  (foreign-call "ikrt_glibc_casinh" X))

(define-inline (glibc-cacosh X)
  (foreign-call "ikrt_glibc_cacosh" X))

(define-inline (glibc-catanh X)
  (foreign-call "ikrt_glibc_catanh" X))

;;; --------------------------------------------------------------------

(define-inline (glibc-erf X)
  (foreign-call "ikrt_glibc_erf" X))

(define-inline (glibc-erfc X)
  (foreign-call "ikrt_glibc_erfc" X))

(define-inline (glibc-tgamma X)
  (foreign-call "ikrt_glibc_tgamma" X))

(define-inline (glibc-lgamma X)
  (foreign-call "ikrt_glibc_lgamma" X))

(define-inline (glibc-j0 X)
  (foreign-call "ikrt_glibc_j0" X))

(define-inline (glibc-j1 X)
  (foreign-call "ikrt_glibc_j1" X))

(define-inline (glibc-jn N X)
  (foreign-call "ikrt_glibc_jn" N X))

(define-inline (glibc-y0 X)
  (foreign-call "ikrt_glibc_y0" X))

(define-inline (glibc-y1 X)
  (foreign-call "ikrt_glibc_y1" X))

(define-inline (glibc-yn N X)
  (foreign-call "ikrt_glibc_yn" N X))


;;;; random numbers

(define-inline (glibc-rand)
  (foreign-call "ikrt_glibc_rand"))

(define-inline (glibc-srand seed)
  (foreign-call "ikrt_glibc_srand" seed))


;;;; pattern matching, globbing, regular expressions

(define-inline (glibc-fnmatch pattern string flags)
  (foreign-call "ikrt_glibc_fnmatch" pattern string flags))

(define-inline (glibc-glob pattern flags error-handler)
  (foreign-call "ikrt_glibc_glob" pattern flags error-handler))

;;; --------------------------------------------------------------------

(define-inline (glibc-regcomp pattern flags)
  (foreign-call "ikrt_glibc_regcomp" pattern flags))

(define-inline (glibc-regexec regex string flags)
  (foreign-call "ikrt_glibc_regexec" regex string flags))

(define-inline (glibc-regfree regex)
  (foreign-call "ikrt_glibc_regfree" regex))


;;;; word expansion

(define-inline (glibc-wordexp words flags)
  (foreign-call "ikrt_glibc_wordexp" words flags))


;;;; system configuration

(define-inline (posix-sysconf parameter)
  (foreign-call "ikrt_posix_sysconf" parameter))

(define-inline (posix-pathconf pathname parameter)
  (foreign-call "ikrt_posix_pathconf" pathname parameter))

(define-inline (posix-fpathconf fd parameter)
  (foreign-call "ikrt_posix_fpathconf" fd parameter))

(define-inline (posix-confstr parameter)
  (foreign-call "ikrt_posix_confstr" parameter))


;;;; iconv

(define-inline (glibc-iconv-open to-code from-code)
  (foreign-call "ikrt_glibc_iconv_open" to-code from-code))

(define-inline (glibc-iconv-close handle)
  (foreign-call "ikrt_glibc_iconv_close" handle))

(define-inline (glibc-iconv handle
			    input  input.start  input.past
			    output output.start output.past)
  (foreign-call "ikrt_glibc_iconv" handle
		input  input.start  input.past
		output output.start output.past))


;;;; done

)

;;; end of file
