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
(library (vicare unsafe-capi)
  (export

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
    posix-fcntl				posix-ioctl
    posix-dup				posix-dup2
    posix-pipe				posix-mkfifo

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
    posix-strftime
    posix-nanosleep
    )
  (import (except (ikarus)
		  posix-remove
		  posix-read		posix-write)
    (only (vicare syntactic-extensions)
	  define-inline))


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

;;; --------------------------------------------------------------------

(define-inline (posix-fcntl fd command arg)
  (foreign-call "ikrt_posix_fcntl" fd command arg))

(define-inline (posix-ioctl fd command arg)
  (foreign-call "ikrt_posix_ioctl" fd command arg))

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


;;;; done

)

;;; end of file
