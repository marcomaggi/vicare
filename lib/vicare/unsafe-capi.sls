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
    posix-bind				posix-getsockname
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

    ;; platform API for file descriptors and Scheme ports
    platform-open-input-fd		platform-open-output-fd
    platform-open-input/output-fd	platform-close-fd
    platform-read-fd			platform-write-fd
    platform-set-position)
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

(define-inline (posix-bind sock sockaddr-bv)
  (foreign-call "ikrt_posix_bind" sock sockaddr-bv))

(define-inline (posix-getsockname sock)
  (foreign-call "ikrt_posix_getsockname" sock))

;;; --------------------------------------------------------------------

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


;;;; done

)

;;; end of file
