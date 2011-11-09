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
    posix-getenv		posix-setenv
    posix-unsetenv		posix-environ
    glibc-clearenv

    ;; process identifiers
    posix-getpid		posix-getppid

    ;; executing and forking processes
    posix-fork			posix-system
    posix-execv			posix-execve
    posix-execvp

    ;; process termination status
    posix-waitpid		linux-waitid
    posix-wait
    posix-WIFEXITED		posix-WEXITSTATUS
    posix-WIFSIGNALED		posix-WTERMSIG
    posix-WCOREDUMP		posix-WIFSTOPPED
    posix-WSTOPSIG		linux-WIFCONTINUED

    ;; delivering interprocess signals
    posix-raise			posix-kill
    posix-pause

    ;; file system inspection
    posix-stat			posix-lstat
    posix-fstat

    ;; platform API for file descriptors
    platform-open-input-fd
    platform-open-output-fd
    platform-open-input/output-fd
    platform-read-fd
    platform-write-fd
    platform-set-position
    platform-close-fd

    ;; reading file system directories
    platform-open-directory
    platform-read-directory-stream
    platform-close-directory)
  (import (ikarus)
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


;;;; reading file system directories

(define-inline (platform-open-directory filename.bv)
  (foreign-call "ikrt_opendir" filename.bv))

(define-inline (platform-read-directory-stream stream.ptr)
  (foreign-call "ikrt_readdir" stream.ptr))

(define-inline (platform-close-directory stream.ptr)
  (foreign-call "ikrt_closedir" stream.ptr))


;;;; done

)

;;; end of file
