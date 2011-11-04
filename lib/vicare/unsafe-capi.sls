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

    ;; normalised errno codes
    normalised-errno-EAGAIN
    normalised-errno-EACCES
    normalised-errno-EFAULT
    normalised-errno-EROFS
    normalised-errno-EEXIST
    normalised-errno-EIO
    normalised-errno-ENOENT

    ;; platform API for file descriptors
    platform-open-input-fd
    platform-open-output-fd
    platform-open-input/output-fd
    platform-read-fd
    platform-write-fd
    platform-set-position
    platform-close-fd
    )
  (import (ikarus)
    (only (vicare syntactic-extensions)
	  define-inline))


;;;; normalised errno codes
;;
;;Notice that  the values returned  by the following function  calls are
;;the  codes normalised  to the  values in  POSIX IEEE  Std  1003.1 2004
;;Edition,  which may  be  different  from the  ones  of the  underlying
;;platform.  In  particular these codes  may be different from  the ones
;;exported by the (vicare errno) library.
;;

(define-inline (normalised-errno-EAGAIN)
  (foreign-call "ik_errno_EAGAIN"))

(define-inline (normalised-errno-EACCES)
  (foreign-call "ik_errno_EACCES"))

(define-inline (normalised-errno-EFAULT)
  (foreign-call "ik_errno_EFAULT"))

(define-inline (normalised-errno-EROFS)
  (foreign-call "ik_errno_EROFS"))

(define-inline (normalised-errno-EEXIST)
  (foreign-call "ik_errno_EEXIST"))

(define-inline (normalised-errno-EIO)
  (foreign-call "ik_errno_EIO"))

(define-inline (normalised-errno-ENOENT)
  (foreign-call "ik_errno_ENOENT"))


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
