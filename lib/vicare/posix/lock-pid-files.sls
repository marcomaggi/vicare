;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: lock pid files facilites
;;;Date: Thu May  9, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare posix lock-pid-files)
  (export with-lock-pid-file)
  (import (vicare)
    (prefix (vicare posix) px.)
    (vicare platform constants))


(define (with-lock-pid-file lock-pathname thunk)
  (with-compensations
    (define fd
      (compensate
	  (px.open lock-pathname
		   (fxior O_CREAT O_EXCL O_RDWR)
		   (fxior S_IRUSR S_IWUSR))
	(with
	 (px.close fd)
	 (delete-file lock-pathname))))
    ;;This locks the file or fail raising an exception.
    (px.lockf fd F_TLOCK 0)
    (px.write fd (string->ascii (number->string (px.getpid))))
    (thunk)))


;;;; done

)

;;; end of file
