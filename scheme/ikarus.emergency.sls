;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: emergency debugging functions
;;;Date: Thu Mar 22, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.emergency)
  (export emergency-write)
  (import (ikarus)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe operations)
	    unsafe.))


;;;; emergency debugging

(define (emergency-write str)
  ;;Interface to the system  "write()" function.  In case something goes
  ;;wrong while modifying  core code, it may be  that the compiled image
  ;;fails to  write understandable messages to the  standard ports using
  ;;the R6RS  functions.  This function  allows direct interface  to the
  ;;platform's stderr.
  ;;
  (let ((bv (string->utf8 str)))
    (capi.platform-write-fd 2 bv 0 (unsafe.bytevector-length bv))
    ;;and a newline
    (capi.platform-write-fd 2 '#vu8(10) 0 1)))


;;;; done

)

;;; end of file
