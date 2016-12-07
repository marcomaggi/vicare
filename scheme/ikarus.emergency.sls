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
;;;Copyright (C) 2012, 2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus.emergency)
  (options typed-language)
  (export emergency-write)
  (import (vicare)
    (only (vicare system $bytevectors)
	  $bytevector-length)
    (prefix (vicare unsafe capi)
	    capi::))


;;;; emergency debugging

(define/std (emergency-write str)
  ;;Interface to the  system "write()" function.  In case something  goes wrong while
  ;;modifying  core  code,  it  may  be  that  the  compiled  image  fails  to  write
  ;;understandable messages  to the  standard ports using  the R6RS  functions.  This
  ;;function allows direct interface to the platform's stderr.
  ;;
  ;;NOTE We really need
  ;;
  (let ((bv (string->utf8 str)))
    (capi::platform-write-fd 2 bv 0 ($bytevector-length bv))
    ;;and a newline
    (capi::platform-write-fd 2 '#vu8(10) 0 1))
  ;;It is  useful to return a  single value, so that  a call to this  function can be
  ;;coded as:
  ;;
  ;;   (define dummy (emergency-write "ciao"))
  ;;
  #f)


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.emergency")))

#| end of library |# )

;;; end of file
