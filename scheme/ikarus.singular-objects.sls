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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus.singular-objects)
  (export base-rtd eof-object void fixnum-width least-fixnum
          greatest-fixnum)
  (import (rename (ikarus)
		  (void			sys:void)
		  (fixnum-width		sys:fixnum-width)
		  (least-fixnum		sys:least-fixnum)
		  (greatest-fixnum	sys:greatest-fixnum)
		  (eof-object		sys:eof-object))
    (rename (ikarus system $structs)
	    (base-rtd	sys:base-rtd)))
  (define (void)		(sys:void))
  ;;FIXME Document  this!!!  We  really need  the definitions  below for
  ;;FIXNUM-WIDTH, LEAST-FIXNUM and GREATEST-FIXNUM not the ones from:
  ;;
  ;;   (include "ikarus.wordsize.scm")
  ;;
  ;;which will cause a segfault in  the execution of this library's code
  ;;object when  the boot image  is loaded.   We must document  why this
  ;;happens.  (Marco Maggi; Wed Jun 19, 2013)
  (begin
    (define (fixnum-width)	(sys:fixnum-width))
    (define (least-fixnum)	(sys:least-fixnum))
    (define (greatest-fixnum)	(sys:greatest-fixnum)))
  (define (eof-object)		(sys:eof-object))
  (define (base-rtd)		(sys:base-rtd)))

;;; end of file
