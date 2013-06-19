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
  #;(include "ikarus.wordsize.scm")
  (define (void)		(sys:void))
  (begin
    (define (fixnum-width)	(sys:fixnum-width))
    (define (least-fixnum)	(sys:least-fixnum))
    (define (greatest-fixnum)	(sys:greatest-fixnum)))
  (define (eof-object)		(sys:eof-object))
  (define (base-rtd)		(sys:base-rtd)))

;;; end of file
