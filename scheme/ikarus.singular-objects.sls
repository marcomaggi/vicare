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
		  (fixnum-width		sys:fixnum-width)
		  (least-fixnum		sys:least-fixnum)
		  (greatest-fixnum	sys:greatest-fixnum)
		  (void			sys:void)
		  (eof-object		sys:eof-object))
    (rename (ikarus system $structs)
	    (base-rtd	sys:base-rtd)))
  ;;For 64-bit platforms.
  #;(begin
    (define (fixnum-width)
      61)
    (define (greatest-fixnum)
      +1152921504606846975)
    (define (least-fixnum)
      -1152921504606846976))
  ;;For 32-bit platforms.
  #;(begin
    (define (fixnum-width)
      30)
    (define (greatest-fixnum)
      +536870911)
    (define (least-fixnum)
      -536870912))
  (begin
    (define (fixnum-width)	(sys:fixnum-width))
    (define (least-fixnum)	(sys:least-fixnum))
    (define (greatest-fixnum)	(sys:greatest-fixnum)))
  (define (void)		(sys:void))
  (define (eof-object)		(sys:eof-object))
  (define (base-rtd)		(sys:base-rtd)))

;;; end of file
