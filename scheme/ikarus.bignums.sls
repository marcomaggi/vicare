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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus bignums)
  (export
    $bignum-positive?	$bignum-negative?
    $bignum-even?	$bignum-odd?)
  (import (ikarus)
    (except (ikarus system $bignums)
	    $bignum-positive?	$bignum-negative?
	    $bignum-even?	$bignum-odd?
	    ))


(define ($bignum-positive? x)
  (foreign-call "ikrt_positive_bn" x))

(define ($bignum-negative? x)
  (not ($bignum-positive? x)))

(define ($bignum-even? x)
  (foreign-call "ikrt_even_bn" x))

(define ($bignum-odd? x)
  (not ($bignum-even? x)))


;;;; done

)

;;; end of file
