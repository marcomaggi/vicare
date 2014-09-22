;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tables of core primitive properties
;;;Date: Sat Sep 20, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; pairs

(declare-core-primitive car
    safe
  (signatures ((T:pair) => (_)))
  (unsafe-replacements $car))

(declare-core-primitive cdr
    safe
  (signatures ((T:pair) => (_)))
  (unsafe-replacements $cdr))

;;; --------------------------------------------------------------------

(declare-core-primitive $car
    unsafe
  (signatures ((T:pair) => (_))))

(declare-core-primitive $cdr
    unsafe
  (signatures ((T:pair) => (_))))


;;;; fixnums

;; (declare-core-primitive fx+ safe
;;   (unsafe-replacements
;;    ($fx+/overflow T:fixnum T:fixnum)))


;;;; general arithmetics

(declare-core-primitive +
    safe
  (signatures (T:number => (T:number)))
  ;;The commented out variants  handle operands of a type that  is not categorised by
  ;;the core type system.
  (unsafe-replacements
   $add-fixnum-fixnum		#;$add-fixnum-bignum	$add-fixnum-flonum
   #;$add-fixnum-ratnum		#;$add-fixnum-compnum	#;$add-fixnum-cflonum

   #;$add-bignum-fixnum		#;$add-bignum-bignum	#;$add-bignum-flonum
   #;$add-bignum-ratnum		#;$add-bignum-compnum	#;$add-bignum-cflonum

   $add-flonum-fixnum		#;$add-flonum-bignum	$add-flonum-flonum
   #;$add-flonum-ratnum		#;$add-flonum-compnum	#;$add-flonum-cflonum

   #;$add-ratnum-fixnum		#;$add-ratnum-bignum	#;$add-ratnum-flonum
   #;$add-ratnum-ratnum		#;$add-ratnum-compnum	#;$add-ratnum-cflonum

   #;$add-compnum-fixnum	#;$add-compnum-bignum	#;$add-compnum-ratnum
   #;$add-compnum-compnum	#;$add-compnum-flonum	#;$add-compnum-cflonum

   #;$add-cflonum-fixnum	#;$add-cflonum-bignum	#;$add-cflonum-ratnum
   #;$add-cflonum-flonum	#;$add-cflonum-compnum	#;$add-cflonum-cflonum

   $add-fixnum-number		#;$add-bignum-number	$add-flonum-number
   #;$add-ratnum-number		#;$add-compnum-number	#;$add-cflonum-number

   $add-number-fixnum		#;$add-number-bignum	$add-number-flonum
   #;$add-number-ratnum		#;$add-number-compnum	#;$add-number-cflonum

   $add-number-number))

;;; --------------------------------------------------------------------

(declare-core-primitive $add-fixnum-fixnum
    unsafe
  (signatures ((T:fixnum T:fixnum) => (T:number))))

(declare-core-primitive $add-fixnum-bignum
    unsafe
  (signatures ((T:fixnum T:bignum) => (T:number))))

(declare-core-primitive $add-fixnum-flonum
    unsafe
  (signatures ((T:fixnum T:flonum) => (T:number))))

(declare-core-primitive $add-fixnum-ratnum
    unsafe
  (signatures ((T:fixnum T:ratnum) => (T:number))))

(declare-core-primitive $add-fixnum-compnum
    unsafe
  (signatures ((T:fixnum T:compnum) => (T:number))))

(declare-core-primitive $add-fixnum-cflonum
    unsafe
  (signatures ((T:fixnum T:cflonum) => (T:number))))

;;;

(declare-core-primitive $add-bignum-fixnum
    unsafe
  (signatures ((T:bignum T:fixnum) => (T:number))))

(declare-core-primitive $add-bignum-bignum
    unsafe
  (signatures ((T:bignum T:bignum) => (T:number))))

(declare-core-primitive $add-bignum-flonum
    unsafe
  (signatures ((T:bignum T:flonum) => (T:number))))

(declare-core-primitive $add-bignum-ratnum
    unsafe
  (signatures ((T:bignum T:ratnum) => (T:number))))

(declare-core-primitive $add-bignum-compnum
    unsafe
  (signatures ((T:bignum T:compnum) => (T:number))))

(declare-core-primitive $add-bignum-cflonum
    unsafe
  (signatures ((T:bignum T:cflonum) => (T:number))))

;;;

(declare-core-primitive $add-flonum-fixnum
    unsafe
  (signatures ((T:flonum T:fixnum) => (T:number))))

(declare-core-primitive $add-flonum-bignum
    unsafe
  (signatures ((T:flonum T:bignum) => (T:number))))

(declare-core-primitive $add-flonum-flonum
    unsafe
  (signatures ((T:flonum T:flonum) => (T:number))))

(declare-core-primitive $add-flonum-ratnum
    unsafe
  (signatures ((T:flonum T:ratnum) => (T:number))))

(declare-core-primitive $add-flonum-compnum
    unsafe
  (signatures ((T:flonum T:compnum) => (T:number))))

(declare-core-primitive $add-flonum-cflonum
    unsafe
  (signatures ((T:flonum T:cflonum) => (T:number))))

;;;

(declare-core-primitive $add-ratnum-fixnum
    unsafe
  (signatures ((T:ratnum T:fixnum) => (T:number))))

(declare-core-primitive $add-ratnum-bignum
    unsafe
  (signatures ((T:ratnum T:bignum) => (T:number))))

(declare-core-primitive $add-ratnum-flonum
    unsafe
  (signatures ((T:ratnum T:flonum) => (T:number))))

(declare-core-primitive $add-ratnum-ratnum
    unsafe
  (signatures ((T:ratnum T:ratnum) => (T:number))))

(declare-core-primitive $add-ratnum-compnum
    unsafe
  (signatures ((T:ratnum T:compnum) => (T:number))))

(declare-core-primitive $add-ratnum-cflonum
    unsafe
  (signatures ((T:ratnum T:cflonum) => (T:number))))

;;;

(declare-core-primitive $add-compnum-fixnum
    unsafe
  (signatures ((T:compnum T:fixnum) => (T:number))))

(declare-core-primitive $add-compnum-bignum
    unsafe
  (signatures ((T:compnum T:bignum) => (T:number))))

(declare-core-primitive $add-compnum-ratnum
    unsafe
  (signatures ((T:compnum T:ratnum) => (T:number))))

(declare-core-primitive $add-compnum-compnum
    unsafe
  (signatures ((T:compnum T:compnum) => (T:number))))

(declare-core-primitive $add-compnum-flonum
    unsafe
  (signatures ((T:compnum T:flonum) => (T:number))))

(declare-core-primitive $add-compnum-cflonum
    unsafe
  (signatures ((T:compnum T:cflonum) => (T:number))))

;;;

(declare-core-primitive $add-cflonum-fixnum
    unsafe
  (signatures ((T:cflonum T:fixnum) => (T:number))))

(declare-core-primitive $add-cflonum-bignum
    unsafe
  (signatures ((T:cflonum T:bignum) => (T:number))))

(declare-core-primitive $add-cflonum-ratnum
    unsafe
  (signatures ((T:cflonum T:ratnum) => (T:number))))

(declare-core-primitive $add-cflonum-flonum
    unsafe
  (signatures ((T:cflonum T:flonum) => (T:number))))

(declare-core-primitive $add-cflonum-compnum
    unsafe
  (signatures ((T:cflonum T:compnum) => (T:number))))

(declare-core-primitive $add-cflonum-cflonum
    unsafe
  (signatures ((T:cflonum T:cflonum) => (T:number))))

;;;

(declare-core-primitive $add-fixnum-number
    unsafe
  (signatures ((T:fixnum T:number) => (T:number))))

(declare-core-primitive $add-bignum-number
    unsafe
  (signatures ((T:bignum T:number) => (T:number))))

(declare-core-primitive $add-flonum-number
    unsafe
  (signatures ((T:flonum T:number) => (T:number))))

(declare-core-primitive $add-ratnum-number
    unsafe
  (signatures ((T:ratnum T:number) => (T:number))))

(declare-core-primitive $add-compnum-number
    unsafe
  (signatures ((T:compnum T:number) => (T:number))))

(declare-core-primitive $add-cflonum-number
    unsafe
  (signatures ((T:cflonum T:number) => (T:number))))

;;;

(declare-core-primitive $add-number-fixnum
    unsafe
  (signatures ((T:number T:fixnum) => (T:number))))

(declare-core-primitive $add-number-bignum
    unsafe
  (signatures ((T:number T:bignum) => (T:number))))

(declare-core-primitive $add-number-flonum
    unsafe
  (signatures ((T:number T:flonum) => (T:number))))

(declare-core-primitive $add-number-ratnum
    unsafe
  (signatures ((T:number T:ratnum) => (T:number))))

(declare-core-primitive $add-number-compnum
    unsafe
  (signatures ((T:number T:compnum) => (T:number))))

(declare-core-primitive $add-number-cflonum
    unsafe
  (signatures ((T:number T:cflonum) => (T:number))))

(declare-core-primitive $add-number-number
    unsafe
  (signatures ((T:number T:number) => (T:number))))


;;;; core primitives: symbols

(declare-core-primitive putprop
    safe
  (signatures ((T:symbol T:symbol _) => (_)))
  (unsafe-replacements $putprop))

(declare-core-primitive getprop
    safe
  (signatures ((T:symbol T:symbol) => (T:void)))
  (unsafe-replacements $getprop))

(declare-core-primitive remprop
    safe
  (signatures ((T:symbol T:symbol) => (T:void)))
  (unsafe-replacements $remprop))

(declare-core-primitive property-list
    safe
  ;;The return value can be null or a pair.
  (signatures ((T:symbol) => (_)))
  (unsafe-replacements $property-list))

;;; --------------------------------------------------------------------

(declare-core-primitive $putprop
    unsafe
  (signatures ((T:symbol T:symbol _) => (T:void))))

(declare-core-primitive $getprop
    unsafe
  (signatures ((T:symbol T:symbol) => (T:void))))

(declare-core-primitive $remprop
    unsafe
  (signatures ((T:symbol T:symbol) => (T:void))))

(declare-core-primitive $property-list
    unsafe
  (signatures ((T:symbol) => (_))))


;;;; done



;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'declare-core-primitive	'scheme-indent-function 2)
;; End:
