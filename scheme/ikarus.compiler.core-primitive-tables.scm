;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tables of core primitive properties
;;;Date: Sat Sep 20, 2014
;;;
;;;Abstract
;;;
;;;   This file  contains a  table of  core primitive  properties for  both primitive
;;;   functions and primitive operations.
;;;
;;;
;;;Core primitive attributes
;;;-------------------------
;;;
;;;Used  by  the  source  optimiser  to  precompute  the  result  of  core  primitive
;;;applications.  Attributes are the symbols:
;;;
;;;   effect-free -  The application produces no side effects.
;;;
;;;   foldable -     The application can be precomputed at compile time.
;;;
;;;   result-true -  The application always has non-#f result.
;;;
;;;   result-false - The application always has #f result.
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; pairs and lists

(declare-core-primitive null?
    (safe)
  (signatures
   ((T:null)	=> (T:true))
   ((T:pair)	=> (T:false))
   ((_)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive pair?
    (safe)
  (signatures
   ((T:null)	=> (T:false))
   ((T:pair)	=> (T:true))
   ((_)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive list?
    (safe)
  (signatures
   ((T:null)	=> (T:true))
   ((T:pair)	=> (T:true))
   ((_)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive car
    (safe)
  (signatures
   ((T:pair) => (_)))
  (attributes
   ((_)			foldable effect-free))
  (replacements $car))

(declare-core-primitive cdr
    (safe)
  (signatures
   ((T:pair) => (_)))
  (attributes
   ((_)			foldable effect-free))
  (replacements $cdr))

(declare-core-primitive cons
    (safe)
  (signatures
   ((_ _)	=> (T:pair)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive cons*
    (safe)
  (signatures
   (_		=> (_)))
  (attributes
   ;;This will return the operand itself, so it is foldable.
   ((_)			foldable effect-free)
   ;;This is  not foldable because  it needs to return  a newly allocated  list every
   ;;time.
   ((_ . _)		effect-free result-true)))

(declare-core-primitive list
    (safe)
  (signatures
   (()		=> (T:null))
   ((_ . _)	=> (T:pair)))
  (attributes
   ;;Foldable because it returns null.
   (()			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_ . _)		         effect-free result-true)))

(declare-core-primitive reverse
    (safe)
  (signatures
   ((T:null)	=> (T:null))
   ((T:pair)	=> (T:pair)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $car
    (unsafe)
  (signatures
   ((T:pair) => (_)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive $cdr
    (unsafe)
  (signatures
   ((T:pair) => (_)))
  (attributes
   ((_)			foldable effect-free)))


;;;; fixnums

;; (declare-core-primitive fx+ safe
;;   (replacements
;;    ($fx+/overflow T:fixnum T:fixnum)))


;;;; general arithmetics

(declare-core-primitive +
    (safe)
  (signatures (T:number => (T:number)))
  (attributes
   (_			foldable effect-free result-true))
  ;;The commented out variants  handle operands of a type that  is not categorised by
  ;;the core type system.
  (replacements
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
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-bignum
    (unsafe)
  (signatures ((T:fixnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-flonum
    (unsafe)
  (signatures ((T:fixnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-ratnum
    (unsafe)
  (signatures ((T:fixnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-compnum
    (unsafe)
  (signatures ((T:fixnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-cflonum
    (unsafe)
  (signatures ((T:fixnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-bignum-fixnum
    (unsafe)
  (signatures ((T:bignum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-bignum
    (unsafe)
  (signatures ((T:bignum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-flonum
    (unsafe)
  (signatures ((T:bignum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-ratnum
    (unsafe)
  (signatures ((T:bignum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-compnum
    (unsafe)
  (signatures ((T:bignum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-cflonum
    (unsafe)
  (signatures ((T:bignum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-flonum-fixnum
    (unsafe)
  (signatures ((T:flonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-bignum
    (unsafe)
  (signatures ((T:flonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-flonum
    (unsafe)
  (signatures ((T:flonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-ratnum
    (unsafe)
  (signatures ((T:flonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-compnum
    (unsafe)
  (signatures ((T:flonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-cflonum
    (unsafe)
  (signatures ((T:flonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-ratnum-fixnum
    (unsafe)
  (signatures ((T:ratnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-bignum
    (unsafe)
  (signatures ((T:ratnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-flonum
    (unsafe)
  (signatures ((T:ratnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-ratnum
    (unsafe)
  (signatures ((T:ratnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-compnum
    (unsafe)
  (signatures ((T:ratnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-cflonum
    (unsafe)
  (signatures ((T:ratnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-compnum-fixnum
    (unsafe)
  (signatures ((T:compnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-bignum
    (unsafe)
  (signatures ((T:compnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-ratnum
    (unsafe)
  (signatures
   ((T:compnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-compnum
    (unsafe)
  (signatures ((T:compnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-flonum
    (unsafe)
  (signatures ((T:compnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-cflonum
    (unsafe)
  (signatures ((T:compnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-cflonum-fixnum
    (unsafe)
  (signatures ((T:cflonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-bignum
    (unsafe)
  (signatures ((T:cflonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-ratnum
    (unsafe)
  (signatures ((T:cflonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-flonum
    (unsafe)
  (signatures ((T:cflonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-compnum
    (unsafe)
  (signatures ((T:cflonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-cflonum
    (unsafe)
  (signatures ((T:cflonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-fixnum-number
    (unsafe)
  (signatures ((T:fixnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-number
    (unsafe)
  (signatures ((T:bignum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-number
    (unsafe)
  (signatures ((T:flonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-number
    (unsafe)
  (signatures ((T:ratnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-number
    (unsafe)
  (signatures ((T:compnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-number
    (unsafe)
  (signatures ((T:cflonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-number-fixnum
    (unsafe)
  (signatures ((T:number T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-bignum
    (unsafe)
  (signatures ((T:number T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-flonum
    (unsafe)
  (signatures ((T:number T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-ratnum
    (unsafe)
  (signatures ((T:number T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-compnum
    (unsafe)
  (signatures ((T:number T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-cflonum
    (unsafe)
  (signatures ((T:number T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-number
    (unsafe)
  (signatures ((T:number T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))


;;;; core primitives: symbols

(declare-core-primitive putprop
    (safe)
  (signatures
   ((T:symbol T:symbol _) => (_)))
  (attributes
   ((_ _ _)		result-true))
  (replacements $putprop))

(declare-core-primitive getprop
    (safe)
  (signatures ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		effect-free result-true))
  (replacements $getprop))

(declare-core-primitive remprop
    (safe)
  (signatures ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		result-true))
  (replacements $remprop))

(declare-core-primitive property-list
    (safe)
  ;;The return value can be null or a pair.
  (signatures ((T:symbol) => (_)))
  (attributes
   ((_)			effect-free result-true))
  (replacements $property-list))

;;; --------------------------------------------------------------------

(declare-core-primitive $putprop
    (unsafe)
  (signatures ((T:symbol T:symbol _) => (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive $getprop
    (unsafe)
  (signatures ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $remprop
    (unsafe)
  (signatures ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive $property-list
    (unsafe)
  (signatures ((T:symbol) => (_)))
  (attributes
   ((_)			effect-free	result-true)))


;;;; strings
;;
;;According to R6RS:  STRING and MAKE-STRING must return a  newly allocated string at
;;every invocation;  if we want the  same string we  just use the double  quotes.  So
;;STRING and MAKE-STRING are not FOLDABLE.
;;

(declare-core-primitive string
    (safe)
  (signatures
   (()		=> (T:string))
   (T:char	=> (T:string)))
  (attributes
   (()			effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive make-string
    (safe)
  (signatures
   ((T:fixnum)		=> (T:string))
   ((T:fixnum T:char)	=> (T:string)))
  (attributes
   ((0)			effect-free result-true)
   ((0 . _)		effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive string?
    (safe)
  (signatures
   ((_)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive string-length
    (safe)
  (signatures
   ((T:string)	=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

;;This cannot have  replacements because there is  no way to validate  the index with
;;respect to the string.
(declare-core-primitive string-ref
    (safe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;This cannot have  replacements because there is  no way to validate  the index with
;;respect to the string.
(declare-core-primitive string-set!
    (safe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive string->number
    (safe)
  (signatures
   ((T:string)		=> (_))
   ((T:string T:fixnum)	=> (_)))
  (attributes
   ((_)			foldable effect-free)
   ((_ _)		foldable effect-free)))

(declare-core-primitive string->utf8
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf16be
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf16le
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf16n
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf32
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->ascii
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->latin1
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->symbol
    (safe)
  (signatures
   ((T:string)		=> (T:symbol)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->keyword
    (safe)
  (signatures
   ((T:string)		=> (T:other-object)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((T:string)	=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-ref
    (safe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (safe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))



#|

      ;;According to R6RS: MAKE-BYTEVECTOR must return a newly allocated
      ;;string at every invocation; so it is not foldable.
      ((make-bytevector 0)		    effect-free result-true)
      ((make-bytevector 0 _)		    effect-free result-true)
      ((make-bytevector . _)				result-true)

      ;;According to  R6RS: VECTOR and	MAKE-VECTOR must return	 a newly
      ;;allocated string at every invocation; so they are not foldable.
      ((vector)				    effect-free result-true)
      ((vector . _)			    effect-free result-true)
      ((make-vector 0)			    effect-free result-true)
      ((make-vector 0 _)		    effect-free result-true)
      ((make-vector . _)		    effect-free result-true)

      ((vector-length _)	   foldable effect-free result-true)
      ((vector-ref _ _)		   foldable effect-free		   )
      ((vector-set! _ _)	   foldable 			   )
      ((eq? _ _)		   foldable effect-free		   )
      ((eqv? _ _)		   foldable effect-free		   )
      ((assq _ _)		   foldable effect-free		   )
      ((assv _ _)		   foldable effect-free		   )
      ((assoc _ _)		   foldable effect-free		   )
      ((not _)			   foldable effect-free		   )
      ((fixnum? _)		   foldable effect-free		   )
      ((vector? _)		   foldable effect-free		   )
      ((char? _)		   foldable effect-free		   )
      ((symbol? _)		   foldable effect-free		   )
      ((procedure? _)		   foldable effect-free		   )
      ((eof-object? _)		   foldable effect-free		   )
      ((flonum? _)		   foldable effect-free		   )
      ((cflonum? _)		   foldable effect-free		   )
      ((compnum? _)		   foldable effect-free		   )
      ((integer? _)		   foldable effect-free		   )
      ((bignum? _)		   foldable effect-free		   )
      ((ratnum? _)		   foldable effect-free		   )
      ((pointer? _)		   foldable effect-free		   )
      ((void)			   foldable effect-free result-true)
      ((set-car! _ _)		   foldable			   )
      ((set-cdr! _ _)		   foldable			   )
      ((caar _)			   foldable effect-free		   )
      ((cadr _)			   foldable effect-free		   )
      ((cdar _)			   foldable effect-free		   )
      ((cddr _)			   foldable effect-free		   )
      ((caaar _)		   foldable effect-free		   )
      ((caadr _)		   foldable effect-free		   )
      ((cadar _)		   foldable effect-free		   )
      ((caddr _)		   foldable effect-free		   )
      ((cdaar _)		   foldable effect-free		   )
      ((cdadr _)		   foldable effect-free		   )
      ((cddar _)		   foldable effect-free		   )
      ((cdddr _)		   foldable effect-free		   )
      ((caaaar _)		   foldable effect-free		   )
      ((caaadr _)		   foldable effect-free		   )
      ((caadar _)		   foldable effect-free		   )
      ((caaddr _)		   foldable effect-free		   )
      ((cadaar _)		   foldable effect-free		   )
      ((cadadr _)		   foldable effect-free		   )
      ((caddar _)		   foldable effect-free		   )
      ((cadddr _)		   foldable effect-free		   )
      ((cdaaar _)		   foldable effect-free		   )
      ((cdaadr _)		   foldable effect-free		   )
      ((cdadar _)		   foldable effect-free		   )
      ((cdaddr _)		   foldable effect-free		   )
      ((cddaar _)		   foldable effect-free		   )
      ((cddadr _)		   foldable effect-free		   )
      ((cdddar _)		   foldable effect-free		   )
      ((cddddr _)		   foldable effect-free		   )
      (($car _)			   foldable effect-free		   )
      (($cdr _)			   foldable effect-free		   )
      (($set-car! _ _)		   foldable			   )
      (($set-cdr! _ _)		   foldable			   )
      ((memq _ _)		   foldable effect-free		   )
      ((memv _ _)		   foldable effect-free		   )
      ((length _)		   foldable effect-free result-true)
;;;
      ((* . _)			   foldable effect-free result-true)
      ((/ _ . _)		   foldable effect-free result-true)
      ((- _ . _)		   foldable effect-free result-true)
      ((real-part _)		   foldable effect-free result-true)
      ((imag-part _)		   foldable effect-free result-true)
      ((greatest-fixnum)	   foldable effect-free result-true)
      ((least-fixnum)		   foldable effect-free result-true)
      ((fixnum-width)		   foldable effect-free result-true)
      ((char->integer _)	   foldable effect-free result-true)
      ((integer->char _)	   foldable effect-free result-true)
      ((eof-object)		   foldable effect-free result-true)
      ((zero? _)		   foldable effect-free		   )
      ((= _ . _)		   foldable effect-free		   )
      ((< _ . _)		   foldable effect-free		   )
      ((<= _ . _)		   foldable effect-free		   )
      ((> _ . _)		   foldable effect-free		   )
      ((>= _ . _)		   foldable effect-free		   )
      ((expt _ _)		   foldable effect-free result-true)
      ((log _)			   foldable effect-free result-true)
      ((sll _ _)		   foldable effect-free result-true)
      ((sra _ _)		   foldable effect-free result-true)
      ((inexact _)		   foldable effect-free result-true)
      ((exact _)		   foldable effect-free result-true)
      ((add1 _)			   foldable effect-free result-true)
      ((sub1 _)			   foldable effect-free result-true)
      ((bitwise-and _ _)	   foldable effect-free result-true)
      ((make-rectangular _ _)	   foldable effect-free result-true)
      ((sin _)			   foldable effect-free result-true)
      ((cos _)			   foldable effect-free result-true)
      ((tan _)			   foldable effect-free result-true)
      ((asin _)			   foldable effect-free result-true)
      ((acos _)			   foldable effect-free result-true)
      ((atan _)			   foldable effect-free result-true)
      ((atan _ _)		   foldable effect-free result-true)
      ((make-eq-hashtable)		    effect-free result-true)

;;; --------------------------------------------------------------------
;;; fixnums

      ((fx+ _ _)		   foldable effect-free result-true)
      ((fx- _ _)		   foldable effect-free result-true)
      ((fx* _ _)		   foldable effect-free result-true)
      ((fxior . _)		   foldable effect-free result-true)
      ((fxlogor . _)		   foldable effect-free result-true)
      ((fxnot _)		   foldable effect-free result-true)
      ((fxadd1 _)		   foldable effect-free result-true)
      ((fxsub1 _)		   foldable effect-free result-true)
      ((fxzero? _)		   foldable effect-free            )
      ((fxpositive? _)		   foldable effect-free            )
      ((fxnegative? _)		   foldable effect-free            )
      ((fx=? _ . _)		   foldable effect-free		   )
      ((fx<? _ . _)		   foldable effect-free		   )
      ((fx<=? _ . _)		   foldable effect-free		   )
      ((fx>? _ . _)		   foldable effect-free		   )
      ((fx>=? _ . _)		   foldable effect-free		   )
      ((fx= _ . _)		   foldable effect-free		   )
      ((fx< _ . _)		   foldable effect-free		   )
      ((fx<= _ . _)		   foldable effect-free		   )
      ((fx> _ . _)		   foldable effect-free		   )
      ((fx>= _ . _)		   foldable effect-free		   )
      ((fxmin _ _)		   foldable effect-free result-true)
      ((fxmax _ _)		   foldable effect-free result-true)
      ((fxsll _ _)		   foldable effect-free result-true)
      ((fxsra _ _)		   foldable effect-free result-true)
      ((fxremainder _ _)	   foldable effect-free result-true)
      ((fxquotient _ _)		   foldable effect-free result-true)
      ((fxmodulo _ _)		   foldable effect-free result-true)
      ((fxsign _)		   foldable effect-free result-true)

      (($fixnum->flonum _)	   foldable effect-free result-true)
      (($char->fixnum _)	   foldable effect-free result-true)
      (($fixnum->char _)	   foldable effect-free result-true)
      (($fxzero? _)		   foldable effect-free		   )
      (($fxpositive? _)		   foldable effect-free		   )
      (($fxnegative? _)		   foldable effect-free		   )
      (($fx+ _ _)		   foldable effect-free result-true)
      (($fx* _ _)		   foldable effect-free result-true)
      (($fx- _ _)		   foldable effect-free result-true)
      (($fx= _ _)		   foldable effect-free		   )
      (($fx>= _ _)		   foldable effect-free		   )
      (($fx> _ _)		   foldable effect-free		   )
      (($fx<= _ _)		   foldable effect-free		   )
      (($fx< _ _)		   foldable effect-free		   )
      (($fxmin _ _)		   foldable effect-free result-true)
      (($fxmax _ _)		   foldable effect-free result-true)
      (($struct-ref _ _)	   foldable effect-free		   )
      (($struct/rtd? _ _)	   foldable effect-free		   )
      (($fxsll _ _)		   foldable effect-free result-true)
      (($fxsra _ _)		   foldable effect-free result-true)
      (($fxlogor _ _)		   foldable effect-free result-true)
      (($fxlogand _ _)		   foldable effect-free result-true)
      (($fxadd1 _)		   foldable effect-free result-true)
      (($fxsub1 _)		   foldable effect-free result-true)
      (($fxsign _)		   foldable effect-free result-true)
      (($fxdiv _ _)		   foldable effect-free result-true)
      (($fxdiv0 _ _)		   foldable effect-free result-true)
      (($fxmod _ _)		   foldable effect-free result-true)
      (($fxmod0 _ _)		   foldable effect-free result-true)
      ;;Do we support multiple values?  No!!!
      ;;(($fxdiv-and-mod _ _)	   foldable effect-free result-true)
      ;;(($fxdiv0-and-mod0 _ _)	   foldable effect-free result-true)

;;; --------------------------------------------------------------------
;;; ratnums

      (($make-ratnum _ _)	   foldable effect-free result-true)
      (($make-rational _ _)	   foldable effect-free result-true)
      (($ratnum-n _)		   foldable effect-free result-true)
      (($ratnum-d _)		   foldable effect-free result-true)
      (($ratnum-num _)		   foldable effect-free result-true)
      (($ratnum-den _)		   foldable effect-free result-true)

;;; --------------------------------------------------------------------
;;; flonums

      ((inexact->exact _)	   foldable effect-free result-true)
      ((exact _)		   foldable effect-free result-true)
      ((fixnum->flonum _)	   foldable effect-free result-true)
      ((flzero? _)		   foldable effect-free            )
      ((flpositive? _)		   foldable effect-free            )
      ((flnegative? _)		   foldable effect-free            )
      ((fleven? _)		   foldable effect-free            )
      ((flodd? _)		   foldable effect-free            )
      ((flround _)		   foldable effect-free result-true)
      ((flfloor _)		   foldable effect-free result-true)
      ((flceiling _)		   foldable effect-free result-true)
      ((fltruncate _)		   foldable effect-free result-true)
      ((flnumerator _)		   foldable effect-free result-true)
      ((fldenominator _)	   foldable effect-free result-true)
      ((flabs _)		   foldable effect-free result-true)
      ((flsin _)		   foldable effect-free result-true)
      ((flcos _)		   foldable effect-free result-true)
      ((fltan _)		   foldable effect-free result-true)
      ((flasin _)		   foldable effect-free result-true)
      ((flacos _)		   foldable effect-free result-true)
      ((flatan _)		   foldable effect-free result-true)
      ((flatan _ _)		   foldable effect-free result-true)
      ((flexp _)		   foldable effect-free result-true)
      ((fllog _)		   foldable effect-free result-true)
      ((fllog _ _)		   foldable effect-free result-true)
      ((flexpm1 _)		   foldable effect-free result-true)
      ((fllog1p _)		   foldable effect-free result-true)
      ((flexpt _)		   foldable effect-free result-true)
      ((flsqrt _)		   foldable effect-free result-true)
      ((flsquare _)		   foldable effect-free result-true)
      ((flinteger? _)		   foldable effect-free            )
      ((flnan? _)		   foldable effect-free            )
      ((flfinite? _)		   foldable effect-free            )
      ((flinfinite? _)		   foldable effect-free            )
      ((fl=? _ _)		   foldable effect-free            )
      ((fl<? _ _)		   foldable effect-free            )
      ((fl>? _ _)		   foldable effect-free            )
      ((fl<=? _ _)		   foldable effect-free            )
      ((fl>=? _ _)		   foldable effect-free            )
      ((fl+)			   foldable effect-free result-true)
      ((fl+ _)			   foldable effect-free result-true)
      ((fl+ _ _)		   foldable effect-free result-true)
      ((fl+ _ _ _)		   foldable effect-free result-true)
      ((fl+ _ _ _ _ . _)	   foldable effect-free result-true)
      ((fl- _)			   foldable effect-free result-true)
      ((fl- _ _)		   foldable effect-free result-true)
      ((fl- _ _ _)		   foldable effect-free result-true)
      ((fl- _ _ _ _ . _)	   foldable effect-free result-true)
      ((fl*)			   foldable effect-free result-true)
      ((fl* _)			   foldable effect-free result-true)
      ((fl* _ _)		   foldable effect-free result-true)
      ((fl* _ _ _)		   foldable effect-free result-true)
      ((fl* _ _ _ . _)		   foldable effect-free result-true)
      ((fl/ _)			   foldable effect-free result-true)
      ((fl/ _ _)		   foldable effect-free result-true)
      ((fl/ _ _ _)		   foldable effect-free result-true)
      ((fl/ _ _ _ . _)		   foldable effect-free result-true)
      ((flmax _)		   foldable effect-free result-true)
      ((flmax _ _)		   foldable effect-free result-true)
      ((flmax _ _ _ . _)	   foldable effect-free result-true)
      ((flmin _)		   foldable effect-free result-true)
      ((flmin _ _)		   foldable effect-free result-true)
      ((flmin _ _ _ . _)	   foldable effect-free result-true)

      ;;$MAKE-FLONUM must return a new flonum every time.
      (($make-flonum . _)	            effect-free result-true)
      (($flonum->exact _)	   foldable effect-free result-true)
      (($flzero? _)		   foldable effect-free            )
      (($flpositive? _)		   foldable effect-free            )
      (($flnegative? _)		   foldable effect-free            )
      (($fleven? _)		   foldable effect-free            )
      (($flodd? _)		   foldable effect-free            )
      (($flnan? _)		   foldable effect-free            )
      (($flfinite? _)		   foldable effect-free            )
      (($flinfinite? _)		   foldable effect-free            )
      (($flonum-integer? _)	   foldable effect-free            )
      (($flonum-rational? _)	   foldable effect-free            )
      (($flround _)		   foldable effect-free result-true)
      (($flfloor _)		   foldable effect-free result-true)
      (($flceiling _)		   foldable effect-free result-true)
      (($fltruncate _)		   foldable effect-free result-true)
      (($flnumerator _)		   foldable effect-free result-true)
      (($fldenominator _)	   foldable effect-free result-true)
      (($flabs _)		   foldable effect-free result-true)
      (($flsin _)		   foldable effect-free result-true)
      (($flcos _)		   foldable effect-free result-true)
      (($fltan _)		   foldable effect-free result-true)
      (($flasin _)		   foldable effect-free result-true)
      (($flacos _)		   foldable effect-free result-true)
      (($flatan _)		   foldable effect-free result-true)
      (($flatan2 _ _)		   foldable effect-free result-true)
      (($flexp _)		   foldable effect-free result-true)
      (($fllog _)		   foldable effect-free result-true)
      (($fllog2 _ _)		   foldable effect-free result-true)
      (($flexpm1 _)		   foldable effect-free result-true)
      (($fllog1p _)		   foldable effect-free result-true)
      (($flexpt _)		   foldable effect-free result-true)
      (($flsqrt _)		   foldable effect-free result-true)
      (($flsquare _)		   foldable effect-free result-true)
      (($flmax _ _)		   foldable effect-free result-true)
      (($flmin _ _)		   foldable effect-free result-true)
      (($fl= _ _)		   foldable effect-free            )
      (($fl< _ _)		   foldable effect-free            )
      (($fl> _ _)		   foldable effect-free            )
      (($fl<= _ _)		   foldable effect-free            )
      (($fl>= _ _)		   foldable effect-free            )
      (($fl+ _ _)		   foldable effect-free result-true)
      (($fl- _ _)		   foldable effect-free result-true)
      (($fl* _ _)		   foldable effect-free result-true)
      (($fl/ _ _)		   foldable effect-free result-true)
      (($fldiv _ _)		   foldable effect-free result-true)
      (($flmod _ _)		   foldable effect-free result-true)
      (($fldiv0 _ _)		   foldable effect-free result-true)
      (($flmod0 _ _)		   foldable effect-free result-true)
      ;;We do not do multiple return values.
      ;;(($fldiv-and-mod _ _)	   foldable effect-free result-true)
      ;;(($fldiv0-and-mod0 _ _)	   foldable effect-free result-true)

;;; --------------------------------------------------------------------
;;; vectors

      (($vector-length _)	   foldable effect-free result-true)
      (($vector-ref _ _)	   foldable effect-free)

;;; --------------------------------------------------------------------
;;; bytevectors

      ;;$MAKE-BYTEVECTOR  must not  be foldable:  it must  return a  new
      ;;bytevector every time.
      (($make-bytevector 0)		    effect-free result-true)
      (($make-bytevector 0 _)		    effect-free result-true)
      (($make-bytevector . _)		    effect-free result-true)
      (($bytevector-u8-ref _ _)	   foldable effect-free result-true)
      (($bytevector-length _)	   foldable effect-free result-true)

;;; --------------------------------------------------------------------

      ((annotation? #f)		    foldable effect-free result-false)
      ((annotation-stripped #f)	    foldable effect-free result-false)

;;; --------------------------------------------------------------------

      ;;This must return a new struct every time.
      (($struct . _)			     effect-free result-true)

      ((condition . _))
      ((top-level-value . _))
      ((make-message-condition . _))
      ((make-lexical-violation . _))
      ((make-who-condition . _))
      ((make-error . _))
      ((make-i/o-error . _))
      ((make-i/o-write-error . _))
      ((make-i/o-read-error . _))
      ((make-i/o-file-already-exists-error . _))
      ((make-i/o-file-is-read-only-error . _))
      ((make-i/o-file-protection-error . _))
      ((make-i/o-file-does-not-exist-error . _))
      ((make-undefined-violation . _))
      ((die . _))
      ;;This must return a new gensym every time.
      ((gensym . _)				effect-free result-true)
      ((values . _))
      ((error . _))
      ((assertion-violation . _))
      ;;FIXME Reduce to display.  (Abdulaziz Ghuloum)
      ((printf . _))
      ((newline . _))
      ((native-transcoder . _))
      ((open-string-output-port . _))
      ((open-string-input-port . _))
      ((environment . _))
      ((print-gensym . _))
      ((exit . _))
      ((interrupt-handler . _))
      ((display . _))
      ((write-char . _))
      ((current-input-port . _))
      ((current-output-port . _))
      ((current-error-port . _))
      ((standard-input-port . _))
      ((standard-output-port . _))
      ((standard-error-port . _))

      ;;It appears that something can go  wrong if we do these, not sure
      ;;why.  (Marco Maggi; Nov 3, 2012)
      ;;
      ;;((current-input-port . _)		 effect-free result-true)
      ;;((current-output-port . _)		 effect-free result-true)
      ;;((current-error-port . _)		 effect-free result-true)

      ((standard-input-port . _)		 effect-free result-true)
      ((standard-output-port . _)		 effect-free result-true)
      ((standard-error-port . _)		 effect-free result-true)
      ((console-input-port . _)			 effect-free result-true)
      ((console-output-port . _)		 effect-free result-true)
      ((console-error-port . _)			 effect-free result-true)
      (($current-frame . _))
      ((pretty-width . _))
      (($fp-at-base . _))
      ((get-annotated-datum . _))
      (($collect-key . _))
      ((make-non-continuable-violation . _))

      ;;FIXME Reduce to string-copy (Abdulaziz Ghuloum).
      ((format . _))
      ((uuid . _))
      ((print-graph . _))
      ((interaction-environment . _))
      ((make-guardian)					 effect-free result-true)
      ((command-line-arguments))
      ;;FIXME (Abdulaziz Ghuloum)
      ((make-record-type-descriptor . _))
      ((record-constructor _)				 effect-free result-true)
      ((record-predicate _)				 effect-free result-true)
      ((record-accessor . _)				 effect-free result-true)
      ((record-mutator . _)				 effect-free result-true)
      ((make-assertion-violation . _))
      ((new-cafe . _))
      ((getenv . _))
      ((gensym-prefix . _))
      (($arg-list . _))
      (($make-symbol . _)				 effect-free result-true)
      (($make-call-with-values-procedure . _))
      (($make-values-procedure . _))
      (($unset-interrupted! . _))
      ((make-interrupted-condition . _))
      (($interrupted? . _))
      (($symbol-value . _))
      ((library-extensions . _))
      ;;The base struct type descriptor is a constant created at process
      ;;boot time.
      ((base-rtd . _))
      (($data->transcoder . _)			foldable effect-free result-true)
      ((current-time . _))

|#


;;;; done



;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'declare-core-primitive	'scheme-indent-function 2)
;; End:
