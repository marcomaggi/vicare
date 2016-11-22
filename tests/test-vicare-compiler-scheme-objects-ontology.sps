;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for scheme objects core types
;;;Date: Thu Dec  4, 2014
;;;
;;;Abstract
;;;
;;;	This file imports directly one of  the source libraries that compose the boot
;;;	image.
;;;
;;;Copyright (C) 2014, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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


#!r6rs
(program (test-vicare-compiler-scheme-objects-ontology)
  (options strict-r6rs)
  (import (except (vicare)
		  maybe)
    (ikarus.compiler.scheme-objects-ontology)
    (vicare expander)
    (vicare checks))

(import SCHEME-OBJECTS-ONTOLOGY)

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compiler: tests for scheme objects core types\n")


;;;; helpers

(define-constant yes	'yes)
(define-constant no	'no)
(define-constant maybe	'maybe)


(parametrise ((check-test-name	'base))

  (check (T:object? T:object)			=> yes)
  (check (T:object? T:other-object)		=> yes)
  (check (T:object? T:true)			=> yes)
  (check (T:object? (core-type-tag-and T:true T:false))	=> no)

  (check (T:true? T:object)			=> maybe)
  (check (T:true? T:true)			=> yes)
  (check (T:true? T:false)			=> no)
  (check (T:true? T:null)			=> no)

  (check (T:non-false? T:true)			=> yes)
  (check (T:non-false? T:null)			=> yes)
  (check (T:non-false? T:false)			=> no)
  (check (T:non-false? T:boolean)		=> maybe)
  (check (T:non-false? T:object)		=> maybe)

  #t)


(parametrise ((check-test-name	'boolean))

  (check (T:boolean? T:true)			=> yes)
  (check (T:boolean? T:false)			=> yes)
  (check (T:boolean? (core-type-tag-ior  T:true T:false)) => yes)
  (check (T:boolean? (core-type-tag-and T:true T:false)) => no)

  #t)


(parametrise ((check-test-name	'numerics))

  (check (T:number? T:fixnum)			=> yes)
  (check (T:number? T:bignum)			=> yes)
  (check (T:number? T:ratnum)			=> yes)
  (check (T:number? T:flonum)			=> yes)
  (check (T:number? T:cflonum)			=> yes)
  (check (T:number? T:compnum)			=> yes)
  (check (T:number? T:number)			=> yes)
  (check (T:number? T:exact)			=> yes)
  (check (T:number? T:inexact)			=> yes)
  (check (T:number? T:exact-integer)		=> yes)
  (check (T:number? T:real)			=> yes)
  (check (T:number? T:non-real)			=> yes)
  (check (T:number? T:string)			=> no)

  (check (T:exact? T:exact)			=> yes)
  (check (T:exact? T:inexact)			=> no)
  (check (T:exact? T:fixnum)			=> yes)
  (check (T:exact? T:bignum)			=> yes)
  (check (T:exact? T:ratnum)			=> yes)
  (check (T:exact? T:flonum)			=> no)
  (check (T:exact? T:cflonum)			=> no)
  (check (T:exact? T:compnum)			=> maybe)
  (check (T:exact? T:number)			=> maybe)
  (check (T:exact? T:exact-integer)		=> yes)
  (check (T:exact? T:real)			=> maybe)
  (check (T:exact? T:non-real)			=> maybe)
  (check (T:exact? T:string)			=> no)

  (check (T:inexact? T:exact)			=> no)
  (check (T:inexact? T:inexact)			=> yes)
  (check (T:inexact? T:fixnum)			=> no)
  (check (T:inexact? T:bignum)			=> no)
  (check (T:inexact? T:ratnum)			=> no)
  (check (T:inexact? T:flonum)			=> yes)
  (check (T:inexact? T:cflonum)			=> yes)
  (check (T:inexact? T:compnum)			=> maybe)
  (check (T:inexact? T:number)			=> maybe)
  (check (T:inexact? T:exact-integer)		=> no)
  (check (T:inexact? T:real)			=> maybe)
  (check (T:inexact? T:non-real)		=> maybe)
  (check (T:inexact? T:string)			=> no)

  (check (T:exact-integer? T:exact)		=> maybe)
  (check (T:exact-integer? T:inexact)		=> no)
  (check (T:exact-integer? T:fixnum)		=> yes)
  (check (T:exact-integer? T:bignum)		=> yes)
  (check (T:exact-integer? T:ratnum)		=> no)
  (check (T:exact-integer? T:flonum)		=> no)
  (check (T:exact-integer? T:cflonum)		=> no)
  (check (T:exact-integer? T:compnum)		=> no)
  (check (T:exact-integer? T:number)		=> maybe)
  (check (T:exact-integer? T:exact-integer)	=> yes)
  (check (T:exact-integer? T:real)		=> maybe)
  (check (T:exact-integer? T:non-real)		=> no)
  (check (T:exact-integer? T:string)		=> no)

  (check (T:real? T:exact)			=> maybe)
  (check (T:real? T:inexact)			=> maybe)
  (check (T:real? T:fixnum)			=> yes)
  (check (T:real? T:bignum)			=> yes)
  (check (T:real? T:ratnum)			=> yes)
  (check (T:real? T:flonum)			=> yes)
  (check (T:real? T:cflonum)			=> no)
  (check (T:real? T:compnum)			=> no)
  (check (T:real? T:number)			=> maybe)
  (check (T:real? T:exact-integer)		=> yes)
  (check (T:real? T:real)			=> yes)
  (check (T:real? T:non-real)			=> no)
  (check (T:real? T:string)			=> no)

  (check (T:non-real? T:exact)			=> maybe)
  (check (T:non-real? T:inexact)			=> maybe)
  (check (T:non-real? T:fixnum)			=> no)
  (check (T:non-real? T:bignum)			=> no)
  (check (T:non-real? T:ratnum)			=> no)
  (check (T:non-real? T:flonum)			=> no)
  (check (T:non-real? T:cflonum)			=> yes)
  (check (T:non-real? T:compnum)			=> yes)
  (check (T:non-real? T:number)			=> maybe)
  (check (T:non-real? T:exact-integer)		=> no)
  (check (T:non-real? T:real)			=> no)
  (check (T:non-real? T:non-real)			=> yes)
  (check (T:non-real? T:string)			=> no)

  (check (T:exact-real? T:exact)		=> maybe)
  (check (T:exact-real? T:inexact)		=> no)
  (check (T:exact-real? T:fixnum)		=> yes)
  (check (T:exact-real? T:bignum)		=> yes)
  (check (T:exact-real? T:ratnum)		=> yes)
  (check (T:exact-real? T:flonum)		=> no)
  (check (T:exact-real? T:cflonum)		=> no)
  (check (T:exact-real? T:compnum)		=> no)
  (check (T:exact-real? T:number)		=> maybe)
  (check (T:exact-real? T:exact-integer)	=> yes)
  (check (T:exact-real? T:exact-real)		=> yes)
  (check (T:exact-real? T:real)			=> maybe)
  (check (T:exact-real? T:non-real)		=> no)
  (check (T:exact-real? T:string)		=> no)

  (check (T:flonum? T:exact)			=> no)
  (check (T:flonum? T:inexact)			=> maybe)
  (check (T:flonum? T:fixnum)			=> no)
  (check (T:flonum? T:bignum)			=> no)
  (check (T:flonum? T:ratnum)			=> no)
  (check (T:flonum? T:flonum)			=> yes)
  (check (T:flonum? T:flonum-integer)		=> yes)
  (check (T:flonum? T:flonum-fractional)	=> yes)
  (check (T:flonum? T:flonum-finite)		=> yes)
  (check (T:flonum? T:flonum-infinite)		=> yes)
  (check (T:flonum? T:flonum-nan)		=> yes)
  (check (T:flonum? T:cflonum)			=> no)
  (check (T:flonum? T:compnum)			=> no)
  (check (T:flonum? T:number)			=> maybe)
  (check (T:flonum? T:exact-integer)		=> no)
  (check (T:flonum? T:exact-real)		=> no)
  (check (T:flonum? T:real)			=> maybe)
  (check (T:flonum? T:non-real)			=> no)
  (check (T:flonum? T:string)			=> no)

  (check (T:flonum-integer? T:exact)		=> no)
  (check (T:flonum-integer? T:inexact)		=> maybe)
  (check (T:flonum-integer? T:fixnum)		=> no)
  (check (T:flonum-integer? T:bignum)		=> no)
  (check (T:flonum-integer? T:ratnum)		=> no)
  (check (T:flonum-integer? T:flonum)		=> maybe)
  (check (T:flonum-integer? T:flonum-integer)	=> yes)
  (check (T:flonum-integer? T:flonum-fractional) => no)
  (check (T:flonum-integer? T:flonum-finite)	=> maybe)
  (check (T:flonum-integer? T:flonum-infinite)	=> no)
  (check (T:flonum-integer? T:flonum-nan)	=> no)
  (check (T:flonum-integer? T:cflonum)		=> no)
  (check (T:flonum-integer? T:compnum)		=> no)
  (check (T:flonum-integer? T:number)		=> maybe)
  (check (T:flonum-integer? T:exact-integer)	=> no)
  (check (T:flonum-integer? T:exact-real)	=> no)
  (check (T:flonum-integer? T:real)		=> maybe)
  (check (T:flonum-integer? T:non-real)		=> no)
  (check (T:flonum-integer? T:string)		=> no)

  (check (T:flonum-fractional? T:exact)			=> no)
  (check (T:flonum-fractional? T:inexact)		=> maybe)
  (check (T:flonum-fractional? T:fixnum)		=> no)
  (check (T:flonum-fractional? T:bignum)		=> no)
  (check (T:flonum-fractional? T:ratnum)		=> no)
  (check (T:flonum-fractional? T:flonum)		=> maybe)
  (check (T:flonum-fractional? T:flonum-integer)	=> no)
  (check (T:flonum-fractional? T:flonum-fractional)	=> yes)
  (check (T:flonum-fractional? T:flonum-finite)		=> maybe)
  (check (T:flonum-fractional? T:flonum-infinite)	=> no)
  (check (T:flonum-fractional? T:flonum-nan)		=> no)
  (check (T:flonum-fractional? T:cflonum)		=> no)
  (check (T:flonum-fractional? T:compnum)		=> no)
  (check (T:flonum-fractional? T:number)		=> maybe)
  (check (T:flonum-fractional? T:exact-integer)		=> no)
  (check (T:flonum-fractional? T:exact-real)		=> no)
  (check (T:flonum-fractional? T:real)			=> maybe)
  (check (T:flonum-fractional? T:non-real)		=> no)
  (check (T:flonum-fractional? T:string)		=> no)

  (check (T:flonum-finite? T:exact)		=> no)
  (check (T:flonum-finite? T:inexact)		=> maybe)
  (check (T:flonum-finite? T:fixnum)		=> no)
  (check (T:flonum-finite? T:bignum)		=> no)
  (check (T:flonum-finite? T:ratnum)		=> no)
  (check (T:flonum-finite? T:flonum)		=> maybe)
  (check (T:flonum-finite? T:flonum-integer)	=> yes)
  (check (T:flonum-finite? T:flonum-fractional)	=> yes)
  (check (T:flonum-finite? T:flonum-finite)	=> yes)
  (check (T:flonum-finite? T:flonum-infinite)	=> no)
  (check (T:flonum-finite? T:flonum-nan)	=> no)
  (check (T:flonum-finite? T:cflonum)		=> no)
  (check (T:flonum-finite? T:compnum)		=> no)
  (check (T:flonum-finite? T:number)		=> maybe)
  (check (T:flonum-finite? T:exact-integer)	=> no)
  (check (T:flonum-finite? T:exact-real)	=> no)
  (check (T:flonum-finite? T:real)		=> maybe)
  (check (T:flonum-finite? T:non-real)		=> no)
  (check (T:flonum-finite? T:string)		=> no)

  (check (T:flonum-infinite? T:exact)			=> no)
  (check (T:flonum-infinite? T:inexact)			=> maybe)
  (check (T:flonum-infinite? T:fixnum)			=> no)
  (check (T:flonum-infinite? T:bignum)			=> no)
  (check (T:flonum-infinite? T:ratnum)			=> no)
  (check (T:flonum-infinite? T:flonum)			=> maybe)
  (check (T:flonum-infinite? T:flonum-integer)		=> no)
  (check (T:flonum-infinite? T:flonum-fractional)	=> no)
  (check (T:flonum-infinite? T:flonum-finite)		=> no)
  (check (T:flonum-infinite? T:flonum-infinite)		=> yes)
  (check (T:flonum-infinite? T:flonum-nan)		=> no)
  (check (T:flonum-infinite? T:cflonum)			=> no)
  (check (T:flonum-infinite? T:compnum)			=> no)
  (check (T:flonum-infinite? T:number)			=> maybe)
  (check (T:flonum-infinite? T:exact-integer)		=> no)
  (check (T:flonum-infinite? T:exact-real)		=> no)
  (check (T:flonum-infinite? T:real)			=> maybe)
  (check (T:flonum-infinite? T:non-real)			=> no)
  (check (T:flonum-infinite? T:string)			=> no)

  (check (T:flonum-nan? T:exact)		=> no)
  (check (T:flonum-nan? T:inexact)		=> maybe)
  (check (T:flonum-nan? T:fixnum)		=> no)
  (check (T:flonum-nan? T:bignum)		=> no)
  (check (T:flonum-nan? T:ratnum)		=> no)
  (check (T:flonum-nan? T:flonum)		=> maybe)
  (check (T:flonum-nan? T:flonum-integer)	=> no)
  (check (T:flonum-nan? T:flonum-fractional)	=> no)
  (check (T:flonum-nan? T:flonum-finite)	=> no)
  (check (T:flonum-nan? T:flonum-infinite)	=> no)
  (check (T:flonum-nan? T:flonum-nan)		=> yes)
  (check (T:flonum-nan? T:cflonum)		=> no)
  (check (T:flonum-nan? T:compnum)		=> no)
  (check (T:flonum-nan? T:number)		=> maybe)
  (check (T:flonum-nan? T:exact-integer)	=> no)
  (check (T:flonum-nan? T:exact-real)		=> no)
  (check (T:flonum-nan? T:real)			=> maybe)
  (check (T:flonum-nan? T:non-real)		=> no)
  (check (T:flonum-nan? T:string)		=> no)

  #t)


(parametrise ((check-test-name	'immediacy))

  (check (T:immediate? T:immediate)		=> yes)
  (check (T:immediate? T:nonimmediate)		=> no)

  (check (T:immediate? T:boolean)		=> yes)
  (check (T:immediate? T:char)			=> yes)
  (check (T:immediate? T:transcoder)		=> yes)
  (check (T:immediate? T:void)			=> yes)
  (check (T:immediate? T:procedure)		=> no)
  (check (T:immediate? T:string)		=> no)
  (check (T:immediate? T:symbol)		=> no)
  (check (T:immediate? T:vector)		=> no)
  (check (T:immediate? T:bytevector)		=> no)
  (check (T:immediate? T:port)			=> no)

  (check (T:immediate? T:null)			=> yes)
  (check (T:immediate? T:standalone-pair)	=> no)
  (check (T:immediate? T:non-empty-proper-list)	=> no)
  (check (T:immediate? T:pair)			=> no)
  (check (T:immediate? T:proper-list)		=> maybe)
  (check (T:immediate? T:improper-list)		=> maybe)

  (check (T:immediate? T:struct)		=> no)
  (check (T:immediate? T:record)		=> no)
  (check (T:immediate? T:struct-type-descriptor) => no)
  (check (T:immediate? T:record-type-descriptor) => no)
  (check (T:immediate? T:other-struct)		=> no)
  (check (T:immediate? T:other-object)		=> no)

  (check (T:immediate? T:fixnum)		=> yes)
  (check (T:immediate? T:bignum)		=> no)
  (check (T:immediate? T:ratnum)		=> no)
  (check (T:immediate? T:flonum)		=> no)
  (check (T:immediate? T:cflonum)		=> no)
  (check (T:immediate? T:compnum)		=> no)

  (check (T:immediate? T:negative)		=> maybe)
  (check (T:immediate? T:zero)			=> maybe)
  (check (T:immediate? T:positive)		=> maybe)

;;; --------------------------------------------------------------------

  (check (T:nonimmediate? T:immediate)			=> no)
  (check (T:nonimmediate? T:nonimmediate)		=> yes)

  (check (T:nonimmediate? T:boolean)			=> no)
  (check (T:nonimmediate? T:char)			=> no)
  (check (T:nonimmediate? T:transcoder)			=> no)
  (check (T:nonimmediate? T:void)			=> no)
  (check (T:nonimmediate? T:procedure)			=> yes)
  (check (T:nonimmediate? T:string)			=> yes)
  (check (T:nonimmediate? T:symbol)			=> yes)
  (check (T:nonimmediate? T:vector)			=> yes)
  (check (T:nonimmediate? T:bytevector)			=> yes)
  (check (T:nonimmediate? T:port)			=> yes)

  (check (T:nonimmediate? T:null)			=> no)
  (check (T:nonimmediate? T:standalone-pair)		=> yes)
  (check (T:nonimmediate? T:non-empty-proper-list)	=> yes)
  (check (T:nonimmediate? T:pair)			=> yes)
  (check (T:nonimmediate? T:proper-list)		=> maybe)
  (check (T:nonimmediate? T:improper-list)		=> maybe)

  (check (T:nonimmediate? T:struct)			=> yes)
  (check (T:nonimmediate? T:record)			=> yes)
  (check (T:nonimmediate? T:struct-type-descriptor)	=> yes)
  (check (T:nonimmediate? T:record-type-descriptor)	=> yes)
  (check (T:nonimmediate? T:other-struct)		=> yes)
  (check (T:nonimmediate? T:other-object)		=> yes)

  (check (T:nonimmediate? T:fixnum)			=> no)
  (check (T:nonimmediate? T:bignum)			=> yes)
  (check (T:nonimmediate? T:ratnum)			=> yes)
  (check (T:nonimmediate? T:flonum)			=> yes)
  (check (T:nonimmediate? T:cflonum)			=> yes)
  (check (T:nonimmediate? T:compnum)			=> yes)

  (check (T:nonimmediate? T:negative)			=> maybe)
  (check (T:nonimmediate? T:zero)			=> maybe)
  (check (T:nonimmediate? T:positive)			=> maybe)

  #t)


(parametrise ((check-test-name	'pairs))

  (check (T:null? T:null)					=> yes)
  (check (T:null? T:standalone-pair)				=> no)
  (check (T:null? T:non-empty-proper-list)			=> no)
  (check (T:null? T:pair)					=> no)
  (check (T:null? T:proper-list)				=> maybe)
  (check (T:null? T:improper-list)				=> maybe)
  (check (T:null? T:string)					=> no)

  (check (T:standalone-pair? T:null)				=> no)
  (check (T:standalone-pair? T:standalone-pair)			=> yes)
  (check (T:standalone-pair? T:non-empty-proper-list)		=> no)
  (check (T:standalone-pair? T:pair)				=> maybe)
  (check (T:standalone-pair? T:proper-list)			=> no)
  (check (T:standalone-pair? T:improper-list)			=> maybe)
  (check (T:standalone-pair? T:string)				=> no)

  (check (T:non-empty-proper-list? T:null)			=> no)
  (check (T:non-empty-proper-list? T:standalone-pair)		=> no)
  (check (T:non-empty-proper-list? T:non-empty-proper-list)	=> yes)
  (check (T:non-empty-proper-list? T:pair)			=> maybe)
  (check (T:non-empty-proper-list? T:proper-list)		=> maybe)
  (check (T:non-empty-proper-list? T:improper-list)		=> maybe)
  (check (T:non-empty-proper-list? T:string)			=> no)

  (check (T:pair? T:null)					=> no)
  (check (T:pair? T:standalone-pair)				=> yes)
  (check (T:pair? T:non-empty-proper-list)			=> yes)
  (check (T:pair? T:pair)					=> yes)
  (check (T:pair? T:proper-list)				=> maybe)
  (check (T:pair? T:improper-list)				=> maybe)
  (check (T:pair? T:string)					=> no)

  (check (T:proper-list? T:null)				=> yes)
  (check (T:proper-list? T:standalone-pair)			=> no)
  (check (T:proper-list? T:non-empty-proper-list)		=> yes)
  (check (T:proper-list? T:pair)				=> maybe)
  (check (T:proper-list? T:proper-list)				=> yes)
  (check (T:proper-list? T:improper-list)			=> maybe)
  (check (T:proper-list? T:string)				=> no)

  (check (T:improper-list? T:null)				=> yes)
  (check (T:improper-list? T:standalone-pair)			=> yes)
  (check (T:improper-list? T:non-empty-proper-list)		=> yes)
  (check (T:improper-list? T:pair)				=> yes)
  (check (T:improper-list? T:proper-list)			=> yes)
  (check (T:improper-list? T:improper-list)			=> yes)
  (check (T:improper-list? T:string)				=> yes)

  #t)


(parametrise ((check-test-name	'structs))

  (check (T:struct? T:struct)					=> yes)
  (check (T:struct? T:record)					=> yes)
  (check (T:struct? T:struct-type-descriptor)			=> yes)
  (check (T:struct? T:record-type-descriptor)			=> yes)
  (check (T:struct? T:string)					=> no)

  (check (T:struct-type-descriptor? T:struct)			=> maybe)
  (check (T:struct-type-descriptor? T:record)			=> no)
  (check (T:struct-type-descriptor? T:struct-type-descriptor)	=> yes)
  (check (T:struct-type-descriptor? T:record-type-descriptor)	=> no)
  (check (T:struct-type-descriptor? T:string)			=> no)

  (check (T:record-type-descriptor? T:struct)			=> maybe)
  (check (T:record-type-descriptor? T:record)			=> no)
  (check (T:record-type-descriptor? T:struct-type-descriptor)	=> no)
  (check (T:record-type-descriptor? T:record-type-descriptor)	=> yes)
  (check (T:record-type-descriptor? T:string)			=> no)

  (check (T:record? T:struct)					=> maybe)
  (check (T:record? T:record)					=> yes)
  (check (T:record? T:struct-type-descriptor)			=> no)
  (check (T:record? T:record-type-descriptor)			=> no)
  (check (T:record? T:string)					=> no)

  (check (T:other-struct? T:struct)				=> maybe)
  (check (T:other-struct? T:record)				=> no)
  (check (T:other-struct? T:struct-type-descriptor)		=> no)
  (check (T:other-struct? T:record-type-descriptor)		=> no)
  (check (T:other-struct? T:string)				=> no)

  #t)


(parametrise ((check-test-name	'ports))

  (check (T:port? T:port)					=> yes)
  (check (T:port? T:string)					=> no)

  (check (T:textual-input-port? T:port)				=> maybe)
  (check (T:textual-input-port? T:input-port)			=> maybe)
  (check (T:textual-input-port? T:output-port)			=> no)
  #;(check (T:textual-input-port? T:input/output-port)		=> maybe)
  (check (T:textual-input-port? T:binary-port)			=> no)
  (check (T:textual-input-port? T:binary-input-port)		=> no)
  (check (T:textual-input-port? T:binary-output-port)		=> no)
  (check (T:textual-input-port? T:binary-input/output-port)	=> no)
  (check (T:textual-input-port? T:textual-port)			=> maybe)
  (check (T:textual-input-port? T:textual-input-port)		=> yes)
  (check (T:textual-input-port? T:textual-output-port)		=> no)
  #;(check (T:textual-input-port? T:textual-input/output-port)	=> yes)

  (check (T:textual-output-port? T:port)			=> maybe)
  (check (T:textual-output-port? T:input-port)			=> no)
  (check (T:textual-output-port? T:output-port)			=> maybe)
  #;(check (T:textual-output-port? T:input/output-port)		=> maybe)
  (check (T:textual-output-port? T:binary-port)			=> no)
  (check (T:textual-output-port? T:binary-input-port)		=> no)
  (check (T:textual-output-port? T:binary-output-port)		=> no)
  (check (T:textual-output-port? T:binary-input/output-port)	=> no)
  (check (T:textual-output-port? T:textual-port)		=> maybe)
  (check (T:textual-output-port? T:textual-input-port)		=> no)
  (check (T:textual-output-port? T:textual-output-port)		=> yes)
  #;(check (T:textual-output-port? T:textual-input/output-port)	=> yes)

  (check (T:textual-input/output-port? T:port)			=> maybe)
  (check (T:textual-input/output-port? T:input-port)		=> no)
  (check (T:textual-input/output-port? T:output-port)		=> no)
  (check (T:textual-input/output-port? T:input/output-port)	=> maybe)
  (check (T:textual-input/output-port? T:binary-port)		=> no)
  (check (T:textual-input/output-port? T:binary-input-port)	=> no)
  (check (T:textual-input/output-port? T:binary-output-port)	=> no)
  (check (T:textual-input/output-port? T:binary-input/output-port) => no)
  (check (T:textual-input/output-port? T:textual-port)		=> maybe)
  (check (T:textual-input/output-port? T:textual-input-port)	=> no)
  (check (T:textual-input/output-port? T:textual-output-port)	=> no)
  (check (T:textual-input/output-port? T:textual-input/output-port) => yes)

  (check (T:binary-input-port? T:port)				=> maybe)
  (check (T:binary-input-port? T:input-port)			=> maybe)
  (check (T:binary-input-port? T:output-port)			=> no)
  #;(check (T:binary-input-port? T:input/output-port)		=> maybe)
  (check (T:binary-input-port? T:binary-port)			=> maybe)
  (check (T:binary-input-port? T:binary-input-port)		=> yes)
  (check (T:binary-input-port? T:binary-output-port)		=> no)
  #;(check (T:binary-input-port? T:binary-input/output-port)	=> yes)
  (check (T:binary-input-port? T:textual-port)			=> no)
  (check (T:binary-input-port? T:textual-input-port)		=> no)
  (check (T:binary-input-port? T:textual-output-port)		=> no)
  #;(check (T:binary-input-port? T:textual-input/output-port)	=> no)

  (check (T:binary-output-port? T:port)				=> maybe)
  (check (T:binary-output-port? T:input-port)			=> no)
  (check (T:binary-output-port? T:output-port)			=> maybe)
  #;(check (T:binary-output-port? T:input/output-port)		=> maybe)
  (check (T:binary-output-port? T:binary-port)			=> maybe)
  (check (T:binary-output-port? T:binary-input-port)		=> no)
  (check (T:binary-output-port? T:binary-output-port)		=> yes)
  #;(check (T:binary-output-port? T:binary-input/output-port)	=> yes)
  (check (T:binary-output-port? T:textual-port)			=> no)
  (check (T:binary-output-port? T:textual-input-port)		=> no)
  (check (T:binary-output-port? T:textual-output-port)		=> no)
  #;(check (T:binary-output-port? T:textual-input/output-port)	=> no)

  (check (T:binary-input/output-port? T:port)			=> maybe)
  (check (T:binary-input/output-port? T:input-port)		=> no)
  (check (T:binary-input/output-port? T:output-port)		=> no)
  (check (T:binary-input/output-port? T:input/output-port)	=> maybe)
  (check (T:binary-input/output-port? T:binary-port)		=> maybe)
  (check (T:binary-input/output-port? T:binary-input-port)	=> no)
  (check (T:binary-input/output-port? T:binary-output-port)	=> no)
  (check (T:binary-input/output-port? T:binary-input/output-port) => yes)
  (check (T:binary-input/output-port? T:textual-port)		=> no)
  (check (T:binary-input/output-port? T:textual-input-port)	=> no)
  (check (T:binary-input/output-port? T:textual-output-port)	=> no)
  (check (T:binary-input/output-port? T:textual-input/output-port) => no)

  (check (T:textual-port? T:textual-port)	=> yes)
  (check (T:textual-port? T:binary-port)	=> no)
  (check (T:textual-port? T:input-port)		=> maybe)
  (check (T:textual-port? T:output-port)	=> maybe)
  (check (T:textual-port? T:input/output-port)	=> maybe)
  (check (T:textual-port? T:port)		=> maybe)

  (check (T:binary-port? T:textual-port)	=> no)
  (check (T:binary-port? T:binary-port)		=> yes)
  (check (T:binary-port? T:input-port)		=> maybe)
  (check (T:binary-port? T:output-port)		=> maybe)
  (check (T:binary-port? T:input/output-port)	=> maybe)
  (check (T:binary-port? T:port)		=> maybe)

  (check (T:input-port? T:textual-port)		=> maybe)
  (check (T:input-port? T:binary-port)		=> maybe)
  (check (T:input-port? T:input-port)		=> yes)
  (check (T:input-port? T:output-port)		=> no)
  #;(check (T:input-port? T:input/output-port)	=> yes)
  (check (T:input-port? T:port)			=> maybe)

  (check (T:output-port? T:textual-port)	=> maybe)
  (check (T:output-port? T:binary-port)		=> maybe)
  (check (T:output-port? T:input-port)		=> no)
  (check (T:output-port? T:output-port)		=> yes)
  #;(check (T:output-port? T:input/output-port)	=> yes)
  (check (T:output-port? T:port)		=> maybe)

  #t)


(parametrise ((check-test-name	'misc))

;;; multitype tests

  (check (T:fixnum? (core-type-tag-ior T:fixnum T:flonum))	=> maybe)
  (check (T:flonum? (core-type-tag-ior T:fixnum T:flonum))	=> maybe)
  (check (T:string? (core-type-tag-ior T:fixnum T:flonum))	=> no)

  (check (T:fixnum? (core-type-tag-ior T:fixnum T:string))	=> maybe)
  (check (T:string? (core-type-tag-ior T:fixnum T:string))	=> maybe)
  (check (T:pair?   (core-type-tag-ior T:fixnum T:string))	=> no)

;;; --------------------------------------------------------------------

  (check (core-type-tag-is-a? T:fixnum T:number)		=> yes)
  (check (core-type-tag-is-a? T:number T:fixnum)		=> maybe)
  (check (core-type-tag-is-a? T:string T:number)		=> no)

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
