;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: definition of hard-coded built-in type annotations
;;;Date: Mon Mar 21, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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

#!vicare
#!vicare
(program (makefile.built-in-type-annotations)
  (import (vicare)
    (prefix (vicare expander) xp::))


;;;; syntaxes

(define-syntax (define-type-annotation stx)
  ;;Hard-coded built-in type annotations are meant to be equivalent to:
  ;;
  ;;   (define-type ?type-name ?type-annotation)
  ;;
  (syntax-case stx ()
    ((?kwd ?type-name ?type-annotation)
     #'(begin
	 (pretty-print '(set-cons! VICARE-CORE-BUILT-IN-TYPE-ANNOTATIONS-SYNTACTIC-BINDING-DESCRIPTORS
				   (quote (?type-name
					   ($core-type-annotation
					    . (?type-name ?type-annotation)))))
		       (stdout))
	 (flush-output-port (stdout))))
    ))


;;;; numerics

(define-type-annotation <non-negative-fixnum>
  (or <zero-fixnum> <positive-fixnum>))

(define-type-annotation <non-positive-fixnum>
  (or <zero-fixnum> <negative-fixnum>))

(define-type-annotation <non-zero-fixnum>
  (or <negative-fixnum> <positive-fixnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <non-positive-byte>
  (or <zero-byte> <negative-byte>))

(define-type-annotation <non-negative-byte>
  (or <zero-byte> <positive-byte>))

;;; --------------------------------------------------------------------

(define-type-annotation <non-zero-exact-integer>
  (or <non-zero-fixnum> <bignum>))

(define-type-annotation <positive-exact-integer>
  (or <positive-fixnum> <positive-bignum>))

(define-type-annotation <negative-exact-integer>
  (or <negative-fixnum> <negative-bignum>))

(define-type-annotation <non-positive-exact-integer>
  (or <non-positive-fixnum> <negative-bignum>))

(define-type-annotation <non-negative-exact-integer>
  (or <non-negative-fixnum> <positive-bignum>))

;;; --------------------------------------------------------------------

(define-type-annotation <exact-rational>
  (or <exact-integer> <ratnum>))

(define-type-annotation <non-negative-exact-rational>
  (or <non-negative-exact-integer> <positive-ratnum>))

(define-type-annotation <non-zero-exact-rational>
  (or <non-zero-exact-integer> <ratnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <non-negative-flonum>
  (or <positive-flonum> <zero-flonum>))

(define-type-annotation <non-positive-flonum>
  (or <negative-flonum> <zero-flonum>))

(define-type-annotation <non-zero-flonum>
  (or <positive-flonum> <negative-flonum>))

;;; --------------------------------------------------------------------

(define-type-annotation <zero-real>
  (or <zero-fixnum> <zero-flonum>))

(define-type-annotation <non-zero-real>
  (or <non-zero-fixnum> <bignum> <non-zero-flonum>))

(define-type-annotation <non-negative-real>
  (or <non-negative-fixnum> <positive-bignum> <positive-flonum>))

(define-type-annotation <positive-real>
  (or <positive-fixnum> <positive-bignum> <positive-flonum>))

(define-type-annotation <negative-real>
  (or <negative-fixnum> <negative-bignum> <negative-flonum>))

;;; --------------------------------------------------------------------

(define-type-annotation <non-zero-compnum>
  (or <exact-compnum> <non-zero-inexact-compnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <exact>
  (or <exact-rational> <exact-compnum>))

(define-type-annotation <inexact>
  (or <flonum> <cflonum> <inexact-compnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <positive>
  (or <positive-fixnum> <positive-bignum> <positive-ratnum> <positive-flonum>))

(define-type-annotation <negative>
  (or <negative-fixnum> <negative-bignum> <negative-ratnum> <negative-flonum>))

(define-type-annotation <non-negative>
  (or <non-negative-fixnum> <positive-bignum> <positive-ratnum> <non-negative-flonum>))

(define-type-annotation <non-positive>
  (or <non-positive-fixnum> <negative-bignum> <negative-ratnum> <non-positive-flonum>))

(define-type-annotation <zero>
  (or <zero-fixnum> <zero-flonum> <zero-compnum> <zero-cflonum>))


;;;; library handling

(define-type-annotation <library-name>
  <top>)

(define-type-annotation <library-reference>
  <top>)

(define-type-annotation <library-descriptor>
  <top>)


;;;; procedures

(define-type-annotation <thunk>
  (lambda () => <list>))

;;; --------------------------------------------------------------------

(define-type-annotation <type-predicate>
  (lambda (_) => (<boolean>)))

(define-type-annotation <type-destructor>
  (lambda (_) => <list>))

(define-type-annotation <type-printer>
  (lambda (_ <textual-output-port> <procedure>) => <list>))

(define-type-annotation <type-method-retriever>
  (lambda (<symbol>) => ((or <false> <procedure>))))

;;; --------------------------------------------------------------------

(define-type-annotation <equality-predicate>
  (lambda (_ _) => (<boolean>)))

(define-type-annotation <comparison-procedure>
  (lambda (_ _) => (<fixnum>)))

(define-type-annotation <hash-function>
  (lambda (_) => (<non-negative-fixnum>)))


;;;; miscellaneous

(define-type-annotation <reader-input-port-mode>
  (enumeration vicare r6rs))

(define-type-annotation <output-port-buffer-mode>
  (enumeration none line block))

(define-type-annotation <&who-value>
  (or <false> <symbol> <string>))

(define-type-annotation <enum-set-indexer>
  (lambda (<symbol>) => ((or <false> <non-negative-fixnum>))))

(define-type-annotation <enum-set-constructor>
  (lambda ((list-of <symbol>)) => (<enum-set>)))

(define-type-annotation <file-descriptor>
  <non-negative-fixnum>)

;; (define-type <syntax-object>
;;   (or <syntactic-identifier>
;;       <boolean>
;;       <char>
;;       <number>
;;       <string>
;;       <bytevector>
;;       <vector-of-syntax-objects>
;;       <list-of-syntax-objects>
;;       ))

(define-type-annotation <type-descriptor>
  (or <core-type-descriptor> <struct-type-descriptor>
      <record-type-descriptor> <record-constructor-descriptor>
      <pair-type-descr> <pair-of-type-descr>
      <list-type-descr> <list-of-type-descr>
      <vector-type-descr> <vector-of-type-descr> <nevector-of-type-descr>
      <enumeration-type-descr> <closure-type-descr> <ancestor-of-type-descr>
      <hashtable-type-descr> <alist-type-descr>
      <union-type-descr> <intersection-type-descr> <complement-type-descr>
      <interface-type-descr>))


;;;; done

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
