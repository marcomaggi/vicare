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


;;;; syntaxes

(define-syntax (define-type-annotation stx)
  ;;Hard-coded built-in type annotations are meant to be equivalent to:
  ;;
  ;;   (define-type ?type-name ?type-annotation)
  ;;
  (syntax-case stx ()
    ((?kwd ?type-name ?type-annotation)
     #'(set-cons! VICARE-CORE-BUILT-IN-TYPE-ANNOTATIONS-SYNTACTIC-BINDING-DESCRIPTORS
		  (quote (?type-name
			  ($core-type-annotation
			   . (?type-name ?type-annotation))))))
    ))


;;;; numerics

(define-type-annotation <non-negative-fixnum>
  (or <zero-fixnum> <positive-fixnum>))

(define-type-annotation <non-positive-fixnum>
  (or <zero-fixnum> <negative-fixnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <non-negative-exact-integer>
  (or <non-negative-fixnum> <positive-bignum>))

(define-type-annotation <positive-exact-integer>
  (or <positive-fixnum> <positive-bignum>))

(define-type-annotation <negative-exact-integer>
  (or <negative-fixnum> <negative-bignum>))

;;; --------------------------------------------------------------------

(define-type-annotation <exact-rational>
  (or <exact-integer> <ratnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <non-negative-flonum>
  (or <positive-flonum> <zero-flonum>))

(define-type-annotation <non-positive-flonum>
  (or <negative-flonum> <zero-flonum>))

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
  (lambda (<bottom>) => (<boolean>)))

(define-type-annotation <type-destructor>
  (lambda (<bottom>) => <list>))

(define-type-annotation <type-printer>
  (lambda (<bottom> <textual-output-port> <procedure>) => <list>))

(define-type-annotation <type-method-retriever>
  (lambda (<symbol>) => ((or <false> <procedure>))))

;;; --------------------------------------------------------------------

(define-type-annotation <equality-predicate>
  (lambda (<bottom> <bottom>) => (<boolean>)))

(define-type-annotation <comparison-procedure>
  (lambda (<bottom> <bottom>) => (<fixnum>)))

(define-type-annotation <hash-function>
  (lambda (<bottom>) => (<non-negative-fixnum>)))


;;;; input/output ports

(define-type-annotation <textual-port>
  (or <textual-input-port> <textual-output-port> <textual-input/output-port>))

(define-type-annotation <binary-port>
  (or <binary-input-port> <binary-output-port> <binary-input/output-port>))


;;;; miscellaneous

(define-type-annotation <&who-value>
  (or <false> <symbol> <string>))

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
      <vector-type-descr> <vector-of-type-descr>
      <enumeration-type-descr> <closure-type-descr> <ancestor-of-type-descr>
      <union-type-descr> <intersection-type-descr> <complement-type-descr>))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
