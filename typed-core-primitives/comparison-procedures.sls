;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for comparison procedures core primitives
;;Date: Tue Apr 26, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives comparison-procedures)
  (export typed-core-primitives.comparison-procedures)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.comparison-procedures)

(define-syntax declare-comparison-procedure
  (syntax-rules ()
    ((_ ?who ?type)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((?type ?type)		=> (<fixnum>)))))
    ))



;;;; comparison procedures

(section

(declare-core-primitive make-comparison-procedure
    (safe)
  (signatures
   ((<type-predicate>
     (lambda (<top> <top>) => (<boolean>))  ;equal-to predicate
     (lambda (<top> <top>) => (<boolean>))) ;less-than predicate
    => (<comparison-procedure>))))

;; numeric procedures
(declare-comparison-procedure compar-fixnum		<fixnum>)
(declare-comparison-procedure compar-bignum		<bignum>)
(declare-comparison-procedure compar-exact-integer	<exact-integer>)
(declare-comparison-procedure compar-ratnum		<ratnum>)
(declare-comparison-procedure compar-exact-real		(or <exact-integer> <ratnum>))
(declare-comparison-procedure compar-flonum		<flonum>)
(declare-comparison-procedure compar-real		<real>)

;; textual procedures
(declare-comparison-procedure compar-char		<char>)
(declare-comparison-procedure compar-string		<string>)
(declare-comparison-procedure compar-string-ci		<string>)
(declare-comparison-procedure compar-symbol		<symbol>)

;; misc procedures
(declare-comparison-procedure compar-boolean		<boolean>)
(declare-comparison-procedure compar-transcoder		<transcoder>)
(declare-comparison-procedure compar-pointer		<pointer>)

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
