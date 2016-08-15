;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <hashtable-type-spec> type
;;;Date: Mon Apr  4, 2016
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
(program (test-types-hashtable-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (prefix (vicare system $hashtables)
	    sys::)
    (prefix (vicare system type-descriptors)
	    td::)
    (only (vicare expander)
	  type-annotation=?
	  type-annotation-super-and-sub?
	  type-annotation-common-ancestor
	  type-annotation-ancestors
	  type-annotation-syntax
	  type-annotation-matching
	  type-signature-super-and-sub?
	  type-signature-common-ancestor
	  type-signature-matching
	  type-signature-union)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <hashtable-type-spec> objects\n")


;;;; helpers

(define-constant EVAL-ENVIRONMENT
  (environment '(vicare)))

(define (%eval sexp)
  (eval sexp EVAL-ENVIRONMENT
	(expander-options typed-language)
	(compiler-options)))

(define (%print-message print? E)
  (when print?
    (fprintf (current-error-port) "~s\n" (condition-message E))))


(parametrise ((check-test-name	'super-and-sub))

  (check-for-true	(type-annotation-super-and-sub? (hashtable <symbol> <number>)
							(hashtable <symbol> <number>)))

  (check-for-true	(type-annotation-super-and-sub? (hashtable <real>   <string>)
							(hashtable <fixnum> <string>)))

  (check-for-true	(type-annotation-super-and-sub? (hashtable <string>   <real>)
							(hashtable <string> <fixnum>)))

  (check-for-false	(type-annotation-super-and-sub? (hashtable <real>    <string>)
							(hashtable <boolean> <string>)))

  (check-for-false	(type-annotation-super-and-sub? (hashtable <string>    <real>)
							(hashtable <string> <boolean>)))

  (void))


(parametrise ((check-test-name	'typed-constructor))

  (check
      (let ((T (new (hashtable <string> <fixnum>))))
	(hashtable-set! T "uno" 1)
	(hashtable-ref  T "uno"))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ((T (new (hashtable <string> <fixnum>))))
	(td::hashtable-type-descr? (sys::$hashtable-type-descriptor T)))
    => #t)

  (check
      (let ((T (new (hashtable <string> <fixnum>))))
	(is-a? (sys::$hashtable-type-descriptor T) td::<hashtable-type-descr>))
    => #t)

  (check
      (let ((T (new (hashtable <string> <fixnum>))))
	(.key-des (sys::$hashtable-type-descriptor T)))
    => td::<string>-ctd)

  (check
      (let ((T (new (hashtable <string> <fixnum>))))
	(.val-des (sys::$hashtable-type-descriptor T)))
    => td::<fixnum>-ctd)

;;; --------------------------------------------------------------------
;;; errors

  ;;Key object-type without equality predicate.
  ;;
  (check
      (try
	  (%eval '(new (hashtable (pair <fixnum> <fixnum>) <fixnum>)))
	(catch E
	  ((&syntax)
	   (%print-message #f E)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(pair <fixnum> <fixnum>))

  ;;More operands.
  ;;
  (check
      (try
	  (%eval '(new (hashtable <string> <fixnum>) 1 2 3))
	(catch E
	  ((&syntax)
	   (%print-message #f E)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(1 2 3))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
