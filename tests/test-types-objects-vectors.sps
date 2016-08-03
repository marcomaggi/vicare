;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <vector> type
;;;Date: Thu Oct 29, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-types-objects-vectors)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
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
(check-display "*** testing Vicare typed language: tests for <vector> objects\n")


;;;; helpers

(define-constant ENVIRONMENT
  (environment '(vicare)))

(define-syntax-rule (%eval ?sexp)
  (eval (quasiquote ?sexp)
	ENVIRONMENT
	(expander-options typed-language)
	(compiler-options)))


(parametrise ((check-test-name	'type-of))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expression ?expected-tags)
       (check
	   ;;The  return value  of  a  TYPE-OF use  expansion  and  evaluation is  an
	   ;;instance of "<type-signature>".
	   (.syntax-object (type-of ?expression))
	 (=> expander::syntax=?)
	 ;;When the expression is a CONDITION application: the expected tags value is
	 ;;a list with a single item.
	 ?expected-tags))
      ))

;;; --------------------------------------------------------------------

  (doit (vector 1)
	#'((vector <positive-fixnum>)))

  (doit (vector 1 2 3)
	#'((vector <positive-fixnum> <positive-fixnum> <positive-fixnum>)))

  (doit (vector 1 "ciao" 'ciao)
	#'((vector <positive-fixnum> <nestring> (enumeration ciao))))

  (doit (vector 1 '#(2 3))
	#'((vector <positive-fixnum> (vector <positive-fixnum> <positive-fixnum>))))

  (doit (vector 1 '())
	#'((vector <positive-fixnum> <null>)))

  (void))


(parametrise ((check-test-name	'type-tags))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation ?expected-tags)
       ;;Here we test only type signature describing a single value.
       (check
	   (type-annotation-syntax ?type-annotation)
	 (=> expander::syntax=?)
	 #'?expected-tags))
      ))

;;; --------------------------------------------------------------------

  (doit <vector>
	<vector>)

  (doit (vector)
  	<empty-vector>)

  (doit (vector <fixnum>)
  	(vector <fixnum>))

  (doit (vector <fixnum> <flonum> <string>)
  	(vector <fixnum> <flonum> <string>))

  (void))


(parametrise ((check-test-name		'predicate))

  (check-for-true	(is-a? '#(1 2 3 4) <vector>))
  (check-for-false	(is-a? 123 <vector>))

  (check-for-true	(let (({O <vector>} '#(1 2 3 4)))
			  (is-a? O <vector>)))

  (check-for-true	(let (({O <top>} '#(1 2 3 4)))
			  (is-a? O <vector>)))

  (check-for-false	(let (({O <top>} 123))
			  (is-a? O <vector>)))

  (void))


(parametrise ((check-test-name		'constructor))

  (check
      (new <vector> 1 2 3)
    => '#(1 2 3))

  (check
      (.syntax-object (type-of (new <vector> (read))))
    (=> expander::syntax=?)
    #'((vector <top>)))

  (void))


(parametrise ((check-test-name		'methods))

  (check
      (let (({O <vector>} '#(1 2 3 4)))
	(.empty? O))
    => #f)

  (check
      (let (({O <vector>} '#()))
	(.empty? O))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <vector>} '#(1 2 3 4))
	    ({P <vector>} '#(5 6 7 8)))
	(.append O P))
    => '#(1 2 3 4 5 6 7 8))

  (check
      (let (({O <vector>} '#(1 2 3 4)))
	(.resize O 2))
    => '#(1 2))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(let (({O <vector>} '#(1 2 3 4)))
	  (.for-each O add-result)))
    => '(#!void (1 2 3 4)))

;;; --------------------------------------------------------------------

  (check
      (let (({O <vector>} '#(1 2 3 4)))
	(fixnum? (hash O)))
    => #t)

  (void))


(parametrise ((check-test-name		'late-binding))

  (check
      (let (({O <vector>} '#(1 2 3 4)))
	(method-call-late-binding 'empty? #f O))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (({O <vector>} '#(1 2 3 4)))
	(fixnum? (method-call-late-binding 'hash #f O)))
    => #t)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
