;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <string> type
;;;Date: Mon Oct 26, 2015
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
(program (test-types-objects-strings)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <string> objects\n")


;;;; helpers

(define-constant ENVIRONMENT
  (environment '(vicare)))

(define-syntax-rule (%eval ?sexp)
  (eval (quasiquote ?sexp)
	ENVIRONMENT
	(expander-options typed-language)
	(compiler-options)))


(parametrise ((check-test-name		'predicate))

  (check-for-true	(is-a? "ciao" <string>))
  (check-for-false	(is-a? 123 <string>))

  (check-for-true	(let (({O <string>} "ciao"))
			  (is-a? O <string>)))

  (check-for-true	(let (({O <top>} "ciao"))
			  (is-a? O <string>)))

  (check-for-false	(let (({O <top>} 123))
			  (is-a? O <string>)))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? "ciao" <nestring>))
  (check-for-false	(is-a? 123 <nestring>))

  (check-for-true	(let (({O <nestring>} "ciao"))
			  (is-a? O <nestring>)))

  (check-for-true	(let (({O <top>} "ciao"))
			  (is-a? O <nestring>)))

  (check-for-false	(let (({O <top>} 123))
			  (is-a? O <nestring>)))

  (void))


(parametrise ((check-test-name		'constructor))

  (check
      (new <string> #\a #\b #\c)
    => "abc")

  ;;There are no arguments here to STRING: the return value is an empty string.
  (check
      (.syntax-object (type-of (new <string>)))
    (=> expander::syntax=?)
    (list #'<empty-string>))

  ;;There is one argument here to STRING: the return value is a non-empty string.
  (check
      (.syntax-object (type-of (new <string> (read))))
    (=> expander::syntax=?)
    (list #'<nestring>))

;;; --------------------------------------------------------------------

  (check
      (new <nestring> #\a #\b #\c)
    => "abc")

  (check
      (expander::type-signature.syntax-object (type-of (new <nestring> (read))))
    (=> expander::syntax=?)
    (list #'<nestring>))

  (void))


(parametrise ((check-test-name		'methods))

  (check
      (let (({O <string>} "ciao"))
	(.empty? O))
    => #f)

  (check
      (let (({O <string>} ""))
	(.empty? O))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <string>} "ciao")
	    ({P <string>} " mamma"))
	(.append O P))
    => "ciao mamma")

  (check
      (let (({O <nestring>} "ciao")
	    ({P <nestring>} " mamma"))
	(.append O P))
    => "ciao mamma")

;;; --------------------------------------------------------------------

  (check
      (with-result
	(let (({O <string>} "ciao"))
	  (.for-each O add-result)))
    => '(#!void (#\c #\i #\a #\o)))

  (check
      (with-result
	(let (({O <nestring>} "ciao"))
	  (.for-each O add-result)))
    => '(#!void (#\c #\i #\a #\o)))

;;; --------------------------------------------------------------------

  (check
      (let (({O <string>} "ciao"))
	(fixnum? (.hash O)))
    => #t)

  (void))


(parametrise ((check-test-name		'late-binding))

  (check
      (let (({O <string>} "ciao"))
	(method-call-late-binding 'empty? O))
    => #f)

  (check
      (let (({O <string>} "ciao"))
	(fixnum? (method-call-late-binding 'hash O)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <nestring>} "ciao"))
	(method-call-late-binding 'empty? O))
    => #f)

  (check
      (let (({O <nestring>} "ciao"))
	(fixnum? (method-call-late-binding 'hash O)))
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
