;;;
;;;Part of: Vicare/Scheme
;;;Contents: test for SILex calculator
;;;Date: Sun Jul 19, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (libtest silex-test)
  (export test-calc)
  (import (except (vicare) =)
    (only (rnrs) =)
    (prefix (vicare parser-tools silex lexer) lex.)
    (vicare checks))


(define (test-calc table)

  (define (tokenize string)
    (let* ((IS		(lex.make-IS (lex.string: string) (lex.counters: 'line)))
	   (lexer	(lex.make-lexer table IS)))
      (do ((token (lexer) (lexer))
	   (out   '()))
	  ((eof-object? token)
	   (reverse out))
	(set! out (cons token out)))))

;;; integers

  (check (tokenize "1")		=> '(1))
  (check (tokenize "-1")	=> '(- 1))
  (check (tokenize "+1")	=> '(+ 1))

;;; reals

  (check (tokenize "1.1")	=> '(1.1))
  (check (tokenize "-1.1")	=> '(- 1.1))
  (check (tokenize "+1.1")	=> '(+ 1.1))
  (check (tokenize "1.1e10")	=> '(1.1e10))
  (check (tokenize "1.1E10")	=> '(1.1e10))
  (check (tokenize "1.1e-10")	=> '(1.1e-10))
  (check (tokenize "1.1E-10")	=> '(1.1e-10))
  (check (tokenize "1e10")	=> '(1e10))
  (check (tokenize "1E10")	=> '(1e10))
  (check (tokenize "1e-10")	=> '(1e-10))
  (check (tokenize "1E-10")	=> '(1e-10))

  (check (tokenize ".0")	=> '(0.0))
  (check (tokenize "-.0")	=> '(- 0.0))
  (check (tokenize "0.")	=> '(0.0))

;;; complexes

  (check (tokenize "1i")	=> '(+1i))
  (check (tokenize "-1i")	=> '(- +1i))
  (check (tokenize "+1.1i")	=> '(+ +1.1i))
  (check (tokenize "-1.1i")	=> '(- +1.1i))
  (check (tokenize "+.1i")	=> '(+ +0.1i))
  (check (tokenize "-.1i")	=> '(- +0.1i))

;;; nan and infinity

  (check (tokenize "+nan.0")	=> '(+nan.0))
  (check (tokenize "-nan.0")	=> '(+nan.0))
  (check (tokenize "+inf.0")	=> '(+inf.0))
  (check (tokenize "-inf.0")	=> '(-inf.0))

;;; arithmetic operators

  (check (tokenize "1+2")	=> '(1 + 2))
  (check (tokenize "1+2+3")	=> '(1 + 2 + 3))
  (check (tokenize "1+2-3")	=> '(1 + 2 - 3))
  (check (tokenize "1+(2+3)")	=> '(1 + #\( 2 + 3 #\)))
  (check (tokenize "1+(2-3)")	=> '(1 + #\( 2 - 3 #\)))

  (check (tokenize "1*1")	=> '(1 * 1))
  (check (tokenize "1*2*3")	=> '(1 * 2 * 3))
  (check (tokenize "1*2/3")	=> '(1 * 2 / 3))
  (check (tokenize "1*(2*3)")	=> '(1 * #\( 2 * 3 #\)))
  (check (tokenize "1*(2/3)")	=> '(1 * #\( 2 / 3 #\)))

  (check (tokenize "1//3")	=> '(1 div 3))
  (check (tokenize "1%3")	=> '(1 mod 3))
  (check (tokenize "1^3")	=> '(1 expt 3))

;;; functions

  (check (tokenize "sin(1.1)")		=> '(sin #\( 1.1 #\)))
  (check (tokenize "cos(sin(1.1))")	=> '(cos #\( sin #\( 1.1 #\) #\)))
  (check (tokenize "cos(sin(1.1)+4)")	=> '(cos #\( sin #\( 1.1 #\) + 4 #\)))
  (check (tokenize "fun(1.1, 2)")	=> '(fun #\( 1.1 cons 2 #\)))

  (check (tokenize "fun(1, 2, 3, 4)")
    => '(fun #\( 1 cons 2 cons 3 cons 4 #\)))

  (check (tokenize "fun(1+a, sin(2), 3, 4)")
    => '(fun #\( 1 + a cons sin #\( 2 #\) cons 3 cons 4 #\)))

  (check
      (tokenize "fun(1+a, sin(2), 3*g, 4+a+f+r+t)")
    => '(fun #\( 1 + a cons sin #\( 2 #\) cons 3 * g cons 4 + a + f + r + t #\)))

  (check
      (tokenize "fun(1+a, sin(2), fun(1, fun(5, 5), fun(1, 2)), 4)")
    => '(fun #\( 1 + a cons sin #\( 2 #\) cons fun #\( 1 cons
	     fun #\( 5 cons 5 #\) cons fun #\( 1 cons 2 #\) #\) cons 4 #\)))

  (check
      (tokenize "1+23e-45+678.9e12*(4113+23i) / sin 545 + tan(1, 2)")
    => '(1 + 23e-45 + 678.9e12 * #\( 4113 + +23i #\)
	   / sin 545 + tan #\( 1 cons 2 #\)))

  (check (tokenize "1 < 3")	=> '(1 < 3))
  (check (tokenize "1 > 3")	=> '(1 > 3))
  (check (tokenize "1 <= 3")	=> '(1 <= 3))
  (check (tokenize "1 >= 3")	=> '(1 >= 3))
  (check (tokenize "1 = 3")	=> '(1 = 3))

;;; variables

  (check (tokenize "a * 1.1")		=> '(a * 1.1))
  (check (tokenize "(a * b) / c")	=> '(#\( a * b #\) / c))
  (check (tokenize "a * (b / c)")	=> '(a * #\( b / c #\)))

  (check (tokenize "cos(a) * (tan(b) / c)")
    => '(cos #\( a #\) * #\( tan #\( b #\) / c #\)))

  )


;;;; done

)

;;; end of file
