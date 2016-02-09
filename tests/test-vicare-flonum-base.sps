;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for flonum functions
;;;Date: Sat Oct 20, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare system $flonums)
  (prefix (vicare expander) expander::)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare flonum functions\n")


(parametrise ((check-test-name	'predicates))

  (check-for-true  (flzero? +0.0))
  (check-for-true  (flzero? -0.0))
  (check-for-false (flzero? -123.0))
  (check-for-false (flzero? -123.0))

  (check-for-true  (flzero?/positive +0.0))
  (check-for-false (flzero?/positive -0.0))
  (check-for-false (flzero?/positive -123.0))
  (check-for-false (flzero?/positive -123.0))

  (check-for-false (flzero?/negative +0.0))
  (check-for-true  (flzero?/negative -0.0))
  (check-for-false (flzero?/negative -123.0))
  (check-for-false (flzero?/negative -123.0))

;;; --------------------------------------------------------------------

  (check-for-true  (apply flzero? '(+0.0)))
  (check-for-true  (apply flzero? '(-0.0)))
  (check-for-false (apply flzero? '(-123.0)))
  (check-for-false (apply flzero? '(-123.0)))

  (check-for-true  (apply flzero?/positive '(+0.0)))
  (check-for-false (apply flzero?/positive '(-0.0)))
  (check-for-false (apply flzero?/positive '(-123.0)))
  (check-for-false (apply flzero?/positive '(-123.0)))

  (check-for-false (apply flzero?/negative '(+0.0)))
  (check-for-true  (apply flzero?/negative '(-0.0)))
  (check-for-false (apply flzero?/negative '(-123.0)))
  (check-for-false (apply flzero?/negative '(-123.0)))

;;; --------------------------------------------------------------------

  (check-for-true  ($flzero? +0.0))
  (check-for-true  ($flzero? -0.0))
  (check-for-false ($flzero? -123.0))
  (check-for-false ($flzero? -123.0))

  (check-for-true  ($flzero?/positive +0.0))
  (check-for-false ($flzero?/positive -0.0))
  (check-for-false ($flzero?/positive -123.0))
  (check-for-false ($flzero?/positive -123.0))

  (check-for-false ($flzero?/negative +0.0))
  (check-for-true  ($flzero?/negative -0.0))
  (check-for-false ($flzero?/negative -123.0))
  (check-for-false ($flzero?/negative -123.0))

;;; --------------------------------------------------------------------

  (check-for-true  (apply $flzero? '(+0.0)))
  (check-for-true  (apply $flzero? '(-0.0)))
  (check-for-false (apply $flzero? '(-123.0)))
  (check-for-false (apply $flzero? '(-123.0)))

  (check-for-true  (apply $flzero?/positive '(+0.0)))
  (check-for-false (apply $flzero?/positive '(-0.0)))
  (check-for-false (apply $flzero?/positive '(-123.0)))
  (check-for-false (apply $flzero?/positive '(-123.0)))

  (check-for-false (apply $flzero?/negative '(+0.0)))
  (check-for-true  (apply $flzero?/negative '(-0.0)))
  (check-for-false (apply $flzero?/negative '(-123.0)))
  (check-for-false (apply $flzero?/negative '(-123.0)))


;;; --------------------------------------------------------------------

  (check-for-false ($flpositive? +0.0))
  (check-for-false ($flpositive? -0.0))
  (check-for-true  ($flpositive? +123.0))
  (check-for-false ($flpositive? -123.0))

  (check-for-false ($flnegative? +0.0))
  (check-for-false ($flnegative? -0.0))
  (check-for-false ($flnegative? +123.0))
  (check-for-true  ($flnegative? -123.0))

  (check-for-false ($flnonpositive? +0.0))
  (check-for-true  ($flnonpositive? -0.0))
  (check-for-false ($flnonpositive? +123.0))
  (check-for-true  ($flnonpositive? -123.0))

  (check-for-true  ($flnonnegative? +0.0))
  (check-for-false ($flnonnegative? -0.0))
  (check-for-true  ($flnonnegative? +123.0))
  (check-for-false ($flnonnegative? -123.0))

;;; --------------------------------------------------------------------

  (check-for-false (apply $flpositive? '(+0.0)))
  (check-for-false (apply $flpositive? '(-0.0)))
  (check-for-true  (apply $flpositive? '(+123.0)))
  (check-for-false (apply $flpositive? '(-123.0)))

  (check-for-false (apply $flnegative? '(+0.0)))
  (check-for-false (apply $flnegative? '(-0.0)))
  (check-for-false (apply $flnegative? '(+123.0)))
  (check-for-true  (apply $flnegative? '(-123.0)))

  (check-for-false (apply $flnonpositive? '(+0.0)))
  (check-for-true  (apply $flnonpositive? '(-0.0)))
  (check-for-false (apply $flnonpositive? '(+123.0)))
  (check-for-true  (apply $flnonpositive? '(-123.0)))

  (check-for-true  (apply $flnonnegative? '(+0.0)))
  (check-for-false (apply $flnonnegative? '(-0.0)))
  (check-for-true  (apply $flnonnegative? '(+123.0)))
  (check-for-false (apply $flnonnegative? '(-123.0)))

;;; --------------------------------------------------------------------

  (check-for-false (flnonpositive? +0.0))
  (check-for-true  (flnonpositive? -0.0))
  (check-for-false (flnonpositive? +123.0))
  (check-for-true  (flnonpositive? -123.0))

  (check-for-true  (flnonnegative? +0.0))
  (check-for-false (flnonnegative? -0.0))
  (check-for-true  (flnonnegative? +123.0))
  (check-for-false (flnonnegative? -123.0))

;;; --------------------------------------------------------------------

  (check-for-false (apply flnonpositive? '(+0.0)))
  (check-for-true  (apply flnonpositive? '(-0.0)))
  (check-for-false (apply flnonpositive? '(+123.0)))
  (check-for-true  (apply flnonpositive? '(-123.0)))

  (check-for-true  (apply flnonnegative? '(+0.0)))
  (check-for-false (apply flnonnegative? '(-0.0)))
  (check-for-true  (apply flnonnegative? '(+123.0)))
  (check-for-false (apply flnonnegative? '(-123.0)))

;;; --------------------------------------------------------------------

  (check-for-true  (zero-flonum? +0.0))
  (check-for-true  (zero-flonum? -0.0))
  (check-for-false (zero-flonum? +1.0))
  (check-for-false (zero-flonum? -1.0))
  (check-for-false (zero-flonum? "ciao"))

  (check-for-true  (positive-zero-flonum? +0.0))
  (check-for-false (positive-zero-flonum? -0.0))
  (check-for-false (positive-zero-flonum? +1.0))
  (check-for-false (positive-zero-flonum? -1.0))
  (check-for-false (positive-zero-flonum? "ciao"))

  (check-for-false (negative-zero-flonum? +0.0))
  (check-for-true  (negative-zero-flonum? -0.0))
  (check-for-false (negative-zero-flonum? +1.0))
  (check-for-false (negative-zero-flonum? -1.0))
  (check-for-false (negative-zero-flonum? "ciao"))

  (check-for-true  (positive-flonum? +123.0))
  (check-for-false (positive-flonum? +0.0))
  (check-for-false (positive-flonum? -0.0))
  (check-for-false (positive-flonum? -123.0))
  (check-for-false (positive-flonum? "ciao"))

  (check-for-true  (negative-flonum? -123.0))
  (check-for-false (negative-flonum? +0.0))
  (check-for-false (negative-flonum? -0.0))
  (check-for-false (negative-flonum? +123.0))
  (check-for-false (negative-flonum? "ciao"))

  (check-for-false (non-positive-flonum? +0.0))
  (check-for-true  (non-positive-flonum? -0.0))
  (check-for-true  (non-positive-flonum? -123.0))
  (check-for-false (non-positive-flonum? +123.0))
  (check-for-false (non-positive-flonum? "ciao"))

  (check-for-true  (non-negative-flonum? +0.0))
  (check-for-false (non-negative-flonum? -0.0))
  (check-for-true  (non-negative-flonum? +123.0))
  (check-for-false (non-negative-flonum? -123.0))
  (check-for-false (non-negative-flonum? "ciao"))

;;; --------------------------------------------------------------------

  (check-for-true  (apply zero-flonum? '(+0.0)))
  (check-for-true  (apply zero-flonum? '(-0.0)))
  (check-for-false (apply zero-flonum? '(+1.0)))
  (check-for-false (apply zero-flonum? '(-1.0)))
  (check-for-false (apply zero-flonum? '("ciao")))

  (check-for-true  (apply positive-zero-flonum? '(+0.0)))
  (check-for-false (apply positive-zero-flonum? '(-0.0)))
  (check-for-false (apply positive-zero-flonum? '(+1.0)))
  (check-for-false (apply positive-zero-flonum? '(-1.0)))
  (check-for-false (apply positive-zero-flonum? '("ciao")))

  (check-for-false (apply negative-zero-flonum? '(+0.0)))
  (check-for-true  (apply negative-zero-flonum? '(-0.0)))
  (check-for-false (apply negative-zero-flonum? '(+1.0)))
  (check-for-false (apply negative-zero-flonum? '(-1.0)))
  (check-for-false (apply negative-zero-flonum? '("ciao")))

  (check-for-true  (apply positive-flonum? '(+123.0)))
  (check-for-false (apply positive-flonum? '(+0.0)))
  (check-for-false (apply positive-flonum? '(-0.0)))
  (check-for-false (apply positive-flonum? '(-123.0)))
  (check-for-false (apply positive-flonum? '("ciao")))

  (check-for-true  (apply negative-flonum? '(-123.0)))
  (check-for-false (apply negative-flonum? '(+0.0)))
  (check-for-false (apply negative-flonum? '(-0.0)))
  (check-for-false (apply negative-flonum? '(+123.0)))
  (check-for-false (apply negative-flonum? '("ciao")))

  (check-for-false (apply non-positive-flonum? '(+0.0)))
  (check-for-true  (apply non-positive-flonum? '(-0.0)))
  (check-for-true  (apply non-positive-flonum? '(-123.0)))
  (check-for-false (apply non-positive-flonum? '(+123.0)))
  (check-for-false (apply non-positive-flonum? '("ciao")))

  (check-for-true  (apply non-negative-flonum? '(+0.0)))
  (check-for-false (apply non-negative-flonum? '(-0.0)))
  (check-for-true  (apply non-negative-flonum? '(+123.0)))
  (check-for-false (apply non-negative-flonum? '(-123.0)))
  (check-for-false (apply non-negative-flonum? '("ciao")))

  #t)


(parametrise ((check-test-name	'comparison))

  (check (fl!=? +1.0 +1.0)		=> #f)
  (check (fl!=? +1.0 +2.0)		=> #t)
  (check (fl!=? +2.0 +1.0)		=> #t)

  ;;This function does *not* distinguish between +0.0  and -0.0.  And it is fine this
  ;;way.
  (check (fl!=? +0.0 +0.0)		=> #f)
  (check (fl!=? -0.0 -0.0)		=> #f)
  (check (fl!=? -0.0 +0.0)		=> #f)
  (check (fl!=? +0.0 -0.0)		=> #f)

  (check (fl!=? +3.0 +2.0 +1.0)		=> #t)
  (check (fl!=? +1.0 +1.0 +1.0)		=> #f)
  (check (fl!=? +2.0 +1.0 +1.0)		=> #f)
  (check (fl!=? +1.0 +2.0 +1.0)		=> #f)
  (check (fl!=? +1.0 +1.0 +2.0)		=> #f)
  (check (fl!=? +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl!=? +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl!=? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl!=? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl!=? +1.0 +1.0 +1.0 +2.0)	=> #f)
  (check (fl!=? +1.0 +2.0 +3.0 +4.0)	=> #t)

;;; --------------------------------------------------------------------

  (check (fl=? +1.0)			=> #t)
  (check (fl=? +1.0 +1.0)		=> #t)
  (check (fl=? +1.0 +2.0)		=> #f)
  (check (fl=? +2.0 +1.0)		=> #f)
  (check (fl=? +0.0 +0.0)		=> #t)
  (check (fl=? -0.0 -0.0)		=> #t)
  (check (fl=? +0.0 -0.0)		=> #t)
  (check (fl=? -0.0 +0.0)		=> #t)
  (check (fl=? +1.0 +1.0 +1.0)		=> #t)
  (check (fl=? +2.0 +1.0 +1.0)		=> #f)
  (check (fl=? +1.0 +2.0 +1.0)		=> #f)
  (check (fl=? +1.0 +1.0 +2.0)		=> #f)
  (check (fl=? +1.0 +1.0 +1.0 +1.0)	=> #t)
  (check (fl=? +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl=? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl=? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl=? +1.0 +1.0 +1.0 +2.0)	=> #f)

  (check (fl!=? +1.0)			=> #f)
  (check (fl!=? +1.0 +1.0)		=> #f)
  (check (fl!=? +1.0 +2.0)		=> #t)
  (check (fl!=? +2.0 +1.0)		=> #t)
  (check (fl!=? +0.0 +0.0)		=> #f)
  (check (fl!=? -0.0 -0.0)		=> #f)
  (check (fl!=? +0.0 -0.0)		=> #f)
  (check (fl!=? -0.0 +0.0)		=> #f)
  (check (fl!=? +1.0 +1.0 +1.0)		=> #f)
  (check (fl!=? +2.0 +1.0 +1.0)		=> #f)
  (check (fl!=? +1.0 +2.0 +1.0)		=> #f)
  (check (fl!=? +1.0 +1.0 +2.0)		=> #f)
  (check (fl!=? +1.0 +2.0 +3.0)		=> #t)
  (check (fl!=? +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl!=? +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl!=? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl!=? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl!=? +1.0 +1.0 +1.0 +2.0)	=> #f)
  (check (fl!=? +1.0 +2.0 +3.0 +4.0)	=> #t)

  (check (fl<? +1.0)			=> #t)
  (check (fl<? +1.0 +1.0)		=> #f)
  (check (fl<? +1.0 +2.0)		=> #t)
  (check (fl<? +2.0 +1.0)		=> #f)
  (check (fl<? +0.0 +0.0)		=> #f)
  (check (fl<? -0.0 -0.0)		=> #f)
  (check (fl<? +0.0 -0.0)		=> #f)
  (check (fl<? -0.0 +0.0)		=> #f)
  (check (fl<? +1.0 +1.0 +1.0)		=> #f)
  (check (fl<? +2.0 +1.0 +1.0)		=> #f)
  (check (fl<? +1.0 +2.0 +1.0)		=> #f)
  (check (fl<? +1.0 +1.0 +2.0)		=> #f)
  (check (fl<? +1.0 +2.0 +3.0)		=> #t)
  (check (fl<? +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl<? +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl<? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl<? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl<? +1.0 +1.0 +1.0 +2.0)	=> #f)

  (check (fl<=? +1.0)			=> #t)
  (check (fl<=? +1.0 +1.0)		=> #t)
  (check (fl<=? +1.0 +2.0)		=> #t)
  (check (fl<=? +2.0 +1.0)		=> #f)
  (check (fl<=? +0.0 +0.0)		=> #t)
  (check (fl<=? -0.0 -0.0)		=> #t)
  (check (fl<=? +0.0 -0.0)		=> #t)
  (check (fl<=? -0.0 +0.0)		=> #t)
  (check (fl<=? +1.0 +1.0 +1.0)		=> #t)
  (check (fl<=? +2.0 +1.0 +1.0)		=> #f)
  (check (fl<=? +1.0 +2.0 +1.0)		=> #f)
  (check (fl<=? +1.0 +1.0 +2.0)		=> #t)
  (check (fl<=? +1.0 +2.0 +3.0)		=> #t)
  (check (fl<=? +1.0 +1.0 +1.0 +1.0)	=> #t)
  (check (fl<=? +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl<=? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl<=? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl<=? +1.0 +1.0 +1.0 +2.0)	=> #t)

  (check (fl>? +1.0)			=> #t)
  (check (fl>? +1.0 +1.0)		=> #f)
  (check (fl>? +1.0 +2.0)		=> #f)
  (check (fl>? +2.0 +1.0)		=> #t)
  (check (fl>? +0.0 +0.0)		=> #f)
  (check (fl>? -0.0 -0.0)		=> #f)
  (check (fl>? +0.0 -0.0)		=> #f)
  (check (fl>? -0.0 +0.0)		=> #f)
  (check (fl>? +1.0 +1.0 +1.0)		=> #f)
  (check (fl>? +2.0 +1.0 +1.0)		=> #f)
  (check (fl>? +1.0 +2.0 +1.0)		=> #f)
  (check (fl>? +1.0 +1.0 +2.0)		=> #f)
  (check (fl>? +3.0 +2.0 +1.0)		=> #t)
  (check (fl>? +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl>? +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check (fl>? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl>? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl>? +1.0 +1.0 +1.0 +2.0)	=> #f)

  (check (fl>=? +1.0)			=> #t)
  (check (fl>=? +1.0 +1.0)		=> #t)
  (check (fl>=? +1.0 +2.0)		=> #f)
  (check (fl>=? +2.0 +1.0)		=> #t)
  (check (fl>=? +0.0 +0.0)		=> #t)
  (check (fl>=? -0.0 -0.0)		=> #t)
  (check (fl>=? +0.0 -0.0)		=> #t)
  (check (fl>=? -0.0 +0.0)		=> #t)
  (check (fl>=? +1.0 +1.0 +1.0)		=> #t)
  (check (fl>=? +2.0 +1.0 +1.0)		=> #t)
  (check (fl>=? +1.0 +2.0 +1.0)		=> #f)
  (check (fl>=? +1.0 +1.0 +2.0)		=> #f)
  (check (fl>=? +3.0 +2.0 +1.0)		=> #t)
  (check (fl>=? +1.0 +1.0 +1.0 +1.0)	=> #t)
  (check (fl>=? +2.0 +1.0 +1.0 +1.0)	=> #t)
  (check (fl>=? +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check (fl>=? +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check (fl>=? +1.0 +1.0 +1.0 +2.0)	=> #f)

;;; --------------------------------------------------------------------

  (check (apply fl=? '(+1.0))			=> #t)
  (check (apply fl=? '(+1.0 +1.0))		=> #t)
  (check (apply fl=? '(+1.0 +2.0))		=> #f)
  (check (apply fl=? '(+2.0 +1.0))		=> #f)
  (check (apply fl=? '(+0.0 +0.0))		=> #t)
  (check (apply fl=? '(-0.0 -0.0))		=> #t)
  (check (apply fl=? '(+0.0 -0.0))		=> #t)
  (check (apply fl=? '(-0.0 +0.0))		=> #t)
  (check (apply fl=? '(+1.0 +1.0 +1.0))		=> #t)
  (check (apply fl=? '(+2.0 +1.0 +1.0))		=> #f)
  (check (apply fl=? '(+1.0 +2.0 +1.0))		=> #f)
  (check (apply fl=? '(+1.0 +1.0 +2.0))		=> #f)
  (check (apply fl=? '(+1.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply fl=? '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl=? '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply fl=? '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply fl=? '(+1.0 +1.0 +1.0 +2.0))	=> #f)

  (check (apply fl!=? '(+1.0))			=> #f)
  (check (apply fl!=? '(+1.0 +1.0))		=> #f)
  (check (apply fl!=? '(+1.0 +2.0))		=> #t)
  (check (apply fl!=? '(+2.0 +1.0))		=> #t)
  (check (apply fl!=? '(+0.0 +0.0))		=> #f)
  (check (apply fl!=? '(-0.0 -0.0))		=> #f)
  (check (apply fl!=? '(+0.0 -0.0))		=> #f)
  (check (apply fl!=? '(-0.0 +0.0))		=> #f)
  (check (apply fl!=? '(+1.0 +1.0 +1.0))	=> #f)
  (check (apply fl!=? '(+2.0 +1.0 +1.0))	=> #f)
  (check (apply fl!=? '(+1.0 +2.0 +1.0))	=> #f)
  (check (apply fl!=? '(+1.0 +1.0 +2.0))	=> #f)
  (check (apply fl!=? '(+1.0 +2.0 +3.0))	=> #t)
  (check (apply fl!=? '(+1.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl!=? '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl!=? '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply fl!=? '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply fl!=? '(+1.0 +1.0 +1.0 +2.0))	=> #f)
  (check (apply fl!=? '(+1.0 +2.0 +3.0 +4.0))	=> #t)

  (check (apply fl<? '(+1.0))			=> #t)
  (check (apply fl<? '(+1.0 +1.0))		=> #f)
  (check (apply fl<? '(+1.0 +2.0))		=> #t)
  (check (apply fl<? '(+2.0 +1.0))		=> #f)
  (check (apply fl<? '(+0.0 +0.0))		=> #f)
  (check (apply fl<? '(-0.0 -0.0))		=> #f)
  (check (apply fl<? '(+0.0 -0.0))		=> #f)
  (check (apply fl<? '(-0.0 +0.0))		=> #f)
  (check (apply fl<? '(+1.0 +1.0 +1.0))		=> #f)
  (check (apply fl<? '(+2.0 +1.0 +1.0))		=> #f)
  (check (apply fl<? '(+1.0 +2.0 +1.0))		=> #f)
  (check (apply fl<? '(+1.0 +1.0 +2.0))		=> #f)
  (check (apply fl<? '(+1.0 +2.0 +3.0))		=> #t)
  (check (apply fl<? '(+1.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl<? '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl<? '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply fl<? '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply fl<? '(+1.0 +1.0 +1.0 +2.0))	=> #f)
  (check (apply fl<? '(+1.0 +2.0 +3.0 +4.0))	=> #t)

  (check (apply fl<=? '(+1.0))			=> #t)
  (check (apply fl<=? '(+1.0 +1.0))		=> #t)
  (check (apply fl<=? '(+1.0 +2.0))		=> #t)
  (check (apply fl<=? '(+2.0 +1.0))		=> #f)
  (check (apply fl<=? '(+0.0 +0.0))		=> #t)
  (check (apply fl<=? '(-0.0 -0.0))		=> #t)
  (check (apply fl<=? '(+0.0 -0.0))		=> #t)
  (check (apply fl<=? '(-0.0 +0.0))		=> #t)
  (check (apply fl<=? '(+1.0 +1.0 +1.0))	=> #t)
  (check (apply fl<=? '(+2.0 +1.0 +1.0))	=> #f)
  (check (apply fl<=? '(+1.0 +2.0 +1.0))	=> #f)
  (check (apply fl<=? '(+1.0 +1.0 +2.0))	=> #t)
  (check (apply fl<=? '(+1.0 +2.0 +3.0))	=> #t)
  (check (apply fl<=? '(+1.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply fl<=? '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl<=? '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply fl<=? '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply fl<=? '(+1.0 +1.0 +1.0 +2.0))	=> #t)

  (check (apply fl>? '(+1.0))			=> #t)
  (check (apply fl>? '(+1.0 +1.0))		=> #f)
  (check (apply fl>? '(+1.0 +2.0))		=> #f)
  (check (apply fl>? '(+2.0 +1.0))		=> #t)
  (check (apply fl>? '(+0.0 +0.0))		=> #f)
  (check (apply fl>? '(-0.0 -0.0))		=> #f)
  (check (apply fl>? '(+0.0 -0.0))		=> #f)
  (check (apply fl>? '(-0.0 +0.0))		=> #f)
  (check (apply fl>? '(+1.0 +1.0 +1.0))		=> #f)
  (check (apply fl>? '(+2.0 +1.0 +1.0))		=> #f)
  (check (apply fl>? '(+1.0 +2.0 +1.0))		=> #f)
  (check (apply fl>? '(+1.0 +1.0 +2.0))		=> #f)
  (check (apply fl>? '(+3.0 +2.0 +1.0))		=> #t)
  (check (apply fl>? '(+1.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl>? '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply fl>? '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply fl>? '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply fl>? '(+1.0 +1.0 +1.0 +2.0))	=> #f)
  (check (apply fl>? '(+4.0 +3.0 +2.0 +1.0))	=> #t)

  (check (apply fl>=? '(+1.0))			=> #t)
  (check (apply fl>=? '(+1.0 +1.0))		=> #t)
  (check (apply fl>=? '(+1.0 +2.0))		=> #f)
  (check (apply fl>=? '(+2.0 +1.0))		=> #t)
  (check (apply fl>=? '(+0.0 +0.0))		=> #t)
  (check (apply fl>=? '(-0.0 -0.0))		=> #t)
  (check (apply fl>=? '(+0.0 -0.0))		=> #t)
  (check (apply fl>=? '(-0.0 +0.0))		=> #t)
  (check (apply fl>=? '(+1.0 +1.0 +1.0))	=> #t)
  (check (apply fl>=? '(+2.0 +1.0 +1.0))	=> #t)
  (check (apply fl>=? '(+1.0 +2.0 +1.0))	=> #f)
  (check (apply fl>=? '(+1.0 +1.0 +2.0))	=> #f)
  (check (apply fl>=? '(+3.0 +2.0 +1.0))	=> #t)
  (check (apply fl>=? '(+1.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply fl>=? '(+2.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply fl>=? '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply fl>=? '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply fl>=? '(+1.0 +1.0 +1.0 +2.0))	=> #f)

;;; --------------------------------------------------------------------

  (check ($fl= +1.0)			=> #t)
  (check ($fl= +1.0 +1.0)		=> #t)
  (check ($fl= +1.0 +2.0)		=> #f)
  (check ($fl= +2.0 +1.0)		=> #f)
  (check ($fl= +0.0 +0.0)		=> #t)
  (check ($fl= -0.0 -0.0)		=> #t)
  (check ($fl= +0.0 -0.0)		=> #t)
  (check ($fl= -0.0 +0.0)		=> #t)
  (check ($fl= +1.0 +1.0 +1.0)		=> #t)
  (check ($fl= +2.0 +1.0 +1.0)		=> #f)
  (check ($fl= +1.0 +2.0 +1.0)		=> #f)
  (check ($fl= +1.0 +1.0 +2.0)		=> #f)
  (check ($fl= +1.0 +1.0 +1.0 +1.0)	=> #t)
  (check ($fl= +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl= +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check ($fl= +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check ($fl= +1.0 +1.0 +1.0 +2.0)	=> #f)

  (check ($fl!= +1.0)			=> #f)
  (check ($fl!= +1.0 +1.0)		=> #f)
  (check ($fl!= +1.0 +2.0)		=> #t)
  (check ($fl!= +2.0 +1.0)		=> #t)
  (check ($fl!= +0.0 +0.0)		=> #f)
  (check ($fl!= -0.0 -0.0)		=> #f)
  (check ($fl!= +0.0 -0.0)		=> #f)
  (check ($fl!= -0.0 +0.0)		=> #f)
  (check ($fl!= +1.0 +1.0 +1.0)		=> #f)
  (check ($fl!= +2.0 +1.0 +1.0)		=> #f)
  (check ($fl!= +1.0 +2.0 +1.0)		=> #f)
  (check ($fl!= +1.0 +1.0 +2.0)		=> #f)
  (check ($fl!= +1.0 +2.0 +3.0)		=> #t)
  (check ($fl!= +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl!= +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl!= +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check ($fl!= +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check ($fl!= +1.0 +1.0 +1.0 +2.0)	=> #f)
  (check ($fl!= +1.0 +2.0 +3.0 +4.0)	=> #t)

  (check ($fl< +1.0)			=> #t)
  (check ($fl< +1.0 +1.0)		=> #f)
  (check ($fl< +1.0 +2.0)		=> #t)
  (check ($fl< +2.0 +1.0)		=> #f)
  (check ($fl< +0.0 +0.0)		=> #f)
  (check ($fl< -0.0 -0.0)		=> #f)
  (check ($fl< +0.0 -0.0)		=> #f)
  (check ($fl< -0.0 +0.0)		=> #f)
  (check ($fl< +1.0 +1.0 +1.0)		=> #f)
  (check ($fl< +2.0 +1.0 +1.0)		=> #f)
  (check ($fl< +1.0 +2.0 +1.0)		=> #f)
  (check ($fl< +1.0 +1.0 +2.0)		=> #f)
  (check ($fl< +1.0 +2.0 +3.0)		=> #t)
  (check ($fl< +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl< +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl< +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check ($fl< +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check ($fl< +1.0 +1.0 +1.0 +2.0)	=> #f)

  (check ($fl<= +1.0)			=> #t)
  (check ($fl<= +1.0 +1.0)		=> #t)
  (check ($fl<= +1.0 +2.0)		=> #t)
  (check ($fl<= +2.0 +1.0)		=> #f)
  (check ($fl<= +0.0 +0.0)		=> #t)
  (check ($fl<= -0.0 -0.0)		=> #t)
  (check ($fl<= +0.0 -0.0)		=> #t)
  (check ($fl<= -0.0 +0.0)		=> #t)
  (check ($fl<= +1.0 +1.0 +1.0)		=> #t)
  (check ($fl<= +2.0 +1.0 +1.0)		=> #f)
  (check ($fl<= +1.0 +2.0 +1.0)		=> #f)
  (check ($fl<= +1.0 +1.0 +2.0)		=> #t)
  (check ($fl<= +1.0 +2.0 +3.0)		=> #t)
  (check ($fl<= +1.0 +1.0 +1.0 +1.0)	=> #t)
  (check ($fl<= +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl<= +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check ($fl<= +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check ($fl<= +1.0 +1.0 +1.0 +2.0)	=> #t)

  (check ($fl> +1.0)			=> #t)
  (check ($fl> +1.0 +1.0)		=> #f)
  (check ($fl> +1.0 +2.0)		=> #f)
  (check ($fl> +2.0 +1.0)		=> #t)
  (check ($fl> +0.0 +0.0)		=> #f)
  (check ($fl> -0.0 -0.0)		=> #f)
  (check ($fl> +0.0 -0.0)		=> #f)
  (check ($fl> -0.0 +0.0)		=> #f)
  (check ($fl> +1.0 +1.0 +1.0)		=> #f)
  (check ($fl> +2.0 +1.0 +1.0)		=> #f)
  (check ($fl> +1.0 +2.0 +1.0)		=> #f)
  (check ($fl> +1.0 +1.0 +2.0)		=> #f)
  (check ($fl> +3.0 +2.0 +1.0)		=> #t)
  (check ($fl> +1.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl> +2.0 +1.0 +1.0 +1.0)	=> #f)
  (check ($fl> +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check ($fl> +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check ($fl> +1.0 +1.0 +1.0 +2.0)	=> #f)

  (check ($fl>= +1.0)			=> #t)
  (check ($fl>= +1.0 +1.0)		=> #t)
  (check ($fl>= +1.0 +2.0)		=> #f)
  (check ($fl>= +2.0 +1.0)		=> #t)
  (check ($fl>= +0.0 +0.0)		=> #t)
  (check ($fl>= -0.0 -0.0)		=> #t)
  (check ($fl>= +0.0 -0.0)		=> #t)
  (check ($fl>= -0.0 +0.0)		=> #t)
  (check ($fl>= +1.0 +1.0 +1.0)		=> #t)
  (check ($fl>= +2.0 +1.0 +1.0)		=> #t)
  (check ($fl>= +1.0 +2.0 +1.0)		=> #f)
  (check ($fl>= +1.0 +1.0 +2.0)		=> #f)
  (check ($fl>= +3.0 +2.0 +1.0)		=> #t)
  (check ($fl>= +1.0 +1.0 +1.0 +1.0)	=> #t)
  (check ($fl>= +2.0 +1.0 +1.0 +1.0)	=> #t)
  (check ($fl>= +1.0 +2.0 +1.0 +1.0)	=> #f)
  (check ($fl>= +1.0 +1.0 +2.0 +1.0)	=> #f)
  (check ($fl>= +1.0 +1.0 +1.0 +2.0)	=> #f)

;;; --------------------------------------------------------------------

  (check (apply $fl= '(+1.0))			=> #t)
  (check (apply $fl= '(+1.0 +1.0))		=> #t)
  (check (apply $fl= '(+1.0 +2.0))		=> #f)
  (check (apply $fl= '(+2.0 +1.0))		=> #f)
  (check (apply $fl= '(+0.0 +0.0))		=> #t)
  (check (apply $fl= '(-0.0 -0.0))		=> #t)
  (check (apply $fl= '(+0.0 -0.0))		=> #t)
  (check (apply $fl= '(-0.0 +0.0))		=> #t)
  (check (apply $fl= '(+1.0 +1.0 +1.0))		=> #t)
  (check (apply $fl= '(+2.0 +1.0 +1.0))		=> #f)
  (check (apply $fl= '(+1.0 +2.0 +1.0))		=> #f)
  (check (apply $fl= '(+1.0 +1.0 +2.0))		=> #f)
  (check (apply $fl= '(+1.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply $fl= '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl= '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl= '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl= '(+1.0 +1.0 +1.0 +2.0))	=> #f)

  (check (apply $fl!= '(+1.0))			=> #f)
  (check (apply $fl!= '(+1.0 +1.0))		=> #f)
  (check (apply $fl!= '(+1.0 +2.0))		=> #t)
  (check (apply $fl!= '(+2.0 +1.0))		=> #t)
  (check (apply $fl!= '(+0.0 +0.0))		=> #f)
  (check (apply $fl!= '(-0.0 -0.0))		=> #f)
  (check (apply $fl!= '(+0.0 -0.0))		=> #f)
  (check (apply $fl!= '(-0.0 +0.0))		=> #f)
  (check (apply $fl!= '(+1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl!= '(+2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl!= '(+1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl!= '(+1.0 +1.0 +2.0))	=> #f)
  (check (apply $fl!= '(+1.0 +2.0 +3.0))	=> #t)
  (check (apply $fl!= '(+1.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl!= '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl!= '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl!= '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl!= '(+1.0 +1.0 +1.0 +2.0))	=> #f)
  (check (apply $fl!= '(+1.0 +2.0 +3.0 +4.0))	=> #t)

  (check (apply $fl< '(+1.0))			=> #t)
  (check (apply $fl< '(+1.0 +1.0))		=> #f)
  (check (apply $fl< '(+1.0 +2.0))		=> #t)
  (check (apply $fl< '(+2.0 +1.0))		=> #f)
  (check (apply $fl< '(+0.0 +0.0))		=> #f)
  (check (apply $fl< '(-0.0 -0.0))		=> #f)
  (check (apply $fl< '(+0.0 -0.0))		=> #f)
  (check (apply $fl< '(-0.0 +0.0))		=> #f)
  (check (apply $fl< '(+1.0 +1.0 +1.0))		=> #f)
  (check (apply $fl< '(+2.0 +1.0 +1.0))		=> #f)
  (check (apply $fl< '(+1.0 +2.0 +1.0))		=> #f)
  (check (apply $fl< '(+1.0 +1.0 +2.0))		=> #f)
  (check (apply $fl< '(+1.0 +2.0 +3.0))		=> #t)
  (check (apply $fl< '(+1.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl< '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl< '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl< '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl< '(+1.0 +1.0 +1.0 +2.0))	=> #f)
  (check (apply $fl< '(+1.0 +2.0 +3.0 +4.0))	=> #t)

  (check (apply $fl<= '(+1.0))			=> #t)
  (check (apply $fl<= '(+1.0 +1.0))		=> #t)
  (check (apply $fl<= '(+1.0 +2.0))		=> #t)
  (check (apply $fl<= '(+2.0 +1.0))		=> #f)
  (check (apply $fl<= '(+0.0 +0.0))		=> #t)
  (check (apply $fl<= '(-0.0 -0.0))		=> #t)
  (check (apply $fl<= '(+0.0 -0.0))		=> #t)
  (check (apply $fl<= '(-0.0 +0.0))		=> #t)
  (check (apply $fl<= '(+1.0 +1.0 +1.0))	=> #t)
  (check (apply $fl<= '(+2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl<= '(+1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl<= '(+1.0 +1.0 +2.0))	=> #t)
  (check (apply $fl<= '(+1.0 +2.0 +3.0))	=> #t)
  (check (apply $fl<= '(+1.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply $fl<= '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl<= '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl<= '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl<= '(+1.0 +1.0 +1.0 +2.0))	=> #t)

  (check (apply $fl> '(+1.0))			=> #t)
  (check (apply $fl> '(+1.0 +1.0))		=> #f)
  (check (apply $fl> '(+1.0 +2.0))		=> #f)
  (check (apply $fl> '(+2.0 +1.0))		=> #t)
  (check (apply $fl> '(+0.0 +0.0))		=> #f)
  (check (apply $fl> '(-0.0 -0.0))		=> #f)
  (check (apply $fl> '(+0.0 -0.0))		=> #f)
  (check (apply $fl> '(-0.0 +0.0))		=> #f)
  (check (apply $fl> '(+1.0 +1.0 +1.0))		=> #f)
  (check (apply $fl> '(+2.0 +1.0 +1.0))		=> #f)
  (check (apply $fl> '(+1.0 +2.0 +1.0))		=> #f)
  (check (apply $fl> '(+1.0 +1.0 +2.0))		=> #f)
  (check (apply $fl> '(+3.0 +2.0 +1.0))		=> #t)
  (check (apply $fl> '(+1.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl> '(+2.0 +1.0 +1.0 +1.0))	=> #f)
  (check (apply $fl> '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl> '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl> '(+1.0 +1.0 +1.0 +2.0))	=> #f)
  (check (apply $fl> '(+4.0 +3.0 +2.0 +1.0))	=> #t)

  (check (apply $fl>= '(+1.0))			=> #t)
  (check (apply $fl>= '(+1.0 +1.0))		=> #t)
  (check (apply $fl>= '(+1.0 +2.0))		=> #f)
  (check (apply $fl>= '(+2.0 +1.0))		=> #t)
  (check (apply $fl>= '(+0.0 +0.0))		=> #t)
  (check (apply $fl>= '(-0.0 -0.0))		=> #t)
  (check (apply $fl>= '(+0.0 -0.0))		=> #t)
  (check (apply $fl>= '(-0.0 +0.0))		=> #t)
  (check (apply $fl>= '(+1.0 +1.0 +1.0))	=> #t)
  (check (apply $fl>= '(+2.0 +1.0 +1.0))	=> #t)
  (check (apply $fl>= '(+1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl>= '(+1.0 +1.0 +2.0))	=> #f)
  (check (apply $fl>= '(+3.0 +2.0 +1.0))	=> #t)
  (check (apply $fl>= '(+1.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply $fl>= '(+2.0 +1.0 +1.0 +1.0))	=> #t)
  (check (apply $fl>= '(+1.0 +2.0 +1.0 +1.0))	=> #f)
  (check (apply $fl>= '(+1.0 +1.0 +2.0 +1.0))	=> #f)
  (check (apply $fl>= '(+1.0 +1.0 +1.0 +2.0))	=> #f)

  #f)


(parametrise ((check-test-name	'exactness))

  (check (exact 1.2e2)	=> 120)
  (check (exact +1.2e8)	=> +120000000)
  (check (exact -1.2e8) => -120000000)
  (check (exact +1.2e9)	=> +1200000000)
  (check (exact -1.2e9) => -1200000000)

  #t)


(parametrise ((check-test-name	'debug))

  (when #f
    (check-pretty-print (flonum->bytevector 123.456)))

  (check
      (bytevector->flonum (flonum->bytevector 123.456))
    => 123.456)

;;; --------------------------------------------------------------------

  (let* ((flo		123.456)
	 (octet0	($flonum-u8-ref flo 0))
	 (octet1	($flonum-u8-ref flo 1))
	 (octet2	($flonum-u8-ref flo 2))
	 (octet3	($flonum-u8-ref flo 3))
	 (octet4	($flonum-u8-ref flo 4))
	 (octet5	($flonum-u8-ref flo 5))
	 (octet6	($flonum-u8-ref flo 6))
	 (octet7	($flonum-u8-ref flo 7)))
    (check
	(let* ((bv	(flonum->bytevector flo))
	       (u0	(bytevector-u8-ref bv (+ 8 0)))
	       (u1	(bytevector-u8-ref bv (+ 8 1)))
	       (u2	(bytevector-u8-ref bv (+ 8 2)))
	       (u3	(bytevector-u8-ref bv (+ 8 3)))
	       (u4	(bytevector-u8-ref bv (+ 8 4)))
	       (u5	(bytevector-u8-ref bv (+ 8 5)))
	       (u6	(bytevector-u8-ref bv (+ 8 6)))
	       (u7	(bytevector-u8-ref bv (+ 8 7))))
	  (reverse (list u0 u1 u2 u3 u4 u5 u6 u7)))
      => (list octet0 octet1 octet2 octet3 octet4 octet5 octet6 octet7)))

  #t)


(parametrise ((check-test-name	'arithmetics))

  (check
      (try
	  (eval '(fl- 1)
		(environment '(rnrs)))
	(catch E
	  ((expander::&expand-time-type-signature-warning)
	   #t)
	  (else E)))
    => #t)

  #t)


(parametrise ((check-test-name	'funcs))

  (check (flmin +0.0 +0.0)	=> +0.0)
  (check (flmin +0.0 -0.0)	=> -0.0)
  (check (flmin -0.0 -0.0)	=> -0.0)

  (check (flmin 1.0 2.0)	=> 1.0)
  (check (flmin 2.0 1.0)	=> 1.0)
  (check (flmin 3.0 1.0 2.0)	=> 1.0)
  (check (flmin 3.0 2.0 1.0)	=> 1.0)
  (check (flmin 1.0 +nan.0)	=> +nan.0)
  (check (flmin +nan.0 1.0)	=> +nan.0)
  (check (flmin 3.0 1.0 +nan.0)	=> +nan.0)
  (check (flmin 3.0 +nan.0 1.0)	=> +nan.0)

  (check (flmax +0.0 +0.0)	=> +0.0)
  (check (flmax +0.0 -0.0)	=> +0.0)
  (check (flmax -0.0 -0.0)	=> -0.0)

  (check (flmax 1.0 2.0)	=> 2.0)
  (check (flmax 2.0 1.0)	=> 2.0)
  (check (flmax 0.0 1.0 2.0)	=> 2.0)
  (check (flmax 0.0 2.0 1.0)	=> 2.0)
  (check (flmax 1.0 +nan.0)	=> +nan.0)
  (check (flmax +nan.0 1.0)	=> +nan.0)
  (check (flmax 3.0 1.0 +nan.0)	=> +nan.0)
  (check (flmax 3.0 +nan.0 1.0)	=> +nan.0)

;;; --------------------------------------------------------------------

  (check
      (flsquare 1.2)
    => (* 1.2 1.2))

  (check
      ($flsquare 1.2)
    => (* 1.2 1.2))

;;; --------------------------------------------------------------------

  (check
      (flcube 1.2)
    => (* 1.2 1.2 1.2))

  (check
      ($flcube 1.2)
    => (* 1.2 1.2 1.2))

;;; --------------------------------------------------------------------

  (check
      (flhypot 1.2 3.4)
    => (sqrt (+ (square 1.2) (square 3.4))))

  (check
      ($flhypot 1.2 3.4)
    => (sqrt (+ (square 1.2) (square 3.4))))

;;; --------------------------------------------------------------------

  (check
      (flcbrt (flcube 1.2))
    => 1.2)

  (check
      ($flcbrt (flcube 1.2))
    => 1.2)

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;coding: utf-8-unix
;;End:
