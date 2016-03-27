;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for ratnum numeric objects
;;;Date: Sat Aug  3, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare system $ratnums)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: ratnum objects\n")


(parametrise ((check-test-name	'predicates))

  (check-for-true	(ratnum-positive? +3/4))
  (check-for-false	(ratnum-negative? +3/4))
  (check-for-false	(ratnum-non-positive? +3/4))
  (check-for-true	(ratnum-non-negative? +3/4))

  (check-for-false	(ratnum-positive? -3/4))
  (check-for-true	(ratnum-negative? -3/4))
  (check-for-true	(ratnum-non-positive? -3/4))
  (check-for-false	(ratnum-non-negative? -3/4))

  (check-for-true	(positive-ratnum? +3/4))
  (check-for-false	(negative-ratnum? +3/4))
  (check-for-false	(non-positive-ratnum? +3/4))
  (check-for-true	(non-negative-ratnum? +3/4))

  (check-for-false	(positive-ratnum? -3/4))
  (check-for-true	(negative-ratnum? -3/4))
  (check-for-true	(non-positive-ratnum? -3/4))
  (check-for-false	(non-negative-ratnum? -3/4))

;;; --------------------------------------------------------------------

  (check ($ratnum-positive? +1/2)		=> #t)
  (check ($ratnum-positive? -1/2)		=> #f)

  (check ($ratnum-negative? +1/2)		=> #f)
  (check ($ratnum-negative? -1/2)		=> #t)

  (check ($ratnum-non-positive? +1/2)		=> #f)
  (check ($ratnum-non-positive? -1/2)		=> #t)

  (check ($ratnum-non-negative? +1/2)		=> #t)
  (check ($ratnum-non-negative? -1/2)		=> #f)

  #t)


;;;; done

(check-report)

;;; end of file
