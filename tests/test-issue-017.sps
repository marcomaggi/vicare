;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for issue 17
;;;Date: Thu Sep  9, 2010
;;;
;;;Abstract
;;;
;;;	EXPT gives  an exception here, but  it should give some  sort of
;;;	NaN value:
;;;
;;;	   Ikarus Scheme version 0.0.4-rc1+, 64-bit (revision 1870, build 2010-01-26)
;;;	   Copyright (c) 2006-2009 Abdulaziz Ghuloum
;;;
;;;	   (expt +i +inf.0)
;;;        Unhandled exception
;;;        Condition components:
;;;
;;;        &assertion
;;;        &who: fllog
;;;        &message: "not a flonum"
;;;        &irritants: (0.0+1.0i)
;;;
;;;     Matches Ikarus bug 633393, reported by Göran Weinholt.
;;;
;;;Copyright (c) 2010, 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 17\n")



(check
    (expt +i +inf.0)
  => +nan.0+nan.0i)

(check
    (expt -i +inf.0)
  => +nan.0+nan.0i)

(check
    (expt -i -inf.0)
  => +nan.0+nan.0i)

(check
    (expt -i -inf.0)
  => +nan.0+nan.0i)


;;;; done

(check-report)

;;; end of file
