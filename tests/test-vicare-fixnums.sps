;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for fixnum functions
;;;Date: Thu Nov 22, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (ikarus system $fx)
  (vicare language-extensions syntaxes)
  (only (vicare platform words)
	case-word-size)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare fixnum functions and operations\n")


(parametrise ((check-test-name	'core))

  (when #t
    (fprintf (current-error-port)
	     "fixnum width=~a\nleast fixnum=~a\ngreatest fixnum=~a\n"
	     (fixnum-width)
	     (least-fixnum)
	     (greatest-fixnum)))

  (check
      (least-fixnum)
    => (case-word-size
	((32)	-536870912)
	((64)	-1152921504606846976)))

  (check
      (greatest-fixnum)
    => (case-word-size
	((32)	+536870911)
	((64)	+1152921504606846975)))

  (check
      (fixnum-width)
    => (case-word-size
	((32)	30)
	((64)	61)))

  #t)


(parametrise ((check-test-name	'mod))

  (check (fxmod +12 +12)	=> 0)
  (check (fxmod +12 -12)	=> 0)
  (check (fxmod -12 +12)	=> 0)
  (check (fxmod -12 -12)	=> 0)

  (check (fxmod +12 +3)		=> 0)
  (check (fxmod +12 -3)		=> 0)
  (check (fxmod -12 +3)		=> 0)
  (check (fxmod -12 -3)		=> 0)

  (check (fxmod +12 +4)		=> 0)
  (check (fxmod +12 -4)		=> 0)
  (check (fxmod -12 +4)		=> 0)
  (check (fxmod -12 -4)		=> 0)

  (check (fxmod +12 +5)		=> +2)
  (check (fxmod +12 -5)		=> +2)
  (check (fxmod -12 +5)		=> +3)
  (check (fxmod -12 -5)		=> +3)

  (check (fxmod +12 +7)		=> +5)
  (check (fxmod +12 -7)		=> +5)
  (check (fxmod -12 +7)		=> +2)
  (check (fxmod -12 -7)		=> +2)

  (check (fxmod +12 +24)	=> +12)
  (check (fxmod +12 -24)	=> +12)
  (check (fxmod -12 +24)	=> +12)
  (check (fxmod -12 -24)	=> +12)

  (check (fxmod +12 +20)	=> +12)
  (check (fxmod +12 -20)	=> +12)
  (check (fxmod -12 +20)	=> +8)
  (check (fxmod -12 -20)	=> +8)

;;; --------------------------------------------------------------------

  (check ($fxmod +12 +12)	=> 0)
  (check ($fxmod +12 -12)	=> 0)
  (check ($fxmod -12 +12)	=> 0)
  (check ($fxmod -12 -12)	=> 0)

  (check ($fxmod +12 +3)	=> 0)
  (check ($fxmod +12 -3)	=> 0)
  (check ($fxmod -12 +3)	=> 0)
  (check ($fxmod -12 -3)	=> 0)

  (check ($fxmod +12 +4)	=> 0)
  (check ($fxmod +12 -4)	=> 0)
  (check ($fxmod -12 +4)	=> 0)
  (check ($fxmod -12 -4)	=> 0)

  (check ($fxmod +12 +5)	=> +2)
  (check ($fxmod +12 -5)	=> +2)
  (check ($fxmod -12 +5)	=> +3)
  (check ($fxmod -12 -5)	=> +3)

  (check ($fxmod +12 +7)	=> +5)
  (check ($fxmod +12 -7)	=> +5)
  (check ($fxmod -12 +7)	=> +2)
  (check ($fxmod -12 -7)	=> +2)

  (check ($fxmod +12 +24)	=> +12)
  (check ($fxmod +12 -24)	=> +12)
  (check ($fxmod -12 +24)	=> +12)
  (check ($fxmod -12 -24)	=> +12)

  (check ($fxmod +12 +20)	=> +12)
  (check ($fxmod +12 -20)	=> +12)
  (check ($fxmod -12 +20)	=> +8)
  (check ($fxmod -12 -20)	=> +8)

  #t)


(parametrise ((check-test-name	'modulo))

  (check (fxmodulo +12 +12)	=> 0)
  (check (fxmodulo +12 -12)	=> 0)
  (check (fxmodulo -12 +12)	=> 0)
  (check (fxmodulo -12 -12)	=> 0)

  (check (fxmodulo +12 +3)	=> 0)
  (check (fxmodulo +12 -3)	=> 0)
  (check (fxmodulo -12 +3)	=> 0)
  (check (fxmodulo -12 -3)	=> 0)

  (check (fxmodulo +12 +4)	=> 0)
  (check (fxmodulo +12 -4)	=> 0)
  (check (fxmodulo -12 +4)	=> 0)
  (check (fxmodulo -12 -4)	=> 0)

  (check (fxmodulo +12 +5)	=> +2)
  (check (fxmodulo +12 -5)	=> -3)
  (check (fxmodulo -12 +5)	=> +3)
  (check (fxmodulo -12 -5)	=> -2)

  (check (fxmodulo +12 +7)	=> +5)
  (check (fxmodulo +12 -7)	=> -2)
  (check (fxmodulo -12 +7)	=> +2)
  (check (fxmodulo -12 -7)	=> -5)

  (check (fxmodulo +12 +24)	=> +12)
  (check (fxmodulo +12 -24)	=> -12)
  (check (fxmodulo -12 +24)	=> +12)
  (check (fxmodulo -12 -24)	=> -12)

  (check (fxmodulo +12 +20)	=> +12)
  (check (fxmodulo +12 -20)	=> -8)
  (check (fxmodulo -12 +20)	=> +8)
  (check (fxmodulo -12 -20)	=> -12)

;;; --------------------------------------------------------------------

  (check ($fxmodulo +12 +12)	=> 0)
  (check ($fxmodulo +12 -12)	=> 0)
  (check ($fxmodulo -12 +12)	=> 0)
  (check ($fxmodulo -12 -12)	=> 0)

  (check ($fxmodulo +12 +3)	=> 0)
  (check ($fxmodulo +12 -3)	=> 0)
  (check ($fxmodulo -12 +3)	=> 0)
  (check ($fxmodulo -12 -3)	=> 0)

  (check ($fxmodulo +12 +4)	=> 0)
  (check ($fxmodulo +12 -4)	=> 0)
  (check ($fxmodulo -12 +4)	=> 0)
  (check ($fxmodulo -12 -4)	=> 0)

  (check ($fxmodulo +12 +5)	=> +2)
  (check ($fxmodulo +12 -5)	=> -3)
  (check ($fxmodulo -12 +5)	=> +3)
  (check ($fxmodulo -12 -5)	=> -2)

  (check ($fxmodulo +12 +7)	=> +5)
  (check ($fxmodulo +12 -7)	=> -2)
  (check ($fxmodulo -12 +7)	=> +2)
  (check ($fxmodulo -12 -7)	=> -5)

  (check ($fxmodulo +12 +24)	=> +12)
  (check ($fxmodulo +12 -24)	=> -12)
  (check ($fxmodulo -12 +24)	=> +12)
  (check ($fxmodulo -12 -24)	=> -12)

  (check ($fxmodulo +12 +20)	=> +12)
  (check ($fxmodulo +12 -20)	=> -8)
  (check ($fxmodulo -12 +20)	=> +8)
  (check ($fxmodulo -12 -20)	=> -12)

  #t)


;;;; done

(check-report)

;;; end of file
