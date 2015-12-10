;;;
;;;Part of: Vicare Scheme
;;;Contents: test for issue #86
;;;Date: Sun Dec  6, 2015
;;;
;;;Abstract
;;;
;;;	Test for a compiler error when using unsafe primitives.
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks)
  (vicare system $fx)
  (vicare system $strings)
  (vicare system $vectors))

(check-set-mode! 'report)
(check-display "*** testing issue 86, compiler error with some unsafe core primitives\n")


;;;; working code

(check
    (internal-body
      (define ($string-reverse! str start past)
	(do ((i ($fxsub1 past) ($fxsub1 i))
	     (j start          ($fxadd1 j)))
	    ((fx<=? i j)
	     str)
	  (let ((ci ($string-ref str i)))
	    ($string-set! str i ($string-ref str j))
	    ($string-set! str j ci))))

      (let ((S (string-copy "ciao")))
	($string-reverse! S 0 4)
	S))
  => "oaic")

;;; --------------------------------------------------------------------

(check
    (internal-body
      (define ($string-reverse! str start past)
	(do ((i ($fxsub1 past) ($fxsub1 i))
	     (j start          ($fxadd1 j)))
	    (($fx<= i j)
	     str)
	  (let ((ci (string-ref str i)))
	    (string-set! str i (string-ref str j))
	    (string-set! str j ci))))

      (let ((S (string-copy "ciao")))
	($string-reverse! S 0 4)
	S))
  => "oaic")

;;; --------------------------------------------------------------------

(check
    (internal-body
      (define ($vector-reverse! str start past)
	(do ((i ($fxsub1 past) ($fxsub1 i))
	     (j start          ($fxadd1 j)))
	    (($fx<= i j)
	     str)
	  (let ((ci ($vector-ref str i)))
	    ($vector-set! str i ($vector-ref str j))
	    ($vector-set! str j ci))))

      (let ((S (vector-copy '#(c i a o))))
        ($vector-reverse! S 0 4)
        S))
  => '#(o a i c))


;;;; crashing code

(check
    (internal-body
      (define ($string-reverse! str start past)
	(do ((i ($fxsub1 past) ($fxsub1 i))
	     (j start          ($fxadd1 j)))
	    (($fx<= i j)
	     str)
	  (let ((ci ($string-ref str i)))
	    ($string-set! str i ($string-ref str j))
	    ($string-set! str j ci))))


      (let ((S (string-copy "ciao")))
	($string-reverse! S 0 4)
	S))
  => "oaic")


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
