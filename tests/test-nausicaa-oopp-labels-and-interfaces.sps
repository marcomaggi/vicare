;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for labels as interfaces
;;;Date: Mon Dec 20, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (rnrs eval)
  (nausicaa language multimethods)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing labels as interfaces\n")


(parametrise ((check-test-name	'example-1))

  (define-generic iface-one (o))
  (define-generic iface-two (o))

  (define-label <iface>
    (methods (one iface-one)
	     (two iface-two)))

  (define-class <alpha>
    (fields a))

  (define-class <beta>
    (fields b))

  (define-method (iface-one (o <alpha>))	'(one . alpha))
  (define-method (iface-two (o <alpha>))	'(two . alpha))
  (define-method (iface-one (o <beta>))		'(one . beta))
  (define-method (iface-two (o <beta>))		'(two . beta))

  (let (((p <iface>) (<alpha> (1)))
	((q <iface>) (<beta>  (2))))
    (check (p one) => '(one . alpha))
    (check (p two) => '(two . alpha))
    (check (q one) => '(one . beta))
    (check (q two) => '(two . beta))
    )

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
