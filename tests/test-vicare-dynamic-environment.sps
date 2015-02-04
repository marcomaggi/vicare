;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the dynamic environment
;;;Date: Wed Feb  4, 2015
;;;
;;;Abstract
;;;
;;;
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: dynamic environment\n")


;;;; helpers

(define parm
  (make-parameter #f))


(parametrise ((check-test-name	'exception-handlers))

  ;;The handler  is called in the  dynamic environment of  the thunk, so that  it can
  ;;access the dynamic environment that contributed to cause the exception.
  ;;
  (check
      (parametrise ((parm 'outer-parm))
	(with-exception-handler
	    (lambda (E)
	      (parm))
	  (lambda ()
	    (parametrise ((parm 'inner-parm))
	      (raise-continuable 2)))))
    => 'inner-parm)

  ;;To block a raised exception we do as  follows.  In the handler we must avoid code
  ;;that may raise a further exception.
  ;;
  (check
      (let-syntax
	  ((with-blocked-exceptions (syntax-rules ()
				      ((_ ?thunk)
				       (call/cc
					   (lambda (reinstate-with-blocked-exceptions-continuation)
					     (with-exception-handler
						 (lambda (E)
						   (reinstate-with-blocked-exceptions-continuation #f))
					       ?thunk))))
				      )))
	(with-blocked-exceptions
	 (lambda ()
	   (raise 123))))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
