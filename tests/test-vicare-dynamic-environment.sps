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

;;; --------------------------------------------------------------------
;;; blocking exceptions

  ;;To  block a  raised exception  we do  as follows.   With this  implementation the
  ;;handler returns the raised object.
  ;;
  (check
      (internal-body
	(define-syntax with-blocked-exceptions
	  (syntax-rules ()
	    ((_ ?thunk)
	     (call/cc
		 (lambda (reinstate-with-blocked-exceptions-continuation)
		   (with-exception-handler
		       reinstate-with-blocked-exceptions-continuation
		     ?thunk))))
	    ))
	(with-blocked-exceptions
	 (lambda ()
	   (raise 123))))
    => 123)

  ;;To  block a  raised exception  we do  as follows.   With this  implementation the
  ;;handler evaluate  an appropriate thunk  to compute the  return values in  case of
  ;;exception.
  ;;
  (check
      (internal-body
	(define-syntax with-blocked-exceptions
	  (syntax-rules ()
	    ((_ ?exception-retvals-maker ?thunk)
	     (call/cc
		 (lambda (reinstate-with-blocked-exceptions-continuation)
		   (with-exception-handler
		       (lambda (E)
			 (call-with-values
			     ?exception-retvals-maker
			   reinstate-with-blocked-exceptions-continuation))
		     ?thunk))))
	    ))
	(with-blocked-exceptions
	 (lambda ()
	   (values 1 2 3))
	 (lambda ()
	   (raise 99))))
    => 1 2 3)

  #t)


(parametrise ((check-test-name	'current-environment))

  (define-syntax with-blocked-exceptions
    (syntax-rules ()
      ((_ ?thunk)
       (call/cc
	   (lambda (reinstate-with-blocked-exceptions-continuation)
	     (with-exception-handler
		 reinstate-with-blocked-exceptions-continuation
	       ?thunk))))
      ))

  (define-syntax with-current-dynamic-environment
    (syntax-rules ()
      ((_ ?thunk)
       (call/cc
	   (lambda (return-thunk-with-packed-environment)
	     ((call/cc
		  (lambda (reinstate-target-environment-continuation)
		    (return-thunk-with-packed-environment
		     (lambda ()
		       (call/cc
			   (lambda (reinstate-thunk-call-continuation)
			     (reinstate-target-environment-continuation
			      (lambda ()
				(call-with-values
				    (lambda ()
				      (with-blocked-exceptions ?thunk))
				  reinstate-thunk-call-continuation)))))))))))))
      ))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
			     (lambda ()
			       (set! counter (+ 1 counter))
			       (add-result (list 'inside-thunk (parm))))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner)
	    calling-thunk-2 (inside-thunk inner))))

  (check	;raising exception
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
			     (lambda ()
			       (set! counter (+ 1 counter))
			       (add-result (list 'inside-thunk (parm)))
			       (add-result 'raise-exception)
			       (raise 123))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner) raise-exception
	    calling-thunk-2 (inside-thunk inner) raise-exception)))

  #f)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
