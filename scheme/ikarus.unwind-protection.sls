;;;
;;;Part of: Vicare Scheme
;;;Contents: unwind-protection mechanism
;;;Date: Sun Mar  1, 2015
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
(library (ikarus unwind-protection)
  (export
    unwinding-call/cc
    run-escape-handler-thunks
    run-unwind-protection-cleanup-upon-exit?)
  (import (except (vicare)
		  unwinding-call/cc
		  run-escape-handler-thunks
		  run-unwind-protection-cleanup-upon-exit?)
    ;;FIXME To be removed at the next boot image rotation.
    (only (ikarus conditions)
	  make-non-reinstatable-violation))


(define (run-escape-handler-thunks handlers)
  (for-each (lambda (handler)
	      (call/cc
		  (lambda (escape)
		    (with-exception-handler
			escape
		      handler))))
    handlers))

(define run-unwind-protection-cleanup-upon-exit?
  ;;This is used  in the interaction between the unwind-protection  mechanism and the
  ;;GUARD syntax.
  ;;
  (make-parameter #f))

(define (unwinding-call/cc receiver)
  (fluid-let-syntax ((__who__ (identifier-syntax 'unwinding-call/cc)))
    (let ((inside? #f))
      (dynamic-wind
	  (lambda ()
	    (set! inside? #t))
	  (lambda ()
	    (begin0
		(call/cc
		    (lambda (escape)
		      (receiver (lambda retvals
				  (if inside?
				      (begin
					(run-unwind-protection-cleanup-upon-exit? #t)
					(apply escape retvals))
				    (raise
				     (condition
				      (make-non-reinstatable-violation)
				      (make-who-condition __who__)
				      (make-message-condition "unwinding escape procedure called outside \
                                                               the dynamic extent of its receive function"))))))))
	      (run-unwind-protection-cleanup-upon-exit? #f)))
	  (lambda ()
	    (set! inside? #f))))))


;;;; done

#| end of library |#)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
