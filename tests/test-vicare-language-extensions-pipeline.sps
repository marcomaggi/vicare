;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the PIPELINE syntax
;;;Date: Tue Sep  9, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare language-extensions pipeline)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: pipeline syntax\n")


;;;; helpers

(define (times10 v)
  (* 10 v))

(define-syntax catch-syntax-violation
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (E ((syntax-violation? E)
		    #t)
		   (else
		    (debug-print E)
		    #f))
	   (eval (quote ?form)
		 (environment '(rnrs (6))
			      '(vicare language-extensions pipeline))))
       => #t))
    ))


(parametrise ((check-test-name	'core))

  (check
      (pipeline 1)
    => 1)

  (check
      (pipeline
       1.2
       => (x0)
       (sin x0)
       => (x1)
       (cos x1)
       => (x2)
       (tan x2)
       => (x3)
       x3)
    => (tan (cos (sin 1.2))))

  (check
      (pipeline
       (values 1 2)
       => (a0 b0)
       (values (times10 a0) (times10 b0))
       => (a1 b1)
       (values (times10 a1) (times10 b1))
       => (a2 b2)
       (values (times10 a2) (times10 b2))
       => (a3 b3)
       (list a3 b3))
    => '(1000 2000))

  (check
      (pipeline
       (values 1 2 3 4)
       => (a0 b0 . rest0)
       (apply values (times10 a0) (times10 b0) (map times10 rest0))
       => (a1 b1 . rest1)
       (apply values (times10 a1) (times10 b1) (map times10 rest1))
       => (a2 b2 . rest2)
       (apply values (times10 a2) (times10 b2) (map times10 rest2))
       => (a3 b3 . rest3)
       (cons* a3 b3 rest3))
    => '(1000 2000 3000 4000))

  ;;error missing producer expression
  (catch-syntax-violation
   (pipeline
    => (a b)
    (print a)))

   ;;error missing consumer expression
  (catch-syntax-violation
   (pipeline
    (values a b)
    => (a b)))

  #t)


;;;; done

(check-report)

;;; end of file
