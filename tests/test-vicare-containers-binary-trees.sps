;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for binary tree skeleton
;;;Date: Fri Aug 14, 2015
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


#!vicare
(import (vicare)
  (vicare containers binary-trees)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: binary tree skeleton\n")


;;;; helpers

(define (make-list-20)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   19)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

(define (make-vector-20)
  ;;Build and return a new vector object holding fixnums from 0 to 99 included.
  ;;
  (do ((vec (make-vector 20))
       (i 0 (fxadd1 i)))
      ((fx=? i 20)
       vec)
    (vector-set! vec i i)))

;;; --------------------------------------------------------------------

(define (make-list-100)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   99)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

(define (make-vector-100)
  ;;Build and return a new vector object holding fixnums from 0 to 99 included.
  ;;
  (do ((vec (make-vector 100))
       (i 0 (fxadd1 i)))
      ((fx=? i 100)
       vec)
    (vector-set! vec i i)))

;;; --------------------------------------------------------------------

;;A list of 5 objects, enstackd in a stack, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enstackd in a stack, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enstackd in a stack, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))

;;A vector of 5 objects, enstackd in a stack, is fully stored in a single buffer.
;;
(define-constant VECTOR-5		(list->vector LIST-5))
(define-constant VECTOR-5-REVERSED	(list->vector LIST-5-REVERSED))

;;A vector of 20 objects, enstackd in a stack, is stored in two buffers.
;;
(define-constant VECTOR-20		(list->vector LIST-20))
(define-constant VECTOR-20-REVERSED	(list->vector LIST-20-REVERSED))

;;A vector of 100 objects, enstackd in a stack, is stored in multiple buffers.
;;
(define-constant VECTOR-100		(list->vector LIST-100))
(define-constant VECTOR-100-REVERSED	(list->vector LIST-100-REVERSED))


(parametrise ((check-test-name	'making))

  (check
      (binary-node? (make-binary-node 123 #f #f))
    => #t)

  (check
      (let* ((lx (make-binary-node 0))
	     (rx (make-binary-node 2))
	     (rt (make-binary-node 1 lx rx)))
	(values (binary-node-object (binary-node-left  rt))
		(binary-node-object rt)
		(binary-node-object (binary-node-right rt))))
    => 0 1 2)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'binary-tree-fold-in-order	'scheme-indent-function 1)
;; End:
