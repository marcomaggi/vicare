;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for iterator thunks
;;;Date: Wed Aug 19, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare containers iteration-thunks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: iterator thunks\n")


;;;; helpers

(define (xcons a b)
  (cons b a))


(parametrise ((check-test-name	'list))

  (check
      (let ((iter (make-list-iteration-thunk '())))
        (iteration-thunk-fold xcons '() iter))
    => '())

  (check
      (let ((iter (make-list-iteration-thunk '(0))))
        (iteration-thunk-fold xcons '() iter))
    => '(0))

  (check
      (let ((iter (make-list-iteration-thunk '(0 1 2 3 4))))
        (iteration-thunk-fold xcons '() iter))
    => '(4 3 2 1 0))

  #t)


(parametrise ((check-test-name	'spine))

  (define (kons knil pair)
    (cons (car pair) knil))

  (check
      (let ((iter (make-spine-iteration-thunk '())))
        (iteration-thunk-fold kons '() iter))
    => '())

  (check
      (let ((iter (make-spine-iteration-thunk '(0))))
        (iteration-thunk-fold kons '() iter))
    => '(0))

  (check
      (let ((iter (make-spine-iteration-thunk '(0 1 2 3 4))))
        (iteration-thunk-fold kons '() iter))
    => '(4 3 2 1 0))

  #t)


(parametrise ((check-test-name	'vector))

  (check
      (let ((iter (make-vector-iteration-thunk '#())))
        (iteration-thunk-fold xcons '() iter))
    => '())

  (check
      (let ((iter (make-vector-iteration-thunk '#(0))))
        (iteration-thunk-fold xcons '() iter))
    => '(0))

  (check
      (let ((iter (make-vector-iteration-thunk '#(0 1 2 3 4))))
        (iteration-thunk-fold xcons '() iter))
    => '(4 3 2 1 0))

  #t)


(parametrise ((check-test-name	'string))

  (check
      (let ((iter (make-string-iteration-thunk "")))
        (iteration-thunk-fold xcons '() iter))
    => '())

  (check
      (let ((iter (make-string-iteration-thunk "0")))
        (iteration-thunk-fold xcons '() iter))
    => '(#\0))

  (check
      (let ((iter (make-string-iteration-thunk "01234")))
        (iteration-thunk-fold xcons '() iter))
    => '(#\4 #\3 #\2 #\1 #\0))

  #t)


(parametrise ((check-test-name	'bytevector))

  (check
      (let ((iter (make-bytevector-u8-iteration-thunk '#vu8())))
        (iteration-thunk-fold xcons '() iter))
    => '())

  (check
      (let ((iter (make-bytevector-u8-iteration-thunk '#vu8(0))))
        (iteration-thunk-fold xcons '() iter))
    => '(0))

  (check
      (let ((iter (make-bytevector-u8-iteration-thunk '#vu8(0 1 2 3 4))))
        (iteration-thunk-fold xcons '() iter))
    => '(4 3 2 1 0))

;;; --------------------------------------------------------------------

  (check
      (let ((iter (make-bytevector-s8-iteration-thunk '#vs8())))
        (iteration-thunk-fold xcons '() iter))
    => '())

  (check
      (let ((iter (make-bytevector-s8-iteration-thunk '#vs8(0))))
        (iteration-thunk-fold xcons '() iter))
    => '(0))

  (check
      (let ((iter (make-bytevector-s8-iteration-thunk '#vs8(0 -1 -2 -3 -4))))
        (iteration-thunk-fold xcons '() iter))
    => '(-4 -3 -2 -1 0))

  #t)


(parametrise ((check-test-name	'folding))

  (check
      (iteration-thunk-fold xcons
	'()
	(make-list-iteration-thunk '(0 1 2 3 4)))
    => '(4 3 2 1 0))

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2)
	      (set-cons! ell (+ knil item1 item2))
	      knil)
	  0
	  (make-list-iteration-thunk '(0  1  2  3  4))
	  (make-list-iteration-thunk '(0 10 20 30 40))))
    => '(44 33 22 11 0))

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2 item3)
	      (set-cons! ell (+ knil item1 item2 item3))
	      knil)
	  0
	  (make-list-iteration-thunk '(0   1   2   3   4))
	  (make-list-iteration-thunk '(0  10  20  30  40))
	  (make-list-iteration-thunk '(0 100 200 300 400))))
    => '(444 333 222 111 0))

;;; --------------------------------------------------------------------

  ;;Two iterators, the first finishes first.
  ;;
  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2)
	      (set-cons! ell (+ knil item1 item2))
	      knil)
	  0
	  (make-list-iteration-thunk '(0   1   2))
	  (make-list-iteration-thunk '(0  10  20  30  40))))
    => '(22 11 0))

  ;;Two iterators, the second finishes first.
  ;;
  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2)
	      (set-cons! ell (+ knil item1 item2))
	      knil)
	  0
	  (make-list-iteration-thunk '(0   1   2   3   4))
	  (make-list-iteration-thunk '(0  10  20))))
    => '(22 11 0))


  ;;Three iterators, the first finishes first.
  ;;
  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2 item3)
	      (set-cons! ell (+ knil item1 item2 item3))
	      knil)
	  0
	  (make-list-iteration-thunk '(0   1   2))
	  (make-list-iteration-thunk '(0  10  20  30  40))
	  (make-list-iteration-thunk '(0 100 200 300 400))))
    => '(222 111 0))

  ;;Three iterators, the second finishes first.
  ;;
  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2 item3)
	      (set-cons! ell (+ knil item1 item2 item3))
	      knil)
	  0
	  (make-list-iteration-thunk '(0   1   2   3   4))
	  (make-list-iteration-thunk '(0  10  20))
	  (make-list-iteration-thunk '(0 100 200 300 400))))
    => '(222 111 0))

  ;;Three iterators, the third finishes first.
  ;;
  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-fold
	    (lambda (knil item1 item2 item3)
	      (set-cons! ell (+ knil item1 item2 item3))
	      knil)
	  0
	  (make-list-iteration-thunk '(0   1   2   3   4))
	  (make-list-iteration-thunk '(0  10  20  30  40))
	  (make-list-iteration-thunk '(0 100 200))))
    => '(222 111 0))

  #t)


(parametrise ((check-test-name	'map))

  (check
      (let ((ell '()))
	(iteration-thunk-map
	    (lambda (rv)
	      (set-cons! ell rv)
	      ell)
	  -
	  (make-list-iteration-thunk '())))
    => (sentinel))

  (check
      (let ((ell '()))
	(iteration-thunk-map
	    (lambda (rv)
	      (set-cons! ell rv)
	      ell)
	  -
	  (make-list-iteration-thunk '(0 1 2 3 4))))
    => '(-4 -3 -2 -1 0))

  (check
      (let ((ell '()))
	(iteration-thunk-map
	    (lambda (rv)
	      (set-cons! ell rv)
	      ell)
	  +
	  (make-list-iteration-thunk '(0  1  2  3  4))
	  (make-list-iteration-thunk '(0 10 20 30 40))))
    => '(44 33 22 11 0))

  (check
      (let ((ell '()))
	(iteration-thunk-map
	    (lambda (rv)
	      (set-cons! ell rv)
	      ell)
	  +
	  (make-list-iteration-thunk '(0   1   2   3   4))
	  (make-list-iteration-thunk '(0  10  20  30  40))
	  (make-list-iteration-thunk '(0 100 200 300 400))))
    => '(444 333 222 111 0))

  #t)


(parametrise ((check-test-name	'for-each))

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-for-each
	    (lambda (item)
	      (set-cons! ell (- item)))
	  (make-list-iteration-thunk '())))
    => '())

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-for-each
	    (lambda (item)
	      (set-cons! ell (- item)))
	  (make-list-iteration-thunk '(0 1 2 3 4))))
    => '(-4 -3 -2 -1 0))

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-for-each
	    (lambda (item1 item2)
	      (set-cons! ell (+ item1 item2)))
	  (make-list-iteration-thunk '(0  1  2  3  4))
	  (make-list-iteration-thunk '(0 10 20 30 40))))
    => '(44 33 22 11 0))

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-for-each
	    (lambda (item1 item2 item3)
	      (set-cons! ell (+ item1 item2 item3)))
	  (make-list-iteration-thunk '(0   1   2   3   4))
	  (make-list-iteration-thunk '(0  10  20  30  40))
	  (make-list-iteration-thunk '(0 100 200 300 400))))
    => '(444 333 222 111 0))

  #t)


(parametrise ((check-test-name	'for-all))

  (check
      (iteration-thunk-for-all
	  even?
	(make-list-iteration-thunk '()))
    => #t)

  (check
      (iteration-thunk-for-all
	  even?
	(make-list-iteration-thunk '(2 4 5 6 8)))
    => #f)

  (check
      (iteration-thunk-for-all
	  even?
	(make-list-iteration-thunk '(2 4 6 8)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '()))
    => #t)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '())
	(make-list-iteration-thunk '(+1 +2 +3 +4)))
    => #t)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(+1 +2 -3 +4)))
    => #f)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(+1 +2 +3 +4)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '())
	(make-list-iteration-thunk '(+1 +2 -3 +4))
	(make-list-iteration-thunk '(+1 +2 +3 +4)))
    => #t)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '())
	(make-list-iteration-thunk '(+1 +2 +3 +4)))
    => #t)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(+1 +2 -3 +4))
	(make-list-iteration-thunk '()))
    => #t)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(+1 +2 -3 +4))
	(make-list-iteration-thunk '(+1 +2 +3 +4)))
    => #f)

  (check
      (iteration-thunk-for-all
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(+1 +2 +3 +4)))
    => #t)
  #t)


(parametrise ((check-test-name	'exists))

  (check
      (iteration-thunk-exists
	  even?
	(make-list-iteration-thunk '()))
    => #f)

  (check
      (iteration-thunk-exists
	  even?
	(make-list-iteration-thunk '(1 3 5 7)))
    => #f)

  (check
      (iteration-thunk-exists
	  even?
	(make-list-iteration-thunk '(1 3 4 5 7)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '()))
    => #f)

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(-1 -2 -3 -4)))
    => #f)

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(-1 -2 +3 -4)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '())
	(make-list-iteration-thunk '(-1 -2 -3 -4))
	(make-list-iteration-thunk '( 0  0  0  0)))
    => #f)

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '())
	(make-list-iteration-thunk '( 0  0  0  0)))
    => #f)

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(-1 -2 -3 -4))
	(make-list-iteration-thunk '()))
    => #f)

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(-1 -2 -3 -4))
	(make-list-iteration-thunk '( 0  0  0  0)))
    => #f)

  (check
      (iteration-thunk-exists
	  =
	(make-list-iteration-thunk '(+1 +2 +3 +4))
	(make-list-iteration-thunk '(-1 -2 +3 -4))
	(make-list-iteration-thunk '( 0  0 +3  0)))
    => #t)
  #t)


(parametrise ((check-test-name	'find))

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '()))
    => #f)

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '(1 3 5 7)))
    => #f)

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '())
	(lambda () 'not-found))
    => 'not-found)

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '(1 3 5 7))
	(lambda () 'not-found))
    => 'not-found)

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '(1 3 5 7))
	'not-found)
    => 'not-found)

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '(1 3 4 5 7)))
    => 4)

  (check
      (iteration-thunk-find
	  even?
	(make-list-iteration-thunk '(1 3 4 5 7))
	(lambda () 'not-found))
    => 4)

  #t)


(parametrise ((check-test-name	'filter))

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-filter
	    (lambda (rv)
	      (set-cons! ell rv))
	    even?
	  (make-list-iteration-thunk '())))
    => '())

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-filter
	    (lambda (rv)
	      (set-cons! ell rv))
	    even?
	  (make-list-iteration-thunk '(1 3 5 7))))
    => '())

  (check
      (receive-and-return (ell)
	  '()
	(iteration-thunk-filter
	    (lambda (rv)
	      (set-cons! ell rv))
	    even?
	  (make-list-iteration-thunk '(1 3 4 5 7 8))))
    => '(8 4))

  #t)


(parametrise ((check-test-name	'partition))

  (check
      (receive-and-return (match-ell no-match-ell)
	  (values '() '())
	(iteration-thunk-partition
	    (lambda (match-rv)
	      (set-cons! match-ell match-rv))
	    (lambda (no-match-rv)
	      (set-cons! no-match-ell no-match-rv))
	    even?
	  (make-list-iteration-thunk '())))
    => '() '())

  (check
      (receive-and-return (match-ell no-match-ell)
	  (values '() '())
	(iteration-thunk-partition
	    (lambda (match-rv)
	      (set-cons! match-ell match-rv))
	    (lambda (no-match-rv)
	      (set-cons! no-match-ell no-match-rv))
	    even?
	  (make-list-iteration-thunk '(1 3 5 7))))
    => '() '(7 5 3 1))

  (check
      (receive-and-return (match-ell no-match-ell)
	  (values '() '())
	(iteration-thunk-partition
	    (lambda (match-rv)
	      (set-cons! match-ell match-rv))
	    (lambda (no-match-rv)
	      (set-cons! no-match-ell no-match-rv))
	    even?
	  (make-list-iteration-thunk '(1 3 4 5 7 8))))
    => '(8 4) '(7 5 3 1))

  #t)



;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
