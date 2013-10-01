;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for bitvectors on top of vectors of fixnums
;;;Date: Mon Aug  8, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa containers bitvectors)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: bitvector containers\n")


;;;; helpers

(define (%bits->list (o <bitvector>))
  (let loop ((result '())
	     (i 0))
    (if (= i (o length))
	result
      (loop (cons (o[i]) result) (+ 1 i)))))


(parametrise ((check-test-name	'constructor))

  (check
      (let (((o <bitvector>) (<bitvector> (8))))
	(o vector))
    => '#(#f #f #f #f  #f #f #f #f))
;;;        0  1  2  3   4  5  6  7

  (check
      (let (((o <bitvector>) (<bitvector> (17))))
	(o vector))
    => '#( ;;
	  #f #f #f #f  #f #f #f #f
	  #f #f #f #f  #f #f #f #f
	  #f))

  (check
      (let (((o <bitvector>) (<bitvector> ((+ 16 16 8)))))
	(o vector))
    => '#( ;;
	  #f #f #f #f  #f #f #f #f    #f #f #f #f  #f #f #f #f
	  #f #f #f #f  #f #f #f #f    #f #f #f #f  #f #f #f #f
	  #f #f #f #f  #f #f #f #f))

  #t)


(parametrise ((check-test-name	'setter-getter))

;;; 8 bits

  (check
      (let (((o <bitvector>) (<bitvector> (8))))
	(%bits->list o))
    => '(#f #f #f #f  #f #f #f #f))
;;;       0  1  2  3   4  5  6  7

  (check
      (let (((o <bitvector>) (<bitvector> (8))))
	(set! (o [0]) #t)
	(set! (o [3]) #t)
	(set! (o [5]) #t)
	(%bits->list o))
    => '(#f #f #t #f  #t #f #f #t))
;;;       0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------
;;; 19 bits

  (check
      (let (((o <bitvector>) (<bitvector> (19))))
	(%bits->list o))
    => '( ;;
	 #f #f #f
;;;      18 17 16
	 #f #f #f #f  #f #f #f #f
;;;      15 14 13 12  11 10  9  8
	 #f #f #f #f  #f #f #f #f))
;;;       7  6  5  4   3  2  1  0

  (check
      (let (((o <bitvector>) (<bitvector> (19))))
	(set! (o [0]) #t) (set! (o [3]) #t)
	(set! (o [5]) #t) (set! (o [9]) #t)
	(set! (o [13]) #t) (set! (o [17]) #t)
	(%bits->list o))
    => '( ;;
	 #f #t #f
;;;      18 17 16
	 #f #f #t #f  #f #f #t #f
;;;      15 14 13 12  11 10  9  8
	 #f #f #t #f  #t #f #f #t))
;;;       7  6  5  4   3  2  1  0

  #t)


(parametrise ((check-test-name	'conversion))

;;; 8 bits

  (check
      (let (((o <bitvector>) (<bitvector> (8))))
	(set! (o [0]) #t)
	(set! (o [3]) #t)
	(set! (o [5]) #t)
	(o list))
    => '(#t #f #f #t  #f #t #f #f))
;;;       0  1  2  3   4  5  6  7

  (check
      (let (((o <bitvector>) (<bitvector> (8))))
	(set! (o [0]) #t)
	(set! (o [3]) #t)
	(set! (o [5]) #t)
	(o vector))
    => '#(#t #f #f #t  #f #t #f #f))
;;;        0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------
;;; 19 bits

  (check
      (let (((o <bitvector>) (<bitvector> (19))))
	(set! (o [0]) #t) (set! (o [3]) #t)
	(set! (o [5]) #t) (set! (o [9]) #t)
	(set! (o [13]) #t) (set! (o [17]) #t)
	(o list))
    => '( ;;
	 #t #f #f #t  #f #t #f #f
;;;       0  1  2  3   4  5  6  7
	 #f #t #f #f  #f #t #f #f
;;;       8  9 10 11  12 13 14 15
	 #f #t #f))
;;;      16 17 18

  (check
      (let (((o <bitvector>) (<bitvector> (19))))
	(set! (o [0]) #t) (set! (o [3]) #t)
	(set! (o [5]) #t) (set! (o [9]) #t)
	(set! (o [13]) #t) (set! (o [17]) #t)
	(o vector))
    => '#( ;;
	  #t #f #f #t  #f #t #f #f
;;;        0  1  2  3   4  5  6  7
	  #f #t #f #f  #f #t #f #f
;;;        8  9 10 11  12 13 14 15
	  #f #t #f))
;;;       16 17 18

;;; --------------------------------------------------------------------

  (let ((ell '(#t #f #f #t  #f #t #f #f)))
    (check
	(let (((o <bitvector>) (list->bitvector ell)))
	  (o list))
      => ell))

  (let ((V '#(#t #f #f #t  #f #t #f #f)))
    (check
	(let (((o <bitvector>) (vector->bitvector V)))
	  (o vector))
      => V))

  #t)


(parametrise ((check-test-name	'comparison))

  (check
      (let* ((L			'(#t #f #f #t))
	     ((a <bitvector>)	(list->bitvector L))
	     ((b <bitvector>)	(list->bitvector L)))
	(a = b))
    => #t)

  (check
      (let (((a <bitvector>) (list->bitvector '(#t #f #f #t)))
	    ((b <bitvector>) (list->bitvector '(#t #f #t #t))))
	(a = b))
    => #f)

  #t)


(parametrise ((check-test-name	'bit-ops))

  (check
      (let (((o <bitvector>) (<bitvector> (8))))
	(o toggle! 0)
	(o toggle! 3)
	(o toggle! 5)
	(o list))
    => '(#t #f #f #t  #f #t #f #f))
;;;       0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------

  (check	;not
      (let (((o <bitvector>) (<bitvector> (8))))
	(set! (o [0]) #t)
	(set! (o [3]) #t)
	(set! (o [5]) #t)
	(let (((r <bitvector>) (o not)))
	  (list (o list) (r list))))
    => '((#t #f #f #t  #f #t #f #f)
	 (#f #t #t #f  #t #f #t #t)))
;;;        0  1  2  3   4  5  6  7

  (check	;not!
      (let (((o <bitvector>) (<bitvector> (8))))
	(set! (o [0]) #t)
	(set! (o [3]) #t)
	(set! (o [5]) #t)
	(let ((L (o list)))
	  (o not!)
	  (list L (o list))))
    => '((#t #f #f #t  #f #t #f #f)
	 (#f #t #t #f  #t #f #t #t)))
;;;        0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------

  (check	;and
      (let* (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f)))
	     ((r <bitvector>)	(a and b)))
	(r list))
    => '(#t #f #f #f))

  (check	;and!
      (let (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	    (b		(list->bitvector '(#t #t #f #f))))
	(a and! b)
	(a list))
    => '(#t #f #f #f))

;;; --------------------------------------------------------------------

  (check	;ior
      (let* (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f)))
	     ((r <bitvector>)	(a ior b)))
	(r list))
    => '(#t #t #f #t))

  (check	;ior!
      (let (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	    (b		(list->bitvector '(#t #t #f #f))))
	(a ior! b)
	(a list))
    => '(#t #t #f #t))

;;; --------------------------------------------------------------------

  (check	;xor
      (let* (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f)))
	     ((r <bitvector>)	(a xor b)))
	(r list))
    => '(#f #t #f #t))

  (check	;xor!
      (let* (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f))))
	(a xor! b)
	(a list))
    => '(#f #t #f #t))

;;; --------------------------------------------------------------------
;;; bit count

  (check
      (let (((o <bitvector>)	(list->bitvector '(#t #f #f #t))))
	(o bit-count))
    => 2)

  (check
      (let (((o <bitvector>)	(list->bitvector '(#t #f #f #t  #f #t #t #f))))
	(o bit-count))
    => 4)

  (check
      (let (((o <bitvector>)	(list->bitvector '( ;;
				      #t #f #f #t  #f #t #t #f
				      #t #f #f #t  #f #t #t #f
				      #t #f #f #t  #f #t #t #f))))
	(o bit-count))
    => 12)

;;; --------------------------------------------------------------------
;;; first bit set

  (check
      (let (((o <bitvector>)	(list->bitvector '(#t #f #f #t))))
	(o first-bit-set))
    => 0)

  (check
      (let (((o <bitvector>)	(list->bitvector '(#f #t #f #t))))
	(o first-bit-set))
    => 1)

  (check
      (let (((o <bitvector>)	(list->bitvector '(#f #f #t #t))))
	(o first-bit-set))
    => 2)

  (check
      (let (((o <bitvector>)	(list->bitvector '(#f #f #f #t))))
	(o first-bit-set))
    => 3)

  (check
      (let (((o <bitvector>)	(list->bitvector '(#f #f #f #f  #f #f #f #t))))
	(o first-bit-set))
    => 7)

  (check
      (let (((o <bitvector>)	(list->bitvector '(#f #f #f #f  #f #f #f #f))))
	(o first-bit-set))
    => -1)

  (check
      (let (((o <bitvector>)	(list->bitvector '( ;;
				      #f #f #f #f  #f #f #f #f
				      #f #f #f #f  #f #f #f #f
				      #f #f #f #f  #f #f #t #f))))
	(o first-bit-set))
    => 22)

  #t)


(parametrise ((check-test-name	'other-ops))

  (check	;clone
      (let* (((a <bitvector>)	(list->bitvector '(#t #f #f #t)))
	     ((r <bitvector>)	(a clone)))
	(r list))
    => '(#t #f #f #t))

  (check	;set-all!
      (let (((o <bitvector>) (list->bitvector '(#t #f #f #t))))
	(o set-all!)
	(o list))
    => '(#t #t #t #t))

  (check	;clear-all!
      (let (((o <bitvector>) (list->bitvector '(#t #f #f #t))))
	(o clear-all!)
	(o list))
    => '(#f #f #f #f))

  #t)


;;;; done

(check-report)

;;; end of file
