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
;;;Copyright (C) 2011, 2013, 2014, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-containers-bitvectors)
  (options typed-language)
  (import (vicare)
    (vicare containers bitvectors)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: bitvector containers\n")


;;;; helpers

(define (%bits->list {o <bitvector>})
  (let loop ((result '())
	     (i 0))
    (if (= i (o length))
	result
      (loop (cons (.bit-ref o i) result) (+ 1 i)))))


(parametrise ((check-test-name	'constructor))

  (check
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.vector o))
    => '#(#f #f #f #f  #f #f #f #f))
;;;        0  1  2  3   4  5  6  7

  (check
      (let (({o <bitvector>} (new <bitvector> 17)))
	(.vector o))
    => '#( ;;
	  #f #f #f #f  #f #f #f #f
	  #f #f #f #f  #f #f #f #f
	  #f))

  (check
      (let (({o <bitvector>} (new <bitvector> (+ 16 16 8))))
	(.vector o))
    => '#( ;;
	  #f #f #f #f  #f #f #f #f    #f #f #f #f  #f #f #f #f
	  #f #f #f #f  #f #f #f #f    #f #f #f #f  #f #f #f #f
	  #f #f #f #f  #f #f #f #f))

  (check
      (let (({o <bitvector>} (new <bitvector> 1234)))
	(.vector o))
    => (make-vector 1234 #f))

  (check
      (let (({o <bitvector>} (new <bitvector> 1024)))
	(.bit-set! o 100 #t)
	(.bit-set! o 200 #t)
	(.bit-set! o 500 #t)
	(.bit-set! o 1023 #t)
	(.vector o))
    => (receive-and-return (V)
	   (make-vector 1024 #f)
	 (vector-set! V 100 #t)
	 (vector-set! V 200 #t)
	 (vector-set! V 500 #t)
	 (vector-set! V 1023 #t)
	 V))

  (void))


(parametrise ((check-test-name	'setter-getter))

;;; 8 bits

  (check
      (let (({o <bitvector>} (new <bitvector> 8)))
	(%bits->list o))
    => '(#f #f #f #f  #f #f #f #f))
;;;       0  1  2  3   4  5  6  7

  (check
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.bit-set! o 0 #t)
	(.bit-set! o 3 #t)
	(.bit-set! o 5 #t)
	(%bits->list o))
    => '(#f #f #t #f  #t #f #f #t))
;;;       0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------
;;; 19 bits

  (check
      (let (({o <bitvector>} (new <bitvector> 19)))
	(%bits->list o))
    => '( ;;
	 #f #f #f
;;;      18 17 16
	 #f #f #f #f  #f #f #f #f
;;;      15 14 13 12  11 10  9  8
	 #f #f #f #f  #f #f #f #f))
;;;       7  6  5  4   3  2  1  0

  (check
      (let (({o <bitvector>} (<bitvector> (19))))
	(.bit-set! o  0 #t) (.bit-set! o  3 #t)
	(.bit-set! o  5 #t) (.bit-set! o  9 #t)
	(.bit-set! o 13 #t) (.bit-set! o 17 #t)
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
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.bit-set! o 0 #t)
	(.bit-set! o 3 #t)
	(.bit-set! o 5 #t)
	(.list o))
    => '(#t #f #f #t  #f #t #f #f))
;;;       0  1  2  3   4  5  6  7

  (check
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.bit-set! o 0 #t)
	(.bit-set! o 3 #t)
	(.bit-set! o 5 #t)
	(.vector o))
    => '#(#t #f #f #t  #f #t #f #f))
;;;        0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------
;;; 19 bits

  (check
      (let (({o <bitvector>} (new <bitvector> 19)))
	(.bit-set! o  0 #t) (.bit-set! o  3 #t)
	(.bit-set! o  5 #t) (.bit-set! o  9 #t)
	(.bit-set! o 13 #t) (.bit-set! o 17 #t)
	(.list o))
    => '( ;;
	 #t #f #f #t  #f #t #f #f
;;;       0  1  2  3   4  5  6  7
	 #f #t #f #f  #f #t #f #f
;;;       8  9 10 11  12 13 14 15
	 #f #t #f))
;;;      16 17 18

  (check
      (let (({o <bitvector>} (new <bitvector> 19)))
	(.bit-set! o  0 #t) (.bit-set! o  3 #t)
	(.bit-set! o  5 #t) (.bit-set! o  9 #t)
	(.bit-set! o 13 #t) (.bit-set! o 17 #t)
	(.vector o))
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
	(let (({o <bitvector>} (list->bitvector ell)))
	  (.list o))
      => ell))

  (let ((V '#(#t #f #f #t  #f #t #f #f)))
    (check
	(let (({o <bitvector>} (vector->bitvector V)))
	  (.vector o))
      => V))

  #t)


(parametrise ((check-test-name	'comparison))

  (check
      (let* ((L			'(#t #f #f #t))
	     ({a <bitvector>}	(list->bitvector L))
	     ({b <bitvector>}	(list->bitvector L)))
	(equal? a b))
    => #t)

  (check
      (let (({a <bitvector>} (list->bitvector '(#t #f #f #t)))
	    ({b <bitvector>} (list->bitvector '(#t #f #t #t))))
	(equal? a b))
    => #f)

  #t)


(parametrise ((check-test-name	'bit-ops))

  (check
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.toggle! o 0)
	(.toggle! o 3)
	(.toggle! o 5)
	(.list o))
    => '(#t #f #f #t  #f #t #f #f))
;;;       0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------

  (check	;not
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.bit-set! o 0 #t)
	(.bit-set! o 3 #t)
	(.bit-set! o 5 #t)
	(let (({r <bitvector>} (.not o)))
	  (list (.list o) (.list r))))
    => '((#t #f #f #t  #f #t #f #f)
	 (#f #t #t #f  #t #f #t #t)))
;;;        0  1  2  3   4  5  6  7

  (check	;not!
      (let (({o <bitvector>} (new <bitvector> 8)))
	(.bit-set! o 0 #t)
	(.bit-set! o 3 #t)
	(.bit-set! o 5 #t)
	(let ((L (.list o)))
	  (.not! o)
	  (list L (.list o))))
    => '((#t #f #f #t  #f #t #f #f)
	 (#f #t #t #f  #t #f #t #t)))
;;;        0  1  2  3   4  5  6  7

;;; --------------------------------------------------------------------

  (check	;and
      (let* (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f)))
	     ({r <bitvector>}	(.and a b)))
	(.list r))
    => '(#t #f #f #f))

  (check	;and!
      (let (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	    (b			(list->bitvector '(#t #t #f #f))))
	(.and! a b)
	(.list a))
    => '(#t #f #f #f))

;;; --------------------------------------------------------------------

  (check	;ior
      (let* (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f)))
	     ({r <bitvector>}	(.ior a b)))
	(.list r))
    => '(#t #t #f #t))

  (check	;ior!
      (let (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	    (b			(list->bitvector '(#t #t #f #f))))
	(.ior! a b)
	(.list a))
    => '(#t #t #f #t))

;;; --------------------------------------------------------------------

  (check	;xor
      (let* (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f)))
	     ({r <bitvector>}	(.xor a b)))
	(.list r))
    => '(#f #t #f #t))

  (check	;xor!
      (let* (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	     (b			(list->bitvector '(#t #t #f #f))))
	(.xor! a b)
	(.list a))
    => '(#f #t #f #t))

;;; --------------------------------------------------------------------
;;; bit count

  (check
      (let (({o <bitvector>}	(list->bitvector '(#t #f #f #t))))
	(.bit-count o))
    => 2)

  (check
      (let (({o <bitvector>}	(list->bitvector '(#t #f #f #t  #f #t #t #f))))
	(.bit-count o))
    => 4)

  (check
      (let (({o <bitvector>}	(list->bitvector '( ;;
				      #t #f #f #t  #f #t #t #f
				      #t #f #f #t  #f #t #t #f
				      #t #f #f #t  #f #t #t #f))))
	(.bit-count o))
    => 12)

;;; --------------------------------------------------------------------
;;; first bit set

  (check
      (let (({o <bitvector>}	(list->bitvector '(#t #f #f #t))))
	(.first-bit-set o))
    => 0)

  (check
      (let (({o <bitvector>}	(list->bitvector '(#f #t #f #t))))
	(.first-bit-set o))
    => 1)

  (check
      (let (({o <bitvector>}	(list->bitvector '(#f #f #t #t))))
	(.first-bit-set o))
    => 2)

  (check
      (let (({o <bitvector>}	(list->bitvector '(#f #f #f #t))))
	(.first-bit-set o))
    => 3)

  (check
      (let (({o <bitvector>}	(list->bitvector '(#f #f #f #f  #f #f #f #t))))
	(.first-bit-set o))
    => 7)

  (check
      (let (({o <bitvector>}	(list->bitvector '(#f #f #f #f  #f #f #f #f))))
	(.first-bit-set o))
    => -1)

  (check
      (let (({o <bitvector>}	(list->bitvector '( ;;
				      #f #f #f #f  #f #f #f #f
				      #f #f #f #f  #f #f #f #f
				      #f #f #f #f  #f #f #t #f))))
	(.first-bit-set o))
    => 22)

  #t)


(parametrise ((check-test-name	'other-ops))

  (check	;clone
      (let* (({a <bitvector>}	(list->bitvector '(#t #f #f #t)))
	     ({r <bitvector>}	(.clone a)))
	(.list r))
    => '(#t #f #f #t))

  (check	;set-all!
      (let (({o <bitvector>} (list->bitvector '(#t #f #f #t))))
	(.set-all! o)
	(.list o))
    => '(#t #t #t #t))

  (check	;clear-all!
      (let (({o <bitvector>} (list->bitvector '(#t #f #f #t))))
	(.clear-all! o)
	(.list o))
    => '(#f #f #f #f))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
