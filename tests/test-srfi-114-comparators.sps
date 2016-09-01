;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 114
;;;Date: Sun Mar  8, 2015
;;;
;;;Abstract
;;;
;;;	There is no test suite in  the reference implementation.  So this program has
;;;	been written specifically for Vicare.
;;;
;;;     NOTE It is  unlikely that I have gotten everything  right.  (Marco Maggi; Sun
;;;     Mar 8, 2015)
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
(program (test-srfi-114-comparators)
  (import (vicare)
    (vicare system structs)
    (vicare checks)
    (srfi :114))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: SRFI 114, comparators\n")


(parametrise ((check-test-name	'predicates))

  (check
      (comparator? 123)
    => #f)

  (check-for-true (comparator? default-comparator))

  (check-for-true (comparator? boolean-comparator))
  (check-for-true (comparator? char-comparator))
  (check-for-true (comparator? char-ci-comparator))
  (check-for-true (comparator? string-comparator))
  (check-for-true (comparator? string-ci-comparator))
  (check-for-true (comparator? symbol-comparator))

  (check-for-true (comparator? fixnum-comparator))
  (check-for-true (comparator? exact-integer-comparator))
  (check-for-true (comparator? integer-comparator))
  (check-for-true (comparator? rational-comparator))
  (check-for-true (comparator? real-comparator))
  (check-for-true (comparator? complex-comparator))
  (check-for-true (comparator? number-comparator))
  (check-for-true (comparator? pair-comparator))
  (check-for-true (comparator? list-comparator))
  (check-for-true (comparator? vector-comparator))
  (check-for-true (comparator? bytevector-comparator))

;;; --------------------------------------------------------------------

  (check-for-procedure-argument-violation
      (comparator-comparison-procedure? 123)
    => '(comparator-comparison-procedure? (123)))

  (check-for-true (comparator-comparison-procedure? default-comparator))

  (check-for-true (comparator-comparison-procedure? boolean-comparator))
  (check-for-true (comparator-comparison-procedure? char-comparator))
  (check-for-true (comparator-comparison-procedure? char-ci-comparator))
  (check-for-true (comparator-comparison-procedure? string-comparator))
  (check-for-true (comparator-comparison-procedure? string-ci-comparator))
  (check-for-true (comparator-comparison-procedure? symbol-comparator))

  (check-for-true (comparator-comparison-procedure? fixnum-comparator))
  (check-for-true (comparator-comparison-procedure? exact-integer-comparator))
  (check-for-true (comparator-comparison-procedure? integer-comparator))
  (check-for-true (comparator-comparison-procedure? rational-comparator))
  (check-for-true (comparator-comparison-procedure? real-comparator))
  (check-for-true (comparator-comparison-procedure? complex-comparator))
  (check-for-true (comparator-comparison-procedure? number-comparator))
  (check-for-true (comparator-comparison-procedure? pair-comparator))
  (check-for-true (comparator-comparison-procedure? list-comparator))
  (check-for-true (comparator-comparison-procedure? vector-comparator))
  (check-for-true (comparator-comparison-procedure? bytevector-comparator))

;;; --------------------------------------------------------------------

  (check-for-procedure-argument-violation
      (comparator-hash-function? 123)
    => '(comparator-hash-function? (123)))

  (check-for-true (comparator-hash-function? default-comparator))

  (check-for-true (comparator-hash-function? boolean-comparator))
  (check-for-true (comparator-hash-function? char-comparator))
  (check-for-true (comparator-hash-function? char-ci-comparator))
  (check-for-true (comparator-hash-function? string-comparator))
  (check-for-true (comparator-hash-function? string-ci-comparator))
  (check-for-true (comparator-hash-function? symbol-comparator))

  (check-for-true (comparator-hash-function? fixnum-comparator))
  (check-for-true (comparator-hash-function? exact-integer-comparator))
  (check-for-true (comparator-hash-function? integer-comparator))
  (check-for-true (comparator-hash-function? rational-comparator))
  (check-for-true (comparator-hash-function? real-comparator))
  (check-for-true (comparator-hash-function? complex-comparator))
  (check-for-true (comparator-hash-function? number-comparator))
  (check-for-true (comparator-hash-function? pair-comparator))
  (check-for-true (comparator-hash-function? list-comparator))
  (check-for-true (comparator-hash-function? vector-comparator))
  (check-for-true (comparator-hash-function? bytevector-comparator))

  #t)


(parametrise ((check-test-name	'standard-with-accessors))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?C ?a ?b)
       (begin
	 (check-for-true  ((comparator-type-test-procedure ?C) ?a))
	 (check-for-true  ((comparator-type-test-procedure ?C) ?b))
	 (check-for-false ((comparator-type-test-procedure ?C) (void)))
	 (check-for-true  ((comparator-equality-predicate ?C) ?a ?a))
	 (check-for-false ((comparator-equality-predicate ?C) ?a ?b))
	 (check
	     ((comparator-comparison-procedure ?C) ?a ?a)
	   => 0)
	 (check
	     ((comparator-comparison-procedure ?C) ?a ?b)
	   => -1)
	 (check
	     ((comparator-comparison-procedure ?C) ?b ?a)
	   => +1)
	 (check-for-true
	  (non-negative-exact-integer? ((comparator-hash-function ?C) ?a)))
	 (check-for-true
	  (non-negative-exact-integer? ((comparator-hash-function ?C) ?b)))
	 ))
      ))

;;; --------------------------------------------------------------------

  (doit boolean-comparator #f #t)
  (doit char-comparator #\a #\b)
  (doit char-ci-comparator #\A #\b)
  (doit string-comparator "a" "b")
  (doit string-ci-comparator "A" "b")
  (doit symbol-comparator 'a 'b)

  (doit fixnum-comparator 1 2)
  (doit exact-integer-comparator 1 2)
  (doit exact-integer-comparator (greatest-negative-bignum) (least-positive-bignum))
  (doit integer-comparator 1 2.0)
  (doit rational-comparator 1/3 2/3)
  (doit real-comparator 1.1 2.2)
  (doit complex-comparator 1.1+3.3i 2.2+4.4i)
  (doit number-comparator 1 2.0+3i)
  (doit pair-comparator '(1 . 2) '(3 . 4))
  (doit list-comparator '(1 2) '(3 4))
  (doit vector-comparator '#(1 2) '#(3 4))
  (doit bytevector-comparator '#vu8(1 2) '#vu8(3 4))

;;; --------------------------------------------------------------------
;;; pair comparison

  (let ((cmp (comparator-comparison-procedure pair-comparator)))
    (check (cmp '(1 . 2) '(1 . 2)) =>  0)
    (check (cmp '(1 . 2) '(1 . 3)) => -1) ;2 < 3
    (check (cmp '(1 . 4) '(1 . 3)) => +1) ;4 > 3
    (check (cmp '(1 . 0) '(2 . 0)) => -1)
    (check (cmp '(3 . 0) '(2 . 0)) => +1)
    #f)

;;; --------------------------------------------------------------------
;;; list comparison

  (let ((cmp (comparator-comparison-procedure list-comparator)))
    (check (cmp '(1 2) '(1 2)) =>  0)
    (check (cmp '(1 2) '(1 3)) => -1) ;2 < 3
    (check (cmp '(1 4) '(1 3)) => +1) ;4 > 3
    (check (cmp '(1 0) '(2 0)) => -1)
    (check (cmp '(3 0) '(2 0)) => +1)

    (check (cmp '() '())	=> 0)
    (check (cmp '() '(1))	=> -1)
    (check (cmp '(1) '())	=> +1)

    ;;If first items are equal: compare the CADRs.  Here one of the CADRs is null.
    (check (cmp '(1 2) '(1))	=> +1)
    (check (cmp '(1)   '(1 2))	=> -1)

    ;;Lists  of  different length,  but  it  does not  matter  because  the CARs  are
    ;;non-equal.
    (check (cmp '(1 2) '(2))	=> -1)
    (check (cmp '(2)   '(1 2))	=> +1)
    #f)

;;; --------------------------------------------------------------------
;;; vector comparison

  (let ((cmp (comparator-comparison-procedure vector-comparator)))
    (check (cmp '#()  '#())	=>  0)
    (check (cmp '#(1) '#())	=>  +1)
    (check (cmp '#()  '#(1))	=>  -1)

    (check (cmp '#(1 2) '#(1 2)) =>  0)
    (check (cmp '#(1 2) '#(1 3)) => -1) ;2 < 3
    (check (cmp '#(1 4) '#(1 3)) => +1) ;4 > 3
    (check (cmp '#(1 0) '#(2 0)) => -1)
    (check (cmp '#(3 0) '#(2 0)) => +1)
    #f)

;;; --------------------------------------------------------------------
;;; bytevector comparison

  (let ((cmp (comparator-comparison-procedure bytevector-comparator)))
    (check (cmp '#vu8()  '#vu8())	=>  0)
    (check (cmp '#vu8(1) '#vu8())	=>  +1)
    (check (cmp '#vu8()  '#vu8(1))	=>  -1)

    (check (cmp '#vu8(1 2) '#vu8(1 2)) =>  0)
    (check (cmp '#vu8(1 2) '#vu8(1 3)) => -1) ;2 < 3
    (check (cmp '#vu8(1 4) '#vu8(1 3)) => +1) ;4 > 3
    (check (cmp '#vu8(1 0) '#vu8(2 0)) => -1)
    (check (cmp '#vu8(3 0) '#vu8(2 0)) => +1)
    #f)

  #t)


(parametrise ((check-test-name	'standard-with-applicators))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?C ?a ?b)
       (begin
	 (check-for-true  (comparator-test-type ?C ?a))
	 (check-for-true  (comparator-test-type ?C ?b))
	 (check-for-false (comparator-test-type ?C (void)))
	 (check-for-true  (comparator-check-type ?C ?a))
	 (check-for-true  (comparator-check-type ?C ?b))
	 (check-for-true
	  (try
	      (comparator-check-type ?C (void))
	    (catch E
	      ((&comparator-type-error)
	       #t)
	      (else #f))))
	 (check-for-true  (comparator-equal? ?C ?a ?a))
	 (check-for-false (comparator-equal? ?C ?a ?b))
	 (check
	     (comparator-compare ?C ?a ?a)
	   => 0)
	 (check
	     (comparator-compare ?C ?a ?b)
	   => -1)
	 (check
	     (comparator-compare ?C ?b ?a)
	   => +1)
	 (check-for-true
	  (non-negative-exact-integer? (comparator-hash ?C ?a)))
	 (check-for-true
	  (non-negative-exact-integer? (comparator-hash ?C ?b)))
	 ))
      ))

;;; --------------------------------------------------------------------

  (doit boolean-comparator #f #t)
  (doit char-comparator #\a #\b)
  (doit char-ci-comparator #\A #\b)
  (doit string-comparator "a" "b")
  (doit string-ci-comparator "A" "b")
  (doit symbol-comparator 'a 'b)

  (doit fixnum-comparator 1 2)
  (doit exact-integer-comparator 1 2)
  (doit exact-integer-comparator (greatest-negative-bignum) (least-positive-bignum))
  (doit integer-comparator 1 2.0)
  (doit rational-comparator 1/3 2/3)
  (doit real-comparator 1.1 2.2)
  (doit complex-comparator 1.1+3.3i 2.2+4.4i)
  (doit number-comparator 1 2.0+3i)

  (doit pair-comparator '(1 . 2) '(3 . 4))
  (doit list-comparator '(1 2) '(3 4))
  (doit vector-comparator '#(1 2) '#(3 4))
  (doit bytevector-comparator '#vu8(1 2) '#vu8(3 4))

;;; --------------------------------------------------------------------
;;; pair comparison

  (let ((cmp (comparator-comparison-procedure pair-comparator)))
    (check (cmp '(1 . 2) '(1 . 2)) =>  0)
    (check (cmp '(1 . 2) '(1 . 3)) => -1) ;2 < 3
    (check (cmp '(1 . 4) '(1 . 3)) => +1) ;4 > 3
    (check (cmp '(1 . 0) '(2 . 0)) => -1)
    (check (cmp '(3 . 0) '(2 . 0)) => +1)
    #f)

;;; --------------------------------------------------------------------
;;; list comparison

  (let ((cmp (comparator-comparison-procedure list-comparator)))
    (check (cmp '(1 2) '(1 2)) =>  0)
    (check (cmp '(1 2) '(1 3)) => -1) ;2 < 3
    (check (cmp '(1 4) '(1 3)) => +1) ;4 > 3
    (check (cmp '(1 0) '(2 0)) => -1)
    (check (cmp '(3 0) '(2 0)) => +1)

    (check (cmp '() '())	=> 0)
    (check (cmp '() '(1))	=> -1)
    (check (cmp '(1) '())	=> +1)

    ;;If first items are equal: compare the CADRs.  Here one of the CADRs is null.
    (check (cmp '(1 2) '(1))	=> +1)
    (check (cmp '(1)   '(1 2))	=> -1)

    ;;Lists  of  different length,  but  it  does not  matter  because  the CARs  are
    ;;non-equal.
    (check (cmp '(1 2) '(2))	=> -1)
    (check (cmp '(2)   '(1 2))	=> +1)
    #f)

;;; --------------------------------------------------------------------
;;; vector comparison

  (let ((cmp (comparator-comparison-procedure vector-comparator)))
    (check (cmp '#()  '#())	=>  0)
    (check (cmp '#(1) '#())	=>  +1)
    (check (cmp '#()  '#(1))	=>  -1)

    (check (cmp '#(1 2) '#(1 2)) =>  0)
    (check (cmp '#(1 2) '#(1 3)) => -1) ;2 < 3
    (check (cmp '#(1 4) '#(1 3)) => +1) ;4 > 3
    (check (cmp '#(1 0) '#(2 0)) => -1)
    (check (cmp '#(3 0) '#(2 0)) => +1)
    #f)

;;; --------------------------------------------------------------------
;;; bytevector comparison

  (let ((cmp (comparator-comparison-procedure bytevector-comparator)))
    (check (cmp '#vu8()  '#vu8())	=>  0)
    (check (cmp '#vu8(1) '#vu8())	=>  +1)
    (check (cmp '#vu8()  '#vu8(1))	=>  -1)

    (check (cmp '#vu8(1 2) '#vu8(1 2)) =>  0)
    (check (cmp '#vu8(1 2) '#vu8(1 3)) => -1) ;2 < 3
    (check (cmp '#vu8(1 4) '#vu8(1 3)) => +1) ;4 > 3
    (check (cmp '#vu8(1 0) '#vu8(2 0)) => -1)
    (check (cmp '#vu8(3 0) '#vu8(2 0)) => +1)
    #f)

  #t)


(parametrise ((check-test-name	'default-with-accessors))

  (define C default-comparator)

;;; --------------------------------------------------------------------
;;; type test

  (check-for-true  ((comparator-type-test-procedure C) #t))
  (check-for-true  ((comparator-type-test-procedure C) #f))
  (check-for-true  ((comparator-type-test-procedure C) '()))
  (check-for-true  ((comparator-type-test-procedure C) '(1 . 2)))
  (check-for-true  ((comparator-type-test-procedure C) '(1 2)))
  (check-for-true  ((comparator-type-test-procedure C) '#(1 2)))
  (check-for-true  ((comparator-type-test-procedure C) '#vu8(1 2)))
  (check-for-true  ((comparator-type-test-procedure C) "12"))

;;; --------------------------------------------------------------------
;;; equality

  (check-for-false
   ((comparator-equality-predicate C) '() '(1 . 2)))

;;; --------------------------------------------------------------------
;;; comparison

  (check ((comparator-comparison-procedure C) #t #t)		=> 0)
  (check ((comparator-comparison-procedure C) #f #t)		=> -1)
  (check ((comparator-comparison-procedure C) #t #f)		=> +1)

;;; --------------------------------------------------------------------
;;; hash functions

  (check-for-true
   (non-negative-exact-integer? ((comparator-hash-function C) #f)))
  (check-for-true
   (non-negative-exact-integer? ((comparator-hash-function C) #t)))

  #t)


(parametrise ((check-test-name	'default-with-applicators))

  (define C default-comparator)

;;; --------------------------------------------------------------------
;;; type test

  (check-for-true  (comparator-test-type C #t))
  (check-for-true  (comparator-test-type C #f))
  (check-for-true  (comparator-test-type C '()))
  (check-for-true  (comparator-test-type C '(1 . 2)))
  (check-for-true  (comparator-test-type C '(1 2)))
  (check-for-true  (comparator-test-type C '#(1 2)))
  (check-for-true  (comparator-test-type C '#vu8(1 2)))
  (check-for-true  (comparator-test-type C #\a))
  (check-for-true  (comparator-test-type C "12"))

  (check-for-true  (comparator-test-type C 12))
  (check-for-true  (comparator-test-type C 1.2))
  (check-for-true  (comparator-test-type C 1/2))
  (check-for-true  (comparator-test-type C 1+2i))
  (check-for-true  (comparator-test-type C 1.0+2i))
  (check-for-true  (comparator-test-type C 1+2.0i))
  (check-for-true  (comparator-test-type C 1.0+2.0i))
  (check-for-true  (comparator-test-type C +nan.0))
  (check-for-true  (comparator-test-type C +inf.0))
  (check-for-true  (comparator-test-type C -inf.0))

  (check-for-true  (comparator-test-type C (least-fixnum)))
  (check-for-true  (comparator-test-type C (greatest-fixnum)))
  (check-for-true  (comparator-test-type C (least-positive-bignum)))
  (check-for-true  (comparator-test-type C (greatest-negative-bignum)))

  (check-for-true  (comparator-test-type C (void)))
  (check-for-true  (comparator-test-type C (eof-object)))
  (check-for-true  (comparator-test-type C (would-block-object)))

;;; --------------------------------------------------------------------
;;; type check

  (check-for-true  (comparator-check-type C #t))
  (check-for-true  (comparator-check-type C #f))
  (check-for-true  (comparator-check-type C '()))
  (check-for-true  (comparator-check-type C '(1 . 2)))
  (check-for-true  (comparator-check-type C '(1 2)))
  (check-for-true  (comparator-check-type C '#(1 2)))
  (check-for-true  (comparator-check-type C '#vu8(1 2)))
  (check-for-true  (comparator-check-type C #\a))
  (check-for-true  (comparator-check-type C "12"))

  (check-for-true  (comparator-check-type C 12))
  (check-for-true  (comparator-check-type C 1.2))
  (check-for-true  (comparator-check-type C 1/2))
  (check-for-true  (comparator-check-type C 1+2i))
  (check-for-true  (comparator-check-type C 1.0+2i))
  (check-for-true  (comparator-check-type C 1+2.0i))
  (check-for-true  (comparator-check-type C 1.0+2.0i))
  (check-for-true  (comparator-check-type C +nan.0))
  (check-for-true  (comparator-check-type C +inf.0))
  (check-for-true  (comparator-check-type C -inf.0))

  (check-for-true  (comparator-check-type C (least-fixnum)))
  (check-for-true  (comparator-check-type C (greatest-fixnum)))
  (check-for-true  (comparator-check-type C (least-positive-bignum)))
  (check-for-true  (comparator-check-type C (greatest-negative-bignum)))

  (check-for-true  (comparator-check-type C (void)))
  (check-for-true  (comparator-check-type C (eof-object)))
  (check-for-true  (comparator-check-type C (would-block-object)))

;;; --------------------------------------------------------------------
;;; equality

  (check-for-true  (comparator-equal? C '() '()))

  (check-for-false (comparator-equal? C '() #t))
  (check-for-false (comparator-equal? C '() #f))
  (check-for-false (comparator-equal? C '() '(1 . 2)))
  (check-for-false (comparator-equal? C '() '(1 2)))
  (check-for-false (comparator-equal? C '() '#(1 2)))
  (check-for-false (comparator-equal? C '() '#vu8(1 2)))
  (check-for-false (comparator-equal? C '() #\a))
  (check-for-false (comparator-equal? C '() "12"))

  (check-for-false (comparator-equal? C '() 12))
  (check-for-false (comparator-equal? C '() 1.2))
  (check-for-false (comparator-equal? C '() 1/2))
  (check-for-false (comparator-equal? C '() 1+2i))
  (check-for-false (comparator-equal? C '() 1.0+2i))
  (check-for-false (comparator-equal? C '() 1+2.0i))
  (check-for-false (comparator-equal? C '() 1.0+2.0i))
  (check-for-false (comparator-equal? C '() +nan.0))
  (check-for-false (comparator-equal? C '() +inf.0))
  (check-for-false (comparator-equal? C '() -inf.0))

  (check-for-false (comparator-equal? C '() (least-fixnum)))
  (check-for-false (comparator-equal? C '() (greatest-fixnum)))
  (check-for-false (comparator-equal? C '() (least-positive-bignum)))
  (check-for-false (comparator-equal? C '() (greatest-negative-bignum)))

  (check-for-false (comparator-equal? C '() (void)))
  (check-for-false (comparator-equal? C '() (eof-object)))
  (check-for-false (comparator-equal? C '() (would-block-object)))

  (check-for-true (comparator-equal? C (void) (void)))
  (check-for-true (comparator-equal? C (eof-object) (eof-object)))
  (check-for-true (comparator-equal? C (would-block-object) (would-block-object)))

;;; --------------------------------------------------------------------
;;; comparison

  (check (comparator-compare C '()	'())		=> 0)
  (check (comparator-compare C '()	'(1 . 2))	=> -1)
  (check (comparator-compare C '(1 . 2)	'())		=> +1)

  (check (comparator-compare C '()	'())		=> 0)
  (check (comparator-compare C '()	'(1 2))		=> -1)
  (check (comparator-compare C '(1 2)	'())		=> +1)

  (check (comparator-compare C '(1 2)	'(1 . 2))	=> -1)
  (check (comparator-compare C '(1 . 2)	'(1 2))		=> +1)

  ;;Proper lists precede improper lists.
  (check (comparator-compare C '(1 2 3)	  '(1 2 . 3))	=> -1)
  (check (comparator-compare C '(1 2 . 3) '(1 2 3))	=> +1)

  (check (comparator-compare C '(1 . 2)	#t)		=> -1)
  (check (comparator-compare C '(1 . 2)	#f)		=> -1)
  (check (comparator-compare C #t	'(1 . 2))	=> +1)
  (check (comparator-compare C #f	'(1 . 2))	=> +1)

  (check (comparator-compare C #t	#\a)		=> -1)
  (check (comparator-compare C #\a	#t)		=> +1)

  (check (comparator-compare C #\a	"A")		=> -1)
  (check (comparator-compare C "A"	#\a)		=> +1)

  (check (comparator-compare C "A"	'b)		=> -1)
  (check (comparator-compare C 'b	"A")		=> +1)

  (check (comparator-compare C 'b	1)		=> -1)
  (check (comparator-compare C 1	'b)		=> +1)

  (check (comparator-compare C 1	'#(1))		=> -1)
  (check (comparator-compare C '#(1)	1)		=> +1)

  (check (comparator-compare C '#(1)	'#vu8(1))	=> -1)
  (check (comparator-compare C '#vu8(1)	'#(1))		=> +1)

  (check (comparator-compare C '#vu8(1)	(void))		=> -1)
  (check (comparator-compare C (void)	'#vu8(1))	=> +1)

  (check (comparator-compare C (void)		(eof-object))	=> -1)
  (check (comparator-compare C (eof-object)	(void))		=> +1)

  (check (comparator-compare C (eof-object)		(would-block-object))	=> -1)
  (check (comparator-compare C (would-block-object)	(eof-object))		=> +1)

;;; --------------------------------------------------------------------
;;; hash function

  (check-for-true (non-negative-exact-integer? (comparator-hash C #t)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C #f)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C '())))
  (check-for-true (non-negative-exact-integer? (comparator-hash C '(1 . 2))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C '(1 2))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C '#(1 2))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C '#vu8(1 2))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C #\a)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C "ab")))

  (check-for-true (non-negative-exact-integer? (comparator-hash C 12)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C 1.2)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C 1/2)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C 1+2i)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C 1.0+2i)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C 1+2.0i)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C 1.0+2.0i)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C +nan.0)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C +inf.0)))
  (check-for-true (non-negative-exact-integer? (comparator-hash C -inf.0)))

  (check-for-true (non-negative-exact-integer? (comparator-hash C (least-fixnum))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C (greatest-fixnum))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C (least-positive-bignum))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C (greatest-negative-bignum))))

  (check-for-true (non-negative-exact-integer? (comparator-hash C (void))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C (eof-object))))
  (check-for-true (non-negative-exact-integer? (comparator-hash C (would-block-object))))

  #t)


(parametrise ((check-test-name	'default-with-custom))

  (define C default-comparator)

  (define-struct a-struct
    (a b c))

  (define-record-type a-record
    (fields a b c))

  (define (a-struct-comparison a b)
    (cond ((struct=? a b)
	   0)
	  ((< (a-struct-a a)
	      (a-struct-a b))
	   -1)
	  (else +1)))

  (define (a-record-comparison a b)
    (cond ((record=? a b)
	   0)
	  ((< (a-record-a a)
	      (a-record-a b))
	   -1)
	  (else +1)))

  (define a-struct-comparator
    (make-comparator a-struct?
		     struct=?
		     a-struct-comparison
		     struct-hash))

  (define a-record-comparator
    (make-comparator a-record?
		     record=?
		     a-record-comparison
		     record-hash))

  (comparator-register-default! a-struct-comparator)
  (comparator-register-default! a-record-comparator)

;;; --------------------------------------------------------------------
;;; type test

  (check-for-true  (comparator-test-type C (make-a-struct 1 2 3)))
  (check-for-true  (comparator-test-type C (make-a-record 1 2 3)))

;;; --------------------------------------------------------------------
;;; type check

  (check-for-true  (comparator-check-type C (make-a-struct 1 2 3)))
  (check-for-true  (comparator-check-type C (make-a-record 1 2 3)))

;;; --------------------------------------------------------------------
;;; equality

  (let ((P (make-a-struct 1 2 3))
	(Q (make-a-struct 1 2 3))
	(R (make-a-struct 1 2 9)))
    (check-for-true  (comparator-equal? C P P))
    (check-for-true  (comparator-equal? C P Q))
    (check-for-false (comparator-equal? C P R)))

  (let ((P (make-a-record 1 2 3))
	(Q (make-a-record 1 2 3))
	(R (make-a-record 1 2 9)))
    (check-for-true  (comparator-equal? C P P))
    (check-for-true  (comparator-equal? C P Q))
    (check-for-false (comparator-equal? C P R)))

;;; --------------------------------------------------------------------
;;; comparison

  (let ((P (make-a-struct +1 2 3))
	(Q (make-a-struct -1 2 3))
	(R (make-a-struct +2 2 9)))
    (check (comparator-compare C P P)	=> 0)
    (check (comparator-compare C P Q)	=> +1)
    (check (comparator-compare C P R)	=> -1))

  (let ((P (make-a-record +1 2 3))
	(Q (make-a-record -1 2 3))
	(R (make-a-record +2 2 9)))
    (check (comparator-compare C P P)	=> 0)
    (check (comparator-compare C P Q)	=> +1)
    (check (comparator-compare C P R)	=> -1))

;;; --------------------------------------------------------------------
;;; hash function

  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (make-a-struct 1 2 3))))

  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (make-a-record 1 2 3))))

  (when #f
    (debug-print (comparator-hash C (make-a-struct 99 2 3))
		 (comparator-hash C (make-a-record 99 2 3))))

  #t)


(parametrise ((check-test-name	'inexact-real-comparator))

  (internal-body

    (define-constant C
      (let ((epsilon		0.1)
	    (rounding		'round)
	    (nan-handling	'error))
	(make-inexact-real-comparator epsilon rounding nan-handling)))

    ;; type test
    (check-for-true  (comparator-test-type C 1.2))
    (check-for-true  (comparator-test-type C +inf.0))
    (check-for-true  (comparator-test-type C -inf.0))
    (check-for-true  (comparator-test-type C -nan.0))
    (check-for-false (comparator-test-type C "ciao"))
    (check-for-false (comparator-test-type C 1+2i))

    ;; type check
    (check-for-true  (comparator-check-type C 1.2))
    (check-for-true
     (try
	 (comparator-check-type C (void))
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E))))

    ;; comparison
    (check (comparator-compare C 1.2 1.2)	=> 0)
    (check (comparator-compare C 1.0 2.0)	=> -1)
    (check (comparator-compare C 2.0 1.0)	=> +1)

    (check (comparator-compare C +inf.0 +inf.0)	=> 0)
    (check (comparator-compare C -inf.0 -inf.0)	=> 0)
    (check (comparator-compare C -inf.0 +inf.0)	=> -1)
    (check (comparator-compare C +inf.0 -inf.0)	=> +1)

    (check (comparator-compare C +nan.0 +nan.0)	=> 0)
    (check-for-true
     (try
	 (comparator-compare C +nan.0 1.0)
       (catch E
	 ((&comparator-nan-comparison-error)
	  #t)
	 (else E))))
    (check-for-true
     (try
	 (comparator-compare C 1.0 +nan.0)
       (catch E
	 ((&comparator-nan-comparison-error)
	  #t)
	 (else E))))

    ;; comparison with rounding
    (check (comparator-compare C 1.04 1.0)	=> 0)
    (check (comparator-compare C 0.96 1.0)	=> 0)

    ;; hash
    (check-for-true
     (= (comparator-hash C 1.0) (comparator-hash C 1.04)))
    (check-for-true
     (= (comparator-hash C 1.0) (comparator-hash C 0.96)))
    (check-for-true
     (non-negative-exact-integer? (comparator-hash C +inf.0)))
    (check-for-true
     (non-negative-exact-integer? (comparator-hash C -inf.0)))
    (check-for-true
     (non-negative-exact-integer? (comparator-hash C +nan.0)))

    #f)

;;; --------------------------------------------------------------------

  (internal-body

    (define* (round-to-epsilon {R flonum?} {epsilon flonum?})
      (infix round(R / epsilon) * epsilon))

    (define-constant C
      (let ((epsilon		0.1)
	    (rounding		round-to-epsilon)
	    (nan-handling	'min))
	(make-inexact-real-comparator epsilon rounding nan-handling)))

    ;; rounding to
    (check (round-to-epsilon 1.0   0.1)	=> 1.0)
    (check (round-to-epsilon 1.05  0.1)	=> 1.0)
    (check (round-to-epsilon 0.951 0.1)	=> 1.0)
    (check (round-to-epsilon 0.949 0.1)	=> 0.9)

    ;; type test
    (check-for-true  (comparator-test-type C 1.2))
    (check-for-true  (comparator-test-type C +inf.0))
    (check-for-true  (comparator-test-type C -inf.0))
    (check-for-true  (comparator-test-type C -nan.0))
    (check-for-false (comparator-test-type C "ciao"))
    (check-for-false (comparator-test-type C 1+2i))

    ;; type check
    (check-for-true  (comparator-check-type C 1.2))
    (check-for-true
     (try
	 (comparator-check-type C (void))
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else #f))))

    ;; comparison
    (check (comparator-compare C 1.2 1.2)	=> 0)
    (check (comparator-compare C 1.0 2.0)	=> -1)
    (check (comparator-compare C 2.0 1.0)	=> +1)

    (check (comparator-compare C +inf.0 +inf.0)	=> 0)
    (check (comparator-compare C -inf.0 -inf.0)	=> 0)
    (check (comparator-compare C -inf.0 +inf.0)	=> -1)
    (check (comparator-compare C +inf.0 -inf.0)	=> +1)

    (check (comparator-compare C +nan.0 +nan.0)	=> 0)
    (check (comparator-compare C +nan.0 1.0)	=> -1)
    (check (comparator-compare C 1.0 +nan.0)	=> +1)

    ;; comparison with rounding
    (check (comparator-compare C 1.04  1.0)	=> 0)
    (check (comparator-compare C 0.96  1.0)	=> 0)
    (check (comparator-compare C 0.951 1.0)	=> 0)

    (check (comparator-compare C 0.949 1.0)	=> -1)
    (check (comparator-compare C 0.949 0.9)	=> 0)

    ;; hash
    (check-for-true
     (= (comparator-hash C 1.0)
	(comparator-hash C 1.04)))
    (check-for-true
     (= (comparator-hash C 1.0)
	(comparator-hash C 0.96)))
    (check-for-true
     (= (comparator-hash C 1.0)
	(comparator-hash C 0.951)))
    (check-for-true
     (= (comparator-hash C 1.04)
	(comparator-hash C 0.951)))
    (check-for-true
     (non-negative-exact-integer? (comparator-hash C +inf.0)))
    (check-for-true
     (non-negative-exact-integer? (comparator-hash C -inf.0)))
    (check-for-true
     (non-negative-exact-integer? (comparator-hash C +nan.0)))

    #f)

;;; --------------------------------------------------------------------
;;; NaN policy to "min"

  (internal-body

    (define-constant C
      (make-inexact-real-comparator #f 'round 'min))

    (check (comparator-compare C +nan.0 +nan.0)	=> 0)
    (check (comparator-compare C +nan.0 1.0)	=> -1)
    (check (comparator-compare C 1.0 +nan.0)	=> +1)

    #f)

;;; --------------------------------------------------------------------
;;; NaN policy to "max"

  (internal-body

    (define-constant C
      (make-inexact-real-comparator #f 'round 'max))

    (check (comparator-compare C +nan.0 +nan.0)	=> 0)
    (check (comparator-compare C +nan.0 1.0)	=> +1)
    (check (comparator-compare C 1.0 +nan.0)	=> -1)

    #f)

;;; --------------------------------------------------------------------
;;; NaN policy to procedure

  (internal-body

    (define (nan-comparison other-R)
      ;;NaN is equal to any number.
      ;;
      0)

    (define-constant C
      (make-inexact-real-comparator #f 'round nan-comparison))

    (check (comparator-compare C +nan.0 +nan.0)	=> 0)
    (check (comparator-compare C +nan.0 +1.0)	=> 0)
    (check (comparator-compare C +1.0   +nan.0)	=> 0)

    #f)

  #t)


(parametrise ((check-test-name	'list-comparator))

  (define-constant C
    (make-list-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C '()))
  (check-for-true  (comparator-test-type C '(1 2)))
  (check-for-false (comparator-test-type C '(1 2 . 3)))
  (check-for-false (comparator-test-type C '(1 2.0)))
  (check-for-false (comparator-test-type C "ciao"))
  (check-for-false (comparator-test-type C '(1+2i)))

  ;; type check
  (check-for-true  (comparator-check-type C '(1 2)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C '(1 2) '(1 2))	=> 0)
  (check (comparator-compare C '(1 2) '(1 3))	=> -1)
  (check (comparator-compare C '(1 3) '(1 2))	=> +1)

  (check (comparator-compare C '()    '())	=> 0)
  (check (comparator-compare C '()    '(1 2))	=> -1)
  (check (comparator-compare C '(1 2) '())	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '())))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '(1 2))))

  #t)


(parametrise ((check-test-name	'improper-list-comparator))

  (module (C)

    (define element-compare
      (let ((compare (comparator-comparison-procedure exact-integer-comparator)))
	(lambda (A B)
	  (if (pair? A)
	      (begin
		(assert (pair? B))
		(let ((rv (compare (car A) (car B))))
		  (if (zero? rv)
		      (comparator-compare C (cdr A) (cdr B))
		    rv)))
	    (compare A B)))))

    (define-constant E
      (make-comparator #t #t
		       element-compare
		       (comparator-hash-function default-comparator)))

    (define-constant C
      (make-improper-list-comparator E))

    #| end of module |# )

  ;; type test
  (check-for-true (comparator-test-type C '()))
  (check-for-true (comparator-test-type C '(1 2)))
  (check-for-true (comparator-test-type C '(1 2 . 3)))
  (check-for-true (comparator-test-type C '(1 2.0)))
  (check-for-true (comparator-test-type C "ciao"))
  (check-for-true (comparator-test-type C '(1+2i)))

  ;; type check
  (check-for-true (comparator-check-type C '(1 2)))
  (check-for-true (comparator-check-type C (void)))

  ;; comparison
  (check (comparator-compare C '(1 2) '(1 2))	=> 0)
  (check (comparator-compare C '(1 2) '(1 3))	=> -1)
  (check (comparator-compare C '(1 3) '(1 2))	=> +1)

  (check (comparator-compare C '()    '())	=> 0)
  (check (comparator-compare C '()    '(1 2))	=> -1)
  (check (comparator-compare C '(1 2) '())	=> +1)

  (check (comparator-compare C '(1 2 . 3) '(1 2 . 3))	=> 0)
  (check (comparator-compare C '(1 2 . 3) '(1 2 . 4))	=> -1)
  (check (comparator-compare C '(1 2 . 4) '(1 2 . 3))	=> +1)

  (check (comparator-compare C '(1 2 9 . 3) '(1 2 9 . 3))	=> 0)
  (check (comparator-compare C '(1 2 9 . 3) '(1 2 9 . 4))	=> -1)
  (check (comparator-compare C '(1 2 9 . 4) '(1 2 9 . 3))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '())))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '(1 2))))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '(1 2 . 3))))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C "ciao")))

  #t)


(parametrise ((check-test-name	'vector-comparator))

  (define-constant C
    (make-vector-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C '#()))
  (check-for-true  (comparator-test-type C '#(1 2)))
  (check-for-false (comparator-test-type C '#(1 2.0)))
  (check-for-false (comparator-test-type C "ciao"))
  (check-for-false (comparator-test-type C '#(1+2i)))

  ;; type check
  (check-for-true  (comparator-check-type C '#(1 2)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C '#(1 2) '#(1 2))	=> 0)
  (check (comparator-compare C '#(1 2) '#(1 3))	=> -1)
  (check (comparator-compare C '#(1 3) '#(1 2))	=> +1)

  (check (comparator-compare C '#()    '#())	=> 0)
  (check (comparator-compare C '#()    '#(1 2))	=> -1)
  (check (comparator-compare C '#(1 2) '#())	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '#())))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '#(1 2))))

  #t)


(parametrise ((check-test-name	'bytevector-comparator))

  (define-constant E
    (make-comparator (lambda (element)
		       (or (= 1 element)
			   (zero? element)))
		     fx=?
		     (lambda (a b)
		       (cond ((fx=? a b)	0)
			     ((fx<? a b)	-1)
			     (else		+1)))
		     fixnum-hash))

  (define-constant C
    (make-bytevector-comparator E))

  ;; type test
  (check-for-true  (comparator-test-type C '#vu8()))
  (check-for-true  (comparator-test-type C '#vu8(1 0)))
  (check-for-false (comparator-test-type C '#vu8(1 2)))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C '#vu8(1 0)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C '#vu8(1 0) '#vu8(1 0))	=> 0)
  (check (comparator-compare C '#vu8(1 0) '#vu8(1 1))	=> -1)
  (check (comparator-compare C '#vu8(1 1) '#vu8(1 0))	=> +1)

  (check (comparator-compare C '#vu8()    '#vu8())	=> 0)
  (check (comparator-compare C '#vu8()    '#vu8(1 0))	=> -1)
  (check (comparator-compare C '#vu8(1 0) '#vu8())	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '#vu8())))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '#vu8(1 0))))

  #t)


(parametrise ((check-test-name	'pair-comparator))

  (define-constant C
    (make-pair-comparator exact-integer-comparator
			  real-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C '(1 . 2.0)))
  (check-for-true  (comparator-test-type C '(1 . 2.0)))
  (check-for-false (comparator-test-type C '()))
  (check-for-false (comparator-test-type C '(1 . 2+1i)))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C '(1 . 2.0)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C '(1 . 2.0) '(1 . 2.0))	=> 0)
  (check (comparator-compare C '(1 . 2.0) '(1 . 3))	=> -1)
  (check (comparator-compare C '(1 . 3)   '(1 . 2.0))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '(1 . 2.0))))

  #t)


(parametrise ((check-test-name	'car-comparator))

  (define-constant C
    (make-car-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C '(1 . 2.0)))
  (check-for-true  (comparator-test-type C '(1 . 2.0)))
  (check-for-true  (comparator-test-type C '(1 . 2+1i)))
  (check-for-false (comparator-test-type C '(2.0 . 1)))
  (check-for-false (comparator-test-type C '()))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C '(1 . 2.0)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C '(1 . 2) '(1 . 3))	=> 0)
  (check (comparator-compare C '(1 . 2) '(2 . 3))	=> -1)
  (check (comparator-compare C '(2 . 2) '(1 . 2))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '(1 . 2.0))))

  #t)


(parametrise ((check-test-name	'cdr-comparator))

  (define-constant C
    (make-cdr-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C '(2.0 . 1)))
  (check-for-true  (comparator-test-type C '(2.0 . 1)))
  (check-for-true  (comparator-test-type C '(2+1i . 1)))
  (check-for-false (comparator-test-type C '(1 . 2.0)))
  (check-for-false (comparator-test-type C '()))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C '(2.0 . 1)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C '(2 . 1) '(3 . 1))	=> 0)
  (check (comparator-compare C '(2 . 1) '(3 . 2))	=> -1)
  (check (comparator-compare C '(2 . 2) '(2 . 1))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C '(2.0 . 1))))

  #t)


(parametrise ((check-test-name	'selecting-comparator))

  (define-constant C
    (make-selecting-comparator boolean-comparator
			       exact-integer-comparator
			       string-comparator))

  ;; type test
  (let ((test-type (comparator-type-test-procedure C)))
    (check-for-true  (test-type #t))
    (check-for-true  (test-type #f))
    (check-for-true  (test-type 1))
    (check-for-true  (test-type "ciao"))
    (check-for-false (test-type '(1 . 2)))
    (check-for-false (test-type 2.0))
    (check-for-false (test-type 1+2i)))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type #t))
    (check-for-true (check-type #f))
    (check-for-true (check-type 1))
    (check-for-true (check-type "ciao"))
    (check-for-true
     (try
	 (comparator-check-type C (void))
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (check (compare #t #t)	=> 0)
    (check (compare #t #f)	=> +1)
    (check (compare #f #t)	=> -1)

    (check (compare 1 1)	=> 0)
    (check (compare 2 1)	=> +1)
    (check (compare 1 2)	=> -1)

    (check (compare "1" "1")	=> 0)
    (check (compare "2" "1")	=> +1)
    (check (compare "1" "2")	=> -1)

    (check-for-true
     (try
	 (compare #t 1)
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E))))
    (check-for-true
     (try
	 (compare #t "ciao")
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E))))
    (check-for-true
     (try
	 (compare 1 "ciao")
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash #t)))
    (check-for-true
     (non-negative-exact-integer? (hash #f)))
    (check-for-true
     (non-negative-exact-integer? (hash 1)))
    (check-for-true
     (non-negative-exact-integer? (hash "ciao")))

    (check-for-true
     (try
	 (hash 1+2i)
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  #t)


(parametrise ((check-test-name	'refining-comparator))

  (define* (round-to-epsilon {R flonum?} {epsilon flonum?})
    (infix round(R / epsilon) * epsilon))

  (define-constant C
    (make-refining-comparator
     (make-inexact-real-comparator 0.1   round-to-epsilon 'error)
     (make-inexact-real-comparator 0.01  round-to-epsilon 'error)
     (make-inexact-real-comparator 0.001 round-to-epsilon 'error)))

  ;; (check (round-to-epsilon 1.05   0.1)		=> 1.0)
  ;; (check (round-to-epsilon 1.05   0.01)		=> 1.05)
  ;; (check (round-to-epsilon 1.05   0.001)	=> 1.05)

  ;; (check (round-to-epsilon 1.005  0.1)		=> 1.0)
  ;; (check (round-to-epsilon 1.005  0.01)		=> 1.0)
  ;; (check (round-to-epsilon 1.005  0.001)	=> 1.005)

  ;; (check (round-to-epsilon 1.0005 0.1)		=> 1.0)
  ;; (check (round-to-epsilon 1.0005 0.01)		=> 1.0)
  ;; (check (round-to-epsilon 1.0005 0.001)	=> 1.0)

  ;; type test
  (let ((test-type (comparator-type-test-procedure C)))
    (check-for-true  (test-type 1.1))
    (check-for-false (test-type 1))
    (check-for-false (test-type "ciao")))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type 1.1))
    (check-for-true
     (try
	 (comparator-check-type C 1)
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (check (compare 1. 1.)	=> 0)
    (check (compare 2. 1.)	=> +1)
    (check (compare 1. 2.)	=> -1)

    ;;Equal according to the first comparator, different according to the second.
    (check (compare 1.00 1.05)	=> -1)
    (check (compare 1.05 1.00)	=> +1)

    ;;Equal according to the first and second comparators, different according to the
    ;;third.
    (check (compare 1.0   1.005)	=> -1)
    (check (compare 1.005 1.0)		=> +1)

    ;;Equal according to the first, second and third comparators.
    (check (compare 1.0 1.0005)	=> 0)
    (check (compare 1.0005 1.0)	=> 0)

    (check-for-true
     (try
    	 (compare 1 "ciao")
       (catch E
    	 ((&comparator-type-error)
    	  #t)
    	 (else E))))
    #f)

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash 1.2)))

    (check-for-true
     (try
	 (hash 1+2i)
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  #t)


(parametrise ((check-test-name	'reverse-comparator))

  (define-constant C
    (make-reverse-comparator exact-integer-comparator))

  ;; type test
  (let ((test-type (comparator-test-type-procedure C)))
    (check-for-true  (test-type 1))
    (check-for-false (test-type '()))
    (check-for-false (test-type "ciao")))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type 1))
    (check-for-true
     (try
	 (check-type (void))
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> +1)
    (check (compare 2 1)	=> -1))

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash 1))))

  #t)


(parametrise ((check-test-name	'debug-comparator))

  (define-constant C
    (make-debug-comparator fixnum-comparator))

  ;; type test
  (let ((test-type (comparator-test-type-procedure C)))
    (check-for-true  (test-type 1))
    (check-for-false (test-type '()))
    (check-for-false (test-type "ciao")))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type 1))
    (check-for-true
     (try
	 (check-type (void))
       (catch E
	 ((&comparator-type-error)
	  #t)
	 (else E)))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1))

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash 1))))

  #t)


(parametrise ((check-test-name	'equal-comparator))

  (define-constant C
    equal-comparator)

  ;; type test
  (let ((test-type (comparator-test-type-procedure C)))
    (check-for-true (test-type 1))
    (check-for-true (test-type '()))
    (check-for-true (test-type "ciao")))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type 1))
    (check-for-true (check-type (void))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (try
	(comparator-compare C 1 1)
      (catch E
	((&unsupported-comparator-operation-error)
	 #t)
	(else E))))

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash 1))))

  #t)


(parametrise ((check-test-name	'eqv-comparator))

  (define-constant C
    eqv-comparator)

  ;; type test
  (let ((test-type (comparator-test-type-procedure C)))
    (check-for-true (test-type 1))
    (check-for-true (test-type '()))
    (check-for-true (test-type "ciao")))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type 1))
    (check-for-true (check-type (void))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (try
	(comparator-compare C 1 1)
      (catch E
	((&unsupported-comparator-operation-error)
	 #t)
	(else E))))

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash 1))))

  #t)


(parametrise ((check-test-name	'eq-comparator))

  (define-constant C
    eq-comparator)

  ;; type test
  (let ((test-type (comparator-test-type-procedure C)))
    (check-for-true (test-type 1))
    (check-for-true (test-type '()))
    (check-for-true (test-type "ciao")))

  ;; type check
  (let ((check-type (comparator-check-type-procedure C)))
    (check-for-true (check-type 1))
    (check-for-true (check-type (void))))

  ;; comparison
  (let ((compare (comparator-comparison-procedure C)))
    (try
	(comparator-compare C 1 1)
      (catch E
	((&unsupported-comparator-operation-error)
	 #t)
	(else E))))

  ;; hash
  (let ((hash (comparator-hash-function C)))
    (check-for-true
     (non-negative-exact-integer? (hash 1))))

  #t)


(parametrise ((check-test-name	'comparison-predicates))

  (define-constant C fixnum-comparator)

;;; --------------------------------------------------------------------

  (check-for-true  (=? C 1 1))
  (check-for-false (=? C 1 2))
  (check-for-false (=? C 2 1))

  (check-for-true  (=? C 1 1 1))
  (check-for-false (=? C 1 2 3))
  (check-for-false (=? C 2 1 3))
  (check-for-false (=? C 3 2 1))
  (check-for-false (=? C 1 3 2))

;;; --------------------------------------------------------------------

  (check-for-false (<? C 1 1))
  (check-for-true  (<? C 1 2))
  (check-for-false (<? C 2 1))

  (check-for-false (<? C 1 1 1))
  (check-for-true  (<? C 1 2 3))
  (check-for-false (<? C 2 1 3))
  (check-for-false (<? C 3 2 1))
  (check-for-false (<? C 1 3 2))

;;; --------------------------------------------------------------------

  (check-for-false (>? C 1 1))
  (check-for-false (>? C 1 2))
  (check-for-true  (>? C 2 1))

  (check-for-false (>? C 1 1 1))
  (check-for-true  (>? C 3 2 1))
  (check-for-false (>? C 1 2 3))
  (check-for-false (>? C 2 1 3))
  (check-for-false (>? C 1 3 2))

;;; --------------------------------------------------------------------

  (check-for-true  (<=? C 1 1))
  (check-for-true  (<=? C 1 2))
  (check-for-false (<=? C 2 1))

  (check-for-true  (<=? C 1 1 1))
  (check-for-true  (<=? C 1 2 3))
  (check-for-false (<=? C 2 1 3))
  (check-for-false (<=? C 3 2 1))
  (check-for-false (<=? C 1 3 2))

;;; --------------------------------------------------------------------

  (check-for-true  (>=? C 1 1))
  (check-for-false (>=? C 1 2))
  (check-for-true  (>=? C 2 1))

  (check-for-true  (>=? C 1 1 1))
  (check-for-true  (>=? C 3 2 1))
  (check-for-false (>=? C 1 2 3))
  (check-for-false (>=? C 2 1 3))
  (check-for-false (>=? C 1 3 2))

;;; --------------------------------------------------------------------

  (let ((fun=? (make= C)))
    (check-for-true  (fun=? 1 1))
    (check-for-false (fun=? 1 2))
    (check-for-false (fun=? 2 1))

    (check-for-true  (fun=? 1 1 1))
    (check-for-false (fun=? 1 2 3))
    (check-for-false (fun=? 2 1 3))
    (check-for-false (fun=? 3 2 1))
    (check-for-false (fun=? 1 3 2))
    #f)

;;; --------------------------------------------------------------------

  (let ((fun<? (make< C)))
    (check-for-false (fun<? 1 1))
    (check-for-true  (fun<? 1 2))
    (check-for-false (fun<? 2 1))

    (check-for-false (fun<? 1 1 1))
    (check-for-true  (fun<? 1 2 3))
    (check-for-false (fun<? 2 1 3))
    (check-for-false (fun<? 3 2 1))
    (check-for-false (fun<? 1 3 2))
    #f)

;;; --------------------------------------------------------------------

  (let ((fun>? (make> C)))
    (check-for-false (fun>? 1 1))
    (check-for-false (fun>? 1 2))
    (check-for-true  (fun>? 2 1))

    (check-for-false (fun>? 1 1 1))
    (check-for-true  (fun>? 3 2 1))
    (check-for-false (fun>? 1 2 3))
    (check-for-false (fun>? 2 1 3))
    (check-for-false (fun>? 1 3 2))
    #f)

;;; --------------------------------------------------------------------

  (let ((fun<=? (make<= C)))
    (check-for-true  (fun<=? 1 1))
    (check-for-true  (fun<=? 1 2))
    (check-for-false (fun<=? 2 1))

    (check-for-true  (fun<=? 1 1 1))
    (check-for-true  (fun<=? 1 2 3))
    (check-for-false (fun<=? 2 1 3))
    (check-for-false (fun<=? 3 2 1))
    (check-for-false (fun<=? 1 3 2))
    #f)

;;; --------------------------------------------------------------------

  (let ((fun>=? (make>= C)))
    (check-for-true  (fun>=? 1 1))
    (check-for-false (fun>=? 1 2))
    (check-for-true  (fun>=? 2 1))

    (check-for-true  (fun>=? 1 1 1))
    (check-for-true  (fun>=? 3 2 1))
    (check-for-false (fun>=? 1 2 3))
    (check-for-false (fun>=? 2 1 3))
    (check-for-false (fun>=? 1 3 2))
    #f)

  #t)


(parametrise ((check-test-name	'comparison-procedure-constructors))

  (let ((compare (make-comparison< fx<?)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1)
    #f)

  (let ((compare (make-comparison> fx>?)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1)
    #f)

  (let ((compare (make-comparison<= fx<=?)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1)
    #f)

  (let ((compare (make-comparison>= fx>=?)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1)
    #f)

  (let ((compare (make-comparison=/< fx=? fx<?)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1)
    #f)

  (let ((compare (make-comparison=/> fx=? fx>?)))
    (check (compare 1 1)	=> 0)
    (check (compare 1 2)	=> -1)
    (check (compare 2 1)	=> +1)
    #f)

  #t)


(parametrise ((check-test-name	'ternary-comparison))

  (define-constant C fixnum-comparator)

;;; --------------------------------------------------------------------

  (check-for-true  (in-open-interval? 1 2 3))
  (check-for-false (in-open-interval? 1 1 3))
  (check-for-false (in-open-interval? 1 3 3))

  (check-for-false (in-open-interval? 1 0 3))
  (check-for-false (in-open-interval? 1 0 3))
  (check-for-false (in-open-interval? 1 0 3))

  (check-for-false (in-open-interval? 1 9 3))
  (check-for-false (in-open-interval? 1 9 3))
  (check-for-false (in-open-interval? 1 9 3))

  (check-for-true  (in-open-interval? C 1 2 3))
  (check-for-false (in-open-interval? C 1 1 3))
  (check-for-false (in-open-interval? C 1 3 3))

  (check-for-false (in-open-interval? C 1 0 3))
  (check-for-false (in-open-interval? C 1 0 3))
  (check-for-false (in-open-interval? C 1 0 3))

  (check-for-false (in-open-interval? C 1 9 3))
  (check-for-false (in-open-interval? C 1 9 3))
  (check-for-false (in-open-interval? C 1 9 3))

;;; --------------------------------------------------------------------

  (check-for-true  (in-closed-interval? 1 2 3))
  (check-for-true  (in-closed-interval? 1 1 3))
  (check-for-true  (in-closed-interval? 1 3 3))

  (check-for-false (in-closed-interval? 1 0 3))
  (check-for-false (in-closed-interval? 1 0 3))
  (check-for-false (in-closed-interval? 1 0 3))

  (check-for-false (in-closed-interval? 1 9 3))
  (check-for-false (in-closed-interval? 1 9 3))
  (check-for-false (in-closed-interval? 1 9 3))

  (check-for-true  (in-closed-interval? C 1 2 3))
  (check-for-true  (in-closed-interval? C 1 1 3))
  (check-for-true  (in-closed-interval? C 1 3 3))

  (check-for-false (in-closed-interval? C 1 0 3))
  (check-for-false (in-closed-interval? C 1 0 3))
  (check-for-false (in-closed-interval? C 1 0 3))

  (check-for-false (in-closed-interval? C 1 9 3))
  (check-for-false (in-closed-interval? C 1 9 3))
  (check-for-false (in-closed-interval? C 1 9 3))

;;; --------------------------------------------------------------------

  (check-for-true  (in-open-closed-interval? 1 2 3))
  (check-for-false (in-open-closed-interval? 1 1 3))
  (check-for-true  (in-open-closed-interval? 1 3 3))

  (check-for-false (in-open-closed-interval? 1 0 3))
  (check-for-false (in-open-closed-interval? 1 0 3))
  (check-for-false (in-open-closed-interval? 1 0 3))

  (check-for-false (in-open-closed-interval? 1 9 3))
  (check-for-false (in-open-closed-interval? 1 9 3))
  (check-for-false (in-open-closed-interval? 1 9 3))

  (check-for-true  (in-open-closed-interval? C 1 2 3))
  (check-for-false (in-open-closed-interval? C 1 1 3))
  (check-for-true  (in-open-closed-interval? C 1 3 3))

  (check-for-false (in-open-closed-interval? C 1 0 3))
  (check-for-false (in-open-closed-interval? C 1 0 3))
  (check-for-false (in-open-closed-interval? C 1 0 3))

  (check-for-false (in-open-closed-interval? C 1 9 3))
  (check-for-false (in-open-closed-interval? C 1 9 3))
  (check-for-false (in-open-closed-interval? C 1 9 3))

;;; --------------------------------------------------------------------

  (check-for-true  (in-closed-open-interval? 1 2 3))
  (check-for-true  (in-closed-open-interval? 1 1 3))
  (check-for-false (in-closed-open-interval? 1 3 3))

  (check-for-false (in-closed-open-interval? 1 0 3))
  (check-for-false (in-closed-open-interval? 1 0 3))
  (check-for-false (in-closed-open-interval? 1 0 3))

  (check-for-false (in-closed-open-interval? 1 9 3))
  (check-for-false (in-closed-open-interval? 1 9 3))
  (check-for-false (in-closed-open-interval? 1 9 3))

  (check-for-true  (in-closed-open-interval? C 1 2 3))
  (check-for-true  (in-closed-open-interval? C 1 1 3))
  (check-for-false (in-closed-open-interval? C 1 3 3))

  (check-for-false (in-closed-open-interval? C 1 0 3))
  (check-for-false (in-closed-open-interval? C 1 0 3))
  (check-for-false (in-closed-open-interval? C 1 0 3))

  (check-for-false (in-closed-open-interval? C 1 9 3))
  (check-for-false (in-closed-open-interval? C 1 9 3))
  (check-for-false (in-closed-open-interval? C 1 9 3))

  #t)


(parametrise ((check-test-name	'comparison-syntax))

  (define-constant C fixnum-comparator)

  (define compare
    (comparator-comparison-procedure C))

;;; --------------------------------------------------------------------

  (check (if3 (compare 1 1) 'less 'equal 'greater)	=> 'equal)
  (check (if3 (compare 1 2) 'less 'equal 'greater)	=> 'less)
  (check (if3 (compare 2 1) 'less 'equal 'greater)	=> 'greater)

;;; --------------------------------------------------------------------

  (check-for-true  (if=? (compare 1 1) #t #f))
  (check-for-false (if=? (compare 1 2) #t #f))
  (check-for-false (if=? (compare 2 1) #t #f))

  (check (if=? (compare 1 1) #t)	=> #t)
  (check (if=? (compare 1 2) #t)	=> (void))
  (check (if=? (compare 2 1) #t)	=> (void))

;;; --------------------------------------------------------------------

  (check-for-true  (if-not=? (compare 1 1) #f #t))
  (check-for-false (if-not=? (compare 1 2) #f #t))
  (check-for-false (if-not=? (compare 2 1) #f #t))

  (check (if-not=? (compare 1 1) #t)	=> (void))
  (check (if-not=? (compare 1 2) #t)	=> #t)
  (check (if-not=? (compare 2 1) #t)	=> #t)

;;; --------------------------------------------------------------------

  (check-for-false (if<? (compare 1 1) #t #f))
  (check-for-true  (if<? (compare 1 2) #t #f))
  (check-for-false (if<? (compare 2 1) #t #f))

  (check (if<? (compare 1 1) #t)	=> (void))
  (check (if<? (compare 1 2) #t)	=> #t)
  (check (if<? (compare 2 1) #t)	=> (void))

;;; --------------------------------------------------------------------

  (check-for-false (if>? (compare 1 1) #t #f))
  (check-for-false (if>? (compare 1 2) #t #f))
  (check-for-true  (if>? (compare 2 1) #t #f))

  (check (if>? (compare 1 1) #t)	=> (void))
  (check (if>? (compare 1 2) #t)	=> (void))
  (check (if>? (compare 2 1) #t)	=> #t)

;;; --------------------------------------------------------------------

  (check-for-true  (if<=? (compare 1 1) #t #f))
  (check-for-true  (if<=? (compare 1 2) #t #f))
  (check-for-false (if<=? (compare 2 1) #t #f))

  (check (if<=? (compare 1 1) #t)	=> #t)
  (check (if<=? (compare 1 2) #t)	=> #t)
  (check (if<=? (compare 2 1) #t)	=> (void))

;;; --------------------------------------------------------------------

  (check-for-true  (if>=? (compare 1 1) #t #f))
  (check-for-false (if>=? (compare 1 2) #t #f))
  (check-for-true  (if>=? (compare 2 1) #t #f))

  (check (if>=? (compare 1 1) #t)	=> #t)
  (check (if>=? (compare 1 2) #t)	=> (void))
  (check (if>=? (compare 2 1) #t)	=> #t)

  #t)


(parametrise ((check-test-name	'minmax))

  (define-constant C fixnum-comparator)

;;; --------------------------------------------------------------------

  (check (comparator-min C 0)		=> 0)

  (check (comparator-min C 0 0)		=> 0)
  (check (comparator-min C 0 1)		=> 0)
  (check (comparator-min C 1 0)		=> 0)

  (check (comparator-min C 0 0 0)	=> 0)

  (check (comparator-min C 0 1 1)	=> 0)
  (check (comparator-min C 1 1 0)	=> 0)
  (check (comparator-min C 1 0 1)	=> 0)

  (check (comparator-min C 0 1 2)	=> 0)
  (check (comparator-min C 1 2 0)	=> 0)
  (check (comparator-min C 2 0 1)	=> 0)

  (check (comparator-min C 0 1 2)	=> 0)
  (check (comparator-min C 1 2 0)	=> 0)
  (check (comparator-min C 2 0 1)	=> 0)

  (check (comparator-min C 0 1 2 3)	=> 0)
  (check (comparator-min C 1 2 3 0)	=> 0)
  (check (comparator-min C 2 3 0 1)	=> 0)
  (check (comparator-min C 3 0 1 2)	=> 0)

  (check (comparator-min C 3 2 1 0)	=> 0)
  (check (comparator-min C 2 1 0 3)	=> 0)
  (check (comparator-min C 1 0 3 2)	=> 0)
  (check (comparator-min C 0 3 2 1)	=> 0)

;;; --------------------------------------------------------------------

  (check (comparator-max C 0)		=> 0)

  (check (comparator-max C 0 0)		=> 0)
  (check (comparator-max C 0 1)		=> 1)
  (check (comparator-max C 1 0)		=> 1)

  (check (comparator-max C 0 0 0)	=> 0)

  (check (comparator-max C 0 1 1)	=> 1)
  (check (comparator-max C 1 1 0)	=> 1)
  (check (comparator-max C 1 0 1)	=> 1)

  (check (comparator-max C 0 1 2)	=> 2)
  (check (comparator-max C 1 2 0)	=> 2)
  (check (comparator-max C 2 0 1)	=> 2)

  (check (comparator-max C 0 1 2)	=> 2)
  (check (comparator-max C 1 2 0)	=> 2)
  (check (comparator-max C 2 0 1)	=> 2)

  (check (comparator-max C 0 1 2 3)	=> 3)
  (check (comparator-max C 1 2 3 0)	=> 3)
  (check (comparator-max C 2 3 0 1)	=> 3)
  (check (comparator-max C 3 0 1 2)	=> 3)

  (check (comparator-max C 3 2 1 0)	=> 3)
  (check (comparator-max C 2 1 0 3)	=> 3)
  (check (comparator-max C 1 0 3 2)	=> 3)
  (check (comparator-max C 0 3 2 1)	=> 3)

  #t)


;;;; done

(check-report)

#| end of PROGRAM |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
