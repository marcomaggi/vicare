;;;
;;;Part of: vicare scheme
;;;Contents: tests for hashtables
;;;Date: Thu Mar 12, 2015
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
(check-display "*** testing Vicare: hashtables\n")


;;;; helpers

(define (mktable-1)
  (alist->hashtable! (make-eq-hashtable) '((a . 1) (b . 2) (c . 3))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
	    (symbol->string s2)))


(parametrise ((check-test-name	'hash-functions))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?fun ?obj)
       (check
	   (non-negative-exact-integer? (?fun ?obj))
	 => #t))
      ))

;;; --------------------------------------------------------------------

  (doit string-hash "ciao")

  (doit string-ci-hash "Ciao")

;;; --------------------------------------------------------------------

  (doit symbol-hash 'ciao)

;;; --------------------------------------------------------------------

  (doit bytevector-hash '#vu8(1 2 3 4))

;;; --------------------------------------------------------------------

  (doit fixnum-hash 123)
  (doit fixnum-hash (greatest-fixnum))
  (doit fixnum-hash (least-fixnum))

;;; --------------------------------------------------------------------

  (doit exact-integer-hash 123)
  (doit exact-integer-hash (greatest-fixnum))
  (doit exact-integer-hash (least-fixnum))
  (doit exact-integer-hash (greatest-negative-bignum))
  (doit exact-integer-hash (least-positive-bignum))

;;; --------------------------------------------------------------------

  (doit flonum-hash +0.0)
  (doit flonum-hash -0.0)
  (doit flonum-hash +1.0)
  (doit flonum-hash -1.0)
  (doit flonum-hash +1.2)
  (doit flonum-hash -1.2)
  (doit flonum-hash +inf.0)
  (doit flonum-hash -inf.0)
  (doit flonum-hash -nan.0)

;;; --------------------------------------------------------------------

  (doit number-hash 123)
  (doit number-hash (greatest-fixnum))
  (doit number-hash (least-fixnum))
  (doit number-hash (greatest-negative-bignum))
  (doit number-hash (least-positive-bignum))
  (doit number-hash +0.0)
  (doit number-hash -0.0)
  (doit number-hash +1.0)
  (doit number-hash -1.0)
  (doit number-hash +1.2)
  (doit number-hash -1.2)
  (doit number-hash +inf.0)
  (doit number-hash -inf.0)
  (doit number-hash -nan.0)
  (doit number-hash +1/2)
  (doit number-hash -1/2)
  (doit number-hash +1+2i)
  (doit number-hash +1-2i)
  (doit number-hash +1.0+2i)
  (doit number-hash +1-2.0i)
  (doit number-hash +1.0+2.0i)
  (doit number-hash -1.0-2.0i)

;;; --------------------------------------------------------------------

  (doit char-hash #\a)

  (doit char-ci-hash #\a)

;;; --------------------------------------------------------------------

  (doit boolean-hash #t)
  (doit boolean-hash #f)

;;; --------------------------------------------------------------------

  (doit void-hash (void))
  (doit eof-object-hash (eof-object))
  (doit would-block-hash (would-block-object))

;;; --------------------------------------------------------------------

  (internal-body
    (define-struct a-struct
      (a b c))
    (doit struct-hash (make-a-struct 1 2 3)))

;;; --------------------------------------------------------------------

  (internal-body
    (define-record-type a-record
      (fields a b c))
    (doit record-hash (make-a-record 1 2 3)))

;;; --------------------------------------------------------------------

  (doit object-hash "ciao")
  (doit object-hash "Ciao")
  (doit object-hash 'ciao)
  (doit object-hash '#vu8(1 2 3 4))
  (doit object-hash #t)
  (doit object-hash #f)
  (doit object-hash 123)
  (doit object-hash (greatest-fixnum))
  (doit object-hash (least-fixnum))
  (doit object-hash (greatest-negative-bignum))
  (doit object-hash (least-positive-bignum))
  (doit object-hash +0.0)
  (doit object-hash -0.0)
  (doit object-hash +1.0)
  (doit object-hash -1.0)
  (doit object-hash +1.2)
  (doit object-hash -1.2)
  (doit object-hash +inf.0)
  (doit object-hash -inf.0)
  (doit object-hash -nan.0)
  (doit object-hash +1/2)
  (doit object-hash -1/2)
  (doit object-hash +1+2i)
  (doit object-hash +1-2i)
  (doit object-hash +1.0+2i)
  (doit object-hash +1-2.0i)
  (doit object-hash +1.0+2.0i)
  (doit object-hash -1.0-2.0i)
  (doit object-hash #\a)
  (doit object-hash (void))
  (doit object-hash (eof-object))
  (doit object-hash (would-block-object))
  (internal-body
    (define-struct a-struct
      (a b c))
    (doit object-hash (make-a-struct 1 2 3)))
  (internal-body
    (define-record-type a-record
      (fields a b c))
    (doit object-hash (make-a-record 1 2 3)))

;;; --------------------------------------------------------------------

  (doit equal-hash "ciao")
  (doit equal-hash "Ciao")
  (doit equal-hash 'ciao)
  (doit equal-hash '#vu8(1 2 3 4))
  (doit equal-hash #t)
  (doit equal-hash #f)
  (doit equal-hash 123)
  (doit equal-hash (greatest-fixnum))
  (doit equal-hash (least-fixnum))
  (doit equal-hash (greatest-negative-bignum))
  (doit equal-hash (least-positive-bignum))
  (doit equal-hash +0.0)
  (doit equal-hash -0.0)
  (doit equal-hash +1.0)
  (doit equal-hash -1.0)
  (doit equal-hash +1.2)
  (doit equal-hash -1.2)
  (doit equal-hash +inf.0)
  (doit equal-hash -inf.0)
  (doit equal-hash -nan.0)
  (doit equal-hash +1/2)
  (doit equal-hash -1/2)
  (doit equal-hash +1+2i)
  (doit equal-hash +1-2i)
  (doit equal-hash +1.0+2i)
  (doit equal-hash +1-2.0i)
  (doit equal-hash +1.0+2.0i)
  (doit equal-hash -1.0-2.0i)
  (doit equal-hash #\a)
  (doit equal-hash (void))
  (doit equal-hash (eof-object))
  (doit equal-hash (would-block-object))
  (internal-body
    (define-struct a-struct
      (a b c))
    (doit equal-hash (make-a-struct 1 2 3)))
  (internal-body
    (define-record-type a-record
      (fields a b c))
    (doit equal-hash (make-a-record 1 2 3)))

  #t)


(parametrise ((check-test-name	'predicates))

;;; hashtable-mutable?

  (check-for-true
   (internal-body
     (define T
       (make-eq-hashtable))
     (hashtable-mutable? T)))

  (check-for-false
   (internal-body
     (define T
       (make-eq-hashtable))
     (define T^
       (hashtable-copy T #f))
     (hashtable-mutable? T^)))

  (check-for-true
   (internal-body
     (define T
       (make-eq-hashtable))
     (define T^
       (hashtable-copy T #t))
     (hashtable-mutable? T^)))

;;; --------------------------------------------------------------------
;;; mutable-hashtable?

  (check-for-true
   (internal-body
     (define T
       (make-eq-hashtable))
     (mutable-hashtable? T)))

  (check-for-false
   (internal-body
     (define T
       (make-eq-hashtable))
     (define T^
       (hashtable-copy T #f))
     (mutable-hashtable? T^)))

  (check-for-true
   (internal-body
     (define T
       (make-eq-hashtable))
     (define T^
       (hashtable-copy T #t))
     (mutable-hashtable? T^)))

  (check-for-false (mutable-hashtable? 123))

  #t)


(parametrise ((check-test-name	'insertion))

  ;;By inserting "many" entries we cause the table to be enlarged multiple times.
  ;;
  (define-constant DIM
    1024)

  (define-constant RESULT-KEY
    (div DIM 2))

;;; --------------------------------------------------------------------
;;; table enlargement

  (check	;EQ? hashtable, symbol keys
      (let ((T (make-eq-hashtable)))
	(do ((i 0 (add1 i)))
	    ((= i DIM)
	     (hashtable-ref T (string->symbol (number->string RESULT-KEY))))
	  (hashtable-set! T (string->symbol (number->string i)) i)))
    => RESULT-KEY)

  (check	;EQV? hashtable, string keys
      ;;We need to remember  that EQV? compares strings using EQ?.   So to retrieve a
      ;;value we need to use the same key  used to insert it: the "same" according to
      ;;EQ?.
      (let ((T  (make-eqv-hashtable))
	    (K  (let ((K (make-vector DIM)))
		  (do ((i 0 (add1 i)))
		      ((= i DIM)
		       K)
		    (vector-set! K i (number->string i))))))
	(do ((i 0 (add1 i)))
	    ((= i DIM)
	     (hashtable-ref T (vector-ref K RESULT-KEY)))
	  (hashtable-set! T (vector-ref K i) i)))
    => RESULT-KEY)

  (check	;EQV? hashtable, number keys
      (let ((T (make-eqv-hashtable)))
	(do ((i 0 (add1 i)))
	    ((= i DIM)
	     (hashtable-ref T RESULT-KEY))
	  (hashtable-set! T i i)))
    => RESULT-KEY)

  (check	;custom hashtable, string keys
      (let ((T (make-hashtable string-hash string=?)))
	(do ((i 0 (add1 i)))
	    ((= i DIM)
	     (hashtable-ref T (number->string RESULT-KEY)))
	  (hashtable-set! T (number->string i) i)))
    => RESULT-KEY)

  #t)


(parametrise ((check-test-name	'deletion))

  (check
      (let ((T (make-eq-hashtable)))
	(hashtable-set! T 'ciao 1)
	(hashtable-delete! T 'ciao))
    => 'ciao 1)

  #t)


(parametrise ((check-test-name	'for-each))

  (check
      (let ((T (mktable-1))
	    (L '()))
	(hashtable-for-each-key
	    (lambda (key)
	      (set-cons! L key))
	  T)
	(list-sort symbol<? L))
    => '(a b c))

;;; --------------------------------------------------------------------

  (check
      (let ((T (mktable-1))
	    (L '()))
	(hashtable-for-each-entry
	    (lambda (key val)
	      (set-cons! L (list key val)))
	  T)
	(list-sort (lambda (x y)
		     (symbol<? (car x) (car y)))
		   L))
    => '((a 1) (b 2) (c 3)))

  #t)


(parametrise ((check-test-name	'for-all))

  (check
      (let ((T (mktable-1)))
	(hashtable-for-all-keys
	    symbol?
	  T))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((T (mktable-1)))
	(hashtable-for-all-entries
	    (lambda (key val)
	      (and (symbol? key)
		   (fixnum? val)))
	  T))
    => #t)

  #t)


(parametrise ((check-test-name	'exists))

  (check
      (let ((T (mktable-1)))
	(hashtable-exists-key
	    (lambda (key)
	      (eq? key 'a))
	  T))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((T (mktable-1)))
	(hashtable-exists-entry
	    (lambda (key val)
	      (and (eq?  key 'a)
		   (fx=? val 1)))
	  T))
    => #t)

  #t)


(parametrise ((check-test-name	'find))

  (check
      (let ((T (mktable-1)))
	(hashtable-find-key
	    (lambda (key)
	      (eq? key 'a))
	  T))
    => 'a)

;;; --------------------------------------------------------------------

  (check
      (let ((T (mktable-1)))
	(hashtable-find-entry
	    (lambda (key val)
	      (and (eq?  key 'a)
		   (fx=? val 1)))
	  T))
    => '(a . 1))

  #t)


(parametrise ((check-test-name	'fold))

  (check
      (let ((T (mktable-1)))
	(list-sort symbol<?
		   (hashtable-fold-keys
		       cons
		     '() T)))
    => '(a b c))

;;; --------------------------------------------------------------------

  (check
      (let ((T (mktable-1)))
	(list-sort (lambda (x y)
		     (symbol<? (car x) (car y)))
		   (hashtable-fold-entries
		       (lambda (key val nil)
			 (cons (cons key val) nil))
		     '() T)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  #t)


(parametrise ((check-test-name	'conversion))

  (check
      (let ((T (mktable-1)))
	(list-sort (lambda (x y)
		     (symbol<? (car x) (car y)))
		   (hashtable->alist T)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (let ((T (mktable-1)))
	(list-sort (lambda (x y)
		     (symbol<? (car x) (car y)))
		   (hashtable->alist T #f)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (let ((T (mktable-1)))
	(hashtable->alist T symbol<?))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
