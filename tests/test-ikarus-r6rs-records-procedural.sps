;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from  the file  "scheme/tests/r6rs-records-procedural.ss"
;;;	file in the original Ikarus distribution.
;;;
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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

#!ikarus
(import (ikarus)
  (rnrs records inspection)
  (rnrs records procedural)
  (vicare checks))

(define (%printf . args)
  (when #f
    (apply fprintf (current-error-port) args)))

(define rtd0
  (make-record-type-descriptor 'rtd0
    #f #f #f #f
    '#((immutable x0))))

(define rcd0-default
  (make-record-constructor-descriptor rtd0 #f #f))

(define make-t0-default (record-constructor rcd0-default))
(define t0? (record-predicate rtd0))
(define t0-x0 (record-accessor rtd0 0))

(define (test0)
  (let ([x (make-t0-default 5)])
    (%printf "x=~s\n" x)
    (assert (not (record-field-mutable? rtd0 0)))
    (assert (record? x))
    (assert (t0? x))
    (assert (= (t0-x0 x) 5))))

(define rtd1
  (make-record-type-descriptor 'rtd1
    rtd0 #f #f #f
    '#((immutable y0) (mutable y1))))

(define rcd1-default
  (make-record-constructor-descriptor rtd1 #f #f))
(define make-t1-default (record-constructor rcd1-default))
(define t1? (record-predicate rtd1))
(define t1-y0 (record-accessor rtd1 0))
(define t1-y1 (record-accessor rtd1 1))

(define (test1)
  (let ([x (make-t1-default 5 1 2)])
    (assert (record-field-mutable? rtd1 1))
    (assert (record? x))
    (assert (t1? x))
    (assert (t0? x))
    (assert (= (t0-x0 x) 5))
    (assert (= (t1-y0 x) 1))
    (assert (= (t1-y1 x) 2))))

(define rcd0-17
  (make-record-constructor-descriptor rtd0 #f
    (lambda (p)
      (lambda ()
        (p 17)))))

(define make-rcd0-17 (record-constructor rcd0-17))

(define (test2)
  (let ([x (make-rcd0-17)])
    (assert (record? x))
    (assert (t0? x))
    (assert (not (t1? x)))
    (assert (= (t0-x0 x) 17))))

(define rcd1-17-rev
  (make-record-constructor-descriptor rtd1 rcd0-17
    (lambda (p)
      (lambda (y0 y1)
        ((p) y1 y0)))))

(define make-rcd1-17-rev (record-constructor rcd1-17-rev))

(define (test3)
  (let ([x (make-rcd1-17-rev 1 2)])
    (assert (record? x))
    (assert (t1? x))
    (assert (t0? x))
    (assert (= (t0-x0 x) 17))
    (assert (= (t1-y0 x) 2))
    (assert (= (t1-y1 x) 1))))


;;This was a  weird test in which the protocol  function calls its maker
;;argument directly,  rather than returning  a function which  will call
;;the maker.   It worked with  the original Ikarus'  implementation, but
;;now  it  assertion fails  with  the  Vicare's implementation.   (Marco
;;Maggi; Thu Mar 22, 2012)
;;
;; (define rcd1-17-default
;;   (make-record-constructor-descriptor rtd1 rcd0-17
;;     (lambda (p) (p))))
;;
;; (define make-rcd1-17-default (record-constructor rcd1-17-default))
;;
;; (define (test4)
;;   (let ([x (make-rcd1-17-default 1 2)])
;;     (assert (record? x))
;;     (assert (t1? x))
;;     (assert (t0? x))
;;     (assert (= (t0-x0 x) 17))
;;     (assert (= (t1-y0 x) 1))
;;     (assert (= (t1-y1 x) 2))))

(define (test5)
  (define :point
    (make-record-type-descriptor 'point
      #f #f #f #f
      '#((mutable x) (mutable y))))

  (define :point-cd
    (make-record-constructor-descriptor :point #f #f))

  (define make-point (record-constructor :point-cd))
  (define point? (record-predicate :point))
  (define point-x (record-accessor :point 0))
  (define point-y (record-accessor :point 1))
  (define set-point-x! (record-mutator :point 0))
  (define set-point-y! (record-mutator :point 1))

  (define :point2
    (make-record-type-descriptor 'point2
      :point #f #f #f
      '#((mutable x) (mutable y))))
  (define make-point2
    (record-constructor
      (make-record-constructor-descriptor :point2 #f #f)))
  (define point2? (record-predicate :point2))
  (define point2-xx (record-accessor :point2 0))
  (define point2-yy (record-accessor :point2 1))



  (define :point-cd/abs
    (make-record-constructor-descriptor :point #f
      (lambda (new)
        (lambda (x y)
          (%printf "point/abs constr ~s ~s\n" x y)
          (let ([r (new (abs x) (abs y))])
            (%printf "point/abs r=~s\n" r)
            r)))))

  (define make-point/abs (record-constructor :point-cd/abs))

  (define :cpoint
    (make-record-type-descriptor 'cpoint :point #f #f #f
      '#((mutable rgb))))

  (define :cpoint-cd
    (make-record-constructor-descriptor :cpoint :point-cd
      (lambda (p)
        (lambda (x y c)
          (%printf "cpoint constr ~s ~s ~s\n" x y c)
          (let ([r ((p x y) (color->rgb c))])
            (%printf "cpoint r=~s\n" r)
            r)))))

  (define make-cpoint
    (record-constructor :cpoint-cd))

  (define (color->rgb c) (cons 'rgb c))

  (define cpoint-rgb (record-accessor :cpoint 0))

  (define cpoint/abs-cd
    (make-record-constructor-descriptor :cpoint :point-cd/abs
      (lambda (p)
        (lambda (x y c)
          (%printf "cpoint/abs constr ~s ~s ~s\n" x y c)
          (let ([r ((p x y) (color->rgb c))])
            (%printf "cpointabs r=~s\n" r)
            r)))))

  (define make-cpoint/abs
    (record-constructor cpoint/abs-cd))

  (%printf "cpoint/abs-cd=~s\n" cpoint/abs-cd)

  (let ()
    (define p1 (make-point 1 2))
    (assert (point? p1))
    (assert (= (point-x p1) 1))
    (assert (= (point-y p1) 2))
    (set-point-x! p1 5)
    (assert (= (point-x p1) 5))
    (assert (= (point-y p1) 2)))

  (let ()
    (define p2 (make-point2 1 2 3 4))
    (assert (point? p2))
    (assert (= (point-x p2) 1))
    (assert (= (point-y p2) 2))
    (assert (= (point2-xx p2) 3))
    (assert (= (point2-yy p2) 4)))


  (let ()
    (assert (= (point-x (make-point/abs -1 -2)) 1))
    (assert (= (point-y (make-point/abs -1 -2)) 2)))

  (let ()
    (assert (equal? (cpoint-rgb (make-cpoint -1 -3 'red)) '(rgb . red)))
    (assert (equal? (cpoint-rgb (make-cpoint/abs -1 -3 'red)) '(rgb . red)))
    (assert (= (point-x (make-cpoint -1 -3 'red)) -1))
    (assert (= (point-x (make-cpoint/abs -1 -3 'red)) 1))
    )
  )

(check-display "*** testing Ikarus R6RS records procedural\n\n")
(test0)
(%printf "test0 ok\n")
(test1)
(%printf "test1 ok\n")
(test2)
(%printf "test2 ok\n")
(test3)
(%printf "test3 ok\n")
;;(test4)
;;(%printf "test4 ok\n")
(test5)
(%printf "test5 ok\n")
(%printf "rtd0=~s\n" rtd0)
(%printf "rcd0=~s\n" rcd0-default)
(%printf "fields of ~s are ~s\n" rtd1 (record-type-field-names rtd1))
(check-display "; *** done\n\n")

;;; end of file
