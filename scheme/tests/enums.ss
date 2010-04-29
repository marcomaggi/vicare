;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (tests enums)
  (export run-tests)
  (import (ikarus))
    
  (define (run-tests)
    (define (trace-equal? x y) (equal? x y))
    (assert 
      (trace-equal? 
        (let* ([e (make-enumeration '(red green blue))]
               [i (enum-set-indexer e)])
          (list (i 'red) (i 'green) (i 'blue) (i 'yellow)))
        '(0 1 2 #f)))
    (assert 
      (trace-equal? 
        (let* ([e (make-enumeration '(red green blue))]
               [c (enum-set-constructor e)])
          (enum-set->list (c '())))
        '()))
    (assert 
      (trace-equal? 
        (let* ([e (make-enumeration '(red green blue))]
               [c (enum-set-constructor e)])
          (enum-set->list (c '(blue red))))
        '(red blue)))
    (assert 
      (trace-equal? 
        (let* ([e (make-enumeration '(red green blue))]
               [c (enum-set-constructor e)])
          (list 
            (enum-set-member? 'blue (c '(red blue)))
            (enum-set-member? 'green (c '(red blue)))
            (enum-set-subset? (c '(red blue)) e)
            (enum-set-subset? (c '(red blue)) (c '(blue red)))
            (enum-set-subset? (c '(red blue)) (c '(red)))
            (enum-set=? (c '(red blue)) (c '(blue red)))))
        '(#t #f #t #t #f #t)))
    (assert 
      (trace-equal? 
        (let* ([e (make-enumeration '(red green blue))]
               [c (enum-set-constructor e)])
          (list 
            (enum-set->list (enum-set-union (c '(blue)) (c '(red))))
            (enum-set->list (enum-set-intersection (c '(red green)) (c '(red blue))))
            (enum-set->list (enum-set-difference (c '(red green)) (c '(red blue))))
            (enum-set->list (enum-set-complement (c '(red))))))
        '((red blue) (red) (green) (green blue))))
    (assert 
      (trace-equal? 
        (let* ([e1 (make-enumeration '(red green blue black))]
               [e2 (make-enumeration '(red black white))])
          (enum-set->list (enum-set-projection e1 e2)))
        '(red black)))))
