;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: keyword objects
;;;Date: Sat Mar 10, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.keywords)
  (export
    symbol->keyword
    keyword->symbol
    keyword?
    keyword=?
    keyword-hash)
  (import (except (ikarus)
		  symbol->keyword
		  keyword->symbol
		  keyword?
		  keyword=?
		  keyword-hash)
    (vicare syntactic-extensions))


(define-struct keyword
  (symbol))

;; (define (%keyword-printer S port sub-printer)
;;   (define-inline (%display thing)
;;     (display thing port))
;;   (%display "#:")
;;   (%display (keyword-symbol S)))

(define-argument-validation (symbol who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as argument" obj))

(define-argument-validation (keyword who obj)
  (keyword? obj)
  (assertion-violation who "expected keyword as argument" obj))

(define (symbol->keyword S)
  (define who 'symbol->keyword)
  (with-arguments-validation (who)
      ((symbol S))
    (make-keyword S)))

(define (keyword->symbol K)
  (define who 'keyword->symbol)
  (with-arguments-validation (who)
      ((keyword	K))
    (keyword-symbol K)))

(define (keyword=? K1 K2)
  (and (keyword? K1)
       (keyword? K2)
       (or (eq? K1 K2)
	   (eq? (keyword-symbol K1)
		(keyword-symbol K2)))))

(define (keyword-hash K)
  (define who 'keyword->symbol)
  (with-arguments-validation (who)
      ((keyword	K))
    (symbol-hash (keyword-symbol K))))


;;;; done

;;(set-rtd-printer! (type-descriptor keyword)	%keyword-printer)

)

;;; end of file
