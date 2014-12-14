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
;;;Copyright (C) 2012, 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus.keywords)
  (export
    symbol->keyword
    keyword->symbol
    keyword->string
    keyword?
    keyword=?
    keyword-hash

    (rename (make-keyword	$symbol->keyword))
    $keyword->symbol
    $keyword->string
    $keyword-hash
    $keyword=?)
  (import (except (vicare)
		  symbol->keyword
		  keyword->symbol
		  keyword->string
		  keyword?
		  keyword=?
		  keyword-hash)
    (only (vicare system $symbols)
	  $symbol->string)
    (only (vicare system $hashtables)
	  $symbol-hash))


(define-struct keyword
  (symbol))

(define* (symbol->keyword {S symbol?})
  (make-keyword S))

(define* (keyword->symbol {K keyword?})
  ($keyword-symbol K))

(define ($keyword->symbol K)
  ;;Remember that the unsafe operation $KEYWORD-SYMBOL is a syntax!!!
  ($keyword-symbol K))

(define* (keyword->string {K keyword?})
  ($keyword->string K))

(define ($keyword->string K)
  (string-append "#:" ($symbol->string ($keyword-symbol K))))

(define* (keyword=? {K1 keyword?} {K2 keyword?})
  ($keyword=? K1 K2))

(define ($keyword=? K1 K2)
  (or (eq? K1 K2)
      (eq? ($keyword->symbol K1)
	   ($keyword->symbol K2))))

(define* (keyword-hash {K keyword?})
  ($keyword-hash K))

(define ($keyword-hash K)
  ($symbol-hash ($keyword->symbol K)))


;;;; done

)

;;; end of file
