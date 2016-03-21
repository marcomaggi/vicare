;;; -*- coding: utf-8-unix -*-
;;;
;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggo <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus booleans)
  (export
    list-of-booleans?

    boolean=?			boolean!=?
    boolean<?			boolean>?
    boolean<=?			boolean>=?

    boolean-min			boolean-max

    ;; unsafe bindings for (vicare system $booleans)
    $boolean=			$boolean!=
    $boolean<			$boolean>
    $boolean<=			$boolean>=
    $boolean-min		$boolean-max)
  (import (except (vicare)
		  list-of-booleans?

		  boolean=?			boolean!=?
		  boolean<?			boolean>?
		  boolean<=?			boolean>=?

		  boolean-min			boolean-max)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate))


;;;; predicates

(define-list-of-type-predicate list-of-booleans? boolean?)


;;;; comparison

(define-equality/sorting-predicate boolean=?	$boolean=	boolean?)
(define-equality/sorting-predicate boolean<?	$boolean<	boolean?)
(define-equality/sorting-predicate boolean<=?	$boolean<=	boolean?)
(define-equality/sorting-predicate boolean>?	$boolean>	boolean?)
(define-equality/sorting-predicate boolean>=?	$boolean>=	boolean?)
(define-inequality-predicate       boolean!=?	$boolean!=	boolean?)

(define ($boolean= bo1 bo2)
  (eq? bo1 bo2))

(define ($boolean!= bo1 bo2)
  (not (eq? bo1 bo2)))

;; we artificially define: #f < #t

(define ($boolean< bo1 bo2)
  ;;True if:
  ;;
  ;;   BO1 == #f
  ;;   BO2 == #t
  ;;
  (and (not bo1) bo2))

(define ($boolean> bo1 bo2)
  ;;True if:
  ;;
  ;;   BO1 == #t
  ;;   BO2 == #f
  ;;
  (and bo1 (not bo2)))

(define ($boolean<= bo1 bo2)
  (or (eq? bo1 bo2)
      ($boolean< bo1 bo2)))

(define ($boolean>= bo1 bo2)
  (or (eq? bo1 bo2)
      ($boolean> bo1 bo2)))


;;;; min max

(define-min/max-comparison boolean-max $boolean-max boolean?)
(define-min/max-comparison boolean-min $boolean-min boolean?)

(define ($boolean-min str1 str2)
  (if ($boolean< str1 str2) str1 str2))

(define ($boolean-max str1 str2)
  (if ($boolean< str1 str2) str2 str1))


;;;; done

#| end of library |# )

;;; end of file
