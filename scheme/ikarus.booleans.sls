;;; -*- coding: utf-8-unix -*-
;;;
;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>, 2010-2016
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
  (options typed-language)
  (export
    true?			false?
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
		  true?				false?
		  list-of-booleans?

		  boolean=?			boolean!=?
		  boolean<?			boolean>?
		  boolean<=?			boolean>=?

		  boolean-min			boolean-max)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define/checked-min/max-comparison
	  define/checked-equality/sorting-predicate
	  define/checked-inequality-predicate))


;;;; predicates

(define (true? x)
  (and (boolean? x)
       x))

(define (false? x)
  (and (boolean? x)
       (not x)))

(define-list-of-type-predicate list-of-booleans? boolean?)


;;;; comparison

(define/checked-equality/sorting-predicate boolean=?	$boolean=	<boolean>)
(define/checked-equality/sorting-predicate boolean<?	$boolean<	<boolean>)
(define/checked-equality/sorting-predicate boolean<=?	$boolean<=	<boolean>)
(define/checked-equality/sorting-predicate boolean>?	$boolean>	<boolean>)
(define/checked-equality/sorting-predicate boolean>=?	$boolean>=	<boolean>)
(define/checked-inequality-predicate       boolean!=?	$boolean!=	<boolean>)

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean= <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  (eq? bo1 bo2))

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean!= <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  (not (eq? bo1 bo2)))

;; we artificially define: #f < #t

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean< <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  ;;True if:
  ;;
  ;;   BO1 == #f
  ;;   BO2 == #t
  ;;
  (and (not bo1) bo2))

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean> <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  ;;True if:
  ;;
  ;;   BO1 == #t
  ;;   BO2 == #f
  ;;
  (and bo1 (not bo2)))

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean<= <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  (or (eq? bo1 bo2)
      ($boolean< bo1 bo2)))

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean>= <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  (or (eq? bo1 bo2)
      ($boolean> bo1 bo2)))


;;;; min max

(define/checked-min/max-comparison boolean-max $boolean-max <boolean>)
(define/checked-min/max-comparison boolean-min $boolean-min <boolean>)

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean-min <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  (if ($boolean< bo1 bo2) bo1 bo2))

;;FIXME This should also be a proper primitive operation.  (Marco Maggi; Wed Dec  7, 2016)
(define ({$boolean-max <boolean>} {bo1 <boolean>} {bo2 <boolean>})
  (if ($boolean< bo1 bo2) bo2 bo1))


;;;; done

#| end of library |# )

;;; end of file
