;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: debugging stuff for the compiler
;;;Date: Wed Oct 31, 2012
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare debugging compiler)
  (export
    compile-up-to

    ;; configuration
    $current-letrec-pass
    $check-for-illegal-letrec

    ;; compiler passes
    $compile-core-expr->code
    $recordize
    $optimize-direct-calls
    $optimize-letrec
    $source-optimize
    $rewrite-references-and-assignments
    $introduce-tags
    $introduce-vars
    $sanitize-bindings
    $optimize-for-direct-jumps
    $insert-global-assignments
    $convert-closures
    $optimize-closures/lift-codes
    $alt-cogen
    $assemble-sources

    ;; code generation passes
    $introduce-primcalls
    $eliminate-fix
    $insert-engine-checks
    $insert-stack-overflow-check
    $specify-representation
    $impose-calling-convention/evaluation-order
    $assign-frame-sizes
    $color-by-chaitin
    $flatten-codes

    ;; auxiliary functions
    $unparse-recordized-code
    $unparse-recordized-code/pretty)
  (import (vicare)
    (ikarus system $compiler))


;;;; code

(define-syntax compile-up-to
  (syntax-rules ($compile-core-expr->code
		 $recordize
		 $optimize-direct-calls
		 $optimize-letrec
		 $source-optimize
		 $rewrite-references-and-assignments
		 $introduce-tags
		 $introduce-vars
		 $sanitize-bindings
		 $optimize-for-direct-jumps
		 $insert-global-assignments
		 $convert-closures
		 $optimize-closures/lift-codes
		 $alt-cogen
		 $assemble-sources

		 $introduce-primcalls
		 $eliminate-fix
		 $insert-engine-checks
		 $insert-stack-overflow-check
		 $specify-representation
		 $impose-calling-convention/evaluation-order
		 $assign-frame-sizes
		 $color-by-chaitin
		 $flatten-codes)
    ;; compiler passes
    ((_ $compile-core-expr->code	?body)	(%compile-up-to  0 ?body))
    ((_ $recordize			?body)	(%compile-up-to  1 ?body))
    ((_ $optimize-direct-calls		?body)	(%compile-up-to  2 ?body))
    ((_ $optimize-letrec		?body)	(%compile-up-to  3 ?body))
    ((_ $source-optimize		?body)	(%compile-up-to  4 ?body))
    ((_ $rewrite-references-and-assignments ?body) (%compile-up-to  5 ?body))
    ((_ $introduce-tags			?body)	(%compile-up-to  6 ?body))
    ((_ $introduce-vars			?body)	(%compile-up-to  7 ?body))
    ((_ $sanitize-bindings		?body)	(%compile-up-to  8 ?body))
    ((_ $optimize-for-direct-jumps	?body)	(%compile-up-to  9 ?body))
    ((_ $insert-global-assignments	?body)	(%compile-up-to 10 ?body))
    ((_ $convert-closures		?body)	(%compile-up-to 11 ?body))
    ((_ $optimize-closures/lift-codes	?body)	(%compile-up-to 12 ?body))
    ((_ $alt-cogen			?body)	(%compile-up-to 13 ?body))
    ((_ $assemble-sources		?body)	(%compile-up-to 14 ?body))
    ;;code generation passes
    ((_ $introduce-primcalls		?body)	(%compile-up-to 15 ?body))
    ((_ $eliminate-fix			?body)	(%compile-up-to 16 ?body))
    ((_ $insert-engine-checks		?body)	(%compile-up-to 17 ?body))
    ((_ $insert-stack-overflow-check	?body)	(%compile-up-to 18 ?body))
    ((_ $specify-representation		?body)	(%compile-up-to 19 ?body))
    ((_ $impose-calling-convention/evaluation-order ?body) (%compile-up-to 19 ?body))
    ((_ $assign-frame-sizes		?body)	(%compile-up-to 20 ?body))
    ((_ $color-by-chaitin		?body)	(%compile-up-to 21 ?body))
    ((_ $flatten-codes			?body)	(%compile-up-to 22 ?body))
    ))

;;; --------------------------------------------------------------------

(define (%compile-up-to requested-idx body)
  (define-syntax doit
    (syntax-rules ()
      ((_ ?R ?pass-idx ?pass)
       (if (fx<= ?pass-idx requested-idx)
	   (?pass ?R)
	 ?R))))
  (if (fx= 0 requested-idx)
      ($compile-core-expr->code body)
    (let* ((R (doit body 1 $recordize))
	   (R (doit R  2 $optimize-direct-calls))
	   (R (doit R  3 $optimize-letrec))
	   (R (doit R  4 $source-optimize))
	   (R (doit R  5 $rewrite-references-and-assignments))
	   (R (doit R  6 $introduce-tags))
	   (R (doit R  7 $introduce-vars))
	   (R (doit R  8 $sanitize-bindings))
	   (R (doit R  9 $optimize-for-direct-jumps))
	   (R (doit R 10 $insert-global-assignments))
	   (R (doit R 12 $convert-closures))
	   (R (doit R 12 $optimize-closures/lift-codes))
	   (R (if (or (fx= 13 requested-idx)
		      (fx= 14 requested-idx))
		  ($alt-cogen R)
		R)))
      (if (fx= 14 requested-idx)
	  ($assemble-sources R)
	;;code generation passes
	(let* ((R (doit R 15 $introduce-primcalls))
	       (R (doit R 16 $eliminate-fix))
	       (R (doit R 17 $insert-engine-checks))
	       (R (doit R 18 $insert-stack-overflow-check))
	       (R (doit R 19 $specify-representation))
	       (R (doit R 20 $impose-calling-convention/evaluation-order))
	       (R (doit R 21 $assign-frame-sizes))
	       (R (doit R 22 $color-by-chaitin))
	       (R (doit R 23 $flatten-codes)))
	  R)))))


;;;; done

)

;;; end of file
