;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for compiler core primitives
;;Date: Mon Apr  4, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives compiler)
  (export typed-core-primitives.compiler)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.compiler)


;;;; syntax-case, safe procedures

(section

(declare-core-primitive initialise-compiler
    (safe)
  (signatures
   (()					=> ())))

;;; --------------------------------------------------------------------
;;; parameters

(declare-parameter current-letrec-pass				<symbol>)
(declare-parameter check-for-illegal-letrec)
(declare-parameter optimize-level				<non-negative-fixnum>)
(declare-parameter source-optimizer-passes-count		<non-negative-fixnum>)
(declare-parameter cp0-size-limit				<non-negative-fixnum>)
(declare-parameter cp0-effort-limit				<non-negative-fixnum>)
(declare-parameter perform-core-type-inference?)
(declare-parameter perform-unsafe-primrefs-introduction?)
(declare-parameter strip-source-info)
(declare-parameter generate-debug-calls)
(declare-parameter enabled-function-application-integration?)
(declare-parameter generate-descriptive-labels?)

(declare-parameter assembler-output)
(declare-parameter optimizer-output)

;;; --------------------------------------------------------------------
;;; condition objects

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    (()			=> (?type))))))))
  ;; (declare make-library-condition			&library-condition)
  ;; (declare make-module-condition			&module-condition)
  (declare make-compile-time-error			&compile-time-error)
  (declare make-compile-time-arity-error		&compile-time-arity-error)
  (declare make-compile-time-core-type-error		&compile-time-core-type-error)
  (declare make-compile-time-operand-core-type-error	&compile-time-operand-core-type-error)
  (declare make-compile-time-retval-core-type-error	&compile-time-retval-core-type-error)
  (declare make-compiler-internal-error			&compiler-internal-error)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?type)
		 (declare-type-predicate ?who	?type)))))
  ;; (declare library-condition?				&library-condition)
  ;; (declare module-condition?				&module-condition)
  (declare compile-time-error?				&compile-time-error)
  (declare compile-time-arity-error?			&compile-time-arity-error)
  (declare compile-time-core-type-error?		&compile-time-core-type-error)
  (declare compile-time-operand-core-type-error?	&compile-time-operand-core-type-error)
  (declare compile-time-retval-core-type-error?		&compile-time-retval-core-type-error)
  (declare compiler-internal-error?			&compiler-internal-error)
  #| end of LET-SYNTAX |# )

;; (declare-core-primitive library-condition-name
;;     (safe)
;;   (signatures
;;    ((&library-condition)		=> (<top>))))

;; (declare-core-primitive module-condition-name
;;     (safe)
;;   (signatures
;;    ((&module-condition)			=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive system-value-gensym
    (safe)
  (signatures
   (()			=> (<symbol>))))

(declare-core-primitive system-value
    (safe)
  (signatures
   ((<symbol>)		=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive compile-core-expr->code
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-recordize
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-optimize-direct-calls
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-optimize-letrec
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-source-optimize
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-rewrite-references-and-assignments
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-core-type-inference
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-introduce-unsafe-primrefs
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-introduce-vars
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-sanitize-bindings
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-optimize-for-direct-jumps
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-insert-global-assignments
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-introduce-closure-makers
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-optimize-combinator-calls/lift-clambdas
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-introduce-primitive-operation-calls
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-rewrite-freevar-references
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-insert-engine-checks
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-insert-stack-overflow-check
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-code-generation
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive assemble-sources
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive pass-specify-representation
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-impose-calling-convention/evaluation-order
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-assign-frame-sizes
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-color-by-chaitin
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive pass-flatten-codes
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive unparse-recordized-code
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive unparse-recordized-code/pretty
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive unparse-recordized-code/sexp
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
