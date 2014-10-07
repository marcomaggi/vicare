;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; Introduction
;;
;;Input to cogen is <Program>:
;;
;;  <Expr> ::= (constant x)
;;           | (var)
;;           | (primref name)
;;           | (bind var* <Expr>* <Expr>)
;;           | (fix var* <FixRhs>* <Expr>)
;;           | (conditional <Expr> <Expr> <Expr>)
;;           | (seq <Expr> <Expr>)
;;           | (closure-maker <codeloc> <var>*)  ; thunk special case
;;           | (forcall "name" <Expr>*)
;;           | (funcall <Expr> <Expr>*)
;;           | (primopcall <primop> <Expr>*)
;;           | (jmpcall <label> <Expr> <Expr>*)
;;  <codeloc> ::= (code-loc <label>)
;;  <clambda> ::= (clambda <label> <case>* <cp> <freevar>*)
;;  <case>    ::= (clambda-case <info> <body>)
;;  <info>    ::= (clambda-info label <arg var>* proper)
;;  <Program> ::= (codes <clambda>* <Expr>)


(module (alt-cogen
	 refresh-common-assembly-subroutines-cached-labels!
	 sl-apply-label
	 specify-representation
	 impose-calling-convention/evaluation-order
	 assign-frame-sizes
	 color-by-chaitin
	 flatten-codes)

  (define (alt-cogen x)
    (let* ((x  (specify-representation x))
	   (x  (impose-calling-convention/evaluation-order x))
	   (x  (assign-frame-sizes x))
	   (x  (color-by-chaitin x))
	   (ls (flatten-codes x)))
      ls))


;;;; syntax helpers

(define-syntax multiple-forms-sequence
  (syntax-rules ()
    ((_ ?expr)
     ?expr)
    ((_ ?expr ... ?last-expr)
     (make-seq (multiple-forms-sequence ?expr ...) ?last-expr))))


;;;; high-level assembly instructions

(define (asm op . rand*)
  ;;Build  and  return  recordised  call   which  performs  the  high-level  Assembly
  ;;instruction OP applying it to the arguments RAND*.
  ;;
  (make-asmcall op rand*))

(define (nop)
  ;;Build  and  return  recordised  call   representing  the  dummy  instruction  "no
  ;;operation".
  ;;
  (asm 'nop))

(define (interrupt)
  ;;Build and  return recordised  call representing  a jump  to a  SHORTCUT interrupt
  ;;handler.
  ;;
  ;;NOTE This  function is shadowed in  the pass "specify representation"  by a local
  ;;INTERRUPT function.
  ;;
  (asm 'interrupt))


;;;; include some external code for compiler passes and modules

(include "ikarus.compiler.scheme-objects-layout.scm"		#t)
(include "ikarus.compiler.intel-assembly.scm"			#t)
(include "ikarus.compiler.common-assembly-subroutines.scm"	#t)

(include "ikarus.compiler.pass-specify-representation.scm"	#t)
(include "ikarus.compiler.pass-impose-evaluation-order.scm"	#t)
(include "ikarus.compiler.pass-assign-frame-sizes.scm"		#t)
(include "ikarus.compiler.pass-color-by-chaitin.scm"		#t)
(include "ikarus.compiler.pass-flatten-codes.scm"		#t)


;;;; done

#| end of module: alt-cogen |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-primopcall		'scheme-indent-function 1)
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'assemble-sources		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; End:
