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


(module INTEGER-SET
  ;;This module  implements sets of  bits; each set is  a nested hierarchy  of lists,
  ;;pairs and fixnums  interpreted as a tree; fixnums are  interpreted as bitvectors.
  ;;The empty set is the fixnum zero.
  ;;
  ;;To search for  a bit: we compute a  "bit index", then start from the  root of the
  ;;tree and: if  the index is even we go  left (the car), if the index  is odd we go
  ;;right (the cdr).
  ;;
  ;;This module has the same API of the module LISTY-SET.
  ;;
  (make-empty-set
   singleton
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union
   set->list		list->set)

;;; --------------------------------------------------------------------

  (define-inline-constant BITS
    28)

  (define-syntax-rule (make-empty-set)
    0)

  (define-syntax-rule ($index-of N)
    ;;Given a set element N to be added  to, or searched into, a set: return a fixnum
    ;;representing the "index" of the fixnum in which N should be stored.
    ;;
    (fxquotient N BITS))

  (define ($mask-of n)
    ;;Given a set element N to be added  to, or searched into, a set: return a fixnum
    ;;representing the bitmask of N for the fixnum in which N should be stored.
    ;;
    (fxsll 1 (fxremainder n BITS)))

  (define (singleton N)
    ;;Return a set containing only N.
    ;;
    (set-add N (make-empty-set)))

;;; --------------------------------------------------------------------

  (define (empty-set? S)
    (eqv? S 0))

  (define* (set-member? {N fixnum?} SET)
    (let loop ((SET SET)
	       (idx ($index-of N))
	       (msk ($mask-of  N))) ;this never changes in the loop
      (cond ((pair? SET)
	     (if (fxeven? idx)
		 (loop (car SET) (fxsra idx 1) msk)
	       (loop (cdr SET) (fxsra idx 1) msk)))
	    ((fxzero? idx)
	     (fx=? msk (fxlogand SET msk)))
	    (else #f))))

  (define* (set-add {N fixnum?} SET)
    (let recur ((SET SET)
		(idx ($index-of N))
		(msk ($mask-of  N))) ;this never changes in the loop
      (cond ((pair? SET)
	     (if (fxeven? idx)
		 (let* ((a0 (car SET))
			(a1 (recur a0 (fxsra idx 1) msk)))
		   (if (eq? a0 a1)
		       SET
		     (cons a1 (cdr SET))))
	       (let* ((d0 (cdr SET))
		      (d1 (recur d0 (fxsra idx 1) msk)))
		 (if (eq? d0 d1)
		     SET
		   (cons (car SET) d1)))))
	    ((fxzero? idx)
	     (fxlogor SET msk))
	    (else
	     (if (fxeven? idx)
		 (cons (recur SET (fxsra idx 1) msk) 0)
	       (cons SET (recur 0 (fxsra idx 1) msk)))))))

  (define (cons^ A D)
    (if (and (eq? D 0)
	     (fixnum? A))
        A
      (cons A D)))

  (define* (set-rem {N fixnum?} SET)
    (let recur ((SET SET)
		(idx ($index-of N))
		(msk ($mask-of  N)))	;this never changes in the loop
      (cond ((pair? SET)
	     (if (fxeven? idx)
		 (let* ((a0 (car SET))
			(a1 (recur a0 (fxsra idx 1) msk)))
		   (if (eq? a0 a1)
		       SET
		     (cons^ a1 (cdr SET))))
	       (let* ((d0 (cdr SET))
		      (d1 (recur d0 (fxsra idx 1) msk)))
		 (if (eq? d0 d1)
		     SET
		   (cons^ (car SET) d1)))))
	    ((fxzero? idx)
	     (fxlogand SET (fxlognot msk)))
	    (else
	     SET))))

  (module (set-union)

    (define (set-union S1 S2)
      (if (pair? S1)
	  (if (pair? S2)
	      (if (eq? S1 S2)
		  S1
		(cons (set-union (car S1) (car S2))
		      (set-union (cdr S1) (cdr S2))))
	    (let ((a0 (car S1)))
	      (let ((a1 (set-union^ a0 S2)))
		(if (eq? a0 a1) S1 (cons a1 (cdr S1))))))
	(if (pair? S2)
	    (let ((a0 (car S2)))
	      (let ((a1 (set-union^ a0 S1)))
		(if (eq? a0 a1) S2 (cons a1 (cdr S2)))))
	  (fxlogor S1 S2))))

    (define (set-union^ S1 M2)
      (if (pair? S1)
	  (let* ((a0 (car S1))
		 (a1 (set-union^ a0 M2)))
	    (if (eq? a0 a1)
		S1
	      (cons a1 (cdr S1))))
	(fxlogor S1 M2)))

    #| end of module: set-union |# )

  (module (set-difference)

    (define (set-difference s1 s2)
      (cond ((pair? s1)
	     (if (pair? s2)
		 (if (eq? s1 s2)
		     0
		   (cons^ (set-difference (car s1) (car s2))
			  (set-difference (cdr s1) (cdr s2))))
	       (let* ((a0 (car s1))
		      (a1 (set-difference^ a0 s2)))
		 (if (eq? a0 a1)
		     s1
		   (cons^ a1 (cdr s1))))))
	    ((pair? s2)
	     (set-difference^^ s1 (car s2)))
	    (else
	     (fxlogand s1 (fxlognot s2)))))

    (define (set-difference^ S1 M2)
      (if (pair? S1)
	  (let* ((a0 (car S1))
		 (a1 (set-difference^ a0 M2)))
	    (if (eq? a0 a1)
		S1
	      (cons^ a1 (cdr S1))))
	(fxlogand S1 (fxlognot M2))))

    (define (set-difference^^ M1 S2)
      (if (pair? S2)
	  (set-difference^^ M1 (car S2))
	(fxlogand M1 (fxlognot S2))))

    #| end of module: set-difference |# )

  (module (list->set)

    (define* (list->set {ls list-of-fixnums?})
      (let recur ((ls ls)
		  (S  0))
	(if (pair? ls)
	    (recur (cdr ls) (set-add (car ls) S))
	  S)))

    (define (list-of-fixnums? obj)
      (and (list? obj)
	   (for-all fixnum? obj)))

    #| end of module: list->set |# )

  (define (set->list S)
    (let outer ((i  0)
		(j  1)
		(S  S)
		(ac '()))
      (if (pair? S)
	  (outer i (fxsll j 1) (car S)
		 (outer (fxlogor i j) (fxsll j 1) (cdr S) ac))
	(let inner ((i  (fx* i BITS))
		    (m  S)
		    (ac ac))
	  (if (fxeven? m)
	      (if (fxzero? m)
		  ac
		(inner (fxadd1 i) (fxsra m 1) ac))
	    (inner (fxadd1 i) (fxsra m 1) (cons i ac)))))))

  #| end of module: INTEGER-SET |# )


(module FRAME-CONFLICT-HELPERS
  (empty-var-set rem-var add-var union-vars mem-var? for-each-var init-var*!
   empty-nfv-set rem-nfv add-nfv union-nfvs mem-nfv? for-each-nfv init-nfv!
   empty-frm-set rem-frm add-frm union-frms mem-frm?
   empty-reg-set rem-reg add-reg union-regs mem-reg?)
  (import INTEGER-SET)

;;; --------------------------------------------------------------------
;;; VAR structs

  (module (init-var*!)

    (case-define init-var*!
      ((ls)
       (init-var*! ls 0))
      ((ls idx)
       (when (pair? ls)
	 (init-var!  (car ls) idx)
	 (init-var*! (cdr ls) (fxadd1 idx)))))

    (define (init-var! x i)
      ($set-var-index! x i)
      ($set-var-var-move! x (empty-var-set))
      ($set-var-reg-move! x (empty-reg-set))
      ($set-var-frm-move! x (empty-frm-set))
      ($set-var-var-conf! x (empty-var-set))
      ($set-var-reg-conf! x (empty-reg-set))
      ($set-var-frm-conf! x (empty-frm-set)))

    #| end of module |# )

  (define-syntax-rule (empty-var-set)
    ;;Build and return a new, empty VS set.
    ;;
    (make-empty-set))

  (define (add-var x vs)
    ;;Add the VAR struct X to the set VS.  Return the new set.
    ;;
    (set-add (var-index x) vs))

  (define (rem-var x vs)
    ;;Remove the VAR struct X from the set VS.  Return the new set.
    ;;
    (set-rem (var-index x) vs))

  (define (mem-var? x vs)
    ;;Return true if the VAR struct X is a member of te set VS.
    ;;
    (set-member? (var-index x) vs))

  (define-syntax-rule (union-vars ?vs1 ?vs2)
    ;;Build  and return  a new  VS set  holding  all the  members of  ?VS1 and  ?VS2;
    ;;duplicate members are included only once.
    ;;
    (set-union ?vs1 ?vs2))

  (define (for-each-var vs varvec func)
    (for-each (lambda (i)
		(func (vector-ref varvec i)))
      (set->list vs)))

;;; --------------------------------------------------------------------
;;; current frame stack operands

  (define-syntax-rule (empty-frm-set)
    ;;Build and return a new, empty FS set.
    ;;
    (make-empty-set))

  (define (add-frm x fs)
    ;;Add the FVAR struct X to the set FS.  Return the new set.
    ;;
    (set-add (fvar-idx x) fs))

  (define (rem-frm x fs)
    ;;Remove the FVAR struct X from the set FS.  Return the new set.
    ;;
    (set-rem (fvar-idx x) fs))

  (define (mem-frm? x fs)
    ;;Return true if the FVAR struct X is a member of the set FS.
    ;;
    (set-member? (fvar-idx x) fs))

  (define-syntax-rule (union-frms ?fs1 ?fs2)
    ;;Build  and return  a new  FS set  holding  all the  members of  ?FS1 and  ?FS2;
    ;;duplicate members are included only once.
    ;;
    (set-union ?fs1 ?fs2))

;;; --------------------------------------------------------------------
;;; CPU registers

  (define-syntax-rule (empty-reg-set)
    ;;Build and return a new, empty RS set.
    ;;
    (make-empty-set))

  (define (add-reg x rs)
    ;;Add the CPU register symbol name X to the set RS.  Return the new set.
    ;;
    (module (%cpu-register-name->index)
      (import INTEL-ASSEMBLY-CODE-GENERATION))
    (set-add (%cpu-register-name->index x) rs))

  (define (rem-reg x rs)
    ;;Remove the CPU register symbol name X from the set RS.  Return the new set.
    ;;
    (module (%cpu-register-name->index)
      (import INTEL-ASSEMBLY-CODE-GENERATION))
    (set-rem (%cpu-register-name->index x) rs))

  (define (mem-reg? x rs)
    ;;Return true if the CPU register symbol name X is a member of the set RS.
    ;;
    (module (%cpu-register-name->index)
      (import INTEL-ASSEMBLY-CODE-GENERATION))
    (set-member? (%cpu-register-name->index x) rs))

  (define-syntax-rule (union-regs ?rs1 ?rs2)
    ;;Build  and return  a new  RS set  holding  all the  members of  ?RS1 and  ?RS2;
    ;;duplicate members are included only once.
    ;;
    (set-union ?rs1 ?rs2))

;;; --------------------------------------------------------------------
;;; next frame stack operands

  (define (init-nfv! x)
    ($set-nfv-frm-conf! x (empty-frm-set))
    ($set-nfv-nfv-conf! x (empty-nfv-set))
    ($set-nfv-var-conf! x (empty-var-set)))

  (define-syntax-rule (empty-nfv-set)
    ;;Build and return a new, empty NS set.
    ;;
    '())

  (define (add-nfv x ns)
    ;;Add the NFV struct X to the set NS.  Return the new set.
    ;;
    (if (memq x ns)
	ns
      (cons x ns)))

  (define-syntax-rule (rem-nfv ?x ?ns)
    ;;Remove the NFV struct ?X from the set ?NS.  Return the new set.
    ;;
    (remq1 ?x ?ns))

  (define-syntax-rule (mem-nfv? ?x ?ns)
    ;;Return true if the NFV struct ?X is a member of the set ?NS.
    ;;
    (memq ?x ?ns))

  (define (union-nfvs ns1 ns2)
    ;;Build and return a new NS set holding all the members of NS1 and NS2; duplicate
    ;;members are included only once.
    ;;
    (let recur ((ns1 ns1)
		(ns2 ns2))
      (cond ((null? ns1)
	     ns2)
	    ((memq (car ns1) ns2)
	     (recur (cdr ns1) ns2))
	    (else
	     (cons (car ns1)
		   (recur (cdr ns1) ns2))))))

  (define-syntax-rule (for-each-nfv ?ns ?func)
    (for-each ?func ?ns))

  #| end of module: FRAME-CONFLICT-HELPERS |# )


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
