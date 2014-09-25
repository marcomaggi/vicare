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


;;;; helpers

(define-syntax multiple-forms-sequence
  (syntax-rules ()
    ((_ ?expr)
     ?expr)
    ((_ ?expr ... ?last-expr)
     (make-seq (multiple-forms-sequence ?expr ...) ?last-expr))))


;;;; some external code

(include "ikarus.compiler.scheme-objects-layout.scm" #t)
(include "ikarus.compiler.common-assembly-subroutines.scm" #t)
(include "ikarus.compiler.pass-specify-representation.scm" #t)


;;;; some Intel CPU registers stuff

(module INTEL-CPU-STUFF
  (ARGC-REGISTER
   RETURN-VALUE-REGISTER
   CP-REGISTER
   PARAMETER-REGISTERS
   ALL-REGISTERS
   NON-8BIT-REGISTERS
   %cpu-register-name->index)
  ;;On the Intel architecture, the CPU registers have special use:
  ;;
  ;;APR = %ebp		allocation pointer
  ;;ESP = %esp		stack pointer
  ;;PCR = %esi		pointer to PCB
  ;;CPR = %edi		pointer to closure
  ;;

  ;;Arguments  count register.   Upon  entering a  function: it  must  hold a  fixnum
  ;;representing zero or the negated number of arguments.
  ;;
  (define-constant ARGC-REGISTER '%eax)

  ;;When a  function returns a single  Scheme object: the immediate  Scheme object or
  ;;the tagged pointer to the Scheme object is stored in this register.
  ;;
  ;;When a  function returns zero, two  or more values: the  immediate Scheme objects
  ;;and/or the tagged  pointers to the Scheme  objects are stored on  the stack; this
  ;;register holds zero or a fixnum representing the negated number of objects.
  ;;
  (define-constant RETURN-VALUE-REGISTER '%eax)

  ;;Closure  Pointer Register  (CPR).  Upon  entering a  function: a  pointer to  the
  ;;closure  object being  executed  is loaded  from "some  machine  word" into  this
  ;;register; this  way the body  of the  function can access  the value of  the free
  ;;variables.
  ;;
  (define-constant CP-REGISTER '%edi)

  ;;The list of CPU registers the function  caller uses to hand special parameters to
  ;;the callee function.  The first item *must* be the CP-REGISTER.
  ;;
  (define-constant PARAMETER-REGISTERS
    `(,CP-REGISTER))

  ;;The  list of  CPU registers  that the  code in  a code  object can  use to  store
  ;;temporary results.
  ;;
  (define-constant ALL-REGISTERS
    (boot.case-word-size
     ((32)
      '(%eax %edi %ebx %edx %ecx))
     ((64)
      '(%eax %edi %ebx %edx %ecx %r8 %r9 %r10 %r11 %r14 %r15))))

  (define-constant NON-8BIT-REGISTERS
    (boot.case-word-size
     ((32)	'(%edi))
     ((64)	'(%edi))))

  (module (%cpu-register-name->index)

    (define-constant INTEL-CPU-REGISTER/INDEX-MAP
      '((%eax . 0)
	(%edi . 1)
	(%ebx . 2)
	(%edx . 3)
	(%ecx . 4)
	(%esi . 5)
	(%esp . 6)
	(%ebp . 7)))

    (define* (%cpu-register-name->index x)
      (cond ((assq x INTEL-CPU-REGISTER/INDEX-MAP)
	     => cdr)
	    (else
	     (compiler-internal-error __who__
	       "expected symbol representing an Intel CPU register name (lower-case)"
	       x))))

    #| end of module |# )

  #| end of module: INTEL-CPU-STUFF |# )


(module ListySet
  ;;This module implements sets of bits; each  set is a proper list of items, wrapped
  ;;by a struct.
  ;;
  ;;This module has the same API of the module IntegerSet.
  ;;
  (make-empty-set
   singleton
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union
   set->list		list->set)

  (define-struct set
    ;;Wrapper for a list of elements used as a set.
    ;;
    (v
		;The list of elements in the set.
     ))

;;; --------------------------------------------------------------------

  (define-inline (make-empty-set)
    (make-set '()))

  (define (singleton x)
    (make-set (list x)))

  (define* (set-member? x {S set?})
    (memq x ($set-v S)))

  (define* (empty-set? {S set?})
    (null? ($set-v S)))

  (define* (set->list {S set?})
    ($set-v S))

  (define (list->set ls)
    (make-set ls))

  (define* (set-add x {S set?})
    (if (memq x ($set-v S))
	S
      (make-set (cons x ($set-v S)))))

  (define ($remq x ell)
    ;;Remove X from the list ELL.
    ;;
    (cond ((pair? ell)
	   (cond ((eq? x (car ell))
		  (cdr ell))
		 (else
		  (cons (car ell) ($remq x (cdr ell))))))
	  (else
	   ;;(assert (null? ell))
	   '())))

  (define* (set-rem x {S set?})
    (make-set ($remq x ($set-v S))))

  (module (set-difference)

    (define* (set-difference {S1 set?} {S2 set?})
      (make-set ($difference ($set-v S1) ($set-v S2))))

    (define ($difference ell1 ell2)
      ;;Remove from the list ELL1 all  the elements of the list ELL2.  Use
      ;;EQ? for comparison.
      ;;
      (if (pair? ell2)
	  ($difference ($remq (car ell2) ell1) (cdr ell2))
	ell1))

    #| end of module: set-difference |# )

  (module (set-union)

    (define* (set-union {S1 set?} {S2 set?})
      (make-set ($union ($set-v S1) ($set-v S2))))

    (define ($union S1 S2)
      (cond ((pair? S1)
	     (cond ((memq (car S1) S2)
		    ($union (cdr S1) S2))
		   (else
		    (cons (car S1) (union (cdr S1) S2)))))
	    (else
	     ;;(assert (null? S1))
	     S2)))

    #| end of module: set-union |# )

  #| end of module: ListySet |# )


(module IntegerSet
  ;;This module  implements sets of  bits; each set is  a nested hierarchy  of lists,
  ;;pairs and fixnums  interpreted as a tree; fixnums are  interpreted as bitvectors.
  ;;The empty set is the fixnum zero.
  ;;
  ;;To search for  a bit: we compute a  "bit index", then start from the  root of the
  ;;tree and: if  the index is even we go  left (the car), if the index  is odd we go
  ;;right (the cdr).
  ;;
  ;;This module has the same API of the module ListySet.
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

  #| end of module: IntegerSet |# )


(module ListyGraphs
  (empty-graph
   add-edge!
   empty-graph?
   print-graph
   node-neighbors
   delete-node!)
  (import ListySet)

  (define-struct graph
    (ls
		;A list of pairs representing the graph.
		;
		;The  car of  each  pair represents  a  graph's node:  a
		;struct  instance  of type  VAR  or  FVAR, or  a  symbol
		;representing a register.
		;
		;The  cdr  of  each  pair is  an  instance  of  ListySet
		;(whatever it  is defined in that  module), representing
		;the edges outgoing from the node.
		;
		;The  ListySet contains  the target  nodes of  the edges
		;outgoing from  the node represented  by the car  of the
		;pair.
     ))

  (define-inline (empty-graph)
    (make-graph '()))

  (define (empty-graph? G)
    (andmap (lambda (x)
	      (empty-set? (cdr x)))
	    ($graph-ls G)))

  (module (add-edge!)

    (define (add-edge! G x y)
      (let ((ls ($graph-ls G)))
	(cond ((assq x ls)
	       => (lambda (p0)
		    (unless (set-member? y (cdr p0))
		      (set-cdr! p0 (set-add y (cdr p0)))
		      (cond ((assq y ls)
			     => (lambda (p1)
				  (set-cdr! p1 (set-add x (cdr p1)))))
			    (else
			     ($set-graph-ls! G (cons (cons y (single x)) ls)))))))
	      ((assq y ls)
	       => (lambda (p1)
		    (set-cdr! p1 (set-add x (cdr p1)))
		    ($set-graph-ls! G (cons (cons x (single y)) ls))))
	      (else
	       ($set-graph-ls! G (cons* (cons x (single y))
					(cons y (single x))
					ls))))))

    (define (single x)
      (set-add x (make-empty-set)))

    #| end of module: add-edge! |# )

  (define (print-graph G)
    (printf "G={\n")
    (parameterize ((print-gensym 'pretty))
      (for-each (lambda (x)
                  (let ((lhs  (car x))
			(rhs* (cdr x)))
                    (printf "  ~s => ~s\n"
                            (unparse-recordized-code lhs)
                            (map unparse-recordized-code (set->list rhs*)))))
        ($graph-ls G)))
    (printf "}\n"))

  (define (node-neighbors x G)
    (cond ((assq x ($graph-ls G))
	   => cdr)
	  (else
	   (make-empty-set))))

  (define (delete-node! x G)
    (let ((ls ($graph-ls G)))
      (cond ((assq x ls)
	     => (lambda (p)
		  (for-each (lambda (y)
			      (let ((p (assq y ls)))
				(set-cdr! p (set-rem x (cdr p)))))
		    (set->list (cdr p)))
		  (set-cdr! p (make-empty-set))))
	    (else
	     (void)))))

  #| end of module: ListyGraphs |# )


(module IntegerGraphs
  ;;This module is like ListyGraph, but it makes use of IntegerSet.
  ;;
  (empty-graph
   add-edge!
   empty-graph?
   print-graph
   node-neighbors
   delete-node!)
  (import IntegerSet)

  (define-struct graph
    (ls
		;A list of pairs representing the graph.
		;
		;The  car of  each  pair represents  a  graph's node:  a
		;struct  instance  of type  VAR  or  FVAR, or  a  symbol
		;representing a register.
		;
		;The  cdr of  each  pair is  an  instance of  IntegerSet
		;(whatever it  is defined in that  module), representing
		;the edges outgoing from the node.
		;
		;The  ListySet contains  the target  nodes of  the edges
		;outgoing from  the node represented  by the car  of the
		;pair.
     ))

  (define-inline (empty-graph)
    (make-graph '()))

  (define (empty-graph? g)
    (andmap (lambda (x)
	      (empty-set? (cdr x)))
	    ($graph-ls g)))

  (define (single x)
    (set-add x (make-empty-set)))

  (define (add-edge! g x y)
    (let ((ls ($graph-ls g)))
      (cond ((assq x ls)
	     => (lambda (p0)
		  (unless (set-member? y (cdr p0))
		    (set-cdr! p0 (set-add y (cdr p0)))
		    (cond ((assq y ls)
			   => (lambda (p1)
				(set-cdr! p1 (set-add x (cdr p1)))))
			  (else
			   ($set-graph-ls! g (cons (cons y (single x)) ls)))))))
	    ((assq y ls)
	     => (lambda (p1)
		  (set-cdr! p1 (set-add x (cdr p1)))
		  ($set-graph-ls! g (cons (cons x (single y)) ls))))
	    (else
	     ($set-graph-ls! g (cons* (cons x (single y))
				      (cons y (single x))
				      ls))))))

  (define (print-graph g)
    (printf "G={\n")
    (parameterize ((print-gensym 'pretty))
      (for-each (lambda (x)
                  (let ((lhs  (car x))
			(rhs* (cdr x)))
                    (printf "  ~s => ~s\n"
                            (unparse-recordized-code lhs)
                            (map unparse-recordized-code (set->list rhs*)))))
        ($graph-ls g)))
    (printf "}\n"))

  (define (node-neighbors x g)
    (cond ((assq x ($graph-ls g))
	   => cdr)
	  (else
	   (make-empty-set))))

  (define (delete-node! x g)
    (let ((ls ($graph-ls g)))
      (cond ((assq x ls)
	     => (lambda (p)
		  (for-each (lambda (y)
			      (let ((p (assq y ls)))
				(set-cdr! p (set-rem x (cdr p)))))
		    (set->list (cdr p)))
		  (set-cdr! p (make-empty-set))))
	    (else
	     (void)))))

  #| end of module: IntegerGraphs |# )


(module FRAME-CONFLICT-HELPERS
  (empty-var-set rem-var add-var union-vars mem-var? for-each-var init-vars!
   empty-nfv-set rem-nfv add-nfv union-nfvs mem-nfv? for-each-nfv init-nfv!
   empty-frm-set rem-frm add-frm union-frms mem-frm?
   empty-reg-set rem-reg add-reg union-regs mem-reg?
   reg?)
  (import IntegerSet)
  (module (%cpu-register-name->index)
    (import INTEL-CPU-STUFF))

  (define (add-frm x s)
    (set-add (fvar-idx x) s))

  (define-inline (rem-nfv x s)
    (remq1 x s))

  (define (init-var! x i)
    ($set-var-index! x i)
    ($set-var-var-move! x (empty-var-set))
    ($set-var-reg-move! x (empty-reg-set))
    ($set-var-frm-move! x (empty-frm-set))
    ($set-var-var-conf! x (empty-var-set))
    ($set-var-reg-conf! x (empty-reg-set))
    ($set-var-frm-conf! x (empty-frm-set)))

  (define (init-vars! ls)
    (let loop ((ls ls)
	       (i  0))
      (when (pair? ls)
	(init-var! (car ls) i)
	(loop (cdr ls) (fxadd1 i)))))

  (define (init-nfv! x)
    ($set-nfv-frm-conf! x (empty-frm-set))
    ($set-nfv-nfv-conf! x (empty-nfv-set))
    ($set-nfv-var-conf! x (empty-var-set)))

  (define-inline (reg? x)
    (symbol? x))

  (define-inline (empty-var-set)
    (make-empty-set))

  (define (add-var x s)
    (set-add (var-index x) s))

  (define (mem-var? x s)
    (set-member? (var-index x) s))

  (define (rem-var x s)
    (set-rem (var-index x) s))

  (define-inline (union-vars s1 s2)
    (set-union s1 s2))

  (define (for-each-var s varvec f)
    (for-each (lambda (i) (f (vector-ref varvec i)))
      (set->list s)))

  (define-inline (empty-reg-set)
    (make-empty-set))

  (define (add-reg x s)
    (set-add (%cpu-register-name->index x) s))

  (define (rem-reg x s)
    (set-rem (%cpu-register-name->index x) s))

  (define (mem-reg? x s)
    (set-member? (%cpu-register-name->index x) s))

  (define-inline (union-regs s1 s2)
    (set-union s1 s2))

  (define-inline (empty-frm-set)
    (make-empty-set))

  (define (mem-frm? x s)
    (set-member? (fvar-idx x) s))

  (define (rem-frm x s)
    (set-rem (fvar-idx x) s))

  (define-inline (union-frms s1 s2)
    (set-union s1 s2))

  (define-inline (empty-nfv-set)
    '())

  (define (add-nfv x s)
    (if (memq x s)
	s
      (cons x s)))

  (define-inline (mem-nfv? x s)
    (memq x s))

  (define (union-nfvs s1 s2)
    (let recur ((s1 s1)
		(s2 s2))
      (cond ((null? s1)
	     s2)
	    ((memq (car s1) s2)
	     (recur (cdr s1) s2))
	    (else
	     (cons (car s1)
		   (recur (cdr s1) s2))))))

  (define-inline (for-each-nfv s f)
    (for-each f s))

  #| end of module: FRAME-CONFLICT-HELPERS |# )


(define (compile-call-table frame-words-count livemask-vec multiarg-rp call-sequence)
  ;;To generate a call to a Scheme  function, we need to follow both the
  ;;protocol for  handling multiple return  values, and the  protocol to
  ;;expose  informations  about the  caller's  stack  frame for  garbage
  ;;collection purposes.
  ;;
  ;;This   means   generating   the   following   "calling"   chunk   of
  ;;pseudo-assembly to be included in the body of the caller function:
  ;;
  ;;     jmp L0
  ;;     livemask-bytes		;array of bytes            --
  ;;     framesize		;data word, a "long"       .
  ;;     rp_offset		;data word, a fixnum       . call table
  ;;     multi-value-rp		;data word, assembly label .
  ;;     pad-bytes                                         --
  ;;   L0:
  ;;     call function-address
  ;;   single-value-rp:		;single value return point
  ;;     ... instructions...
  ;;   multi-value-rp:		;multi value return point
  ;;     ... instructions...
  ;;
  ;;and remember that the "call" pushes on the stack the return address,
  ;;which is the label SINGLE-VALUE-RP.
  ;;
  ;;If the callee function returns a single value: it puts it in EAX and
  ;;performs a "ret"; this will make the execution flow jump back to the
  ;;entry point SINGLE-VALUE-RP.
  ;;
  ;;If the callee function wants to  return zero or 2 or more arguments:
  ;;it retrieves the address SINGLE-VALUE-RP  from the stack, adds to it
  ;;the    constant    DISP-MULTIVALUE-RP    obtaining    the    address
  ;;MULTI-VALUE-RP, then it performs a "jmp" directly to MULTI-VALUE-RP.
  ;;
  ;;The  argument  FRAME-WORDS-COUNT  must   be  a  non-negative  fixnum
  ;;representing the  number of words on  the stack frame of  the caller
  ;;function.
  ;;
  ;;The  argument  LIVEMASK-VEC  must  be   a  vector  of  fixnums  each
  ;;representing an  octect; the bits in  the octets are live  flags for
  ;;the  machine words  on the  stack of  the caller.   See the  garbage
  ;;collector for details.
  ;;
  ;;MULTIARG-RP must  be a symbolic expression  representing the address
  ;;of the multi  value return point: the  assembly label MULTI-VALUE-RP
  ;;in the  pseudo-code above.   This label must  be implemented  in the
  ;;assembly code generated for the caller.
  ;;
  ;;CALL-SEQUENCE  must  be  a   symbolic  expression  representing  the
  ;;assembly code needed to actually  call the closure object.  Examples
  ;;of this argument are:
  ;;
  ;;(call %ebx)
  ;;	Call the address in %ebx.
  ;;
  ;;(call (label ?target))
  ;;	Call a label generated at compile time.
  ;;
  ;;(call (disp off-closure-code CPR))
  ;;	Call the closure object referenced by the Closure Pointer
  ;;	Register (CPR).
  ;;
  ;;(call (disp EAX EBX))
  ;;	Call the entry point at offset EAX from the address in EBX.
  ;;
  ;;When the  execution flow arrives on  the calling chunk of  code: the
  ;;Scheme arguments for  the closure to call are already  on the stack;
  ;;the  Frame  Pointer Register  (FPR)  references  the uplevel  return
  ;;address.  The situation on the Scheme stack is as follows:
  ;;
  ;;* When FRAME-WORDS-COUNT is 2 or more:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    | <-- FPR .
  ;;   |---------------------------|         --
  ;;   | uplevel Scheme argument 0 |         .
  ;;   |---------------------------|         . stack frame described
  ;;   | uplevel Scheme argument 1 |         . by the call table
  ;;   |---------------------------|         .
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  in this picture FRAME-WORDS-COUNT  is 3, counting: return address,
  ;;  uplevel Scheme argument 0, uplevel Scheme argument 1.
  ;;
  ;;   Before executing  the  "call" assembly  instruction:  we need  to
  ;;  adjust the FPR so that  it references the machine word right above
  ;;  the "empty word":
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   | uplevel Scheme argument 0 |         .
  ;;   |---------------------------|         .
  ;;   | uplevel Scheme argument 1 | <- FPR  . stack frame described
  ;;   |---------------------------|         . by the call table
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  so that right after the "call" the stack looks like this:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   | uplevel Scheme argument 0 |         .
  ;;   |---------------------------|         .
  ;;   | uplevel Scheme argument 1 |         . stack frame described
  ;;   |---------------------------|         . by the call table
  ;;   |       return address      | <- FPR  .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  whenever  the execution flow returns  here we will have  to adjust
  ;;   back the  FPR so  that it  again references  the "uplevel  return
  ;;  address".
  ;;
  ;;* When FRAME-WORDS-COUNT is 1:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    | <- FPR  .
  ;;   |---------------------------|         --
  ;;   |        empty word         |         .  stack frame described
  ;;   |---------------------------|         -- by the call table
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  only the return address is the frame; we do not need to adjust the
  ;;  FPR, rather we just do the "call" so that right after it the stack
  ;;  looks like this:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   |        empty word         | <- FPR  .  stack frame described
  ;;   |---------------------------|         -- by the call table
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;* When FRAME-WORDS-COUNT is 0:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   |   uplevel Scheme object   |         .
  ;;   |---------------------------|         .
  ;;                ...                      .
  ;;   |---------------------------|         . stack frame described
  ;;   |   uplevel Scheme object   |         . by the call table
  ;;   |---------------------------|         .
  ;;   |    runtime frame size     | <- FPR  .
  ;;   |---------------------------|         .
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  an  unspecified number of Scheme  objects is on the  stack and FPR
  ;;  references a fixnum representing the negated number of such words;
  ;;  the number of Scheme objects on  the stack is not known at compile
  ;;  time, rather it is computed at runtime.  We just need to perform a
  ;;  "call"  instruction, so that right  after it the stack  looks like
  ;;  this:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   |   uplevel Scheme object   |         .
  ;;   |---------------------------|         .
  ;;                ...                      .
  ;;   |---------------------------|         . stack frame described
  ;;   |   uplevel Scheme object   |         . by the call table
  ;;   |---------------------------|         .
  ;;   |    runtime frame size     | <- FPR  .
  ;;   |---------------------------|         .
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  (let ((L_CALL (label (gensym "call_label"))))
    (define %adjust-frame-pointer-register
      (let ((FPR-DELTA (if (or (fxzero? frame-words-count)
			       (fx=? frame-words-count 1))
			   #f
			 (fx* (fxsub1 frame-words-count) wordsize))))
	(lambda (asm-instr)
	  (if FPR-DELTA
	      (list asm-instr FPR-DELTA fpr)
	    ;;NOP generates no assembly code.
	    '(nop)))))
    (list 'seq
	  (%adjust-frame-pointer-register 'subl)
	  (jmp L_CALL)
	  `(byte-vector ,livemask-vec)
	  `(int ,(* frame-words-count wordsize))
	  '(current-frame-offset)
	  multiarg-rp
	  `(pad ,call-instruction-size ,L_CALL ,call-sequence)
	  (%adjust-frame-pointer-register 'addl))))



(include "ikarus.compiler.pass-impose-order.scm"       #t)
(include "ikarus.compiler.pass-assign-frame-sizes.scm" #t)
(include "ikarus.compiler.pass-color-by-chaitin.scm"   #t)
(include "ikarus.compiler.pass-flatten-codes.scm"      #t)


;;;; done

#| end of module: alt-cogen |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-primcall 'scheme-indent-function 1)
;; eval: (put 'assemble-sources 'scheme-indent-function 1)
;; eval: (put 'make-conditional 'scheme-indent-function 2)
;; eval: (put 'struct-case 'scheme-indent-function 1)
;; End:
