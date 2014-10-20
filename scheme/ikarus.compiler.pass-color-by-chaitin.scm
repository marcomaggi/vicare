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


(module (color-by-chaitin)
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   asm-instr	code-loc	conditional
  ;;   constant		disp		fvar
  ;;   locals		nfv		non-tail-call
  ;;   asmcall		seq		shortcut
  ;;   var
  ;;
  ;;in addition CLOSURE-MAKER structs can appear in side CONSTANT structs.
  ;;
  (import INTEL-ASSEMBLY-CODE-GENERATION)

  (define-syntax __module_who__
    (identifier-syntax 'color-by-chaitin))

  (module (color-by-chaitin)
    ;;The purpose of this module is to apply the function %COLOR-PROGRAM below to all
    ;;the bodies.
    ;;
    (define (color-by-chaitin x)
      (struct-case x
	((codes code* body)
	 (make-codes (map E-clambda code*) (%color-program body)))))

    (define (E-clambda x)
      (struct-case x
	((clambda label case* cp freevar* name)
	 (make-clambda label (map E-clambda-clause case*) cp freevar* name))))

    (define (E-clambda-clause x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (%color-program body)))))

    (module (%color-program)

      (define (%color-program x)
	(module (list->set make-empty-set)
	  (import LISTY-SET))
	(struct-case x
	  ((locals x.vars x.body)
	   (let ((varvec     (car x.vars))
		 (spillable* (cdr x.vars)))
	     (let loop ((spillable*^   (list->set spillable*))
			(unspillable*  (make-empty-set))
			(body          x.body))
	       (receive (unspillable*^ body^)
		   (%add-unspillables unspillable* body)
		 (let ((G (%build-graph body^)))
		   (receive (spills spillable*^^ env)
		       (%color-graph spillable*^ unspillable*^ G)
		     (if (null? spills)
			 (%substitute env body^)
		       (let* ((env^   (%do-spill spills varvec))
			      (body^^ (%substitute env^ body^)))
			 (loop spillable*^^ unspillable*^ body^^)))))))))))

      (define (%do-spill spillable* varvec)
	(module (for-each-var add-frm rem-var mem-frm?)
	  (import FRAME-CONFLICT-SETS))
	(define (find/set-loc x)
	  (let loop ((i    1)
		     (conf ($var-frm-conf x)))
	    (let ((fv (mkfvar i)))
	      (if (mem-frm? fv conf)
		  (loop (fxadd1 i) conf)
		(begin
		  (for-each-var
		      ($var-var-conf x)
		      varvec
		    (lambda (y)
		      ($set-var-var-conf! y (rem-var x  ($var-var-conf y)))
		      ($set-var-frm-conf! y (add-frm fv ($var-frm-conf y)))))
		  ($set-var-loc! x fv)
		  (cons x fv))))))
	(map find/set-loc spillable*))

      #| end of module: %COLOR-PROGRAM |# )

    #| end of module: COLOR-BY-CHAITIN |# )


(module LISTY-SET
  ;;This module implements sets of bits; each  set is a proper list of items, wrapped
  ;;by a struct.
  ;;
  ;;This module has the same API of the module INTEGER-SET.
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

  (define-syntax-rule (make-empty-set)
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

  #| end of module: LISTY-SET |# )


(module GRAPHS
  ;;This module is like INTEGER-GRAPHS, but it makes use of LISTY-SET.
  ;;
  (empty-graph
   add-edge!
   empty-graph?
   print-graph
   node-neighbors
   delete-node!)
  (import LISTY-SET)

  (define-struct graph
    (ls
		;A list of pairs representing the graph.
		;
		;The car of each pair represents a graph's node: a struct instance of
		;type VAR or FVAR, or a symbol representing a register.
		;
		;The cdr  of each pair  is an instance  of LISTY-SET (whatever  it is
		;defined in  that module), representing  the edges outgoing  from the
		;node.
		;
		;The LISTY-SET contains  the target nodes of the  edges outgoing from
		;the node represented by the car of the pair.
     ))

  (define-syntax-rule (empty-graph)
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

  #| end of module: GRAPHS |# )


(module (%add-unspillables)

  (define-syntax __who__
    (identifier-syntax '%add-unspillables))

  (define (%add-unspillables un* x)
    ;;
    ;;A  lot  of  functions  are  nested  here  because  they  call  the
    ;;subfunction MKU, and the MKU needs to close upon the argument UN*.
    ;;

    (define (mku)
      (module (set-add)
	(import LISTY-SET))
      (receive-and-return (u)
	  (make-unique-var 'u)
	(set! un* (set-add u un*))))

    (module (E)

      (define (E x)
	;;unspillable effect
	(struct-case x
	  ((seq e0 e1)
	   (make-seq (E e0) (E e1)))

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (E e1) (E e2)))

	  ((asm-instr op a b)
	   (E-asm-instr op a b x))

	  ((asmcall op rands)
	   (case op
	     ((nop interrupt incr/zero? fl:single->double fl:double->single)
	      x)
	     (else
	      (compiler-internal-error __module_who__  __who__ "invalid op in" (unparse-recordized-code x)))))

	  ((non-tail-call)
	   x)

	  ((shortcut body handler)
	   ;;Do BODY first, then HANDLER.
	   (let ((body^ (E body)))
	     (make-shortcut body^ (E handler))))

	  (else
	   (compiler-internal-error __module_who__  __who__ "invalid effect" (unparse-recordized-code x)))))

      (define (E-asm-instr op a b x)
	(case op
	  ((load8 load32)
	   (%fix-address b (lambda (b)
			     (if (or (register? a) (var? a))
				 (make-asm-instr op a b)
			       (let ((u (mku)))
				 (make-seq (make-asm-instr op u b)
					   (E (make-asm-instr 'move a u))))))))

	  ((logor logxor logand int+ int- int* move int-/overflow int+/overflow int*/overflow)
	   (cond ((and (eq? op 'move)
		       (eq? a b))
		  ;;Source and dest are the same: do nothing.
		  (nop))
		 ((and (= wordsize 8)
		       (not (eq? op 'move))
		       (long-imm? b))
		  (let ((u (mku)))
		    (make-seq (E (make-asm-instr 'move u b))
			      (E (make-asm-instr op a u)))))
		 ((and (memq op '(int* int*/overflow))
		       (mem? a))
		  (let ((u (mku)))
		    (make-seq (make-seq (E (make-asm-instr 'move u a))
					(E (make-asm-instr op u b)))
			      (E (make-asm-instr 'move a u)))))
		 ((and (mem? a)
		       (not (small-operand? b)))
		  (let ((u (mku)))
		    (make-seq (E (make-asm-instr 'move u b))
			      (E (make-asm-instr op a u)))))
		 ((disp? a)
		  (let ((s0 (disp-objref a))
			(s1 (disp-offset a)))
		    (cond ((not (small-operand? s0))
			   (let ((u (mku)))
			     (make-seq (E (make-asm-instr 'move u s0))
				       (E (make-asm-instr op (make-disp u s1) b)))))
			  ((not (small-operand? s1))
			   (let ((u (mku)))
			     (make-seq (E (make-asm-instr 'move u s1))
				       (E (make-asm-instr op (make-disp s0 u) b)))))
			  ((small-operand? b) x)
			  (else
			   (let ((u (mku)))
			     (make-seq (E (make-asm-instr 'move u b))
				       (E (make-asm-instr op a u))))))))
		 ((disp? b)
		  (let ((s0 (disp-objref b))
			(s1 (disp-offset b)))
		    (cond ((not (small-operand? s0))
			   (let ((u (mku)))
			     (make-seq (E (make-asm-instr 'move u s0))
				       (E (make-asm-instr op a (make-disp u s1))))))
			  ((not (small-operand? s1))
			   (let ((u (mku)))
			     (make-seq (E (make-asm-instr 'move u s1))
				       (E (make-asm-instr op a (make-disp s0 u))))))
			  (else
			   x))))
		 (else
		  x)))

	  ((bswap!)
	   (if (mem? b)
	       (let ((u (mku)))
		 (make-seq (make-seq (E (make-asm-instr 'move u a))
				     (E (make-asm-instr 'bswap! u u)))
			   (E (make-asm-instr 'move b u))))
	     x))

	  ((cltd)
	   (unless (and (symbol? a)
			(symbol? b))
	     (compiler-internal-error __module_who__  __who__ "invalid args to cltd"))
	   x)

	  ((idiv)
	   (unless (symbol? a)
	     (compiler-internal-error __module_who__  __who__ "invalid arg to idiv"))
	   (if (or (var? b)
		   (symbol? b))
	       x
	     (let ((u (mku)))
	       (make-seq (E (make-asm-instr 'move u b))
			 (E (make-asm-instr 'idiv a u))))))

	  ((sll sra srl sll/overflow)
	   (unless (or (constant? b)
		       (eq? b ecx))
	     (compiler-internal-error __module_who__  __who__ "invalid shift" b))
	   x)

	  ((mset mset32 bset)
	   (if (not (small-operand? b))
	       (let ((u (mku)))
		 (make-seq (E (make-asm-instr 'move u b))
			   (E (make-asm-instr op a u))))
	     (check-disp a
			 (lambda (a)
			   (let ((s0 (disp-objref a))
				 (s1 (disp-offset a)))
			     (if (and (constant? s0)
				      (constant? s1))
				 (let ((u (mku)))
				   (make-seq (make-seq (E (make-asm-instr 'move u s0))
						       (E (make-asm-instr 'int+ u s1)))
					     (make-asm-instr op
							     (make-disp u (make-constant 0))
							     b)))
			       (make-asm-instr op a b)))))))

	  ((fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:load-single fl:store-single)
	   (check-disp-arg a (lambda (a)
			       (check-disp-arg b (lambda (b)
						   (make-asm-instr op a b))))))

	  ((fl:from-int fl:shuffle)
	   x)

	  (else
	   (compiler-internal-error __module_who__  __who__ "invalid effect op" op))))

      (define (%fix-address x kont)
	;;Recursive function.
	;;
	(if (disp? x)
	    (let ((s0 (disp-objref x))
		  (s1 (disp-offset x)))
	      (cond ((not (small-operand? s0))
		     (let ((u (mku)))
		       (make-seq (E (make-asm-instr 'move u s0))
				 (%fix-address (make-disp u s1) kont))))
		    ((not (small-operand? s1))
		     (let ((u (mku)))
		       (make-seq (E (make-asm-instr 'move u s1))
				 (%fix-address (make-disp s0 u) kont))))
		    (else
		     (kont x))))
	  (kont x)))

      #| end of module: E |# )

;;; --------------------------------------------------------------------

    (define (check-disp-arg x kont)
      (if (small-operand? x)
	  (kont x)
	(let ((u (mku)))
	  (make-seq (E (make-asm-instr 'move u x))
		    (kont u)))))

    (define (check-disp x kont)
      (struct-case x
	((disp a b)
	 (check-disp-arg a (lambda (a)
			     (check-disp-arg b (lambda (b)
						 (kont (make-disp a b)))))))
	(else
	 (kont x))))

;;; --------------------------------------------------------------------

    (define (P x)
      (struct-case x
	((constant)
	 x)

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (P e1) (P e2)))

	((seq e0 e1)
	 (make-seq (E e0) (P e1)))

	((asm-instr op a b)
	 (cond ((memq op '(fl:= fl:< fl:<= fl:> fl:>=))
		(if (mem? a)
		    (let ((u (mku)))
		      (make-seq (E (make-asm-instr 'move u a))
				(make-asm-instr op u b)))
		  x))
	       ((and (not (mem?           a))
		     (not (small-operand? a)))
		(let ((u (mku)))
		  (make-seq (E (make-asm-instr 'move u a))
			    (P (make-asm-instr op u b)))))
	       ((and (not (mem?           b))
		     (not (small-operand? b)))
		(let ((u (mku)))
		  (make-seq (E (make-asm-instr 'move u b))
			    (P (make-asm-instr op a u)))))
	       ((and (mem? a)
		     (mem? b))
		(let ((u (mku)))
		  (make-seq (E (make-asm-instr 'move u b))
			    (P (make-asm-instr op a u)))))
	       (else
		(check-disp a (lambda (a)
				(check-disp b (lambda (b)
						(make-asm-instr op a b))))))))

	((shortcut body handler)
	 ;;Do BODY first, then HANDLER.
	 (let ((body (P body)))
	   (make-shortcut body (P handler))))

	(else
	 (compiler-internal-error __module_who__  __who__ "invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (define (T x)
      (struct-case x
	((asmcall op rands)
	 x)

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (T e1) (T e2)))

	((seq e0 e1)
	 (make-seq (E e0) (T e1)))

	((shortcut body handler)
	 (make-shortcut (T body) (T handler)))

	(else
	 (compiler-internal-error __module_who__  __who__ "invalid tail" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (let ((x (T x)))
      (values un* x))) ;;end of function %ADD-UNSPILLABLES

;;; --------------------------------------------------------------------

  (define (mem? x)
    (or (disp? x) (fvar? x)))

  (module (long-imm? small-operand?)

    (define (long-imm? x)
      ;;Return true  if X  represents a  constant signed  integer too  big to  fit in
      ;;32-bit.
      ;;
      (struct-case x
	((constant n)
	 (cond ((integer? n)
		(not (<= MIN-SIGNED-32-BIT-INTEGER
			 n
			 MAX-SIGNED-32-BIT-INTEGER)))
	       (else #t)))
	(else #f)))

    (define (small-operand? x)
      (boot.case-word-size
       ((32)
	(not (mem? x)))
       ((64)
	(struct-case x
	  ((constant n)
	   (if (integer? n)
	       (<= MIN-SIGNED-32-BIT-INTEGER
		   n
		   MAX-SIGNED-32-BIT-INTEGER)
	     #f))
	  (else
	   (or (register? x)
	       (var?      x)))))))

    (define-inline-constant MIN-SIGNED-32-BIT-INTEGER
      (- (expt 2 31)))

    (define-inline-constant MAX-SIGNED-32-BIT-INTEGER
      (- (expt 2 31) 1))

    #| end of module |# )

  #| end of module: %add-unspillables |# )


(define (%build-graph x)
  ;;
  ;;A lot of functions are nested here because they need to close upon GRAPH.
  ;;
  (import LISTY-SET)
  (import GRAPHS)

  (define-syntax __who__
    (identifier-syntax '%build-graph))

  (define GRAPH
    (empty-graph))

  (define exception-live-set
    (make-parameter #f))

  (define (R* ls)
    (if (pair? ls)
	(set-union (R  (car ls))
		   (R* (cdr ls)))
      (make-empty-set)))

  (define (R x)
    (struct-case x
      ((constant)
       (make-empty-set))
      ((var)
       (singleton x))
      ((disp s0 s1)
       (set-union (R s0) (R s1)))
      ((fvar)
       (make-empty-set))
      ((code-loc)
       (make-empty-set))
      (else
       (if (symbol? x)
	   (if (memq x ALL-REGISTERS)
	       (set-add x (make-empty-set))
	     (make-empty-set))
	 (compiler-internal-error __module_who__ __who__ "invalid R" x)))))

  (module (E)

    (define (E x s)
      (struct-case x
	((asm-instr op d v)
	 (E-asm-instr op d v s))

	((seq e0 e1)
	 (E e0 (E e1 s)))

	((conditional e0 e1 e2)
	 (let ((s1 (E e1 s))
	       (s2 (E e2 s)))
	   (P e0 s1 s2 (set-union s1 s2))))

	((non-tail-call targ value args mask size)
	 (set-union (R* args) s))

	((asmcall op arg*)
	 (case op
	   ((nop fl:single->double fl:double->single)
	    s)
	   ((interrupt incr/zero?)
	    (or (exception-live-set)
		(compiler-internal-error __module_who__ __who__ "uninitialized exception")))
	   (else
	    (compiler-internal-error __module_who__ __who__ "invalid effect asmcall" op))))

	((shortcut body handler)
	 (let ((s2 (E handler s)))
	   (parameterize ((exception-live-set s2))
	     (E body s))))

	(else
	 (compiler-internal-error __module_who__ __who__ "invalid effect" (unparse-recordized-code x)))))

    (define (E-asm-instr op d v s)
      (define-syntax-rule (set-for-each ?func ?set)
	(for-each ?func (set->list ?set)))
      (case op
	((move load32)
	 (let ((s (set-rem d s)))
	   (set-for-each (lambda (y)
			   (add-edge! GRAPH d y))
			 s)
	   (set-union (R v) s)))

	((load8)
	 (let ((s (set-rem d s)))
	   (set-for-each (lambda (y)
			   (add-edge! GRAPH d y))
			 s)
	   (when (var? d)
	     (for-each (lambda (r)
			 (add-edge! GRAPH d r))
	       NON-8BIT-REGISTERS))
	   (when (var? v)
	     (for-each (lambda (r)
			 (add-edge! GRAPH v r))
	       NON-8BIT-REGISTERS))
	   (set-union (R v) s)))

	((int-/overflow int+/overflow int*/overflow)
	 (unless (exception-live-set)
	   (compiler-internal-error __module_who__ __who__ "uninitialized live set"))
	 (let ((s (set-rem d (set-union s (exception-live-set)))))
	   (set-for-each (lambda (y)
			   (add-edge! GRAPH d y))
			 s)
	   (set-union (set-union (R v) (R d)) s)))

	((logand logxor int+ int- int* logor sll sra srl bswap! sll/overflow)
	 (let ((s (set-rem d s)))
	   (set-for-each (lambda (y)
			   (add-edge! GRAPH d y))
			 s)
	   (set-union (set-union (R v) (R d)) s)))

	((bset)
	 (when (var? v)
	   (for-each (lambda (r)
		       (add-edge! GRAPH v r))
	     NON-8BIT-REGISTERS))
	 (set-union (set-union (R v) (R d)) s))

	((cltd)
	 (let ((s (set-rem edx s)))
	   (when (register? edx)
	     (set-for-each (lambda (y)
			     (add-edge! GRAPH edx y))
			   s))
	   (set-union (R eax) s)))

	((idiv)
	 (let ((s (set-rem eax (set-rem edx s))))
	   (when (register? eax)
	     (set-for-each (lambda (y)
			     (add-edge! GRAPH eax y)
			     (add-edge! GRAPH edx y))
			   s))
	   (set-union (set-union (R eax) (R edx))
		      (set-union (R v) s))))

	(( ;;some assembly instructions
	  mset		mset32
			fl:load		fl:store
			fl:add!		fl:sub!
			fl:mul!		fl:div!
			fl:from-int		fl:shuffle
			fl:store-single	fl:load-single)
	 (set-union (R v) (set-union (R d) s)))

	(else
	 (compiler-internal-error __module_who__ __who__ "invalid effect" x))))

    #| end of module: E |# )

  (define (P x st sf su)
    (struct-case x
      ((constant c)
       (if c st sf))

      ((seq e0 e1)
       (E e0 (P e1 st sf su)))

      ((conditional e0 e1 e2)
       (let ((s1 (P e1 st sf su))
	     (s2 (P e2 st sf su)))
	 (P e0 s1 s2 (set-union s1 s2))))

      ((asm-instr op s0 s1)
       (set-union (set-union (R s0) (R s1)) su))

      ((shortcut body handler)
       (let ((s2 (P handler st sf su)))
	 (parameterize ((exception-live-set s2))
	   (P body st sf su))))

      (else
       (compiler-internal-error __module_who__ __who__ "invalid pred" (unparse-recordized-code x)))))

  (define (T x)
    ;;Process the  struct instance X,  representing recordized code,  as if it  is in
    ;;tail position.
    ;;
    (struct-case x
      ((conditional e0 e1 e2)
       (let ((s1 (T e1))
	     (s2 (T e2)))
	 (P e0 s1 s2 (set-union s1 s2))))

      ((asmcall op rands)
       (R* rands))

      ((seq e0 e1)
       (E e0 (T e1)))

      ((shortcut body handler)
       (let ((s2 (T handler)))
	 (parameterize ((exception-live-set s2))
	   (T body))))

      (else
       (compiler-internal-error __module_who__ __who__ "invalid tail" (unparse-recordized-code x)))))

  (let ((s (T x)))
    ;;(pretty-print (unparse-recordized-code x))
    ;;(print-graph GRAPH)
    GRAPH))


(module (%color-graph)
  (import LISTY-SET)
  (import GRAPHS)

  (define (%color-graph spillable* unspillable* G)
    ;;Non-tail recursive function.
    ;;
    (cond ((and (empty-set? spillable*)
		(empty-set? unspillable*))
	   (values '() (make-empty-set) '()))

	  ((find-low-degree (set->list unspillable*) G)
	   => (lambda (un)
		(let ((n* (node-neighbors un G)))
		  (delete-node! un G)
		  (receive (spills spillable* env)
		      (%color-graph spillable* (set-rem un unspillable*) G)
		    (let ((r (find-color un n* env)))
		      (values spills spillable* (cons (cons un r) env)))))))

	  ((find-low-degree (set->list spillable*) G)
	   => (lambda (sp)
		(let ((n* (node-neighbors sp G)))
		  (delete-node! sp G)
		  (receive (spills spillable* env)
		      (%color-graph (set-rem sp spillable*) unspillable* G)
		    (let ((r (find-color sp n* env)))
		      (values spills (set-add sp spillable*) (cons (cons sp r) env)))))))

	  ((pair? (set->list spillable*))
	   (let* ((sp (car (set->list spillable*)))
		  (n* (node-neighbors sp G)))
	     (delete-node! sp G)
	     (receive (spills spillable* env)
		 (%color-graph (set-rem sp spillable*) unspillable* G)
	       (let ((r (find-color/maybe sp n* env)))
		 (if r
		     (values spills (set-add sp spillable*) (cons (cons sp r) env))
		   (values (cons sp spills) spillable* env))))))

	  (else
	   (compiler-internal-error __module_who__  '%color-graph "whoaaa"))))

  (define (find-low-degree ls G)
    (cond ((null? ls)
	   #f)
	  ((fx< (length (set->list (node-neighbors (car ls) G)))
		(length ALL-REGISTERS))
	   (car ls))
	  (else
	   (find-low-degree (cdr ls) G))))

  (define (find-color/maybe x confs env)
    (let ((cr (map (lambda (x)
		     (cond ((symbol? x)
			    x)
			   ((assq x env)
			    => cdr)
			   (else
			    #f)))
		(set->list confs))))
      (let ((r* (set->list (set-difference (list->set ALL-REGISTERS)
					   (list->set cr)))))
	(if (pair? r*)
	    (car r*)
	  #f))))

  (define (find-color x confs env)
    (or (find-color/maybe x confs env)
	(compiler-internal-error __module_who__  'find-color "cannot find color for" x)))

  #| end of module: %COLOR-GRAPH |# )


(define* (%substitute env x)
  ;;X must represent  recordized code; this function builds and  returns a new struct
  ;;instance representing recordized code, which is meant to replace X.
  ;;
  ;;The purpose of this function is to apply the subfunction R to the operands in the
  ;;structures of type ASM-INSTR and ASMCALL.
  ;;
  ;;A lot  of functions  are nested  here because  they make  use of  the subfunction
  ;;"Var", and "Var" needs to close upon the argument ENV.
  ;;
  (module (R)

    (define (R x)
      (struct-case x
	((constant)
	 x)
	((var)
	 (Var x))
	((fvar)
	 x)
	((nfv c loc)
	 (or loc
	     (compiler-internal-error __module_who__  __who__ "unset nfv in R" x)))
	((disp s0 s1)
	 (make-disp (D s0) (D s1)))
	(else
	 (if (symbol? x)
	     x
	   (compiler-internal-error __module_who__  __who__ "invalid R" x)))))

    (define (D x)
      (struct-case x
	((constant)
	 x)
	((var)
	 (Var x))
	((fvar)
	 x)
	(else
	 (if (symbol? x)
	     x
	   (compiler-internal-error __module_who__  __who__ "invalid D" x)))))

    (define (Var x)
      (cond ((assq x env)
	     => cdr)
	    (else x)))

    ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
    ;;
    ;; (module (Rhs)
    ;;
    ;;   (define (Rhs x)
    ;; 	(struct-case x
    ;; 	  ((var)
    ;; 	   (Var x))
    ;; 	  ((asmcall op rand*)
    ;; 	   (make-asmcall op (map Rand rand*)))
    ;; 	  (else x)))
    ;;
    ;;   (define (Rand x)
    ;; 	(struct-case x
    ;; 	  ((var)
    ;; 	   (Var x))
    ;; 	  (else x)))
    ;;
    ;;   #| end of module: Rhs |# )

    ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
    ;;
    ;; (define (Lhs x)
    ;;   (struct-case x
    ;;     ((var)
    ;; 	 (Var x))
    ;;     ((nfv confs loc)
    ;;      (or loc
    ;; 	     (compiler-internal-error __module_who__  __who__ "LHS not set" x)))
    ;;     (else x)))

    #| end of module: R |# )

  (define (E x)
    ;;substitute effect
    (struct-case x
      ((seq e0 e1)
       (make-seq (E e0) (E e1)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2)))
      ((asm-instr op x v)
       (make-asm-instr op (R x) (R v)))
      ((asmcall op rands)
       (make-asmcall op (map R rands)))
      ((non-tail-call)
       x)
      ((shortcut body handler)
       (make-shortcut (E body) (E handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid effect" (unparse-recordized-code x)))))

  (define (P x)
    (struct-case x
      ((constant)
       x)
      ((asm-instr op x v)
       (make-asm-instr op (R x) (R v)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))
      ((seq e0 e1)
       (make-seq (E e0) (P e1)))
      ((shortcut body handler)
       (make-shortcut (P body) (P handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid pred" (unparse-recordized-code x)))))

  (define (T x)
    (struct-case x
      ((asmcall op rands)
       x)
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (T e1) (T e2)))
      ((seq e0 e1)
       (make-seq (E e0) (T e1)))
      ((shortcut body handler)
       (make-shortcut (T body) (T handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid tail" (unparse-recordized-code x)))))

  (T x))


;;;; done

#| end of module: color-by-chaitin |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'assemble-sources		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; End:
