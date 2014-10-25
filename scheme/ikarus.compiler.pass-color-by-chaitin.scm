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
	((codes x.clambda* x.locals)
	 (make-codes (map E-clambda x.clambda*) (%color-program x.locals)))))

    (define (E-clambda x)
      (struct-case x
	((clambda label clause* cp freevar* name)
	 (make-clambda label (map E-clambda-clause clause*) cp freevar* name))))

    (define (E-clambda-clause x)
      (struct-case x
	((clambda-case x.info x.locals)
	 (make-clambda-case x.info (%color-program x.locals)))))

    (module (%color-program)

      (define (%color-program x)
	;;The argument X must  be a LOCALS struct representing the  body of a CLAMBDA
	;;clause or the  body of an initialisation expression.  Return  the return of
	;;the call to %SUBSTITUTE-VARS-WITH-ASSOCIATED-LOCATIONS.
	;;
	(module (list->set make-empty-set)
	  (import LISTY-SET))
	(struct-case x
	  ((locals x.vars x.body)
	   ;;X.VARS.VEC  is  a vector  of  VAR  structs  representing all  the  local
	   ;;variables in  X.BODY.  Some of these  VAR structs have a  FVAR struct in
	   ;;their LOC  field: they have  already been allocated to  stack locations;
	   ;;the other  VAR structs  have #f  in their  LOC field:  they are  not yet
	   ;;allocated.
	   ;;
	   ;;X.VARS.SPILLABLE* is  a list of  VAR structs representing the  subset of
	   ;;VAR structs  in X.VARS.VEC that have  #f in their LOC  field.  These VAR
	   ;;structs  can  be  allocated  to  CPU registers  or  to  stack  locations
	   ;;(spilled).
	   (let ((x.vars.vec        (car x.vars))
		 (x.vars.spillable* (cdr x.vars)))
	     (let loop ((spillable.set    (list->set x.vars.spillable*))
			(unspillable.set  (make-empty-set))
			(body             x.body))
	       (receive (unspillable.set^ body^)
		   ;;FIXME This really needs to be inside the loop.  But why?  Insert
		   ;;explanation here.  (Marco Maggi; Wed Oct 22, 2014)
		   (%add-unspillables unspillable.set body)
		 (let ((G (%build-graph body^)))
		   #;(print-graph G)
		   (receive (spilled* spillable.set^ env)
		       (%color-graph spillable.set unspillable.set^ G)
		     (if (null? spilled*)
			 (%substitute-vars-with-associated-locations env body^)
		       (let* ((env^   (%assign-stack-locations-to-spilled-vars spilled* x.vars.vec))
			      (body^^ (%substitute-vars-with-associated-locations env^ body^)))
			 (loop spillable.set^ unspillable.set^ body^^)))))))))))

      (define (%assign-stack-locations-to-spilled-vars spilled* x.vars.vec)
	;;The argument  SPILLED* is the  list of  the VAR structs  representing local
	;;variables spilled on the stack: VAR structs  whose LOC field must be set to
	;;a newly allocated FVAR struct.
	;;
	;;The argument  X.VARS.VEC is a  vector of  VAR structs representing  all the
	;;local variables in X.BODY.
	;;
	;;For every VAR struct in SPILLED*: select a stack frame location that is not
	;;already used by  other VAR structs and  allocated it to the  VAR by storing
	;;the  associated FVAR  struct in  its  LOC field;  updated the  interference
	;;graphs accordingly.
	;;
	;;Return an ENV  value: an alist whose  keys are the spilled  VAR structs and
	;;whose values are the associated FVAR structs.
	;;
	(fprintf (current-error-port) "start\n")
	($map/stx
	    (lambda (spilled)
	      (module (for-each-var add-frm rem-var mem-frm?)
		(import FRAME-CONFLICT-SETS))
	      (let ((spilled.fvar (let loop ((stack-frame-conflicts ($var-frm-conf spilled))
					     (stack-offset          1))
				    (let ((stack-frame-loc (mkfvar stack-offset)))
				      (if (mem-frm? stack-frame-loc stack-frame-conflicts)
					  ;;The  stack frame  location referenced  by
					  ;;STACK-FRAME-LOC   is   already  used   by
					  ;;another  local variable,  check the  next
					  ;;one.
					  (begin
					    (fprintf (current-error-port) "~a, " stack-offset)
					    (loop stack-frame-conflicts (fxadd1 stack-offset)))
					;;The  stack  frame  location  referended  by
					;;STACK-FRAME-LOC is unused: choose it.
					stack-frame-loc)))))
		(fprintf (current-error-port) " i=~a\n" (fvar-idx spilled.fvar))
		;;For  each local  variable in  X.VARS.VEC: remove  SPILLED from  the
		;;interference  graph of  VAR structs  (VAR-CONF) and  add it  to the
		;;interference graph of FVAR structs (FRM-CONF).
		(for-each-var
		    ($var-var-conf spilled)
		    x.vars.vec
		  (lambda (x.var)
		    ($set-var-var-conf! x.var (rem-var spilled      ($var-var-conf x.var)))
		    ($set-var-frm-conf! x.var (add-frm spilled.fvar ($var-frm-conf x.var)))))
		($set-var-loc! spilled spilled.fvar)
		(cons spilled spilled.fvar)))
	  spilled*))

      #| end of module: %COLOR-PROGRAM |# )

    #| end of module: COLOR-BY-CHAITIN |# )


(module LISTY-SET
  ;;This module implements sets of bits; each  set is a proper list of items, wrapped
  ;;by a struct.
  ;;
  ;;NOTE This  module has the  same API  of the module  INTEGER-SET, but this  is not
  ;;actually important.
  ;;
  (make-empty-set
   element->set
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union
   set->list		list->set
   set-for-each)

  (define-struct set
    ;;Wrapper for a list of elements used as a set.
    ;;
    (element*
		;The  list of  elements in  the set:  VAR structs,  FVAR structs  and
		;symbols representing CPU register names.  The included registers are
		;only  the ones  being  full  machine word  registers  and *not*  for
		;special purpose (not APR, not CPR, not FPR, not PCR).
     ))

  (define (element? x)
    (or (var? x) (register? x) (fvar? x)))

;;; --------------------------------------------------------------------
;;; constructors

  (define-syntax-rule (make-empty-set)
    (make-set '()))

  (define (element->set x)
    #;(assert (element? x))
    (make-set (list x)))

  (define (list->set element*)
    #;(assert (for-all element? element*))
    (make-set element*))

;;; --------------------------------------------------------------------

  (define* (set-member? x {S set?})
    (memq x ($set-element* S)))

  (define* (empty-set? {S set?})
    (null? ($set-element* S)))

  (define* (set->list {S set?})
    ($set-element* S))

  (define* (set-add x {S set?})
    ;;Add X to S, but only if it is not already contained.
    ;;
    #;(assert (element? x))
    (if (memq x ($set-element* S))
	S
      (make-set (cons x ($set-element* S)))))

  (define* (set-rem x {S set?})
    #;(assert (element? x))
    (make-set ($remq x ($set-element* S))))

  (define* (set-difference {S1 set?} {S2 set?})
    (make-set (let $difference ((element1* ($set-element* S1))
				(element2* ($set-element* S2)))
		;;Remove  from  the list  ELEMENT1*  all  the  elements of  the  list
		;;ELEMENT2*.  Use EQ? for comparison.
		(if (pair? element2*)
		    ($difference ($remq (car element2*) element1*)
				 (cdr element2*))
		  element1*))))

  (define* (set-union {S1 set?} {S2 set?})
    (make-set (let $union ((element1* ($set-element* S1))
			   (element2* ($set-element* S2)))
		(cond ((pair? element1*)
		       (cond ((memq (car element1*) element2*)
			      ($union (cdr element1*) element2*))
			     (else
			      (cons (car element1*)
				    (union (cdr element1*) element2*)))))
		      (else
		       ;;(assert (null? element1*))
		       element2*)))))

  (define-syntax-rule (set-for-each ?func ?set)
    ($for-each/stx ?func (set->list ?set)))

;;; --------------------------------------------------------------------

  (define ($remq x element*)
    ;;Remove X from the list ELEMENT*.
    ;;
    (cond ((pair? element*)
	   (cond ((eq? x (car element*))
		  (cdr element*))
		 (else
		  (cons (car element*)
			($remq x (cdr element*))))))
	  (else
	   ;;(assert (null? element*))
	   '())))

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
    ;;Represent the interference graph.  The graph is undirected: for every edge from
    ;;NODE1 to NODE2 there is an edge from NODE2 to NODE1.
    ;;
    (entry*
		;An alist  representing the nodes and  edges.  The keys of  the alist
		;represent graph's  nodes: structs  of type VAR  or FVAR,  or symbols
		;representing register  names.  The values  of the alist are  sets as
		;defined  by the  module LISTY-SET,  representing the  edges outgoing
		;from the node.
		;
		;The destination  nodes of the edges  outgoing from node N  are alive
		;when node N is alive.
     ))

  (define-syntax-rule (empty-graph)
    (make-graph '()))

  (define (empty-graph? G)
    (andmap (lambda (x)
	      (empty-set? (cdr x)))
	    ($graph-entry* G)))

  (module (add-edge!)

    (define (add-edge! G node1 node2)
      ;;Add an undirected edge to graph G between NODE1 and NODE2.
      ;;
      (let ((old-entry* ($graph-entry* G)))
	(cond ((assq node1 old-entry*)
	       ;;NODE1 is already in the graph.
	       => (lambda (node1.entry)
		    ;;Is NODE2 already in the set of nodes connected to NODE1?
		    (unless (set-member? node2 (cdr node1.entry))
		      ;;Not it is not, so add it.
		      (%add-directed-edge-to-entry! node1.entry node2)
		      (cond ((assq node2 old-entry*)
			     ;;NODE2 is already in the graph.  Add an edge from NODE2
			     ;;to NODE1.
			     => (lambda (node2.entry)
				  (%add-directed-edge-to-entry! node2.entry node1)))
			    (else
			     ;;NODE2 is  not in  the graph.  Add  NODE2 to  the graph
			     ;;with an edge from NODE2 to NODE1.
			     ($set-graph-entry*! G (cons (%make-singleton-entry node2 node1)
							 old-entry*)))))))
	      ((assq node2 old-entry*)
	       => (lambda (node2.entry)
		    (%add-directed-edge-to-entry! node2.entry node1)
		    ($set-graph-entry*! G (cons (%make-singleton-entry node1 node2)
						old-entry*))))
	      (else
	       ;;Neither NODE1 nor NODE2 are already nodes in the graph.  Add both of
	       ;;them.
	       ($set-graph-entry*! G (cons* (%make-singleton-entry node1 node2)
					    (%make-singleton-entry node2 node1)
					    old-entry*))))))

    (define-syntax-rule (%make-singleton-entry ?src-node ?dst-node)
      ;;Build and  return a new  entry representing  node ?SRC-NODE with  an outgoing
      ;;edge from ?SRC-NODE to ?DST-NODE.
      ;;
      (cons ?src-node (element->set ?dst-node)))

    (define-syntax (%add-directed-edge-to-entry! stx)
      ;;Given a node's entry: add an edge from the entry's node to ?DST-NODE.
      ;;
      (syntax-case stx ()
	((_ ?src-node-entry ?dst-node)
	 (and (identifier? #'?src-node-entry)
	      (identifier? #'?dst-noe))
	 #'(set-cdr! ?src-node-entry (set-add ?dst-node (cdr ?src-node-entry))))
	))

    #| end of module: ADD-EDGE! |# )

  (define (node-neighbors x G)
    ;;Return a set containing the nodes connected to X.  Such nodes are the locations
    ;;alive when X is written.
    ;;
    (cond ((assq x ($graph-entry* G))
	   => cdr)
	  (else
	   (make-empty-set))))

  (define* (delete-node! x G)
    ;;Removing a  node means removing  all the  edges to and  from X; the  location X
    ;;still has an entry in G.  If X is not a node: nothing happens.
    ;;
    (let ((entry* ($graph-entry* G)))
      (cond ((assq x entry*)
	     => (lambda (x.entry)
		  ;;For every node Y connected to X: remove the edge from Y to X.
		  (set-for-each (lambda (y)
				  (let ((y.entry (assq y entry*)))
				    (set-cdr! y.entry (set-rem x (cdr y.entry)))))
		    (cdr x.entry))
		  ;;Remove all the edges from X to other nodes.
		  (set-cdr! x.entry (make-empty-set)))))))

;;; --------------------------------------------------------------------

  (define (print-graph G)
    (printf "G={\n")
    (parameterize ((print-gensym 'pretty))
      (for-each (lambda (x)
                  (let ((lhs  (car x))
			(rhs* (cdr x)))
                    (printf "  ~s => ~s\n"
                            (unparse-recordized-code lhs)
                            (map unparse-recordized-code (set->list rhs*)))))
        ($graph-entry* G)))
    (printf "}\n"))

  #| end of module: GRAPHS |# )


(module (%add-unspillables)

  (define-syntax __who__
    (identifier-syntax '%add-unspillables))

  (define (%add-unspillables unspillable.set body)
    ;;The argument UNSPILLABLE.SET  is a set of VAR  structs representing unspillable
    ;;local variables  in BODY;  it starts  empty and  we fill  it in  this function.
    ;;Return  2 values:  the filled  UNSPILLABLE.SET  and recordised  code that  must
    ;;replace BODY.
    ;;
    ;;A  lot  of  functions  are  nested  here  because  they  call  the  subfunction
    ;;%MAKE-UNSPILLABLE-VAR, and  the %MAKE-UNSPILLABLE-VAR  needs to close  upon the
    ;;argument UNSPILLABLE.SET.
    ;;

    (define (main body)
      ;;Here we want to return the value of the mutated UNSPILLABLE.SET binding.
      (values unspillable.set (T body)))

    (define (%make-unspillable-var)
      (module (set-add)
	(import LISTY-SET))
      (receive-and-return (unspillable)
	  (make-unique-var 'unspillable-tmp)
	(set! unspillable.set (set-add unspillable unspillable.set))))

    (module (E)

      (define (E x)
	(struct-case x
	  ((seq e0 e1)
	   (make-seq (E e0) (E e1)))

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (E e1) (E e2)))

	  ((asm-instr op dst src)
	   (E-asm-instr op dst src x))

	  ((asmcall op rands)
	   (case op
	     ((nop interrupt incr/zero? fl:single->double fl:double->single)
	      x)
	     (else
	      (compiler-internal-error __module_who__  __who__
		"ASMCALL struct with invalid operand in E context"
		(unparse-recordized-code x)))))

	  ((non-tail-call)
	   x)

	  ((shortcut body handler)
	   ;;Do BODY first, then HANDLER.
	   (let ((body^ (E body)))
	     (make-shortcut body^ (E handler))))

	  (else
	   (compiler-internal-error __module_who__  __who__
	     "invalid code in E context"
	     (unparse-recordized-code x)))))

      (define (E-asm-instr op dst src x)
	(case op
	  ((load8 load32)
	   (%fix-address src
			 (lambda (src)
			   (if (or (register? dst)
				   (var?      dst))
			       (make-asm-instr op dst src)
			     (let ((unspillable (%make-unspillable-var)))
			       (make-seq
				 (make-asm-instr op unspillable src)
				 (E (make-asm-instr 'move dst unspillable))))))))

	  ((logor logxor logand int+ int- int* move int-/overflow int+/overflow int*/overflow)
	   (cond ((and (eq? op 'move)
		       (eq? dst src))
		  ;;Source and dest are the same: do nothing.
		  (nop))
		 ((and (= wordsize 8)
		       (not (eq? op 'move))
		       (long-imm? src))
		  (let ((unspillable (%make-unspillable-var)))
		    (make-seq
		      (E (make-asm-instr 'move unspillable src))
		      (E (make-asm-instr op dst unspillable)))))
		 ((and (memq op '(int* int*/overflow))
		       (mem? dst))
		  (let ((unspillable (%make-unspillable-var)))
		    (multiple-forms-sequence
		      (E (make-asm-instr 'move unspillable dst))
		      (E (make-asm-instr op unspillable src))
		      (E (make-asm-instr 'move dst unspillable)))))
		 ((and (mem? dst)
		       (not (small-operand? src)))
		  (let ((unspillable (%make-unspillable-var)))
		    (make-seq
		      (E (make-asm-instr 'move unspillable src))
		      (E (make-asm-instr op dst unspillable)))))
		 ((disp? dst)
		  (let ((s0 (disp-objref dst))
			(s1 (disp-offset dst)))
		    (cond ((not (small-operand? s0))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s0))
			       (E (make-asm-instr op (make-disp unspillable s1) src)))))
			  ((not (small-operand? s1))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s1))
			       (E (make-asm-instr op (make-disp s0 unspillable) src)))))
			  ((small-operand? src)
			   x)
			  (else
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable src))
			       (E (make-asm-instr op dst unspillable))))))))
		 ((disp? src)
		  (let ((s0 (disp-objref src))
			(s1 (disp-offset src)))
		    (cond ((not (small-operand? s0))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s0))
			       (E (make-asm-instr op dst (make-disp unspillable s1))))))
			  ((not (small-operand? s1))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s1))
			       (E (make-asm-instr op dst (make-disp s0 unspillable))))))
			  (else x))))
		 (else x)))

	  ((bswap!)
	   (if (mem? src)
	       (let ((unspillable (%make-unspillable-var)))
		 (multiple-forms-sequence
		   (E (make-asm-instr 'move   unspillable dst))
		   (E (make-asm-instr 'bswap! unspillable unspillable))
		   (E (make-asm-instr 'move   src         unspillable))))
	     x))

	  ((cltd)
	   ;;Here we know that  DST is the register EDX and SRC  is the register EAX.
	   ;;We know that CLTD and IDIV always come together.
	   (assert (eq? dst edx))
	   (assert (eq? src eax))
	   x)

	  ((idiv)
	   ;;Here we know  that DST is either  the register EAX or  the register EDX;
	   ;;SRC is an operand, we do not know which one here.  We know that CLTD and
	   ;;IDIV always come together.
	   (assert (or (eq? dst eax) (eq? dst edx)))
	   (if (or (var?      src)
		   (register? src))
	       x
	     (let ((unspillable (%make-unspillable-var)))
	       (make-seq
		 (E (make-asm-instr 'move unspillable src))
		 (E (make-asm-instr 'idiv dst unspillable))))))

	  ((sll sra srl sll/overflow)
	   (unless (or (constant? src)
		       (eq? src ecx))
	     (compiler-internal-error __module_who__  __who__ "invalid shift" src))
	   x)

	  ((mset mset32 bset)
	   ;;We expect X to have one of the formats:
	   ;;
	   ;;   (asm-instr mset   (disp ?objref ?offset) ?new-val)
	   ;;   (asm-instr bset   (disp ?objref ?offset) ?new-val)
	   ;;   (asm-instr mset32 (disp ?objref ?offset) ?new-val)
	   ;;
	   ;;MSET is used, for example, to store objects in the car and cdr of pairs.
	   ;;MSET32 is  used, for example,  to store  single characters in  a string.
	   ;;BSET is used, for example, to store single bytes in a bytevector.
	   (if (not (small-operand? src))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable src))
		   (E (make-asm-instr op dst unspillable))))
	     (check-disp dst
			 (lambda (dst)
			   (let ((s0 (disp-objref dst))
				 (s1 (disp-offset dst)))
			     (if (and (constant? s0)
				      (constant? s1))
				 (let ((unspillable (%make-unspillable-var)))
				   (multiple-forms-sequence
				     (E (make-asm-instr 'move unspillable s0))
				     (E (make-asm-instr 'int+ unspillable s1))
				     (let ((disp (make-disp unspillable (make-constant 0))))
				       (make-asm-instr op disp src))))
			       (make-asm-instr op dst src)))))))

	  ((fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:load-single fl:store-single)
	   (check-disp-arg dst
			   (lambda (dst)
			     (check-disp-arg src
					     (lambda (src)
					       (make-asm-instr op dst src))))))

	  ((fl:from-int fl:shuffle)
	   x)

	  (else
	   (compiler-internal-error __module_who__  __who__
	     "invalid ASM-INSTR operator" op))))

      (define (%fix-address x kont)
	;;Non-tail recursive function.
	;;
	(if (disp? x)
	    (let ((s0 (disp-objref x))
		  (s1 (disp-offset x)))
	      (cond ((not (small-operand? s0))
		     (let ((unspillable (%make-unspillable-var)))
		       (make-seq
			 (E (make-asm-instr 'move unspillable s0))
			 (%fix-address (make-disp unspillable s1) kont))))
		    ((not (small-operand? s1))
		     (let ((unspillable (%make-unspillable-var)))
		       (make-seq
			 (E (make-asm-instr 'move unspillable s1))
			 (%fix-address (make-disp s0 unspillable) kont))))
		    (else
		     (kont x))))
	  (kont x)))

      #| end of module: E |# )

;;; --------------------------------------------------------------------

    (module (P)

      (define (P x)
	(struct-case x
	  ((constant)
	   x)

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (P e1) (P e2)))

	  ((seq e0 e1)
	   (make-seq (E e0) (P e1)))

	  ((asm-instr op dst src)
	   (P-asm-instr op dst src x))

	  ((shortcut body handler)
	   ;;Do BODY first, then HANDLER.
	   (let ((body (P body)))
	     (make-shortcut body (P handler))))

	  (else
	   (compiler-internal-error __module_who__  __who__
	     "invalid code in P context" (unparse-recordized-code x)))))

      (define (P-asm-instr op dst src x)
	(cond ((memq op '(fl:= fl:< fl:<= fl:> fl:>=))
	       (if (mem? dst)
		   (let ((unspillable (%make-unspillable-var)))
		     (make-seq
		       (E (make-asm-instr 'move unspillable dst))
		       (make-asm-instr op unspillable src)))
		 x))
	      ((and (not (mem?           dst))
		    (not (small-operand? dst)))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable dst))
		   (P (make-asm-instr op unspillable src)))))
	      ((and (not (mem?           src))
		    (not (small-operand? src)))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable src))
		   (P (make-asm-instr op dst unspillable)))))
	      ((and (mem? dst)
		    (mem? src))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable src))
		   (P (make-asm-instr op    dst unspillable)))))
	      (else
	       (check-disp dst
			   (lambda (dst)
			     (check-disp src
					 (lambda (src)
					   (make-asm-instr op dst src))))))))

      #| end of module: P |# )

;;; --------------------------------------------------------------------

    (define (T x)
      (struct-case x
	((asmcall op rands)
	 #;(assert (or (eq? op 'return) (eq? op 'direct-jump) (eq? op 'indirect-jump)))
	 ;;We assume the input code is correct; this means in tail position there are
	 ;;only ASMCALL  structs with  operand RETURN, DIRECT-JUMP  or INDIRECT-JUMP.
	 ;;We know that the operands of  such ASMCALL structs are CPU register symbol
	 ;;names and FVAR structs; so there is  nothing to be done here.  Just return
	 ;;X itself.
	 x)

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (T e1) (T e2)))

	((seq e0 e1)
	 (make-seq (E e0) (T e1)))

	((shortcut body handler)
	 (make-shortcut (T body) (T handler)))

	(else
	 (compiler-internal-error __module_who__  __who__
	   "invalid code in T context" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (define (check-disp-arg x kont)
      (if (small-operand? x)
	  (kont x)
	(let ((unspillable (%make-unspillable-var)))
	  (make-seq
	    (E (make-asm-instr 'move unspillable x))
	    (kont unspillable)))))

    (define (check-disp x kont)
      (struct-case x
	((disp objref offset)
	 (check-disp-arg objref
			 (lambda (objref^)
			   (check-disp-arg offset
					   (lambda (offset^)
					     (kont (make-disp objref^ offset^)))))))
	(else
	 (kont x))))

    (main body)) ;;end of function %ADD-UNSPILLABLES

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


(define* (%build-graph body)
  ;;Process BODY with a depth-first traversal, visiting the tail branches first, in a
  ;;post-order fashion; while rewinding:
  ;;
  ;;*  Build  the live  set:  a  SET struct,  as  defined  by the  LISTY-SET  module,
  ;;  containing all the VAR structs, FVAR structs and CPU register symbol names that
  ;;  are *read* at least once in the continuation.
  ;;
  ;;* Build the interference graph: a GRAPH  struct, as defined by the GRAPHS module,
  ;;  containing one node for every VAR  struct, FVAR struct and CPU register that is
  ;;  *written* at least  once.  The edges of each node  represent the locations that
  ;;  are  alive when the  node is written:  all the locations  that will be  read at
  ;;  least once in the continuation.
  ;;
  ;;Return the GRAPH struct representing the interference graph.
  ;;
  ;;NOTE Locations are added  to a live set only by the  function "R" which processes
  ;;operands.  "R" processes the leaves of the  BODY and creates either an empty live
  ;;set or  a live set  containing a single  location; other functions  take multiple
  ;;live sets and compose new live sets with the union operation.
  ;;
  ;;NOTE  Edges   are  added  to  the   interference  graph  only  by   the  function
  ;;"E-asm-instr",  which  is the  only  one  that  processes  code which  writes  to
  ;;locations.
  ;;
  ;;NOTE A lot  of functions are nested  here because they need to  close upon GRAPH,
  ;;which is mutated as  the code traversal progresses and finally  it is returned to
  ;;the caller.
  ;;
  (import LISTY-SET)
  (import GRAPHS)

  (define (main body)
    ;;We discard the return value from T.
    (T body)
    #;(pretty-print (unparse-recordized-code/sexp x))
    #;(print-graph THE-GRAPH)
    THE-GRAPH)

  (define THE-GRAPH
    (empty-graph))

  (define exception-live-set
    ;;Whenever we enter a SHORTCUT, we.
    ;;
    ;;1. Process the handler first.
    ;;
    ;;2. Set this parameter to the handler's set.
    ;;
    ;;3. Process the body.
    ;;
    ;;The handler's  set becomes the set  of ASMCALL structs with  operands INTERRUPT
    ;;and  INCR/ZERO?,  and  it  is  consumed by  ASM-INSTR  structs  with  operands:
    ;;INT-/OVERFLOW, INT+/OVERFLOW, INT*/OVERFLOW.
    ;;
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (define (T x)
    ;;Process the  struct instance X,  representing recordized code,  as if it  is in
    ;;tail position.
    ;;
    (struct-case x
      ((conditional test conseq altern)
       (let ((conseq.set (T conseq))
	     (altern.set (T altern)))
	 (P test conseq.set altern.set (set-union conseq.set altern.set))))

      ((asmcall op rand*)
       #;(assert (or (eq? op 'return) (eq? op 'direct-jump) (eq? op 'indirect-jump)))
       ;;We assume the input  code is correct; this means in  tail position there are
       ;;only ASMCALL structs with operand  RETURN, DIRECT-JUMP or INDIRECT-JUMP.  We
       ;;know that the operands of such ASMCALL structs are CPU register symbol names
       ;;and FVAR structs.
       (R* rand*))

      ((seq e0 e1)
       (E e0 (T e1)))

      ((shortcut body handler)
       ;;Do the handler first, then the body.
       (let ((handler.set (T handler)))
	 (parameterize ((exception-live-set handler.set))
	   (T body))))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid code in T context" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (R* rand*)
    ;;Process the  operands of ASMCALL  and NON-TAIL-CALL structs.  Build  and return
    ;;the union between the live sets of all the RAND*.
    ;;
    (if (pair? rand*)
	(set-union (R  (car rand*))
		   (R* (cdr rand*)))
      (make-empty-set)))

  (define (R x)
    ;;Process X which must be an operand in a struct of type: ASMCALL, NON-TAIL-CALL,
    ;;ASM-INSTR, DISP.  Return the live set of X.
    ;;
    (struct-case x
      ((constant)
       (make-empty-set))
      ((var)
       (element->set x))
      ((disp objref offset)
       (set-union (R objref) (R offset)))
      ((fvar)
       (make-empty-set))
      ((code-loc)
       (make-empty-set))
      (else
       (if (register? x)
	   ;;Build a live  set containing the register  only if it is  a full machine
	   ;;word register and *not* a register  with special purpose (APR, CPR, FPR,
	   ;;PCR).
	   (if (memq x ALL-REGISTERS)
	       (element->set x)
	     (make-empty-set))
	 (compiler-internal-error __module_who__ __who__
	   "invalid code in R context" (unparse-recordised-code/sexp x))))))

;;; --------------------------------------------------------------------

  (define (P x tail-conseq.set tail-altern.set tail-union.set)
    ;;If we are in "for predicate" context:  there is other code in tail context that
    ;;has been  processed before X by  the backwards code traversal.   Such tail code
    ;;has produced the sets:
    ;;
    ;;TAIL-CONSEQ.SET -
    ;;  From the uplevel CONSEQ branch, it is to be used for test-true processing.
    ;;
    ;;TAIL-ALTERN.SET -
    ;;  From the uplevel ALTERN branch, it is to be used for test-false processing.
    ;;
    ;;TAIL-UNION.SET -
    ;;  The union  between the uplevel TAIL-CONSEQ.SET and TAIL-ALTERN.SET;  it is to
    ;;  be  joined with  the sets  produced here and  the result  is returned  to the
    ;;  caller.
    ;;
    (struct-case x
      ((constant x.const)
       (if x.const tail-conseq.set tail-altern.set))

      ((seq e0 e1)
       (E e0 (P e1 tail-conseq.set tail-altern.set tail-union.set)))

      ((conditional test conseq altern)
       (let ((conseq.set (P conseq tail-conseq.set tail-altern.set tail-union.set))
	     (altern.set (P altern tail-conseq.set tail-altern.set tail-union.set)))
	 (P test conseq.set altern.set (set-union conseq.set altern.set))))

      ((asm-instr op dst src)
       (set-union (set-union (R dst) (R src))
		  tail-union.set))

      ((shortcut body handler)
       (let ((handler.set (P handler tail-conseq.set tail-altern.set tail-union.set)))
	 (parameterize ((exception-live-set handler.set))
	   (P body tail-conseq.set tail-altern.set tail-union.set))))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid code in P context" (unparse-recordized-code/sexp x)))))

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x tail.set)
      ;;If we are in  "for effect" context: there is other code  in tail context that
      ;;has been processed before X by  the backwards code traversal.  Such tail code
      ;;has produced the live set TAIL.SET used as argument here.
      ;;
      (struct-case x
	((asm-instr op dst src)
	 (E-asm-instr op dst src tail.set x))

	((seq e0 e1)
	 (E e0 (E e1 tail.set)))

	((conditional test conseq altern)
	 (let ((conseq.set (E conseq tail.set))
	       (altern.set (E altern tail.set)))
	   (P test conseq.set altern.set (set-union conseq.set altern.set))))

	((non-tail-call unused.target unused.retval-var all-rand*)
	 (set-union (R* all-rand*) tail.set))

	((asmcall op arg*)
	 (case op
	   ((nop fl:single->double fl:double->single)
	    tail.set)
	   ((interrupt incr/zero?)
	    (or (exception-live-set)
		(compiler-internal-error __module_who__ __who__
		  "missing live set for SHORTCUT's handler while processing body")))
	   (else
	    (compiler-internal-error __module_who__ __who__
	      "invalid ASMCALL operand in E context" op))))

	((shortcut body handler)
	 (let ((handler.set (E handler tail.set)))
	   (parameterize ((exception-live-set handler.set))
	     (E body tail.set))))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid code in E context" (unparse-recordized-code/sexp x)))))

    (define (E-asm-instr op dst src tail.set x)
      (case op
	((move load32)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr move   ?dst ?src)
	 ;;   (asm-instr load32 ?dst ?src)
	 ;;
	 ;;here ?DST is written  and ?SRC is read: in the uplevel  code ?DST is dead,
	 ;;in the  tail code ?DST is  alive; in both  the uplevel code and  tail code
	 ;;?SRC is alive.
	 ;;
	 ;;Here ?DST changes  its status from live  to dead: it is the  moment to add
	 ;;?DST as  node of  the interference graph.   The interference  sub-graph of
	 ;;?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set - ?dst)
	 ;;
	 (let ((S (set-rem dst tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst <--> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   (set-union (R src) S)))

	((load8)
	 ;;We expect X ot have the format:
	 ;;
	 ;;   (asm-instr load8 ?dst (disp ?objref ?offset))
	 ;;
	 ;;here ?DST  is written, ?OBJREF and  ?OFFSET are read: in  the uplevel code
	 ;;?DST is dead, in the tail code ?DST is alive; in both the uplevel code and
	 ;;tail code ?OBJREF and ?OFFSET are alive.
	 ;;
	 ;;While rewinding the  BODY traversal: ?DST changes its status  from live to
	 ;;dead, it is the moment to add ?DST as node of the interference graph.  The
	 ;;interference sub-graph of ?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set - ?dst)
	 ;;
	 #;(assert (disp? src))
	 (let ((S (set-rem dst tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst <--> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   ;;The destination of an 8-bit load cannot  be any register: it can be only
	   ;;a register among the ones that support 8-bit operations; for example EAX
	   ;;is fine, because we can use the 8-bit register AL as operand.  So, here,
	   ;;we add interference edges between DST  and all the CPU registers that do
	   ;;*not* support 8-bit operations; this makes sure that no such register is
	   ;;allocated to DST.
	   (when (var? dst)
	     (for-each (lambda (register)
			 (add-edge! THE-GRAPH dst register))
	       NON-8BIT-REGISTERS))
	   (set-union (R src) S)))

	((int-/overflow int+/overflow int*/overflow)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr int+/overflow ?dst ?src)
	 ;;   (asm-instr int-/overflow ?dst ?src)
	 ;;   (asm-instr int*/overflow ?dst ?src)
	 ;;
	 ;;and  these  instructions  might  jump  to the  handler  of  the  enclosing
	 ;;SHORTCUT; they will become the Intel Assembly instructions:
	 ;;
	 ;;   (addl ?src ?dst)
	 ;;   (jo ?handler-label)
	 ;;
	 ;;   (subl ?src ?dst)
	 ;;   (jo ?handler-label)
	 ;;
	 ;;   (imull ?src ?dst)
	 ;;   (jo ?handler-label)
	 ;;
	 ;;so ?DST is both  read and written and ?SRC is  read.  The continuation can
	 ;;be either the subsequent instructions  in the enclosing SHORTCUT's body or
	 ;;the enclosing SHORTCUT's interrupt handler.
	 ;;
	 ;;While rewinding the  BODY traversal: ?DST changes its status  from live to
	 ;;dead to  live, it is the  moment to add  ?DST as node of  the interference
	 ;;graph.  The interference sub-graph of ?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set + handler.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set + handler.set)
	 ;;
	 (unless (exception-live-set)
	   (compiler-internal-error __module_who__ __who__
	     "missing live set for SHORTCUT's handler while processing body"))
	 (let ((S (set-rem dst (set-union tail.set (exception-live-set)))))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst -> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   (set-union (set-union (R src) (R dst))
		      S)))

	((logand logor logxor sll sra srl int+ int- int* bswap! sll/overflow)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr logand ?dst ?src)
	 ;;   (asm-instr logor  ?dst ?src)
	 ;;   (asm-instr logxor ?dst ?src)
	 ;;   (asm-instr sll    ?dst ?src)
	 ;;   (asm-instr sra    ?dst ?src)
	 ;;   (asm-instr srl    ?dst ?src)
	 ;;   (asm-instr int+   ?dst ?src)
	 ;;   (asm-instr int-   ?dst ?src)
	 ;;   (asm-instr int*   ?dst ?src)
	 ;;   (asm-instr bswap! ?dst ?src)
	 ;;   (asm-instr sll/overflow (?dst ?src))
	 ;;
	 ;;where ?DST is both read and written and ?SRC is read.
	 ;;
	 ;;While rewinding the  BODY traversal: ?DST changes its status  from live to
	 ;;dead to  live, it is the  moment to add  ?DST as node of  the interference
	 ;;graph.  The interference sub-graph of ?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set)
	 ;;
	 (let ((S (set-rem dst tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst -> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   (set-union (R src) tail.set)))

	((bset)
	 ;;We expect X ot have the format:
	 ;;
	 ;;   (asm-instr load8 (disp ?objref ?offset) ?src)
	 ;;
	 ;;here  ?SRC is  read, the  machine word  referenced by  the DISP  struct is
	 ;;written but ?OBJREF and ?OFFSET themselves are read.
	 ;;
	 ;;There is no true node created here because no location is written, but see
	 ;;below.  The live set returned to the caller is:
	 ;;
	 ;;   ?src + ?objref + ?offset + (tail.set)
	 ;;
	 (assert (disp? dst))
	 ;;The source  of an 8-bit  store cannot  be any register:  it can be  only a
	 ;;register among the ones that support  8-bit operations; for example EAX is
	 ;;fine, because we can  use the 8-bit register AL as  operand.  So, here, we
	 ;;add interference edges between SRC and all the CPU registers that do *not*
	 ;;support  8-bit  operations; this  makes  sure  that  no such  register  is
	 ;;allocated to SRC.
	 (when (var? src)
	   (for-each (lambda (reg)
		       (add-edge! THE-GRAPH src reg))
	     NON-8BIT-REGISTERS))
	 (set-union (set-union (R src) (R dst))
		    tail.set))

	((cltd)
	 ;;NOTE  CLTD (Convert  Long To  Double)  is the  AT&T conventional  mnemonic
	 ;;corresponding to the Intel  conventional mnemonic CDQ (Convert Double-word
	 ;;to Quad-word); it takes the value of EAX and sign-extends it to EDX:EAX.
	 ;;
	 ;;Here we know that DST is the register EDX and SRC is the register EAX.  We
	 ;;know that CLTD and IDIV always come together.
	 (assert (eq? dst edx))
	 (assert (eq? src eax))
	 ;;We expect X ot have the format:
	 ;;
	 ;;   (asm-instr cltd EDX EAX)
	 ;;
	 ;;here EAX is read and EDX is written.
	 ;;
	 ;;The interference sub-graph of EDX is:
	 ;;
	 ;;   EDX <--> (tail.set - EDX)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   EAX + (tail.set - EDX)
	 ;;
	 (let ((S (set-rem edx tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   EDX -> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH edx live-loc))
	     S)
	   (set-union (R eax) S)))

	((idiv)
	 ;;NOTE On  32-bit platforms, as  used by  Vicare, IDIV divides  (signed) the
	 ;;value EDX:EAX  (where EDX must contain  the sign-extension of EAX)  by the
	 ;;?SRC operand;  the results  are: EAX=quotient, EDX=remainder.   The source
	 ;;operand ?SRC can  be a general-purpose register or a  memory location.  As
	 ;;used by Vicare, the IDIV instruction always comes after a CLTD instruction
	 ;;that sign-extends the machine word in EAX to EDX:EAX.
	 ;;
	 ;;Here we know that DST is either  the register EAX or the register EDX; SRC
	 ;;is an operand, we do not know which  one here.  We know that CLTD and IDIV
	 ;;always come together.
	 (assert (or (eq? dst eax) (eq? dst edx)))
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr idiv EAX ?src)
	 ;;   (asm-instr idiv EDX ?src)
	 ;;
	 ;;here both  EAX and EDX are  written and read  and ?SRC is read.   The ?DST
	 ;;operand,  here, is  just a  place-holder and  it is  not used  to generate
	 ;;machine code.
	 ;;
	 ;;The interference sub-graphs of EAX and EDX are:
	 ;;
	 ;;   EAX <--> (tail.set - EAX - EDX)
	 ;;   EDX <--> (tail.set - EAX - EDX)
	 ;;
	 ;;and the live set returned to the caller is:
	 ;;
	 ;;   ?src + EAX + EDX + (tail.set)
	 ;;
	 (let ((S (set-rem eax (set-rem edx tail.set))))
	   (when (register? eax)
	     (set-for-each (lambda (y)
			     (add-edge! THE-GRAPH eax y)
			     (add-edge! THE-GRAPH edx y))
	       S))
	   (set-union (set-union (R eax) (R edx))
		      (set-union (R src) S))))

	(( ;;some assembly instructions
	  mset			mset32
	  fl:load		fl:store
	  fl:add!		fl:sub!
	  fl:mul!		fl:div!
	  fl:from-int		fl:shuffle
	  fl:store-single	fl:load-single)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr mset     (disp ?objref ?offset) ?src)
	 ;;   (asm-instr mset32   (disp ?objref ?offset) ?src)
	 ;;
	 ;;   (asm-instr fl:load  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:store ?flonum-operand ?offset)
	 ;;
	 ;;   (asm-instr fl:add!  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:sub!  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:mul!  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:div!  ?flonum-operand ?offset)
	 ;;
	 ;;   (asm-instr fl:from-int  (KN 0) ?int-operand)
	 ;;   (asm-instr fl:shuffle   ?pointer ?offset)
	 ;;
	 ;;   (asm-instr fl:store-single  ?pointer ?offset)
	 ;;   (asm-instr fl:load-single   ?pointer ?offset)
	 ;;
	 ;;where ?FLONUM-OPERAND is a tagged pointer to Scheme object of type flonum.
	 ;;Here  both ?DST  and ?SRC  are read  and neither  stack locations  nor CPU
	 ;;registers are written; so no nodes are added to the graph.
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + ?dst + (tail.set)
	 ;;
	 (set-union (R src)
		    (set-union (R dst) tail.set)))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid ASM-INSTR operator in E context" (unparse-recordised-code/sexp x)))))

    #| end of module: E |# )

  (main body))


(module (%color-graph)
  (import LISTY-SET)
  (import GRAPHS)

  (define* (%color-graph spillable.set unspillable.set G)
    ;;Non-tail recursive function.
    ;;
    ;;The  argument SPILLABLE.SET  must be  a  set including  the VAR  structs that  are
    ;;spillable.  The argument  UNSPILLABLE.SET must be a set including  the VAR structs
    ;;that are unspillable.  The argument G is a graph.
    ;;
    ;;Return 3 values:
    ;;
    ;;1. A list of VAR structs representing the spillED local variables.
    ;;
    ;;2. A set holding the VAR structs representing the spillABLE local variables.
    ;;
    ;;3.  An  ENV  value:  an  possibly  empty  alist  whose  keys  are  VAR  structs
    ;;   representing  local variables in BODY,  and whose values are  the associated
    ;;   locations: FVAR structs or CPU register symbol names.
    ;;
    (cond ((and (empty-set? spillable.set)
		(empty-set? unspillable.set))
	   (values '() (make-empty-set) '()))

	  ((find-low-degree (set->list unspillable.set) G)
	   => (lambda (unspillable)
		(let ((neighbor* (node-neighbors unspillable G)))
		  (delete-node! unspillable G)
		  (receive (spilled* spillable.set env)
		      (%color-graph spillable.set (set-rem unspillable unspillable.set) G)
		    (let ((register (find-color unspillable neighbor* env)))
		      (values spilled*
			      spillable.set
			      (cons (cons unspillable register) env)))))))

	  ((find-low-degree (set->list spillable.set) G)
	   => (lambda (spillable)
		(let ((neighbor* (node-neighbors spillable G)))
		  (delete-node! spillable G)
		  (receive (spilled* spillable.set env)
		      (%color-graph (set-rem spillable spillable.set) unspillable.set G)
		    (let ((register (find-color spillable neighbor* env)))
		      (values spilled*
			      (set-add spillable spillable.set)
			      (cons (cons spillable register) env)))))))

	  ((pair? (set->list spillable.set))
	   (let* ((spillable (car (set->list spillable.set)))
		  (neighbor* (node-neighbors spillable G)))
	     (delete-node! spillable G)
	     (receive (spilled* spillable.set env)
		 (%color-graph (set-rem spillable spillable.set) unspillable.set G)
	       (let ((register (find-color/maybe spillable neighbor* env)))
		 (if register
		     (values spilled*
			     (set-add spillable spillable.set)
			     (cons (cons spillable register) env))
		   (values (cons spillable spilled*)
			   spillable.set
			   env))))))

	  (else
	   (compiler-internal-error __module_who__ __who__ "this should never happen"))))

  (define (find-low-degree ls G)
    (and (pair? ls)
	 (if (fx< (length (set->list (node-neighbors (car ls) G)))
		  (length ALL-REGISTERS))
	     (car ls)
	   (find-low-degree (cdr ls) G))))

  (define* (find-color x confs env)
    ;;The argument X is a VAR struct for which we want to allocate a register.
    ;;
    ;;The argument  ENV is  an alist  whose keys are  VAR structs  representing local
    ;;variables in BODY, and whose values  are the associated locations: FVAR structs
    ;;or CPU register symbol names.
    ;;
    (or (find-color/maybe x confs env)
	(compiler-internal-error __module_who__ __who__
	  "cannot find color local variable" x)))

  (module (find-color/maybe)

    (define (find-color/maybe x confs env)
      ;;The argument X is a VAR struct for which we want to allocate a register.
      ;;
      ;;The argument  ENV is an alist  whose keys are VAR  structs representing local
      ;;variables  in BODY,  and  whose  values are  the  associated locations:  FVAR
      ;;structs or CPU register symbol names.
      ;;
      (define available-register*
	;;Compose a  set containing all the  registers that are alive  here; then and
	;;remove them from  the set of all  the registers.  The result is  the set of
	;;registers that are *not* alive here, which can be allocated for to hold the
	;;VAR struct X.
	(let ((cr ($fold-right/stx (lambda (x knil)
				     (cond ((register? x)
					    (cons x knil))
					   ((assq x env)
					    => (lambda (entry)
						 (let ((loc (cdr entry)))
						   (if (register? loc)
						       (cons loc knil)
						     knil))))
					   (else
					    ;;If  we are  here:  X is  either a  FVAR
					    ;;struct or a VAR struct not in ENV.
					    (assert (or (var? x) (fvar? x)))
					    knil)))
		    '()
		    (set->list confs))))
	  (set->list (set-difference ALL-REGISTERS-SET (list->set cr)))))
      (and (pair? available-register*)
	   (car   available-register*)))

    (define-constant ALL-REGISTERS-SET
      (list->set ALL-REGISTERS))

    #| end of module |# )

  #| end of module: %COLOR-GRAPH |# )


(define* (%substitute-vars-with-associated-locations env body)
  ;;The argument BODY  must represent recordised code.  The argument  ENV is an alist
  ;;whose keys are VAR structs representing local variables in BODY, and whose values
  ;;are the associated locations: FVAR structs or CPU register symbol names.
  ;;
  ;;This function  builds and returns  a new struct instance  representing recordised
  ;;code, which is meant  to replace BODY.  The purpose of this  function is to apply
  ;;the  subfunction R  to  the operands  in  the structures  of  type ASM-INSTR  and
  ;;ASMCALL; as a consequence: we apply the subfunction R-var to all the VAR structs.
  ;;The subfunction R-var replaces its VAR argument with the associated location.
  ;;
  ;;A lot  of functions  are nested  here because  they make  use of  the subfunction
  ;;"R-var", and "R-var" needs to close upon the argument ENV.
  ;;
  (module (R)

    (define (R x)
      (struct-case x
	((constant)
	 x)
	((var)
	 (R-var x))
	((fvar)
	 x)
	((nfv unused.idx loc)
	 #;(assert (fvar? loc))
	 loc)
	((disp objref offset)
	 (make-disp (R-disp objref) (R-disp offset)))
	(else
	 (if (register? x)
	     x
	   (compiler-internal-error __module_who__  __who__
	     "invalid operand struct" x)))))

    (define (R-disp x)
      (struct-case x
	((constant)
	 x)
	((var)
	 (R-var x))
	((fvar)
	 x)
	(else
	 (if (register? x)
	     x
	   (compiler-internal-error __module_who__  __who__
	     "invalid DISP field value" x)))))

    (define (R-var x)
      (cond ((assq x env)
	     ;;This VAR struct  is in the list of spilled  variables: replace it with
	     ;;its associated FVAR struct.
      	     => cdr)
	    ;;This VAR struct is not in the  list of spilled variables: just leave it
	    ;;alone.
      	    (else x)))

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
      ((asmcall op)
       #;(assert (or (eq? op 'return) (eq? op 'direct-jump) (eq? op 'indirect-jump)))
       ;;We assume the input  code is correct; this means in  tail position there are
       ;;only ASMCALL structs with operand  RETURN, DIRECT-JUMP or INDIRECT-JUMP.  We
       ;;know that the operands of such ASMCALL structs are CPU register symbol names
       ;;and FVAR  structs; so there  are no VAR  structs to substitute  there.  Just
       ;;return X itself.
       x)
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (T e1) (T e2)))
      ((seq e0 e1)
       (make-seq (E e0) (T e1)))
      ((shortcut body handler)
       (make-shortcut (T body) (T handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid tail" (unparse-recordized-code x)))))

  (T body))


;;;; done

#| end of module: color-by-chaitin |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'set-for-each		'scheme-indent-function 1)
;; End:
