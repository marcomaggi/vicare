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


(module (assign-frame-sizes)
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   asm-instr	conditional	constant
  ;;   locals		nframe		non-tail-call
  ;;   asmcall		seq		shortcut
  ;;
  ;;in addition CLOSURE-MAKER structs can appear in side CONSTANT structs.
  ;;
  (import IntegerSet)
  (import FRAME-CONFLICT-HELPERS)
  (import INTEL-ASSEMBLY-CODE-GENERATION)
  (define who 'assign-frame-sizes)


;;;;

(define (assign-frame-sizes x)
  (let ((v (Program x)))
    v))

(module (Program)
  ;;The purpose of  this module is to apply the  function "Main" below
  ;;to all the bodies.
  ;;
  (define (Program x)
    (struct-case x
      ((codes code* body)
       (make-codes (map Clambda code*) (Main body)))))

  (define (Clambda x)
    (struct-case x
      ((clambda label case* cp freevar* name)
       (make-clambda label (map ClambdaCase case*) cp freevar* name))))

  (define (ClambdaCase x)
    (struct-case x
      ((clambda-case info body)
       (make-clambda-case info (Main body)))))

  (define (Main x)
    ;;X must  be a struct instance  of type LOCALS.  Update  the field
    ;;VARS of X and return a  new struct instance of type LOCALS which
    ;;is meant to replace X.
    ;;
    (struct-case x
      ((locals vars body)
       (init-vars! vars)
       (let* ((vars.vec		(list->vector vars))
	      (call-live*	(uncover-frame-conflicts body vars.vec))
	      (body		(%rewrite body vars.vec)))
	 (make-locals (cons vars.vec (%discard-vars-with-loc vars))
		      body)))
      (else
       (error who "invalid main" x))))

  (define (%discard-vars-with-loc vars)
    ;;Given a list of struct instances  of type VAR, return a new list
    ;;containing only those having #f in the LOC field.
    ;;
    (cond ((null? vars)
	   '())
	  (($var-loc (car vars))
	   (%discard-vars-with-loc (cdr vars)))
	  (else
	   (cons (car vars) (%discard-vars-with-loc (cdr vars))))))

  #| end of module: Program |# )

;;; --------------------------------------------------------------------

(define (%rewrite x vars.vec)
  ;;X must be a struct instance representing a recordized body.
  ;;
  ;;A lot of functions are nested here because they need to close upon
  ;;the argument VARS.VEC.
  ;;
  (define (NFE idx mask x)
    (struct-case x
      ((seq e0 e1)
       (let ((e0^ (E e0)))
	 (make-seq e0^ (NFE idx mask e1))))
      ((non-tail-call target value args mask^ size)
       (make-non-tail-call target value
		    (map (lambda (x)
			   (cond ((symbol? x)
				  x)
				 ((nfv? x)
				  ($nfv-loc x))
				 (else
				  (error who "invalid arg"))))
		      args)
		    mask idx))
      (else
       (error who "invalid NF effect" x))))

  (define (Var x)
    (cond (($var-loc x)
	   => (lambda (loc)
		(if (fvar? loc)
		    loc
		  (%assign x vars.vec))))
	  (else x)))

  (define (R x)
    (cond ((or (constant? x)
	       (reg?      x)
	       (fvar?     x))
	   x)
	  ((nfv? x)
	   (or ($nfv-loc x)
	       (error who "unassigned nfv")))
	  ((var? x)
	   (Var x))
	  ((disp? x)
	   (make-disp (R ($disp-s0 x)) (R ($disp-s1 x))))
	  (else
	   (error who "invalid R" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x)
      (struct-case x
	((seq e0 e1)
	 (let ((e0^ (E e0)))
	   (make-seq e0^ (E e1))))

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (E e1) (E e2)))

	((asm-instr op d s)
	 (E-asm-instr op d s))

	((nframe vars live body)
	 (E-nframe vars live body))

	((asmcall op args)
	 (case op
	   ((nop interrupt incr/zero? fl:double->single fl:single->double)
	    x)
	   (else
	    (error who "invalid effect prim" op))))

	((shortcut body handler)
	 (make-shortcut (E body) (E handler)))

	(else
	 (error who "invalid effect" (unparse-recordized-code x)))))

    (define (E-asm-instr op d s)
      (case op
	((move load8 load32)
	 ;;If  the   destination  equals  the  source:   convert  this
	 ;;instruction into a NOP.
	 (let ((d (R d))
	       (s (R s)))
	   (if (eq? d s)
	       (nop)
	     (make-asm-instr op d s))))

	(( ;;some assembly instructions
	  logand		logor		logxor
	  int+			int-		int*
	  mset			mset32
	  bset			bswap!
	  sll			sll/overflow
	  sra			srl
	  cltd			idiv
	  int-/overflow		int+/overflow	int*/overflow
	  fl:load		fl:store
	  fl:add!		fl:sub!		fl:mul!		fl:div!
	  fl:from-int		fl:shuffle	fl:load-single	fl:store-single)
	 (make-asm-instr op (R d) (R s)))

	((nop)
	 (nop))

	(else
	 (error who "invalid op" op))))

    (define (E-nframe vars live body)
      (let ((live-frms1 (map (lambda (i)
			       (Var (vector-ref vars.vec i)))
			  (set->list (vector-ref live 0))))
	    (live-frms2 (set->list (vector-ref live 1)))
	    (live-nfvs  (vector-ref live 2)))

	(define (max-frm ls i)
	  (if (pair? ls)
	      (max-frm (cdr ls) (max i ($fvar-idx (car ls))))
	    i))

	(define (max-ls ls i)
	  (if (pair? ls)
	      (max-ls  (cdr ls) (max i (car ls)))
	    i))

	(define (max-nfv ls i)
	  (if (pair? ls)
	      (let ((loc ($nfv-loc (car ls))))
		(unless (fvar? loc)
		  (error who "FVAR not assigned in MAX-NFV" loc))
		(max-nfv (cdr ls) (max i ($fvar-idx loc))))
	    i))

	(module (actual-frame-size)

	  (define (actual-frame-size vars i)
	    (if (%frame-size-ok? i vars)
		i
	      (actual-frame-size vars (fxadd1 i))))

	  (define (%frame-size-ok? i vars)
	    (or (null? vars)
		(let ((x (car vars)))
		  (and (not (set-member?    i ($nfv-frm-conf x)))
		       (not (%var-conflict? i ($nfv-var-conf x)))
		       (%frame-size-ok? (fxadd1 i) (cdr vars))))))

	  (define (%var-conflict? i vs)
	    (ormap (lambda (xi)
		     (let ((loc ($var-loc (vector-ref vars.vec xi))))
		       (and (fvar? loc)
			    (fx=? i ($fvar-idx loc)))))
		   (set->list vs)))

	  #| end of module: actual-frame-size |# )

	(define (%assign-frame-vars! vars i)
	  (when (pair? vars)
	    (let ((v  (car vars))
		  (fv (mkfvar i)))
	      ($set-nfv-loc! v fv)
	      (for-each (lambda (x)
			  (let ((loc ($nfv-loc x)))
			    (if loc
				(when (fx=? ($fvar-idx loc) i)
				  (error who "invalid assignment"))
			      (begin
				($set-nfv-nfv-conf! x (rem-nfv v  ($nfv-nfv-conf x)))
				($set-nfv-frm-conf! x (add-frm fv ($nfv-frm-conf x)))))))
		($nfv-nfv-conf v))
	      (for-each-var ($nfv-var-conf v) vars.vec
			    (lambda (x)
			      (let ((loc ($var-loc x)))
				(if (fvar? loc)
				    (when (fx=? (fvar-idx loc) i)
				      (error who "invalid assignment"))
				  ($set-var-frm-conf! x (add-frm fv ($var-frm-conf x))))))))
	    (%assign-frame-vars! (cdr vars) (fxadd1 i))))

	(module (make-mask)

	  (define (make-mask n)
	    (let ((vec (make-vector (fxsra (fx+ n 7) 3) 0)))
	      (for-each (lambda (fvar)
			  (%set-bit! vec ($fvar-idx fvar)))
		live-frms1)
	      (for-each (lambda (idx)
			  (%set-bit! vec idx))
		live-frms2)
	      (for-each (lambda (nfv)
			  (let ((loc ($nfv-loc nfv)))
			    (when loc
			      (%set-bit! vec ($fvar-idx loc)))))
		live-nfvs)
	      vec))

	  (define (%set-bit! vec idx)
	    (let ((q (fxsra    idx 3))
		  (r (fxlogand idx 7)))
	      (vector-set! vec q (fxlogor (vector-ref vec q) (fxsll 1 r)))))

	  #| end of module: make-mask |# )

	(let ((i (actual-frame-size
		  vars
		  (fx+ 2 (max-frm live-frms1
				  (max-nfv live-nfvs
					   (max-ls live-frms2 0)))))))
	  (%assign-frame-vars! vars i)
	  (NFE (fxsub1 i) (make-mask (fxsub1 i)) body))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (define (P x)
    (struct-case x
      ((seq e0 e1)
       (let ((e0^ (E e0)))
	 (make-seq e0^ (P e1))))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))

      ((asm-instr op d s)
       (make-asm-instr op (R d) (R s)))

      ((constant)
       x)

      ((shortcut body handler)
       (make-shortcut (P body) (P handler)))

      (else
       (error who "invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (T x)
    ;;Process the struct instance X representing recordized code as if
    ;;it is in tail position.
    ;;
    (struct-case x
      ((seq e0 e1)
       (let ((e0^ (E e0)))
	 (make-seq e0^ (T e1))))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (T e1) (T e2)))

      ((asmcall op args)
       x)

      ((shortcut body handler)
       (make-shortcut (T body) (T handler)))

      (else
       (error who "invalid tail" (unparse-recordized-code x)))))

  (T x))

;;; --------------------------------------------------------------------

(module (%assign)

  (define (%assign x vars.vec)
    (or (%assign-move x vars.vec)
	(%assign-any  x vars.vec)))

  (define (%assign-any x vars.vec)
    (let ((frms ($var-frm-conf x))
	  (vars ($var-var-conf x)))
      (let loop ((i 1))
	(if (set-member? i frms)
	    (loop (fxadd1 i))
	  (receive-and-return (fv)
	      (mkfvar i)
	    ($set-var-loc! x fv)
	    (for-each-var vars vars.vec
			  (lambda (var)
			    ($set-var-frm-conf! var (add-frm fv ($var-frm-conf var))))))))))

  (define (%assign-move x vars.vec)
    (let ((mr (set->list (set-difference ($var-frm-move x) ($var-frm-conf x)))))
      (and (pair? mr)
	   (receive-and-return (fv)
	       (mkfvar (car mr))
	     ($set-var-loc! x fv)
	     (for-each-var ($var-var-conf x) vars.vec
			   (lambda (var)
			     ($set-var-frm-conf! var (add-frm fv ($var-frm-conf var)))))
	     (for-each-var ($var-var-move x) vars.vec
			   (lambda (var)
			     ($set-var-frm-move! var (add-frm fv ($var-frm-move var)))))))))

  #| end of module: %assign |# )


(define (uncover-frame-conflicts x vars.vec)
  ;;This function is used only by ASSIGN-FRAME-SIZES.
  ;;
  (import IntegerSet)
  (import FRAME-CONFLICT-HELPERS)
  (define who 'uncover-frame-conflicts)

  (define spill-set
    ;;This will be the return value.
    (make-empty-set))

  (define (mark-reg/vars-conf! r vs)
    (for-each-var vs vars.vec
      (lambda (v)
        ($set-var-reg-conf! v (add-reg r ($var-reg-conf v))))))

  (define (mark-frm/vars-conf! f vs)
    (for-each-var vs vars.vec
      (lambda (v)
        ($set-var-frm-conf! v (add-frm f ($var-frm-conf v))))))

  (define (mark-frm/nfvs-conf! f ns)
    (for-each-nfv ns
      (lambda (n)
        ($set-nfv-frm-conf! n (add-frm f ($nfv-frm-conf n))))))

  (define (mark-var/vars-conf! v vs)
    (for-each-var vs vars.vec
      (lambda (w)
        ($set-var-var-conf! w (add-var v ($var-var-conf w)))))
    ($set-var-var-conf! v (union-vars vs ($var-var-conf v))))

  (define (mark-var/frms-conf! v fs)
    ($set-var-frm-conf! v (union-frms fs ($var-frm-conf v))))

  (define (mark-var/regs-conf! v rs)
    ($set-var-reg-conf! v (union-regs rs ($var-reg-conf v))))

  (define (mark-var/nfvs-conf! v ns)
    (for-each-nfv ns
      (lambda (n)
        ($set-nfv-var-conf! n (add-var v ($nfv-var-conf n))))))

  (define (mark-nfv/vars-conf! n vs)
    ($set-nfv-var-conf! n (union-vars vs ($nfv-var-conf n))))

  (define (mark-nfv/frms-conf! n fs)
    ($set-nfv-frm-conf! n (union-frms fs ($nfv-frm-conf n))))

  (define (mark-nfv/nfvs-conf! n ns)
    ($set-nfv-nfv-conf! n (union-nfvs ns ($nfv-nfv-conf n)))
    (for-each-nfv ns
      (lambda (m)
        ($set-nfv-nfv-conf! m (add-nfv n ($nfv-nfv-conf m))))))

  (define (mark-var/var-move! x y)
    ($set-var-var-move! x (add-var y ($var-var-move x)))
    ($set-var-var-move! y (add-var x ($var-var-move y))))

  (define (mark-var/frm-move! x y)
    ($set-var-frm-move! x (add-frm y ($var-frm-move x))))

  (define (mark-var/reg-move! x y)
    ($set-var-reg-move! x (add-reg y ($var-reg-move x))))

  (define (const? x)
    (or (constant? x)
        (code-loc? x)))

  (define (R x vs rs fs ns)
    (cond ((const? x)
	   (values vs rs fs ns))
	  ((reg? x)
	   (values vs (add-reg x rs) fs ns))
	  ((fvar? x)
	   (values vs rs (add-frm x fs) ns))
	  ((var? x)
	   (values (add-var x vs) rs fs ns))
	  ((nfv? x)
	   (values vs rs fs (add-nfv x ns)))
	  ((disp? x)
	   (let-values (((vs rs fs ns)
			 (R (disp-s0 x) vs rs fs ns)))
	     (R (disp-s1 x) vs rs fs ns)))
	  (else
	   (error who "invalid R" x))))

  (define (R* ls vs rs fs ns)
    (if (pair? ls)
	(receive (vs rs fs ns)
	    (R (car ls) vs rs fs ns)
	  (R* (cdr ls) vs rs fs ns))
      (values vs rs fs ns)))

;;; --------------------------------------------------------------------

  (define (E x vs rs fs ns)
    (struct-case x

      ((seq e0 e1)
       (let-values (((vs rs fs ns)
		     (E e1 vs rs fs ns)))
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values (((vs1 rs1 fs1 ns1)  (E e1 vs rs fs ns))
                    ((vs2 rs2 fs2 ns2)  (E e2 vs rs fs ns)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((asm-instr op d s)
       (case op
         ((move load8 load32)
          (cond ((reg? d)
		 (cond ((not (mem-reg? d rs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       ((or (const? s) (disp? s) (reg? s))
			(let ((rs (rem-reg d rs)))
			  (mark-reg/vars-conf! d vs)
			  (R s vs rs fs ns)))
		       ((var? s)
			(let ((rs (rem-reg d rs))
			      (vs (rem-var s vs)))
			  (mark-var/reg-move! s d)
			  (mark-reg/vars-conf! d vs)
			  (values (add-var s vs) rs fs ns)))
		       ((fvar? s)
			(let ((rs (rem-reg d rs)))
			  (mark-reg/vars-conf! d vs)
			  (values vs rs (add-frm s fs) ns)))
		       (else
			(error who "invalid rs" (unparse-recordized-code x)))))
		((fvar? d)
		 (cond ((not (mem-frm? d fs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       ((or (const? s) (disp? s) (reg? s))
			(let ((fs (rem-frm d fs)))
			  (mark-frm/vars-conf! d vs)
			  (mark-frm/nfvs-conf! d ns)
			  (R s vs rs fs ns)))
		       ((var? s)
			(let ((fs (rem-frm d fs))
			      (vs (rem-var s vs)))
			  (mark-var/frm-move! s d)
			  (mark-frm/vars-conf! d vs)
			  (mark-frm/nfvs-conf! d ns)
			  (values (add-var s vs) rs fs ns)))
		       (else
			(error who "invalid fs" s))))
		((var? d)
		 (cond ((not (mem-var? d vs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       ((or (disp? s) (constant? s))
			(let ((vs (rem-var d vs)))
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (R s vs rs fs ns)))
		       ((reg? s)
			(let ((vs (rem-var d vs))
			      (rs (rem-reg s rs)))
			  (mark-var/reg-move! d s)
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (values vs (add-reg s rs) fs ns)))
		       ((var? s)
			(let ((vs (rem-var d (rem-var s vs))))
			  (mark-var/var-move! d s)
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (values (add-var s vs) rs fs ns)))
		       ((fvar? s)
			(let ((vs (rem-var d vs))
			      (fs (rem-frm s fs)))
			  (mark-var/frm-move! d s)
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (values vs rs (add-frm s fs) ns)))
		       (else
			(error who "invalid vs" s))))
		((nfv? d)
		 (cond ((not (mem-nfv? d ns))
			(error who "dead nfv"))
		       ((or (disp?     s)
			    (constant? s)
			    (reg?      s))
			(let ((ns (rem-nfv d ns)))
			  (mark-nfv/vars-conf! d vs)
			  (mark-nfv/frms-conf! d fs)
			  (R s vs rs fs ns)))
		       ((var? s)
			(let ((ns (rem-nfv d ns))
			      (vs (rem-var s vs)))
			  (mark-nfv/vars-conf! d vs)
			  (mark-nfv/frms-conf! d fs)
			  (values (add-var s vs) rs fs ns)))
		       ((fvar? s)
			(let ((ns (rem-nfv d ns))
			      (fs (rem-frm s fs)))
			  (mark-nfv/vars-conf! d vs)
			  (mark-nfv/frms-conf! d fs)
			  (values vs rs (add-frm s fs) ns)))
		       (else
			(error who "invalid ns" s))))
		(else
		 (error who "invalid d" d))))
         ((int-/overflow int+/overflow int*/overflow)
          (let ((v (exception-live-set)))
            (unless (vector? v)
              (error who "unbound exception" x v))
            (let ((vs (union-vars vs (vector-ref v 0)))
                  (rs (union-regs rs (vector-ref v 1)))
                  (fs (union-frms fs (vector-ref v 2)))
                  (ns (union-nfvs ns (vector-ref v 3))))
              (cond ((var? d)
		     (cond ((not (mem-var? d vs))
			    (set-asm-instr-op! x 'nop)
			    (values vs rs fs ns))
			   (else
			    (let ((vs (rem-var d vs)))
			      (mark-var/vars-conf! d vs)
			      (mark-var/frms-conf! d fs)
			      (mark-var/nfvs-conf! d ns)
			      (mark-var/regs-conf! d rs)
			      (R s (add-var d vs) rs fs ns)))))
		    ((reg? d)
		     (if (not (mem-reg? d rs))
			 (values vs rs fs ns)
		       (let ((rs (rem-reg d rs)))
			 (mark-reg/vars-conf! d vs)
			 (R s vs (add-reg d rs) fs ns))))
		    ((nfv? d)
		     (if (not (mem-nfv? d ns))
			 (error who "dead nfv")
		       (let ((ns (rem-nfv d ns)))
			 (mark-nfv/vars-conf! d vs)
			 (mark-nfv/frms-conf! d fs)
			 (R s vs rs fs (add-nfv d ns)))))
		    (else
		     (error who "invalid op d" (unparse-recordized-code x)))))))
         ((nop)
	  (values vs rs fs ns))
         ((logand logor logxor sll sra srl int+ int- int* bswap!
           sll/overflow)
          (cond ((var? d)
		 (cond ((not (mem-var? d vs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       (else
			(let ((vs (rem-var d vs)))
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/nfvs-conf! d ns)
			  (mark-var/regs-conf! d rs)
			  (R s (add-var d vs) rs fs ns)))))
		((reg? d)
		 (cond ((not (mem-reg? d rs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       (else
			(let ((rs (rem-reg d rs)))
			  (mark-reg/vars-conf! d vs)
			  (R s vs (add-reg d rs) fs ns)))))
		((nfv? d)
		 (if (not (mem-nfv? d ns))
		     (error who "dead nfv")
		   (let ((ns (rem-nfv d ns)))
		     (mark-nfv/vars-conf! d vs)
		     (mark-nfv/frms-conf! d fs)
		     (R s vs rs fs (add-nfv d ns)))))
		(else
		 (error who "invalid op d" (unparse-recordized-code x)))))
         ((idiv)
          (mark-reg/vars-conf! eax vs)
          (mark-reg/vars-conf! edx vs)
          (R s vs (add-reg eax (add-reg edx rs)) fs ns))
         ((cltd)
          (mark-reg/vars-conf! edx vs)
          (R s vs (rem-reg edx rs) fs ns))
         ((mset mset32 bset
           fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:from-int
           fl:shuffle fl:load-single fl:store-single)
          (R* (list s d) vs rs fs ns))
         (else
	  (error who "invalid effect op" (unparse-recordized-code x)))))

      ((non-tail-call target value args mask size)
       (set! spill-set (union-vars vs spill-set))
       (for-each-var vs vars.vec (lambda (x)
				   ($set-var-loc! x #t)))
       (R* args vs (empty-reg-set) fs ns))

      ((nframe nfvs live body)
       (for-each init-nfv! nfvs)
       (set-nframe-live! x (vector vs fs ns))
       (E body vs rs fs ns))

      ((asmcall op args)
       (case op
         ((nop fl:double->single fl:single->double)
	  (values vs rs fs ns))
         ((interrupt incr/zero?)
          (let ((v (exception-live-set)))
            (if (vector? v)
		(values (vector-ref v 0)
			(vector-ref v 1)
			(vector-ref v 2)
			(vector-ref v 3))
              (error who "unbound exception2"))))
         (else
	  (error who "invalid effect op" op))))

      ((shortcut body handler)
       (let-values (((vsh rsh fsh nsh)
		     (E handler vs rs fs ns)))
	 (parameterize ((exception-live-set (vector vsh rsh fsh nsh)))
            (E body vs rs fs ns))))

      (else
       (error who "invalid effect" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (P x vst rst fst nst
               vsf rsf fsf nsf
               vsu rsu fsu nsu)
    (struct-case x
      ((seq e0 e1)
       (let-values (((vs rs fs ns)
                     (P e1 vst rst fst nst
                        vsf rsf fsf nsf
                        vsu rsu fsu nsu)))
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values (((vs1 rs1 fs1 ns1)
                     (P e1 vst rst fst nst
                           vsf rsf fsf nsf
			   vsu rsu fsu nsu))
                    ((vs2 rs2 fs2 ns2)
                     (P e2 vst rst fst nst
                           vsf rsf fsf nsf
			   vsu rsu fsu nsu)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((constant t)
       (if t
           (values vst rst fst nst)
           (values vsf rsf fsf nsf)))

      ((asm-instr op d s)
       (R* (list d s) vsu rsu fsu nsu))

      ((shortcut body handler)
       (let-values (((vsh rsh fsh nsh)
                     (P handler vst rst fst nst
                                vsf rsf fsf nsf
                                vsu rsu fsu nsu)))
          (parameterize ((exception-live-set (vector vsh rsh fsh nsh)))
            (P body vst rst fst nst
                    vsf rsf fsf nsf
                    vsu rsu fsu nsu))))

      (else
       (error who "invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (T x)
    ;;Process the struct  instance T representing recordized  code as if
    ;;it is in tail position.
    ;;
    (struct-case x
      ((seq e0 e1)
       (let-values (((vs rs fs ns) (T e1)))
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values (((vs1 rs1 fs1 ns1) (T e1))
                    ((vs2 rs2 fs2 ns2) (T e2)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((asmcall op arg*)
       (case op
         ((return indirect-jump direct-jump)
          (R* arg*
	      (empty-var-set)
              (empty-reg-set)
              (empty-frm-set)
              (empty-nfv-set)))
         (else
	  (error who "invalid tail op" x))))

      ((shortcut body handler)
       (let-values (((vsh rsh fsh nsh) (T handler)))
          (parameterize ((exception-live-set (vector vsh rsh fsh nsh)))
             (T body))))

      (else
       (error who "invalid tail" x))))

;;; --------------------------------------------------------------------

  (define exception-live-set
    (make-parameter #f))

  (T x)
  spill-set)


;;;; done

#| end of module: assign-frame-sizes |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'assemble-sources		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; End:
