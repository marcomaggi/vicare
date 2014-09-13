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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should have received a copy of the GNU General Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(module (introduce-tags)
  ;;This optional compiler  pass analyses the type of values  returned by expressions
  ;;with the purpose of transforming FUNCALL recordised forms:
  ;;
  ;;   (funcall (primref ?prim-name) ?rand ...)
  ;;   (funcall ?rator               ?rand ...)
  ;;
  ;;into:
  ;;
  ;;   (funcall (primref ?prim-name)       (known ?rand ?rand-type) ...)
  ;;   (funcall (known ?rator ?rator-type) (known ?rand ?rand-type) ...)
  ;;
  ;;where: ?RATOR-TYPE is the type description  of the value returned by ?RATOR; each
  ;;?RAND-TYPE is  the type description  of the  value returned by  the corresponding
  ;;?RAND; a PRIMREF  operator is left untouched.  The type  descriptions are records
  ;;of type CORE-TYPE-TAG whose represented types are inferred by this compiler pass.
  ;;
  ;;The  structs of  type  KNOWN  are annotation  "tags"  consumed  by the  functions
  ;;generating  the implementation  of the  core primitive  operations; for  example,
  ;;given the recordised code:
  ;;
  ;;   (funcall (primref vector-length) ?rand)
  ;;
  ;;which makes use  of the primitive operation VECTOR-LENGTH:
  ;;
  ;;*  If no  type tag  is  assigned to  ?RAND: the  implementation of  VECTOR-LENGTH
  ;;   integrated at  the call  site must  include a  validation of  ?RAND as  vector
  ;;  object.
  ;;
  ;;* If the tag "T:vector" is introduced for the operand:
  ;;
  ;;     (funcall (primref vector-length) (known ?rand T:vector))
  ;;
  ;;  the  implementation of  VECTOR-LENGTH integrated  at the  call site  does *not*
  ;;  include a validation of ?RAND as vector object.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda
  ;;   forcall		funcall
  ;;
  ;;NOTE Every PRELEX struct in the  input expression must represent a proper lexical
  ;;binding defined by a BIND or FIX struct.
  ;;
  ;;NOTE This module stores a number in the field OPERAND of the PRELEX structs.
  ;;
  (import SCHEME-OBJECTS-ONTOLOGY)

  (define-fluid-override __who__
    (identifier-syntax 'introduce-tags))

  (define (introduce-tags x)
    (receive (y env y.tag)
	(V x EMPTY-ENV)
      y))


(module (V)

  (define (V x env)
    ;;
    ;;ENV maps PRELEX structs to records of type CORE-TYPE-TAG.
    ;;
    (struct-case x
      ((constant k)
       (values x env (%determine-constant-type k)))

      ((prelex)
       ;;We search  the PRELEX in  the environment collected  so far to  retrieve its
       ;;type tag  (previously determined when  processing the RHS expression  of the
       ;;binding struct that defined the PRELEX's binding).
       (values x env (%determine-prelex-type x env)))

      ((primref op)
       ;;This PRIMREF is standalone, it is not the operator of a FUNCALL; so it is an
       ;;error if it references a core primitive operation that is not also a lexical
       ;;core primitive function.
       (values x env T:procedure))

      ((seq x.e0 x.e1)
       ;;First we  do X.E0  and after we  do X.E1; this  allows type  propagation for
       ;;PRELEX structures  used as arguments to  core primitives: a type  tag can be
       ;;inferred in E0 and it can be used in E1.
       (receive (e0 e0.env e0.tag.unused)
	   (V x.e0 env)
	 (receive (e1 e1.env e1.tag)
	     (V x.e1 e0.env)
	   (values (make-seq e0 e1) e1.env e1.tag))))

      ((conditional x.test x.conseq x.altern)
       ;;FIXME Should TEST.ENV be merged  with CONSEQ.ENV and ALTERN.ENV to propagate
       ;;tag informations?  I am not sure.  (Marco Maggi; Sat Sep 13, 2014)
       (receive (test test.env test.tag)
	   (V x.test env)
	 (case (T:false? test.tag)
	   ((yes)
	    ;;We know the test is false, so do the transformation:
	    ;;
	    ;;   (conditional ?test ?conseq ?altern)
	    ;;   ==> (seq ?test ?conseq)
	    ;;
	    ;;we keep ?TEST for its side effects.
	    (receive (altern altern.env altern.tag)
		(V x.altern env)
	      (values (make-seq test altern) altern.env altern.tag)))
	   ((no)
	    ;;We know the test is true, so do the transformation:
	    ;;
	    ;;   (conditional ?test ?conseq ?altern)
	    ;;   ==> (seq ?test ?altern)
	    ;;
	    ;;we kepp ?TEST for its side effects.
	    (receive (conseq conseq.env conseq.tag)
		(V x.conseq env)
	      (values (make-seq test conseq) conseq.env conseq.tag)))
	   (else
	    ;;We do not know the result of the test.
	    (let-values
		(((conseq conseq.env conseq.tag) (V x.conseq env))
		 ((altern altern.env altern.tag) (V x.altern env)))
	      (values (make-conditional test conseq altern)
		      (%or-envs conseq.env altern.env)
		      (core-type-tag-or conseq.tag altern.tag)))))))

      ((bind lhs* x.rhs* x.body)
       (receive (rhs* env^ rhs*.tag)
	   (V* x.rhs* env)
	 ;;Assign a unique number to each  PRELEX.  Remember that the RHS expressions
	 ;;of a BIND do *not* reference the LHS PRELEX structs.
	 ($for-each/stx %assign-index-to-prelex! lhs*)
	 (receive (body body.env body.tag)
	     (V x.body (extend-env* lhs* rhs*.tag env^))
	   (values (make-bind lhs* rhs* body) body.env body.tag))))

      ((fix lhs* x.rhs* x.body)
       ;;Assign a unique number to each PRELEX.  Remember that the RHS expressions of
       ;;a FIX might reference the LHS PRELEX structs.
       ($for-each/stx %assign-index-to-prelex! lhs*)
       (receive (rhs* env^ rhs*.tag)
	   (V* x.rhs* env)
	 (receive (body body.env body.tag)
	     (V x.body (extend-env* lhs* rhs*.tag env^))
	   (values (make-fix lhs* rhs* body) body.env body.tag))))

      ((clambda label clause* cp free name)
       (values (make-clambda label
			     ($map/stx (lambda (clause)
					 (struct-case clause
					   ((clambda-case info body)
					    ;;Assign a unique number to each PRELEX.
					    ($for-each/stx %assign-index-to-prelex! (case-info-args info))
					    (receive (body env t)
						(V body env)
					      ;;We drop the ENV and T of the body.
					      (make-clambda-case info body)))))
			       clause*)
			     cp free name)
	       env T:procedure))

      ((funcall rator rand*)
       (let-values
	   (((rator rator.env rator.tag) (V  rator env))
	    ((rand* rand*.env rand*.tag) (V* rand* env)))
	 (%apply-funcall rator     rand*
			 rator.tag rand*.tag
			 rator.env rand*.env)))

      ((forcall rator rand*)
       (receive (rand* rand*.env rand*.tag)
	   (V* rand* env)
	 ;;FIXME Maybe, by inspecting the RATOR, we could determine a better type tag
	 ;;than "T:object" for  FORCALL.  Remember that the type tag  of this FORCALL
	 ;;struct  is the  one of  the object  returned by  the RATOR,  which is  a C
	 ;;language foreign function.  (Marco Maggi; Fri Sep 12, 2014)
	 (values (make-forcall rator rand*) rand*.env T:object)))

      (else
       (compiler-internal-error __who__
	 "invalid expression" (unparse-recordized-code x)))))

  (define (V* x* env)
    ;;Apply V to all the structs in the list X*; gather the environment.
    ;;
    (if (null? x*)
	(values '() env '())
      (let-values (((x  env1 t)  (V  ($car x*) env))
		   ((x* env2 t*) (V* ($cdr x*) env)))
	(values (cons x x*)
		(%and-envs env1 env2)
		(cons t t*)))))

;;; --------------------------------------------------------------------

  (define %assign-index-to-prelex!
    (let ((i 0))
      (lambda (x)
	(set-prelex-operand! x i)
	(set! i (fxadd1 i)))))

  (define (%determine-prelex-type prel env)
    ;;Search the alist ENV  for an entry whose key is the index  of the PRELEX struct
    ;;PREL;  if found:  return a  record  of type  T  representing the  type of  PREL
    ;;previously determined, otherwise return "T:object".
    ;;
    (cond ((assq (prelex-operand prel) env)
	   => cdr)
	  (else
	   T:object)))

  (module (%apply-funcall)
    ;;Here!!!   The whole  purpose of  determining type  tags is  to wrap  into KNOWN
    ;;structs the RATOR and  RAND* of FUNCALL structs.  Notice that  we do *not* wrap
    ;;the RATOR if it is a PRIMREF.
    ;;
    (define (%apply-funcall rator rand* rator.tag rand*.tag rator.env rand*.env)
      (let ((env         (%and-envs rator.env rand*.env))
	    (rand*.known ($map/stx %wrap-into-known rand* rand*.tag)))
	(struct-case rator
	  ((primref op)
	   ;;It is a core primitive application, either lexical primitive function or
	   ;;primitive operation: we process it specially.
	   (%apply-primcall op rand*.known env))
	  (else
	   (values (make-funcall (%wrap-into-known rator rator.tag) rand*.known)
		   env T:object)))))

    (define (%wrap-into-known x t)
      (if (core-type-tag=? t T:object)
	  ;;It is useless  to tag a value with "T:object",  because any Scheme object
	  ;;has tag "T:object".
	  x
	(make-known x t)))

    #| end of module: %APPLY-FUNCALL |# )

  #| end of module: V |# )


(module (%determine-constant-type)

  (define (%determine-constant-type x)
    (cond ((number?     x)   (%determine-numeric-constant-type x))
	  ((boolean?    x)   (if x T:true T:false))
	  ((null?       x)   T:null)
	  ((char?       x)   T:char)
	  ((string?     x)   T:string)
	  ((vector?     x)   T:vector)
	  ((pair?       x)   T:pair)
	  ((bytevector? x)   T:bytevector)
	  ((eq? x (void))    T:void)
	  (else              T:object)))

  (define (%determine-numeric-constant-type x)
    (cond ((fixnum? x)
	   (%sign x T:fixnum))
	  ((flonum? x)
	   (%sign x T:flonum))
	  ((or (bignum? x)
	       (ratnum? x))
	   (%sign x (core-type-tag-and T:exact T:other-number)))
	  (else
	   T:number)))

  (define (%sign x t)
    (core-type-tag-and t (cond ((< x 0) T:negative)
			       ((> x 0) T:positive)
			       ((= x 0) T:zero)
			       (else    t))))

  #| end of module: %DETERMINE-CONSTANT-TYPE |# )


(module (%apply-primcall)
  ;;This module  processes a core  primitive application, lexical  primitive function
  ;;and/or primitive operation:
  ;;
  ;;   (funcall (primref ?op) (known ?rand ?rand-type) ...)
  ;;
  ;;A core primitive  might care about the type  of its operands or not.   If it does
  ;;not care, either it is because: any operand will do; no optimisation is possible;
  ;;some optimisation is possible, but at present it is not implemented.
  ;;
  (define (%apply-primcall op rand* env)
    (define (return retval.tag)
      ;;We use  this when  the core  primitive does not  care about  the type  of its
      ;;operands, but we known the type of the return value.
      (values (make-funcall (mk-primref op) rand*) env retval.tag))

    (define-syntax-rule (%inject ?retval.tag ?rand.tag ...)
      ;;This is for core primitives accepting a fixed number of arguments.
      (inject op rand* env ?retval.tag ?rand.tag ...))

    (define-syntax-rule (%inject* ?retval.tag ?rand.tag)
      ;;This  is for  core  primitives accepting  any number  of  operands after  the
      ;;mandatory ones, but with all the operands of the same type RAND.TAG.
      (inject* op rand* env ?retval.tag ?rand.tag))

    (case op
      ((cons)
       (return T:pair))

      ((car cdr
	    caar cadr cdar cddr
	    caaar caadr cadar caddr cdaar cdadr cddar cdddr
	    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
       ;;All of these accept a pair as operand and return some object.
       (%inject T:object T:pair))

      ((set-car! set-cdr!)
       ;;All of these accept a pair and some object as operands and return void.
       (%inject T:void T:pair T:object))

      ((vector make-vector list->vector)
       (return T:vector))

      ((string make-string list->string)
       (return T:string))

      ((string-length)
       (%inject T:fixnum T:string))

      ((vector-length)
       (%inject T:fixnum T:vector))

      ((string-ref)
       (%inject T:char T:string T:fixnum))

      ((string-set!)
       (%inject T:void T:string T:fixnum T:char))

      ((vector-ref)
       (%inject T:object T:vector T:fixnum))

      ((vector-set!)
       (%inject T:void T:vector T:fixnum T:object))

      ((length)
       (%inject T:fixnum (core-type-tag-or T:null T:pair)))

      ((bytevector-length)
       (%inject T:fixnum T:bytevector))

      ((integer->char)
       (%inject T:char T:fixnum))

      ((char->integer)
       (%inject T:fixnum T:char))

      ((bytevector-u8-ref bytevector-s8-ref bytevector-u16-native-ref bytevector-s16-native-ref)
       (%inject T:fixnum T:bytevector T:fixnum))

      ((bytevector-u16-ref bytevector-s16-ref)
       (%inject T:fixnum T:bytevector T:fixnum T:symbol))

      ((bytevector-u8-set! bytevector-s8-set! bytevector-u16-native-set! bytevector-s16-native-set!)
       (%inject T:void T:bytevector T:fixnum T:fixnum))

      ((bytevector-u16-set! bytevector-s16-set!)
       (%inject T:void T:bytevector T:fixnum T:fixnum T:symbol))

      ((fx+         fx-         fx*         fxadd1      fxsub1
		    fxquotient  fxremainder fxmodulo    fxsll       fxsra
		    fxand       fxdiv       fxdiv0      fxif        fxior
		    fxlength    fxmax       fxmin       fxmod       fxmod0
		    fxnot       fxxor       fxlogand    fxlogor     fxlognot
		    fxlogxor)
       (%inject* T:fixnum T:fixnum))

      ((fx= fx< fx<= fx> fx>= fx=? fx<? fx<=? fx>? fx>=?
	    fxeven? fxodd? fxnegative? fxpositive? fxzero?
	    fxbit-set?)
       (%inject* T:boolean T:fixnum))

      ((fl=? fl<? fl<=? fl>? fl>=?
	     fleven? flodd? flzero? flpositive? flnegative?
	     flfinite? flinfinite? flinteger? flnan?)
       (%inject* T:boolean T:flonum))

      ((char=? char<? char<=? char>? char>=?
	       char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?)
       (%inject* T:boolean T:char))

      ((string=? string<? string<=? string>? string>=?
		 string-ci=? string-ci<? string-ci<=? string-ci>?
		 string-ci>=?)
       (%inject* T:boolean T:string))

      ((make-parameter
	   record-constructor record-accessor record-constructor record-predicate
	   condition-accessor condition-predicate
	   enum-set-constructor enum-set-indexer
	   make-guardian)
       (return T:procedure))

      ((fixnum-width greatest-fixnum least-fixnum)
       (return T:fixnum))

      (else
       (return T:object))))

;;; --------------------------------------------------------------------

  (module (inject)

    (define (inject op rand* env retval.tag . rand*.tag)
      ;;This is for core primitives accepting a fixed number of arguments.
      ;;
      (let ((env^ (if (fx=? (length rand*.tag)
			    (length rand*))
		      (%extend* rand* rand*.tag env)
		    ;;If we assume to have specified the number of operands correctly
		    ;;above: here we have a "wrong number of args" error.
		    env)))
	(values (make-funcall (mk-primref op) rand*)
		env^ retval.tag)))

    (define (%extend* rand* rand.tag* env)
      ;;Non-tail  recursive   function.   Extend   the  environment  ENV   with  type
      ;;informations  about the  PRELEX  structs in  RAND*, using  the  T records  in
      ;;RAND.TAG*.  Return the extended environment.
      ;;
      ;;RAND* can  be a list of  structs representing recordised code,  either PRELEX
      ;;structs, KNOWN  structs or  some other struct  type; if a  RAND is  neither a
      ;;PRELEX nor a KNOWN holding a PRELEX: it is silently skipped here.
      ;;
      (if (pair? rand*)
	  (%extend ($car rand*) ($car rand.tag*)
		   (%extend* ($cdr rand*) ($cdr rand.tag*) env))
	env))

    (define (%extend rand rand.tag env)
      (struct-case rand
	((known rand.expr rand.accum-tag)
	 (%extend rand.expr (core-type-tag-and rand.tag rand.accum-tag) env))
	((prelex)
	 (extend-env rand rand.tag env))
	(else
	 ;;The RAND is not a PRELEX struct: just return the original environment.
	 env)))

    #| end of module: inject |# )

;;; --------------------------------------------------------------------

  (module (inject*)

    (define (inject* op rand* env retval.tag rand.tag)
      ;;This  is for  core  primitives accepting  any number  of  operands after  the
      ;;mandatory ones, but with all the operands of the same type RAND.TAG.
      ;;
      (values (make-funcall (mk-primref op) rand*)
	      (%extend* rand* env rand.tag)
	      retval.tag))

    (define (%extend* rand* env rand.tag)
      ;;Non-tail  recursive   function.   Extend   the  environment  ENV   with  type
      ;;informations about  the PRELEX structs  in RAND*, using  for all of  them the
      ;;same T record RAND.TAG.  Return the extended environment.
      ;;
      ;;RAND* can  be a list of  structs representing recordised code,  either PRELEX
      ;;structs, KNOWN  structs or  some other struct  type; if a  RAND is  neither a
      ;;PRELEX nor a KNOWN holding a PRELEX: it is silently skipped here.
      ;;
      (if (null? rand*)
	  env
	(%extend ($car rand*) rand.tag
		 (%extend* ($cdr rand*) env rand.tag))))

    (define (%extend rand rand.tag env)
      (struct-case rand
	((known rand.expr rand.accum-tag)
	 (%extend rand.expr (core-type-tag-and rand.tag rand.accum-tag) env))
	((prelex)
	 (extend-env rand rand.tag env))
	(else
	 ;;The RAND is not a PRELEX struct: just return the original environment.
	 env)))

    #| end of module: inject* |# )

  #| end of module: %APPLY-PRIMCALL |# )


;;;; env functions
;;
;;Environments have the purpose  to map PRELEX structs to the type  tag of the values
;;they reference.  An environment is an association list with the format:
;;
;;   ((?number . ?prelex-type) ...)
;;
;;where:  ?NUMBER  is an  exact  integer  uniquely  associated  to a  PRELEX  struct;
;;?PRELEX-TYPE  is a  record of  type  T representing  the type  informations of  the
;;PRELEX.  The entries are kept sorted in the alist:
;;
;;   ((0 . ?prelex-type-0)
;;    (1 . ?prelex-type-1)
;;    (2 . ?prelex-type-2)
;;    ...)
;;
;;to make  it faster to  merge multiple  enviroments and search  for an entry  with a
;;given key.
;;

(define-inline-constant EMPTY-ENV
  '())

(define (extend-env* x* v* env)
  (if (pair? x*)
      (extend-env* ($cdr x*) ($cdr v*)
		   (extend-env ($car x*) ($car v*) env))
    env))

(define (extend-env x t env)
  ;;Add the given PRELEX and the record of type T to the environment.  Return the new
  ;;environment.
  ;;
  #;(assert (prelex? x))
  #;(assert (core-type-tag? x))
  (if (core-type-tag=? t T:object)
      ;;It is useless to  tag a value with "T:object", because  any Scheme object has
      ;;tag "T:object"; so we skip this PRELEX.
      env
    (let ((x.index (prelex-operand x)))
      (let recur ((env env))
	(if (or (null? env)
		;;If this is true: we can stop  recursing because we know in a sorted
		;;alist all the following entries have greater key.
		(< x.index (caar env)))
	    (cons (cons x.index t) env)
	  (cons ($car env) (recur ($cdr env))))))))

;;; --------------------------------------------------------------------

(module (%or-envs)

  (define-syntax-rule (%or-envs env1 env2)
    (%merge-envs env1 env2))

  (define (%merge-envs env1 env2)
    (cond ((eq? env1 env2)
	   env1)
	  ((pair? env1)
	   (if (pair? env2)
	       (%merge-envs2 ($car env1) ($cdr env1)
			     ($car env2) ($cdr env2))
	     EMPTY-ENV))
	  (else
	   EMPTY-ENV)))

  (define (%merge-envs2 a1 env1 a2 env2)
    (let ((x1 ($car a1))
	  (x2 ($car a2)))
      (cond ((eq? x1 x2)
	     (cons-env x1 (core-type-tag-or ($cdr a1) ($cdr a2))
		       (%merge-envs env1 env2)))
	    ((< x2 x1)
	     (%merge-envs1 a1 env1 env2))
	    (else
	     #;(assert (>= x2 x1))
	     (%merge-envs1 a2 env2 env1)))))

  (define (%merge-envs1 a1 env1 env2)
    (if (pair? env2)
	(%merge-envs2 a1 env1 ($car env2) ($cdr env2))
      EMPTY-ENV))

  (define (cons-env x v env)
    (if (core-type-tag=? v T:object)
	;;It is useless to tag a value with "T:object", because any Scheme object has
	;;tag "T:object"; so we skip this PRELEX.
	env
      (cons (cons x v) env)))

  #| end of module: %or-envs |# )

;;; --------------------------------------------------------------------

(module (%and-envs)

  (define-syntax-rule (%and-envs env1 env2)
    (%merge-envs env1 env2))

  (define (%merge-envs env1 env2)
    (cond ((eq? env1 env2)
	   env1)
	  ((pair? env1)
	   (if (pair? env2)
	       (%merge-envs2 ($car env1) ($cdr env1)
			     ($car env2) ($cdr env2))
	     env1))
	  (else
	   env2)))

  (define (%merge-envs2 a1 env1 a2 env2)
    (let ((x1 ($car a1))
	  (x2 ($car a2)))
      (cond ((eq? x1 x2)
	     (cons-env x1 (core-type-tag-and ($cdr a1) ($cdr a2))
		       (%merge-envs env1 env2)))
	    ((< x2 x1)
	     (cons a2 (%merge-envs1 a1 env1 env2)))
	    (else
	     (cons a1 (%merge-envs1 a2 env2 env1))))))

  (define (%merge-envs1 a1 env1 env2)
    (if (pair? env2)
	(%merge-envs2 a1 env1 ($car env2) ($cdr env2))
      env1))

  (define (cons-env x v env)
    (if (core-type-tag=? v T:object)
	;;It is useless to tag a value with "T:object", because any Scheme object has
	;;tag "T:object"; so we skip this PRELEX.
	env
      (cons (cons x v) env)))

  #| end of module: %and-envs |# )


;;;; miscellaneous stuff

;;Commented out by Abdulaziz Ghuloum.
;;
;; (define primitive-return-types
;;   '((=                     boolean)
;;     (<                     boolean)
;;     (<=                    boolean)
;;     (>                     boolean)
;;     (>=                    boolean)
;;     (even?                 boolean)
;;     (odd?                  boolean)
;;     (rational?             boolean)
;;     (rational-valued?      boolean)
;;     (real?                 boolean)
;;     (real-valued?          boolean)
;;     (bignum?               boolean)
;;     (ratnum?               boolean)
;;     (flonum?               boolean)
;;     (fixnum?               boolean)
;;     (integer?              boolean)
;;     (exact?                boolean)
;;     (finite?               boolean)
;;     (inexact?              boolean)
;;     (infinite?             boolean)
;;     (positive?             boolean)
;;     (negative?             boolean)
;;     (nan?                  boolean)
;;     (number?               boolean)
;;     (compnum?              boolean)
;;     (cflonum?              boolean)
;;     (complex?              boolean)
;;     (list?                 boolean)
;;     (eq?                   boolean)
;;     (eqv?                  boolean)
;;     (equal?                boolean)
;;     (gensym?               boolean)
;;     (symbol-bound?         boolean)
;;     (code?                 boolean)
;;     (immediate?            boolean)
;;     (pair?                 boolean)
;;     (procedure?            boolean)
;;     (symbol?               boolean)
;;     (symbol=?              boolean)
;;     (boolean?              boolean)
;;     (boolean=?             boolean)
;;     (vector?               boolean)
;;     (bitwise-bit-set?      boolean)
;;     (bytevector?           boolean)
;;     (bytevector=?          boolean)
;;     (enum-set=?            boolean)
;;     (binary-port?          boolean)
;;     (textual-port?         boolean)
;;     (input-port?           boolean)
;;     (output-port?          boolean)
;;     (port?                 boolean)
;;     (port-eof?             boolean)
;;     (port-closed?          boolean)
;;     (eof-object?           boolean)
;;     (hashtable?            boolean)
;;     (hashtable-mutable?    boolean)
;;     (file-exists?          boolean)
;;     (file-readable?        boolean)
;;     (file-writable?        boolean)
;;     (file-executable?      boolean)
;;     (file-symbolic-link?   boolean)
;;     (record?               boolean)
;;     (record-field-mutable? boolean)
;;     (record-type-generative? boolean)
;;     (record-type-sealed?   boolean)
;;     (record-type-descriptor boolean)
;;     (free-identifier=?     boolean)
;;     (bound-identifier=?    boolean)
;;     (identifier?           boolean)
;;     (char-lower-case?      boolean)
;;     (char-upper-case?      boolean)
;;     (char-title-case?      boolean)
;;     (char-whitespace?      boolean)
;;     (char-numeric?         boolean)
;;     (char-alphabetic?      boolean)
;;     ))


;;;; done

#| end of module: introduce-tags |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
