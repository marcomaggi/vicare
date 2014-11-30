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


(module (core-type-inference)
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
  ;;   seq		clambda		typed-expr
  ;;   forcall		funcall
  ;;
  ;;NOTE Every PRELEX struct in the  input expression must represent a proper lexical
  ;;binding defined by a BIND or FIX struct.
  ;;
  ;;NOTE This module stores a number in the field OPERAND of the PRELEX structs.
  ;;
  (import SCHEME-OBJECTS-ONTOLOGY)

  (define-syntax __module_who__
    (identifier-syntax 'core-type-inference))

  (define (core-type-inference x)
    (receive (y env y.tag)
	(%infer-core-types x EMPTY-ENV)
      y))


;;;; env functions
;;
;;Environments map PRELEX structs  to the type tag of the  values they reference.  An
;;environment is an association list with the format:
;;
;;   ((?number . ?prelex-type) ...)
;;
;;where: ?NUMBER is a fixnum uniquely  associated to a PRELEX struct; ?PRELEX-TYPE is
;;a record of type  T representing the type informations of  the PRELEX.  The entries
;;are kept sorted in the alist:
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
      (extend-env* (cdr x*) (cdr v*)
		   (extend-env (car x*) (car v*) env))
    env))

(define (extend-env prel tag env)
  ;;Add the given PRELEX  and the record of type TAG to  the environment.  Return the
  ;;new environment.
  ;;
  #;(assert (prelex? prel))
  #;(assert (core-type-tag? tag))
  (if (core-type-tag=? tag T:object)
      ;;It is useless to  tag a value with "T:object", because  any Scheme object has
      ;;tag "T:object"; so we do not extent ENV.
      env
    ;;In this loop we scan ENV searching for the entry associated to PREL.
    (let recur ((prel.index (prelex-operand prel))
		(env        env))
      (if (pair? env)
	  (cond ((fx<? prel.index (caar env))
		 ;;If we are here: we can stop  recursing because we know in a sorted
		 ;;alist all the following entries have greater key.
		 (cons (cons prel.index tag) env))
		((fx=? prel.index (caar env))
		 ;;There is already an entry associated to PREL.
		 (if (core-type-tag=? (cdar env) tag)
		     ;;The entry in ENV has type equal to TAG, so there is no need to
		     ;;push a new entry.
		     env
		   ;;The entry  in ENV  has type  different from TAG:  we push  a new
		   ;;entry that shadows the old one.
		   (begin
		     ;; (debug-print 'extend-env 'found-prel-in-env
		     ;; 		  'old-entry (cons (caar env) (core-type-tag-description (cdar env)))
		     ;; 		  'new-entry (cons prel.index (core-type-tag-description tag)))
		     (cons (cons prel.index tag) env))))
		(else
		 (cons (car env) (recur prel.index (cdr env)))))
	;;Add the new entry as last item in ENV.
	(list (cons prel.index tag))))))

;;; --------------------------------------------------------------------

(module (%or-envs)

  (define-syntax-rule (%or-envs env1 env2)
    (%merge-envs env1 env2))

  (define (%merge-envs env1 env2)
    (cond ((eq? env1 env2)
	   env1)
	  ((pair? env1)
	   (if (pair? env2)
	       (%merge-envs2 (car env1) (cdr env1)
			     (car env2) (cdr env2))
	     EMPTY-ENV))
	  (else
	   EMPTY-ENV)))

  (define (%merge-envs2 a1 env1 a2 env2)
    (let ((x1 (car a1))
	  (x2 (car a2)))
      (cond ((eq? x1 x2)
	     (cons-env x1 (core-type-tag-ior (cdr a1) (cdr a2))
		       (%merge-envs env1 env2)))
	    ((< x2 x1)
	     (%merge-envs1 a1 env1 env2))
	    (else
	     #;(assert (>= x2 x1))
	     (%merge-envs1 a2 env2 env1)))))

  (define (%merge-envs1 a1 env1 env2)
    (if (pair? env2)
	(%merge-envs2 a1 env1 (car env2) (cdr env2))
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
	       (%merge-envs2 (car env1) (cdr env1)
			     (car env2) (cdr env2))
	     env1))
	  (else
	   env2)))

  (define (%merge-envs2 a1 env1 a2 env2)
    (let ((x1 (car a1))
	  (x2 (car a2)))
      (cond ((eq? x1 x2)
	     (cons-env x1 (core-type-tag-and (cdr a1) (cdr a2))
		       (%merge-envs env1 env2)))
	    ((< x2 x1)
	     (cons a2 (%merge-envs1 a1 env1 env2)))
	    (else
	     (cons a1 (%merge-envs1 a2 env2 env1))))))

  (define (%merge-envs1 a1 env1 env2)
    (if (pair? env2)
	(%merge-envs2 a1 env1 (car env2) (cdr env2))
      env1))

  (define (cons-env x v env)
    (if (core-type-tag=? v T:object)
	;;It is useless to tag a value with "T:object", because any Scheme object has
	;;tag "T:object"; so we skip this PRELEX.
	env
      (cons (cons x v) env)))

  #| end of module: %and-envs |# )


(define (%infer-core-types x env)
  ;;Here we  use nested  functions because,  for every compilation  unit, we  want to
  ;;reset to zero the counter used by %ASSIGN-INDEX-TO-PRELEX!.
  ;;

  (define* (V x env)
    ;;Recursively  process the  recordised code  in X.   ENV maps  PRELEX structs  to
    ;;records of type CORE-TYPE-TAG.
    ;;
    ;;Return 3 values:
    ;;
    ;;1.  A struct  representing  recordised  code, which  is  meant  to replace  the
    ;;argument X.
    ;;
    ;;2. An  ENV value representing the  type informations gathered by  inspecting X;
    ;;such informations can be used to process expressions that are evaluated *after*
    ;;X.
    ;;
    ;;3. A record  of type CORE-TYPE-TAG representing the type  of the value returned
    ;;by X.
    ;;
    (struct-case x
      ((constant x.const)
       (values x env (determine-constant-core-type x.const)))

      ((typed-expr expr core-type)
       (receive (expr^ env^ inferred-core-type)
	   (V expr env)
	 (case (core-type-tag-is-a? inferred-core-type core-type)
	   ((yes maybe)
	    ;;FIXME Is it fine to always  return CORE-TYPE?  If we can determine that
	    ;;INFERRED-CORE-TYPE is more refined than  CORE-TYPE, then it makes sense
	    ;;to return INFERRED-CORE-TYPE.
	    ;;
	    ;;We  know  that "T:fixnum"  is  more  refined  than "T:number",  so  the
	    ;;following expression is true:
	    ;;
	    ;;   (and
	    ;;     (eq? 'yes   (core-type-tag-is-a? T:fixnum T:number))
	    ;;     (eq? 'maybe (core-type-tag-is-a? T:number T:fixnum)))
	    ;;
	    ;;So if:
	    ;;
	    ;;   (and
	    ;;     (eq? 'yes   (core-type-tag-is-a? inferred-core-type core-type))
	    ;;     (eq? 'maybe (core-type-tag-is-a? core-type inferred-core-type)))
	    ;;
	    ;;is  true,  then  INFERRED-CORE-TYPE  is more  refined  than  CORE-TYPE.
	    ;;(Marco Maggi; Wed Nov 19, 2014)
	    ;;
	    (values expr^ env^ core-type))
	   (else
	    (compiler-internal-error __module_who__ __who__
	      "typed expression's core type incompatible with inferred core type"
	      (unparse-recordized-code x)
	      inferred-core-type)))))

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
       ;;
       ;;NOTE The  return value of X.E0  is discarded.  E0.TAG.UNUSED is  a record of
       ;;type CORE-TYPE-TAG  representing the type  of the X.E0's return  value; such
       ;;record is unused.
       ;;
       ;;NOTE The initial  environment ENV is first augmented  with type informations
       ;;from X.E0;  then augmented with type  informations from X.E1; finally  it is
       ;;returned to the caller.
       (receive (e0 e0.env e0.tag.unused)
	   (V x.e0 env)
	 (receive (e1 e1.env e1.tag)
	     (V x.e1 e0.env)
	   (values (make-seq e0 e1) e1.env e1.tag))))

      ((conditional x.test x.conseq x.altern)
       (V-conditional x.test x.conseq x.altern env))

      ((bind x.lhs* x.rhs* x.body)
       (receive (rhs* env^ rhs*.tag)
	   (V* x.rhs* env)
	 ;;Assign a unique number to each  PRELEX.  Remember that the RHS expressions
	 ;;of a BIND do *not* reference the LHS PRELEX structs.
	 ($for-each/stx %assign-index-to-prelex! x.lhs*)
	 (receive (body body.env body.tag)
	     (V x.body (extend-env* x.lhs* rhs*.tag env^))
	   (values (make-bind x.lhs* rhs* body) body.env body.tag))))

      ((fix x.lhs* x.rhs* x.body)
       ;;Assign a unique number to each PRELEX.  Remember that the RHS expressions of
       ;;a FIX might reference the LHS PRELEX structs.
       ($for-each/stx %assign-index-to-prelex! x.lhs*)
       (receive (rhs* env^ rhs*.tag)
	   (V* x.rhs* env)
	 (receive (body body.env body.tag)
	     (V x.body (extend-env* x.lhs* rhs*.tag env^))
	   (values (make-fix x.lhs* rhs* body) body.env body.tag))))

      ((clambda)
       (V-clambda x env))

      ((funcall rator rand*)
       (V-funcall rator rand* env))

      ((forcall rator rand*)
       (receive (rand* rand*.env rand*.tag)
	   (V* rand* env)
	 ;;FIXME Maybe, by inspecting the RATOR, we could determine a better type tag
	 ;;than "T:object" for  FORCALL.  Remember that the type tag  of this FORCALL
	 ;;struct  is the  one of  the object  returned by  the RATOR,  which is  a C
	 ;;language foreign function.  (Marco Maggi; Fri Sep 12, 2014)
	 (values (make-forcall rator rand*) rand*.env T:object)))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid expression" (unparse-recordized-code x)))))

  (define (V* x* env)
    ;;Apply V to all the structs in the list X*; gather the environment.
    ;;
    (if (pair? x*)
	(let-values (((x  env1 tag)  (V  (car x*) env))
		     ((x* env2 tag*) (V* (cdr x*) env)))
	  (values (cons x x*)
		  (%and-envs env1 env2)
		  (cons tag tag*)))
      (values '() env '())))

;;; --------------------------------------------------------------------

  (define (V-conditional x.test x.conseq x.altern x.env)
    (receive (test test.env test.tag)
	(V x.test x.env)
      (receive (x.conseq.env x.altern.env)
	  (%augment-env-with-conditional-test-info test test.env)
	(case (T:false? test.tag)
	  ((yes)
	   ;;We know the test is false, so do the transformation:
	   ;;
	   ;;   (conditional ?test ?conseq ?altern)
	   ;;   ==> (seq ?test ?conseq)
	   ;;
	   ;;we keep ?TEST for its side effects.
	   (receive (altern altern.env altern.tag)
	       (V x.altern x.altern.env)
	     (values (make-seq test altern) altern.env altern.tag)))
	  ((no)
	   ;;We know the test is true, so do the transformation:
	   ;;
	   ;;   (conditional ?test ?conseq ?altern)
	   ;;   ==> (seq ?test ?altern)
	   ;;
	   ;;we kepp ?TEST for its side effects.
	   (receive (conseq conseq.env conseq.tag)
	       (V x.conseq x.conseq.env)
	     (values (make-seq test conseq) conseq.env conseq.tag)))
	  (else
	   ;;We do not know the result of the test.
	   (let-values
	       (((conseq conseq.env conseq.tag) (V x.conseq x.conseq.env))
		((altern altern.env altern.tag) (V x.altern x.altern.env)))
	     (values (make-conditional test conseq altern)
		     (%or-envs conseq.env altern.env)
		     (core-type-tag-ior conseq.tag altern.tag))))))))

  (module (V-clambda)
    ;;The purposes of this  module are: to apply V to all  the CLAMBDA clause bodies;
    ;;to  tag  the  CLAMBDA  itself  with the  type  descriptor  "T:procedure".   The
    ;;environment  is not  modified: no  binding defined  by the  CLAMBDA is  visible
    ;;outside.
    ;;
    (define (V-clambda x x.env)
      (struct-case x
	((clambda label clause* cp free name)
	 (let ((clause*^ ($map/stx (lambda (clause)
				     (V-clambda-clause clause x.env))
			   clause*)))
	   (values (make-clambda label clause*^ cp free name)
		   x.env T:procedure)))))

    (define (V-clambda-clause clause x.env)
      (struct-case clause
	((clambda-case clause.info clause.body)
	 ;;Assign a unique number to each PRELEX.
	 ($for-each/stx %assign-index-to-prelex! (case-info-args clause.info))
	 (receive (body body.env.unused body.tag.unused)
	     (V clause.body x.env)
	   (make-clambda-case clause.info body)))))

    #| end of module: V-clambda |# )

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

  (module (V-funcall)
    ;;Here!!!   The whole  purpose of  determining type  tags is  to wrap  into KNOWN
    ;;structs the RATOR and  RAND* of FUNCALL structs.  Notice that  we do *not* wrap
    ;;the RATOR if it is a PRIMREF.
    ;;
    (define (V-funcall rator rand* env)
      (let-values
	  (((rator rator.env rator.tag) (V  rator env))
	   ((rand* rand*.env rand*.tag) (V* rand* env)))
	(let ((env         (%and-envs rator.env rand*.env))
	      (rand*.known ($map/stx %wrap-into-known rand* rand*.tag)))
	  (struct-case rator
	    ((primref op)
	     ;;It is a core primitive  application, either lexical primitive function
	     ;;or primitive operation: we process it specially.
	     (%process-primitive-application op rand*.known env))
	    (else
	     (values (make-funcall (%wrap-into-known rator rator.tag) rand*.known)
		     env T:object))))))

    (define (%wrap-into-known x t)
      (if (core-type-tag=? t T:object)
	  ;;It is useless  to tag a value with "T:object",  because any Scheme object
	  ;;has tag "T:object".
	  x
	(make-known x t)))

    #| end of module: V-funcall |# )

;;; --------------------------------------------------------------------

  (V x env))


(module (%process-primitive-application)
  ;;This module  processes a core  primitive application, either a  lexical primitive
  ;;function or primitive operation (or both):
  ;;
  ;;   (funcall (primref ?op) (known ?rand ?rand-type) ...)
  ;;
  ;;A core primitive  might care about the type  of its operands or not.   If it does
  ;;not care, either it is because: any operand will do; no optimisation is possible;
  ;;some optimisation is possible, but at present it is not implemented.
  ;;
  ;;This module has two purposes:
  ;;
  ;;* To specify the type tag of the  FUNCALL return value; this can be done for both
  ;;  safe and unsafe core primitives.
  ;;
  ;;* To  attach additional type informations  to PRELEX structs used  as operands to
  ;;  the core primitive;  this can be done only for safe  primitives.  We extend the
  ;;   environment with  informations gathered  from the  fact that:  after the  safe
  ;;  primitive has been successfully executed,  its arguments have been validated as
  ;;  correct, so we know their type.
  ;;
  ;;As example of second purpose, let's consider the following standard code:
  ;;
  ;;   (let ((f (lambda (y) y))
  ;;         (x (read)))
  ;;     (f (cdr x))
  ;;     (f x))
  ;;
  ;;the body  of the LET  form contains a  sequence of forms:  "(f (cdr x))"  will be
  ;;executed first, "(f x)"  will be executed last.  We know that  CDR accepts a pair
  ;;as argument:  after "(cdr  x)" has  been executed without  raising a  "wrong type
  ;;argument" exception,  we know that the  variable X is  a pair.  So when,  in this
  ;;very  module, we  process  "(cdr x)"  we extend  the  environment by  associating
  ;;"T:pair" to X; when later we process "(f x)", X is tagged.
  ;;
  (define (%process-primitive-application op rand* env)
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
       (%inject T:fixnum (core-type-tag-ior T:null T:pair)))

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

      ((exact)
       (%inject T:exact T:number))

      ((inexact)
       (%inject T:inexact T:number))

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

      ((not)
       (return T:boolean))

      (else
       (return T:object))))

;;; --------------------------------------------------------------------

  (module (inject)
    ;;Extend the environment with informations gathered from the fact that: after the
    ;;safe  primitive  has  been  successfully  executed,  its  arguments  have  been
    ;;determined as correct, so we know their type.
    ;;
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
      ;;informations  about the  PRELEX  structs in  RAND*,  using the  CORE-TYPE-TAG
      ;;records in RAND.TAG*.  Return the extended environment.
      ;;
      ;;RAND* can  be a list of  structs representing recordised code,  either PRELEX
      ;;structs, KNOWN  structs or  some other struct  type; if a  RAND is  neither a
      ;;PRELEX nor a KNOWN  holding a PRELEX: it is silently  skipped because no type
      ;;informations can be associated to it.
      ;;
      (if (pair? rand*)
	  (%extend (car rand*) (car rand.tag*)
		   (%extend* (cdr rand*) (cdr rand.tag*) env))
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
    ;;Extend the environment with informations gathered from the fact that: after the
    ;;safe  primitive  has  been  successfully  executed,  its  arguments  have  been
    ;;determined as correct, so we know their type.
    ;;
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
      ;;same CORE-TYPE-TAG record RAND.TAG.  Return the extended environment.
      ;;
      ;;RAND* can  be a list of  structs representing recordised code,  either PRELEX
      ;;structs, KNOWN  structs or  some other struct  type; if a  RAND is  neither a
      ;;PRELEX nor a KNOWN  holding a PRELEX: it is silently  skipped because no type
      ;;informations can be associated to it.
      ;;
      (if (pair? rand*)
	  (%extend (car rand*) rand.tag
		   (%extend* (cdr rand*) env rand.tag))
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

    #| end of module: inject* |# )

  #| end of module: %PROCESS-PRIMITIVE-APPLICATION |# )


(module (%augment-env-with-conditional-test-info)
  ;;Here process the ?TEST expression from a conditional:
  ;;
  ;;   (conditional ?test ?conseq ?altern)
  ;;
  ;;with the  purpose of adding informations  to the environment used  to process the
  ;;?CONSEQ and ?ALTERN.   This module must be used *after*  ?TEST has been processed
  ;;by  the function  V;  in this  module  we do  not  change ?TEST,  we  try to  add
  ;;informations to the argument ENV.
  ;;
  ;;Return  2 values:  an augmented  environment to  be used  to process  ?CONSEQ; an
  ;;augmented environment to be used to process ?ALTERN.
  ;;
  ;;There is a special, but common, case of ?TEST expression:
  ;;
  ;;   (funcall (primref ?type-pred) ?var)
  ;;
  ;;in which the  operator of the function application is  a type predicate (FIXNUM?,
  ;;STRING?, ...) and the  operand is a PRELEX struct; in this case  we know that, if
  ;;the test is successful, the type of ?VAR in the consequent branch is determined.
  ;;
  ;;Another special case is:
  ;;
  ;;   (if var ?conseq ?altern)
  ;;
  ;;here we know that the VAR is non-false in the ?CONSEQ and false in the ?ALTERN.
  ;;
  ;;
  ;;Note about non-type predicates
  ;;------------------------------
  ;;
  ;;Let's consider the recordised code:
  ;;
  ;;   (conditional ?test ?conseq ?altern)
  ;;
  ;;where ?TEST is:
  ;;
  ;;   ((primitive flinfinite?) x)
  ;;
  ;;FLINFINITE? is a predicate, but not a type predicate.  It is known, and described
  ;;by this compiler  pass, that FLINFINITE?  raises an exception  if its argument is
  ;;not a flonum; so the continuation of ?TEST  is executed only if the argument is a
  ;;flonum.
  ;;
  ;;The function V processes ?TEST and determines that when either ?CONSEQ or ?ALTERN
  ;;are processed X is  known to be a "T:finxum"; this  happens regardless the result
  ;;of applying FLINFINITE?  to X, and it is done by %PROCESS-PRIMITIVE-APPLICATION.
  ;;
  (define (%augment-env-with-conditional-test-info test env)
    ;;TEST must  be recordised code  representing the  test of a  CONDITIONAL struct.
    ;;ENV must be the environment in which the conseq and altern will be processed.
    ;;
    ;;Inspect TEST and  augment ENV with inferred type properties.   Return 2 values:
    ;;the  augmented  environment in  which  the  consequent  can be  processed;  the
    ;;augmented environment in which the alternate can be processed.
    ;;
    (struct-case test
      ((funcall rator rand*)
       (struct-case rator
	 ((primref prim-name)
	  (if (and (pair? rand*)
		   (null? (cdr rand*)))
	      ;;There is only one operand.
	      (let ((rand (car rand*)))
		(struct-case rand
		  ((prelex)
		   (%process-predicate-application-to-var prim-name rand env))
		  (else
		   (values env env))))
	    (values env env)))
	 (else
	  (values env env))))
      ((prelex)
       (values (extend-env test T:non-false env)
	       (extend-env test T:false     env)))
      (else
       (values env env))))

  (define (%process-predicate-application-to-var prim-name rand env)
    (case prim-name
      ((not)
       (values (extend-env rand T:false     env)
	       (extend-env rand T:non-false env)))
      ((null?)
       (values (extend-env rand T:null env)
	       env))
      ((pair?)
       (values (extend-env rand T:pair env)
	       env))
      ((list?)
       (values (extend-env rand T:proper-list env)
	       env))

      ;; numbers
      ((fixnum?)
       (values (extend-env rand T:fixnum env)
	       env))
      ((bignum?)
       (values (extend-env rand T:bignum env)
	       env))
      ((ratnum?)
       (values (extend-env rand T:ratnum env)
	       env))
      ((flonum?)
       (values (extend-env rand T:flonum env)
	       env))
      ((cflonum?)
       (values (extend-env rand T:cflonum env)
	       env))
      ((compnum?)
       (values (extend-env rand T:compnum env)
	       env))
      ((exact-integer?)
       (values (extend-env rand T:exact-integer env)
	       env))
      ((real?)
       (values (extend-env rand T:real env)
	       env))
      ((complex?)
       (values (extend-env rand T:complex env)
	       env))
      ((number? integer? integer-valued? real-valued? rational? rational-valued?)
       (values (extend-env rand T:number env)
	       env))
      ((exact?)
       (values (extend-env rand T:exact env)
	       env))
      ((inexact?)
       (values (extend-env rand T:inexact env)
	       env))

      ((infinite?)
       (values (extend-env rand T:number env)
	       env))
      ((finite?)
       (values (extend-env rand T:number env)
	       env))
      ((nan?)
       (values (extend-env rand T:number env)
	       env))

      ((flinteger?)
       (values (extend-env rand T:flonum-integer env)
	       env))
      ((flfinite?)
       (values (extend-env rand T:flonum-finite env)
	       env))
      ((flinfinite?)
       (values (extend-env rand T:flonum-infinite env)
	       env))
      ((flnan?)
       (values (extend-env rand T:flonum-nan env)
	       env))

      (($flonum-integer?)
       (values (extend-env rand T:flonum-integer env)
	       env))
      (($flonum-rational?)
       (values (extend-env rand T:flonum-finite env)
	       env))
      (($flinfinite?)
       (values (extend-env rand T:flonum-infinite env)
	       env))
      (($flnan?)
       (values (extend-env rand T:flonum-nan env)
	       env))


      ((positive?)
       (values (extend-env rand T:positive env)
	       env))
      ((negative?)
       (values (extend-env rand T:negative env)
	       env))
      ((zero?)
       (values (extend-env rand T:zero env)
	       env))

      ;; other objects
      ((string?)
       (values (extend-env rand T:string env)
	       env))
      ((vector?)
       (values (extend-env rand T:vector env)
	       env))
      ((char?)
       (values (extend-env rand T:char env)
	       env))
      ((transcoder?)
       (values (extend-env rand T:transcoder env)
	       env))
      ((symbol?)
       (values (extend-env rand T:symbol env)
	       env))
      ((bytevector?)
       (values (extend-env rand T:bytevector env)
	       env))
      ((boolean?)
       (values (extend-env rand T:boolean env)
	       env))
      ((procedure?)
       (values (extend-env rand T:procedure env)
	       env))

      ((port?)
       (values (extend-env rand T:port env)
	       env))

      ((pointer?)
       (values (extend-env rand T:pointer env)
	       env))

      ((hashtable?)
       (values (extend-env rand T:hashtable env)
	       env))

      ;; structs and records
      ((struct?)
       (values (extend-env rand T:struct env)
	       env))

      ((struct-type-descriptor?)
       (values (extend-env rand T:struct-type-descriptor env)
	       env))

      ((annotation?)
       (values (extend-env rand T:other-struct env)
	       env))

      ((memory-block?)
       (values (extend-env rand T:other-struct env)
	       env))

      ((record?)
       (values (extend-env rand T:record env)
	       env))

      (else
       (values env env))))

  #| end of module: %AUGMENT-ENV-WITH-CONDITIONAL-TEST-INFO |# )


;;;; done

#| end of module: CORE-TYPE-INFERENCE |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
