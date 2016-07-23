;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.pass-introduce-unsafe-primrefs)
  (export pass-introduce-unsafe-primrefs)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code)
    (only (ikarus.compiler.pass-source-optimizer)
	  optimize-level))


;;;; introduction
;;
;;This optional  compiler pass  recognises the application  of *safe*  core primitive
;;functions  having  operands  of  the  correct  type  and  replaces  them  with  the
;;corresponding application of *unsafe* core  primitive functions or operations.  The
;;result is faster code.
;;
;;It makes sense  to perform this compiler pass only  if CORE-TYPE-INFERENCE has been
;;performed first.
;;
;;Accept as input a nested hierarchy of the following structs:
;;
;;   constant		prelex		primref
;;   bind		fix		conditional
;;   seq		clambda		known
;;   forcall		funcall		typed-expr
;;
(define-syntax __module_who__
  (identifier-syntax 'pass-introduce-unsafe-primrefs))

(define (pass-introduce-unsafe-primrefs x)
  (case optimize-level
    ((0)	x)
    (else	(E x))))


(define* (E x)
  (struct-case x
    ((constant)
     x)

    ;;If we  are performing this compiler  pass without first having  performed "core
    ;;type inference" (which we should not do): there may still be TYPED-EXPR structs
    ;;in the input.  We remove them.
    ((typed-expr expr core-type)
     (E expr))

    ((prelex)
     x)

    ((primref op)
     x)

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((conditional test conseq altern)
     (make-conditional (E test) (E conseq) (E altern)))

    ((bind lhs* rhs* body)
     (make-bind lhs* ($map/stx E rhs*) (E body)))

    ((fix  lhs* rhs* body)
     (make-fix  lhs* ($map/stx E rhs*) (E body)))

    ((clambda)
     (E-clambda x))

    ((funcall rator rand*)
     (E-funcall rator rand*))

    ((forcall rator rand*)
     (make-forcall rator ($map/stx E rand*)))

    (else
     (compiler-internal-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))

(define (E-known x)
  (struct-case x
    ((known x.expr x.type)
     (make-known (E x.expr) x.type))
    (else x)))

(module (E-clambda)
  ;;The purpose of this module is to apply E to all the CLAMBDA clause bodies.
  ;;
  (define (E-clambda x)
    (struct-case x
      ((clambda label clause* cp free name)
       (make-clambda label ($map/stx E-clambda-clause clause*) cp free name))))

  (define (E-clambda-clause clause)
    (struct-case clause
      ((clambda-case info body)
       (make-clambda-case info (E body)))))

  #| end of module: E-clambda |# )


(module (E-funcall)
  (module (core-primitive-name->core-type-signature*
	   core-primitive-name->replacement*
	   core-type-tag?
	   core-type-tag-is-a?
	   core-type-tag-matches-any-object?
	   tuple-tags-arity
	   tuple-tags-rest-objects-tag
	   tuple-tags-ref)
    (import (ikarus.compiler.core-primitive-properties)))

  (define (E-funcall rator rand*)
    (let ((rand*^ ($map/stx E-known rand*)))
      (struct-case rator
	((primref op)
	 (%E-primref-call op rand*^))
	(else
	 (make-funcall (E-known rator) rand*^)))))

  (define* (%E-primref-call prim-name rand*)
    (define (%no-replacement)
      ;;When no replacement is possible: we return the return value of this function.
      ;;Just return a copy of the original primitive application.
      ;;
      (make-funcall (make-primref prim-name) rand*))
    (if (null? rand*)
	(%no-replacement)
      (let ((rand*.vec (list->vector rand*)))
	(case (%compatible-operands-for-primitive-call? prim-name rand*.vec)
	  ((yes)
	   (or (%find-core-primitive-replacement prim-name rand* rand*.vec)
	       (%no-replacement)))
	  ((no)
	   (cond ((options::strict-r6rs)
		  ;;The  operands do  not  match the  expected  arguments: resort  to
		  ;;run-time error as mandated by R6RS.
		  (print-compiler-warning-message "operands of invalid core type in call to core primitive: ~a"
						  (unparse-recordized-code/sexp (%no-replacement)))
		  (%no-replacement))
		 (else
		  (compile-time-operand-core-type-error __module_who__ __who__
		    "operands of invalid core type in call to core primitive"
		    (unparse-recordized-code/sexp (%no-replacement))))))
	  ((wrong-num-args)
	   (cond ((options::strict-r6rs)
		  ;;The operands  are in  wrong number: resort  to run-time  error as
		  ;;mandated by R6RS.
		  (print-compiler-warning-message "wrong number of operands in call to core primitive: ~a"
						  (unparse-recordized-code/sexp (%no-replacement)))
		  (%no-replacement))
		 (else
		  (compile-time-arity-error __module_who__ __who__
		    "wrong number of arguments in core primitive application"
		    (unparse-recordized-code/sexp (%no-replacement))))))
	  (else
	   (compiler-internal-error __module_who__ __who__ "invalid return value"))))))

;;; --------------------------------------------------------------------

  (define (%compatible-operands-for-primitive-call? prim-name rand*.vec)
    ;;Validate the operands  against the types expected by the  core primitive.  Each
    ;;applicable core primitive might support multiple operands signatures:
    ;;
    ;;*  If at  least one  signature matches  the given  operands: return  the symbol
    ;;"yes".
    ;;
    ;;* If no signature matches the given  operands because the number of operands is
    ;;wrong: return the symbol "wrong-num-args".
    ;;
    ;;* If no signature matches the given  operands, but at least one has the correct
    ;;number of operand (but wrong operand types): return the symbol "no".
    ;;
    (cond ((core-primitive-name->core-type-signature* prim-name)
	   => (lambda (signature*)
		;;We expect SIGNATURE* to be a list of pairs pair with the format:
		;;
		;;   ((?operands-tuple-tags . ?return-values-tuple-tags) ...)
		;;
		;;in  which both  ?OPERANDS-TUPLE-TAGS and  ?RETURN-VALUES-TUPLE-TAGS
		;;are TUPLE-TAGS values.
		(let loop ((signature*      signature*)
			   ;;This is set  to false if at least one  signature has the
			   ;;correct number of arguments.
			   (wrong-num-args? #t))
		  (cond ((pair? signature*)
			 ;;Test the next signature.
			 (case (%compatible-tuple-tags-and-operands? (caar signature*) rand*.vec)
			   ((yes)
			    ;;This signature does match.  Success!
			    'yes)
			   ((no)
			    ;;This signature  does not match because  the operands do
			    ;;not match the required type tags: try the next.
			    (loop (cdr signature*) #f))
			   ((wrong-num-args)
			    ;;This signature  does not match because  of wrong number
			    ;;of operands: try the next.
			    (loop (cdr signature*) (and wrong-num-args? #t)))))
			(wrong-num-args?
			 ;;No more signatures to test:  no match.  All the signatures
			 ;;failed to match because of wrong number of operands.
			 'wrong-num-args)
			(else
			 ;;No more signatures to test:  no match.  All the signatures
			 ;;failed; at least  one with correct number  of operands but
			 ;;wrong operands types.
			 'no)))))
	  ;;This core primitive  has no registered core type  signatures.  Let's fake
	  ;;successfully matching arguments.
	  ;;
	  ;;FIXME  In future  we  should  replace this  with  an  exception: all  the
	  ;;primitive should  have a type  specification.  (Marco Maggi; Sun  Sep 21,
	  ;;2014)
	  (else
	   #;(print-compiler-warning-message "core primitive without registered core type signature: ~a" prim-name)
	   'yes)))

;;; --------------------------------------------------------------------

  (module (%find-core-primitive-replacement)

    (define (%find-core-primitive-replacement original-prim-name rand* rand*.vec)
      ;;This function should be called if we know the operands in RAND*.VEC match the
      ;;expected  argument  types  of  the  ORIGINAL-PRIM-NAME.   Scan  the  list  of
      ;;registered  unsafe primitives  that  can replace  ORIGINAL-PRIM-NAME for  one
      ;;whose expected argument  types strictly match the  RAND*.VEC.  If successful:
      ;;return  a FUNCALL  struct that  must replace  the original;  otherwise return
      ;;false.
      ;;
      (cond ((core-primitive-name->replacement* original-prim-name)
	     ;;REPLACEMENT-PRIM-NAME*  is a  vector  of  symbols representing  public
	     ;;names of unsafe primitives.
	     => (lambda (replacement-prim-name.vec)
		  (vector-exists
		      (lambda (replacement-prim-name)
			(and (%strictly-matching-operands-for-primitive-call? replacement-prim-name rand*.vec)
			     (make-funcall (make-primref replacement-prim-name) rand*)))
		    replacement-prim-name.vec)))
	    ;;This primitive has no registered unsafe replacements.
	    (else #f)))

    (define (%strictly-matching-operands-for-primitive-call? prim-name rand*.vec)
      ;;Validate the operands  against the types expected by the  core primitive.  If
      ;;they  match: return  true, otherwise  return false.   If a  "wrong number  of
      ;;operands" is detected: return false.
      ;;
      (cond ((core-primitive-name->core-type-signature* prim-name)
	     => (lambda (signature*)
		  ;;We expect SIGNATURE* to be a list of pairs pair with the format:
		  ;;
		  ;;   ((?operands-tuple-tags . ?return-values-tuple-tags) ...)
		  ;;
		  ;;in which both  ?OPERANDS-TUPLE-TAGS and ?RETURN-VALUES-TUPLE-TAGS
		  ;;are TUPLE-TAGS values.
		  (find (lambda (signature)
			  (case (%strictly-matching-tuple-tags-and-operands? (car signature) rand*.vec)
			    ((yes)	#t)
			    (else	#f)))
		    signature*)))
	    ;;This core primitive has no  registered type signatures.  We must assume
	    ;;the operands do *not* match.
	    (else #f)))

    #| end of module: %FIND-CORE-PRIMITIVE-REPLACEMENT |# )

;;; --------------------------------------------------------------------

  (module (%compatible-tuple-tags-and-operands?
	   %strictly-matching-tuple-tags-and-operands?)

    (define* (%compatible-tuple-tags-and-operands? tags rand*.vec)
      ;;The  purpose of  this function  is to  detect if  the operands  are *invalid*
      ;;according to the KNOWN structs.  TAGS  must be a TUPLE-TAGS value.  RAND*.VEC
      ;;must be a vector of structs representing recordised code.
      ;;
      ;;Validate the operands in RAND*.VEC against the core type tags in TAGS:
      ;;
      ;;* If the operands are compatible: return the symbol "yes".
      ;;
      ;;* If the operands have wrong type: return the symbol "no".
      ;;
      ;;* If the operands are in wrong number: return the symbol "wrong-num-args".
      ;;
      (%match-tuple-tags-and-operands tags rand*.vec %compatible-type-tag-and-operand?))

    (define* (%strictly-matching-tuple-tags-and-operands? tags rand*.vec)
      ;;The  purpose  of this  function  is  to check  if  the  operands are  *valid*
      ;;according to the KNOWN structs.  TAGS  must be a TUPLE-TAGS value.  RAND*.VEC
      ;;must be a vector of structs representing recordised code.
      ;;
      ;;Validate the operands in RAND*.VEC against the type tags in TAGS:
      ;;
      ;;* If the operands strictly match: return the symbol "yes".
      ;;
      ;;* If the operands have wrong type: return the symbol "no".
      ;;
      ;;* If the operands have compatible types, but not exclusively matching: return
      ;;the symbol "no".
      ;;
      ;;* If the operands are in wrong number: return the symbol "wrong-num-args".
      ;;
      (%match-tuple-tags-and-operands tags rand*.vec %strictly-matching-type-tag-and-operand?))

    (define* (%match-tuple-tags-and-operands tags rand*.vec matching?)
      ;;Validate the  operands in RAND*.VEC against  the type tags in  TAGS using the
      ;;matching procedure  MATCHING?.  TAGS must  be a TUPLE-TAGS  value.  RAND*.VEC
      ;;must be a vector of structs representing recordised code.
      ;;
      ;;If the  operands match according to  MATCHING?: return the symbol  "yes".  If
      ;;the operands do  not match: return the  symbol "no".  If the  operands are in
      ;;wrong number: return the symbol "wrong-num-args".
      ;;
      (import (only (vicare system $vectors) $vector-ref))
      (let ((number-of-mandatory-operands (tuple-tags-arity tags))
	    (number-of-given-operands     (vector-length rand*.vec)))
	(define (%check-mandatory-operands i)
	  (cond ((fx=? i number-of-mandatory-operands)
		 'yes)
		((matching? (tuple-tags-ref tags i) ($vector-ref rand*.vec i))
		 (%check-mandatory-operands (fxadd1 i)))
		(else 'no)))
	(define (%check-rest-operands i rest-tag)
	  (cond ((fx=? i number-of-given-operands)
		 'yes)
		((matching? rest-tag ($vector-ref rand*.vec i))
		 (%check-rest-operands (fxadd1 i) rest-tag))
		(else 'no)))
	(cond ((tuple-tags-rest-objects-tag tags)
	       ;;If  we  are here:  the  TUPLE-TAGS  represents  a tuple  of  objects
	       ;;matching any  number of operands; the  tag of the "rest"  operand is
	       ;;REST-TAG.
	       => (lambda (rest-tag)
		    (cond ((fx=? number-of-mandatory-operands number-of-given-operands)
			   (%check-mandatory-operands 0))
			  ((fx<? number-of-mandatory-operands number-of-given-operands)
			   (if (eq? 'yes (%check-mandatory-operands 0))
			       ;;If  the REST-TAG  is "T:object":  the rest  operands
			       ;;always match, so there is no need to iterate them.
			       (if (core-type-tag-matches-any-object? rest-tag)
				   'yes
				 (%check-rest-operands number-of-mandatory-operands rest-tag))
			     'no))
			  (else
			   ;;The number  of given operands  is less than  the minimum
			   ;;number of required operands.
			   #;(assert (fx>? number-of-mandatory-operands number-of-given-operands))
			   'wrong-num-args))))
	      (else
	       ;;If  we a  re  here: the  TUPLE-TAGS represents  a  tuple of  objects
	       ;;matching a fixed, mandatory, number of operands.
	       (if (fx=? number-of-mandatory-operands number-of-given-operands)
		   (%check-mandatory-operands 0)
		 'wrong-num-args)))))

    (define (%compatible-type-tag-and-operand? tag rand)
      ;;Match the core type specification TAG against the operand RAND.  Return false
      ;;if it is known that RAND is not of type TAG; otherwise return true.
      ;;
      #;(assert (core-type-tag? tag))
      (struct-case rand
	((known _ type)
	 (case (core-type-tag-is-a? type tag)
	   ;;Operand's type matches the expected argument's type.
	   ((yes) #t)
	   ;;Operand's type does *not* match the expected argument's type.
	   ((no)
	    #f)
	   ;;Operand's type maybe matches the expected argument's type, maybe not: it
	   ;;is compatible.
	   (else  #t)))
	;;Operand of unknown type: let's handle it as compatible.
	(else #t)))

    (define (%strictly-matching-type-tag-and-operand? tag rand)
      ;;Match the core type specification TAG  against the operand RAND.  Return true
      ;;if it is known that RAND is of type TAG; otherwise return false.
      ;;
      #;(assert (core-type-tag? tag))
      (struct-case rand
	((known _ type)
	 (case (core-type-tag-is-a? type tag)
	   ;;Operand's type matches the expected argument's type.
	   ((yes) #t)
	   ;;Operand's type does *not* match the expected argument's type.
	   ((no)  #f)
	   ;;Operand's type  maybe matches the  expected argument's type,  maybe not:
	   ;;let's handle it as *not* matching.
	   (else  #f)))
	;;Operand of unknown type: let's handle it as *not* matching.
	(else #f)))

    #| end of module |# )

  #| end of module: E-funcall |# )


;;;; done

#| end of LIBRARY |# )

;;; end of file
;; Local Variables:
;; End:
