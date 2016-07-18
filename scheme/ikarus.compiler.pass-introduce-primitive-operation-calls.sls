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
(library (ikarus.compiler.pass-introduce-primitive-operation-calls)
  (export pass-introduce-primitive-operation-calls)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code)
    (ikarus.compiler.core-primitive-operation-names))


;;;; introduction
;;
;;The purpose of this module is to examine all the function calls:
;;
;;   (?operator ?arg ...)
;;
;;which, in recordized  code, are represented by struct instances  of type FUNCALL;
;;everything else is left untouched.  If the ?OPERATOR is a struct instance of type
;;PRIMREF  representing  a primitive  operation:  such  struct  is replaced  by  an
;;appropriate struct instance of type PRIMOPCALL; recordized code like:
;;
;;   #[funcall  #[primref ?name] (?arg ...)]
;;
;;is converted to:
;;
;;   #[primopcall ?name (?arg ...)]
;;
;;If  the FUNCALL  struct represents  a call  to a  proper primitive  function (not
;;operation): it is left untouched as FUNCALL struct.
;;
;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
;;recordized code must be composed by struct instances of the following types:
;;
;;   bind		closure-maker	conditional
;;   constant		fix		forcall
;;   funcall		jmpcall		known
;;   primref		seq		var
;;
;;NOTE  Not  all  the  struct  instances of  type  PRIMREF  reference  a  primitive
;;operation: the struct  type PRIMREF is used  to represent a reference  to all the
;;lexical core bindings defined by the boot image.  Only those for which ?NAME is a
;;symbol   satisfying  the   predicate  CORE-PRIMITIVE-OPERATION?    are  primitive
;;operations;  in  other   words,  only  the  operations  defined   by  the  syntax
;;DEFINE-PRIMITIVE-OPERATION  are  primitive  operations.   Examples:  $CAR,  $CDR,
;;FIXNUM?  are  primitive operations;  LIST, NUMBER?,  DISPLAY are  *not* primitive
;;operations.
;;
;;NOTE Not all  the instances of struct PRIMOPCALL are  generated from instances of
;;FUNCALL; so not all the instances of PRIMOPCALL are generated here.
;;


(define-syntax __module_who__
  (identifier-syntax 'pass-introduce-primitive-operation-calls))

(define* (pass-introduce-primitive-operation-calls Program)
  (struct-case Program
    ((codes code* body)
     (make-codes ($map/stx E-clambda code*)
		 (E body)))
    (else
     (compiler-internal-error __module_who__ __who__
       "invalid input expression" (unparse-recordized-code Program)))))


(module (E)

  (define* (E x)
    ;;Perform code transformation  traversing the whole hierarchy in  X, which must
    ;;be a struct instance representing recordized  code, and build a new hierarchy
    ;;of transformed, recordized code; return the new hierarchy.
    ;;
    ;;The purpose of this recordized code  traversal is to process struct instances
    ;;of  type  FUNCALL  with  the  module  MK-FUNCALL;  everything  else  is  left
    ;;untouched.
    ;;
    (struct-case x
      ((constant)
       x)

      ((var)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (make-bind lhs* ($map/stx E rhs*) (E body)))

      ((fix lhs* rhs* body)
       (make-fix lhs* rhs* (E body)))

      ((conditional e0 e1 e2)
       (make-conditional (E e0) (E e1) (E e2)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((closure-maker)
       x)

      ((forcall op arg*)
       (make-forcall op ($map/stx E arg*)))

      ((funcall rator arg*)
       (mk-funcall (E-known rator) ($map/stx E-known arg*)))

      ((jmpcall label rator arg*)
       (make-jmpcall label (E rator) ($map/stx E arg*)))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid input expression" (unparse-recordized-code x)))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  #| end of module: E |# )


(module (E-clambda)
  ;;The purpose  of this  module is  to apply E  to the  body of  every CASE-LAMBDA
  ;;clause.
  ;;
  (define* (E-clambda x)
    (struct-case x
      ((clambda label case* cp freevar* name)
       (make-clambda label ($map/stx E-clambda-case case*) cp freevar* name))
      (else
       (compiler-internal-error __module_who__ __who__ "invalid clambda" x))))

  (define* (E-clambda-case x)
    (struct-case x
      ((clambda-case info body)
       (make-clambda-case info (E body)))
      (else
       (compiler-internal-error __module_who__ __who__ "invalid clambda-case" x))))

  #| end of module: E-clambda |# )


(module (mk-funcall)

  (define (mk-funcall op arg*)
    ;;OP is a struct instance representing the operator in a function application.
    ;;
    ;;ARG* is a  list of struct instances representing the  arguments of a function
    ;;application.
    ;;
    ;;If the  operator is  a struct  instance of type  PRIMREF representing  a core
    ;;primitive  operation:  such  struct  is replaced  by  an  appropriate  struct
    ;;instance of type PRIMOPCALL.
    ;;
    (struct-case op
      ((known expr)
       (mk-funcall expr arg*))

      ((primref name)
       (if (%primitive-operation? name)
	   (make-primopcall name arg*)
	 (make-funcall op arg*)))

      (else
       (make-funcall op arg*))))

  (define (%primitive-operation? x)
    ;;Import  the  function CORE-PRIMITIVE-OPERATION?   from  a  module defined  in
    ;;"pass-specify-rep.ss".  (Marco Maggi; Oct 14, 2012)
    (module (core-primitive-operation?)
      (import CORE-PRIMITIVE-OPERATION-NAMES))
    (or (eq? x 'debug-call)
	(core-primitive-operation? x)))

  #| end of module: MK-FUNCALL |# )


;;;; done

#| end of library |# )

;;; end of file
