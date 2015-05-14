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
(library (ikarus.compiler.pass-insert-engine-checks)
  (export insert-engine-checks)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;This module traverses all the function bodies  and, if the body contains at least
;;one  JMPCALL struct  or one  FUNCALL struct  (in which  the operator  is *not*  a
;;PRIMREF), it transforms the ?BODY into:
;;
;;   (begin
;;     (primopcall '$do-event '())
;;     ?body)
;;
;;the call  to the primitive operation  $DO-EVENT suspends the execution  of Scheme
;;code  for the  current process  and enters  a subprocess  which can  take actions
;;asynchronously.
;;
;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
;;recordized code must be composed by struct instances of the following types:
;;
;;   bind		closure-maker	conditional
;;   constant		fix		forcall
;;   funcall		jmpcall		known
;;   primref		seq		var
;;


(define-syntax __module_who__
  (identifier-syntax 'insert-engine-checks))

(module (insert-engine-checks)

  (define (insert-engine-checks x)
    (struct-case x
      ((codes list body)
       (make-codes ($map/stx E-clambda list)
		   (%introduce-check-maybe body)))))

  (define (E-clambda x)
    (struct-case x
      ((clambda label cases cp freevar* name)
       (make-clambda label ($map/stx E-clambda-clause cases) cp freevar* name))))

  (define (E-clambda-clause x)
    (struct-case x
      ((clambda-case info body)
       (make-clambda-case info (%introduce-check-maybe body)))))

  (define (%introduce-check-maybe body)
    (if (E body)
	(make-seq EVENT-PRIMOPCALL body)
      body))

  (define-constant EVENT-PRIMOPCALL
    (make-primopcall '$do-event '()))

  #| end of module |# )


(module (E)

  (define* (E x)
    ;;The purpose of this recordized code traversal is to return true if:
    ;;
    ;;* At least one of the nested structs is an instance of JMPCALL.
    ;;
    ;;* At least one  of the nested structs is an instance of  FUNCALL in which the
    ;;  operator is *not* a struct instance of type PRIMREF.
    ;;
    ;;else the return value is false.
    ;;
    (struct-case x
      ((constant)
       #f)

      ((var)
       #f)

      ((primref)
       #f)

      ((jmpcall label rator arg*)
       #t)

      ((funcall rator arg*)
       (if (%known-primref? rator)
	   (ormap E-known arg*)
	 #t))

      ((bind lhs* rhs* body)
       (or (ormap E rhs*) (E body)))

      ((fix lhs* rhs* body)
       (E body))

      ((conditional e0 e1 e2)
       (or (E e0) (E e1) (E e2)))

      ((seq e0 e1)
       (or (E e0) (E e1)))

      ((primopcall op arg*)
       (ormap E-known arg*))

      ((forcall op arg*)
       (ormap E arg*))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid input expression" (unparse-recordized-code x)))))

  (define (E-known x)
    (struct-case x
      ((known expr)
       (E expr))
      (else
       (E x))))

  (define (%known-primref? x)
    ;;Return true if X is a struct  instance of type PRIMREF, possibly wrapped into
    ;;a struct instance of type KNOWN.
    ;;
    (struct-case x
      ((known expr)
       (%known-primref? expr))
      ((primref)
       #t)
      (else
       #f)))

  #| end of module: E |# )


;;;; done

#| end of library |# )

;;; end of file
