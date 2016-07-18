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
(library (ikarus.compiler.pass-insert-stack-overflow-check)
  (export pass-insert-stack-overflow-check)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;This module traverses all the function bodies  and: if a ?BODY contains code that
;;will cause further use of the stack, it transforms it as follows:
;;
;;   (begin
;;     (primopcall '$stack-overflow-check '())
;;     ?body)
;;
;;so  that, right  after entering  the execution  of a  function, the  call to  the
;;primitive operation $STACK-OVERFLOW-CHECK  checks if the current  Scheme stack is
;;about to be  exhausted.  If a ?BODY does  not make further use of  the stack: its
;;function execution is a "stack tail".
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
  (identifier-syntax 'pass-insert-stack-overflow-check))

(module (pass-insert-stack-overflow-check)

  (define (pass-insert-stack-overflow-check x)
    (struct-case x
      ((codes code* body)
       (make-codes (map E-clambda code*)
		   (%process-body body)))))

  (module (E-clambda)
    ;;The purpose of this module is  to apply %PROCESS-BODY to all the
    ;;bodies of closure's clauses.
    ;;
    (define (E-clambda x)
      (struct-case x
	((clambda label case* cp freevar* name)
	 (make-clambda label (map E-clambda-clause case*) cp freevar* name))))

    (define (E-clambda-clause x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (%process-body body)))))

    #| end of module: E-clambda |# )

  (define (%process-body body)
    (if (%tail? body)
	(make-seq CHECK-PRIMOPCALL body)
      body))

  (define-constant CHECK-PRIMOPCALL
    (make-primopcall '$stack-overflow-check '()))

  #| end of module |# )


(module (%tail?)

  (define* (%tail? body)
    ;;Return true if  the recordized code BODY  contains only function
    ;;calls in tail position.
    ;;
    (struct-case body
      ((constant)		#f)
      ((var)			#f)
      ((primref)		#f)

      ((bind lhs* rhs* body)
       (or (ormap %non-tail? rhs*)
	   (%tail? body)))

      ((fix lhs* rhs* body)
       (%tail? body))

      ((conditional e0 e1 e2)
       (or (%non-tail? e0)
	   (%tail? e1)
	   (%tail? e2)))

      ((seq e0 e1)
       (or (%non-tail? e0)
	   (%tail? e1)))

      ((primopcall op arg*)
       (ormap %non-tail? arg*))

      ((forcall op arg*)
       (ormap %non-tail? arg*))

      ((funcall rator arg*)
       (or (%non-tail? rator)
	   (ormap %non-tail? arg*)))

      ((jmpcall label rator arg*)
       (or (%non-tail? rator)
	   (ormap %non-tail? arg*)))

      (else
       (compiler-internal-error __module_who__ __who__ "invalid expr" body))))

  (module (%non-tail?)

    (define* (%non-tail? x)
      ;;Notice that this function never  calls %TAIL?.  Return true if
      ;;the recordized code X contains any type of function call.
      ;;
      (struct-case x
	((constant)			#f)
	((var)			#f)
	((primref)			#f)

	((funcall rator arg*)		#t)
	((jmpcall label rator arg*)	#t)

	;;FIXME!  (Abdulaziz Ghuloum)
	((primopcall op arg*)
	 (ormap %non-tail?-known arg*))

	((bind lhs* rhs* body)
	 (or (ormap %non-tail? rhs*)
	     (%non-tail? body)))

	((fix lhs* rhs* body)
	 (%non-tail? body))

	((conditional e0 e1 e2)
	 (or (%non-tail? e0)
	     (%non-tail? e1)
	     (%non-tail? e2)))

	((seq e0 e1)
	 (or (%non-tail? e0)
	     (%non-tail? e1)))

	((forcall op arg*)
	 (ormap %non-tail? arg*))

	((known expr)
	 (%non-tail? expr))

	(else
	 (compiler-internal-error __module_who__ __who__ "invalid expr" x))))

    (define (%non-tail?-known x)
      (struct-case x
	((known expr)
	 (%non-tail? expr))
	(else
	 (%non-tail? x))))

    #| end of module: %non-tail? |# )

  #| end of module: %tail? |# )


;;;; done

#| end of library |# )

;;; end of file
