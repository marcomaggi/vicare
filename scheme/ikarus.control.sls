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


#!r6rs
(library (ikarus control)
  (export
    call/cf		call/cc
    dynamic-wind
    (rename (call/cc call-with-current-continuation))
    exit		exit-hooks)
  (import (except (ikarus)
		  call/cf		call/cc
		  call-with-current-continuation
		  dynamic-wind
		  exit			exit-hooks
		  list-tail)
    (ikarus system $stack)
    (ikarus system $pairs)
    (ikarus system $fx)
    (vicare arguments validation)
    #;(ikarus.emergency))


;;;; helpers

(module common-tail
  (%common-tail)

  (define (%common-tail x y)
    ;;This function  is used only  by the function %DO-WIND.   Given two
    ;;lists X and  Y (being lists of winders), which  are known to share
    ;;the  same  tail, return  the  first  pair  of their  common  tail;
    ;;example:
    ;;
    ;;   T = (p q r)
    ;;   X = (a b c d . T)
    ;;   Y = (i l m . T)
    ;;
    ;;return the  list T.   Attempt to  make this  operation as  fast as
    ;;possible.
    ;;
    (let ((lx (%unsafe-length x 0))
	  (ly (%unsafe-length y 0)))
      (let ((x (if ($fx> lx ly)
		   (%list-tail x ($fx- lx ly))
		 x))
	    (y (if ($fx> ly lx)
		   (%list-tail y ($fx- ly lx))
		 y)))
	(if (eq? x y)
	    x
	  (%drop-uncommon-heads ($cdr x) ($cdr y))))))

  (define (%list-tail ls n)
    ;;Skip the  first N items  in the list  LS and return  the resulting
    ;;tail.
    ;;
    (if ($fxzero? n)
	ls
      (%list-tail ($cdr ls) ($fxsub1 n))))

  (define (%drop-uncommon-heads x y)
    ;;Given two lists X and Y skip  their heads until the first EQ? pair
    ;;is found; return such pair.
    ;;
    (if (eq? x y)
	x
      (%drop-uncommon-heads ($cdr x) ($cdr y))))

  (define (%unsafe-length ls n)
    ;;Recursive  function returning  the  length of  the  list LS.   The
    ;;initial value of N must be 0.
    ;;
    (if (null? ls)
	n
      (%unsafe-length ($cdr ls) ($fxadd1 n))))

  #| end of module: %common-tail |# )


;;;; winders

(module winders-handling
  (%current-winders
   %winders-set!
   %winders-push!
   %winders-pop!
   %winders-eq?)

  (define the-winders
    ;;A list  of pairs  beind the in-guard  and out-guard  functions set
    ;;with DYNAMIC-WIND:
    ;;
    ;;   ((?in-guard . ?out-guard) ...)
    ;;
    ;;In   a   multithreading   context    this   variable   should   be
    ;;thread-specific.
    ;;
    '())

  (define (%current-winders)
    the-winders)

  (define (%winders-set! ?new)
    (set! the-winders ?new))

  (define (%winders-push! ?in ?out)
    (set! the-winders (cons (cons ?in ?out) the-winders)))

  (define (%winders-pop!)
    (set! the-winders ($cdr the-winders)))

  (define (%winders-eq? ?save)
    (eq? ?save the-winders))

  #| end of module: winders-handling |# )


;;;; continuations

(define (%primitive-call/cf func)
  ;;In tail  position: apply FUNC  to a continuation  object referencing
  ;;the  current  Scheme stack.   Remember  that  this function  can  be
  ;;inlined, but if it is not it can be called in tail position.
  ;;
  ;;Remember that FPR stands for Frame Pointer Register.
  ;;
  ;;CASE OF FRAME  POINTER AT BASE.  If the Scheme  stack scenario after
  ;;entering this function is:
  ;;
  ;;          high memory
  ;;   |                      | <-- pcb->frame_base
  ;;   |----------------------|
  ;;   | ik_underflow_handler | <-- FPR
  ;;   |----------------------|
  ;;   |         func         | --> closure object
  ;;   |----------------------|
  ;;             ...
  ;;   |----------------------|
  ;;   |      free word       | <-- pcb->stack_base
  ;;   |----------------------|
  ;;   |                      |
  ;;          low memory
  ;;
  ;;there is  no continuation to be  created, because we already  are at
  ;;the  base of  the stack.   We just  apply FUNC  to the  next process
  ;;continuation "pcb->next_k"; notice that "pcb->next_k" is not removed
  ;;from the PCB structure.
  ;;
  ;;NOTE Believe it or not: it has  been verified that this case (FPR at
  ;;the  frame  base) actually  happens,  especially  when running  with
  ;;debugging mode enabled.  (Marco Maggi; Mar 26, 2013)
  ;;
  ;;CASE OF  SOME FRAMES  ON THE  STACK.  If  the Scheme  stack scenario
  ;;after entering this function is:
  ;;
  ;;         high memory
  ;;   |                      | <-- pcb->frame_base
  ;;   |----------------------|
  ;;   | ik_underflow_handler |
  ;;   |----------------------|
  ;;     ... other frames ...
  ;;   |----------------------|          --
  ;;   |     local value 1    |          .
  ;;   |----------------------|          .
  ;;   |     local value 1    |          . frame 1
  ;;   |----------------------|          .
  ;;   |   return address 1   |          .
  ;;   |----------------------|          --
  ;;   |     local value 0    |          .
  ;;   |----------------------|          .
  ;;   |     local value 0    |          . frame 0
  ;;   |----------------------|          .
  ;;   |   return address 0   | <-- FPR  .
  ;;   |----------------------|          --
  ;;   |         func         | --> closure object
  ;;   |----------------------|
  ;;             ...
  ;;   |----------------------|
  ;;   |      free word       | <-- pcb->stack_base
  ;;   |----------------------|
  ;;   |                      |
  ;;          low memory
  ;;
  ;;we need to  freeze the current stack into a  continuation object and
  ;;then apply FUNC to such object.
  ;;
  ;;
  (if ($fp-at-base)
      (begin
	#;(emergency-write "primitive-call/cf: stack at base")
	(func ($current-frame)))
    (begin
      #;(emergency-write "primitive-call/cf: freezing the stack")
      ($seal-frame-and-call func))))

(define (call/cf func)
  (define who 'call/cf)
  (with-arguments-validation (who)
      ((procedure	func))
    (%primitive-call/cf func)))

(module (call/cc)
  (import winders-handling)

  (define (call/cc func)
    (define who 'call/cc)
    (with-arguments-validation (who)
	((procedure	func))
      (define (func-with-winders escape-function)
	(let ((save (%current-winders)))
	  (define (%do-wind-maybe)
	    (unless (%winders-eq? save)
	      (%do-wind save)))
	  (define escape-function-with-winders
	    (case-lambda
	     ((v)
	      (%do-wind-maybe)
	      (escape-function v))
	     (()
	      (%do-wind-maybe)
	      (escape-function))
	     ((v1 v2 . v*)
	      (%do-wind-maybe)
	      (apply escape-function v1 v2 v*))))
	  (func escape-function-with-winders)))
      (%primitive-call/cc func-with-winders)))

  (define (%primitive-call/cc func-with-winders)
    ;;In tail position: applies  FUNC-WITH-WINDERS to an escape function
    ;;which, when evaluated, reinstates the current continuation.
    ;;
    ;;The argument  FREEZED-FRAMES is  a continuation object  created by
    ;;%PRIMITIVE-CALL/CF  referencing  the   Scheme  stack  right  after
    ;;entering this function.  When  FREEZED-FRAMES arrives here, it has
    ;;already been prepended to the list "pcb->next_k".
    ;;
    ;;The  return  value of  $FRAME->CONTINUATION  is  a closure  object
    ;;which,  when evaluated,  resumes the  continuation represented  by
    ;;FREEZED-FRAMES.
    ;;
    (%primitive-call/cf (lambda (freezed-frames)
			  (func-with-winders ($frame->continuation freezed-frames)))))

  (module (%do-wind)

    (define (%do-wind new)
      (import common-tail)
      (let ((tail (%common-tail new (%current-winders))))
	(%unwind* (%current-winders) tail)
	(%rewind* new                tail)))

    (define (%unwind* ls tail)
      ;;The list LS must be the head  of WINDERS, TAIL must be a tail of
      ;;WINDERS.  Run the out-guards from LS, and pop their entry, until
      ;;TAIL is left in WINDERS.
      ;;
      ;;In other words, given LS and TAIL:
      ;;
      ;;   LS   = ((?old-in-guard-N . ?old-out-guard-N)
      ;;           ...
      ;;           (?old-in-guard-1 . ?old-out-guard-1)
      ;;           (?old-in-guard-0 . ?old-out-guard-0)
      ;;           . TAIL)
      ;;   TAIL = ((?in-guard . ?out-guard) ...)
      ;;
      ;;run  the out-guards  from ?OLD-OUT-GUARD-N  to ?OLD-OUT-GUARD-0;
      ;;finally set winders to TAIL.
      ;;
      (unless (eq? ls tail)
	(%winders-set! ($cdr ls))
	(($cdr ($car ls)))
	(%unwind* ($cdr ls) tail)))

    (define (%rewind* ls tail)
      ;;The list LS must be the new head of WINDERS, TAIL must be a tail
      ;;of WINDERS.   Run the in-guards  from LS in reverse  order, from
      ;;TAIL excluded to the top; finally set WINDERS to LS.
      ;;
      ;;In other words, given LS and TAIL:
      ;;
      ;;   LS   = ((?new-in-guard-N . ?new-out-guard-N)
      ;;           ...
      ;;           (?new-in-guard-1 . ?new-out-guard-1)
      ;;           (?new-in-guard-0 . ?new-out-guard-0)
      ;;           . TAIL)
      ;;   TAIL = ((?in-guard . ?out-guard) ...)
      ;;
      ;;run  the  in-guards  from  ?NEW-IN-GUARD-0  to  ?NEW-IN-GUARD-N;
      ;;finally set WINDERS to LS.
      ;;
      (unless (eq? ls tail)
	(%rewind* ($cdr ls) tail)
	(($car ($car ls)))
	(%winders-set! ls)))

    #| end of module: %do-wind |# )

  #| end of module: call/cc |# )


;;;; dynamic wind

(define (dynamic-wind in-guard body out-guard)
  (define who 'dynamic-wind)
  (import winders-handling)
  (with-arguments-validation (who)
      ((procedure	in-guard)
       (procedure	body)
       (procedure	out-guard))
    (in-guard)
    ;;We  do *not*  push  the guards  if an  error  occurs when  running
    ;;IN-GUARD.
    (%winders-push! in-guard out-guard)
    (call-with-values
	body
      (case-lambda
       ((v)
	(%winders-pop!)
	(out-guard)
	v)
       (()
	(%winders-pop!)
	(out-guard)
	(values))
       ((v1 v2 . v*)
	(%winders-pop!)
	(out-guard)
	(apply values v1 v2 v*))))))


;;;; other functions

(define exit
  (case-lambda
   (()
    (exit 0))
   ((status)
    (for-each (lambda (f)
		;;Catch and discard any  exception: exit hooks must take
		;;care of themselves.
		(guard (E (else (void)))
		  (f)))
      (exit-hooks))
    (foreign-call "ikrt_exit" status))))

(define exit-hooks
  (make-parameter '()
    (lambda (obj)
      (assert (and (list? obj)
		   (for-all procedure? obj)))
      obj)))


;;;; done

)

;;; end of file
