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
    dynamic-wind	exit
    (rename (call/cc call-with-current-continuation)))
  (import (except (ikarus)
		  call/cf		call/cc
		  call-with-current-continuation
		  dynamic-wind		exit
		  list-tail)
    (ikarus system $stack)
    (ikarus system $pairs)
    (ikarus system $fx)
    (vicare arguments validation)
    (only (vicare syntactic-extensions)
	  define-inline))


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
    (let ((lx (%unsafe-len x))
	  (ly (%unsafe-len y)))
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

  (module (%unsafe-len)

    (define-inline (%unsafe-len ls)
      (%len ls 0))

    (define (%len ls n)
      ;;Return the length of the list LS.
      ;;
      (if (null? ls)
	  n
	(%len ($cdr ls) ($fxadd1 n))))

    #| end of module: %unsafe-len |# )

  #| end of module: %common-tail |# )


;;;; winders

(module winders-handling
  (winders
   %winders-push!
   %winders-pop!)

  (define winders
    ;;A list  of pairs  beind the in-guard  and out-guard  functions set
    ;;with DYNAMIC-WIND:
    ;;
    ;;   ((?in-guard . ?out-guard) ...)
    ;;
    '())

  (define-inline (%winders-push! ?in ?out)
    (set! winders (cons (cons ?in ?out) winders)))

  (define-inline (%winders-pop!)
    (set! winders ($cdr winders)))

  #| end of module: winders-handling |# )


;;;; continuations

(define (%primitive-call/cf func)
  (if ($fp-at-base)
      (func ($current-frame))
    ($seal-frame-and-call func)))

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
      (%primitive-call/cc (lambda (kont)
			    (let ((save winders))
			      (define-inline (%do-wind-maybe)
				(unless (eq? save winders)
				  (%do-wind save)))
			      (func (case-lambda
				     ((v)
				      (%do-wind-maybe)
				      (kont v))
				     (()
				      (%do-wind-maybe)
				      (kont))
				     ((v1 v2 . v*)
				      (%do-wind-maybe)
				      (apply kont v1 v2 v*)))))))))

  (define-inline (%primitive-call/cc ?func)
    (%primitive-call/cf (lambda (frm)
			  (?func ($frame->continuation frm)))))

  (module (%do-wind)

    (define (%do-wind new)
      (import common-tail)
      (let ((tail (%common-tail new winders)))
	(%unwind* winders tail)
	(%rewind* new     tail)))

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
	(set! winders ($cdr ls))
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
	(set! winders ls)))

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
    (foreign-call "ikrt_exit" status))))


;;;; done

)

;;; end of file
