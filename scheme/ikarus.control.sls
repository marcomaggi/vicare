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
    call-with-current-continuation
    dynamic-wind	exit)
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

(module (%common-tail)

  (define (%common-tail x y)
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


(define (primitive-call/cf func)
  (if ($fp-at-base)
      (func ($current-frame))
    ($seal-frame-and-call func)))

(define (call/cf func)
  (define who 'call/cf)
  (with-arguments-validation (who)
      ((procedure	func))
    (primitive-call/cf func)))

(define (primitive-call/cc func)
  (primitive-call/cf (lambda (frm)
		       (func ($frame->continuation frm)))))

(define winders
  '())

(define unwind*
  (lambda (ls tail)
    (unless (eq? ls tail)
      (set! winders (cdr ls))
      ((cdar ls))
      (unwind* (cdr ls) tail))))

(define rewind*
  (lambda (ls tail)
    (unless (eq? ls tail)
      (rewind* (cdr ls) tail)
      ((caar ls))
      (set! winders ls))))

(define do-wind
  (lambda (new)
    (let ((tail (%common-tail new winders)))
      (unwind* winders tail)
      (rewind* new tail))))

(define call/cc
  (lambda (f)
    (unless (procedure? f)
      (die 'call/cc "not a procedure" f))
    (primitive-call/cc
     (lambda (k)
       (let ((save winders))
	 (f (case-lambda
	     ((v) (unless (eq? save winders) (do-wind save)) (k v))
	     (()  (unless (eq? save winders) (do-wind save)) (k))
	     ((v1 v2 . v*)
	      (unless (eq? save winders) (do-wind save))
	      (apply k v1 v2 v*)))))))))

(define call-with-current-continuation
  (lambda (f)
    (unless (procedure? f)
      (die 'call-with-current-continuation
	   "not a procedure" f))
    (call/cc f)))

(define dynamic-wind
  (lambda (in body out)
    (unless (procedure? in)
      (die 'dynamic-wind "not a procedure" in))
    (unless (procedure? body)
      (die 'dynamic-wind "not a procedure" body))
    (unless (procedure? out)
      (die 'dynamic-wind "not a procedure" out))
    (in)
    (set! winders (cons (cons in out) winders))
    (call-with-values
	body
      (case-lambda
       ((v) (set! winders (cdr winders)) (out) v)
       (()  (set! winders (cdr winders)) (out) (values))
       ((v1 v2 . v*)
	(set! winders (cdr winders))
	(out)
	(apply values v1 v2 v*))))))

(define exit
  (case-lambda
   (() (exit 0))
   ((status) (foreign-call "ikrt_exit" status))))


;;;; done

)

;;; end of file
