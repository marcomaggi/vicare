;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Without  license  notice in  the  original  distribution, added  this
;;;license by induction (Marco Maggi; Nov 3, 2011).
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
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (ikarus.debugger)
  (export
    debug-call			guarded-start
    make-traced-procedure	make-traced-macro

    integer->machine-word	machine-word->integer)
  (import (except (ikarus)
		  make-traced-procedure
		  make-traced-macro

		  integer->machine-word
		  machine-word->integer

		  display write newline printf
		  pretty-print pretty-print* write-char
		  print-condition)
    (prefix (only (ikarus)
		  display write newline printf
		  pretty-print pretty-print* write-char
		  print-condition)
	    ikarus.)
    (vicare language-extensions syntaxes)
    (prefix (vicare platform words)
	    words.)
    (prefix (vicare unsafe capi)
	    capi.))


;;;; data types

(define-struct scell
  (cf ocell trace filter prev))

(define (mkcell prev)
  (make-scell #f #f #f #f prev))

(define *scell* (mkcell #f))

;;; --------------------------------------------------------------------

(define-struct trace (src/expr rator rands))

(define (trace-src x)
  (let ((x (trace-src/expr x)))
    (if (pair? x) (car x) #f)))

(define (trace-expr x)
  (let ((x (trace-src/expr x)))
    (if (pair? x) (cdr x) #f)))


;;;; arguments validation

(define-argument-validation (ulong who obj)
  (words.unsigned-long? obj)
  (assertion-violation who "expected exact integer representing unsigned long as argument" obj))


;;;; helpers

(define (%display thing)
  (let ((port (console-output-port)))
    (ikarus.display thing port)
    (flush-output-port port)))

(define (%write thing)
  (let ((port (console-output-port)))
    (ikarus.write thing port)
    (flush-output-port port)))

(define (%write-char thing)
  (let ((port (console-output-port)))
    (ikarus.write-char thing port)
    (flush-output-port port)))

(define (%newline)
  (let ((port (console-output-port)))
    (ikarus.newline port)
    (flush-output-port port)))

(define (%printf . args)
  (let ((port (console-output-port)))
    (apply fprintf port args)
    (flush-output-port port)))

(define (%pretty-print thing start-column)
  (let ((port (console-output-port)))
    (ikarus.pretty-print* thing port start-column #t)
    (flush-output-port port)))

(define (print-condition con)
  (let ((port (console-output-port)))
    (ikarus.print-condition con port)
    (flush-output-port port)))

(define (with-output-to-string/limit x len)
  ;;Serialise the  object X to  its string representation  generating at
  ;;most LEN characters.
  ;;
  (define n 0)
  (define str (make-string len))
  (call/cc
      (lambda (k)
        (define p
          (make-custom-textual-output-port "*limited-port*"
					   (lambda (buf i count)
					     (let f ((i i) (count count))
					       (unless (zero? count)
						 (if (= n len)
						     (k str)
						   (begin
						     (string-set! str n (string-ref buf i))
						     (set! n (+ n 1))
						     (f (+ i 1) (- count 1))))))
					     count)
					   #f #f #f))
        (parameterize ((print-graph #f))
	  (ikarus.write x p)
	  (flush-output-port p))
        (substring str 0 n))))


;;;; utilities

(define (integer->machine-word int)
  (define who 'integer->machine-word)
  (with-arguments-validation (who)
      ((ulong int))
    (foreign-call "ikrt_integer_to_machine_word" int)))

(define (machine-word->integer w)
  (foreign-call "ikrt_integer_from_machine_word" w))


(define (stacked-call pre thunk post)
  (call/cf
   (lambda (cf)
     ;;CF is  a continuation  object describing  the Scheme  stack frame
     ;;right after CALL/CF has been called.
     (if (eq? cf (scell-cf *scell*))
	 (thunk)
       (dynamic-wind
	   (let ((scell (mkcell *scell*)))
	     (lambda ()
	       (set! *scell* scell)
	       (pre)))
	   (lambda ()
	     (call-with-values
		 (lambda ()
		   (call/cf (lambda (cf)
			      (set-scell-cf! *scell* cf)
			      (thunk))))
	       (lambda v*
		 (set-scell-ocell! *scell* #f)
		 (cond ((scell-trace *scell*)
			=> (lambda (n)
			     (%display-return-trace n ((scell-filter *scell*) v*)))))
		 (apply values v*))))
	   (lambda ()
	     (post)
	     (set! *scell* (scell-prev *scell*))))))))


(module (%display-return-trace make-traced-procedure make-traced-macro)
  (define *trace-depth* 0)

  (define (%display-prefix n)
    (let f ((i 0))
      (unless (= i n)
	(%display (if (even? i) "|" " "))
	(f (+ i 1)))))

  (define (%display-call-trace n ls)
    (%display-prefix n)
    (%write ls)
    (%newline))

  (define (%display-return-trace n ls)
    (%display-prefix n)
    (unless (null? ls)
      (%write (car ls))
      (let f ((ls (cdr ls)))
	(unless (null? ls)
	  (%write-char #\space)
	  (%write (car ls))
	  (f (cdr ls)))))
    (%newline))

  (define make-traced-procedure
    (case-lambda
     ((name proc) (make-traced-procedure name proc (lambda (x) x)))
     ((name proc filter)
      (lambda args
	(stacked-call
	 (lambda ()
	   (set! *trace-depth* (add1 *trace-depth*)))
	 (lambda ()
	   (set-scell-trace!  *scell* *trace-depth*)
	   (set-scell-filter! *scell* filter)
	   (%display-call-trace *trace-depth* (filter (cons name args)))
	   (apply proc args))
	 (lambda ()
	   (set! *trace-depth* (sub1 *trace-depth*))))))))

  (define (make-traced-macro name x)
    (cond ((procedure? x)
	   (make-traced-procedure name x syntax->datum))
	  ((variable-transformer? x)
	   (make-variable-transformer
	    (make-traced-procedure name
				   (variable-transformer-procedure x)
				   syntax->datum)))
	  (else x)))

  #| end of module |# )


(module (get-traces debug-call)

  (define outer-ring-size 16)
  (define inner-ring-size 8)

  (define end-marker -1)

  (define-struct icell (prev next num content))
  (define-struct ocell (prev next num icell))

  (define (make-ring n cell-prev cell-next cell-prev-set! cell-next-set! make-cell)
    (let ((ring (make-cell)))
      (cell-prev-set! ring ring)
      (cell-next-set! ring ring)
      (do ((n n (- n 1)))
          ((<= n 1))
	(let ((cell (make-cell))
	      (next (cell-next ring)))
	  (cell-prev-set! cell ring)
	  (cell-next-set! cell next)
	  (cell-prev-set! next cell)
	  (cell-next-set! ring cell)))
      ring))

  (define (make-double-ring n m)
    (make-ring n
	       ocell-prev ocell-next set-ocell-prev! set-ocell-next!
	       (lambda ()
		 (make-ocell #f #f end-marker
			     (make-ring m
					icell-prev icell-next set-icell-prev! set-icell-next!
					(lambda () (make-icell #f #f end-marker (lambda () #f))))))))

  (define (ring->list x cell-num cell-prev cell-content)
    (let f ((x x) (orig #f))
      (if (or (eq? x orig) (eqv? (cell-num x) end-marker))
	  '()
	(cons (cons (cell-num x) (cell-content x))
	      (f (cell-prev x) (or orig x))))))

  (define (get-traces)
    (ring->list step-ring ocell-num ocell-prev
		(lambda (x)
		  (ring->list (ocell-icell x) icell-num icell-prev icell-content))))

  (define step-ring
    (make-double-ring outer-ring-size inner-ring-size))

  (define (debug-call src/expr rator . rands)
    (stacked-call
     (lambda ()
       (let ((prev step-ring))
	 (let ((next (ocell-next prev)))
	   (set-ocell-num! next (+ (ocell-num prev) 1))
	   (set-icell-num! (ocell-icell next) end-marker)
	   (set! step-ring next))))
     (lambda ()
       (set-scell-ocell! *scell* step-ring)
       (let ((trace (make-trace src/expr rator rands)))
	 (let ((prev (ocell-icell step-ring)))
	   (let ((next (icell-next prev)))
	     (set-icell-content! next trace)
	     (set-icell-num! next (+ (icell-num prev) 1))
	     (set-ocell-icell! step-ring next))))
       (apply rator rands))
     (lambda ()
       (let ((next step-ring))
	 (let ((prev (ocell-prev next)))
	   (set-ocell-num! prev (- (ocell-num next) 1))
	   (set-ocell-num! next end-marker)
	   (set-icell-num! (ocell-icell next) end-marker)
	   (set! step-ring prev))))))

  #| end of module |# )


(define (print-trace x)
  (define (chop x)
    (if (> (string-length x) 60)
	(format "~a#..." (substring x 0 56))
      x))
  (let ((n (car x))
	(x (cdr x)))
    (let ((str (format " [~a] " n)))
      (%printf str)
      (%pretty-print (trace-expr x) (string-length str)))
    (let ((src (trace-src x)))
      (when (pair? src)
	(%printf "     source: char ~a of ~a\n" (cdr src) (car src))))
    (%printf "     operator: ~s\n" (trace-rator x))
    (%printf "     operands: ")
    (let ((ls (map (lambda (x)
		     (with-output-to-string/limit x 80))
		(trace-rands x))))
      (if (< (apply + 1 (length ls) (map string-length ls)) 60)
	  (%write (trace-rands x))
	(begin
	  (%display "(")
	  (let f ((a (car ls)) (ls (cdr ls)))
	    (%display (chop a))
	    (if (null? ls)
		(%display ")")
	      (begin
		(%display "\n                ")
		(f (car ls) (cdr ls))))))))
    (%newline)))

(define (print-step x)
  (let ((n (car x)) (ls (cdr x)))
    (unless (null? ls)
      (%printf "FRAME ~s:\n" n)
      (for-each print-trace (reverse ls)))))

(define (print-all-traces)
  (let ((ls (reverse (get-traces))))
    (%printf "CALL FRAMES:\n")
    (for-each print-step ls)))

(define (guarded-start proc)
  (with-exception-handler
      (lambda (con)
        (define (enter-debugger con)
          (define (help)
            (%printf "Exception trapped by debugger.\n")
            (print-condition con)
            (%printf "~a\n"
		    (string-append
		     "[t] Trace. "
		     "[r] Reraise exception. "
		     "[c] Continue. "
		     "[q] Quit. "
		     "[?] Help. ")))
          (help)
          ((call/cc
	       (lambda (k)
		 (new-cafe
		  (lambda (x)
		    (case x
		      ((R r) (k (lambda () (raise-continuable con))))
		      ((Q q) (exit 99))
		      ((T t) (print-all-traces))
		      ((C c) (k void))
		      ((?)   (help))
		      (else (%printf "invalid option\n")))))
		 void))))
        (if (serious-condition? con)
            (enter-debugger con)
	  (raise-continuable con)))
    proc))


;;;; done

)

;;; end of file
