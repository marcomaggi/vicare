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
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (psyntax.compat)
  (export
    define*				define-constant
    case-define				case-define*
    case-lambda*			lambda*
    define-auxiliary-syntaxes
    define-inline			define-syntax-rule
    define-fluid-syntax			fluid-let-syntax
    unwind-protect
    receive				receive-and-return
    module				import
    begin0				define-values
    include
    define-list-of-type-predicate
    expand-time-gensym			expand-library
    with-blocked-exceptions

    non-compound-sexp?			self-evaluating?

    __who__				brace

    (rename (records.record-type-printer-set! record-type-printer-set!))

    make-struct-type
    make-parameter			parametrise
    symbol-value			set-symbol-value!
    symbol-bound?
    keyword?
    gensym				gensym?
    vector-append			vector-exists
    add1				sub1
    pretty-print			pretty-print*
    fprintf				debug-print
    print-gensym			print-graph
    pretty-width
    void				port-id
    console-error-port			all-identifiers?
    string-empty?			syntax=?
    ratnum?				bignum?
    compnum?				cflonum?
    fx=
    fxadd1				fxsub1
    fxnonnegative?

    ;; compiler related operations
    compiler.eval-core			compiler.core-expr->optimized-code
    compiler.core-expr->optimisation-and-core-type-inference-code
    compiler.core-expr->assembly-code	compiler.compile-core-expr-to-thunk

    ;; runtime options
    option.debug-mode-enabled?
    option.drop-assertions?
    option.typed-language?
    option.strict-r6rs
    option.enable-arguments-validation?
    option.print-loaded-libraries?
    option.print-debug-messages?
    option.print-library-debug-messages?

    expander-option.integrate-special-list-functions?
    foreign.dynamically-load-shared-object-from-identifier

    ;; interpreting the result of reading annotated sources
    annotation?				annotation-expression
    annotation-stripped			annotation-source
    annotation-textual-position

    ;; source position condition objects
    make-source-position-condition	source-position-condition?
    source-position-byte		source-position-character
    source-position-line		source-position-column
    source-position-port-id

    label-binding			set-label-binding!
    remove-location

    ;; symbol property lists
    putprop				getprop
    remprop				property-list

    ;; error handlers
    print-verbose-message
    print-error-message
    print-expander-warning-message
    print-expander-debug-message
    procedure-argument-violation
    warning
    print-library-debug-message

    ;; system stuff
    file-modification-time

    ;; unsafe bindings
    $car $cdr
    $fx= $fx< $fx> $fx<= $fx>= $fxadd1 $fxsub1
    $fxzero? $fxpositive? $fxnonnegative?
    $vector-length $vector-empty? $vector-ref $vector-set!
    $putprop $getprop $remprop $property-list)
  (import (except (vicare)
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Wed Sep 30, 2015)
		  with-blocked-exceptions)
    (prefix (only (ikarus.compiler)
		  eval-core
		  compile-core-expr-to-thunk
		  core-expr->optimized-code
		  core-expr->optimisation-and-core-type-inference-code
		  core-expr->assembly-code
		  optimize-level)
	    compiler.)
    (prefix (rename (only (ikarus.options)
			  debug-mode-enabled?
			  drop-assertions?
			  strict-r6rs
			  print-loaded-libraries?
			  print-verbose-messages?
			  print-debug-messages?
			  print-library-debug-messages?
			  typed-language?
			  vicare-built-with-arguments-validation-enabled)
		    (vicare-built-with-arguments-validation-enabled
		     enable-arguments-validation?))
	    option.)
    (ikarus.printing-messages)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Fri Oct 2,
    ;;2015)
    (prefix (only (ikarus records procedural)
		  record-type-printer-set!)
	    records.)
    (only (vicare language-extensions posix) #;(ikarus.posix)
	  ;;This is  used by INCLUDE to  register the modification time  of the files
	  ;;included at expand-time.  Such time is used in a STALE-WHEN test.
	  file-modification-time)
    (prefix (only (vicare.foreign-libraries)
		  dynamically-load-shared-object-from-identifier)
	    foreign.)
    ;;NOTE Let's  try to import  the unsafe  operations from the  built-in libraries,
    ;;when possible, rather that using external libraries of macros.
    (only (vicare system $symbols)
	  $unintern-gensym
	  $putprop $getprop $remprop $property-list)
    (only (vicare system $fx)
	  $fx= $fx< $fx> $fx<= $fx>= $fxadd1 $fxsub1
	  $fxzero? $fxpositive? $fxnonnegative?)
    (only (vicare system $pairs)
	  $car $cdr)
    (only (vicare system $vectors)
	  $vector-empty? $vector-length
	  $vector-ref $vector-set!))


;;;; configuration to build boot image

(define-syntax if-building-rotation-boot-image?
  (lambda (stx)
    (define rotating?
      (equal? "yes" (getenv "BUILDING_ROTATION_BOOT_IMAGE")))
    (fprintf (current-error-port)
	     "makefile.sps: conditional for ~a boot image\n"
	     (if rotating? "rotation" "normal"))
    (syntax-case stx ()
      ((_ ?true-body)
       (if rotating? #'?true-body #'(module ())))
      ((_ ?true-body ?false-body)
       (if rotating? #'?true-body #'?false-body))
      )))

;;FIXME To be fixed at the next boot image rotation.  (Marco Maggi; Sun May 10, 2015)
(if-building-rotation-boot-image?
    (import (only (vicare libraries) expand-library))
  (import (only (vicare) expand-library)))


;;;; printing debug and verbose messages

(module (print-expander-warning-message)

  (define-syntax-rule (print-expander-warning-message . ?args)
    (when (option.print-verbose-messages?)
      (%print-expander-warning-message . ?args)))

  (define (%print-expander-warning-message template . args)
    (print-stderr-message "expander warning: " template args))

  #| end of module |# )

(module (print-expander-debug-message)

  (define-syntax-rule (print-expander-debug-message . ?args)
    (when (option.print-debug-messages?)
      (%print-expander-debug-message . ?args)))

  (define (%print-expander-debug-message template . args)
    (print-verbose-message (string-append "expander expander: " (apply format template args))))

  #| end of module |# )

(module (print-library-debug-message)

  (define-syntax-rule (print-library-debug-message . ?args)
    (when (option.print-library-debug-messages?)
      (%print-library-debug-message . ?args)))

  (define (%print-library-debug-message template . args)
    (print-stderr-message #f template args))

  #| end of module |# )


;;;; stuff

(define (set-label-binding! label binding)
  (set-symbol-value! label binding))

(define (label-binding label)
  (and (symbol-bound? label) (symbol-value label)))

(define (remove-location x)
  ($unintern-gensym x))

(define (expander-option.integrate-special-list-functions?)
  (fx>=? 3 (compiler.optimize-level)))

;;; --------------------------------------------------------------------

(define (non-compound-sexp? obj)
  (or (null? obj)
      (self-evaluating? obj)
      ;;Notice that struct instances are not self evaluating.
      (struct? obj)))

(define (self-evaluating? x)
  (or (number?			x)
      (string?			x)
      (char?			x)
      (boolean?			x)
      (bytevector?		x)
      (keyword?			x)
      (eq? x (void))
      (would-block-object?	x)
      (unbound-object?		x)
      (bwp-object?		x)))


;;;; syntax helpers

(define-syntax define-list-of-type-predicate
  (syntax-rules ()
    ((_ ?who ?type-pred)
     (define (?who obj)
       (if (pair? obj)
	   (and (?type-pred (car obj))
		(?who (cdr obj)))
	 (null? obj))))
    ))

(define-syntax (expand-time-gensym stx)
  (syntax-case stx ()
    ((_ ?template)
     (let* ((tmp (syntax->datum (syntax ?template)))
	    (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	    (str (apply string-append tmp (map (lambda (N)
						 (string-append "." (number->string N)))
					    fxs)))
	    (sym (gensym str)))
       (with-syntax
	   ((SYM (datum->syntax (syntax here) sym)))
	 (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	 (syntax (quote SYM)))))))

;;FIXME To  be removed at  the next  boot image rotation.   (Marco Maggi; Thu  Mar 5,
;;2015)
;;
(define-syntax with-blocked-exceptions
  (syntax-rules ()
    ((_ ?thunk)
     (call/cc
	 (lambda (reinstate-with-blocked-exceptions-continuation)
	   (with-exception-handler
	       reinstate-with-blocked-exceptions-continuation
	     ?thunk))))
    ))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.compat")))

#| end of library |# )

;;; end of file
