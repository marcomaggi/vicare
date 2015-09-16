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
    define-record			define-auxiliary-syntaxes
    define-inline			define-syntax-rule
    fluid-let-syntax
    unwind-protect
    receive				receive-and-return
    module				import
    begin0				define-values
    include
    define-list-of-type-predicate
    expand-time-gensym			expand-library

    __who__				brace

    make-struct-type			struct?
    struct-type-descriptor?		struct-type-field-names

    make-parameter			parametrise
    symbol-value			set-symbol-value!
    keyword?				would-block-object?
    unbound-object?			bwp-object?
    gensym
    vector-append			vector-exists
    add1				sub1
    pretty-print			pretty-print*
    fprintf				debug-print
    print-gensym			pretty-width
    void				port-id
    console-error-port			all-identifiers?
    string-empty?			syntax=?
    ratnum?				bignum?
    compnum?				cflonum?
    fx=
    fxadd1				fxsub1

    ;; compiler related operations
    compiler.eval-core			compiler.core-expr->optimized-code
    compiler.core-expr->optimisation-and-core-type-inference-code
    compiler.core-expr->assembly-code	compiler.compile-core-expr-to-thunk

    ;; runtime options
    option.debug-mode-enabled?
    option.drop-assertions?
    option.strict-r6rs
    option.enable-arguments-validation?
    option.print-loaded-libraries?
    option.print-debug-messages?
    option.typed-language.rhs-tag-propagation?
    option.typed-language.datums-as-operators?
    option.typed-language.setter-forms?
    option.typed-language?

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
    print-expander-warning-message
    print-expander-debug-message
    procedure-argument-violation
    warning
    library-debug-message

    ;; system stuff
    file-modification-time

    ;; unsafe bindings
    $car $cdr
    $fx= $fx< $fx> $fx<= $fx>= $fxadd1 $fxsub1
    $fxzero? $fxpositive? $fxnonnegative?
    $vector-length $vector-empty? $vector-ref $vector-set!
    $putprop $getprop $remprop $property-list)
  (import (vicare)
    (prefix (only (ikarus.compiler)
		  eval-core
		  compile-core-expr-to-thunk
		  core-expr->optimized-code
		  core-expr->optimisation-and-core-type-inference-code
		  core-expr->assembly-code
		  optimize-level)
	    compiler.)
    (prefix (rename (only (ikarus.options)
			  verbose?
			  debug-mode-enabled?
			  drop-assertions?
			  strict-r6rs
			  print-loaded-libraries?
			  print-debug-messages?
			  typed-language.rhs-tag-propagation?
			  typed-language.datums-as-operators?
			  typed-language.setter-forms?
			  typed-language?
			  vicare-built-with-arguments-validation-enabled)
		    (vicare-built-with-arguments-validation-enabled
		     enable-arguments-validation?))
	    option.)
    (only (ikarus.posix)
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


(define (print-expander-warning-message template . args)
  (when (option.verbose?)
    (let ((P (current-error-port)))
      (display "vicare: expander warning: " P)
      (apply fprintf P template args)
      (newline P))))

(define (print-expander-debug-message template . args)
  (when (option.print-debug-messages?)
    (let ((P (current-error-port)))
      (display "vicare: expander: " P)
      (apply fprintf P template args)
      (newline P))))

(define (library-debug-message template . args)
  (when (option.print-debug-messages?)
    ;;We do not want an exception from the I/O layer to ruin things.
    (guard (E (else (void)))
      (let ((P (current-error-port)))
	(apply fprintf P (string-append "vicare: " template "\n") args)
	(flush-output-port P)))))

;;; --------------------------------------------------------------------

(define (set-label-binding! label binding)
  (set-symbol-value! label binding))

(define (label-binding label)
  (and (symbol-bound? label) (symbol-value label)))

(define (remove-location x)
  ($unintern-gensym x))

(define (expander-option.integrate-special-list-functions?)
  (fx>=? 3 (compiler.optimize-level)))


;;;; syntax helpers

(define-syntax define-record
  (syntax-rules ()
    ((_ (?name ?maker ?pred) (?field* ...) ?printer)
     (begin
       (define-struct (?name ?maker ?pred) (?field* ...))
       (module ()
	 (set-rtd-printer! (type-descriptor ?name)
	   ?printer))))
    ((_ ?name (?field* ...) ?printer)
     (begin
       (define-struct ?name (?field* ...))
       (module ()
	 (set-rtd-printer! (type-descriptor ?name)
	   ?printer))))
    ((_ ?name (?field* ...))
     (define-struct ?name (?field* ...)))
    ))

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


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.compat")))

#| end of library |# )

;;; end of file
