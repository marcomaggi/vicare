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
    define-fluid-override		unwind-protect
    receive				receive-and-return
    module				import
    begin0				define-values
    include

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
    fxadd1

    ;; low-level symbols properties
    system-label			system-id-gensym

    ;; compiler related operations
    compiler.eval-core			compiler.core-expr->optimized-code
    compiler.core-expr->assembly-code	compiler.compile-core-expr

    ;; runtime options
    option.debug-mode-enabled?
    option.strict-r6rs
    option.enable-arguments-validation?
    option.descriptive-labels
    option.print-loaded-libraries?
    option.print-debug-messages?
    option.tagged-language.rhs-tag-propagation?
    option.tagged-language.datums-as-operators?
    option.tagged-language.setter-forms?
    option.tagged-language?

    expander-option.integrate-special-list-functions?

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
    library-version-mismatch-warning
    library-stale-warning
    procedure-argument-violation
    warning
    library-debug-message

    ;; system stuff
    file-modification-time

    ;; library names and version numbers
    library-name?
    library-version-numbers?		library-version-number?
    library-name-decompose
    library-name->identifiers		library-name->version
    library-name-identifiers=?		library-name=?
    library-name<?			library-name<=?
    library-version=?
    library-version<?			library-version<=?

    ;; library references and conformity
    library-reference?			library-version-reference?
    library-sub-version-reference?	library-sub-version?
    library-reference-decompose
    library-reference->identifiers
    library-reference->version-reference
    library-reference-identifiers=?
    conforming-sub-version-and-sub-version-reference?
    conforming-version-and-version-reference?
    conforming-library-name-and-library-reference?

    ;; unsafe bindings
    $car $cdr
    $fx= $fx< $fx> $fx<= $fx>= $fxadd1
    $fxzero? $fxpositive? $fxnonnegative?
    $vector-length $vector-empty? $vector-ref $vector-set!
    $putprop $getprop $remprop $property-list)
  (import (except (vicare)
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Fri May 23, 2014)
		  system-id-gensym
		  system-label)
    (prefix (only (ikarus.compiler)
		  eval-core
		  compile-core-expr
		  core-expr->optimized-code
		  core-expr->assembly-code
		  optimize-level)
	    compiler.)
    (prefix (rename (ikarus.options)
		    (vicare-built-with-arguments-validation-enabled enable-arguments-validation?))
	    option.)
    (ikarus library-utils)
    (only (ikarus.posix)
	  ;;This is  used by INCLUDE to  register the modification time  of the files
	  ;;included at expand-time.  Such time is used in a STALE-WHEN test.
	  file-modification-time)
    ;;NOTE Let's  try to import  the unsafe  operations from the  built-in libraries,
    ;;when possible, rather that using external libraries of macros.
    (only (vicare system $symbols)
	  $unintern-gensym
	  $putprop $getprop $remprop $property-list)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Apr 15,
    ;;2014)
    (only (ikarus.symbols)
	  system-id-gensym
	  system-label)
    (only (vicare system $fx)
	  $fx= $fx< $fx> $fx<= $fx>= $fxadd1
	  $fxzero? $fxpositive? $fxnonnegative?)
    (only (vicare system $pairs)
	  $car $cdr)
    (only (vicare system $vectors)
	  $vector-empty? $vector-length
	  $vector-ref $vector-set!))


(define (library-version-mismatch-warning name depname filename)
  (when (option.verbose?)
    (fprintf (current-error-port)
	     "*** Vicare warning: library ~s has an inconsistent dependency \
              on library ~s; file ~s will be recompiled from source.\n"
	     name depname filename)))

(define (library-stale-warning name filename)
  (when (option.verbose?)
    (fprintf (current-error-port)
	     "*** Vicare warning: library ~s is stale; file ~s will be \
              recompiled from source.\n"
	     name filename)))

(define (library-debug-message template . args)
  (when (option.print-debug-messages?)
    ;;We do not want an exception from the I/O layer to ruin things.
    (guard (E (else (void)))
      (let ((P (current-error-port)))
	(apply fprintf P (string-append "vicare: " template "\n") args)
	(flush-output-port P)))))

;;; --------------------------------------------------------------------

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

(define (set-label-binding! label binding)
  (set-symbol-value! label binding))

(define (label-binding label)
  (and (symbol-bound? label) (symbol-value label)))

(define (remove-location x)
  ($unintern-gensym x))

(define (expander-option.integrate-special-list-functions?)
  (fx>=? 3 (compiler.optimize-level)))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.compat")))

#| end of library |# )

;;; end of file
