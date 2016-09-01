;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008,2016  Abdulaziz Ghuloum
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
(library (psyntax.compat)
  (export
    define*				define-constant
    case-define				case-define*
    case-lambda*			lambda*
    define-auxiliary-syntaxes		let*-syntax
    define-inline			define-syntax-rule
    define-fluid-syntax			fluid-let-syntax
    unwind-protect
    receive				receive-and-return
    module				import
    begin0				define-values
    include				internal-body
    expand-time-gensym			expand-library
    with-blocked-exceptions
    custom-printer			super-protocol
    returnable				return
    set-cons!
    __who__				brace
    try					catch
    non-compound-sexp?			self-evaluating?
    delete-duplicates

    define-core-record-type		define-type-descriptors
    strip-angular-parentheses		equality-predicate

    cnd::define-core-condition-type
    cnd::&condition			cnd::&error
    cnd::&violation			cnd::&warning

    for-each-in-order
    define-struct			make-struct-type
    struct-type-symbol
    make-parameter			parametrise
    symbol-value			set-symbol-value!
    symbol-bound?
    keyword?				circular-list?
    gensym				gensym?
    vector-append			vector-exists
    vector-fold-right			vector-for-all
    vector-empty?			bytevector-empty?
    add1				sub1
    pretty-print			pretty-print*
    fprintf				debug-print
    print-gensym			print-graph
    pretty-width
    void				void-object?
    port-id				format
    console-error-port
    string-empty?
    ratnum?				bignum?
    compnum?				cflonum?
    fx=
    fxadd1				fxsub1
    fxnonnegative?			non-negative-fixnum?
    flzero?/positive			flzero?/negative
    ratnum-positive?			ratnum-negative?
    bignum-positive?			bignum-negative?
    uuid
    exact-compnum?
    zero-compnum?			zero-cflonum?
    procedure-arguments-consistency-violation
    last-pair				enum-set?
    list-of-nestrings?			list-of-single-item?

    list-of-symbols?			list-of-symbols.union
    list-of-symbols.intersection	list-of-symbols.subset?
    list-of-symbols.delete-duplicates
    list-of-symbols.delete-first-duplicates

    ;; compiler related operations
    compiler::eval-core			compiler::core-expr->optimized-code
    compiler::core-expr->optimisation-and-core-type-inference-code
    compiler::core-expr->assembly-code	compiler::compile-core-expr-to-thunk
    compiler::options::strict-r6rs

    ;; runtime options
    options::strict-type-checking?
    options::debug-mode-enabled?
    options::drop-assertions?
    options::enable-arguments-validation?
    options::print-loaded-libraries?
    options::print-debug-messages?
    options::print-library-debug-messages?

    expander-option.integrate-special-list-functions?
    foreign::dynamically-load-shared-object-from-identifier

    ;; interpreting the result of reading annotated sources
    reader-annotation?			reader-annotation-expression
    reader-annotation-stripped		reader-annotation-source
    reader-annotation-textual-position

    interaction-environment-maker-for-reader-extensions
    eval-for-reader-extension

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
    posix::getenv
    posix::file-string-pathname?
    posix::list-of-string-pathnames?
    posix::file-modification-time
    posix::real-pathname
    posix::split-search-path-string
    directory-exists?

    ;; special definitions
    define-list-of-type-predicate
    define-min/max-comparison
    define-equality/sorting-predicate
    define-inequality-predicate

    ;; unsafe bindings
    $car $cdr
    $fx= $fx< $fx> $fx<= $fx>= $fxadd1 $fxsub1
    $fxzero? $fxpositive? $fxnonnegative?
    $vector-length $vector-empty? $vector-ref $vector-set!
    $putprop $getprop $remprop $property-list
    $symbol-value $set-symbol-value!)
  (import (vicare)
    (ikarus records syntactic)
    (prefix (only (ikarus.compiler)
		  eval-core
		  compile-core-expr-to-thunk
		  core-expr->optimized-code
		  core-expr->optimisation-and-core-type-inference-code
		  core-expr->assembly-code
		  optimize-level)
	    compiler::)
    (prefix (rename (only (ikarus.compiler)
			  strict-r6rs-compilation)
		    (strict-r6rs-compilation	strict-r6rs))
	    compiler::options::)
    (prefix (rename (only (ikarus.options)
			  strict-type-checking?
			  debug-mode-enabled?
			  drop-assertions?
			  print-loaded-libraries?
			  print-verbose-messages?
			  print-debug-messages?
			  print-library-debug-messages?
			  vicare-built-with-arguments-validation-enabled)
		    (vicare-built-with-arguments-validation-enabled
		     enable-arguments-validation?))
	    options::)
    (prefix (only (ikarus conditions)
		  define-core-condition-type
		  &condition
		  &error
		  &violation
		  &warning)
	    cnd::)
    (ikarus.printing-messages)
    (only (ikarus.reader)
	  interaction-environment-maker-for-reader-extensions
	  eval-for-reader-extension)
    (prefix (only (vicare system posix)
		  getenv
		  file-string-pathname?
		  list-of-string-pathnames?
		  real-pathname
		  split-search-path-string
		  ;;This is used by INCLUDE to  register the modification time of the
		  ;;files included at expand-time.  Such time is used in a STALE-WHEN
		  ;;test.
		  file-modification-time)
	    posix::)
    (prefix (only (vicare.foreign-libraries)
		  dynamically-load-shared-object-from-identifier)
	    foreign::)
    (vicare system structs)
    ;;The syntactic  binding EXPAND-LIBRARY  is needed by  the implementation  of the
    ;;LIBRARY syntax, which  (for source code partitioning  reasons) cannot reference
    ;;the one defined in "psyntax.expander.sls".
    (only (vicare libraries)
	  expand-library)
    ;;NOTE Let's  try to import  the unsafe  operations from the  built-in libraries,
    ;;when possible, rather that using external libraries of macros.
    (only (vicare system $symbols)
	  $unintern-gensym
	  $putprop $getprop $remprop $property-list
	  $symbol-value $set-symbol-value!)
    (only (vicare system $fx)
	  $fx= $fx< $fx> $fx<= $fx>= $fxadd1 $fxsub1
	  $fxzero? $fxpositive? $fxnonnegative?)
    (only (vicare system $pairs)
	  $car $cdr)
    (only (vicare system $vectors)
	  $vector-empty? $vector-length
	  $vector-ref $vector-set!)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate))


;;;; printing debug and verbose messages

(module (print-expander-warning-message)

  (define-syntax-rule (print-expander-warning-message . ?args)
    (when (options::print-verbose-messages?)
      (%print-expander-warning-message . ?args)))

  (define (%print-expander-warning-message template . args)
    (print-stderr-message "expander warning: " template args))

  #| end of module |# )

(module (print-expander-debug-message)

  (define-syntax-rule (print-expander-debug-message . ?args)
    (when (options::print-debug-messages?)
      (%print-expander-debug-message . ?args)))

  (define (%print-expander-debug-message template . args)
    (print-verbose-message (string-append "expander expander: " (apply format template args))))

  #| end of module |# )

(module (print-library-debug-message)

  (define-syntax-rule (print-library-debug-message . ?args)
    (when (options::print-library-debug-messages?)
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
  (fx>=? 3 (compiler::optimize-level)))

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
      (void-object?		x)
      (would-block-object?	x)
      (unbound-object?		x)
      (bwp-object?		x)))

;;; --------------------------------------------------------------------

(case-define delete-duplicates
  ((ell)
   (delete-duplicates ell equal?))
  ((ell item=)
   (if (pair? ell)
       (let* ((x        (car ell))
	      (tail     (cdr ell))
	      (new-tail (delete-duplicates (remp (lambda (y) (item= x y)) tail)
					   item=)))
	 (if (eq? tail new-tail)
	     ell
	   (cons x new-tail)))
     '())))


;;;; lists of symbols

(define (list-of-symbols.delete-duplicates sym*)
  ;;Deletes duplicates  in the list  of symbols SYM*.   The first occurrence  of each
  ;;symbol is preserved in its position.
  ;;
  (if (pair? sym*)
      (let ((head (car sym*)))
	(cons head (list-of-symbols.delete-duplicates (remq head (cdr sym*)))))
    '()))

(define (list-of-symbols.delete-first-duplicates sym*)
  ;;Deletes duplicates  in the  list of  symbols SYM*.  The  last occurrence  of each
  ;;symbol is preserved in its position.
  ;;
  (reverse (list-of-symbols.delete-duplicates (reverse sym*))))

(define (list-of-symbols.union sym1* sym2*)
  (list-of-symbols.delete-duplicates (append sym1* sym2*)))

(define (list-of-symbols.intersection sym1* sym2*)
  (if (pair? sym1*)
      (if (memq (car sym1*) sym2*)
	  (cons (car sym1*) (list-of-symbols.intersection (cdr sym1*) sym2*))
	(list-of-symbols.intersection (cdr sym1*) sym2*))
    '()))

(define (list-of-symbols.subset? sub* super*)
  (and (for-all (lambda (sub)
		  (memq sub super*))
	 sub*)
       #t))


;;;; syntax helpers

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
