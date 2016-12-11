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


#!r6rs
(library (ikarus.compiler.compat)
  (export
    import				module
    include				internal-body
    define*				lambda*
    case-define*			case-lambda*
    case-define				define-inline
    define-syntax-rule
    define-auxiliary-syntaxes		fluid-let-syntax
    brace				__who__
    define-constant			define-inline-constant
    let-constants
    receive				receive-and-return
    begin0
    parametrise				parameterize
    make-parameter
    define-struct			struct?
    type-descriptor
    reader-annotation?
    reader-annotation-source		reader-annotation-stripped
    getenv
    printf				fprintf
    format
    pretty-print			debug-print
    debug-print*
    gensym				print-gensym
    gensym-prefix
    foreign-call
    exact-integer?
    fxadd1				fxsub1
    fxnonnegative?
    fx=
    fx>					fx<
    fx>=				fx<=
    fxsll				fxsra
    fxremainder				fxquotient
    immediate?				bignum?
    ratnum?				compnum?
    cflonum?
    add1				sub1
    sll					sra
    make-list
    andmap				ormap
    set-car!				set-cdr!
    set-cons!
    vector-exists			vector-for-all
    bwp-object?				void-object?
    void
    reset-symbol-proc!
    procedure-argument-violation
    expression-return-value-violation
    print-stderr-message
    putprop				getprop
    property-list			remprop
    identifier-suffix			syntax->list
    all-identifiers?
    set-symbol-value!			symbol-value
    fasl-write

    cnd::define-core-condition-type
    cnd::&condition			cnd::&assertion

    symbols::$getprop
    symbols::$symbol-value
    symbols::$unbound-object?

    code-objects::make-code
    code-objects::code?
    code-objects::procedure-annotation
    code-objects::code-reloc-vector
    code-objects::code-freevars
    code-objects::code-size
    code-objects::code-ref
    code-objects::code->thunk
    code-objects::code-set!
    code-objects::set-code-reloc-vector!
    code-objects::set-code-annotation!
    code-objects::assembler-property-key
    code-objects::$closure-code
    code-objects::$code->closure
    code-objects::$code-reloc-vector
    code-objects::$code-freevars
    code-objects::$code-size
    code-objects::$code-annotation
    code-objects::$code-ref
    code-objects::$code-set!
    code-objects::$set-code-annotation!
    code-objects::$make-annotated-procedure
    code-objects::$annotated-procedure-annotation
    code-objects::$cpref

    ;; syntax helpers
    cond-boot-expansion
    inclusion-in-normal-boot-image
    inclusion-in-rotation-boot-image
    bootstrapping-for-normal-boot-image
    bootstrapping-for-rotation-boot-image)
  (import (vicare)
    (only (vicare expander)
	  all-identifiers?
	  identifier-suffix
	  syntax->list)
    (only (ikarus.printing-messages)
	  print-stderr-message)
    (vicare system structs)
    (prefix (only (vicare system $symbols)
		  $getprop
		  $symbol-value
		  $unbound-object?)
	    symbols::)
    (prefix (only (vicare system code-objects)
		  make-code
		  code?
		  procedure-annotation
		  code-reloc-vector
		  code-freevars
		  code-size
		  code-ref
		  code->thunk
		  code-set!
		  set-code-reloc-vector!
		  set-code-annotation!)
	    code-objects::)
    (prefix (only (vicare system $codes)
		  assembler-property-key
		  $closure-code
		  $code->closure
		  $code-reloc-vector
		  $code-freevars
		  $code-size
		  $code-annotation
		  $code-ref
		  $code-set!
		  $set-code-annotation!
		  $make-annotated-procedure
		  $annotated-procedure-annotation
		  $cpref)
	    code-objects::))

  (include "cond-boot-expansion.scm" #t)


;;;; condition-object definition

(cond-boot-expansion "condition-object definition syntax"
  ((inclusion-in-normal-boot-image)
   (import (prefix (only (ikarus conditions)
			 define-core-condition-type
			 &condition
			 &assertion)
		   cnd::)))

  ((inclusion-in-rotation-boot-image)
   (import (prefix (only (ikarus conditions)
			 define-core-condition-type
			 &condition
			 &assertion)
		   cnd::)))

  ((bootstrapping-for-normal-boot-image)
   (import (prefix (only (rnrs)
			 &condition
			 &assertion)
		   cnd::))
   (define-syntax cnd::define-core-condition-type
     (lambda (stx)
       (syntax-case stx ()
	 ((_ ?type-name . ?body)
	  (identifier? #'?type-name)
	  (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
	    (with-syntax
		((RTD (datum->syntax #'?type-name (string->symbol (string-append type-name.str "-rtd"))))
		 (RCD (datum->syntax #'?type-name (string->symbol (string-append type-name.str "-rcd")))))
	      #'(begin
		  (define-condition-type ?type-name . ?body)
		  (define RTD (record-type-descriptor        ?type-name))
		  (define RCD (record-constructor-descriptor ?type-name))))))
	 ))))

  ((bootstrapping-for-rotation-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a rotation boot image.
   (import (prefix (only (rnrs)
			 &condition
			 &assertion)
		   cnd::))
   (define-syntax cnd::define-core-condition-type
     (lambda (stx)
       (syntax-case stx ()
	 ((_ ?type-name . ?body)
	  (identifier? #'?type-name)
	  (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
	    (with-syntax
		((RTD (datum->syntax #'?type-name (string->symbol (string-append type-name.str "-rtd"))))
		 (RCD (datum->syntax #'?type-name (string->symbol (string-append type-name.str "-rcd")))))
	      #'(begin
		  (define-condition-type ?type-name . ?body)
		  (define RTD (record-type-descriptor        ?type-name))
		  (define RCD (record-constructor-descriptor ?type-name))))))
	 )))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'cond-compiler-expansion		'scheme-indent-function 1)
;; End:
