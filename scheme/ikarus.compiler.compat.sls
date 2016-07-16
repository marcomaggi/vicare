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
    fxlogor				fxlogand
    fxlognot
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
    code?

    cnd::define-core-condition-type
    cnd::&condition			cnd::&assertion

    ;; syntax helpers
    cond-boot-expansion
    inclusion-in-normal-boot-image
    inclusion-in-rotation-boot-image
    bootstrapping-for-normal-boot-image
    bootstrapping-for-rotation-boot-image)
  (import (except (vicare)
		  reader-annotation?
		  reader-annotation-source
		  reader-annotation-stripped)
    (only (ikarus.printing-messages)
	  print-stderr-message))


;;;; helper syntaxes

(include "cond-boot-expansion.scm" #t)


;;;; stuff

(cond-boot-expansion "miscellaneous syntactic bindings"
  ((inclusion-in-normal-boot-image)
   (import (only (vicare expander)
		 all-identifiers?
		 identifier-suffix
		 syntax->list)))

  ((inclusion-in-rotation-boot-image)
   (import (only (vicare expander)
		 all-identifiers?
		 identifier-suffix
		 syntax->list)))

  ((bootstrapping-for-normal-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a normal boot image.
   (import (only (vicare expander)
		 all-identifiers?
		 identifier-suffix
		 syntax->list)))

  ((bootstrapping-for-rotation-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a rotation boot image.
   (import (only (vicare expander)
		 all-identifiers?
		 identifier-suffix
		 syntax->list))))


;;;; reader annotation objects API

;;FIXME To  be removed at the  next boot image  rotation.  (Marco Maggi; Sat  Dec 26,
;;2015)
(cond-boot-expansion "reader annotation object API"
  ((inclusion-in-normal-boot-image)
   (import (only (ikarus.reader)
		 reader-annotation?
		 reader-annotation-source
		 reader-annotation-stripped)))

  ;; ((inclusion-in-normal-boot-image)
  ;;  ;;This is used  when the compiler's source  code is expanded for  inclusion in the
  ;;  ;;boot image.
  ;;  (define reader-annotation?			annotation?)
  ;;  (define reader-annotation-source		annotation-source)
  ;;  (define reader-annotation-stripped		annotation-stripped))

  ((inclusion-in-rotation-boot-image)
   (import (only (ikarus.reader)
		 reader-annotation?
		 reader-annotation-source
		 reader-annotation-stripped)))

  ((bootstrapping-for-normal-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a normal boot image.
   (import (only (vicare)
		 reader-annotation?
		 reader-annotation-source
		 reader-annotation-stripped)))

  ((bootstrapping-for-rotation-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a rotation boot image.
   (import (only (vicare)
		 reader-annotation?
		 reader-annotation-source
		 reader-annotation-stripped))))


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
