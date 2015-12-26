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
    include
    define*				lambda*
    case-define*			case-lambda*
    case-define				define-inline
    define-syntax-rule			define-syntax*
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
    add1				sub1
    sll					sra
    make-list
    andmap				ormap
    set-car!				set-cdr!
    set-cons!
    vector-exists			vector-for-all
    getprop				putprop
    bwp-object?				void-object?
    void
    reset-symbol-proc!
    procedure-argument-violation
    expression-return-value-violation
    print-stderr-message

    ;; syntax helpers
    cond-compiler-expansion
    building-normal-boot-image
    building-rotation-boot-image
    inclusion-in-boot-image)
  (import (vicare)
    (only (ikarus.printing-messages)
	  print-stderr-message))


;;;; helper syntaxes

(define-auxiliary-syntaxes
  building-normal-boot-image
  building-rotation-boot-image
  inclusion-in-boot-image)

(define-syntax (cond-compiler-expansion stx)

  ;;This is  true when the compiler  libraries are expanded  to be included in  a new
  ;;boot image, either  normal or rotation.  It is false  when the compiler libraries
  ;;are expanded to build a new boot image, not to be included in it.
  ;;
  (define expanding-for-inclusion-in-boot-image?
    (equal? "yes" (getenv "BUILDING_FOR_INCLUSION_IN_BOOT_IMAGE")))

  ;;This is meaningful only  when the compiler libraries are expanded  to build a new
  ;;boot image, not  to be included in it.  It  is true when the new boot  image is a
  ;;rotation one; it is false when the new boot image is a normal one.
  ;;
  (define expanding-to-build-new-rotation-boot-image?
    (equal? "yes" (getenv "BUILDING_ROTATION_BOOT_IMAGE")))

  (define (log description.stx)
    (fprintf (current-error-port)
	     "ikarus.compiler: conditional for ~a: ~a\n"
	     (syntax->datum description.stx)
	     (cond (expanding-for-inclusion-in-boot-image?
		    "inclusion in a new boot image")
		   (expanding-to-build-new-rotation-boot-image?
		    "building a new rotation boot image")
		   (else
		    "building a new normal boot image"))))

  (syntax-case stx (building-normal-boot-image
		    building-rotation-boot-image
		    inclusion-in-boot-image)
    ((_ ?description
	((inclusion-in-boot-image)	. ?inclusion-in-boot-body)
	((building-normal-boot-image)	. ?building-normal-body)
	((building-rotation-boot-image)	. ?building-rotation-body))
     (begin
       (log #'?description)
       (cond (expanding-for-inclusion-in-boot-image?
	      #'(begin . ?inclusion-in-boot-body))
	     (expanding-to-build-new-rotation-boot-image?
	      #'(begin . ?building-rotation-body))
	     (else
	      #'(begin . ?building-normal-body)))))
    ))


;;;; reader annotation objects API

;;FIXME To  be removed at the  next boot image  rotation.  (Marco Maggi; Sat  Dec 26,
;;2015)
(cond-compiler-expansion "reader annotation object API"
  ((inclusion-in-boot-image)
   (import (only (ikarus.reader)
		 reader-annotation?
		 reader-annotation-source
		 reader-annotation-stripped)))
  #;((inclusion-in-boot-image)
   ;;This is used  when the compiler's source  code is expanded for  inclusion in the
   ;;boot image.
   (define reader-annotation?			annotation?)
   (define reader-annotation-source		annotation-source)
   (define reader-annotation-stripped		annotation-stripped))

  ((building-normal-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a normal boot image.
   (import (rename (only (vicare)
			 annotation?
			 annotation-source
			 annotation-stripped)
		   (annotation?			reader-annotation?)
		   (annotation-source		reader-annotation-source)
		   (annotation-stripped		reader-annotation-stripped))))

  ((building-rotation-boot-image)
   ;;This is  used when the compiler's  source code is imported  in "makefile.sps" to
   ;;build a rotation boot image.
   (void)))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'cond-compiler-expansion		'scheme-indent-function 1)
;; End:
