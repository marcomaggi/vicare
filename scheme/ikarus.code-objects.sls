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


#!vicare
(library (ikarus.code-objects)
  (export
    make-code
    code-reloc-vector		code-freevars
    code-size			code-ref
    code-set!			set-code-reloc-vector!
    set-code-annotation!	code-annotation
    procedure-annotation
    make-annotation-indirect	annotation-indirect?
    code->thunk			code-reloc-vector->sexp)
  (import (except (vicare)
		  make-code
		  code-reloc-vector		code-freevars
		  code-size			code-ref
		  code-set!			set-code-reloc-vector!
		  set-code-annotation!		code-annotation
		  procedure-annotation
		  code-reloc-vector->sexp)
    ;;NOTE  This library  is needed  to build  a  new boot  image.  Let's  try to  do
    ;;everything here using the system  libraries and not loading external libraries.
    ;;(Marco Maggi; Fri May 23, 2014)
    (vicare system $fx)
    (except (vicare system $codes)
	    code-reloc-vector->sexp)
    (prefix (vicare platform words)
	    words.))


;;;; helpers

(define ($fx-non-negative? X)
  (and (fixnum? X)
       ($fx>= X 0)))

(define (code-size? obj)
  ($fx-non-negative? obj))

(define (number-of-freevars? obj)
  ($fx-non-negative? obj))

(define (code-with-no-freevars? code)
  (and (code? code)
       ($fxzero? ($code-freevars code))))

(define (code-object-and-index? code idx)
  (and ($fxnonnegative? idx)
       ($fx<  idx ($code-size code))))

;;; --------------------------------------------------------------------

;;FIXME At the next boot image rotation: this syntax must be replaced with the one in
;;the library (vicare language-extensions syntaxes).  (Marco Maggi; Sun May 3, 2015)
;;
(define-syntax (preconditions stx)
  (module (vicare-built-with-arguments-validation-enabled)
    (module (arguments-validation)
      (include "ikarus.config.scm" #t))
    (define (vicare-built-with-arguments-validation-enabled)
      arguments-validation)
    #| end of module |# )
  (syntax-case stx ()
    ;;Single precondition.
    ;;
    ((_ (?predicate ?arg ...))
     (identifier? #'?who)
     (if (vicare-built-with-arguments-validation-enabled)
	 #'(unless (?predicate ?arg ...)
	     (assertion-violation __who__
	       "failed precondition"
	       '(?predicate ?arg ...) ?arg ...))
       #'(void)))

    ;;Multiple preconditions.
    ;;
    ((_ (?predicate ?arg ...) ...)
     (identifier? #'?who)
     (if (vicare-built-with-arguments-validation-enabled)
	 #'(begin
	     (preconditions (?predicate ?arg ...))
	     ...)
       #'(void)))
    ))


(define* (make-code {code-size code-size?} {freevars number-of-freevars?})
  (foreign-call "ikrt_make_code" code-size freevars '#()))

(define* (code-reloc-vector {x code?})
  ($code-reloc-vector x))

(define* (code-freevars {x code?})
  ($code-freevars x))

(define* (code-size {x code?})
  ($code-size x))

(define* (code-set! {code code?} {idx fixnum?} {octet words.word-u8?})
  (preconditions
   (code-object-and-index? code idx))
  ($code-set! code idx octet))

(define* (code-ref {code code?} {idx fixnum?})
  (preconditions
   (code-object-and-index? code idx))
  ($code-ref code idx))

(define* (set-code-reloc-vector! {code code?} {vec vector?})
  (foreign-call "ikrt_set_code_reloc_vector" code vec))

(define* (set-code-annotation! {code code?} v)
  (foreign-call "ikrt_set_code_annotation" code v))

(define* (code-annotation {code code?})
  ($code-annotation code))

(define* (code->thunk {code code-with-no-freevars?})
  ($code->closure code))

(define-struct annotation-indirect
  ())

(define* (procedure-annotation {x procedure?})
  (let ((ae ($code-annotation ($closure-code x))))
    (if (annotation-indirect? ae)
	($annotated-procedure-annotation x)
      ae)))


(define (procedure-or-code-or-vector? obj)
  (or (procedure? obj)
      (code?      obj)
      (vector?    obj)))

(define* (code-reloc-vector->sexp {code procedure-or-code-or-vector?})
  ;;CODE must be a code object.
  ;;
  (define-constant IK_RELOC_RECORD_VANILLA_OBJECT_TAG		0)
  (define-constant IK_RELOC_RECORD_FOREIGN_ADDRESS_TAG		1)
  (define-constant IK_RELOC_RECORD_OFFSET_IN_OBJECT_TAG		2)
  (define-constant IK_RELOC_RECORD_JUMP_TO_LABEL_OFFSET_TAG	3)
  (module (off-code-data)
    (module (wordsize)
      (include "ikarus.config.scm"))
    (define-constant vector-tag		5)
    (define-constant disp-code-data	(* 6 wordsize))
    (define-constant off-code-data	(fx- disp-code-data vector-tag)))
  (let* ((vec  (cond ((code? code)
		      ($code-reloc-vector code))
		     ((procedure? code)
		      ($code-reloc-vector ($closure-code code)))
		     (else code)))
	 (len  (vector-length vec)))
    (do ((i 0 (fxadd1 i))
	 (sexp '()))
	((fx=? i len)
	 (reverse sexp))
      (let* ((first-word (vector-ref vec i))
	     ;;Tag bits describing the type of this relocation vector record.
	     (first-word.tag   (fxand first-word #b11))
	     ;;Number of  bytes representing  the displacement  in the  code object's
	     ;;data area.
	     (first-word.disp  (fxsra first-word 2)))
	(cond ((fx=? first-word.tag IK_RELOC_RECORD_VANILLA_OBJECT_TAG)
	       (set! i (fxadd1 i))
	       (set-cons! sexp `(vanilla-object
				 (data-area-displacement ,first-word.disp)
				 (scheme-object          ,(vector-ref vec i)))))

	      ((fx=? first-word.tag IK_RELOC_RECORD_FOREIGN_ADDRESS_TAG)
	       (set! i (fxadd1 i))
	       (set-cons! sexp `(foreign-address
				 (data-area-displacement ,first-word.disp)
				 (foreign-function       ,(ascii->string (vector-ref vec i))))))

	      ((fx=? first-word.tag IK_RELOC_RECORD_OFFSET_IN_OBJECT_TAG)
	       (let ((offset        (vector-ref vec (fxadd1 i)))
		     (scheme-object (vector-ref vec (fx+ 2 i))))
		 (set! i (fx+ 2 i))
		 (set-cons! sexp `(displaced-object
				   (data-area-displacement ,first-word.disp)
				   (offset                 ,(if (and (code? scheme-object)
								     (fx=? offset off-code-data))
								'off-code-data
							      offset))
				   (scheme-object          ,scheme-object)))))

	      ((fx=? first-word.tag IK_RELOC_RECORD_JUMP_TO_LABEL_OFFSET_TAG)
	       (let ((offset        (vector-ref vec (fxadd1 i)))
		     (scheme-object (vector-ref vec (fx+ 2 i))))
		 (set! i (fx+ 2 i))
		 (set-cons! sexp `(jump-label
				   (data-area-displacement ,first-word.disp)
				   (offset                 ,offset)
				   (scheme-object          ,scheme-object)))))

	      (else
	       (error __who__
		 "invalid tag bits in first word of relocation vector record"
		 code first-word.tag)))))))


;;;; done

;; (define end-of-file-dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.code-objects end")))

#| end of library |# )

;;; end of file
