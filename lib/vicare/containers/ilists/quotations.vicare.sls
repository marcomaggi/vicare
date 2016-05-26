;;;
;;;Part of: Vicare Scheme
;;;Contents: immutable QUASIQUOTE syntax
;;;Date: Sun Jun 14, 2015
;;;
;;;Abstract
;;;
;;;	This library  is derived from  code in  Vicare Scheme, the  psyntax expander,
;;;	which in turn comes from Ikarus Scheme.
;;;
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;


(library (vicare containers ilists quotations)
  (export iquote iquasiquote iunquote iunquote-splicing)
  (import (vicare)
    (vicare containers ilists))


;;;; auxiliary syntaxes

(define-auxiliary-syntaxes iunquote iunquote-splicing)


;;;; foldable primitives
;;
;;The primitives IPAIR, ILIST, IAPPEND,  IVECTOR, ILIST->VECTOR are defined to return
;;a new object at each application.  This  means a compiler must not precompute their
;;application even  when their  operands are constants  known at  compile-time; these
;;primitives are "not foldable".
;;
;;When building  a tree of immutable  pairs, for compliance with  what R6RS specifies
;;about quasiquotation:
;;
;;   A `quasiquote'  expression may return  either fresh, mutable objects  or literal
;;   structure  for  any  structure  that  is constructed  at  run  time  during  the
;;   evaluation of the expression.
;;
;;we are allowed to  build a tree by precomputing it  at compile-time, when possible.
;;For this we should use "foldable" variants of the primitives.
;;
;;At present  (Mon Jun  15, 2015) Vicare  does not allow  the definition  of foldable
;;functions  in libraries,  so  below  we just  define  aliases  to the  non-foldable
;;functions.
;;

(define foldable-ipair		ipair)
(define foldable-ilist		ilist)
(define foldable-iappend	iappend)
(define foldable-vector		vector)
(define foldable-ilist->vector	ilist->vector)


;;;; quotation

(define-syntax iquote
  (syntax-rules ()
    ((_ (?item0 ?item ...))
     (foldable-ilist (iquote ?item0) (iquote ?item) ...))

    ((_ (?car . ?cdr))
     (foldable-ipair (iquote ?car) (iquote ?cdr)))

    ((_ #(?item ...))
     (foldable-vector (iquote ?item) ...))

    ((_ ?datum)
     (quote ?datum))
    ))


;;;; quasiquotation

(define-syntax (iquasiquote input-form.stx)
  (define (main stx)
    (syntax-case stx (iunquote iunquote-splicing)

      ;;For  coherence  with what  R6RS  specifies  about UNQUOTE:  a  single-operand
      ;;IUNQUOTE can appear outside of list  and vector templates.  This happens when
      ;;the input form is:
      ;;
      ;;   (iquasiquote (iunquote 1))	=> 1
      ;;
      ((_ (iunquote ?expr))
       #'?expr)

      ((_ (iunquote ?expr0 ?expr ...))
       (__synner__ "invalid multi-operand IUNQUOTE form outside list and vector templates"
		   #'(iunquote ?expr0 ?expr ...)))

      ((_ (iunquote-splicing ?expr ...))
       (__synner__ "invalid IUNQUOTE-SPLICING form outside list and vector templates"
		   #'(iunquote-splicing ?expr ...)))

      ((_ (?car . ?cdr))
       (%quasi #'(?car . ?cdr) 0))

      ((_ #(?expr ...))
       (%quasi #'#(?expr ...) 0))

      ;;This happens when the input form is:
      ;;
      ;;   (iquasiquote 1)	=> 1
      ;;
      ((_ ?expr)
       #'(quote ?expr))))

  (define (%quasi stx nesting-level)
    (syntax-case stx (iunquote iunquote-splicing iquasiquote)

      ;;This happens when STX appears in improper tail position:
      ;;
      ;;   (iquasiquote (1 . (iunquote (+ 2 3)))) => (ipair 1 5)
      ;;
      ((iunquote ?expr)
       (if (zero? nesting-level)
	   #'?expr
	 (%quasicons #'(quote iunquote) (%quasi (list #'?expr) (sub1 nesting-level)))))

      ;;This happens when the input form is:
      ;;
      ;;   (iquasiquote (1 . (iunquote)))
      ;;
      ((iunquote)
       (__synner__ "invalid IUNQUOTE form in improper tail position" stx))

      (((iunquote ?input-car-subexpr ...) . ?input-cdr)
       ;;For  coherence  with what  R6RS  specifies  about UNQUOTE:  a  multi-operand
       ;;IUNQUOTE must appear only inside a list or vector template.
       ;;
       ;;When the nesting level requires processing of unquoted expressions:
       ;;
       ;;* The expressions ?INPUT-CAR-SUBEXPR must be evaluated at run-time.
       ;;
       ;;* The input syntax object ?INPUT-CDR must be processed to produce the output
       ;;  syntax object ?OUTPUT-TAIL.
       ;;
       ;;* The returned syntax object must represent an expression that, at run-time,
       ;;  will construct the result as:
       ;;
       ;;     (ipair* ?input-car-subexpr ... ?output-tail)
       ;;
       (let ((input-car-subexpr*.stx  (syntax->list #'(?input-car-subexpr ...)))
	     (output-tail.stx         (%quasi #'?input-cdr nesting-level)))
	 (if (zero? nesting-level)
	     (%unquote-splice-cons* input-car-subexpr*.stx output-tail.stx)
	   (%quasicons (%quasicons #'(quote iunquote) (%quasi input-car-subexpr*.stx (sub1 nesting-level)))
		       output-tail.stx))))

      (((iunquote ?input-car-subexpr ... . ?input-car-tail) . ?input-cdr)
       (__synner__ "invalid improper list as IUNQUOTE form"
		   #'(iunquote ?input-car-subexpr ... . ?input-car-tail)))

      ;;This happens when the input form is:
      ;;
      ;;   (iquasiquote (1 . (iunquote-splicing)))
      ;;
      ((iunquote-splicing)
       (__synner__ "invalid IUNQUOTE-SPLICING form in improper tail position" stx))

      ;;This happens when STX appears in improper tail position:
      ;;
      ;;   (iquasiquote (1 . (iunquote-splicing (ilist (+ 2 3))))) => (ipair 1 5)
      ;;
      ((iunquote-splicing ?expr)
       (if (zero? nesting-level)
	   #'?expr
	 (%quasicons #'(quote iunquote-splicing) (%quasi (list #'?expr) (sub1 nesting-level)))))

      ((iunquote-splicing ?input-car-subexpr0 ?input-car-subexpr ...)
       (__synner__ "invalid multi-operand IUNQUOTE-SPLICING form in improper tail position" stx))

      (((iunquote-splicing ?input-car-subexpr ...) . ?input-cdr)
       ;;For  coherence   with  what   R6RS  specifies  about   UNQUOTE-SPLICING:  an
       ;;IUNQUOTE-SPLICING must appear only inside a list or vector template.
       ;;
       ;;When the nesting level requires processing of unquoted expressions:
       ;;
       ;;* The  subexpressions ?INPUT-CAR-SUBEXPR must  be evaluated at  run-time and
       ;;  their results must be lists:
       ;;
       ;;     ?input-car-subexpr => (?output-car-item ...)
       ;;
       ;;* The input syntax object ?INPUT-CDR must be processed to produce the output
       ;;  syntax object ?OUTPUT-TAIL.
       ;;
       ;;* The returned syntax object must represent an expression that, at run-time,
       ;;  will construct the result as:
       ;;
       ;;     (iappend ?input-car-subexpr ... ?output-tail)
       ;;
       (let ((input-car-subexpr*.stx  (syntax->list #'(?input-car-subexpr ...)))
	     (output-tail.stx         (%quasi #'?input-cdr nesting-level)))
	 (if (zero? nesting-level)
	     (%unquote-splice-append input-car-subexpr*.stx output-tail.stx)
	   (%quasicons (%quasicons #'(quote iunquote-splicing) (%quasi input-car-subexpr*.stx (sub1 nesting-level)))
		       output-tail.stx))))

      (((iunquote-splicing ?input-car-subexpr ... . ?input-car-tail) . ?input-cdr)
       (__synner__ "invalid improper list as IUNQUOTE-SPLICING form"
		   #'(iunquote-splicing ?input-car-subexpr ... . ?input-car-tail)))

      ((iquasiquote ?expr ...)
       (%quasicons #'(quote iquasiquote) (%quasi #'(?expr ...) (add1 nesting-level))))

      ((?car . ?cdr)
       (%quasicons (%quasi #'?car nesting-level)
		   (%quasi #'?cdr nesting-level)))

      (#(?item ...)
       (%quasivector (%vector-quasi #'(?item ...) nesting-level)))

      (?atom
       #'(quote ?atom))))

;;; --------------------------------------------------------------------

  (define (%quasicons output-car.stx output-cdr.stx)
    ;;Called to compose the output form resulting from processing:
    ;;
    ;;   (?car . ?cdr)
    ;;
    ;;return  a  syntax  object.   The  argument  OUTPUT-CAR.STX  is  the  result  of
    ;;processing  "(syntax ?car)".   The  argument OUTPUT-CDR.STX  is  the result  of
    ;;processing "(syntax ?cdr)".
    ;;
    (syntax-case output-cdr.stx (foldable-ilist)

      ;;When the result of  processing ?CDR is a quoted or iquoted  datum, we want to
      ;;return one among:
      ;;
      ;;   #'(ipair (quote  ?car-input-datum) (quote  ?output-cdr-datum))
      ;;   #'(ipair (quote  ?car-input-datum) (iquote ?output-cdr-datum))
      ;;   #'(ipair (iquote ?car-input-datum) (quote  ?output-cdr-datum))
      ;;   #'(ipair (iquote ?car-input-datum) (iquote ?output-cdr-datum))
      ;;   #'(ipair         ?car-input-datum  (quote  ?output-cdr-datum))
      ;;   #'(ipair         ?car-input-datum  (iquote ?output-cdr-datum))
      ;;
      ;;and we know that we can simplify:
      ;;
      ;;   #'(ipair (iquote ?car-input-datum) (quote ()))
      ;;   ===> #'(ilist (iquote ?car-input-datum))
      ;;
      ;;   #'(ipair (quote ?car-input-datum) (quote ()))
      ;;   ===> #'(ilist (quote ?car-input-datum))
      ;;
      ;;   #'(ipair        ?car-input-datum  (quote ()))
      ;;   ===> #'(ilist ?car-input-datum)
      ;;
      ((?cdr-quote ?cdr-datum)
       (%quote-or-iquote-id? #'?cdr-quote)
       (syntax-case output-car.stx ()
	 ((?car-quote ?car-datum)
	  (%quote-or-iquote-id? #'?car-quote)
	  (syntax-case #'?cdr-datum ()
	    (()
	     #'(foldable-ilist (?car-quote ?car-datum)))
	    (_
	     #'(foldable-ipair (?car-quote ?car-datum) (?cdr-quote ?cdr-datum)))))
	 (_
	  (syntax-case #'?cdr-datum ()
	    (()
	     #`(foldable-ilist #,output-car.stx))
	    (_
	     #`(foldable-ipair #,output-car.stx (?cdr-quote ?cdr-datum)))))
	 ))

      ;;When  the result  of  processing  ?CDR is  a  syntax  object representing  an
      ;;expression that, at run-time, will build an immutable list: prepend the input
      ;;expression as first item of the list.
      ;;
      ((foldable-ilist ?cdr-expr ...)
       #`(foldable-ilist #,output-car.stx ?cdr-expr ...))

      ;;When  the result  of processing  ?CDR is  a syntax  object representing  some
      ;;generic expression: return  a syntax object representing  an expression that,
      ;;at run-time, will build a pair.
      ;;
      (_
       #`(foldable-ipair #,output-car.stx #,output-cdr.stx))
      ))

  (define (%unquote-splice-cons* input-car-subexpr*.stx output-tail.stx)
    ;;Recursive function.  Called to build  the output form resulting from processing
    ;;the input form:
    ;;
    ;;   ((iunquote ?input-car-subexpr ...) . ?input-cdr)
    ;;
    ;;return a syntax object.  At the first application:
    ;;
    ;;* The argument INPUT-CAR-SUBEXPR*.STX is the list of syntax objects:
    ;;
    ;;     ((syntax ?input-car-subexpr) ...)
    ;;
    ;;* The argument OUTPUT-TAIL.STX is the result of processing:
    ;;
    ;;     (syntax ?input-cdr)
    ;;
    ;;The returned  output form must  be a  syntax object representing  an expression
    ;;that, at run-time, constructs the result as:
    ;;
    ;;   (ipair* ?input-car-subexpr0 ?input-car-subexpr ... ?output-tail)
    ;;
    ;;notice that the following expansion takes place:
    ;;
    ;;   ((iunquote) . ?input-cdr) ==> ?output-tail
    ;;
    (if (null? input-car-subexpr*.stx)
	output-tail.stx
      (%quasicons (car input-car-subexpr*.stx)
		  (%unquote-splice-cons* (cdr input-car-subexpr*.stx) output-tail.stx))))

  (define (%unquote-splice-append input-car-subexpr*.stx output-tail.stx)
    ;;Called to build the result of processing the input form:
    ;;
    ;;   ((iunquote-splicing ?input-car-subexpr ...) . ?input-cdr)
    ;;
    ;;return a syntax object.  At the first application:
    ;;
    ;;* The argument INPUT-CAR-SUBEXPR*.STX is the list of syntax objects:
    ;;
    ;;     ((syntax ?input-car-subexpr) ...)
    ;;
    ;;  where each expression ?INPUT-CAR-SUBEXPR is expected to return a list.
    ;;
    ;;* The argument OUTPUT-TAIL.STX is the result of processing:
    ;;
    ;;     (syntax ?input-cdr)
    ;;
    ;;The returned  output form must  be a  syntax object representing  an expression
    ;;that constructs the result as:
    ;;
    ;;   (iappend ?input-car-subexpr0 ?input-car-subexpr ... ?output-tail)
    ;;
    ;;notice that the following expansion takes place:
    ;;
    ;;   ((iunquote-splicing) . ?input-cdr) ==> ?output-tail
    ;;
    (let ((ls (let recur ((stx* input-car-subexpr*.stx))
		(if (null? stx*)
		    (syntax-case output-tail.stx (quote)
		      ((quote ())
		       '())
		      (_
		       (list output-tail.stx)))
		  (syntax-case (car stx*) (quote)
		    ((quote ())
		     (recur (cdr stx*)))
		    (_
		     (cons (car stx*) (recur (cdr stx*)))))))))
      (cond ((null? ls)
	     #'(quote ()))
	    ((null? (cdr ls))
	     (car ls))
	    (else
	     #`(foldable-iappend . #,ls)))))

;;; --------------------------------------------------------------------

  (define (%vector-quasi item*.stx nesting-level)
    ;;Recursive function.  Called to process an input syntax object with the format:
    ;;
    ;;   #(?item ...)
    ;;
    ;;At the first invocation, the argument ITEM*.STX is a syntax object representing
    ;;a proper list of items from the vector:
    ;;
    ;;   (syntax (?item ...))
    ;;
    ;;Return a syntax object representing an expression that, at run-time, will build
    ;;an ilist holding the vector items.
    ;;
    ;;NOTE  The difference  between  %QUASI  and %VECTOR-QUASI  is  that: the  former
    ;;accepts both  *proper* and *improper* lists  of items; the latter  accepts only
    ;;*proper* lists of items.
    ;;
    (syntax-case item*.stx ()
      ((?input-car . ?input-cdr)
       (let ((output-tail.stx (%vector-quasi #'?input-cdr nesting-level)))
	 (syntax-case #'?input-car (iquasiquote iunquote iunquote-splicing)

	   ((iunquote ?input-car-subexpr ...)
	    ;;When the nesting level requires processing of unquoted expressions:
	    ;;
	    ;;* The expressions ?INPUT-CAR-SUBEXPR must be evaluated at run-time.
	    ;;
	    ;;* The input  syntax object ?INPUT-CDR must be processed  to produce the
	    ;;  output syntax object ?OUTPUT-TAIL.
	    ;;
	    ;;*  The returned  syntax object  must represent  an expression  that, at
	    ;;  run-time, will construct the result as:
	    ;;
	    ;;     (ipair* ?input-car-subexpr ... ?output-tail)
	    ;;
	    ;;  notice that the following expansion takes place:
	    ;;
	    ;;     ((iunquote) . ?input-cdr) ==> ?output-tail
	    ;;
	    (let ((input-car-subexpr*.stx (syntax->list #'(?input-car-subexpr ...))))
	      (if (zero? nesting-level)
		  (%unquote-splice-cons* input-car-subexpr*.stx output-tail.stx)
		(%quasicons (%quasicons #'(quote iunquote) (%quasi input-car-subexpr*.stx (sub1 nesting-level)))
			    output-tail.stx))))

	   ((iunquote ?input-car-subexpr ... . ?input-car-tail)
	    (__synner__ "invalid improper list as IUNQUOTE form"
			#'(iunquote ?input-car-subexpr ... . ?input-car-tail)))

	   ((iunquote-splicing ?input-car-subexpr ...)
	    ;;When the nesting level requires processing of unquoted expressions:
	    ;;
	    ;;* The  subexpressions ?INPUT-CAR-SUBEXPR must be  evaluated at run-time
	    ;;  and their results must be lists:
	    ;;
	    ;;     ?input-car-subexpr => (?output-car-item ...)
	    ;;
	    ;;* The input  syntax object ?INPUT-CDR must be processed  to produce the
	    ;;  output syntax object ?OUTPUT-TAIL.
	    ;;
	    ;;*  The returned  syntax object  must represent  an expression  that, at
	    ;;  run-time, will construct the result as:
	    ;;
	    ;;     (iappend ?input-car-subexpr ... ?output-tail)
	    ;;
	    ;;  notice that the following expansion takes place:
	    ;;
	    ;;     ((iunquote-splicing) . ?input-cdr) ==> ?output-tail
	    ;;
	    (let ((input-car-subexpr*.stx (syntax->list #'(?input-car-subexpr ...))))
	      (if (zero? nesting-level)
		  (%unquote-splice-append input-car-subexpr*.stx output-tail.stx)
		(%quasicons (%quasicons #'(quote iunquote-splicing) (%quasi input-car-subexpr*.stx (sub1 nesting-level)))
			    output-tail.stx))))

	   ((iunquote-splicing ?input-car-subexpr ... . ?input-car-tail)
	    (__synner__ "invalid improper list as IUNQUOTE-SPLICING form"
			#'(iunquote-splicing ?input-car-subexpr ... . ?input-car-tail)))

	   ((iquasiquote ?nested-expr ...)
	    (%quasicons (%quasicons #'(quote iquasiquote) (%quasi #'(?nested-expr ...) (add1 nesting-level)))
			output-tail.stx))

	   ((?nested-input-car . ?nested-input-cdr)
	    (%quasicons (%quasicons (%quasi #'?nested-input-car nesting-level)
				    (%quasi #'?nested-input-cdr nesting-level))
			output-tail.stx))

	   (#(?nested-input-item ...)
	    (%quasicons (%quasivector (%vector-quasi #'(?nested-input-item ...) nesting-level))
			output-tail.stx))

	   (?input-atom
	    (%quasicons #'(quote ?input-atom) output-tail.stx)))))

      (()
       #'(quote ()))))

  (define (%quasivector output-list.stx)
    ;;Process to call the result of %QUASI-VECTOR.  The argument OUTPUT-LIST.STX is a
    ;;syntax object representing an expression that, at run-time, will build an ilist
    ;;holding the vector items.
    ;;
    ;;Return  a syntax  object representing  an  expression that,  at run-time,  will
    ;;convert the ilist to a vector.  In general applying ILIST->VECTOR always works,
    ;;but there are special cases where a more efficient processing is possible.
    ;;
    (syntax-case output-list.stx (foldable-ilist quote)
      ((foldable-ilist (quote ?datum) ...)
       #`(quote #(?datum ...)))
      (_
       #`(foldable-ilist->vector #,output-list.stx))))

;;; --------------------------------------------------------------------

  (define (%quote-or-iquote-id? id)
    (and (identifier? id)
	 (or (free-identifier=? id  #'quote)
	     (free-identifier=? id #'iquote))))

  (main input-form.stx))


;;;; done

#| end of library |# )

;;; end of file
