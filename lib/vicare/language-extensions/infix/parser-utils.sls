;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: parser utilities for the infix syntax
;;;Date: Thu Nov 28, 2013
;;;
;;;Abstract
;;;
;;;	The parser  table and the  general concept  of the package  is a
;;;	rework of Guile-Arith  by Ian Grant.  The parser  driver is from
;;;	the Lalr-scm package  by Dominique Boucher; the  parser table is
;;;	also generated using Lalr-scm.
;;;
;;;Copyright (c) 2010, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Copyright (C) 2000 The Free Software Foundation
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare language-extensions infix parser-utils)
  (export make-infix-transformer )
  (import (vicare)
    (for (vicare language-extensions increments)
      (meta -1))
    (for (vicare language-extensions infix auxiliary-syntaxes)
      (meta -1))
    (vicare language-extensions infix tokens)
    (vicare language-extensions infix parser-table))


;;;; Constant tokens representing the recognised operators.

(module (identifier->token
	 tok.eoi
	 tok.left-paren
	 tok.right-paren)

  (define (identifier->token atom)
    (case-stx atom
      ((+)		tok.add)
      ((-)		tok.sub)
      ((*)		tok.mul)
      ((/)		tok./)
      ((mod)		tok.mod)
      ((mod0)		tok.mod0)
      ((expt)		tok.expt)
      ((div)		tok.div)
      ((div0)		tok.div0)
      ((<)		tok.lt)
      ((>)		tok.gt)
      ((<=)		tok.le)
      ((>=)		tok.ge)
      ((=)		tok.eq)
      ((eq?)		tok.eq?)
      ((eqv?)		tok.eqv?)
      ((equal?)		tok.equal?)
      ((?)		tok.question)
      ((:)		tok.colon)

      ((incr!)		tok.incr!)
      ((decr!)		tok.decr!)

      ((and)		tok.and)
      ((not)		tok.not)
      ((or)		tok.ior)
      ((xor)		tok.xor)

      ((fx+)		tok.fxadd)
      ((fx-)		tok.fxsub)
      ((fx*)		tok.fxmul)
      ((fxdiv)		tok.fxdiv)
      ((fxdiv0)		tok.fxdiv0)
      ((fxmod)		tok.fxmod)
      ((fxmod0)		tok.fxmod0)
      ((fx<?)		tok.fxlt)
      ((fx>?)		tok.fxgt)
      ((fx<=?)		tok.fxle)
      ((fx>=?)		tok.fxge)
      ((fx=?)		tok.fxeq)

      ((fl+)		tok.fladd)
      ((fl-)		tok.flsub)
      ((fl*)		tok.flmul)
      ((fl/)		tok.fl/)
      ((flexpt)		tok.flexpt)
      ((fl<?)		tok.fllt)
      ((fl>?)		tok.flgt)
      ((fl<=?)		tok.flle)
      ((fl>=?)		tok.flge)
      ((fl=?)		tok.fleq)

      ((bitwise-and)			tok.bit-and)
      ((bitwise-ior)			tok.bit-ior)
      ((bitwise-xor)			tok.bit-xor)
      ((bitwise-not)			tok.bit-not)
      ((bitwise-arithmetic-shift-left)	tok.bit-shl)
      ((bitwise-arithmetic-shift-right)	tok.bit-shr)

      ((fxand)				tok.fxbit-and)
      ((fxior)				tok.fxbit-ior)
      ((fxxor)				tok.fxbit-xor)
      ((fxnot)				tok.fxbit-not)
      ((fxarithmetic-shift-left)	tok.fxbit-shl)
      ((fxarithmetic-shift-right)	tok.fxbit-shr)

      (else
       (make-<lexical-token> 'ID atom))))

  (define-syntax case-stx
    (syntax-rules (else)
      ((_ ?atom ((?s ...) ?e ...) ... (else ?b ...))
       (cond ((memv-stx ?atom (syntax ?s) ...) ?e ...) ... (else ?b ...)))))

  (define-syntax memv-stx
    (syntax-rules ()
      ((_ ?atom ?stx)
       (free-identifier=? ?atom ?stx))
      ((_ ?atom ?stx ...)
       (or (free-identifier=? ?atom ?stx) ...))))

  ;; Arithmetic operations.
  (define tok.add		(make-<lexical-token> 'ADD #'+))
  (define tok.sub		(make-<lexical-token> 'SUB #'-))
  (define tok.mul		(make-<lexical-token> 'MUL #'*))
  (define tok./			(make-<lexical-token> 'DIV #'/))
  (define tok.mod		(make-<lexical-token> 'MOD #'mod))
  (define tok.mod0		(make-<lexical-token> 'MOD #'mod0))
  (define tok.div		(make-<lexical-token> 'DIV #'div))
  (define tok.div0		(make-<lexical-token> 'DIV #'div0))
  (define tok.expt		(make-<lexical-token> 'EXPT #'expt))

  ;; Comparison operators.
  (define tok.lt		(make-<lexical-token> 'LT #'<))
  (define tok.gt		(make-<lexical-token> 'GT #'>))
  (define tok.le		(make-<lexical-token> 'LE #'<=))
  (define tok.ge		(make-<lexical-token> 'GE #'>=))
  (define tok.eq		(make-<lexical-token> 'EQ #'=))
  (define tok.eq?		(make-<lexical-token> 'EQ #'eq?))
  (define tok.eqv?		(make-<lexical-token> 'EQ #'eqv?))
  (define tok.equal?		(make-<lexical-token> 'EQ #'equal?))

  ;; Increment and decrement.
  (define tok.incr!		(make-<lexical-token> 'INCR	(cons #'pre-incr! #'post-incr!)))
  (define tok.decr!		(make-<lexical-token> 'DECR	(cons #'pre-decr! #'post-decr!)))

  ;; Logical operators
  (define tok.and		(make-<lexical-token> 'AND	#'and))
  (define tok.not		(make-<lexical-token> 'NOT	#'not))
  (define tok.ior		(make-<lexical-token> 'IOR	#'or))
  (define tok.xor		(make-<lexical-token> 'XOR	#'xor))

  ;; bitwise operators
  (define tok.bit-and		(make-<lexical-token> 'BIT-AND	#'bitwise-and))
  (define tok.bit-ior		(make-<lexical-token> 'BIT-IOR	#'bitwise-ior))
  (define tok.bit-xor		(make-<lexical-token> 'BIT-XOR	#'bitwise-xor))
  (define tok.bit-not		(make-<lexical-token> 'BIT-NOT	#'bitwise-not))
  (define tok.bit-shl		(make-<lexical-token> 'BIT-SHL	#'bitwise-arithmetic-shift-left))
  (define tok.bit-shr		(make-<lexical-token> 'BIT-SHR	#'bitwise-arithmetic-shift-right))

  ;;Fixnum operators.
  (define tok.fxadd		(make-<lexical-token> 'ADD	#'fx+))
  (define tok.fxsub		(make-<lexical-token> 'SUB	#'fx-))
  (define tok.fxmul		(make-<lexical-token> 'MUL	#'fx*))
  (define tok.fx/		(make-<lexical-token> 'DIV	#'fx/))
  (define tok.fxdiv		(make-<lexical-token> 'DIV	#'fxdiv))
  (define tok.fxdiv0		(make-<lexical-token> 'DIV	#'fxdiv0))
  (define tok.fxmod		(make-<lexical-token> 'MOD	#'fxmod))
  (define tok.fxmod0		(make-<lexical-token> 'MOD	#'fxmod0))
  (define tok.fxlt		(make-<lexical-token> 'LT	#'fx<?))
  (define tok.fxgt		(make-<lexical-token> 'GT	#'fx>?))
  (define tok.fxle		(make-<lexical-token> 'LE	#'fx<=?))
  (define tok.fxge		(make-<lexical-token> 'GE	#'fx>=?))
  (define tok.fxeq		(make-<lexical-token> 'EQ	#'fx=?))
  (define tok.fxbit-and		(make-<lexical-token> 'BIT-AND	#'fxand))
  (define tok.fxbit-ior		(make-<lexical-token> 'BIT-IOR	#'fxior))
  (define tok.fxbit-xor		(make-<lexical-token> 'BIT-XOR	#'fxxor))
  (define tok.fxbit-not		(make-<lexical-token> 'BIT-NOT	#'fxnot))
  (define tok.fxbit-shl		(make-<lexical-token> 'BIT-SHL	#'fxarithmetic-shift-left))
  (define tok.fxbit-shr		(make-<lexical-token> 'BIT-SHR	#'fxarithmetic-shift-right))

  ;;Flonum operators.
  (define tok.fladd		(make-<lexical-token> 'ADD	#'fl+))
  (define tok.flsub		(make-<lexical-token> 'SUB	#'fl-))
  (define tok.flmul		(make-<lexical-token> 'MUL	#'fl*))
  (define tok.fl/		(make-<lexical-token> 'DIV	#'fl/))
  (define tok.flexpt		(make-<lexical-token> 'EXPT	#'flexpt))
  (define tok.fllt		(make-<lexical-token> 'LT	#'fl<?))
  (define tok.flgt		(make-<lexical-token> 'GT	#'fl>?))
  (define tok.flle		(make-<lexical-token> 'LE	#'fl<=?))
  (define tok.flge		(make-<lexical-token> 'GE	#'fl>=?))
  (define tok.fleq		(make-<lexical-token> 'EQ	#'fl=?))

  ;;Ternary if-then-else.
  ;;
  ;;Here we use the #'if syntax object  as semantic value: it is a trick
  ;;to avoid insertion of  a raw value: the parser will  take it and use
  ;;it as the IF in the output form.
  (define tok.question		(make-<lexical-token> 'QUESTION-ID #'if))
  (define tok.colon		(make-<lexical-token> 'COLON-ID #':))

  ;;Special  constant tokens.   Notice that  the left  and right  parens
  ;;tokens  are  wrapped in  a  list  because  below  they are  used  as
  ;;arguments to APPEND.
  (define tok.eoi		(make-<lexical-token> '*eoi* (eof-object)))
  (define tok.left-paren	(make-<lexical-token> 'LPAREN #\())
  (define tok.right-paren	(make-<lexical-token> 'RPAREN #\)))

  #| end of module |# )


;;;; symbolic expression lexer utilities

(module (make-infix-transformer)

  (define make-infix-transformer
    (case-lambda
     (()
      (make-infix-transformer (lambda (atom kont) (kont atom)) #'begin))
     ((atom->token begin-id)
      (lambda (stx)
	(syntax-case stx ()
	  ((?infix ?operand ...)
	   (let ((stx-lst (sexp->list #'(?operand ...) begin-id)))
	     (cond ((null? stx-lst)
		    #'(values))
		   ((not (pair? stx-lst))
		    stx-lst)
		   (else
		    (let ((tokens (reverse (stx-list->reversed-tokens stx-lst '() atom->token begin-id))))
		      ((make-infix-sexp-parser) ;parser function
		       (lambda ()			;lexer function
			 (if (null? tokens)
			     tok.eoi
			   (let ((t (car tokens)))
			     (set! tokens (cdr tokens))
			     t)))
		       (lambda (message token) ;error handler
			 (syntax-violation 'infix
			   (string-append "processing infix expression: " message)
			   (syntax->datum #'(?infix ?operand ...)) token))
		       #f))))))
	  )))
     ))

  (define (sexp->list stx begin-id)
    (syntax-case stx (begin syntax quasisyntax quote quasiquote)
      (()			'())
      ((quote . ?body)		stx)
      ((quasiquote . ?body)	stx)
      ((syntax . ?body)		stx)
      ((quasisyntax . ?body)	stx)

      ((?begin . ?body)
       (and (identifier? #'?begin)
	    (free-identifier=? #'?begin begin-id))
       stx)

      ((?car . ?cdr)
       (cons (sexp->list #'?car begin-id)
	     (sexp->list #'?cdr begin-id)))

      (?atom
       (identifier? #'?atom)
       #'?atom)

      (?atom			(syntax->datum #'?atom))))

  (define (stx-list->reversed-tokens sexp reversed-tokens atom->token begin-id)
    ;;Convert a list  of syntax objects to the reversed  list of tokens.
    ;;This is a recursive function:  it recurses to process nested lists
    ;;in the  input SEXP; for this  reason we cannot reverse  the tokens
    ;;here, we have to let the caller reverse it.
    ;;
    (if (null? sexp)
	reversed-tokens
      (let ((atom (car sexp)))
	(cond ((identifier? atom)
	       (stx-list->reversed-tokens (cdr sexp)
					  (cons (atom->token atom identifier->token)
						reversed-tokens)
					  atom->token begin-id))

	      ((pair? atom)
	       (if (and (identifier? (car atom))
			(free-identifier=? #'begin (car atom)))
		   (stx-list->reversed-tokens
		    (cdr sexp)
		    (cons (make-<lexical-token> 'NUM (cons #'begin (cdr atom)))
			  reversed-tokens)
		    atom->token begin-id)
		 ;;Parentheses in reverse order  because the TOKENS will
		 ;;be reversed!!!
		 (stx-list->reversed-tokens
		  (cdr sexp)
		  (cons tok.right-paren
			(stx-list->reversed-tokens atom
						   (cons tok.left-paren reversed-tokens)
						   atom->token begin-id))
		  atom->token begin-id)))

	      (else
	       ;;Everything else is just put  there as "NUM", that is as
	       ;;operand.
	       (stx-list->reversed-tokens (cdr sexp)
					  (cons (make-<lexical-token> 'NUM atom)
						reversed-tokens)
					  atom->token begin-id))))))

  #| end of module |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'case-stx 'scheme-indent-function 1)
;; End:
