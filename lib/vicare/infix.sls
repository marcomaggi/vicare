;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: implementation of the INFIX syntax
;;;Date: Tue May 18, 2010
;;;
;;;Abstract
;;;
;;;	The parser  table and the  general concept  of the package  is a
;;;	rework of Guile-Arith  by Ian Grant.  The parser  driver is from
;;;	the Lalr-scm package  by Dominique Boucher; the  parser table is
;;;	also generated using Lalr-scm.  The  implementation of XOR is by
;;;	Derick Eddington.
;;;
;;;	  The parser table  is build in the  Nausicaa/Scheme package and
;;;	copied here, verbatim, from the file:
;;;
;;;		nausicaa/language/infix/sexp-parser.sls
;;;
;;;Copyright (c) 2010, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; copyright notice for the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!vicare
(library (vicare infix)
  (export infix incr! decr! xor ? :
	  (rename (mod					%)
		  (and					&&)
		  (or					!!)
		  (xor					^^)
		  (not					~~)
		  (incr!				++)
		  (decr!				--))
	  (rename (bitwise-and				&)
		  (bitwise-ior				!)
		  (bitwise-xor				^)
		  (bitwise-not				~)
		  (bitwise-arithmetic-shift-left	<<)
		  (bitwise-arithmetic-shift-right	>>))
	  (rename (fxand				fx&)
		  (fxior				fx!)
		  (fxxor				fx^)
		  (fxnot				fx~)
		  (fxarithmetic-shift-left		fx<<)
		  (fxarithmetic-shift-right		fx>>)))
  (import (for (rnrs) run expand (meta 2)))


;;;; helpers

(define-syntax ?
  (syntax-rules ()))

(define-syntax :
  (syntax-rules ()))

(define-syntax incr!
  (syntax-rules ()))

(define-syntax decr!
  (syntax-rules ()))

(define-syntax pre-incr
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?id)
       (identifier? #'?id)
       #'(begin
	   (set! ?id (+ ?id 1))
	   ?id))
      ((_ ?expr)
       #'(+ ?expr 1))
      (_
       (syntax-violation 'pre-incr "invalid pre-increment operation" (syntax->datum stx))))))

(define-syntax pre-decr
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?id)
       (identifier? #'?id)
       #'(begin
	   (set! ?id (- ?id 1))
	   ?id))
      ((_ ?expr)
       #'(- ?expr 1))
      (_
       (syntax-violation 'pre-decr "invalid pre-decrement operation" (syntax->datum stx))))))

(define-syntax post-incr
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?id)
       (identifier? #'?id)
       #'(let ((v ?id))
	   (set! ?id (+ ?id 1))
	   v))
      ((_ ?expr)
       #'(+ ?expr 1))
      (_
       (syntax-violation 'post-incr "invalid post-increment operation" (syntax->datum stx))))))

(define-syntax post-decr
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?id)
       (identifier? #'?id)
       #'(let ((v ?id))
	   (set! ?id (- ?id 1))
	   v))
      ((_ ?expr)
       #'(- ?expr 1))
      (_
       (syntax-violation 'post-decr "invalid post-decrement operation" (syntax->datum stx))))))

(define-syntax xor
  (syntax-rules ()
    ((_ expr ...)
     (xor-aux #F expr ...))))

(define-syntax xor-aux
  (syntax-rules ()
    ((_ r)
     r)
    ((_ r expr)
     (let ((x expr))
       (if r
           (and (not x) r)
	 x)))
    ((_ r expr0 expr ...)
     (let ((x expr0))
       (and (or (not r) (not x))
	    (let ((n (or r x)))
	      (xor-aux n expr ...)))))))


(define-syntax infix
  (let ()
    (define make-<lexical-token>	cons)
    (define <lexical-token>?		pair?)
    (define <lexical-token>-category	car)
    (define <lexical-token>-value	cdr)

    ;;; Constant tokens representing the recognised operators.
    ;;
    ;; Arithmetic operations.
    (define $add	(make-<lexical-token> 'ADD #'+))
    (define $sub	(make-<lexical-token> 'SUB #'-))
    (define $mul	(make-<lexical-token> 'MUL #'*))
    (define $/		(make-<lexical-token> 'DIV #'/))
    (define $mod	(make-<lexical-token> 'MOD #'mod))
    (define $mod0	(make-<lexical-token> 'MOD #'mod0))
    (define $div	(make-<lexical-token> 'DIV #'div))
    (define $div0	(make-<lexical-token> 'DIV #'div0))
    (define $expt	(make-<lexical-token> 'EXPT #'expt))
    ;; Comparison operators.
    (define $lt		(make-<lexical-token> 'LT #'<))
    (define $gt		(make-<lexical-token> 'GT #'>))
    (define $le		(make-<lexical-token> 'LE #'<=))
    (define $ge		(make-<lexical-token> 'GE #'>=))
    (define $eq		(make-<lexical-token> 'EQ #'=))
    (define $eq?	(make-<lexical-token> 'EQ #'eq?))
    (define $eqv?	(make-<lexical-token> 'EQ #'eqv?))
    (define $equal?	(make-<lexical-token> 'EQ #'equal?))
    ;; Increment and decrement.
    (define $incr!	(make-<lexical-token> 'INCR	(cons #'pre-incr #'post-incr)))
    (define $decr!	(make-<lexical-token> 'DECR	(cons #'pre-decr #'post-decr)))
    ;; Logical operators
    (define $and	(make-<lexical-token> 'AND	#'and))
    (define $not	(make-<lexical-token> 'NOT	#'not))
    (define $ior	(make-<lexical-token> 'IOR	#'or))
    (define $xor	(make-<lexical-token> 'XOR	#'xor))
    ;; bitwise operators
    (define $bit-and	(make-<lexical-token> 'BIT-AND	#'bitwise-and))
    (define $bit-ior	(make-<lexical-token> 'BIT-IOR	#'bitwise-ior))
    (define $bit-xor	(make-<lexical-token> 'BIT-XOR	#'bitwise-xor))
    (define $bit-not	(make-<lexical-token> 'BIT-NOT	#'bitwise-not))
    (define $bit-shl	(make-<lexical-token> 'BIT-SHL	#'bitwise-arithmetic-shift-left))
    (define $bit-shr	(make-<lexical-token> 'BIT-SHR	#'bitwise-arithmetic-shift-right))
    ;;Fixnum operators.
    (define $fxadd	(make-<lexical-token> 'ADD	#'fx+))
    (define $fxsub	(make-<lexical-token> 'SUB	#'fx-))
    (define $fxmul	(make-<lexical-token> 'MUL	#'fx*))
    (define $fx/	(make-<lexical-token> 'DIV	#'fx/))
    (define $fxdiv	(make-<lexical-token> 'DIV	#'fxdiv))
    (define $fxdiv0	(make-<lexical-token> 'DIV	#'fxdiv0))
    (define $fxmod	(make-<lexical-token> 'MOD	#'fxmod))
    (define $fxmod0	(make-<lexical-token> 'MOD	#'fxmod0))
    (define $fxlt	(make-<lexical-token> 'LT	#'fx<?))
    (define $fxgt	(make-<lexical-token> 'GT	#'fx>?))
    (define $fxle	(make-<lexical-token> 'LE	#'fx<=?))
    (define $fxge	(make-<lexical-token> 'GE	#'fx>=?))
    (define $fxeq	(make-<lexical-token> 'EQ	#'fx=?))
    (define $fxbit-and	(make-<lexical-token> 'BIT-AND	#'fxand))
    (define $fxbit-ior	(make-<lexical-token> 'BIT-IOR	#'fxior))
    (define $fxbit-xor	(make-<lexical-token> 'BIT-XOR	#'fxxor))
    (define $fxbit-not	(make-<lexical-token> 'BIT-NOT	#'fxnot))
    (define $fxbit-shl	(make-<lexical-token> 'BIT-SHL	#'fxarithmetic-shift-left))
    (define $fxbit-shr	(make-<lexical-token> 'BIT-SHR	#'fxarithmetic-shift-right))
    ;;Flonum operators.
    (define $fladd	(make-<lexical-token> 'ADD	#'fl+))
    (define $flsub	(make-<lexical-token> 'SUB	#'fl-))
    (define $flmul	(make-<lexical-token> 'MUL	#'fl*))
    (define $fl/	(make-<lexical-token> 'DIV	#'fl/))
    (define $flexpt	(make-<lexical-token> 'EXPT	#'flexpt))
    (define $fllt	(make-<lexical-token> 'LT	#'fl<?))
    (define $flgt	(make-<lexical-token> 'GT	#'fl>?))
    (define $flle	(make-<lexical-token> 'LE	#'fl<=?))
    (define $flge	(make-<lexical-token> 'GE	#'fl>=?))
    (define $fleq	(make-<lexical-token> 'EQ	#'fl=?))
    ;;Ternary if-then-else.
    ;;
    ;;Here we  use the  #'if syntax  object as semantic  value: it  is a
    ;;trick to avoid  insertion of a raw value: the  parser will take it
    ;;and use it as the IF in the output form.
    (define $question	(make-<lexical-token> 'QUESTION-ID #'if))
    (define $colon	(make-<lexical-token> 'COLON-ID #':))
    ;;Special constant  tokens.  Notice that  the left and  right parens
    ;;tokens  are wrapped  in  a list  because  below they  are used  as
    ;;arguments to APPEND.
    (define $eoi		(make-<lexical-token> '*eoi* (eof-object)))
    (define $left-paren		(make-<lexical-token> 'LPAREN #\())
    (define $right-paren	(make-<lexical-token> 'RPAREN #\)))

    (define-syntax memv-stx
      (syntax-rules ()
	((_ ?atom ?stx)
	 (free-identifier=? ?atom ?stx))
	((_ ?atom ?stx ...)
	 (or (free-identifier=? ?atom ?stx) ...))))

    (define-syntax case-stx
      (syntax-rules (else)
	((_ ?atom ((?s ...) ?e ...) ... (else ?b ...))
	 (cond ((memv-stx ?atom (syntax ?s) ...) ?e ...) ... (else ?b ...)))))

    (define-syntax drop/stx
      (syntax-rules ()
	((_ ?ell ?k)
	 (let loop ((ell ?ell)
		    (k   ?k))
	   (if (zero? k)
	       ell
	     (loop (cdr ell) (- k 1)))))))

    (define-syntax define-inline
      (syntax-rules ()
	((_ (?name ?arg ...) ?form0 ?form ...)
	 (define-syntax ?name
	   (syntax-rules ()
	     ((_ ?arg ...)
	      (begin ?form0 ?form ...)))))))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?begin . ?body)
	 (and (identifier? #'?begin) (free-identifier=? #'begin #'?begin))
	 (cons #'begin #'?body))
	(()		'())
	((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
	(?atom		(identifier? #'?atom)	#'?atom)
	(?atom		(syntax->datum #'?atom))))

    (define (stx-list->reversed-tokens sexp reversed-tokens)
      ;;Convert a list of syntax objects to the reversed list of tokens.
      ;;This  is a  recursive function:  it recurses  to  process nested
      ;;lists in the  input SEXP; for this reason  we cannot reverse the
      ;;tokens here, we have to let the caller reverse it.
      ;;
      (if (null? sexp)
	  reversed-tokens
	(let ((atom (car sexp)))
	  (cond ((identifier? atom)
		 (stx-list->reversed-tokens
		  (cdr sexp)
		  (cons (case-stx atom
			  ((+)		$add)
			  ((-)		$sub)
			  ((*)		$mul)
			  ((/)		$/)
			  ((mod)	$mod)
			  ((mod0)	$mod0)
			  ((expt)	$expt)
			  ((div)	$div)
			  ((div0)	$div0)
			  ((<)		$lt)
			  ((>)		$gt)
			  ((<=)		$le)
			  ((>=)		$ge)
			  ((=)		$eq)
			  ((eq?)	$eq?)
			  ((eqv?)	$eqv?)
			  ((equal?)	$equal?)
			  ((?)		$question)
			  ((:)		$colon)

			  ((incr!)	$incr!)
			  ((decr!)	$decr!)

			  ((and)	$and)
			  ((not)	$not)
			  ((or)		$ior)
			  ((xor)	$xor)

			  ((fx+)	$fxadd)
			  ((fx-)	$fxsub)
			  ((fx*)	$fxmul)
			  ((fxdiv)	$fxdiv)
			  ((fxdiv0)	$fxdiv0)
			  ((fxmod)	$fxmod)
			  ((fxmod0)	$fxmod0)
			  ((fx<?)	$fxlt)
			  ((fx>?)	$fxgt)
			  ((fx<=?)	$fxle)
			  ((fx>=?)	$fxge)
			  ((fx=?)	$fxeq)

			  ((fl+)	$fladd)
			  ((fl-)	$flsub)
			  ((fl*)	$flmul)
			  ((fl/)	$fl/)
			  ((flexpt)	$flexpt)
			  ((fl<?)	$fllt)
			  ((fl>?)	$flgt)
			  ((fl<=?)	$flle)
			  ((fl>=?)	$flge)
			  ((fl=?)	$fleq)

			  ((bitwise-and)			$bit-and)
			  ((bitwise-ior)			$bit-ior)
			  ((bitwise-xor)			$bit-xor)
			  ((bitwise-not)			$bit-not)
			  ((bitwise-arithmetic-shift-left)	$bit-shl)
			  ((bitwise-arithmetic-shift-right)	$bit-shr)

			  ((fxand)				$fxbit-and)
			  ((fxior)				$fxbit-ior)
			  ((fxxor)				$fxbit-xor)
			  ((fxnot)				$fxbit-not)
			  ((fxarithmetic-shift-left)		$fxbit-shl)
			  ((fxarithmetic-shift-right)		$fxbit-shr)

			  (else
			   (make-<lexical-token> 'ID atom)))
			reversed-tokens)))
		((pair? atom)
		 (if (and (identifier? (car atom))
			  (free-identifier=? #'begin (car atom)))
		     (stx-list->reversed-tokens
		      (cdr sexp)
		      (cons (make-<lexical-token> 'NUM (cons #'begin (cdr atom)))
			    reversed-tokens))
		   ;;Parentheses  in reverse  order  because the  TOKENS
		   ;;will be reversed!!!
		   (stx-list->reversed-tokens
		    (cdr sexp)
		    (cons $right-paren
			  (stx-list->reversed-tokens atom (cons $left-paren reversed-tokens))))))
		(else
		 ;;Everything else  is just put there as  "NUM", that is
		 ;;as operand.
		 (stx-list->reversed-tokens
		  (cdr sexp)
		  (cons (make-<lexical-token> 'NUM atom)
			reversed-tokens)))))))


    (define (lr-driver action-table goto-table reduction-table)
      (define (parser-instance true-lexer error-handler yycustom)
	(let ((stack-values		'(#f))
	      (stack-states		'(0))
	      (reuse-last-token	#f))

	  (define (main lookahead)
	    (let ((category (<lexical-token>-category lookahead)))
	      (if (eq? '*lexer-error* category)
		  (main (attempt-error-recovery lookahead))
		(let ((action (select-action category (current-state))))
		  (cond ((eq? action 'accept) ;success, end of parse
			 (cadr stack-values)) ;return the value to the caller

			((eq? action '*error*) ;syntax error in input
			 (if (eq? category '*eoi*)
			     (error-handler "unexpected end of input" lookahead)
			   (main (attempt-error-recovery lookahead))))

			((<= 0 action) ;shift (= push) token on the stack
			 (stack-push! action (<lexical-token>-value lookahead))
			 (main (if (eq? category '*eoi*)
				   lookahead
				 (begin
				   (reduce-using-default-actions)
				   (lexer)))))

			(else ;reduce using the rule at index "(- ACTION)"
			 (reduce (- action))
			 (main lookahead)))))))

	  (define lexer
	    (let ((last-token #f))
	      (lambda ()
		(if reuse-last-token
		    (set! reuse-last-token #f)
		  (begin
		    (set! last-token (true-lexer))
		    (unless (<lexical-token>? last-token)
		      (error-handler "expected lexical token from lexer" last-token)
		      (true-lexer))))
		last-token)))

	  (define (yypushback)
	    (set! reuse-last-token #t))

	  (define (select-action terminal-symbol state-index)
	    (let* ((action-alist (vector-ref action-table state-index))
		   (pair         (assq terminal-symbol action-alist)))
	      (if pair (cdr pair) (cdar action-alist))))

	  (define (reduce reduction-table-index)
	    (define (%main)
	      (apply (vector-ref reduction-table reduction-table-index)
		     reduce-pop-and-push yypushback yycustom stack-states stack-values))

	    (define (reduce-pop-and-push used-values goto-keyword semantic-clause-result
					 yy-stack-states yy-stack-values)
	      (let* ((yy-stack-states (drop/stx yy-stack-states used-values))
		     (new-state-index (cdr (assq goto-keyword
						 (vector-ref goto-table (car yy-stack-states))))))
		;;This is NOT a call to STACK-PUSH!
		(set! stack-states (cons new-state-index        yy-stack-states))
		(set! stack-values (cons semantic-clause-result yy-stack-values))))

	    (%main))

	  (define (reduce-using-default-actions)
	    (let ((actions-alist (vector-ref action-table (current-state))))
	      (when (= 1 (length actions-alist))
		(let ((default-action (cdar actions-alist)))
		  (when (< default-action 0)
		    (reduce (- default-action))
		    (reduce-using-default-actions))))))

	  (define (attempt-error-recovery lookahead)

	    (define (%main)
	      (error-handler "syntax error, unexpected token" lookahead)
	      (let ((token (synchronise-parser/rewind-stack)))
		;;If recovery succeeds: TOKEN  is set to the next lookahead.
		;;If recovery fails: TOKEN is set to end-of-input.
		(unless (eq? '*eoi* (<lexical-token>-category token))
		  (reduce-using-default-actions))
		token))

	    (define (synchronise-parser/rewind-stack)
	      (if (null? stack-values)
		  (begin ;recovery failed, simulate end-of-input
		    (stack-push! 0 #f) ;restore start stacks state
		    (make-<lexical-token> '*eoi* (eof-object)))
		(let* ((entry (state-entry-with-error-action (current-state))))
		  (if entry
		      (synchronise-lexer/skip-tokens (cdr entry))
		    (begin
		      (stack-pop!)
		      (synchronise-parser/rewind-stack))))))

	    (define-inline (state-entry-with-error-action state-index)
	      (assq 'error (vector-ref action-table state-index)))

	    (define (synchronise-lexer/skip-tokens error-state-index)
	      (let* ((error-actions	   (vector-ref action-table error-state-index))
		     (error-categories (map car (cdr error-actions))))
		(let skip-token ((token lookahead))
		  (let ((category (<lexical-token>-category token)))
		    (cond ((eq? category '*eoi*) ;unexpected end-of-input while trying to recover
			   token)
			  ((memq category error-categories) ;recovery success
			   ;;The following  stack entries will  be processed
			   ;;by  REDUCE-USING-DEFAULT-ACTIONS,  causing  the
			   ;;evaluation  of  the  semantic  action  for  the
			   ;;"error" right-hand  side rule.
			   ;;
			   ;;We want  $1 set  to "error" and  $2 set  to the
			   ;;recovery synchronisation token value.
			   (stack-push! #f 'error)
			   (stack-push! (cdr (assq category error-actions))
					(<lexical-token>-value token))
			   (lexer))
			  (else
			   (skip-token (lexer))))))))

	    (%main))

	  (define-inline (current-state)
	    (car stack-states))

	  (define-inline (stack-push! state value)
	    (set! stack-states (cons state stack-states))
	    (set! stack-values (cons value stack-values)))

	  (define-inline (stack-pop!)
	    (set! stack-states (cdr stack-states))
	    (set! stack-values (cdr stack-values)))

	  (main (lexer))))

      (case-lambda
       ((true-lexer error-handler)
	(parser-instance true-lexer error-handler #f))
       ((true-lexer error-handler yycustom)
	(parser-instance true-lexer error-handler yycustom))))


;;; This function is taken from (nausicaa language infix sexp-parser).
    (define (make-infix-sexp-parser)
      (lr-driver
       '#(((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -32))
	  ((*default* . -29) (LPAREN . 18))
	  ((*default* . *error*) (*eoi* . 41)
           (QUESTION-ID . 40) (AND . 39) (IOR . 38)
           (XOR . 37) (LT . 36) (GT . 35) (LE . 34)
           (GE . 33) (EQ . 32) (ADD . 31) (SUB . 30)
           (MUL . 29) (DIV . 28) (MOD . 27) (EXPT . 26)
           (INCR . 25) (DECR . 24) (BIT-AND . 23)
           (BIT-IOR . 22) (BIT-XOR . 21) (BIT-SHL . 20)
           (BIT-SHR . 19))
	  ((*default* . -26) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -16) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -15) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -14) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -13) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -22) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . *error*) (QUESTION-ID . 40)
           (RPAREN . 42) (AND . 39) (IOR . 38) (XOR . 37)
           (LT . 36) (GT . 35) (LE . 34) (GE . 33) (EQ . 32)
           (ADD . 31) (SUB . 30) (MUL . 29) (DIV . 28)
           (MOD . 27) (EXPT . 26) (INCR . 25) (DECR . 24)
           (BIT-AND . 23) (BIT-IOR . 22) (BIT-XOR . 21)
           (BIT-SHL . 20) (BIT-SHR . 19))
	  ((*default* . -34) (BIT-NOT . 1) (DECR . 2)
           (INCR . 3) (SUB . 4) (ADD . 5) (NOT . 6)
           (LPAREN . 7) (NUM . 8) (ID . 9))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -18)) ((*default* . -17))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -1) (*eoi* . accept))
	  ((*default* . -33))
	  ((*default* . *error*) (RPAREN . 65))
	  ((*default* . -37) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-NOT . 1) (BIT-XOR . 21) (BIT-IOR . 22)
           (BIT-AND . 23) (DECR . 66) (INCR . 67)
           (EXPT . 26) (MOD . 27) (DIV . 28) (MUL . 29)
           (SUB . 68) (ADD . 69) (EQ . 32) (GE . 33)
           (LE . 34) (GT . 35) (LT . 36) (NOT . 6)
           (XOR . 37) (IOR . 38) (AND . 39) (LPAREN . 7)
           (NUM . 8) (QUESTION-ID . 40) (ID . 9))
	  ((*default* . -28)) ((*default* . -27))
	  ((*default* . -25) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -24) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -23) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -7) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26))
	  ((*default* . -6) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26))
	  ((*default* . -4) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27))
	  ((*default* . -5) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27))
	  ((*default* . -3) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -2) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -12) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -11) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -10) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -9) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -8) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -21) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . -20) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . -19) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . *error*) (QUESTION-ID . 40)
           (COLON-ID . 72) (AND . 39) (IOR . 38) (XOR . 37)
           (LT . 36) (GT . 35) (LE . 34) (GE . 33) (EQ . 32)
           (ADD . 31) (SUB . 30) (MUL . 29) (DIV . 28)
           (MOD . 27) (EXPT . 26) (INCR . 25) (DECR . 24)
           (BIT-AND . 23) (BIT-IOR . 22) (BIT-XOR . 21)
           (BIT-SHL . 20) (BIT-SHR . 19))
	  ((*default* . -30))
	  ((*default* . -18) (BIT-NOT . 1) (DECR . 2)
           (INCR . 3))
	  ((*default* . -17) (BIT-NOT . 1) (DECR . 2)
           (INCR . 3))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -35))
	  ((*default* . -37) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-NOT . 1) (BIT-XOR . 21) (BIT-IOR . 22)
           (BIT-AND . 23) (DECR . 66) (INCR . 67)
           (EXPT . 26) (MOD . 27) (DIV . 28) (MUL . 29)
           (SUB . 68) (ADD . 69) (EQ . 32) (GE . 33)
           (LE . 34) (GT . 35) (LT . 36) (NOT . 6)
           (XOR . 37) (IOR . 38) (AND . 39) (LPAREN . 7)
           (NUM . 8) (QUESTION-ID . 40) (ID . 9))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -3) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -2) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29)) ((*default* . -36))
	  ((*default* . -31) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36)
           (XOR . 37) (IOR . 38) (AND . 39)
           (QUESTION-ID . 40)))
       (vector '((1 . 10)) '((1 . 11)) '((1 . 12))
	       '((1 . 13)) '((1 . 14)) '((1 . 15)) '((1 . 16))
	       '((1 . 17)) '() '() '() '() '() '() '() '() '() '()
	       '((2 . 43) (1 . 44)) '((1 . 45)) '((1 . 46))
	       '((1 . 47)) '((1 . 48)) '((1 . 49)) '() '()
	       '((1 . 50)) '((1 . 51)) '((1 . 52)) '((1 . 53))
	       '((1 . 54)) '((1 . 55)) '((1 . 56)) '((1 . 57))
	       '((1 . 58)) '((1 . 59)) '((1 . 60)) '((1 . 61))
	       '((1 . 62)) '((1 . 63)) '((1 . 64)) '() '() '()
	       '((3 . 70) (1 . 71)) '() '() '() '() '() '() '() '()
	       '() '() '() '() '() '() '() '() '() '() '() '() '()
	       '((1 . 12)) '((1 . 13)) '((1 . 73)) '((1 . 74)) '()
	       '((3 . 75) (1 . 71)) '((1 . 76)) '() '() '() '())
       (vector '()
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 $1)
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 $2 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (car $1) $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (car $1) $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (cdr $2) $1)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (cdr $2) $1)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $4 $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 4 1 (cons $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 5 1 (list $2 $1 $3 $5)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 $2 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states . yy-stack-values)
		 (yy-reduce-pop-and-push 0 2 '() yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 2 (cons $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 3 (cons $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states . yy-stack-values)
		 (yy-reduce-pop-and-push 0 3 '() yy-stack-states
					 yy-stack-values)))))


    (lambda (stx)
      (syntax-case stx ()
	((?infix ?operand ...)
	 (let ((stx-lst (syntax->list #'(?operand ...))))
	   (cond ((null? stx-lst)
		  #'(values))
		 ((not (pair? stx-lst))
		  stx-lst)
		 (else
		  (let ((tokens (reverse (stx-list->reversed-tokens stx-lst '()))))
		    ((make-infix-sexp-parser) ;parser function
		     (lambda ()		      ;lexer function
		       (if (null? tokens)
			   $eoi
			 (let ((t (car tokens)))
			   (set! tokens (cdr tokens))
			   t)))
		     (lambda (message token) ;error handler
		       (syntax-violation 'infix
			 (string-append "processing infix expression: " message)
			 (syntax->datum #'(?infix ?operand ...)) token))
		     #f))))))))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'case-stx 'scheme-indent-function 1)
;; End:
