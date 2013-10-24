;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: show LALR parsers error
;;;Date: Thu Oct 24, 2013
;;;
;;;Abstract
;;;
;;;	Show error  when using the  "prec:" qualifier in  LALR generated
;;;	parsers.
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (prefix (nausicaa parser-tools lalr) lalr.)
  (prefix (nausicaa parser-tools lexical-tokens) lt.)
  (prefix (nausicaa parser-tools source-locations) sl.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 58: error while using prec: qualifier in LALR parsers non-terminals\n")


;;;; helpers

(define-constant EOI-TOKEN
  (lt.<end-of-input> ()))

(define (make-lexer list-of-tokens)
  ;;Return a lexer closure drawing  tokens from the list LIST-OF-TOKENS.
  ;;When the list is empty, return the EOI-TOKEN.
  ;;
  (lambda ()
    (if (null? list-of-tokens)
	EOI-TOKEN
      (begin0
	  (car list-of-tokens)
	(set! list-of-tokens (cdr list-of-tokens))))))

(define (make-token category value)
  (lt.<lexical-token> ((lt.category: category)
		       (lt.value:    value)
		       (lt.length:   0))))

(define make-error-handler
  ;;Return  an error  handler closure  that calls  YYCUSTOM with  a pair
  ;;describing the offending token.  To just return the pair invoke as:
  ;;
  ;;	(make-error-handler (lambda x x))
  ;;
  (case-lambda
   (()
    (make-error-handler (lambda x x)))
   ((yycustom)
    (lambda ((message <string>) (token lt.<lexical-token>))
      (yycustom `(error-handler . ,(token value)))))))

(define (doit . tokens)
  (let* ((lexer		(make-lexer tokens))
	 (error-handler	(make-error-handler))
	 (parser	(make-parser)))
    (parser lexer error-handler)))


;;;; grammar and tests

(define make-parser
  (lalr.lalr-parser
   (lalr.output-value: #t)
   (lalr.expect: 0)
   ;;Output to a file the human  readable LALR table.  This file goes in
   ;;the directory "$(builddir)/test".
   #;(lalr.dump-table: "issue-058-parser-tables.txt")

   (lalr.terminals: '(NUM		 ;precedence 0
		      (left: ADD SUB)    ;precedence 1
		      (left: MUL DIV)    ;precedence 2
		      (nonassoc: UADD)   ;precedence 3
		      (nonassoc: USUB))) ;precedence 4
   (lalr.rules:
    '((EXPR

       ;;These are the offending rules.
       (ADD EXPR (prec: UADD))	: (list $1 $2)
       (SUB EXPR (prec: USUB))	: (list $1 $2)

       (EXPR ADD EXPR)		: (list $2 $1 $3)
       (EXPR SUB EXPR)		: (list $2 $1 $3)
       (EXPR MUL EXPR)		: (list $2 $1 $3)
       (EXPR DIV EXPR)		: (list $2 $1 $3)

       (NUM)			: $1)))))

;;When using the rules, in this order:
;;
;;   (ADD EXPR (prec: UADD))	: (list $1 $2)
;;   (SUB EXPR (prec: USUB))	: (list $1 $2)
;;
;;unary ADD operator looks fine, while  unary SUB operator has the wrong
;;precedence when used as subexpression of MUL and DIV.
;;
;;When using the rules, in this order (reverse of the one above):
;;
;;   (SUB EXPR (prec: USUB))	: (list $1 $2)
;;   (ADD EXPR (prec: UADD))	: (list $1 $2)
;;
;;unary SUB operator looks fine, while  unary ADD operator has the wrong
;;precedence when used as subexpression of MUL and DIV.
;;

(define-constant ADD	(make-token 'ADD '+))
(define-constant SUB	(make-token 'SUB '-))
(define-constant MUL	(make-token 'MUL '*))
(define-constant DIV	(make-token 'DIV '/))
(define-constant ONE	(make-token 'NUM 1))
(define-constant TWO	(make-token 'NUM 2))

(check (doit ADD ONE)		=> '(+ 1))
(check (doit SUB ONE)		=> '(- 1))

(check (doit ADD ADD ONE)	=> '(+ (+ 1)))
(check (doit SUB SUB ONE)	=> '(- (- 1)))
(check (doit ADD SUB ONE)	=> '(+ (- 1)))
(check (doit SUB ADD ONE)	=> '(- (+ 1)))

(check (doit ADD ONE ADD TWO)	=> '(+ (+ 1) 2))
(check (doit ADD ONE SUB TWO)	=> '(- (+ 1) 2))

(check (doit SUB ONE ADD TWO)	=> '(+ (- 1) 2))
(check (doit SUB ONE SUB TWO)	=> '(- (- 1) 2))

;;; the following will fail

(check (doit ADD ONE MUL TWO)	=> '(* (+ 1) 2))
(check (doit ADD ONE DIV TWO)	=> '(/ (+ 1) 2))

(check (doit SUB ONE MUL TWO)	=> '(* (- 1) 2))
(check (doit SUB ONE DIV TWO)	=> '(/ (- 1) 2))


;;;; done

(check-report)

;;; end of file
