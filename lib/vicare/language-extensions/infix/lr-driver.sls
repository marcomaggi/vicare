;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: LR driver
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
(library (vicare language-extensions infix lr-driver)
  (export lr-driver)
  (import (vicare)
    (vicare language-extensions infix tokens))


;;;; helpers

(define-syntax drop/stx
  (syntax-rules ()
    ((_ ?ell ?k)
     (let loop ((ell ?ell)
		(k   ?k))
       (if (zero? k)
	   ell
	 (loop (cdr ell) (- k 1)))))))


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


;;;; done

)

;;; end of file
