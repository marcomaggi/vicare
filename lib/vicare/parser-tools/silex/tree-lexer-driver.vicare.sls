;;; -*- coding: utf-8-unix -*-
;;;
;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (C) 2001 Danny Dube'
;;;Port to R6RS and Vicare integration by Marco Maggi
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


#!r6rs
(library (vicare parser-tools silex tree-lexer-driver)
  (export make-tree-lexer)
  (import (rnrs)
    (vicare parser-tools silex input-system)
    (vicare unsafe operations))


(define lexer-integer-newline (char->integer #\newline))

(define (make-tree-lexer tables IS)
  (assert (vector? tables))
  (assert (<= 9 ($vector-length tables)))
  (assert (lexer-input-system? IS))
  ;;Fabrication de lexer a partir d'arbres de decision.
  ;;
  (letrec
      (		; Contenu de la table
       (counters-type        ($vector-ref tables 0))
       (<<EOF>>-pre-action   ($vector-ref tables 1))
       (<<ERROR>>-pre-action ($vector-ref tables 2))
       (rules-pre-actions    ($vector-ref tables 3))
       (table-nl-start       ($vector-ref tables 5))
       (table-no-nl-start    ($vector-ref tables 6))
       (trees-v              ($vector-ref tables 7))
       (acc-v                ($vector-ref tables 8))

		; Contenu du IS
       (IS-start-go-to-end    ($<input-system>-start-go-to-end		IS))
       (IS-end-go-to-point    ($<input-system>-end-go-to-point		IS))
       (IS-init-lexeme        ($<input-system>-init-lexeme		IS))
       (IS-get-start-line     ($<input-system>-get-start-line		IS))
       (IS-get-start-column   ($<input-system>-get-start-column		IS))
       (IS-get-start-offset   ($<input-system>-get-start-offset		IS))
       (IS-peek-left-context  ($<input-system>-peek-left-context	IS))
       (IS-peek-char          ($<input-system>-peek-char		IS))
       (IS-read-char          ($<input-system>-read-char		IS))
       (IS-get-start-end-text ($<input-system>-get-start-end-text	IS))
       (IS-get-user-line      ($<input-system>-get-user-line		IS))
       (IS-get-user-column    ($<input-system>-get-user-column		IS))
       (IS-get-user-offset    ($<input-system>-get-user-offset		IS))
       (IS-user-getc          ($<input-system>-user-getc		IS))
       (IS-user-ungetc        ($<input-system>-user-ungetc		IS))

		; Resultats
       (<<EOF>>-action   #f)
       (<<ERROR>>-action #f)
       (rules-actions    #f)
       (states           #f)
       (final-lexer      #f)

		; Gestion des hooks
       (hook-list '())
       (add-hook
	(lambda (thunk)
	  (set! hook-list (cons thunk hook-list))))
       (apply-hooks
	(lambda ()
	  (let loop ((l hook-list))
	    (if (pair? l)
		(begin
		  ((car l))
		  (loop (cdr l)))))))

		; Preparation des actions
       (set-action-statics
	(lambda (pre-action)
	  (pre-action final-lexer IS-user-getc IS-user-ungetc)))
       (prepare-special-action-none
	(lambda (pre-action)
	  (let ((action #f))
	    (let ((result
		   (lambda ()
		     (action "")))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-special-action-line
	(lambda (pre-action)
	  (let ((action #f))
	    (let ((result
		   (lambda (yyline)
		     (action "" yyline)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-special-action-all
	(lambda (pre-action)
	  (let ((action #f))
	    (let ((result
		   (lambda (yyline yycolumn yyoffset)
		     (action "" yyline yycolumn yyoffset)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-special-action
	(lambda (pre-action)
	  (case counters-type
	    ((none) (prepare-special-action-none pre-action))
	    ((line) (prepare-special-action-line pre-action))
	    ((all)  (prepare-special-action-all  pre-action)))))
       (prepare-action-yytext-none
	(lambda (pre-action)
	  (let ((get-start-end-text IS-get-start-end-text)
		(start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda ()
		     (let ((yytext (get-start-end-text)))
		       (start-go-to-end)
		       (action yytext))))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-yytext-line
	(lambda (pre-action)
	  (let ((get-start-end-text IS-get-start-end-text)
		(start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline)
		     (let ((yytext (get-start-end-text)))
		       (start-go-to-end)
		       (action yytext yyline))))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-yytext-all
	(lambda (pre-action)
	  (let ((get-start-end-text IS-get-start-end-text)
		(start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline yycolumn yyoffset)
		     (let ((yytext (get-start-end-text)))
		       (start-go-to-end)
		       (action yytext yyline yycolumn yyoffset))))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-yytext
	(lambda (pre-action)
	  (case counters-type
	    ((none) (prepare-action-yytext-none pre-action))
	    ((line) (prepare-action-yytext-line pre-action))
	    ((all)  (prepare-action-yytext-all  pre-action)))))
       (prepare-action-no-yytext-none
	(lambda (pre-action)
	  (let ((start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda ()
		     (start-go-to-end)
		     (action)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-no-yytext-line
	(lambda (pre-action)
	  (let ((start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline)
		     (start-go-to-end)
		     (action yyline)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-no-yytext-all
	(lambda (pre-action)
	  (let ((start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline yycolumn yyoffset)
		     (start-go-to-end)
		     (action yyline yycolumn yyoffset)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-no-yytext
	(lambda (pre-action)
	  (case counters-type
	    ((none) (prepare-action-no-yytext-none pre-action))
	    ((line) (prepare-action-no-yytext-line pre-action))
	    ((all)  (prepare-action-no-yytext-all  pre-action)))))

		; Fabrique les fonctions de dispatch
       (prepare-dispatch-err
	(lambda (leaf)
	  (lambda (c)
	    #f)))
       (prepare-dispatch-number
	(lambda (leaf)
	  (let ((state-function #f))
	    (let ((result
		   (lambda (c)
		     state-function))
		  (hook
		   (lambda ()
		     (set! state-function ($vector-ref states leaf)))))
	      (add-hook hook)
	      result))))
       (prepare-dispatch-leaf
	(lambda (leaf)
	  (if (eq? leaf 'err)
	      (prepare-dispatch-err leaf)
	    (prepare-dispatch-number leaf))))
       (prepare-dispatch-<
	(lambda (tree)
	  (let ((left-tree  (list-ref tree 1))
		(right-tree (list-ref tree 2)))
	    (let ((bound      (list-ref tree 0))
		  (left-func  (prepare-dispatch-tree left-tree))
		  (right-func (prepare-dispatch-tree right-tree)))
	      (lambda (c)
		(if ($fx< c bound)
		    (left-func c)
		  (right-func c)))))))
       (prepare-dispatch-=
	(lambda (tree)
	  (let ((left-tree  (list-ref tree 2))
		(right-tree (list-ref tree 3)))
	    (let ((bound      (list-ref tree 1))
		  (left-func  (prepare-dispatch-tree left-tree))
		  (right-func (prepare-dispatch-tree right-tree)))
	      (lambda (c)
		(if ($fx= c bound)
		    (left-func c)
		  (right-func c)))))))
       (prepare-dispatch-tree
	(lambda (tree)
	  (cond ((not (pair? tree))
		 (prepare-dispatch-leaf tree))
		((eq? (car tree) '=)
		 (prepare-dispatch-= tree))
		(else
		 (prepare-dispatch-< tree)))))
       (prepare-dispatch
	(lambda (tree)
	  (let ((dicho-func (prepare-dispatch-tree tree)))
	    (lambda (c)
	      (and c (dicho-func c))))))

		; Fabrique les fonctions de transition (read & go) et (abort)
       (prepare-read-n-go
	(lambda (tree)
	  (let ((dispatch-func (prepare-dispatch tree))
		(read-char     IS-read-char))
	    (lambda ()
	      (dispatch-func (read-char))))))
       (prepare-abort
	(lambda (tree)
	  (lambda ()
	    #f)))
       (prepare-transition
	(lambda (tree)
	  (if (eq? tree 'err)
	      (prepare-abort     tree)
	    (prepare-read-n-go tree))))

		; Fabrique les fonctions d'etats ([set-end] & trans)
       (prepare-state-no-acc
	(lambda (s r1 r2)
	  (let ((trans-func (prepare-transition ($vector-ref trees-v s))))
	    (lambda (action)
	      (let ((next-state (trans-func)))
		(if next-state
		    (next-state action)
		  action))))))
       (prepare-state-yes-no
	(lambda (s r1 r2)
	  (let ((peek-char       IS-peek-char)
		(end-go-to-point IS-end-go-to-point)
		(new-action1     #f)
		(trans-func (prepare-transition ($vector-ref trees-v s))))
	    (let ((result
		   (lambda (action)
		     (let* ((c (peek-char))
			    (new-action
			     (if (or (not c) ($fx= c lexer-integer-newline))
				 (begin
				   (end-go-to-point)
				   new-action1)
			       action))
			    (next-state (trans-func)))
		       (if next-state
			   (next-state new-action)
			 new-action))))
		  (hook
		   (lambda ()
		     (set! new-action1 ($vector-ref rules-actions r1)))))
	      (add-hook hook)
	      result))))
       (prepare-state-diff-acc
	(lambda (s r1 r2)
	  (let ((end-go-to-point IS-end-go-to-point)
		(peek-char       IS-peek-char)
		(new-action1     #f)
		(new-action2     #f)
		(trans-func (prepare-transition ($vector-ref trees-v s))))
	    (let ((result
		   (lambda (action)
		     (end-go-to-point)
		     (let* ((c (peek-char))
			    (new-action
			     (if (or (not c) ($fx= c lexer-integer-newline))
				 new-action1
			       new-action2))
			    (next-state (trans-func)))
		       (if next-state
			   (next-state new-action)
			 new-action))))
		  (hook
		   (lambda ()
		     (set! new-action1 ($vector-ref rules-actions r1))
		     (set! new-action2 ($vector-ref rules-actions r2)))))
	      (add-hook hook)
	      result))))
       (prepare-state-same-acc
	(lambda (s r1 r2)
	  (let ((end-go-to-point IS-end-go-to-point)
		(trans-func (prepare-transition ($vector-ref trees-v s)))
		(new-action #f))
	    (let ((result
		   (lambda (action)
		     (end-go-to-point)
		     (let ((next-state (trans-func)))
		       (if next-state
			   (next-state new-action)
			 new-action))))
		  (hook
		   (lambda ()
		     (set! new-action ($vector-ref rules-actions r1)))))
	      (add-hook hook)
	      result))))
       (prepare-state
	(lambda (s)
	  (let* ((acc ($vector-ref acc-v s))
		 (r1 (car acc))
		 (r2 (cdr acc)))
	    (cond ((not r1)  (prepare-state-no-acc   s r1 r2))
		  ((not r2)  (prepare-state-yes-no   s r1 r2))
		  ((< r1 r2) (prepare-state-diff-acc s r1 r2))
		  (else      (prepare-state-same-acc s r1 r2))))))

		; Fabrique la fonction de lancement du lexage a l'etat de depart
       (prepare-start-same
	(lambda (s1 s2)
	  (let ((peek-char    IS-peek-char)
		(eof-action   #f)
		(start-state  #f)
		(error-action #f))
	    (let ((result
		   (lambda ()
		     (if (not (peek-char))
			 eof-action
		       (start-state error-action))))
		  (hook
		   (lambda ()
		     (set! eof-action   <<EOF>>-action)
		     (set! start-state  ($vector-ref states s1))
		     (set! error-action <<ERROR>>-action))))
	      (add-hook hook)
	      result))))
       (prepare-start-diff
	(lambda (s1 s2)
	  (let ((peek-char         IS-peek-char)
		(eof-action        #f)
		(peek-left-context IS-peek-left-context)
		(start-state1      #f)
		(start-state2      #f)
		(error-action      #f))
	    (let ((result
		   (lambda ()
		     (cond ((not (peek-char))
			    eof-action)
			   ((= (peek-left-context) lexer-integer-newline)
			    (start-state1 error-action))
			   (else
			    (start-state2 error-action)))))
		  (hook
		   (lambda ()
		     (set! eof-action <<EOF>>-action)
		     (set! start-state1 ($vector-ref states s1))
		     (set! start-state2 ($vector-ref states s2))
		     (set! error-action <<ERROR>>-action))))
	      (add-hook hook)
	      result))))
       (prepare-start
	(lambda ()
	  (let ((s1 table-nl-start)
		(s2 table-no-nl-start))
	    (if (= s1 s2)
		(prepare-start-same s1 s2)
	      (prepare-start-diff s1 s2)))))

		; Fabrique la fonction principale
       (prepare-lexer-none
	(lambda ()
	  (let ((init-lexeme IS-init-lexeme)
		(start-func  (prepare-start)))
	    (lambda ()
	      (init-lexeme)
	      ((start-func))))))
       (prepare-lexer-line
	(lambda ()
	  (let ((init-lexeme    IS-init-lexeme)
		(get-start-line IS-get-start-line)
		(start-func     (prepare-start)))
	    (lambda ()
	      (init-lexeme)
	      (let ((yyline (get-start-line)))
		((start-func) yyline))))))
       (prepare-lexer-all
	(lambda ()
	  (let ((init-lexeme      IS-init-lexeme)
		(get-start-line   IS-get-start-line)
		(get-start-column IS-get-start-column)
		(get-start-offset IS-get-start-offset)
		(start-func       (prepare-start)))
	    (lambda ()
	      (init-lexeme)
	      (let ((yyline   (get-start-line))
		    (yycolumn (get-start-column))
		    (yyoffset (get-start-offset)))
		((start-func) yyline yycolumn yyoffset))))))
       (prepare-lexer
	(lambda ()
	  (case counters-type
	    ((none) (prepare-lexer-none))
	    ((line) (prepare-lexer-line))
	    ((all)  (prepare-lexer-all))))))

		; Calculer la valeur de <<EOF>>-action et de <<ERROR>>-action
    (set! <<EOF>>-action   (prepare-special-action <<EOF>>-pre-action))
    (set! <<ERROR>>-action (prepare-special-action <<ERROR>>-pre-action))

		; Calculer la valeur de rules-actions
    (let* ((len (div (vector-length rules-pre-actions) 2))
	   (v (make-vector len)))
      (let loop ((r (- len 1)))
	(if (< r 0)
	    (set! rules-actions v)
	  (let* ((yytext? ($vector-ref rules-pre-actions (* 2 r)))
		 (pre-action ($vector-ref rules-pre-actions (+ (* 2 r) 1)))
		 (action (if yytext?
			     (prepare-action-yytext    pre-action)
			   (prepare-action-no-yytext pre-action))))
	    ($vector-set! v r action)
	    (loop (- r 1))))))

		; Calculer la valeur de states
    (let* ((len (vector-length trees-v))
	   (v (make-vector len)))
      (let loop ((s (- len 1)))
	(if (< s 0)
	    (set! states v)
	  (begin
	    ($vector-set! v s (prepare-state s))
	    (loop (- s 1))))))

		; Calculer la valeur de final-lexer
    (set! final-lexer (prepare-lexer))

		; Executer les hooks
    (apply-hooks)

		; Resultat
    final-lexer))


;;;; done

)

;;; end of file
