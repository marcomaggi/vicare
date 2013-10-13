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
(library (vicare parser-tools silex input-system)
  (export
    make-IS
    (rename (<input-system>?			lexer-input-system?)
	    (<input-system>-user-getc		lexer-get-func-getc)
	    (<input-system>-user-ungetc		lexer-get-func-ungetc)
	    (<input-system>-get-user-line	lexer-get-func-line)
	    (<input-system>-get-user-column	lexer-get-func-column)
	    (<input-system>-get-user-offset	lexer-get-func-offset))

    ;;Accessors needed in the output tables with the "code" format.
    <input-system>-start-go-to-end		$<input-system>-start-go-to-end
    <input-system>-end-go-to-point		$<input-system>-end-go-to-point
    <input-system>-init-lexeme			$<input-system>-init-lexeme
    <input-system>-get-start-line		$<input-system>-get-start-line
    <input-system>-get-start-column		$<input-system>-get-start-column
    <input-system>-get-start-offset		$<input-system>-get-start-offset
    <input-system>-peek-left-context		$<input-system>-peek-left-context
    <input-system>-peek-char			$<input-system>-peek-char
    <input-system>-read-char			$<input-system>-read-char
    <input-system>-get-start-end-text		$<input-system>-get-start-end-text
    <input-system>-get-user-line		$<input-system>-get-user-line
    <input-system>-get-user-column		$<input-system>-get-user-column
    <input-system>-get-user-offset		$<input-system>-get-user-offset
    <input-system>-user-getc			$<input-system>-user-getc
    <input-system>-user-ungetc			$<input-system>-user-ungetc

    ;; auxiliary syntaxes
    counters:		port:
    procedure:		string:)
  (import (vicare)
    (vicare language-extensions case-identifiers)
    (vicare language-extensions let-constants)
    (prefix (vicare language-extensions makers) mk.))


(define-record-type <input-system>
  (nongenerative vicare:parser-tools:silex:input-system:<input-system>)
  (fields (immutable start-go-to-end)
	  (immutable end-go-to-point)
	  (immutable init-lexeme)
	  (immutable get-start-line)
	  (immutable get-start-column)
	  (immutable get-start-offset)
	  (immutable peek-left-context)
	  (immutable peek-char)
	  (immutable read-char)
	  (immutable get-start-end-text)
	  (immutable get-user-line)
	  (immutable get-user-column)
	  (immutable get-user-offset)
	  (immutable user-getc)
	  (immutable user-ungetc)))


(define-auxiliary-syntaxes
  counters:
  port:
  procedure:
  string:)

(mk.define-maker make-IS
    %make-IS
  ((counters:	'all)
   (port:	#f	(mk.without procedure: string:))
   (procedure:	#f	(mk.without port: string:))
   (string:	#f	(mk.without port: procedure:))))

(define lexer-init-buffer-len 1024)

(define (%make-IS counters-type input-port input-procedure input-string)
  (define who 'make-IS)
  (let-values (((buffer read-ptr input-function)
		(cond ((and input-string (string? input-string))
		       (values (string-append (string #\newline) input-string)
			       (+ 1 (string-length input-string))
			       (lambda () (eof-object))))
		      ((and input-port (input-port? input-port))
		       (values (make-string lexer-init-buffer-len #\newline)
			       1
			       (lambda () (read-char input-port))))
		      ((and input-procedure (procedure? input-procedure))
		       (values (make-string lexer-init-buffer-len #\newline)
			       1
			       input-procedure))
		      (else
		       (assertion-violation who "input source was not specified")))))
    (lexer-raw-IS-maker buffer read-ptr input-function
			(if (memq counters-type '(none line all))
			    counters-type
			  (assertion-violation who "invalid selection of counters type" counters-type)))))


(define (lexer-raw-IS-maker buffer read-ptr input-f counters)
  (let ((input-f          input-f) ; Entree reelle
	(buffer           buffer)
	(buflen           (string-length buffer))
	(read-ptr         read-ptr)
	(start-ptr        1) ; Marque de debut de lexeme
	(start-line       1)
	(start-column     1)
	(start-offset     0)
	(end-ptr          1) ; Marque de fin de lexeme
	(point-ptr        1) ; Le point
	(user-ptr         1) ; Marque de l'usager
	(user-line        1)
	(user-column      1)
	(user-offset      0)
	(user-up-to-date? #t)) ; Concerne la colonne seul.
    (letrec
	((start-go-to-end-none ; Fonctions de depl. des marques
	  (lambda ()
	    (set! start-ptr end-ptr)))
	 (start-go-to-end-line
	  (lambda ()
	    (let loop ((ptr start-ptr) (line start-line))
	      (if (= ptr end-ptr)
		  (begin
		    (set! start-ptr ptr)
		    (set! start-line line))
		(if (char=? (string-ref buffer ptr) #\newline)
		    (loop (+ ptr 1) (+ line 1))
		  (loop (+ ptr 1) line))))))
	 (start-go-to-end-all
	  (lambda ()
	    (set! start-offset (+ start-offset (- end-ptr start-ptr)))
	    (let loop ((ptr start-ptr)
		       (line start-line)
		       (column start-column))
	      (if (= ptr end-ptr)
		  (begin
		    (set! start-ptr ptr)
		    (set! start-line line)
		    (set! start-column column))
		(if (char=? (string-ref buffer ptr) #\newline)
		    (loop (+ ptr 1) (+ line 1) 1)
		  (loop (+ ptr 1) line (+ column 1)))))))
	 (start-go-to-user-none
	  (lambda ()
	    (set! start-ptr user-ptr)))
	 (start-go-to-user-line
	  (lambda ()
	    (set! start-ptr user-ptr)
	    (set! start-line user-line)))
	 (start-go-to-user-all
	  (lambda ()
	    (set! start-line user-line)
	    (set! start-offset user-offset)
	    (if user-up-to-date?
		(begin
		  (set! start-ptr user-ptr)
		  (set! start-column user-column))
	      (let loop ((ptr start-ptr) (column start-column))
		(if (= ptr user-ptr)
		    (begin
		      (set! start-ptr ptr)
		      (set! start-column column))
		  (if (char=? (string-ref buffer ptr) #\newline)
		      (loop (+ ptr 1) 1)
		    (loop (+ ptr 1) (+ column 1))))))))
	 (end-go-to-point
	  (lambda ()
	    (set! end-ptr point-ptr)))
	 (point-go-to-start
	  (lambda ()
	    (set! point-ptr start-ptr)))
	 (user-go-to-start-none
	  (lambda ()
	    (set! user-ptr start-ptr)))
	 (user-go-to-start-line
	  (lambda ()
	    (set! user-ptr start-ptr)
	    (set! user-line start-line)))
	 (user-go-to-start-all
	  (lambda ()
	    (set! user-ptr start-ptr)
	    (set! user-line start-line)
	    (set! user-column start-column)
	    (set! user-offset start-offset)
	    (set! user-up-to-date? #t)))
	 (init-lexeme-none ; Debute un nouveau lexeme
	  (lambda ()
	    (if (< start-ptr user-ptr)
		(start-go-to-user-none))
	    (point-go-to-start)))
	 (init-lexeme-line
	  (lambda ()
	    (if (< start-ptr user-ptr)
		(start-go-to-user-line))
	    (point-go-to-start)))
	 (init-lexeme-all
	  (lambda ()
	    (if (< start-ptr user-ptr)
		(start-go-to-user-all))
	    (point-go-to-start)))
	 (get-start-line ; Obtention des stats du debut du lxm
	  (lambda ()
	    start-line))
	 (get-start-column
	  (lambda ()
	    start-column))
	 (get-start-offset
	  (lambda ()
	    start-offset))
	 (peek-left-context ; Obtention de caracteres (#f si EOF)
	  (lambda ()
	    (char->integer (string-ref buffer (- start-ptr 1)))))
	 (peek-char
	  (lambda ()
	    (if (< point-ptr read-ptr)
		(char->integer (string-ref buffer point-ptr))
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer point-ptr c)
		      (set! read-ptr (+ point-ptr 1))
		      (char->integer c))
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    #f))))))
	 (read-char
	  (lambda ()
	    (if (< point-ptr read-ptr)
		(let ((c (string-ref buffer point-ptr)))
		  (set! point-ptr (+ point-ptr 1))
		  (char->integer c))
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer point-ptr c)
		      (set! read-ptr (+ point-ptr 1))
		      (set! point-ptr read-ptr)
		      (char->integer c))
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    #f))))))
	 (get-start-end-text ; Obtention du lexeme
	  (lambda ()
	    (substring buffer start-ptr end-ptr)))
	 (get-user-line-line ; Fonctions pour l'usager
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-line))
	    user-line))
	 (get-user-line-all
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-all))
	    user-line))
	 (get-user-column-all
	  (lambda ()
	    (cond ((< user-ptr start-ptr)
		   (user-go-to-start-all)
		   user-column)
		  (user-up-to-date?
		   user-column)
		  (else
		   (let loop ((ptr start-ptr) (column start-column))
		     (if (= ptr user-ptr)
			 (begin
			   (set! user-column column)
			   (set! user-up-to-date? #t)
			   column)
		       (if (char=? (string-ref buffer ptr) #\newline)
			   (loop (+ ptr 1) 1)
			 (loop (+ ptr 1) (+ column 1)))))))))
	 (get-user-offset-all
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-all))
	    user-offset))
	 (user-getc-none
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-none))
	    (if (< user-ptr read-ptr)
		(let ((c (string-ref buffer user-ptr)))
		  (set! user-ptr (+ user-ptr 1))
		  c)
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer user-ptr c)
		      (set! read-ptr (+ read-ptr 1))
		      (set! user-ptr read-ptr)
		      c)
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    (eof-object)))))))
	 (user-getc-line
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-line))
	    (if (< user-ptr read-ptr)
		(let ((c (string-ref buffer user-ptr)))
		  (set! user-ptr (+ user-ptr 1))
		  (if (char=? c #\newline)
		      (set! user-line (+ user-line 1)))
		  c)
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer user-ptr c)
		      (set! read-ptr (+ read-ptr 1))
		      (set! user-ptr read-ptr)
		      (if (char=? c #\newline)
			  (set! user-line (+ user-line 1)))
		      c)
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    (eof-object)))))))
	 (user-getc-all
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-all))
	    (if (< user-ptr read-ptr)
		(let ((c (string-ref buffer user-ptr)))
		  (set! user-ptr (+ user-ptr 1))
		  (if (char=? c #\newline)
		      (begin
			(set! user-line (+ user-line 1))
			(set! user-column 1))
		    (set! user-column (+ user-column 1)))
		  (set! user-offset (+ user-offset 1))
		  c)
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer user-ptr c)
		      (set! read-ptr (+ read-ptr 1))
		      (set! user-ptr read-ptr)
		      (if (char=? c #\newline)
			  (begin
			    (set! user-line (+ user-line 1))
			    (set! user-column 1))
			(set! user-column (+ user-column 1)))
		      (set! user-offset (+ user-offset 1))
		      c)
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    (eof-object)))))))
	 (user-ungetc-none
	  (lambda ()
	    (when (> user-ptr start-ptr)
	      (set! user-ptr (- user-ptr 1)))))
	 (user-ungetc-line
	  (lambda ()
	    (when (> user-ptr start-ptr)
	      (set! user-ptr (- user-ptr 1))
	      (let ((c (string-ref buffer user-ptr)))
		(if (char=? c #\newline)
		    (set! user-line (- user-line 1)))))))
	 (user-ungetc-all
	  (lambda ()
	    (when (> user-ptr start-ptr)
	      (set! user-ptr (- user-ptr 1))
	      (let ((c (string-ref buffer user-ptr)))
		(if (char=? c #\newline)
		    (begin
		      (set! user-line (- user-line 1))
		      (set! user-up-to-date? #f))
		  (set! user-column (- user-column 1)))
		(set! user-offset (- user-offset 1))))))
	 (reorganize-buffer ; Decaler ou agrandir le buffer
	  (lambda ()
	    (if (< (* 2 start-ptr) buflen)
		(let* ((newlen (* 2 buflen))
		       (newbuf (make-string newlen))
		       (delta (- start-ptr 1)))
		  (let loop ((from (- start-ptr 1)))
		    (if (< from buflen)
			(begin
			  (string-set! newbuf
				       (- from delta)
				       (string-ref buffer from))
			  (loop (+ from 1)))))
		  (set! buffer    newbuf)
		  (set! buflen    newlen)
		  (set! read-ptr  (- read-ptr delta))
		  (set! start-ptr (- start-ptr delta))
		  (set! end-ptr   (- end-ptr delta))
		  (set! point-ptr (- point-ptr delta))
		  (set! user-ptr  (- user-ptr delta)))
	      (let ((delta (- start-ptr 1)))
		(let loop ((from (- start-ptr 1)))
		  (if (< from buflen)
		      (begin
			(string-set! buffer
				     (- from delta)
				     (string-ref buffer from))
			(loop (+ from 1)))))
		(set! read-ptr  (- read-ptr delta))
		(set! start-ptr (- start-ptr delta))
		(set! end-ptr   (- end-ptr delta))
		(set! point-ptr (- point-ptr delta))
		(set! user-ptr  (- user-ptr delta)))))))
      (make-<input-system>
       (case counters
	 ((none) start-go-to-end-none)
	 ((line) start-go-to-end-line)
	 ((all)  start-go-to-end-all))
       end-go-to-point
       (case counters
	 ((none) init-lexeme-none)
	 ((line) init-lexeme-line)
	 ((all)  init-lexeme-all))
       get-start-line
       get-start-column
       get-start-offset
       peek-left-context
       peek-char
       read-char
       get-start-end-text
       (case counters
	 ((none) #f)
	 ((line) get-user-line-line)
	 ((all)  get-user-line-all))
       (case counters
	 ((none) #f)
	 ((line) #f)
	 ((all)  get-user-column-all))
       (case counters
	 ((none) #f)
	 ((line) #f)
	 ((all)  get-user-offset-all))
       (case counters
	 ((none) user-getc-none)
	 ((line) user-getc-line)
	 ((all)  user-getc-all))
       (case counters
	 ((none) user-ungetc-none)
	 ((line) user-ungetc-line)
	 ((all)  user-ungetc-all))))))


;;;; done

)

;;; end of file
