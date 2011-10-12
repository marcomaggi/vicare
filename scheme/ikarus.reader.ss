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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(library (ikarus.reader)
  (export read read-initial read-token comment-handler get-datum
          read-annotated read-script-annotated annotation?
          annotation-expression annotation-source
          annotation-stripped)
  (import (except (ikarus)
		  read-char read read-token comment-handler get-datum
		  read-annotated read-script-annotated annotation?
		  annotation-expression annotation-source annotation-stripped)
    (only (ikarus.string-to-number)
	  define-string->number-parser)
    (ikarus system $chars)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $bytevectors))


(define (make-compound-position port)
  (cons (port-id port) (input-port-byte-position port)))

(define (make-compound-position/with-offset port offset)
  (let ((byte (input-port-byte-position port)))
    (cons (port-id port) (and byte (+ byte offset)))))


(define (die/lex pos who msg arg*)
  (raise
   (condition (make-lexical-violation)
	      (make-message-condition msg)
	      (if (null? arg*)
		  (condition)
		(make-irritants-condition arg*))
	      (let ((port-id (car pos))
		    (byte    (cdr pos)))
		(make-source-position-condition port-id byte #f))
	      )))

(define (die/pos port offset who msg arg*)
  (die/lex (make-compound-position/with-offset port offset) who msg arg*))

(define (die/p p who msg . arg*)
  (die/pos p 0 who msg arg*))

(define (die/p-1 p who msg . arg*)
  (die/pos p -1 who msg arg*))

(define (die/ann ann who msg . arg*)
  (die/lex (annotation-source ann) who msg arg*))


(define (checked-integer->char n ac p)
  (define (valid-integer-char? n)
    (cond
     [(<= n #xD7FF)   #t]
     [(< n #xE000)    #f]
     [(<= n #x10FFFF) #t]
     [else            #f]))
  (if (valid-integer-char? n)
      ($fixnum->char n)
    (die/p p 'tokenize
	   "invalid numeric value for character"
	   (list->string (reverse ac)))))

(define-syntax read-char
  (syntax-rules ()
    [(_ p) (get-char p)]))

(define delimiter?
  (lambda (c)
    (or (char-whitespace? c)
	(memq c '(#\( #\) #\[ #\] #\" #\# #\; #\{ #\} #\|)))))
(define digit?
  (lambda (c)
    (and ($char<= #\0 c) ($char<= c #\9))))
(define char->num
  (lambda (c)
    (fx- ($char->fixnum c) ($char->fixnum #\0))))
(define initial?
  (lambda (c)
    (cond
     [($char<= c ($fixnum->char 127))
      (or (letter? c) (special-initial? c))]
     [else (unicode-printable-char? c)])))
(define letter?
  (lambda (c)
    (or (and ($char<= #\a c) ($char<= c #\z))
	(and ($char<= #\A c) ($char<= c #\Z)))))
(define special-initial?
  (lambda (c)
    (memq c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))))
(define special-subsequent?
  (lambda (c)
    (memq c '(#\+ #\- #\. #\@))))
(define subsequent?
  (lambda (c)
    (cond
     [($char<= c ($fixnum->char 127))
      (or (letter? c)
	  (digit? c)
	  (special-initial? c)
	  (special-subsequent? c))]
     [else
      (or (unicode-printable-char? c)
	  (memq (char-general-category c) '(Nd Mc Me)))])))
(define tokenize-identifier
  (lambda (ls p)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c) ls]
       [(subsequent? c)
	(read-char p)
	(tokenize-identifier (cons c ls) p)]
       [(delimiter? c)
	ls]
       [(char=? c #\\)
	(read-char p)
	(tokenize-backslash ls p)]
       [(eq? (port-mode p) 'r6rs-mode)
	(die/p p 'tokenize "invalid identifier syntax"
	       (list->string (reverse (cons c ls))))]
       [else ls]))))
(define (tokenize-string ls p)
  (let ([c (read-char p)])
    (cond
     [(eof-object? c)
      (die/p p 'tokenize "invalid eof inside string")]
     [else (tokenize-string-char ls p c)])))
(define LF1 '(#\xA #\x85 #\x2028)) ;;; these are considered newlines
(define LF2 '(#\xA #\x85))         ;;; these are not newlines if they
                                     ;;; appear after CR
(define (tokenize-string-char ls p c)
  (define (intraline-whitespace? c)
    (or (eqv? c #\x9)
	(eq? (char-general-category c) 'Zs)))
  (define (tokenize-string-continue ls p c)
    (cond
     [(eof-object? c)
      (die/p p 'tokenize "invalid eof inside string")]
     [(intraline-whitespace? c)
      (let f ()
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'tokenize "invalid eof inside string")]
	   [(intraline-whitespace? c) (f)]
	   [else (tokenize-string-char ls p c)])))]
     [else (tokenize-string-char ls p c)]))
  (cond
   [($char= #\" c) ls]
   [($char= #\\ c)
    (let ([c (read-char p)])
      (cond
       [(eof-object? c)
	(die/p p 'tokenize "invalid eof after string escape")]
       [($char= #\a c) (tokenize-string (cons #\x7 ls) p)]
       [($char= #\b c) (tokenize-string (cons #\x8 ls) p)]
       [($char= #\t c) (tokenize-string (cons #\x9 ls) p)]
       [($char= #\n c) (tokenize-string (cons #\xA ls) p)]
       [($char= #\v c) (tokenize-string (cons #\xB ls) p)]
       [($char= #\f c) (tokenize-string (cons #\xC ls) p)]
       [($char= #\r c) (tokenize-string (cons #\xD ls) p)]
       [($char= #\" c) (tokenize-string (cons #\x22 ls) p)]
       [($char= #\\ c) (tokenize-string (cons #\x5C ls) p)]
       [($char= #\x c) ;;; unicode escape \xXXX;
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'tokenize "invalid eof inside string")]
	   [(hex c) =>
	    (lambda (n)
	      (let f ([n n] [ac (cons c '(#\x))])
		(let ([c (read-char p)])
		  (cond
		   [(eof-object? n)
		    (die/p p 'tokenize "invalid eof inside string")]
		   [(hex c) =>
		    (lambda (v) (f (+ (* n 16) v) (cons c ac)))]
		   [($char= c #\;)
		    (tokenize-string
		     (cons (checked-integer->char n ac p) ls) p)]
		   [else
		    (die/p-1 p 'tokenize
			     "invalid char in escape sequence"
			     (list->string (reverse (cons c ac))))]))))]
	   [else
	    (die/p-1 p 'tokenize
		     "invalid char in escape sequence" c)]))]
       [(intraline-whitespace? c)
	(let f ()
	  (let ([c (read-char p)])
	    (cond
	     [(eof-object? c)
	      (die/p p 'tokenize "invalid eof inside string")]
	     [(intraline-whitespace? c) (f)]
	     [(memv c LF1)
	      (tokenize-string-continue ls p (read-char p))]
	     [(eqv? c #\return)
	      (let ([c (read-char p)])
		(cond
		 [(memv c LF2)
		  (tokenize-string-continue ls p (read-char p))]
		 [else
		  (tokenize-string-continue ls p c)]))]
	     [else
	      (die/p-1 p 'tokenize
		       "non-whitespace character after escape")])))]
       [(memv c LF1)
	(tokenize-string-continue ls p (read-char p))]
       [(eqv? c #\return)
	(let ([c (read-char p)])
	  (cond
	   [(memv c LF2)
	    (tokenize-string-continue ls p (read-char p))]
	   [else
	    (tokenize-string-continue ls p c)]))]
       [else (die/p-1 p 'tokenize "invalid string escape" c)]))]
   [(memv c LF1)
    (tokenize-string (cons #\linefeed ls) p)]
   [(eqv? c #\return)
    (let ([c (peek-char p)])
      (when (memv c LF2) (read-char p))
      (tokenize-string (cons #\linefeed ls) p))]
   [else
    (tokenize-string (cons c ls) p)]))
(define skip-comment
  (lambda (p)
    (let ([c (read-char p)])
      (unless (or (eof-object? c) (memv c LF1) (eqv? c #\return))
	(skip-comment p)))))
(define tokenize-dot
  (lambda (p)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c) 'dot]
       [(delimiter? c)  'dot]
       [($char= c #\.)  ; this is second dot
	(read-char p)
	(let ([c (peek-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'tokenize "invalid syntax .. near end of file")]
	   [($char= c #\.) ; this is the third
	    (read-char p)
	    (let ([c (peek-char p)])
	      (cond
	       [(eof-object? c) '(datum . ...)]
	       [(delimiter? c)  '(datum . ...)]
	       [else
		(die/p p 'tokenize "invalid syntax"
                       (string-append "..." (string c)))]))]
	   [else
	    (die/p p 'tokenize "invalid syntax"
		   (string-append ".." (string c)))]))]
       [else
	(cons 'datum
	      (u:dot p '(#\.) 10 #f #f +1))]))))
(define tokenize-char*
  (lambda (i str p d)
    (cond
     [(fx= i (string-length str))
      (let ([c (peek-char p)])
	(cond
	 [(eof-object? c) d]
	 [(delimiter? c)  d]
	 [else
	  (die/p p 'tokenize "invalid character after sequence"
		 (string-append str (string c)))]))]
     [else
      (let ([c (read-char p)])
	(cond
	 [(eof-object? c)
	  (die/p p 'tokenize
		 "invalid eof in the middle of expected sequence" str)]
	 [($char= c (string-ref str i))
	  (tokenize-char* (fxadd1 i) str p d)]
	 [else
	  (die/p-1 p 'tokenize
		   "invalid char while scanning string"
		   c str)]))])))
(define tokenize-char-seq
  (lambda (p str d)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c) (cons 'datum (string-ref str 0))]
       [(delimiter? c)  (cons 'datum (string-ref str 0))]
       [($char= (string-ref str 1) c)
	(read-char p)
	(tokenize-char*  2 str p d)]
       [else
	(die/p p 'tokenize "invalid syntax"
	       (string-ref str 0) c)]))))
(define tokenize-char
  ;;Called after a hash character followed by a backslash character have
  ;;been read from P.
  ;;
  (lambda (p)
    (let ([c (read-char p)])
      (cond
       [(eof-object? c)
	(die/p p 'tokenize "invalid #\\ near end of file")]
       [(eqv? #\n c)
	(let ([c (peek-char p)])
	  (cond
	   [(eof-object? c)
	    (read-char p)
	    '(datum . #\n)]
	   [(eqv? #\u c)
	    (read-char p)
	    (tokenize-char-seq p "ul" '(datum . #\x0))]
	   [(eqv? #\e c)
	    (read-char p)
	    (tokenize-char-seq p "ewline" '(datum . #\xA))]
	   [(delimiter? c)
	    '(datum . #\n)]
	   [else
	    (die/p p 'tokenize "invalid syntax"
		   (string #\# #\\ #\n c))]))]
       [(eqv? #\a c)
	(tokenize-char-seq p "alarm" '(datum . #\x7))]
       [(eqv? #\b c)
	(tokenize-char-seq p "backspace" '(datum . #\x8))]
       [(eqv? #\t c)
	(tokenize-char-seq p "tab" '(datum . #\x9))]
       [(eqv? #\l c)
	(tokenize-char-seq p "linefeed" '(datum . #\xA))]
       [(eqv? #\v c)
	(tokenize-char-seq p "vtab" '(datum . #\xB))]
       [(eqv? #\p c)
	(tokenize-char-seq p "page" '(datum . #\xC))]
       [(eqv? #\r c)
	(tokenize-char-seq p "return" '(datum . #\xD))]
       [(eqv? #\e c)
	(tokenize-char-seq p "esc" '(datum . #\x1B))]
       [(eqv? #\s c)
	(tokenize-char-seq p "space" '(datum . #\x20))]
       [(eqv? #\d c)
	(tokenize-char-seq p "delete" '(datum . #\x7F))]
       [(eqv? #\x c)
	(let ([n (peek-char p)])
	  (cond
	   [(or (eof-object? n) (delimiter? n))
	    '(datum . #\x)]
	   [(hex n) =>
	    (lambda (v)
	      (read-char p)
	      (let f ([v v] [ac (cons n '(#\x))])
		(let ([c (peek-char p)])
		  (cond
		   [(eof-object? c)
		    (cons 'datum (checked-integer->char v ac p))]
		   [(delimiter? c)
		    (cons 'datum (checked-integer->char v ac p))]
		   [(hex c) =>
		    (lambda (v0)
		      (read-char p)
		      (f (+ (* v 16) v0) (cons c ac)))]
		   [else
		    (die/p p 'tokenize
                           "invalid character sequence"
                           (list->string (reverse (cons c ac))))]))))]
	   [else
	    (die/p p 'tokenize "invalid character sequence"
		   (string-append "#\\" (string n)))]))]
       [else
	(let ([n (peek-char p)])
	  (cond
	   [(eof-object? n) (cons 'datum c)]
	   [(delimiter? n)  (cons 'datum c)]
	   [else
	    (die/p p 'tokenize "invalid syntax"
		   (string-append "#\\" (string c n)))]))]))))
(define (hex x)
  (cond
   [(and ($char<= #\0 x) ($char<= x #\9))
    ($fx- ($char->fixnum x) ($char->fixnum #\0))]
   [(and ($char<= #\a x) ($char<= x #\f))
    ($fx- ($char->fixnum x)
	  ($fx- ($char->fixnum #\a) 10))]
   [(and ($char<= #\A x) ($char<= x #\F))
    ($fx- ($char->fixnum x)
	  ($fx- ($char->fixnum #\A) 10))]
   [else #f]))
(define multiline-error
  (lambda (p)
    (die/p p 'tokenize
	   "end of file encountered while inside a #|-style comment")))
(define apprev
  (lambda (str i ac)
    (cond
     [(fx= i (string-length str)) ac]
     [else
      (apprev str (fx+ i 1) (cons (string-ref str i) ac))])))
(define multiline-comment
  (lambda (p)
    (define f
      (lambda (p ac)
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c) (multiline-error p)]
	   [($char= #\| c)
	    (let g ([c (read-char p)] [ac ac])
	      (cond
	       [(eof-object? c) (multiline-error p)]
	       [($char= #\# c) ac]
	       [($char= #\| c)
		(g (read-char p) (cons c ac))]
	       [else (f p (cons c ac))]))]
	   [($char= #\# c)
	    (let ([c (read-char p)])
	      (cond
	       [(eof-object? c) (multiline-error p)]
	       [($char= #\| c)
		(let ([v (multiline-comment p)])
		  (if (string? v)
		      (f p (apprev v 0 ac))
		    (f p ac)))]
	       [else
		(f p (cons c (cons #\# ac)))]))]
	   [else (f p (cons c ac))]))))
    (let ([ac (f p '())])
      ((comment-handler)
       (list->string (reverse ac))))))

(define (tokenize-hash port)
  ;;Read a token from PORT.  Called after a #\# character has been read.
  ;;
  (tokenize-hash/c (read-char port) port))

(define (skip-whitespace p caller)
  (let ([c (read-char p)])
    (cond
     [(eof-object? c)
      (die/p p 'tokenize "invalid eof inside" caller)]
     [(char-whitespace? c)
      (skip-whitespace p caller)]
     [else c])))


(define (tokenize-hash/c c p)
  ;;Recognise a token  to be read from P.  Called  after a #\# character
  ;;has been read.  C is the character right after the hash.
  ;;
  ;;Return a datum representing the token that must be read:
  ;;
  ;;(datum . #t)		The token is the value #t.
  ;;(datum . #f)		The token is the value #f.
  ;;(datum . <char>)		The token is the character <char>.
  ;;vparen			The token is a vector.
  ;;(macro . syntax)		The token is a syntax form: #'---.
  ;;(macro . quasisyntax)	The token is a quasisyntax form: #`---.
  ;;(macro . unsyntax-splicing)	The token is an unsyntax-splicing form: #,@---.
  ;;(macro . unsyntax)		The token is an unsyntax form: #,---.
  ;;(datum . #!eof)		The token is the "#!eof" comment.
  ;;(mark . <n>)		The token is a graph syntax mark: #<N>=---
  ;;(ref . <n>)			The token is a graph syntax reference: #<N>#
  ;;
  ;;When the token is the  "#!r6rs" or "#!vicare" comment: the port mode
  ;;is changed  accordingly and TOKENIZE/1  is applied to the  port; the
  ;;return value is the return value of TOKENIZE/1.
  ;;
  (cond
   [(eof-object? c)
    (die/p p 'tokenize "invalid # near end of file")]

   [(or ($char= #\t c) ($char= #\T c)) #;(memq c '(#\t #\T))
    (let ([c1 (peek-char p)])
      (cond [(eof-object? c1) '(datum . #t)]
	    [(delimiter?  c1) '(datum . #t)]
	    [else
	     (die/p p 'tokenize (format "invalid syntax near #~a~a" c c1))]))]

   [(or ($char= #\f c) ($char= #\F c)) #;(memq c '(#\f #\F))
    (let ([c1 (peek-char p)])
      (cond [(eof-object? c1) '(datum . #f)]
	    [(delimiter? c1)  '(datum . #f)]
	    [else (die/p p 'tokenize (format "invalid syntax near #~a~a" c c1))]))]

   [($char= #\\ c) (tokenize-char p)]
   [($char= #\( c) 'vparen]
   [($char= #\' c) '(macro . syntax)]
   [($char= #\` c) '(macro . quasisyntax)]

   [($char= #\, c)
    (let ([c (peek-char p)])
      (cond [(eqv? c #\@)
	     (read-char p)
	     '(macro . unsyntax-splicing)]
	    [else
	     '(macro . unsyntax)]))]

   [($char= #\! c)
    (let ([e (read-char p)])
      (when (eof-object? e)
	(die/p p 'tokenize "invalid eof near #!"))
      (case e
	[(#\e)
	 (when (eq? (port-mode p) 'r6rs-mode)
	   (die/p-1 p 'tokenize "invalid syntax: #!e"))
	 (read-char* p '(#\e) "of" "eof sequence" #f #f)
	 (cons 'datum (eof-object))]
	[(#\r)
	 (read-char* p '(#\r) "6rs" "#!r6rs comment" #f #f)
	 (set-port-mode! p 'r6rs-mode)
	 (tokenize/1 p)]
	[(#\v)
	 (read-char* p '(#\v) "icare" "#!vicare comment" #f #f)
	 (set-port-mode! p 'vicare-mode)
	 (tokenize/1 p)]
	[else
	 (die/p-1 p 'tokenize (format "invalid syntax near #!~a" e))]))]

   [(digit? c)
    (when (eq? (port-mode p) 'r6rs-mode)
      (die/p-1 p 'tokenize "graph syntax is invalid in #!r6rs mode"
	       (format "#~a" c)))
    (tokenize-hashnum p (char->num c))]

   [($char= #\: c)
    (when (eq? (port-mode p) 'r6rs-mode)
      (die/p-1 p 'tokenize "gensym syntax is invalid in #!r6rs mode" (format "#~a" c)))
    (let* ([c (skip-whitespace p "gensym")]
	   [id0 (cond [(initial? c)
		       (list->string (reverse (tokenize-identifier (cons c '()) p)))]
		      [($char= #\| c)
		       (list->string (reverse (tokenize-bar p '())))]
		      [else
		       (die/p-1 p 'tokenize "invalid char inside gensym" c)])])
      (cons 'datum (gensym id0)))]

   [($char= #\{ c)
    (when (eq? (port-mode p) 'r6rs-mode)
      (die/p-1 p 'tokenize "gensym syntax is invalid in #!r6rs mode"
	       (format "#~a" c)))
    (let* ([c (skip-whitespace p "gensym")]
	   [id0
	    (cond
	     [(initial? c)
	      (list->string
	       (reverse (tokenize-identifier (cons c '()) p)))]
	     [($char= #\| c)
	      (list->string
	       (reverse (tokenize-bar p '())))]
	     [else
	      (die/p-1 p 'tokenize
		       "invalid char inside gensym" c)])]
	   [c (skip-whitespace p "gensym")])
      (cond
       [($char= #\} c)
	(cons 'datum
	      (foreign-call "ikrt_strings_to_gensym" #f id0))]
       [else
	(let ([id1
	       (cond
		[(initial? c)
		 (list->string
		  (reverse
		   (tokenize-identifier
		    (cons c '()) p)))]
		[($char= #\| c)
		 (list->string
		  (reverse (tokenize-bar p '())))]
		[else
		 (die/p-1 p 'tokenize
			  "invalid char inside gensym" c)])])
	  (let ([c (skip-whitespace p "gensym")])
	    (cond
	     [($char= #\} c)
	      (cons 'datum
		    (foreign-call "ikrt_strings_to_gensym"
				  id0 id1))]
	     [else
	      (die/p-1 p 'tokenize
		       "invalid char inside gensym" c)])))]))]
   [($char= #\v c)
    (let ([c (read-char p)])
      (cond [($char= #\u c)
	     (let ([c (read-char p)])
	       (cond [($char= c #\8)
		      (let ([c (read-char p)])
			(cond [($char= c #\() 'vu8]
			      [(eof-object? c)
			       (die/p p 'tokenize "invalid eof object after #vu8")]
			      [else
			       (die/p-1 p 'tokenize (format "invalid sequence #vu8~a" c))]))]
		     [(eof-object? c)
		      (die/p p 'tokenize "invalid eof object after #vu")]
		     [else (die/p-1 p 'tokenize (format "invalid sequence #vu~a" c))]))]
	    [($char= #\s c)
	     [when (eq? (port-mode p) 'r6rs-mode)
	       (die/p p 'tokenize "invalid #vs8 syntax in #!r6rs mode" "#vs8")]
	     (let ([c (read-char p)])
	       (cond [($char= c #\8)
		      (let ([c (read-char p)])
			(cond [($char= c #\() 'vs8]
			      [(eof-object? c)
			       (die/p p 'tokenize "invalid eof object after #vs8")]
			      [else
			       (die/p-1 p 'tokenize (format "invalid sequence #vs8~a" c))]))]
		     [(eof-object? c)
		      (die/p p 'tokenize "invalid eof object after #vs")]
		     [else (die/p-1 p 'tokenize (format "invalid sequence #vs~a" c))]))]
	    [(eof-object? c)
	     (die/p p 'tokenize "invalid eof object after #v")]
	    [else (die/p p 'tokenize (format "invalid sequence #v~a" c))]))]
   [(memq c '(#\e #\E))
    (cons 'datum (parse-string p (list c #\#) 10 #f 'e))]
   [(memq c '(#\i #\I))
    (cons 'datum (parse-string p (list c #\#) 10 #f 'i))]
   [(memq c '(#\b #\B))
    (cons 'datum (parse-string p (list c #\#) 2 2 #f))]
   [(memq c '(#\x #\X))
    (cons 'datum (parse-string p (list c #\#) 16 16 #f))]
   [(memq c '(#\o #\O))
    (cons 'datum (parse-string p (list c #\#) 8 8 #f))]
   [(memq c '(#\d #\D))
    (cons 'datum (parse-string p (list c #\#) 10 10 #f))]
;;;[($char= #\@ c) DEAD: Unfixable due to port encoding
;;;                 that does not allow mixing binary and
;;;                 textual data in the same port.
;;;                Left here for historical value
;;; (when (eq? (port-mode p) 'r6rs-mode)
;;;   (die/p-1 p 'tokenize "fasl syntax is invalid in #!r6rs mode"
;;;      (format "#~a" c)))
;;; (die/p-1 p 'read "FIXME: fasl read disabled")
;;; '(cons 'datum ($fasl-read p))]
   [else
    (die/p-1 p 'tokenize (format "invalid syntax #~a" c))]))


(define (num-error p str ls)
  (die/p-1 p 'read str
	   (list->string (reverse ls))))

(define-syntax port-config
  (syntax-rules (GEN-TEST GEN-ARGS FAIL EOF-ERROR GEN-DELIM-TEST)
    [(_ GEN-ARGS k . rest) (k (p ac) . rest)]
    [(_ FAIL (p ac))
     (num-error p "invalid numeric sequence" ac)]
    [(_ FAIL (p ac) c)
     (num-error p "invalid numeric sequence" (cons c ac))]
    [(_ EOF-ERROR (p ac))
     (num-error p "invalid eof while reading number" ac)]
    [(_ GEN-DELIM-TEST c sk fk)
     (if (delimiter? c) sk fk)]
    [(_ GEN-TEST var next fail (p ac) eof-case char-case)
     (let ([c (peek-char p)])
       (if (eof-object? c)
	   (let ()
	     (define-syntax fail
	       (syntax-rules ()
		 [(_) (num-error p "invalid numeric sequence" ac)]))
	     eof-case)
	 (let ([var c])
	   (define-syntax fail
	     (syntax-rules ()
	       [(_)
		(num-error p "invalid numeric sequence"
			   (cons var ac))]))
	   (define-syntax next
	     (syntax-rules ()
	       [(_ who args (... ...))
		(who p (cons (get-char p) ac) args (... ...))]))
	   char-case)))]))

(define-string->number-parser port-config
  (parse-string u:digit+ u:sign u:dot))

(define (read-char* p ls str who ci? delimited?)
  (let f ([i 0] [ls ls])
    (cond
     [(fx= i (string-length str))
      (when delimited?
	(let ([c (peek-char p)])
	  (when (and (not (eof-object? c)) (not (delimiter? c)))
	    (die/p p 'tokenize
		   (format "invalid ~a: ~s" who
			   (list->string (reverse (cons c ls))))))))]
     [else
      (let ([c (read-char p)])
	(cond
	 [(eof-object? c)
	  (die/p p 'tokenize
		 (format "invalid eof inside ~a" who))]
	 [(or (and (not ci?) (char=? c (string-ref str i)))
	      (and ci? (char=? (char-downcase c) (string-ref str i))))
	  (f (add1 i) (cons c ls))]
	 [else
	  (die/p-1 p 'tokenize
		   (format "invalid ~a: ~s" who
			   (list->string (reverse (cons c ls)))))]))])))
(define (tokenize-hashnum p n)
  (let ([c (read-char p)])
    (cond
     [(eof-object? c)
      (die/p p 'tokenize "invalid eof inside #n mark/ref")]
     [($char= #\= c) (cons 'mark n)]
     [($char= #\# c) (cons 'ref n)]
     [(digit? c)
      (tokenize-hashnum p (fx+ (fx* n 10) (char->num c)))]
     [else
      (die/p-1 p 'tokenize "invalid char while inside a #n mark/ref" c)])))

(define tokenize-bar
  (lambda (p ac)
    (let ([c (read-char p)])
      (cond
       [(eof-object? c)
	(die/p p 'tokenize "unexpected eof while reading symbol")]
       [($char= #\\ c)
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'tokenize "unexpected eof while reading symbol")]
	   [else (tokenize-bar p (cons c ac))]))]
       [($char= #\| c) ac]
       [else (tokenize-bar p (cons c ac))]))))
(define (tokenize-backslash main-ac p)
  (let ([c (read-char p)])
    (cond
     [(eof-object? c)
      (die/p p 'tokenize "invalid eof after symbol escape")]
     [($char= #\x c)
      (let ([c (read-char p)])
	(cond
	 [(eof-object? c)
	  (die/p p 'tokenize "invalid eof after \\x")]
	 [(hex c) =>
	  (lambda (v)
	    (let f ([v v] [ac `(,c #\x #\\)])
	      (let ([c (read-char p)])
		(cond
		 [(eof-object? c)
		  (die/p p 'tokenize
                         (format "invalid eof after ~a"
                           (list->string (reverse ac))))]
		 [($char= #\; c)
		  (tokenize-identifier
		   (cons (checked-integer->char v ac p) main-ac)
		   p)]
		 [(hex c) =>
		  (lambda (v0)
		    (f (+ (* v 16) v0) (cons c ac)))]
		 [else
		  (die/p-1 p 'tokenize "invalid sequence"
			   (list->string (cons c (reverse ac))))]))))]
	 [else
	  (die/p-1 p 'tokenize
		   (format "invalid sequence \\x~a" c))]))]
     [else
      (die/p-1 p 'tokenize
	       (format "invalid sequence \\~a" c))])))
(define tokenize/c
  (lambda (c p)
    (cond
     [(eof-object? c)
      (error 'tokenize/c "hmmmm eof")
      (eof-object)]
     [($char= #\( c)   'lparen]
     [($char= #\) c)   'rparen]
     [($char= #\[ c)   'lbrack]
     [($char= #\] c)   'rbrack]
     [($char= #\' c)   '(macro . quote)]
     [($char= #\` c)   '(macro . quasiquote)]
     [($char= #\, c)
      (let ([c (peek-char p)])
	(cond
	 [(eof-object? c) '(macro . unquote)]
	 [($char= c #\@)
	  (read-char p)
	  '(macro . unquote-splicing)]
	 [else '(macro . unquote)]))]
     [($char= #\# c)
      (tokenize-hash p)]
     [(char<=? #\0 c #\9)
      (let ([d (fx- (char->integer c) (char->integer #\0))])
	(cons 'datum
	      (u:digit+ p (list c) 10 #f #f +1 d)))]
     [(initial? c)
      (let ([ls (reverse (tokenize-identifier (cons c '()) p))])
	(cons 'datum (string->symbol (list->string ls))))]
     [($char= #\" c)
      (let ([ls (tokenize-string '() p)])
	(cons 'datum (list->string (reverse ls))))]
     [(memq c '(#\+))
      (let ([c (peek-char p)])
	(cond
	 [(eof-object? c) '(datum . +)]
	 [(delimiter? c)  '(datum . +)]
	 [else
	  (cons 'datum
                (u:sign p '(#\+) 10 #f #f +1))]))]
     [(memq c '(#\-))
      (let ([c (peek-char p)])
	(cond
	 [(eof-object? c) '(datum . -)]
	 [(delimiter? c)  '(datum . -)]
	 [($char= c #\>)
	  (read-char p)
	  (let ([ls (tokenize-identifier '() p)])
	    (let ([str (list->string (cons* #\- #\> (reverse ls)))])
	      (cons 'datum (string->symbol str))))]
	 [else
	  (cons 'datum
                (u:sign p '(#\-) 10 #f #f -1))]))]
     [($char= #\. c)
      (tokenize-dot p)]
     [($char= #\| c)
      (when (eq? (port-mode p) 'r6rs-mode)
	(die/p p 'tokenize "|symbol| syntax is invalid in #!r6rs mode"))
      (let ([ls (reverse (tokenize-bar p '()))])
	(cons 'datum (string->symbol (list->string ls))))]
     [($char= #\\ c)
      (cons 'datum
            (string->symbol
	     (list->string
	      (reverse (tokenize-backslash '() p)))))]
		;[($char= #\{ c) 'lbrace]
     [($char= #\@ c)
      (when (eq? (port-mode p) 'r6rs-mode)
	(die/p p 'tokenize "@-expr syntax is invalid in #!r6rs mode"))
      'at-expr]
     [else
      (die/p-1 p 'tokenize "invalid syntax" c)])))

(define tokenize/1
  (lambda (p)
    (let ([c (read-char p)])
      (cond
       [(eof-object? c) (eof-object)]
       [(eqv? c #\;)
	(skip-comment p)
	(tokenize/1 p)]
       [(eqv? c #\#)
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'tokenize "invalid eof after #")]
	   [(eqv? c #\;)
	    (read-as-comment p)
	    (tokenize/1 p)]
	   [(eqv? c #\|)
	    (multiline-comment p)
	    (tokenize/1 p)]
	   [else
	    (tokenize-hash/c c p)]))]
       [(char-whitespace? c) (tokenize/1 p)]
       [else (tokenize/c c p)]))))

(define tokenize/1+pos
  (lambda (p)
    (let ((pos (make-compound-position p)))
      (let ([c (read-char p)])
	(cond
	 [(eof-object? c) (values (eof-object) pos)]
	 [(eqv? c #\;)
	  (skip-comment p)
	  (tokenize/1+pos p)]
	 [(eqv? c #\#)
	  (let ((pos (make-compound-position p)))
	    (let ([c (read-char p)])
	      (cond
	       [(eof-object? c)
		(die/p p 'tokenize "invalid eof after #")]
	       [(eqv? c #\;)
		(read-as-comment p)
		(tokenize/1+pos p)]
	       [(eqv? c #\|)
		(multiline-comment p)
		(tokenize/1+pos p)]
	       [else
		(values (tokenize-hash/c c p) pos)])))]
	 [(char-whitespace? c) (tokenize/1+pos p)]
	 [else
	  (values (tokenize/c c p) pos)])))))

(define tokenize-script-initial
  (lambda (p)
    (let ([c (read-char p)])
      (cond
       [(eof-object? c) c]
       [(eqv? c #\;)
	(skip-comment p)
	(tokenize/1 p)]
       [(eqv? c #\#)
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'tokenize "invalid eof after #")]
	   [(eqv? c #\!)
	    (skip-comment p)
	    (tokenize/1 p)]
	   [(eqv? c #\;)
	    (read-as-comment p)
	    (tokenize/1 p)]
	   [(eqv? c #\|)
	    (multiline-comment p)
	    (tokenize/1 p)]
	   [else
	    (tokenize-hash/c c p)]))]
       [(char-whitespace? c) (tokenize/1 p)]
       [else (tokenize/c c p)]))))

(define tokenize-script-initial+pos
  (lambda (p)
    (let ((pos (make-compound-position p)))
      (let ([c (read-char p)])
	(cond
	 [(eof-object? c) (values (eof-object) pos)]
	 [(eqv? c #\;)
	  (skip-comment p)
	  (tokenize/1+pos p)]
	 [(eqv? c #\#)
	  (let ((pos (make-compound-position p)))
	    (let ([c (read-char p)])
	      (cond
	       [(eof-object? c)
		(die/p p 'tokenize "invalid eof after #")]
	       [(eqv? c #\!)
		(skip-comment p)
		(tokenize/1+pos p)]
	       [(eqv? c #\;)
		(read-as-comment p)
		(tokenize/1+pos p)]
	       [(eqv? c #\|)
		(multiline-comment p)
		(tokenize/1+pos p)]
	       [else
		(values (tokenize-hash/c c p) pos)])))]
	 [(char-whitespace? c) (tokenize/1+pos p)]
	 [else (values (tokenize/c c p) pos)])))))

(define-struct loc (value value^ set?))

  ;;; this is reverse engineered from psyntax.ss
(define-struct annotation (expression source stripped))
  ;;; - source is a pair of file-name x char-position
  ;;; - stripped is an s-expression with no annotations
  ;;; - expression is a list/vector/id/whathaveyou that
  ;;;   may contain further annotations.



(module (read-expr read-expr-script-initial)
  (define-syntax tokenize/1 syntax-error)
  (define (annotate-simple datum pos p)
    (make-annotation datum pos #;(cons (port-id p) pos) datum))
  (define (annotate stripped expression pos p)
    (make-annotation expression pos #;(cons (port-id p) pos) stripped))

  (define (read-list p locs k end mis init?)
    (let-values ([(t pos) (tokenize/1+pos p)])
      (cond
       [(eof-object? t)
	(die/p p 'read "end of file encountered while reading list")]
       [(eq? t end) (values '() '() locs k)]
       [(eq? t mis)
	(die/p-1 p 'read "paren mismatch")]
       [(eq? t 'dot)
	(when init?
	  (die/p-1 p 'read "invalid dot while reading list"))
	(let-values ([(d d^ locs k) (read-expr p locs k)])
	  (let-values ([(t pos^) (tokenize/1+pos p)])
	    (cond
	     [(eq? t end) (values d d^ locs k)]
	     [(eq? t mis)
	      (die/p-1 p 'read "paren mismatch")]
	     [(eq? t 'dot)
	      (die/p-1 p 'read "cannot have two dots in a list")]
	     [else
	      (die/p-1 p 'read
		       (format "expecting ~a, got ~a" end t))])))]
       [else
	(let-values ([(a a^ locs k) (parse-token p locs k t pos)])
	  (let-values ([(d d^ locs k) (read-list p locs k end mis #f)])
	    (let ([x (cons a d)] [x^ (cons a^ d^)])
	      (values x x^ locs (extend-k-pair x x^ a d k)))))])))

  (define (extend-k-pair x x^ a d k)
    (cond [(or (loc? a) (loc? d))
	   (lambda ()
	     (let ([a (car x)])
	       (when (loc? a)
		 (set-car! x (loc-value a))
		 (set-car! x^ (loc-value^ a))))
	     (let ([d (cdr x)])
	       (when (loc? d)
		 (set-cdr! x (loc-value d))
		 (set-cdr! x^ (loc-value^ d))))
	     (k))]
	  [else k]))

  (define (vector-put v v^ k i ls ls^)
    (cond [(null? ls) k]
	  [else
	   (let ([a (car ls)])
	     (vector-set! v i a)
	     (vector-set! v^ i (car ls^))
	     (vector-put v v^
			 (if (loc? a)
			     (lambda ()
			       (vector-set! v i (loc-value a))
			       (vector-set! v^ i (loc-value^ a))
			       (k))
			   k)
			 (fxsub1 i)
			 (cdr ls)
			 (cdr ls^)))]))

  (define (read-vector p locs k count ls ls^)
    (let-values (((token pos) (tokenize/1+pos p)))
      (cond [(eof-object? token)
	     (die/p p 'read "end of file encountered while reading a vector")]
	    [(eq? token 'rparen)
	     (let ([v  (make-vector count)]
		   [v^ (make-vector count)])
	       (let ([k (vector-put v v^ k (fxsub1 count) ls ls^)])
		 (values v v^ locs k)))]
	    [(eq? token 'rbrack)
	     (die/p-1 p 'read "unexpected ] while reading a vector")]
	    [(eq? token 'dot)
	     (die/p-1 p 'read "unexpected . while reading a vector")]
	    [else
	     (let-values ([(a a^ locs k) (parse-token p locs k token pos)])
	       (read-vector p locs k (fxadd1 count)
			    (cons a ls) (cons a^ ls^)))])))

  (define (read-u8-bytevector p locs k count ls)
    (let-values ([(t pos) (tokenize/1+pos p)])
      (cond
       [(eof-object? t)
	(die/p p 'read "end of file encountered while reading a bytevector")]
       [(eq? t 'rparen)
	(let ([v (u8-list->bytevector (reverse ls))])
	  (values v v locs k))]
       [(eq? t 'rbrack)
	(die/p-1 p 'read "unexpected ] while reading a bytevector")]
       [(eq? t 'dot)
	(die/p-1 p 'read "unexpected . while reading a bytevector")]
       [else
	(let-values ([(a a^ locs k) (parse-token p locs k t pos)])
	  (unless (and (fixnum? a) (fx<= 0 a) (fx<= a 255))
	    (die/ann a^ 'read "invalid value in a u8 bytevector" a))
	  (read-u8-bytevector p locs k (fxadd1 count) (cons a ls)))])))

  (define (read-s8-bytevector p locs k count ls)
    (let-values ([(t pos) (tokenize/1+pos p)])
      (cond
       [(eof-object? t)
	(die/p p 'read "end of file encountered while reading a bytevector")]
       [(eq? t 'rparen)
	(let ((v (let ((bv ($make-bytevector count)))
		   (let loop ((i  (- count 1))
			      (ls ls))
		     (if (null? ls)
			 bv
		       (begin
			 ($bytevector-set! bv i (car ls))
			 (loop (- i 1) (cdr ls))))))))
	  (values v v locs k))]
       [(eq? t 'rbrack)
	(die/p-1 p 'read "unexpected ] while reading a bytevector")]
       [(eq? t 'dot)
	(die/p-1 p 'read "unexpected . while reading a bytevector")]
       [else
	(let-values ([(a a^ locs k) (parse-token p locs k t pos)])
	  (unless (and (fixnum? a) (fx<= -128 a) (fx<= a 127))
	    (die/ann a^ 'read "invalid value in a s8 bytevector" a))
	  (read-s8-bytevector p locs k (fxadd1 count) (cons a ls)))])))


(define (read-at-expr p locs k at-pos)
  (define-struct nested (a a^))
  (define-struct nested* (a* a*^))

  ;;Commented out because it is never used (Marco Maggi; Oct 12, 2011).
  ;;
  ;; (define (get-chars chars pos p a* a*^)
  ;;   (if (null? chars)
  ;; 	(values a* a*^)
  ;;     (let ([str (list->string chars)])
  ;; 	(let ([str^ (annotate-simple str pos p)])
  ;; 	  (values (cons str a*) (cons str^ a*^))))))

  (define (return start-pos start-col c*** p)
    (let ([indent (apply min start-col
			 (map (lambda (c**)
				(define (st00 c* c** n)
				  (if (null? c*)
				      (st0 c** n)
				    (if (char=? (car c*) #\space)
					(st00 (cdr c*) c** (+ n 1))
				      n)))
				(define (st0 c** n)
				  (if (null? c**)
				      start-col
				    (let ([c* (car c**)])
				      (if (or (nested? c*) (nested*? c*))
					  start-col
					(st00 (car c*) (cdr c**) n)))))
				(st0 c** 0))
			   (cdr c***)))])
      (define (convert c*)
	(if (or (nested? c*) (nested*? c*))
	    c*
	  (let ([str (list->string (car c*))])
	    (let ([str^ (annotate-simple str (cdr c*) p)])
	      (make-nested str str^)))))
      (define (trim/convert c**)
	(define (mk n pos)
	  (let ([str (make-string (- n indent) #\space)])
	    (let ([str^ (annotate-simple str pos p)])
	      (make-nested str str^))))
	(define (s1 c* pos c** n)
	  (if (null? c*)
	      (let ([c* (car c**)])
		(if (or (nested? c*) (nested*? c*))
		    (cons (mk n pos) (map convert c**))
		  (s1 c* pos (cdr c**) n)))
	    (if (char=? (car c*) #\space)
		(s1 (cdr c*) pos c** (+ n 1))
	      (cons*
	       (mk n pos)
	       (map convert (cons (cons c* pos) c**))))))
	(define (s00 c* pos c** n)
	  (if (null? c*)
	      (s0 c** n)
	    (if (char=? #\space (car c*))
		(if (< n indent)
		    (s00 (cdr c*) pos c** (+ n 1))
		  (s1 (cdr c*) pos c** (+ n 1)))
	      (map convert (cons (cons c* pos) c**)))))
	(define (s0 c** n)
	  (if (null? c**)
	      '()
	    (let ([c* (car c**)])
	      (if (or (nested? c*) (nested*? c*))
		  (map convert c**)
		(s00 (car c*) (cdr c*) (cdr c**) n)))))
	(s0 c** 0))
      (define (cons-initial c** c***)
	(define (all-white? c**)
	  (andmap (lambda (c*)
		    (and (not (nested? c*))
			 (not (nested*? c*))
			 (andmap
			  (lambda (c) (char=? c #\space))
			  (car c*))))
		  c**))
	(define (nl)
	  (let ([str "\n"])
	    (list (make-nested str str))))
	(define (S1 c*** n)
	  (if (null? c***)
	      (make-list n (nl))
	    (let ([c** (car c***)] [c*** (cdr c***)])
	      (if (all-white? c**)
		  (S1 c*** (+ n 1))
		(append
		 (make-list n (nl))
		 (cons (trim/convert c**)
		       (S2 c*** 0 0)))))))
	(define (S2 c*** n m)
	  (if (null? c***)
	      (make-list (+ n m) (nl))
	    (let ([c** (car c***)] [c*** (cdr c***)])
	      (if (all-white? c**)
		  (S2 c*** (+ n 1) -1)
		(append
		 (make-list (+ n 1) (nl))
		 (cons (trim/convert c**)
		       (S2 c*** 0 0)))))))
	(define (S0 c** c***)
	  (if (all-white? c**)
	      (S1 c*** 0)
	    (cons
	     (map convert c**)
	     (S2 c*** 0 0))))
	(S0 c** c***))
      (let ([c** (cons-initial (car c***) (cdr c***))])
	(let ([n* (apply append c**)])
	  (define (extract p p* ls)
	    (let f ([ls ls])
	      (cond
	       [(null? ls) '()]
	       [(nested? (car ls)) (cons (p (car ls)) (f (cdr ls)))]
	       [else (append (p* (car ls)) (f (cdr ls)))])))
	  (let ([c* (extract nested-a nested*-a* n*)]
		[c*^ (extract nested-a^ nested*-a*^ n*)])
	    (values c* (annotate c* c*^ start-pos p) locs k))))))
;;; end of RETURN function

  (define (read-text p locs k pref*)
    (let ([start-pos (port-position p)]
	  [start-col (input-port-column-number p)])
      (let f ([c* '()] [pos start-pos]
	      [c** '()] [c*** '()]
	      [depth 0] [locs locs] [k k])
	(define (match-prefix c* pref*)
	  (cond
	   [(and (pair? c*) (pair? pref*))
	    (and (char=? (car c*) (car pref*))
		 (match-prefix (cdr c*) (cdr pref*)))]
	   [else (and (null? pref*) c*)]))
	(let ([c (read-char p)])
	  (cond
	   [(eof-object? c)
	    (die/p p 'read "end of file while reading @-expr text")]
	   [(char=? c #\})
	    (let g ([x* (cons #\} c*)] [p* pref*])
	      (if (null? p*)
		  (if (= depth 0)
		      (let ([c**
			     (reverse
			      (if (null? c*)
				  c**
				(cons (cons (reverse c*) pos) c**)))])
			(let ([c*** (reverse (cons c** c***))])
			  (return start-pos start-col c*** p)))
		    (f x* pos c** c*** (- depth 1) locs k))
		(let ([c (peek-char p)])
		  (cond
		   [(eof-object? c)
		    (die/p p 'read "invalid eof inside @-expression")]
		   [(char=? c (rev-punc (car p*)))
		    (read-char p)
		    (g (cons c x*) (cdr p*))]
		   [else
		    (f x* pos c** c*** depth locs k)]))))]
	   [(char=? c #\{)
	    (f (cons c c*) pos c** c***
	       (if (match-prefix c* pref*) (+ depth 1) depth)
	       locs k)]
	   [(char=? c #\newline)
	    (f '()
	       (port-position p)
	       '()
	       (cons (reverse
		      (if (null? c*)
			  c**
			(cons (cons (reverse c*) pos) c**)))
		     c***)
	       depth locs k)]
	   [(and (char=? c #\@) (match-prefix c* pref*)) =>
	    (lambda (c*)
	      (let ([c (peek-char p)])
		(cond
		 [(eof-object? c)
		  (die/p p 'read "invalid eof inside nested @-expr")]
		 [(char=? c #\")
		  (read-char p)
		  (let ([c* (tokenize-string c* p)])
		    (f c* pos c** c*** depth locs k))]
		 [else
		  (let-values ([(a* a*^ locs k)
				(read-at-text-mode p locs k)])
		    (f '()
		       (port-position p)
		       (cons (make-nested* a* a*^)
			     (if (null? c*)
				 c**
			       (cons (cons (reverse c*) pos) c**)))
		       c*** depth locs k))])))]
	   [else
	    (f (cons c c*) pos c** c*** depth locs k)])))))
;;;end of READ-TEXT function

  (define (read-brackets p locs k)
    (let-values ([(a* a*^ locs k)
		  (read-list p locs k 'rbrack 'rparen #t)])
      (unless (list? a*)
	(die/ann a*^ 'read "not a proper list"))
      (let ([c (peek-char p)])
	(cond
	 [(eof-object? c) ;;; @<cmd>[...]
	  (values a* a*^ locs k)]
	 [(char=? c #\{)
	  (read-char p)
	  (let-values ([(b* b*^ locs k)
			(read-text p locs k '())])
	    (values (append a* b*)
		    (append a*^ b*^)
		    locs k))]
	 [(char=? c #\|)
	  (read-char p)
	  (let-values ([(b* b*^ locs k)
			(read-at-bar p locs k #t)])
	    (values (append a* b*)
		    (append a*^ b*^)
		    locs k))]
	 [else (values a* a*^ locs k)]))))
  (define (left-punc? c)
    (define chars "([<!?~$%^&*-_+=:")
    (let f ([i 0])
      (cond
       [(= i (string-length chars)) #f]
       [(char=? c (string-ref chars i)) #t]
       [else (f (+ i 1))])))
  (define (rev-punc c)
    (cond
     [(char=? c #\() #\)]
     [(char=? c #\[) #\]]
     [(char=? c #\<) #\>]
     [else c]))
  (define (read-at-bar p locs k text-mode?)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c)
	(die/p p 'read "eof inside @|-expression")]
       [(and (char=? c #\|) text-mode?) ;;; @||
	(read-char p)
	(values '() '() locs k)]
       [(char=? c #\{) ;;; @|{
	(read-char p)
	(read-text p locs k '(#\|))]
       [(left-punc? c) ;;; @|<({
	(read-char p)
	(let ([pos (port-position p)])
	  (let f ([ls (list c)])
	    (let ([c (peek-char p)])
	      (cond
	       [(eof-object? c)
		(die/p p 'read "eof inside @|< mode")]
	       [(left-punc? c)
		(read-char p)
		(f (cons c ls))]
	       [(char=? c #\{)
		(read-char p)
		(read-text p locs k (append ls '(#\|)))]
	       [else
		(read-at-bar-others ls p locs k)]))))]
       [text-mode? ;;; @|5 6 7|
	(read-at-bar-datum p locs k)]
       [else
	(die/p p 'read "invalid char in @| mode" c)])))

  (define (read-at-bar-others ls p locs k)
    (define (split ls)
      (cond
       [(null? ls) (values '() '())]
       [(initial? (car ls))
	(let-values ([(a d) (split (cdr ls))])
	  (values (cons (car ls) a) d))]
       [else
	(values '() ls)]))
    (define (mksymbol ls)
      (let ([s (string->symbol (list->string (reverse ls)))])
	(values s s)))
    (let-values ([(inits rest) (split ls)])
      (let ([ls (tokenize-identifier inits p)])
	(let-values ([(s s^) (mksymbol ls)])
	  (let g ([rest rest]
		  [a* (list s)]
		  [a*^ (list s^)]
		  [locs locs]
		  [k k])
	    (if (null? rest)
		(let-values ([(b* b*^ locs k)
			      (read-at-bar-datum p locs k)])
		  (values (append a* b*) (append a*^ b*^) locs k))
	      (let ([x (car rest)])
		(case x
		  [(#\() #\) ;;; vim paren-matching sucks
		   (let-values ([(b* b*^ locs k)
				 (read-list p locs k 'rparen 'rbrack #t)])
		     (g (cdr rest)
			(list (append a* b*))
			(list (append a*^ b*^))
			locs k))]
		  [(#\[) #\] ;;;  vim paren-matching sucks
		   (let-values ([(b* b*^ locs k)
				 (read-list p locs k 'rbrack 'rparen #t)])
		     (g (cdr rest)
			(list (append a* b*))
			(list (append a*^ b*^))
			locs k))]
		  [else
		   (let-values ([(inits rest) (split rest)])
		     (let-values ([(s s^) (mksymbol inits)])
		       (g rest
			  (cons s a*)
			  (cons s^ a*^)
			  locs k)))]))))))))
;;; end of READ-AT-BAR-OTHERS

  (define (read-at-bar-datum p locs k)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c) (die/p p 'read "eof inside @|datum mode")]
       [(char-whitespace? c)
	(read-char p)
	(read-at-bar-datum p locs k)]
       [(char=? c #\|)
	(read-char p)
	(values '() '() locs k)]
       [else
	(let-values ([(a a^ locs k) (read-expr p locs k)])
	  (let-values ([(a* a*^ locs k) (read-at-bar-datum p locs k)])
	    (values (cons a a*) (cons a^ a*^) locs k)))])))

  (define (read-at-text-mode p locs k)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c)
	(die/p p 'read "eof encountered inside @-expression")]
       [(char=? c #\|)
	(read-char p)
	(read-at-bar p locs k #t)]
       [else
	(let-values ([(a a^ locs k)
		      (read-at-sexpr-mode p locs k)])
	  (values (list a) (list a^) locs k))])))

  (define (read-at-sexpr-mode p locs k)
    (let ([c (peek-char p)])
      (cond
       [(eof-object? c)
	(die/p p 'read "eof encountered inside @-expression")]
       [(eqv? c '#\[) ;;;   @[ ...
	(read-char p)
	(read-brackets p locs k)]
       [(eqv? c #\{) ;;;   @{ ...
	(read-char p)
	(read-text p locs k '())]
       [(char=? c #\|)
	(read-char p)
	(read-at-bar p locs k #f)]
       [else ;;;   @<cmd> ...
	(let-values ([(a a^ locs k) (read-expr p locs k)])
	  (let ([c (peek-char p)])
	    (cond
	     [(eof-object? c) ;;; @<cmd><eof>
	      (values a a^ locs k)]
	     [(eqv? c #\[)
	      (read-char p)
	      (let-values ([(a* a*^ locs k)
			    (read-brackets p locs k)])
		(let ([v (cons a a*)] [v^ (cons a^ a*^)])
		  (values v (annotate v v^ at-pos p) locs k)))]
	     [(eqv? c #\{) ;;; @<cmd>{ ...
	      (read-char p)
	      (let-values ([(a* a*^ locs k)
			    (read-text p locs k '())])
		(let ([v (cons a a*)] [v^ (cons a^ a*^)])
		  (values v (annotate v v^ at-pos p) locs k)))]
	     [(eqv? c #\|) ;;; @<cmd>| ...
	      (read-char p)
	      (let-values ([(a* a*^ locs k)
			    (read-at-bar p locs k #f)])
		(let ([v (cons a a*)] [v^ (cons a^ a*^)])
		  (values v (annotate v v^ at-pos p) locs k)))]
	     [else
	      (values a a^ locs k)])))])))

  (read-at-sexpr-mode p locs k))


(define (parse-token p locs k t pos)
  (cond
   [(eof-object? t)
    (values (eof-object)
	    (annotate-simple (eof-object) pos p) locs k)]
   [(eq? t 'lparen)
    (let-values ([(ls ls^ locs k)
		  (read-list p locs k 'rparen 'rbrack #t)])
      (values ls (annotate ls ls^ pos p) locs k))]
   [(eq? t 'lbrack)
    (let-values ([(ls ls^ locs k)
		  (read-list p locs k 'rbrack 'rparen #t)])
      (values ls (annotate ls ls^ pos p) locs k))]
   [(eq? t 'vparen)
    (let-values ([(v v^ locs k)
		  (read-vector p locs k 0 '() '())])
      (values v (annotate v v^ pos p) locs k))]
   [(eq? t 'vu8)
    (let-values ([(v v^ locs k)
		  (read-u8-bytevector p locs k 0 '())])
      (values v (annotate v v^ pos p) locs k))]
   [(eq? t 'vs8)
    (let-values ([(v v^ locs k)
		  (read-s8-bytevector p locs k 0 '())])
      (values v (annotate v v^ pos p) locs k))]
   [(eq? t 'at-expr)
    (read-at-expr p locs k pos)]
   [(pair? t)
    (cond
     [(eq? (car t) 'datum)
      (values (cdr t)
	      (annotate-simple (cdr t) pos p) locs k)]
     [(eq? (car t) 'macro)
      (let ([macro (cdr t)])
	(define (read-macro)
	  (let-values ([(t pos) (tokenize/1+pos p)])
	    (cond
	     [(eof-object? t)
	      (die/p p 'read
		     (format "invalid eof after ~a read macro"
		       macro))]
	     [else (parse-token p locs k t pos)])))
	(let-values ([(expr expr^ locs k) (read-macro)])
	  (let ([d (list expr)] [d^ (list expr^)])
	    (let ([x (cons macro d)]
		  [x^ (cons (annotate-simple macro pos p) d^)])
	      (values x (annotate x x^ pos p) locs
		      (extend-k-pair d d^ expr '() k))))))]
     [(eq? (car t) 'mark)
      (let ([n (cdr t)])
	(let-values ([(expr expr^ locs k)
		      (read-expr p locs k)])
	  (cond
	   [(assq n locs) =>
	    (lambda (x)
	      (let ([loc (cdr x)])
		(when (loc-set? loc) ;;; FIXME: pos
		  (die/p p 'read "duplicate mark" n))
		(set-loc-value! loc expr)
		(set-loc-value^! loc expr^)
		(set-loc-set?! loc #t)
		(values expr expr^ locs k)))]
	   [else
	    (let ([loc (make-loc expr 'unused #t)])
	      (let ([locs (cons (cons n loc) locs)])
		(values expr expr^ locs k)))])))]
     [(eq? (car t) 'ref)
      (let ([n (cdr t)])
	(cond
	 [(assq n locs) =>
	  (lambda (x)
	    (values (cdr x) 'unused locs k))]
	 [else
	  (let ([loc (make-loc #f 'unused #f)])
	    (let ([locs (cons (cons n loc) locs)])
	      (values loc 'unused locs k)))]))]
     [else (die/p p 'read "invalid token" t)])]
   [else
    (die/p-1 p 'read (format "unexpected ~s found" t))]))

(define read-expr
  (lambda (p locs k)
    (let-values ([(t pos) (tokenize/1+pos p)])
      (parse-token p locs k t pos))))

(define read-expr-script-initial
  (lambda (p locs k)
    (let-values ([(t pos) (tokenize-script-initial+pos p)])
      (parse-token p locs k t pos))))

#| end of module |# )


(define (reduce-loc! p)
  (lambda (x)
    (let ([loc (cdr x)])
      (unless (loc-set? loc)
	(die/p p 'read "referenced mark is not set" (car x)))
      (when (loc? (loc-value loc))
	(let f ([h loc] [t loc])
	  (if (loc? h)
	      (let ([h1 (loc-value h)])
		(if (loc? h1)
		    (begin
		      (when (eq? h1 t)
			(die/p p 'read "circular marks"))
		      (let ([v (f (loc-value h1) (loc-value t))])
			(set-loc-value! h1 v)
			(set-loc-value! h v)
			v))
		  (begin
		    (set-loc-value! h h1)
		    h1)))
	    h))))))

(define (read-as-comment p)
  (begin (read-expr p '() void) (void)))

(define (return-annotated x)
  (cond
   [(and (annotation? x) (eof-object? (annotation-expression x)))
    (eof-object)]
   [else x]))

(define my-read
  (lambda (p)
    (let-values ([(expr expr^ locs k) (read-expr p '() void)])
      (cond
       [(null? locs) expr]
       [else
	(for-each (reduce-loc! p) locs)
	(k)
	(if (loc? expr)
	    (loc-value expr)
	  expr)]))))

(define read-initial
  (lambda (p)
    (let-values ([(expr expr^ locs k) (read-expr-script-initial p '() void)])
      (cond
       [(null? locs) expr]
       [else
	(for-each (reduce-loc! p) locs)
	(k)
	(if (loc? expr)
	    (loc-value expr)
	  expr)]))))

(define read-annotated
  (case-lambda
   [(p)
    (unless (input-port? p)
      (error 'read-annotated "not an input port" p))
    (let-values ([(expr expr^ locs k) (read-expr p '() void)])
      (cond
       [(null? locs) (return-annotated expr^)]
       [else
	(for-each (reduce-loc! p) locs)
	(k)
	(if (loc? expr)
	    (loc-value^ expr)
	  (return-annotated expr^))]))]
   [() (read-annotated (current-input-port))]))

(define read-script-annotated
  (lambda (p)
    (let-values ([(expr expr^ locs k) (read-expr-script-initial p '() void)])
      (cond
       [(null? locs) (return-annotated expr^)]
       [else
	(for-each (reduce-loc! p) locs)
	(k)
	(if (loc? expr)
	    (loc-value^ expr)
	  (return-annotated expr^))]))))

(define read-token
  (case-lambda
   [() (tokenize/1 (current-input-port))]
   [(p)
    (if (input-port? p)
	(tokenize/1 p)
      (die 'read-token "not an input port" p))]))

(define read
  (case-lambda
   [() (my-read (current-input-port))]
   [(p)
    (if (input-port? p)
	(my-read p)
      (die 'read "not an input port" p))]))

(define (get-datum p)
  (unless (input-port? p)
    (die 'get-datum "not an input port"))
  (my-read p))

(define comment-handler
    ;;; this is stale, maybe delete
  (make-parameter
      (lambda (x) (void))
    (lambda (x)
      (unless (procedure? x)
	(die 'comment-handler "not a procedure" x))
      x)))


;;;; done

)

;;; end of file
