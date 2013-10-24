;;;
;;;Part of: Vicare Scheme
;;;Contents: SILex 1.0 - Scheme Implementation of Lex
;;;Date: Fri Jul 17, 2009
;;;
;;;Abstract
;;;
;;;	SILex stands for "Scheme Implementation of Lex".  It generates a
;;;	Scheme  lexical analyser  from a  Lex--like  specification file.
;;;	Notice  that  the  original  distribution  of  SILex  should  be
;;;	included in  the Nausicaa  source tree, under  the "src/foreign"
;;;	directory.
;;;
;;;	  Quick guide to browsing the code:
;;;
;;;	LEX - This is the main function to produce the lexer table.
;;;
;;;	OUTPUT - This  function is the main output  function.  It parses
;;;	the options given to LEX and produces a proper Scheme library or
;;;	just  the lexer  table.   The output  can  be saved  to a  file,
;;;	printed to a supplied port or (only for the raw table) evaluated
;;;	and returned as a Scheme value.
;;;
;;;	OUT-PRINT-TABLE  - This  function prints  the lexer  table  to a
;;;	given  output  port.   This  function  is the  one  to  read  to
;;;	understand the basic format of lexer tables.
;;;
;;;Copyright (c) 2001 Danny Dube' <dube@iro.umontreal.ca>
;;;
;;;Original code  by Danny Dube'.   Port to R6RS Scheme  and integration
;;;into Vicare by Marco Maggi.
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
(library (vicare parser-tools silex)
  (export
    lex

    ;; auxiliary syntaxes
    input-file:
    input-port:
    input-string:
    library-spec:
    library-language:
    library-imports:
    table-name:
    pretty-print:
    counters:
    lexer-format:
    output-value:
    output-file:
    output-port:)
  (import (vicare)
    (vicare parser-tools silex lexer)
    (vicare parser-tools silex semantic)
    (vicare parser-tools silex action-l)
    (vicare parser-tools silex class-l)
    (vicare parser-tools silex macro-l)
    (vicare parser-tools silex regexp-l)
    (vicare parser-tools silex string-l)
    (vicare parser-tools silex nested-comment-l)
    (prefix (vicare language-extensions makers) mk.)
    (vicare arguments validation)
    (vicare $posix))


;;;; helpers

(define (lex-error who line column . message-strings)
  (assertion-violation who
    (apply string-append "lex error in "
	   (or (string-append "\"" (input-source) "\"") "anonymous source")
	   " line "   (if line   (number->string line)   "?")
	   " column " (if column (number->string column) "?")
	   ": " message-strings)))

(define-syntax with-context-to-lex-file
  (syntax-rules ()
    ((_ ?IS . ?body)
     (let ((IS ?IS))
       (parametrise ((action-lexer		(make-lexer action-tables IS))
		     (class-lexer		(make-lexer class-tables  IS))
		     (macro-lexer		(make-lexer macro-tables  IS))
		     (regexp-lexer		(make-lexer regexp-tables IS))
		     (string-lexer		(make-lexer string-tables IS))
		     (nested-comment-lexer	(make-lexer nested-comment-tables IS))
		     (lexer-raw			#f)
		     (lexer-stack		'())
		     (lexer-buffer-empty?	#t)
		     (lexer-buffer		#f)
		     (lexer-history		'())
		     (lexer-history-interp	#f))
	 . ?body)))))

(define (big-string-append strings)
  (let-values (((port getter) (open-string-output-port)))
    (for-each (lambda (S)
		(display S port))
      strings)
    (getter)))


;;;; module main.scm

(define-auxiliary-syntaxes
  input-file:
  input-port:
  input-string:
  library-spec:
  library-language:
  library-imports:
  table-name:
  pretty-print:
;;;  counters:		;already defined in (vicare parser-tools silex lexer)
  lexer-format:
  output-value:
  output-file:
  output-port:)

(mk.define-maker lex
    %lex
  ((input-file:		#f	(mk.without input-port:   input-string:))
   (input-port:		#f	(mk.without input-string: input-file:))
   (input-string:	#f	(mk.without input-file:   input-port:))

   (library-spec:	#f	(mk.with table-name:))
   (library-language:	'(rnrs))
   (library-imports:	'())
   (table-name:		#f)
   (pretty-print:	#f)
   (counters:		'line)
   (lexer-format:	'decision-tree)

   (output-value:	#f	(mk.without output-file:  output-port:  table-name:))
   (output-file:	#f	(mk.without output-port:  output-value:))
   (output-port:	#f	(mk.without output-value: output-file:))))

(define (%lex input-file input-port input-string
	      library-spec library-language library-imports
	      table-name pretty? counters-type lexer-format
	      output-value output-file output-port)

  (when (and library-spec (not table-name))
    (assertion-violation 'lex
      "missing table name but library specification given, cannot create library"))
  (when (and output-value table-name)
    (assertion-violation 'lex
      "requested output as value, but a table name was also given"))
  (let ((close-port? #f))
    (dynamic-wind
	(lambda ()
	  (set! input-port
		(cond (input-string (open-string-input-port input-string))
		      (input-port   input-port)
		      (input-file   (begin0
					(open-input-file input-file)
				      (set! close-port? #t)))
		      (else
		       (assertion-violation 'lex
			 "missing input method for lexer")))))
	(lambda ()
	  (let ((IS (make-IS (port: input-port) (counters: 'all))))
	    (parametrise ((input-source		(or input-file #f))
			  (include-files	(if input-file `(,input-file) '())))
	      (with-context-to-lex-file IS
		(let-values (((<<EOF>>-action <<ERROR>>-action rules)
			      (adapt-rules (parse-rules (parse-macros)))))

		  (let-values (((nl-start no-nl-start arcs acc)
				(re2nfa rules)))

		    (let-values (((nl-start no-nl-start arcs acc)
				  (noeps nl-start no-nl-start arcs acc)))

		      (let-values (((nl-start no-nl-start arcs acc)
				    (sweep nl-start no-nl-start arcs acc)))

			(let-values (((nl-start no-nl-start arcs acc)
				      (nfa2dfa nl-start no-nl-start arcs acc)))
			  (prep-set-rules-yytext? rules)
			  (output input-file
				  library-spec library-language library-imports
				  table-name pretty? counters-type lexer-format
				  output-file output-port output-value
				  <<EOF>>-action <<ERROR>>-action
				  rules nl-start no-nl-start arcs acc))))))))))
	(lambda ()
	  (when close-port?
	    (close-input-port input-port))))))


;;;; module util.scm

(define (make-dispatch-table size alist default)
  ;;Fabrication de tables de dispatch.
  ;;
  (let ((v (make-vector size default)))
    (let loop ((alist alist))
      (if (null? alist)
	  v
	(begin
	  (vector-set! v (caar alist) (cdar alist))
	  (loop (cdr alist)))))))


;;; Fonctions de manipulations des regles

(define-record-type <rule>
  (nongenerative vicare:parser-tools:silex:<rule>)
  (protocol (lambda (make-rule)
	      (lambda (line eof? error? bol? eol? regexp action)
		(make-rule line eof? error? bol? eol? regexp action #f))))
  (fields (immutable line)
	  (immutable eof?)
	  (immutable error?)
	  (immutable bol?)
	  (immutable eol?)
	  (mutable regexp)
	  (mutable action)
	  (mutable yytext?)))


;;; Noeuds des regexp

(define-record-type <regexp>
  (nongenerative vicare:parser-tools:silex:<regexp>)
  (protocol (lambda (make-regexp)
	      (case-lambda
	       ((re-type)
		(make-regexp re-type #f #f))
	       ((re-type attr1)
		(make-regexp re-type attr1 #f))
	       ((re-type attr1 attr2)
		(make-regexp re-type attr1 attr2)))))
  (fields (immutable type)
	  (immutable attr1)
	  (immutable attr2)))

;;The following are used as indexes into vectors.
(define-constant epsilon-re  0)
(define-constant or-re       1)
(define-constant conc-re     2)
(define-constant star-re     3)
(define-constant plus-re     4)
(define-constant question-re 5)
(define-constant class-re    6)
(define-constant char-re     7)

(define (regexp=? a b)
  (define (== x y)
    (cond ((<regexp>? x)
	   (and (<regexp>? y) (regexp=?  x y)))
	  ((boolean? x)
	   (and (boolean? y)  (boolean=? x y)))
	  ((integer? x)
	   (and (integer? y)  (=         x y)))
	  ((pair? x)
	   (and (pair? y)     (ell=?     x y)))
	  ((null? x)
	   (null? y))
	  (else
	   (= a b))))
  (define (ell=? p q)
    (cond ((null? p)
	   (null? q))
	  ((pair? p)
	   (and (pair? q)
		(ell=? (car p) (car q))
		(ell=? (cdr p) (cdr q))))
	  ((<regexp>? p)
	   (and (<regexp>? q) (regexp=? p q)))
	  (else
	   (= p q))))
  (and (=  (<regexp>-type  a) (<regexp>-type  b))
       (== (<regexp>-attr1 a) (<regexp>-attr1 b))
       (== (<regexp>-attr2 a) (<regexp>-attr2 b))))


;;; Fonctions de manipulation des ensembles d'etats

; Intersection de deux ensembles d'etats
(define (ss-inter ss1 ss2)
  (cond ((null? ss1)
	 '())
	((null? ss2)
	 '())
	(else
	 (let ((t1 (car ss1))
	       (t2 (car ss2)))
	   (cond ((< t1 t2)
		  (ss-inter (cdr ss1) ss2))
		 ((= t1 t2)
		  (cons t1 (ss-inter (cdr ss1) (cdr ss2))))
		 (else
		  (ss-inter ss1 (cdr ss2))))))))

; Difference entre deux ensembles d'etats
(define (ss-diff ss1 ss2)
  (cond ((null? ss1)
	 '())
	((null? ss2)
	 ss1)
	(else
	 (let ((t1 (car ss1))
	       (t2 (car ss2)))
	   (cond ((< t1 t2)
		  (cons t1 (ss-diff (cdr ss1) ss2)))
		 ((= t1 t2)
		  (ss-diff (cdr ss1) (cdr ss2)))
		 (else
		  (ss-diff ss1 (cdr ss2))))))))

; Union de deux ensembles d'etats
(define (ss-union ss1 ss2)
  (cond ((null? ss1)
	 ss2)
	((null? ss2)
	 ss1)
	(else
	 (let ((t1 (car ss1))
	       (t2 (car ss2)))
	   (cond ((< t1 t2)
		  (cons t1 (ss-union (cdr ss1) ss2)))
		 ((= t1 t2)
		  (cons t1 (ss-union (cdr ss1) (cdr ss2))))
		 (else
		  (cons t2 (ss-union ss1 (cdr ss2)))))))))

; Decoupage de deux ensembles d'etats
(define (ss-sep ss1 ss2)
  (let loop ((ss1 ss1) (ss2 ss2) (l '()) (c '()) (r '()))
    (if (null? ss1)
	(if (null? ss2)
	    (vector (reverse l) (reverse c) (reverse r))
	  (loop ss1 (cdr ss2) l c (cons (car ss2) r)))
      (if (null? ss2)
	  (loop (cdr ss1) ss2 (cons (car ss1) l) c r)
	(let ((t1 (car ss1))
	      (t2 (car ss2)))
	  (cond ((< t1 t2)
		 (loop (cdr ss1) ss2 (cons t1 l) c r))
		((= t1 t2)
		 (loop (cdr ss1) (cdr ss2) l (cons t1 c) r))
		(else
		 (loop ss1 (cdr ss2) l c (cons t2 r)))))))))


;;;; Fonctions de manipulation des classes de caracteres
;;
;;FIXME Are these "classes of characters" functionally equivalent to the
;;char-sets?
;;

;; Comparaisons de bornes d'intervalles
(define class-= eqv?)

(define (class-<= b1 b2)
  (cond ((eq? b1 'inf-) #t)
	((eq? b2 'inf+) #t)
	((eq? b1 'inf+) #f)
	((eq? b2 'inf-) #f)
	(else (<= b1 b2))))

(define (class->= b1 b2)
  (cond ((eq? b1 'inf+) #t)
	((eq? b2 'inf-) #t)
	((eq? b1 'inf-) #f)
	((eq? b2 'inf+) #f)
	(else (>= b1 b2))))

(define (class-< b1 b2)
  (cond ((eq? b1 'inf+) #f)
	((eq? b2 'inf-) #f)
	((eq? b1 'inf-) #t)
	((eq? b2 'inf+) #t)
	(else (< b1 b2))))

(define (class-> b1 b2)
  (cond ((eq? b1 'inf-) #f)
	((eq? b2 'inf+) #f)
	((eq? b1 'inf+) #t)
	((eq? b2 'inf-) #t)
	(else (> b1 b2))))

(define (class-compl c)
  ;;Complementation d'une classe.
  ;;
  (let loop ((c c) (start 'inf-))
    (if (null? c)
	(list (cons start 'inf+))
      (let* ((r (car c))
	     (rstart (car r))
	     (rend (cdr r)))
	(if (class-< start rstart)
	    (cons (cons start (- rstart 1))
		  (loop c rstart))
	  (if (class-< rend 'inf+)
	      (loop (cdr c) (+ rend 1))
	    '()))))))

(define (class-union c1 c2)
  ;;Union de deux classes de caracteres.
  ;;
  (let loop ((c1 c1) (c2 c2) (u '()))
    (if (null? c1)
	(if (null? c2)
	    (reverse u)
	  (loop c1 (cdr c2) (cons (car c2) u)))
      (if (null? c2)
	  (loop (cdr c1) c2 (cons (car c1) u))
	(let* ((r1 (car c1))
	       (r2 (car c2))
	       (r1start (car r1))
	       (r1end (cdr r1))
	       (r2start (car r2))
	       (r2end (cdr r2)))
	  (if (class-<= r1start r2start)
	      (cond ((class-= r1end 'inf+)
		     (loop c1 (cdr c2) u))
		    ((class-< (+ r1end 1) r2start)
		     (loop (cdr c1) c2 (cons r1 u)))
		    ((class-<= r1end r2end)
		     (loop (cdr c1)
			   (cons (cons r1start r2end) (cdr c2))
			   u))
		    (else
		     (loop c1 (cdr c2) u)))
	    (cond ((class-= r2end 'inf+)
		   (loop (cdr c1) c2 u))
		  ((class-> r1start (+ r2end 1))
		   (loop c1 (cdr c2) (cons r2 u)))
		  ((class->= r1end r2end)
		   (loop (cons (cons r2start r1end) (cdr c1))
			 (cdr c2)
			 u))
		  (else
		   (loop (cdr c1) c2 u)))))))))

(define (class-sep c1 c2)
  ;;Decoupage de deux classes de caracteres.
  ;;
  (let loop ((c1 c1) (c2 c2) (l '()) (c '()) (r '()))
    (if (null? c1)
	(if (null? c2)
	    (vector (reverse l) (reverse c) (reverse r))
	  (loop c1 (cdr c2) l c (cons (car c2) r)))
      (if (null? c2)
	  (loop (cdr c1) c2 (cons (car c1) l) c r)
	(let* ((r1 (car c1))
	       (r2 (car c2))
	       (r1start (car r1))
	       (r1end (cdr r1))
	       (r2start (car r2))
	       (r2end (cdr r2)))
	  (cond ((class-< r1start r2start)
		 (if (class-< r1end r2start)
		     (loop (cdr c1) c2 (cons r1 l) c r)
		   (loop (cons (cons r2start r1end) (cdr c1)) c2
			 (cons (cons r1start (- r2start 1)) l) c r)))
		((class-> r1start r2start)
		 (if (class-> r1start r2end)
		     (loop c1 (cdr c2) l c (cons r2 r))
		   (loop c1 (cons (cons r1start r2end) (cdr c2))
			 l c (cons (cons r2start (- r1start 1)) r))))
		(else
		 (cond ((class-< r1end r2end)
			(loop (cdr c1)
			      (cons (cons (+ r1end 1) r2end) (cdr c2))
			      l (cons r1 c) r))
		       ((class-= r1end r2end)
			(loop (cdr c1) (cdr c2) l (cons r1 c) r))
		       (else
			(loop (cons (cons (+ r2end 1) r1end) (cdr c1))
			      (cdr c2)
			      l (cons r2 c) r))))))))))

(define (class->char-list c)
  ;;Transformer une classe (finie) de caracteres en une liste de ...
  ;;
  (let loop1 ((c c))
    (if (null? c)
	'()
      (let* ((r (car c))
	     (rend (cdr r))
	     (tail (loop1 (cdr c))))
	(let loop2 ((rstart (car r)))
	  (if (<= rstart rend)
	      (cons (integer->char rstart) (loop2 (+ rstart 1)))
	    tail))))))

(define (class->tagged-char-list c)
  ;;Transformer une classe de caracteres  en une liste poss. compl.
  ;;
  ;;1er element = #t -> classe complementee
  ;;
  (let* ((finite? (or (null? c) (number? (caar c))))
	 (c2 (if finite? c (class-compl c)))
	 (c-l (class->char-list c2)))
    (cons (not finite?) c-l)))


;;; Fonction digraph

; Fonction "digraph".
; Etant donne un graphe dirige dont les noeuds comportent une valeur,
; calcule pour chaque noeud la "somme" des valeurs contenues dans le
; noeud lui-meme et ceux atteignables a partir de celui-ci.  La "somme"
; consiste a appliquer un operateur commutatif et associatif aux valeurs
; lorsqu'elles sont additionnees.
; L'entree consiste en un vecteur de voisinages externes, un autre de
; valeurs initiales et d'un operateur.
; La sortie est un vecteur de valeurs finales.
(define (digraph arcs init op)
  (let* ((nbnodes (vector-length arcs))
	 (infinity nbnodes)
	 (prio (make-vector nbnodes -1))
	 (stack (make-vector nbnodes #f))
	 (sp 0)
	 (final (make-vector nbnodes #f)))
    (letrec ((store-final
	      (lambda (self-sp value)
		(let loop ()
		  (when (> sp self-sp)
		    (let ((voisin (vector-ref stack (- sp 1))))
		      (vector-set! prio voisin infinity)
		      (set! sp (- sp 1))
		      (vector-set! final voisin value)
		      (loop))))))
	     (visit-node
	      (lambda (n)
		(let ((self-sp sp))
		  (vector-set! prio n self-sp)
		  (vector-set! stack sp n)
		  (set! sp (+ sp 1))
		  (vector-set! final n (vector-ref init n))
		  (let loop ((vois (vector-ref arcs n)))
		    (when (pair? vois)
		      (let* ((v (car vois))
			     (vprio (vector-ref prio v)))
			(if (= vprio -1)
			    (visit-node v))
			(vector-set! prio n (min (vector-ref prio n)
						 (vector-ref prio v)))
			(vector-set! final n (op (vector-ref final n)
						 (vector-ref final v)))
			(loop (cdr vois)))))
		  (when (= (vector-ref prio n) self-sp)
		    (store-final self-sp (vector-ref final n)))))))
      (let loop ((n 0))
	(when (< n nbnodes)
	  (when (= (vector-ref prio n) -1)
	    (visit-node n))
	  (loop (+ n 1))))
      final)))


;;; Fonction de tri

(define (merge-sort-merge l1 l2 cmp-<=)
  (cond ((null? l1)	l2)
	((null? l2)	l1)
	(else
	 (let ((h1 (car l1))
	       (h2 (car l2)))
	   (if (cmp-<= h1 h2)
	       (cons h1 (merge-sort-merge (cdr l1) l2 cmp-<=))
	     (cons h2 (merge-sort-merge l1 (cdr l2) cmp-<=)))))))

(define (merge-sort l cmp-<=)
  (if (null? l)
      l
    (let loop1 ((ll (map list l)))
      (if (null? (cdr ll))
	  (car ll)
	(loop1
	 (let loop2 ((ll ll))
	   (cond ((null? ll)		ll)
		 ((null? (cdr ll))	ll)
		 (else
		  (cons (merge-sort-merge (car ll) (cadr ll) cmp-<=)
			(loop2 (cddr ll)))))))))))


;;Whenever a SILex table or macro file is parsed, this parameter must be
;;set to #f or a string representing a human readable description of the
;;input source.  It is used to report errors.
(define input-source	(make-parameter #f))

(define action-lexer	(make-parameter #f))
(define class-lexer	(make-parameter #f))
(define macro-lexer	(make-parameter #f))
(define regexp-lexer	(make-parameter #f))
(define string-lexer	(make-parameter #f))
(define nested-comment-lexer	(make-parameter #f))

;;Values of this  parameter are lists of macro  file pathnames; they are
;;used to detect recursive inclusion of files.
(define include-files	(make-parameter #f))


;;;; lexer generique

(define lexer-raw
  (make-parameter #f))

(define lexer-stack
  (make-parameter '()))

(define lexer-buffer
  (make-parameter #f))

(define lexer-buffer-empty?
  (make-parameter #t))

(define lexer-history
  (make-parameter '()))

(define lexer-history-interp
  (make-parameter #f))

; Lexer brut
; S'assurer qu'il n'y a pas de risque de changer de
; lexer quand le buffer est rempli
(define (push-lexer lexer)
  (lexer-stack (cons (lexer-raw) (lexer-stack)))
  (lexer-raw   lexer))

(define (pop-lexer)
  (lexer-raw   (car (lexer-stack)))
  (lexer-stack (cdr (lexer-stack))))

; Traite le "unget" (capacite du unget: 1)
(define (lexer2)
  (if (lexer-buffer-empty?)
      ((lexer-raw))
    (begin
      (lexer-buffer-empty? #t)
      (lexer-buffer))))

(define (lexer2-unget tok)
  (lexer-buffer tok)
  (lexer-buffer-empty? #f))

; Traite l'historique
(define (lexer)
  (let* ((tok (lexer2))
	 (tok-lexeme (get-tok-lexeme tok))
	 (hist-lexeme (if (lexer-history-interp)
			  (blank-translate tok-lexeme)
			tok-lexeme)))
    (lexer-history (cons hist-lexeme (lexer-history)))
    tok))

(define (lexer-unget tok)
  (lexer-history (cdr (lexer-history)))
  (lexer2-unget tok))

(define (lexer-set-blank-history b)
  (lexer-history-interp b))

(define (blank-translate s)
  (let ((ss (string-copy s)))
    (let loop ((i (- (string-length ss) 1)))
      (cond ((< i 0)
	     ss)
	    ((char=? (string-ref ss i) (integer->char tab-ch))
	     (loop (- i 1)))
	    ((char=? (string-ref ss i) #\newline)
	     (loop (- i 1)))
	    (else
	     (string-set! ss i #\space)
	     (loop (- i 1)))))))

(define (lexer-get-history)
  (let* ((rightlist (reverse (lexer-history)))
	 (str (big-string-append rightlist))
	 (strlen (string-length str))
	 (str2 (if (and (> strlen 0)
			(char=? (string-ref str (- strlen 1)) #\newline))
		   str
		 (string-append str "\n"))))
    (lexer-history '())
    str2))


;;;; traitement des listes de tokens

(define de-anchor-tokens
  (let ((not-anchor-toks (make-dispatch-table number-of-tokens
					      (list (cons caret-tok     #f)
						    (cons dollar-tok    #f)
						    (cons <<EOF>>-tok   #f)
						    (cons <<ERROR>>-tok #f))
					      #t)))
    (lambda (tok-list)
      (if (null? tok-list)
	  '()
	(let* ((tok (car tok-list))
	       (tok-type (get-tok-type tok))
	       (toks (cdr tok-list))
	       (new-toks (de-anchor-tokens toks)))
	  (cond ((vector-ref not-anchor-toks tok-type)
		 (cons tok new-toks))
		((or (= tok-type caret-tok) (= tok-type dollar-tok))
		 (let* ((line (get-tok-line tok))
			(column (get-tok-column tok))
			(attr (if (= tok-type caret-tok) caret-ch dollar-ch))
			(new-tok (make-tok char-tok "" line column attr)))
		   (cons new-tok new-toks)))
		((= tok-type <<EOF>>-tok)
		 (lex-error 'lex:de-anchor-tokens (get-tok-line tok) (get-tok-column tok)
			    "the <<EOF>> anchor must be used alone and only after %%"))
		((= tok-type <<ERROR>>-tok)
		 (lex-error 'lex:de-anchor-tokens (get-tok-line tok) (get-tok-column tok)
			    "the <<ERROR>> anchor must be used alone and only after %%"))))))))

(define (strip-end l)
  (if (null? (cdr l))
      '()
    (cons (car l) (strip-end (cdr l)))))

(define (extract-anchors tok-list)
  (let* ((tok1 (car tok-list))
	 (line (get-tok-line tok1))
	 (tok1-type (get-tok-type tok1)))
    (cond ((and (= tok1-type <<EOF>>-tok) (null? (cdr tok-list)))
	   (make-<rule> line #t #f #f #f '() #f))
	  ((and (= tok1-type <<ERROR>>-tok) (null? (cdr tok-list)))
	   (make-<rule> line #f #t #f #f '() #f))
	  (else
	   (let* ((bol? (= tok1-type caret-tok))
		  (tok-list2 (if bol? (cdr tok-list) tok-list)))
	     (if (null? tok-list2)
		 (make-<rule> line #f #f bol? #f tok-list2 #f)
	       (let* ((len (length tok-list2))
		      (tok2 (list-ref tok-list2 (- len 1)))
		      (tok2-type (get-tok-type tok2))
		      (eol? (= tok2-type dollar-tok))
		      (tok-list3 (if eol?
				     (strip-end tok-list2)
				   tok-list2)))
		 (make-<rule> line #f #f bol? eol? tok-list3 #f))))))))

(define (char-list->conc char-list)
  (if (null? char-list)
      (make-<regexp> epsilon-re)
    (let loop ((cl char-list))
      (let* ((c (car cl))
	     (cl2 (cdr cl)))
	(if (null? cl2)
	    (make-<regexp> char-re c)
	  (make-<regexp> conc-re (make-<regexp> char-re c) (loop cl2)))))))

(define parse-tokens-atom
  (let ((action-table
	 (make-dispatch-table
	  number-of-tokens
	  (list (cons lpar-tok
		      (lambda (tok tok-list macros)
			(parse-tokens-sub tok-list macros)))
		(cons dot-tok
		      (lambda (tok tok-list macros)
			(cons (make-<regexp> class-re dot-class) (cdr tok-list))))
		(cons subst-tok
		      (lambda (tok tok-list macros)
			(let* ((name (get-tok-attr tok))
			       (ass (assoc name macros)))
			  (if ass
			      (cons (cdr ass) (cdr tok-list))
			    (lex-error 'lex:parse-tokens-atom
				       (get-tok-line tok) (get-tok-column tok)
				       "unknown macro \"" (get-tok-2nd-attr tok) "\"")))))
		(cons char-tok
		      (lambda (tok tok-list macros)
			(let ((c (get-tok-attr tok)))
			  (cons (make-<regexp> char-re c) (cdr tok-list)))))
		(cons class-tok
		      (lambda (tok tok-list macros)
			(let ((class (get-tok-attr tok)))
			  (cons (make-<regexp> class-re class) (cdr tok-list)))))
		(cons string-tok
		      (lambda (tok tok-list macros)
			(let* ((char-list (get-tok-attr tok))
			       (re (char-list->conc char-list)))
			  (cons re (cdr tok-list))))))
	  (lambda (tok tok-list macros)
	    (lex-error 'lex:parse-tokens-atom (get-tok-line tok) (get-tok-column tok)
		       "syntax error in regular expression")))))
    (lambda (tok-list macros)
      (let* ((tok (car tok-list))
	     (tok-type (get-tok-type tok))
	     (action (vector-ref action-table tok-type)))
	(action tok tok-list macros)))))

(define (check-power-tok tok)
  (let* ((range (get-tok-attr tok))
	 (start (car range))
	 (end (cdr range)))
    (if (or (eq? 'inf end) (<= start end))
	range
      (lex-error 'lex:check-power-tok (get-tok-line tok) (get-tok-column tok)
		 "incorrect power specification"))))

(define (power->star-plus re range)
  (power->star-plus-rec re (car range) (cdr range)))

(define (power->star-plus-rec re start end)
  (cond ((eq? end 'inf)
	 (cond ((= start 0)
		(make-<regexp> star-re re))
	       ((= start 1)
		(make-<regexp> plus-re re))
	       (else
		(make-<regexp> conc-re
			 re
			 (power->star-plus-rec re (- start 1) 'inf)))))
	((= start 0)
	 (cond ((= end 0)
		(make-<regexp> epsilon-re))
	       ((= end 1)
		(make-<regexp> question-re re))
	       (else
		(make-<regexp> question-re
			 (power->star-plus-rec re 1 end)))))
	((= start 1)
	 (if (= end 1)
	     re
	   (make-<regexp> conc-re re (power->star-plus-rec re 0 (- end 1)))))
	(else
	 (make-<regexp> conc-re
		  re
		  (power->star-plus-rec re (- start 1) (- end 1))))))

(define parse-tokens-fact
  (let ((not-op-toks (make-dispatch-table number-of-tokens
					  (list (cons question-tok #f)
						(cons plus-tok     #f)
						(cons star-tok     #f)
						(cons power-tok    #f))
					  #t)))
    (lambda (tok-list macros)
      (let* ((result (parse-tokens-atom tok-list macros))
	     (re (car result))
	     (tok-list2 (cdr result)))
	(let loop ((re re) (tok-list3 tok-list2))
	  (let* ((tok (car tok-list3))
		 (tok-type (get-tok-type tok)))
	    (cond ((vector-ref not-op-toks tok-type)
		   (cons re tok-list3))
		  ((= tok-type question-tok)
		   (loop (make-<regexp> question-re re) (cdr tok-list3)))
		  ((= tok-type plus-tok)
		   (loop (make-<regexp> plus-re re) (cdr tok-list3)))
		  ((= tok-type star-tok)
		   (loop (make-<regexp> star-re re) (cdr tok-list3)))
		  ((= tok-type power-tok)
		   (loop (power->star-plus re (check-power-tok tok))
			 (cdr tok-list3))))))))))

(define (parse-tokens-conc tok-list macros)
  (let* ((result1 (parse-tokens-fact tok-list macros))
	 (re1 (car result1))
	 (tok-list2 (cdr result1))
	 (tok (car tok-list2))
	 (tok-type (get-tok-type tok)))
    (cond ((or (= tok-type pipe-tok)
	       (= tok-type rpar-tok))
	   result1)
	  (else ; Autres facteurs
	   (let* ((result2 (parse-tokens-conc tok-list2 macros))
		  (re2 (car result2))
		  (tok-list3 (cdr result2)))
	     (cons (make-<regexp> conc-re re1 re2) tok-list3))))))

(define (parse-tokens-or tok-list macros)
  (let* ((result1 (parse-tokens-conc tok-list macros))
	 (re1 (car result1))
	 (tok-list2 (cdr result1))
	 (tok (car tok-list2))
	 (tok-type (get-tok-type tok)))
    (cond ((= tok-type pipe-tok)
	   (let* ((tok-list3 (cdr tok-list2))
		  (result2 (parse-tokens-or tok-list3 macros))
		  (re2 (car result2))
		  (tok-list4 (cdr result2)))
	     (cons (make-<regexp> or-re re1 re2) tok-list4)))
	  (else ; rpar-tok
	   result1))))

(define (parse-tokens-sub tok-list macros)
  (let* ((tok-list2 (cdr tok-list)) ; Manger le lpar-tok
	 (result (parse-tokens-or tok-list2 macros))
	 (re (car result))
	 (tok-list3 (cdr result))
	 (tok-list4 (cdr tok-list3))) ; Manger le rpar-tok
    (cons re tok-list4)))

(define (parse-tokens-match tok-list line)
  (define (%error)
    (lex-error 'lex:parse-tokens-match line #f "mismatched parentheses"))
  (let loop ((tl tok-list) (count 0))
    (if (null? tl)
	(when (> count 0)
	  (%error))
      (let* ((tok (car tl))
	     (tok-type (get-tok-type tok)))
	(cond ((= tok-type lpar-tok)
	       (loop (cdr tl) (+ count 1)))
	      ((= tok-type rpar-tok)
	       (when (zero? count)
		 (%error))
	       (loop (cdr tl) (- count 1)))
	      (else
	       (loop (cdr tl) count)))))))

; Ne traite pas les anchors
(define (parse-tokens tok-list macros)
  (if (null? tok-list)
      (make-<regexp> epsilon-re)
    (let ((line (get-tok-line (car tok-list))))
      (parse-tokens-match tok-list line)
      (let* ((begin-par (make-tok lpar-tok "" line 1))
	     (end-par (make-tok rpar-tok "" line 1)))
	(let* ((tok-list2 (append (list begin-par)
				  tok-list
				  (list end-par)))
	       (result (parse-tokens-sub tok-list2 macros)))
	  (car result)))))) ; (cdr result) == () obligatoirement

(define (tokens->regexp tok-list macros)
  (let ((tok-list2 (de-anchor-tokens tok-list)))
    (parse-tokens tok-list2 macros)))

(define (tokens->rule tok-list macros)
  (let* ((rule (extract-anchors tok-list))
	 (tok-list2 (<rule>-regexp rule))
	 (tok-list3 (de-anchor-tokens tok-list2))
	 (re (parse-tokens tok-list3 macros)))
    (<rule>-regexp-set! rule re)
    rule))

; Retourne une paire: <<EOF>>-action et vecteur des regles ordinaires
(define (adapt-rules rules)
  (let loop ((r rules) (revr '()) (<<EOF>>-action #f) (<<ERROR>>-action #f))
    (if (null? r)
	(values (or <<EOF>>-action default-<<EOF>>-action)
		(or <<ERROR>>-action default-<<ERROR>>-action)
		(list->vector (reverse revr)))
      (let ((r1 (car r)))
	(cond ((<rule>-eof? r1)
	       (if <<EOF>>-action
		   (lex-error 'lex:adapt-rules (<rule>-line r1) #f
			      "the <<EOF>> anchor can be used at most once")
		 (loop (cdr r) revr (<rule>-action r1) <<ERROR>>-action)))
	      ((<rule>-error? r1)
	       (if <<ERROR>>-action
		   (lex-error 'lex:adapt-rules (<rule>-line r1) #f
			      "the <<ERROR>> anchor can be used at most once")
		 (loop (cdr r) revr <<EOF>>-action (<rule>-action r1))))
	      (else
	       (loop (cdr r) (cons r1 revr) <<EOF>>-action <<ERROR>>-action)))))))


;;;; parser functions for SILex tables

(module (parse-macros)

  (define-struct include-file-data
    (source-pathname
		;Scheme string  representing the file pathname  from the
		;source  file.   It  can  be  an  absolute  or  relative
		;pathname.
     source-line
		;Exact  integer  representing  the line  number  in  the
		;source file  of the  file inclusion request.   Used for
		;error reporting.
     source-column
		;Exact  integer representing  the column  number in  the
		;source file  of the  file inclusion request.   Used for
		;error reporting.
     ))

  (define parse-macros
    ;;Parse  the header  of a  SILex table  definition.  Accumulate  the
    ;;macro definitions in an alist  and return it.  Tokens are acquired
    ;;calling the LEXER function.
    ;;
    ;;The argument PARSING-FULL-TABLE?  must be  false is we are parsing
    ;;a full table file; it must be  true if we are parsing a macro file
    ;;included from another file.
    ;;
    (case-lambda
     (()
      (parse-macros '() #t))
     ((macros-already-defined parsing-full-table?)
      (let ((result (%parse-next-macro macros-already-defined parsing-full-table?)))
	(cond ((include-file-data? result)
	       (parse-macros (%parse-external-macro-file result macros-already-defined)
			     parsing-full-table?))
	      ;;If RESULT is a pair: it is a macros alist entry.
	      ((pair? result)
	       (parse-macros (cons result macros-already-defined)
			     parsing-full-table?))
	      (result
	       (assertion-violation 'lex:parse-macros
		 "internal error, invalid value from macros lexer" result))
	      (else
	       ;;If RESULT is false: it signals the end of macros header
	       ;;in the table file.
	       macros-already-defined))))
     ))

  (define (%parse-next-macro macros-already-defined parsing-full-table?)
    ;;Parse the next macro definition in the table.
    ;;
    ;;MACROS-ALREADY-DEFINED  is  null  or  an alist  of  the  already
    ;;defined macros  with macro names as keywords  and parsed regexps
    ;;as values.  It is used to detect duplicate macro definitions.
    ;;
    ;;The  argument  PARSING-FULL-TABLE?   must  be false  is  we  are
    ;;parsing a full table  file; it must be true if  we are parsing a
    ;;macro file included from another file.
    ;;
    ;;If the macro is successfully parsed:
    ;;
    ;;* If the parsed macro defines  a new symbol: return a pair being
    ;;a new element for the MACROS-ALREADY-DEFINED alist.
    ;;
    ;;* If the parsed macro is a directive to load another macro file:
    ;;return a string representing the specified file pathname.
    ;;
    ;;While reading a  full table file: if a  token associated to "%%"
    ;;is lexed, return false, signaling the end of the table's header.
    ;;
    ;;While reading  a macro file: if  the EOF token  is lexed, return
    ;;false, signaling the end of the macro file.
    ;;
    ;;If  a token associated  to "%[<pathname>]"  is lexed:  parse the
    ;;corresponding macro file.
    ;;
    ;;It is an error if the  EOF token is lexed by this function while
    ;;reading a  table file:  the table must  have the  "%%" separator
    ;;followed by at least one semantic action definition.
    ;;
    ;;It is  NOT an error if the  EOF token is lexed  by this function
    ;;while reading a macro file.
    ;;
    (push-lexer (macro-lexer))
    (parse-horiz-and-vertical-blanks)
    (let* ((tok	(lexer))
	   (tok-type	(get-tok-type tok)))
      (define (%error . message-strings)
	(apply lex-error 'lex:%parse-next-macro (get-tok-line tok) (get-tok-column tok) message-strings))
      (cond ((= tok-type id-tok)
	     (let* ((name	(get-tok-attr tok))
		    (tok-list	(%parse-white-space-one-regexp))
		    (regexp	(tokens->regexp tok-list macros-already-defined))
		    (old	(assoc name macros-already-defined)))
	       (if (and old (not (regexp=? regexp (cdr old))))
		   (%error "the macro \"" (get-tok-2nd-attr tok) "\" has already been defined"
			   " with different regexp specification")
		 (begin
		   (pop-lexer)
		   (cons name regexp)))))
	    ((= tok-type percent-percent-tok)
	     (pop-lexer)
	     (if parsing-full-table?
		 #f
	       (%error "invalid %% mark found while parsing macro file")))
	    ((= tok-type percent-include-tok)
	     (%parse-white-space-separator)
	     ;;Tokens of  type "percent include" have  the file pathname
	     ;;as string in the attribute slot.
	     (make-include-file-data (get-tok-attr tok) (get-tok-line tok) (get-tok-column tok)))
	    ((= tok-type illegal-tok)
	     (%error "macro name expected"))
	    ((= tok-type eof-tok)
	     (if parsing-full-table?
		 (%error "end of file found before %%")
	       #f)))))

  (define (%parse-white-space-one-regexp)
    ;;After a  macro identifier  has been  successfully parsed,  parse a
    ;;horizontal or vertical white space followed by the regexp.  Return
    ;;the result of parsing the regexp.
    ;;
    ;;It is an error if the next token is not a white space.
    ;;
    (let* ((tok	(lexer))
	   (tok-type	(get-tok-type tok)))
      (cond ((or (= tok-type hblank-tok)
		 (= tok-type vblank-tok))
	     (parse-regexp))
	    ((= tok-type open-comment-tok)
	     (parse-nested-comment)
	     (parse-regexp))
	    (else
	     (lex-error 'lex:%parse-white-space-one-regexp ; percent-percent-tok, id-tok or illegal-tok
			(get-tok-line tok) (get-tok-column tok) "white space expected")))))

  (define (%parse-white-space-separator)
    ;;After a  percent include  directive has been  successfully parsed,
    ;;parse a horizontal or vertical white space as separator.
    ;;
    ;;It is an error if the next token is not a white space.
    ;;
    (let* ((tok	(lexer))
	   (tok-type	(get-tok-type tok)))
      (cond ((or (= tok-type hblank-tok)
		 (= tok-type vblank-tok))
	     #t)
	    ((= tok-type open-comment-tok)
	     (parse-nested-comment)
	     #t)
	    (else
	     (lex-error 'lex:%parse-white-space-separator (get-tok-line tok) (get-tok-column tok)
			"white space expected")))))

  (define (%parse-external-macro-file file-data macros-already-defined)
    ;;Parse the file  in PATHNAME reading macro  definitions; only macro
    ;;definitions must be there: the format of the file must be the same
    ;;of the  header of  an ordinary silex  table, without  the trailing
    ;;"%%".
    ;;
    ;;MACROS-ALREADY-DEFINED must be an alist  in the same format as the
    ;;one  built  by  PARSE-MACROS:  it  is  used  to  detect  duplicate
    ;;definitions.
    ;;
    ;;Return  the   alist  of   defined  macros,  already   joined  with
    ;;MACROS-ALREADY-DEFINED.
    ;;
    (define who 'lex:%parse-external-macro-file)
    (let ((pathname (%normalise-include-file who file-data)))
      (parametrise ((include-files (cons pathname (include-files))))
	(let ((input-port (open-input-file pathname)))
	  (unwind-protect
	      (let ((IS (make-IS (port: input-port) (counters: 'all))))
		(parametrise ((input-source pathname))
		  (with-context-to-lex-file IS
		    (parse-macros macros-already-defined #f))))
	    (close-input-port input-port))))))

  (define (%normalise-include-file who file-data)
    ;;Validate and normalise the  pathname in FILE-DATA.  If successful:
    ;;return a Scheme  string representing the absolute  pathname of the
    ;;file; otherwise raise an exception.
    ;;
    (assert (include-file-data? file-data))
    (let* ((source-pathname   ($include-file-data-source-pathname file-data))
	   (absolute-pathname (search-file-in-environment-path source-pathname "SILEX_PATH")))
      (cond ((not absolute-pathname)
	     (apply lex-error who
		    ($include-file-data-source-line   file-data)
		    ($include-file-data-source-column file-data)
		    "request to include non-existent macro file: \"" source-pathname "\""))
	    ((member absolute-pathname (include-files))
	     (apply lex-error who
		    ($include-file-data-source-line   file-data)
		    ($include-file-data-source-column file-data)
		    "recursive inclusion of macro file: \"" absolute-pathname "\""))
	    (else
	     absolute-pathname))))

  #| end of module: parse-macros |# )

(define (parse-rules available-macros)
  ;;Parse the rules section of a  SILex table; return the list of <rule>
  ;;records.   AVAILABLE-MACROS  is  the  alist of  macros  returned  by
  ;;PARSE-MACRO.
  ;;
  (define (main available-macros)
    ;;(Marco Maggi; Tue Jan 18,  2011) Guess: this call to %PARSE-ACTION
    ;;removes  the blanks  after  the  "%%" mark,  which  was parsed  by
    ;;PARSE-MACROS.
    (%parse-action #f #f)
    (let loop ((rule (%parse-rule available-macros)))
      (if rule
	  (cons rule (loop (%parse-rule available-macros)))
	'())))

  (define (%parse-rule available-macros)
    (let ((tok-list (parse-regexp)))
      (if (null? tok-list)
	  #f
	(let* ((rule	(tokens->rule tok-list available-macros))
	       (action	(%parse-action (<rule>-eof?   rule)
				       (<rule>-error? rule))))
	  (<rule>-action-set! rule action)
	  rule))))

  (define (%parse-action <<EOF>>-action? <<ERROR>>-action?)
    (define (main <<EOF>>-action? <<ERROR>>-action?)
      (push-lexer (action-lexer))
      (let loop ((action? #f))
	(let* ((tok (lexer))
	       (tok-type (get-tok-type tok)))
	  (cond ((= tok-type char-tok)
		 (loop #t))
		((= tok-type hblank-tok)
		 (loop action?))
		((= tok-type open-comment-tok)
		 (parse-nested-comment)
		 (loop action?))
		((= tok-type vblank-tok)
		 (push-lexer (regexp-lexer))
		 (let* ((tok (lexer))
			(tok-type (get-tok-type tok))
			(bidon (lexer-unget tok)))
		   (pop-lexer)
		   (cond ((or (= tok-type hblank-tok)
			      (= tok-type vblank-tok))
			  (loop action?))
			 ;; ((= tok-type open-comment-tok)
			 ;;  (parse-nested-comment)
			 ;;  (loop action?))
			 (else
			  (pop-lexer)
			  (%parse-action-end <<EOF>>-action? <<ERROR>>-action? action?)))))
		(else ; eof-tok
		 (lexer-unget tok)
		 (pop-lexer)
		 (%parse-action-end <<EOF>>-action? <<ERROR>>-action? action?))))))

    (define (%parse-action-end <<EOF>>-action? <<ERROR>>-action? action?)
      (let ((act (lexer-get-history)))
	(cond (action?
	       act)
	      (<<EOF>>-action?
	       (string-append act default-<<EOF>>-action))
	      (<<ERROR>>-action?
	       (string-append act default-<<ERROR>>-action))
	      (else
	       (string-append act default-action)))))

    (main <<EOF>>-action? <<ERROR>>-action?))

  (main available-macros))

(define parse-regexp
  ;;Parse a regexp.
  ;;
  (let ()
    (define (end-action	tok loop)
      (lexer-unget tok)
      (pop-lexer)
      (lexer-set-blank-history #f)
      `())
    (define action-table
      (make-dispatch-table
       number-of-tokens
       `((,eof-tok	. ,end-action)
	 (,hblank-tok	. ,end-action)
	 (,vblank-tok	. ,end-action)
	 ;; "["
	 (,lbrack-tok	. ,(lambda (tok loop)
			     ;;*** KEEP THE LET ***
			     ;;
			     ;;It   imposes  evaluation   order  calling
			     ;;%PARSE-CLASS before LOOP.
			     (let ((tok1 (%parse-class (list) #f (get-tok-line tok) (get-tok-column tok))))
			       (cons tok1 (loop)))))
	 ;; "[]"
	 (,lbrack-rbrack-tok . ,(lambda (tok loop)
				  ;;*** KEEP THE LET ***
				  ;;
				  ;;It imposes  evaluation order calling
				  ;;%PARSE-CLASS before LOOP.
				  (let ((tok1 (%parse-class (list (cons rbrack-ch rbrack-ch)) #f
							    (get-tok-line tok) (get-tok-column tok))))
				    (cons tok1 (loop)))))
	 ;; "[^"
	 (,lbrack-caret-tok . ,(lambda (tok loop)
				 ;;*** KEEP THE LET ***
				 ;;
				 ;;It  imposes evaluation  order calling
				 ;;%PARSE-CLASS before LOOP.
				 (let ((tok1 (%parse-class (list) #t
							   (get-tok-line tok) (get-tok-column tok)) ))
				   (cons tok1 (loop)))))
	 ;; "[-"
	 (,lbrack-minus-tok . ,(lambda (tok loop)
				 ;;*** KEEP THE LET ***
				 ;;
				 ;;It  imposes evaluation  order calling
				 ;;%PARSE-CLASS before LOOP.
				 (let ((tok1 (%parse-class (list (cons minus-ch minus-ch)) #f
							   (get-tok-line tok) (get-tok-column tok))))
				   (cons tok1 (loop)))))
	 (,doublequote-tok . ,(lambda (tok loop)
				;;*** KEEP THE LET ***
				;;
				;;It  imposes  evaluation order  calling
				;;%PARSE-CLASS before LOOP.
				(let ((tok1 (parse-string (get-tok-line tok) (get-tok-column tok))))
				  (cons tok1 (loop)))))
	 (,illegal-tok . ,(lambda (tok loop)
			    (lex-error 'lex:parse-regexp (get-tok-line tok) (get-tok-column tok)
				       "syntax error in macro reference"))))
       (lambda (tok loop)
	 (cons tok (loop)))))

    (define (%parse-class initial-class negative-class? line column)
      ;;Parse a character class in a regexp.
      ;;
      (define (main initial-class negative-class? line column)
	(push-lexer (class-lexer))
	(let loop ((class initial-class))
	  (let ((new-range (%parse-class-range)))
	    (if new-range
		(loop (class-union (list new-range) class))
	      (let ((class (if negative-class?
			       (class-compl class)
			     class)))
		(pop-lexer)
		(make-tok class-tok "" line column class))))))

      (define (%parse-class-range)
	;;Parse the next character range in a character class regexp.  A
	;;character class like:
	;;
	;;	[ABCD-HIL-N]
	;;
	;;is split into the single character "ranges":
	;;
	;;	A  B  C  D-H  I  L-N
	;;
	;;each of which is parsed with a single call to this function.
	;;
	;;If the  next class range is  a single character  C: return the
	;;pair (C  . C).  If the next  class range is a  dash range C-K:
	;;return the pair (C . K).
	;;
	;;If LEXER returns the close bracket token: return false.
	;;
	;;It is an error if LEXER  returns the EOF token: the class must
	;;end with a close bracket.
	;;
	(define (%error tok . message-strings)
	  (apply lex-error 'lex:%parse-class-range (get-tok-line tok) (get-tok-column tok)
		 message-strings))
	(let* ((tok		(lexer))
	       (tok-type	(get-tok-type tok)))
	  (cond ((= tok-type char-tok)
		 (let* ((c		(get-tok-attr tok))
			(tok2		(lexer))
			(tok2-type	(get-tok-type tok2)))
		   (if (not (= tok2-type minus-tok))
		       (begin
			 (lexer-unget tok2)
			 (cons c c))
		     (let* ((tok3		(lexer))
			    (tok3-type	(get-tok-type tok3)))
		       (cond ((= tok3-type char-tok)
			      (let ((c2 (get-tok-attr tok3)))
				(if (> c c2)
				    (%error tok3
					    "bad range specification in character class; "
					    "the start character is higher than the end one")
				  (cons c c2))))
			     ((or (= tok3-type rbrack-tok)
				  (= tok3-type minus-tok))
			      (%error tok3
				      "bad range specification in character class; a specification "
				      "like \"-x\", \"x--\" or \"x-]\" has been used"))
			     ((= tok3-type eof-tok)
			      (%error tok3 "eof of file found while parsing a character class")))))))
		((= tok-type minus-tok)
		 (%error tok
			 "bad range specification in character class; a specification "
			 "like \"-x\", \"x--\" or \"x-]\" has been used"))
		((= tok-type rbrack-tok)
		 #f)
		((= tok-type eof-tok)
		 (%error tok "eof found while parsing a regexp character class"))
		(else
		 (assertion-violation 'lex:%parse-class-range
		   "internal error, unexpected token type while parsing character class in regexp"
		   tok)))))

      (main initial-class negative-class? line column))

    (define (parse-string line column)
      ;;Parse a string in a regexp.  This function must be invoked after
      ;;the opening doublequote has been consumed.
      ;;
      (push-lexer (string-lexer))
      (let ((char-list (let loop ()
			 (let* ((tok      (lexer))
				(tok-type (get-tok-type tok)))
			   (cond ((= tok-type char-tok)
				  (cons (get-tok-attr tok) (loop)))
				 ((= tok-type doublequote-tok)
				  (pop-lexer)
				  '())
				 (else
				  (assert (= tok-type eof-tok))
				  (lex-error 'lex:parse-string (get-tok-line tok) #f
					     "end of file found while parsing a string")))))))
	(make-tok string-tok "" line column char-list)))

    (lambda ()	;this is the PARSE-REGEXP function
      (push-lexer (regexp-lexer))
      (lexer-set-blank-history #t)
      (parse-horiz-and-vertical-blanks)
      (let loop ()
	(let* ((tok	 (lexer))
	       (tok-type (get-tok-type tok))
	       (action	 (vector-ref action-table tok-type)))
	  (action tok loop))))))

(define (parse-horiz-and-vertical-blanks)
  ;;Consume and  discard tokens  from LEXER until  a non-blank  is read;
  ;;unget the first non-blank token with LEXER-UNGET, then return.
  ;;
  (let* ((tok		(lexer))
	 (tok-type	(get-tok-type tok)))
    (cond ((or (= tok-type hblank-tok)
	       (= tok-type vblank-tok))
	   (parse-horiz-and-vertical-blanks))
	  ((= tok-type open-comment-tok)
	   (parse-nested-comment)
	   (parse-horiz-and-vertical-blanks))
	  (else
	   (lexer-unget tok)))))

(define (parse-nested-comment)
  ;;Parse a  nested comment.   This function must  be invoked  after the
  ;;opening comment token has been parsed.
  ;;
  (push-lexer (nested-comment-lexer))
  (let loop ((count 1))
    (if (zero? count)
	(pop-lexer)
      (let* ((tok	(lexer))
	     (tok-type	(get-tok-type tok)))
	(cond ((= tok-type open-comment-tok)
	       (loop (+ +1 count)))
	      ((= tok-type close-comment-tok)
	       (loop (+ -1 count)))
	      ((= tok-type eof-tok)
	       (lex-error 'lex:parse-nested-comment (get-tok-line tok) (get-tok-column tok)
			  "end of input found while parsing nested comment"))
	      (else ;any char
	       (loop count)))))))


;;;; module re2nfa.scm

;; Le vecteur d'etats contient la table de transition du nfa.
;; Chaque entree contient les arcs partant de l'etat correspondant.
;; Les arcs sont stockes dans une liste.
;; Chaque arc est une paire (class . destination).
;; Les caracteres d'une classe sont enumeres par ranges.
;; Les ranges sont donnes dans une liste,
;;   chaque element etant une paire (debut . fin).
;; Le symbole eps peut remplacer une classe.
;; L'acceptation est decrite par une paire (acc-if-eol . acc-if-no-eol).

(define (re2nfa rules)
  ;; Construction de l'automate complet
  ;;

  (define r2n-counter	0)
  (define r2n-v-arcs	(vector '()))
  (define r2n-v-acc	(vector #f))
  (define r2n-v-len	1)

  ;; Agrandissement des vecteurs
  (define (r2n-extend-v)
    (let* ((new-len (* 2 r2n-v-len))
	   (new-v-arcs (make-vector new-len '()))
	   (new-v-acc (make-vector new-len #f)))
      (let loop ((i 0))
	(if (< i r2n-v-len)
	    (begin
	      (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
	      (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
	      (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len new-len)))

  ;; Finalisation des vecteurs
  (define (r2n-finalize-v)
    (let* ((new-v-arcs (make-vector r2n-counter))
	   (new-v-acc  (make-vector r2n-counter)))
      (let loop ((i 0))
	(when (< i r2n-counter)
	  (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
	  (vector-set! new-v-acc  i (vector-ref r2n-v-acc  i))
	  (loop (+ i 1))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len r2n-counter)))

  ;; Creation d'etat
  (define (r2n-get-state acc)
    (when (= r2n-counter r2n-v-len)
      (r2n-extend-v))
    (let ((state r2n-counter))
      (set! r2n-counter (+ r2n-counter 1))
      (vector-set! r2n-v-acc state (or acc (cons #f #f)))
      state))

  ;; Ajout d'un arc
  (define (r2n-add-arc start chars end)
    (vector-set! r2n-v-arcs
		 start
		 (cons (cons chars end) (vector-ref r2n-v-arcs start))))

  ;; Construction de l'automate a partir des regexp
  (define (r2n-build-epsilon re start end)
    (r2n-add-arc start 'eps end))

  (define (r2n-build-or re start end)
    (let ((re1 (<regexp>-attr1 re))
	  (re2 (<regexp>-attr2 re)))
      (r2n-build-re re1 start end)
      (r2n-build-re re2 start end)))

  (define (r2n-build-conc re start end)
    (let* ((re1 (<regexp>-attr1 re))
	   (re2 (<regexp>-attr2 re))
	   (inter (r2n-get-state #f)))
      (r2n-build-re re1 start inter)
      (r2n-build-re re2 inter end)))

  (define (r2n-build-star re start end)
    (let* ((re1 (<regexp>-attr1 re))
	   (inter1 (r2n-get-state #f))
	   (inter2 (r2n-get-state #f)))
      (r2n-add-arc start  'eps inter1)
      (r2n-add-arc inter1 'eps inter2)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter2 inter1)))

  (define (r2n-build-plus re start end)
    (let* ((re1 (<regexp>-attr1 re))
	   (inter1 (r2n-get-state #f))
	   (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter2 'eps inter1)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter1 inter2)))

  (define (r2n-build-question re start end)
    (let ((re1 (<regexp>-attr1 re)))
      (r2n-add-arc start 'eps end)
      (r2n-build-re re1 start end)))

  (define (r2n-build-class re start end)
    (let ((class (<regexp>-attr1 re)))
      (r2n-add-arc start class end)))

  (define (r2n-build-char re start end)
    (let* ((c (<regexp>-attr1 re))
	   (class (list (cons c c))))
      (r2n-add-arc start class end)))

  (define r2n-build-re
    (let ((sub-function-v (vector r2n-build-epsilon
				  r2n-build-or
				  r2n-build-conc
				  r2n-build-star
				  r2n-build-plus
				  r2n-build-question
				  r2n-build-class
				  r2n-build-char)))
      (lambda (re start end)
	(let* ((re-type (<regexp>-type re))
	       (sub-f (vector-ref sub-function-v re-type)))
	  (sub-f re start end)))))

  ;; Construction de l'automate relatif a une regle
  (define (r2n-build-rule rule ruleno nl-start no-nl-start)
    (let* ((re		(<rule>-regexp rule))
	   (bol?		(<rule>-bol? rule))
	   (eol?		(<rule>-eol? rule))
	   (rule-start	(r2n-get-state #f))
	   (rule-end	(r2n-get-state (if eol?
					   (cons ruleno #f)
					 (cons ruleno ruleno)))))
      (r2n-build-re re rule-start rule-end)
      (r2n-add-arc nl-start 'eps rule-start)
      (when (not bol?)
	(r2n-add-arc no-nl-start 'eps rule-start))))


  (let* ((nb-of-rules	(vector-length rules))
	 (nl-start	(r2n-get-state #f))
	 (no-nl-start	(r2n-get-state #f)))
    (let loop ((i 0))
      (when (< i nb-of-rules)
	(r2n-build-rule (vector-ref rules i)
			i
			nl-start
			no-nl-start)
	(loop (+ i 1))))
    (r2n-finalize-v)
    (let ((v-arcs r2n-v-arcs)
	  (v-acc r2n-v-acc))
      (values nl-start no-nl-start v-arcs v-acc))))


;;;; module noeps.scm

(define (noeps-merge-1 l1 l2)
  ;; Fonction "merge" qui elimine les repetitions.
  ;;
  (cond ((null? l1)
	 l2)
	((null? l2)
	 l1)
	(else
	 (let ((t1 (car l1))
	       (t2 (car l2)))
	   (cond ((< t1 t2)
		  (cons t1 (noeps-merge-1 (cdr l1) l2)))
		 ((= t1 t2)
		  (cons t1 (noeps-merge-1 (cdr l1) (cdr l2))))
		 (else
		  (cons t2 (noeps-merge-1 l1 (cdr l2)))))))))

(define (noeps nl-start no-nl-start arcs acc)
  ;; Elimination des transitions eps.
  ;;

  ;; Fabrication des voisinages externes
  (define (noeps-mkvois trans-v)
    (let* ((nbnodes	(vector-length trans-v))
	   (arcs		(make-vector nbnodes '())))
      (let loop1 ((n 0))
	(when (< n nbnodes)
	  (let loop2 ((trans (vector-ref trans-v n))
		      (ends  '()))
	    (if (null? trans)
		(vector-set! arcs n ends)
	      (let* ((tran  (car trans))
		     (class (car tran))
		     (end   (cdr tran)))
		(loop2 (cdr trans) (if (eq? class 'eps)
				       (noeps-merge-1 ends (list end))
				     ends)))))
	  (loop1 (+ n 1))))
      arcs))

  ;; Fabrication des valeurs initiales
  (define (noeps-mkinit trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (init (make-vector nbnodes)))
      (let loop ((n 0))
	(when (< n nbnodes)
	  (vector-set! init n (list n))
	  (loop (+ n 1))))
      init))

  ;; Traduction d'une liste d'arcs
  (define (noeps-trad-arcs trans dict)
    (let loop ((trans trans))
      (if (null? trans)
	  '()
	(let* ((tran (car trans))
	       (class (car tran))
	       (end (cdr tran)))
	  (if (eq? class 'eps)
	      (loop (cdr trans))
	    (let* ((new-end (vector-ref dict end))
		   (new-tran (cons class new-end)))
	      (cons new-tran (loop (cdr trans)))))))))

  (let* ((digraph-arcs (noeps-mkvois arcs))
	 (digraph-init (noeps-mkinit arcs))
	 (dict (digraph digraph-arcs digraph-init noeps-merge-1))
	 (new-nl-start (vector-ref dict nl-start))
	 (new-no-nl-start (vector-ref dict no-nl-start)))
    (let loop ((i (- (vector-length arcs) 1)))
      (when (>= i 0)
	(vector-set! arcs i (noeps-trad-arcs (vector-ref arcs i) dict))
	(loop (- i 1))))
    (values new-nl-start new-no-nl-start arcs acc)))


;;;; module sweep.scm

(define (sweep nl-start no-nl-start arcs-v acc-v)
  ;; Elimination des etats inutiles
  ;;

  (define (main)
    (let* ((digraph-arcs	(sweep-mkarcs arcs-v))
	   (digraph-init	acc-v)
	   (digraph-op		sweep-op)
	   (dist-acc-v		(digraph digraph-arcs digraph-init digraph-op))
	   (result		(sweep-renum dist-acc-v))
	   (new-nbnodes		(car result))
	   (dict		(cdr result))
	   (new-nl-start	(sweep-list nl-start dict))
	   (new-no-nl-start	(sweep-list no-nl-start dict))
	   (new-arcs-v		(sweep-states (sweep-all-arcs arcs-v dict)
					      new-nbnodes
					      dict))
	   (new-acc-v		(sweep-states acc-v new-nbnodes dict)))
      (values new-nl-start new-no-nl-start new-arcs-v new-acc-v)))

  ;; Preparer les arcs pour digraph
  (define (sweep-mkarcs trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (arcs-v (make-vector nbnodes '())))
      (let loop1 ((n 0))
	(if (< n nbnodes)
	    (let loop2 ((trans (vector-ref trans-v n)) (arcs '()))
	      (if (null? trans)
		  (begin
		    (vector-set! arcs-v n arcs)
		    (loop1 (+ n 1)))
		(loop2 (cdr trans) (noeps-merge-1 (cdar trans) arcs))))
	  arcs-v))))

  ;; Preparer l'operateur pour digraph
  (define sweep-op
    (let ((acc-min (lambda (rule1 rule2)
		     (cond ((not rule1)
			    rule2)
			   ((not rule2)
			    rule1)
			   (else
			    (min rule1 rule2))))))
      (lambda (acc1 acc2)
	(cons (acc-min (car acc1) (car acc2))
	      (acc-min (cdr acc1) (cdr acc2))))))

  ;; Renumerotation des etats (#f pour etat a eliminer)
  ;; Retourne (new-nbnodes . dict)
  (define (sweep-renum dist-acc-v)
    (let* ((nbnodes (vector-length dist-acc-v))
	   (dict (make-vector nbnodes)))
      (let loop ((n 0) (new-n 0))
	(if (< n nbnodes)
	    (let* ((acc (vector-ref dist-acc-v n))
		   (dead? (equal? acc '(#f . #f))))
	      (if dead?
		  (begin
		    (vector-set! dict n #f)
		    (loop (+ n 1) new-n))
		(begin
		  (vector-set! dict n new-n)
		  (loop (+ n 1) (+ new-n 1)))))
	  (cons new-n dict)))))

  ;; Elimination des etats inutiles d'une liste d'etats
  (define (sweep-list ss dict)
    (if (null? ss)
	'()
      (let* ((olds (car ss))
	     (news (vector-ref dict olds)))
	(if news
	    (cons news (sweep-list (cdr ss) dict))
	  (sweep-list (cdr ss) dict)))))

  ;; Elimination des etats inutiles d'une liste d'arcs
  (define (sweep-arcs arcs dict)
    (if (null? arcs)
	'()
      (let* ((arc (car arcs))
	     (class (car arc))
	     (ss (cdr arc))
	     (new-ss (sweep-list ss dict)))
	(if (null? new-ss)
	    (sweep-arcs (cdr arcs) dict)
	  (cons (cons class new-ss)
		(sweep-arcs (cdr arcs) dict))))))

  ;; Elimination des etats inutiles dans toutes les transitions
  (define (sweep-all-arcs arcs-v dict)
    (let loop ((n (- (vector-length arcs-v) 1)))
      (if (>= n 0)
	  (begin
	    (vector-set! arcs-v n (sweep-arcs (vector-ref arcs-v n) dict))
	    (loop (- n 1)))
	arcs-v)))

  ;; Elimination des etats inutiles dans un vecteur
  (define (sweep-states v new-nbnodes dict)
    (let ((nbnodes (vector-length v))
	  (new-v (make-vector new-nbnodes)))
      (let loop ((n 0))
	(if (< n nbnodes)
	    (let ((new-n (vector-ref dict n)))
	      (if new-n
		  (vector-set! new-v new-n (vector-ref v n)))
	      (loop (+ n 1)))
	  new-v))))

  (main))


;;;; module nfa2dfa.scm

(define (nfa2dfa nl-start no-nl-start arcs-v acc-v)
  ;;Transform   the   automaton    from   non-deterministic   (NFA)   to
  ;;deterministic (DFA).
  ;;

  (define n2d-state-dict	(vector #f))
  (define n2d-state-len		1)
  (define n2d-state-count	0)
  (define n2d-state-hash	(make-vector (vector-length arcs-v) '()))

  (define (main)
    (let* ((nl-d	(n2d-search-state nl-start))
	   (no-nl-d	(n2d-search-state no-nl-start))
	   (norm-arcs-v	(n2d-normalize-arcs-v arcs-v)))
      (let loop ((n 0))
	(when (< n n2d-state-count)
	  (let* ((dentry (vector-ref n2d-state-dict n))
		 (ss (get-dentry-ss dentry))
		 (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
		 (arcs (n2d-combine-arcs-l arcs-l))
		 (darcs (n2d-translate-arcs arcs))
		 (fact-darcs (n2d-factorize-darcs darcs))
		 (accs (map (lambda (s) (vector-ref acc-v s)) ss))
		 (acc (n2d-acc-mins accs)))
	    (set-dentry-darcs dentry fact-darcs)
	    (set-dentry-acc   dentry acc)
	    (loop (+ n 1)))))
      (let* ((result (n2d-extract-vs))
	     (new-arcs-v (car result))
	     (new-acc-v  (cdr result)))
	(values nl-d no-nl-d new-arcs-v new-acc-v))))

  (define (n2d-2arcs arc1 arc2)
    ;;Recoupement de deux arcs.
    ;;
    (let* ((class1 (car arc1))
	   (ss1 (cdr arc1))
	   (class2 (car arc2))
	   (ss2 (cdr arc2))
	   (result (class-sep class1 class2))
	   (classl (vector-ref result 0))
	   (classc (vector-ref result 1))
	   (classr (vector-ref result 2))
	   (ssl ss1)
	   (ssc (ss-union ss1 ss2))
	   (ssr ss2))
      (vector (if (or (null? classl) (null? ssl)) #f (cons classl ssl))
	      (if (or (null? classc) (null? ssc)) #f (cons classc ssc))
	      (if (or (null? classr) (null? ssr)) #f (cons classr ssr)))))

  (define (n2d-insert-arc new-arc arcs)
    ;;Insertion d'un arc dans une liste d'arcs a classes distinctes.
    ;;
    (if (null? arcs)
	(list new-arc)
      (let* ((arc (car arcs))
	     (others (cdr arcs))
	     (result (n2d-2arcs new-arc arc))
	     (arcl (vector-ref result 0))
	     (arcc (vector-ref result 1))
	     (arcr (vector-ref result 2))
	     (list-arcc (if arcc (list arcc) '()))
	     (list-arcr (if arcr (list arcr) '())))
	(append list-arcc list-arcr (if arcl
					(n2d-insert-arc arcl others)
				      others)))))

  (define (n2d-factorize-arcs arcs)
    ;;Regroupement  des  arcs  qui  aboutissent  au  meme  sous-ensemble
    ;;d'etats.
    ;;
    (if (null? arcs)
	'()
      (let* ((arc		(car arcs))
	     (arc-ss		(cdr arc))
	     (others-no-fact	(cdr arcs))
	     (others		(n2d-factorize-arcs others-no-fact)))
	(let loop ((o others))
	  (if (null? o)
	      (list arc)
	    (let* ((o1 (car o))
		   (o1-ss (cdr o1)))
	      (if (equal? o1-ss arc-ss)
		  (let* ((arc-class (car arc))
			 (o1-class (car o1))
			 (new-class (class-union arc-class o1-class))
			 (new-arc (cons new-class arc-ss)))
		    (cons new-arc (cdr o)))
		(cons o1 (loop (cdr o))))))))))

  (define (n2d-distinguish-arcs arcs)
    ;;Transformer  une liste d'arcs  quelconques en  des arcs  a classes
    ;;distinctes.
    ;;
    (let loop ((arcs arcs) (n-arcs '()))
      (if (null? arcs)
	  n-arcs
	(loop (cdr arcs) (n2d-insert-arc (car arcs) n-arcs)))))

  (define (n2d-normalize-arcs arcs)
    ;;Transformer une liste d'arcs quelconques  en des arcs a classes et
    ;;a destinations distinctes.
    ;;
    (n2d-factorize-arcs (n2d-distinguish-arcs arcs)))

  (define (n2d-factorize-darcs arcs)
    ;;Factoriser des arcs a destination unique (~deterministes).
    ;;
    (if (null? arcs)
	'()
      (let* ((arc (car arcs))
	     (arc-end (cdr arc))
	     (other-arcs (cdr arcs))
	     (farcs (n2d-factorize-darcs other-arcs)))
	(let loop ((farcs farcs))
	  (if (null? farcs)
	      (list arc)
	    (let* ((farc (car farcs))
		   (farc-end (cdr farc)))
	      (if (= farc-end arc-end)
		  (let* ((arc-class (car arc))
			 (farc-class (car farc))
			 (new-class (class-union farc-class arc-class))
			 (new-arc (cons new-class arc-end)))
		    (cons new-arc (cdr farcs)))
		(cons farc (loop (cdr farcs))))))))))

  (define (n2d-normalize-arcs-v arcs-v)
    ;;Normaliser un vecteur de listes d'arcs.
    ;;
    (let* ((nbnodes (vector-length arcs-v))
	   (new-v (make-vector nbnodes)))
      (let loop ((n 0))
	(if (= n nbnodes)
	    new-v
	  (begin
	    (vector-set! new-v n (n2d-normalize-arcs (vector-ref arcs-v n)))
	    (loop (+ n 1)))))))

  (define (n2d-ins-sep-arc new-arc arcs)
    ;;Inserer  un arc  dans une  liste  d'arcs a  classes distinctes  en
    ;;separant les arcs contenant une  partie de la classe du nouvel arc
    ;;des autres arcs.  Retourne: (oui . non).
    ;;
    (if (null? arcs)
	(cons (list new-arc) '())
      (let* ((arc (car arcs))
	     (others (cdr arcs))
	     (result (n2d-2arcs new-arc arc))
	     (arcl (vector-ref result 0))
	     (arcc (vector-ref result 1))
	     (arcr (vector-ref result 2))
	     (l-arcc (if arcc (list arcc) '()))
	     (l-arcr (if arcr (list arcr) '()))
	     (result (if arcl
			 (n2d-ins-sep-arc arcl others)
		       (cons '() others)))
	     (oui-arcs (car result))
	     (non-arcs (cdr result)))
	(cons (append l-arcc oui-arcs) (append l-arcr non-arcs)))))

  (define (n2d-combine-arcs arcs1 arcs2)
    ;;Combiner deux listes d'arcs a classes distinctes.  Ne tente pas de
    ;;combiner   les  arcs   qui  ont   nec.  des   classes  disjointes.
    ;;Conjecture: les  arcs crees  ont leurs classes  disjointes.  Note:
    ;;envisager de rajouter un "n2d-factorize-arcs" !!!!!!!!!!!!
    ;;
    (let loop ((arcs1 arcs1) (arcs2 arcs2) (dist-arcs2 '()))
      (if (null? arcs1)
	  (append arcs2 dist-arcs2)
	(let* ((arc (car arcs1))
	       (result (n2d-ins-sep-arc arc arcs2))
	       (oui-arcs (car result))
	       (non-arcs (cdr result)))
	  (loop (cdr arcs1) non-arcs (append oui-arcs dist-arcs2))))))

  ;;Section  temporaire: vieille  facon de  generer le  dfa Dictionnaire
  ;;d'etat  det.  Table  de hashage.   Creation des  arcs  d'un ensemble
  ;;d'etats en combinant des ensembles d'arcs a classes distinctes.

  ;; Fonctions de gestion des entrees du dictionnaire
  (define (make-dentry ss)
    (vector ss #f #f))

  (define (get-dentry-ss    dentry)	(vector-ref dentry 0))
  (define (get-dentry-darcs dentry)	(vector-ref dentry 1))
  (define (get-dentry-acc   dentry)	(vector-ref dentry 2))

  (define (set-dentry-darcs dentry arcs)	(vector-set! dentry 1 arcs))
  (define (set-dentry-acc   dentry acc)		(vector-set! dentry 2 acc))

  (define (n2d-extend-dict)
    ;;Extension du dictionnaire.
    ;;
    (let* ((new-len (* 2 n2d-state-len))
	   (v (make-vector new-len #f)))
      (let loop ((n 0))
	(if (= n n2d-state-count)
	    (begin
	      (set! n2d-state-dict v)
	      (set! n2d-state-len new-len))
	  (begin
	    (vector-set! v n (vector-ref n2d-state-dict n))
	    (loop (+ n 1)))))))

  (define (n2d-add-state ss)
    ;; Ajout d'un etat.
    ;;
    (let* ((s n2d-state-count)
	   (dentry (make-dentry ss)))
      (if (= n2d-state-count n2d-state-len)
	  (n2d-extend-dict))
      (vector-set! n2d-state-dict s dentry)
      (set! n2d-state-count (+ n2d-state-count 1))
      s))

  (define (n2d-search-state ss)
    ;;Recherche d'un etat.
    ;;
    (let* ((hash-no (if (null? ss) 0 (car ss)))
	   (alist (vector-ref n2d-state-hash hash-no))
	   (ass (assoc ss alist)))
      (if ass
	  (cdr ass)
	(let* ((s (n2d-add-state ss))
	       (new-ass (cons ss s)))
	  (vector-set! n2d-state-hash hash-no (cons new-ass alist))
	  s))))

  (define (n2d-combine-arcs-l arcs-l)
    ;;Combiner des listes d'arcs a classes dictinctes.
    ;;
    (if (null? arcs-l)
	'()
      (let* ((arcs (car arcs-l))
	     (other-arcs-l (cdr arcs-l))
	     (other-arcs (n2d-combine-arcs-l other-arcs-l)))
	(n2d-combine-arcs arcs other-arcs))))

  (define (n2d-translate-arc arc)
    ;;Transformer un arc non-det. en un arc det.
    ;;
    (let* ((class (car arc))
	   (ss (cdr arc))
	   (s (n2d-search-state ss)))
      (cons class s)))

  (define (n2d-translate-arcs arcs)
    ;;Transformer une liste d'arcs non-det. en ...
    ;;
    (map n2d-translate-arc arcs))

  (define n2d-acc-min2
    ;;Trouver le minimum de deux acceptants.
    ;;
    (let ((acc-min (lambda (rule1 rule2)
		     (cond ((not rule1)
			    rule2)
			   ((not rule2)
			    rule1)
			   (else
			    (min rule1 rule2))))))
      (lambda (acc1 acc2)
	(cons (acc-min (car acc1) (car acc2))
	      (acc-min (cdr acc1) (cdr acc2))))))

  (define (n2d-acc-mins accs)
    ;;Trouver le minimum de plusieurs acceptants.
    ;;
    (if (null? accs)
	(cons #f #f)
      (n2d-acc-min2 (car accs)
		    (n2d-acc-mins (cdr accs)))))

  (define (n2d-extract-vs)
    ;;Fabriquer les vecteurs d'arcs et d'acceptance.
    ;;
    (let* ((arcs-v (make-vector n2d-state-count))
	   (acc-v (make-vector n2d-state-count)))
      (let loop ((n 0))
	(if (= n n2d-state-count)
	    (cons arcs-v acc-v)
	  (let ((v (vector-ref n2d-state-dict n)))
	    (vector-set! arcs-v n (get-dentry-darcs v))
	    (vector-set! acc-v  n (get-dentry-acc   v))
	    (loop (+ n 1)))))))

  (main))


;;;; module prep.scm
;;
;;Divers pre-traitements avant l'ecriture des tables.
;;

(define (prep-arc->sharcs arc)
  ;;Passe d'un arc multi-range a une liste d'arcs mono-range.
  ;;
  (let* ((range-l (car arc))
	 (dest (cdr arc))
	 (op (lambda (range) (cons range dest))))
    (map op range-l)))

(define (prep-sharc-<= sharc1 sharc2)
  ;;Compare des arcs courts selon leur premier caractere.
  ;;
  (class-<= (caar sharc1) (caar sharc2)))

(define (prep-fill-error sharcs)
  ;;Remplit les trous parmi les sharcs avec des arcs "erreur".
  ;;
  (let loop ((sharcs sharcs) (start 'inf-))
    (cond ((class-= start 'inf+)
	   '())
	  ((null? sharcs)
	   (cons (cons (cons start 'inf+) 'err) (loop sharcs 'inf+)))
	  (else
	   (let* ((sharc (car sharcs))
		  (h (caar sharc))
		  (t (cdar sharc)))
	     (if (class-< start h)
		 (cons (cons (cons start (- h 1)) 'err) (loop sharcs h))
	       (cons sharc (loop (cdr sharcs)
				 (if (class-= t 'inf+)
				     'inf+
				   (+ t 1))))))))))

(define (prep-arcs->tree arcs)
  ;;Passe  d'une liste  d'arcs  a  un arbre  de  decision 2eme  methode:
  ;;permettre des comparaisons = quand ca adonne.
  ;;
  (let* ((sharcs-l (map prep-arc->sharcs arcs))
	 (sharcs (apply append sharcs-l))
	 (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
	 (sorted (prep-fill-error sorted-with-holes))
	 (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
	 (table (list->vector (map op sorted))))
    (let loop ((left 0) (right (- (vector-length table) 1)))
      (if (= left right)
	  (cdr (vector-ref table left))
	(let ((mid (div (+ left right 1) 2)))
	  (if (and (= (+ left 2) right)
		   (= (+ (car (vector-ref table mid)) 1)
		      (car (vector-ref table right)))
		   (eqv? (cdr (vector-ref table left))
			 (cdr (vector-ref table right))))
	      (list '=
		    (car (vector-ref table mid))
		    (cdr (vector-ref table mid))
		    (cdr (vector-ref table left)))
	    (list (car (vector-ref table mid))
		  (loop left (- mid 1))
		  (loop mid right))))))))

(define (prep-detect-yytext s)
  ;;Determine si une action a besoin de calculer yytext.
  ;;
  (let loop1 ((i (- (string-length s) 6)))
    (cond ((< i 0)
	   #f)
	  ((char-ci=? (string-ref s i) #\y)
	   (let loop2 ((j 5))
	     (cond ((= j 0)
		    #t)
		   ((char-ci=? (string-ref s (+ i j))
			       (string-ref "yytext" j))
		    (loop2 (- j 1)))
		   (else
		    (loop1 (- i 1))))))
	  (else
	   (loop1 (- i 1))))))

(define (prep-set-rule-yytext? rule)
  ;;Note dans une regle si son action a besoin de yytext.
  ;;
  (let ((action (<rule>-action rule)))
    (<rule>-yytext?-set! rule (prep-detect-yytext action))))

(define (prep-set-rules-yytext? rules)
  ;;Note dans toutes les regles si leurs actions ont besoin de yytext.
  ;;
  (let loop ((n (- (vector-length rules) 1)))
    (if (>= n 0)
	(begin
	  (prep-set-rule-yytext? (vector-ref rules n))
	  (loop (- n 1))))))


;;;; module output.scm

(define (output input-file
		library-spec library-language library-imports
		table-name pretty? counters-type lexer-format
		output-file output-port output-value
		<<EOF>>-action <<ERROR>>-action rules nl-start no-nl-start arcs acc)
  ;;Print the output code.
  ;;

  (define (main)
    (receive (opened-file? value-getter)
	(cond (output-value
	       (receive (sport getter)
		   (open-string-output-port)
		 (set! output-port sport)
		 (values #f getter)))
	      ((and output-file (not output-port))
	       (set! output-port (open-file-output-port output-file
							(file-options no-fail)
							(buffer-mode block)
							(native-transcoder)))
	       (values #t #f)))
      (unwind-protect
	  (begin
	    (when library-spec
	      (display (string-append "#!r6rs\n(library "
				      (library-spec->string-spec library-spec)
				      "\n"
				      "  (export\n"
				      "    " (table-name->export-name table-name) ")\n"
				      "  (import ")
		       output-port)
	      (write library-language output-port)
	      (newline output-port)
	      (display "(vicare parser-tools silex input-system)\n" output-port)
	      (display "(ikarus system $fx)\n" output-port)
	      (for-each (lambda (spec)
			  (write spec output-port)
			  (newline output-port))
		library-imports)
	      (display ")\n\n" output-port))
	    (out-print-table input-file table-name pretty? counters-type lexer-format
			     <<EOF>>-action <<ERROR>>-action rules
			     nl-start no-nl-start arcs acc
			     output-port)
	    (when library-spec
	      (display "\n) ; end of library\n\n" output-port)))
	(flush-output-port output-port)
	(when opened-file?
	  (close-output-port output-port)))
      (or (not value-getter)
	  ;;Make the output value.
	  (let ((ell (read (open-string-input-port (value-getter)))))
	    (eval ell (if (eq? lexer-format 'code)
			  (apply environment library-language
				 '(vicare parser-tools silex input-system)
				 '(vicare system $fx)
				 library-imports)
			(apply environment library-language library-imports)))))))

  (define (library-spec->string-spec spec)
    ;;We allow the library specification  to be: a string, including the
    ;;parentheses; a symbol, to which  parentheses will be added; a list
    ;;of values, which will be converted to string.
    ;;
    (cond ((string? spec)
	   spec)
	  ((symbol? spec)
	   (string-append "(" (symbol->string spec) ")"))
	  ((list? spec)
	   (let-values (((port getter)
			 (open-string-output-port)))
	     (write spec port)
	     (getter)))
	  (else
	   (assertion-violation 'library-spec->string-spec
	     "invalid library name specification" spec))))

  (define (table-name->export-name table-name)
    ;;We allow the table name to be specified as string or symbol.
    ;;
    (cond ((string? table-name)
	   table-name)
	  ((symbol? table-name)
	   (symbol->string table-name))
	  (else
	   (assertion-violation 'table-name->export-name
	     "invalid table name specification" table-name))))


;;;Nettoie les actions en enlevant les lignes blanches avant et apres.
;;;Here we are still inside the OUTPUT function.

  (define (out-split-in-lines s)
    (let ((len (string-length s)))
      (let loop ((i 0) (start 0))
	(cond ((= i len)
	       '())
	      ((char=? (string-ref s i) #\newline)
	       (cons (substring s start (+ i 1))
		     (loop (+ i 1) (+ i 1))))
	      (else
	       (loop (+ i 1) start))))))

  (define (out-empty-line? s)
    (let ((len (- (string-length s) 1)))
      (let loop ((i 0))
	(cond ((= i len)
	       #t)
	      ((char-whitespace? (string-ref s i))
	       (loop (+ i 1)))
	      (else
	       #f)))))

  (define (out-remove-empty-lines lines)
    ;;Enleve les lignes vides dans une liste avant et apres l'action.
    ;;
    (let loop ((lines lines) (top? #t))
      (if (null? lines)
	  '()
	(let ((line (car lines)))
	  (cond ((not (out-empty-line? line))
		 (cons line (loop (cdr lines) #f)))
		(top?
		 (loop (cdr lines) #t))
		(else
		 (let ((rest (loop (cdr lines) #f)))
		   (if (null? rest)
		       '()
		     (cons line rest)))))))))

  (define (out-clean-action s)
    ;;Enleve les lignes vides avant et apres l'action.
    ;;
    (let* ((lines (out-split-in-lines s))
	   (clean-lines (out-remove-empty-lines lines)))
      (big-string-append clean-lines)))

  ;;Pretty-printer  pour les  booleens, la  liste vide,  les  nombres, les
  ;;symboles, les caracteres, les chaines, les listes et les vecteurs.


  (define out-max-col 76)
		;colonne  limite  pour   le  pretty-printer  (a  ne  pas
		;atteindre)

  (define (out-flatten-list ll)
    (let loop ((ll ll) (part-out '()))
      (if (null? ll)
	  part-out
	(let* ((new-part-out (loop (cdr ll) part-out))
	       (head (car ll)))
	  (cond ((null? head)
		 new-part-out)
		((pair? head)
		 (loop head new-part-out))
		(else
		 (cons head new-part-out)))))))

  (define (out-force-string obj)
    (if (char? obj)
	(string obj)
      obj))

		; Transforme une liste impropre en une liste propre qui s'ecrit
		; de la meme facon
  (define out-regular-list
    (let ((symbolic-dot (string->symbol ".")))
      (lambda (p)
	(let ((tail (cdr p)))
	  (cond ((null? tail)
		 p)
		((pair? tail)
		 (cons (car p) (out-regular-list tail)))
		(else
		 (list (car p) symbolic-dot tail)))))))

		; Cree des chaines d'espaces de facon paresseuse
  (define out-blanks
    (let ((cache-v (make-vector 80 #f)))
      (lambda (n)
	(or (vector-ref cache-v n)
	    (let ((result (make-string n #\space)))
	      (vector-set! cache-v n result)
	      result)))))

  (define (out-separate text-l sep)
    ;;Insere le separateur entre chaque element d'une liste non-vide.
    ;;
    (if (null? (cdr text-l))
	text-l
      (cons (car text-l) (cons sep (out-separate (cdr text-l) sep)))))

  (define (out-pp-columns left right wmax txt&lens)
    ;;Met des donnees en colonnes.  Retourne comme out-pp-aux-list.
    ;;
    (let loop1 ((tls     txt&lens)
		(lwmax   0)
		(lwlast  0)
		(lines  '()))
      (if (null? tls)
	  (vector #t 0 lwmax lwlast (reverse lines))
	(let loop2 ((tls tls) (len 0) (first? #t) (prev-pad 0) (line '()))
	  (cond ((null? tls)
		 (loop1 tls
			(max len lwmax)
			len
			(cons (reverse line) lines)))
		((> (+ left len prev-pad 1 wmax) out-max-col)
		 (loop1 tls
			(max len lwmax)
			len
			(cons (reverse line) lines)))
		(first?
		 (let ((text     (caar tls))
		       (text-len (cdar tls)))
		   (loop2 (cdr tls)
			  (+ len text-len)
			  #f
			  (- wmax text-len)
			  (cons text line))))
		((pair? (cdr tls))
		 (let* ((prev-pad-s (out-blanks prev-pad))
			(text     (caar tls))
			(text-len (cdar tls)))
		   (loop2 (cdr tls)
			  (+ len prev-pad 1 text-len)
			  #f
			  (- wmax text-len)
			  (cons text (cons " " (cons prev-pad-s line))))))
		(else
		 (let ((prev-pad-s (out-blanks prev-pad))
		       (text     (caar tls))
		       (text-len (cdar tls)))
		   (if (> (+ left len prev-pad 1 text-len) right)
		       (loop1 tls
			      (max len lwmax)
			      len
			      (cons (reverse line) lines))
		     (loop2 (cdr tls)
			    (+ len prev-pad 1 text-len)
			    #f
			    (- wmax text-len)
			    (append (list text " " prev-pad-s)
				    line))))))))))

  (define (out-pp-aux-list l left right)
    ;;Retourne un vecteur:
    ;;
    ;;	#( multiline? width-all width-max width-last text-l )
    ;;
    (let loop ((l l) (multi? #f) (wall -1) (wmax -1) (wlast -1) (txt&lens '()))
      (if (null? l)
	  (cond (multi?
		 (vector #t wall wmax wlast (map car (reverse txt&lens))))
		((<= (+ left wall) right)
		 (vector #f wall wmax wlast (map car (reverse txt&lens))))
		((<= (+ left wmax 1 wmax) out-max-col)
		 (out-pp-columns left right wmax (reverse txt&lens)))
		(else
		 (vector #t wall wmax wlast (map car (reverse txt&lens)))))
	(let* ((obj (car l))
	       (last? (null? (cdr l)))
	       (this-right (if last? right out-max-col))
	       (result (out-pp-aux obj left this-right))
	       (obj-multi? (vector-ref result 0))
	       (obj-wmax   (vector-ref result 1))
	       (obj-wlast  (vector-ref result 2))
	       (obj-text   (vector-ref result 3)))
	  (loop (cdr l)
		(or multi? obj-multi?)
		(+ wall obj-wmax 1)
		(max wmax obj-wmax)
		obj-wlast
		(cons (cons obj-text obj-wmax) txt&lens))))))

  (define (out-pp-aux obj left right)
    ;;Retourne un vecteur:
    ;;
    ;;	#( multiline? wmax wlast text )
    ;;
    (cond ((boolean? obj)
	   (vector #f 2 2 (if obj '("#t") '("#f"))))
	  ((null? obj)
	   (vector #f 2 2 '("()")))
	  ((number? obj)
	   (let* ((s (number->string obj))
		  (len (string-length s)))
	     (vector #f len len (list s))))
	  ((symbol? obj)
	   (let* ((s (symbol->string obj))
		  (len (string-length s)))
	     (vector #f len len (list s))))
	  ((char? obj)
	   (cond ((char=? obj #\space)
		  (vector #f 7 7 (list "#\\space")))
		 ((char=? obj #\newline)
		  (vector #f 9 9 (list "#\\newline")))
		 (else
		  (vector #f 3 3 (list "#\\" obj)))))
	  ((string? obj)
	   (let loop ((i (- (string-length obj) 1))
		      (len 1)
		      (text '("\"")))
	     (if (= i -1)
		 (vector #f (+ len 1) (+ len 1) (cons "\"" text))
	       (let ((c (string-ref obj i)))
		 (cond ((char=? c #\\)
			(loop (- i 1) (+ len 2) (cons "\\\\" text)))
		       ((char=? c #\")
			(loop (- i 1) (+ len 2) (cons "\\\"" text)))
		       (else
			(loop (- i 1) (+ len 1) (cons (string c) text))))))))
	  ((pair? obj)
	   (let* ((l (out-regular-list obj))
		  (result (out-pp-aux-list l (+ left 1) (- right 1)))
		  (multiline? (vector-ref result 0))
		  (width-all  (vector-ref result 1))
		  (width-max  (vector-ref result 2))
		  (width-last (vector-ref result 3))
		  (text-l     (vector-ref result 4)))
	     (if multiline?
		 (let* ((sep (list #\newline (out-blanks left)))
			(formatted-text (out-separate text-l sep))
			(text (list "(" formatted-text ")")))
		   (vector #t
			   (+ (max width-max (+ width-last 1)) 1)
			   (+ width-last 2)
			   text))
	       (let* ((sep (list " "))
		      (formatted-text (out-separate text-l sep))
		      (text (list "(" formatted-text ")")))
		 (vector #f (+ width-all 2) (+ width-all 2) text)))))
	  ((and (vector? obj) (zero? (vector-length obj)))
	   (vector #f 3 3 '("#()")))
	  ((vector? obj)
	   (let* ((l (vector->list obj))
		  (result (out-pp-aux-list l (+ left 2) (- right 1)))
		  (multiline? (vector-ref result 0))
		  (width-all  (vector-ref result 1))
		  (width-max  (vector-ref result 2))
		  (width-last (vector-ref result 3))
		  (text-l     (vector-ref result 4)))
	     (if multiline?
		 (let* ((sep (list #\newline (out-blanks (+ left 1))))
			(formatted-text (out-separate text-l sep))
			(text (list "#(" formatted-text ")")))
		   (vector #t
			   (+ (max width-max (+ width-last 1)) 2)
			   (+ width-last 3)
			   text))
	       (let* ((sep (list " "))
		      (formatted-text (out-separate text-l sep))
		      (text (list "#(" formatted-text ")")))
		 (vector #f (+ width-all 3) (+ width-all 3) text)))))
	  (else
	   (display "Internal error: out-pp")
	   (newline))))

		; Retourne la chaine a afficher
  (define (out-pp obj col)
    (let* ((list-rec-of-strings-n-chars
	    (vector-ref (out-pp-aux obj col out-max-col) 3))
	   (list-of-strings-n-chars
	    (out-flatten-list list-rec-of-strings-n-chars))
	   (list-of-strings
	    (map out-force-string list-of-strings-n-chars)))
      (big-string-append list-of-strings)))

  (define (out-np obj start)
    ;;Nice-printer, plus rapide mais moins beau que le pretty-printer.
    (letrec ((line-pad
	      (string-append "\n"
			     (out-blanks (- start 1))))
	     (step-line
	      (lambda (p)
		(set-car! p line-pad)))
	     (p-bool
	      (lambda (obj col objw texts hole cont)
		(let ((text (if obj "#t" "#f")))
		  (cont (+ col 2) (+ objw 2) (cons text texts) hole))))
	     (p-number
	      (lambda (obj col objw texts hole cont)
		(let* ((text (number->string obj))
		       (len (string-length text)))
		  (cont (+ col len) (+ objw len) (cons text texts) hole))))
	     (p-symbol
	      (lambda (obj col objw texts hole cont)
		(let* ((text (symbol->string obj))
		       (len (string-length text)))
		  (cont (+ col len) (+ objw len) (cons text texts) hole))))
	     (p-char
	      (lambda (obj col objw texts hole cont)
		(let* ((text
			(cond ((char=? obj #\space) "#\\space")
			      ((char=? obj #\newline) "#\\newline")
			      (else (string-append "#\\" (string obj)))))
		       (len (string-length text)))
		  (cont (+ col len) (+ objw len) (cons text texts) hole))))
	     (p-list
	      (lambda (obj col objw texts hole cont)
		(p-tail obj (+ col 1) (+ objw 1) (cons "(" texts) hole cont)))
	     (p-vector
	      (lambda (obj col objw texts hole cont)
		(p-list (vector->list obj)
			(+ col 1) (+ objw 1) (cons "#" texts) hole cont)))
	     (p-tail
	      (lambda (obj col objw texts hole cont)
		(if (null? obj)
		    (cont (+ col 1) (+ objw 1) (cons ")" texts) hole)
		  (p-obj (car obj) col objw texts hole
			 (make-cdr-cont obj cont)))))
	     (make-cdr-cont
	      (lambda (obj cont)
		(lambda (col objw texts hole)
		  (cond ((null? (cdr obj))
			 (cont (+ col 1) (+ objw 1) (cons ")" texts) hole))
			((> col out-max-col)
			 (step-line hole)
			 (let ((hole2 (cons " " texts)))
			   (p-cdr obj (+ start objw 1) 0 hole2 hole2 cont)))
			(else
			 (let ((hole2 (cons " " texts)))
			   (p-cdr obj (+ col 1) 0 hole2 hole2 cont)))))))
	     (p-cdr
	      (lambda (obj col objw texts hole cont)
		(if (pair? (cdr obj))
		    (p-tail (cdr obj) col objw texts hole cont)
		  (p-dot col objw texts hole
			 (make-cdr-cont (list #f (cdr obj)) cont)))))
	     (p-dot
	      (lambda (col objw texts hole cont)
		(cont (+ col 1) (+ objw 1) (cons "." texts) hole)))
	     (p-obj
	      (lambda (obj col objw texts hole cont)
		(cond ((boolean? obj)
		       (p-bool obj col objw texts hole cont))
		      ((number? obj)
		       (p-number obj col objw texts hole cont))
		      ((symbol? obj)
		       (p-symbol obj col objw texts hole cont))
		      ((char? obj)
		       (p-char obj col objw texts hole cont))
		      ((or (null? obj) (pair? obj))
		       (p-list obj col objw texts hole cont))
		      ((vector? obj)
		       (p-vector obj col objw texts hole cont))))))
      (p-obj obj start 0 '() (cons #f #f)
	     (lambda (col objw texts hole)
	       (if (> col out-max-col)
		   (step-line hole))
	       (big-string-append (reverse texts))))))


;;;Main  output table  function.  Here  we are  still inside  the OUTPUT
;;;function.

  (define (out-print-table input-file table-name pretty? counters-type lexer-format
			   <<EOF>>-action <<ERROR>>-action rules
			   nl-start no-nl-start arcs-v acc-v
			   output-port)
    ;;Print the lexer table.
    ;;
    (define (%display stuff)
      (display stuff output-port))
    (define (%write stuff)
      (write stuff output-port))
    (define (%newline)
      (newline output-port))

    (let* ((counters-param-list	(case  counters-type
		;NOTE: The leading space in the result is important.
				  ((none)	")")
				  ((line)	" yyline)")
				  (else		" yyline yycolumn yyoffset)")))
	   (counters-param-list-short
	    (if (char=? (string-ref counters-param-list 0) #\space)
		(substring counters-param-list
			   1
			   (string-length counters-param-list))
	      counters-param-list))
	   (clean-eof-action	(out-clean-action <<EOF>>-action))
	   (clean-error-action	(out-clean-action <<ERROR>>-action))
	   (rule-op		(lambda (rule)
				  (out-clean-action (<rule>-action rule))))
	   (rules-l		(vector->list rules))
	   (clean-actions-l	(map rule-op rules-l))
	   (yytext?-l		(map <rule>-yytext? rules-l)))

      ;;Preamble of comments.
      (%display ";\n")
      (%display "; Table generated from the file ")
      (%display input-file)
      (%display " by SILex 1.0")
      (%newline)
      (%display ";\n\n")

      ;;Print the opening of the table.
      (when table-name
	(%display "(define ")
	(%display table-name)
	(%newline))
      (%display "  (vector\n")

      ;;Print the description of the selected counters.  This is the value
      ;;of the "counters" option.
      (%display "   '")
      (%write counters-type)
      (%newline)

      ;;Print  the  action function  to  call  when  the lexer  finds  the
      ;;end-of-file.
      (%display "   (lambda (yycontinue yygetc yyungetc)\n")
      (%display "     (lambda (yytext")
      (%display counters-param-list)
      (%newline)
      (%display clean-eof-action)
      (%display "       ))\n")

      ;;Print the action function to call when the lexer finds an error in
      ;;the input.
      (%display "   (lambda (yycontinue yygetc yyungetc)\n")
      (%display "     (lambda (yytext")
      (%display counters-param-list)
      (%newline)
      (%display clean-error-action)
      (%display "       ))\n")

      ;;Print the subvector of action functions for the lexer rules.
      (%display "   (vector\n")
      (let loop ((al clean-actions-l)
		 (yyl yytext?-l))
	(when (pair? al)
	  (let ((yytext? (car yyl)))
	    (%display "    ")
	    (%write yytext?)
	    (%newline)
	    (%display "    (lambda (yycontinue yygetc yyungetc)\n")
	    (if yytext?
		(begin
		  (%display "      (lambda (yytext")
		  (%display counters-param-list))
	      (begin
		(%display "      (lambda (")
		(%display counters-param-list-short)))
	    (%newline)
	    (%display (car al))
	    (%display "        ))")
	    (when (pair? (cdr al))
	      (%newline))
	    (loop (cdr al) (cdr yyl)))))
      (%display ")\n")
		;close the subvector of action functions

      ;;Print  the  automaton  in  one  of the  three  supported  formats:
      ;;portable, scheme code, raw data.
      (case lexer-format
	((portable)
	 (out-print-table-chars pretty?
				nl-start no-nl-start arcs-v acc-v
				output-port))
	((code)
	 (out-print-table-code counters-type (vector-length rules) yytext?-l
			       nl-start no-nl-start arcs-v acc-v
			       output-port))
	((decision-tree)
	 (out-print-table-data pretty?
			       nl-start no-nl-start arcs-v acc-v
			       output-port))
	(else
	 (assertion-violation 'lex
	   "unknown lexer output format" lexer-format)))

      ;;Terminate the table vector and the DEFINE, is one was opened.
      (%display (if table-name "))\n" ")\n"))))


;;;Auxiliary output table function.  Here we are still inside the OUTPUT
;;;function.

  (define (out-print-table-data pretty? nl-start no-nl-start arcs-v acc-v output-port)
    ;;Print the table in the decision tree format, which is the raw data
    ;;format.
    ;;
    (define (%display stuff)
      (display stuff output-port))
    (define (%write stuff)
      (write stuff output-port))
    (define (%newline)
      (newline output-port))

    (let* ((len (vector-length arcs-v))
	   (trees-v (make-vector len)))
      (let loop ((i 0))
	(when (< i len)
	  (vector-set! trees-v i (prep-arcs->tree (vector-ref arcs-v i)))
	  (loop (+ i 1))))

		; Decrire le format de l'automate
      (%display "   'decision-trees")
      (%newline)

		; Ecrire l'etat de depart pour le cas "debut de la ligne"
      (%display "   ")
      (%write nl-start)
      (%newline)

		; Ecrire l'etat de depart pour le cas "pas au debut de la ligne"
      (%display "   ")
      (%write no-nl-start)
      (%newline)

		; Ecrire la table de transitions
      (%display "   '")
      (if pretty?
	  (%display (out-pp trees-v 5))
	(%display (out-np trees-v 5)))
      (%newline)

		; Ecrire la table des acceptations
      (%display "   '")
      (if pretty?
	  (%display (out-pp acc-v 5))
	(%display (out-np acc-v 5)))))


;;;Auxiliary output table function.  Here we are still inside the OUTPUT
;;;function.

  (define (out-print-table-chars pretty? nl-start no-nl-start arcs-v acc-v output-port)
    ;;Print the automation in the portable format.
    ;;
    (define (%display stuff)
      (display stuff output-port))
    (define (%write stuff)
      (write stuff output-port))
    (define (%newline)
      (newline output-port))

    (let* ((len		(vector-length arcs-v))
	   (portable-v	(make-vector len))
	   (arc-op	(lambda (arc)
			  (cons (class->tagged-char-list (car arc))
				(cdr arc)))))
      (let loop ((s 0))
	(when (< s len)
	  (let* ((arcs	  (vector-ref arcs-v s))
		 (port-arcs (map arc-op arcs)))
	    (vector-set! portable-v s port-arcs)
	    (loop (+ s 1)))))
      ;; Decrire le format de l'automate
      (%display "   'tagged-chars-lists")
      (%newline)
      ;; Ecrire l'etat de depart pour le cas "debut de la ligne"
      (%display "   ")
      (%write nl-start)
      (%newline)
      ;; Ecrire l'etat de depart pour le cas "pas au debut de la ligne"
      (%display "   ")
      (%write no-nl-start)
      (%newline)
      ;; Ecrire la table de transitions
      (%display "   '")
      (%display ((if pretty? out-pp out-np) portable-v 5))
      (%newline)
      ;; Ecrire la table des acceptations
      (%display "   '")
      (%display ((if pretty? out-pp out-np) acc-v 5))))


;;;Auxiliary output table function.  Here we are still inside the OUTPUT
;;;function.

  (define (out-print-code-trans3 margin tree action-var output-port)
    ;;Generate the automaton in  Scheme code form.
    ;;
    (define (%display stuff)
      (display stuff output-port))
    (define (%write stuff)
      (write stuff output-port))
    (define (%newline)
      (newline output-port))

    (%newline)
    (%display (out-blanks margin))
    (cond ((eq? tree 'err)
	   (%display action-var))
	  ((number? tree)
	   (%display "(state-")
	   (%display tree)
	   (%display " ")
	   (%display action-var)
	   (%display ")"))
	  ((eq? (car tree) '=)
	   (%display "(if ($fx= c ")
	   (%display (list-ref tree 1))
	   (%display ")")
	   (out-print-code-trans3 (+ margin 4)
				  (list-ref tree 2)
				  action-var
				  output-port)
	   (out-print-code-trans3 (+ margin 4)
				  (list-ref tree 3)
				  action-var
				  output-port)
	   (%display ")"))
	  (else
	   (%display "(if ($fx< c ")
	   (%display (list-ref tree 0))
	   (%display ")")
	   (out-print-code-trans3 (+ margin 4)
				  (list-ref tree 1)
				  action-var
				  output-port)
	   (out-print-code-trans3 (+ margin 4)
				  (list-ref tree 2)
				  action-var
				  output-port)
	   (%display ")"))))


;;;Auxiliary  output table  functions.   Here we  are  still inside  the
;;;OUTPUT function.

  (define (out-print-code-trans2 margin tree action-var output-port)
    (display (string-append
	      "\n"
	      (out-blanks margin)
	      "(if c")
	     output-port)
    (out-print-code-trans3 (+ margin 4) tree action-var output-port)
    (display (string-append
	      "\n"
	      (out-blanks (+ margin 4))
	      action-var ")")
	     output-port))

  (define (out-print-code-trans1 margin tree action-var output-port)
    (newline output-port)
    (display (out-blanks margin) output-port)
    (if (eq? tree 'err)
	(display action-var output-port)
      (begin
	(display "(let ((c (read-char)))" output-port)
	(out-print-code-trans2 (+ margin 2) tree action-var output-port)
	(display ")" output-port))))


;;;Auxiliary output table function.  Here we are still inside the OUTPUT
;;;function.

  (define (out-print-table-code counters nbrules yytext?-l
				nl-start no-nl-start arcs-v acc-v
				output-port)
    (define (%display stuff)
      (display stuff output-port))
    (define (%write stuff)
      (write stuff output-port))
    (define (%newline)
      (newline output-port))

    (let-values (((counters-params counters-params-short)
		  (case counters
		    ((none) (values ")" ")"))
		    ((line) (values " yyline)"
				    "yyline)"))
		    ((all)  (values " yyline yycolumn yyoffset)"
				    "yyline yycolumn yyoffset)")))))
      (let* ((nbstates (vector-length arcs-v))
	     (trees-v (make-vector nbstates)))
	(let loop ((s 0))
	  (if (< s nbstates)
	      (begin
		(vector-set! trees-v s (prep-arcs->tree (vector-ref arcs-v s)))
		(loop (+ s 1)))))

	;;Print the format of the automaton.
	(%display "   'code\n" )

	(%display (string-append
		   ;;Ecrire l'entete de la fonction
		   "   (lambda (<<EOF>>-pre-action\n"
		   "            <<ERROR>>-pre-action\n"
		   "            rules-pre-action\n"
		   "            IS)\n"
		   ;;Ecrire le  debut du letrec et  les variables d'actions
		   ;;brutes.
		   "     (letrec\n"
		   "         ((user-action-<<EOF>> #f)\n"
		   "          (user-action-<<ERROR>> #f)\n"))
	(let loop ((i 0))
	  (when (< i nbrules)
	    (%display "          (user-action-")
	    (%write i)
	    (%display " #f)")
	    (%newline)
	    (loop (+ i 1))))

	(%display (string-append
		   ;;Ecrire l'extraction des fonctions du IS.
		   "          (start-go-to-end    (<input-system>-start-go-to-end	IS))\n"
		   "          (end-go-to-point    (<input-system>-end-go-to-point	IS))\n"
		   "          (init-lexeme        (<input-system>-init-lexeme	IS))\n"
		   "          (get-start-line     (<input-system>-get-start-line	IS))\n"
		   "          (get-start-column   (<input-system>-get-start-column	IS))\n"
		   "          (get-start-offset   (<input-system>-get-start-offset	IS))\n"
		   "          (peek-left-context  (<input-system>-peek-left-context	IS))\n"
		   "          (peek-char          (<input-system>-peek-char		IS))\n"
		   "          (read-char          (<input-system>-read-char		IS))\n"
		   "          (get-start-end-text (<input-system>-get-start-end-text IS))\n"
		   "          (user-getc          (<input-system>-user-getc		IS))\n"
		   "          (user-ungetc        (<input-system>-user-ungetc	IS))\n"
		   ;;Ecrire les variables d'actions.
		   "          (action-<<EOF>>\n"
		   "           (lambda (" counters-params-short "\n"
		   "             (user-action-<<EOF>> \"\"" counters-params "))\n"
		   "          (action-<<ERROR>>\n"
		   "           (lambda (" counters-params-short "\n"
		   "             (user-action-<<ERROR>> \"\"" counters-params "))\n"))

	(let loop ((i 0)
		   (yyl yytext?-l))
	  (when (< i nbrules)
	    (%display (string-append
		       "          (action-" (number->string i) "\n"
		       "           (lambda (" counters-params-short "\n"
		       (if (car yyl)
			   (string-append
			    "             (let ((yytext (get-start-end-text)))\n"
			    "               (start-go-to-end)\n"
			    "               (user-action-" (number->string i) " yytext"
			    counters-params ")))\n")
			 (string-append
			  "             (start-go-to-end)\n"
			  "             (user-action-" (number->string i) counters-params "))\n"))))
	    (loop (+ i 1) (cdr yyl))))

	;; Ecrire les variables d'etats
	(let loop ((s 0))
	  (if (< s nbstates)
	      (let* ((tree (vector-ref trees-v s))
		     (acc (vector-ref acc-v s))
		     (acc-eol (car acc))
		     (acc-no-eol (cdr acc)))
		(%display (string-append
			   "          (state-" (number->string s) "\n"
			   "           (lambda (action)"))
		(cond ((not acc-eol)
		       (out-print-code-trans1 13 tree "action" output-port))
		      ((not acc-no-eol)
		       (%display (string-append
				  "\n"
				  (if (eq? tree 'err)
				      "             (let* ((c (peek-char))"
				    "             (let* ((c (read-char))")
				  "\n"
				  "                    (new-action (if (o"
				  "r (not c) (= c lexer-integer-newline))\n"
				  "                                  "
				  "  (begin (end-go-to-point) action-" (number->string acc-eol) ")\n"
				  "                       "
				  "             action)))"))
		       ((if (eq? tree 'err)
			    out-print-code-trans1
			  out-print-code-trans2) 15 tree "new-action" output-port)
		       (%display ")"))
		      ((< acc-eol acc-no-eol)
		       (%display
			(string-append
			 "\n"
			 "             (end-go-to-point)\n"
			 "             (let* ((c (" (if (eq? tree 'err) "peek-char" "read-char") "))\n"
			 "                    (new-action (if (or (not c) (= c lexer-integer-newline))\n"
			 "                      "
			 "              action-" (number->string acc-eol) "\n"
			 "                      "
			 "              action-" (number->string acc-no-eol) ")))"))
		       ((if (eq? tree 'err)
			    out-print-code-trans1
			  out-print-code-trans2) 15 tree "new-action" output-port)
		       (%display ")"))
		      (else
		       (let ((action-var (string-append "action-" (number->string acc-eol))))
			 (%display "\n             (end-go-to-point)")
			 (out-print-code-trans1 13 tree action-var output-port))))
		(%display "))\n")
		(loop (+ s 1)))))

		; Ecrire la variable de lancement de l'automate
	(%display
	 (string-append
	  "          (start-automaton\n"
	  "           (lambda ()\n"
	  (if (= nl-start no-nl-start)
	      (string-append
	       "             (if (peek-char)\n"
	       "                 (state-" (number->string nl-start) " action-<<ERROR>>)\n"
	       "               action-<<EOF>>)")
	    (string-append
	     "             (cond ((not (peek-char))\n"
	     "                    action-<<EOF>>)\n"
	     "                   ((= (peek-left-context) lexer-integer-newline)\n"
	     "                    (state-" (number->string nl-start) " action-<<ERROR>>))\n"
	     "                   (else\n"
	     "                    (state-" (number->string no-nl-start) " action-<<ERROR>>)))"))
	  "))\n"
	  ;; Ecrire la fonction principale de lexage
	  "          (final-lexer\n"
	  "           (lambda ()\n"
	  "             (init-lexeme)\n"
	  (cond ((eq? counters 'none)
		 "             ((start-automaton))")
		((eq? counters 'line)
		 (string-append
		  "             (let ((yyline (get-start-line)))\n"
		  "               ((start-automaton) yyline))"))
		((eq? counters 'all)
		 (string-append
		  "             (let ((yyline (get-start-line))\n"
		  "                   (yycolumn (get-start-column))\n"
		  "                   (yyoffset (get-start-offset)))\n"
		  "               ((start-automaton) yyline yycolumn yyoffset))")))
	  "))"
	  ;; Fermer les bindings du grand letrec
	  ")\n"

	  ;;Initialiser les variables user-action-XX
	  "       (set! user-action-<<EOF>>"
	  " (<<EOF>>-pre-action\n"
	  "                                  final-lexer user-getc user-ungetc))\n"
	  "       (set! user-action-<<ERROR>>"
	  " (<<ERROR>>-pre-action\n"
	  "                                    final-lexer user-getc user-ungetc))\n"))

	(let loop ((r 0))
	  (when (< r nbrules)
	    (let* ((str-r  (number->string r))
		   (blanks (out-blanks (string-length str-r))))
	      (%display (string-append
			 "       (set! user-action-" str-r " ((vector-ref rules-pre-action "
			 (number->string (+ (* 2 r) 1)) ")\n"
			 blanks
			 "                           final-lexer user-getc user-ungetc))\n"))
	      (loop (+ r 1)))))

	;; Faire retourner le lexer final
	(%display "       final-lexer))"))))


;;;; end of the OUTPUT function

  (main))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-context-to-lex-file 'scheme-indent-function 1)
;; End:
