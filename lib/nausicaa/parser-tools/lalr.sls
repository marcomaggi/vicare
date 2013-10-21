;;;
;;;Part of: Vicare Scheme
;;;Contents: LALR(1) parser generator
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	This library  is a LALR(1)  parser generator written  in Scheme.
;;;	It implements an efficient algorithm for computing the lookahead
;;;	sets.  The  algorithm is the  same as used  in GNU Bison  and is
;;;	described in:
;;;
;;;	   F.  DeRemer  and  T.  Pennello.  ``Efficient  Computation  of
;;;	   LALR(1)  Look-Ahead Set''.   TOPLAS, vol.  4, no.  4, october
;;;	   1982.
;;;
;;;	As a consequence, it is not written in a fully functional style.
;;;	In fact,  much of  the code  is a direct  translation from  C to
;;;	Scheme of the Bison sources.
;;;
;;;	The  library  is  a  port  to @rnrs{6}  Scheme  of  Lalr-scm  by
;;;	Dominique Boucher.  The original code is available at:
;;;
;;;			<http://code.google.com/p/lalr-scm/>
;;;
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Port to R6RS and Vicare integration by Marco Maggi.
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
(library (nausicaa parser-tools lalr)
  (export
    lalr-parser (rename (lalr-parser make-lalr-parser))
    library-spec:
    library-language:
    library-imports:
    default-library-imports:
    parser-type:
    parser-name:
    output-value:
    output-port:
    output-file:
    dump-table:
    expect:
    rules:
    terminals:)
  (import (rename (nausicaa)
		  (error rnrs.error))
    (prefix (nausicaa parser-tools lexical-tokens)   lt.)
    (prefix (nausicaa parser-tools source-locations) sl.)
    (prefix (vicare language-extensions makers) mk.))


;;;; helpers

(define (with-output-to-new-file pathname proc)
  (let ((port #f))
    (dynamic-wind
	(lambda ()
	  (set! port (open-file-output-port pathname
					    (file-options no-fail)
					    (buffer-mode block)
					    (native-transcoder))))
	(lambda () (proc port))
	(lambda ()
	  (close-output-port port)))))

(define-syntax position-in-list
  (syntax-rules ()
    ((_ ?element ?list)
     (list-index (lambda (elm)
		   (equal? ?element elm))
       ?list))))

(define-syntax union-of-sorted-lists-of-numbers
  (syntax-rules ()
    ((_ ell1 ell2)
     (union-of-sorted-lists/uniq ell1 ell2 < >))))

(define-syntax sorted-list-of-numbers-insert
  (syntax-rules ()
    ((_ ?item ?ell)
     (sorted-list-insert/uniq ?item ?ell < >))))

(define error
  (case-lambda
   ((message)
    (rnrs.error 'lalr-parser message))
   ((message irritants)
    (apply rnrs.error 'lalr-parser message irritants))))


;;;; list utilities
;;
;;These make this library independent from (lists).
;;

(define (list-index pred lis1)
  (let lp ((lis lis1) (n 0))
    (and (not (null? lis))
	 (if (pred (car lis))
	     n
	   (lp (cdr lis) (+ n 1))))))

(define (sorted-list-insert/uniq item ell item< item>)
  (let loop ((reversed-head '())
	     (tail          ell))
    (if (null? tail)
	(append-reverse (cons item reversed-head) tail)
      (let ((x (car tail)))
	(cond ((item< item x)
	       (append-reverse (cons item reversed-head) tail))
	      ((item> item x)
	       (loop (cons x reversed-head) (cdr tail)))
	      (else
	       ell))))))

(define (union-of-sorted-lists/uniq ell1 ell2 item< item>)
  (let loop ((result '())
	     (ell1 ell1)
	     (ell2 ell2))
    (cond ((null? ell1)    (append-reverse result ell2))
	  ((null? ell2)    (append-reverse result ell1))
	  (else
	   (let ((x (car ell1))
		 (y (car ell2)))
	     (cond
	      ((item> x y)
	       (loop (cons y result) ell1 (cdr ell2)))
	      ((item< x y)
	       (loop (cons x result) (cdr ell1) ell2))
	      (else
	       (loop result (cdr ell1) ell2))))))))

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head)
	   (tail tail))
    (if (null? rev-head)
	tail
      (lp (cdr rev-head) (cons (car rev-head) tail)))))


;;;; bit fields
;;
;;The following functions handle Scheme vectors whose elements are exact
;;integer  numbers.  Each  integer number  is used  as a  "word" holding
;;bits: It  is meant to  hold at least LALR-BITS-PER-WORD  bits.  This
;;value is configurable.
;;
;;The whole vector  is a bit field split into words;  let's say the bits
;;per integer are 30, then the first 30 bits are in the word at index 0,
;;the next 30 bits are in the word at index 1, and so on.
;;
;;The library always uses bit  fields/vector of fixed size.  The size is
;;computed with an equivalent of:
;;
;;  (define token-set-size (+ 1 (div nterms lalr-bits-per-word)))
;;
;;in the body of LALR-PARSER.
;;
;;*NOTE* We do not have to confuse indexes in the vector with indexes of
;;bits.
;;
;;*FIXME*  These bit  fields can  probably be  replaced with  single big
;;integers;  do it  after understanding  how  they are  used.  A  simple
;;vector of  booleans could do, but  it seems we need  the OR operation,
;;which is faster with a single integer.
;;

(define lalr-bits-per-word (fixnum-width))

(define (new-set nelem)
  (make-vector nelem 0))

(define (set-bit bit-field bit-index)
  ;;Interpret V as vector of numbers, which in turn are bit fields.
  ;;
  (let* ((nbits		lalr-bits-per-word)
	 (word-index	(div  bit-index nbits))
	 (word		(expt 2 (mod bit-index nbits))))
    (vector-set! bit-field word-index
		 (bitwise-ior (vector-ref bit-field word-index) word))))

(define (bit-union v1 v2 vector-size)
  ;;Compute the OR between the bit fields and store the result in V1.
  ;;
  (do ((i 0 (+ 1 i)))
      ((= i vector-size))
    (vector-set! v1 i (bitwise-ior (vector-ref v1 i)
				   (vector-ref v2 i)))))


(define-auxiliary-syntaxes
  library-spec:
  library-language:
  library-imports:
  default-library-imports:
  parser-type:
  parser-name:
  output-value:
  output-port:
  output-file:
  dump-table:
  expect:
  rules:
  terminals:)

(mk.define-maker lalr-parser
    %lalr-parser
  ((library-spec:		#f)
   (library-language:		'(nausicaa))
   (library-imports:		'())
   (default-library-imports:	#t)
   (parser-type:		'lr)
   (parser-name:		#f)

   (output-value:		#f)
   (output-port:		#f)
   (output-file:		#f)

   (dump-table:			#f)

   (expect:			0)
   (rules:			#f)
   (terminals:			#f)))

(define (%lalr-parser library-spec library-language library-imports default-library-imports
		      parser-type parser-name
		      output-value output-port output-file
		      dump-table
		      expect rules terminals)

  (define (main)
    (set! expected-conflicts expect)
    (set! driver-name (case parser-type
			((glr)	'glr-driver)
			((lr)	'lr-driver)
			(else
			 (assertion-violation 'lalr-parser
			   "expected \"lr\" or \"glr\" as parser type"
			   parser-type))))
    (let* ((gram/actions (gen-tables! terminals rules))
	   (code         `(,driver-name ',action-table
					,(build-goto-table)
					,(build-reduction-table gram/actions))))

      (when dump-table
	(with-output-to-new-file dump-table debug:print-states))
      (let* ((imports	(append (list library-language)
				(if default-library-imports
				    `((nausicaa parser-tools lalr ,driver-name)
				      (nausicaa parser-tools lexical-tokens)
				      (vicare language-extensions sentinels))
				  '())
				library-imports))
	     (exports	`(,parser-name))
	     (code	(cond (library-spec ;generate a library
			       (unless parser-name
				 (assertion-violation 'lalr-parser
				   "parser binding name required when building a library"))
			       `(library ,library-spec
				  (export ,@exports)
				  (import ,@imports)
				  (define (,parser-name) ,code)))

			      (parser-name ;generate a DEFINE form
			       `(define (,parser-name) ,code))

			      (else ;generate a lambda
			       `(lambda () ,code)))))
	(cond (output-value
	       (eval code (apply environment imports)))
	      (output-port
	       (display "#!r6rs\n" output-port)
	       (pretty-print code output-port))
	      (output-file
	       (with-output-to-new-file output-file
					(lambda (port)
					  (display "#!r6rs\n" port)
					  (pretty-print code port))))))))


;;;; macro pour les structures de donnees

(define-record-type core
  (nongenerative vicare:parser-tools:lalr:core)
  (fields (mutable number)
	  (mutable acc-sym)
	  (mutable nitems)
	  (mutable items)))

(define-record-type shift
  (nongenerative vicare:parser-tools:lalr:shift)
  (fields (mutable number)
	  (mutable nshifts)
	  (mutable shifts)))

(define-record-type red
  (nongenerative vicare:parser-tools:lalr:red)
  (fields (mutable number)
	  (mutable nreds)
	  (mutable rules)))


;;;; state variables

;; - Constantes
(define STATE-TABLE-SIZE 1009)

;; - Tableaux
(define rrhs         #f)
(define rlhs         #f)
(define ritem        #f)
(define nullable     #f)
(define derives      #f)
(define fderives     #f)
(define firsts       #f)
(define kernel-base  #f)
(define kernel-end   #f)
(define shift-symbol #f)
(define shift-set    #f)
(define red-set      #f)
(define state-table  (make-vector STATE-TABLE-SIZE '()))
(define acces-symbol #f)
(define reduction-table #f)
(define shift-table  #f)
(define consistent   #f)
(define lookaheads   #f)
(define LA           #f)
(define LAruleno     #f)
(define lookback     #f)
(define goto-map     #f)
(define from-state   #f)
(define to-state     #f)
(define includes     #f)
(define F            #f)
(define action-table #f)

;; - Variables
(define nitems          #f)
(define nrules          #f)
(define nvars           #f)
(define nterms          #f)
(define nsyms           #f)
(define nstates         #f)
(define first-state     #f)
(define last-state      #f)
(define final-state     #f)
(define first-shift     #f)
(define last-shift      #f)
(define first-reduction #f)
(define last-reduction  #f)
(define nshifts         #f)
(define maxrhs          #f)
(define ngotos          #f)
(define token-set-size  #f)

(define driver-name     'lr-driver)


;;;; tables generation, part 1

(define (gen-tables! terminals grammar)
  ;;TERMINALS  is  the  full,  untouched,  list of  terminals  given  to
  ;;LALR-PARSER.
  ;;
  ;;GRAMMAR  is the  full, untouched,  list  of grammar  rules given  to
  ;;LALR-PARSER.
  ;;

  (define eoi '*eoi*)

  (define (check-terminal term terms)
    (cond ((not (symbol? term))
	   (error "invalid terminal" term))
	  ((member term terms)
	   (error "duplicate definition of terminal" term))))

  (define (prec->type prec)
    (cdr (assq prec '((left:     . left)
		      (right:    . right)
		      (nonassoc: . nonassoc)))))

  (define terms		#f)
  (define terms/prec	#f)
  (define nonterm-defs	#f)

  (unless (list? terminals)
    (error "Invalid token list: " terminals))
  (when (null? grammar)
    (error "grammar definition must have a non-empty list of productions"))

  ;;Validate TERMINALS and generate the values for TERMS and TERMS/PREC.
  (let check-terminals ((terminals      terminals)
			(rev-terms      '())
			(rev-terms/prec '())
			(prec-level     0))
    (if (null? terminals)
	(begin
	  (set! terms      (reverse rev-terms))
	  (set! terms/prec (reverse rev-terms/prec)))
      (let ((term (car terminals)))
	(cond ((pair? term)
	       (if (and (memq (car term) '(left: right: nonassoc:))
			(not (null? (cdr term))))
		   (let ((prec    (+ prec-level 1))
			 (optype  (prec->type (car term))))
		     (let loop-toks ((l              (cdr term))
				     (rev-terms      rev-terms)
				     (rev-terms/prec rev-terms/prec))
		       (if (null? l)
			   (check-terminals (cdr terminals) rev-terms rev-terms/prec prec)
			 (let ((term (car l)))
			   (check-terminal term rev-terms)
			   (loop-toks (cdr l)
				      (cons term rev-terms)
				      (cons (list term optype prec)
					    rev-terms/prec))))))
		 (error "invalid operator precedence specification" term)))
	      (else
	       (check-terminal term rev-terms)
	       (check-terminals (cdr terminals)
				(cons term rev-terms)
				(cons (list term 'none 0) rev-terms/prec)
				prec-level))))))

  ;;Validate GRAMMAR and generate the value for NONTERM-DEFS.
  (let check-grammar ((grammar          grammar)
		      (rev-nonterm-defs '()))
    (if (null? grammar)
	(set! nonterm-defs (reverse rev-nonterm-defs))
      (let ((def (car grammar)))
	(unless (pair? def)
	  (error "Nonterminal definition must be a non-empty list" '()))
	(let ((nonterm (car def)))
	  (cond ((not (symbol? nonterm))
		 (error "Invalid nonterminal:" nonterm))
		((or (member nonterm terms)
		     (assoc  nonterm rev-nonterm-defs))
		 (error "Nonterminal previously defined:" nonterm))
		(else
		 (check-grammar (cdr grammar) (cons def rev-nonterm-defs))))))))

  (let* ((terms		`(,eoi error                  . ,terms))
	 (terms/prec	`((eoi none 0) (error none 0) . ,terms/prec))
	 (nonterms	`(*start*                     . ,(map car nonterm-defs)))
	 (defs		`((*start* (,(cadr nonterms) ,eoi) : $1) . ,nonterm-defs))
	 (compiled-nonterm-defs (let loop-defs ((defs      defs)
						(ruleno    0)
						(comp-defs '()))
				  (if (null? defs)
				      (reverse comp-defs)
				    (let ((compiled-def (rewrite-nonterm-def (car defs) ruleno
									     terms nonterms)))
				      (loop-defs (cdr defs)
						 (+ ruleno (length compiled-def))
						 (cons compiled-def comp-defs))))))
	 (gram    (map (lambda (x)
			 (cons (caaar x) (map cdar x)))
		    compiled-nonterm-defs))
	 (actions (apply append compiled-nonterm-defs)))
    (set! the-terminals/prec (list->vector terms/prec))
    (set! the-terminals (list->vector terms))
    (set! the-nonterminals (list->vector nonterms))
    (set! nterms (length terms))
    (set! nvars  (length nonterms))
    (set! nsyms  (+ nterms nvars))
    (let ((no-of-rules (length actions))
	  (no-of-items (let loop ((l actions) (count 0))
			 (if (null? l)
			     count
			   (loop (cdr l) (+ count (length (caar l))))))))
      (pack-grammar no-of-rules no-of-items gram)
      (set-derives)
      (set-nullable)
      (generate-states)
      (lalr)
      (build-tables)
      (compact-action-table terms)
      ;;The LR  generator produces action  tables as lists  of key/value
      ;;pairs.   The GLR generator  produces action  tables as  lists of
      ;;key/list pairs.
      (when (eq? 'lr-driver driver-name)
	(action-table-list->alist))
      actions)))


;;;; tables generation, part 2

(define (rewrite-nonterm-def nonterm-def ruleno terms nonterms)

  (define (encode category)
    (or (position-in-list category nonterms)
	(let ((pos (position-in-list category terms)))
	  (if pos
	      (+ pos (length nonterms))
	    (error "undefined symbol in nonterminal definition" category)))))

  (define (process-prec-directive right-hand-side ruleno)
    (let loop ((l right-hand-side))
      (if (null? l)
	  '()
	(let ((first (car l))
	      (rest  (cdr l)))
	  (cond ((or (member first terms) (member first nonterms))
		 (cons first (loop rest))) ;non-recursive exit
		((and (pair? first)
		      (eq? (car first) 'prec:))
		 (if (and (pair? (cdr first))
			  (null? (cddr first))
			  (member (cadr first) terms))
		     (if (null? rest)
			 (begin
			   (add-rule-precedence! ruleno (position-in-list (cadr first) terms))
			   (loop rest))
		       (error "\"prec:\" directive should be at end of rule" right-hand-side))
		   (error "invalid \"prec:\" directive" first)))
		(else
		 (error "invalid terminal or nonterminal" first)))))))

  (define (check-error-production right-hand-side)
    (let loop ((right-hand-side right-hand-side))
      (when (pair? right-hand-side)
	(if (and (eq? (car right-hand-side) 'error)
		 (or (null? (cdr right-hand-side))
		     (not (member (cadr right-hand-side) terms))
		     (not (null? (cddr right-hand-side)))))
	    (error "invalid 'error' production, a single terminal symbol must follow the 'error' token"
	      right-hand-side))
	(loop (cdr right-hand-side)))))

  (when (null? (cdr nonterm-def))
    (error "at least one production needed for nonterminal" nonterm-def))

  (let loop ((lst (cdr nonterm-def))
	     (i 1)
	     (rev-productions-and-actions '()))
    (if (null? lst)
	(reverse rev-productions-and-actions)
      (let* ((right-hand-side	(process-prec-directive (car lst) (+ ruleno i -1)))
	     (rest		(cdr lst))
	     (prod		(map encode (cons (car nonterm-def) right-hand-side))))
	;;Check for undefined tokens.
	;;
	;;FIXME This has been already  done by ENCODE, no? (Marco Maggi;
	;;Thu Aug 6, 2009)
	(for-each (lambda (x)
		    (when (not (or (member x terms) (member x nonterms)))
		      (error "invalid terminal or nonterminal" x)))
	  right-hand-side)

	(check-error-production right-hand-side)

	;;The  following form detects  the presence  of the  ": ---"
	;;trailer in a rule definition, that is it detects if a rule
	;;has a semantic action.
	(if (and (pair? rest)
		 (eq?   (car rest) ':)
		 (pair? (cdr rest)))
	    (loop (cddr rest)
		  (+ i 1)
		  (cons (cons prod (cadr rest))
			rev-productions-and-actions))
	  (loop rest
		(+ i 1)
		(cons (cons prod 'sentinel)
		      rev-productions-and-actions))
	  )))))


;;;; tables generation, part 3

(define (pack-grammar no-of-rules no-of-items gram)
  (set! nrules (+  no-of-rules 1))
  (set! nitems no-of-items)
  (set! rlhs (make-vector nrules #f))
  (set! rrhs (make-vector nrules #f))
  (set! ritem (make-vector (+ 1 nitems) #f))

  (let loop ((p gram) (item-no 0) (rule-no 1))
    (if (not (null? p))
	(let ((nt (caar p)))
	  (let loop2 ((prods (cdar p)) (it-no2 item-no) (rl-no2 rule-no))
	    (if (null? prods)
		(loop (cdr p) it-no2 rl-no2)
	      (begin
		(vector-set! rlhs rl-no2 nt)
		(vector-set! rrhs rl-no2 it-no2)
		(let loop3 ((rhs (car prods)) (it-no3 it-no2))
		  (if (null? rhs)
		      (begin
			(vector-set! ritem it-no3 (- rl-no2))
			(loop2 (cdr prods) (+ it-no3 1) (+ rl-no2 1)))
		    (begin
		      (vector-set! ritem it-no3 (car rhs))
		      (loop3 (cdr rhs) (+ it-no3 1))))))))))))

(define (set-derives)
  (let ((delts (make-vector (+ nrules 1) 0))
	(dset  (make-vector nvars -1)))
    (let loop ((i 1) (j 0))
      (if (< i nrules)
	  (let ((lhs (vector-ref rlhs i)))
	    (if (>= lhs 0)
		(begin
		  (vector-set! delts j (cons i (vector-ref dset lhs)))
		  (vector-set! dset lhs j)
		  (loop (+ i 1) (+ j 1)))
	      (loop (+ i 1) j)))))
    (set! derives (make-vector nvars 0))
    (let loop ((i 0))
      (if (< i nvars)
	  (let ((q (let loop2 ((j (vector-ref dset i)) (s '()))
		     (if (< j 0)
			 s
		       (let ((x (vector-ref delts j)))
			 (loop2 (cdr x) (cons (car x) s)))))))
	    (vector-set! derives i q)
	    (loop (+ i 1)))))))

(define (set-nullable)
  (set! nullable (make-vector nvars #f))
  (let ((squeue (make-vector nvars #f))
	(rcount (make-vector (+ nrules 1) 0))
	(rsets  (make-vector nvars #f))
	(relts  (make-vector (+ nitems nvars 1) #f)))
    (let loop ((r 0) (s2 0) (p 0))
      (let ((*r (vector-ref ritem r)))
	(if *r
	    (if (< *r 0)
		(let ((symbol (vector-ref rlhs (- *r))))
		  (if (and (>= symbol 0)
			   (not (vector-ref nullable symbol)))
		      (begin
			(vector-set! nullable symbol #t)
			(vector-set! squeue s2 symbol)
			(loop (+ r 1) (+ s2 1) p))))
	      (let loop2 ((r1 r) (any-tokens #f))
		(let* ((symbol (vector-ref ritem r1)))
		  (if (> symbol 0)
		      (loop2 (+ r1 1) (or any-tokens (>= symbol nvars)))
		    (if (not any-tokens)
			(let ((ruleno (- symbol)))
			  (let loop3 ((r2 r) (p2 p))
			    (let ((symbol (vector-ref ritem r2)))
			      (if (> symbol 0)
				  (begin
				    (vector-set! rcount ruleno
						 (+ (vector-ref rcount ruleno) 1))
				    (vector-set! relts p2
						 (cons (vector-ref rsets symbol)
						       ruleno))
				    (vector-set! rsets symbol p2)
				    (loop3 (+ r2 1) (+ p2 1)))
				(loop (+ r2 1) s2 p2)))))
		      (loop (+ r1 1) s2 p))))))
	  (let loop ((s1 0) (s3 s2))
	    (if (< s1 s3)
		(let loop2 ((p (vector-ref rsets (vector-ref squeue s1))) (s4 s3))
		  (if p
		      (let* ((x (vector-ref relts p))
			     (ruleno (cdr x))
			     (y (- (vector-ref rcount ruleno) 1)))
			(vector-set! rcount ruleno y)
			(if (= y 0)
			    (let ((symbol (vector-ref rlhs ruleno)))
			      (if (and (>= symbol 0)
				       (not (vector-ref nullable symbol)))
				  (begin
				    (vector-set! nullable symbol #t)
				    (vector-set! squeue s4 symbol)
				    (loop2 (car x) (+ s4 1)))
				(loop2 (car x) s4)))
			  (loop2 (car x) s4))))
		  (loop (+ s1 1) s4)))))))))

(define (set-firsts)
  (set! firsts (make-vector nvars '()))

  ;; -- initialization
  (let loop ((i 0))
    (when (< i nvars)
      (let loop2 ((sp (vector-ref derives i)))
	(if (null? sp)
	    (loop (+ i 1))
	  (let ((sym (vector-ref ritem (vector-ref rrhs (car sp)))))
	    (when (< -1 sym nvars)
	      (vector-set! firsts i (sorted-list-of-numbers-insert sym (vector-ref firsts i))))
	    (loop2 (cdr sp)))))))

  ;; -- reflexive and transitive closure
  (let loop ((continue #t))
    (when continue
      (let loop2 ((i 0) (cont #f))
	(if (>= i nvars)
	    (loop cont)
	  (let* ((x (vector-ref firsts i))
		 (y (let loop3 ((l x) (z x))
		      (if (null? l)
			  z
			(loop3 (cdr l)
			       (union-of-sorted-lists-of-numbers (vector-ref firsts (car l)) z))))))
	    (if (equal? x y)
		(loop2 (+ i 1) cont)
	      (begin
		(vector-set! firsts i y)
		(loop2 (+ i 1) #t))))))))

  (let loop ((i 0))
    (when (< i nvars)
      (vector-set! firsts i (sorted-list-of-numbers-insert i (vector-ref firsts i)))
      (loop (+ i 1)))))

(define (set-fderives)
  (set! fderives (make-vector nvars #f))
  (set-firsts)
  (let loop ((i 0))
    (if (< i nvars)
	(let ((x (let loop2 ((l (vector-ref firsts i)) (fd '()))
		   (if (null? l)
		       fd
		     (loop2 (cdr l)
			    (union-of-sorted-lists-of-numbers (vector-ref derives (car l)) fd))))))
	  (vector-set! fderives i x)
	  (loop (+ i 1))))))

(define (closure core)
  ;; Initialization
  (define ruleset (make-vector nrules #f))

  (let loop ((csp core))
    (if (not (null? csp))
	(let ((sym (vector-ref ritem (car csp))))
	  (if (< -1 sym nvars)
	      (let loop2 ((dsp (vector-ref fderives sym)))
		(if (not (null? dsp))
		    (begin
		      (vector-set! ruleset (car dsp) #t)
		      (loop2 (cdr dsp))))))
	  (loop (cdr csp)))))

  (let loop ((ruleno 1) (csp core) (itemsetv '())) ; ruleno = 0
    (if (< ruleno nrules)
	(if (vector-ref ruleset ruleno)
	    (let ((itemno (vector-ref rrhs ruleno)))
	      (let loop2 ((c csp) (itemsetv2 itemsetv))
		(if (and (pair? c)
			 (< (car c) itemno))
		    (loop2 (cdr c) (cons (car c) itemsetv2))
		  (loop (+ ruleno 1) c (cons itemno itemsetv2)))))
	  (loop (+ ruleno 1) csp itemsetv))
      (let loop2 ((c csp) (itemsetv2 itemsetv))
	(if (pair? c)
	    (loop2 (cdr c) (cons (car c) itemsetv2))
	  (reverse itemsetv2))))))


;;;;

(define (generate-states)
  (set! kernel-base	(make-vector nsyms 0))
  (set! kernel-end	(make-vector nsyms #f))
  (set! red-set		(make-vector (+ nrules 1) 0))
  (set-fderives)
  ;;Initialize states.
  (let ((p (make-core 0 #f 1 '(0))))
    (set! first-state (list p))
    (set! last-state first-state)
    (set! nstates 1))
  (let loop ((this-state first-state))
    (when (pair? this-state)
      (let* ((x  (car this-state))
	     (is (closure (core-items x))))
	(save-reductions x is)
	(new-itemsets is)
	(append-states)
	(when (> nshifts 0)
	  (save-shifts x))
	(loop (cdr this-state))))))

(define (new-itemsets itemset)
  ;; - Initialization
  (set! shift-symbol '())
  (let loop ((i 0))
    (if (< i nsyms)
	(begin
	  (vector-set! kernel-end i '())
	  (loop (+ i 1)))))

  (let loop ((isp itemset))
    (if (pair? isp)
	(let* ((i (car isp))
	       (sym (vector-ref ritem i)))
	  (if (>= sym 0)
	      (begin
		(set! shift-symbol (sorted-list-of-numbers-insert sym shift-symbol))
		(let ((x (vector-ref kernel-end sym)))
		  (if (null? x)
		      (begin
			(vector-set! kernel-base sym (cons (+ i 1) x))
			(vector-set! kernel-end sym (vector-ref kernel-base sym)))
		    (begin
		      (set-cdr! x (list (+ i 1)))
		      (vector-set! kernel-end sym (cdr x)))))))
	  (loop (cdr isp)))))

  (set! nshifts (length shift-symbol)))

(define (get-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (key  (let loop ((isp1 isp) (k 0))
		 (if (null? isp1)
		     (mod k STATE-TABLE-SIZE)
		   (loop (cdr isp1) (+ k (car isp1))))))
	 (sp   (vector-ref state-table key)))
    (if (null? sp)
	(let ((x (new-state sym)))
	  (vector-set! state-table key (list x))
	  (core-number x))
      (let loop ((sp1 sp))
	(if (and (= n (core-nitems (car sp1)))
		 (let loop2 ((i1 isp) (t (core-items (car sp1))))
		   (if (and (pair? i1)
			    (= (car i1)
			       (car t)))
		       (loop2 (cdr i1) (cdr t))
		     (null? i1))))
	    (core-number (car sp1))
	  (if (null? (cdr sp1))
	      (let ((x (new-state sym)))
		(set-cdr! sp1 (list x))
		(core-number x))
	    (loop (cdr sp1))))))))

(define (new-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (p    (make-core nstates sym n isp)))
    (if (= sym nvars) (set! final-state nstates))
    (set-cdr! last-state (list p))
    (set! last-state (cdr last-state))
    (set! nstates (+ nstates 1))
    p))


;;;;

(define (append-states)
  (set! shift-set
	(let loop ((l (reverse shift-symbol)))
	  (if (null? l)
	      '()
	    (cons (get-state (car l)) (loop (cdr l)))))))


;;;;

(define (save-shifts core)
  (let ((p (make-shift (core-number core) nshifts shift-set)))
    (if last-shift
	(begin
	  (set-cdr! last-shift (list p))
	  (set! last-shift (cdr last-shift)))
      (begin
	(set! first-shift (list p))
	(set! last-shift first-shift)))))

(define (save-reductions core itemset)
  (let ((rs (let loop ((l itemset))
	      (if (null? l)
		  '()
		(let ((item (vector-ref ritem (car l))))
		  (if (< item 0)
		      (cons (- item) (loop (cdr l)))
		    (loop (cdr l))))))))
    (if (pair? rs)
	(let ((p (make-red 0 0 0)))
	  (red-number-set! p (core-number core))
	  (red-nreds-set!  p (length rs))
	  (red-rules-set!  p rs)
	  (if last-reduction
	      (begin
		(set-cdr! last-reduction (list p))
		(set! last-reduction (cdr last-reduction)))
	    (begin
	      (set! first-reduction (list p))
	      (set! last-reduction first-reduction)))))))


;;;;

(define (lalr)
  (set! token-set-size (+ 1 (div nterms lalr-bits-per-word)))
  (set-accessing-symbol)
  (set-shift-table)
  (set-reduction-table)
  (set-max-rhs)
  (initialize-LA)
  (set-goto-map)
  (initialize-F)
  (build-relations)
  (digraph includes)
  (compute-lookaheads))

(define (set-accessing-symbol)
  (set! acces-symbol (make-vector nstates #f))
  (let loop ((l first-state))
    (when (pair? l)
      (let ((x (car l)))
	(vector-set! acces-symbol (core-number x) (core-acc-sym x))
	(loop (cdr l))))))

(define (set-shift-table)
  (set! shift-table (make-vector nstates #f))
  (let loop ((l first-shift))
    (when (pair? l)
      (let ((x (car l)))
	(vector-set! shift-table (shift-number x) x)
	(loop (cdr l))))))

(define (set-reduction-table)
  (set! reduction-table (make-vector nstates #f))
  (let loop ((l first-reduction))
    (when (pair? l)
      (let ((x (car l)))
	(vector-set! reduction-table (red-number x) x)
	(loop (cdr l))))))

(define (set-max-rhs)
  (let loop ((p 0) (curmax 0) (length 0))
    (let ((x (vector-ref ritem p)))
      (if x
	  (if (>= x 0)
	      (loop (+ p 1) curmax (+ length 1))
	    (loop (+ p 1) (max curmax length) 0))
	(set! maxrhs curmax)))))

(define (initialize-LA)
  (define (last l)
    (if (null? (cdr l))
	(car l)
      (last (cdr l))))

  (set! consistent (make-vector nstates #f))
  (set! lookaheads (make-vector (+ nstates 1) #f))

  (let loop ((count 0) (i 0))
    (if (< i nstates)
	(begin
	  (vector-set! lookaheads i count)
	  (let ((rp (vector-ref reduction-table i))
		(sp (vector-ref shift-table i)))
	    (if (and rp
		     (or (> (red-nreds rp) 1)
			 (and sp
			      (not
			       (< (vector-ref acces-symbol
					      (last (shift-shifts sp)))
				  nvars)))))
		(loop (+ count (red-nreds rp)) (+ i 1))
	      (begin
		(vector-set! consistent i #t)
		(loop count (+ i 1))))))

      (begin
	(vector-set! lookaheads nstates count)
	(let ((c (max count 1)))
	  (set! LA (make-vector c #f))
	  (do ((j 0 (+ j 1)))
	      ((= j c))
	    (vector-set! LA j (new-set token-set-size)))
	  (set! LAruleno (make-vector c -1))
	  (set! lookback (make-vector c #f)))
	(let loop ((i 0) (np 0))
	  (if (< i nstates)
	      (if (vector-ref consistent i)
		  (loop (+ i 1) np)
		(let ((rp (vector-ref reduction-table i)))
		  (if rp
		      (let loop2 ((j   (red-rules rp))
				  (np2 np))
			(if (null? j)
			    (loop (+ i 1) np2)
			  (begin
			    (vector-set! LAruleno np2 (car j))
			    (loop2 (cdr j) (+ np2 1)))))
		    (loop (+ i 1) np))))))))))

(define (set-goto-map)
  (set! goto-map (make-vector (+ nvars 1) 0))
  (let ((temp-map (make-vector (+ nvars 1) 0)))
    (let loop ((ng 0) (sp first-shift))
      (if (pair? sp)
	  (let loop2 ((i (reverse (shift-shifts (car sp)))) (ng2 ng))
	    (if (pair? i)
		(let ((symbol (vector-ref acces-symbol (car i))))
		  (if (< symbol nvars)
		      (begin
			(vector-set! goto-map symbol
				     (+ 1 (vector-ref goto-map symbol)))
			(loop2 (cdr i) (+ ng2 1)))
		    (loop2 (cdr i) ng2)))
	      (loop ng2 (cdr sp))))

	(let loop ((k 0) (i 0))
	  (if (< i nvars)
	      (begin
		(vector-set! temp-map i k)
		(loop (+ k (vector-ref goto-map i)) (+ i 1)))

	    (begin
	      (do ((i 0 (+ i 1)))
		  ((>= i nvars))
		(vector-set! goto-map i (vector-ref temp-map i)))

	      (set! ngotos ng)
	      (vector-set! goto-map nvars ngotos)
	      (vector-set! temp-map nvars ngotos)
	      (set! from-state (make-vector ngotos #f))
	      (set! to-state (make-vector ngotos #f))

	      (do ((sp first-shift (cdr sp)))
		  ((null? sp))
		(let* ((x (car sp))
		       (state1 (shift-number x)))
		  (do ((i (shift-shifts x) (cdr i)))
		      ((null? i))
		    (let* ((state2 (car i))
			   (symbol (vector-ref acces-symbol state2)))
		      (if (< symbol nvars)
			  (let ((k (vector-ref temp-map symbol)))
			    (vector-set! temp-map symbol (+ k 1))
			    (vector-set! from-state k state1)
			    (vector-set! to-state k state2))))))))))))))

(define (map-goto state symbol)
  (let loop ((low (vector-ref goto-map symbol))
	     (high (- (vector-ref goto-map (+ symbol 1)) 1)))
    (if (> low high)
	(begin
	  (display (list "Error in map-goto" state symbol)) (newline)
	  0)
      (let* ((middle (div (+ low high) 2))
	     (s (vector-ref from-state middle)))
	(cond
	 ((= s state)
	  middle)
	 ((< s state)
	  (loop (+ middle 1) high))
	 (else
	  (loop low (- middle 1))))))))

(define (initialize-F)
  (set! F (make-vector ngotos #f))
  (do ((i 0 (+ 1 i)))
      ((= i ngotos))
    (vector-set! F i (new-set token-set-size)))

  (let ((reads (make-vector ngotos #f)))

    (let loop ((i 0) (rowp 0))
      (if (< i ngotos)
	  (let* ((rowf (vector-ref F rowp))
		 (stateno (vector-ref to-state i))
		 (sp (vector-ref shift-table stateno)))
	    (if sp
		(let loop2 ((j (shift-shifts sp)) (edges '()))
		  (if (pair? j)
		      (let ((symbol (vector-ref acces-symbol (car j))))
			(if (< symbol nvars)
			    (if (vector-ref nullable symbol)
				(loop2 (cdr j) (cons (map-goto stateno symbol)
						     edges))
			      (loop2 (cdr j) edges))
			  (begin
			    (set-bit rowf (- symbol nvars))
			    (loop2 (cdr j) edges))))
		    (if (pair? edges)
			(vector-set! reads i (reverse edges))))))
	    (loop (+ i 1) (+ rowp 1)))))
    (digraph reads)))

(define (add-lookback-edge stateno ruleno gotono)
  (let ((k (vector-ref lookaheads (+ stateno 1))))
    (let loop ((found #f) (i (vector-ref lookaheads stateno)))
      (if (and (not found) (< i k))
	  (if (= (vector-ref LAruleno i) ruleno)
	      (loop #t i)
	    (loop found (+ i 1)))

	(if (not found)
	    (begin (display "Error in add-lookback-edge : ")
		   (display (list stateno ruleno gotono)) (newline))
	  (vector-set! lookback i
		       (cons gotono (vector-ref lookback i))))))))

(define (transpose r-arg n)
  (let ((new-end (make-vector n #f))
	(new-R  (make-vector n #f)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (let ((x (list 'bidon)))
	(vector-set! new-R i x)
	(vector-set! new-end i x)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (let ((sp (vector-ref r-arg i)))
	(if (pair? sp)
	    (let loop ((sp2 sp))
	      (if (pair? sp2)
		  (let* ((x (car sp2))
			 (y (vector-ref new-end x)))
		    (set-cdr! y (cons i (cdr y)))
		    (vector-set! new-end x (cdr y))
		    (loop (cdr sp2))))))))
    (do ((i 0 (+ i 1)))
	((= i n))
      (vector-set! new-R i (cdr (vector-ref new-R i))))

    new-R))

(define (build-relations)

  (define (get-state stateno symbol)
    (let loop ((j (shift-shifts (vector-ref shift-table stateno)))
	       (stno stateno))
      (if (null? j)
	  stno
	(let ((st2 (car j)))
	  (if (= (vector-ref acces-symbol st2) symbol)
	      st2
	    (loop (cdr j) st2))))))

  (set! includes (make-vector ngotos #f))
  (do ((i 0 (+ i 1)))
      ((= i ngotos))
    (let ((state1 (vector-ref from-state i))
	  (symbol1 (vector-ref acces-symbol (vector-ref to-state i))))
      (let loop ((rulep (vector-ref derives symbol1))
		 (edges '()))
	(if (pair? rulep)
	    (let ((*rulep (car rulep)))
	      (let loop2 ((rp (vector-ref rrhs *rulep))
			  (stateno state1)
			  (states (list state1)))
		(let ((*rp (vector-ref ritem rp)))
		  (if (> *rp 0)
		      (let ((st (get-state stateno *rp)))
			(loop2 (+ rp 1) st (cons st states)))
		    (begin

		      (if (not (vector-ref consistent stateno))
			  (add-lookback-edge stateno *rulep i))

		      (let loop2 ((done #f)
				  (stp (cdr states))
				  (rp2 (- rp 1))
				  (edgp edges))
			(if (not done)
			    (let ((*rp (vector-ref ritem rp2)))
			      (if (< -1 *rp nvars)
				  (loop2 (not (vector-ref nullable *rp))
					 (cdr stp)
					 (- rp2 1)
					 (cons (map-goto (car stp) *rp) edgp))
				(loop2 #t stp rp2 edgp)))

			  (loop (cdr rulep) edgp))))))))
	  (vector-set! includes i edges)))))
  (set! includes (transpose includes ngotos)))

(define (compute-lookaheads)
  (let ((n (vector-ref lookaheads nstates)))
    (let loop ((i 0))
      (if (< i n)
	  (let loop2 ((sp (vector-ref lookback i)))
	    (if (pair? sp)
		(let ((LA-i (vector-ref LA i))
		      (F-j  (vector-ref F (car sp))))
		  (bit-union LA-i F-j token-set-size)
		  (loop2 (cdr sp)))
	      (loop (+ i 1))))))))

(define (digraph relation)
  (define infinity (+ ngotos 2))
  (define INDEX (make-vector (+ ngotos 1) 0))
  (define VERTICES (make-vector (+ ngotos 1) 0))
  (define top 0)
  (define R relation)

  (define (traverse i)
    (set! top (+ 1 top))
    (vector-set! VERTICES top i)
    (let ((height top))
      (vector-set! INDEX i height)
      (let ((rp (vector-ref R i)))
	(if (pair? rp)
	    (let loop ((rp2 rp))
	      (if (pair? rp2)
		  (let ((j (car rp2)))
		    (if (zero? (vector-ref INDEX j))
			(traverse j))
		    (if (> (vector-ref INDEX i)
			   (vector-ref INDEX j))
			(vector-set! INDEX i (vector-ref INDEX j)))
		    (let ((F-i (vector-ref F i))
			  (F-j (vector-ref F j)))
		      (bit-union F-i F-j token-set-size))
		    (loop (cdr rp2))))))
	(if (= (vector-ref INDEX i) height)
	    (let loop ()
	      (let ((j (vector-ref VERTICES top)))
		(set! top (- top 1))
		(vector-set! INDEX j infinity)
		(if (not (= i j))
		    (begin
		      (bit-union (vector-ref F i)
				 (vector-ref F j)
				 token-set-size)
		      (loop)))))))))

  (let loop ((i 0))
    (if (< i ngotos)
	(begin
	  (if (and (zero? (vector-ref INDEX i))
		   (pair? (vector-ref R i)))
	      (traverse i))
	  (loop (+ i 1)))))) ; end of DIGRAPH


;;;; operator precedence management

;;A vector of  precedence descriptors where each element  is of the form
;;(terminal type precedence).
(define the-terminals/prec #f)   ; terminal symbols with precedence
		; the precedence is an integer >= 0
(define (get-symbol-precedence sym)
  (caddr (vector-ref the-terminals/prec sym)))
		; the operator type is either 'none, 'left, 'right, or 'nonassoc
(define (get-symbol-assoc encoded-terminal-symbol)
  (cadr (vector-ref the-terminals/prec encoded-terminal-symbol)))

(define rule-precedences '())
(define (add-rule-precedence! rule sym)
  (set! rule-precedences
	(cons (cons rule sym) rule-precedences)))

(define (get-rule-precedence ruleno)
;;;  (debug "ruleno ~s rule-precedences ~s ~s" ruleno rule-precedences ritem)
  (cond
   ((assv ruleno rule-precedences)
    => (lambda (p)
	 (get-symbol-precedence (cdr p))))
   (else ;process the rule symbols from left to right
    (let loop ((i    (vector-ref rrhs ruleno))
	       (prec 0))
      (let ((item (vector-ref ritem i)))
	;; end of rule
	(if (< item 0)
	    prec
	  (let ((i1 (+ i 1)))
	    (if (>= item nvars)
		;; it's a terminal symbol
		(loop i1 (get-symbol-precedence (- item nvars)))
	      (loop i1 prec)))))))))


;;;; build the various tables

(define expected-conflicts	0)
(define the-terminals		#f)	;names of terminal symbols
(define the-nonterminals	#f)	;non-terminals

(define (get-symbol n)
  (if (>= n nvars)
      (vector-ref the-terminals (- n nvars))
    (vector-ref the-nonterminals n)))

(define (build-tables)

  (define (main)
    (set! action-table (make-vector nstates '()))
    (do ((i 0 (+ i 1)))	; i = state
	((= i nstates))
      (let ((red (vector-ref reduction-table i)))
	(when (and red (>= (red-nreds red) 1))
	  (if (and (= 1 (red-nreds red))
		   (vector-ref consistent i))
	      (add-action-for-all-terminals i (- (car (red-rules red))))
	    (let ((k (vector-ref lookaheads (+ i 1))))
	      (let loop ((j (vector-ref lookaheads i)))
		(when (< j k)
		  (let ((rule (- (vector-ref LAruleno j)))
			(lav  (vector-ref LA j)))
		    (let loop2 ((token 0)
				(x (vector-ref lav 0))
				(y 1)
				(z 0))
		      (when (< token nterms)
			(let ((in-la-set? (mod x 2)))
			  (when (= in-la-set? 1)
			    (add-action i token rule)))
			(if (= y lalr-bits-per-word)
			    (loop2 (+ token 1) (vector-ref lav (+ z 1)) 1 (+ z 1))
			  (loop2 (+ token 1) (div x 2) (+ y 1) z))))
		    (loop (+ j 1)))))))))
      (let ((shiftp (vector-ref shift-table i)))
	(when shiftp
	  (let loop ((k (shift-shifts shiftp)))
	    (if (pair? k)
		(let* ((state (car k))
		       (symbol (vector-ref acces-symbol state)))
		  (when (>= symbol nvars)
		    (add-action i (- symbol nvars) state))
		  (loop (cdr k))))))))

    (add-action final-state 0 'accept)
    (log-conflicts))

  (define (add-action state symbol new-action)
    ;;Try to  add a single action  NEW-ACTION to the  entry with keyword
    ;;SYMBOL in  the alist  at index STATE  of the  vector ACTION-TABLE.
    ;;The first time this function  is called ACTION-TABLE is empty: All
    ;;its elements are null.
    ;;
    ;;STATE is a non-negative exact  integer representing a state of the
    ;;parser.  SYMBOL is a non-negative exact integer which is mapped to
    ;;a  terminal; the  higher  the value,  the  higher the  precedence.
    ;;NEW-ACTION is an  exact integer representing an action  to take to
    ;;move from a state to the  next; zero means accept and should never
    ;;be  used here,  positive  actions are  shift operations,  negative
    ;;actions are reduce operations.
    ;;
    ;;The LR  driver has a single  action for each entry  in the alists;
    ;;conflicts  between actions are  solved here.   The GLR  driver may
    ;;have multiple  actions for each  entry, a "conflict" is  solved by
    ;;adding both the actions.  If NEW-ACTION is already in the selected
    ;;alist, nothing happens.
    ;;
    ;;For  the LR  driver:  in  case of  a  reduce/reduce conflict,  the
    ;;reduction rule with the higher  absolute number wins; in case of a
    ;;shift/reduce conflict, the action is shift.
    ;;
    (let* ((state-actions (vector-ref action-table state))
	   (actions       (assv symbol state-actions)))
      (if (not actions)
	  ;;Add a new entry to the alist.
	  (vector-set! action-table state (cons (list symbol new-action) state-actions))
	(let ((top-action (cadr actions)))
	  (unless (= new-action top-action)
	    ;;There is a conflict.
	    (cond ((and (<= top-action 0) (<= new-action 0)) ;reduce/reduce conflict
		   (add-reduce/reduce-conflict-message new-action top-action symbol state)
		   (if (eq? driver-name 'glr-driver)
		       (set-cdr! (cdr actions) (cons new-action (cddr actions)))
		     (set-car! (cdr actions) (max top-action new-action))))
		  (else ;shift/reduce conflict
		   (let ((solution (resolve-conflict symbol (- top-action))))
		     (case solution
		       ((reduce)
			#f)
		       (else
			(if (eq? driver-name 'glr-driver)
			    (set-cdr! (cdr actions) (cons new-action (cddr actions)))
			  (set-car! (cdr actions) new-action))
			(unless (eq? 'shift solution)
			  (add-shift/reduce-conflict-message
			   new-action top-action symbol state))))))))))))

  (define (resolve-conflict symbol reduction-rule)
    (let ((sym-prec   (get-symbol-precedence symbol))
	  (rule-prec  (get-rule-precedence   reduction-rule)))
      (cond ((> sym-prec rule-prec) 'shift)
	    ((< sym-prec rule-prec) 'reduce)
	    (else (case (get-symbol-assoc symbol)
		    ((left)
		     'reduce)
		    ((right none nonassoc)
		     'shift)
		    (else ;this should never happen
		     (assertion-violation 'lalr-parser
		       "invalid association symbol"
		       (get-symbol-assoc symbol))))))))

  (define (add-action-for-all-terminals state action)
    (do ((i 1 (+ i 1)))
	((= i nterms))
      (add-action state i action)))

  (define conflict-messages '())

  (define (log-conflicts)
    (when (and expected-conflicts
	       (> (length conflict-messages) expected-conflicts))
      (for-each (lambda (message)
		  (for-each (lambda (s)
			      (display s (current-error-port)))
		    message)
		  (newline (current-error-port)))
	conflict-messages)))

  (define (add-reduce/reduce-conflict-message  new-action top-action symbol state)
    (add-conflict-message "%% Reduce/Reduce conflict "
			  "(reduce " (- new-action) ", reduce " (- top-action) ") on '"
			  (get-symbol (+ symbol nvars)) "' in state " state))

  (define (add-shift/reduce-conflict-message new-action top-action symbol state)
    (add-conflict-message "%% Shift/Reduce conflict "
			  "(shift " new-action ", reduce " (- top-action) ") on '"
			  (get-symbol (+ symbol nvars)) "' in state " state))

  (define (add-conflict-message . l)
    (set! conflict-messages (cons l conflict-messages)))

  (main))

(define (compact-action-table terms)
  ;;TERMS is the list of  terminal symbols.  "*eoi*" is always the first
  ;;(index 0) and "error" is always the second (index 1).
  ;;
  ;;At this  stage: the elements  in REDUCTION-TABLE are vector,  if the
  ;;state has a reduction, or #f.
  ;;
  ;;FIXME What is this compaction exactly?
  ;;
  (define (main)
    (do ((i 0 (+ i 1)))
	((= i nstates))
      (let ((acts (vector-ref action-table i)))
	(if (red? (vector-ref reduction-table i))
	    (let ((act (most-common-reduce-action acts)))
	      (vector-set! action-table i
			   (cons `(*default* ,(if act act '*error*))
				 (translate-terms
				  (filter (lambda (x)
					    (not (and (= (length x) 2)
						      (eqv? (cadr x) act))))
				    acts)))))
	  (vector-set! action-table i
		       (cons '(*default* *error*)
			     (translate-terms acts)))))))

  (define (most-common-reduce-action acts)
    ;;ACTS  is  a terminal/actions  alist.   The  keywords are  integers
    ;;representing  terminal symbols,  the values  are lists  of actions
    ;;which  can be  integers  or symbols  (like  "accept" or  "error").
    ;;Positive  integer  actions   represent  shifts,  negative  integer
    ;;actions represent reduces.
    ;;
    ;;For the LR  driver the actions list has only  one element, for the
    ;;GLR driver it can have  multiple elements.  Only the first element
    ;;in the action lists is considered.
    ;;
    ;;Return the  reduce first-element action which is  more frequent in
    ;;the action lists.
    ;;
    (let ((counters '()))
      ;;Fill COUNTERS  with an alist action/count holding  the number of
      ;;times the the  action appears in ACTS.  Only  reduce actions are
      ;;considered.
      (let loop ((acts acts))
	(when (pair? acts)
	  (let* ((action (cadar acts)) ;get the first action value
		 (entry  (assv action counters)))
	    (when (and (number? action)
		       (< action 0))
	      (if entry
		  (set-cdr! entry (+ 1 (cdr entry)))
		(set! counters (cons (cons action 1) counters))))
	    (loop (cdr acts)))))
      ;;Return the reduce action whose entry is maximum in COUNTERS.
      (let loop ((counters counters)
		 (max      0)
		 (sym      #f))
	(if (null? counters)
	    sym
	  (let* ((entry (car counters))
		 (count (cdr entry)))
	    (if (> count max)
		(loop (cdr counters) count (car entry))
	      (loop (cdr counters) max sym)))))))

  (define (translate-terms acts)
    ;;Translate   the   key-integer/integer   alist   in   ACTS   to   a
    ;;key-symbol/integer alist,  using key-integer as index  in the list
    ;;of symbols in TERMS.
    ;;
    (map (lambda (entry)
	   (cons (list-ref terms (car entry))
		 (cdr entry)))
      acts))

  (main))

(define (action-table-list->alist)
  ;;The action list for each state  is an alist with a symbol as keyword
  ;;and a  list of integers  as value.  With  the LR driver the  list of
  ;;integers is always of length 1; with the GLR driver it can be of any
  ;;length >= 1.
  ;;
  ;;This function  is called  only for the  LR driver, after  the action
  ;;table  construction is  finished, to  convert the  list  values into
  ;;their single integer element.  This makes it possible to use:
  ;;
  ;;	(cdr (assq KEY ALIST))
  ;;
  ;;to get the value rather than:
  ;;
  ;;	(cadr (assq KEY ALIST))
  ;;
  (do ((len (vector-length action-table))
       (i 0 (+ 1 i)))
      ((= i len))
    (vector-set! action-table i
		 (map (lambda (ell)
			(cons (car ell) (cadr ell)))
		   (vector-ref action-table i)))))


;;; debugging tools: print parser in human-readable format

(define (debug:print-states port)
  (define (%display stuff)
    (display stuff port))
  (define (%newline)
    (newline port))

  (define (print-item item-no)
    (let loop ((i item-no))
      (let ((v (vector-ref ritem i)))
	(if (>= v 0)
	    (loop (+ i 1))
	  (let* ((rlno    (- v))
		 (nt      (vector-ref rlhs rlno)))
	    (%display (vector-ref the-nonterminals nt)) (%display " --> ")
	    (let loop ((i (vector-ref rrhs rlno)))
	      (let ((v (vector-ref ritem i)))
		(if (= i item-no)
		    (%display ". "))
		(if (>= v 0)
		    (begin
		      (%display (get-symbol v))
		      (%display " ")
		      (loop (+ i 1)))
		  (begin
		    (%display "   (rule ")
		    (%display (- v))
		    (%display ")")
		    (%newline))))))))))

  (define (print-action act)
    (cond ((eq? act '*error*)
	   (%display " : Error"))
	  ((eq? act 'accept)
	   (%display " : Accept input"))
	  ((< act 0)
	   (%display " : reduce using rule ")
	   (%display (- act)))
	  (else
	   (%display " : shift and goto state ")
	   (%display act)))
    (%newline))

  (define (print-actions acts)
    (let loop ((l acts))
      (if (null? l)
	  #t
	(let ((sym (caar l))
	      (act (cdar l)))
	  (%display "   ")
	  (cond
	   ((eq? sym 'default)
	    (%display "default action"))
	   (else
	    (if (number? sym)
		(%display (get-symbol (+ sym nvars)))
	      (%display sym))))
	  (print-action act)
	  (loop (cdr l))))))

  (if action-table
      (begin
	(%display "State table") (%newline)
	(%display "-----------") (%newline) (%newline)
	(let loop ((l first-state))
	  (or (null? l)
	      (let* ((core	(car l))
		     (i		(core-number core))
		     (items	(core-items core))
		     (actions	(vector-ref action-table i)))
		(%display "state ")
		(%display i)
		(%newline)
		(%newline)
		(for-each (lambda (x)
			    (%display "   ")
			    (print-item x))
		  items)
		(%newline)
		(print-actions actions)
		(%newline)
		(loop (cdr l))))))
    (begin
      (%display "No generated parser available!")
      (%newline))))


;;;; build goto and reduction table

(define build-goto-table
  (lambda ()
    `(vector
      ,@(map (lambda (shifts)
	       (list 'quote
		     (if shifts
			 (let loop ((l (shift-shifts shifts)))
			   (if (null? l)
			       '()
			     (let* ((state  (car l))
				    (symbol (vector-ref acces-symbol state)))
			       (if (< symbol nvars)
				   (cons (cons symbol state)
					 (loop (cdr l)))
				 (loop (cdr l))))))
		       '())))
	  (vector->list shift-table)))))

(define (build-reduction-table gram/actions)
  ;;Build and  return the  reduction table: A  vector of closures  to be
  ;;invoked to  reduce the current  stack of values.  GRAM/ACTIONS  is a
  ;;list of pairs;  each pair represents the right-hand  side (RHS) of a
  ;;non-terminal category definition.
  ;;
  ;;The CDR of  each pair is the semantic action  associated to the RHS;
  ;;for the RHS  having no semantic action: It sould  be just the simbol
  ;;"sentinel".
  ;;
  ;;The CAR of each pair is a list of exact non-negative integers.
  ;;
  ;;The first, (caar pair), is the keyword for the goto table used after
  ;;the reduction to find the state to move the parser in.
  ;;
  ;;The rest,  (cdar pair), is  a list of numbers  representing terminal
  ;;and non-terminal symbols appearing in the definition of the RHS; the
  ;;length of the rest is the number  of bindings $1, $2, ...  to add to
  ;;the formals of the reduction closure.
  ;;
  ;;FIXME Temporarily  this function only  supports the LR  driver.  The
  ;;GLR  driver is  easy  to  support once  other  stuff is  stabilised.
  ;;(Marco Maggi; Sat Aug 8, 2009)
  ;;
  (let ((l (map (lambda (pair)
		  (let* ((semantic-action (cdr pair))
			 (goto-keyword	(caar pair))
			 (rhs		(cdar pair))
			 (val-num	(length rhs))
			 (bindings	(let loop ((i 1) (v '()))
					  (if (> i val-num)
					      v
					    (loop (+ 1 i)
						  (cons (string->symbol
							 (string-append "$" (number->string i)))
							v)))))
			 (body	(if (zero? goto-keyword)
				    '$1
				  `(yy-reduce-pop-and-push ,val-num ,goto-keyword ,semantic-action
							   yy-stack-states yy-stack-values))))
		    `(lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states
						     ,@bindings . yy-stack-values)
		       ,body)))
	     gram/actions)))
    `(vector '() ,@l)))


;;;; done

(main)))

;;; end of file
