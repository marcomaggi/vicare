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
  (export
    ;; public functions
    read get-datum get-annotated-datum

    ;; internal functions only for Vicare
    read-source-file read-script-source-file read-library-source-file

    annotation? annotation-expression
    annotation-source annotation-stripped)
  (import (except (ikarus)
		  ;; public functions
		  read get-datum get-annotated-datum

		  read-source-file read-script-source-file
		  read-library-source-file

		  read-char

		  annotation? annotation-expression
		  annotation-source annotation-stripped)
    (only (ikarus.string-to-number)
	  define-string->number-parser)
    (ikarus system $chars)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $bytevectors)
    (prefix (rename (ikarus system $fx) #;(ikarus fixnums unsafe)
		    ($fxzero?	fxzero?)
		    ($fxadd1	fxadd1)		 ;increment
		    ($fxsub1	fxsub1)		 ;decrement
		    ($fxsra	fxsra)		 ;shift right
		    ($fxsll	fxsll)		 ;shift left
		    ($fxlogor	fxlogor)	 ;inclusive logic OR
		    ($fxlogand	fxand)		 ;logic AND
		    ($fx+	fx+)
		    ($fx-	fx-)
		    ($fx*	fx*)
		    ($fx<	fx<)
		    ($fx>	fx>)
		    ($fx>=	fx>=)
		    ($fx<=	fx<=)
		    ($fx=	fx=))
	    unsafe.)
    (prefix (rename (ikarus system $chars) #;(ikarus system chars)
		    ($char=		char=)
		    ($char<		char<)
		    ($char<=		char<=)
		    ($char>		char>)
		    ($char>=		char>=)
		    ($char->fixnum	char->fixnum)
		    ($fixnum->char	fixnum->char))
	    unsafe.)
    (prefix (rename (ikarus system $strings) #;(ikarus system strings)
		    ($make-string	make-string)
		    ($string-length	string-length)
		    ($string-ref	string-ref)
		    ($string-set!	string-set!))
	    unsafe.))


;;;; syntax helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (?who ?stx) . ?body)
     (define-syntax ?who (lambda (?stx) . ?body)))))

(define-syntax unwind-protect
  ;;Not a general UNWIND-PROTECT for Scheme, but fine here because we do
  ;;not use continuations to escape from the body.
  ;;
  (syntax-rules ()
    ((_ ?body ?cleanup0 ?cleanup ...)
     (let ((cleanup (lambda () ?cleanup0 ?cleanup ...)))
       (with-exception-handler
	   (lambda (E)
	     (cleanup)
	     (raise E))
	 (lambda ()
	   (call-with-values
	       (lambda () ?body)
	     (lambda return-values
	       (cleanup)
	       (apply values return-values)))))))))


;;;; reading and peeking characters helpers

(define-inline (read-char port)
  ;;Attempt to  read a  character from PORT.   If successful  return the
  ;;character, if  an error occurs raise  an exception, if  EOF is found
  ;;return the EOF object.
  ;;
  (get-char-and-track-textual-position port))

(define-syntax* (read-char-no-eof stx)
  (syntax-case stx ()
    ((read-char-no-eof (?port ?ch-name ?raise-error) . ?cond-clauses)
     (and (identifier? #'?ch-name)
	  (identifier? #'?raise-error))
     #'(let ((?ch-name (read-char ?port)))
	 (cond ((eof-object? ?ch-name)
		(?raise-error))
	       . ?cond-clauses)))))

(define-syntax* (peek-char-no-eof stx)
  (syntax-case stx ()
    ((peek-char-no-eof (?port ?ch-name ?raise-error) . ?cond-clauses)
     (and (identifier? #'?ch-name)
	  (identifier? #'?raise-error))
     #'(let ((?ch-name (peek-char ?port)))
	 (cond ((eof-object? ?ch-name)
		(?raise-error))
	       . ?cond-clauses)))))


;;;; miscellaneous helpers

(define-inline (reverse-list->string ell)
  ;;There are more efficient ways to do this, but ELL is usually short.
  ;;
  (list->string (reverse ell)))

(define-inline (port-in-r6rs-mode? port)
  (eq? (port-mode port) 'r6rs))

(define-inline (port-in-vicare-mode? port)
  (eq? (port-mode port) 'vicare))

(define-inline (source-code-port? port)
  (and (input-port? port) (textual-port? port)))

;;; --------------------------------------------------------------------

(define-inline (%assert-argument-is-source-code-port who port)
  (unless (source-code-port? port)
    (assertion-violation who "expected textual input port as argument" port)))

(define-inline (%assert-argument-is-procedure who x)
  (unless (procedure? x)
    (assertion-violation who "expected procedure as argument" x)))


;;;; interface to low level functions

(define-inline (%seed-strings->gensym id0 id1)
  (foreign-call "ikrt_strings_to_gensym" id0 id1))


;;;; data structures

;;Constructor: make-loc VALUE VALUE/ANN SET?
;;
;;Predicate: loc? OBJ
;;
;;Field name: value
;;Field accessor: loc-value LOC
;;Field mutator: set-loc-value! LOC NEW-VALUE
;;
;;Field name: value/ann
;;Field accessor: loc-value/ann LOC
;;Field mutator: set-loc-value/ann! LOC NEW-VALUE/ANN
;;
;;Field name: set?
;;Field accessor: loc-set? LOC
;;Field mutator: set-loc-set?! LOC NEW-SET?
;;
(define-struct loc
  (value value/ann set?))

;;Constructor: make-annotation EXPR SOURCE STRIPPED
;;
;;Predicate: annotation? OBJ
;;
;;Field name: expression
;;Field accessor: annotation-expression ANN
;;Field mutator: set-annotation-expression! ANN NEW-EXPR
;;  Expression  is  a   list/vector/id/what-have-you  that  may  contain
;;  further annotations.
;;
;;Field name: source
;;Field accessor: annotation-source ANN
;;Field mutator: set-annotation-source! ANN NEW-SOURCE
;;  A pair: (file-name . byte-offset)
;;
;;Field name: stripped
;;Field accessor: annotation-stripped ANN
;;Field mutator: set-annotation-stripped! ANN NEW-STRIP
;;  Stripped is an S-expression with no annotations.
;;
(define-struct annotation
  (expression source stripped))

(define (annotate-simple datum textual-pos port)
  (make-annotation datum
		   (cons (source-position-port-id   textual-pos)
			 (source-position-character textual-pos))
		   #;(cons (port-id port) byte)
		   datum))

(define (annotate stripped expression textual-pos port)
  (make-annotation expression
		   (cons (source-position-port-id   textual-pos)
			 (source-position-character textual-pos))
		   #;(cons (port-id port) byte)
		   stripped))


;;;; source position handling
;;
;;The original Ikarus'  code tracked only byte offset;  the POS argument
;;was  the byte  offset; later  I changed  the POS  argument to  a pair:
;;port-id, byte  offset; now POS is a  &source-position condition object
;;(Marco Maggi; Oct 17, 2011).
;;

(define-inline (make-compound-position port)
  (port-textual-position port))

(define (make-compound-position/with-offset port offset)
  (let ((textual-position (port-textual-position port)))
    ;;FIXME  In rare  cases:  applying  the offset  may  make the  colum
    ;;negative!!!  But notice that, at present, the OFFSET is always -1.
    (make-source-position-condition (port-id port)
				    (+ offset (source-position-byte      textual-position))
				    (+ offset (source-position-character textual-position))
				    (source-position-line textual-position)
				    (+ offset (source-position-column    textual-position)))))

(define-inline (compound-position-char textual-pos)
  (source-position-character textual-pos))

(define-inline (compound-position-line textual-pos)
  (source-position-line textual-pos))

(define-inline (compound-position-column textual-pos)
  (source-position-column textual-pos))


;;;; exception raisers

(define (die/lex textual-pos who msg . irritants)
  (raise
   (condition (make-lexical-violation) ;mandated by R6RS
	      (make-i/o-read-error)    ;mandated by R6RS
	      (make-message-condition msg)
	      (if (null? irritants)
		  (condition)
		(make-irritants-condition irritants))
	      textual-pos)))

(define-inline (die/pos port offset who msg . irritants)
  (die/lex (make-compound-position/with-offset port offset) who msg . irritants))

(define-inline (die/p p who msg . irritants)
  (die/pos p 0 who msg . irritants))

(define-inline (die/p-1 p who msg . irritants)
  (die/pos p -1 who msg . irritants))

(define-inline (die/ann ann who msg . irritants)
  (die/lex (annotation-source ann) who msg . irritants))


;;;; characters classification helpers

(define CHAR-FIXNUM-0		(unsafe.char->fixnum #\0))
(define CHAR-FIXNUM-a		(unsafe.char->fixnum #\a))
;;(define CHAR-FIXNUM-f		(unsafe.char->fixnum #\f))
(define CHAR-FIXNUM-A		(unsafe.char->fixnum #\A))
;;(define CHAR-FIXNUM-F		(unsafe.char->fixnum #\F))
(define CHAR-FIXNUM-a-10	(unsafe.fx- CHAR-FIXNUM-a 10))
(define CHAR-FIXNUM-A-10	(unsafe.fx- CHAR-FIXNUM-A 10))
(define CHAR-FIXNUM-SHARP	(unsafe.char->fixnum #\#))
(define CHAR-FIXNUM-BANG	(unsafe.char->fixnum #\!))
(define CHAR-FIXNUM-GREATEST-ASCII
  #\x7F #;($fixnum->char 127))

(define-inline (char-is-single-char-line-ending? ch)
  (or (unsafe.fx= ch #\x000A)	;; linefeed
      (unsafe.fx= ch #\x0085)	;; next line
      (unsafe.fx= ch #\x2028)))	;; line separator

(define-inline (char-is-carriage-return? ch)
  (unsafe.fx= ch #\xD))

(define-inline (char-is-newline-after-carriage-return? ch)
  ;;This is used to recognise 2-char newline sequences.
  ;;
  (or (unsafe.fx= ch #\x000A)	;; linefeed
      (unsafe.fx= ch #\x0085)))	;; next line

(define (delimiter? ch)
  (or (char-whitespace? ch)
      (unsafe.char= ch #\()
      (unsafe.char= ch #\))
      (unsafe.char= ch #\[)
      (unsafe.char= ch #\])
      (unsafe.char= ch #\")
      (unsafe.char= ch #\#)
      (unsafe.char= ch #\;)
      (unsafe.char= ch #\{)
      (unsafe.char= ch #\})
      (unsafe.char= ch #\|))
  #;(or (char-whitespace? ch)
      (memq ch '(#\( #\) #\[ #\] #\" #\# #\; #\{ #\} #\|))))

(define-inline (dec-digit? ch)
  (and (unsafe.char<= #\0 ch) (unsafe.char<= ch #\9)))

(define (initial? ch)
  (cond ((unsafe.char<= ch CHAR-FIXNUM-GREATEST-ASCII)
	 (or (letter? ch)
	     (special-initial? ch)))
	(else
	 (unicode-printable-char? ch))))

(define (letter? ch)
  (or (and (unsafe.char<= #\a ch) (unsafe.char<= ch #\z))
      (and (unsafe.char<= #\A ch) (unsafe.char<= ch #\Z))))

(define (special-initial? ch)
  (or (unsafe.char= ch #\!)
      (unsafe.char= ch #\$)
      (unsafe.char= ch #\%)
      (unsafe.char= ch #\&)
      (unsafe.char= ch #\*)
      (unsafe.char= ch #\/)
      (unsafe.char= ch #\:)
      (unsafe.char= ch #\<)
      (unsafe.char= ch #\=)
      (unsafe.char= ch #\>)
      (unsafe.char= ch #\?)
      (unsafe.char= ch #\^)
      (unsafe.char= ch #\_)
      (unsafe.char= ch #\~))
  #;(memq c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~)))

(define (special-subsequent? ch)
  (or (unsafe.char= ch #\+)
      (unsafe.char= ch #\-)
      (unsafe.char= ch #\.)
      (unsafe.char= ch #\@)))

(define (subsequent? ch)
  (cond ((unsafe.char<= ch CHAR-FIXNUM-GREATEST-ASCII)
	 (or (letter? ch)
	     (dec-digit?  ch)
	     (special-initial? ch)
	     (special-subsequent? ch)))
	(else
	 (or (unicode-printable-char? ch)
	     (memq (char-general-category ch) '(Nd Mc Me))))))


;;;; conversion between characters and integers helpers

(define (fixnum->char/checked N accumulated-chars port)
  ;;Validate the  fixnum N  as valid Unicode  code point and  return the
  ;;corresponding character.
  ;;
  ;;If  N is  invalid: raise  an exception  using  ACCUMULATED-CHARS and
  ;;PORT.  ACCUMULATED-CHARS must be a reversed list of chars from which
  ;;N was  parsed.  PORT  must be  the port from  which the  chars where
  ;;drawn.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define (valid-integer-char? N)
    (cond ((<= N #xD7FF)   #t)
	  ((<  N #xE000)   #f)
	  ((<= N #x10FFFF) #t)
	  (else            #f)))
  (if (valid-integer-char? N)
      (unsafe.fixnum->char N)
    (%error "invalid numeric value for character" (reverse-list->string accumulated-chars))))

(define-inline (char->dec-digit ch)
  (unsafe.fx- (unsafe.char->fixnum ch) CHAR-FIXNUM-0))

(define (char->hex-digit/or-false x)
  ;;If X is a character in the range of hex digits [0-9a-fA-F]: return a
  ;;fixnum representing such digit, else return #f.
  ;;
  (define-inline (y)
    (unsafe.char->fixnum x))
  (cond ((and (unsafe.char<= #\0 x) (unsafe.char<= x #\9))
	 (unsafe.fx- (y) CHAR-FIXNUM-0))
	((and (unsafe.char<= #\a x) (unsafe.char<= x #\f))
	 (unsafe.fx- (y) CHAR-FIXNUM-a-10))
	((and (unsafe.char<= #\A x) (unsafe.char<= x #\F))
	 (unsafe.fx- (y) CHAR-FIXNUM-A-10))
	(else #f)))


;;;; public functions

(define read
  ;;Defined by  R6RS.  Read an external representation  from the textual
  ;;input PORT and return the datum it represents.
  ;;
  ;;The READ procedure operates in the same way as GET-DATUM.
  ;;
  ;;If  PORT  is   omitted,  it  defaults  to  the   value  returned  by
  ;;CURRENT-INPUT-PORT.
  ;;
  (case-lambda
   (()
    (get-datum (current-input-port)))
   ((port)
    (%assert-argument-is-source-code-port 'read port)
    (get-datum port))))

(define (get-datum port)
  ;;Defined by  R6RS.  Read an external representation  from the textual
  ;;input  PORT  and return  the  datum  it  represents.  The  GET-DATUM
  ;;procedure returns the  next datum that can be  parsed from the given
  ;;PORT, updating  PORT to point exactly  past the end  of the external
  ;;representation of the object.
  ;;
  ;;Any <interlexeme-space> in the input is first skipped.  If an end of
  ;;file  occurs  after  the  <interlexeme-space>,  the  EOF  object  is
  ;;returned.
  ;;
  ;;If  a  character inconsistent  with  an  external representation  is
  ;;encountered  in  the  input,   an  exception  with  condition  types
  ;;"&lexical" and "&i/o-read" is raised.
  ;;
  ;;Also, if  the end of file  is encountered after the  beginning of an
  ;;external   representation,  but   the  external   representation  is
  ;;incomplete  and  therefore  cannot  be  parsed,  an  exception  with
  ;;condition types "&lexical" and "&i/o-read" is raised.
  ;;
  (define who 'get-datum)
  (%assert-argument-is-source-code-port who port)
  (let-values (((expr expr/ann locs-alist kont)
		(read-expr port '() void)))
    (if (null? locs-alist)
	expr
      (begin
	(for-each (reduce-loc! port)
	  locs-alist)
	(kont)
	(if (loc? expr)
	    (loc-value expr)
	  expr)))))

(define (get-annotated-datum port)
  ;;Defined  by Ikarus.   Like GET-DATUM,  but rather  than  returning a
  ;;datum  return a  hierarchy of  ANNOTATION structures  with  the same
  ;;hierarchy of the datum and embedding the datum itself.
  ;;
  (define who 'get-annotated-datum)
  (define (%return-annotated x)
    (if (and (annotation? x)
	     (eof-object? (annotation-expression x)))
	(eof-object)
      x))
  (%assert-argument-is-source-code-port who port)
  (let-values (((expr expr/ann locs-alist kont)
		(read-expr port '() void)))
    (if (null? locs-alist)
	(%return-annotated expr/ann)
      (begin
	(for-each (reduce-loc! port)
	  locs-alist)
	(kont)
	(if (loc? expr)
	    (loc-value/ann expr)
	  (%return-annotated expr/ann))))))


;;;; Public functions used by Vicare itself
;;
;;These  functions  are exported  by  this  library  but not  listed  in
;;"makefile.sps", so they are not visible to client code.
;;

(define (read-library-source-file filename)
  ;;Open FILENAME for input only  using the native transcoder, then read
  ;;and return the first datum; close the port.
  ;;
  (let ((port (open-input-file filename)))
    (unwind-protect
	(get-annotated-datum port)
      (close-input-port port))))

(define (read-source-file filename)
  ;;Open FILENAME for input only  using the native transcoder, then read
  ;;and return all the datums in a list; close the port.
  ;;
  (let ((port (open-input-file filename)))
    (define-inline (%next-datum)
      (get-annotated-datum port))
    (unwind-protect
	(let read-next-datum ((obj (%next-datum)))
	  (if (eof-object? obj)
	      '()
	    (cons obj (read-next-datum (%next-datum)))))
      (close-input-port))))

(define (read-script-source-file filename)
  ;;Open FILENAME for input only  using the native transcoder, then read
  ;;and return all the datums in a list.
  ;;
  ;;Discard  the  first  line from  the  file  if  the first  two  bytes
  ;;represent  the sharp-bang  sequence "#!";  this is  useful  to allow
  ;;scripts on Unix systems to start with the command line needed to use
  ;;them.
  ;;
  ;;Notice that this  will discard valid sharp-bang comments  if the are
  ;;at the very beginning of a file.
  ;;
  (let* ((port		(open-file-input-port filename))
	 (sharp-bang?	(let-values (((octet1 octet2)
				      ;;If  an error  happens  here PORT
				      ;;will  be   closed  by  the  port
				      ;;guardian.
				      (lookahead-two-u8 port)))
			  (and (= octet1 CHAR-FIXNUM-SHARP)
			       (= octet2 CHAR-FIXNUM-BANG))))
	 (port		(transcoded-port port (native-transcoder))))
    (define-inline (%next-datum)
      (get-annotated-datum port))
    (unwind-protect
	(begin
	  (when sharp-bang?
	    (read-and-discard-up-to-and-including-line-ending port))
	  (let read-next-datum ((obj (%next-datum)))
	    (if (eof-object? obj)
		'()
	      (cons obj (read-next-datum (%next-datum))))))
      (close-input-port port))))


;;;; helpers for public functions

(define (read-expr port locs-alist kont)
  (let-values (((token pos) (start-tokenising/pos port)))
    (finalise-tokenisation port locs-alist kont token pos)))

(define (reduce-loc! port)
  ;;Subroutine of GET-DATUM and GET-ANNOTATED-DATUM.  Finalise the graph
  ;;notation locations.
  ;;
  ;;This computation  needs two  arguments: PORT and  an entry  from the
  ;;LOCS-ALIST which is the result  of reading an S-expression.  PORT is
  ;;fixed, so it  may be a little faster to make  this function return a
  ;;closure on PORT rather than to evaluate:
  ;;
  ;;   (for-each (lambda (entry)
  ;;               (reduce-loc! port entry))
  ;;     locs-alist)
  ;;
  (lambda (entry)
    (define-inline (%error msg . irritants)
      (die/p port 'read msg . irritants))
    (let ((loc (cdr entry)))
      (unless (loc-set? loc)
	;;FIXME It  would be beautiful  to include the position  in this
	;;error report.
	(%error "referenced location mark is not set" (car entry)))
      (when (loc? (loc-value loc))
	(let f ((h loc) (t loc))
	  (if (loc? h)
	      (let ((h1 (loc-value h)))
		(if (loc? h1)
		    (begin
		      (when (eq? h1 t)
			(%error "circular marks"))
		      (let ((v (f (loc-value h1) (loc-value t))))
			(set-loc-value! h1 v)
			(set-loc-value! h v)
			v))
		  (begin
		    (set-loc-value! h h1)
		    h1)))
	    h))))))

(define (read-and-discard-sexp port)
  ;;Read a  full expression and discard  it.  This is used  to consume a
  ;;sexp commented out with "#;".
  ;;
  (let ((locs-alist	'())
	(kont		void))
    (read-expr port locs-alist kont))
  (void))


(define (start-tokenising/pos port)
  ;;Recursive  function.  Start  tokenizing  the next  datum from  PORT,
  ;;discarding  comments  and  whitespaces; after  discarding  something
  ;;recurse calling  itself; if  the first character  is a  #\# delegate
  ;;actual   parsing   to   ADVANCE-TOKENISATION-OF-HASH-DATUM/C;   else
  ;;delegate actual parsing to ADVANCE-TOKENISATION-OF-NON-HASH-DATUM/C.
  ;;
  ;;Return two values:  a datum representing the next  token, a compound
  ;;position value.
  ;;
  (define-inline (recurse)
    (start-tokenising/pos port))
  (define-inline (%error msg . irritants)
    (die/p port 'tokenize msg . irritants))
  (let* ((pos (make-compound-position port))
	 (ch  (read-char port)))
    (cond ((eof-object? ch)
	   (values ch pos))

	  ;;discard line comments
	  ((unsafe.char= ch #\;)
	   (read-and-discard-up-to-and-including-line-ending port)
	   (recurse))

	  ;;tokenise everything starting with a #
	  ((unsafe.char= ch #\#)
	   ;;FIXME Why are we taking the position again here?
	   (let* ((pos1 (make-compound-position port))
		  (ch1  (read-char port)))
	     (cond ((eof-object? ch1)
		    (%error "invalid eof after #"))

		   ;;discard sexp comments
		   ((unsafe.char= ch1 #\;)
		    (read-and-discard-sexp port)
		    (recurse))

		   ;;discard multiline comments
		   ((unsafe.char= ch1 #\|)
		    (finish-tokenisation-of-multiline-comment port)
		    (recurse))

		   ;;tokenize datums whose syntax starts with #
		   (else
		    (values (advance-tokenisation-of-hash-datum/c ch1 port) pos1)))))

	  ;;discard whitespaces
	  ((char-whitespace? ch)
	   (recurse))

	  ;;tokenise every datum whose syntax does not start with a #
	  (else
	   (values (advance-tokenisation-of-non-hash-datum/c ch port) pos)))))


(define (start-tokenising port)
  ;;Recursive  function.  Start  tokenizing  the next  datum from  PORT,
  ;;discarding  comments  and  whitespaces; after  discarding  something
  ;;recurse  calling itself;  if the  first  character is  a #  delegate
  ;;actual   parsing   to   ADVANCE-TOKENISATION-OF-HASH-DATUM/C;   else
  ;;delegate actual parsing to ADVANCE-TOKENISATION-OF-NON-HASH-DATUM/C.
  ;;
  ;;Return a datum representing the next token.
  ;;
  ;;This function does the  same thing of START-TOKENISING/POS, but does
  ;;not track the position, which is sometimes a bit faster.
  ;;
  (define-inline (recurse)
    (start-tokenising port))
  (define-inline (%error msg . irritants)
    (die/p port 'tokenize msg . irritants))
  (let ((ch (read-char port)))
    (cond ((eof-object? ch)
	   ch)

	  ;;discard line comments
	  ((unsafe.char= ch #\;)
	   (read-and-discard-up-to-and-including-line-ending port)
	   (recurse))

	  ;;tokenise everything starting with a #
	  ((unsafe.char= ch #\#)
	   (let ((ch1 (read-char port)))
	     (cond ((eof-object? ch1)
		    (%error "invalid EOF after #"))

		   ;;discard sexp comments
		   ((unsafe.char= ch1 #\;)
		    (read-and-discard-sexp port)
		    (recurse))

		   ;;discard multiline comments
		   ((unsafe.char= ch1 #\|)
		    (finish-tokenisation-of-multiline-comment port)
		    (recurse))

		   ;;tokenize datums whose syntax starts with #
		   (else
		    (advance-tokenisation-of-hash-datum/c ch1 port)))))

	  ;;discard whitespaces
	  ((char-whitespace? ch)
	   (recurse))

	  ;;tokenise every datum whose syntax does not start with a #
	  (else
	   (advance-tokenisation-of-non-hash-datum/c ch port)))))


(define (advance-tokenisation-of-non-hash-datum/c ch port)
  ;;Parse standalone  datums and compound  datums whose syntax  does not
  ;;start with a # character.   Read characters from PORT.  Handle CH as
  ;;the first character of the datum, already consumed from PORT.
  ;;
  ;;Return a datum representing a  full token already read or describing
  ;;a token that must still be read:
  ;;
  ;;lparen			The token is a left paranthesis.
  ;;rparen			The token is a right paranthesis.
  ;;lbrack			The token is a left bracket.
  ;;rbrack			The token is a right bracket.
  ;;(datum . <num>)		The token is the number <NUM>.
  ;;(datum . <sym>)		The token is the symbol <SYM>.
  ;;(datum . <str>)		The token is the string <STR>.
  ;;(datum . <ch>)		The token is the character <CH>.
  ;;(macro . quote)		The token is a quoted form.
  ;;(macro . quasiquote)	The token is a quasiquoted form.
  ;;(macro . unquote)		The token is an unquoted form.
  ;;(macro . unquote-splicing)	The token is an unquoted splicing form.
  ;;
  ;;If CH is the dot character:  the return value is the return value of
  ;;FINISH-TOKENISATION-OF-DOT-DATUM.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (cond ((eof-object? ch)
	 (error 'advance-tokenisation-of-non-hash-datum/c "hmmmm eof")
	 (eof-object))

	((unsafe.char= #\( ch)   'lparen)
	((unsafe.char= #\) ch)   'rparen)
	((unsafe.char= #\[ ch)   'lbrack)
	((unsafe.char= #\] ch)   'rbrack)
	((unsafe.char= #\' ch)   '(macro . quote))
	((unsafe.char= #\` ch)   '(macro . quasiquote))

	((unsafe.char= #\, ch)
	 (let ((ch1 (peek-char port)))
	   (cond ((eof-object? ch1)
		  '(macro . unquote))
		 ((unsafe.char= ch1 #\@)
		  (read-char port)
		  '(macro . unquote-splicing))
		 (else
		  '(macro . unquote)))))

	;;number
	((dec-digit? ch)
	 (let ((d ($fx- (unsafe.char->fixnum ch) (unsafe.char->fixnum #\0))))
	   (cons 'datum (u:digit+ port (list ch) 10 #f #f +1 d))))

	;;symbol
	((initial? ch)
	 (finish-tokenisation-of-identifier (cons ch '()) port))

	;;string
	((unsafe.char= #\" ch)
	 (let ((ls (%accumulate-string-chars '() port)))
	   (cons 'datum (reverse-list->string ls))))

	;;symbol "+" or number
	((unsafe.char= #\+ ch)
	 (let ((ch1 (peek-char port)))
	   (cond ((eof-object? ch1)	'(datum . +))
		 ((delimiter?  ch1)	'(datum . +))
		 (else
		  (cons 'datum (u:sign port '(#\+) 10 #f #f +1))))))

	;;Identfier "-",  peculiar identifier "->",  peculiar identifier
	;;"->abc" or number.
	;;
	;;Notice that  "-ciao" is not  an identifier according  to R6RS;
	;;this  is  to  speed  up  reading numbers  "-i",  "-inf.0"  and
	;;"-nan.0"  without confusing them  with identifiers  by looking
	;;only at the first char right after the first "-".
	;;
	((unsafe.char= #\- ch)
	 (let ((ch1 (peek-char port)))
	   (cond ((eof-object? ch1)	'(datum . -))
		 ((delimiter?  ch1)	'(datum . -))

		 ;;peculiar identifier: -> <subsequent>*
		 ((unsafe.char= ch1 #\>)
		  (read-char port)
		  (finish-tokenisation-of-identifier '(#\> #\-) port))

		 ;;number
		 (else
		  (cons 'datum (u:sign port '(#\-) 10 #f #f -1))))))

	;;everything  starting  with  a  dot (standalone  dot,  ellipsis
	;;symbol, inexact number, other symbols)
	((unsafe.char= #\. ch)
	 (finish-tokenisation-of-dot-datum port))

	;;symbol with syntax "|<sym>|"
	((unsafe.char= #\| ch)
	 (when (port-in-r6rs-mode? port)
	   (%error "|symbol| syntax is invalid in #!r6rs mode"))
	 (finish-tokenisation-of-identifier/bar '() port))

	;;symbol whose first char is a backslash sequence, "\x41;-ciao"
	((unsafe.char= #\\ ch)
	 (finish-tokenisation-of-identifier/backslash '() port))

;;;Unused for now.
;;;
;;;     ((unsafe.char= #\{ ch) 'lbrace)

	(else
	 (%error-1 "invalid syntax" ch))))


(define (advance-tokenisation-of-hash-datum/c ch port)
  ;;Parse standalone datums and compound datums whose syntax starts with
  ;;a # character.   Read characters from PORT.  Handle  CH as the first
  ;;character of the datum after #, already consumed from PORT.
  ;;
  ;;Return a datum representing the token that must be read:
  ;;
  ;;(datum . #t)		The token is the value #t.
  ;;(datum . #f)		The token is the value #f.
  ;;(datum . <char>)		The token is the character <char>.
  ;;(datum . <sym>)		The token is the symbol <sym>.
  ;;(datum . <num>)		The token is the number <num>.
  ;;(datum . #!eof)		The token is the "#!eof" comment.
  ;;(macro . syntax)		The token is a syntax form: #'---.
  ;;(macro . quasisyntax)	The token is a quasisyntax form: #`---.
  ;;(macro . unsyntax-splicing)	The token is an unsyntax-splicing form: #,@---.
  ;;(macro . unsyntax)		The token is an unsyntax form: #,---.
  ;;(mark . <n>)		The token is a graph syntax mark: #<N>=---
  ;;(ref . <n>)			The token is a graph syntax reference: #<N>#
  ;;vparen			The token is a vector.
  ;;vu8				The token is a u8 bytevector.
  ;;vs8				The token is a s8 bytevector.
  ;;
  ;;When the token is the  "#!r6rs" or "#!vicare" comment: the port mode
  ;;is changed accordingly and  START-TOKENISING is applied to the port;
  ;;the return value is the return value of START-TOKENISING.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading non-hash datum"))
  (define-inline (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (cond
   ((eof-object? ch)
    (%error "invalid # near end of file"))

   ((or (unsafe.char= #\t ch) (unsafe.char= #\T ch)) #;(memq ch '(#\t #\T))
    (let ((c1 (peek-char port)))
      (cond ((eof-object? c1) '(datum . #t))
	    ((delimiter?  c1) '(datum . #t))
	    (else
	     (%error (format "invalid syntax near #~a~a" ch c1))))))

   ((or (unsafe.char= #\f ch) (unsafe.char= #\F ch)) #;(memq ch '(#\f #\F))
    (let ((ch1 (peek-char port)))
      (cond ((eof-object? ch1) '(datum . #f))
	    ((delimiter?  ch1) '(datum . #f))
	    (else
	     (%error (format "invalid syntax near #~a~a" ch ch1))))))

   ((unsafe.char= #\\ ch)
    (finish-tokenisation-of-char port))
   ((unsafe.char= #\( ch)
    'vparen)
   ((unsafe.char= #\' ch)
    '(macro . syntax))
   ((unsafe.char= #\` ch)
    '(macro . quasisyntax))

   ((unsafe.char= #\, ch)
    (let ((ch1 (peek-char port)))
      (cond ((unsafe.char= ch1 #\@)
	     (read-char port)
	     '(macro . unsyntax-splicing))
	    (else
	     '(macro . unsyntax)))))

   ;; #! comments and such
   ((unsafe.char= #\! ch)
    (let* ((token (finish-tokenisation-of-identifier '() port))
	   (sym   (cdr token)))
      (case sym
	((vicare ikarus)
	 (set-port-mode! port 'vicare)
	 (start-tokenising port))
	((r6rs)
	 (set-port-mode! port 'r6rs)
	 (start-tokenising port))
	((eof)
	 (if (port-in-r6rs-mode? port)
	     (%error-1 "invalid syntax" "#!eof")
	   `(datum . ,(eof-object))))
	(else
	 ;;If not recognised,  just handle it as a  comment and read the
	 ;;next datum.
	 (start-tokenising port)))))

   ((dec-digit? ch)
    (when (port-in-r6rs-mode? port)
      (%error-1 "graph notation marks syntax is invalid in #!r6rs mode" (string #\# ch)))
    (finish-tokenisation-of-graph-location port (char->dec-digit ch)))

   ((unsafe.char= #\: ch)
    (when (port-in-r6rs-mode? port)
      (%error-1 "gensym syntax is invalid in #!r6rs mode" (format "#~a" ch)))
    (let* ((ch1 (read-char-skip-whitespace port "gensym"))
	   (id0 (cond ((initial? ch1)
		       (reverse-list->string (%accumulate-identifier-chars (cons ch1 '()) port)))
		      ((unsafe.char= #\| ch1)
		       (reverse-list->string (%accumulate-identifier-chars/bar '() port)))
		      (else
		       (%error-1 "invalid char inside gensym" ch1)))))
      (cons 'datum (gensym id0))))

   ;;Gensym with one of the following syntaxes:
   ;;
   ;;#{ciao}
   ;;   In which "ciao" is ID0, and will become the unique string.
   ;;
   ;;#{|ciao|}
   ;;   In which "ciao" is ID0, and will become the unique string.
   ;;
   ;;#{d |95BEx%X86N?8X&yC|}
   ;;   In which "d" is ID0 and "95BEx%X86N?8X&yC" is ID1.
   ;;
   ;;#{|d| |95BEx%X86N?8X&yC|}
   ;;   In which "d" is ID0 and "95BEx%X86N?8X&yC" is ID1.
   ;;
   ((unsafe.char= #\{ ch)
    (when (port-in-r6rs-mode? port)
      (%error-1 "gensym syntax is invalid in #!r6rs mode" "#{"))
    (let ((ch1 (read-char-skip-whitespace port "gensym")))
      (define-inline (%end-syntax? chX)
	(unsafe.char= #\} chX))
      (define-inline (%read-identifier chX)
	(cond ((initial? chX)
	       (reverse-list->string (%accumulate-identifier-chars (cons chX '()) port)))
	      ((unsafe.char= #\| chX)
	       (reverse-list->string (%accumulate-identifier-chars/bar '() port)))
	      (else
	       (%error-1 "invalid char inside gensym syntax" chX))))
      (let ((id0 (%read-identifier ch1))
	    (ch2 (read-char-skip-whitespace port "gensym")))
	(if (%end-syntax? ch2)
	    `(datum . ,(%seed-strings->gensym #f id0))
	  (let* ((id1 (%read-identifier ch2))
		 (ch3 (read-char-skip-whitespace port "gensym")))
	    (if (%end-syntax? ch3)
		`(datum . ,(%seed-strings->gensym id0 id1))
	      (%error-1 "invalid char while looking for end of gensym syntax" ch3)))))))

   ;;bytevectors
   ((unsafe.char= #\v ch)
    ;;Correct sequences of chars:
    ;;
    ;; ch  ch1  ch2  ch3  ch4  ch5  datum
    ;; ----------------------------------
    ;; v   u    8    (              #vu8
    ;; v   s    8    (              #vs8
    ;; v   u    1    6    l    (    #vu16l
    ;; v   u    1    6    b    (    #vu16b
    ;; v   s    1    6    l    (    #vs16l
    ;; v   s    1    6    b    (    #vs16b
    ;; v   u    3    2    l    (    #vu32l
    ;; v   u    3    2    b    (    #vu32b
    ;; v   s    3    2    l    (    #vs32l
    ;; v   s    3    2    b    (    #vs32b
    ;; v   u    6    4    l    (    #vu64l
    ;; v   u    6    4    b    (    #vu64b
    ;; v   s    6    4    l    (    #vs64l
    ;; v   s    6    4    b    (    #vs64b
    ;;
    (let ((ch1/eof (read-char port)))
      (define-inline (%read-bytevector)
	(cond ((char=? #\u ch1/eof)
	       (%read-unsigned))
	      ((char=? #\s ch1/eof)
	       (when (port-in-r6rs-mode? port)
		 (%error "invalid #vs8 syntax in #!r6rs mode" "#vs8"))
	       (%read-signed))
	      ((eof-object? ch1/eof)
	       (%error "invalid eof object after #v"))
	      (else
	       (%error (format "invalid sequence #v~a" ch1/eof)))))

      (define-inline (%read-unsigned)
	(let ((ch2/eof (read-char port)))
	  (cond ((char=? ch2/eof #\8) ;unsigned bytes bytevector
		 (%read-unsigned-8))
		((eof-object? ch2/eof)
		 (%error "invalid eof object after #vu"))
		(else
		 (%error-1 (format "invalid sequence #vu~a" ch2/eof))))))

      (define-inline (%read-signed)
	(let ((ch2/eof (read-char port)))
	  (cond ((char=? ch2/eof #\8) ;signed bytes bytevector
		 (%read-signed-8))
		((eof-object? ch2/eof)
		 (%error "invalid eof object after #vs"))
		(else
		 (%error-1 (format "invalid sequence #vs~a" ch2/eof))))))

      (define-inline (%read-unsigned-8)
	(let ((ch3/eof (read-char port)))
	  (cond ((char=? ch3/eof #\()
		 'vu8)
		((eof-object? ch3/eof)
		 (%error "invalid eof object after #vu8"))
		(else
		 (%error-1 (format "invalid sequence #vu8~a" ch3/eof))))))

      (define-inline (%read-signed-8)
	(let ((ch3/eof (read-char port)))
	  (cond ((char=? ch3/eof #\()
		 'vs8)
		((eof-object? ch3/eof)
		 (%error "invalid eof object after #vs8"))
		(else
		 (%error-1 (format "invalid sequence #vs8~a" ch3/eof))))))

      (%read-bytevector)))

   ((or (unsafe.char= ch #\e) (unsafe.char= ch #\E)) #;(memq ch '(#\e #\E))
    (cons 'datum (parse-string port (list ch #\#) 10 #f 'e)))

   ((or (unsafe.char= ch #\i) (unsafe.char= ch #\I)) #;(memq ch '(#\i #\I))
    (cons 'datum (parse-string port (list ch #\#) 10 #f 'i)))

   ((or (unsafe.char= ch #\b) (unsafe.char= ch #\B)) #;(memq ch '(#\b #\B))
    (cons 'datum (parse-string port (list ch #\#) 2 2 #f)))

   ((or (unsafe.char= ch #\x) (unsafe.char= ch #\X)) #;(memq ch '(#\x #\X))
    (cons 'datum (parse-string port (list ch #\#) 16 16 #f)))

   ((or (unsafe.char= ch #\o) (unsafe.char= ch #\O)) #;(memq ch '(#\o #\O))
    (cons 'datum (parse-string port (list ch #\#) 8 8 #f)))

   ((or (unsafe.char= ch #\d) (unsafe.char= ch #\D)) #;(memq ch '(#\d #\D))
    (cons 'datum (parse-string port (list ch #\#) 10 10 #f)))

;;;((unsafe.char= #\@ ch) DEAD: Unfixable due to port encoding
;;;                 that does not allow mixing binary and
;;;                 textual data in the same port.
;;;                Left here for historical value
;;; (when (port-in-r6rs-mode? port)
;;;   (%error-1 "fasl syntax is invalid in #!r6rs mode"
;;;      (format "#~a" ch)))
;;; (die/p-1 port 'read "FIXME: fasl read disabled")
;;; '(cons 'datum ($fasl-read port)))

   (else
    (%error-1 (format "invalid syntax #~a" ch)))))


(define (finish-tokenisation-of-dot-datum port)
  ;;Read from  PORT a token starting  with a dot, the  dot being already
  ;;read.  There return value is a datum describing the token:
  ;;
  ;;dot			The token is a standalone dot.
  ;;(datum . ...)	The token is the ellipsis symbol.
  ;;(datum . <num>)	The token is the inexact number <NUM>.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (let ((ch (peek-char port)))
    (cond ((or (eof-object? ch)
	       (delimiter?  ch))
	   'dot)

	  ;;A second dot: an ellipsis opening or an error.
	  ;;
	  ;;Notice that ".ciao", "..ciao", "...ciao" and ".....ciao" are
	  ;;lexical violations; according  to R6RS, an identifier cannot
	  ;;start with a dot, with  the single exception of the ellipsis
	  ;;"...".  This  is to speed up  distinguishing between numbers
	  ;;and  symbols by  looking only  at the  char right  after the
	  ;;first dot.
	  ;;
	  ((unsafe.char= ch #\.)
	   (read-char port)
	   (let ((ch1 (read-char port)))
	     (cond ((eof-object? ch1)
		    (%error "invalid syntax near end of file" ".."))
		   ((unsafe.char= ch1 #\.) ;this is the third
		    (let ((ch2 (peek-char port)))
		      (cond ((eof-object? ch2)	'(datum . ...))
			    ((delimiter?  ch2)	'(datum . ...))
			    (else
			     (%error "invalid syntax" (string #\. #\. #\. ch2))))))
		   (else
		    (%error "invalid syntax" (string #\. #\. ch1))))))

	  ;;then it must be a number
	  (else
	   (cons 'datum (u:dot port '(#\.) 10 #f #f +1))))))


(define (finish-tokenisation-of-graph-location port N)
  ;;Read characters from PORT parsing  a graph notation hash num mark or
  ;;reference.  Return a datum describing the token:
  ;;
  ;;(mark . <num>)	The token is a new hashnum mark.
  ;;(ref . <num>)	The token is reference to an existing hashnum.
  ;;
  (define-inline (recurse N1)
    (finish-tokenisation-of-graph-location port N1))
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading character"))
  (define-inline (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (%read-char-no-eof (port ch)
    ((unsafe.char= #\= ch) (cons 'mark N))
    ((unsafe.char= #\# ch) (cons 'ref  N))
    ((dec-digit? ch)
     (recurse (unsafe.fx+ (unsafe.fx* N 10) (char->dec-digit ch))))
    (else
     (%error "invalid char while inside a #n mark/ref" ch))))


;;;; tokenising identifiers
;;
;;From the R6RS document, the identifier syntax is:
;;
;;  <identifier>    -> <initial> <subsequent>*
;;                   | <peculiar identifier>
;;  <peculiar identifier>
;;                  -> + | - | ... | -> <subsequent>*
;;  <initial>       -> <constituent>
;;                   | <special initial>
;;                   | <inline hex escape>
;;  <subsequent>    -> <initial>
;;                   | <digit>
;;                   | <any character whose category is Nd, Mc, or Me>
;;                   | <special subsequent>
;;  <constituent>   -> <letter>
;;                   | <any character whose Unicode scalar value is
;;                      greater than 127, and whose category is Lu,
;;                      Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po, Sc,
;;                      Sm, Sk, So, or Co>
;;  <special initial>
;;                  -> ! | $ | % | & | * | / | : | < | =
;;                   | > | ? | ^ | _ | ~
;;  <special subsequent>
;;                  -> + | - | . | @
;;  <letter>        -> a | b | c | ... | z
;;                   | A | B | C | ... | Z
;;  <inline hex escape>
;;                  -> \x<hex scalar value>;
;;  <hex scalar value>
;;                  -> <hex digit>+
;;  <hex digit>     -> <digit>
;;                   | a | b | c | d | e | f
;;                   | A | B | C | D | E | F
;;  <digit>         -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;;

(define (finish-tokenisation-of-identifier accumulated-chars port)
  ;;To be called when one or more characters starting an identifier have
  ;;been read from PORT and  we must finish the identifier tokenisation.
  ;;Read the remaining characters and return:
  ;;
  ;;  (datum . <sym>)
  ;;
  ;;where <SYM> is the tokenised symbol.
  ;;
  `(datum . ,(string->symbol
	      (reverse-list->string
	       (%accumulate-identifier-chars accumulated-chars port)))))

(define (finish-tokenisation-of-identifier/bar accumulated-chars port)
  ;;To be called  when one or more characters  starting an identifier in
  ;;bar  syntax  have  been  read  from  PORT and  we  must  finish  the
  ;;identifier tokenisation.  Read the remaining characters and return:
  ;;
  ;;  (datum . <sym>)
  ;;
  ;;where <SYM> is the tokenised symbol.
  ;;
  `(datum . ,(string->symbol
	      (reverse-list->string
	       (%accumulate-identifier-chars/bar accumulated-chars port)))))

(define (finish-tokenisation-of-identifier/backslash accumulated-chars port)
  ;;To be called  when a backslash character starting  an identifier has
  ;;been read from PORT and  we must finish the identifier tokenisation.
  ;;Read the remaining characters and return:
  ;;
  ;;  (datum . <sym>)
  ;;
  ;;where <SYM> is the tokenised symbol.
  ;;
  (cons 'datum (string->symbol
		(reverse-list->string
		 (%accumulate-identifier-chars/backslash '() port #f)))))

;;Three functions are involved in accumulating identifier's chars:
;;
;;  %ACCUMULATE-IDENTIFIER-CHARS
;;  %ACCUMULATE-IDENTIFIER-CHARS/BAR
;;  %ACCUMULATE-IDENTIFIER-CHARS/BACKSLASH
;;
;;they call each other accumulating characters in a reversed list.  When
;;all of  an identifier has  been read: the  return value is  always the
;;reversed list of characters.
;;

(define (%accumulate-identifier-chars accumulated-chars port)
  ;;Read from PORT characters from an identifier token, accumulate them,
  ;;in reverse order and return the resulting list.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (recurse accum)
    (%accumulate-identifier-chars accum port))
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch)
	   accumulated-chars)
	  ((subsequent? ch)
	   (read-char port)
	   (recurse (cons ch accumulated-chars)))
	  ((delimiter? ch)
	   accumulated-chars)
	  ((unsafe.char= ch #\\)
	   (read-char port)
	   (%accumulate-identifier-chars/backslash accumulated-chars port #f))
	  ((port-in-r6rs-mode? port)
	   (%error "invalid identifier syntax" (reverse-list->string (cons ch accumulated-chars))))
	  ;;FIXME Is this  correct?  To return the list  if peeked CH is
	  ;;not recognised?
	  (else accumulated-chars))))

(define (%accumulate-identifier-chars/bar accumulated-chars port)
  ;;Read from PORT characters  from an identifier token between vertical
  ;;bars  "|abcd|" after  the  opening bar  has  been already  consumed;
  ;;accumulate the characters in  reverse order and return the resulting
  ;;list.
  ;;
  ;;This is a syntax outside  of R6RS: identifiers between bars can hold
  ;;any character.
  ;;
  (define-inline (%unexpected-eof-error . args)
    (die/p port 'tokenize "unexpected EOF while reading symbol" . args))
  (define-inline (recurse accum)
    (%accumulate-identifier-chars/bar accum port))
  (define-inline (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (%read-char-no-eof (port ch)
    ((unsafe.char= #\\ ch)
     (%accumulate-identifier-chars/backslash accumulated-chars port #t))
    ((unsafe.char= #\| ch) ;end of symbol, whatever comes after
     accumulated-chars)
    (else
     (recurse (cons ch accumulated-chars)))))

(define (%accumulate-identifier-chars/backslash accumulated-chars port inside-bar?)
  ;;Read from PORT characters from  an identifier datum whose first char
  ;;is  a backslash sequence  "\x41;", after  the opening  backslash has
  ;;been already  consumed; accumulate  the characters in  reverse order
  ;;and return the resulting list.
  ;;
  ;;When reading the baskslash sequence is terminated: if INSIDE-BAR? is
  ;;true   %ACCUMULATE-IDENTIFIER-CHARS/BAR  is   invoked   to  continue
  ;;reading,  else %ACCUMULATE-IDENTIFIER-CHARS  is invoked  to continue
  ;;reading.
  ;;
  (define-inline (%error msg . args)
    (die/p port   'tokenize msg . args))
  (define-inline (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error . args)
    (%error "unexpected EOF while reading symbol" . args))
  (define-inline (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (define-inline (main)
    (%read-char-no-eof (port ch)
      ((unsafe.char= #\x ch)
       (%tokenize-hex-digits))
      (else
       (%error "expected character \"x\" after backslash while reading symbol"
	       (string #\\ ch) (reverse-list->string accumulated-chars)))))

  (define-inline (%tokenize-hex-digits)
    (let next-digit ((code-point 0)
		     (accumul    (list #\x #\\)))
      (%read-char-no-eof (port ch)
	((unsafe.char= #\; ch)
	 (let ((accum (cons (fixnum->char/checked code-point accumul port)
			    accumulated-chars)))
	   (if inside-bar?
	       (%accumulate-identifier-chars/bar accum port)
	     (%accumulate-identifier-chars accum port))))
	((char->hex-digit/or-false ch)
	 => (lambda (digit)
	      (next-digit (unsafe.fx+ digit (unsafe.fx* code-point 16))
			  (cons ch accumul))))
	(else
	 (%error "expected hex digit after backslash sequence while reading symbol"
		 (reverse-list->string (cons ch accumul))
		 (reverse-list->string accumulated-chars))))))

  (main))


(define (finalise-tokenisation port locs-alist kont token pos)
  (define-inline (%error   msg . irritants)
    (die/p   port 'read msg . irritants))
  (define-inline (%error-1 msg . irritants)
    (die/p-1 port 'read msg . irritants))

  (define-inline (main)
    (cond ((eof-object? token)
	   (values (eof-object)
		   (annotate-simple (eof-object) pos port) locs-alist kont))

	  ;;Read list that was opened by a round parenthesis.
	  ((eq? token 'lparen)
	   (let-values (((ls ls/ann locs-alist kont)
			 (finish-tokenisation-of-list port pos locs-alist kont 'rparen 'rbrack)))
	     (values ls (annotate ls ls/ann pos port) locs-alist kont)))

	  ;;Read list that was opened by a square bracket.
	  ((eq? token 'lbrack)
	   (let-values (((ls ls/ann locs-alist kont)
			 (finish-tokenisation-of-list port pos locs-alist kont 'rbrack 'rparen)))
	     (values ls (annotate ls ls/ann pos port) locs-alist kont)))

	  ;;Read a vector opened by "#(".
	  ((eq? token 'vparen)
	   (let-values (((vec vec/ann locs-alist kont)
			 (read-vector port locs-alist kont 0 '() '())))
	     (values vec (annotate vec vec/ann pos port) locs-alist kont)))

	  ;;Read a bytevector opened by "#vu8(".
	  ((eq? token 'vu8)
	   (let-values (((bv bv/ann locs-alist kont)
			 (read-u8-bytevector port locs-alist kont 0 '())))
	     (values bv (annotate bv bv/ann pos port) locs-alist kont)))

	  ;;Read a bytevector opened by "#vs8(".
	  ((eq? token 'vs8)
	   (let-values (((bv bv/ann locs-alist kont)
			 (read-s8-bytevector port locs-alist kont 0 '())))
	     (values bv (annotate bv bv/ann pos port) locs-alist kont)))

	  ((pair? token)
	   (%process-pair-token token))

	  (else
	   (%error-1 (format "unexpected ~s found" token)))))

  (define-inline (%process-pair-token token)
    (cond ((eq? (car token) 'datum) ;datum already tokenised
	   (values (cdr token)
		   (annotate-simple (cdr token) pos port) locs-alist kont))

	  ;;Read  a  sexp  quoted  with one  among:  QUOTE,  QUASIQUOTE,
	  ;;UNQUOTE,  UNQUOTE-SPLICING,  SYNTAX, QUASISYNTAX,  UNSYNTAX,
	  ;;UNSYNTAX-SPLICING.
	  ;;
	  ((eq? (car token) 'macro)
	   (let ((quoting-keyword (cdr token)))
	     (define (%read-quoted-sexp)
	       (let-values (((token1 pos) (start-tokenising/pos port)))
		 (cond ((eof-object? token1)
			(%error (format "invalid EOF after ~a read macro" quoting-keyword)))
		       (else
			(finalise-tokenisation port locs-alist kont token1 pos)))))
	     (let-values (((expr expr/ann locs-alist kont) (%read-quoted-sexp)))
	       (let ((d     (list expr))
		     (d/ann (list expr/ann)))
		 (let ((x     (cons quoting-keyword d))
		       (x/ann (cons (annotate-simple quoting-keyword pos port) d/ann)))
		   (values x (annotate x x/ann pos port) locs-alist
			   (extend-k-pair d d/ann expr '() kont)))))))

	  ;;Read an expression marked with graph notation for locations.
	  ;;
	  ;;If an entry with the same digit N is not in LOCS-ALIST: this
	  ;;mark is new;  create a new LOC structure,  marked as set and
	  ;;holding the  expression, and  register it in  in LOCS-ALIST.
	  ;;Return the expression.
	  ;;
	  ;;If an entry with the  same digit N is already in LOCS-ALIST,
	  ;;and marked set: it means a mark "#N=" has already been read,
	  ;;so raise an exception.
	  ;;
	  ;;If an entry with the  same digit N is already in LOCS-ALIST,
	  ;;but marked unset: it means that one or more references "#N#"
	  ;;have  been  already   processed;  mutate  the  existing  LOC
	  ;;structure to  reference the expression  and mark it  as set.
	  ;;References will  be processed later  by REDUCE-LOC!.  Return
	  ;;the expression.
	  ;;
	  ;;FIXME Would it be intelligent to raise an exception, in case
	  ;;of  multiple  reading  of  the  same  mark,  only  when  the
	  ;;expressions differ?
	  ;;
	  ;;Examples:
	  ;;
	  ;;  #N=123
	  ;;  #N=ciao
	  ;;  #N=(1 2 3)
	  ;;  #N=#(1 2 3)
	  ;;  #N=#vu8(1 2 3)
	  ;;
	  ((eq? (car token) 'mark)
	   (let ((N (cdr token)))
	     (let-values (((expr expr/ann locs-alist kont)
			   (read-expr port locs-alist kont)))
	       (cond ((assq N locs-alist)
		      => (lambda (pair)
			   (let ((loc (cdr pair)))
			     (when (loc-set? loc)
			       ;;FIXME It  would be beautiful  to report
			       ;;the  positions, too.   Is  it possible,
			       ;;from  the  point   of  view  of  shared
			       ;;contexts, to store the positions in LOC
			       ;;itself?
			       (%error "duplicate location mark for graph notation" N))
			     (set-loc-value!     loc expr)
			     (set-loc-value/ann! loc expr/ann)
			     (set-loc-set?!      loc #t)
			     (values expr expr/ann locs-alist kont))))
		     (else
		      (let* ((loc         (let ((value     expr)
						(value/ann 'unused)
						(set?      #t))
					    (make-loc value value/ann set?)))
			     (locs-alist1 (cons (cons N loc) locs-alist)))
			(values expr expr/ann locs-alist1 kont)))))))

	  ;;Process reference to graph notation location.  Example:
	  ;;
	  ;;  (#1=ciao #1#) => (ciao ciao)
	  ;;
	  ;;If an entry with the same digit N is in LOCS-ALIST: it means
	  ;;that either the associated  mark "#N=" has already been read
	  ;;or  another   reference  with  digit  N   has  already  been
	  ;;processed; in any case  extract the LOC structure and return
	  ;;it so that it can be later processed by REDUCE-LOC!.
	  ;;
	  ;;If an entry with digit N is not in LOCS-ALIST: it means that
	  ;;neither the associated mark  "#N=" has been read nor another
	  ;;reference  with digit  N  has been  processed;  in any  case
	  ;;create  a  new  LOC  structure, marked  unset,  register  in
	  ;;LOCS-ALIST and return  it so that it can  be later processed
	  ;;by REDUCE-LOC!.
	  ;;
	  ((eq? (car token) 'ref)
	   (let ((N (cdr token)))
	     (cond ((assq N locs-alist)
		    => (lambda (pair)
			 (values (cdr pair) 'unused locs-alist kont)))
		   (else
		    (let* ((the-loc     (let ((value     #f)
					      (value/ann 'unused)
					      (set?      #f))
					  (make-loc value value/ann set?)))
			   (locs-alist1 (cons (cons N the-loc) locs-alist)))
		      (values the-loc 'unused locs-alist1 kont))))))

	  (else
	   (%error "Vicare internal error: unknown token from reader functions" token))))

  (main))


(define (%accumulate-string-chars ls port)
  ;;Read  from PORT  characters from  the  internals of  a string  token
  ;;(after the opening double quote  has been read), accumulate them, in
  ;;reverse order and return the resulting list.
  ;;
  (define-inline (recurse accum)
    (%accumulate-string-chars accum port))
  (define-inline (%error msg . args)
    (die/p   port 'tokenize msg . args))
  (define-inline (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading string"))

  (define-inline (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (define-inline (%peek-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (peek-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (define-inline (main)
    (%read-char-no-eof (port ch)
      (else
       (%accumulate-char ls port ch))))

  (define (%accumulate-char ls port ch)
    (cond ((unsafe.char= #\" ch) ;end of the string
	   ls)
	  ((unsafe.char= #\\ ch)
	   (%parse-escape-sequence ls port))
	  (else
	   (recurse (cons ch ls)))))

  (define-inline (%parse-escape-sequence ls port)
    ;;Read  chars from PORT  parsing an  escape sequence.   The starting
    ;;backslash character has already been consumed.
    ;;
    (%read-char-no-eof (port ch)
      ;;recognise single char escape sequences
      ((unsafe.char= #\a ch)  (recurse (cons #\x7  ls)))
      ((unsafe.char= #\b ch)  (recurse (cons #\x8  ls)))
      ((unsafe.char= #\t ch)  (recurse (cons #\x9  ls)))
      ((unsafe.char= #\n ch)  (recurse (cons #\xA  ls)))
      ((unsafe.char= #\v ch)  (recurse (cons #\xB  ls)))
      ((unsafe.char= #\f ch)  (recurse (cons #\xC  ls)))
      ((unsafe.char= #\r ch)  (recurse (cons #\xD  ls)))
      ((unsafe.char= #\" ch)  (recurse (cons #\x22 ls)))
      ((unsafe.char= #\\ ch)  (recurse (cons #\x5C ls)))

      ;;inline hex escape "\xHHHH;"
      ((unsafe.char= #\x ch)
       (%read-char-no-eof (port ch1)
	 ((char->hex-digit/or-false ch1)
	  => (lambda (first-digit)
	       (%parse-escape-hex-sequence ch1 first-digit)))
	 (else
	  (%error-1 "invalid character in inline hex escape while reading string" ch1))))

      ;;Consume the sequence:
      ;;
      ;;  \<intraline whitespace><line ending><intraline whitespace>
      ;;
      ;;after the backslash: read all the white space chars until a line
      ;;ending, read the line ending  (LF, CRLF, NEL, CRNEL or LS), then
      ;;read again all the white space chars.
      ;;
      ((intraline-whitespace? ch)
       (let next-whitespace-char ()
	 (%read-char-no-eof (port chX)
	   ((intraline-whitespace? chX)
	    (next-whitespace-char))
	   ((char-is-single-char-line-ending? chX)
	    (%discard-trailing-intraline-whitespace ls port (read-char port)))
	   ((char-is-carriage-return? chX)
	    (%read-char-no-eof (port chY)
	      ((char-is-newline-after-carriage-return? chY)
	       (%discard-trailing-intraline-whitespace ls port (read-char port)))
	      (else
	       (%discard-trailing-intraline-whitespace ls port chY))))
	   (else
	    (%error-1 "invalid non-whitespace character after escape")))))

      ;;Consume the sequence:
      ;;
      ;;  \<line ending><intraline whitespace>
      ;;
      ;;in which  the line ending  is a standalone  char LF, NEL  or LS,
      ;;without prefix intraline whitespace.
      ;;
      ((char-is-single-char-line-ending? ch)
       (%discard-trailing-intraline-whitespace ls port (read-char port)))

      ;;Consume the sequence:
      ;;
      ;;  \<line ending><intraline whitespace>
      ;;
      ;;in which the line ending is CRLF or CRNEL, without prefix
      ;;intraline blanks.
      ;;
      ((char-is-carriage-return? ch)
       (%read-char-no-eof (port ch1)
	 ((char-is-newline-after-carriage-return? ch1)
	  (%discard-trailing-intraline-whitespace ls port (read-char port)))
	 (else
	  (%discard-trailing-intraline-whitespace ls port ch1))))

      (else
       (%error-1 "invalid escape sequence while reading string" ch))))

  (define-inline (%parse-escape-hex-sequence ch first-digit)
    ;;Read from  PORT characters composing  an escaped character  in hex
    ;;format "\xHHHH;" and return the resulting character.
    ;;
    ;;CH is the first character  in the hex sequence; FIRST-DIGIT is the
    ;;fixnum representing the first digit in the hex sequence, it is the
    ;;conversion result of CH.
    ;;
    (let next-char ((code-point first-digit)
		    (accum      (cons ch '(#\x #\\))))
      (%read-char-no-eof (port chX)
	((char->hex-digit/or-false chX)
	 => (lambda (digit)
	      (next-char (unsafe.fx+ (unsafe.fx* code-point 16) digit)
			 (cons chX accum))))
	((unsafe.char= chX #\;)
	 (recurse (cons (fixnum->char/checked code-point (cons chX accum) port) ls)))
	(else
	 (%error-1 "invalid char in escape sequence while reading string"
		   (reverse-list->string (cons chX accum)))))))

  (define (%discard-trailing-intraline-whitespace ls port ch)
    ;;Analyse CH,  and then chars read from  PORT, discarding whitespace
    ;;characters;    at    the    first   non-whitespace    char    call
    ;;%ACCUMULATE-CHAR.
    ;;
    ;;This function  is used to consume the  second intraline whitespace
    ;;in the sequence:
    ;;
    ;;  \<intraline whitespace><line ending><intraline whitespace>
    ;;                                             ^
    ;;                                         this one
    ;;
    (define-inline (next-char ch)
      (%accumulate-char ls port ch))
    (if (intraline-whitespace? ch)
	(let next-whitespace-char ()
	  (%read-char-no-eof (port ch1)
	    ((intraline-whitespace? ch1)
	     (next-whitespace-char))
	    (else
	     (next-char ch1))))
      (next-char ch)))

  (define-inline (intraline-whitespace? ch)
    (or (unsafe.char= ch #\x9)
	(eq? (char-general-category ch) 'Zs)))

  (main))


(define (finish-tokenisation-of-char port)
  ;;Called after a hash character followed by a backslash character have
  ;;been read from PORT.  Read  characters from PORT parsing a character
  ;;datum; return the datum:
  ;;
  ;;   (datum . <ch>)
  ;;
  ;;where <CH> is the character value.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading character"))
  (define-inline (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
    (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
      . ?cond-clauses))

  (define-inline (main)
    (%read-char-no-eof (port ch)
      ;;There are multiple character sequences starting with "#\n".
      ((unsafe.char= #\n ch)
       (let ((ch1 (peek-char port)))
	 (cond ((eof-object? ch1)
		'(datum . #\n))
	       ((unsafe.char= #\u ch1)
		(read-char port)
		(%finish-reading-character-name port "ul" '(datum . #\x0)))
	       ((unsafe.char= #\e ch1)
		(read-char port)
		(%finish-reading-character-name port "ewline" '(datum . #\xA)))
	       ((delimiter? ch1)
		'(datum . #\n))
	       (else
		(%error "invalid syntax" (string #\# #\\ #\n ch1))))))

      ((unsafe.char= #\a ch)
       (%finish-reading-character-name port "alarm"	'(datum . #\x7)))
      ((unsafe.char= #\b ch)
       (%finish-reading-character-name port "backspace"	'(datum . #\x8)))
      ((unsafe.char= #\t ch)
       (%finish-reading-character-name port "tab"	'(datum . #\x9)))
      ((unsafe.char= #\l ch)
       (%finish-reading-character-name port "linefeed"	'(datum . #\xA)))
      ((unsafe.char= #\v ch)
       (%finish-reading-character-name port "vtab"	'(datum . #\xB)))
      ((unsafe.char= #\p ch)
       (%finish-reading-character-name port "page"	'(datum . #\xC)))
      ((unsafe.char= #\r ch)
       (%finish-reading-character-name port "return"	'(datum . #\xD)))
      ((unsafe.char= #\e ch)
       (%finish-reading-character-name port "esc"	'(datum . #\x1B)))
      ((unsafe.char= #\s ch)
       (%finish-reading-character-name port "space"	'(datum . #\x20)))
      ((unsafe.char= #\d ch)
       (%finish-reading-character-name port "delete"	'(datum . #\x7F)))

      ;;Read the char "#\x" or a character in hex format "#\xHHHH".
      ((unsafe.char= #\x ch)
       (let ((ch1 (peek-char port)))
	 (cond ((or (eof-object? ch1)
		    (delimiter?  ch1))
		'(datum . #\x))
	       ((char->hex-digit/or-false ch1)
		=> (lambda (digit)
		     (read-char port)
		     (let next-digit ((digit       digit)
				      (accumulated (cons ch1 '(#\x))))
		       (let ((chX (peek-char port)))
			 (cond ((or (eof-object? chX)
				    (delimiter? chX))
				(cons 'datum (fixnum->char/checked digit accumulated port)))
			       ((char->hex-digit/or-false chX)
				=> (lambda (digit0)
				     (read-char port)
				     (next-digit (+ (* digit 16) digit0)
						 (cons chX accumulated))))
			       (else
				(%error "invalid character sequence"
					(reverse-list->string (cons chX accumulated)))))))))
	       (else
		(%error "invalid character sequence" (string #\# #\\ ch1))))))

      ;;It is a normal character.
      (else
       (let ((ch1 (peek-char port)))
	 (if (or (eof-object? ch1)
		 (delimiter?  ch1))
	     (cons 'datum ch)
	   (%error "invalid syntax" (string #\# #\\ ch ch1)))))))

  (define (%finish-reading-character-name port str datum)
    ;;Read characters  from PORT  verifying that they  are equal  to the
    ;;characters drawn from the string  STR; if reading and comparing is
    ;;successful: peek one more char from PORT and verify that it is EOF
    ;;or a delimiter (according to DELIMITER?).
    ;;
    ;;If successful return DATUM, else raise an exception.
    ;;
    ;;This  function   is  used  to  parse  characters   in  the  format
    ;;"#\newline" when the sequence "#\ne" has already been consumed; in
    ;;this case the function is called as:
    ;;
    ;;   (%finish-reading-character-name port "ewline" '(datum . #\xA))
    ;;
    ;;As an extension (currently not used in the lexer, Marco Maggi; Oct
    ;;12, 2011),  this function supports  also the case of  character in
    ;;the  format  "#\A"  when  the  sequence  "#\A"  has  already  been
    ;;consumed, and we only need to  verify that the next char from PORT
    ;;is EOF or a delimiter.  In this case DATUM is ignored.
    ;;
    (define-inline (%error msg . args)
      (die/p port 'tokenize msg . args))
    (let ((ch (peek-char port)))
      (cond ((or (eof-object? ch)
		 (delimiter? ch))
	     (cons 'datum (unsafe.string-ref str 0)))
	    ((unsafe.char= ch (unsafe.string-ref str 1))
	     (read-char port)
	     (let loop ((str.index 2))
		 (if (unsafe.fx= str.index (unsafe.string-length str))
		     (let ((ch (peek-char port)))
		       (cond ((eof-object? ch) datum)
			     ((delimiter?  ch) datum)
			     (else
			      (%error "invalid character after expected sequence"
				      (string-append str (string ch))))))
		   (let ((ch (read-char port)))
		     (cond ((eof-object? ch)
			    (%error "invalid EOF in the middle of expected sequence" str))
			   ((unsafe.char= ch (unsafe.string-ref str str.index))
			    (loop (unsafe.fxadd1 str.index)))
			   (else
			    (%error "invalid char while scanning string" ch str)))))))
	    (else
	     (%error "invalid syntax" (unsafe.string-ref str 0) ch)))))

  (main))


;;;; reading numbers

(let-syntax ((num-error (syntax-rules ()
			  ((_ p str ls)
			   (die/p-1 p 'read str (reverse-list->string ls))))))
  (define-syntax port-config
    (syntax-rules (GEN-TEST GEN-ARGS FAIL EOF-ERROR GEN-DELIM-TEST)
      ((_ GEN-ARGS k . rest) (k (p ac) . rest))
      ((_ FAIL (p ac))
       (num-error p "invalid numeric sequence" ac))
      ((_ FAIL (p ac) c)
       (num-error p "invalid numeric sequence" (cons c ac)))
      ((_ EOF-ERROR (p ac))
       (num-error p "invalid eof while reading number" ac))
      ((_ GEN-DELIM-TEST c sk fk)
       (if (delimiter? c) sk fk))
      ((_ GEN-TEST var next fail (p ac) eof-case char-case)
       (let ((c (peek-char p)))
	 (if (eof-object? c)
	     (let ()
	       (define-syntax fail
		 (syntax-rules ()
		   ((_) (num-error p "invalid numeric sequence" ac))))
	       eof-case)
	   (let ((var c))
	     (define-syntax fail
	       (syntax-rules ()
		 ((_)
		  (num-error p "invalid numeric sequence" (cons var ac)))))
	     (define-syntax next
	       (syntax-rules ()
		 ((_ who args (... ...))
		  (who p (cons (get-char p) ac) args (... ...)))))
	     char-case)))))))

(define-string->number-parser port-config
  (parse-string u:digit+ u:sign u:dot))


;;;; reading comments

(define (read-and-discard-up-to-and-including-line-ending port)
  (let ((ch (read-char port)))
    (unless (or (eof-object? ch)
		(char-is-single-char-line-ending? ch)
		;;A standalone CR ends  the line, see R6RS syntax formal
		;;account.
		(char-is-carriage-return? ch))
      (read-and-discard-up-to-and-including-line-ending port))))

(define (finish-tokenisation-of-multiline-comment port)
  ;;Parse a multiline comment  "#| ... |#", possibly nested.  Accumulate
  ;;the  characters in  the comment,  excluding the  "#|" and  "|#", and
  ;;discard them.
  ;;
  (define-inline (%multiline-error)
    (die/p port 'tokenize "end of file encountered while inside a #|-style comment"))

  (define-inline (string->reverse-list str str.start accumulated)
    (%string->reverse-list str str.start (unsafe.string-length str) accumulated))
  (define (%string->reverse-list str str.index str.len accumulated)
    (if (unsafe.fx= str.index str.len)
	accumulated
      (%string->reverse-list str (unsafe.fxadd1 str.index) str.len
			     (cons (unsafe.string-ref str str.index) accumulated))))

  (define (accumulate-comment-chars port ac)
    (define-inline (recurse ac)
      (accumulate-comment-chars port ac))
    (let ((c (read-char port)))
      (cond ((eof-object? c)
	     (%multiline-error))

	    ;;A vertical bar character may or may not end this multiline
	    ;;comment.
	    ((unsafe.char= #\| c)
	     (let next-vertical-bar ((ch1 (read-char port)) (ac ac))
	       (cond ((eof-object? ch1)
		      (%multiline-error))
		     ((unsafe.char= #\# ch1) ;end of comment
		      ac)
		     ((unsafe.char= #\| ch1) ;optimisation for sequence of bars?!?
		      (next-vertical-bar (read-char port) (cons ch1 ac)))
		     (else
		      (recurse (cons ch1 ac))))))

	    ;;A hash character  may or may not start  a nested multiline
	    ;;comment.   Read a  nested multiline  comment, if  there is
	    ;;one.
	    ((unsafe.char= #\# c)
	     (let ((ch1 (read-char port)))
	       (cond ((eof-object? ch1)
		      (%multiline-error))
		     ((unsafe.char= #\| ch1) ;it is a nested comment
		      (let ((v (finish-tokenisation-of-multiline-comment port)))
			(if (string? v)
			    (recurse (string->reverse-list v 0 ac))
			  (recurse ac))))
		     (else ;it is a standalone hash char
		      (recurse (cons ch1 (cons #\# ac)))))))

	    (else
	     (recurse (cons c ac))))))

  (accumulate-comment-chars port '()))


;;;; character reading helpers

(define-syntax read-char*
  (syntax-rules ()
    ((_ port ls str who case-insensitive? delimited?)
     (%read-char* port ls str who case-insensitive? delimited?))
    ((_ port ls str who)
     (%read-char* port ls str who #f #f))))

(define (%read-char* port ls str who case-insensitive? delimited?)
  ;;Read multiple characters from PORT expecting them to be the chars in
  ;;the string STR; this function is  used to read a chunk of token.  If
  ;;successful return  unspecified values; if  an error occurs  raise an
  ;;exception.
  ;;
  ;;LS  must  be  a  list  of  characters already  read  from  PORT  and
  ;;recognised to be the opening of  the token: they are used to build a
  ;;better error message.  WHO must  be a string describing the expected
  ;;token.
  ;;
  ;;If CASE-INSENSITIVE? is true: the comparison between characters read
  ;;from PORT and characters drawn from STR is case insensitive.
  ;;
  ;;If DELIMITED? is true: after the chars in STR have been successfully
  ;;read from PORT, a lookahead is performed on PORT and the result must
  ;;be EOF or a delimiter character (according to DELIMITER?).
  ;;
  ;;Usage example:  when reading the  comment "#!r6rs" this  function is
  ;;called as:
  ;;
  ;;	(read-char* port '(#\r) "6rs" #f #f)
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define str.len
    (string-length str))
  (let loop ((i 0) (ls ls))
    (if (fx= i str.len)
	(when delimited?
	  (let ((ch (peek-char port)))
	    (when (and (not (eof-object? ch))
		       (not (delimiter?  ch)))
	      (%error (format "invalid ~a: ~s" who (reverse-list->string (cons ch ls)))))))
      (let ((ch (read-char port)))
	(cond ((eof-object? ch)
	       (%error (format "invalid eof inside ~a" who)))
	      ((or (and (not case-insensitive?)
			(unsafe.char= ch (string-ref str i)))
		   (and case-insensitive?
			(unsafe.char= (char-downcase ch) (string-ref str i))))
	       (loop (add1 i) (cons ch ls)))
	      (else
	       (%error-1 (format "invalid ~a: ~s" who (reverse-list->string (cons ch ls))))))))))

(define (read-char-skip-whitespace port caller)
  ;;Read and  discard characters from  PORT while they are  white spaces
  ;;according  to  CHAR-WHITESPACE?.  Return  the  first character  read
  ;;which is not a white space.
  ;;
  ;;CALLER must be a string  describing the token the caller is parsing,
  ;;it is used for error reporting.
  ;;
  (define-inline (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (recurse)
    (read-char-skip-whitespace port caller))
  (let ((ch (read-char port)))
    (cond ((eof-object? ch)
	   (%error "invalid EOF inside" caller))
	  ((char-whitespace? ch)
	   (recurse))
	  (else ch))))


(define-inline (finish-tokenisation-of-list port start-pos locs kont matching-paren wrong-paren)
  ;;Finish tokenisation  of list datum  reading from PORT; to  be called
  ;;after the opening parenthesis has been already tokenised.
  ;;
  ;;This function parses the next datum then calls itself recursively to
  ;;parse the  remaining items; whenever  this function returns,  it has
  ;;successfully read  all the  items in the  list including  the ending
  ;;parenthesis.
  ;;
  ;;Return four  values: the plain  S-expression being the  list itself;
  ;;the annotated S-expression; the updated collection of graph notation
  ;;locations; a continuation thunk to be used to finalise references to
  ;;graph notation locations.
  ;;
  ;;START-POS is  the compound position value  representing the position
  ;;of the opening parenthesis; useful to report errors.
  ;;
  ;;LOCS is  the collection of  graph notation locations  accumulated so
  ;;far.
  ;;
  ;;MATCHING-PAREN must be either the symbol RPAREN or the symbol RBRACK
  ;;and it represents the token matching the opening parenthesis.
  ;;
  ;;WRONG-PAREN must  be either the  symbol RPAREN or the  symbol RBRACK
  ;;and it  represents the  which, if found,  causes a  mismatch between
  ;;opening and closing parentheses.
  ;;
  (%finish-tokenisation-of-list port start-pos locs kont matching-paren wrong-paren #t))

(define (%finish-tokenisation-of-list port start-pos locs kont matching-paren wrong-paren
				      reading-first-item?)
  (define-inline (recurse-to-read-cdr locs1 kont1)
    (%finish-tokenisation-of-list port start-pos locs1 kont1 matching-paren wrong-paren #f))
  (define-inline (%error msg . irritants)
    (die/p port 'read msg . irritants))
  (define-inline (%error-1 msg . irritants)
    (die/p-1 port 'read msg . irritants))
  (define-inline (%paren-symbol->char paren)
    (if (eq? paren 'rparen) #\) #\]))
  (define (%mismatched-paren-error)
    (%error (format "mismatching parenthesis while reading list, \
                     expecting \"~a\" found \"~a\""
	      (%paren-symbol->char matching-paren)
	      (%paren-symbol->char wrong-paren))))

  (let-values (((token pos) (start-tokenising/pos port)))
    (cond ((eof-object? token)
	   (%error (string-append "unexpected end of file while reading list \
                                   started at line "
				  (number->string (compound-position-line   start-pos))
				  " column "
				  (number->string (compound-position-column start-pos)))))

	  ;;the correct ending parenthesis was found
	  ((eq? token matching-paren)
	   (values '() '() locs kont))

	  ;;a mismatched ending parenthesis was found
	  ((eq? token wrong-paren)
	   (%mismatched-paren-error))

	  ;;The token is  a dot, the next token must be  the last in the
	  ;;list.
	  ((eq? token 'dot)
	   (when reading-first-item?
	     (%error "invalid dot as first item while reading list"))
	   (let*-values (((the-cdr the-cdr/ann locs1 kont1) (read-expr port locs kont))
	   		 ((token1 pos1)                     (start-tokenising/pos port)))
	     (cond ((eq? token1 matching-paren)
		    (values the-cdr the-cdr/ann locs1 kont1))
		   ((eq? token1 wrong-paren)
		    (%mismatched-paren-error))
		   ((eq? token1 'dot)
		    (%error "invalid second dot while reading list"))
		   (else
		    (%error "invalid second form after dot while reading list" token1)))))

	  ;;It is an item.
	  (else
	   (let*-values (((the-car the-car/ann locs1 kont1)
			  (finalise-tokenisation port locs kont token pos))
	   		 ((the-cdr the-cdr/ann locs2 kont2)
			  (recurse-to-read-cdr locs1 kont1)))
	     (let ((the-list      (cons the-car     the-cdr))
		   (the-list/ann  (cons the-car/ann the-cdr/ann)))
	       (values the-list the-list/ann locs2
		       (extend-k-pair the-list the-list/ann the-car the-cdr kont2))))))))


(define (extend-k-pair x x^ a d k)
  (cond ((or (loc? a) (loc? d))
	 (lambda ()
	   (let ((a (car x)))
	     (when (loc? a)
	       (set-car! x (loc-value a))
	       (set-car! x^ (loc-value/ann a))))
	   (let ((d (cdr x)))
	     (when (loc? d)
	       (set-cdr! x (loc-value d))
	       (set-cdr! x^ (loc-value/ann d))))
	   (k)))
	(else k)))

(define (vector-put v v^ k i ls ls^)
  (cond ((null? ls) k)
	(else
	 (let ((a (car ls)))
	   (vector-set! v i a)
	   (vector-set! v^ i (car ls^))
	   (vector-put v v^
		       (if (loc? a)
			   (lambda ()
			     (vector-set! v i (loc-value a))
			     (vector-set! v^ i (loc-value/ann a))
			     (k))
			 k)
		       (fxsub1 i)
		       (cdr ls)
		       (cdr ls^))))))

(define (read-vector p locs k count ls ls^)
  (let-values (((token pos) (start-tokenising/pos p)))
    (cond ((eof-object? token)
	   (die/p p 'read "end of file encountered while reading a vector"))
	  ((eq? token 'rparen)
	   (let ((v  (make-vector count))
		 (v^ (make-vector count)))
	     (let ((k (vector-put v v^ k (fxsub1 count) ls ls^)))
	       (values v v^ locs k))))
	  ((eq? token 'rbrack)
	   (die/p-1 p 'read "unexpected \")\" while reading a vector"))
	  ((eq? token 'dot)
	   (die/p-1 p 'read "unexpected \".\" while reading a vector"))
	  (else
	   (let-values (((a a^ locs k) (finalise-tokenisation p locs k token pos)))
	     (read-vector p locs k (fxadd1 count)
			  (cons a ls) (cons a^ ls^)))))))

(define (read-u8-bytevector p locs k count ls)
  (let-values (((t pos) (start-tokenising/pos p)))
    (cond
     ((eof-object? t)
      (die/p p 'read "end of file encountered while reading a bytevector"))
     ((eq? t 'rparen)
      (let ((v (u8-list->bytevector (reverse ls))))
	(values v v locs k)))
     ((eq? t 'rbrack)
      (die/p-1 p 'read "unexpected ) while reading a bytevector"))
     ((eq? t 'dot)
      (die/p-1 p 'read "unexpected . while reading a bytevector"))
     (else
      (let-values (((a a^ locs k) (finalise-tokenisation p locs k t pos)))
	(unless (and (fixnum? a) (fx<= 0 a) (fx<= a 255))
	  (die/ann a^ 'read "invalid value in a u8 bytevector" a))
	(read-u8-bytevector p locs k (fxadd1 count) (cons a ls)))))))

(define (read-s8-bytevector p locs k count ls)
  (let-values (((t pos) (start-tokenising/pos p)))
    (cond
     ((eof-object? t)
      (die/p p 'read "end of file encountered while reading a bytevector"))
     ((eq? t 'rparen)
      (let ((v (let ((bv ($make-bytevector count)))
		 (let loop ((i  (- count 1))
			    (ls ls))
		   (if (null? ls)
		       bv
		     (begin
		       ($bytevector-set! bv i (car ls))
		       (loop (- i 1) (cdr ls))))))))
	(values v v locs k)))
     ((eq? t 'rbrack)
      (die/p-1 p 'read "unexpected ) while reading a bytevector"))
     ((eq? t 'dot)
      (die/p-1 p 'read "unexpected . while reading a bytevector"))
     (else
      (let-values (((a a^ locs k) (finalise-tokenisation p locs k t pos)))
	(unless (and (fixnum? a) (fx<= -128 a) (fx<= a 127))
	  (die/ann a^ 'read "invalid value in a s8 bytevector" a))
	(read-s8-bytevector p locs k (fxadd1 count) (cons a ls)))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'read-char-no-eof		'scheme-indent-function 1)
;;eval: (put 'peek-char-no-eof		'scheme-indent-function 1)
;;eval: (put '%read-char-no-eof		'scheme-indent-function 1)
;;eval: (put '%peek-char-no-eof		'scheme-indent-function 1)
;;End:
