;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011, 2012, 2013  Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
    read			get-datum
    get-annotated-datum

    ;; annotated datum inspection
    annotation?
    annotation-expression	annotation-stripped
    annotation-source		annotation-textual-position

    ;; internal functions only for Vicare
    read-source-file		read-script-source-file
    read-library-source-file)
  (import (except (ikarus)
		  ;; public functions
		  read				get-datum
		  get-annotated-datum

		  ;; annotated datum inspection
		  annotation?
		  annotation-expression		annotation-stripped
		  annotation-source		annotation-textual-position

		  ;; internal functions only for Vicare
		  read-source-file		read-script-source-file
		  read-library-source-file)
    (only (vicare.foreign-libraries)
	  register-filename-foreign-library
	  autoload-filename-foreign-library)
    (vicare language-extensions syntaxes)
    (prefix (vicare platform words) words.)
    (prefix (vicare unsafe operations)
	    unsafe.))


;;;; syntax helpers

(define-syntax read-char-no-eof
  (lambda (stx)
    (syntax-case stx ()
      ((read-char-no-eof (?port ?ch-name ?raise-error) . ?cond-clauses)
       (and (identifier? #'?ch-name)
	    (identifier? #'?raise-error))
       #'(let ((?ch-name (get-char-and-track-textual-position ?port)))
	   (cond ((eof-object? ?ch-name)
		  (?raise-error))
		 . ?cond-clauses))))))

(define (%implementation-violation who msg . irritants)
  (raise (condition
	  (make-assertion-violation)
	  (make-implementation-restriction-violation)
	  (make-who-condition who)
	  (make-message-condition msg)
	  (make-irritants-condition irritants))))


;;;; miscellaneous helpers

;;If set to  true enables loading shared libraries  specified by comment
;;lists.  This  must be enabled only  when reading a program  or library
;;source file.
;;
(define shared-library-loading-enabled?
  (make-parameter #f))

;;Used to make the reader functions aware of the library file name being
;;read.
(define current-library-file
  (make-parameter #f))

;;Used to turn on or  off case sensitivity for identifiers.  The default
;;for R6RS is case sensitive identifiers.  Sensitivity can be changed on
;;the fly with the #ci<form> and #cs<form> syntaxes.
(define case-insensitive?
  (make-parameter #f))

;;Used to define  custom character names.  With the  syntax "\{name}" in
;;strings.
(define custom-named-chars
  (make-parameter #f))

(define-syntax-rule (reverse-list->string ell)
  ;;There are more efficient ways to do this, but ELL is usually short.
  ;;
  (list->string (reverse ell)))

(define-syntax-rule (port-in-r6rs-mode? port)
  (eq? (port-mode port) 'r6rs))

(define-syntax-rule (port-in-vicare-mode? port)
  (eq? (port-mode port) 'vicare))

(define-syntax-rule (source-code-port? port)
  (and (or (input-port? port)
	   (input/output-port? port))
       (textual-port? port)))

;;; --------------------------------------------------------------------

(define-inline (%assert-argument-is-source-code-port who port)
  (unless (source-code-port? port)
    (assertion-violation who "expected textual input port as argument" port)))

(define-inline (%assert-argument-is-procedure who x)
  (unless (procedure? x)
    (assertion-violation who "expected procedure as argument" x)))

;;; --------------------------------------------------------------------

(define-inline (bytevector-flonum-single-le-set! bv i x)
  (bytevector-ieee-single-set! bv i x (endianness little)))

(define-inline (bytevector-flonum-single-be-set! bv i x)
  (bytevector-ieee-single-set! bv i x (endianness big)))

(define-inline (bytevector-flonum-single-ne-set! bv i x)
  (bytevector-ieee-single-native-set! bv i x))

(define-inline (bytevector-flonum-double-le-set! bv i x)
  (bytevector-ieee-double-set! bv i x (endianness little)))

(define-inline (bytevector-flonum-double-be-set! bv i x)
  (bytevector-ieee-double-set! bv i x (endianness big)))

(define-inline (bytevector-flonum-double-ne-set! bv i x)
  (bytevector-ieee-double-native-set! bv i x))

;;; --------------------------------------------------------------------

(define-inline (bytevector-cflonum-single-le-set! bv i x)
  (begin
    (bytevector-ieee-single-set! bv i                (real-part x) (endianness little))
    (bytevector-ieee-single-set! bv (unsafe.fx+ 4 i) (imag-part x) (endianness little))))

(define-inline (bytevector-cflonum-single-be-set! bv i x)
  (begin
    (bytevector-ieee-single-set! bv i                (real-part x) (endianness big))
    (bytevector-ieee-single-set! bv (unsafe.fx+ 4 i) (imag-part x) (endianness big))))

(define-inline (bytevector-cflonum-single-ne-set! bv i x)
  (begin
    (bytevector-ieee-single-native-set! bv i                (real-part x))
    (bytevector-ieee-single-native-set! bv (unsafe.fx+ 4 i) (imag-part x))))

(define-inline (bytevector-cflonum-double-le-set! bv i x)
  (begin
    (bytevector-ieee-double-set! bv i                (real-part x) (endianness little))
    (bytevector-ieee-double-set! bv (unsafe.fx+ 8 i) (imag-part x) (endianness little))))

(define-inline (bytevector-cflonum-double-be-set! bv i x)
  (begin
    (bytevector-ieee-double-set! bv i                (real-part x) (endianness big))
    (bytevector-ieee-double-set! bv (unsafe.fx+ 8 i) (imag-part x) (endianness big))))

(define-inline (bytevector-cflonum-double-ne-set! bv i x)
  (begin
    (bytevector-ieee-double-native-set! bv i                (real-part x))
    (bytevector-ieee-double-native-set! bv (unsafe.fx+ 8 i) (imag-part x))))


;;;; interface to low level functions

(define-inline (%seed-strings->gensym pretty-string unique-string)
  (foreign-call "ikrt_strings_to_gensym" pretty-string unique-string))


;;;; annotated datums
;;
;;Constructor: make-annotation EXPR STRIPPED SOURCE POS
;;Predicate: annotation? OBJ
;;
;;Field name: expression
;;Field accessor: annotation-expression ANN
;;  A list,  vector, identifier, what-have-you that  may contain further
;;  annotations.
;;
;;Field name: stripped
;;Field accessor: annotation-stripped ANN
;;  The same S-expression of the EXPRESSION field with no annotations.
;;
;;Field name: source
;;Field accessor: annotation-source ANN
;;  A pair whose car is the port  identifier and whose cdr is the offset
;;  of the character.
;;
;;Field name: textual-position
;;Field accessor: annotation-textual-position ANN
;;  A  condition  object  of  type "&source-position"  representing  the
;;  position of  the expression in the  source code.  It is  used by the
;;  expander.
;;
(define-struct annotation
  (expression stripped source textual-position))

(define-inline (annotate-simple datum textual-pos)
  (make-annotation datum datum
		   (cons (source-position-port-id   textual-pos)
			 (source-position-character textual-pos))
		   textual-pos))

(define-inline (annotate stripped expression textual-pos)
  (make-annotation expression stripped
		   (cons (source-position-port-id   textual-pos)
			 (source-position-character textual-pos))
		   textual-pos))

(define (%annotation-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
    (define-inline (%pretty-print thing)
      (pretty-print* thing port 0 #f))
  (%display "#[annotation")
  ;;Writing   the  annotation   expression  makes   the  output   really
  ;;unreadable.
  (%display " expression=#<omitted>")
  (%display " stripped=")		(%pretty-print (annotation-stripped S))
  ;;Avoid printing  the SOURCE field  because it  may be removed  in the
  ;;future and  all its  informations are  also in  the TEXTUAL-POSITION
  ;;field.
  (%display " textual-position=")	(%write (annotation-textual-position S))
  (%display "]"))


;;;; graph notation location structures
;;
;;Graph  notation  allows  the   construction  at  read-time  of  shared
;;structures as described by SRFI 38:
;;
;;   <http://srfi.schemers.org/srfi-38/srfi-38.html>
;;
;;Graph notation  markers and references are supported  inside lists and
;;vectors; a mark  has syntax "#N=<expr>" a reference  has syntax "#N#",
;;where N is an exact integer.  Examples:
;;
;;  (#1=ciao #1#)	=> (ciao ciao)
;;  (#1# #1=ciao)	=> (ciao ciao)
;;
;;The reader builds a tree  of datums representing a symbolic expression
;;and keeps a collection of locations, currently an association list:
;;
;;* Whenever  a new mark is found,  its associated datum is  read.  If a
;;LOC structure  with the  same number is  already in the  collection in
;;"unset" state: the datum is stored in the structure, its state changed
;;to "set" and the datum is returned.  Else a new LOC structure in "set"
;;state  is registered  in the  collection, holding  the datum,  and the
;;datum is returned.
;;
;;* Whenever  a reference  is found:  if a LOC  structure with  the same
;;number  is already  in the  collection, the  associated  expression is
;;extracted  and used  as datum;  else a  new LOC  structure  in "unset"
;;state, holding  the datum,  is both registered  in the  collection and
;;returned as result of the reading.
;;
;;At the end  of the reading the tree  may contain unresolved references
;;represented by  LOC structures; if  all the referenced  locations have
;;been read, each LOC structure  is substituted by the associated datum.
;;This  substitution,  also  called  "reduction",  is  performed  by  an
;;appropriate thunk incrementally built  while reading the expression to
;;keep track of  unresolved references; such thunk is  the KONT argument
;;of many reader functions in this library.
;;
;;
;;Constructor: make-loc VALUE VALUE/ANN SET?
;;Predicate: loc? OBJ
;;
;;Field name: value
;;Field accessor: loc-value LOC
;;Field mutator: set-loc-value! LOC NEW-VALUE
;;   The expression marked by a graph location.
;;
;;Field name: value/ann
;;Field accessor: loc-value/ann LOC
;;Field mutator: set-loc-value/ann! LOC NEW-VALUE/ANN
;;   The expression  marked by a  graph location, wrapped  in ANNOTATION
;;   data structures.
;;
;;Field name: set?
;;Field accessor: loc-set? LOC
;;Field mutator: set-loc-set?! LOC NEW-SET?
;;
;;   A boolean value.
;;
;;   True  if the  datum associated  to this  location has  already been
;;   read; in this case the VLAUE and VALUE/ANN fields contain the datum
;;   and the  annotated datum.
;;
;;   False if  the datum associated to  this location hasn  not yet been
;;   read;  in  this  case   the  VLAUE  and  VALUE/ANN  fields  contain
;;   meaningless values.
;;
;;Field name: textual-position
;;Field accessor: loc-textual-position ANN
;;  A  condtion  object  of  type  "&source-position"  representing  the
;;  position of the  location in the source code.  It  is used to report
;;  better errors.
;;
(define-struct loc
  (value value/ann set? textual-position))

(define EMPTY-LOCATIONS-COLLECTION '())


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

(define-syntax-rule (die/pos port offset who msg . irritants)
  (die/lex (make-compound-position/with-offset port offset) who msg . irritants))

(define-syntax-rule (die/p p who msg . irritants)
  (die/pos p 0 who msg . irritants))

(define-syntax-rule (die/p-1 p who msg . irritants)
  (die/pos p -1 who msg . irritants))

(define-syntax-rule (die/ann ann who msg . irritants)
  (die/lex (annotation-textual-position ann) who msg . irritants))


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
      (unsafe.char= ch #\|)))

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
      (unsafe.char= ch #\~)))

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
  (define-syntax-rule (%error msg . args)
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
;;
;;There are multiple entry points for the reader:
;;
;;  read
;;  get-datum
;;  get-annotated-datum
;;
;;but:   READ    is   a    wrapper   for   GET-DATUM;    GET-DATUM   and
;;GET-ANNOTATED-DATUM call READ-EXPR.
;;
;;READ-EXPR can call itself recursively.
;;

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
  (parametrise ((shared-library-loading-enabled? #f))
    (let-values (((expr expr/ann locations kont)
		  (parametrise ((custom-named-chars (make-eq-hashtable)))
		    (read-expr port EMPTY-LOCATIONS-COLLECTION void))))
      (if (null? locations)
	  expr
	(begin
	  (for-each (reduce-loc! port)
	    locations)
	  (kont)
	  (if (loc? expr)
	      (loc-value expr)
	    expr))))))

(define (get-annotated-datum port)
  ;;Defined  by Ikarus.   Like GET-DATUM,  but rather  than  returning a
  ;;datum  return a  hierarchy of  ANNOTATION structures  with  the same
  ;;hierarchy of the datum and embedding the datum itself.
  ;;
  (parametrise ((shared-library-loading-enabled? #f))
    ($get-annotated-datum port)))

(define ($get-annotated-datum port)
  (define who 'get-annotated-datum)
  (define (%return-annotated x)
    (if (and (annotation? x)
	     (eof-object? (annotation-expression x)))
	(eof-object)
      x))
  (%assert-argument-is-source-code-port who port)
  (let-values (((expr expr/ann locations kont)
		(parametrise ((custom-named-chars (make-eq-hashtable)))
		  (read-expr port EMPTY-LOCATIONS-COLLECTION void))))
    (if (null? locations)
	(%return-annotated expr/ann)
      (begin
	(for-each (reduce-loc! port)
	  locations)
	(kont)
	(if (loc? expr)
	    (loc-value/ann expr)
	  (%return-annotated expr/ann))))))


;;;; public functions used by Vicare itself
;;
;;These  functions  are exported  by  this  library  but not  listed  in
;;"makefile.sps", so they are not visible to client code.
;;
;;The following functions are entry points to the reader:
;;
;;   read-source-file
;;   read-script-source-file
;;   read-library-source-file
;;
;;but all of them call $GET-ANNOTATED-DATUM.
;;

(define (read-library-source-file filename)
  ;;Open FILENAME for input only  using the native transcoder, then read
  ;;and return the first datum; close the port.
  ;;
  (let ((port (open-input-file filename)))
    (parameterize ((current-library-file		filename)
		   (shared-library-loading-enabled?	#t))
      (unwind-protect
	  ($get-annotated-datum port)
	(close-input-port port)))))

(define (read-source-file filename)
  ;;Open FILENAME for input only  using the native transcoder, then read
  ;;and return all the datums in a list; close the port.
  ;;
  (parameterize ((shared-library-loading-enabled? #t))
    (let ((port (open-input-file filename)))
      (define-inline (%next-datum)
	($get-annotated-datum port))
      (unwind-protect
	  (let read-next-datum ((obj (%next-datum)))
	    (if (eof-object? obj)
		'()
	      (cons obj (read-next-datum (%next-datum)))))
	(close-input-port port)))))

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
  (parameterize ((shared-library-loading-enabled? #t))
    (let* ((port	(open-file-input-port filename))
	   (sharp-bang?	(let-values (((octet1 octet2)
				      ;;If  an error  happens  here PORT
				      ;;will  be   closed  by  the  port
				      ;;guardian.
				      (lookahead-two-u8 port)))
			  (and (= octet1 CHAR-FIXNUM-SHARP)
			       (= octet2 CHAR-FIXNUM-BANG))))
	   (port	(transcoded-port port (native-transcoder))))
      (define-inline (%next-datum)
	($get-annotated-datum port))
      (unwind-protect
	  (begin
	    (when sharp-bang?
	      (read-and-discard-up-to-and-including-line-ending port))
	    (let read-next-datum ((obj (%next-datum)))
	      (if (eof-object? obj)
		  '()
		(cons obj (read-next-datum (%next-datum))))))
	(close-input-port port)))))


;;;; helpers for public functions

(define (read-expr port locations kont)
  (let-values (((token pos) (start-tokenising/pos port)))
    (finalise-tokenisation port locations kont token pos)))

(define (reduce-loc! port)
  ;;Subroutine of GET-DATUM and GET-ANNOTATED-DATUM.  Finalise the graph
  ;;notation locations.
  ;;
  ;;This computation  needs two  arguments: PORT and  an entry  from the
  ;;LOCATIONS which is the result  of reading an S-expression.  PORT is
  ;;fixed, so it  may be a little faster to make  this function return a
  ;;closure on PORT rather than to evaluate:
  ;;
  ;;   (for-each (lambda (entry)
  ;;               (reduce-loc! port entry))
  ;;     locations)
  ;;
  (lambda (entry)
    (define-syntax-rule (%error msg . irritants)
      (die/p port 'vicare-reader msg . irritants))
    (let ((loc (unsafe.cdr entry)))
      (unless (loc-set? loc)
	(die/lex (loc-textual-position loc) 'vicare-reader
		 "referenced location mark is not set" (unsafe.car entry)))
      (when (loc? (loc-value loc))
	(let loop ((h loc) (t loc))
	  (if (loc? h)
	      (let ((h1 (loc-value h)))
		(if (loc? h1)
		    (begin
		      (when (eq? h1 t)
			(%error "circular marks"))
		      (let ((v (loop (loc-value h1) (loc-value t))))
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
  (let ((locations	'())
	(kont		void))
    (read-expr port locations kont))
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
  (define-syntax-rule (%error msg . irritants)
    (die/p port 'tokenize msg . irritants))
  (let* ((pos (make-compound-position port))
	 (ch  (get-char-and-track-textual-position port)))
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
		  (ch1  (get-char-and-track-textual-position port)))
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
  ;;recurse calling  itself; if  the first character  is a  #\# delegate
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
  (define-syntax-rule (%error msg . irritants)
    (die/p port 'tokenize msg . irritants))
  (let ((ch (get-char-and-track-textual-position port)))
    (cond ((eof-object? ch)
	   ch)

	  ;;discard line comments
	  ((unsafe.char= ch #\;)
	   (read-and-discard-up-to-and-including-line-ending port)
	   (recurse))

	  ;;tokenise everything starting with a #
	  ((unsafe.char= ch #\#)
	   (let ((ch1 (get-char-and-track-textual-position port)))
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
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-syntax-rule (%error-1 msg . args)
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
		  (get-char-and-track-textual-position port)
		  '(macro . unquote-splicing))
		 (else
		  '(macro . unquote)))))

	;;number
	((dec-digit? ch)
	 (let ((d (unsafe.fx- (unsafe.char->fixnum ch) (unsafe.char->fixnum #\0))))
	   (cons 'datum (u:digit+ port (list ch) 10 #f #f +1 d))))

	;;symbol
	((initial? ch)
	 (finish-tokenisation-of-identifier (cons ch '()) port #t))

	;;string
	((unsafe.char= #\" ch)
	 (let ((ls (%accumulate-string-chars '() port)))
	   (cons 'datum (reverse-list->string ls))))

	;;symbol "+" or number
	((unsafe.char= #\+ ch)
	 (let ((ch1 (peek-char port)))
	   (cond ((eof-object? ch1)	'(datum . +))
		 ((delimiter?  ch1)	'(datum . +))

		 ;;This is to allow reading symbols:
		 ;;
		 ;;  +greek-pi
		 ;;  +greek-pi/2	+greek-pi*2
		 ;;
		 ;;and so on.
		 ((unsafe.char= ch1 #\g)
		  (if (port-in-r6rs-mode? port)
		      (%error "+g syntax is invalid in #!r6rs mode")
		    (begin
		      (get-char-and-track-textual-position port)
		      (finish-tokenisation-of-identifier '(#\g #\+) port #t))))

		 ((unsafe.char= #\+ ch1)
		  (if (port-in-r6rs-mode? port)
		      (%error "++ syntax is invalid in #!r6rs mode")
		    (begin
		      (get-char-and-track-textual-position port)
		      (let ((ch2 (peek-char port)))
			(if (or (eof-object? ch2)
				(delimiter?  ch2))
			    '(datum . |++|)
			  (%error "invalid syntax ++"))))))

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
		  (get-char-and-track-textual-position port)
		  (finish-tokenisation-of-identifier '(#\> #\-) port #t))

		 ;;This is to allow reading symbols:
		 ;;
		 ;;  -greek-pi
		 ;;  -greek-pi/2	-greek-pi*2
		 ;;
		 ;;and so on.
		 ((unsafe.char= ch1 #\g)
		  (if (port-in-r6rs-mode? port)
		      (%error "-g syntax is invalid in #!r6rs mode")
		    (begin
		      (get-char-and-track-textual-position port)
		      (finish-tokenisation-of-identifier '(#\g #\-) port #t))))

		 ((unsafe.char= ch1 #\-)
		  (if (port-in-r6rs-mode? port)
		      (%error "-- syntax is invalid in #!r6rs mode")
		    (begin
		      (get-char-and-track-textual-position port)
		      (let ((ch2 (peek-char port)))
			(if (or (eof-object? ch2)
				(delimiter?  ch2))
			    '(datum . |--|)
			  (%error "invalid syntax --"))))))

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
	 (finish-tokenisation-of-identifier/bar '() port #t))

	;;symbol whose first char is a backslash sequence, "\x41;-ciao"
	((unsafe.char= #\\ ch)
	 (finish-tokenisation-of-identifier/backslash '() port #t))

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
  ;;(datum . <key>)		The token is the keyword object <key>.
  ;;(datum . <num>)		The token is the number <num>.
  ;;(datum . #!eof)		The token is the "#!eof" comment.
  ;;(macro . syntax)		The token is a syntax form: #'---.
  ;;(macro . quasisyntax)	The token is a quasisyntax form: #`---.
  ;;(macro . unsyntax-splicing)	The token is an unsyntax-splicing form: #,@---.
  ;;(macro . unsyntax)		The token is an unsyntax form: #,---.
  ;;(mark . <n>)		The token is a graph syntax mark: #<N>=---
  ;;(ref . <n>)			The token is a graph syntax reference: #<N>#
  ;;vparen			The token is a vector.
  ;;comment-paren		The token is a comment list.
  ;;case-sensitive		The token is a case sensitive directive.
  ;;case-insensitive		The token is a case insensitive directive.
  ;;
  ;;When the token is a bytevector: the return value is the return value
  ;;of ADVANCE-TOKENISATION-OF-BYTEVECTORS.
  ;;
  ;;When the token is the  "#!r6rs" or "#!vicare" comment: the port mode
  ;;is changed accordingly and  START-TOKENISING is applied to the port;
  ;;the return value is the return value of START-TOKENISING.
  ;;
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-syntax-rule (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-syntax-rule (%unexpected-eof-error)
    (%error "invalid EOF while reading hash datum"))
  ;; (define-syntax-rule (%read-char-no-eof (?port ?ch-name) . ?cond-clauses)
  ;;   (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
  ;;     . ?cond-clauses))

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
	     (get-char-and-track-textual-position port)
	     '(macro . unsyntax-splicing))
	    (else
	     '(macro . unsyntax)))))

   ;; #! comments and such
   ((unsafe.char= #\! ch)
    (let ((ch1 (peek-char port)))
      (cond ((eof-object? ch1)
	     (%unexpected-eof-error))
	    ((unsafe.char= ch1 #\()
	     (read-char port)
	     (if (port-in-r6rs-mode? port)
		 (%error-1 "invalid syntax" "#!(")
	       'comment-paren))
	    (else
	     (let* ((token (finish-tokenisation-of-identifier '() port #t))
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
		  ;;If not  recognised, just handle it as  a comment and
		  ;;read the next datum.
		  (start-tokenising port))))))))

   ((dec-digit? ch)
    (if (port-in-r6rs-mode? port)
	(%error-1 "graph notation marks syntax is invalid in #!r6rs mode" (string #\# ch))
      (finish-tokenisation-of-graph-location port (char->dec-digit ch))))

   ((unsafe.char= #\: ch)
    (if (port-in-r6rs-mode? port)
	(%error-1 "keyword object syntax is invalid in #!r6rs mode" "#:")
      (let* ((ch1 (%read-char-skip-whitespace port "keyword object"))
	     (keyword-name
	      (if (initial? ch1)
		  (reverse-list->string (%accumulate-identifier-chars (cons ch1 '()) port))
		(%error-1 "invalid char inside keyword object" ch1))))
	(cons 'datum (symbol->keyword (string->symbol keyword-name))))))

;;;The original Ikarus code used the syntax:
;;;
;;;  #:pretty
;;;
;;;to  read a  gensym  with PRETTY  as  pretty string.   Such syntax  is
;;;currently used to read keyword objects.  It is currently not possible
;;;to read a  gensym by pretty string (because I was  unable to invent a
;;;cute syntax for them).  (Marco Maggi; Mon Mar 12, 2012)
;;;
;;;((unsafe.char= #\: ch)
;;; (if (port-in-r6rs-mode? port)
;;;     (%error-1 "gensym syntax is invalid in #!r6rs mode" (format "#~a" ch))
;;;   (let* ((ch1 (%read-char-skip-whitespace port "gensym"))
;;;          (pretty-name
;;;           (cond ((initial? ch1)
;;;                  (reverse-list->string (%accumulate-identifier-chars (cons ch1 '()) port)))
;;;                 ((unsafe.char= #\| ch1)
;;;                  (reverse-list->string (%accumulate-identifier-chars/bar '() port)))
;;;                 (else
;;;                  (%error-1 "invalid char inside gensym" ch1)))))
;;;     (cons 'datum (gensym pretty-name)))))

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
    (let ((ch1 (%read-char-skip-whitespace port "gensym")))
      (define-inline (%end-of-gensym? chX)
	(unsafe.char= #\} chX))
      (define-inline (%read-identifier chX)
	(cond ((initial? chX)
	       (reverse-list->string (%accumulate-identifier-chars (cons chX '()) port)))
	      ((unsafe.char= #\| chX)
	       (reverse-list->string (%accumulate-identifier-chars/bar '() port)))
	      (else
	       (%error-1 "invalid char inside gensym syntax" chX))))
      (let ((id0 (%read-identifier ch1))
	    (ch2 (%read-char-skip-whitespace port "gensym")))
	(if (%end-of-gensym? ch2)
	    ;;ID0 is the unique string.
	    `(datum . ,(%seed-strings->gensym #f id0))
	  (let* ((id1 (%read-identifier ch2))
		 (ch3 (%read-char-skip-whitespace port "gensym")))
	    (if (%end-of-gensym? ch3)
		;;ID0 is the pretty string, ID1 is the unique string.
		`(datum . ,(%seed-strings->gensym id0 id1))
	      (%error-1 "invalid char while looking for end of gensym syntax" ch3)))))))

   ;;bytevectors
   ((unsafe.char= #\v ch)
    (advance-tokenisation-of-bytevectors port))

   ;; #eNNNN -> exact integer number
   ((or (unsafe.char= ch #\e) (unsafe.char= ch #\E))
    (cons 'datum (parse-numeric-string port (list ch #\#) 10 #f 'e)))

   ;; #iNNNN -> inexact integer number
   ((or (unsafe.char= ch #\i) (unsafe.char= ch #\I))
    (cons 'datum (parse-numeric-string port (list ch #\#) 10 #f 'i)))

   ;; #bNNNN -> exact integer number in binary base
   ((or (unsafe.char= ch #\b) (unsafe.char= ch #\B))
    (cons 'datum (parse-numeric-string port (list ch #\#) 2 2 #f)))

   ;; #xNNNN -> exact integer number in hex base
   ((or (unsafe.char= ch #\x) (unsafe.char= ch #\X))
    (cons 'datum (parse-numeric-string port (list ch #\#) 16 16 #f)))

   ;; #oNNNN -> exact integer number in octal base
   ((or (unsafe.char= ch #\o) (unsafe.char= ch #\O))
    (cons 'datum (parse-numeric-string port (list ch #\#) 8 8 #f)))

   ;; #dNNNN -> exact integer number in decimal base
   ((or (unsafe.char= ch #\d) (unsafe.char= ch #\D))
    (cons 'datum (parse-numeric-string port (list ch #\#) 10 10 #f)))

   ((unsafe.char= ch #\c)
    (let ((ch1 (get-char-and-track-textual-position port)))
      (cond ((eof-object? ch1)
	     (%unexpected-eof-error))
	    ((unsafe.char= ch1 #\i)
	     'case-insensitive)
	    ((unsafe.char= ch1 #\s)
	     'case-sensitive)
	    (else
	     (%error-1 "invalid syntax" (string #\# #\c ch1))))))

;;;((unsafe.char= #\@ ch) DEAD: Unfixable due to port encoding
;;;                 that does not allow mixing binary and
;;;                 textual data in the same port.
;;;                Left here for historical value
;;; (when (port-in-r6rs-mode? port)
;;;   (%error-1 "fasl syntax is invalid in #!r6rs mode"
;;;      (format "#~a" ch)))
;;; (die/p-1 port 'vicare-reader "FIXME: fasl read disabled")
;;; '(cons 'datum ($fasl-read port)))

   (else
    (%error-1 (format "invalid syntax #~a" ch)))))


(define (advance-tokenisation-of-bytevectors port)
  ;;Read from  PORT the opening tag  of bytevectors up  to and including
  ;;the  opening parentheses,  after  the #\#  and  #\v characters  have
  ;;already been read.  Return one among:
  ;;
  ;;vu8				The token is a u8 bytevector.
  ;;vs8				The token is a s8 bytevector.
  ;;
  ;;vu16l			The token is a u16l bytevector.
  ;;vs16b			The token is a u16b bytevector.
  ;;vu16n			The token is a u16n bytevector.
  ;;
  ;;vs16l			The token is a s16l bytevector.
  ;;vs16b			The token is a s16b bytevector.
  ;;vs16n			The token is a s16n bytevector.
  ;;
  ;;vu32l			The token is a u32l bytevector.
  ;;vs32b			The token is a u32b bytevector.
  ;;vu32n			The token is a u32n bytevector.
  ;;
  ;;vs32l			The token is a s32l bytevector.
  ;;vs32b			The token is a s32b bytevector.
  ;;vs32n			The token is a s32n bytevector.
  ;;
  ;;vu64l			The token is a u64l bytevector.
  ;;vs64b			The token is a u64b bytevector.
  ;;vu64n			The token is a u64n bytevector.
  ;;
  ;;vs64l			The token is a s64l bytevector.
  ;;vs64b			The token is a s64b bytevector.
  ;;vs64n			The token is a s64n bytevector.
  ;;
  ;;vf4l			The token is a f4l bytevector.
  ;;vf4b			The token is a f4b bytevector.
  ;;vf4n			The token is a f4n bytevector.
  ;;
  ;;vf8l			The token is a f8l bytevector.
  ;;vf8b			The token is a f8b bytevector.
  ;;vf8n			The token is a f8n bytevector.
  ;;
  ;;vc4l			The token is a f4l bytevector.
  ;;vc4b			The token is a f4b bytevector.
  ;;vc4n			The token is a f4n bytevector.
  ;;
  ;;vc8l			The token is a c8l bytevector.
  ;;vc8b			The token is a c8b bytevector.
  ;;vc8n			The token is a c8n bytevector.
  ;;
  ;;ve				The toekn is a ve bytevector.
  ;;
  ;;Correct sequences of chars:
  ;;
  ;; ch  ch1  ch2  ch3  ch4  ch5  datum
  ;; ----------------------------------
  ;; v   u    8    (              #vu8
  ;; v   s    8    (              #vs8
  ;;
  ;; v   u    1    6    l    (    #vu16l
  ;; v   u    1    6    b    (    #vu16b
  ;; v   u    1    6    n    (    #vu16n
  ;;
  ;; v   s    1    6    l    (    #vs16l
  ;; v   s    1    6    b    (    #vs16b
  ;; v   s    1    6    b    (    #vs16n
  ;;
  ;; v   u    3    2    l    (    #vu32l
  ;; v   u    3    2    b    (    #vu32b
  ;; v   u    3    2    n    (    #vu32n
  ;;
  ;; v   s    3    2    l    (    #vs32l
  ;; v   s    3    2    b    (    #vs32b
  ;; v   s    3    2    b    (    #vs32n
  ;;
  ;; v   u    6    4    l    (    #vu64l
  ;; v   u    6    4    b    (    #vu64b
  ;; v   u    6    4    n    (    #vu64n
  ;;
  ;; v   s    6    4    l    (    #vs64l
  ;; v   s    6    4    b    (    #vs64b
  ;; v   s    6    4    n    (    #vs64n
  ;;
  ;; v   f    4    l    (         #vf4l
  ;; v   f    4    b    (         #vf4b
  ;; v   f    4    n    (         #vf4n
  ;;
  ;; v   f    8    l    (         #vf8l
  ;; v   f    8    b    (         #vf8b
  ;; v   f    8    n    (         #vf8n
  ;;
  ;; v   e    (                   #ve
  ;;
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-syntax-rule (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading hash datum"))
  (define-syntax %read-char-no-eof
    (syntax-rules ()
      ((_ (?port ?ch-name) . ?cond-clauses)
       (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
	 . ?cond-clauses))))

  (define-syntax-rule (%invalid-sequence-of-chars   . chars)
    (%error   "invalid sequence of characters" (string . chars)))
  (define-syntax-rule (%invalid-sequence-of-chars-1 . chars)
    (%error-1 "invalid sequence of characters" (string . chars)))

;;; --------------------------------------------------------------------

  (define-inline (%read-second-tag-char)
    (%read-char-no-eof (port ch1)
      ((unsafe.char= #\u ch1)
       (%read-unsigned))
      ((unsafe.char= #\s ch1)
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #vs syntax in #!r6rs mode" "#vs"))
       (%read-signed))
      ((unsafe.char= #\f ch1)
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #vf syntax in #!r6rs mode" "#vf"))
       (%read-flonum))
      ((unsafe.char= #\c ch1)
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #vc syntax in #!r6rs mode" "#vc"))
       (%read-cflonum))
      ((unsafe.char= #\e ch1)
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #ve syntax in #!r6rs mode" "#ve"))
       (%read-encoded))
      (else
       (%invalid-sequence-of-chars #\# #\v ch1))))

  (define-syntax-rule (%read-open-paren token . chars)
    (%read-char-no-eof (port ch)
      ((unsafe.char= ch #\()
       token)
      (else
       (%invalid-sequence-of-chars-1 . chars))))

;;; --------------------------------------------------------------------

  (define-inline (%read-unsigned)
    (%read-char-no-eof (port ch2)
      ((unsafe.char= ch2 #\8) ;unsigned 8
       (%read-open-paren 'vu8 #\# #\v #\u #\8))

      ((unsafe.char= ch2 #\1) ;unsigned 16
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #vu1 syntax in #!r6rs mode" "#vu1"))
       (%read-char-no-eof (port ch3)
	 ((unsafe.char= #\6 ch3)
	  (%read-unsigned-16))
	 (else
	  (%invalid-sequence-of-chars-1 #\# #\v #\u #\1 ch3))))

      ((unsafe.char= ch2 #\3) ;unsigned 32
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #vu3 syntax in #!r6rs mode" "#vu3"))
       (%read-char-no-eof (port ch3)
	 ((unsafe.char= #\2 ch3)
	  (%read-unsigned-32))
	 (else
	  (%invalid-sequence-of-chars-1 #\# #\v #\u #\3 ch3))))

      ((unsafe.char= ch2 #\6) ;unsigned 64
       (when (port-in-r6rs-mode? port)
	 (%error "invalid #vu6 syntax in #!r6rs mode" "#vu6"))
       (%read-char-no-eof (port ch3)
	 ((unsafe.char= #\4 ch3)
	  (%read-unsigned-64))
	 (else
	  (%invalid-sequence-of-chars-1 #\# #\v #\u #\6 ch3))))

      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\u ch2))))

  (define-inline (%read-signed)
    (%read-char-no-eof (port ch2)
      ((unsafe.char= ch2 #\8) ;signed bytes
       (%read-open-paren 'vs8 #\# #\v #\s #\8))

      ((unsafe.char= ch2 #\1) ;signed 16
       (%read-char-no-eof (port ch3)
	 ((unsafe.char= #\6 ch3)
	  (%read-signed-16))
	 (else
	  (%invalid-sequence-of-chars-1 #\# #\v #\s #\1 ch3))))

      ((unsafe.char= ch2 #\3) ;signed 32
       (%read-char-no-eof (port ch3)
	 ((unsafe.char= #\2 ch3)
	  (%read-signed-32))
	 (else
	  (%invalid-sequence-of-chars-1 #\# #\v #\s #\3 ch3))))

      ((unsafe.char= ch2 #\6) ;signed 64
       (%read-char-no-eof (port ch3)
	 ((unsafe.char= #\4 ch3)
	  (%read-signed-64))
	 (else
	  (%invalid-sequence-of-chars-1 #\# #\v #\s #\6 ch3))))

      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\s ch2))))

;;; --------------------------------------------------------------------

  (define-inline (%read-unsigned-16)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vu16l #\# #\v #\u #\1 #\6 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vu16b #\# #\v #\u #\1 #\6 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vu16n #\# #\v #\u #\1 #\6 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\u #\1 #\6 ch4))))

  (define-inline (%read-signed-16)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vs16l #\# #\v #\s #\1 #\6 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vs16b #\# #\v #\s #\1 #\6 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vs16n #\# #\v #\s #\1 #\6 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\s #\1 #\6 ch4))))

;;; --------------------------------------------------------------------

  (define-inline (%read-unsigned-32)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vu32l #\# #\v #\u #\3 #\2 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vu32b #\# #\v #\u #\3 #\2 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vu32n #\# #\v #\u #\3 #\2 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\u #\3 #\2 ch4))))

  (define-inline (%read-signed-32)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vs32l #\# #\v #\s #\3 #\2 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vs32b #\# #\v #\s #\3 #\2 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vs32n #\# #\v #\s #\3 #\2 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\s #\3 #\2 ch4))))

;;; --------------------------------------------------------------------

  (define-inline (%read-unsigned-64)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vu64l #\# #\v #\u #\6 #\4 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vu64b #\# #\v #\u #\6 #\4 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vu64n #\# #\v #\u #\6 #\4 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\u #\6 #\4 ch4))))

  (define-inline (%read-signed-64)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vs64l #\# #\v #\s #\6 #\4 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vs64b #\# #\v #\s #\6 #\4 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vs64n #\# #\v #\s #\6 #\4 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\s #\6 #\4 ch4))))

;;; --------------------------------------------------------------------

  (define-inline (%read-flonum)
    (%read-char-no-eof (port ch2)
      ((unsafe.char= ch2 #\4)	;single precision flonums
       (%read-flonum-single-precision))

      ((unsafe.char= ch2 #\8)	;double precision flonums
       (%read-flonum-double-precision))

      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\f ch2))))

  (define-inline (%read-flonum-single-precision)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vf4l #\# #\v #\f #\4 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vf4b #\# #\v #\f #\4 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vf4n #\# #\v #\f #\4 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\f #\4 ch4))))

  (define-inline (%read-flonum-double-precision)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vf8l #\# #\v #\f #\8 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vf8b #\# #\v #\f #\8 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vf8n #\# #\v #\f #\8 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\f #\8 ch4))))

;;; --------------------------------------------------------------------

  (define-inline (%read-cflonum)
    (%read-char-no-eof (port ch2)
      ((unsafe.char= ch2 #\4)	;single precision flonums
       (%read-cflonum-single-precision))

      ((unsafe.char= ch2 #\8)	;double precision flonums
       (%read-cflonum-double-precision))

      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\c ch2))))

  (define-inline (%read-cflonum-single-precision)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vc4l #\# #\v #\c #\4 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vc4b #\# #\v #\c #\4 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vc4n #\# #\v #\c #\4 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\c #\4 ch4))))

  (define-inline (%read-cflonum-double-precision)
    (%read-char-no-eof (port ch4)
      ((unsafe.char= ch4 #\l)
       (%read-open-paren 'vc8l #\# #\v #\c #\8 #\l))
      ((unsafe.char= ch4 #\b)
       (%read-open-paren 'vc8b #\# #\v #\c #\8 #\b))
      ((unsafe.char= ch4 #\n)
       (%read-open-paren 'vc8n #\# #\v #\c #\8 #\n))
      (else
       (%invalid-sequence-of-chars-1 #\# #\v #\c #\8 ch4))))

;;; --------------------------------------------------------------------

  (define-inline (%read-encoded)
    (%read-open-paren 've #\# #\v #\e))

;;; --------------------------------------------------------------------

  (%read-second-tag-char))


(define (finish-tokenisation-of-dot-datum port)
  ;;Read from  PORT a token starting  with a dot, the  dot being already
  ;;read.  There return value is a datum describing the token:
  ;;
  ;;dot			The token is a standalone dot.
  ;;(datum . ...)	The token is the ellipsis symbol.
  ;;(datum . <num>)	The token is the inexact number <NUM>.
  ;;
  (define-syntax-rule (%error msg . args)
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
	   (get-char-and-track-textual-position port)
	   (let ((ch1 (get-char-and-track-textual-position port)))
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
  ;;Recursive  function.   Read characters  from  PORT  parsing a  graph
  ;;notation hash num mark or  reference after the opening #\# character
  ;;and  the first  digit have  been already  consumed.  Return  a datum
  ;;describing the token:
  ;;
  ;;N is  an exact integer representing the  location number accumulated
  ;;so far.
  ;;
  ;;(mark . <num>)	The token is a new hashnum mark.
  ;;(ref . <num>)	The token is reference to an existing hashnum.
  ;;
  (define-inline (recurse N1)
    (finish-tokenisation-of-graph-location port N1))
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading character"))
  (define-syntax %read-char-no-eof
    (syntax-rules ()
      ((_ (?port ?ch-name) . ?cond-clauses)
       (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
	 . ?cond-clauses))))

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

(define (finish-tokenisation-of-identifier accumulated-chars port honour-sensitivity?)
  ;;To be called when one or more characters starting an identifier have
  ;;been read from PORT and  we must finish the identifier tokenisation.
  ;;Read the remaining characters and return:
  ;;
  ;;  (datum . <sym>)
  ;;
  ;;where <SYM> is the tokenised symbol.
  ;;
  ;;If HONOUR-SENSITIVITY?  is true honour the  current case sensititivy
  ;;setting.
  ;;
  (let* ((str (reverse-list->string (%accumulate-identifier-chars accumulated-chars port)))
	 (sym (string->symbol (if (and honour-sensitivity? (case-insensitive?))
				  (string-foldcase str)
				str))))
    `(datum . ,sym)))

(define (finish-tokenisation-of-identifier/bar accumulated-chars port honour-sensitivity?)
  ;;To be called  when one or more characters  starting an identifier in
  ;;bar  syntax  have  been  read  from  PORT and  we  must  finish  the
  ;;identifier tokenisation.  Read the remaining characters and return:
  ;;
  ;;  (datum . <sym>)
  ;;
  ;;where <SYM> is the tokenised symbol.
  ;;
  (let* ((str (reverse-list->string (%accumulate-identifier-chars/bar accumulated-chars port)))
	 (sym (string->symbol (if (and honour-sensitivity? (case-insensitive?))
				  (string-foldcase str)
				str))))
    `(datum . ,sym)))

(define (finish-tokenisation-of-identifier/backslash accumulated-chars port honour-sensitivity?)
  ;;To be called  when a backslash character starting  an identifier has
  ;;been read from PORT and  we must finish the identifier tokenisation.
  ;;Read the remaining characters and return:
  ;;
  ;;  (datum . <sym>)
  ;;
  ;;where <SYM> is the tokenised symbol.
  ;;
  (let* ((str (reverse-list->string (%accumulate-identifier-chars/backslash '() port #f)))
	 (sym (string->symbol (if (and honour-sensitivity? (case-insensitive?))
				  (string-foldcase str)
				str))))
    `(datum . ,sym)))

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
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (recurse accum)
    (%accumulate-identifier-chars accum port))
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch)
	   accumulated-chars)
	  ((subsequent? ch)
	   (get-char-and-track-textual-position port)
	   (recurse (cons ch accumulated-chars)))
	  ((delimiter? ch)
	   accumulated-chars)
	  ((unsafe.char= ch #\\)
	   (get-char-and-track-textual-position port)
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
  (define-syntax-rule (%unexpected-eof-error . args)
    (die/p port 'tokenize "unexpected EOF while reading symbol" . args))
  (define-inline (recurse accum)
    (%accumulate-identifier-chars/bar accum port))
  (define-syntax %read-char-no-eof
    (syntax-rules ()
      ((_ (?port ?ch-name) . ?cond-clauses)
       (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
	 . ?cond-clauses))))

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
  (define-syntax-rule (%error msg . args)
    (die/p port   'tokenize msg . args))
  (define-syntax-rule (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-syntax-rule (%unexpected-eof-error . args)
    (%error "unexpected EOF while reading symbol" . args))
  (define-syntax %read-char-no-eof
    (syntax-rules ()
      ((_ (?port ?ch-name) . ?cond-clauses)
       (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
	 . ?cond-clauses))))

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


(define (finalise-tokenisation port locations kont token pos)
  (define-syntax-rule (%error   msg . irritants)
    (die/p   port 'vicare-reader msg . irritants))
  (define-syntax-rule (%error-1 msg . irritants)
    (die/p-1 port 'vicare-reader msg . irritants))

  (define-inline (main)
    (cond ((eof-object? token)
	   (values (eof-object)
		   (annotate-simple (eof-object) pos) locations kont))

	  ;;Read list that was opened by a round parenthesis.
	  ((eq? token 'lparen)
	   (let-values (((ls ls/ann locations kont)
			 (finish-tokenisation-of-list port pos locations kont 'rparen 'rbrack)))
	     (values ls (annotate ls ls/ann pos) locations kont)))

	  ;;Read list that was opened by a square bracket.
	  ((eq? token 'lbrack)
	   (let-values (((ls ls/ann locations kont)
			 (finish-tokenisation-of-list port pos locations kont 'rbrack 'rparen)))
	     (values ls (annotate ls ls/ann pos) locations kont)))

	  ;;Read a vector opened by "#(".
	  ((eq? token 'vparen)
	   (let-values (((vec vec/ann locations kont)
			 (finish-tokenisation-of-vector port locations kont 0 '() '())))
	     (values vec (annotate vec vec/ann pos) locations kont)))

	  ;;Read a bytevector.
	  ((memq token '( ;;
			 vu8   vs8
			 vu16l vu16b vu16n  vs16l vs16b vs16n
			 vu32l vu32b vu32n  vs32l vs32b vs32n
			 vu64l vu64b vu64n  vs64l vs64b vs64n
			 vf4l  vf4b  vf4n   vf8l  vf8b  vf8n
			 vc4l  vc4b  vc4n   vc8l  vc8b  vc8n
			 ve))
	   (let-values
	       (((bv bv/ann locations kont)
		 (cond ((eq? token 'vu8)
			(finish-tokenisation-of-bytevector-u8 port locations kont 0 '()))
		       ((eq? token 'vs8)
			(finish-tokenisation-of-bytevector-s8 port locations kont 0 '()))

		       ((eq? token 'vu16l)
			(finish-tokenisation-of-bytevector-u16l port locations kont 0 '()))
		       ((eq? token 'vs16l)
			(finish-tokenisation-of-bytevector-s16l port locations kont 0 '()))

		       ((eq? token 'vu16b)
			(finish-tokenisation-of-bytevector-u16b port locations kont 0 '()))
		       ((eq? token 'vs16b)
			(finish-tokenisation-of-bytevector-s16b port locations kont 0 '()))

		       ((eq? token 'vu16n)
			(finish-tokenisation-of-bytevector-u16n port locations kont 0 '()))
		       ((eq? token 'vs16n)
			(finish-tokenisation-of-bytevector-s16n port locations kont 0 '()))

		       ((eq? token 'vu32l)
			(finish-tokenisation-of-bytevector-u32l port locations kont 0 '()))
		       ((eq? token 'vs32l)
			(finish-tokenisation-of-bytevector-s32l port locations kont 0 '()))

		       ((eq? token 'vu32b)
			(finish-tokenisation-of-bytevector-u32b port locations kont 0 '()))
		       ((eq? token 'vs32b)
			(finish-tokenisation-of-bytevector-s32b port locations kont 0 '()))

		       ((eq? token 'vu32n)
			(finish-tokenisation-of-bytevector-u32n port locations kont 0 '()))
		       ((eq? token 'vs32n)
			(finish-tokenisation-of-bytevector-s32n port locations kont 0 '()))

		       ((eq? token 'vu64l)
			(finish-tokenisation-of-bytevector-u64l port locations kont 0 '()))
		       ((eq? token 'vs64l)
			(finish-tokenisation-of-bytevector-s64l port locations kont 0 '()))

		       ((eq? token 'vu64b)
			(finish-tokenisation-of-bytevector-u64b port locations kont 0 '()))
		       ((eq? token 'vs64b)
			(finish-tokenisation-of-bytevector-s64b port locations kont 0 '()))

		       ((eq? token 'vu64n)
			(finish-tokenisation-of-bytevector-u64n port locations kont 0 '()))
		       ((eq? token 'vs64n)
			(finish-tokenisation-of-bytevector-s64n port locations kont 0 '()))

		       ((eq? token 'vf4l)
			(finish-tokenisation-of-bytevector-f4l port locations kont 0 '()))
		       ((eq? token 'vf4b)
			(finish-tokenisation-of-bytevector-f4b port locations kont 0 '()))
		       ((eq? token 'vf4n)
			(finish-tokenisation-of-bytevector-f4n port locations kont 0 '()))

		       ((eq? token 'vf8l)
			(finish-tokenisation-of-bytevector-f8l port locations kont 0 '()))
		       ((eq? token 'vf8b)
			(finish-tokenisation-of-bytevector-f8b port locations kont 0 '()))
		       ((eq? token 'vf8n)
			(finish-tokenisation-of-bytevector-f8n port locations kont 0 '()))

		       ((eq? token 'vc4l)
			(finish-tokenisation-of-bytevector-c4l port locations kont 0 '()))
		       ((eq? token 'vc4b)
			(finish-tokenisation-of-bytevector-c4b port locations kont 0 '()))
		       ((eq? token 'vc4n)
			(finish-tokenisation-of-bytevector-c4n port locations kont 0 '()))

		       ((eq? token 'vc8l)
			(finish-tokenisation-of-bytevector-c8l port locations kont 0 '()))
		       ((eq? token 'vc8b)
			(finish-tokenisation-of-bytevector-c8b port locations kont 0 '()))
		       ((eq? token 'vc8n)
			(finish-tokenisation-of-bytevector-c8n port locations kont 0 '()))
		       ((eq? token 've)
			(finish-tokenisation-of-bytevector-ve  port locations kont))
		       )))
	     (values bv (annotate bv bv/ann pos) locations kont)))

	  ;;Read a comment list.
	  ((eq? token 'comment-paren)
	   (let-values (((ls ls/ann locations kont)
			 (finish-tokenisation-of-list port pos locations kont 'rparen 'rbrack)))
	     (%process-comment-list port ls)
	     ;;Go on with the next token.
	     (let-values (((token pos) (start-tokenising/pos port)))
	       (finalise-tokenisation port locations kont token pos))))

	  ((eq? token 'case-sensitive)
	   (let-values (((expr expr/ann locations kont)
			 (parametrise ((case-insensitive? #f))
			   (read-expr port locations kont))))
	     (values expr expr/ann locations kont)))

	  ((eq? token 'case-insensitive)
	   (let-values (((expr expr/ann locations kont)
			 (parametrise ((case-insensitive? #t))
			   (read-expr port locations kont))))
	     (values expr expr/ann locations kont)))

	  ((pair? token)
	   (%process-pair-token token))

	  (else
	   (%error-1 (format "unexpected ~s found" token)))))

  (define-inline (%process-pair-token token)
    (let ((class (unsafe.car token)))
      (cond ((eq? class 'datum) ;datum already tokenised
	     (let ((X (unsafe.cdr token)))
	       (values X (annotate-simple X pos) locations kont)))

	    ;;Read  a sexp  quoted  with one  among: QUOTE,  QUASIQUOTE,
	    ;;UNQUOTE, UNQUOTE-SPLICING,  SYNTAX, QUASISYNTAX, UNSYNTAX,
	    ;;UNSYNTAX-SPLICING.
	    ;;
	    ((eq? class 'macro)
	     (let ((quoting-keyword (unsafe.cdr token)))
	       (define (%read-quoted-sexp)
		 (let-values (((token1 pos) (start-tokenising/pos port)))
		   (if (eof-object? token1)
		       (%error (string-append "invalid EOF after "
					      (symbol->string quoting-keyword)
					      " read macro" ))
		     (finalise-tokenisation port locations kont token1 pos))))
	       (let-values (((expr expr/ann locations kont) (%read-quoted-sexp)))
		 (let ((d     (list expr))
		       (d/ann (list expr/ann)))
		   (let ((x     (cons quoting-keyword d))
			 (x/ann (cons (annotate-simple quoting-keyword pos) d/ann)))
		     (values x (annotate x x/ann pos) locations
			     (extend-graph-notation-kont-for-pair d d/ann expr '() kont)))))))

	    ;;Read  an   expression  marked  with   graph  notation  for
	    ;;locations; whatever we do  either we return the expression
	    ;;or raise an exception.
	    ;;
	    ;;If an  entry with  the same digit  N is not  in LOCATIONS:
	    ;;this mark  is new; create  a new LOC structure,  marked as
	    ;;set  and holding  the expression,  and register  it  in in
	    ;;LOCATIONS.  Return the expression.
	    ;;
	    ;;If an entry with the same digit N is already in LOCATIONS,
	    ;;and marked  set: it  means a mark  "#N=" has  already been
	    ;;read, so raise an exception.
	    ;;
	    ;;If an entry with the same digit N is already in LOCATIONS,
	    ;;but  marked unset: it  means that  one or  more references
	    ;;"#N#" have been already processed; mutate the existing LOC
	    ;;structure to reference the  expression and mark it as set.
	    ;;References will be processed later by REDUCE-LOC!.  Return
	    ;;the expression.
	    ;;
	    ;;FIXME Would  it be intelligent  to raise an  exception, in
	    ;;case of multiple  reading of the same mark,  only when the
	    ;;expressions  differ?   Would  checking  equality  of  such
	    ;;expressions generate infinite loops?
	    ;;
	    ;;Examples:
	    ;;
	    ;;  #N=123
	    ;;  #N=ciao
	    ;;  #N=(1 2 3)
	    ;;  #N=#(1 2 3)
	    ;;  #N=#vu8(1 2 3)
	    ;;
	    ((eq? class 'mark)
	     (let ((N (unsafe.cdr token)))
	       (let-values (((expr expr/ann locations kont)
			     (read-expr port locations kont)))
		 (cond ((assq N locations)
			=> (lambda (pair)
			     (let ((loc (unsafe.cdr pair)))
			       (when (loc-set? loc)
				 (die/lex (condition (loc-textual-position loc) pos)
					  'vicare-reader "duplicate location mark for graph notation" N))
			       (set-loc-value!     loc expr)
			       (set-loc-value/ann! loc expr/ann)
			       (set-loc-set?!      loc #t)
			       (values expr expr/ann locations kont))))
		       (else
			(let* ((loc         (let ((value     expr)
						  (value/ann 'unused)
						  (set?      #t))
					      (make-loc value value/ann set? pos)))
			       (locations1 (cons (cons N loc) locations)))
			  (values expr expr/ann locations1 kont)))))))

	    ;;Process reference to graph notation location; we return an
	    ;;expression or a LOC structure.
	    ;;
	    ;;If  an entry with  the same  digit N  is in  LOCATIONS: it
	    ;;means that  either the  associated mark "#N="  has already
	    ;;been read  or another reference  with digit N  has already
	    ;;been processed; in any  case extract the LOC structure and
	    ;;return  it   so  that  it   can  be  later   processed  by
	    ;;REDUCE-LOC!.
	    ;;
	    ;;If an  entry with  digit N is  not in LOCATIONS:  it means
	    ;;that neither  the associated mark "#N=" has  been read nor
	    ;;another reference with digit  N has been processed; in any
	    ;;case create a new LOC structure, marked unset, register it
	    ;;in  LOCATIONS  and return  it  so  that  it can  be  later
	    ;;processed by REDUCE-LOC!.
	    ;;
	    ((eq? class 'ref)
	     (let ((N (unsafe.cdr token)))
	       (cond ((assq N locations)
		      => (lambda (pair)
			   (values (unsafe.cdr pair) 'unused locations kont)))
		     (else
		      (let* ((the-loc     (let ((value     #f)
						(value/ann 'unused)
						(set?      #f))
					    (make-loc value value/ann set? pos)))
			     (locations1 (cons (cons N the-loc) locations)))
			(values the-loc 'unused locations1 kont))))))

	    (else
	     (%error "Vicare internal error: unknown token from reader functions" token)))))

  (main))


(define (%accumulate-string-chars ls port)
  ;;Read  from PORT  characters from  the  internals of  a string  token
  ;;(after the opening double quote  has been read), accumulate them, in
  ;;reverse order and return the resulting list.
  ;;
  (define-inline (recurse accum)
    (%accumulate-string-chars accum port))
  (define-syntax-rule (%error msg . args)
    (die/p   port 'tokenize msg . args))
  (define-syntax-rule (%error-1 msg . args)
    (die/p-1 port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading string"))

  (define-syntax %read-char-no-eof
    (syntax-rules ()
      ((_ (?port ?ch-name) . ?cond-clauses)
       (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
	 . ?cond-clauses))))

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

      ;;inline named char "\{name}"
      ((unsafe.char= #\{ ch)
       (when (port-in-r6rs-mode? port)
	 (%error "invalid custom named character syntax in R6RS mode"))
       (recurse (cons (%parse-escape-named-char) ls)))

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
	    (%discard-trailing-intraline-whitespace ls port (get-char-and-track-textual-position port)))
	   ((char-is-carriage-return? chX)
	    (%read-char-no-eof (port chY)
	      ((char-is-newline-after-carriage-return? chY)
	       (%discard-trailing-intraline-whitespace ls port (get-char-and-track-textual-position port)))
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
       (%discard-trailing-intraline-whitespace ls port (get-char-and-track-textual-position port)))

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
	  (%discard-trailing-intraline-whitespace ls port (get-char-and-track-textual-position port)))
	 (else
	  (%discard-trailing-intraline-whitespace ls port ch1))))

      (else
       (%error-1 "invalid escape sequence while reading string" ch))))

  (define-inline (%parse-escape-hex-sequence ch first-digit)
    ;;Read from  PORT characters composing  an escaped character  in hex
    ;;format "\xHHHH;" and recurse using the resulting character.
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

  (define-inline (%parse-escape-named-char)
    ;;Read  from PORT  characters  composing a  custom named  character:
    ;;"\{name}" and return the resulting character
    ;;
    (let ((token (finish-tokenisation-of-identifier '() port #f)))
      (%read-char-no-eof (port chX)
	((unsafe.char= chX #\})
	 (let* ((name (cdr token))
		(ch   (hashtable-ref (custom-named-chars) name #f)))
	   (or ch (%error "unknown named char in escape sequence while reading string" name))))
	(else
	 (%error-1 "invalid char in escape sequence while reading string")))))

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
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (%unexpected-eof-error)
    (%error "invalid EOF while reading character"))
  (define-syntax %read-char-no-eof
    (syntax-rules ()
      ((_ (?port ?ch-name) . ?cond-clauses)
       (read-char-no-eof (?port ?ch-name %unexpected-eof-error)
	 . ?cond-clauses))))

  (define-inline (main)
    (%read-char-no-eof (port ch)
      ;;There are multiple character sequences starting with "#\n".
      ((unsafe.char= #\n ch)
       (let ((ch1 (peek-char port)))
	 (cond ((eof-object? ch1)
		'(datum . #\n))
	       ((unsafe.char= #\u ch1)
		(get-char-and-track-textual-position port)
		(%finish-reading-character-name port "ul" '(datum . #\x0)))
	       ((unsafe.char= #\e ch1)
		(get-char-and-track-textual-position port)
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
		     (get-char-and-track-textual-position port)
		     (let next-digit ((digit       digit)
				      (accumulated (cons ch1 '(#\x #\\))))
		       (let ((chX (peek-char port)))
			 (cond ((or (eof-object? chX)
				    (delimiter? chX))
				(cons 'datum (fixnum->char/checked digit accumulated port)))
			       ((char->hex-digit/or-false chX)
				=> (lambda (digit0)
				     (get-char-and-track-textual-position port)
				     (next-digit (+ (* digit 16) digit0)
						 (cons chX accumulated))))
			       (else
				(%error "invalid character sequence"
					(reverse-list->string (cons chX accumulated)))))))))
	       (else
		(%error "invalid character sequence" (string #\# #\\ ch1))))))

      ;;Read the char "#\{" or a custom named character "#\{name}".
      ((unsafe.char= #\{ ch)
       (let ((ch1 (peek-char port)))
	 (cond ((or (eof-object? ch1)
		    (delimiter? ch1))
		'(datum . #\{))
	       (else
		(when (port-in-r6rs-mode? port)
		  (%error "invalid custom named character syntax in R6RS mode"))
		(let ((token (finish-tokenisation-of-identifier '() port #f)))
		  (%read-char-no-eof (port chX)
		    ((unsafe.char= chX #\})
		     (let* ((name (cdr token))
			    (ch   (hashtable-ref (custom-named-chars) name #f)))
		       (cons 'datum (or ch (%error "unknown custom named character" name)))))
		    (else
		     (%error "invalid syntax in standalone custom named character"))))))))

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
    (define-syntax-rule (%error msg . args)
      (die/p port 'tokenize msg . args))
    (let ((ch (peek-char port)))
      (cond ((or (eof-object? ch)
		 (delimiter? ch))
	     (cons 'datum (unsafe.string-ref str 0)))
	    ((unsafe.char= ch (unsafe.string-ref str 1))
	     (get-char-and-track-textual-position port)
	     (let loop ((str.index 2))
		 (if (unsafe.fx= str.index (unsafe.string-length str))
		     (let ((ch (peek-char port)))
		       (cond ((eof-object? ch) datum)
			     ((delimiter?  ch) datum)
			     (else
			      (%error "invalid character after expected sequence"
				      (string-append str (string ch))))))
		   (let ((ch (get-char-and-track-textual-position port)))
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

(module (parse-numeric-string u:digit+ u:sign u:dot)
  (import
      (only (vicare parser-logic)
	    :introduce-device-arguments
	    :generate-end-of-input-or-char-tests
	    :generate-delimiter-test
	    :unexpected-end-of-input
	    :invalid-input-char)
    (only (ikarus.string-to-number)
	  define-string->number-parser))

  (define-syntax port-logic
    ;;Define the device logic to  parse a numeric lexeme embedded in the
    ;;input from a Scheme textual input port.
    ;;
    (syntax-rules (:introduce-device-arguments
		   :generate-end-of-input-or-char-tests
		   :generate-delimiter-test
		   :unexpected-end-of-input
		   :invalid-input-char)

      ;;Introduce  a   list  of  identifiers   used  as  device-specific
      ;;arguments;  they will  be the  first arguments  for  each parser
      ;;operator function.
      ;;
      ((_ :introduce-device-arguments ?kont . ?rest)
       (?kont (port accumulated-chars) . ?rest))

      ;;Whenever  an input  character  is not  accepted  by an  operator
      ;;function  this rule is  used to  decide what  to do.   For input
      ;;ports the action is to raise an exception.
      ((_ :invalid-input-char (?port ?accumulated-chars) ?ch)
       (%error-invalid-sequence ?port (cons ?ch ?accumulated-chars)))

      ;;Whenever the end-of-input is found  in a position in which it is
      ;;unexpected, this rule  is used to decide what  to do.  For input
      ;;ports the action is to raise an exception.
      ((_ :unexpected-end-of-input (?port ?accumulated-chars))
       (%error-unexpected-eof ?port ?accumulated-chars))

      ;;This rule is used for input devices for which the numeric string
      ;;is embedded into a sequence of other characters, so there exists
      ;;a set of characters  that delimit the end-of-number.  The parser
      ;;delegates  to the  device  the responsibility  of knowing  which
      ;;characters are delimiters, if any.
      ;;
      ;;When the input  device is an input port:  we test for delimiters
      ;;as specified by R6RS.
      ((_ :generate-delimiter-test ?ch-var ?ch-is-delimiter-kont ?ch-is-not-delimiter-kont)
       (if (delimiter? ?ch-var)
	   ?ch-is-delimiter-kont
	 ?ch-is-not-delimiter-kont))

      ;;This rule is used to generate the "next input char" tests for an
      ;;operator function.   First of all the  end-of-input condition is
      ;;checked;  then  the continuation  form  for  more characters  is
      ;;expanded.
      ((_ :generate-end-of-input-or-char-tests ?ch-var ?next ?fail
	  (?port ?accumulated-chars)
	  ?end-of-input-kont ?parse-input-char-kont)
       (let ((?ch-var (peek-char ?port)))
	 (if (eof-object? ?ch-var)
	     (let-syntax
		 ((?fail (syntax-rules ()
			   ((_)
			    (%error-invalid-sequence ?port ?accumulated-chars)))))
	       ?end-of-input-kont)
	   (let-syntax
	       ((?fail (syntax-rules ()
			 ((_)
			  (%error-invalid-sequence ?port (cons ?ch-var ?accumulated-chars)))))
		(?next (syntax-rules ()
			 ((_ who args (... ...))
			  (who ?port (cons (get-char ?port) ?accumulated-chars) args (... ...))))))
	     ?parse-input-char-kont))))
      ))

  (define-syntax %error-invalid-sequence
    (syntax-rules ()
      ((_ ?port ?accumulated-characters)
       (die/p-1 ?port 'vicare-reader
		"invalid sequence of characters while parsing numeric lexeme"
		(reverse-list->string ?accumulated-characters)))))

  (define-syntax %error-unexpected-eof
    (syntax-rules ()
      ((_ ?port ?accumulated-characters)
       (die/p-1 ?port 'vicare-reader
		"unexpected end of input while parsing numeric lexeme"
		(reverse-list->string ?accumulated-characters)))))

  (define-string->number-parser port-logic
    (parse-numeric-string u:digit+ u:sign u:dot))

  #| end of module |# )


;;;; reading comments

(define (read-and-discard-up-to-and-including-line-ending port)
  (let ((ch (get-char-and-track-textual-position port)))
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
    (let ((c (get-char-and-track-textual-position port)))
      (cond ((eof-object? c)
	     (%multiline-error))

	    ;;A vertical bar character may or may not end this multiline
	    ;;comment.
	    ((unsafe.char= #\| c)
	     (let next-vertical-bar ((ch1 (get-char-and-track-textual-position port)) (ac ac))
	       (cond ((eof-object? ch1)
		      (%multiline-error))
		     ((unsafe.char= #\# ch1) ;end of comment
		      ac)
		     ((unsafe.char= #\| ch1) ;optimisation for sequence of bars?!?
		      (next-vertical-bar (get-char-and-track-textual-position port) (cons ch1 ac)))
		     (else
		      (recurse (cons ch1 ac))))))

	    ;;A hash character  may or may not start  a nested multiline
	    ;;comment.   Read a  nested multiline  comment, if  there is
	    ;;one.
	    ((unsafe.char= #\# c)
	     (let ((ch1 (get-char-and-track-textual-position port)))
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
    ((_ port ls str who case-insensitive-char? delimited?)
     (%read-char* port ls str who case-insensitive-char? delimited?))
    ((_ port ls str who)
     (%read-char* port ls str who #f #f))))

(define (%read-char* port ls str who case-insensitive-char? delimited?)
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
  ;;If CASE-INSENSITIVE-CHAR? is true: the comparison between characters
  ;;read from PORT and characters drawn from STR is case insensitive.
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
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-syntax-rule (%error-1 msg . args)
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
      (let ((ch (get-char-and-track-textual-position port)))
	(cond ((eof-object? ch)
	       (%error (format "invalid eof inside ~a" who)))
	      ((or (and (not case-insensitive-char?)
			(unsafe.char= ch (string-ref str i)))
		   (and case-insensitive-char?
			(unsafe.char= (char-downcase ch) (string-ref str i))))
	       (loop (add1 i) (cons ch ls)))
	      (else
	       (%error-1 (format "invalid ~a: ~s" who (reverse-list->string (cons ch ls))))))))))

(define (%read-char-skip-whitespace port caller)
  ;;Read and  discard characters from  PORT while they are  white spaces
  ;;according  to  CHAR-WHITESPACE?.  Return  the  first character  read
  ;;which is not a white space.
  ;;
  ;;CALLER must be a string  describing the token the caller is parsing,
  ;;it is used for error reporting.
  ;;
  (define-syntax-rule (%error msg . args)
    (die/p port 'tokenize msg . args))
  (define-inline (recurse)
    (%read-char-skip-whitespace port caller))
  (let ((ch (get-char-and-track-textual-position port)))
    (cond ((eof-object? ch)
	   (%error (string-append "invalid EOF while parsing " caller)))
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
  (define-syntax-rule (%error msg . irritants)
    (die/p port 'vicare-reader msg . irritants))
  (define-syntax-rule (%error-1 msg . irritants)
    (die/p-1 port 'vicare-reader msg . irritants))
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
		       (extend-graph-notation-kont-for-pair the-list the-list/ann
							    the-car the-cdr kont2))))))))

(define (extend-graph-notation-kont-for-pair pair pair/ann the-car the-cdr kont)
  ;;Return a new KONT thunk to finalise the graph notation locations for
  ;;a pair.
  ;;
  (if (or (loc? the-car)
	  (loc? the-cdr))
      (lambda ()
	;;When we  are sure that all  the locations have  been found and
	;;the corresponding datums  gathered: substitute the LOC structs
	;;in the pair with the corresponding datum and annotated datum.
	(let ((the-car1 (unsafe.car pair)))
	  (when (loc? the-car1)
	    (unsafe.set-car! pair     (loc-value     the-car1))
	    (unsafe.set-car! pair/ann (loc-value/ann the-car1))))
	(let ((the-cdr1 (unsafe.cdr pair)))
	  (when (loc? the-cdr1)
	    (unsafe.set-cdr! pair     (loc-value     the-cdr1))
	    (unsafe.set-cdr! pair/ann (loc-value/ann the-cdr1))))
	(kont))
    kont))


(define (finish-tokenisation-of-vector port locs kont count ls ls/ann)
  ;;Finish tokenising a vector reading  from PORT after the opening "#("
  ;;has been already consumed.  This function recursively invokes itself
  ;;to parse the next item in  the vector until a closing parenthesis is
  ;;read.
  ;;
  ;;COUNT is the number of items  in the vector, zero upon entering this
  ;;recursive function for the first time.
  ;;
  ;;LS is  the reversed list  of items collected  so far; LS/ANN  is the
  ;;annotated reversed  list of  items collected so  far.  Both  are nil
  ;;upon entering this recursive function for the first time.
  ;;
  ;;Return 4 values:  the vector datum, the annotated  vector datum, the
  ;;graph notation locations alist, a  thunk to be evaluated to finalise
  ;;the graph notation locations.
  ;;
  (define-inline (recurse locs1 kont1 ls1 ls1/ann)
    (finish-tokenisation-of-vector port locs1 kont1 (fxadd1 count) ls1 ls1/ann))
  (define-syntax-rule (%error msg . irritants)
    (die/p port 'vicare-reader msg . irritants))
  (define-syntax-rule (%error-1 msg . irritants)
    (die/p-1 port 'vicare-reader msg . irritants))

  (define-inline (main)
    (let-values (((token pos)
		  ;;start tokenising the next item
		  (start-tokenising/pos port)))
      (cond ((eof-object? token)
	     (%error "end of file encountered while reading vector"))
	    ((eq? token 'rparen)
	     (let* ((vec     (make-vector count))
		    (vec/ann (make-vector count))
		    (kont1   (%store-items-in-vector vec vec/ann kont (fxsub1 count) ls ls/ann)))
	       (values vec vec/ann locs kont1)))
	    ((eq? token 'rbrack)
	     (%error-1 "unexpected \")\" while reading vector"))
	    ((eq? token 'dot)
	     (%error-1 "unexpected \".\" while reading vector"))
	    (else
	     (let-values (((item item/ann locs1 kont1)
			   ;;finish tokenising the next item
			   (finalise-tokenisation port locs kont token pos)))
	       (recurse locs1 kont1 (cons item ls) (cons item/ann ls/ann)))))))

  (define (%store-items-in-vector vec vec/ann kont index ls ls/ann)
    (define-inline (recurse kont1)
      (%store-items-in-vector vec vec/ann kont1 (unsafe.fxsub1 index)
			      (unsafe.cdr ls) (unsafe.cdr ls/ann)))
    (if (null? ls)
	kont
      (let ((item (unsafe.car ls)))
	(vector-set! vec     index item)
	(vector-set! vec/ann index (unsafe.car ls/ann))
	(recurse (if (loc? item)
		     (lambda ()
		       ;;When we  are sure  that all the  locations have
		       ;;been   found  and   the   corresponding  datums
		       ;;gathered:  substitute  the  LOC struct  in  the
		       ;;vector   with  the   corresponding   datum  and
		       ;;annotated datum.
		       (vector-set! vec     index (loc-value     item))
		       (vector-set! vec/ann index (loc-value/ann item))
		       (kont))
		   kont)))))

  (main))


;;;; bytevectors tokenisation

(define-syntax define-finish-bytevector
  (syntax-rules ()
    ((_ ?who ?tag ?number-pred ?bytes-in-word ?unsafe.bytevector-set!)
     (define (?who port locs kont count ls)
       (define-inline (recurse locs1 kont1 count1 ls1)
	 (?who port locs1 kont1 count1 ls1))
       (define-syntax-rule (%error msg . irritants)
	 (die/p port 'vicare-reader msg . irritants))
       (define-syntax-rule (%error-1 msg . irritants)
	 (die/p-1 port 'vicare-reader msg . irritants))

       (define-inline (%make-bv the-count the-ls)
	 (let ((bv (unsafe.make-bytevector (* ?bytes-in-word the-count))))
	   (let loop ((i  (unsafe.fx- (unsafe.fx* count ?bytes-in-word) ?bytes-in-word))
		      (ls the-ls))
	     (if (null? ls)
		 bv
	       (let ((word (unsafe.car ls)))
		 (?unsafe.bytevector-set! bv i word)
		 (loop (unsafe.fx- i ?bytes-in-word) (unsafe.cdr ls)))))))

       (let-values (((token pos) (start-tokenising/pos port)))
	 (cond ((eof-object? token)
		(%error "unexpected EOF while reading a bytevector"))
	       ((eq? token 'rparen)
		(let ((v (%make-bv count ls)))
		  (values v v locs kont)))
	       ((eq? token 'rbrack)
		(%error-1 "unexpected ] while reading a bytevector"))
	       ((eq? token 'dot)
		(%error-1 "unexpected . while reading a bytevector"))
	       (else
		(let-values (((a a^ locs1 kont1) (finalise-tokenisation port locs kont token pos)))
		  (unless (?number-pred a)
		    (die/ann a^ 'vicare-reader "invalid value for this bytevector type" '?tag a))
                  (when (<= (greatest-fixnum) (* count ?bytes-in-word))
		    (%implementation-violation
		     'vicare-reader "number of elements too big for bytevector" count))
		  (recurse locs1 kont1 (fxadd1 count) (cons a ls))))))))))

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-u8
  'vu8			     ;tag
  words.word-u8?	     ;to validate numbers
  1			     ;number of bytes in word
  unsafe.bytevector-u8-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s8
  'vs8			     ;tag
  words.word-s8?	     ;to validate numbers
  1			     ;number of bytes in word
  unsafe.bytevector-s8-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-u16l
  'vu16l		       ;tag
  words.word-u16?	       ;to validate numbers
  2			       ;number of bytes in word
  unsafe.bytevector-u16l-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-u16b
  'vu16b		       ;tag
  words.word-u16?	       ;to validate numbers
  2			       ;number of bytes in word
  unsafe.bytevector-u16b-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-u16n
  'vu16n		       ;tag
  words.word-u16?	       ;to validate numbers
  2			       ;number of bytes in word
  unsafe.bytevector-u16n-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-s16l
  'vs16l		       ;tag
  words.word-s16?	       ;to validate numbers
  2			       ;number of bytes in word
  unsafe.bytevector-s16l-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s16b
  'vs16b		       ;tag
  words.word-s16?	       ;to validate numbers
  2			       ;number of bytes in word
  unsafe.bytevector-s16b-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s16n
  'vs16n		       ;tag
  words.word-s16?	       ;to validate numbers
  2			       ;number of bytes in word
  unsafe.bytevector-s16n-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-u32l
  'vu32l		       ;tag
  words.word-u32?	       ;to validate numbers
  4			       ;number of bytes in word
  unsafe.bytevector-u32l-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-u32b
  'vu32b		       ;tag
  words.word-u32?	       ;to validate numbers
  4			       ;number of bytes in word
  unsafe.bytevector-u32b-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-u32n
  'vu32n		       ;tag
  words.word-u32?	       ;to validate numbers
  4			       ;number of bytes in word
  unsafe.bytevector-u32n-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-s32l
  'vs32l		       ;tag
  words.word-s32?	       ;to validate numbers
  4			       ;number of bytes in word
  unsafe.bytevector-s32l-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s32b
  'vs32b		       ;tag
  words.word-s32?	       ;to validate numbers
  4			       ;number of bytes in word
  unsafe.bytevector-s32b-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s32n
  'vs32n		       ;tag
  words.word-s32?	       ;to validate numbers
  4			       ;number of bytes in word
  unsafe.bytevector-s32n-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-u64l
  'vu64l		       ;tag
  words.word-u64?	       ;to validate numbers
  8			       ;number of bytes in word
  unsafe.bytevector-u64l-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-u64b
  'vu64b		       ;tag
  words.word-u64?	       ;to validate numbers
  8			       ;number of bytes in word
  unsafe.bytevector-u64b-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-u64n
  'vu64n		       ;tag
  words.word-u64?	       ;to validate numbers
  8			       ;number of bytes in word
  unsafe.bytevector-u64n-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-s64l
  'vs64l		       ;tag
  words.word-s64?	       ;to validate numbers
  8			       ;number of bytes in word
  unsafe.bytevector-s64l-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s64b
  'vs64b		       ;tag
  words.word-s64?	       ;to validate numbers
  8			       ;number of bytes in word
  unsafe.bytevector-s64b-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-s64n
  'vs64n		       ;tag
  words.word-s64?	       ;to validate numbers
  8			       ;number of bytes in word
  unsafe.bytevector-s64n-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-f4l
  'vf4l				    ;tag
  flonum?			    ;to validate numbers
  4				    ;number of bytes in word
  bytevector-flonum-single-le-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-f4b
  'vf4b				    ;tag
  flonum?			    ;to validate numbers
  4				    ;number of bytes in word
  bytevector-flonum-single-be-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-f4n
  'vf4n				    ;tag
  flonum?			    ;to validate numbers
  4				    ;number of bytes in word
  bytevector-flonum-single-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-f8l
  'vf8l				    ;tag
  flonum?			    ;to validate numbers
  8				    ;number of bytes in word
  bytevector-flonum-double-le-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-f8b
  'vf8b				    ;tag
  flonum?			    ;to validate numbers
  8				    ;number of bytes in word
  bytevector-flonum-double-be-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-f8n
  'vf8n				    ;tag
  flonum?			    ;to validate numbers
  8				    ;number of bytes in word
  bytevector-flonum-double-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-c4l
  'vc4l				     ;tag
  cflonum?			     ;to validate numbers
  8				     ;number of bytes in word
  bytevector-cflonum-single-le-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-c4b
  'vc4b				     ;tag
  cflonum?			     ;to validate numbers
  8				     ;number of bytes in word
  bytevector-cflonum-single-be-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-c4n
  'vc4n				     ;tag
  cflonum?			     ;to validate numbers
  8				     ;number of bytes in word
  bytevector-cflonum-single-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define-finish-bytevector finish-tokenisation-of-bytevector-c8l
  'vc8l				     ;tag
  cflonum?			     ;to validate numbers
  16				     ;number of bytes in word
  bytevector-cflonum-double-le-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-c8b
  'vc8b				     ;tag
  cflonum?			     ;to validate numbers
  16				     ;number of bytes in word
  bytevector-cflonum-double-be-set!) ;setter

(define-finish-bytevector finish-tokenisation-of-bytevector-c8n
  'vc8n				     ;tag
  cflonum?			     ;to validate numbers
  16				     ;number of bytes in word
  bytevector-cflonum-double-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define (finish-tokenisation-of-bytevector-ve port locs kont)
  (define-syntax-rule (%error msg . irritants)
    (die/p port 'vicare-reader msg . irritants))
  (define-syntax-rule (%error-1 msg . irritants)
    (die/p-1 port 'vicare-reader msg . irritants))

  (let-values (((token pos) (start-tokenising/pos port)))
    (cond ((eof-object? token)
	   (%error "unexpected EOF while reading a bytevector"))
	  ((eq? token 'rparen)
	   (%error-1 "unexpected ) while reading a bytevector"))
	  ((eq? token 'rbrack)
	   (%error-1 "unexpected ] while reading a bytevector"))
	  ((eq? token 'dot)
	   (%error-1 "unexpected . while reading a bytevector"))
	  (else
	   (let-values
	       (((encoding encoding^ locs1 kont1)
		 (finalise-tokenisation port locs kont token pos)))
	     (unless (and (symbol? encoding)
			  (memq encoding '(ascii latin1 utf8 utf16be utf16le utf16n
						 hex base64)))
	       (die/ann encoding^ 'vicare-reader
			"expected encoding symbol for this bytevector type" encoding))
	     (let-values (((token pos) (start-tokenising/pos port)))
	       (cond ((eof-object? token)
		      (%error "unexpected EOF while reading a bytevector"))
		     ((eq? token 'rparen)
		      (%error-1 "unexpected ) while reading a bytevector"))
		     ((eq? token 'rbrack)
		      (%error-1 "unexpected ] while reading a bytevector"))
		     ((eq? token 'dot)
		      (%error-1 "unexpected . while reading a bytevector"))
		     (else
		      (let-values
			  (((string string^ locs1 kont1)
			    (finalise-tokenisation port locs kont token pos)))
			(unless (string? string)
			  (die/ann string^ 'vicare-reader
				   "expected data string for this bytevector type" string))
			(let-values (((token pos) (start-tokenising/pos port)))
			  (cond ((eof-object? token)
				 (%error "unexpected EOF while reading a bytevector"))
				((eq? token 'rparen)
				 (let ((v (case encoding
					    ((ascii)	(string->ascii		string))
					    ((latin1)	(string->latin1		string))
					    ((utf8)	(string->utf8		string))
					    ((utf16be)	(string->utf16be	string))
					    ((utf16le)	(string->utf16le	string))
					    ((utf16n)	(string->utf16n		string))
					    ((hex)	(string-hex->bytevector	string))
					    ((base64)	(string-base64->bytevector string))
					    (else
					     (%error "invalid bytevector encoding" encoding)))))
				   (values v v locs kont)))
				(else
				 (%error-1 "unexpected token while reading a bytevector" token)))))))))))))


(define (%process-comment-list port ls)
  ;;Called when a comment list syntax has been read "#!(<datum> ...)" to
  ;;process the list executing desired directives.
  ;;
  (define-inline (%error msg)
    (die/p port 'tokenize msg ls))
  (unless (null? ls)
    (case (car ls)
      ((load-shared-library)
       ;;Cause a foreign shared library to be loaded immediately or when
       ;;the FASL file is loaded.  The format of the comment list is:
       ;;
       ;;  #!(load-shared-library "library-id")
       ;;
       (cond ((null? (cdr ls))
	      (%error "expected argument to load-shared-library"))
	     ((not (null? (cddr ls)))
	      (%error "expected single argument to load-shared-library"))
	     (else
	      (let ((libid (cadr ls)))
		(if (string? libid)
		    (begin
		      (unless (shared-library-loading-enabled?)
			(%error
			 "comment list processing is disabled for this reading operation"))
		      (when (current-library-file)
			(register-filename-foreign-library (current-library-file) libid))
		      (autoload-filename-foreign-library libid))
		  (%error "expected string argument to load-shared-library"))))))
      ((char-names)
       ;;Define a  set of named  characters.  The format of  the comment
       ;;list is:
       ;;
       ;;   #!(char-names (<name> . <char>) ...)
       ;;
       (when (port-in-r6rs-mode? port)
	 (%error "invalid custom named character definition in R6RS mode"))
       (let ((table (custom-named-chars)))
	 (for-each (lambda (entry)
		     (if (and (symbol? (car entry))
			      (char?   (cdr entry)))
			 (hashtable-set! table (car entry) (cdr entry))
		       (%error "invalid entry in custom character names definition")))
	   (cdr ls))))
      (else
       (%error "invalid comment list")))))


;;;; done

(set-rtd-printer! (type-descriptor annotation) %annotation-printer)

)

;;; end of file
;;Local Variables:
;;eval: (put 'read-char-no-eof		'scheme-indent-function 1)
;;eval: (put '%read-char-no-eof		'scheme-indent-function 1)
;;End:
