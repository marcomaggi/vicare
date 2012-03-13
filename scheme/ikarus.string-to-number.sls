;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
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


;;;; internals documentation
;;
;;We  parse the  input  using a  set  of operator  functions which  draw
;;characters from  a configurable input device.   Each operator function
;;can either:  terminate parsing successfully, raise or  return an error
;;or tail-call another operator function.
;;
;;Operator  functions  are  generated  by  macros from  a  big  symbolic
;;expression  DEFINE-PARSER-LOGIC containing  a sub-expression  for each
;;operator.
;;
;;Access   to  the   input  device   is  specified   by   another  macro
;;(?DEVICE-LOGIC) which must implement  a set of syntax-rules to expand
;;to  the appropriate forms.   At the  end of  the expansion:  the input
;;device forms are hard coded into the parser functions.
;;
;;This  documentation makes use  of pseudo-code  to introduce  the basic
;;organisation of the code; the structure of operator functions is shown
;;in steps, adding new details at each iteration.
;;
;;An operator function accepting characters X or Y looks like this:
;;
;;  (define (operator-function-1 device)
;;    (let ((ch (get-next-char-from device)))
;;      (cond ((char=? X ch)
;;             (a-clause-form))
;;            ((char=? Y ch)
;;             (another-clause-form))
;;            (else
;;             (error-form))))
;;
;;and is specified in the parser logic as the symbolic subexpression:
;;
;;  (operator-function-1
;;    ((X)
;;     (a-clause-form))
;;    ((Y)
;;     (another-clause-form)))
;;
;;An operator function  accepting characters X, Y or Z, with  Y and Z to
;;be processed in the same way, looks like this:
;;
;;  (define (operator-function-2 device)
;;    (let ((ch (get-next-char-from device)))
;;      (cond ((char=? X ch)
;;             (a-clause-form))
;;            ((or (char=? Y ch) (char=? Z ch))
;;             (another-clause-form))
;;            (else
;;             (error-form))))
;;
;;and is specified in the parser logic as the symbolic subexpression:
;;
;;  (operator-function-2
;;    ((X)
;;     (a-clause-form))
;;    ((Y Z)
;;     (another-clause-form)))
;;
;;An  operator  function accepting  characters  X  or  Y, but  also  the
;;end-of-input from the device, looks like this:
;;
;;  (define (operator-function-3 device)
;;    (let ((ch (get-next-char-from device)))
;;      (cond ((end-of-input? ch)
;;             (end-of-input-form))
;;            ((char=? X ch)
;;             (a-clause-form))
;;            ((char=? Y ch)
;;             (another-clause-form))
;;            (else
;;             (error-form))))
;;
;;and is specified in the parser logic as the symbolic subexpression:
;;
;;  (operator-function-3
;;    ((eof)
;;     (end-of-input-form))
;;    ((X)
;;     (a-clause-form))
;;    ((Y)
;;     (another-clause-form)))
;;
;;for historical reasons  the end-of-input is called EOF  and "eof" must
;;be a free identifier.
;;
;;An  operator function accepting  characters X  or Y,  the end-of-input
;;from the device, and also a set of end-of-number delimiter characters,
;;looks like this:
;;
;;  (define (operator-function-4 device)
;;    (let ((ch (get-next-char-from device)))
;;      (cond ((end-of-input? ch)
;;             (end-of-input-form))
;;            ((char=? X ch)
;;             (a-clause-form))
;;            ((char=? Y ch)
;;             (another-clause-form))
;;            ((end-of-number-delimiter? ch)
;;             (end-of-input-form))
;;            (else
;;             (error-form))))
;;
;;and is specified in the parser logic as the symbolic subexpression:
;;
;;  (operator-function-4
;;    ((eof)
;;     (end-of-input-form))
;;    ((X)
;;     (a-clause-form))
;;    ((Y)
;;     (another-clause-form)))
;;
;;notice  how  the  END-OF-INPUT-FORM   is  used  for  both  the  proper
;;end-of-input  condition  and the  end-of-number  condition; also,  the
;;end-of-number condition  is not  explicitly specified in  the symbolic
;;subexpression: its  generation is  completely delegated to  the device
;;logic.
;;


(library (ikarus.string-to-number)
  (export string->number define-string->number-parser)
  (import (except (ikarus)
		  string->number)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))

;;; <number>          ::= <num 2>
;;;                     | <num 8>
;;;                     | <num 10>
;;;                     | <num 16>
;;; <num R>           ::= <prefix R> <complex R>
;;; <complex R>       ::= <real R>
;;;                     | <real R> "@" <real R>
;;;                     | <real R> "+" <ureal R> "i"
;;;                     | <real R> "-" <ureal R> "i"
;;;                     | <real R> "+" <naninf> "i"
;;;                     | <real R> "-" <naninf> "i"
;;;                     | <real R> "+" "i"
;;;                     | <real R> "-" "i"
;;;                     | "+" <ureal R> "i"
;;;                     | "-" <ureal R> "i"
;;;                     | "+" <naninf> "i"
;;;                     | "-" <naninf> "i"
;;;                     | "+" "i"
;;;                     | "-" "i"
;;; <real R>          ::= <sign> <ureal R>
;;;                     | "+" <naninf>
;;;                     | "-" <naninf>
;;; <naninf>          ::= "nan.0"
;;;                     | "inf.0"
;;; <ureal R>           | <uinteger R>
;;;                     | <uinteger R> "/" <uinteger R>
;;;                     | <decimal R> <mantissa width>
;;; <decimal 10>      ::= <uinteger 10> <suffix>
;;;                     | "." <digit 10> + <suffix>
;;;                     | <digit 10> + "." <digit 10> * <suffix>
;;;                     | <digit 10> + "." <suffix>
;;; <uinteger R>      ::= <digit R> +
;;; <prefix R>          | <radix R> <exactness>
;;;                     | <exactness <radix R>
;;; <suffix>          ::= epsilon
;;;                     | <exponent-marker> <sign> <digit 10> +
;;; <exponent-marker> ::= "e"
;;;                     | "E"
;;;                     | "s"
;;;                     | "S"
;;;                     | "f"
;;;                     | "F"
;;;                     | "d"
;;;                     | "D"
;;;                     | "l"
;;;                     | "L"
;;; <mantissa-width>  ::= epsilon
;;;                     | "|" <digit +>
;;; <sign>            ::= epsilon
;;;                     | "+"
;;;                     | "-"
;;; <exactness>       ::= epsilon
;;;                     | "#i"
;;;                     | "#I"
;;;                     | "#e"
;;;                     | "#E"
;;; <radix-2>         ::= "#b"
;;;                     | "#B"
;;; <radix-8>         ::= "#o"
;;;                     | "#O"
;;; <radix-10>        ::= epsilon
;;;                     | "#d"
;;;                     | "#D"
;;; <radix-16>        ::= "#x"
;;;                     | "#X"
;;; <digit-2>         ::= "0"
;;;                     | "1"
;;; <digit-8>         ::= "0"
;;;                     | "1"
;;;                     | "2"
;;;                     | "3"
;;;                     | "4"
;;;                     | "5"
;;;                     | "6"
;;;                     | "7"
;;; <digit-10>        ::= <digit>
;;; <digit-16>        ::= <hex-digit>
;;; <digit>           ::= "0"
;;;                     | "1"
;;;                     | "2"
;;;                     | "3"
;;;                     | "4"
;;;                     | "5"
;;;                     | "6"
;;;                     | "7"
;;;                     | "8"
;;;                     | "9"
;;; <hex-digit>       ::= <hex>
;;;                     | "A"
;;;                     | "B"
;;;                     | "C"
;;;                     | "D"
;;;                     | "E"
;;;                     | "F"
;;;                     | "a"
;;;                     | "b"
;;;                     | "c"
;;;                     | "d"
;;;                     | "e"
;;;                     | "f"


;;;; arguments validation

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (radix who obj)
  (memv obj '(10 16 2 8))
  (assertion-violation who "expected supported radix as argument" obj))


;;;; helpers

(define-inline-constant CHAR-FIXNUM-0	48 #;(unsafe.char->fixnum #\0))
(define-inline-constant CHAR-FIXNUM-a	97 #;(unsafe.char->fixnum #\a))
(define-inline-constant CHAR-FIXNUM-A	65 #;(unsafe.char->fixnum #\A))

(define-inline (compose-sign-and-accum-with-exactness ?sign ?exactness ?number-expr)
  ;;Compose  the number  ?NUMBER-EXPR with  the  sign ?SIGN  (+1 or  -1)
  ;;according to the exactness ?EXACTNESS  (false, the symbol "i" or the
  ;;symbol "e").
  ;;
  ;;If  ?EXACTNESS  is  false  (neither  "#e"  nor  "#i"  prefixes  were
  ;;present): the exactness defaults to "exact".
  ;;
  (let ((ac ?number-expr)
	(ex ?exactness))
    (* ?sign (if (eq? ex 'i)
		 (inexact ac)
	       ac))))

(define-inline (compose-sign-and-accum-with-inexactness sn ex ?ac)
  ;;Compose the accumulated number ?AC with the sign SN according to the
  ;;exactness EX.
  ;;
  ;;If  ?EXACTNESS  is  false  (neither  "#e"  nor  "#i"  prefixes  were
  ;;present): the exactness defaults to "inexact".
  ;;
  (let ((ac ?ac))
    (* sn (if (eq? ex 'e) ac (inexact ac)))))

(define-inline (sign ?ch)
  (let ((ch ?ch))
    (cond ((unsafe.char= #\+ ch)	+1)
	  ((unsafe.char= #\- ch)	-1)
	  (else				#f))))

(define (digit ch radix)
  ;;Given  a character  CH representing  a  digit in  some RADIX  (which
  ;;should be  the fixnum 2, 8,  10 or 16) return  a fixnum representing
  ;;the character digit CH.
  ;;
  ;;If the arguments are wrong: return false.
  ;;
  (let* ((F (unsafe.char->fixnum ch))
	 (N (unsafe.fx- F CHAR-FIXNUM-0)))
    (cond ((and (unsafe.fx>= N 0)
		(unsafe.fx<  N radix))
	   N)
	  ((unsafe.fx= radix 16)
	   (let ((N (unsafe.fx- F CHAR-FIXNUM-a)))
	     (if (and (unsafe.fx>= N 0)
		      (unsafe.fx<  N 6))
		 (unsafe.fx+ N 10)
	       (let ((N (unsafe.fx- F CHAR-FIXNUM-A)))
		 (if (and (unsafe.fx>= N 0)
			  (unsafe.fx<  N 6))
		     (unsafe.fx+ N 10)
		   #f)))))
	  (else #f))))

(define (mkrec0 n0 n1)
  (cond ((not n0)
	 (make-rectangular 0 n1))
	((and (pair? n0)
	      (eq? (unsafe.car n0) 'polar))
	 (make-polar (unsafe.cdr n0) n1))
	(else
	 (make-rectangular n0 n1))))

(define (mkrec1 n0 n1)
  (cond ((not n0)
	 n1)
	((and (pair? n0)
	      (eq? (unsafe.car n0) 'polar))
	 (make-polar (unsafe.cdr n0) n1))
	(else
	 (make-rectangular n0 n1))))


(module (define-parser-logic)

  (define-syntax %generate-end-of-input-form
    (syntax-rules (eof)
      ;;End-of-input was  found from the  input device, but there  is no
      ;;EOF clause  specified for this parser  operator: end-of-input is
      ;;an error here.
      ((_ ?device-logic ?device-arg-list)
       (?device-logic UNEXPECTED-EOF-ERROR ?device-arg-list))
      ;;End-of-input was found from the input device and there is an EOF
      ;;clause specified for this  parser operator: end-of-input is fine
      ;;here, so execute the clause.
      ((_ ?device-logic ?device-arg-list ((eof) ?then-form) . ?other-clauses)
       ?then-form)
      ;;Discard ?NOT-EOF-CLAUSE and recurse.
      ((_ ?device-logic ?device-arg-list ?not-eof-clause . ?other-clauses)
       (%generate-end-of-input-form ?device-logic ?device-arg-list . ?other-clauses))))

  (define-syntax %generate-delimiter-test
    ;;Expand to a single form  testing if the character bound to ?CH-VAR
    ;;is   an  end-of-number   delimiter.    Both  the   test  and   the
    ;;responsibility  to perfom an  action are  delegated to  the device
    ;;logic; the parser merely suggests a is-delimiter continuation form
    ;;and a is-not-delimiter continuation form.
    ;;
    (syntax-rules (eof)
      ;;There  is  no EOF  clause  specified  for  the current  operator
      ;;function: expand to the error form.
      ((_ ?device-logic ?device-arg-list ?ch-var)
       (?device-logic GENERATE-DELIMITER-TEST ?ch-var
		      (?device-logic FAIL ?device-arg-list ?ch-var)
		      (?device-logic FAIL ?device-arg-list ?ch-var)))
      ;;There  is  an EOF  clause  specified  for  the current  operator
      ;;function; such  clause is used to process  both the end-of-input
      ;;and end-of-number conditions.
      ((_ ?device-logic ?device-arg-list ?ch-var ((eof) ?then-clause) . ?other-clauses)
       (?device-logic GENERATE-DELIMITER-TEST ?ch-var
		      ?then-clause
		      (?device-logic FAIL ?device-arg-list ?ch-var)))
      ;;Discard ?NOT-EOF-CLAUSE and recurse.
      ((_ ?device-logic ?device-arg-list ?ch-var ?not-eof-clause . ?other-clauses)
       (%generate-delimiter-test ?device-logic ?device-arg-list ?ch-var . ?other-clauses))))

  (define-syntax %generate-parse-more-chars-form
    ;;Recursively iterate  over the clauses of an  operator function and
    ;;expand to  code that  checks them against  the character  bound to
    ;;?CH-VAR.
    ;;
    (syntax-rules (eof =>)

      ;;No more operator clauses.  Expand to the end-of-number delimiter
      ;;characters test.
      ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form)
       ?test-delimiter-form)

      ;;The  operator clause specifies  that end-of-input  is acceptable
      ;;for  this  operator function.   Skip  this  clause here  because
      ;;(being checked  first among  all the clauses)  it is  handled by
      ;;another syntax.
      ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	  ((eof) ?then-form) . ?other-operator-clauses)
       (%generate-parse-more-chars-form ?device-logic ?device-arg-list ?ch-var
					?test-delimiter-form . ?other-operator-clauses))

      ;;The clause  specifies a test  function to be applied  to ?CH-VAR
      ;;along with some additional arguments.
      ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	  ((?test . ?test-args) => ?test-result ?then-form) . ?other-operator-clauses)
       (cond ((?test ?ch-var . ?test-args)
	      => (lambda (?test-result) ?then-form))
	     (else
	      (%generate-parse-more-chars-form ?device-logic ?device-arg-list ?ch-var
					       ?test-delimiter-form . ?other-operator-clauses))))

      ;;The  operator clause  specifies a  list of  characters  to match
      ;;against ?CH-VAR.
      ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	  ((?char ...) ?then-form) . ?other-operator-clauses)
       (if (or (unsafe.char= ?char ?ch-var)
	       ...)
	   ?then-form
	 (%generate-parse-more-chars-form ?device-logic ?device-arg-list ?ch-var
					  ?test-delimiter-form . ?other-operator-clauses)))))

  (define-syntax %generate-operator
    ;;Define  an  operator  function.   The  name  of  the  function  is
    ;;automatically generated; it  is responsibility of DEFINE-PARSER to
    ;;define "public" identifiers bound to the private name.
    ;;
    (syntax-rules ()
      ((_ (?device-arg ...) ?device-logic ?next ?fail
	  ?operator-name (?operator-arg ...) (?operator-clause ...))
       (define (?operator-name ?device-arg ... ?operator-arg ...)
	 ;;Introduce the identifier "ch" used to bind the next char from
	 ;;the input device.
	 (?device-logic GENERATE-EOF-THEN-CHARS-TESTS ch
			?next ?fail (?device-arg ...)
			(%generate-end-of-input-form
			 ?device-logic (?device-arg ...) ?operator-clause ...)
			(%generate-parse-more-chars-form
			 ?device-logic (?device-arg ...) ch
			 (%generate-delimiter-test ?device-logic (?device-arg ...) ch
						   ?operator-clause ...)
			 ?operator-clause ...)
			)))))

  (define-syntax define-parser
    ;;This is the true parser logic generator.
    ;;
    (lambda (x)
      (define (lookup original-operator-names actual-operator-names)
	(lambda (public-operator-name)
	  ;;Check that  a requested public  operator name is  indeed the
	  ;;name of  an operator function in the  abstract parser logic.
	  ;;If successful: return  an identifier representing the actual
	  ;;parser operator name.
	  ;;
	  (let loop ((ls1 original-operator-names)
		     (ls2 actual-operator-names))
	    (cond ((null? ls1)
		   (assertion-violation 'define-parser
		     "unknown requested public parser operator name"
		     public-operator-name))
		  ((bound-identifier=? public-operator-name (unsafe.car ls1))
		   (unsafe.car ls2))
		  (else
		   (loop (unsafe.cdr ls1) (unsafe.cdr ls2)))))))
      (syntax-case x ()
	((_ (?public-operator-name ...) ?device-logic ?next ?fail
	    ?original-operator-names
	    (?operator-name (?operator-arg ...) ?operator-clause ...) ...)
	 (with-syntax (((OPERATOR-NAME ...)
			(map (lookup (syntax->datum #'?original-operator-names)
				     #'(?operator-name ...))
			  #'(?public-operator-name ...))))
	   #'(begin
	       ;;This form  expands to an  operator function bound  to a
	       ;;name  in the lexical  scope of  the DEFINE-PARSER-LOGIC
	       ;;use.
	       (?device-logic INTRODUCE-DEVICE-ARGUMENTS
			      %generate-operator ?device-logic ?next ?fail ?operator-name
			      (?operator-arg ...)
			      (?operator-clause ...))
	       ...

	       ;;This form aliases an operator name in the lexical scope
	       ;;of the  DEFINE-PARSER-LOGIC use to an  operator name in
	       ;;the lexical scope of the concrete parser definition.
	       (define ?public-operator-name OPERATOR-NAME)
	       ...))))))

  (define-syntax define-parser-logic
    ;;Define the parser logic  through a list of symbolic subexpressions
    ;;representing operator function  spefications.  The parser logic is
    ;;an  "abstract parser"  which must  be specialised  with  the input
    ;;device logic.
    ;;
    ;;?PARSER-DEFINER must be the  identifier that will become the macro
    ;;keyword of the concrete parser-definition macro.
    ;;
    ;;?NEXT must  be the identifier  that will become the  macro keyword
    ;;used to draw  the next character from the  input device.  ?NEXT is
    ;;used in the operator subexpressions.
    ;;
    ;;?FAIL must  be the identifier  that will become the  macro keyword
    ;;used to  return an  error to the  caller of an  operator function.
    ;;?FAIL is used in the operator subexpressions.
    ;;
    ;;?OPERATOR-NAME  must  be  the  identifier  to  which  an  operator
    ;;function will be bound.  This identifier is in the lexical context
    ;;of  the  DEFINE-PARSER-LOGIC form,  which  is  different from  the
    ;;lexical context of a use of the ?PARSER-DEFINER macro.
    ;;
    ;;?OPERATOR-SPECIFIC-ARG must  be an  identifier that will  become a
    ;;formal argument for an operator function.  These arguments are the
    ;;operator-specific ones.
    ;;
    ;;?OPERATOR-CLAUSE  must  be   a  symbolic  expression  representing
    ;;accepted input conditions for an operator function.
    ;;
    (lambda (x)
      (syntax-case x ()
	((_ ?parser-definer ?next ?fail
	    (?operator-name (?operator-specific-arg ...) ?operator-clause ...)
	    ...)
	 (with-syntax
	     ;;Insert:
	     ;;
	     ;;  #'(?operator-name ...)
	     ;;
	     ;;in  a syntax object  so that  it can  be embedded  in the
	     ;;output form of this macro and later extracted intact from
	     ;;the input form of DEFINE-PARSER.
	     ((ORIGINAL-OPERATOR-NAMES (datum->syntax #'foo #'(?operator-name ...))))
	   #'(define-syntax ?parser-definer
	       (syntax-rules ()
		 ((_ ??device-logic (??public-operator-name (... ...)))
		  (define-parser (??public-operator-name (... ...))
		    ??device-logic ?next ?fail
		    ORIGINAL-OPERATOR-NAMES
		    (?operator-name (?operator-specific-arg ...) ?operator-clause ...) ...))))
	   ))
	)))

  #| end of module |#)


;;;; parser logic

(define-parser-logic define-string->number-parser next fail

  ;;Continue processing  the denominator of  a rational number  after at
  ;;least the first digit has been acquired.
  ;;
  ;;RADIX must be a fixnum among: 2, 8, 10, 16.
  ;;
  ;;N0 ???
  ;;
  ;;EXACTNESS must be false or the  symbol "e" (for exact) or the symbol
  ;;"i" (for inexact).
  ;;
  ;;SN must  be +1 or -1 and  represents the sign of  the numerator (the
  ;;denominator cannot have a sign).
  ;;
  ;;NUM is the a non-negative integer representing the numerator.
  ;;
  ;;ACCUM must  be real non-negative number; it  represents the absolute
  ;;value  of  the number  accumulated  so  far  from previosuly  parsed
  ;;digits.
  ;;
  (u:ratio+ (radix n0 exactness sn num accum)
	    ((eof)
	     (if (zero? accum)
		 (fail)
	       (mkrec0 n0 (compose-sign-and-accum-with-exactness sn exactness (/ num accum)))))
	    ((digit radix) => digit-fx
	     (next u:ratio+ radix n0 exactness sn num (+ (* accum radix) digit-fx)))
	    ((sign) => sn2
		;terminate a  rational number being  the real part  of a
		;complex in rectangular notation and start the imaginary
		;part
	     (if (or n0 (zero? accum))
		 (fail)
	       (let ((real (compose-sign-and-accum-with-exactness sn exactness (/ num accum))))
		 (next u:sign radix real exactness sn2))))
	    ((#\@)
		;terminate a  rational number  being the magnitude  of a
		;complex in polar notation and start the angle part
	     (if (or n0 (zero? accum))
		 (fail)
	       (let ((mag (compose-sign-and-accum-with-exactness sn exactness (/ num accum))))
		 (next u:polar radix mag exactness))))
	    ((#\i)
		;terminate a rational number being the imaginary part of
		;a  complex  in rectangular  notation;  after this  only
		;end-of-input or end-of-number are valid
	     (if (zero? accum)
		 (fail)
	       (next u:done
		     (mkrec0 n0 (compose-sign-and-accum-with-exactness sn exactness (/ num accum)))))))

  ;;Start processing the denominator of a rational number.  At least one
  ;;digit is expected.
  ;;
  ;;RADIX must be a fixnum among: 2, 8, 10, 16.
  ;;
  ;;N0 ???
  ;;
  ;;EXACTNESS must be false or the  symbol "e" (for exact) or the symbol
  ;;"i" (for inexact).
  ;;
  ;;SN must  be +1 or -1 and  represents the sign of  the numerator (the
  ;;denominator cannot have a sign).
  ;;
  ;;NUM is the a non-negative integer representing the numerator.
  ;;
  (u:ratio (radix n0 exactness sn num)
	   ((digit radix) => digit-fx
	    (next u:ratio+ radix n0 exactness sn num digit-fx)))

  ;;Terminate  parsing when  we know  that the  last  consumed character
  ;;terminates the numeric sequence.  In practice this only happens when
  ;;we have read the #\i that terminates the imaginary part of a complex
  ;;in rectangular notation.
  ;;
  ;;N must  be the  number object to  be returned  to the caller  of the
  ;;parser.
  ;;
  (u:done (n)
	  ((eof) n))

  (u:polar (r mag ex)
	   ((digit r) => d
	    (next u:digit+ r (cons 'polar mag) ex +1 d))
	   ((#\.)
	    (if (= r 10)
		(next u:dot r (cons 'polar mag) ex +1)
	      (fail)))
	   ((sign) => sn
	    (next u:sign r (cons 'polar mag) ex sn)))

  (u:exponent+digit (r n0 ex sn ac exp1 exp2 exp-sign)
		    ((eof)
		     (if (number? n0)
			 (fail)
		       (mkrec1 n0 (compose-sign-and-accum-with-inexactness
				   sn ex (* ac (expt 10 (+ exp1 (* exp2 exp-sign))))))))
		    ((digit r) => d
		     (next u:exponent+digit r n0 ex sn ac exp1 (+ (* exp2 r) d) exp-sign))
		    ((sign) => sn2
		     (if n0
			 (fail)
		       (let ((real (compose-sign-and-accum-with-inexactness
				    sn ex (* ac (expt 10 (+ exp1 (* exp2 exp-sign)))))))
			 (next u:sign r real ex sn2))))
		    ((#\@)
		     (if n0
			 (fail)
		       (let ((mag (compose-sign-and-accum-with-inexactness
				   sn ex (* ac (expt 10 (+ exp1 (* exp2 exp-sign)))))))
			 (next u:polar r mag ex))))
		    ((#\i)
		     (let ((n1 (compose-sign-and-accum-with-inexactness
				sn ex (* ac (expt 10 (+ exp1 (* exp2 exp-sign)))))))
		       (next u:done (mkrec0 n0 n1))))
		    ((#\|)
		     (let ((n1 (compose-sign-and-accum-with-inexactness
				sn ex (* ac (expt 10 (+ exp1 (* exp2 exp-sign)))))))
		       (next u:mant r n0 n1 ex))))

  (u:exponent+sign (r n0 ex sn ac exp1 exp-sign)
		   ((digit r) => d
		    (next u:exponent+digit r n0 ex sn ac exp1 d exp-sign)))

  (u:exponent (r n0 ex sn ac exp1)
	      ((digit r) => d
	       (next u:exponent+digit r n0 ex sn ac exp1 d +1))
	      ((sign) => sn2
	       (next u:exponent+sign r n0 ex sn ac exp1 sn2)))

  (u:digit+dot (r n0 ex sn ac exp)
	       ((eof)
		(if (and n0 (not (pair? n0)))
		    (fail)
		  (mkrec1 n0 (compose-sign-and-accum-with-inexactness sn ex (* ac (expt 10 exp))))))
	       ((digit r) => d
		(next u:digit+dot r n0 ex sn (+ (* ac r) d) (- exp 1)))
	       ((#\i)
		(let ((n1 (compose-sign-and-accum-with-inexactness sn ex (* ac (expt 10 exp)))))
		  (next u:done (mkrec0 n0 n1))))
	       ((sign) => sn2
		(if n0
		    (fail)
		  (let ((real (compose-sign-and-accum-with-inexactness sn ex (* ac (expt 10 exp)))))
		    (next u:sign r real ex sn2))))
	       ((#\@)
		(if n0
		    (fail)
		  (let ((mag (compose-sign-and-accum-with-inexactness sn ex (* ac (expt 10 exp)))))
		    (next u:polar r mag ex))))
	       ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
		(if (fx=? r 10)
		    (next u:exponent r n0 ex sn ac exp)
		  (fail)))
	       ((#\|)
		(let ((n1 (compose-sign-and-accum-with-inexactness sn ex (* ac (expt 10 exp)))))
		  (next u:mant r n0 n1 ex))))

  ;;This operator  accumulates a number from  digit characters compliant
  ;;with RADIX.   It is used to  accumulate a number  before the decimal
  ;;dot.
  ;;
  ;;RADIX must be a fixnum among: 2, 8, 10, 16.
  ;;
  ;;N0 ???
  ;;
  ;;EXACTNESS must be false or the  symbol "e" (for exact) or the symbol
  ;;"i" (for inexact).
  ;;
  ;;SN  must be  +1 or  -1 and  represents the  sign of  the accumulated
  ;;number.
  ;;
  ;;ACCUM must  be real non-negative number; it  represents the absolute
  ;;value  of  the number  accumulated  so  far  from previosuly  parsed
  ;;digits.
  ;;
  (u:digit+ (radix n0 exactness sn accum)
	    ((eof)
	     (if (and n0 (not (pair? n0)))
		 (fail)
	       (mkrec1 n0 (compose-sign-and-accum-with-exactness sn exactness accum))))
	    ((digit radix) => digit-fx
	     (next u:digit+ radix n0 exactness sn (+ (* accum radix) digit-fx)))
	    ((#\.)
	     (if (unsafe.fx= radix 10)
		 (next u:digit+dot radix n0 exactness sn accum 0)
	       (fail)))
	    ((#\/) ;terminate the numerator of a rational
	     (next u:ratio radix n0 exactness sn accum))
	    ((sign) => sn2
		;terminate  the real  part of  a complex  in rectangular
		;notation and start the imaginary part
	     (if n0
		 (fail)
	       (let ((real (compose-sign-and-accum-with-exactness sn exactness accum)))
		 (next u:sign radix real exactness sn2))))
	    ((#\i) ;terminate the imaginary part of a complex
	     (next u:done (mkrec0 n0 (compose-sign-and-accum-with-exactness sn exactness accum))))
	    ((#\@)
		;terminate the magnitude of  a complex in polar notation
		;and start the angle part
	     (if n0
		 (fail)
	       (let ((mag (compose-sign-and-accum-with-exactness sn exactness accum)))
		 (next u:polar radix mag exactness))))
	    ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L) ;exponent markers
	     (if (unsafe.fx= radix 10)
		 (next u:exponent radix n0 exactness sn accum 0)
	       (fail)))
	    ((#\|) ;start the mantissa width
	     (next u:mant radix n0 (compose-sign-and-accum-with-exactness sn 'i accum) exactness)))

  (u:mant (r n0 n1 ex)
	  ((digit r) => d_
	   (next u:mant+ r n0 n1 ex)))

  (u:mant+ (r n0 n1 ex)
	   ((eof) (mkrec1 n0 n1))
	   ((digit r) => d_
	    (next u:mant+ r n0 n1 ex))
	   ((sign) => sn2
	    (if n0
		(fail)
	      (next u:sign r n1 ex sn2)))
	   ((#\@)
	    (if n0
		(fail)
	      (next u:polar r n1 ex)))
	   ((#\i)
	    (if (pair? n0)
		(fail)
	      (next u:done (mkrec0 n0 n1)))))

  (u:sign-i (r n0 ex sn)
	    ((eof)
	     (mkrec0 n0 (compose-sign-and-accum-with-exactness sn ex 1)))
	    ((#\n)
	     (next u:sign-in r n0 (* sn +inf.0) ex)))
  (u:sign-in (r n0 n1 ex)
	     ((#\f) (next u:sign-inf r n0 n1 ex)))
  (u:sign-inf (r n0 n1 ex)
	      ((#\.)
	       (next u:sign-inf. r n0 n1 ex)))
  (u:sign-inf. (r n0 n1 ex)
	       ((#\0)
		(next u:sign-inf.0 r n0 n1 ex)))
  (u:sign-inf.0 (r n0 n1 ex)
		((eof)
		 (mkrec1 n0 n1))
		((sign) => sn2
		 (if n0
		     (fail)
		   (next u:sign r n1 ex sn2)))
		((#\@)
		 (if n0
		     (fail)
		   (next u:polar r n1 ex)))
		((#\i)
		 (next u:done (mkrec0 n0 n1))))

  (u:dot (r n0 ex sn)
	 ((digit r) => d
	  (next u:digit+dot r n0 ex sn d -1)))

  (u:sign (r n0 ex sn)
	  ((digit r) => d
	   (next u:digit+ r n0 ex sn d))
	  ((#\i)
	   (next u:sign-i r n0 ex sn))
	  ((#\n)
	   (next u:sign-n r n0 ex))
	  ((#\.)
	   (if (= r 10)
	       (next u:dot r n0 ex sn)
	     (fail))))

  (u:sign-n (r n0 ex)
	    ((#\a)
	     (next u:sign-na r n0 ex)))
  (u:sign-na (r n0 ex)
	     ((#\n)
	      (next u:sign-nan r n0 ex)))
  (u:sign-nan (r n0 ex)
	      ((#\.)
	       (next u:sign-nan. r n0 ex)))
  (u:sign-nan. (r n0 ex)
	       ((#\0)
		(next u:sign-nan.0 r n0 ex)))
  (u:sign-nan.0 (r n0 ex)
		((eof)
		 (mkrec1 n0 +nan.0))
		((sign) => sn2
		 (if n0
		     (fail)
		   (next u:sign r +nan.0 ex sn2)))
		((#\@)
		 (if n0
		     (fail)
		   (next u:polar r +nan.0 ex)))
		((#\i)
		 (next u:done (mkrec0 n0 +nan.0))))

  ;;Parse  an  input char  after  the  opening  #\# character  has  been
  ;;consumed by PARSE-STRING.
  ;;
  ;;We  must  remember  that  the  prefixes  "#e"  and  "#i"  specifying
  ;;exactness can be preceeded or followed by prefixes "#b", "#d", "#o",
  ;;"#x"  overriding the  default  radix.   It is  an  error to  specify
  ;;multiple exactness prefixes or multiple radix prefixes.
  ;;
  (parse-string-after-hash (radix override-radix exactness)
			   ((#\x #\X)
			    (if override-radix
				(fail)
			      (next parse-string 16 16 exactness)))
			   ((#\o #\O)
			    (if override-radix
				(fail)
			      (next parse-string 8 8 exactness)))
			   ((#\b #\B)
			    (if override-radix
				(fail)
			      (next parse-string 2 2 exactness)))
			   ((#\d #\D)
			    (if override-radix
				(fail)
			      (next parse-string 10 10 exactness)))
			   ((#\e #\E)
			    (if exactness
				(fail)
			      (next parse-string radix override-radix 'e)))
			   ((#\i #\I)
			    (if exactness
				(fail)
			      (next parse-string radix override-radix 'i))))

  ;;This  is the  entry point  to parse  any valid  numeric  sequence of
  ;;characters  from  the input.   This  operator  may recursively  call
  ;;itself.
  ;;
  ;;RADIX must be a fixnum among: 2, 8, 10, 16.
  ;;
  ;;OVERRIDE-RADIX must be false or a  fixnum among: 2, 8, 10, 16.  When
  ;;set to  false it means that  none of the prefixes  "#b", "#d", "#o",
  ;;"#x" were read from the input.  This argument is changed only if one
  ;;of the prefixes "#b", "#d", "#o",  "#x" is found in the input.  This
  ;;argument exists for the only purpose of checking that radix prefixes
  ;;are not used multiple times.
  ;;
  ;;EXACTNESS must be false or the  symbol "e" (for exact) or the symbol
  ;;"i" (for  inexact).  When set  to false it  meanst that none  of the
  ;;prefixes  "#e", "#i"  were read  from the  input.  This  argument is
  ;;changed  only if  one of  the prefixes  "#i", "#e"  is found  in the
  ;;input.
  ;;
  (parse-string (radix override-radix exactness)
		((#\#)
		 (next parse-string-after-hash radix override-radix exactness))
		((sign) => sn2
		 (next u:sign radix #f exactness sn2))
		((#\.)
		 (if (unsafe.fx= radix 10)
		     (next u:dot radix #f exactness +1)
		   (fail)))
		((digit radix) => start-number
		 (next u:digit+ radix #f exactness +1 start-number)))

  #| end of DEFINE-PARSER |# )


;;;; device logic for STRING->NUMBER

(define-syntax string->number-logic
  (syntax-rules (INTRODUCE-DEVICE-ARGUMENTS
		 GENERATE-EOF-THEN-CHARS-TESTS
		 GENERATE-DELIMITER-TEST
		 UNEXPECTED-EOF-ERROR
		 FAIL)

    ;;Introduce a list of identifiers used as device-specific arguments;
    ;;they  will  be  the  first  arguments  for  each  parser  operator
    ;;function.
    ;;
    ((_ INTRODUCE-DEVICE-ARGUMENTS ?kont . ?rest)
     (?kont (input.string input.length input.index) . ?rest))

    ;;Whenever  an  input  character  is  not accepted  by  an  operator
    ;;function  this   rule  is  used   to  decide  what  to   do.   For
    ;;STRING->NUMBER the action to do is to return false.
    ((_ FAIL (?input.string ?input.length ?input.index) ?ch-var)
     #f)

    ;;Whenever the  end-of-input is found in  a position in  which it is
    ;;unexpected,  this  rule  is  used  to  decide  what  to  do.   For
    ;;STRING->NUMBER the action to do is to return false.
    ;;
    ((_ UNEXPECTED-EOF-ERROR (?input.string ?input.length ?input.index))
     #f)

    ;;This rule is  used for input devices for  which the numeric string
    ;;is embedded into a sequence of other characters, so there exists a
    ;;set  of characters  that  delimit the  end-of-number.  The  parser
    ;;delegates  to  the  device  the responsibility  of  knowing  which
    ;;characters are delimiters.
    ;;
    ;;When the input  device is a string containing  only the number, as
    ;;is  the case  for  STRING->NUMBER: there  are  no delimiters,  the
    ;;end-of-number  is the  end of  the  string.  We  avoid looking  at
    ;;?CH-VAR and just expand to the not-delimiter continuation form.
    ;;
    ((_ GENERATE-DELIMITER-TEST ?ch-var ?ch-is-delimiter-kont ?ch-is-not-delimiter-kont)
     ?ch-is-not-delimiter-kont)

    ;;This rule is used to  generate the tests for an operator function.
    ;;First  of all  the  end-of-input condition  is  checked; then  the
    ;;continuation form for more characters is expanded.
    ;;
    ((_ GENERATE-EOF-THEN-CHARS-TESTS ?ch-var ?next ?fail
	(?input.string ?input.length ?input.index)
	?end-of-input-kont ?more-characters-kont)
     (let-syntax
	 ((?fail (syntax-rules ()
		   ((_) #f)))
	  (?next (syntax-rules ()
		   ((_ ?operator-name ?operator-arg (... ...))
		    (?operator-name ?input.string ?input.length (unsafe.fxadd1 ?input.index)
				    ?operator-arg (... ...))))))
       (if (unsafe.fx= ?input.index ?input.length) ;end-of-input
	   ?end-of-input-kont
	 (let ((?ch-var (unsafe.string-ref ?input.string ?input.index)))
	   ?more-characters-kont))))
    ))


;;;; definition of STRING->NUMBER

(define-string->number-parser string->number-logic (parse-string))

(define string->number
  ;;Defined  by R6RS.   Convert a  string into  a number;  if successful
  ;;return the number object, else return false.  The string is meant to
  ;;hold a complete number from the first to the last character.
  ;;
  (case-lambda
   ((S)
    (with-arguments-validation (string->number)
	((string S))
      ;;The arguments:
      ;;
      ;;  S (unsafe.string-length S) 0
      ;;
      ;;are  the device-specific arguments  for the  operator functions:
      ;;INPUT.STRING, INPUT.LENGTH, INPUT.INDEX.
      ;;
      ;;The argument 10  is the selected radix, which  can be overridden
      ;;by the prefixes #o, #b, #x, #d in the string S itself.
      ;;
      ;;The first  #f argument  is the override  radix; if the  string S
      ;;starts  with  a  prefix among  #o,  #b,  #x,  #d, the  radix  is
      ;;overridden.   This  argument  exists  for the  only  purpose  of
      ;;checking that radix prefixes are not used multiple times.
      ;;
      ;;The second #f argument is  the exactness specification and it is
      ;;selected by the  prefixes #i and #e.  This  value should be: #f,
      ;;the symbol "e" or the symbol "i".
      ;;
      (parse-string S (unsafe.string-length S) 0 10 #f #f)))
   ((S radix)
    (with-arguments-validation (string->number)
	((string S)
	 (radix	 radix))
      (parse-string S (unsafe.string-length S) 0 radix #f #f)))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put '?device-logic 'scheme-indent-function 1)
;; End:
