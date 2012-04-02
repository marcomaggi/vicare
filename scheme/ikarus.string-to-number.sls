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


;;;; documentation
;;
;;For the  parser constructor see the library  (vicare parser-logic) and
;;its documentation in Texinfo format.
;;


(library (ikarus.string-to-number)
  (export string->number define-string->number-parser)
  (import (except (ikarus)
		  string->number)
    (vicare syntactic-extensions)
    (vicare parser-logic)
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
  (and (fixnum? obj)
       (or (unsafe.fx= obj 10)
	   (unsafe.fx= obj 16)
	   (unsafe.fx= obj 2)
	   (unsafe.fx= obj 8)))
  (assertion-violation who "expected supported radix as argument" obj))


;;;; helpers

(define-inline-constant CHAR-FIXNUM-0	48 #;(char->integer #\0))
(define-inline-constant CHAR-FIXNUM-a	97 #;(char->integer #\a))
(define-inline-constant CHAR-FIXNUM-A	65 #;(char->integer #\A))

(define-syntax sign*accum-with-exactness
  ;;Compose  the number  ?NUMBER-EXPR with  the  sign ?SIGN  (+1 or  -1)
  ;;according to the exactness ?EXACTNESS  (false, the symbol "i" or the
  ;;symbol "e").
  ;;
  ;;If  ?EXACTNESS  is  false  (neither  "#e"  nor  "#i"  prefixes  were
  ;;present): the exactness defaults to "exact".
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?sign ?exactness ?number-expr)
       #'(let ((accum ?number-expr))
	   (* ?sign (if (eq? ?exactness 'i)
			(inexact accum)
		      accum)))))))

(define-syntax sign*accum-with-INexactness
  ;;Compose  the number  ?NUMBER-EXPR with  the  sign ?SIGN  (+1 or  -1)
  ;;according to the exactness ?EXACTNESS  (false, the symbol "i" or the
  ;;symbol "e").
  ;;
  ;;If  ?EXACTNESS  is  false  (neither  "#e"  nor  "#i"  prefixes  were
  ;;present): the exactness defaults to "inexact".
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?sign ?exactness ?number-expr)
       #'(let ((accum ?number-expr))
	   (* ?sign (if (eq? ?exactness 'e)
			accum
		      (inexact accum))))))))

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

(define-syntax %make-number-after-ending-i
  ;;To be called  to finalise parsing of a numeric  string when the last
  ;;parsed character  was the #\i representing the  imaginary unit; this
  ;;means the  number is complex  in rectangular notation.   This syntax
  ;;either returns a complex number or calls ?FAIL.
  ;;
  ;;If ?REALP is  false: the number is imaginary  and the imaginary part
  ;;is ?IMAGP.
  ;;
  ;;If ?REALP  is not false, it  must be a number  representing the real
  ;;part; the imaginary part is ?IMAGP.
  ;;
  ;;In the  case of a  numeric string like  "1@2i", this syntax  is used
  ;;with arguments:
  ;;
  ;;   ?REALP = (polar . 1)
  ;;   ?IMAGP = 2
  ;;
  ;;this syntax detects the fact that ?REALP is a pair and calls ?FAIL.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?fail ?realp ?imagp)
       (and (identifier? #'?fail)
	    (identifier? #'?realp))
       #'(let ((imagp ?imagp))
	   (cond ((not ?realp)
		  (make-rectangular 0 imagp))
		 ((pair? ?realp)
		  ;;It is an error if a complex number in polar notation
		  ;;ends with the character #\i.
		  (?fail))
		 (else
		  (make-rectangular ?realp imagp))))))))

(define-syntax %make-number-non-rectangular
  ;;To be  used to finalise  parsing of a  numeric string when  the last
  ;;parsed character  was NOT the  #\i representing the  imaginary unit;
  ;;this  means the  number  is either  a  real or  a  complex in  polar
  ;;notation.  This syntax returns a real or complex number.
  ;;
  ;;If ?N0 is false: the number is real and is represented by ?N1.
  ;;
  ;;If  ?N0 is  a pair:  the number  is complex  in polar  notation; the
  ;;magnitude is in the cdr of ?N0, the angle is ?N1.  The car of ?N0 is
  ;;the symbol "polar".
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?n0 ?n1)
       (identifier? #'?n0)
       #'(let ((n1 ?n1))
	   (if (not ?n0)
	       n1
	     (make-polar (unsafe.cdr ?n0) n1)))))))


;;;; string to number parser logic
;;
;;There are 4 suggested entry points to this parser for numeric strings:
;;
;;PARSE-NUMERIC-STRING
;;  To be used when: (1) we  know that the next character from the input
;;  starts a numeric string; (2) we have already parsed a numeric string
;;  prefix (like #e, #i,  #x, etc) and we want to parse  the rest of the
;;  string.  In the second case:  the arguments to the operator allow us
;;  to inform the parser about the prefix we have already consumed.
;;
;;U:DIGIT+
;;  To be  used when  we have  already parsed the  first character  of a
;;  numeric string  and such character is  a digit in  the default radix
;;  (which is 10).  The arguments to the operator allow us to inform the
;;  parser about the digit we have already parsed.
;;
;;U:SIGN
;;  To be  used when  we have  already parsed the  first character  of a
;;  numeric string and such character is sign specification, #\+ or #\-.
;;  The arguments  to the operator allow  us to inform  the parser about
;;  the sign we have already parsed.
;;
;;U:DOT
;;  To be  used when  we have  already parsed the  first character  of a
;;  numeric  string and  such character  is  a decimal  dot starting  an
;;  inexact number in radix 10.   The arguments to the operator allow us
;;  to inform the parser about the nature of the expected number.
;;
;;in general, every operator function can  be used as entry point to the
;;parser, provided we use appropriate arguments.
;;
;;The operator functions use the following names for common arguments:
;;
;;* RADIX must be a fixnum among:  2, 8, 10, 16; it represents the radix
;;  in which the numeric sequence expresses the number.  Notice that for
;;  radixes 2, 8, 10 a digit  character is represented by a digit fixnum
;;  in the range: 0 <= digit-fx < RADIX.
;;
;;* EXACTNESS must be false or  the symbol "e" (for exact) or the symbol
;;  "i" (for  inexact).  When set to  false: none of  the prefixes "#e",
;;  "#i" were read from the input.  This argument is changed only if one
;;  of the prefixes "#i", "#e" is found in the input.
;;
;;* SN  must be the fixnum  +1 or -1 and  it represents the  sign of the
;;  number  being parsed.   Notice  that for  exact  rationals only  the
;;  numerator has a sign, the denominator cannot have it.
;;
;;* ACCUM  must be an exact  non-negative integer and  it represents the
;;  absolute  value of  the number  accumulated so  far  from previosuly
;;  parsed digits.  Notice that we parse digits from left to right; when
;;  parsing the numeric string "1234",  with radix 10, we accumulate the
;;  number as follows:
;;
;;    ACCUM = 0
;;    ACCUM = (ACCUM * RADIX) + 1 =   0 * 10 + 1 = 1
;;    ACCUM = (ACCUM * RADIX) + 2 =   1 * 10 + 2 = 12
;;    ACCUM = (ACCUM * RADIX) + 3 =  12 * 10 + 3 = 123
;;    ACCUM = (ACCUM * RADIX) + 4 = 123 * 10 + 4 = 1234
;;
;;* EXPONENT must be an  exact integer representing the decimal exponent
;;  of a number.  Let's say we parse the numeric string "123.456"; using
;;  PARSE-NUMERIC-STRING  and U:DIGIT+ we  accumulate ACCUM=123  and are
;;  left with ".456"; now for each parsed character:
;;
;;    #\. -> ACCUM=123      EXPONENT=0
;;    #\4 -> ACCUM=1234     EXPONENT=-1
;;    #\5 -> ACCUM=12345    EXPONENT=-2
;;    #\6 -> ACCUM=123456   EXPONENT=-3
;;
;;  and to compose ACCUM with EXPONENT we do:
;;
;;    ACCUM * 10^EXPONENT = 123456 * 10^-3 = 123.456
;;
;;* N0 must  be false, a real number  or a pair whose car  is the symbol
;;  "polar"  and whose  cdr is  a real  number.  All  the  operators but
;;  PARSE-NUMERIC-STRING and its subroutine have a N0 argument.
;;
;;  Whenever  the parser  finishes parsing  the real  part of  a complex
;;  number in rectangular notation,  the resulting number becomes the N0
;;  argument to the next operator call.
;;
;;  Whenever  the parser  finishes parsing  the magnitude  of  a complex
;;  number in polar notation, a pair whose car is the symbol "polar" and
;;  whose cdr is the magnitude  number object becomes the N0 argument to
;;  the next operator call.
;;
;;  While the  parser is  parsing a real  number or  the real part  of a
;;  complex number in rectangular notation or the magnitude of a complex
;;  number in polar notation: the N0 argument is false.
;;
;;  At the  end of  a complex  number parsing: the  argument N0  and the
;;  second parsed number, representing  the imaginary part or the angle,
;;  are composed to build the full complex number:
;;
;;    N0=#f                N1=456    -> 456
;;    N0=123               N1=456    -> 123+456i
;;    N0=(polar . 123)     N1=456    -> 123@456
;;

(define-parser-logic define-string->number-parser next fail

  (parse-numeric-string (radix override-radix exactness)
    ;;This  operator is  the  entry  point to  parse  any valid  numeric
    ;;sequence of characters  from the input when we  know that the next
    ;;character starts a numeric sequence.
    ;;
    ;;OVERRIDE-RADIX  must be false  or a  fixnum among:  2, 8,  10, 16.
    ;;When set to  false it means that none of  the prefixes "#b", "#d",
    ;;"#o", "#x"  were read  from the input.   This argument  is changed
    ;;only if one of the prefixes "#b", "#d", "#o", "#x" is found in the
    ;;input.  This argument exists for the only purpose of checking that
    ;;radix prefixes are not used multiple times, for example "#b#x".
    ;;
    ((#\#)
     (next parse-prefix-after-hash radix override-radix exactness))
    ((sign) => sn
     (let-inline ((n0 #f))
       (next u:sign radix n0 exactness sn)))
    ((#\.)
     (if (unsafe.fx= radix 10)
	 (let-inline ((n0 #f)
		      (sn +1))
	   (next u:dot radix n0 exactness sn))
       (fail)))
    ((digit radix) => digit-fx
     (let-inline ((accum digit-fx)
		  (n0    #f)
		  (sn    +1))
       (next u:digit+ radix n0 exactness sn accum))))

  (parse-prefix-after-hash (radix override-radix exactness)
    ;;Parse  an input  char after  the  opening #\#  character has  been
    ;;consumed by PARSE-NUMERIC-STRING; this operator is a subroutine of
    ;;PARSE-NUMERIC-STRING.
    ;;
    ;;We  must  remember that  the  prefixes  "#e"  and "#i"  specifying
    ;;exactness  can be preceeded  or followed  by prefixes  "#b", "#d",
    ;;"#o",  "#x" overriding  the  default  radix.  It  is  an error  to
    ;;specify multiple exactness prefixes or multiple radix prefixes.
    ;;
    ((#\x #\X)
     (if override-radix
	 (fail)
       (next parse-numeric-string 16 16 exactness)))
    ((#\o #\O)
     (if override-radix
	 (fail)
       (next parse-numeric-string 8 8 exactness)))
    ((#\b #\B)
     (if override-radix
	 (fail)
       (next parse-numeric-string 2 2 exactness)))
    ((#\d #\D)
     (if override-radix
	 (fail)
       (next parse-numeric-string 10 10 exactness)))
    ((#\e #\E)
     (if exactness
	 (fail)
       (next parse-numeric-string radix override-radix 'e)))
    ((#\i #\I)
     (if exactness
	 (fail)
       (next parse-numeric-string radix override-radix 'i))))

  (u:digit+ (radix n0 exactness sn accum)
    ;;Accumulate  a number  from digit  characters compliant  with RADIX
    ;;after at least  one digit has been already parsed.   It is used to
    ;;accumulate: exact integers, numerators of exact rationals, flonums
    ;;before  the  decimal  dot;   notice  that  denominators  of  exact
    ;;rationals are not accumulated by this operator.
    ;;
    ((eof)
     ;;The number terminated without an  ending #\i, so this is either a
     ;;real number or a complex  number in polar notation; this means N0
     ;;must be either false or a pair containing the magnitude.
     (if (and n0 (not (pair? n0)))
	 (fail)
       (let-inline ((n1 (sign*accum-with-exactness sn exactness accum)))
	 (%make-number-non-rectangular n0 n1))))
    ((digit radix) => digit-fx
     (let-inline ((accum (+ (* accum radix) digit-fx)))
       (next u:digit+ radix n0 exactness sn accum)))
    ((#\.)
     (if (unsafe.fx= radix 10)
	 (let-inline ((exponent 0))
	   (next u:digit+dot radix n0 exactness sn accum exponent))
       (fail)))
    ((#\/)
     ;;Terminate the numerator of  an exact rational; the next character
     ;;will be part of the denominator.
     (let-inline ((numerator accum))
       (next u:denominator radix n0 exactness sn numerator)))
    ((sign) => sn2
     ;;Terminate  the  real part  of  a  complex  number in  rectangular
     ;;notation and start the imaginary part.
     (if n0
	 (fail)
       (let-inline ((n0 (sign*accum-with-exactness sn exactness accum)))
	 (next u:sign radix n0 exactness sn2))))
    ((#\i)
     ;;Terminate the  imaginary part of  a complex number  in rectangular
     ;;notation.
     (let-inline ((n1 (sign*accum-with-exactness sn exactness accum)))
       (next u:done (%make-number-after-ending-i fail n0 n1))))
    ((#\@)
     ;;Terminate the  magnitude of a  complex number in  polar notation;
     ;;the next character will be part of the angle.
     (if n0
	 (fail)
       (let ((mag (sign*accum-with-exactness sn exactness accum)))
	 (next u:polar radix mag exactness))))
    ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L) ;exponent markers
     ;;Terminate  the  significand  of   an  inexact  number;  the  next
     ;;character will be  part of the exponent.  We  ignore the specific
     ;;exponent because Vicare only has "double" flonums.
     (if (unsafe.fx= radix 10)
	 (let-inline ((exponent 0))
	   (next u:exponent radix n0 exactness sn accum exponent))
       (fail)))
    ((#\|)
     ;;Terminate  an inexact  number with  mantissa width  attached; the
     ;;next character will be part of the mantissa width.
     (let-inline ((n1 (sign*accum-with-exactness sn 'i accum)))
       (next u:mant radix n0 n1 exactness))))

  (u:digit+dot (radix n0 exactness sn accum exponent)
    ;;Accumulate an inexact number  from digit characters compliant with
    ;;RADIX after the decimal dot has been parsed.  At each digit parsed
    ;;after  the dot: the  ACCUM is  accumulated as  in U:DIGIT  and the
    ;;exponent EXPONENT is decremented by 1.
    ;;
    ((eof)
     ;;The number terminated without an  ending #\i, so this is either a
     ;;real number or a complex  number in polar notation; this means N0
     ;;must be either false or a pair containing the magnitude.
     (if (and n0 (not (pair? n0)))
	 (fail)
       (let*-inline ((accum (* accum (expt 10 exponent)))
		     (n1    (sign*accum-with-INexactness sn exactness accum)))
	 (%make-number-non-rectangular n0 n1))))
    ((digit radix) => digit-fx
     (let-inline ((accum    (+ (* accum radix) digit-fx))
		  (exponent (- exponent 1)))
       (next u:digit+dot radix n0 exactness sn accum exponent)))
    ((#\i)
     ;;Terminate the  imaginary part of  a complex number  in rectangular
     ;;notation.
     (let*-inline ((accum (* accum (expt 10 exponent)))
		   (n1    (sign*accum-with-INexactness sn exactness accum)))
       (next u:done (%make-number-after-ending-i fail n0 n1))))
    ((sign) => sn2
     ;;Terminate  the  real part  of  a  complex  number in  rectangular
     ;;notation and start the imaginary part.
     (if n0
	 (fail)
       (let*-inline ((accum (* accum (expt 10 exponent)))
		     (n1    (sign*accum-with-INexactness sn exactness accum)))
	 (next u:sign radix n1 exactness sn2))))
    ((#\@)
     ;;Terminate the  magnitude of a  complex number in  polar notation;
     ;;the next character will be part of the angle.
     (if n0
	 (fail)
       (let*-inline ((accum (* accum (expt 10 exponent)))
		     (mag   (sign*accum-with-INexactness sn exactness accum)))
	 (next u:polar radix mag exactness))))
    ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L) ;exponent markers
     ;;Terminate  the  significand  of   an  inexact  number;  the  next
     ;;character will be  part of the exponent.  We  ignore the specific
     ;;exponent because Vicare only has "double" flonums.
     (if (unsafe.fx= radix 10)
	 (next u:exponent radix n0 exactness sn accum exponent)
       (fail)))
    ((#\|)
     ;;Terminate  an inexact  number with  mantissa width  attached; the
     ;;next character will be part of the mantissa width.
     (let*-inline ((accum (* accum (expt 10 exponent)))
		   (n1    (sign*accum-with-INexactness sn exactness accum)))
       (next u:mant radix n0 n1 exactness))))

  (u:dot (radix n0 exactness sn)
    ;;Start processing digits  after a decimal dot when  the decimal dot
    ;;is NOT preceeded by digits itseld, for example "-.123".
    ;;
    ((digit radix) => digit-fx
     (let-inline ((accum    digit-fx)
		  (exponent -1))
       (next u:digit+dot radix n0 exactness sn accum exponent))))

;;; --------------------------------------------------------------------

  (u:sign (radix n0 exactness sn)
    ;;Parse the first character after a sign character, #\+ or #\-; used
    ;;for: integers,  numerators of exact rationals,  real numbers, real
    ;;and imaginary  parts of  complex numbers in  rectangular notation,
    ;;magnitude and  angle parts of  complex numbers in  polar notation.
    ;;Notice that denominators of exact rationals do not have a number.
    ;;
    ;;Such first character can be: a digit or a decimal dot if it starts
    ;;a "common" number; #\i if the numeric string being parsed is "+i",
    ;;"-i", "+inf.0" or "-inf.0"; #\n if the numeric string being parsed
    ;;is "+nan.0" or "-nan.0".
    ;;
    ((digit radix) => digit-fx
     (let-inline ((accum digit-fx))
       (next u:digit+ radix n0 exactness sn accum)))
    ((#\i)
     (next u:sign-i radix n0 exactness sn))
    ((#\n)
     (next u:sign-n radix n0 exactness))
    ((#\.)
     (if (unsafe.fx= radix 10)
	 (next u:dot radix n0 exactness sn)
       (fail))))

;;; --------------------------------------------------------------------

  (u:denominator (radix n0 exactness sn numerator)
    ;;Start processing the denominator  of an exact rational number.  At
    ;;least one digit is expected.
    ;;
    ;;NUMERATOR is the non-negative integer representing the numerator.
    ;;
    ((digit radix) => digit-fx
     (next u:denominator+ radix n0 exactness sn numerator digit-fx)))

  (u:denominator+ (radix n0 exactness sn numerator accum)
    ;;Continue processing  the denominator  of an exact  rational number
    ;;after the numerator  has been fully parsed and  at least the first
    ;;digit character of the denominator has been accumulated in ACCUM.
    ;;
    ;;NUMERATOR is a non-negative integer representing the numerator.
    ;;
    ((eof)
     (if (zero? accum)
	 (fail)
       (let-inline ((n1 (sign*accum-with-exactness sn exactness (/ numerator accum))))
	 (%make-number-non-rectangular n0 n1))))
    ((digit radix) => digit-fx
     (let-inline ((accum (+ (* accum radix) digit-fx)))
       (next u:denominator+ radix n0 exactness sn numerator accum)))
    ((sign) => sn2
     ;;Terminate  an exact  rational number  being  the real  part of  a
     ;;complex in rectangular notation and start the imaginary part.
     (if (or n0 (zero? accum))
	 (fail)
       (let-inline ((n0 (sign*accum-with-exactness sn exactness (/ numerator accum))))
	 (next u:sign radix n0 exactness sn2))))
    ((#\@)
     ;;Terminate  an exact  rational  number being  the  magnitude of  a
     ;;complex in polar notation and start the angle part.
     (if (or n0 (zero? accum))
	 (fail)
       (let-inline ((mag (sign*accum-with-exactness sn exactness (/ numerator accum))))
	 (next u:polar radix mag exactness))))
    ((#\i)
     ;;Terminate an exact rational number  being the imaginary part of a
     ;;complex in rectangular notation;  after this only end-of-input or
     ;;end-of-number are valid.
     (if (zero? accum)
	 (fail)
       (let-inline ((n1 (sign*accum-with-exactness sn exactness (/ numerator accum))))
	 (next u:done (%make-number-after-ending-i fail n0 n1))))))

;;; --------------------------------------------------------------------

  (u:done (n)
    ;;Terminate parsing  when we know  that the last  consumed character
    ;;terminates the numeric string.  In practice this only happens when
    ;;we  have read  the #\i  that terminates  the imaginary  part  of a
    ;;complex  in  rectangular notation:  we  know  that  after it  only
    ;;end-of-input or end-of-number are valid.
    ;;
    ;;N must  be the number object to  be returned to the  caller of the
    ;;parser.
    ;;
    ((eof) n))

  (u:polar (radix mag exactness)
    ;;Start  parsing  the  angle  part  of a  complex  number  in  polar
    ;;notation; to be called after the #\@ character has been consumed.
    ;;
    ;;MAG  must be  a  real  number representing  the  magnitude of  the
    ;;complex number.
    ;;
    ((digit radix) => digit-fx
     (let-inline ((n0    (cons 'polar mag))
		  (sn    +1)
		  (accum digit-fx))
       (next u:digit+ radix n0 exactness sn accum)))
    ((#\.)
     (if (unsafe.fx= radix 10)
	 (let-inline ((n0 (cons 'polar mag))
		      (sn +1))
	   (next u:dot radix n0 exactness sn))
       (fail)))
    ((sign) => sn
     (let-inline ((n0 (cons 'polar mag)))
       (next u:sign radix n0 exactness sn))))

;;; --------------------------------------------------------------------
;;; exponent of inexact numbers

  (u:exponent (radix n0 exactness sn accum exponent1)
    ;;Parse the first character after  an exponent marker has been read.
    ;;ACCUM  and  EXPONENT1 are  the  results  of  parsing the  previous
    ;;characters  representing  a  flonum.   For example,  when  parsing
    ;;"123.456e+78":
    ;;
    ;;  SN        = +1
    ;;  ACCUM     = 123456
    ;;  EXPONENT1 = -3
    ;;
    ;;and  the  next  characters  from  the  input  will  be  "+78";  we
    ;;accumulate new digits in EXP-SIGN and EXPONENT2:
    ;;
    ;;  EXP-SIGN  = +1
    ;;  EXPONENT2 = 78
    ;;
    ;;and finally compose the number:
    ;;
    ;;  SN * ACCUM * 10^(EXPONENT1 + EXP-SIGN * EXPONENT2)
    ;;
    ;;It  looks easier  (and  probably faster)  to accumulate  EXPONENT2
    ;;separately  rather than  to accumulate  new digits  into EXPONENT1
    ;;keeping track of the sign.
    ;;
    ((digit radix) => digit-fx
     (let-inline ((exponent2 digit-fx)
		  (exp-sign  +1))
       (next u:exponent+digit radix n0 exactness sn accum exponent1 exponent2 exp-sign)))
    ((sign) => sn2
     (next u:exponent+sign radix n0 exactness sn accum exponent1 sn2)))

  (u:exponent+sign (radix n0 exactness sn accum exponent1 exp-sign)
    ;;Parse the first  character after an exponent marker  followed by a
    ;;sign have been parsed; a digit is expected.
    ;;
    ((digit radix) => digit-fx
     (let-inline ((exponent2 digit-fx))
       (next u:exponent+digit radix n0 exactness sn accum exponent1 exponent2 exp-sign))))

  (u:exponent+digit (radix n0 exactness sn accum exponent1 exponent2 exp-sign)
    ;;Parse  a digit  character part  of the  exponential of  an inexact
    ;;number  after   the  exponential  marker,  and   possibly  a  sign
    ;;character, has been parsed.
    ;;
    ((eof)
     ;;The number terminated without an  ending #\i, so this is either a
     ;;real number or a complex  number in polar notation; this means N0
     ;;must be either false or a pair containing the magnitude.
     (if (and n0 (not (pair? n0))) #;(number? n0)
	 (fail)
       (let*-inline ((accum (* accum (expt 10 (+ exponent1 (* exponent2 exp-sign)))))
		     (n1    (sign*accum-with-INexactness sn exactness accum)))
	 (%make-number-non-rectangular n0 n1))))
    ((digit radix) => digit-fx
     (let-inline ((exponent2 (+ (* exponent2 radix) digit-fx)))
       (next u:exponent+digit radix n0 exactness sn accum exponent1 exponent2 exp-sign)))
    ((sign) => sn2
     ;;Terminate  an inexact  number being  the real  part of  a complex
     ;;number in rectangular notation and start the imaginary part.
     (if n0
	 (fail)
       (let*-inline ((accum (* accum (expt 10 (+ exponent1 (* exponent2 exp-sign)))))
		     (n1    (sign*accum-with-INexactness sn exactness accum)))
	 (next u:sign radix n1 exactness sn2))))
    ((#\@)
     ;;Terminate an inexact number being the magnitude part of a complex
     ;;number in polar notation and start the angle part.
     (if n0
	 (fail)
       (let*-inline ((accum (* accum (expt 10 (+ exponent1 (* exponent2 exp-sign)))))
		     (mag   (sign*accum-with-INexactness sn exactness accum)))
	 (next u:polar radix mag exactness))))
    ((#\i)
     ;;Terminate an inexact number being the imaginary part of a complex
     ;;number in rectangular notation; this must also end the number.
     (let*-inline ((accum (* accum (expt 10 (+ exponent1 (* exponent2 exp-sign)))))
		   (n1    (sign*accum-with-INexactness sn exactness accum)))
       (next u:done (%make-number-after-ending-i fail n0 n1))))
    ((#\|)
     ;;Terminate an  inexact number with a mantissa  width attached; the
     ;;next character will be part of the mantissa width.
     (let*-inline ((accum (* accum (expt 10 (+ exponent1 (* exponent2 exp-sign)))))
		   (n1    (sign*accum-with-INexactness sn exactness accum)))
       (next u:mant radix n0 n1 exactness))))

;;; --------------------------------------------------------------------
;;; mantissa width of inexact numbers

  (u:mant (radix n0 n1 exactness)
    ;;Parse  the first  character  of  a mantissa  width  after the  #\|
    ;;character  has been  parsed; a  digit is  expected.  The  digit is
    ;;discarded  because Vicare  does not  support inexact  numbers with
    ;;mantissa width specification.
    ;;
    ((digit radix) => digit-fx
     (next u:mant+ radix n0 n1 exactness)))

  (u:mant+ (radix n0 n1 exactness)
    ;;Parse  the  next character  of  a  mantissa  width after  the  #\|
    ;;character  and at  least  one digit  character  have been  parsed.
    ;;Digits  are  discarded because  Vicare  does  not support  inexact
    ;;numbers with mantissa width specification.
    ;;
    ((eof)
     (%make-number-non-rectangular n0 n1))
    ((digit radix) => digit-fx
     (next u:mant+ radix n0 n1 exactness))
    ((sign) => sn2
     ;;Terminate an inexact number,  with mantissa width, being the real
     ;;part of  a complex number  in rectangular notation and  start the
     ;;imaginary part.
     (if n0
	 (fail)
       (next u:sign radix n1 exactness sn2)))
    ((#\@)
     ;;Terminate  an  inexact number,  with  mantissa  width, being  the
     ;;magnitude part  of a complex  number in polar notation  and start
     ;;the angle part.
     (if n0
	 (fail)
       (next u:polar radix n1 exactness)))
    ((#\i)
     ;;Terminate  an  inexact number,  with  mantissa  width, being  the
     ;;imaginary part of a  complex number in rectangular notation; this
     ;;must also end the number.
     (if (pair? n0)
	 (fail)
       (next u:done (%make-number-after-ending-i fail n0 n1)))))

;;; --------------------------------------------------------------------
;;; parsing after a sign character and the #\i character

  (u:sign-i (radix n0 exactness sn)
    ;;Parse the first character after the numeric sequence "+i" or "-i".
    ;;This operator is a subroutine of U:SIGN.
    ;;
    ((eof)
     (let-inline ((n1 (sign*accum-with-exactness sn exactness 1)))
       (%make-number-after-ending-i fail n0 n1)))
    ((#\n)
     ;;After  parsing the  #\n we  already know  that the  only possible
     ;;numbers  are  +inf.0 and  -inf.0,  and  since  we know  the  sign
     ;;already, we can compute N1 right now.
     ;;
     (let-inline ((n1 (* sn +inf.0)))
       (next u:sign-in radix n0 n1 exactness))))

  (u:sign-in (radix n0 n1 exactness)
    ;;Parse  the first  character after  the numeric  sequence  "+in" or
    ;;"-in".   N1 is  the infinity  being  parsed.  This  operator is  a
    ;;subroutine of U:SIGN-I.
    ;;
    ((#\f)
     (next u:sign-inf radix n0 n1 exactness)))

  (u:sign-inf (radix n0 n1 exactness)
    ;;Parse  the first character  after the  numeric sequence  "+inf" or
    ;;"-inf".   N1 is  the infinity  being parsed.   This operator  is a
    ;;subroutine of U:SIGN-IN.
    ;;
    ((#\.)
     (next u:sign-inf. radix n0 n1 exactness)))

  (u:sign-inf. (radix n0 n1 exactness)
    ;;Parse the  first character after  the numeric sequence  "+inf." or
    ;;"-inf.".  N1  is the  infinity being parsed.   This operator  is a
    ;;subroutine of U:SIGN-INF.
    ;;
    ((#\0)
     (next u:sign-inf.0 radix n0 n1 exactness)))

  (u:sign-inf.0 (radix n0 n1 exactness)
    ;;Parse the  first character after the numeric  sequence "+inf.0" or
    ;;"-inf.0".  N1  is the infinity  being parsed.  This operator  is a
    ;;subroutine of "u:sign-inf.".
    ;;
    ;;N1 is the parse infinity number +inf.0 or -inf.0.
    ;;
    ((eof)
     (%make-number-non-rectangular n0 n1))
    ((sign) => sn2
     ;;Terminate  the  real part  of  a  complex  number in  rectangular
     ;;notation  (with  the real  part  being  infinity)  and start  the
     ;;imaginary part.
     ;;
     (if n0
	 (fail)
       (next u:sign radix n1 exactness sn2)))
    ((#\@)
     ;;Terminate  the  magnitude  part  of  a complex  number  in  polar
     ;;notation (with the magnitude  being infinity) and start the angle
     ;;part.
     ;;
     (if n0
	 (fail)
       (next u:polar radix n1 exactness)))
    ((#\i)
     ;;Terminate the  imaginary part of a complex  number in rectangular
     ;;notation (with the imaginary part being infinity); this also ends
     ;;the number.
     ;;
     (next u:done (%make-number-after-ending-i fail n0 n1))))

;;; --------------------------------------------------------------------

  (u:sign-n (radix n0 exactness)
    ;;Parse the first character after the numeric sequence "+n" or "-n".
    ;;This operator is a subroutine of "u:sign".
    ;;
    ((#\a)
     (next u:sign-na radix n0 exactness)))

  (u:sign-na (radix n0 exactness)
    ;;Parse  the first  character after  the numeric  sequence  "+na" or
    ;;"-na".  This operator is a subroutine of "u:sign-n".
    ;;
    ((#\n)
     (next u:sign-nan radix n0 exactness)))

  (u:sign-nan (radix n0 exactness)
    ;;Parse  the first character  after the  numeric sequence  "+nan" or
    ;;"-nan".  This operator is a subroutine of "u:sign-na".
    ;;
    ((#\.)
     (next u:sign-nan. radix n0 exactness)))

  (u:sign-nan. (radix n0 exactness)
    ;;Parse the  first character after  the numeric sequence  "+nan." or
    ;;"-nan.".  This operator is a subroutine of "u:sign-nan".
    ;;
    ((#\0)
     (next u:sign-nan.0 radix n0 exactness)))

  (u:sign-nan.0 (radix n0 exactness)
    ;;Parse the  first character after the numeric  sequence "+nan.0" or
    ;;"-nan.0".  This operator is a subroutine of "u:sign-nan.".
    ;;
    ((eof)
     (%make-number-non-rectangular n0 +nan.0))
    ((sign) => sn2
     ;;Terminate  the  real part  of  a  complex  number in  rectangular
     ;;notation (with the  real part being NaN) and  start the imaginary
     ;;part.
     (if n0
	 (fail)
       (let-inline ((n1 +nan.0))
	 (next u:sign radix n1 exactness sn2))))
    ((#\@)
     ;;Terminate  the  magnitude  part  of  a complex  number  in  polar
     ;;notation (with the magnitude being NaN) and start the angle part.
     (if n0
	 (fail)
       (let-inline ((n1 +nan.0))
	 (next u:polar radix n1 exactness))))
    ((#\i)
     ;;Terminate the  imaginary part of a complex  number in rectangular
     ;;notation (with the imaginary part  being NaN); this must also end
     ;;the numeric string.
     (next u:done
	   (let-inline ((n1 +nan.0))
	     (%make-number-after-ending-i fail n0 n1)))))

  #| end of DEFINE-PARSER-LOGIC |# )


;;;; device logic for STRING->NUMBER

(define-syntax string->number-logic
  ;;Define  the device logic  to parse  a numeric  string from  a Scheme
  ;;string object.
  ;;
  (syntax-rules (:introduce-device-arguments
		 :generate-eof-then-chars-tests
		 :unexpected-eof-error
		 :generate-delimiter-test
		 :invalid-input-char)

    ;;Introduce a list of identifiers used as device-specific arguments;
    ;;they  will  be  the  first  arguments  for  each  parser  operator
    ;;function.
    ((_ :introduce-device-arguments ?kont . ?rest)
     (?kont (input.string input.length input.index) . ?rest))

    ;;Whenever  an  input  character  is  not accepted  by  an  operator
    ;;function  this   rule  is  used   to  decide  what  to   do.   For
    ;;STRING->NUMBER the action is to return false.
    ((_ :invalid-input-char (?input.string ?input.length ?input.index) ?ch-var)
     #f)

    ;;Whenever the  end-of-input is found in  a position in  which it is
    ;;unexpected,  this  rule  is  used  to  decide  what  to  do.   For
    ;;STRING->NUMBER the action is to return false.
    ((_ :unexpected-eof-error (?input.string ?input.length ?input.index))
     #f)

    ;;This rule is  used for input devices for  which the numeric string
    ;;is embedded into a sequence of other characters, so there exists a
    ;;set  of characters  that  delimit the  end-of-number.  The  parser
    ;;delegates  to  the  device  the responsibility  of  knowing  which
    ;;characters are delimiters, if any.
    ;;
    ;;When the input  device is a string containing  only the number, as
    ;;is  the case  for  STRING->NUMBER: there  are  no delimiters,  the
    ;;end-of-number  is the  end of  the  string.  We  avoid looking  at
    ;;?CH-VAR and just expand to the not-delimiter continuation form.
    ((_ :generate-delimiter-test ?ch-var ?ch-is-delimiter-kont ?ch-is-not-delimiter-kont)
     ?ch-is-not-delimiter-kont)

    ;;This rule is used to  generate the tests for an operator function.
    ;;First  of all  the  end-of-input condition  is  checked; then  the
    ;;continuation form for more characters is expanded.
    ((_ :generate-eof-then-chars-tests ?ch-var ?next ?fail
	(?input.string ?input.length ?input.index)
	?end-of-input-kont ?parse-input-char-kont)
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
	   ?parse-input-char-kont))))
    ))


;;;; definition of STRING->NUMBER

(define-string->number-parser string->number-logic
  (parse-numeric-string))

(define string->number
  ;;Defined  by R6RS.   Convert a  string into  a number;  if successful
  ;;return the number object, else return false.  The string is meant to
  ;;hold a complete number from the first to the last character.
  ;;
  (case-lambda
   ((S)
    (define who 'string->number)
    (with-arguments-validation (who)
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
      ;;The first #f argument is the override radix; if the string S has
      ;;a prefix  among #o, #b, #x,  #d, the radix  is overridden.  This
      ;;argument  exists for  the only  purpose of  checking  that radix
      ;;prefixes are not used multiple times.
      ;;
      ;;The second #f argument is  the exactness specification and it is
      ;;selected by the  prefixes #i and #e.  This  value should be: #f,
      ;;the symbol "e" or the symbol "i".
      ;;
      (parse-numeric-string S (unsafe.string-length S) 0
			    10 #f #f)))
   ((S radix)
    (define who 'string->number)
    (with-arguments-validation (who)
	((string S)
	 (radix	 radix))
      (parse-numeric-string S (unsafe.string-length S) 0
			    radix #f #f)))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'parse-numeric-string	'scheme-indent-function 1)
;;eval: (put 'parse-prefix-after-hash	'scheme-indent-function 1)
;;eval: (put 'u:done		'scheme-indent-function 1)
;;eval: (put 'u:sign		'scheme-indent-function 1)
;;eval: (put 'u:sign-i		'scheme-indent-function 1)
;;eval: (put 'u:sign-in		'scheme-indent-function 1)
;;eval: (put 'u:sign-inf	'scheme-indent-function 1)
;;eval: (put 'u:sign-inf.	'scheme-indent-function 1)
;;eval: (put 'u:sign-inf.0	'scheme-indent-function 1)
;;eval: (put 'u:sign-n		'scheme-indent-function 1)
;;eval: (put 'u:sign-na		'scheme-indent-function 1)
;;eval: (put 'u:sign-nan	'scheme-indent-function 1)
;;eval: (put 'u:sign-nan.	'scheme-indent-function 1)
;;eval: (put 'u:sign-nan.0	'scheme-indent-function 1)
;;eval: (put 'u:mant		'scheme-indent-function 1)
;;eval: (put 'u:mant+		'scheme-indent-function 1)
;;eval: (put 'u:polar		'scheme-indent-function 1)
;;eval: (put 'u:digit		'scheme-indent-function 1)
;;eval: (put 'u:digit+		'scheme-indent-function 1)
;;eval: (put 'u:digit+dot	'scheme-indent-function 1)
;;eval: (put 'u:dot		'scheme-indent-function 1)
;;eval: (put 'u:denominator	'scheme-indent-function 1)
;;eval: (put 'u:denominator+	'scheme-indent-function 1)
;;eval: (put 'u:exponent	'scheme-indent-function 1)
;;eval: (put 'u:exponent+sign	'scheme-indent-function 1)
;;eval: (put 'u:exponent+digit	'scheme-indent-function 1)
;;eval: (put 'let-inline	'scheme-indent-function 1)
;;eval: (put 'let*-inline	'scheme-indent-function 1)
;;End:
