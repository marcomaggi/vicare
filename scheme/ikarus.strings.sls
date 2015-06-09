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


#!vicare
(library (ikarus strings)
  (export
    make-string			string
    substring			string-length
    string-empty?
    string-ref			string-set!
    string->list		list->string
    string-append		string-for-each
    string-copy			string-copy!
    string-fill!
    string=?			string!=?
    string<?			string<=?
    string>?			string>=?
    string-min			string-max
    string-concatenate
    string-reverse-and-concatenate
    uuid

    list-of-strings?

    ;; Vicare specific
    string-hex->bytevector	bytevector->string-hex
    bytevector->hex		hex->bytevector

    string-base64->bytevector	bytevector->string-base64
    bytevector->base64		base64->bytevector

    string->uri-encoding	uri-encoding->string
    uri-encode			uri-decode
    normalise-uri-encoding
    uri-encoded-bytevector?	uri-encoded-string?

    (rename (string->uri-encoding	string->percent-encoding)
	    (uri-encoding->string	percent-encoding->string)
	    (uri-encode			percent-encode)
	    (uri-decode			percent-decode)
	    (normalise-uri-encoding	normalise-percent-encoding)
	    (uri-encoded-bytevector?	percent-encoded-bytevector?)
	    (uri-encoded-string?	percent-encoded-string?))

    ;; unsafe operations
    $string			$string-empty?
    $string=			$string!=
    $string<			$string>
    $string<=			$string>=
    $string-min			$string-max
    $string-total-length
    $substring
    $string-copy		$string-copy!
    $string-concatenate		$string-reverse-and-concatenate
    $string-copy!/count
    $string-self-copy-forwards!/count
    $string-self-copy-backwards!/count
    $string-fill!

    $string-base64->bytevector	$bytevector->string-base64
    $bytevector->base64		$base64->bytevector

    $uri-encode			$uri-decode
    $normalise-uri-encoding
    $uri-encoded-bytevector?	$uri-encoded-string?
    (rename ($uri-encode		$percent-encode)
	    ($uri-decode		$percent-decode)
	    ($normalise-uri-encoding	$percent-normalise-encoding)
	    ($uri-encoded-bytevector?	$percent-encoded-bytevector?)
	    ($uri-encoded-string?	$percent-encoded-string?))
    #| end of export |# )
  (import (except (vicare)
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Wed May 6, 2015)
		  procedure-arguments-consistency-violation
		  non-negative-fixnum?
		  list-of-chars?
		  ;;;

		  make-string			string
		  substring			string-length
		  string-empty?
		  string-ref			string-set!
		  string->list			list->string
		  string-append			string-for-each
		  string-copy			string-copy!
		  string-fill!
		  string=?			string!=?
		  string<?			string<=?
		  string>?			string>=?
		  string-min			string-max
		  string-concatenate
		  string-reverse-and-concatenate
		  uuid

		  list-of-strings?

		  ;; Vicare specific
		  bytevector->hex		hex->bytevector
		  string-hex->bytevector	bytevector->string-hex
		  string-base64->bytevector	bytevector->string-base64
		  bytevector->base64		base64->bytevector

		  string->uri-encoding		uri-encoding->string
		  string->percent-encoding	percent-encoding->string
		  uri-encode			percent-encode
		  uri-decode			percent-decode
		  normalise-uri-encoding	normalise-percent-encoding
		  uri-encoded-bytevector?	percent-encoded-bytevector?
		  uri-encoded-string?		percent-encoded-string?
		  #| end of except |# )
    ;;NOTE Let's try  to import unsafe operations only from  built-in libraries, when
    ;;possible, avoiding the use of external libraries of macros.
    (except (vicare system $fx)
	    $fx<=
	    ;;FIXME This  except must  be removed  at the  next boot  image rotation.
	    ;;(Marco Maggi; Fri Mar 27, 2015)
	    $fx!=)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Mar 27,
    ;;2015)
    (only (ikarus fixnums)
	  $fx!=)
    (vicare system $pairs)
    (only (vicare system $chars)
	  $char=
	  $char<
	  $char->fixnum
	  $fixnum->char
	  ;;FIXME This  is to be  included at the  next boot image  rotation.  (Marco
	  ;;Maggi; Fri Mar 27, 2015)
	  #;$char!=
	  )
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Mar 27,
    ;;2015)
    (only (ikarus chars)
	  $char!=)
    (only (vicare system $vectors)
	  $vector-ref)
    (only (vicare system $bytevectors)
	  $make-bytevector
	  $bytevector-length
	  $bytevector-set!
	  $bytevector-u8-ref)
    (only (vicare system $strings)
	  $make-string
	  $string-length
	  $string-ref
	  $string-set!
	  $ascii->string
	  $string->ascii)
    (only (vicare unsafe operations)
	  $fx<=
	  $fxincr!)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Mon Mar 23,
    ;;2015)
    (only (ikarus chars)
	  list-of-chars?)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Sat Mar 21,
    ;;2015)
    (only (ikarus fixnums)
	  non-negative-fixnum?)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate))


;;;; arguments validation

;;FIXME To  be removed at  the next  boot image rotation.   (Marco Maggi; Wed  May 6,
;;2015)
;;
(define procedure-arguments-consistency-violation
  assertion-violation)

(define string-index?	non-negative-fixnum?)
(define string-length?	non-negative-fixnum?)

(module (assert-index-for-string)

  (define-syntax (assert-index-for-string stx)
    (syntax-case stx ()
      ((_ ?str ?idx)
       (and (identifier? #'?str)
	    (identifier? #'?idx))
       #'(unless (%index-for-string ?str ?idx)
	   (procedure-arguments-consistency-violation __who__ "string index out of range" ?str ?idx)))
      ))

  (define (%index-for-string str idx)
    ($fx< idx ($string-length str)))

  #| end of module |# )

(define-syntax (assert-total-string-length stx)
  (syntax-case stx ()
    ((_ ?len)
     (identifier? #'?len)
     #'(unless (string-length? ?len)
	 (procedure-arguments-consistency-violation __who__ "total resulting string length out of range, expected non-negative fixnum" ?len)))
    ))

(define-syntax (assert-start-index-for-string stx)
  (syntax-case stx ()
    ((_ ?start ?len)
     (and (identifier? #'?start)
	  (identifier? #'?len))
     #'(unless ($fx<= ?start ?len)
	 (procedure-arguments-consistency-violation __who__ "start index out of range" ?start ?len)))
    ))

(define-syntax (assert-end-index-for-string stx)
  (syntax-case stx ()
    ((_ ?end ?len)
     (and (identifier? #'?end)
	  (identifier? #'?len))
     #'(unless ($fx<= ?end ?len)
	 (procedure-arguments-consistency-violation __who__ "end index out of range" ?end ?len)))
    ))

(define-syntax (assert-start/end-indexes-for-string stx)
  (syntax-case stx ()
    ((_ ?start ?end ?len)
     (and (identifier? #'?start)
	  (identifier? #'?end)
	  (identifier? #'?len))
     #'(begin
	 (assert-start-index-for-string ?start ?len)
	 (assert-end-index-for-string   ?end   ?len)
	 (unless ($fx<= ?start ?end)
	   (procedure-arguments-consistency-violation __who__ "start index less than end index" ?start ?end))))
    ))

(define-syntax (assert-start-index-and-count-for-string stx)
  (syntax-case stx ()
    ((_ ?str ?start ?count ?len)
     (and (identifier? #'?start)
	  (identifier? #'?count)
	  (identifier? #'?len))
     #'(begin
	 (assert-start-index-for-string ?start ?len)
	 (let ((end (+ ?start ?count)))
	   (unless (string-index? end)
	     (procedure-arguments-consistency-violation __who__
	       "count of characters out of range for string and start index" ?str ?start ?count)))))
    ))

(define-syntax (assert-char stx)
  (syntax-case stx ()
    ((_ ?ch)
     (identifier? #'?ch)
     #'(unless (char? ?ch)
	 (procedure-argument-violation __who__ "expected character as argument" ?ch)))
    ))

(define-syntax (assert-equal-length-strings stx)
  (syntax-case stx ()
    ((_ ?str*)
     #'(let* ((str* ?str*)
	      (len* (map (lambda (S)
			   ($string-length S))
		      str*)))
	 (unless (apply = len*)
	   (procedure-arguments-consistency-violation __who__ "expected strings with equal length" len* str*))))
    ))


;;;; helpers

(define ($string-last-index str)
  ;;To be called only if BV is not empty!!!
  ($fxsub1 ($string-length str)))


;;;; predicates

(define-list-of-type-predicate list-of-strings? string?)


(define* (string-length {str string?})
  ;;Defined by R6RS.   Return the number of  characters in the given STR  as an exact
  ;;integer object.
  ;;
  ($string-length str))

(define* (string-ref {str string?} {idx string-index?})
  ;;Defined by  R6RS.  IDX must  be a  valid index of  STR.  Return the  character at
  ;;offset IDX of STR using zero-origin indexing.
  ;;
  ;;NOTE Implementors should make STRING-REF run in constant time.
  ;;
  (assert-index-for-string str idx)
  ($string-ref str idx))

(define* (string-set! {str string?} {idx string-index?} {ch char?})
  ;;Defined by R6RS.  IDX  must be a valid index of STR.  Store  CH in element IDX of
  ;;STR and return unspecified values.
  ;;
  ;;Passing  an  immutable string  to  STRING-SET!  should  cause an  exception  with
  ;;condition type @condition{assertion} to be raised.
  ;;
  ;;NOTE Implementors should make STRING-SET!  run in constant time.
  ;;
  (assert-index-for-string str idx)
  ($string-set! str idx ch))


;;;; constructors

(case-define* make-string
  ;;Defined by  R6RS.  Return  a newly allocated  string of length  LEN.  If  FILL is
  ;;given, then  all elements of  the string are  initialized to FILL,  otherwise the
  ;;contents of the string are unspecified.
  ;;
  (({len string-length?})
   (make-string len #\x0))
  (({len string-length?} {fill char?})
   (let loop ((str ($make-string len))
	      (idx 0)
	      (len len))
     (if ($fx< idx len)
	 (begin
	   ($string-set! str idx fill)
	   (loop str ($fxadd1 idx) len))
       str))))

(case-define* string
  ;;Defined by R6RS.  Return a newly allocated string composed of the arguments.
  ;;
  (()
   ($make-string 0))

  (({one char?})
   (receive-and-return (str)
       ($make-string 1)
     ($string-set! str 0 one)))

  (({one char?} {two char?})
   (receive-and-return (str)
       ($make-string 2)
     ($string-set! str 0 one)
     ($string-set! str 1 two)))

  (({one char?} {two char?} {three char?})
   (receive-and-return (str)
       ($make-string 3)
     ($string-set! str 0 one)
     ($string-set! str 1 two)
     ($string-set! str 2 three)))

  (({one char?} {two char?} {three char?} {four char?})
   (receive-and-return (str)
       ($make-string 4)
     ($string-set! str 0 one)
     ($string-set! str 1 two)
     ($string-set! str 2 three)
     ($string-set! str 3 four)))

  (({one char?} {two char?} {three char?} {four char?} {five char?} . {char* list-of-chars?})
   (define len
     (+ 5 (length char*)))
   (assert-total-string-length len)
   (let ((str ($make-string len)))
     ($string-set! str 0 one)
     ($string-set! str 1 two)
     ($string-set! str 2 three)
     ($string-set! str 3 four)
     ($string-set! str 4 five)
     (let loop ((str   str)
		(idx   5)
		(char* char*))
       (if (pair? char*)
	   (begin
	     ($string-set! str idx ($car char*))
	     (loop str ($fxadd1 idx) ($cdr char*)))
	 str)))))

;;; --------------------------------------------------------------------

(case-define* $string
  ;;Defined by R6RS.  Return a newly allocated string composed of the arguments.
  ;;
  (()
   ($make-string 0))

  ((one)
   (receive-and-return (str)
       ($make-string 1)
     ($string-set! str 0 one)))

  ((one two)
   (receive-and-return (str)
       ($make-string 2)
     ($string-set! str 0 one)
     ($string-set! str 1 two)))

  ((one two three)
   (receive-and-return (str)
       ($make-string 3)
     ($string-set! str 0 one)
     ($string-set! str 1 two)
     ($string-set! str 2 three)))

  ((one two three four)
   (receive-and-return (str)
       ($make-string 4)
     ($string-set! str 0 one)
     ($string-set! str 1 two)
     ($string-set! str 2 three)
     ($string-set! str 3 four)))

  ((one two three four five . char*)
   (define len
     (+ 5 (length char*)))
   (let ((str ($make-string len)))
     ($string-set! str 0 one)
     ($string-set! str 1 two)
     ($string-set! str 2 three)
     ($string-set! str 3 four)
     ($string-set! str 4 five)
     (let loop ((str   str)
		(idx   5)
		(char* char*))
       (if (pair? char*)
	   (begin
	     ($string-set! str idx ($car char*))
	     (loop str ($fxadd1 idx) ($cdr char*)))
	 str)))))


;;;; copying

(define* (substring {str string?} {start string-index?} {end string-index?})
  ;;Defined by R6RS.  STR  must be a string, and START and END  must be exact integer
  ;;objects satisfying:
  ;;
  ;;   0 <= START <= END <= (string-length STR)
  ;;
  ;;Return a newly allocated string formed  from the characters of STR beginning with
  ;;index START (inclusive) and ending with index END (exclusive).
  ;;
  (let ((len ($string-length str)))
    (assert-start/end-indexes-for-string start end len)
    ($substring str start end)))

(define ($substring str start end)
  ;;Return a  new string holding  characters from STR  from START (inclusive)  to END
  ;;(exclusive).
  ;;
  (let ((dst.len ($fx- end start)))
    (if ($fx< 0 dst.len)
	(receive-and-return (dst.str)
	    ($make-string dst.len)
	  ($string-copy! str start dst.str 0 end))
      (string))))

;;; --------------------------------------------------------------------

(define* (string-copy {str string?})
  ;;Defined by R6RS.  Return a newly allocated copy of the given STR.
  ;;
  ($string-copy str))

(define ($string-copy str)
  ($substring str 0 ($string-length str)))


;;;; string comparison
;;
;;Defined by R6RS.   These procedures are the lexicographic extensions  to strings of
;;the  corresponding  orderings  on  characters.    For  example,  STRING<?   is  the
;;lexicographic ordering  on strings induced  by the ordering CHAR<?   on characters.
;;If two strings  differ in length but are  the same up to the length  of the shorter
;;string, the  shorter string  is considered  to be  lexicographically less  than the
;;longer string.
;;

(define-equality/sorting-predicate string=?	$string=	string?)
(define-equality/sorting-predicate string<?	$string<	string?)
(define-equality/sorting-predicate string<=?	$string<=	string?)
(define-equality/sorting-predicate string>?	$string>	string?)
(define-equality/sorting-predicate string>=?	$string>=	string?)
(define-inequality-predicate       string!=?	$string!=	string?)

;;; --------------------------------------------------------------------

(define ($string= str1 str2)
  (or (eq? str1 str2)
      (let ((len ($string-length str1)))
	(and ($fx= len ($string-length str2))
	     (let loop ((idx  0) (len  len))
	       (or ($fx= idx len)
		   (and ($char= ($string-ref str1 idx)
				($string-ref str2 idx))
			(loop ($fxadd1 idx) len))))))))

(define ($string!= str1 str2)
  (not ($string= str1 str2)))

(define ($string< str1 str2)
  (if (eq? str1 str2)
      #f
    (let ((len1 ($string-length str1))
	  (len2 ($string-length str2)))
      (if ($fx< len1 len2)
	  (let next-char ((idx  0)
			  (len1 len1)
			  (str1 str1)
			  (str2 str2))
	    (or ($fx= idx len1)
		(let ((ch1 ($string-ref str1 idx))
		      (ch2 ($string-ref str2 idx)))
		  (or ($char< ch1 ch2)
		      (if ($char= ch1 ch2)
			  (next-char ($fxadd1 idx) len1 str1 str2)
			#f)))))
	(let next-char ((idx  0)
			(len2 len2)
			(str1 str1)
			(str2 str2))
	  (if ($fx= idx len2)
	      #f
	    (let ((ch1 ($string-ref str1 idx))
		  (ch2 ($string-ref str2 idx)))
	      (or ($char< ch1 ch2)
		  (if ($char= ch1 ch2)
		      (next-char ($fxadd1 idx) len2 str1 str2)
		    #f)))))))))

(define ($string<= str1 str2)
  (or (eq? str1 str2)
      (let ((len1 ($string-length str1))
	    (len2 ($string-length str2)))
	(if ($fx<= len1 len2)
	    (let next-char ((idx  0)
			    (len1 len1)
			    (str1 str1)
			    (str2 str2))
	      (or ($fx= idx len1)
		  (let ((ch1 ($string-ref str1 idx))
			(ch2 ($string-ref str2 idx)))
		    (or ($char< ch1 ch2)
			(if ($char= ch1 ch2)
			    (next-char ($fxadd1 idx) len1 str1 str2)
			  #f)))))
	  (let next-char ((idx  0)
			  (len2 len2)
			  (str1 str1)
			  (str2 str2))
	    (if ($fx= idx len2)
		#f
	      (let ((ch1 ($string-ref str1 idx))
		    (ch2 ($string-ref str2 idx)))
		(or ($char< ch1 ch2)
		    (if ($char= ch1 ch2)
			(next-char ($fxadd1 idx) len2 str1 str2)
		      #f)))))))))

(define ($string> str1 str2)
  ($string< str2 str1))

(define ($string>= str1 str2)
  ($string<= str2 str1))


;;;; min max

(define-min/max-comparison string-max $string-max string?)
(define-min/max-comparison string-min $string-min string?)

(define ($string-min str1 str2)
  (if ($string< str1 str2) str1 str2))

(define ($string-max str1 str2)
  (if ($string< str1 str2) str2 str1))


;;;; list conversion

(define* (string->list {str string?})
  ;;Defined by R6RS.   Return a newly allocated  list of the characters  that make up
  ;;the given string.
  ;;
  (let next-char ((str str)
		  (i   ($string-length str))
		  (ac  '()))
    (if ($fxzero? i)
	ac
      (let ((i ($fxsub1 i)))
	(next-char str i (cons ($string-ref str i) ac))))))

(define* (list->string ls)
  ;;Defined by R6RS.   Return a newly allocated string formed  from the characters in
  ;;LS.
  ;;
  (define (race h t ls n)
    (cond ((pair? h)
	   (let ((h ($cdr h)))
	     (if (pair? h)
		 (if (not (eq? h t))
		     (race ($cdr h) ($cdr t) ls ($fx+ n 2))
		   (procedure-argument-violation __who__ "circular list is invalid as argument" ls))
	       (if (null? h)
		   ($fx+ n 1)
		 (procedure-argument-violation __who__ "expected proper list as argument" ls)))))
	  ((null? h)
	   n)
	  (else
	   (procedure-argument-violation __who__ "expected proper list as argument" ls))))

  (define (fill s i ls)
    (if (null? ls)
	s
      (let ((c ($car ls)))
	(assert-char c)
	($string-set! s i c)
	(fill s ($fxadd1 i) ($cdr ls)))))

  (let ((len (race ls ls ls 0)))
    (assert-total-string-length len)
    (fill ($make-string len) 0 ls)))


(case-define* string-append
  ;;Defined  by R6RS.   Return a  newly allocated  string whose  characters form  the
  ;;concatenation of the given strings.
  ;;
  (()
   ;;It must be a newly allocated string.
   (string))

  (({str string?})
   ;;It must be a newly allocated string.
   ($string-copy str))

  (({str1 string?} {str2 string?})
   (let* ((len1		($string-length str1))
	  (len2		($string-length str2))
	  (dst.len	(+ len1 len2)))
     (assert-total-string-length dst.len)
     (receive-and-return (dst.str)
	 ($make-string dst.len)
       ($string-copy! str1 0 dst.str 0    len1)
       ($string-copy! str2 0 dst.str len1 len2))))

  (({str1 string?} {str2 string?} {str3 string?})
   (let* ((len1		($string-length str1))
	  (len2		($string-length str2))
	  (len3		($string-length str3))
	  (dst.len	(+ len1 len2 len3)))
     (assert-total-string-length dst.len)
     (receive-and-return (dst.str)
	 ($make-string dst.len)
       ($string-copy! str1 0 dst.str 0    len1)
       ($string-copy! str2 0 dst.str len1 len2)
       ($string-copy! str3 0 dst.str ($fx+ len1 len2) len3))))

  (({str1 string?} {str2 string?} {str3 string?} {str4 string?} . str*)

   (define (%compute-total-string-length len str*)
     (if (pair? str*)
	 (%compute-total-string-length (+ len ($string-length (car str*)))
				       (cdr str*))
       len))

   (define (%fill-strings dst.str str* dst.start)
     (if (pair? str*)
	 (let* ((src.str ($car str*))
		(src.len ($string-length src.str)))
	   (begin
	     ($string-copy! src.str 0 dst.str dst.start src.len)
	     (%fill-strings dst.str ($cdr str*) ($fx+ dst.start src.len))))
       dst.str))

   ;;FIXME At the  next boot image rotation: this validation  must be integrated with
   ;;the signature.  (Marco Maggi; Sun May 10, 2015)
   (for-each (lambda (item)
	       (unless (string? item)
		 (procedure-argument-violation __who__ "failed argument validation" item)))
     str*)
   (let* ((len1		($string-length str1))
	  (len2		($string-length str2))
	  (len3		($string-length str3))
	  (len4		($string-length str4))
	  (dst.len	(%compute-total-string-length (+ len1 len2 len3 len4) str*)))
     (assert-total-string-length dst.len)
     (let ((dst.str ($make-string dst.len)))
       ;;Append first string.
       ($string-copy! str1 0 dst.str 0    len1)
       ;;Append second string.
       ($string-copy! str2 0 dst.str len1 len2)
       ;;Append third string.
       (let ((dst.start ($fx+ len1 len2)))
	 ($string-copy! str3 0 dst.str dst.start len3)
	 ;;Append fourth string.
	 (let ((dst.start ($fx+ dst.start len3)))
	   ($string-copy! str4 0 dst.str dst.start len4)
	   ;;Append rest strings.
	   (%fill-strings dst.str str* ($fx+ dst.start len4))))))))


;;; concatenating

(define* (string-concatenate {str* list-of-strings?})
  ;;Defined by Vicare.  Concatenate the STR*  and return the resulting string.  It is
  ;;an error  if the sum  of the string  lengths is not in  the range of  the maximum
  ;;string length.
  ;;
  (let ((total-length ($string-total-length 0 str*)))
    (assert-total-string-length total-length)
    ($string-concatenate total-length str*)))

(define ($string-concatenate total-length str*)
  ;;Concatenate the  strings in STR*, return  the result.  The resulting  string must
  ;;have length TOTAL-LENGTH.  Assume the arguments have been already validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION  The strings must have  a fixnum length and  the whole
  ;;string must at maximum have a fixnum length.
  ;;
  (let loop ((dst.str	($make-string total-length))
	     (dst.start	0)
	     (str*	str*))
    (if (pair? str*)
	(let* ((src.str  ($car str*))
	       (src.len  ($string-length src.str)))
	  ($string-copy!/count src.str 0 dst.str dst.start src.len)
	  (loop dst.str ($fx+ dst.start src.len) ($cdr str*)))
      dst.str)))

;;; --------------------------------------------------------------------

(define* (string-reverse-and-concatenate {str* list-of-strings?})
  ;;Defined by Vicare.   Reverse the STR*, concatenate them and  return the resulting
  ;;string.  It is an error  if the sum of the string lengths is  not in the range of
  ;;the maximum string length.
  ;;
  (let ((total-length ($string-total-length 0 str*)))
    (assert-total-string-length total-length)
    ($string-reverse-and-concatenate total-length str*)))

(define ($string-reverse-and-concatenate total-length str*)
  ;;Reverse STR* and concatenate its string  items; return the result.  The resulting
  ;;string must  have length  TOTAL-LENGTH.  Assume the  arguments have  been already
  ;;validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION  The strings must have  a fixnum length and  the whole
  ;;string must at maximum have a fixnum length.
  ;;
  (let loop ((dst.str	($make-string total-length))
	     (dst.start	total-length)
	     (str*	str*))
    (if (pair? str*)
	(let* ((src.str   ($car str*))
	       (src.len   ($string-length src.str))
	       (dst.start ($fx- dst.start src.len)))
	  ($string-copy!/count src.str 0 dst.str dst.start src.len)
	  (loop dst.str dst.start ($cdr str*)))
      dst.str)))

;;; --------------------------------------------------------------------

(define ($string-total-length total-len str*)
  ;;Given the STR*: compute the total length  of the strings, add it to TOTAL-LEN and
  ;;return the result.  If TOTAL-LEN is zero:  the returned value is the total length
  ;;of the strings.  The returned value may or may not be in the range of the maximum
  ;;string size.
  ;;
  (if (pair? str*)
      ($string-total-length (+ total-len ($string-length ($car str*)))
			    ($cdr str*))
    total-len))


(case-define* string-for-each
  ;;Defined by R6RS.  The STRS must all  have the same length.  PROC should accept as
  ;;many arguments as there are STRS.
  ;;
  ;;The STRING-FOR-EACH procedure applies PROC  element-wise to the characters of the
  ;;STRS for its side effects, in order  from the first characters to the last.  PROC
  ;;is always called in the same  dynamic environment as STRING-FOR-EACH itself.  The
  ;;return values are unspecified.
  ;;
  ;;Analogous to FOR-EACH.
  ;;
  ;;Implementation responsibilities:  the implementation must check  the restrictions
  ;;on  @var{proc}  to  the  extent  performed  by  applying  it  as  described.   An
  ;;implementation may check whether PROC  is an appropriate argument before applying
  ;;it.
  ;;
  (({proc procedure?} {str string?})
   (let next-char ((str.index	0)
		   (str.len	($string-length str)))
     (unless ($fx= str.index str.len)
       (proc ($string-ref str str.index))
       (next-char ($fxadd1 str.index) str.len))))

  (({proc procedure?} {str0 string?} {str1 string?})
   (assert-equal-length-strings (list str0 str1))
   (let next-char ((str.index	0)
		   (str.len	($string-length str0)))
     (unless ($fx= str.index str.len)
       (proc ($string-ref str0 str.index)
	     ($string-ref str1 str.index))
       (next-char ($fxadd1 str.index) str.len))))

  (({proc procedure?} {str0 string?} {str1 string?} {str2 string?} . str*)
   ;;FIXME At the  next boot image rotation: this validation  must be integrated with
   ;;the signature.  (Marco Maggi; Sun May 10, 2015)
   (for-each (lambda (item)
	       (unless (string? item)
		 (procedure-argument-violation __who__ "failed argument validation" item)))
     str*)
   (assert-equal-length-strings (cons* str0 str1 str2 str*))
   (let next-char ((str*	str*)
		   (str.index	0)
		   (str.len	($string-length str0)))
     (unless ($fx= str.index str.len)
       (apply proc
	      ($string-ref str0 str.index)
	      ($string-ref str1 str.index)
	      ($string-ref str2 str.index)
	      (let next-string ((str.index str.index)
				(str*      str*))
		(if (pair? str*)
		    (cons ($string-ref ($car str*) str.index)
			  (next-string str.index ($cdr str*)))
		  '())))
       (next-char str* ($fxadd1 str.index) str.len)))))


(define* (string-fill! {str string?} {fill char?})
  ;;Defined by  R6RS.  Store FILL in  every element of  the given STR and  return STR
  ;;itself.
  ;;
  ($string-fill! str 0 ($string-length str) fill))

(define ($string-fill! str index end fill)
  ;;Fill  the positions  in STR  from  INDEX inclusive  to END  exclusive with  FILL.
  ;;Return STR itself.
  ;;
  (if ($fx< index end)
      (begin
	($string-set! str index fill)
	($string-fill! str ($fxadd1 index) end fill))
    str))


(define* (string-copy! {src.str string?} {src.start string-index?} {dst.str string?} {dst.start string-index?} {count non-negative-fixnum?})
  ;;Defined  by Ikarus.   Copy COUNT  characters from  SRC.STR starting  at SRC.START
  ;;(inclusive) to DST.STR starting at DST.START.  Return unspecified values.
  ;;
  (let ((src.len ($string-length src.str))
	(dst.len ($string-length dst.str)))
    (assert-start-index-and-count-for-string src.str src.start count src.len)
    (assert-start-index-and-count-for-string dst.str dst.start count dst.len)
    ($string-copy!/count src.str src.start dst.str dst.start count)))

;;; --------------------------------------------------------------------

(define ($string-copy! src.str src.start dst.str dst.start src.end)
  ;;Copy the characters of SRC.STR from  SRC.START inclusive to SRC.END exclusive, to
  ;;DST.STR starting at DST.START inclusive.
  ;;
  (cond (($fx= src.start src.end)
	 (void))
	((eq? src.str dst.str)
	 (cond (($fx< dst.start src.start)
		($string-copy-forwards!  src.str src.start dst.str dst.start src.end))
	       (($fx> dst.start src.start)
		($string-copy-backwards! src.str src.start dst.str dst.start src.end))
	       (else (void))))
	(else
	 ($string-copy-forwards!  src.str src.start dst.str dst.start src.end))))

(define ($string-copy-forwards! src.str src.start dst.str dst.start src.end)
  ;;Copy characters  of STR.STR  from SRC.START inclusive  to SRC.END  exclusive into
  ;;DST.STR itself starting at DST.START inclusive.  The copy happens forwards, so if
  ;;SRC.STR  and DST.STR  are the  same:  it is  suitable  for the  case SRC.START  >
  ;;DST.START.
  ;;
  (unless ($fx= src.start src.end)
    ($string-set! dst.str dst.start ($string-ref src.str src.start))
    ($string-copy-forwards! src.str ($fxadd1 src.start) dst.str ($fxadd1 dst.start) src.end)))

(define ($string-copy-backwards! src.str src.start dst.str dst.start src.end)
  ;;Copy characters  of STR.STR  from SRC.START inclusive  to SRC.END  exclusive into
  ;;DST.STR itself starting  at DST.START inclusive.  The copy  happens backwards, so
  ;;if SRC.STR  and DST.STR are  the same:  it is suitable  for the case  SRC.START <
  ;;DST.START.
  ;;
  (define count ($fx- src.end src.start))
  (let loop ((src.end   src.end)
	     (dst.end	($fx+ dst.start count)))
    (unless ($fx= src.start src.end)
      (let ((src.end ($fxsub1 src.end))
	    (dst.end ($fxsub1 dst.end)))
	($string-set! dst.str dst.end ($string-ref src.str src.end))
	(loop src.end dst.end)))))

;;; --------------------------------------------------------------------

(define ($string-copy!/count src.str src.start dst.str dst.start count)
  ;;Copy COUNT  characters from  SRC.STR starting at  SRC.START inclusive  to DST.STR
  ;;starting at DST.START inclusive.
  ;;
  (cond (($fxzero? count)
	 (void))
	((eq? src.str dst.str)
	 (cond (($fx< dst.start src.start)
		($string-self-copy-forwards!/count  src.str src.start dst.start count))
	       (($fx> dst.start src.start)
		($string-self-copy-backwards!/count src.str src.start dst.start count))
	       (else (void))))
	(else
	 (let ((src.end ($fx+ src.start count)))
	   ($string-copy-forwards! src.str src.start dst.str dst.start src.end)))))

(define ($string-self-copy-forwards!/count str src.start dst.start count)
  ;;Copy COUNT characters  of STR from SRC.START inclusive to  STR itself starting at
  ;;DST.START inclusive.  The  copy happens forwards, so it is  suitable for the case
  ;;SRC.START > DST.START.
  ;;
  (let loop ((str	str)
	     (src.start	src.start)
	     (dst.start	dst.start)
	     (src.end	($fx+ src.start count)))
    (unless ($fx= src.start src.end)
      ($string-set! str dst.start ($string-ref str src.start))
      (loop str ($fxadd1 src.start) ($fxadd1 dst.start) src.end))))

(define ($string-self-copy-backwards!/count str src.start dst.start count)
  ;;Copy COUNT characters  of STR from SRC.START inclusive to  STR itself starting at
  ;;DST.START inclusive.  The copy happens backwards,  so it is suitable for the case
  ;;SRC.START < DST.START.
  ;;
  (let loop ((str	str)
	     (src.start	($fx+ src.start count))
	     (dst.start	($fx+ dst.start count))
	     (src.end	src.start))
    (unless ($fx= src.start src.end)
      (let ((src.start ($fxsub1 src.start))
	    (dst.start ($fxsub1 dst.start)))
	($string-set! str dst.start ($string-ref str src.start))
	(loop str src.start dst.start src.end)))))


(define* (uuid)
  ;;Defined by Ikarus.  Attempt the generation of a unique string.
  ;;
  (let* ((s ($make-bytevector 16))
	 (r (foreign-call "ik_uuid" s)))
    (if (bytevector? r)
	(utf8->string r)
      (error __who__ "cannot obtain unique id"))))


(define* (string-empty? {str string?})
  ;;Defined by Vicare.  Return true if STR is empty, otherwise return false.
  ;;
  ($string-empty? str))

;;FIXME This  should become  a true  primitive operation.  (Marco  Maggi; Tue  Oct 8,
;;2013)
(define ($string-empty? str)
  ($fxzero? ($string-length str)))


;;;; bytevectors to/from HEX strings

(define* (bytevector->hex {bv bytevector?})
  ;;Defined by Vicare.  Convert a bytevector of octets into a bytevector representing
  ;;the  ASCII  HEX  encoding  of  the octets.   If  successful  return  the  encoded
  ;;bytevector, else return #f.
  ;;
  ;;The length of the output bytevector is  twice the length of the input bytevector.
  ;;An  error  occurs if  the  output  bytevector  length  would exceed  the  maximum
  ;;bytevector length (which is the greatest fixnum).
  ;;
  (foreign-call "ikrt_bytevector_to_hex" bv))

(define* (hex->bytevector {bv bytevector?})
  ;;Defined by Vicare.   Convert a bytevector representing the ASCII  HEX encoding of
  ;;octets into a bytevector of octets.  If successful return the encoded bytevector,
  ;;else return #f.
  ;;
  ;;The length of the  output bytevector is half the length  of the input bytevector.
  ;;An error occurs if the input bytevector holds invalid data.
  ;;
  (foreign-call "ikrt_bytevector_from_hex" bv))

;;; --------------------------------------------------------------------

(define* (bytevector->string-hex {bv bytevector?})
  ;;Defined  by Vicare.   Convert the  bytevector BV  into a  string holding  the HEX
  ;;representation of the bytes.
  ;;
  (let ((rv (foreign-call "ikrt_bytevector_to_hex" bv)))
    (and rv ($ascii->string rv))))

(define* (string-hex->bytevector {S string?})
  ;;Defined  by Vicare.   Convert the  string S  into a  bytevector holding  the byte
  ;;representation of the HEX sequences.
  ;;
  (foreign-call "ikrt_bytevector_from_hex" ($string->ascii S)))


;;;; bytevectors to/from BASE64 strings

(define* (bytevector->base64 {bv bytevector?})
  ;;Defined by Vicare.  Convert a bytevector of octets into a bytevector representing
  ;;the  ASCII Base64  encoding  of the  octets.  If  successful  return the  encoded
  ;;bytevector, else return #f.
  ;;
  ;;False  is returned  if  the output  bytevector length  would  exceed the  maximum
  ;;bytevector length (which is the greatest fixnum).
  ;;
  ($bytevector->base64 bv))

(define ($bytevector->base64 bv)
  (foreign-call "ikrt_bytevector_to_base64" bv))

(define* (base64->bytevector {bv bytevector?})
  ;;Defined by Vicare.   Convert a bytevector representing the  ASCII Base64 encoding
  ;;of  octets  into a  bytevector  of  octets.   If  successful return  the  encoded
  ;;bytevector, else return #f.
  ;;
  ;;False is returned if the input bytevector holds invalid data.
  ;;
  ($base64->bytevector bv))

(define ($base64->bytevector bv)
  (foreign-call "ikrt_bytevector_from_base64" bv))

;;; --------------------------------------------------------------------

(define* (bytevector->string-base64 {bv bytevector?})
  ;;Defined by  Vicare.  Convert the bytevector  BV into a string  holding the Base64
  ;;representation of the bytes.
  ;;
  ($bytevector->string-base64 bv))

(define ($bytevector->string-base64 bv)
  (let ((rv (foreign-call "ikrt_bytevector_to_base64" bv)))
    (and rv ($ascii->string rv))))

;;; --------------------------------------------------------------------

(define* (string-base64->bytevector {S string?})
  ;;Defined  by Vicare.   Convert the  string S  into a  bytevector holding  the byte
  ;;representation of the Base64 sequences.
  ;;
  ($string-base64->bytevector S))

(define* ($string-base64->bytevector S)
  (let ((bv (foreign-call "ikrt_bytevector_from_base64" ($string->ascii S))))
    (or bv
	(procedure-argument-violation __who__
	  "invalid characters in string for base64 encoding" S))))


;;;; bytevectors to/from RFC 3986 URI percent encoding

(define (string->uri-encoding str)
  (uri-encode (string->utf8 str)))

(define (uri-encoding->string bv)
  (utf8->string (uri-decode bv)))

(module ( ;;
	 uri-encode			$uri-encode
	 uri-decode			$uri-decode
	 normalise-uri-encoding		$normalise-uri-encoding
	 uri-encoded-bytevector?	$uri-encoded-bytevector?
	 uri-encoded-string?		$uri-encoded-string?)

  (define* (uri-encode {bv bytevector?})
    ;;Return  a  percent-encoded  bytevector  representation  of  the  bytevector  BV
    ;;according to RFC 3986.
    ;;
    ;;FIXME This could be made significantly faster,  but I have no will now.  (Marco
    ;;Maggi; Tue Apr 9, 2013)
    ;;
    ($uri-encode bv))

  (define ($uri-encode bv)
    (receive (port getter)
	(open-bytevector-output-port)
      (do ((i 0 ($fxadd1 i)))
	  (($fx= i ($bytevector-length bv))
	   (getter))
	(let ((chi ($bytevector-u8-ref bv i)))
	  (if ($is-unreserved? chi)
	      (put-u8 port chi)
	    (put-bytevector port ($vector-ref PERCENT-ENCODER-TABLE chi)))))))

;;; --------------------------------------------------------------------

  (define* (uri-decode {bv bytevector?})
    ;;Percent-decode the given bytevector according to RFC 3986.
    ;;
    ($uri-decode bv))

  (define* ($uri-decode bv)
    ;;FIXME This could be made significantly faster,  but I have no will now.  (Marco
    ;;Maggi; Tue Apr 9, 2013)
    ;;
    (define-syntax-rule (%percent-error ch)
      (error __who__ "invalid octet in percent-encoded bytevector, percent sequence" bv ch))
    (receive (port getter)
	(open-bytevector-output-port)
      (do ((buf (make-string 2))
	   (i 0 ($fxadd1 i)))
	  (($fx= i ($bytevector-length bv))
	   (getter))
	(let ((chi ($bytevector-u8-ref bv i)))
	  (put-u8 port
		  (cond (($fx= chi INT-PERCENT)
			 (if ($two-more-octets-after-this? bv i)
			     (begin
			       (set! i ($fxadd1 i))
			       (let* ((chi ($bytevector-u8-ref bv i))
				      (ch  ($fixnum->char chi)))
				 (if ($is-hex-digit? chi)
				     ($string-set! buf 0 ch)
				   (%percent-error ch)))
			       (set! i ($fxadd1 i))
			       (let* ((chi ($bytevector-u8-ref bv i))
				      (ch  ($fixnum->char chi)))
				 (if ($is-hex-digit? chi)
				     ($string-set! buf 1 ch)
				   (%percent-error ch)))
			       (string->number buf 16))
			   (error __who__ "incomplete percent sequence in percent-encoded bytevector" bv)))
			(($is-unreserved? chi)
			 chi)
			(else
			 (error __who__ "invalid octet in percent-encoded bytevector" bv chi))))))))

;;; --------------------------------------------------------------------

  (define* (normalise-uri-encoding {bv bytevector?})
    ;;Normalise  the given  percent-encoded bytevector;  chars that  are encoded  but
    ;;should  not   are  decoded.   Return   the  normalised  bytevector,   in  which
    ;;percent-encoded characters are displayed in upper case.
    ;;
    ;;We assume  that BV is composed  by integers corresponding to  characters in the
    ;;valid range for URIs.
    ;;
    ($normalise-uri-encoding bv))

  (define* ($normalise-uri-encoding bv)
    ;;FIXME This could be made significantly faster,  but I have no will now.  (Marco
    ;;Maggi; Tue Apr 9, 2013)
    ;;
    (define-syntax-rule (%percent-error ch)
      (error __who__ "invalid octet in percent-encoded bytevector, percent sequence" ch))
    (receive (port getter)
	(open-bytevector-output-port)
      (do ((buf ($make-string 2))
	   (i 0 ($fxadd1 i)))
	  (($fx= i ($bytevector-length bv))
	   (getter))
	(let ((chi ($bytevector-u8-ref bv i)))
	  (cond (($fx= chi INT-PERCENT)
		 (if ($two-more-octets-after-this? bv i)
		     (begin
		       (set! i ($fxadd1 i))
		       (let* ((chi ($bytevector-u8-ref bv i))
			      (ch  ($fixnum->char chi)))
			 (if ($is-hex-digit? chi)
			     ($string-set! buf 0 ch)
			   (%percent-error ch)))
		       (set! i ($fxadd1 i))
		       (let* ((chi ($bytevector-u8-ref bv i))
			      (ch  ($fixnum->char chi)))
			 (if ($is-hex-digit? chi)
			     ($string-set! buf 1 ch)
			   (%percent-error ch)))
		       (let ((chi (string->number (string-upcase buf) 16)))
			 (if ($is-unreserved? chi)
			     (put-u8 port chi)
			   (put-bytevector port ($vector-ref PERCENT-ENCODER-TABLE chi)))))
		   (error __who__ "incomplete percent sequence in percent-encoded bytevector" bv)))
		(($is-unreserved? chi)
		 (put-u8 port chi))
		(else
		 (error __who__ "invalid octet in percent-encoded bytevector" bv chi)))))))

;;; --------------------------------------------------------------------

  (define* (uri-encoded-bytevector? {bv bytevector?})
    ;;Return true if  the argument is correctly  percent-encoded bytevector according
    ;;to RFC 3986.
    ;;
    ($uri-encoded-bytevector? bv))

  (define ($uri-encoded-bytevector? bv)
    (let loop ((i 0))
      (or ($fx= i ($bytevector-length bv))
	  (let ((chi ($bytevector-u8-ref bv i)))
	    (cond (($fx= chi INT-PERCENT)
		   (and ($two-more-octets-after-this? bv i)
			(begin
			  ;;The  first octet  must  represent a  HEX
			  ;;digit in ASCII encoding.
			  (set! i ($fxadd1 i))
			  (and ($is-hex-digit? ($bytevector-u8-ref bv i))
			       (begin
				 ;;The second octet must represent a
				 ;;HEX digit in ASCII encoding.
				 (set! i ($fxadd1 i))
				 (and ($is-hex-digit? ($bytevector-u8-ref bv i))
				      (loop ($fxadd1 i))))))))
		  ((and ($fx<= 32 chi 126)
			($is-unreserved? chi))
		   (loop ($fxadd1 i)))
		  (else #f))))))

;;; --------------------------------------------------------------------

  (define* (uri-encoded-string? {str string?})
    ;;Return true  if the argument  is correctly percent-encoded string  according to
    ;;RFC 3986.
    ;;
    ($uri-encoded-string? str))

  (define ($uri-encoded-string? str)
    (define-syntax-rule ($string-chi-ref str i)
      ($char->fixnum ($string-ref str i)))
    (let loop ((i 0))
      (or ($fx= i ($string-length str))
	  (let ((chi ($string-chi-ref str i)))
	    (cond (($fx= chi INT-PERCENT)
		   (and ($two-more-chars-after-this? str i)
			(begin
			  ;;The first char must represent a HEX digit in
			  ;;ASCII encoding.
			  ($fxincr! i)
			  (and ($is-hex-digit? ($string-chi-ref str i))
			       (begin
				 ;;The second octet must represent a
				 ;;HEX digit in ASCII encoding.
				 ($fxincr! i)
				 (and ($is-hex-digit? ($string-chi-ref str i))
				      (loop ($fxadd1 i))))))))
		  ((and ($fx<= 32 chi 126)
			($is-unreserved? chi))
		   (loop ($fxadd1 i)))
		  (else #f))))))

;;; --------------------------------------------------------------------

  (define-syntax-rule ($two-more-octets-after-this? bv i)
    (< (+ 2 i) ($bytevector-length bv)))

  (define-syntax-rule ($two-more-chars-after-this? str i)
    (< (+ 2 i) ($string-length str)))

;;; --------------------------------------------------------------------

  (define-inline ($is-unreserved? chi)
    (or ($is-alpha-digit? chi)
	(= chi INT-DASH)
	(= chi INT-DOT)
	(= chi INT-UNDERSCORE)
	(= chi INT-TILDE)))

  (define-inline ($is-alpha-digit? chi)
    (or ($is-alpha? chi)
	($is-dec-digit? chi)))

  (define-inline ($is-alpha? chi)
    (or (<= INT-A chi INT-Z)
	(<= INT-a chi INT-z)))

  (define-inline ($is-dec-digit? chi)
    (<= INT-0 chi INT-9))

  (define-inline ($is-hex-digit? chi)
    (or ($is-dec-digit? chi)
	($fx<= INT-a chi INT-f)
	($fx<= INT-A chi INT-F)))

  (define-inline-constant INT-a			(char->integer #\a))
  (define-inline-constant INT-f			(char->integer #\f))
  (define-inline-constant INT-z			(char->integer #\z))
  (define-inline-constant INT-A			(char->integer #\A))
  (define-inline-constant INT-F			(char->integer #\F))
  (define-inline-constant INT-Z			(char->integer #\Z))
  (define-inline-constant INT-0			(char->integer #\0))
  (define-inline-constant INT-9			(char->integer #\9))

  (define-inline-constant INT-PERCENT		(char->integer #\%))

  ;; unreserved
  (define-inline-constant INT-DASH		(char->integer #\-))
  (define-inline-constant INT-DOT		(char->integer #\.))
  (define-inline-constant INT-UNDERSCORE	(char->integer #\_))
  (define-inline-constant INT-TILDE		(char->integer #\~))

;;; --------------------------------------------------------------------

  (define-constant PERCENT-ENCODER-TABLE
    ;;Section 2.1 Percent-Encoding of  RFC 3986 states "For consistency,
    ;;URI  producers and  normalizers should  use uppercase  hexadecimal
    ;;digits for all percent-encodings."
    ;;
    '#( ;;
       #ve(ascii "%00") #ve(ascii "%01") #ve(ascii "%02") #ve(ascii "%03")
       #ve(ascii "%04") #ve(ascii "%05") #ve(ascii "%06") #ve(ascii "%07")
       #ve(ascii "%08") #ve(ascii "%09") #ve(ascii "%0A") #ve(ascii "%0B")
       #ve(ascii "%0C") #ve(ascii "%0D") #ve(ascii "%0E") #ve(ascii "%0F")

       #ve(ascii "%10") #ve(ascii "%11") #ve(ascii "%12") #ve(ascii "%13")
       #ve(ascii "%14") #ve(ascii "%15") #ve(ascii "%16") #ve(ascii "%17")
       #ve(ascii "%18") #ve(ascii "%19") #ve(ascii "%1A") #ve(ascii "%1B")
       #ve(ascii "%1C") #ve(ascii "%1D") #ve(ascii "%1E") #ve(ascii "%1F")

       #ve(ascii "%20") #ve(ascii "%21") #ve(ascii "%22") #ve(ascii "%23")
       #ve(ascii "%24") #ve(ascii "%25") #ve(ascii "%26") #ve(ascii "%27")
       #ve(ascii "%28") #ve(ascii "%29") #ve(ascii "%2A") #ve(ascii "%2B")
       #ve(ascii "%2C") #ve(ascii "%2D") #ve(ascii "%2E") #ve(ascii "%2F")

       #ve(ascii "%30") #ve(ascii "%31") #ve(ascii "%32") #ve(ascii "%33")
       #ve(ascii "%34") #ve(ascii "%35") #ve(ascii "%36") #ve(ascii "%37")
       #ve(ascii "%38") #ve(ascii "%39") #ve(ascii "%3A") #ve(ascii "%3B")
       #ve(ascii "%3C") #ve(ascii "%3D") #ve(ascii "%3E") #ve(ascii "%3F")

       #ve(ascii "%40") #ve(ascii "%41") #ve(ascii "%42") #ve(ascii "%43")
       #ve(ascii "%44") #ve(ascii "%45") #ve(ascii "%46") #ve(ascii "%47")
       #ve(ascii "%48") #ve(ascii "%49") #ve(ascii "%4A") #ve(ascii "%4B")
       #ve(ascii "%4C") #ve(ascii "%4D") #ve(ascii "%4E") #ve(ascii "%4F")

       #ve(ascii "%50") #ve(ascii "%51") #ve(ascii "%52") #ve(ascii "%53")
       #ve(ascii "%54") #ve(ascii "%55") #ve(ascii "%56") #ve(ascii "%57")
       #ve(ascii "%58") #ve(ascii "%59") #ve(ascii "%5A") #ve(ascii "%5B")
       #ve(ascii "%5C") #ve(ascii "%5D") #ve(ascii "%5E") #ve(ascii "%5F")

       #ve(ascii "%60") #ve(ascii "%61") #ve(ascii "%62") #ve(ascii "%63")
       #ve(ascii "%64") #ve(ascii "%65") #ve(ascii "%66") #ve(ascii "%67")
       #ve(ascii "%68") #ve(ascii "%69") #ve(ascii "%6A") #ve(ascii "%6B")
       #ve(ascii "%6C") #ve(ascii "%6D") #ve(ascii "%6E") #ve(ascii "%6F")

       #ve(ascii "%70") #ve(ascii "%71") #ve(ascii "%72") #ve(ascii "%73")
       #ve(ascii "%74") #ve(ascii "%75") #ve(ascii "%76") #ve(ascii "%77")
       #ve(ascii "%78") #ve(ascii "%79") #ve(ascii "%7A") #ve(ascii "%7B")
       #ve(ascii "%7C") #ve(ascii "%7D") #ve(ascii "%7E") #ve(ascii "%7F")

       #ve(ascii "%80") #ve(ascii "%81") #ve(ascii "%82") #ve(ascii "%83")
       #ve(ascii "%84") #ve(ascii "%85") #ve(ascii "%86") #ve(ascii "%87")
       #ve(ascii "%88") #ve(ascii "%89") #ve(ascii "%8A") #ve(ascii "%8B")
       #ve(ascii "%8C") #ve(ascii "%8D") #ve(ascii "%8E") #ve(ascii "%8F")

       #ve(ascii "%90") #ve(ascii "%91") #ve(ascii "%92") #ve(ascii "%93")
       #ve(ascii "%94") #ve(ascii "%95") #ve(ascii "%96") #ve(ascii "%97")
       #ve(ascii "%98") #ve(ascii "%99") #ve(ascii "%9A") #ve(ascii "%9B")
       #ve(ascii "%9C") #ve(ascii "%9D") #ve(ascii "%9E") #ve(ascii "%9F")

       #ve(ascii "%A0") #ve(ascii "%A1") #ve(ascii "%A2") #ve(ascii "%A3")
       #ve(ascii "%A4") #ve(ascii "%A5") #ve(ascii "%A6") #ve(ascii "%A7")
       #ve(ascii "%A8") #ve(ascii "%A9") #ve(ascii "%AA") #ve(ascii "%AB")
       #ve(ascii "%AC") #ve(ascii "%AD") #ve(ascii "%AE") #ve(ascii "%AF")

       #ve(ascii "%B0") #ve(ascii "%B1") #ve(ascii "%B2") #ve(ascii "%B3")
       #ve(ascii "%B4") #ve(ascii "%B5") #ve(ascii "%B6") #ve(ascii "%B7")
       #ve(ascii "%B8") #ve(ascii "%B9") #ve(ascii "%BA") #ve(ascii "%BB")
       #ve(ascii "%BC") #ve(ascii "%BD") #ve(ascii "%BE") #ve(ascii "%BF")

       #ve(ascii "%C0") #ve(ascii "%C1") #ve(ascii "%C2") #ve(ascii "%C3")
       #ve(ascii "%C4") #ve(ascii "%C5") #ve(ascii "%C6") #ve(ascii "%C7")
       #ve(ascii "%C8") #ve(ascii "%C9") #ve(ascii "%CA") #ve(ascii "%CB")
       #ve(ascii "%CC") #ve(ascii "%CD") #ve(ascii "%CE") #ve(ascii "%CF")

       #ve(ascii "%D0") #ve(ascii "%D1") #ve(ascii "%D2") #ve(ascii "%D3")
       #ve(ascii "%D4") #ve(ascii "%D5") #ve(ascii "%D6") #ve(ascii "%D7")
       #ve(ascii "%D8") #ve(ascii "%D9") #ve(ascii "%DA") #ve(ascii "%DB")
       #ve(ascii "%DC") #ve(ascii "%DD") #ve(ascii "%DE") #ve(ascii "%DF")

       #ve(ascii "%E0") #ve(ascii "%E1") #ve(ascii "%E2") #ve(ascii "%E3")
       #ve(ascii "%E4") #ve(ascii "%E5") #ve(ascii "%E6") #ve(ascii "%E7")
       #ve(ascii "%E8") #ve(ascii "%E9") #ve(ascii "%EA") #ve(ascii "%EB")
       #ve(ascii "%EC") #ve(ascii "%ED") #ve(ascii "%EE") #ve(ascii "%EF")

       #ve(ascii "%F0") #ve(ascii "%F1") #ve(ascii "%F2") #ve(ascii "%F3")
       #ve(ascii "%F4") #ve(ascii "%F5") #ve(ascii "%F6") #ve(ascii "%F7")
       #ve(ascii "%F8") #ve(ascii "%F9") #ve(ascii "%FA") #ve(ascii "%FB")
       #ve(ascii "%FC") #ve(ascii "%FD") #ve(ascii "%FE") #ve(ascii "%FF")))

  #| end of module |# )


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.strings")))

#| end of library |# )


(library (vicare system strings)
  (export $make-string
	  $string-length
	  $string-ref
	  $string-set!)
  (import (vicare))
  (define $make-string		make-string)
  (define $string-length	string-length)
  (define $string-ref		string-ref)
  (define $string-set!		string-set!))

;;; end of file
