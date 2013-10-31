;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: low level functions for URI handling
;;;Date: Fri Jun  4, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (nausicaa parser-tools uri)
  (export

    ;; condition objects and error raisers
    &uri-parser-error
    raise-uri-parser-error

    ;; percent character encoding/decoding
    unreserved-char?			not-unreserved-char?
    percent-encode			percent-decode
    normalise-percent-encoded-string	normalise-percent-encoded-bytevector

    ;; parser functions
    parse-scheme		collect-hier-part
    parse-query			parse-fragment
    parse-authority		parse-userinfo
    parse-host
    parse-reg-name		parse-ip-literal
    parse-ipvfuture
    parse-ipv4-address		parse-ipv6-address
    parse-port

    (rename (collect-hier-part	collect-relative-part))

    parse-segment		parse-segment-nz
    parse-segment-nz-nc		parse-slash-and-segment

    parse-path-empty		parse-path-abempty
    parse-path-absolute		parse-path-noscheme
    parse-path-rootless		parse-path

    parse-uri			parse-relative-ref

    ;; miscellaneous
    valid-component?		normalise-path

    ;; auxiliary syntaxes
    char-selector		string-result?)
  (import (nausicaa)
    (prefix (nausicaa parser-tools ipv4-addresses) ip.)
    (prefix (nausicaa parser-tools ipv6-addresses) ip.)
    ;; (prefix (nausicaa net addresses ipv4) net.)
    ;; (prefix (nausicaa net addresses ipv6) net.)
    (prefix (vicare language-extensions makers) mk.)
    (vicare unsafe operations)
    (vicare arguments validation))


;;;; helpers

(define-auxiliary-syntaxes
  char-selector
  string-result?)


;;;; constants

(define-inline-constant INT-a			(char->integer #\a))
(define-inline-constant INT-f			(char->integer #\f))
(define-inline-constant INT-z			(char->integer #\z))
(define-inline-constant INT-A			(char->integer #\A))
(define-inline-constant INT-F			(char->integer #\F))
(define-inline-constant INT-Z			(char->integer #\Z))
(define-inline-constant INT-0			(char->integer #\0))
(define-inline-constant INT-9			(char->integer #\9))

(define-inline-constant INT-v			(char->integer #\v))
(define-inline-constant INT-V			(char->integer #\V))

(define-inline-constant INT-PERCENT		(char->integer #\%))
(define-inline-constant INT-MINUS		(char->integer #\-))

;; gen-delims
(define-inline-constant INT-COLON		(char->integer #\:))
(define-inline-constant INT-SLASH		(char->integer #\/))
(define-inline-constant INT-QUESTION-MARK	(char->integer #\?))
(define-inline-constant INT-NUMBER-SIGN		(char->integer #\#))
(define-inline-constant INT-OPEN-BRACKET	(char->integer #\[))
(define-inline-constant INT-CLOSE-BRACKET	(char->integer #\]))
(define-inline-constant INT-AT-SIGN		(char->integer #\@))

;; sub-delims
(define-inline-constant INT-BANG		(char->integer #\!))
(define-inline-constant INT-DOLLAR		(char->integer #\$))
(define-inline-constant INT-AMPERSAND		(char->integer #\&))
(define-inline-constant INT-QUOTE		(char->integer #\'))
(define-inline-constant INT-OPEN-PAREN		(char->integer #\())
(define-inline-constant INT-CLOSE-PAREN		(char->integer #\)))
(define-inline-constant INT-STAR		(char->integer #\*))
(define-inline-constant INT-PLUS		(char->integer #\+))
(define-inline-constant INT-COMMA		(char->integer #\,))
(define-inline-constant INT-SEMICOLON		(char->integer #\;))
(define-inline-constant INT-EQUAL		(char->integer #\=))

;; unreserved
(define-inline-constant INT-DASH		(char->integer #\-))
(define-inline-constant INT-DOT			(char->integer #\.))
(define-inline-constant INT-UNDERSCORE		(char->integer #\_))
(define-inline-constant INT-TILDE		(char->integer #\~))


;;;; char integer predicates

(define-inline ($is-alpha? chi)
  (or ($fx<= INT-a chi INT-z)
      ($fx<= INT-A chi INT-Z)))

(define-inline ($is-dec-digit? chi)
  ($fx<= INT-0 chi INT-9))

(define-inline ($is-hex-digit? chi)
  (or ($is-dec-digit? chi)
      ($fx<= INT-a chi INT-f)
      ($fx<= INT-A chi INT-F)))

(define-inline ($ascii-hex->integer chi)
  ;;This must be used only after "$is-hex-digit?" has validated CHI.
  ;;
  (cond (($fx<= INT-0 chi INT-9)
	 ($fx- chi INT-0))
	(($fx<= INT-a chi INT-f)
	 ($fx+ 10 ($fx- chi INT-a)))
	(else
	 #;(assert (<= INT-A chi INT-F))
	 ($fx+ 10 ($fx- chi INT-A)))))

(define-inline ($integer->ascii-hex n)
  (if ($fx<= 0 n 9)
      ($fx+ INT-0 n)
    ($fx+ INT-A n)))

(define-inline ($is-alpha-digit? chi)
  (or ($is-alpha? chi)
      ($is-dec-digit? chi)))

(define-inline ($is-gen-delim? chi)
  (or ($fx= chi INT-COLON)
      ($fx= chi INT-SLASH)
      ($fx= chi INT-QUESTION-MARK)
      ($fx= chi INT-NUMBER-SIGN)
      ($fx= chi INT-OPEN-BRACKET)
      ($fx= chi INT-CLOSE-BRACKET)
      ($fx= chi INT-AT-SIGN)))

(define-inline ($is-sub-delim? chi)
  (or ($fx= chi INT-BANG)
      ($fx= chi INT-DOLLAR)
      ($fx= chi INT-AMPERSAND)
      ($fx= chi INT-QUOTE)
      ($fx= chi INT-OPEN-PAREN)
      ($fx= chi INT-CLOSE-PAREN)
      ($fx= chi INT-STAR)
      ($fx= chi INT-PLUS)
      ($fx= chi INT-COMMA)
      ($fx= chi INT-SEMICOLON)
      ($fx= chi INT-EQUAL)))

(define-inline ($is-reserved? chi)
  (or ($is-gen-delim? chi)
      ($is-sub-delim? chi)))

(define-inline ($is-unreserved? chi)
  (or ($is-alpha-digit? chi)
      ($fx= chi INT-DASH)
      ($fx= chi INT-DOT)
      ($fx= chi INT-UNDERSCORE)
      ($fx= chi INT-TILDE)))

(define-inline ($is-pchar-not-percent-encoded? chi)
  ;;Evaluate  to true  if CHI  matches  the "pchar"  component with  the
  ;;exception of the percent-encoded sequence.
  ;;
  (or ($is-unreserved? chi)
      ($is-sub-delim? chi)
      ($fx= chi INT-COLON)
      ($fx= chi INT-AT-SIGN)))


;;;; condition objects and exception raisers

(define-condition-type &uri-parser-error
    (parent &error)
  (fields offset))

(define (raise-uri-parser-error who message offset . irritants)
  (raise
   (condition (&uri-parser-error (offset))
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))

(define (%to-string obj)
  (if (string? obj)
      obj
    (ascii->string obj)))


;;;; percent encoding/decoding: tables

(define-constant PERCENT-ENCODER-TABLE
  ;;Section 2.1  Percent-Encoding of  RFC 3986 states  "For consistency,
  ;;URI  producers  and  normalizers should  use  uppercase  hexadecimal
  ;;digits for all percent-encodings."
  ;;
  '#( ;;
     #ve(ascii "%00") #ve(ascii "%01") #ve(ascii "%02") #ve(ascii "%03") #ve(ascii "%04")
     #ve(ascii "%05") #ve(ascii "%06") #ve(ascii "%07") #ve(ascii "%08") #ve(ascii "%09")
     #ve(ascii "%0A") #ve(ascii "%0B") #ve(ascii "%0C") #ve(ascii "%0D") #ve(ascii "%0E")
     #ve(ascii "%0F")

     #ve(ascii "%10") #ve(ascii "%11") #ve(ascii "%12") #ve(ascii "%13") #ve(ascii "%14")
     #ve(ascii "%15") #ve(ascii "%16") #ve(ascii "%17") #ve(ascii "%18") #ve(ascii "%19")
     #ve(ascii "%1A") #ve(ascii "%1B") #ve(ascii "%1C") #ve(ascii "%1D") #ve(ascii "%1E")
     #ve(ascii "%1F")

     #ve(ascii "%20") #ve(ascii "%21") #ve(ascii "%22") #ve(ascii "%23") #ve(ascii "%24")
     #ve(ascii "%25") #ve(ascii "%26") #ve(ascii "%27") #ve(ascii "%28") #ve(ascii "%29")
     #ve(ascii "%2A") #ve(ascii "%2B") #ve(ascii "%2C") #ve(ascii "%2D") #ve(ascii "%2E")
     #ve(ascii "%2F")

     #ve(ascii "%30") #ve(ascii "%31") #ve(ascii "%32") #ve(ascii "%33") #ve(ascii "%34")
     #ve(ascii "%35") #ve(ascii "%36") #ve(ascii "%37") #ve(ascii "%38") #ve(ascii "%39")
     #ve(ascii "%3A") #ve(ascii "%3B") #ve(ascii "%3C") #ve(ascii "%3D") #ve(ascii "%3E")
     #ve(ascii "%3F")

     #ve(ascii "%40") #ve(ascii "%41") #ve(ascii "%42") #ve(ascii "%43") #ve(ascii "%44")
     #ve(ascii "%45") #ve(ascii "%46") #ve(ascii "%47") #ve(ascii "%48") #ve(ascii "%49")
     #ve(ascii "%4A") #ve(ascii "%4B") #ve(ascii "%4C") #ve(ascii "%4D") #ve(ascii "%4E")
     #ve(ascii "%4F")

     #ve(ascii "%50") #ve(ascii "%51") #ve(ascii "%52") #ve(ascii "%53") #ve(ascii "%54")
     #ve(ascii "%55") #ve(ascii "%56") #ve(ascii "%57") #ve(ascii "%58") #ve(ascii "%59")
     #ve(ascii "%5A") #ve(ascii "%5B") #ve(ascii "%5C") #ve(ascii "%5D") #ve(ascii "%5E")
     #ve(ascii "%5F")

     #ve(ascii "%60") #ve(ascii "%61") #ve(ascii "%62") #ve(ascii "%63") #ve(ascii "%64")
     #ve(ascii "%65") #ve(ascii "%66") #ve(ascii "%67") #ve(ascii "%68") #ve(ascii "%69")
     #ve(ascii "%6A") #ve(ascii "%6B") #ve(ascii "%6C") #ve(ascii "%6D") #ve(ascii "%6E")
     #ve(ascii "%6F")

     #ve(ascii "%70") #ve(ascii "%71") #ve(ascii "%72") #ve(ascii "%73") #ve(ascii "%74")
     #ve(ascii "%75") #ve(ascii "%76") #ve(ascii "%77") #ve(ascii "%78") #ve(ascii "%79")
     #ve(ascii "%7A") #ve(ascii "%7B") #ve(ascii "%7C") #ve(ascii "%7D") #ve(ascii "%7E")
     #ve(ascii "%7F")

     #ve(ascii "%80") #ve(ascii "%81") #ve(ascii "%82") #ve(ascii "%83") #ve(ascii "%84")
     #ve(ascii "%85") #ve(ascii "%86") #ve(ascii "%87") #ve(ascii "%88") #ve(ascii "%89")
     #ve(ascii "%8A") #ve(ascii "%8B") #ve(ascii "%8C") #ve(ascii "%8D") #ve(ascii "%8E")
     #ve(ascii "%8F")

     #ve(ascii "%90") #ve(ascii "%91") #ve(ascii "%92") #ve(ascii "%93") #ve(ascii "%94")
     #ve(ascii "%95") #ve(ascii "%96") #ve(ascii "%97") #ve(ascii "%98") #ve(ascii "%99")
     #ve(ascii "%9A") #ve(ascii "%9B") #ve(ascii "%9C") #ve(ascii "%9D") #ve(ascii "%9E")
     #ve(ascii "%9F")

     #ve(ascii "%A0") #ve(ascii "%A1") #ve(ascii "%A2") #ve(ascii "%A3") #ve(ascii "%A4")
     #ve(ascii "%A5") #ve(ascii "%A6") #ve(ascii "%A7") #ve(ascii "%A8") #ve(ascii "%A9")
     #ve(ascii "%AA") #ve(ascii "%AB") #ve(ascii "%AC") #ve(ascii "%AD") #ve(ascii "%AE")
     #ve(ascii "%AF")

     #ve(ascii "%B0") #ve(ascii "%B1") #ve(ascii "%B2") #ve(ascii "%B3") #ve(ascii "%B4")
     #ve(ascii "%B5") #ve(ascii "%B6") #ve(ascii "%B7") #ve(ascii "%B8") #ve(ascii "%B9")
     #ve(ascii "%BA") #ve(ascii "%BB") #ve(ascii "%BC") #ve(ascii "%BD") #ve(ascii "%BE")
     #ve(ascii "%BF")

     #ve(ascii "%C0") #ve(ascii "%C1") #ve(ascii "%C2") #ve(ascii "%C3") #ve(ascii "%C4")
     #ve(ascii "%C5") #ve(ascii "%C6") #ve(ascii "%C7") #ve(ascii "%C8") #ve(ascii "%C9")
     #ve(ascii "%CA") #ve(ascii "%CB") #ve(ascii "%CC") #ve(ascii "%CD") #ve(ascii "%CE")
     #ve(ascii "%CF")

     #ve(ascii "%D0") #ve(ascii "%D1") #ve(ascii "%D2") #ve(ascii "%D3") #ve(ascii "%D4")
     #ve(ascii "%D5") #ve(ascii "%D6") #ve(ascii "%D7") #ve(ascii "%D8") #ve(ascii "%D9")
     #ve(ascii "%DA") #ve(ascii "%DB") #ve(ascii "%DC") #ve(ascii "%DD") #ve(ascii "%DE")
     #ve(ascii "%DF")

     #ve(ascii "%E0") #ve(ascii "%E1") #ve(ascii "%E2") #ve(ascii "%E3") #ve(ascii "%E4")
     #ve(ascii "%E5") #ve(ascii "%E6") #ve(ascii "%E7") #ve(ascii "%E8") #ve(ascii "%E9")
     #ve(ascii "%EA") #ve(ascii "%EB") #ve(ascii "%EC") #ve(ascii "%ED") #ve(ascii "%EE")
     #ve(ascii "%EF")

     #ve(ascii "%F0") #ve(ascii "%F1") #ve(ascii "%F2") #ve(ascii "%F3") #ve(ascii "%F4")
     #ve(ascii "%F5") #ve(ascii "%F6") #ve(ascii "%F7") #ve(ascii "%F8") #ve(ascii "%F9")
     #ve(ascii "%FA") #ve(ascii "%FB") #ve(ascii "%FC") #ve(ascii "%FD") #ve(ascii "%FE")
     #ve(ascii "%FF")))


;;;; percent encoding/decoding

(define (unreserved-char? obj)
  ;;Return true if  OBJ represents an unreserved  character according to
  ;;the RFC.  OBJ can be either a character or an integer representing a
  ;;character according to CHAR->INTEGER.
  ;;
  (define who 'unreserved-char?)
  (cond ((char? obj)
	 ($is-unreserved? ($char->fixnum obj)))
	((fixnum? obj)
	 ($is-unreserved? obj))
	(else
	 (assertion-violation who "expected char or fixnum as argument" obj))))

(define (not-unreserved-char? obj)
  (not (unreserved-char? obj)))

;;; --------------------------------------------------------------------

(module (percent-encode)

  (mk.define-maker (percent-encode obj)
      %percent-encode
    ((char-selector	not-unreserved-char?)
     (string-result?	#f)))

  (define (%percent-encode obj char-encode? string-result?)
    ;;Return a  percent-encoded bytevector  or string  representation of
    ;;OBJ, which can be a char or string or bytevector.
    ;;
    (define who 'percent-encode)
    (define bv
      (cond ((bytevector? obj)
	     obj)
	    ((string? obj)
	     (string->utf8 obj))
	    ((char? obj)
	     (string->utf8 (string obj)))
	    (else
	     (assertion-violation who
	       "expected char, string or bytevector as input" obj))))
    (receive (port getter)
	(open-bytevector-output-port)
      (do ((i 0 ($fxadd1 i)))
	  (($fx= i ($bytevector-length bv))
	   (if string-result?
	       (ascii->string (getter))
	     (getter)))
	(let ((chi ($bytevector-u8-ref bv i)))
	  (if (char-encode? chi)
	      (put-bytevector port ($vector-ref PERCENT-ENCODER-TABLE chi))
	    (put-u8 port chi))))))

  #| end of module |# )

(module (percent-decode)

  (mk.define-maker (percent-decode obj)
      %percent-decode
    ((string-result?	#f)))

  (define (%percent-decode obj string-result?)
    ;;Percent-decode the given object;  return the decoded bytevector or
    ;;string.
    ;;
    (define who 'percent-decode)
    (define bv
      (cond ((string? obj)
	     (string->utf8 obj))
	    ((bytevector? obj)
	     obj)
	    (else
	     (assertion-violation who
	       "expected string or bytevector as input" obj))))
    (receive (port getter)
	(open-bytevector-output-port)
      (let loop ((i 0)
		 (buf (make-string 2)))
	(if ($fx= i ($bytevector-length bv))
	    (if string-result?
		(ascii->string (getter))
	      (getter))
	  (let ((chi ($bytevector-u8-ref bv i)))
	    (put-u8 port (if ($fx= chi INT-PERCENT)
			     (begin
			       (incr! i)
			       ($string-set! buf 0 ($fixnum->char ($bytevector-u8-ref bv i)))
			       (incr! i)
			       ($string-set! buf 1 ($fixnum->char ($bytevector-u8-ref bv i)))
			       (string->number buf 16))
			   chi))
	    (loop ($fxadd1 i) buf))))))

  #| end of module |# )


;;;; percent-encoding normalisation

(define (normalise-percent-encoded-string in-str)
  ;;Normalise the  given percent-encoded string; chars  that are encoded
  ;;but should not are decoded.   Return the normalised string, in which
  ;;percent-encoded characters are displayed in upper case.
  ;;
  ;;We assume that  IN-STR is composed by characters  in the valid range
  ;;for URIs.
  ;;
  (define who 'normalise-percent-encoded-string)
  (with-arguments-validation (who)
      ((string	in-str))
    (receive (port getter)
	(open-string-output-port)
      ;;Beware of  string indexes here: they  must be fixnums, so  we do
      ;;not use unsafe bindings for these fx operations.
      (do ((i 0 (fxadd1 i)))
	  ((= i ($string-length in-str))
	   (getter))
	(let ((ch ($string-ref in-str i)))
	  (if ($char= ch #\%)
	      (begin
		;;There must  be at  least 2 more  chars in  the string.
		;;Beware of  out of  bounds string  indexes: do  not use
		;;unsafe bindings to increment "i" by 2.
		(unless (< (+ 2 i) ($string-length in-str))
		  (assertion-violation who
		    "invalid percent-encoded string, not enough bytes after percent char"
		    in-str))
		(let* ((pc ($substring in-str ($fxadd1 i) ($fx+ 3 i)))
		       (ch ($fixnum->char (string->number pc 16))))
		  (set! i ($fx+ 2 i))
		  (if (unreserved-char? ch)
		      (put-char port ch)
		    (begin
		      (put-char port #\%)
		      (display (string-upcase pc) port)))))
	    (put-char port ch)))))))

(define (normalise-percent-encoded-bytevector in-bv)
  ;;Normalise  the  given  percent-encoded  bytevector; chars  that  are
  ;;encoded  but   should  not  are  decoded.    Return  the  normalised
  ;;bytevector,  in which  percent-encoded characters  are  displayed in
  ;;upper case.
  ;;
  ;;We  assume  that  IN-BV  is  composed by  integer  corresponding  to
  ;;characters in the valid range for URIs.
  ;;
  (define who 'normalise-percent-encoded-bytevector)
  (with-arguments-validation (who)
      ((bytevector	in-bv))
    (receive (port getter)
	(open-bytevector-output-port)
      (do ((buf (make-string 2))
	   (i 0 ($fxadd1 i)))
	  ((= i ($bytevector-length in-bv))
	   (getter))
	(let ((chi ($bytevector-u8-ref in-bv i)))
	  (if ($fx= chi INT-PERCENT)
	      (begin
		;;There must be at least 2 more bytes in the bytevector.
		;;Beware of out of bounds bytevector indexes: do not use
		;;unsafe bindings to increment "i" by 2.
		(unless (< (+ 2 i) ($bytevector-length in-bv))
		  (assertion-violation who
		    "invalid percent-encoded bytevector, not enough bytes after percent char"
		    in-bv))
		(set! i ($fxadd1 i))
		($string-set! buf 0 ($fixnum->char ($bytevector-u8-ref in-bv i)))
		(set! i ($fxadd1 i))
		($string-set! buf 1 ($fixnum->char ($bytevector-u8-ref in-bv i)))
		(let ((chi (string->number (string-upcase buf) 16)))
		  (if (unreserved-char? chi)
		      (put-u8 port chi)
		    (put-bytevector port (percent-encode (integer->char chi))))))
	    (put-u8 port chi)))))))


;;;; parser helpers

(define-syntax (define-parser-macros stx)
  (syntax-case stx ()
    ((?k ?port)
     (with-syntax
	 ((SET-POSITION-START!		(datum->syntax #'?k 'set-position-start!))
	  (SET-POSITION-BACK-ONE!	(datum->syntax #'?k 'set-position-back-one!))
	  (RETURN-FAILURE		(datum->syntax #'?k 'return-failure)))
       #'(begin
	   (define start-position
	     (port-position ?port))
	   (define-inline (SET-POSITION-START!)
	     (set-port-position! ?port start-position))
	   (define-inline (SET-POSITION-BACK-ONE! last-read-byte)
	     (unless (eof-object? last-read-byte)
	       (set-port-position! ?port (- (port-position ?port) 1))))
	   (define-inline (RETURN-FAILURE)
	     (begin
	       (SET-POSITION-START!)
	       #f)))))
    ))

(define (%parse-percent-encoded-sequence who in-port ou-port set-position-start!)
  ;;To be called after a  byte representing a percent character in ASCII
  ;;encoding as been read from  IN-PORT; parse the two HEXDIG bytes from
  ;;IN-PORT validating them as percent-encoded byte.
  ;;
  ;;If  successful: put  a  percent byte  and  the two  HEXDIG bytes  in
  ;;OU-PORT and return void.
  ;;
  ;;If EOF  or an invalid byte  is read: rewind the  input port position
  ;;calling  SET-POSITION-START!,  then  raise an  exception  with  type
  ;;"&uri-parser-error",  using WHO  as value  for the  "&who" condition
  ;;component; in this case nothing is written to OU-PORT.
  ;;
  (define (%error)
    (set-position-start!)
    (raise-uri-parser-error who
      "end of input or invalid byte in percent-encoded sequence"
      (sub1 (port-position in-port))))
  (let ((first-hexdig-byte (get-u8 in-port)))
    (cond ((eof-object? first-hexdig-byte)
	   (%error))
	  (($is-hex-digit? first-hexdig-byte)
	   (let ((second-hexdig-byte (get-u8 in-port)))
	     (cond ((eof-object? second-hexdig-byte)
		    (%error))
		   (($is-hex-digit? second-hexdig-byte)
		    (put-u8 ou-port INT-PERCENT)
		    (put-u8 ou-port first-hexdig-byte)
		    (put-u8 ou-port second-hexdig-byte))
		   (else
		    (%error)))))
	  ;;Invalid byte in percent-encoded sequence.
	  (else
	   (%error)))))


;;;; URI component parsers

(define (parse-scheme in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a scheme URI
  ;;element.   If a  colon is  found:  return a  bytevector holding  the
  ;;accumulated bytes, colon excluded; else return false.
  ;;
  ;;When successful: leave  the port position to the  byte after the one
  ;;representing the colon; if an error occurs: rewind the port position
  ;;to the one before this function call.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     #f)
	    ;;A "scheme" component starts with an alpha and goes on with
	    ;;alpha, digit or "+", "-", ".".
	    (($is-alpha? chi)
	     (put-u8 ou-port chi)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (return-failure))
		     ((or ($is-alpha-digit? chi)
			  ($fx= chi INT-PLUS)
			  ($fx= chi INT-MINUS)
			  ($fx= chi INT-DOT))
		      (put-u8 ou-port chi)
		      (process-next-byte (get-u8 in-port)))
		     (($fx= chi INT-COLON)
		      (getter))
		     (else
		      (return-failure)))))
	    (else
	     (return-failure))))))

(define (collect-hier-part in-port)
  ;;Accumulate  bytes  from IN-PORT  while  they  are acceptable  for  a
  ;;"hier-part"  URI  component.  When  EOF  or  a  question mark  or  a
  ;;number-sign is  found: return  a bytevector holding  the accumulated
  ;;bytes,  question  mark  or  number-sign excluded.   Leave  the  port
  ;;position to the byte after the last byte of the "hier-part".
  ;;
  ;;An empty hier-part is not accepted: if the first value from the port
  ;;is EOF, the return value is false.
  ;;
  ;;This function does no validation of the returned bytevector.
  ;;
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (lookahead-u8 in-port)))
      (if (or (eof-object? chi)
	      ($fx= chi INT-QUESTION-MARK)
	      ($fx= chi INT-NUMBER-SIGN))
	  #f
	(begin
	  (put-u8 ou-port chi)
	  (get-u8 in-port)
	  (let process-next-byte ((chi (lookahead-u8 in-port)))
	    (cond ((eof-object? chi)
		   (getter))
		  ((or ($fx= chi INT-QUESTION-MARK)
		       ($fx= chi INT-NUMBER-SIGN))
		   (getter))
		  (else
		   (put-u8 ou-port chi)
		   (get-u8 in-port)
		   (process-next-byte (lookahead-u8 in-port))))))))))

(define (parse-query in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for a "query" URI
  ;;component; the first byte read from IN-PORT must be a question mark.
  ;;If EOF  or a  number-sign is read:  return a bytevector  holding the
  ;;accumulated  bytes,  starting  question  mark  excluded  and  ending
  ;;number-sign excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;byte of the  "query" component; if an error  occurs: rewind the port
  ;;position to the one before this function call.
  ;;
  ;;Notice that  an empty  "query" component is  valid (a  question mark
  ;;followed by EOF).
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   #f)

	  ;;A "query" component must begin with a question mark.
	  ((not ($fx= chi INT-QUESTION-MARK))
	   (return-failure))

	  (else
	   (receive (ou-port getter)
	       (open-bytevector-output-port)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (getter))

		     ;;A  number-sign terminates  the  "query" component
		     ;;and starts a "fragment" component.
		     (($fx= chi INT-NUMBER-SIGN)
		      (set-position-back-one! chi)
		      (getter))

		     ;;Characters     in     categories    "unreserved",
		     ;;"sub-delim" or "/", "?", ":", "@" are valid.
		     ((or ($is-unreserved? chi)
			  ($is-sub-delim? chi)
			  ($fx= chi INT-SLASH)
			  ($fx= chi INT-QUESTION-MARK)
			  ($fx= chi INT-COLON)
			  ($fx= chi INT-AT-SIGN))
		      (put-u8 ou-port chi)
		      (process-next-byte (get-u8 in-port)))

		     ;;A percent-encoded sequence is valid.
		     (($fx= chi INT-PERCENT)
		      (let ((chi1 (get-u8 in-port)))
			(cond ((eof-object? chi1)
			       (return-failure))
			      (($is-hex-digit? chi1)
			       (let ((chi2 (get-u8 in-port)))
				 (cond ((eof-object? chi2)
					(return-failure))
				       (($is-hex-digit? chi2)
					(put-u8 ou-port INT-PERCENT)
					(put-u8 ou-port chi1)
					(put-u8 ou-port chi2)
					(process-next-byte (get-u8 in-port)))
				       (else
					(return-failure)))))
			      (else
			       (return-failure)))))
		     (else
		      (return-failure)))))))))

(define (parse-fragment in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a "fragment"
  ;;URI  component;  the  first  byte   read  from  IN-PORT  must  be  a
  ;;number-sign.   If  EOF is  read:  return  a  bytevector holding  the
  ;;accumulated bytes, starting number-sign excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;byte of  the "fragment"  component; if an  error occurs:  rewind the
  ;;port position to the one before this function call.
  ;;
  ;;Notice that  an empty "fragment"  component is valid  (a number-sign
  ;;followed by EOF).
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   #f)

	  ;;A "fragment" component starts with a number sign.
	  ((not ($fx= chi INT-NUMBER-SIGN))
	   (return-failure))

	  (else
	   (receive (ou-port getter)
	       (open-bytevector-output-port)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (getter))

		     ;;Characters   in   the  categories   "unreserved",
		     ;;"sub-delim" or "/", "?", ":", "@" are valid.
		     ((or ($is-unreserved? chi)
			  ($is-sub-delim? chi)
			  ($fx= chi INT-SLASH)
			  ($fx= chi INT-QUESTION-MARK)
			  ($fx= chi INT-COLON)
			  ($fx= chi INT-AT-SIGN))
		      (put-u8 ou-port chi)
		      (process-next-byte (get-u8 in-port)))

		     ;;A percent-encoded sequence is valid.
		     (($fx= chi INT-PERCENT)
		      (let ((chi1 (get-u8 in-port)))
			(cond ((eof-object? chi1)
			       (return-failure))
			      (($is-hex-digit? chi1)
			       (let ((chi2 (get-u8 in-port)))
				 (cond ((eof-object? chi2)
					(return-failure))
				       (($is-hex-digit? chi2)
					(put-u8 ou-port INT-PERCENT)
					(put-u8 ou-port chi1)
					(put-u8 ou-port chi2)
					(process-next-byte (get-u8 in-port)))
				       (else
					(return-failure)))))
			      (else
			       (return-failure)))))
		     (else
		      (return-failure)))))))))


;;;; hier-part/relative-part component parsers

(define (parse-authority in-port)
  ;;Accumulate  bytes from  IN-PORT  while they  are  acceptable for  an
  ;;"authority"  component   in  the  "hier-part"  of  an   URI  or  the
  ;;"relative-part" of a "relative-ref".   The first two bytes read must
  ;;represent, in  ASCII encoding, two  slash characters; after  the two
  ;;slashes, if EOF or a byte representing a slash, a question mark or a
  ;;number-sign  is read:  return a  bytevector holding  the accumulated
  ;;bytes,  ending slash,  question mark  or number-sign  excluded; else
  ;;return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;accumulated byte;  if an error  occurs: rewind the port  position to
  ;;the one before this function call.
  ;;
  ;;Notice that  an empty authority  (after the two leading  slashes) is
  ;;valid: it  is the case of  "authority" equal to  a "host" component,
  ;;equal to a "reg-name" component which can be empty.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   (return-failure))
	  ((not ($fx= chi INT-SLASH))
	   (return-failure))
	  (else
	   (let ((chi1 (get-u8 in-port)))
	     (cond ((eof-object? chi1)
		    (return-failure))
		   (($fx= chi1 INT-SLASH)
		    (receive (ou-port getter)
			(open-bytevector-output-port)
		      (let process-next-byte ((chi (get-u8 in-port)))
			(cond ((eof-object? chi)
			       (getter))
			      ((or ($fx= chi INT-SLASH)
				   ($fx= chi INT-QUESTION-MARK)
				   ($fx= chi INT-NUMBER-SIGN))
			       (set-position-back-one! chi)
			       (getter))
			      (else
			       (put-u8 ou-port chi)
			       (process-next-byte (get-u8 in-port)))))))
		   (else
		    (return-failure))))))))

(define (parse-userinfo in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for an "userinfo"
  ;;component in  the "authority" component.   If a byte  representing a
  ;;commercial at-sign, in ASCII  encoding, is read: return a bytevector
  ;;holding the accumulated bytes,  ending at-sign excluded; else return
  ;;false.
  ;;
  ;;If successful: leave the port  position to the byte after the ending
  ;;at-sign; if  an error  occurs: rewind the  port position to  the one
  ;;before this function call.
  ;;
  ;;Notice  that an  empty  "userinfo" component  is  valid (an  at-sign
  ;;preceded by nothing).
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (return-failure))

	    ;;An at-sign terminates the "userinfo" component.
	    (($fx= chi INT-AT-SIGN)
	     (getter))

	    ;;Characters   in    the   categories   "unreserved"   and
	    ;;"sub-delims" or ":" are valid.
	    ((or ($is-unreserved? chi)
		 ($is-sub-delim?  chi)
		 ($fx= chi INT-COLON))
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    (($fx= chi INT-PERCENT)
	     (let ((chi1 (get-u8 in-port)))
	       (cond ((eof-object? chi1)
		      (return-failure))
		     (($is-hex-digit? chi1)
		      (let ((chi2 (get-u8 in-port)))
			(cond ((eof-object? chi2)
			       (return-failure))
			      (($is-hex-digit? chi2)
			       (put-u8 ou-port INT-PERCENT)
			       (put-u8 ou-port chi1)
			       (put-u8 ou-port chi2)
			       (process-next-byte (get-u8 in-port)))
			      (else
			       (return-failure)))))
		     ;;Invalid byte in percent-encoded sequence.
		     (else
		      (return-failure)))))

	    ;;Invalid byte.
	    (else
	     (return-failure))))))

(define (parse-ipv4-address in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IPv4address"  component,  then  parse  them as  IPv4  address.   If
  ;;successful return  two values: a bytevector  holding the accumulated
  ;;bytes,  a list  holding the  octecs as  exact integers;  else return
  ;;false and false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;No validation is  performed on the first byte  after the address, if
  ;;any.
  ;;
  (define-parser-macros in-port)
  (define (%error)
    (values (return-failure) #f))
  (let ((bv (receive (host-port getter)
		(open-bytevector-output-port)
	      (let process-next-byte ((chi (get-u8 in-port)))
		(cond ((eof-object? chi)
		       (getter))
		      ((or ($is-dec-digit? chi) ($fx= chi INT-DOT))
		       (put-u8 host-port chi)
		       (process-next-byte (get-u8 in-port)))
		      (else
		       (set-position-back-one! chi)
		       (getter)))))))
    (if (zero? (bytevector-length bv))
	(%error)
      (try
	  (values bv (ip.parse-ipv4-address-only (ascii->string bv)))
	(catch E
	  (ip.&ipv4-address-parser-error
	   (%error))
	  (else
	   (raise E)))))))

(define (parse-ipv6-address in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IPv6address"  component,  then  parse  them as  IPv6  address.   If
  ;;successful return  two values: a bytevector  holding the accumulated
  ;;bytes,  a list  holding the  8 numeric  address components  as exact
  ;;integers; else return false and false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;No validation is  performed on the first byte  after the address, if
  ;;any.
  ;;
  (define-parser-macros in-port)
  (define (%error)
    (values (return-failure) #f))
  (let ((bv (receive (host-port getter)
		(open-bytevector-output-port)
	      (let process-next-byte ((chi (get-u8 in-port)))
		(cond ((eof-object? chi)
		       (getter))
		      ((or ($is-hex-digit? chi) ($fx= chi INT-DOT) ($fx= chi INT-COLON))
		       (put-u8 host-port chi)
		       (process-next-byte (get-u8 in-port)))
		      (else
		       (set-position-back-one! chi)
		       (getter)))))))
    (if (zero? (bytevector-length bv))
	(%error)
      (try
	  (values bv (ip.parse-ipv6-address-only (ascii->string bv)))
	(catch E
	  (ip.&ipv6-address-parser-error
	   (%error))
	  (else
	   (raise E)))))))

(define (parse-ip-literal in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IP-literal" component  of a "host" component.  The  first byte must
  ;;represent an  open bracket  character in ASCII  encoding; if  a byte
  ;;representing a  closed bracket is read: return  a bytevector holding
  ;;the accumulated bytes, brackets excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;No validation is performed  on the returned bytevector contents; the
  ;;returned  bytevector  can  be  empty  even  though  an  "IP-literal"
  ;;component  cannot be  of  zero  length inside  the  brackets: it  is
  ;;responsibility of  the caller  to check the  length of  the returned
  ;;bytevector.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (if (or (eof-object? chi) (not ($fx= chi INT-OPEN-BRACKET)))
	(return-failure)
      (receive (ou-port getter)
	  (open-bytevector-output-port)
	(let process-next-byte ((chi (get-u8 in-port)))
	  (cond ((eof-object? chi)
		 (return-failure))
		(($fx= chi INT-CLOSE-BRACKET)
		 (getter))
		(else
		 (put-u8 ou-port chi)
		 (process-next-byte (get-u8 in-port)))))))))

(define (parse-ipvfuture in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IPvFuture" component in the "IP-literal" component.  The first byte
  ;;must  represent "v"  in  ASCII  encoding and  the  second byte  must
  ;;represent a  single hexadecimal digit  in ASCII encoding;  after the
  ;;prolog is read, bytes are accumulated until EOF is found.
  ;;
  ;;Return  two values:  an exact  integer representing  the hexadecimal
  ;;digit in ASCII encoding; a bytevector holding the accumulated bytes;
  ;;else return false and false.
  ;;
  ;;If an error occurs: rewind the  port position to the one before this
  ;;function call.
  ;;
  ;;No  specific  validation is  performed  on  the returned  bytevector
  ;;contents, bytes are  only tested to be valid  for the component; the
  ;;returned  bytevector  can  be   empty  even  though  an  "IPvFuture"
  ;;component cannot be of zero length inside the brackets.
  ;;
  (define-parser-macros in-port)
  (define (%error)
    (values (return-failure) #f))
  (let ((chi (get-u8 in-port)))
    (if (or (eof-object? chi) (not (or ($fx= chi INT-V) ($fx= chi INT-V))))
	(%error)
      (let ((version-chi (get-u8 in-port)))
	(if ($is-hex-digit? version-chi)
	    (receive (ou-port getter)
		(open-bytevector-output-port)
	      (let process-next-byte ((chi (get-u8 in-port)))
		(cond ((eof-object? chi)
		       (values ($ascii-hex->integer version-chi) (getter)))
		      ((or ($is-unreserved? chi)
			   ($is-sub-delim? chi)
			   ($fx= chi INT-COLON))
		       (put-u8 ou-port chi)
		       (process-next-byte (get-u8 in-port)))
		      (else
		       (%error)))))
	  (%error))))))

(define (parse-reg-name in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a "reg-name"
  ;;component in the "host" component.   If EOF or a byte representing a
  ;;colon,  slash, question mark  or number-sign  in ASCII  encoding, is
  ;;read: return a bytevector holding the accumulated bytes, ending byte
  ;;excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;Notice  that  an  empty  "reg-name"  component  is  valid;  also,  a
  ;;"reg-name" cannot be longer than  255 bytes: if it is, this function
  ;;returns false.
  ;;
  (define-parser-macros in-port)
  (define (%error)
    (values (return-failure) #f))
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi	(get-u8 in-port))
  			    (count	0))
      (cond ((eof-object? chi)
  	     (getter))

  	    (($fx= 255 count)
  	     (return-failure))

  	    ((or ($fx= chi INT-COLON)
  		 ($fx= chi INT-SLASH)
  		 ($fx= chi INT-QUESTION-MARK)
  		 ($fx= chi INT-NUMBER-SIGN))
  	     (set-position-back-one! chi)
  	     (getter))

  	    ;;Characters in the categories "unreserved" and "sub-delims"
  	    ;;are valid.
  	    ((or ($is-unreserved? chi) ($is-sub-delim? chi))
  	     (put-u8 ou-port chi)
  	     (process-next-byte (get-u8 in-port) (+ 1 count)))

  	    ;;A percent-encoded sequence is valid.
  	    (($fx= chi INT-PERCENT)
  	     (let ((chi1 (get-u8 in-port)))
  	       (cond ((eof-object? chi1)
  		      (return-failure))
  		     (($is-hex-digit? chi1)
  		      (let ((chi2 (get-u8 in-port)))
  			(cond ((eof-object? chi2)
  			       (return-failure))
  			      (($is-hex-digit? chi2)
  			       (put-u8 ou-port INT-PERCENT)
  			       (put-u8 ou-port chi1)
  			       (put-u8 ou-port chi2)
  			       (process-next-byte (get-u8 in-port) (+ 1 count)))
  			      (else
  			       (return-failure)))))
  		     ;;Invalid byte in percent-encoded sequence.
  		     (else
  		      (return-failure)))))

  	    ;;Invalid byte.
  	    (else
  	     (return-failure))))))

(define (parse-host in-port)
  ;;Accumulate  bytes from  IN-PORT while  they are  valid for  a "host"
  ;;component;  parse the  accumulated bytes  as "host"  and  return two
  ;;values,  the first being  one of  the Scheme  symbols: ipv4-address,
  ;;ipv6-address, ipvfuture, reg-name.
  ;;
  ;;The second returned value depends upon the first:
  ;;
  ;;ipv4-address: the second value is  a pair, whose car is a bytevector
  ;;holding the  accumulated input and  whose cdr is  a list of  4 exact
  ;;integers representing the octets.
  ;;
  ;;ipv6-address: the second value is  a pair, whose car is a bytevector
  ;;holding  the   accumulated  input  (without   the  enclosing  square
  ;;brackets) and whose  cdr is a list of  8 exact integers representing
  ;;the address components.
  ;;
  ;;ipvfuture:  the second value  is a  pair, whose  car is  the version
  ;;number as  exact integer  in the range  [0, 15]  and whose cdr  is a
  ;;possibly empty bytevector holding the accumulated input (without the
  ;;enclosing square brackets).
  ;;
  ;;reg-name: the  second value is  a possibly empty  bytevector holding
  ;;the accumulated bytes.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  (define-parser-macros in-port)
  (define (%error)
    (values (return-failure) #f))
  ;;We start by looking for an  "IP-literal" even though it is the least
  ;;probable; this is because once  we have verified that the first byte
  ;;is not a  "[" we can come back from  PARSE-IP-LITERAL: this is quick
  ;;compared to what PARSE-IPV4-ADDRESS  has to do.  PARSE-REG-NAME must
  ;;be the last because it is a "catch all" parser function.
  (let ((ip-literal.bv (parse-ip-literal in-port)))
    (if ip-literal.bv
	(let ((ip-literal.port (open-bytevector-input-port ip-literal.bv)))
	  (let-values (((ipv6.bv ipv6.ell) (parse-ipv6-address ip-literal.port)))
	    (if ipv6.bv
		(values 'ipv6-address (cons ipv6.bv ipv6.ell))
	      (let-values (((ipvfuture.version ipvfuture.bv) (parse-ipvfuture ip-literal.port)))
		(if ipvfuture.version
		    (values 'ipvfuture (cons ipvfuture.version ipvfuture.bv))
		  (%error))))))
      (let-values (((ipv4.bv ipv4.ell) (parse-ipv4-address in-port)))
	(if ipv4.bv
	    (values 'ipv4-address (cons ipv4.bv ipv4.ell))
	  (let ((reg-name.bv (parse-reg-name in-port)))
	    (if reg-name.bv
		(values 'reg-name reg-name.bv)
	      (values #f #f))))))))

(define (parse-port in-port)
  ;;Accumulate  bytes from  IN-PORT while  they are  valid for  a "port"
  ;;component  in  the  "authority"  component.   The  first  byte  must
  ;;represent a  colon in ASCII encoding;  after that: if EOF  or a byte
  ;;not representing a decimal digit, in ASCII encoding, is read: return
  ;;a bytevector holding the accumulated bytes, starting colon excluded;
  ;;else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;Notice that an  empty "port" component after the  mandatory colon is
  ;;valid.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (if (or (eof-object? chi) (not ($fx= chi INT-COLON)))
	(return-failure)
      (receive (ou-port getter)
	  (open-bytevector-output-port)
	(let process-next-byte ((chi (get-u8 in-port)))
	  (cond ((eof-object? chi)
		 (getter))

		(($is-dec-digit? chi)
		 (put-u8 ou-port chi)
		 (process-next-byte (get-u8 in-port)))

		(else
		 (set-position-back-one! chi)
		 (getter))))))))


;;;; segment component parsers

(define (parse-segment in-port)
  ;;Accumulate bytes from  IN-PORT while they are valid  for a "segment"
  ;;component; notice that an empty "segment" is valid.
  ;;
  ;;If  EOF or  a  byte not  valid for  a  "segment" is  read: return  a
  ;;possibly  empty bytevector  holding  the bytes  accumulated so  far,
  ;;invalid byte  excluded; the  port position is  left pointing  to the
  ;;byte after the last accumulated one.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an exception  is
  ;;raised with type "&uri-parser-error"; the port position is rewind to
  ;;the one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (getter))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" or "@" are valid.
	    (($is-pchar-not-percent-encoded? chi)
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    (($fx= chi INT-PERCENT)
	     (%parse-percent-encoded-sequence who in-port ou-port (lambda ()
								    (set-position-start!)))
	     (process-next-byte (get-u8 in-port)))

	    ;;Any other character terminates the segment.
	    (else
	     (set-position-back-one! chi)
	     (getter))))))

(define (parse-segment-nz in-port)
  ;;Accumulate  bytes   from  IN-PORT  while   they  are  valid   for  a
  ;;"segment-nz"  component; notice  that an  empty "segment-nz"  is not
  ;;valid.
  ;;
  ;;If the first read operation returns EOF or an invalid byte: the port
  ;;position is  restored to the one  before this function  call and the
  ;;return value is false.
  ;;
  ;;If, after at least one valid byte is read, EOF or an invalid byte is
  ;;read:  return a  bytevector holding  the bytes  accumulated  so far,
  ;;invalid byte  excluded; the  port position is  left pointing  to the
  ;;byte after the last accumulated one.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment-nz)
  (define at-least-one? #f)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (if at-least-one?
		 (getter)
	       (return-failure)))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" or "@" are valid.
	    (($is-pchar-not-percent-encoded? chi)
	     (put-u8 ou-port chi)
	     (set! at-least-one? #t)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    (($fx= chi INT-PERCENT)
	     (%parse-percent-encoded-sequence who in-port ou-port (lambda ()
								    (set-position-start!)))
	     (process-next-byte (get-u8 in-port)))

	    ;;Any other character terminates the segment.
	    (else
	     (if at-least-one?
		 (begin
		   (set-position-back-one! chi)
		   (getter))
	       (return-failure)))))))

(define (parse-segment-nz-nc in-port)
  ;;Accumulate  bytes   from  IN-PORT  while   they  are  valid   for  a
  ;;"segment-nz-nc" component;  notice that an  empty "segment-nz-nc" is
  ;;not valid.
  ;;
  ;;If the first read operation returns EOF or an invalid byte: the port
  ;;position is  restored to the one  before this function  call and the
  ;;return value is false.
  ;;
  ;;If, after at least one valid byte is read, EOF or an invalid byte is
  ;;read:  return a  bytevector holding  the bytes  accumulated  so far,
  ;;invalid byte  excluded; the  port position is  left pointing  to the
  ;;byte after the last accumulated one.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment-nz-nc)
  (define at-least-one? #f)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (if at-least-one?
		 (getter)
	       (return-failure)))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" or "@" are valid.
	    ((or ($is-unreserved? chi)
		 ($is-sub-delim? chi)
		 ($fx= chi INT-AT-SIGN))
	     (put-u8 ou-port chi)
	     (set! at-least-one? #t)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    (($fx= chi INT-PERCENT)
	     (%parse-percent-encoded-sequence who in-port ou-port (lambda ()
								    (set-position-start!)))
	     (process-next-byte (get-u8 in-port)))

	    ;;Any other character terminates the segment.
	    (else
	     (if at-least-one?
		 (begin
		   (set-position-back-one! chi)
		   (getter))
	       (return-failure)))))))

(define (parse-slash-and-segment in-port)
  ;;Attempt  to read  from  IN-PORT the  sequence  slash character  plus
  ;;"segment" component; notice that an empty "segment" is valid.
  ;;
  ;;If  these  components are  successfully  read:  return a  bytevector
  ;;(possibly empty)  holding the accumulated "segment"  bytes; the port
  ;;position is  left pointing  to the byte  after the  last accumulated
  ;;byte from the "segment".
  ;;
  ;;If EOF or a byte different  from slash is read as first byte: return
  ;;false; the port  position is rewind to the  one before this function
  ;;call.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (if (or (eof-object? chi) (not ($fx= chi INT-SLASH)))
	  (return-failure)
	;;In case of  failure from PARSE-SEGMENT: we do  not just return
	;;its return value  because we have to rewind  the port position
	;;to the slash byte.
	(let ((bv (with-exception-handler
		      (lambda (E)
			(set-position-start!)
			(raise E))
		    (lambda ()
		      (parse-segment in-port)))))
	  (or bv (return-failure)))))))


;;;; path components

(define (parse-path-empty in-port)
  ;;Parse a "path-empty" component;  lookahead one byte from IN-PORT: if
  ;;it  is EOF  or a  question mark  or number-sign  in  ASCII encoding:
  ;;return null; else return false.
  ;;
  ;;In any case leave the port position where it was before the function
  ;;call.
  ;;
  (define-parser-macros in-port)
  (let ((chi (lookahead-u8 in-port)))
    (if (or (eof-object? chi)
	    ($fx= chi INT-QUESTION-MARK)
	    ($fx= chi INT-NUMBER-SIGN))
	'()
      #f)))

(define (parse-path-abempty in-port)
  ;;Parse from  IN-PORT a, possibly  empty, sequence of  sequences: byte
  ;;representing  the  slash  character  in  ASCII  encoding,  "segment"
  ;;component.   Return  a   possibly  empty  list  holding  bytevectors
  ;;representing the segments.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (with-exception-handler
      (lambda (E)
	(set-position-start!)
	(raise E))
    (lambda ()
      (let read-next-segment ((segments '()))
	(let ((bv (parse-slash-and-segment in-port)))
	  (if (bytevector? bv)
	      (read-next-segment (cons bv segments))
	    (reverse segments)))))))

(define (parse-path-absolute in-port)
  ;;Parse  from   IN-PORT  a  "path-absolute"  component;   it  is  like
  ;;PARSE-PATH-ABEMPTY,  but  expects  a  slash  as  first  byte  and  a
  ;;non-slash  as second  byte.  Return  a possibly  empty  list holding
  ;;bytevectors representing the segments, or false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-absolute" can  be just a slash character with no
  ;;segments attached.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (if (or (eof-object? chi) (not ($fx= chi INT-SLASH)))
	(return-failure)
      (let ((chi1 (lookahead-u8 in-port)))
	(if (and (not (eof-object? chi1)) ($fx= chi1 INT-SLASH))
	    (return-failure)
	  (begin
	    (set-position-back-one! chi)
	    (parse-path-abempty in-port)))))))

(define (parse-path-noscheme in-port)
  ;;Parse  from  IN-PORT  a  "path-noscheme" URI  component.   Return  a
  ;;non-empty  list holding  bytevectors representing  the  segments, or
  ;;false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-noscheme" must not start with a slash character,
  ;;and then  it must have  at least one non-empty  "segment" component;
  ;;the first "segment" must not contain a colon caracter.
  ;;
  (define-parser-macros in-port)
  (let ((bv (parse-segment-nz-nc in-port)))
    (if (and bv (let ((chi (lookahead-u8 in-port)))
		  (or (eof-object? chi) (not ($fx= chi INT-COLON)))))
	(let ((segments (parse-path-abempty in-port)))
	  (if segments
	      (cons bv segments)
	    (list bv)))
      (return-failure))))

(define (parse-path-rootless in-port)
  ;;Parse  from  IN-PORT  a  "path-rootless" URI  component.   Return  a
  ;;non-empty  list holding  bytevectors representing  the  segments, or
  ;;false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-rootless" must not start with a slash character,
  ;;and then it must have at least one non-empty "segment" component.
  ;;
  (let ((bv (parse-segment-nz in-port)))
    (and bv
	 (let ((segments (parse-path-abempty in-port)))
	   (if segments
	       (cons bv segments)
	     (list bv))))))


(define (parse-path in-port)
  ;;Parse from IN-PORT a "path"  component.  Return two values: false or
  ;;one    of     the    symbols    "path-absolute",    "path-noscheme",
  ;;"path-rootless", "path-empty"; the  list of bytevectors representing
  ;;the segments, possibly null.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that the "path" component can be followed only by EOF.
  ;;
  (define who 'parse-path)
  (define (%check-eof)
    (unless (eof-object? (lookahead-u8 in-port))
      (raise-uri-parser-error who
	"expected end of input after segments while parsing path"
	(port-position in-port) in-port)))
  (let ((chi (lookahead-u8 in-port)))
    (cond ((eof-object? chi)
	   (values 'path-empty '()))
	  ((parse-path-absolute in-port)
	   => (lambda (segments)
		(%check-eof)
		(values 'path-absolute segments)))
	  ((and ($fx= chi INT-SLASH) (parse-path-abempty in-port))
	   => (lambda (segments)
		(%check-eof)
		(values 'path-abempty segments)))
	  ((parse-path-noscheme in-port)
	   => (lambda (segments)
		(%check-eof)
		(values 'path-noscheme segments)))
	  ((parse-path-rootless in-port)
	   => (lambda (segments)
		(%check-eof)
		(values 'path-rootless segments)))
	  (else
	   (raise-uri-parser-error who
	     "invalid input while parsing path"
	     (port-position in-port) in-port)))))


(define (parse-uri in-port)
  ;;Read bytes from IN-PORT expecting to get, from the first byte to the
  ;;EOF,  a URI  component;  parse  the input  decomposing  it into  its
  ;;subcomponents.   This function does  not decode  the percent-encoded
  ;;bytes.
  ;;
  ;;Return multiple value being:
  ;;
  ;;scheme: a bytevector representing the scheme component; false if the
  ;;scheme is not  present.  According to the RFC:  the scheme component
  ;;is mandatory, but this function accepts its absence.
  ;;
  ;;authority:  a bytevector representing  the authority  component, not
  ;;including  the  leading  slashes;  false  if the  authority  is  not
  ;;present.
  ;;
  ;;userinfo:  a  bytevector representing  the  userinfo component,  not
  ;;including the ending at-sign.
  ;;
  ;;host-type: one of the symbols: reg-name, ipv4-address, ipv6-address,
  ;;ipvfuture; when the host is empty: this value is reg-name.
  ;;
  ;;host:  host  data  represented  as  the  second  return  value  from
  ;;PARSE-HOST.
  ;;
  ;;port:  a bytevector representing  the port  component; false  if the
  ;;port is not present.
  ;;
  ;;path-type:   one   of   the   symbols:   path-abempty,   path-empty,
  ;;path-absolute, path-rootless.   When the authority  is present: this
  ;;value is always path-abempty.
  ;;
  ;;path: a possibly empty list representing the path segments.
  ;;
  ;;query: a bytevector representing the query component; false when the
  ;;query is not present.
  ;;
  ;;fragment:  a bytevector representing  the fragment  component; false
  ;;when the fragment is not present.
  ;;
  ;;If  the  host  cannot  be  classified  in  reg-name,  ip-literal  or
  ;;ipvfuture:  an   exception  is  raised   with  condition  components
  ;;&parser-error, &who,  &message, &irritants, the  irritants being the
  ;;input port.
  ;;
  ;;If  the path  cannot  be  classified: an  exception  is raised  with
  ;;condition components &parser-error,  &who, &message, &irritants, the
  ;;irritants being the input port.
  ;;
  (define-parser-macros in-port)
  (let* ((scheme	(parse-scheme in-port))
	 (authority	(parse-authority in-port))
	 (auth-port	(and authority (open-bytevector-input-port authority)))
	 (userinfo	(and auth-port (parse-userinfo auth-port)))
	 (host-position	(port-position in-port)))
    (let-values (((host-type host)
		  (if auth-port
		      (parse-host auth-port)
		    (values 'reg-name '#vu8()))))
      (let ((port (and auth-port (parse-port auth-port))))
	(let-values (((path-type path)
		      (if authority
			  (values 'path-abempty (parse-path-abempty in-port))
			(let ((chi (lookahead-u8 in-port)))
			  (cond ((or (eof-object? chi)
				     ($fx= chi INT-QUESTION-MARK)
				     ($fx= chi INT-NUMBER-SIGN))
				 (values 'path-empty '()))
				((parse-path-absolute in-port)
				 => (lambda (segments)
				      (values 'path-absolute segments)))
				((parse-path-rootless in-port)
				 => (lambda (segments)
				      (values 'path-rootless segments)))
				(else
				 (raise-uri-parser-error 'parse-uri
				   "invalid path component while parsing URI"
				   (port-position in-port) in-port)))))))
	  (let* ((query	(parse-query in-port))
		 (fragment	(parse-fragment in-port)))
	    (assert (eof-object? (lookahead-u8 in-port)))
	    (values scheme authority userinfo host-type host port path-type path query fragment)))))))


(define (parse-relative-ref in-port)
  ;;Read bytes from IN-PORT expecting to get, from the first byte to the
  ;;EOF, a  relative-ref component; parse the input  decomposing it into
  ;;its   subcomponents.    This    function   does   not   decode   the
  ;;percent-encoded bytes.
  ;;
  ;;Return multiple value being:
  ;;
  ;;authority:  a bytevector representing  the authority  component, not
  ;;including  the  leading  slashes;  false  if the  authority  is  not
  ;;present.
  ;;
  ;;userinfo:  a  bytevector representing  the  userinfo component,  not
  ;;including the ending at-sign.
  ;;
  ;;host-type: one of the symbols: reg-name, ipv4-address, ipv6-address,
  ;;ipvfuture; when the host is empty: this value is reg-name.
  ;;
  ;;host:  host  data  represented  as  the  second  return  value  from
  ;;PARSE-HOST.
  ;;
  ;;port:  a bytevector representing  the port  component; false  if the
  ;;port is not present.
  ;;
  ;;path-type:   one   of   the   symbols:   path-abempty,   path-empty,
  ;;path-absolute, path-noscheme.   When the authority  is present: this
  ;;value is always path-abempty.
  ;;
  ;;path: a possibly empty list representing the path segments.
  ;;
  ;;query: a bytevector representing the query component; false when the
  ;;query is not present.
  ;;
  ;;fragment:  a bytevector representing  the fragment  component; false
  ;;when the fragment is not present.
  ;;
  ;;If  the  host  cannot  be  classified  in  reg-name,  ip-literal  or
  ;;ipvfuture:  an   exception  is  raised   with  condition  components
  ;;&parser-error, &who,  &message, &irritants, the  irritants being the
  ;;input port.
  ;;
  ;;If the  path cannot  be classified in:  an exception is  raised with
  ;;condition components &parser-error,  &who, &message, &irritants, the
  ;;irritants being the input port.
  ;;
  (define-parser-macros in-port)
  (let* ((authority	(parse-authority in-port))
	 (auth-port	(and authority (open-bytevector-input-port authority)))
	 (userinfo	(and auth-port (parse-userinfo auth-port)))
	 (host-position	(port-position in-port)))
    (let-values (((host-type host)
		  (if auth-port
		      (parse-host auth-port)
		    (values 'reg-name '#vu8()))))
      (let ((port (and auth-port (parse-port auth-port))))
	(let-values (((path-type path)
		      (if authority
			  (values 'path-abempty (parse-path-abempty in-port))
			(let ((chi (lookahead-u8 in-port)))
			  (cond ((or (eof-object? chi)
				     ($fx= chi INT-QUESTION-MARK)
				     ($fx= chi INT-NUMBER-SIGN))
				 (values 'path-empty '()))
				((parse-path-absolute in-port)
				 => (lambda (segments)
				      (values 'path-absolute segments)))
				((parse-path-noscheme in-port)
				 => (lambda (segments)
				      (values 'path-noscheme segments)))
				(else
				 (raise-uri-parser-error 'parse-uri
				   "invalid path component while parsing relative-ref"
				   (port-position in-port) in-port)))))))
	  (let* ((query	(parse-query in-port))
		 (fragment	(parse-fragment in-port)))
	    (assert (eof-object? (lookahead-u8 in-port)))
	    (values authority userinfo host-type host port path-type path query fragment)))))))


;;;; validation

(define (valid-component? port)
  ;;Scan bytes  from PORT  until EOF is  found; return two  values, when
  ;;success:  true and the  port position  of the  last byte  read; when
  ;;failure: false  and the port position  of the invalid  byte.  In any
  ;;case: the port  position is reverted to the state  it had before the
  ;;call to this function.
  ;;
  ;;Ensure that:
  ;;
  ;;*  A percent  character is  followed by  two bytes  representing hex
  ;;digits.
  ;;
  ;;*  All the  non  percent-encoded  bytes are  in  the unreserved  set
  ;;defined by RFC 3986.
  ;;
  (let ((start-position (port-position port)))
    (unwind-protect
	(let ()
	  (define-inline (return bool)
	    (values bool (port-position port)))
	  (let process-next-byte ((chi (get-u8 port)))
	    (cond ((eof-object? chi)
		   (return #t))
		  (($fx= chi INT-PERCENT)
		   (let ((chi1 (get-u8 port)))
		     (cond ((eof-object? chi1)
			    (return #f))
			   (($is-hex-digit? chi1)
			    (let ((chi2 (get-u8 port)))
			      (cond ((eof-object? chi2)
				     (return #f))
				    (($is-hex-digit? chi2)
				     (process-next-byte (get-u8 port)))
				    (else
				     (return #f)))))
			   (else
			    (return #f)))))
		  (($is-unreserved? chi)
		   (process-next-byte (get-u8 port)))
		  (else
		   (return #f)))))
      (set-port-position! port start-position))))


(define (normalise-path input)
  (let next-segment ((input	input)
		     (output	'()))
    (cond ((null? input)
	   (reverse output))
	  ((equal? '#vu8(46) (car input))
	   (next-segment (cdr input) output))
	  ((equal? '#vu8(46 46) (car input))
	   (next-segment (cdr input) (if (null? output)
					 output
				       (cdr output))))
	  (else
	   (next-segment (cdr input) (cons (car input) output))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'raise-uri-parser-error 'scheme-indent-function 1)
;;End:
