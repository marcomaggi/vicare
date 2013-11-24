;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: parser functions for Unix pathnames
;;;Date: Tue Nov 19, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare parser-tools unix-pathnames)
  (export

    ;; predicates
    list-of-segments?
    pathname?			bytevector-pathname?			string-pathname?
				$bytevector-pathname?			$string-pathname?
    segment?			bytevector-segment?			string-segment?
				$bytevector-segment?			$string-segment?
    absolute?			$bytevector-absolute?			$string-absolute?
    relative?			$bytevector-relative?			$string-relative?

    ;; conversion
    string/bytevector->pathname-bytevector
    pathname-bytevector->string

    ;; parser functions
    parse-segment
    parse-segment-nz
    parse-slash-and-segment
    parse-pathname

    ;; pathname manipulation
    normalise-segments
    serialise-segments

    ;; components
    extension			$bytevector-extension			$string-extension
    dirname			$bytevector-dirname			$string-dirname
    tailname			$bytevector-tailname			$string-tailname
    rootname			$bytevector-rootname			$string-rootname
    strip-trailing-slashes	$bytevector-strip-trailing-slashes	$string-strip-trailing-slashes
    split			$bytevector-split			$string-split
    normalise			$bytevector-normalise			$string-normalise
    prefix?			$bytevector-prefix?			$string-prefix?
    suffix?			$bytevector-suffix?			$string-suffix?
    prepend			$bytevector-prepend			$string-prepend
    append			$bytevector-append			$string-append
    replace-extension		$bytevector-replace-extension		$string-replace-extension

    ;; condition objects
    &unix-pathname-parser-error
    make-unix-pathname-parser-error
    unix-pathname-parser-error?
    &unix-pathname-normalisation-error
    make-unix-pathname-normalisation-error
    unix-pathname-normalisation-error?
    raise-unix-pathname-parser-error
    raise-unix-pathname-normalisation-error)
  (import (except (vicare)
		  append)
    (vicare unsafe operations))


;;;; constants

(define-inline-constant CHI-SLASH	47) ;;(char->integer #\/)
(define-inline-constant CHI-DOT		46) ;;(char->integer #\.)

(define-inline-constant ROOT-DIRECTORY-BV
  '#vu8(47))

(define-inline-constant CURRENT-DIRECTORY-BV
  '#vu8(46))

(define-inline-constant UPLEVEL-DIRECTORY-BV
  '#vu8(46 46))

(define-inline-constant ROOT-DIRECTORY-STR
  "/")

(define-inline-constant CURRENT-DIRECTORY-STR
  ".")

(define-inline-constant UPLEVEL-DIRECTORY-STR
  "..")


;;;; helpers

(define-inline ($valid-chi-for-pathname? chi)
  ;;Assuming  CHI  is a  fixnum:  return  true if  it  is  in the  range
  ;;acceptable  for an  ASCII  char part  of a  Unix  pathname.  A  Unix
  ;;pathname might contain any octet in the range [1, 255]; that is: any
  ;;byte with the exclusion of the zero.
  ;;
  ($fx< 0 chi 256))

(define-inline ($valid-chi-for-segment? chi)
  ;;Assuming  CHI  is a  fixnum:  return  true if  it  is  in the  range
  ;;acceptable for an ASCII char part of a Unix pathname segment. A Unix
  ;;pathname segment might  contain any octet in the ranges  [1, 47) and
  ;;[48, 255]; that is: any byte with  the exclusion of the zero and 47,
  ;;which represents the slash character in ASCII encoding.
  ;;
  (or ($fx< 0         chi CHI-SLASH)
      ($fx< CHI-SLASH chi 256)))

;;; --------------------------------------------------------------------

(define-inline ($root-bv? bv)
  ;;Assuming BV is  a bytevector: return true if it  holds a single byte
  ;;being the ASCII representation of the slash.
  ;;
  (and ($fx= 1 ($bytevector-length bv))
       ($fx= CHI-SLASH ($bytevector-u8-ref bv 0))))

(define-inline ($root-str? str)
  ;;Assuming STR is a string: return true if it holds a single character
  ;;being the ASCII representation of the slash.
  ;;
  (and ($fx= 1 ($string-length str))
       ($char= #\/ ($string-ref str 0))))

;;; --------------------------------------------------------------------

(define-inline ($dot-bv? bv)
  ;;Assuming BV is  a bytevector: return true if it  holds a single byte
  ;;being the ASCII representation of the dot.
  ;;
  (and ($fx=  1 ($bytevector-length bv))
       ($fx= CHI-DOT ($bytevector-u8-ref bv 0))))

(define-inline ($dot-str? str)
  ;;Assuming STR is a string: return true if it holds a single character
  ;;being the ASCII representation of the dot.
  ;;
  (and ($fx=  1 ($string-length str))
       ($char= #\. ($string-ref str 0))))

;;; --------------------------------------------------------------------

(define-inline ($dot-dot-bv? bv)
  ;;Assuming BV is a bytevector: return  true if it holds two bytes both
  ;;being the ASCII representation of the dot.
  ;;
  (and ($fx= 2       ($bytevector-length bv))
       ($fx= CHI-DOT ($bytevector-u8-ref bv 0))
       ($fx= CHI-DOT ($bytevector-u8-ref bv 1))))

(define-inline ($dot-dot-str? str)
  ;;Assuming STR  is a string:  return true  if it holds  two characters
  ;;both being the ASCII representation of the dot.
  ;;
  (and ($fx= 2     ($string-length str))
       ($char= #\. ($string-ref str 0))
       ($char= #\. ($string-ref str 1))))

;;; --------------------------------------------------------------------

(define ($subbytevector-or-full bv start past)
  ;;If the fixnums START and PAST select the whole bytevector BV: return
  ;;BV itself, otherwise return the selected subbytevector.
  ;;
  (if (and ($fxzero? start)
	   ($fx= past ($bytevector-length bv)))
      bv
    ($subbytevector-u8 bv start past)))

(define ($substring-or-full str start past)
  ;;If the  fixnums START and PAST  select the whole string  STR: return
  ;;STR itself, otherwise return the selected substring.
  ;;
  (if (and ($fxzero? start)
	   ($fx= past ($string-length str)))
      str
    ($substring str start past)))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-chi-slash? bv i)
  ($fx= CHI-SLASH ($bytevector-u8-ref bv i)))

(define-inline ($bytevector-chi-dot? bv i)
  ($fx= CHI-DOT   ($bytevector-u8-ref bv i)))

(define-inline ($string-chi-slash? str i)
  ($char= #\/ ($string-ref str i)))

(define-inline ($string-chi-dot? str i)
  ($char= #\. ($string-ref str i)))

;;; --------------------------------------------------------------------

(define ($bytevector-backwards-index-of-slash str i)
  ;;Examples:
  ;;
  ;;  ($bytevector-backwards-index-of-slash '#ve(ascii "ciao") 3)	=> 4
  ;;  ($bytevector-backwards-index-of-slash '#ve(ascii "cia/") 3)	=> 3
  ;;  ($bytevector-backwards-index-of-slash '#ve(ascii "ci//") 3)	=> 2
  ;;  ($bytevector-backwards-index-of-slash '#ve(ascii "c///") 3)	=> 1
  ;;  ($bytevector-backwards-index-of-slash '#ve(ascii "////") 3)	=> 0
  ;;  ($bytevector-backwards-index-of-slash '#ve(ascii "////") 0)	=> 0
  ;;
  (cond (($fxnegative? i)
	 0)
	(($bytevector-chi-slash? str i)
	 ($bytevector-backwards-index-of-slash str ($fxsub1 i)))
	(else
	 ($fxadd1 i))))

(define ($string-backwards-index-of-slash str i)
  ;;Examples:
  ;;
  ;;  ($string-backwards-index-of-slash "ciao" 3)	=> 4
  ;;  ($string-backwards-index-of-slash "cia/" 3)	=> 3
  ;;  ($string-backwards-index-of-slash "ci//" 3)	=> 2
  ;;  ($string-backwards-index-of-slash "c///" 3)	=> 1
  ;;  ($string-backwards-index-of-slash "////" 3)	=> 0
  ;;  ($string-backwards-index-of-slash "////" 0)	=> 0
  ;;
  (cond (($fxnegative? i)
	 0)
	(($string-chi-slash? str i)
	 ($string-backwards-index-of-slash str ($fxsub1 i)))
	(else
	 ($fxadd1 i))))

;;; --------------------------------------------------------------------

(case-define $bytevector-index-past-last-segment
  ((bv)
   ($bytevector-index-past-last-segment bv (bytevector-length bv)))
  ((bv past)
   ;;This function assumes that:
   ;;
   ;;   (assert (and (bytevector? BV)
   ;;                (positive? (bytevector-length BV))
   ;;                (<= PAST (bytevector-length BV))))
   ;;
   ;;It is  a recursive function  which is meant  to be called  the first
   ;;time with: LAST == (bytevector-length BV).
   ;;
   ;;Scan backwards the bytevector skipping the octets representing slash
   ;;characters  in   ASCII  encoding;   return  a   non-negative  fixnum
   ;;representing the index past the  last character in the last pathname
   ;;segment; if PAST is zero: return zero.
   ;;
   ;;Examples:
   ;;
   ;;  ($bytevector-index-past-last-segment '#ve(ascii "ciao") 4) => 4
   ;;  ($bytevector-index-past-last-segment '#ve(ascii "cia/") 4) => 3
   ;;  ($bytevector-index-past-last-segment '#ve(ascii "ci//") 4) => 2
   ;;  ($bytevector-index-past-last-segment '#ve(ascii "c///") 4) => 1
   ;;  ($bytevector-index-past-last-segment '#ve(ascii "////") 4) => 0
   ;;
   (if ($fxzero? past)
       past
     (let ((i ($fxsub1 past)))
       (if ($bytevector-chi-slash? bv i)
	   ($bytevector-index-past-last-segment bv i)
	 past)))))

(case-define $string-index-past-last-segment
  ((str)
   ($string-index-past-last-segment str ($string-length str)))
  ((str past)
   ;;This function assumes that:
   ;;
   ;;   (assert (and (string? str)
   ;;                (positive? (string-length str))
   ;;                (<= past (string-length str))))
   ;;
   ;;It is  a recursive function  which is meant  to be called  the first
   ;;time with: LAST == (string-length STR).
   ;;
   ;;Scan backwards  the string skipping  the slash characters;  return a
   ;;non-negative fixnum  representing the index past  the last character
   ;;in the last pathname segment; if PAST is zero: return zero.
   ;;
   ;;Examples:
   ;;
   ;;  ($string-index-past-last-segment "ciao" 4)	=> 4
   ;;  ($string-index-past-last-segment "cia/" 4)	=> 3
   ;;  ($string-index-past-last-segment "ci//" 4)	=> 2
   ;;  ($string-index-past-last-segment "c///" 4)	=> 1
   ;;  ($string-index-past-last-segment "////" 4)	=> 0
   ;;
   (if ($fxzero? past)
       past
     (let ((i ($fxsub1 past)))
       (if ($string-chi-slash? str i)
	   ($string-index-past-last-segment str i)
	 past)))))

;;; --------------------------------------------------------------------

(define ($bytevector-end-of-double-dot? bv i past)
  ;;This function assumes that:
  ;;
  ;;   (assert (and (bytevector? bv)
  ;;                (<= past (bytevector-length str))
  ;;                (<  i    past)
  ;;                ($bytevector-chi-dot? bv i)))
  ;;
  ;;Return true if I references the  second dot in a double-dot pathname
  ;;segment.  Examples:
  ;;
  ;;  ($bytevector-end-of-double-dot? '#ve(ascii "..")     1 2)	=> #t
  ;;  ($bytevector-end-of-double-dot? '#ve(ascii "/..")    2 3)	=> #t
  ;;  ($bytevector-end-of-double-dot? '#ve(ascii "a..")    2 3)	=> #f
  ;;  ($bytevector-end-of-double-dot? '#ve(ascii "a.")     1 2)	=> #f
  ;;  ($bytevector-end-of-double-dot? '#ve(ascii ".emacs") 0 6)	=> #f
  ;;  ($bytevector-end-of-double-dot? '#ve(ascii "..a")    1 3)	=> #f
  ;;
  (and ($fx= past ($fxadd1 i))
       (let ((j ($fxsub1 i)))
	 (cond (($fxnegative? j)
		#f)
	       (($bytevector-chi-dot? bv j)
		(or ($fxzero? j)
		    ($bytevector-chi-slash? bv ($fxsub1 j))))
	       (else #f)))))

(define ($string-end-of-double-dot? str i past)
  ;;This function assumes that:
  ;;
  ;;   (assert (and (string? str)
  ;;                (<= past (string-length str))
  ;;                (<  i    past)
  ;;                ($string-chi-dot? str i)))
  ;;
  ;;Return true if I references the  second dot in a double-dot pathname
  ;;segment.  Examples:
  ;;
  ;;  ($string-end-of-double-dot? ".."     1 2)	=> #t
  ;;  ($string-end-of-double-dot? "/.."    2 3)	=> #t
  ;;  ($string-end-of-double-dot? "a.."    2 3)	=> #f
  ;;  ($string-end-of-double-dot? "a."     1 2)	=> #f
  ;;  ($string-end-of-double-dot? ".emacs" 0 6)	=> #f
  ;;  ($string-end-of-double-dot? "..a"    1 3)	=> #f
  ;;
  (and ($fx= past ($fxadd1 i))
       (let ((j ($fxsub1 i)))
	 (cond (($fxnegative? j)
		#f)
	       (($string-chi-dot? str j)
		(or ($fxzero? j)
		    ($string-chi-slash? str ($fxsub1 j))))
	       (else #f)))))

;;; --------------------------------------------------------------------

(define ($bytevector-beginning-of-segment? bv i)
  ;;This function assumes that:
  ;;
  ;;   (assert (and (bytevector? bv)
  ;;                (< i (bytevector-length bv))
  ;;                ($bytevector-chi-dot? bv i)))
  ;;
  ;;Return true if I references an  octet at the beginning of a pathname
  ;;segment.
  ;;
  (or ($fxzero? i)
      ($bytevector-chi-slash? bv ($fxsub1 i))))

(define ($string-beginning-of-segment? str i)
  ;;This function assumes that:
  ;;
  ;;   (assert (and (string? str)
  ;;                (< i (string-length str))))
  ;;
  ;;Return  true if  I  references a  character at  the  beginning of  a
  ;;pathname segment.
  ;;
  (or ($fxzero? i)
      ($string-chi-slash? str ($fxsub1 i))))

;;; --------------------------------------------------------------------

(define ($bytevector-last-is-slash? bv)
  ($fx= CHI-SLASH ($bytevector-u8-ref bv ($fxsub1 ($bytevector-length bv)))))

(define ($string-last-is-slash? str)
  ($char= #\/ ($string-ref str ($fxsub1 ($string-length str)))))

;;; --------------------------------------------------------------------

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


;;;; condition objects

(define-condition-type &unix-pathname-parser-error
    &error
  make-unix-pathname-parser-error
  unix-pathname-parser-error?)

(define-condition-type &unix-pathname-normalisation-error
    &error
  make-unix-pathname-normalisation-error
  unix-pathname-normalisation-error?)

(define (raise-unix-pathname-parser-error who message . irritants)
  (raise
   (condition (make-unix-pathname-parser-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))

(define (raise-unix-pathname-normalisation-error who message . irritants)
  (raise
   (condition (make-unix-pathname-normalisation-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))


;;;; pathname predicates

(define (pathname? obj)
  (or (bytevector-pathname? obj)
      (string-pathname?     obj)))

(define (bytevector-pathname? obj)
  (and (bytevector? obj)
       ($bytevector-pathname? obj)))

(define ($bytevector-pathname? obj)
  (and ($bytevector-not-empty? obj)
       (let loop ((bv obj)
		  (i  0))
	 (cond (($fx= i ($bytevector-length bv))
		#t)
	       (($valid-chi-for-pathname? ($bytevector-u8-ref bv i))
		(loop bv ($fxadd1 i)))
	       (else
		#f)))))

(define (string-pathname? obj)
  (and (string? obj)
       ($string-pathname? obj)))

(define ($string-pathname? obj)
  (and ($string-not-empty? obj)
       (let loop ((obj obj)
		  (i   0))
	 (cond (($fx= i ($string-length obj))
		#t)
	       (($valid-chi-for-pathname? ($char->fixnum ($string-ref obj i)))
		(loop obj ($fxadd1 i)))
	       (else
		#f)))))


;;;; segment predicates

(define (segment? obj)
  (or (bytevector-segment? obj)
      (string-segment?     obj)))

(define (bytevector-segment? obj)
  (and (bytevector? obj)
       ($bytevector-segment? obj)))

(define ($bytevector-segment? obj)
  (let loop ((bv obj)
	     (i  0))
    (cond (($fx= i ($bytevector-length bv))
	   #t)
	  (($valid-chi-for-segment? ($bytevector-u8-ref bv i))
	   (loop bv ($fxadd1 i)))
	  (else #f))))

(define (string-segment? obj)
  (and (string? obj)
       ($string-segment? obj)))

(define ($string-segment? obj)
  (let loop ((obj obj)
	     (i   0))
    (cond (($fx= i ($string-length obj))
	   #t)
	  (($valid-chi-for-segment? ($char->fixnum ($string-ref obj i)))
	   (loop obj ($fxadd1 i)))
	  (else #f))))

;;; --------------------------------------------------------------------

(define (list-of-segments? obj)
  (cond ((pair? obj)
	 (and (segment? ($car obj))
	      (list-of-segments? ($cdr obj))))
	((null? obj)
	 #t)
	(else #f)))


;;;; plain string <-> bytevector conversion

(case-define* string/bytevector->pathname-bytevector
  ;;Convert the string or bytevector  OBJ to a bytevector representation
  ;;of  a pathname;  when successful  return a  bytevector, if  an error
  ;;occurs  raise  an  exception  using  WHO as  value  for  the  "&who"
  ;;condition object.
  ;;
  ;;When OBJ is a string: only characters whose Unicode code point is in
  ;;the range  [1, 255] are accepted,  notice that zero is  excluded; in
  ;;this  case a  new bytevector  is returned.   An empty  bytevector is
  ;;equivalent to  a bytevector representing the  current directory: the
  ;;return value is #vu8(46).
  ;;
  ;;When OBJ  is a  bytevector: all  the octets  are accepted,  with the
  ;;exception of  the octet zero; in  this case OBJ itself  is returned.
  ;;An  empty string  is  equivalent to  a  bytevector representing  the
  ;;current directory: the return value is #vu8(46).
  ;;
  ((obj)
   (string/bytevector->pathname-bytevector obj __who__))
  ((obj who)
   (cond ((bytevector? obj)
	  (if ($bytevector-empty? obj)
	      CURRENT-DIRECTORY-BV
	    (do ((i 0 ($fxadd1 i)))
		(($fx= i ($bytevector-length obj))
		 obj)
	      (unless ($valid-chi-for-pathname? ($bytevector-u8-ref obj i))
		(raise-unix-pathname-parser-error who
		  "octet from bytevector out of range for pathname"
		  obj ($bytevector-u8-ref obj i))))))
	 ((string? obj)
	  (if ($string-empty? obj)
	      CURRENT-DIRECTORY-BV
	    (do ((i  0 ($fxadd1 i))
		 (bv ($make-bytevector ($string-length obj))))
		(($fx= i ($string-length obj))
		 bv)
	      (let ((chi ($char->fixnum ($string-ref obj i))))
		(if ($valid-chi-for-pathname? chi)
		    ($bytevector-u8-set! bv i chi)
		  (raise-unix-pathname-parser-error who
		    "character from string out of range for pathname"
		    obj ($string-ref obj i)))))))
	 (else
	  (procedure-argument-violation who
	    "expected string or bytevector as argument" obj)))))

(case-define* pathname-bytevector->string
  ;;Convert  the  bytevector pathname  representation  OBJ  to a  string
  ;;pathname  representation; when  successful  return a  string, if  an
  ;;error occurs  raise an exception using  WHO as value for  the "&who"
  ;;condition object.  An empty bytevector is equivalent to a bytevector
  ;;representing the current directory: the return value is ".".
  ;;
  ;;All  the octets  in the  bytevector are  considered valid,  with the
  ;;exception of the octet zero.
  ;;
  ((obj)
   (pathname-bytevector->string obj __who__))
  ((obj who)
   (if (bytevector? obj)
       (if ($bytevector-empty? obj)
	   "."
	 (do ((i   0 ($fxadd1 i))
	      (str (make-string ($bytevector-length obj))))
	     (($fx= i ($bytevector-length obj))
	      str)
	   (let ((chi ($bytevector-u8-ref obj i)))
	     (if ($valid-chi-for-pathname? chi)
		 ($string-set! str i ($fixnum->char chi))
	       (raise-unix-pathname-parser-error who
		 "octet from bytevector out of range for pathname"
		 obj ($bytevector-u8-ref obj i))))))
     (procedure-argument-violation who
       "expected pathname bytevector as argument" obj))))


;;;; segment component parsers

(define* (parse-segment in-port)
  ;;Accumulate bytes from the binary  input port IN-PORT, while they are
  ;;valid  for a  "segment"  component; IN-PORT  must  support the  port
  ;;position.  Notice  that an empty  "segment" is valid:  it represents
  ;;the  segment between  2  slash characters;  a  pathanme like  "a//b"
  ;;should be parsed as:
  ;;
  ;;  (#ve(ascii "a") #vu8() #ve(ascii "b"))
  ;;
  ;;where the empty bytevector is  equivalent to the "current directory"
  ;;segment: #ve(ascii ".").
  ;;
  ;;If  EOF or  a slash  character  in ASCII  coding is  read: return  a
  ;;possibly empty bytevector holding the  bytes accumulated so far; the
  ;;port  position  is  left  pointing   to  the  byte  after  the  last
  ;;accumulated one.
  ;;
  ;;If  an   invalid  byte  is   read:  an  exception  is   raised  with
  ;;"raise-unix-pathname-parser-error"; the  port position is  rewind to
  ;;the one before this function call.
  ;;
  (define-parser-macros in-port)
  (define* (%error)
    (let ((pos (port-position in-port)))
      (set-position-start!)
      (raise-unix-pathname-parser-error __who__
	"invalid byte while parsing pathname" in-port pos)))
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (getter))

	    (($fx= chi CHI-SLASH)
	     (set-position-back-one! chi)
	     (getter))

	    (($valid-chi-for-segment? chi)
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port)))

	    (else
	     (%error))))))

(define* (parse-segment-nz in-port)
  ;;Accumulate  bytes   from  IN-PORT  while   they  are  valid   for  a
  ;;"segment-nz"  component; notice  that an  empty "segment-nz"  is not
  ;;valid.
  ;;
  ;;If the  first read  operation returns  EOF or  a slash  character in
  ;;ASCII coding: the  port position is restored to the  one before this
  ;;function call and the return value is false.
  ;;
  ;;If, after at least  one valid byte is read, EOF or  a slash is read:
  ;;return a bytevector  holding the bytes accumulated so  far; the port
  ;;position is  left pointing  to the byte  after the  last accumulated
  ;;one.
  ;;
  ;;If  an   invalid  byte  is   read:  an  exception  is   raised  with
  ;;"raise-unix-pathname-parser-error"; the  port position is  rewind to
  ;;the one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment-nz)
  (define* (%error)
    (let ((pos (port-position in-port)))
      (set-position-start!)
      (raise-unix-pathname-parser-error __who__
	"invalid byte while parsing pathname" in-port pos)))
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi		(get-u8 in-port))
			    (at-least-one?	#f))
      (cond ((eof-object? chi)
	     (if at-least-one?
		 (getter)
	       (return-failure)))

	    (($fx= chi CHI-SLASH)
	     (if at-least-one?
		 (begin
		   (set-position-back-one! chi)
		   (getter))
	       (return-failure)))

	    (($valid-chi-for-segment? chi)
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port) #t))

	    (else
	     (%error))))))

(define* (parse-slash-and-segment in-port)
  ;;Attempt  to read  from  IN-PORT the  sequence  slash character  plus
  ;;"segment" component; notice that an empty "segment" is valid.
  ;;
  ;;If  these  components are  successfully  read:  return a  bytevector
  ;;(possibly empty)  holding the accumulated "segment"  bytes; the port
  ;;position is  left pointing  to the byte  after the  last accumulated
  ;;byte from the "segment".
  ;;
  ;;If EOF or a  valid byte different from slash is  read as first byte:
  ;;return false;  the port position  is rewind  to the one  before this
  ;;function call.
  ;;
  ;;If  an   invalid  byte  is   read:  an  exception  is   raised  with
  ;;"raise-unix-pathname-parser-error"; the  port position is  rewind to
  ;;the one before this function call.
  ;;
  (define-parser-macros in-port)
  (define* (%error)
    (let ((pos (port-position in-port)))
      (set-position-start!)
      (raise-unix-pathname-parser-error __who__
	"invalid byte while parsing pathname" in-port pos)))
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (return-failure))

	    ((not ($valid-chi-for-pathname? chi))
	     (%error))

	    ((not ($fx= chi CHI-SLASH))
	     (return-failure))

	    ;;In  case of  failure from  PARSE-SEGMENT: we  do not  just
	    ;;return its return value because we have to rewind the port
	    ;;position to the slash byte.
	    (else
	     (let ((bv (with-exception-handler
			   (lambda (E)
			     (set-position-start!)
			     (raise E))
			 (lambda ()
			   (parse-segment in-port)))))
	       (if bv
		   (if (zero? (bytevector-length bv))
		       (quote #vu8(46))
		     bv)
		 (return-failure))))))))


;;;; path components

(define* (parse-pathname in-port)
  ;;Parse from  IN-PORT an  absolute or relative  pathname until  EOF is
  ;;found;  return  two values:  a  boolean,  true  if the  pathname  is
  ;;absolute;  a possibly  empty  list of  bytevectors representing  the
  ;;segments.
  ;;
  ;;If an  invalid byte  is read  or EOF  is read  before any  octet: an
  ;;exception  is  raised with  "raise-unix-pathname-parser-error";  the
  ;;port position is rewind to the one before this function call.
  ;;
  (define-parser-macros in-port)
  (with-exception-handler
      (lambda (E)
	(set-position-start!)
	(raise E))
    (lambda ()
      (let* ((first-segment	(parse-segment-nz in-port))
	     (segments		(let read-next-segment ((segments '()))
				  (let ((bv (parse-slash-and-segment in-port)))
				    (if (bytevector? bv)
					(read-next-segment (cons bv segments))
				      (reverse segments))))))
	(cond ((and first-segment (not (null? segments)))
	       (values #f (cons first-segment segments)))
	      (first-segment
	       (values #f (list first-segment)))
	      ((not (null? segments))
	       (values #t segments))
	      (else
	       (raise-unix-pathname-parser-error __who__
		 "invalid input bytevector" in-port)))))))


(define* (normalise-segments absolute? segments)
  ;;Given  a list  of bytevectors  representing Unix  pathname segments:
  ;;normalise them, as much  as possible, removing segments representing
  ;;single-dot and double-dot directory  entries; if ABSOLUTE?  is true:
  ;;normalise SEGMENTS as  an absolute pathname, else normalise  it as a
  ;;relative pathname.  Return two values:
  ;;
  ;;1. A  boolean, true  if some  change was made  from SEGMENTS  to the
  ;;second return value;  this allows us to detect if  a normalised list
  ;;of segments when serialised into a bytevector becomes different from
  ;;the original bytevector that generated SEGMENTS.
  ;;
  ;;2.   A new,  possibly empty,  list of  bytevectors representing  the
  ;;normalisation  of SEGMENTS.   Absolute  pathname  segments can  hold
  ;;neither single-dot nor double-dot  segments: if a double-dot segment
  ;;cannot annihilate its previous segment, it is just discarded.
  ;;
  (let next-segment ((input	segments)
		     (output	'())
		     (changed?	#f))
    (cond ((pair? input)
	   (let ((segment ($car input)))
	     (unless (bytevector? segment)
	       (procedure-argument-violation __who__
		 "expected list of bytevectors as argument" segment))
	     (cond (($dot-bv? segment)
		    ;;Just skip the "current directory" segment.
		    (next-segment ($cdr input) output #t))
		   (($dot-dot-bv? segment)
		    (if absolute?
			(if (pair? output)
			    ;;Annihilate  the previous  segment.  Notice
			    ;;that here  the previous segment  cannot be
			    ;;itself a double-dot.
			    (next-segment ($cdr input) ($cdr output) #t)
			  ;;No  previous   segment,  just   discard  the
			  ;;double-dot.
			  (next-segment ($cdr input) output #t))
		      ;;Relative segment.
		      (if (or (null? output)
			      ($dot-dot-bv? ($car output)))
			  ;;No discardable previous  segment, just store
			  ;;the double-dot for future processing.
			  (next-segment ($cdr input) (cons segment output) changed?)
			;;Annihilate the previous segment.
			(next-segment ($cdr input) ($cdr output) #t))))
		   (else
		    (next-segment ($cdr input) (cons segment output) changed?)))))
	  ((null? input)
	   (values changed? (reverse output)))
	  (else
	   (procedure-argument-violation __who__
	     "expected list of bytevectors as argument" segments)))))


(define* (serialise-segments absolute? segments)
  ;;Given  a possibly  empty list  of bytevectors  representing pathname
  ;;segments build  and return  a new  bytevector representing  the full
  ;;pathname;  if ABSOLUTE?   is  true:  the first  byte  of the  result
  ;;represents a slash in ASCII encoding.
  ;;
  ;;If SEGMENTS is null and ABSOLUTE?   is true: the returned value is a
  ;;bytevector  holding a  single  byte representing  a  slash in  ASCII
  ;;encoding.
  ;;
  ;;If SEGMENTS is null and ABSOLUTE?  is false: the returned value is a
  ;;bytevector holding a single byte representing a dot in ASCII coding.
  ;;
  (if (null? segments)
      (if absolute? ROOT-DIRECTORY-BV CURRENT-DIRECTORY-BV)
    (receive (port getter)
	(open-bytevector-output-port)
      (when absolute?
	(put-u8 port CHI-SLASH))
      (put-bytevector port (car segments))
      (for-each (lambda (segment)
		  (put-u8 port CHI-SLASH)
		  (put-bytevector port segment))
	(cdr segments))
      (getter))))


;;;; pathname components: helpers

(define-syntax (define-pathname-operation stx)
  (syntax-case stx (string bytevector)
    ((_ ?who
	((bytevector)	. ?bytevector-body)
	((string)	. ?string-body))
     (with-syntax
	 ((OBJ (datum->syntax #'?who 'obj)))
       #'(define (?who OBJ)
	   (cond ((bytevector-pathname? OBJ)
		  (begin . ?bytevector-body))
		 ((string-pathname? OBJ)
		  (begin . ?string-body))
		 (else
		  (procedure-argument-violation (quote ?who)
		    "expected string or bytevector Unix pathname as argument" OBJ))))))
    ))

(define-syntax (define-pathname-operation-2 stx)
  (syntax-case stx (string bytevector)
    ((_ ?who
	((bytevector)	. ?bytevector-body)
	((string)	. ?string-body))
     (with-syntax
	 ((OBJ1 (datum->syntax #'?who 'obj1))
	  (OBJ2 (datum->syntax #'?who 'obj2)))
       #'(define (?who OBJ1 OBJ2)
	   (cond ((bytevector-pathname? OBJ1)
		  (if (bytevector-pathname? OBJ2)
		      (begin . ?bytevector-body)
		    (procedure-argument-violation (quote ?who)
		      "expected bytevector Unix pathname as second argument" OBJ2)))
		 ((string-pathname? OBJ1)
		  (if (string-pathname? OBJ2)
		      (begin . ?string-body)
		    (procedure-argument-violation (quote ?who)
		      "expected string Unix pathname as second argument" OBJ2)))
		 (else
		  (procedure-argument-violation (quote ?who)
		    "expected string or bytevector Unix pathname as first argument" OBJ1))))))
    ))


;;;; pathname predicates: absolute? relative?

(define-pathname-operation absolute?
  ;;Return true  if the argument  OBJ, which must  be a valid  string or
  ;;bytevector representation of a Unix pathname, is absolute; otherwise
  ;;return false.
  ;;
  ((bytevector)	($bytevector-absolute? obj))
  ((string)	($string-absolute?     obj)))

(define ($bytevector-absolute? obj)
  ($bytevector-chi-slash? obj 0))

(define ($string-absolute? obj)
  ($string-chi-slash? obj 0))

;;; --------------------------------------------------------------------

(define-pathname-operation relative?
  ;;Return true  if the argument  OBJ, which must  be a valid  string or
  ;;bytevector representation of a Unix pathname, is relative; otherwise
  ;;return false.
  ;;
  ((bytevector)	($bytevector-relative? obj))
  ((string)	($string-relative?     obj)))

(define ($bytevector-relative? obj)
  (not ($bytevector-absolute? obj)))

(define ($string-relative? obj)
  (not ($string-absolute? obj)))


;;;; pathname components: extension

(define-pathname-operation extension
  ;;Return a  string or  bytevector representing  the extension  of OBJ,
  ;;which  must   be  a  valid   Unix  pathname  string   or  bytevector
  ;;representation.   The extension  of a  pathname is  the sequence  of
  ;;characters from  the end up  to the  first dot character  before the
  ;;first slash character; the returned value does *not* include the dot
  ;;character and can be empty.
  ;;
  ;;If the  dot is the first  character in the pathname's  last segment:
  ;;return the  empty bytevector because  we interpret this  pathname as
  ;;representing a Unix-style "hidden" filename or dirname.
  ;;
  ((bytevector)	($bytevector-extension obj))
  ((string)	($string-extension     obj)))

(define ($bytevector-extension obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($bytevector-index-past-last-segment obj))
  (if ($fxzero? past)
      '#vu8()
    (let backwards-search-dot/slash ((i ($fxsub1 past)))
      (cond ((or ($fxzero? i)
		 ($bytevector-chi-slash? obj i))
	     '#vu8())
	    (($bytevector-chi-dot? obj i)
	     (let ((pre ($fxsub1 i)))
	       (if (or ($fxnegative? pre)
		       ($bytevector-chi-slash? obj pre))
		   ;;The dot  is the  first character in  the pathname's
		   ;;last segment:  return the empty  bytevector because
		   ;;we  interpret  this   pathname  as  representing  a
		   ;;"hidden" filename.
		   '#vu8()
		   ($subbytevector-u8 obj ($fxadd1 i) past))))
	    (else
	     (backwards-search-dot/slash ($fxsub1 i)))))))

(define ($string-extension obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($string-index-past-last-segment obj))
  (if ($fxzero? past)
      ""
    (let backwards-search-dot/slash ((i ($fxsub1 past)))
      (cond ((or ($fxzero? i)
		 ($string-chi-slash? obj i))
	     "")
	    (($string-chi-dot? obj i)
	     (let ((pre ($fxsub1 i)))
	       (if (or ($fxnegative? pre)
		       ($string-chi-slash? obj pre))
		   ;;The  dot is  the first  character in  the pathname's
		   ;;last  segment: return  the empty  string because  we
		   ;;interpret this  pathname as representing  a "hidden"
		   ;;filename.
		   ""
		 ($substring obj ($fxadd1 i) past))))
	    (else
	     (backwards-search-dot/slash ($fxsub1 i)))))))


;;;; pathname components: directory part

(define-pathname-operation dirname
  ;;Return a string or bytevector representing the dirname of OBJ, which
  ;;must be a  valid Unix pathname string  or bytevector representation.
  ;;The dirname  of a pathname  is the  sequence of characters  from the
  ;;beginning up  to the last  slash character; the returned  value does
  ;;*not* include the slash character and  is never empty: when there is
  ;;no directory part in the pathname, the returned value represents the
  ;;current directory as single dot.
  ;;
  ((bytevector)	($bytevector-dirname obj))
  ((string)	($string-dirname     obj)))

(define ($bytevector-dirname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($bytevector-index-past-last-segment obj))
  (if ($fxzero? past)
      ROOT-DIRECTORY-BV
    (let backwards-search-slash ((i ($fxsub1 past)))
      (cond (($fxnegative? i)
	     ;;No slash found.
	     CURRENT-DIRECTORY-BV)
	    (($bytevector-chi-slash? obj i)
	     ;;A slash is present.
	     (let ((past ($bytevector-index-past-last-segment obj i)))
	       (cond (($fxzero? past)
		      ;;The pathname  has slashes  up to  the beginning;
		      ;;examples: "/file.ext", "///file.ext".
		      ROOT-DIRECTORY-BV)
		     (else
		      ($subbytevector-or-full obj 0 past)))))
	    (else
	     (backwards-search-slash ($fxsub1 i)))))))

(define ($string-dirname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($string-index-past-last-segment obj))
  (if ($fxzero? past)
      ROOT-DIRECTORY-STR
    (let backwards-search-slash ((i ($fxsub1 past)))
      (cond (($fxnegative? i)
	     ;;No slash found.
	     CURRENT-DIRECTORY-STR)
	    (($string-chi-slash? obj i)
	     ;;A slash is present.
	     (let ((past ($string-index-past-last-segment obj i)))
	       (cond (($fxzero? past)
		      ;;The pathname  has slashes  up to  the beginning;
		      ;;examples: "/file.ext", "///file.ext".
		      ROOT-DIRECTORY-STR)
		     (else
		      ($substring-or-full obj 0 past)))))
	    (else
	     (backwards-search-slash ($fxsub1 i)))))))


;;;; pathname components: tail part

(define-pathname-operation tailname
  ;;Return  a string  or bytevector  representing the  tailname of  OBJ,
  ;;which  must   be  a  valid   Unix  pathname  string   or  bytevector
  ;;representation.  The tailname of a pathname is its last segment; the
  ;;returned value  does *not* include  the leading slash  character, if
  ;;any, and it  can be empty; when  the whole OBJ is  the tailname: the
  ;;returned value is OBJ itself.
  ;;
  ((bytevector)	($bytevector-tailname obj))
  ((string)	($string-tailname     obj)))

(define ($bytevector-tailname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($bytevector-index-past-last-segment obj))
  (if ($fxzero? past)
      '#vu8()
    (let backwards-search-slash ((i ($fxsub1 past)))
      (if (or ($fxnegative? i)
	      ($bytevector-chi-slash? obj i))
	  ($subbytevector-or-full obj ($fxadd1 i) past)
	(backwards-search-slash ($fxsub1 i))))))

(define ($string-tailname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($string-index-past-last-segment obj))
  (if ($fxzero? past)
      ""
    (let backwards-search-slash ((i ($fxsub1 past)))
      (if (or ($fxnegative? i)
	      ($string-chi-slash? obj i))
	  ($substring-or-full obj ($fxadd1 i) past)
	(backwards-search-slash ($fxsub1 i))))))


;;;; pathname components: rootname

(define-pathname-operation rootname
  ;;Return  a string  or bytevector  representing the  rootname of  OBJ,
  ;;which  must   be  a  valid   Unix  pathname  string   or  bytevector
  ;;representation.   The rootname  of  a pathname  is  the sequence  of
  ;;characters from  the beginning up  to the last dot  character before
  ;;the extension,  in other  words: everything  but the  extension; the
  ;;returned value  does *not* include  the dot character and  cannot be
  ;;empty.
  ;;
  ;;If the  dot is the first  character in the pathname's  last segment:
  ;;return the  whole bytevector because  we interpret such  pathname as
  ;;representing a Unix-style "hidden" filename or dirname.
  ;;
  ((bytevector)	($bytevector-rootname obj))
  ((string)	($string-rootname     obj)))

(define ($bytevector-rootname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($bytevector-index-past-last-segment obj))
  (if ($fxzero? past)
      ROOT-DIRECTORY-BV
    (let backwards-search-dot/slash ((i ($fxsub1 past)))
      (cond ((or ($fxzero? i)
		 ($bytevector-chi-slash? obj i))
	     ;;Here we  are at the beginning  of the pathname or  at the
	     ;;beginning of the last segment;  it means that there is no
	     ;;extension to cut.
	     ($subbytevector-or-full obj 0 past))
	    (($bytevector-chi-dot? obj i)
	     (if (or ($bytevector-beginning-of-segment? obj i)
		     ($bytevector-end-of-double-dot? obj i past))
		 ;;Either  the  dot  is   the  first  character  in  the
		 ;;pathname's last  segment or  the last segment  is the
		 ;;double-dot.   For the  first  case  return the  whole
		 ;;pathname  because  we   interpret  this  pathname  as
		 ;;representing  a "hidden"  filename.   For the  second
		 ;;case return  the whole  pathname because there  is no
		 ;;extension to cut.
		 ($subbytevector-or-full obj 0 past)
	       ;;The last  segment has an  extension: cut it  and return
	       ;;the result.
	       ($subbytevector-u8 obj 0 i)))
	    (else
	     (backwards-search-dot/slash ($fxsub1 i)))))))

(define ($string-rootname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (define past
    ($string-index-past-last-segment obj))
  (if ($fxzero? past)
      ROOT-DIRECTORY-STR
    (let backwards-search-dot/slash ((i ($fxsub1 past)))
      (cond ((or ($fxzero? i)
		 ($string-chi-slash? obj i))
	     ;;Here we  are at  the beginning  of the  string or  at the
	     ;;beginning of the last segment;  it means that there is no
	     ;;extension to cut.
	     ($substring-or-full obj 0 past))
	    (($string-chi-dot? obj i)
	     (if (or ($string-beginning-of-segment? obj i)
		     ($string-end-of-double-dot? obj i past))
		 ;;Either  the  dot  is   the  first  character  in  the
		 ;;pathname's last  segment or  the last segment  is the
		 ;;double-dot.   For the  first  case  return the  whole
		 ;;pathname  because  we   interpret  this  pathname  as
		 ;;representing  a "hidden"  filename.   For the  second
		 ;;case return  the whole  pathname because there  is no
		 ;;extension to cut.
		 ($substring-or-full obj 0 past)
	       ;;The last  segment has an  extension: cut it  and return
	       ;;the result.
	       ($substring obj 0 i)))
	    (else
	     (backwards-search-dot/slash ($fxsub1 i)))))))


;;;; pathname components: stripping trailing slashes

(define-pathname-operation strip-trailing-slashes
  ;;Return a  string or  bytevector representing  the argument  with the
  ;;trailing  slashes  stripped,  if  any.  If  there  are  no  trailing
  ;;slashes: return OBJ itself.
  ;;
  ((bytevector)	($bytevector-strip-trailing-slashes obj))
  ((string)	($string-strip-trailing-slashes     obj)))

(define ($bytevector-strip-trailing-slashes obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let ((past ($bytevector-index-past-last-segment obj)))
    (if ($fxzero? past)
	ROOT-DIRECTORY-BV
      ($subbytevector-or-full obj 0 past))))

(define ($string-strip-trailing-slashes obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let ((past ($string-index-past-last-segment obj)))
    (if ($fxzero? past)
	ROOT-DIRECTORY-STR
      ($substring-or-full obj 0 past))))


;;;; pathname components: splitting

(define-pathname-operation split
  ;;Split into  segments the argument  OBJ, which  must be a  valid Unix
  ;;pathname string  or bytevector  representation.  Return 2  values: a
  ;;boolean, true if the pathname is  absolute; null or a proper list of
  ;;bytevectors representing  the segments.   The returned  segments are
  ;;normalised  by removing,  when possible,  segments representing  the
  ;;current directory and segments representing the uplevel directory.
  ;;
  ((bytevector)	($bytevector-split obj))
  ((string)	($string-split     obj)))

(define ($bytevector-split obj)
  (let ((port (open-bytevector-input-port obj)))
    (receive (absolute? original-segments)
	(parse-pathname port)
      (receive (changed? normalised-segments)
	  (normalise-segments absolute? original-segments)
	(values absolute? normalised-segments)))))

(define ($string-split obj)
  ($bytevector-split (string->utf8 obj)))


;;;; pathname components: normalisation

(define-pathname-operation normalise
  ;;Normalise as  much as  possible the  argument OBJ,  which must  be a
  ;;valid Unix pathname string or bytevector representation.
  ;;
  ((bytevector)	($bytevector-normalise obj))
  ((string)	($string-normalise     obj)))

(define ($bytevector-normalise obj)
  (receive (absolute? segments)
      ($bytevector-split obj)
    (serialise-segments absolute? segments)))

(define ($string-normalise obj)
  (receive (absolute? segments)
      ($string-split obj)
    (utf8->string (serialise-segments absolute? segments))))


;;;; pathname components: subpathname predicates

(define-pathname-operation-2 prefix?
  ;;Given  two  strings  or   two  bytevectors  representing  valid  and
  ;;normalised Unix  pathname representations: return true  if the first
  ;;is the prefix of the second, otherwise return false.
  ;;
  ((bytevector)	($bytevector-prefix? obj1 obj2))
  ((string)	($string-prefix?     obj1 obj2)))

(define ($bytevector-prefix? obj1 obj2)
  (and ($fx<= ($bytevector-length obj1)
	      ($bytevector-length obj2))
       (let compare-next-octet ((i 0))
	 (or ($fx= i ($bytevector-length obj1))
	     (and ($fx= ($bytevector-u8-ref obj1 i)
			($bytevector-u8-ref obj2 i))
		  (compare-next-octet ($fxadd1 i)))))))

(define ($string-prefix? obj1 obj2)
  (and ($fx<= ($string-length obj1)
	      ($string-length obj2))
       (let compare-next-octet ((i 0))
	 (or ($fx= i ($string-length obj1))
	     (and ($char= ($string-ref obj1 i)
			  ($string-ref obj2 i))
		  (compare-next-octet ($fxadd1 i)))))))

;;; --------------------------------------------------------------------

(define-pathname-operation-2 suffix?
  ;;Given  two  strings  or   two  bytevectors  representing  valid  and
  ;;normalised Unix  pathname representations: return true  if the first
  ;;is the suffix of the second, otherwise return false.
  ;;
  ((bytevector)	($bytevector-suffix? obj1 obj2))
  ((string)	($string-suffix?     obj1 obj2)))

(define ($bytevector-suffix? obj1 obj2)
  (and ($fx<= ($bytevector-length obj1)
	      ($bytevector-length obj2))
       (let compare-prev-octet ((i ($fxsub1 ($bytevector-length obj1)))
				(j ($fxsub1 ($bytevector-length obj2))))
	 (or ($fxnegative? i)
	     (and ($fx= ($bytevector-u8-ref obj1 i)
			($bytevector-u8-ref obj2 j))
		  (compare-prev-octet ($fxsub1 i) ($fxsub1 j)))))))

(define ($string-suffix? obj1 obj2)
  (and ($fx<= ($string-length obj1)
	      ($string-length obj2))
       (let compare-prev-octet ((i ($fxsub1 ($string-length obj1)))
				(j ($fxsub1 ($string-length obj2))))
	 (or ($fxnegative? i)
	     (and ($char= ($string-ref obj1 i)
			  ($string-ref obj2 j))
		  (compare-prev-octet ($fxsub1 i) ($fxsub1 j)))))))


;;;; pathname components: subpathname composition

(define-pathname-operation-2 prepend
  ;;Given  two  strings  or  two  bytevectors  representing  valid  Unix
  ;;pathnames: prepend the first to the second and return the result.
  ;;
  ((bytevector)	($bytevector-prepend obj1 obj2))
  ((string)	($string-prepend     obj1 obj2)))

(define* ($bytevector-prepend obj1 obj2)
  (let* ((obj1   ($bytevector-strip-trailing-slashes obj1))
	 (slash? ($bytevector-last-is-slash? obj1)))
    (receive-and-return (result)
	(let ((len (+ ($bytevector-length obj1)
		      ($bytevector-length obj2)
		      (if slash? 0 1))))
	  (if (fixnum? len)
	      ($make-bytevector len)
	    (error __who__ "request to join pathnames leads to bytevector too big" obj1 obj2)))
      (do ((i 0 ($fxadd1 i)))
	  (($fx= i ($bytevector-length obj1))
	   (unless slash?
	     ($bytevector-u8-set! result i CHI-SLASH)
	     ($fxincr! i))
	   (do ((j 0 ($fxadd1 j))
		(k i ($fxadd1 k)))
	       (($fx= j ($bytevector-length obj2)))
	     ($bytevector-u8-set! result k ($bytevector-u8-ref obj2 j))))
	($bytevector-u8-set! result i ($bytevector-u8-ref obj1 i))))))

(define* ($string-prepend obj1 obj2)
  (let* ((obj1   ($string-strip-trailing-slashes obj1))
	 (slash? ($string-last-is-slash? obj1)))
    (receive-and-return (result)
	(let ((len (+ ($string-length obj1)
		      ($string-length obj2)
		      (if slash? 0 1))))
	  (if (fixnum? len)
	      (make-string len) #;($make-string len)
	    (error __who__ "request to join pathnames leads to string too big" obj1 obj2)))
      (do ((i 0 ($fxadd1 i)))
	  (($fx= i ($string-length obj1))
	   (unless slash?
	     ($string-set! result i #\/)
	     ($fxincr! i))
	   (do ((j 0 ($fxadd1 j))
		(k i ($fxadd1 k)))
	       (($fx= j ($string-length obj2)))
	     ($string-set! result k ($string-ref obj2 j))))
	($string-set! result i ($string-ref obj1 i))))))

;;; --------------------------------------------------------------------

(define-pathname-operation-2 append
  ;;Given  two  strings  or   two  bytevectors  representing  valid  and
  ;;normalised Unix pathnames: append the first to the second and return
  ;;the result.
  ;;
  ((bytevector)	($bytevector-prepend obj2 obj1))
  ((string)	($string-prepend     obj2 obj1)))

(define ($bytevector-append obj1 obj2)
  ($bytevector-prepend obj2 obj1))

(define ($string-append obj1 obj2)
  ($bytevector-prepend obj2 obj1))


;;;; pathname components: replacing extension

(define* (replace-extension ptn ext)
  ;;Given  a string  representing a  valid  Unix pathname  and a  string
  ;;representing  a  valid  Unix   pathname  segment,  or  a  bytevector
  ;;representing a valid  Unix pathname and a  bytevector representing a
  ;;valid Unix pathname  segment: strip the extension  from the pathname
  ;;and append the segment to the result as new extension.
  ;;
  (cond ((bytevector-pathname? ptn)
	 (if (bytevector-segment? ext)
	     ($bytevector-replace-extension ptn ext)
	   (procedure-argument-violation __who__
	     "expected bytevector Unix pathname segment as argument" ext)))
	((string-pathname? ptn)
	 (if (string-segment? ext)
	     ($string-replace-extension ptn ext)
	   (procedure-argument-violation __who__
	     "expected string Unix pathname segment as argument" ext)))
	(else
	 (procedure-argument-violation __who__
	   "expected string or bytevector Unix pathname as argument" ptn))))

(define* ($bytevector-replace-extension ptn ext)
  (let ((ptn ($bytevector-strip-trailing-slashes ptn)))
    (if (or ($dot-bv?     ptn)
	    ($dot-dot-bv? ptn)
	    ($root-bv?    ptn))
	(raise-unix-pathname-normalisation-error __who__
	  "cannot append extension to special directory pathname" ptn)
      (let ((ptn ($bytevector-rootname ptn)))
	(receive-and-return (result)
	    (let ((len (+ ($bytevector-length ptn)
			  ($bytevector-length ext)
			  1)))
	      (if (fixnum? len)
		  ($make-bytevector len)
		(raise-unix-pathname-normalisation-error __who__
		  "request to append pathname extension leads to bytevector too big" ptn ext)))
	  (do ((i 0 ($fxadd1 i)))
	      (($fx= i ($bytevector-length ptn))
	       ($bytevector-u8-set! result i CHI-DOT)
	       ($fxincr! i)
	       (do ((j 0 ($fxadd1 j))
		    (k i ($fxadd1 k)))
		   (($fx= j ($bytevector-length ext)))
		 ($bytevector-u8-set! result k ($bytevector-u8-ref ext j))))
	    ($bytevector-u8-set! result i ($bytevector-u8-ref ptn i))))))))

(define* ($string-replace-extension ptn ext)
  (let ((ptn ($string-strip-trailing-slashes ptn)))
    (if (or ($dot-str?     ptn)
	    ($dot-dot-str? ptn)
	    ($root-str?    ptn))
	(raise-unix-pathname-normalisation-error __who__
	  "cannot append extension to special directory pathname" ptn)
      (let ((ptn ($string-rootname ptn)))
	(receive-and-return (result)
	    (let ((len (+ ($string-length ptn)
			  ($string-length ext)
			  1)))
	      (if (fixnum? len)
		  ($make-string len)
		(raise-unix-pathname-normalisation-error __who__
		  "request to append pathname extension leads to string too big" ptn ext)))
	  (do ((i 0 ($fxadd1 i)))
	      (($fx= i ($string-length ptn))
	       ($string-set! result i #\.)
	       ($fxincr! i)
	       (do ((j 0 ($fxadd1 j))
		    (k i ($fxadd1 k)))
		   (($fx= j ($string-length ext)))
		 ($string-set! result k ($string-ref ext j))))
	    ($string-set! result i ($string-ref ptn i))))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'raise-unix-pathname-parser-error 'scheme-indent-function 1)
;;eval: (put 'raise-unix-pathname-normalisation-error 'scheme-indent-function 1)
;;End:
