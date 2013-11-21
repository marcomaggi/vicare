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
    pathname? bytevector-pathname? string-pathname?
    segment?
    list-of-segments?

    ;; conversion
    string/bytevector->pathname-bytevector
    pathname-bytevector->string

    ;; parser functions
    parse-segment
    parse-segment-nz
    parse-slash-and-segment
    parse-pathname

    ;; pathname manipulation
    normalise-pathname
    serialise-segments

    ;; components
    extension		$bytevector-extension	$string-extension
    dirname		$bytevector-dirname	$string-dirname
    tailname		$bytevector-tailname	$string-tailname
    rootname		$bytevector-rootname	$string-rootname

    ;; condition objects
    &unix-pathname-parser-error
    make-unix-pathname-parser-error
    unix-pathname-parser-error?
    &unix-pathname-normalisation-error
    make-unix-pathname-normalisation-error
    unix-pathname-normalisation-error?
    raise-unix-pathname-parser-error
    raise-unix-pathname-normalisation-error)
  (import (vicare)
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

(define-inline ($dot-bv? bv)
  ;;Assuming BV is  a bytevector: return true if it  holds a single byte
  ;;being the ASCII representation of the dot.
  ;;
  (and ($fx=  1 ($bytevector-length bv))
       ($fx= CHI-DOT ($bytevector-u8-ref bv 0))))

(define-inline ($dot-dot-bv? bv)
  ;;Assuming BV is a bytevector: return  true if it holds two bytes both
  ;;being the ASCII representation of the dot.
  ;;
  (and ($fx= 2       ($bytevector-length bv))
       ($fx= CHI-DOT ($bytevector-u8-ref bv 0))
       ($fx= CHI-DOT ($bytevector-u8-ref bv 1))))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-chi-slash? bv i)
  ($fx= CHI-SLASH ($bytevector-u8-ref bv i)))

(define-inline ($bytevector-chi-dot? bv i)
  ($fx= CHI-DOT   ($bytevector-u8-ref bv i)))

(define-inline ($string-chi-slash? str i)
  ($fx= CHI-SLASH ($char->fixnum ($string-ref str i))))

(define-inline ($string-chi-dot? str i)
  ($fx= CHI-DOT   ($char->fixnum ($string-ref str i))))

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


;;;; predicates

(define (pathname? obj)
  (or (bytevector-pathname? obj)
      (string-pathname?     obj)))

(define (bytevector-pathname? obj)
  (and (bytevector? obj)
       ($bytevector-not-empty? obj)
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
       ($string-not-empty? obj)
       (let loop ((obj obj)
		  (i   0))
	 (cond (($fx= i ($string-length obj))
		#t)
	       (($valid-chi-for-pathname? ($char->fixnum ($string-ref obj i)))
		(loop obj ($fxadd1 i)))
	       (else
		#f)))))

;;; --------------------------------------------------------------------

(define (segment? obj)
  (cond ((bytevector? obj)
	 (let loop ((bv obj)
		    (i  0))
	   (cond (($fx= i ($bytevector-length bv))
		  #t)
		 (($valid-chi-for-segment? ($bytevector-u8-ref bv i))
		  (loop bv ($fxadd1 i)))
		 (else
		  #f))))
	((string? obj)
	 (let loop ((obj obj)
		    (i   0))
	   (cond (($fx= i ($string-length obj))
		  #t)
		 (($valid-chi-for-segment? ($char->fixnum ($string-ref obj i)))
		  (loop obj ($fxadd1 i)))
		 (else
		  #f))))
	(else #f)))

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


(define* (normalise-pathname absolute? segments)
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
		    "expected string or bytevector as argument" OBJ))))))
    ))


;;;; pathname components: extension

(define-pathname-operation extension
  ;;Return a new string or bytevector representing the extension of OBJ,
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
  (let backwards-search-dot/slash ((i ($fxsub1 ($bytevector-length obj))))
    (cond ((or ($fxzero? i)
	       ($bytevector-chi-slash? obj i))
	   '#vu8())
	  (($bytevector-chi-dot? obj i)
	   (let ((pre ($fxsub1 i)))
	     (if (or ($fxnegative? pre)
		     ($bytevector-chi-slash? obj pre))
		 ;;The  dot is  the first  character in  the pathname's
		 ;;last segment: return the empty bytevector because we
		 ;;interpret this  pathname as representing  a "hidden"
		 ;;filename.
		 '#vu8()
		 ($subbytevector-u8 obj ($fxadd1 i) ($bytevector-length obj)))))
	  (else
	   (backwards-search-dot/slash ($fxsub1 i))))))

(define ($string-extension obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let backwards-search-dot/slash ((i ($fxsub1 ($string-length obj))))
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
	       ($substring obj ($fxadd1 i) ($string-length obj)))))
	  (else
	   (backwards-search-dot/slash ($fxsub1 i))))))


;;;; pathname components: directory part

(define-pathname-operation dirname
  ;;Return a new  string or bytevector representing the  dirname of OBJ,
  ;;which  must   be  a  valid   Unix  pathname  string   or  bytevector
  ;;representation.   The  dirname of  a  pathname  is the  sequence  of
  ;;characters from  the beginning up  to the last slash  character; the
  ;;returned value does  *not* include the slash character  and is never
  ;;empty: when there is no directory part in the pathname, the returned
  ;;value represents the current directory as single dot.
  ;;
  ((bytevector)	($bytevector-dirname obj))
  ((string)	($string-dirname     obj)))

(define ($bytevector-dirname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let backwards-search-slash ((i ($bytevector-u8-last-index obj)))
    (cond (($fxnegative? i)
	   ;;No slash found.
	   CURRENT-DIRECTORY-BV)
	  (($bytevector-chi-slash? obj i)
	   ;;A slash is present.
	   (let ((last-is-slash? ($fx= i ($bytevector-u8-last-index obj))))
	     (let drop-contiguous-slashes ((j ($fxsub1 i)))
	       (cond (($fxnegative? j)
		      ;;The pathname  has slashes  up to  the beginning;
		      ;;examples: "/file.ext", "///file.ext".
		      ROOT-DIRECTORY-BV)
		     (($bytevector-chi-slash? obj j)
		      (drop-contiguous-slashes ($fxsub1 j)))
		     (else
		      ;;There  is  at   least  one  non-slash  character
		      ;;between the beginning and the slash.
		      (if last-is-slash?
			  (backwards-search-slash ($fxsub1 j))
			(let ((end ($fxadd1 j)))
			  (if ($fxzero? end)
			      ($subbytevector-u8 obj 0 0)
			    ($subbytevector-u8 obj 0 end)))))))))
	  (else
	   (backwards-search-slash ($fxsub1 i))))))

(define ($string-dirname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let backwards-search-slash ((i ($string-last-index obj)))
    (cond (($fxnegative? i)
	   ;;No slash found.
	   CURRENT-DIRECTORY-STR)
	  (($string-chi-slash? obj i)
	   ;;A slash is present.
	   (let ((last-is-slash? ($fx= i ($string-last-index obj))))
	     (let drop-contiguous-slashes ((j ($fxsub1 i)))
	       (cond (($fxnegative? j)
		      ;;The pathname  has slashes  up to  the beginning;
		      ;;examples: "/file.ext", "///file.ext".
		      ROOT-DIRECTORY-STR)
		     (($string-chi-slash? obj j)
		      (drop-contiguous-slashes ($fxsub1 j)))
		     (else
		      ;;There  is  at   least  one  non-slash  character
		      ;;between the beginning and the slash.
		      (if last-is-slash?
			  (backwards-search-slash ($fxsub1 j))
			(let ((end ($fxadd1 j)))
			  (if ($fxzero? end)
			      ($substring obj 0 0)
			    ($substring obj 0 end)))))))))
	   (else
	    (backwards-search-slash ($fxsub1 i))))))


;;;; pathname components: tail part

(define-pathname-operation tailname
  ;;Return a new string or  bytevector representing the tailname of OBJ,
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
  (let backwards-search-slash ((i   ($bytevector-u8-last-index obj))
			       (end ($bytevector-length obj)))
    (cond (($fxnegative? i)
	   ;;No leading slash found.
	   (if ($fx= end ($bytevector-length obj))
	       obj
	     ;;Notice that this return value may be empty.
	     ($subbytevector-u8 obj 0 end)))
	  (($bytevector-chi-slash? obj i)
	   ;;A slash is present.
	   (let ((slash-index    ($bytevector-backwards-index-of-slash obj ($fxsub1 i)))
		 (last-is-slash? ($fx= i ($bytevector-u8-last-index obj))))
	     (if last-is-slash?
		 (backwards-search-slash ($fxsub1 slash-index) slash-index)
	       ($subbytevector-u8 obj ($fxadd1 i) end))))
	  (else
	   (backwards-search-slash ($fxsub1 i) end)))))

(define ($string-tailname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let backwards-search-slash ((i   ($string-last-index obj))
			       (end ($string-length obj)))
    (cond (($fxnegative? i)
	   ;;No leading slash found.
	   (if ($fx= end ($string-length obj))
	       obj
	     ;;Notice that this return value may be empty.
	     ($substring obj 0 end)))
	  (($string-chi-slash? obj i)
	   ;;A slash is present.
	   (let ((slash-index    ($string-backwards-index-of-slash obj ($fxsub1 i)))
		 (last-is-slash? ($fx= i ($string-last-index obj))))
	     (if last-is-slash?
		 (backwards-search-slash ($fxsub1 slash-index) slash-index)
	       ($substring obj ($fxadd1 i) end))))
	  (else
	   (backwards-search-slash ($fxsub1 i) end)))))


;;;; pathname components: rootname

(define-pathname-operation rootname
  ;;Return a new string or  bytevector representing the rootname of OBJ,
  ;;which  must   be  a  valid   Unix  pathname  string   or  bytevector
  ;;representation.   The rootname  of  a pathname  is  the sequence  of
  ;;characters from  the beginning up  to the last dot  character before
  ;;the extension,  in other  words: everything  but the  extension; the
  ;;returned  value does  *not* include  the  dot character  and can  be
  ;;empty.
  ;;
  ;;If the  dot is the first  character in the pathname's  last segment:
  ;;return the  whole bytevector because  we interpret this  pathname as
  ;;representing a Unix-style "hidden" filename or dirname.
  ;;
  ((bytevector)	($bytevector-rootname obj))
  ((string)	($string-rootname     obj)))

(define ($bytevector-rootname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let backwards-search-dot/slash ((i ($fxsub1 ($bytevector-length obj))))
    (cond ((or ($fxzero? i)
	       ($bytevector-chi-slash? obj i))
	   obj)
	  (($bytevector-chi-dot? obj i)
	   (let ((pre ($fxsub1 i)))
	     (if (or ($fxnegative? pre)
		     ($bytevector-chi-slash? obj pre))
		 ;;The dot is the first character in the pathname's last
		 ;;segment:  return  the  whole  bytevector  because  we
		 ;;interpret  this pathname  as representing  a "hidden"
		 ;;filename.
		 obj
	       ($subbytevector-u8 obj 0 i))))
	  (else
	   (backwards-search-dot/slash ($fxsub1 i))))))

(define ($string-rootname obj)
  ;;We know that if OBJ is a valid pathname: it cannot be empty.
  (let backwards-search-dot/slash ((i ($fxsub1 ($string-length obj))))
    (cond ((or ($fxzero? i)
	       ($string-chi-slash? obj i))
	   obj)
	  (($string-chi-dot? obj i)
	   (let ((pre ($fxsub1 i)))
	     (if (or ($fxnegative? pre)
		     ($string-chi-slash? obj pre))
		 ;;The dot is the first character in the pathname's last
		 ;;segment: return the whole string because we interpret
		 ;;this pathname as representing a "hidden" filename.
		 obj
	       ($substring obj 0 i))))
	  (else
	   (backwards-search-dot/slash ($fxsub1 i))))))



#|
(define ( mbfl_file_subpathname () {
    mbfl_mandatory_parameter(PATHNAME, 1, pathname)
    mbfl_mandatory_parameter(BASEDIR, 2, base directory)
    test "${BASEDIR:$((${#BASEDIR}-1))}" = '/' && \
        BASEDIR="${BASEDIR:0:$((${#BASEDIR}-1))}"
    if test "${PATHNAME}" = "${BASEDIR}"
    then
        printf './\n'
        return 0
    elif test "${PATHNAME:0:${#BASEDIR}}" = "${BASEDIR}"
    then
        printf  './%s\n' "${PATHNAME:$((${#BASEDIR}+1))}"
        return 0
    else return 1
    fi
}
(define ( mbfl_p_file_remove_dots_from_pathname () {
    mbfl_mandatory_parameter(PATHNAME, 1, pathname)
    local item i
    local SPLITPATH SPLITCOUNT; declare -a SPLITPATH
    local output output_counter=0; declare -a output
    local input_counter=0
    mbfl_file_split "${PATHNAME}"
    for ((input_counter=0; $input_counter < $SPLITCOUNT; ++input_counter))
    do
        case "${SPLITPATH[$input_counter]}" in
            .)
                ;;
            ..)
                let --output_counter
                ;;
            *)
                output[$output_counter]="${SPLITPATH[$input_counter]}"
                let ++output_counter
                ;;
        esac
    done
    PATHNAME="${output[0]}"
    for ((i=1; $i < $output_counter; ++i))
    do PATHNAME="${PATHNAME}/${output[$i]}"
    done
    printf '%s\n' "${PATHNAME}"
}
(define ( mbfl_file_rootname () {
    mbfl_mandatory_parameter(PATHNAME, 1, pathname)
    local i="${#PATHNAME}"
    test $i = 1 && {
        printf '%s\n' "${PATHNAME}"
        return 0
    }
    for ((i="${#PATHNAME}"; $i >= 0; --i))
    do
        ch="${PATHNAME:$i:1}"
        if test "$ch" = "."
        then
            if test $i -gt 0
            then
                printf '%s\n' "${PATHNAME:0:$i}"
                break
            else printf '%s\n' "${PATHNAME}"
            fi
        elif test "$ch" = "/"
        then
            printf '%s\n' "${PATHNAME}"
            break
        fi
    done
    return 0
}
(define ( mbfl_file_split () {
    mbfl_mandatory_parameter(PATHNAME, 1, pathname)
    local i=0 last_found=0
    mbfl_string_skip "${PATHNAME}" i /
    last_found=$i
    for ((SPLITCOUNT=0; $i < "${#PATHNAME}"; ++i))
    do
        test "${PATHNAME:$i:1}" = / && {
            SPLITPATH[$SPLITCOUNT]="${PATHNAME:$last_found:$(($i-$last_found))}"
            let ++SPLITCOUNT; let ++i
            mbfl_string_skip "${PATHNAME}" i /
            last_found=$i
        }
    done
    SPLITPATH[$SPLITCOUNT]="${PATHNAME:$last_found}"
    let ++SPLITCOUNT
    return 0
}

|#


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'raise-unix-pathname-parser-error 'scheme-indent-function 1)
;;eval: (put 'raise-unix-pathname-normalisation-error 'scheme-indent-function 1)
;;End: