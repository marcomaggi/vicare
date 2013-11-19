;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: Unix file pathnames
;;;Date: Sat Apr  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa uri pathnames unix)
  (export
    <pathname>
    <relative-pathname>
    <absolute-pathname>
    pathname

    ;; auxiliary syntaxes
    string: bytevector: segments:)
  (import (nausicaa)
    (nausicaa pathnames abstract)
    (prefix (nausicaa pathnames unix low) low.))


;;; helpers

(define-constant $int-dot		46)
(define-constant $int-slash		47)
(define-constant $bv-single-dot		'#vu8(46))
(define-constant $bv-slash		'#vu8(47))
(define-constant $pair-single-dot	'(#vu8(46)))

(define (%error-normalisation who absolute? original-segments)
  (raise-normalisation-error who "error normalising segments" absolute? original-segments))

(define (%drop-last-pair ell)
  (if (null? (cdr ell))
      '()
    (cons (car ell) (%drop-last-pair (cdr ell)))))

(define (%replace-last-pair ell replacement)
  (if (null? (cdr ell))
      replacement
    (cons (car ell) (%replace-last-pair (cdr ell) replacement))))


(module (<absolute-unix-pathname>)

  (define-class <absolute-unix-pathname>
    (nongenerative nausicaa:uri:pathnames:unix:<absolute-unix-pathname>)
    (parent <absolute-pathname>)

    (protocol
     (lambda (make-absolute-pathname)
       (%make-absolute-unix-pathname obj)))

    #| end of class |# )

  (define (%make-absolute-unix-pathname obj)
    ;;Recursive function.
    ;;
    (define __who__ 'make-<absolute-unix-pathname>)
    (cond ((string? obj)
	   (%make-absolute-unix-pathname ($string->ascii obj)))
	  ((bytevector? obj)
	   (let ((port (open-bytevector-input-port obj)))
	     (receive (absolute? original-segments)
		 (low.parse-pathname port)
	       (unless original-segments
		 (raise-parser-error who "invalid input bytevector" port 0))
	       (unless absolute?
		 (assertion-violation who "expected absolute pathname" obj))
	       (receive (changed? normalised-segments)
		   (low.normalise-pathname absolute? original-segments)
		 (when (not normalised-segments)
		   (%error-normalisation who absolute? original-segments))
		 ((make-absolute-pathname (if changed?
					      (low.serialise-segments absolute? normalised-segments)
					    (bytevector-copy obj))
					  normalised-segments))))))

	  ((list-of-bytevectors? obj)
	   ((make-absolute-pathname (low.serialise-segments #t obj) obj)))

	  (else
	   (procedure-argument-violation __who__ "invalid argument type" obj))))

    #| end of module |# )


(module (<relative-unix-pathname>)

  (define-class <relative-unix-pathname>
    (nongenerative nausicaa:uri:pathnames:unix:<relative-unix-pathname>)
    (parent <relative-pathname>)

    (protocol
     (lambda (make-relative-pathname)
       (%make-relative-unix-pathname obj)))

    #| end of class |# )

  (define (%make-unix-relative-pathname obj make-relative)
    ;;Recursive function.
    ;;
    (define __who__ 'make-<relative-unix-pathname>)
    (cond ((string? obj)
	   (%make-relative-unix-pathname ($string->ascii obj)))

	  ((bytevector? obj)
	   (let ((port (open-bytevector-input-port obj)))
	     (receive (absolute? original-segments)
		 (low.parse-pathname port)
	       (unless original-segments
		 (raise-parser-error who "invalid input bytevector" port 0))
	       (when absolute?
		 (assertion-violation who "expected relative pathname" obj))
	       (receive (changed? normalised-segments)
		   (low.normalise-pathname absolute? original-segments)
		 (when (not normalised-segments)
		   (%error-normalisation who absolute? original-segments))
		 ((make-relative (if changed?
				     (low.serialise-segments absolute? normalised-segments)
				   (bytevector-copy obj))
				 normalised-segments))))))

	  ((list-of-bytevectors? obj)
	   ((make-absolute-pathname (low.serialise-segments #t obj) obj)))

	  (else
	   (procedure-argument-violation __who__ "invalid argument type" obj))))

  #| end of module |# )


(define (pathname obj)
  (let (((bv <bytevector>) (if (bytevector? obj)
			       obj
			     (string->ascii obj))))
    (cond ((and ($bytevector-non-empty? bv)
		($fx= $int-slash (bv[0])))
	   (<absolute-unix-pathname> (bv)))
	  (else
	   (<relative-unix-pathname> (bv))))))


(define-method (pathname-absolute (o <absolute-unix-pathname>) (absolute <absolute-unix-pathname>))
  o)

(define-method (pathname-absolute (o <relative-unix-pathname>) (absolute <absolute-unix-pathname>))
  (let ((original-segments (append (absolute segments) (o segments))))
    (receive (changed? normalised-segments)
	(low.normalise-pathname #t original-segments)
      (if normalised-segments
	  (<absolute-unix-pathname> (normalised-segments))
	(%error-normalisation __who__ #t original-segments)))))


(define-method (pathname-string (O <relative-unix-pathname>))
  ($ascii->string (O bytevector)))

(define-method (pathname-string (O <absolute-unix-pathname>))
  ($ascii->string (O bytevector)))


(define-method (pathname-prepend (suffix <relative-unix-pathname>) (prefix <relative-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(low.normalise-pathname #f original-segments)
      (if normalised-segments
	  (<relative-unix-pathname> (normalised-segments))
	(%error-normalisation __who__ #f original-segments)))))

(define-method (pathname-prepend (suffix <relative-unix-pathname>) (prefix <absolute-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(low.normalise-pathname #t original-segments)
      (if normalised-segments
	  (<absolute-unix-pathname> (normalised-segments))
	(%error-normalisation __who__ #t original-segments)))))


(define-method (pathname-append (prefix <relative-unix-pathname>) (suffix <relative-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(low.normalise-pathname #f original-segments)
      (if normalised-segments
	  (<relative-unix-pathname> (normalised-segments))
	(%error-normalisation __who__ #f original-segments)))))

(define-method (pathname-append (prefix <absolute-unix-pathname>) (suffix <relative-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(low.normalise-pathname #f original-segments)
      (if normalised-segments
	  (<absolute-unix-pathname> ((low.serialise-segments #t normalised-segments)))
	(%error-normalisation __who__ #f original-segments)))))


(define-method (pathname-tail (o <absolute-unix-pathname>))
  (<relative-unix-pathname> ((or (o last-pair) $pair-single-dot))))

(define-method (pathname-dirname (o <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((if (null? (o segments))
				 (o segments)
			       (%drop-last-pair (o segments))))))

(define-method (pathname-rootname (o <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((if (or (null? (o segments))
				     (not (o last-dot-index-in-last-segment)))
				 (o segments)
			       (let ((tail (car (o last-pair)))
				     (name (make-bytevector (o last-dot-index-in-last-segment))))
				 (bytevector-copy! tail 0 name 0 (o last-dot-index-in-last-segment))
				 (%replace-last-pair (o segments) (list name)))))))


(define (%replace-extension who (o <absolute-unix-pathname>) (extension <bytevector>))
  (let ((name (o name)))
    (if (bytevector=? name $bv-single-dot)
	o
      (let ((segments (%replace-last-pair (o segments) (list (bytevector-append name extension)))))
	(receive (changed? normalised-segments)
	    (low.normalise-pathname #t segments)
	  (if normalised-segments
	      (<absolute-unix-pathname> (normalised-segments))
	    ;;This should never happen.
	    (%error-normalisation who #t segments)))))))

(define-method (pathname-replace-extension (o <absolute-unix-pathname>) (extension <string>))
  (%replace-extension 'pathname-replace-extension o (low.to-bytevector extension)))

(define-method (pathname-replace-extension (o <absolute-unix-pathname>) (source <absolute-unix-pathname>))
  (let ((extension (source extension)))
    (if extension
	(%replace-extension __who__ o extension)
      (raise (condition (&pathname ())
			(&error ())
			(&who (__who__))
			(&message ("source pathname has no extension"))
			(&irritants ((list o source))))))))

(add-method pathname-replace-extension (<absolute-unix-pathname> <bytevector>) %replace-extension)


(module LOW
  (string/bytevector->pathname-bytevector
   pathname-bytevector->string
   parse-segment		parse-segment-nz
   parse-slash-and-segment

   parse-pathname		normalise-pathname
   serialise-segments)

;;;; constants

  (define-inline-constant CHI-SLASH	47) ;;(char->integer #\/)
  (define-inline-constant CHI-DOT	46) ;;(char->integer #\.)

;;;; helpers

  (define-inline ($valid-chi-for-pathname? chi)
    ($fx< 0 chi 256))

  (define-inline ($valid-chi-for-segment? chi)
    (or ($fx<  0 chi 47) ;47 = #\/
	($fx< 47 chi 256)))

  (define (parser-error who message port offset)
    (raise
     (condition (make-parser-condition port offset)
		(make-who-condition who)
		(make-message-condition message))))

  (define (byte-error who message sequence offset)
    (raise
     (condition (make-byte-condition sequence offset)
		(make-who-condition who)
		(make-message-condition message))))

  (define-syntax (define-parser-macros stx)
    (syntax-case stx ()
      ((?k ?port)
       (with-syntax
	   ((SET-POSITION-START!	(datum->syntax #'?k 'set-position-start!))
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

;;;; plain string <-> bytevector conversion

  (define (string/bytevector->pathname-bytevector who obj)
    ;;Convert   the   string  or   bytevector   OBJ   to  a   bytevector
    ;;representation of a pathname; when successful return a bytevector,
    ;;if an error  occurs raise an exception using WHO  as value for the
    ;;"&who" condition object.
    ;;
    ;;When OBJ  is a string: only  characters in the set  defined by the
    ;;ASCII encoding are  accepted, with the exception  of the character
    ;;whose  code point  is  zero;  in this  case  a  new bytevector  is
    ;;returned.
    ;;
    ;;When OBJ is  a bytevector: only octets  representing characters in
    ;;the  set defined  by the  ASCII  encoding are  accepted, with  the
    ;;exception of the octet zero; in this case OBJ itself is returned.
    ;;
    (cond ((bytevector? obj)
	   (do ((i 0 ($fxadd1 i)))
	       (($fx= i ($bytevector-length obj))
		obj)
	     (unless ($valid-chi-for-pathname? ($bytevector-u8-ref obj i))
	       (byte-error who
		 "octet from bytevector out of range for pathname"
		 obj ($bytevector-u8-ref obj i)))))
	  ((string? obj)
	   (do ((i  0 ($fxadd1 i))
		(bv (make-bytevector ($string-length obj))))
	       (($fx= i ($string-length obj))
		bv)
	     (let ((chi ($char->fixnum ($string-ref obj i))))
	       (if ($valid-chi-for-pathname? chi)
		   ($bytevector-u8-set! obj i chi)
		 (byte-error who
		   "character from string out of range for pathname"
		   obj ($string-ref obj i))))))
	  (else
	   (procedure-argument-violation who
	     "expected string or bytevector as argument" obj))))

  (define (pathname-bytevector->string who obj)
    ;;Convert  the bytevector  pathname representation  OBJ to  a string
    ;;pathname representation;  when successful  return a string,  if an
    ;;error occurs raise an exception using  WHO as value for the "&who"
    ;;condition object.
    ;;
    ;;Only  octets representing  characters in  the set  defined by  the
    ;;ASCII encoding are accepted, with the exception of the octet zero.
    ;;
    (if (bytevector? obj)
	(do ((i   0 ($fxadd1 i))
	     (str (make-string ($bytevector-length obj))))
	    (($fx= i ($bytevector-length obj))
	     str)
	  (let ((chi ($bytevector-u8-ref obj i)))
	    (if ($valid-chi-for-pathname? chi)
		($string-set! obj i ($fixnum->char chi))
	      (byte-error who
		"octet from bytevector out of range for pathname"
		obj ($bytevector-u8-ref obj i)))))
      (procedure-argument-violation who
	"expected pathname bytevector as argument" obj)))

;;;; segment component parsers

  (define (parse-segment in-port)
    ;;Accumulate bytes  from the binary  input port IN-PORT,  while they
    ;;are valid for a "segment" component; IN-PORT must support the port
    ;;position.  Notice that an empty  "segment" is valid: it represents
    ;;the segment  between 2  slash characters;  a pathanme  like "a//b"
    ;;should be parsed as:
    ;;
    ;;  (#ve(ascii "a") #vu8() #ve(ascii "b"))
    ;;
    ;;where  the   empty  bytevector  is  equivalent   to  the  "current
    ;;directory" segment: #ve(ascii ".").
    ;;
    ;;If EOF  or a  slash character  in ASCII coding  is read:  return a
    ;;possibly empty  bytevector holding  the bytes accumulated  so far;
    ;;the port  position is  left pointing  to the  byte after  the last
    ;;accumulated one.
    ;;
    ;;If  an invalid  byte is  read: an  exception is  raised with  type
    ;;"&parser";  the port  position is  rewind to  the one  before this
    ;;function call.
    ;;
    (define-parser-macros in-port)
    (define (%error)
      (let ((pos (port-position in-port)))
	(set-position-start!)
	(parser-error __who__ "invalid byte while parsing pathname" in-port pos)))
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

  (define (parse-segment-nz in-port)
    ;;Accumulate  bytes  from  IN-PORT  while   they  are  valid  for  a
    ;;"segment-nz" component;  notice that an empty  "segment-nz" is not
    ;;valid.
    ;;
    ;;If the  first read operation returns  EOF or a slash  character in
    ;;ASCII coding: the port position is restored to the one before this
    ;;function call and the return value is false.
    ;;
    ;;If, after at least one valid byte is read, EOF or a slash is read:
    ;;return a bytevector holding the bytes accumulated so far; the port
    ;;position is left  pointing to the byte after  the last accumulated
    ;;one.
    ;;
    ;;If  an invalid  byte is  read: an  exception is  raised with  type
    ;;"&parser";  the port  position is  rewind to  the one  before this
    ;;function call.
    ;;
    (define-parser-macros in-port)
    (define who 'parse-segment-nz)
    (define (%error)
      (let ((pos (port-position in-port)))
	(set-position-start!)
	(parser-error 'who "invalid byte while parsing pathname" in-port pos)))
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

  (define (parse-slash-and-segment in-port)
    ;;Attempt to  read from  IN-PORT the  sequence slash  character plus
    ;;"segment" component; notice that an empty "segment" is valid.
    ;;
    ;;If  these components  are successfully  read: return  a bytevector
    ;;(possibly empty) holding the accumulated "segment" bytes; the port
    ;;position is left  pointing to the byte after  the last accumulated
    ;;byte from the "segment".
    ;;
    ;;If EOF or a valid byte different from slash is read as first byte:
    ;;return false; the  port position is rewind to the  one before this
    ;;function call.
    ;;
    ;;If  an invalid  byte is  read: an  exception is  raised with  type
    ;;"&parser";  the port  position is  rewind to  the one  before this
    ;;function call.
    ;;
    (define-parser-macros in-port)
    (define (%error)
      (let ((pos (port-position in-port)))
	(set-position-start!)
	(parser-error 'who "invalid byte while parsing pathname" in-port pos)))
    (receive (ou-port getter)
	(open-bytevector-output-port)
      (let ((chi (get-u8 in-port)))
	(cond ((eof-object? chi)
	       (return-failure))

	      ((not ($valid-chi-for-pathname? chi))
	       (%error))

	      ((not ($fx= chi CHI-SLASH))
	       (return-failure))

	      ;;In case  of failure from  PARSE-SEGMENT: we do  not just
	      ;;return its  return value because  we have to  rewind the
	      ;;port position to the slash byte.
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

  (define (parse-pathname in-port)
    ;;Parse from IN-PORT  an absolute or relative pathname  until EOF is
    ;;found;  return two  values: a  boolean,  true if  the pathname  is
    ;;absolute;  false if  EOF is  the first  byte read  or a,  possibly
    ;;empty, list o bytevectors representing the segments.
    ;;
    ;;If  an invalid  byte is  read, an  exception is  raised with  type
    ;;"&parser";  the port  position is  rewind to  the one  before this
    ;;function call.
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
		 (values #f #f)))))))

  (define (normalise-pathname absolute? segments)
    ;;Given a list of bytevectors representing segments: normalise them,
    ;;as much as possible, removing segments representing single-dot and
    ;;double-dot  directory entries;  if ABSOLUTE?   is true:  normalise
    ;;SEGMENTS as an absolute pathname,  else normalise it as a relative
    ;;pathname.  Return two values:
    ;;
    ;;1. A  boolean, true if some  change was made from  SEGMENTS to the
    ;;second return value; this allows us to detect if a normalised list
    ;;of segments  when serialised  into a bytevector  becomes different
    ;;from the original bytevector.
    ;;
    ;;2. When the normalisation succeeds: a new, possibly empty, list of
    ;;bytevectors representing the  normalisation of SEGMENTS.  Absolute
    ;;pathname  segments  can  hold neither  single-dot  nor  double-dot
    ;;segments: if a double-dot segment cannot be removed, this value is
    ;;false (and the first too).
    ;;
    (define-inline (%dot-bv? segment)
      ;;Return true  if SEGMENT  is a bytevector  holding a  single byte
      ;;being the ASCII representation of the dot.
      ;;
      (and (=  1 (bytevector-length segment))
	   (= 46 (bytevector-u8-ref segment 0))))
    (define-inline (%dot-dot-bv? segment)
      ;;Return true  if SEGMENT is  a bytevector holding two  bytes both
      ;;being the ASCII representation of the dot.
      ;;
      (and (=  2 (bytevector-length segment))
	   (= 46 (bytevector-u8-ref segment 0))
	   (= 46 (bytevector-u8-ref segment 1))))
    (let next-segment ((input	segments)
		       (output	'())
		       (changed?	#f))
      (if (null? input)
	  (values changed? (reverse output))
	(let ((segment (car input)))
	  (cond ((%dot-bv? segment)
		 (next-segment (cdr input) output #t))
		((%dot-dot-bv? segment)
		 (if absolute?
		     (if (null? output)
			 (values #f #f)
		       (next-segment (cdr input) (cdr output) #t))
		   (cond ((null? output)
			  (next-segment (cdr input) (cons segment output) changed?))
			 ((%dot-dot-bv? (car output))
			  (next-segment (cdr input) (cons segment output) changed?))
			 (else
			  (next-segment (cdr input) (cdr output) #t)))))
		(else
		 (next-segment (cdr input) (cons segment output) changed?)))))))

  (define (serialise-segments absolute? segments)
    (if (null? segments)
	(if absolute? '#vu8(47) '#vu8(46))
      (receive (port getter)
	  (open-bytevector-output-port)
	(when absolute?
	  (put-u8 port 47))
	(put-bytevector port (car segments))
	(for-each (lambda (segment)
		    (put-u8 port 47)
		    (put-bytevector port segment))
	  (cdr segments))
	(getter))))

  #| end of module |# )


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'parser-error 'scheme-indent-function 1)
;;eval: (put 'byte-error 'scheme-indent-function 1)
;;End:
