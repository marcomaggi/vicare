;;;Copyright (c) 2009-2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2009 Derick Eddington
;;;
;;;Derived from the SRFI 13 reference implementation.
;;;
;;;Olin Shivers 7/2000
;;;
;;;Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;;Copyright (c) 1998, 1999, 2000 Olin Shivers.  All rights reserved.
;;;   The details of the copyrights appear at the end of the file. Short
;;;   summary: BSD-style open source.
;;;
;;;Copyright details
;;;=================
;;;
;;;The prefix/suffix and comparison routines in this code had (extremely
;;;distant) origins  in MIT Scheme's  string lib, and  was substantially
;;;reworked by  Olin Shivers (shivers@ai.mit.edu)  9/98. As such,  it is
;;;covered by MIT Scheme's open source copyright. See below for details.
;;;
;;;The KMP string-search code  was influenced by implementations written
;;;by Stephen  Bevan, Brian Dehneyer and Will  Fitzgerald. However, this
;;;version was written from scratch by myself.
;;;
;;;The remainder  of this  code was written  from scratch by  myself for
;;;scsh.  The scsh  copyright is a BSD-style open  source copyright. See
;;;below for details.
;;;
;;;-- Olin Shivers
;;;
;;;MIT Scheme copyright terms
;;;==========================
;;;
;;;This   material  was  developed   by  the   Scheme  project   at  the
;;;Massachusetts  Institute  of  Technology,  Department  of  Electrical
;;;Engineering and Computer Science.  Permission to copy and modify this
;;;software, to redistribute either  the original software or a modified
;;;version, and to use this software for any purpose is granted, subject
;;;to the following restrictions and understandings.
;;;
;;;1. Any copy made of  this software must include this copyright notice
;;;   in full.
;;;
;;;2. Users  of this software  agree to make  their best efforts  (a) to
;;;   return to  the MIT Scheme  project any improvements  or extensions
;;;   that they make, so that  these may be included in future releases;
;;;   and (b) to inform MIT of noteworthy uses of this software.
;;;
;;;3.  All materials  developed  as a  consequence  of the  use of  this
;;;   software shall  duly acknowledge such use, in  accordance with the
;;;   usual standards of acknowledging credit in academic research.
;;;
;;;4. MIT has made no  warrantee or representation that the operation of
;;;   this software will  be error-free, and MIT is  under no obligation
;;;   to  provide  any  services,  by  way of  maintenance,  update,  or
;;;   otherwise.
;;;
;;;5. In  conjunction  with  products  arising  from  the  use  of  this
;;;   material, there shall  be no use of the  name of the Massachusetts
;;;   Institute  of Technology  nor  of any  adaptation  thereof in  any
;;;   advertising,  promotional,  or   sales  literature  without  prior
;;;   written consent from MIT in each case.
;;;
;;;Scsh copyright terms
;;;====================
;;;
;;;All rights reserved.
;;;
;;;Redistribution and  use in source  and binary forms, with  or without
;;;modification,  are permitted provided  that the  following conditions
;;;are met:
;;;
;;;1.  Redistributions of source  code must  retain the  above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary  form must reproduce the above copyright
;;;   notice, this  list of conditions  and the following  disclaimer in
;;;   the  documentation  and/or   other  materials  provided  with  the
;;;   distribution.
;;;
;;;3. The  name of  the authors may  not be  used to endorse  or promote
;;;   products derived from this software without specific prior written
;;;   permission.
;;;
;;;THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;;IMPLIED  WARRANTIES,  INCLUDING,  BUT  NOT LIMITED  TO,  THE  IMPLIED
;;;WARRANTIES OF  MERCHANTABILITY AND  FITNESS FOR A  PARTICULAR PURPOSE
;;;ARE  DISCLAIMED.  IN NO  EVENT SHALL  THE AUTHORS  BE LIABLE  FOR ANY
;;;DIRECT,  INDIRECT, INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL
;;;DAMAGES  (INCLUDING, BUT  NOT LIMITED  TO, PROCUREMENT  OF SUBSTITUTE
;;;GOODS  OR  SERVICES; LOSS  OF  USE,  DATA,  OR PROFITS;  OR  BUSINESS
;;;INTERRUPTION) HOWEVER CAUSED AND  ON ANY THEORY OF LIABILITY, WHETHER
;;;IN  CONTRACT,  STRICT LIABILITY,  OR  TORT  (INCLUDING NEGLIGENCE  OR
;;;OTHERWISE) ARISING IN  ANY WAY OUT OF THE USE  OF THIS SOFTWARE, EVEN
;;;IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;Other copyright terms
;;;=====================
;;;
;;;Copyright (c) 2008 Derick Eddington.  Ported to R6RS.


#!r6rs
(library (srfi :13 strings)
  (export

    ;; constructors
    make-string				string-tabulate
    string				string-append
    string-concatenate			string-concatenate-reverse
    (rename (string-append		string-append/shared)
	    (string-concatenate		string-concatenate/shared)
	    (string-concatenate-reverse	string-concatenate-reverse/shared))

    ;; predicates
    string?			string-null?
    string-every		string-any

    ;; lexicographic comparison
    string-compare		string-compare-ci
    string=	string<>	string-ci=	string-ci<>
    string<	string<=	string-ci<	string-ci<=
    string>	string>=	string-ci>	string-ci>=

    ;; mapping
    string-map			string-map!
    string-for-each		string-for-each-index
    string-hash			string-hash-ci

    ;; case hacking
    string-titlecase		string-titlecase!
    string-upcase		string-upcase!
    string-downcase		string-downcase!

    ;; folding and unfolding
    string-fold			string-fold-right
    string-unfold		string-unfold-right

    ;; selecting
    substring/shared		string-ref
    string-copy			string-copy!
    string-take			string-take-right
    string-drop			string-drop-right

    ;; modification
    string-fill!		string-set!

    ;; padding and trimming
    string-trim			string-trim-right	string-trim-both
    string-pad			string-pad-right

    ;; prefix and suffix
    string-prefix-length	string-prefix-length-ci
    string-suffix-length	string-suffix-length-ci
    string-prefix?		string-prefix-ci?
    string-suffix?		string-suffix-ci?

    ;; searching
    string-index		string-index-right
    string-skip			string-skip-right
    string-contains		string-contains-ci
    string-count		string-length

    ;; filtering
    string-delete		string-filter

    ;; lists
    string->list		list->string		reverse-list->string
    string-tokenize		string-join

    ;; replicating
    xsubstring			string-xcopy!

    ;; reverse and replace
    string-reverse		string-reverse!		string-replace)
  (import (except (rnrs)
		  string->list
		  string-copy
		  string-upcase
		  string-downcase
		  string-titlecase
		  string-hash
		  string-for-each)
    (prefix (only (rnrs)
		  string-copy
		  string-hash)
	    rnrs.)
    (only (rnrs mutable-strings)
	  string-set!)
    (only (vicare)
	  module
	  pretty-print)
    (srfi :14 char-sets)
    (vicare arguments validation)
    (vicare syntactic-extensions)
    (prefix (except (vicare unsafe-operations)
		    string-copy!
		    string-fill!)
	    $)
    (only (ikarus system $numerics)
	  $min-fixnum-fixnum
	  $add-number-fixnum))


;;;; helpers

(define (%strings-list-min-length strings)
  (apply min (map string-length strings)))

(define-syntax cond-criterion
  (syntax-rules (char? char-set? procedure? else)
    ((_ ?criterion
	((char?)	?ch-body0 ?ch-body ...)
	((char-set?)	?cs-body0 ?cs-body ...)
	((procedure?)	?pr-body0 ?pr-body ...)
	(else		?el-body0 ?el-body ...))
     (cond ((char?      ?criterion)	?ch-body0 ?ch-body ...)
	   ((char-set?  ?criterion)	?cs-body0 ?cs-body ...)
	   ((procedure? ?criterion)	?pr-body0 ?pr-body ...)
	   (else			?el-body0 ?el-body ...)))))

(define (%error-wrong-criterion who criterion)
  (assertion-violation who
    "expected char, char-set or predicate as criterion argument" criterion))

;;; --------------------------------------------------------------------

(define-argument-validation (list-of-strings who obj)
  (and (list? obj)
       (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))

(define-argument-validation (list-of-chars who obj)
  (and (list? obj)
       (for-all char? obj))
  (assertion-violation who "expected list of characters as argument" obj))

(define-argument-validation (string-length who obj)
  (fixnum? obj)
  (assertion-violation who
    "list of characters too long, at most greatest-fixnum characters are accepted" obj))

(define-argument-validation (char-set who obj)
  (char-set? obj)
  (assertion-violation who "expected char-set as argument" obj))

;;; --------------------------------------------------------------------

(define-auxiliary-syntaxes
  arguments
  optional-arguments
  pre-arguments
  validators)

(define-syntax define-string2-func
  (syntax-rules (arguments validators)
    ((define-string2-func ?who
       (?proc ?embedded-argument ...))
     (define-string2-func ?who
       (?proc ?embedded-argument ...)
       (arguments)
       (validators)))

    ((define-string2-func ?who
       (?proc ?embedded-argument ...)
       (arguments ?arg ...)
       (validators ?validation-clause ...))
     (module (?who)
       (define who '?who)

       (define ?who
	 (case-lambda
	  ((str1 str2 ?arg ...)
	   (with-arguments-validation (who)
	       ((string			str1)
		(string			str2)
		?validation-clause ...)
	     (?proc ?embedded-argument ... str1 str2 ?arg ...
		    0 ($string-length str1) 0 ($string-length str2))))

	  ((str1 str2 ?arg ... start1)
	   (with-arguments-validation (who)
	       ((string				str1)
		(string				str2)
		?validation-clause ...
		(one-off-index-for-string	str1 start1))
	     (?proc ?embedded-argument ... str1 str2 ?arg ...
		    start1 ($string-length str1) 0 ($string-length str2))))

	  ((str1 str2 ?arg ... start1 past1)
	   (with-arguments-validation (who)
	       ((string				str1)
		(string				str2)
		?validation-clause ...
		(start-and-past-for-string	str1 start1 past1))
	     (?proc ?embedded-argument ... str1 str2 ?arg ...
		    start1 past1 0 ($string-length str2))))

	  ((str1 str2 ?arg ... start1 past1 start2)
	   (with-arguments-validation (who)
	       ((string				str1)
		(string				str2)
		?validation-clause ...
		(start-and-past-for-string	str1 start1 past1)
		(one-off-index-for-string	str2 start2))
	     (?proc ?embedded-argument ... str1 str2 ?arg ...
		    start1 past1 start2 ($string-length str2))))

	  ((str1 str2 ?arg ... start1 past1 start2 past2)
	   (with-arguments-validation (who)
	       ((string				str1)
		(string				str2)
		?validation-clause ...
		(start-and-past-for-string	str1 start1 past1)
		(start-and-past-for-string	str2 start2 past2))
	     (?proc ?embedded-argument ... str1 str2 ?arg ...
		    start1 past1 start2 past2)))))

       #| end of module: ?who |# ))))

(define-syntax define-string-func
  (syntax-rules (arguments optional-arguments pre-arguments validators)
    ((define-string-func ?who
       (?proc ?embedded-argument ...))
     (define-string-func ?who
       (?proc ?embedded-argument ...)
       (arguments)
       (validators)))

    ((define-string-func ?who
       (?proc ?embedded-argument ...)
       (arguments ?arg ...)
       (validators ?validation-clause ...))
     (module (?who)
       (define who '?who)

       (define ?who
	 (case-lambda
	  ((str ?arg ...)
	   (with-arguments-validation (who)
	       ((string		str)
		?validation-clause ...)
	     (?proc ?embedded-argument ... str ?arg ... 0 ($string-length str))))

	  ((str ?arg ... start)
	   (with-arguments-validation (who)
	       ((string				str)
		?validation-clause ...
		(one-off-index-for-string	str start))
	     (?proc ?embedded-argument ... str ?arg ... start ($string-length str))))

	  ((str ?arg ... start past)
	   (with-arguments-validation (who)
	       ((string				str)
		?validation-clause ...
		(start-and-past-for-string	str start past))
	     (?proc ?embedded-argument ... str ?arg ... start past)))
	  ))

       #| end of module: ?who |# ))

    ((define-string-func ?who
       (?proc ?embedded-argument ...)
       (optional-arguments (?arg ?arg-default) ...)
       (validators ?validation-clause ...))
     (module (?who)
       (define who '?who)

       (define ?who
	 (case-lambda
	  ((str)
	   (with-arguments-validation (who)
	       ((string		str)
		?validation-clause ...)
	     (?proc ?embedded-argument ... str ?arg-default ... 0 ($string-length str))))

	  ((str ?arg ...)
	   (with-arguments-validation (who)
	       ((string		str)
		?validation-clause ...)
	     (?proc ?embedded-argument ... str ?arg ... 0 ($string-length str))))

	  ((str ?arg ... start)
	   (with-arguments-validation (who)
	       ((string				str)
		?validation-clause ...
		(one-off-index-for-string	str start))
	     (?proc ?embedded-argument ... str ?arg ... start ($string-length str))))

	  ((str ?arg ... start past)
	   (with-arguments-validation (who)
	       ((string				str)
		?validation-clause ...
		(start-and-past-for-string	str start past))
	     (?proc ?embedded-argument ... str ?arg ... start past)))
	  ))

       #| end of module: ?who |# ))

    ((define-string-func ?who
       (?proc ?embedded-argument ...)
       (pre-arguments ?arg ...)
       (validators ?validation-clause ...))
     (module (?who)
       (define who '?who)

       (define ?who
	 (case-lambda
	  ((?arg ... str)
	   (with-arguments-validation (who)
	       ((string		str)
		?validation-clause ...)
	     (?proc ?embedded-argument ... ?arg ... str 0 ($string-length str))))

	  ((?arg ... str start)
	   (with-arguments-validation (who)
	       ((string				str)
		?validation-clause ...
		(one-off-index-for-string	str start))
	     (?proc ?embedded-argument ... ?arg ... str start ($string-length str))))

	  ((?arg ... str start past)
	   (with-arguments-validation (who)
	       ((string				str)
		?validation-clause ...
		(start-and-past-for-string	str start past))
	     (?proc ?embedded-argument ... ?arg ... str start past)))
	  ))

       #| end of module: ?who |# ))
    ))


;;;; predicates

(define (string-null? str)
  (define who 'string-null?)
  (with-arguments-validation (who)
      ((string	str))
    ($fxzero? ($string-length str))))

(module (string-every)
  (define who 'string-every)

  (define-string-func string-every
    ($string-every)
    (pre-arguments criterion)
    (validators))

  (define ($string-every criterion str start past)
    (cond-criterion criterion
      ((char?)		($string-every/char criterion str start past))
      ((char-set?)	($string-every/cset criterion str start past))
      ((procedure?)	($string-every/pred criterion str start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-every/char ch str start past)
    (or ($fx<= past start)
	(and ($char= ch ($string-ref str start))
	     ($string-every/char ch str ($fxadd1 start) past))))

  (define ($string-every/cset cset str start past)
    (or ($fx<= past start)
	(and (char-set-contains? cset ($string-ref str start))
	     ($string-every/cset cset str ($fxadd1 start) past))))

  (define ($string-every/pred pred str start past)
    (let ((ch     ($string-ref str start))
	  (start1 ($fxadd1 start)))
      (if ($fx= start1 past)
	  ;;This has to be a tail call.
	  (pred ch)
	(and (pred ch)
	     ($string-every/pred pred str start1 past)))))

  #| end of module: string-every |# )

(module (string-any)
  (define who 'string-any)

  (define-string-func string-any
    ($string-any)
    (pre-arguments criterion)
    (validators))

  (define ($string-any criterion str start past)
    (cond-criterion criterion
      ((char?)		($string-any/char criterion str start past))
      ((char-set?)	($string-any/cset criterion str start past))
      ((procedure?)	($string-any/pred criterion str start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-any/char ch str start past)
    (and ($fx< start past)
	 (or ($char= ch ($string-ref str start))
	     ($string-any/char ch str ($fxadd1 start) past))))

  (define ($string-any/cset cset str start past)
    (and ($fx< start past)
	 (or (char-set-contains? cset ($string-ref str start))
	     ($string-any/cset cset str ($fxadd1 start) past))))

  (define ($string-any/pred pred str start past)
    (let ((ch     ($string-ref str start))
	  (start1 ($fxadd1 start)))
      (if ($fx= start1 past)
	  ;;This has to be a tail call.
	  (pred ch)
	(or (pred ch)
	    ($string-any/pred pred str start1 past)))))

  #| end of module: string-any |# )


;;;; constructors

(define (string-tabulate proc len)
  (define who 'string-tabulate)
  (with-arguments-validation (who)
      ((procedure		proc)
       (non-negative-fixnum	len))
    (let ((str (make-string len)))
      (do ((i ($fxsub1 len) ($fxsub1 i)))
	  (($fxnegative? i)
	   str)
	($string-set! str i (proc i))))))


;;;; strings and lists

(module (string->list)
  (define who 'string->list)

  (define-string-func string->list
    ($string->list))

  (define ($string->list str start past)
    (do ((i ($fxsub1 past) ($fxsub1 i))
	 (result '() (cons ($string-ref str i) result)))
	(($fx< i start)
	 result)))

  #| end of module: string->list |# )

(define (reverse-list->string clist)
  (define who 'reverse-list->string)
  (with-arguments-validation (who)
      ((list-of-chars	clist))
    (let ((len (length clist)))
      (with-arguments-validation (who)
	  ((string-length	len))
	(let ((str (make-string len)))
	  (do ((i ($fxsub1 len) ($fxsub1 i))
	       (clist clist ($cdr clist)))
	      ((not (pair? clist))
	       str)
	    ($string-set! str i ($car clist))))))))

(module (string-join)
  (define who 'string-join)
  (define default-delimiter " ")
  (define default-grammar   'infix)

  (define string-join
    (case-lambda
     ((strings)
      (with-arguments-validation (who)
	  ((list-of-strings	strings))
	(%string-join strings default-delimiter default-grammar)))

     ((strings delim)
      (with-arguments-validation (who)
	  ((list-of-strings	strings)
	   (delimiter		delim))
	(%string-join strings delim default-grammar)))

     ((strings delim grammar)
      (with-arguments-validation (who)
	  ((list-of-strings	strings)
	   (delimiter		delim)
	   (grammar		grammar))
	(%string-join strings delim grammar)))))

  (define (%string-join strings delim grammar)
    (cond ((pair? strings)
	   (string-concatenate
	    (case grammar
	      ((infix strict-infix)
	       (cons (car strings)
		     (%join-with-delim delim (cdr strings) '())))
	      ((prefix)
	       (%join-with-delim delim strings '()))
	      ((suffix)
	       (cons (car strings)
		     (%join-with-delim delim (cdr strings) (list delim))))
	      (else
	       (assertion-violation 'string-join
		 "illegal join grammar" grammar)))))

	  ((not (null? strings))
	   (assertion-violation 'string-join
	     "STRINGS parameter is not a list" strings))

	  ;; here we know that STRINGS is the empty list
	  ((eq? grammar 'strict-infix)
	   (assertion-violation who
	     "empty list cannot be joined with STRICT-INFIX grammar."))

	  ;;Special-cased for infix grammar.
	  (else "")))

  (define (%join-with-delim delim ell final)
    (let loop ((ell ell))
      (if (pair? ell)
	  (cons delim
		(cons (car ell)
		      (loop (cdr ell))))
	final)))

  (define-argument-validation (grammar who obj)
    (and (symbol? obj)
	 (memq obj '(infix strict-infix suffix prefix)))
    (assertion-violation who "invalid grammar argument" obj))

  (define-argument-validation (delimiter who obj)
    (string? obj)
    (assertion-violation who "expected string as delimiter argument" obj))

  #| end of module: string-join |# )


;;;; selecting

(define-string-func substring/shared
  ($substring))

(define-string-func string-copy
  ($substring))

(module (string-copy! $string-copy!)
  (define who 'string-copy!)

  (define string-copy!
    (case-lambda
     ((dst.str dst.start src.str)
      (with-arguments-validation (who)
	  ((string			dst.str)
	   (string			src.str)
	   (one-off-index-for-string	dst.str dst.start))
	($string-copy! dst.str dst.start src.str 0 ($string-length src.str))))
     ((dst.str dst.start src.str src.start)
      (with-arguments-validation (who)
	  ((string			dst.str)
	   (string			src.str)
	   (one-off-index-for-string	dst.str dst.start)
	   (one-off-index-for-string	src.str src.start))
	($string-copy! dst.str dst.start src.str src.start ($string-length src.str))))
     ((dst.str dst.start src.str src.start src.past)
      (with-arguments-validation (who)
	  ((string			dst.str)
	   (string			src.str)
	   (one-off-index-for-string	dst.str dst.start)
	   (start-and-past-for-string	src.str src.start src.past))
	($string-copy! dst.str dst.start src.str src.start src.past)))))

  (define ($string-copy! dst.str dst.start src.str src.start src.past)
    (with-arguments-validation (who)
	((indices-for-copy	dst.str dst.start src.start src.past))
      (if ($fx> src.start dst.start)
	  (do ((i src.start ($fxadd1 i))
	       (j dst.start ($fxadd1 j)))
	      (($fx>= i src.past))
	    ($string-set! dst.str j ($string-ref src.str i)))
	(let* ((src.count ($fx- src.past src.start))
	       (dst.past  ($fx+ dst.start src.count)))
	  (do ((i ($fxsub1 src.past) ($fxsub1 i))
	       (j ($fxsub1 dst.past) ($fxsub1 j)))
	      (($fx< i src.start))
	    ($string-set! dst.str j ($string-ref src.str i)))))))

  (define-argument-validation (indices-for-copy who dst.str dst.start src.start src.past)
    ($fx>= ($fx- ($string-length dst.str) dst.start)
	   ($fx- src.past src.start))
    (assertion-violation who "not enough room in destination string"))

  #| end of module: string-copy! |# )

;;; --------------------------------------------------------------------

(define (string-take str nchars)
  (define who 'string-take)
  (with-arguments-validation (who)
      ((string				str)
       (one-off-index-for-string	str nchars))
    ($substring str 0 nchars)))

(define (string-take-right str nchars)
  (define who 'string-take-right)
  (with-arguments-validation (who)
      ((string				str)
       (one-off-index-for-string	str nchars))
    (let* ((past  ($string-length str))
	   (start ($fx- past nchars)))
      ($substring str start past))))

(define (string-drop str nchars)
  (define who 'string-drop)
  (with-arguments-validation (who)
      ((string				str)
       (one-off-index-for-string	str nchars))
    ($substring str nchars ($string-length str))))

(define (string-drop-right str nchars)
  (define who 'string-drop-right)
  (with-arguments-validation (who)
      ((string				str)
       (one-off-index-for-string	str nchars))
    ($substring str 0 ($fx- ($string-length str) nchars))))

;;; --------------------------------------------------------------------

(module (string-pad)
  (define who 'string-pad)

  (define string-pad
    (case-lambda
     ((str len)
      (with-arguments-validation (who)
	  ((string		str)
	   (non-negative-fixnum	len))
	($string-pad str len #\space 0 ($string-length str))))
     ((str len ch)
      (with-arguments-validation (who)
	  ((string		str)
	   (non-negative-fixnum	len)
	   (char		ch))
	($string-pad str len ch 0 ($string-length str))))
     ((str len ch start)
      (with-arguments-validation (who)
	  ((string			str)
	   (non-negative-fixnum		len)
	   (char			ch)
	   (one-off-index-for-string	str start))
	($string-pad str len ch start ($string-length str))))
     ((str len ch start past)
      (with-arguments-validation (who)
	  ((string			str)
	   (non-negative-fixnum		len)
	   (char			ch)
	   (start-and-past-for-string	str start past))
	($string-pad str len ch start past)))))

  (define ($string-pad src.str requested-len pad-char src.start src.past)
    (let ((substr.len ($fx- src.past src.start)))
      (if ($fx<= requested-len substr.len)
	  ($substring src.str ($fx- src.past requested-len) src.past)
	(let ((dst.str (make-string requested-len pad-char)))
	  ($string-copy! dst.str ($fx- requested-len substr.len)
			 src.str src.start src.past)
	  dst.str))))

  #| end of module: string-pad |# )

(module (string-pad-right)
  (define who 'string-pad-right)

  (define string-pad-right
    (case-lambda
     ((str len)
      (with-arguments-validation (who)
	  ((string		str)
	   (non-negative-fixnum	len))
	($string-pad-right str len #\space 0 ($string-length str))))
     ((str len ch)
      (with-arguments-validation (who)
	  ((string		str)
	   (non-negative-fixnum	len)
	   (char		ch))
	($string-pad-right str len ch 0 ($string-length str))))
     ((str len ch start)
      (with-arguments-validation (who)
	  ((string			str)
	   (non-negative-fixnum		len)
	   (char			ch)
	   (one-off-index-for-string	str start))
	($string-pad-right str len ch start ($string-length str))))
     ((str len ch start past)
      (with-arguments-validation (who)
	  ((string			str)
	   (non-negative-fixnum		len)
	   (char			ch)
	   (start-and-past-for-string	str start past))
	($string-pad-right str len ch start past)))))

  (define ($string-pad-right src.str requested-len pad-char src.start src.past)
    (let ((substr.len ($fx- src.past src.start)))
      (if ($fx<= requested-len substr.len)
	  ($substring src.str src.start ($fx- requested-len src.start))
	(let ((dst.str (make-string requested-len pad-char)))
	  ($string-copy! dst.str 0
			 src.str src.start src.past)
	  dst.str))))

  #| end of module: string-pad-right |# )

;;; --------------------------------------------------------------------

(module (string-trim
	 $string-trim/char
	 $string-trim/cset
	 $string-trim/pred)
  (define who 'string-trim)

  (define-string-func string-trim
    ($string-trim)
    (optional-arguments (criterion char-set:whitespace))
    (validators))

  (define ($string-trim str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-trim/char str criterion start past))
      ((char-set?)	($string-trim/cset str criterion start past))
      ((procedure?)	($string-trim/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-trim/char str char start past)
    (cond (($string-skip/char str char start past)
	   => (lambda (idx)
		($substring str idx past)))
	  (else "")))

  (define ($string-trim/cset str cset start past)
    (cond (($string-skip/cset str cset start past)
	   => (lambda (idx)
		($substring str idx past)))
	  (else "")))

  (define ($string-trim/pred str pred start past)
    (cond (($string-skip/pred str pred start past)
	   => (lambda (idx)
		($substring str idx past)))
	  (else "")))

  #| end of module: string-trim |# )

(module (string-trim-right
	 $string-trim-right/char
	 $string-trim-right/cset
	 $string-trim-right/pred)
  (define who 'string-trim-right)

  (define-string-func string-trim-right
    ($string-trim-right)
    (optional-arguments (criterion char-set:whitespace))
    (validators))

  (define ($string-trim-right str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-trim-right/char str criterion start past))
      ((char-set?)	($string-trim-right/cset str criterion start past))
      ((procedure?)	($string-trim-right/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-trim-right/char str char start past)
    (cond (($string-skip-right/char str char start past)
	   => (lambda (idx)
		($substring str start ($fxadd1 idx))))
	  (else "")))

  (define ($string-trim-right/cset str cset start past)
    (cond (($string-skip-right/cset str cset start past)
	   => (lambda (idx)
		($substring str start ($fxadd1 idx))))
	  (else "")))

  (define ($string-trim-right/pred str pred start past)
    (cond (($string-skip-right/pred str pred start past)
	   => (lambda (idx)
		($substring str start ($fxadd1 idx))))
	  (else "")))

  #| end of module: string-trim-right |# )

(module (string-trim-both
	 $string-trim-both/char
	 $string-trim-both/cset
	 $string-trim-both/pred)
  (define who 'string-trim-both)

  (define-string-func string-trim-both
    ($string-trim-both)
    (optional-arguments (criterion char-set:whitespace))
    (validators))

  (define ($string-trim-both str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-trim-both/char str criterion start past))
      ((char-set?)	($string-trim-both/cset str criterion start past))
      ((procedure?)	($string-trim-both/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-trim-both/char str char start past)
    (let ((str ($string-trim/char str char start past)))
      ($string-trim-right/char str char start ($string-length str))))

  (define ($string-trim-both/cset str cset start past)
    (let ((str ($string-trim/cset str cset start past)))
      ($string-trim-right/cset str cset start ($string-length str))))

  (define ($string-trim-both/pred str pred start past)
    (let ((str ($string-trim/pred str pred start past)))
      ($string-trim-right/pred str pred start ($string-length str))))

  #| end of module: string-trim-both |# )


;;;; modification

(module (string-fill! $string-fill!)
  (define who 'string-fill!)

  (define-string-func string-fill!
    ($string-fill!)
    (pre-arguments fill-char)
    (validators (char	fill-char)))

  (define ($string-fill! fill-char str start past)
    (do ((i ($fxsub1 past) ($fxsub1 i)))
	(($fx< i start))
      ($string-set! str i fill-char)))

  #| end of module: string-fill! |# )


;;;; lexicographic comparison

(define-string2-func string-compare
  (%string-compare)
  (arguments proc< proc= proc>)
  (validators (procedure proc<)
	      (procedure proc=)
	      (procedure proc>)))

(define-string2-func string-compare-ci
  (%string-compare-ci)
  (arguments proc< proc= proc>)
  (validators (procedure proc<)
	      (procedure proc=)
	      (procedure proc>)))

(define (%string-compare str1 str2 proc< proc= proc> start1 past1 start2 past2)
  (%true-string-compare $string-prefix-length char<?
			str1 str2 proc< proc= proc> start1 past1 start2 past2))

(define (%string-compare-ci str1 str2 proc< proc= proc> start1 past1 start2 past2)
  (%true-string-compare $string-prefix-length-ci char-ci<?
			str1 str2 proc< proc= proc> start1 past1 start2 past2))

(define (%true-string-compare string-prefix-length-proc char-less-proc
			      str1 str2 proc< proc= proc> start1 past1 start2 past2)
  (let ((size1 ($fx- past1 start1))
	(size2 ($fx- past2 start2)))
    (let ((match (string-prefix-length-proc str1 str2 start1 past1 start2 past2)))
      (if ($fx= match size1)
	  ((if ($fx= match size2) proc= proc<) past1)
	((if ($fx= match size2)
	     proc>
	   (if (char-less-proc ($string-ref str1 ($fx+ start1 match))
			       ($string-ref str2 ($fx+ start2 match)))
	       proc< proc>))
	 ($fx+ match start1))))))

;;; --------------------------------------------------------------------

(define-string2-func string=
  (%true-string= %string-compare))

(define-string2-func string-ci=
  (%true-string= %string-compare-ci))

(define (%true-string= string-compare-proc str1 str2 start1 past1 start2 past2)
  (and ($fx= ($fx- past1 start1) ($fx- past2 start2))
       (or (and (eq? str1 str2) ($fx= start1 start2))
	   (string-compare-proc str1 str2
				(lambda (i) #f) values (lambda (i) #f)
				start1 past1 start2 past2))))

;;; --------------------------------------------------------------------

(define-string2-func string<>
  (%true-string<> %string-compare))

(define-string2-func string-ci<>
  (%true-string<> %string-compare-ci))

(define (%true-string<> string-compare-proc str1 str2 start1 past1 start2 past2)
  (or (not ($fx= ($fx- past1 start1) ($fx- past2 start2)))
      (and (not (and (eq? str1 str2) ($fx= start1 start2)))
	   (string-compare-proc str1 str2
				values (lambda (i) #f) values
				start1 past1 start2 past2))))

;;; --------------------------------------------------------------------

(define-string2-func string<
  (%true-string< $string-prefix-length char<?))

(define-string2-func string-ci<
  (%true-string< $string-prefix-length-ci char-ci<?))

(define (%true-string< string-prefix-proc char-pred str1 str2 start1 past1 start2 past2)
  (if (and (eq? str1 str2) ($fx= start1 start2))
      ($fx< past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred
			  str1 str2
			  values (lambda (i) #f) (lambda (i) #f)
			  start1 past1 start2 past2)))

;;; --------------------------------------------------------------------

(define-string2-func string<=
  (%true-string<= $string-prefix-length char<=?))

(define-string2-func string-ci<=
  (%true-string<= $string-prefix-length-ci char-ci<=?))

(define (%true-string<= string-prefix-proc char-pred str1 str2 start1 past1 start2 past2)
  (if (and (eq? str1 str2) ($fx= start1 start2))
      ($fx<= past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred
			  str1 str2
			  values values (lambda (i) #f)
			  start1 past1 start2 past2)))

;;; --------------------------------------------------------------------

(define-string2-func string>
  (%true-string> $string-prefix-length char<?))

(define-string2-func string-ci>
  (%true-string> $string-prefix-length-ci char-ci<?))

(define (%true-string> string-prefix-proc char-pred str1 str2 start1 past1 start2 past2)
  (if (and (eq? str1 str2) ($fx= start1 start2))
      ($fx> past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred
			  str1 str2
			  (lambda (i) #f) (lambda (i) #f) values
			  start1 past1 start2 past2)))

;;; --------------------------------------------------------------------

(define-string2-func string>=
  (%true-string>= $string-prefix-length char<=?))

(define-string2-func string-ci>=
  (%true-string>= $string-prefix-length-ci char-ci<=?))

(define (%true-string>= string-prefix-proc char-pred str1 str2 start1 past1 start2 past2)
  (if (and (eq? str1 str2) ($fx= start1 start2))
      ($fx>= past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred
			  str1 str2
			  (lambda (i) #f) values values
			  start1 past1 start2 past2)))


;;;; hashing

(define string-hash
  (case-lambda
   ((str)
    ;;We know that RNRS.STRING-HASH returns a fixnum.
    (rnrs.string-hash str))
   ((str bound)
    (let ((bound (if (zero? bound)
		     (greatest-fixnum)
		   bound)))
      ;;We know that RNRS.STRING-HASH returns a fixnum.
      (mod (rnrs.string-hash str) bound)))
   ((str bound start)
    (string-hash (substring str start (string-length str)) bound))
   ((str bound start end)
    (string-hash (substring str start end) bound))))

(define string-hash-ci
  (case-lambda
   ((str)
    (string-hash (string-downcase str)))
   ((str bound)
    (string-hash (string-downcase str) bound))
   ((str bound start)
    (string-hash (string-downcase str) bound start))
   ((str bound start end)
    (string-hash (string-downcase str) bound start end))))


;;;; prefix and suffix

(module (string-prefix-length
	 string-prefix-length-ci
	 $string-prefix-length
	 $string-prefix-length-ci)

  (define-string2-func string-prefix-length
    ($string-prefix-length))

  (define-string2-func string-prefix-length-ci
    ($string-prefix-length-ci))

  (define ($string-prefix-length str1 str2 start1 past1 start2 past2)
    (%true-string-prefix-length char=? str1 str2 start1 past1 start2 past2))

  (define ($string-prefix-length-ci str1 str2 start1 past1 start2 past2)
    (%true-string-prefix-length char-ci=? str1 str2 start1 past1 start2 past2))

  (define (%true-string-prefix-length char-cmp? str1 str2 start1 past1 start2 past2)
    ;;Find the length of the common prefix.  It is not required that the
    ;;two substrings passed be of equal length.
    (let* ((delta ($min-fixnum-fixnum ($fx- past1 start1)
				      ($fx- past2 start2)))
	   (past1 ($fx+ start1 delta)))
      (if (and (eq? str1 str2)
	       ($fx= start1 start2))
	  delta
	(let lp ((i start1)
		 (j start2))
	  (if (or ($fx>= i past1)
		  (not (char-cmp? ($string-ref str1 i)
				  ($string-ref str2 j))))
	      ($fx- i start1)
	    (lp ($fxadd1 i) ($fxadd1 j)))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (string-suffix-length
	 string-suffix-length-ci
	 $string-suffix-length
	 $string-suffix-length-ci)

  (define-string2-func string-suffix-length
    ($string-suffix-length))

  (define-string2-func string-suffix-length-ci
    ($string-suffix-length-ci))

  (define ($string-suffix-length str1 str2 start1 past1 start2 past2)
    (%true-string-suffix-length char=? str1 str2 start1 past1 start2 past2))

  (define ($string-suffix-length-ci str1 str2 start1 past1 start2 past2)
    (%true-string-suffix-length char-ci=? str1 str2 start1 past1 start2 past2))

  (define (%true-string-suffix-length char-cmp? str1 str2 start1 past1 start2 past2)
    ;;Find the length  of the common suffix.  It is  not required that the
    ;;two substrings passed be of equal length.
    (let* ((delta  ($min-fixnum-fixnum ($fx- past1 start1)
				       ($fx- past2 start2)))
	   (start1 ($fx- past1 delta)))
      (if (and (eq? str1 str2) ($fx= past1 past2))
	  delta
	(let loop ((i ($fxsub1 past1))
		   (j ($fxsub1 past2)))
	  (if (or ($fx< i start1)
		  (not (char-cmp? ($string-ref str1 i)
				  ($string-ref str2 j))))
	      ($fxsub1 ($fx- past1 i))
	    (loop ($fxsub1 i) ($fxsub1 j)))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (string-prefix? $string-prefix?)

  (define-string2-func string-prefix?
    ($string-prefix?))

  (define ($string-prefix? str1 str2 start1 past1 start2 past2)
    (let ((len1 ($fx- past1 start1)))
      (and ($fx<= len1 ($fx- past2 start2)) ; Quick check
	   ($fx= len1 ($string-prefix-length str1 str2 start1 past1 start2 past2)))))

  #| end of module |#)

(module (string-prefix-ci? $string-prefix-ci?)

  (define-string2-func string-prefix-ci?
    ($string-prefix-ci?))

  (define ($string-prefix-ci? str1 str2 start1 past1 start2 past2)
    (let ((len1 ($fx- past1 start1)))
      (and ($fx<= len1 ($fx- past2 start2)) ; Quick check
	   ($fx= len1 ($string-prefix-length-ci str1 str2 start1 past1 start2 past2)))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (string-suffix? $string-suffix?)

  (define-string2-func string-suffix?
    ($string-suffix?))

  (define ($string-suffix? str1 str2 start1 past1 start2 past2)
    (let ((len1 ($fx- past1 start1)))
      (and ($fx<= len1 ($fx- past2 start2))
	   ($fx= len1 ($string-suffix-length str1 str2 start1 past1 start2 past2)))))

  #| end of module |# )

(module (string-suffix-ci? $string-suffix-ci?)

  (define-string2-func string-suffix-ci?
    ($string-suffix-ci?))

  (define ($string-suffix-ci? str1 str2 start1 past1 start2 past2)
    (let ((len1 ($fx- past1 start1)))
      (and ($fx<= len1 ($fx- past2 start2)) ; Quick check
	   ($fx= len1 ($string-suffix-length-ci str1 str2 start1 past1 start2 past2)))))

  #| end of module |# )


;;;; searching

(module (string-index
	 $string-index
	 $string-index/char
	 $string-index/cset
	 $string-index/pred)
  (define who 'string-index)

  (define-string-func string-index
    ($string-index)
    (arguments criterion)
    (validators))

  (define ($string-index str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-index/char str criterion start past))
      ((char-set?)	($string-index/cset str criterion start past))
      ((procedure?)	($string-index/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-index/char str criterion start past)
    (and ($fx< start past)
	 (if ($char= criterion ($string-ref str start))
	     start
	   ($string-index/char str criterion ($fxadd1 start) past))))

  (define ($string-index/cset str criterion start past)
    (and ($fx< start past)
	 (if (char-set-contains? criterion ($string-ref str start))
	     start
	   ($string-index/cset str criterion ($fxadd1 start) past))))

  (define ($string-index/pred str criterion start past)
    (and ($fx< start past)
	 (if (criterion ($string-ref str start))
	     start
	   ($string-index/pred str criterion ($fxadd1 start) past))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (string-index-right
	 $string-index-right
	 $string-index-right/char
	 $string-index-right/cset
	 $string-index-right/pred)
  (define who 'string-index-right)

  (define-string-func string-index-right
    ($string-index-right)
    (arguments criterion)
    (validators))

  (define ($string-index-right str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-index-right/char str criterion start past))
      ((char-set?)	($string-index-right/cset str criterion start past))
      ((procedure?)	($string-index-right/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-index-right/char str criterion start past)
    (let loop ((i ($fxsub1 past)))
      (and ($fx>= i start)
	   (if ($char= criterion ($string-ref str i))
	       i
	     (loop ($fxsub1 i))))))

  (define ($string-index-right/cset str criterion start past)
    (let loop ((i ($fxsub1 past)))
      (and ($fx>= i start)
	   (if (char-set-contains? criterion ($string-ref str i))
	       i
	     (loop ($fxsub1 i))))))

  (define ($string-index-right/pred str criterion start past)
    (let loop ((i ($fxsub1 past)))
      (and ($fx>= i start)
	   (if (criterion ($string-ref str i))
	       i
	     (loop ($fxsub1 i))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (string-skip
	 $string-skip
	 $string-skip/char
	 $string-skip/cset
	 $string-skip/pred)
  (define who 'string-skip)

  (define-string-func string-skip
    ($string-skip)
    (arguments criterion)
    (validators))

  (define ($string-skip str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-skip/char str criterion start past))
      ((char-set?)	($string-skip/cset str criterion start past))
      ((procedure?)	($string-skip/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-skip/char str char start past)
    (and ($fx< start past)
	 (if ($char= char ($string-ref str start))
	     ($string-skip/char str char ($fxadd1 start) past)
	   start)))

  (define ($string-skip/cset str cset start past)
    (and ($fx< start past)
	 (if (char-set-contains? cset ($string-ref str start))
	     ($string-skip/cset str cset ($fxadd1 start) past)
	   start)))

  (define ($string-skip/pred str pred start past)
    (and ($fx< start past)
	 (if (pred ($string-ref str start))
	     ($string-skip/pred str pred ($fxadd1 start) past)
	   start)))

  #| end of module: string-skip |# )

;;; --------------------------------------------------------------------

(module (string-skip-right
	 $string-skip-right
	 $string-skip-right/char
	 $string-skip-right/cset
	 $string-skip-right/pred)
  (define who 'string-skip-right)

  (define-string-func string-skip-right
    ($string-skip-right)
    (arguments criterion)
    (validators))

  (define ($string-skip-right str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-skip-right/char str criterion start past))
      ((char-set?)	($string-skip-right/cset str criterion start past))
      ((procedure?)	($string-skip-right/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-skip-right/char str char start past)
    (let loop ((i ($fxsub1 past)))
      (and ($fx>= i start)
	   (if ($char= char ($string-ref str i))
	       (loop ($fxsub1 i))
	     i))))

  (define ($string-skip-right/cset str cset start past)
    (let loop ((i ($fxsub1 past)))
      (and ($fx>= i start)
	   (if (char-set-contains? cset ($string-ref str i))
	       (loop ($fxsub1 i))
	     i))))

  (define ($string-skip-right/pred str pred start past)
    (let loop ((i ($fxsub1 past)))
      (and ($fx>= i start)
	   (if (pred ($string-ref str i))
	       (loop ($fxsub1 i))
	     i))))

  #| end of module: string-skip-right |# )

;;; --------------------------------------------------------------------

(module (string-count)
  (define who 'string-count)

  (define-string-func string-count
    ($string-count)
    (arguments criterion)
    (validators))

  (define ($string-count str criterion start past)
    (cond-criterion criterion
      ((char?)		($string-count/char str criterion start past))
      ((char-set?)	($string-count/cset str criterion start past))
      ((procedure?)	($string-count/pred str criterion start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-count/char str criterion start past)
    (do ((i start ($fxadd1 i))
	 (count 0 (if ($char= criterion ($string-ref str i))
		      ($fxadd1 count)
		    count)))
	(($fx>= i past)
	 count)))

  (define ($string-count/cset str criterion start past)
    (do ((i start ($fxadd1 i))
	 (count 0 (if (char-set-contains? criterion ($string-ref str i))
		      ($fxadd1 count)
		    count)))
	(($fx>= i past)
	 count)))

  (define ($string-count/pred str criterion start past)
    (do ((i start ($fxadd1 i))
	 (count 0 (if (criterion ($string-ref str i))
		      ($fxadd1 count)
		    count)))
	(($fx>= i past)
	 count)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (string-contains $string-contains)

  (define-string2-func string-contains
    ($string-contains))

  (define ($string-contains text pattern text.start text.past pattern.start pattern.past)
    (%kmp-search char=? string-ref
		 text text.start text.past
		 pattern pattern.start pattern.past))

  #| end of module |# )

(module (string-contains-ci $string-contains-ci)

  (define-string2-func string-contains-ci
    ($string-contains-ci))

  (define ($string-contains-ci text pattern text.start text.past pattern.start pattern.past)
    (%kmp-search char-ci=? string-ref
		 text text.start text.past
		 pattern pattern.start pattern.past))

  #| end of module |# )


;;;; case hacking

(define-string-func string-upcase
  ($string-upcase))

(define ($string-upcase str start end)
  ($string-map char-upcase str start end))

(define-string-func string-upcase!
  ($string-upcase!))

(define ($string-upcase! str start end)
  ($string-map! char-upcase str start end))

;;; --------------------------------------------------------------------

(define-string-func string-downcase
  ($string-downcase))

(define ($string-downcase str start end)
  ($string-map char-downcase str start end))

(define-string-func string-downcase!
  ($string-downcase!))

(define ($string-downcase! str start end)
  ($string-map! char-downcase str start end))

;;; --------------------------------------------------------------------

(module (string-titlecase
	 string-titlecase!)

  (define-string-func string-titlecase
    ($string-titlecase))

  (define ($string-titlecase str start end)
    ($string-titlecase! ($substring str start end) start end))

  (define-string-func string-titlecase!
    ($string-titlecase!))

  (define ($string-titlecase! str start end)
    (cond (($string-index/pred str %char-cased? start end)
	   => (lambda (i)
		($string-set! str i (char-titlecase ($string-ref str i)))
		(let ((i^ ($fxadd1 i)))
		  (cond (($string-skip/pred str %char-cased? i^ end)
			 => (lambda (j)
			      ($string-map! char-downcase str i^ j)
			      ($string-titlecase! str ($fxadd1 j) end)))
			(else
			 ($string-map! char-downcase str i^ end))))))))

  (define (%char-cased? c)
    ;;This works because CHAR-UPCASE returns  #f if the character has no
    ;;upcase version.
    (char-upper-case? (char-upcase c)))

  #| end of module |# )


;;;; reverse and concatenate

(define-string-func string-reverse
  ($string-reverse))

(define ($string-reverse str start past)
  (let* ((len    ($fx- past start))
	 (result (make-string len)))
    (do ((i start         ($fxadd1 i))
	 (j ($fxsub1 len) ($fxsub1 j)))
	(($fxnegative? j)
	 result)
      ($string-set! result j ($string-ref str i)))))

(define-string-func string-reverse!
  ($string-reverse!))

(define ($string-reverse! str start past)
  (do ((i ($fxsub1 past) ($fxsub1 i))
       (j start          ($fxadd1 j)))
      (($fx<= i j)
       str)
    (let ((ci ($string-ref str i)))
      ($string-set! str i ($string-ref str j))
      ($string-set! str j ci))))

;;; --------------------------------------------------------------------

(module (string-concatenate
	 string-concatenate-reverse)

  (define (string-concatenate strings)
    (define who 'string-concatenate)
    (let ((total (%compute-total-length who 0 strings)))
      (with-arguments-validation (who)
	  ((string-length	total))
	(let loop ((result  (make-string total))
		   (i       0)
		   (strings strings))
	  (if (null? strings)
	      result
	    (let* ((s    ($car strings))
		   (slen ($string-length s)))
	      ($string-copy! result i s 0 slen)
	      (loop result ($fx+ i slen) ($cdr strings))))))))

  (module (string-concatenate-reverse)
    (define who 'string-concatenate-reverse)

    (define string-concatenate-reverse
      (case-lambda
       ((strings)
	(string-concatenate-reverse "" 0))

       ((strings final)
	(with-arguments-validation (who)
	    ((string	final))
	  (string-concatenate-reverse final ($string-length final))))

       ((strings final final.len)
	(with-arguments-validation (who)
	    ((string	final))
	  (let* ((strings.len (%compute-total-length who 0 strings))
		 (total.len   (+ final.len strings.len)))
	    (with-arguments-validation (who)
		((string-length	total.len))
	      (let ((result (make-string total.len)))
		($string-copy! result strings.len final 0 final.len)
		(let loop ((i       strings.len)
			   (strings strings))
		  (if (null? strings)
		      result
		    (let* ((S       ($car strings))
			   (S.len   ($string-length S))
			   (i       ($fx- i S.len)))
		      ($string-copy! result i S 0 S.len)
		      (loop i ($cdr strings))))))))))
       ))

    #| end of module |# )

  (define (%compute-total-length who accum strings)
    (if (null? strings)
	accum
      (with-arguments-validation (who)
	  ((pair	strings))
	(let ((S ($car strings)))
	  (with-arguments-validation (who)
	      ((string	S))
	    (%compute-total-length who
				   ($add-number-fixnum accum ($string-length S))
				   ($cdr strings)))))))

  (define-argument-validation (string-length who obj)
    (fixnum? obj)
    (assertion-violation who "total string length too big, it must be a fixnum" obj))

  #| end of module |# )


;;;; mapping

(define-string-func string-map
  ($string-map)
  (pre-arguments proc)
  (validators (procedure proc)))

(define ($string-map proc str start end)
  (let ((S (make-string ($fx- end start))))
    (do ((i start ($fxadd1 i))
	 (j 0     ($fxadd1 j)))
	(($fx= i end)
	 S)
      ($string-set! S j (proc ($string-ref str i))))))

;;; --------------------------------------------------------------------

(define-string-func string-map!
  ($string-map!)
  (pre-arguments proc)
  (validators (procedure proc)))

(define ($string-map! proc str start end)
  (if ($fx= start end)
      str
    (begin
      ($string-set! str start (proc ($string-ref str start)))
      ($string-map! proc str ($fxadd1 start) end))))

;;; --------------------------------------------------------------------

(define-string-func string-for-each
  ($string-for-each)
  (pre-arguments proc)
  (validators (procedure proc)))

(define ($string-for-each proc str start end)
  (if ($fx= start end)
      ;;This return  value is not  required by this  SRFI, but we  do it
      ;;because it makes some sense with little cost.
      str
    (begin
      (proc ($string-ref str start))
      ($string-for-each proc str ($fxadd1 start) end))))

;;; --------------------------------------------------------------------

(define-string-func string-for-each-index
  ($string-for-each-index)
  (pre-arguments proc)
  (validators (procedure proc)))

(define ($string-for-each-index proc str start end)
  (if ($fx= start end)
      ;;This return  value is not  required by this  SRFI, but we  do it
      ;;because it makes some sense with little cost.
      str
    (begin
      (proc start)
      ($string-for-each-index proc str ($fxadd1 start) end))))


;;;; folding

(define-string-func string-fold
  ($string-fold)
  (pre-arguments kons knil)
  (validators (procedure kons)))

(define ($string-fold kons knil str start end)
  (let loop ((v knil)
	     (i start))
    (if ($fx< i end)
	(loop (kons ($string-ref str i) v)
	      ($fxadd1 i))
      v)))

;;; --------------------------------------------------------------------

(define-string-func string-fold-right
  ($string-fold-right)
  (pre-arguments kons knil)
  (validators (procedure kons)))

(define ($string-fold-right kons knil str start end)
  (let loop ((v knil)
	     (i ($fxsub1 end)))
    (if ($fx>= i start)
	(loop (kons ($string-ref str i) v)
	      ($fxsub1 i))
      v)))

;;; --------------------------------------------------------------------

(define string-unfold
  (case-lambda
   ((stop? seed->char make-seed first-seed)
    (string-unfold stop? seed->char make-seed first-seed ""       (lambda (x) "")))

   ((stop? seed->char make-seed first-seed base-str)
    (string-unfold stop? seed->char make-seed first-seed base-str (lambda (x) "")))

   ((stop? seed->char make-seed first-seed base-str make-final)
    (let-values (((port getter)
		  (open-string-output-port)))
      (display base-str port)
      (let loop ((seed first-seed))
	(if (stop? seed)
	    (begin
	      (display (make-final seed) port)
	      (getter))
	  (begin
	    (display (seed->char seed) port)
	    (loop (make-seed seed)))))))))

(define string-unfold-right
  (case-lambda
   ((stop? seed->char make-seed first-seed)
    (string-unfold-right stop? seed->char make-seed first-seed ""       (lambda (x) "")))

   ((stop? seed->char make-seed first-seed base-str)
    (string-unfold-right stop? seed->char make-seed first-seed base-str (lambda (x) "")))

   ((stop? seed->char make-seed first-seed base-str make-final)
    (let-values (((port getter)
		  (open-string-output-port)))
      (display base-str port)
      (let loop ((seed first-seed))
	(if (stop? seed)
	    (begin
	      (display (make-final seed) port)
	      (begin0-let ((retval (getter)))
		($string-reverse! retval 0 ($string-length retval))))
	  (begin
	    (display (seed->char seed) port)
	    (loop (make-seed seed)))))))))


;;;; replicate and rotate

(module (xsubstring
	 string-xcopy!)

  (module (xsubstring)
    (define who 'xsubstring)

    (define xsubstring
      (case-lambda
       ((str from)
	(with-arguments-validation (who)
	    ((string			str)
	     (fixnum			from))
	  (let ((str.len ($string-length str)))
	    ($xsubstring str from str.len 0 str.len))))

       ((str from to)
	(with-arguments-validation (who)
	    ((string			str)
	     (fixnum			from)
	     (fixnum			to))
	  ($xsubstring str from to 0 ($string-length str))))

       ((str from to start)
	(with-arguments-validation (who)
	    ((string			str)
	     (fixnum			from)
	     (fixnum			to)
	     (one-off-index-for-string	str start))
	  ($xsubstring str from to start ($string-length str))))

       ((str from to start past)
	(with-arguments-validation (who)
	    ((string			str)
	     (fixnum			from)
	     (fixnum			to)
	     (start-and-past-for-string	str start past))
	  ($xsubstring str from to start past)))))

    #| end of module |# )

  (module (string-xcopy!)
    (define who 'string-xcopy!)

    (define string-xcopy!
      (case-lambda
       ((dst.str dst.start src.str from)
	(with-arguments-validation (who)
	    ((string			dst.str)
	     (one-off-index-for-string	dst.str dst.start)
	     (string			src.str)
	     (fixnum			from))
	  (let ((src.len ($string-length src.str)))
	    ($string-xcopy! dst.str dst.start src.str from src.len 0 src.len))))

       ((dst.str dst.start src.str from to)
	(with-arguments-validation (who)
	    ((string			dst.str)
	     (one-off-index-for-string	dst.str dst.start)
	     (string			src.str)
	     (fixnum			from)
	     (fixnum			to))
	  ($string-xcopy! dst.str dst.start src.str from to 0 ($string-length src.str))))

       ((dst.str dst.start src.str from to src.start)
	(with-arguments-validation (who)
	    ((string			dst.str)
	     (one-off-index-for-string	dst.str dst.start)
	     (string			src.str)
	     (fixnum			from)
	     (fixnum			to)
	     (one-off-index-for-string	src.str src.start))
	  ($string-xcopy! dst.str dst.start src.str from to src.start ($string-length src.str))))

       ((dst.str dst.start src.str from to src.start src.end)
	(with-arguments-validation (who)
	    ((string			dst.str)
	     (one-off-index-for-string	dst.str dst.start)
	     (string			src.str)
	     (fixnum			from)
	     (fixnum			to)
	     (start-and-past-for-string	src.str src.start src.end))
	  ($string-xcopy! dst.str dst.start src.str from to src.start src.end)))
       ))

    #| end of module |# )

  (define ($xsubstring str from to start past)
    (define who 'xsubstring)
    (let ((str.len	($fx- past start)) ;length of the source substring
	  (result.len	(- to from))) ;length of the result substring
      (with-arguments-validation (who)
	  ((string-length	result.len))
	(cond (($fxzero? result.len)
	       "")

	      (($fxzero? str.len)
	       (assertion-violation who "cannot replicate empty (sub)string"))

	      ;;If  the  source  substring   is  composed  of  a  single
	      ;;character: the  result is  just the replication  of such
	      ;;character.
	      (($fx= 1 str.len)
	       (begin0-let ((retval ($make-string result.len)))
		 ($string-fill! retval ($string-ref str start))))

	      ;;Selected text falls entirely within one span.
	      (($fx= ($fxdiv from str.len)
		     ($fxdiv to   str.len))
	       ($substring str
			   ($fx+ start ($fxmod from str.len))
			   ($fx+ start ($fxmod to   str.len))))

	      ;; Selected text requires multiple spans.
	      (else
	       (begin0-let ((result ($make-string result.len)))
		 (%multispan-repcopy! from to result 0 str start past)))))))

  (define ($string-xcopy! dst.str dst.start src.str from to src.start src.past)
    (define who 'string-xcopy!)
    (let ((tocopy	(- to from)))
      (with-arguments-validation (who)
	  ((string-length	tocopy))
	(let ((str.len	($fx- src.past src.start)))
	  (cond (($fxzero? tocopy))

		(($fxzero? str.len)
		 (assertion-violation who "cannot replicate empty (sub)string"))

		(($fx= 1 str.len)
		 ($string-fill! dst.str ($string-ref src.str src.start) dst.start tocopy))

		;; Selected text falls entirely within one span.
		(($fx= ($fxdiv from str.len)
		       ($fxdiv to   str.len))
		 ($string-copy! dst.str dst.start src.str
				($fx+ src.start ($fxmod from str.len))
				($fx+ src.start ($fxmod to   str.len))))

		(else
		 (%multispan-repcopy! from to dst.str dst.start
				      src.str src.start src.past)))))))

  (define (%multispan-repcopy! from to dst.str dst.start src.str src.start src.past)
    ;;This is the core copying loop for XSUBSTRING and STRING-XCOPY!
    ;;
    (let* ((str.len	(- src.past src.start))
	   (i0		(+ src.start (mod from str.len)))
	   (total-chars	(- to from)))

      ;;Copy the partial span @ the beginning
      ($string-copy! dst.str dst.start src.str i0 src.past)

      (let* ((ncopied (- src.past i0)) ;We've copied this many.
	     (nleft   (- total-chars ncopied)) ;Number of chars left to copy.
	     (nspans  (div nleft str.len))) ;Number of whole spans to copy.

	;;Copy the whole spans in the middle.
	(do ((i (+ dst.start ncopied) (+ i str.len)) ;Current target index.
	     (nspans nspans (- nspans 1))) ;Number of spans to copy.
	    ((zero? nspans)
	     ;;Copy the partial-span at the end and we're done.
	     ($string-copy! dst.str i
			    src.str src.start (+ src.start (- total-chars (- i dst.start)))))
	  ;;Copy a whole span.
	  ($string-copy! dst.str i src.str src.start src.past)))))

  #| end of module |# )


;;;; insertion, parsing, filtering, deleting

(module (string-replace $string-replace)
  (define who 'string-replace)

  (define string-replace
    (case-lambda
     ((str1 str2 start1 past1)
      (with-arguments-validation (who)
	  ((string			str1)
	   (string			str2)
	   (start-and-past-for-string	str1 start1 past1))
	(let ((start2	0)
	      (past2	($string-length str2)))
	  ($string-replace str1 str2 start1 past1 start2 past2))))

     ((str1 str2 start1 past1 start2)
      (with-arguments-validation (who)
	  ((string			str1)
	   (string			str2)
	   (start-and-past-for-string	str1 start1 past1)
	   (one-off-index-for-string	str2 start2))
	(let ((past2	($string-length str2)))
	  ($string-replace str1 str2 start1 past1 start2 past2))))

     ((str1 str2 start1 past1 start2 past2)
      (with-arguments-validation (who)
	  ((string			str1)
	   (string			str2)
	   (start-and-past-for-string	str1 start1 past1)
	   (start-and-past-for-string	str2 start2 past2))
	($string-replace str1 str2 start1 past1 start2 past2)))
     ))

  (define ($string-replace str1 str2 start1 past1 start2 past2)
    (let* ((len1	($string-length str1))
	   (len2	($fx- past2 start2))
	   (result	($make-string ($fx+ len2 ($fx- len1 ($fx- past1 start1))))))
      ($string-copy! result 0                  str1 0      start1)
      ($string-copy! result start1             str2 start2 past2)
      ($string-copy! result ($fx+ start1 len2) str1 past1  len1)
      result))

  #| end of module |# )

(module (string-tokenize $string-tokenize)
  (define who 'string-tokenize)

  (define string-tokenize
    (case-lambda
     ((str)
      (with-arguments-validation (who)
	  ((string	str))
	($string-tokenize str char-set:graphic 0 ($string-length str))))

     ((str token-set)
      (with-arguments-validation (who)
	  ((string	str)
	   (char-set	token-set))
	($string-tokenize str token-set 0 ($string-length str))))

     ((str token-set start)
      (with-arguments-validation (who)
	  ((string			str)
	   (char-set			token-set)
	   (one-off-index-for-string	str start))
	($string-tokenize str token-set start ($string-length str))))

     ((str token-set start past)
      (with-arguments-validation (who)
	  ((string			str)
	   (char-set			token-set)
	   (start-and-past-for-string	str start past))
	($string-tokenize str token-set start past)))
     ))

  (define ($string-tokenize str token-set start past)
    (let loop ((i	past)
	       (result	'()))
      (cond ((and ($fx< start i)
		  ($string-index-right/cset str token-set start i))
	     => (lambda (tpast-1)
		  (let ((tpast ($fxadd1 tpast-1)))
		    (cond (($string-skip-right/cset str token-set start tpast-1)
			   => (lambda (tstart-1)
				(loop tstart-1
				      (cons ($substring str ($fxadd1 tstart-1) tpast)
					    result))))
			  (else
			   (cons ($substring str start tpast) result))))))
	    (else result))))

  #| end of module |# )

(module (string-filter
	 $string-filter
	 $string-filter/char
	 $string-filter/cset
	 $string-filter/pred)
  (define who 'string-filter)

  (define-string-func string-filter
    ($string-filter)
    (pre-arguments criterion)
    (validators))

  (define ($string-filter criterion str start past)
    (cond-criterion criterion
      ((char?)		($string-filter/char criterion str start past))
      ((char-set?)	($string-filter/cset criterion str start past))
      ((procedure?)	($string-filter/pred criterion str start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-filter/char criterion str start past)
    ($string-filter/cset (char-set criterion) str start past))

  (define ($string-filter/cset criterion str start past)
    (let* ((len ($string-fold (lambda (c i)
				(if (char-set-contains? criterion c)
				    ($fxadd1 i)
				  i))
			      0 str start past))
	   (ans ($make-string len)))
      ($string-fold (lambda (c i)
		      (if (char-set-contains? criterion c)
			  (begin
			    ($string-set! ans i c)
			    ($fxadd1 i))
			i))
		    0 str start past)
      ans))

  (define ($string-filter/pred criterion str start past)
    (let* ((slen	($fx- past start))
	   (temp	($make-string slen))
	   (ans-len	($string-fold (lambda (c i)
					(if (criterion c)
					    (begin
					      ($string-set! temp i c)
					      ($fxadd1 i))
					  i))
				      0 str start past)))
      (if ($fx= ans-len slen)
	  temp
	($substring temp 0 ans-len))))

  #| end of module |# )

(module (string-delete
	 $string-delete
	 $string-delete/char
	 $string-delete/cset
	 $string-delete/pred)
  (define who 'string-delete)

  (define-string-func string-delete
    ($string-delete)
    (pre-arguments criterion)
    (validators))

  (define ($string-delete criterion str start past)
    (cond-criterion criterion
      ((char?)		($string-delete/char criterion str start past))
      ((char-set?)	($string-delete/cset criterion str start past))
      ((procedure?)	($string-delete/pred criterion str start past))
      (else
       (%error-wrong-criterion who criterion))))

  (define ($string-delete/char criterion str start past)
    ($string-delete/cset (char-set criterion) str start past))

  (define ($string-delete/cset criterion str start past)
    (let* ((len ($string-fold (lambda (c i)
				(if (char-set-contains? criterion c)
				    i
				  ($fxadd1 i)))
			      0 str start past))
	   (ans ($make-string len)))
      ($string-fold (lambda (c i)
		      (if (char-set-contains? criterion c)
			  i
			(begin
			  ($string-set! ans i c)
			  ($fxadd1 i))))
		    0 str start past)
      ans))

  (define ($string-delete/pred criterion str start past)
    (let* ((slen	($fx- past start))
	   (temp	($make-string slen))
	   (ans-len	($string-fold (lambda (c i)
					(if (criterion c)
					    i
					  (begin
					    ($string-set! temp i c)
					    ($fxadd1 i))))
				      0 str start past)))
      (if ($fx= ans-len slen)
	  temp
	($substring temp 0 ans-len))))

  #| end of module |# )


;;;; knuth-morris-pratt search algorithm

(define (%kmp-search item= item-ref
		     text text-start text-past
		     pattern pattern-start pattern-past)
  (let ((plen (- pattern-past pattern-start))
	(restart-vector (%kmp-make-restart-vector item= item-ref
						  pattern pattern-start pattern-past)))
    ;; The search loop. TJ & PJ are redundant state.
    (let loop ((ti text-start) (pi 0)
	       (tj (- text-past text-start)) ; (- tlen ti) -- how many chars left.
	       (pj plen)) ; (- plen pi) -- how many chars left.
      (if (= pi plen)
	  (- ti plen)			   ; Win.
	(and (<= pj tj)			   ; Lose.
	     (if (item= (item-ref text ti) ; Search.
			(item-ref pattern (+ pattern-start pi)))
		 (loop (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.
	       (let ((pi (vector-ref restart-vector pi)))   ; Retreat.
		 (if (= pi -1)
		     (loop (+ ti 1) 0  (- tj 1) plen) ; Punt.
		   (loop ti       pi tj       (- plen pi))))))))))

(define (%kmp-make-restart-vector item= item-ref
				  pattern pattern-start pattern-past)
  (let* ((rvlen (- pattern-past pattern-start))
	 (restart-vector (make-vector rvlen -1)))
    (when (> rvlen 0)
      (let ((rvlen-1 (- rvlen 1))
	    (c0 (item-ref pattern pattern-start)))
	;;Here's the main loop.  We have  set RV[0] ...  RV[i].  K = I
	;;+ START -- it is the corresponding index into PATTERN.
	(let loop1 ((i 0) (j -1) (k pattern-start))
	  (when (< i rvlen-1)
	    ;; loop2 invariant:
	    ;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
	    ;;   or j = -1.
	    (let loop2 ((j j))
	      (cond ((= j -1)
		     (let ((i1 (+ 1 i)))
		       (when (not (item= (item-ref pattern (+ k 1)) c0))
			 (vector-set! restart-vector i1 0))
		       (loop1 i1 0 (+ k 1))))
		    ;; pat[(k-j) .. k] matches pat[start..start+j].
		    ((item= (item-ref pattern k) (item-ref pattern (+ j pattern-start)))
		     (let* ((i1 (+ 1 i))
			    (j1 (+ 1 j)))
		       (vector-set! restart-vector i1 j1)
		       (loop1 i1 j1 (+ k 1))))

		    (else (loop2 (vector-ref restart-vector j)))))))))
    restart-vector))

(define (%kmp-step item= item-ref
		   restart-vector next-item-from-text
		   next-index-in-pattern pattern pattern-start)
  (let loop ((i next-index-in-pattern))
    (if (item= next-item-from-text (item-ref pattern (+ i pattern-start)))
	(+ i 1)				       ; Done.
      (let ((i (vector-ref restart-vector i))) ; Back up in PATTERN.
	(if (= i -1)
	    0		;;Can't back up  further, return the first index
			;;in pattern from the start.
	  (loop i))))))	;Keep trying for match.

(define (%kmp-partial-search item= item-ref
			     restart-vector
			     next-index-in-pattern
			     text text-start text-end
			     pattern pattern-start)
  (let ((patlen (vector-length restart-vector)))
    (let loop ((ti text-start)
	       (pi next-index-in-pattern))
      (cond ((= pi patlen) (- ti)) ; found
	    ((= ti text-end) pi)   ; consumed all text
	    (else
	     (let ((c (item-ref text ti)))
	       (loop (+ ti 1)
		     ;;The  following  loop  is  an inlined  version  of
		     ;;%KMP-STEP.
		     (let loop2 ((pi pi))
		       (if (item= c (item-ref pattern (+ pi pattern-start)))
			   (+ pi 1)
			 (let ((pi (vector-ref restart-vector pi)))
			   (if (= pi -1) 0
			     (loop2 pi))))))))))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'cond-criterion 'scheme-indent-function 1)
;; End:
