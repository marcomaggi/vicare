;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus writer)
  (export
    write			display
    put-datum			format
    printf			fprintf
    print-unicode		print-graph
    printer-integer-radix	printer-printing-style

    ;;These are for internal use.
    the-printer-printing-style
    case-printing-style

    ;;These are needed by PRETTY-PRINT.
    traverse			TRAVERSAL-HELPERS)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  write			display
		  put-datum		format
		  printf		fprintf
		  print-unicode		print-graph
		  printer-integer-radix	printer-printing-style)
    (only (vicare system $symbols)
	  $unbound-object?)
    (only (vicare system $structs)
	  base-rtd)
    (only (vicare system $codes)
	  $code-annotation)
    (prefix (only (ikarus records procedural)
		  rcd-rtd
		  rcd-parent-rcd)
	    records::))

  (include "ikarus.wordsize.scm" #t)


;;;; public API: configuration parameters

(define print-graph
  (make-parameter #t))

(define print-unicode
  (make-parameter #f))

(define printer-integer-radix
  (make-parameter 10
    (lambda (obj)
      (case obj
	((2 8 10 16)
	 obj)
	(else
	 (assertion-violation 'printer-integer-radix
	   "invalid radix to print integers, expected 2, 8, 10 or 16" obj))))))

(module (printer-printing-style
	 the-printer-printing-style
	 case-printing-style)

  (define the-printer-printing-style
    (make-parameter 'write
      (lambda (obj)
	(case obj
	  ((write)		obj)
	  ((display)		obj)
	  ((pretty-print)	obj)
	  (else
	   (procedure-argument-violation 'the-printer-printing-style
	     "invalid Scheme objects printing style, expected a symbol among: write, display, pretty-print"
	     obj))))))

  (define (printer-printing-style)
    (the-printer-printing-style))

  (define-syntax-rule (set-printing-style-pretty-print)
    (set! the-printer-printing-style 'pretty-print))

  (define-syntax case-printing-style
    (syntax-rules (write display)
      ((_ ((write)		. ?write-body)
	  ((display)		. ?display-body)
	  ((pretty-print)	. ?pretty-print))
       (case (the-printer-printing-style)
	 ((write)		. ?write-body)
	 ((display)		. ?display-body)
	 ((pretty-print)	. ?pretty-print)))
      ))

  #| end of module |# )


;;;; public API: functions

(module (put-datum
	 write		display
	 printf		fprintf
	 format)

  (case-define* write
    ((x)
     (%write-to-port x (current-output-port))
     (void))
    ((x {p open-textual-output-port?})
     (%write-to-port x p)
     (void)))

  (define* (put-datum {p open-textual-output-port?} x)
    (%write-to-port x p)
    (void))

  (case-define* display
    ((x)
     (%display-to-port x (current-output-port))
     (void))
    ((x {p open-textual-output-port?})
     (%display-to-port x p)
     (void)))

;;; --------------------------------------------------------------------

  (define* (fprintf {p open-textual-output-port?} {fmt string?} . args)
    (%formatter __who__ p fmt args)
    (void))

  (define* (printf {fmt string?} . args)
    (%formatter __who__ (current-output-port) fmt args)
    (void))

  (define* (format {fmt string?} . args)
    (receive (port extract)
	(open-string-output-port)
      (%formatter __who__ port fmt args)
      (extract)))

;;; --------------------------------------------------------------------

  (define (%write-to-port x p)
    (parametrise ((the-printer-printing-style 'write))
      (let ((marks-table (make-eq-hashtable)))
	(traverse x marks-table)
	(write-object x p #t marks-table 0)
	(void))))

  (define (%display-to-port x p)
    (parametrise ((the-printer-printing-style 'display))
      (let ((marks-table (make-eq-hashtable)))
	(traverse x marks-table)
	(write-object x p #f marks-table 0)
	(void))))

;;; --------------------------------------------------------------------

  (define (%formatter who p fmt arg*)
    ;;We  want to  print to  the output  port P  only if  we are  sure that  that the
    ;;arguments are  correct, so that  the state of P  is not mutated  with incorrect
    ;;data.  First validate the arguments.
    (let loop ((i     0)
	       (arg*  arg*))
      (if (fx= i (string-length fmt))
	  (unless (null? arg*)
	    (assertion-violation who "extra arguments given for template string" fmt))
	(let ((ch (string-ref fmt i)))
	  (cond ((eqv? ch #\~)
		 (let ((i (fxadd1 i)))
		   (when (fx= i (string-length fmt))
		     (error who "invalid ~ at end of format string" fmt))
		   (let ((ch (string-ref fmt i)))
		     (case ch
		       ((#\~ #\%)
			(loop (fxadd1 i) arg*))

		       ((#\a #\s)
			(when (null? arg*)
			  (error who "insufficient arguments"))
			(loop (fxadd1 i) (cdr arg*)))

		       ((#\b #\o #\x #\d)
			(when (null? arg*)
			  (error who "insufficient arguments"))
			(let ((a (car arg*)))
			  (unless (number? a)
			    (error who "not a number" a))
			  (unless (or (char=? ch #\d) (exact? a))
			    (error who
			      (format "inexact numbers cannot be printed with ~~~a" ch)
			      a)))
			(loop (fxadd1 i) (cdr arg*)))

		       (else
			(error who "invalid sequence character after ~" ch))))))
		(else
		 (loop (fxadd1 i) arg*))))))
    ;;Then format.
    (let loop ((i    0)
	       (arg* arg*))
      (unless (fx= i (string-length fmt))
	(let ((ch (string-ref fmt i)))
	  (if (char=? ch #\~)
	      (let ((i (fxadd1 i)))
		(let ((ch (string-ref fmt i)))
		  (case ch
		    ((#\~)
		     (write-char #\~ p)
		     (loop (fxadd1 i) arg*))

		    ((#\%)
		     (write-char #\newline p)
		     (loop (fxadd1 i) arg*))

		    ((#\a)
		     (%display-to-port (car arg*) p)
		     (loop (fxadd1 i) (cdr arg*)))

		    ((#\s)
		     (%write-to-port (car arg*) p)
		     (loop (fxadd1 i) (cdr arg*)))

		    (else
		     (cond ((assv ch '((#\b . 2) (#\o . 8) (#\x . 16) (#\d . 10)))
			    => (lambda (x)
				 (let ((a (car arg*)))
				   (%display-to-port (number->string a (cdr x)) p))
				 (loop (fxadd1 i) (cdr arg*))))

			   (else
			    (error who "BUG" ch)))))))
	    (begin
	      (write-char ch p)
	      (loop (fxadd1 i) arg*)))))))

  #| end of module |# )


(module TRAVERSAL-HELPERS
  (traverse-shared
   write-shared
   get-writer-marks-bitfield
   writer-marks-bitfield.decode-mark
   writer-marks-bitfield.encode-mark
   writer-marks-bitfield.encode-mark-and-flag
   writer-marks-bitfield.cyclic-set?
   writer-marks-bitfield.shared-set?
   writer-marks-bitfield.mark-set?
   writer-marks-table.set-mark!
   writer-marks-table.shared?
   cache? make-cache cache-string cache-object cache-next)

  (define (traverse-shared X marks-table traverse-kont)
    ;;Step  on the  depth-first traversal  of the  object we  are printing.   Here we
    ;;expect the Scheme  object X to have sub-objects worth  visiting for the purpose
    ;;of  finding  cyclic  references  and shared  references.   TRAVERSE-KONT  is  a
    ;;function to be applied to X to visit its sub-objects.
    ;;
    (cond ((hashtable-ref marks-table X #f)
	   => (lambda (B)
		;;If we  are here it  means we have  already traversed X  before: the
		;;Scheme object X is either  a compound object referencing itself, or
		;;an object appearing  multiple times in the enclosing  object we are
		;;writing.
		(cond ((fixnum? B)
		       (hashtable-set! marks-table X (writer-marks-bitfield.set-shared-bit B)))
		      (else
		       (set-car! B (writer-marks-bitfield.set-cyclic-bit (car B)))))))
	  (else
	   ;;If we  are here  it means  this is the  first time  we visit  the Scheme
	   ;;object X.
	   (hashtable-set! marks-table X EMPTY-BITFIELD)
	   (traverse-kont X marks-table)
	   (let ((B (hashtable-ref marks-table X #f)))
	     ;;After traversing X:  if the bitfield has the shared  bit set, it means
	     ;;the object  X is a compound  object that references itself,  so we set
	     ;;the cyclic bit too.
	     (cond ((fixnum? B)
		    (when (writer-marks-bitfield.shared-set? B)
		      (hashtable-set! marks-table X (writer-marks-bitfield.set-cyclic-bit B))))
		   (else
		    (let ((A (car B)))
		      (when (writer-marks-bitfield.shared-set? A)
			(set-car! B (writer-marks-bitfield.set-cyclic-bit A))))))))))

  (define* (write-shared X port m marks-table next-mark-idx writer-kont)
    ;;Takes  care of  printing  cyclic and  shared structures  with  "#N=" and  "#N#"
    ;;elements,  rather than  going into  infinite recursion.   For this  function to
    ;;work: every  interesting sub-object of the  object X must have  been previously
    ;;visited by the function TRAVERSE, so that the hashtable MARKS-TABLE knows about
    ;;them.
    ;;
    (let ((B (get-writer-marks-bitfield __who__ marks-table X)))
      (cond ((writer-marks-bitfield.mark-set? B)
	     ;;The Scheme object X is either a compound object referencing itself, or
	     ;;an  object appearing  multiple times  in the  enclosing object  we are
	     ;;writing.
	     ;;
	     ;;The Scheme object X has already been written.  This means its bitfield
	     ;;B already contains a mark index.  Extract from the bitfield B its mark
	     ;;index N and write it as "#N#".
	     (write-char #\# port)
	     (put-string port (fixnum->string (writer-marks-bitfield.decode-mark B)))
	     (write-char #\# port)
	     next-mark-idx)

	    ((or (writer-marks-bitfield.cyclic-set? B)
		 (and (writer-marks-bitfield.shared-set? B)
		      (print-graph)))
	     ;;The Scheme object X is either a compound object referencing itself, or
	     ;;an  object appearing  multiple times  in the  enclosing object  we are
	     ;;writing.
	     ;;
	     ;;The Scheme object X  has not yet been written: this  is the first time
	     ;;we write it  to PORT.  We assign  a mark index to it,  then prefix the
	     ;;string representation with "#N=".
	     (writer-marks-table.set-mark! marks-table X next-mark-idx)
	     (write-char #\# port)
	     (put-string port (fixnum->string next-mark-idx))
	     (write-char #\= port)
	     (writer-kont X port m marks-table (fxadd1 next-mark-idx)))

	    (else
	     ;;This object X is neither a  compound object referencing itself, nor an
	     ;;object appearing multiple times in an enclosing object.  We just write
	     ;;its string representation.
	     (writer-kont X port m marks-table next-mark-idx)))))

  (define (get-writer-marks-bitfield who marks-table obj)
    (let ((B (hashtable-ref marks-table obj #f)))
      (cond ((fixnum? B)	B)
	    ((pair?   B)	(car B))
	    (else
	     (assertion-violation who
	       "object has not been processed correctly to handle shared structures"
	       obj)))))

;;; --------------------------------------------------------------------

  (define-constant EMPTY-BITFIELD	0)

  ;;The writer  marks bitfield is  an inclusive OR  composition of the  following bit
  ;;flags:
  (define-constant CYCLIC-BIT		#b001)
  (define-constant SHARED-BIT		#b010)
  (define-constant MARKED-BIT		#b100)
  (define-constant MARK-SHIFT		3)

;;;

  (define (writer-marks-bitfield.decode-mark B)
    (fxsra B MARK-SHIFT))

  (define (writer-marks-bitfield.encode-mark B N)
    (fxior B (fxsll N MARK-SHIFT)))

  (define (writer-marks-bitfield.encode-mark-and-flag B N flag)
    (fxior B flag (fxsll N MARK-SHIFT)))

;;;

  (define (writer-marks-bitfield.set-cyclic-bit B)
    (fxior B CYCLIC-BIT))

  (define (writer-marks-bitfield.set-shared-bit B)
    (fxior B SHARED-BIT))

;;;

  (define (writer-marks-bitfield.cyclic-set? B)
    (fx=? (fxand B CYCLIC-BIT) CYCLIC-BIT))

  (define (writer-marks-bitfield.shared-set? B)
    (fx=? (fxand B SHARED-BIT) SHARED-BIT))

  (define (writer-marks-bitfield.mark-set? B)
    (fx=? (fxand B MARKED-BIT) MARKED-BIT))

;;; --------------------------------------------------------------------

  ;;The  struct CACHE  is used  only when  writing structs  (including R6RS  records)
  ;;having a custom printer function.  Given the following code:
  ;;
  ;;   (define-struct duo
  ;;     (one two))
  ;;
  ;;   (set-rtd-printer! (struct-type-descriptor duo)
  ;;     (lambda (stru port sub-printer)
  ;;       (display "#{duo " port)
  ;;       (sub-printer (duo-one stru))
  ;;       (display " " port)
  ;;       (sub-printer (duo-two stru))
  ;;       (display "}" port))
  ;;
  ;;   (display (make-duo 1 2))
  ;;
  ;;the writer builds  a value named CACHE-STACK  in the code.  The  CACHE-STACK is a
  ;;pair whose car is the last string written  to PORT and whose cdr is a linked list
  ;;of CACHE structs:
  ;;
  ;;   ("}" . ?caches)
  ;;
  ;;where the linked list ?CACHES is as follows:
  ;;
  ;;   ---
  ;;    | string=" "
  ;;    | object=2
  ;;    | next --> ---
  ;;   ---          | string="#duo "
  ;;                | object=1
  ;;                | next --> #f
  ;;               ---
  ;;
  ;;every new  CACHE struct  is built  by the sub-printer  function and  contains the
  ;;string displayed to PORT before the call  and the object to which the sub-printer
  ;;is applied.
  ;;
  ;;The linked list  of CACHE structs contains  the output of the  printer in reverse
  ;;order.  We can use the following code to build a list out of a CACHE-STACK:
  ;;
  ;;   (cons (car cache-stack)
  ;;         (let recur ((cache (cdr cache-stack)))
  ;;           (if cache
  ;;               (cons* (cache-object cache)
  ;;                      (cache-string cache)
  ;;                      (recur (cache-next cache)))
  ;;             '())))
  ;;
  ;;for the example the return value is:
  ;;
  ;;   ("}" 2 " " 1 "#{duo ")
  ;;
  (define-struct cache
    (string object next))

;;; --------------------------------------------------------------------

  (define (writer-marks-table.set-mark! marks-table X N)
    (let ((B (hashtable-ref marks-table X #f)))
      (if (fixnum? B)
	  (hashtable-set! marks-table X (writer-marks-bitfield.encode-mark-and-flag B N MARKED-BIT))
	(set-car! B (writer-marks-bitfield.encode-mark-and-flag (car B) N MARKED-BIT)))))

  (define (writer-marks-table.shared? marks-table X)
    (cond ((hashtable-ref marks-table X #f)
	   => (lambda (B)
		(writer-marks-bitfield.shared-set? (if (fixnum? B) B (car B)))))
	  (else #f)))

  #| end of module |#)


(module (traverse)
  (import TRAVERSAL-HELPERS)

  (define (traverse X marks-table)
    ;;Perform a depth-first search in the Scheme object X with the purpose of finding
    ;;compound  objects  referencing  themselves   (cyclic  references)  and  objects
    ;;appearing  multiple  times  in  an  enclosing object  we  are  writing  (shared
    ;;objects).
    ;;
    ;;Fill the EQ hashtable MARKS-TABLE with an entry for every visited sub-object of
    ;;X.
    ;;
    (cond ((pair?       X)	(traverse-shared X marks-table traverse-pair))
	  ((vector?     X)	(traverse-shared X marks-table traverse-vector))
	  ((string?     X)	(traverse-shared X marks-table traverse-noop))
	  ;;At present  keywords are  Vicare structs,  so we have  to make  sure this
	  ;;branch comes before the one below.
	  ((keyword?	X)	(void))
	  ((struct?     X)	(traverse-shared X marks-table traverse-struct))
	  ((bytevector? X)	(traverse-shared X marks-table traverse-noop))
	  ((gensym?     X)	(traverse-shared X marks-table traverse-noop))
	  ((code?       X)	(traverse-shared X marks-table traverse-code))
	  (else			(void))))

  (define (traverse-noop X marks-table)
    (void))

  (define (traverse-pair X marks-table)
    (traverse (car X) marks-table)
    (traverse (cdr X) marks-table))

  (define (traverse-code X marks-table)
    (cond (($code-annotation X)
	   => (lambda (ann)
		(traverse ann marks-table)))
	  (else
	   (void))))

  (define (traverse-vector X marks-table)
    (let f ((i 0) (n (vector-length X)))
      (unless (fx=? i n)
        (traverse (vector-ref X i) marks-table)
        (f (fx+ i 1) n))))

  (module (traverse-struct)
    ;;This module processes: Vicare's structs; Vicare's struct-type descriptors; R6RS
    ;;records; R6RS record-type descriptors.
    ;;
    (define (traverse-struct stru marks-table)
      (if (record-object? stru)
	  ;;It is an R6RS record.
	  (cond ((record-printer stru)
		 => (lambda (printer)
		      (%traverse-custom-struct stru marks-table printer)))
		(else
		 (%traverse-vanilla-struct stru marks-table)))
	(cond ((struct-printer stru)
	       => (lambda (printer)
		    (%traverse-custom-struct stru marks-table printer)))
	      (else
	       (%traverse-vanilla-struct stru marks-table)))))

    (define (%traverse-vanilla-struct stru marks-table)
      ;;Traverse a struct object that is meant to use the built-in printer function.
      ;;
      (let ((rtd (struct-rtd stru)))
	(unless (and (record-type-descriptor? rtd)
		     (record-type-opaque? rtd))
	  (traverse (struct-name stru) marks-table)
	  (let ((num-of-fields (struct-length stru)))
	    (let loop ((field-idx 0))
	      (unless (fx=? field-idx num-of-fields)
		(traverse (struct-ref stru field-idx) marks-table)
		(loop (fxadd1 field-idx))))))))

    (define* (%traverse-custom-struct stru marks-table printer)
      ;;Traverse a struct object with a  custom printer function.  The custom printer
      ;;is used to print the struct in  a string output port; the resulting string is
      ;;cached
      ;;
      ;;Here we build a CACHE-STACK.  Given the following code:
      ;;
      ;;   (define-struct duo
      ;;     (one two))
      ;;
      ;;   (set-rtd-printer! (struct-type-descriptor duo)
      ;;     (lambda (stru port sub-printer)
      ;;       (display "#{duo " port)
      ;;       (sub-printer (duo-one stru))
      ;;       (display " " port)
      ;;       (sub-printer (duo-two stru))
      ;;       (display "}" port))
      ;;
      ;;   (display (make-duo 1 2))
      ;;
      ;;the CACHE-STACK is  a pair whose car  is the last string written  to PORT and
      ;;whose cdr is a linked list of CACHE structs:
      ;;
      ;;   ("}" . ?caches)
      ;;
      ;;where the linked list ?CACHES is as follows:
      ;;
      ;;   ---
      ;;    | string=" "
      ;;    | object=2
      ;;    | next --> ---
      ;;   ---          | string="#duo "
      ;;                | object=1
      ;;                | next --> #f
      ;;               ---
      ;;
      ;;every new CACHE struct is built  by the sub-printer function and contains the
      ;;string displayed before  the call and the object to  which the sub-printer is
      ;;applied.  The linked list of CACHE structs contains the output of the printer
      ;;in reverse order.
      ;;
      (receive (port extract)
	  (open-string-output-port)
	(let* ((cache-stack  #f)
	       (sub-printer  (lambda (sub-object)
			       (set! cache-stack (make-cache (extract) sub-object cache-stack))
			       (traverse sub-object marks-table))))
	  (printer stru port sub-printer)
	  (let ((cache-stack (cons (extract) cache-stack))
		(B           (hashtable-ref marks-table stru #f)))
	    ;; (debug-print 'built-cache-stack
	    ;; 		 (cons (car cache-stack)
	    ;; 		       (let recur ((cache (cdr cache-stack)))
	    ;; 			 (if cache
	    ;; 			     (cons* (cache-object cache)
	    ;; 				    (cache-string cache)
	    ;; 				    (recur (cache-next cache)))
	    ;; 			   '()))))
	    (if (fixnum? B)
		(hashtable-set! marks-table stru (cons B cache-stack))
	      (error __who__ "internal error"))))))

    #| end of module: TRAVERSE-STRUCT |# )

  #| end of module |# )


(module (write-object)
  ;;All the function whose name starts  with "write-object-" are writers for specific
  ;;object types.
  ;;
  ;;For all  the functions: if the  argument WRITE-STYLE? is non-false,  it means the
  ;;object must be  written in WRITE style;  otherwise the object must  be written in
  ;;DISPLAY style.
  ;;
  (import TRAVERSAL-HELPERS)

  (define (write-object x p write-style? marks-table next-mark-idx)
    (cond ((pair? x)
	   (write-shared x p write-style? marks-table next-mark-idx write-object-pair))

	  ((symbol? x)
	   (if (gensym? x)
	       (write-shared x p write-style? marks-table next-mark-idx write-object-gensym)
	     (begin
	       ;;We do not cache the representation of interned symbols!
	       (write-object-symbol x p write-style?)
	       next-mark-idx)))

	  ((fixnum? x)
	   (write-object-fixnum x p)
	   next-mark-idx)

	  ((string? x)
	   ;; (begin
	   ;;   (write-object-string x p write-style?)
	   ;;   next-mark-idx)
	   (write-shared x p write-style? marks-table next-mark-idx write-object-string))

	  ((boolean? x)
	   (write-char #\# p)
	   (write-char (if x #\t #\f) p)
	   next-mark-idx)

	  ((char? x)
	   (write-object-character x p write-style?)
	   next-mark-idx)

	  ((null? x)
	   (write-char #\( p)
	   (write-char #\) p)
	   next-mark-idx)

	  ((number? x)
	   (write-char* (if (or (fixnum? x)
				(bignum? x))
			    (number->string x (printer-integer-radix))
			  (number->string x))
			p)
	   next-mark-idx)

	  ((vector? x)
	   (write-shared x p write-style? marks-table next-mark-idx write-object-vector))

	  ((bytevector? x)
	   (write-shared x p write-style? marks-table next-mark-idx write-object-bytevector))

	  ((procedure? x)
	   (write-object-procedure x p)
	   next-mark-idx)

	  ((port? x)
	   (write-object-port x p marks-table next-mark-idx)
	   next-mark-idx)

	  ((eq? x (void))
	   (write-char* "#!void" p)
	   next-mark-idx)

	  ((eof-object? x)
	   (write-char* "#!eof" p)
	   next-mark-idx)

	  ((bwp-object? x)
	   (write-char* "#!bwp" p)
	   next-mark-idx)

	  ((would-block-object? x)
	   (write-char* "#!would-block" p)
	   next-mark-idx)

	  ((transcoder? x)
	   (write-char* (string-append "#<transcoder"
				       " codec="		(symbol->string (transcoder-codec x))
				       " eol-style="		(symbol->string (transcoder-eol-style x))
				       " error-handling-mode="	(symbol->string (transcoder-error-handling-mode x))
				       ">")
			p)
	   next-mark-idx)

	  ((keyword? x)
	   ;;At present  keywords are Vicare  structs, so we  have to make  sure this
	   ;;branch comes before the one below.
	   (write-char* (keyword->string x) p)
	   next-mark-idx)

	  ((struct? x)
	   (write-shared x p write-style? marks-table next-mark-idx write-object-struct))

	  ((code? x)
	   (write-object-code x p write-style? marks-table next-mark-idx))

	  ((pointer? x)
	   (write-object-pointer x p)
	   next-mark-idx)

	  (($unbound-object? x)
	   (write-char* "#!unbound-object" p)
	   next-mark-idx)

	  (else
	   (write-char* "#<unknown>" p)
	   next-mark-idx)))

;;; --------------------------------------------------------------------

  (define (write-object-pointer ptr port)
    (write-char* "#<pointer #x" port)
    (write-hex (pointer->integer ptr)
	       (boot.case-word-size
		((32)		8)
		((64)		16))
	       port)
    (write-char #\> port))

;;; --------------------------------------------------------------------

  (define (write-object-fixnum fx port)
    ;;Write the fixnum FX to the output textual port PORT.
    ;;
    (define (loop fx port)
      (unless (fxzero? fx)
	(loop (fxquotient fx 10) port)
	(write-char (integer->char (fx+ (fxremainder fx 10)
					(char->integer #\0)))
		    port)))
    (let ((radix (printer-integer-radix)))
      (if (fx=? 10 radix)
	  (cond ((fxzero? fx)
		 (write-char #\0 port))
		((fx< fx 0)
		 (write-char* (fixnum->string fx) port))
		(else
		 (loop fx port)))
	(begin
	  (write-char #\# port)
	  (case radix
	    ((2)	(write-char #\b port))
	    ((8)	(write-char #\o port))
	    ((16)	(write-char #\x port)))
	  (write-char* (number->string fx radix) port)))))

;;; --------------------------------------------------------------------

  (module (write-object-pair)

    (define (write-object-pair x p write-style? marks-table next-mark-idx)
      (cond ((%pretty-format-reader-macro x marks-table)
	     => (lambda (prefix-str)
		  ;;If we  are here PREFIX-STR  is a string and  X is a  list holding
		  ;;only one item.  Examples:
		  ;;
		  ;;* X  is the list  "(quote ?thing)" and  PREFIX-STR is "'",  so we
		  ;;want to print "'?thing".
		  ;;
		  ;;* X is  the list "(quasiquote ?thing)" and PREFIX-STR  is ",", so
		  ;;we want to print ",?thing".
		  ;;
		  ;;* X is  the list "(syntax ?thing)" and PREFIX-STR  is "#'", so we
		  ;;want to print "#'?thing".
		  ;;
		  (let ((next-mark-idx (write-object-string prefix-str p #f marks-table next-mark-idx)))
		    (write-object (cadr x) p write-style? marks-table next-mark-idx))))
	    (else
	     (write-char #\( p)
	     (receive-and-return (next-mark-idx)
		 (let loop ((d  (cdr x))
			    (next-mark-idx  (write-object (car x) p write-style? marks-table next-mark-idx)))
		   (cond ((null? d)
			  next-mark-idx)
			 ((not (pair? d))
			  (write-char #\space p)
			  (write-char #\. p)
			  (write-char #\space p)
			  (write-object d p write-style? marks-table next-mark-idx))
			 ((writer-marks-table.shared? marks-table d)
			  (write-char #\space p)
			  (when (print-graph)
			    (write-char #\. p)
			    (write-char #\space p))
			  (write-object d p write-style? marks-table next-mark-idx))
			 (else
			  (write-char #\space p)
			  (let ((next-mark-idx (write-object (car d) p write-style? marks-table next-mark-idx)))
			    (loop (cdr d) next-mark-idx)))))
	       (write-char #\) p)))))

    (define (%pretty-format-reader-macro x marks-table)
      ;;Evaluate to non-false if:
      ;;
      ;;* X is a list with the format:
      ;;
      ;;   (?symbol ?thing)
      ;;
      ;;* The symbol ?SYMBOL  is recognised by PRETTY-FORMAT as the  name of a syntax
      ;;having  a  special   reader  macro.   For  example:  QUOTE   has  the  reader
      ;;abbreviation "'";  SYNTAX has  the reader  abbreviation "#'";  and so  on for
      ;;QUASIQUOTE, UNQUOTE, QUASISYNTAX, UNSYNTAX.
      ;;
      ;;otherwise evaluate to false.
      ;;
      ;;The  non-false return  value is  the  string prefix  representing the  reader
      ;;abbreviation.  So if X  is "(quote ?thing)" the return value is  "'"; if X is
      ;;"(syntax ?thing)" the return value is "#'"; and so on.
      ;;
      (and (pair? x)
	   (let ((a (car x)))
	     (and (symbol? a)
		  ;;Evaluate to true  if "(cdr X)" is a proper  list holding only one
		  ;;item and the item is not a shared object.
		  (let ((d (cdr x)))
		    (and (pair? d)
			 (null? (cdr d))
			 (not (writer-marks-table.shared? marks-table d))))
		  ;;Evaluate  to true  if  P is  a  pair having:  as  car the  symbol
		  ;;"pretty-format-reader-macro"; as cdr a string.  The true value is
		  ;;the string itself.
		  ;;
		  ;;NOTE  PRETTY-FORMAT return  a  thunk.  If  A  is a  pretty-format
		  ;;selector, the thunk  returns a pair; otherwise  the thunk returns
		  ;;false.
		  (let ((p ((pretty-format a))))
		    (and (pair? p)
			 (eq? (car p) 'pretty-format-reader-macro)
			 (let ((d (cdr p)))
			   (and (string? d)
				d))))))))

    #| end of module: WRITE-OBJECT-PAIR |# )

;;; --------------------------------------------------------------------

  (define (write-object-vector x p write-style? marks-table next-mark-idx)
    (define (f x p write-style? marks-table next-mark-idx idx n)
      (cond
       ((fx= idx n) next-mark-idx)
       (else
	(write-char #\space p)
	(let ((next-mark-idx (write-object (vector-ref x idx) p write-style? marks-table next-mark-idx)))
	  (f x p write-style? marks-table next-mark-idx (fx+ idx 1) n)))))
    (write-char #\# p)
    (let ((n (vector-length x)))
      (cond ((fxzero? n)
	     (write-char #\( p)
	     (write-char #\) p)
	     next-mark-idx)
	    (else
	     (write-char #\( p)
	     (let ((next-mark-idx (write-object (vector-ref x 0) p write-style? marks-table next-mark-idx)))
	       (f x p write-style? marks-table next-mark-idx 1 n)
	       (write-char #\) p)
	       next-mark-idx)))))

;;; --------------------------------------------------------------------

  (define (write-object-bytevector x p write-style? marks-table next-mark-idx)
    (write-char #\# p)
    (write-char #\v p)
    (write-char #\u p)
    (write-char #\8 p)
    (write-char #\( p)
    (let ((n (bytevector-length x)))
      (when (fx> n 0)
        (write-object-fixnum (bytevector-u8-ref x 0) p)
        (let f ((idx 1) (n n) (x x) (p p))
          (unless (fx= idx n)
            (write-char #\space p)
            (write-object-fixnum (bytevector-u8-ref x idx) p)
            (f (fxadd1 idx) n x p)))))
    (write-char #\) p)
    next-mark-idx)

  (define (write-positive-hex-fx n p)
    (unless (fx= n 0)
      (write-positive-hex-fx (fxsra n 4) p)
      (let ((n (fxand n #xF)))
        (cond
	 ((fx<= n 9)
	  (write-char (integer->char
		       (fx+ (char->integer #\0) n))
		      p))
	 (else
	  (write-char (integer->char
		       (fx+ (char->integer #\A) (fx- n 10)))
		      p))))))

  (define (write-inline-hex b p)
    (write-char #\\ p)
    (write-char #\x p)
    (if (fxzero? b)
        (write-char #\0 p)
      (write-positive-hex-fx b p))
    (write-char #\; p))

;;; --------------------------------------------------------------------

  (define (write-object-character x p write-style?)
    (define char-table ; first nonprintable chars
      '#("nul" "x1" "x2" "x3" "x4" "x5" "x6" "alarm"
         "backspace" "tab" "linefeed" "vtab" "page" "return" "xE" "xF"
         "x10" "x11" "x12" "x13" "x14" "x15" "x16" "x17"
         "x18" "x19" "x1A" "esc" "x1C" "x1D" "x1E" "x1F"
         "space"))
    (if write-style?
        (let ((i (char->integer x)))
          (write-char #\# p)
          (cond ((fx< i (vector-length char-table))
		 (write-char #\\ p)
		 (write-char* (vector-ref char-table i) p))
		((fx< i 127)
		 (write-char #\\ p)
		 (write-char x p))
		((fx= i 127)
		 (write-char #\\ p)
		 (write-char* "delete" p))
		((and (print-unicode)
		      (unicode-printable-char? x))
		 (write-char #\\ p)
		 (write-char x p))
		(else
		 (write-char #\\ p)
		 (write-char #\x p)
		 (write-positive-hex-fx i p))))
      (write-char x p)))

;;; --------------------------------------------------------------------

  (module (write-object-string)

    (define (write-object-string x p write-style? marks-table next-mark-idx)
      (if write-style?
	  (write-string-escape x p)
	(write-char* x p))
      next-mark-idx)

    (define (write-string-escape x p)
      ;; commonize with write-symbol-bar-escape
      (define (loop x i n p)
	(unless (fx= i n)
	  (let* ((ch   (string-ref x i))
		 (byte (char->integer ch)))
	    (cond
	     ((fx< byte 32)
	      (cond
	       ((fx< byte 7)
		(write-inline-hex byte p))
	       ((fx< byte 14)
		(write-char #\\ p)
		(write-char (string-ref "abtnvfr" (fx- byte 7)) p))
	       (else
		(write-inline-hex byte p))))
	     ((or (char=? #\" ch) (char=? #\\ ch))
	      (write-char #\\ p)
	      (write-char ch p))
	     ((fx< byte 127)
	      (write-char ch p))
	     ((or (fx= byte 127) ;this is the #\delete char
		  (fx= byte #x85)
		  (fx= byte #x2028))
	      (write-inline-hex byte p))
	     ((print-unicode)
		;PRINT-UNICODE  is  a parameter,  #t  if  we must  write
		;unicode chars
	      (write-char ch p))
	     (else
	      (write-inline-hex byte p))))
	  (loop x (fxadd1 i) n p)))
      (write-char #\" p)
      (loop x 0 (string-length x) p)
      (write-char #\" p))

    #| end of module: WRITE-OBJECT-STRING |# )

;;; --------------------------------------------------------------------

  (module (write-object-gensym write-object-symbol)

    (define (write-object-symbol x p write-style?)
      (write-symbol-string (symbol->string x) p write-style?))

    (define (write-object-gensym x p write-style? marks-table next-mark-idx)
      (cond ((and write-style? (print-gensym))
	     =>	(lambda (gensym-how)
		  (case gensym-how
		    ((pretty)
		     (let ((str (symbol->string x)))
		       (write-char #\# p)
		       (write-char #\: p)
		       (write-symbol-string str p write-style?)))
		    (else
		     (let ((str (symbol->string x))
			   (ustr (gensym->unique-string x)))
		       (write-char #\# p)
		       (write-char #\{ p)
		       (write-symbol-string str p write-style?)
		       (write-char #\space p)
		       (write-symbol-bar-esc ustr p)
		       (write-char #\} p))))
		  next-mark-idx))
	    (else
	     (write-object-symbol x p write-style?)
	     next-mark-idx)))

    (module (write-symbol-bar-esc)

      (define (write-symbol-bar-esc x p)
	(write-char #\| p)
	(%write-symbol-bar-esc-loop x 0 (string-length x) p)
	(write-char #\| p))

      (define (%write-symbol-bar-esc-loop x i n p)
	(unless (fx= i n)
	  (let* ((c (string-ref x i))
		 (b (char->integer c)))
	    (cond ((fx< b 32)
		   (cond ((fx< b 7)
			  (write-inline-hex b p))
			 ((fx< b 14)
			  (write-char #\\ p)
			  (write-char (string-ref "abtnvfr" (fx- b 7)) p))
			 (else
			  (write-inline-hex b p))))
		  ((memq c '(#\\ #\|))
		   (write-char #\\ p)
		   (write-char c p))
		  ((fx< b 127)
		   (write-char c p))
		  (else
		   (write-inline-hex b p))))
	  (%write-symbol-bar-esc-loop x (fxadd1 i) n p)))

      #| end of module |# )

    (define (write-symbol-string str p write-style?)
      (define-syntax ascii-map
        (lambda (x)
          (syntax-case x ()
            ((stx str) (string? (syntax->datum #'str))
             (let ((s (syntax->datum #'str))
                   (bv (make-bytevector 16 0)))
               (for-each
		   (lambda (c)
		     (let ((b (char->integer c)))
		       (let ((i (fxlogand b 7))
			     (j (fxsra b 3)))
			 (bytevector-u8-set! bv j
					     (fxlogor (bytevector-u8-ref bv j)
						      (fxsll 1 i))))))
                 (string->list s))
               (with-syntax ((bv (datum->syntax #'stx bv)))
                 #'(quote bv)))))))
      (define subsequents-map
        (ascii-map
	 "!$%&*/:<=>?^_~+-.@abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
      (define initials-map
        (ascii-map
	 "!$%&*/:<=>?^_~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      (define initial-categories
        '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))
      (define subsequent-categories
        '(Nd Mc Me))
      (define (in-map? byte map)
        (let ((i (fxand byte 7))
              (j (fxsra byte 3)))
          (and
	   (fx< j (bytevector-length map))
	   (let ((mask (fxsll 1 i)))
	     (not (fxzero?
		   (fxlogand mask
			     (bytevector-u8-ref map j))))))))
      (define (subsequent*? str i n)
        (or (fx= i n)
            (and (subsequent? (string-ref str i))
                 (subsequent*? str (fxadd1 i) n))))
      (define (subsequent? x)
        (define (digit? c)
          (and (char<=? #\0 c) (char<=? c #\9)))
        (define (special-subsequent? x)
          (memq x '(#\+ #\- #\. #\@)))
        (define (special-initial? x)
          (memq x '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~)))
        (define (letter? c)
          (or (and (char<=? #\a c) (char<=? c #\z))
              (and (char<=? #\A c) (char<=? c #\Z))))
        (define (initial? c)
          (or (letter? c) (special-initial? c)))
        (or (initial? x)
            (digit? x)
            (special-subsequent? x)))
      (define (peculiar-symbol-string? str)
        (let ((n (string-length str)))
          (cond
	   ((fx= n 1)
	    (memq (string-ref str 0) '(#\+ #\-)))
	   ((fx>= n 2)
	    (or (and (char=? (string-ref str 0) #\-)
		     (char=? (string-ref str 1) #\>)
		     (subsequent*? str 2 n))
		(string=? str "...")))
	   (else #f))))
      (define (write-symbol-hex-esc str p)
        (let ((n (string-length str)))
          (cond
	   ((fx= n 0)
	    (write-char #\| p)
	    (write-char #\| p))
	   (else
	    (let* ((c0 (string-ref str 0))
		   (b0 (char->integer c0)))
	      (cond
	       ((in-map? b0 initials-map)
		(write-char c0 p))
	       ((fx< b0 128) (write-inline-hex b0 p))
	       ((and (print-unicode)
		     (memq (char-general-category c0) initial-categories))
		(write-char c0 p))
	       (else (write-inline-hex b0 p)))
	      (write-subsequent* str 1 n p))))))
      (define (write-subsequent* str i j p)
        (unless (fx= i j)
          (let* ((c (string-ref str i))
                 (b (char->integer c)))
            (cond
	     ((in-map? b subsequents-map)
	      (write-char c p))
	     ((fx< b 128)
	      (write-inline-hex b p))
	     ((and (print-unicode)
		   (let ((cat (char-general-category c)))
		     (or (memq cat initial-categories)
			 (memq cat subsequent-categories))))
	      (write-char c p))
	     (else
	      (write-inline-hex b p))))
          (write-subsequent* str (fxadd1 i) j p)))
      (define (write-peculiar str p)
        (let ((n (string-length str)))
          (cond
	   ((fx= n 1)
	    (write-char (string-ref str 0) p))
	   ((and (fx>= n 2)
		 (char=? (string-ref str 0) #\-)
		 (char=? (string-ref str 1) #\>))
	    (write-char #\- p)
	    (write-char #\> p)
	    (write-subsequent* str 2 n p))
	   ((string=? str "...")
	    (write-char #\. p)
	    (write-char #\. p)
	    (write-char #\. p))
	   (else (error 'write-peculiar "BUG")))))
      (if write-style?
	  (if (peculiar-symbol-string? str)
	      (write-peculiar str p)
	    (write-symbol-hex-esc str p))
	(write-char* str p)))

    #| end of module: WRITE-OBJECT-SYMBOL, WRITE-OBJECT-GENSYM |# )

;;; --------------------------------------------------------------------

  (module (write-object-struct)

    (define (write-object-struct x p write-style? marks-table next-mark-idx)
      (let ((B (hashtable-ref marks-table x #f)))
	(cond ((pair? B)
	       (%write-struct/custom-printer (cdr B) p write-style? marks-table next-mark-idx))
	      (else
	       (%write-struct/default-printer x p write-style? marks-table next-mark-idx)))))

    (define (%write-struct/custom-printer cache-stack p write-style? marks-table next-mark-idx)
      ;;Write a struct having a custom printer function.
      ;;
      ;;We expect CACHE-STACK to  be a pair whose car is a suffix  string to write at
      ;;the end and whose  cdr is false or a chain of  CACHE structs representing the
      ;;strings to print in reverse order.  So we recurse to the end of the chain and
      ;;print the node from the tail to the head.
      ;;
      (begin0
	  (let recur ((cache (cdr cache-stack)))
	    (if (not cache)
		next-mark-idx
	      (let ((next-mark-idx (recur (cache-next cache))))
		(write-char*  (cache-string cache) p)
		(write-object (cache-object cache) p write-style? marks-table next-mark-idx))))
	(write-char* (car cache-stack) p)))

    (define (%write-struct/default-printer stru p write-style? marks-table next-mark-idx)
      ;;Write a struct that is meant to use the built-in printer function.
      ;;
      (cond ((record-type-descriptor? stru)
	     (%write-r6rs-record-type-descriptor stru p write-style? marks-table next-mark-idx))

	    ((record-constructor-descriptor? stru)
	     (%write-r6rs-record-constructor-descriptor stru p write-style? marks-table next-mark-idx))

	    ((record-type-descriptor? (struct-rtd stru))
	     (%write-r6rs-record stru p write-style? marks-table next-mark-idx))

	    ;;We do not handle opaque records specially.
	    ;; ((let ((rtd (struct-rtd x)))
	    ;;    (and (record-type-descriptor? rtd)
	    ;; 	    (record-type-opaque? rtd)))
	    ;;  (write-char* "#<unknown>" p)
	    ;;  next-mark-idx)

	    (else
	     (%write-vicare-struct stru p write-style? marks-table next-mark-idx))))

    (define (%write-r6rs-record-type-descriptor rtd port write-style? marks-table next-mark-idx)
      ;;Remember that record-type descriptors are struct instances.
      ;;
      (let ((std (struct-rtd rtd)))
	(write-char* "#[rtd " port)
	(write-char* (symbol->string (record-type-name rtd)) port)
	(%write-struct-fields rtd 1 (cdr (struct-type-field-names std)) port write-style? marks-table next-mark-idx)))

    (define (%write-r6rs-record-constructor-descriptor rcd port write-style? marks-table next-mark-idx)
      (let ((rtd (records::rcd-rtd rcd)))
	(write-char* "#[rcd " port)
	(write-char* (symbol->string (record-type-name rtd)) port)
	(write-char #\space port)
	(write-char* "rtd=" port)
	(let ((next-mark-idx (write-object rtd port write-style? marks-table next-mark-idx)))
	  (write-char #\space port)
	  (write-char* "parent-rcd=" port)
	  (begin0
	      (write-object (records::rcd-parent-rcd rcd) port write-style? marks-table next-mark-idx)
	    (write-char #\] port)))))

    (define (%write-vicare-struct stru port write-style? marks-table next-mark-idx)
      ;;If it is an instance we want to print all the fields as in:
      ;;
      ;;   #[struct ?type-name ?field=?value ...]
      ;;
      ;;if it  is a struct-type we  want to skip the  first field, which is  the type
      ;;name, as in:
      ;;
      ;;   #[struct-type ?type-name length=?len fields=?fields ...]
      ;;
      (let* ((std       (struct-rtd stru))
	     (instance? (not (eq? std (base-rtd)))))
	(write-char* (if instance? "#[struct " "#[struct-type ") port)
	(write-char* (if instance?
			 (struct-type-name std)
		       (struct-ref stru 0))
		     port)
	(%write-struct-fields stru
			      (if instance? 0 1)
			      (let ((names (struct-type-field-names std)))
				(if instance? names (cdr names)))
			      port write-style? marks-table next-mark-idx)))

    (define (%write-struct-fields stru stru.idx names port write-style? marks-table next-mark-idx)
      ;;Tail recursive  function.  Write to  PORT the  fields from the  Vicare struct
      ;;STRU, starting  from field index STRU.IDX.   NAMES must be a  list of symbols
      ;;representing field names, with the first item  being the name of the field at
      ;;STRU.IDX.  Finish  the writing with  a close  bracket and return  the updated
      ;;mark index.
      ;;
      (if (pair? names)
	  (begin
	    (write-char #\space port)
	    (let ((next-mark-idx (write-object (car names) port write-style? marks-table next-mark-idx)))
	      (write-char #\= port)
	      (let ((next-mark-idx (write-object (struct-ref stru stru.idx) port write-style? marks-table next-mark-idx)))
		(%write-struct-fields stru (fxadd1 stru.idx) (cdr names) port write-style? marks-table next-mark-idx))))
	(begin
	  (write-char #\] port)
	  next-mark-idx)))

    (module (%write-r6rs-record)

      (define (%write-r6rs-record record port write-style? marks-table next-mark-idx)
	(define rtd (record-rtd record))
	(write-char* (if (record-type-opaque? rtd)
			 "#[opaque-record "
		       "#[record ")
		     port)
	(write-char* (symbol->string (record-type-name rtd))
		     port)
	(receive (next-mark-idx record.idx)
	    (let upper-rtd ((rtd rtd))
	      (cond ((record-type-parent rtd)
		     => (lambda (prtd)
			  (receive (next-mark-idx record.idx)
			      (upper-rtd prtd)
			    (%print-record-fields prtd record.idx record port write-style? marks-table next-mark-idx))))
		    (else
		     (values next-mark-idx 0))))
	  (%print-record-fields rtd record.idx record port write-style? marks-table next-mark-idx)
	  (write-char #\] port)
	  next-mark-idx))

      (define (%print-record-fields rtd next-record.idx record port write-style? marks-table next-mark-idx)
	(let* ((vec      (record-type-field-names rtd))
	       (vec.len  (vector-length vec)))
	  (do ((vec.idx    0               (fxadd1 vec.idx))
	       (record.idx next-record.idx (fxadd1 record.idx)))
	      ((fx=? vec.idx vec.len)
	       (values next-mark-idx record.idx))
	    (let* ((field-nam  (vector-ref vec vec.idx))
		   (field-val  (struct-ref record record.idx)))
	      (write-char #\space port)
	      (let ((next-mark-idx (write-object field-nam port write-style? marks-table next-mark-idx)))
		(write-char #\= port)
		(set! next-mark-idx (write-object field-val port write-style? marks-table next-mark-idx)))))))

      #| end of module: WRITE-R6RS-RECORD |# )

    #| end of module: WRITE-OBJECT-STRUCT |# )

;;; --------------------------------------------------------------------

  (define (write-object-procedure x p)
    (write-char* "#<procedure" p)
    (let-values (((name src)
                  (let ((ae (procedure-annotation x)))
                    (if (pair? ae)
                        (values (car ae) (cdr ae))
		      (values ae #f)))))
      (when name
        (write-char* " " p)
        (display name p))
      (when (pair? src)
        (let ((file (car src)) (char (cdr src)))
          (write-char* " (char " p)
          (display char p)
          (write-char* " of " p)
          (display file p)
          (write-char* ")" p))))
    (write-char* ">" p))

;;; --------------------------------------------------------------------

  (define (write-object-port x p marks-table next-mark-idx)
    (write-char* "#<" p)
    (write-char* (cond ((input/output-port? x)	"input/output")
		       ((input-port? x)		"input")
		       (else			"output"))
		 p)
    (write-char* "-port " p)
    (write-char* (if (binary-port? x) "(binary) " "(textual) ") p)
    (write-char* (port-id x) p)
    (write-char #\> p)
    next-mark-idx)

;;; --------------------------------------------------------------------

  (define (write-object-code x port write-style? marks-table next-mark-idx)
    (cond (($code-annotation x)
	   => (lambda (ann)
		(write-char* "#<code annotation=" port)
		(begin0
		    (write-object ann port write-style? marks-table next-mark-idx)
		  (write-char #\> port))))
	  (else
	   (write-char* "#<code>" port)
	   next-mark-idx)))

;;; --------------------------------------------------------------------
;;; helpers

  (define (write-char* str port)
    ;;Write the characters from the string STR in the textual output port PORT.
    ;;
    (let loop ((i  0)
	       (n  (string-length str)))
      (unless (fx=? i n)
        (write-char (string-ref str i) port)
        (loop (fxadd1 i) n))))

  (define (write-hex x n p)
    (define-constant s "0123456789ABCDEF")
    (unless (zero? n)
      (write-hex (sra x 4) (- n 1) p)
      (write-char (string-ref s (bitwise-and x #xF)) p)))

  #| end of module |# )


;;;; done

;;(foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.writer end"))

#| end of library |# )

;;; end of file
