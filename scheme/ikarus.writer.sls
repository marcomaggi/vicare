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
    debug-print
    print-unicode		print-graph
    printer-integer-radix

    ;;These are needed by PRETTY-PRINT.
    traverse			TRAVERSAL-HELPERS)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  write			display
		  put-datum		format
		  printf		fprintf
		  debug-print
		  print-unicode		print-graph
		  printer-integer-radix)
    (only (vicare system $symbols)
	  $unbound-object?)
    (only (vicare system $structs)
	  $struct-rtd)
    (only (vicare system $codes)
	  $code-annotation)
    (only (ikarus.pretty-formats)
	  get-fmt)
    (only (ikarus records procedural)
	  print-r6rs-record-instance))

  (include "ikarus.wordsize.scm" #t)


(module TRAVERSAL-HELPERS
  (cyclic-set? shared-set? mark-set? set-mark! set-shared! shared?
               shared-bit cyclic-bit marked-bit mark-shift
               make-cache cache-string cache-object cache-next)
;;; association list in hash table is one of the following forms:
;;;
;;; a fixnum:
  (define cyclic-bit         #b001)
  (define shared-bit         #b010)
  (define marked-bit         #b100)
  (define mark-shift         3)
;;;
;;; or a pair of a fixnum (above) and a cache:
  (define-struct cache
    (string object next))
  (define (cyclic-set? b)
    (fx= (fxand b cyclic-bit) cyclic-bit))
  (define (shared-set? b)
    (fx= (fxand b shared-bit) shared-bit))
  (define (mark-set? b)
    (fx= (fxand b marked-bit) marked-bit))

  (define (set-mark! x h n)
    (let ((b (hashtable-ref h x #f)))
      (cond
       ((fixnum? b)
	(hashtable-set! h x
			(fxior (fxsll n mark-shift) marked-bit b)))
       (else
	(set-car! b
		  (fxior (fxsll n mark-shift) marked-bit (car b)))))))

  (define (set-shared! x h)
    (let ((b (hashtable-ref h x #f)))
      (cond
       ((fixnum? b)
	(hashtable-set! h x (fxior shared-bit b)))
       (else
	(set-car! b (fxior shared-bit (car b)))))))

  (define (shared? x h)
    (cond
     ((hashtable-ref h x #f) =>
      (lambda (b)
	(if (fixnum? b)
	    (shared-set? b)
	  (let ((b (car b)))
	    (shared-set? b)))))
     (else #f)))

  #| end of module |#)


(module (traverse)
  (import TRAVERSAL-HELPERS)

  (define (traverse x h)
    ;;Fill  the hashtable  H with  an entry  for every  sub-object of  X; later  such
    ;;entries will be used to correctly print shared structures.
    ;;
    (cond ((pair?       x)	(traverse-shared x h traverse-pair))
	  ((vector?     x)	(traverse-shared x h traverse-vector))
	  ((struct?     x)	(traverse-shared x h traverse-struct))
	  ((bytevector? x)	(traverse-shared x h traverse-noop))
	  ((gensym?     x)	(traverse-shared x h traverse-noop))
	  ((code?       x)	(traverse-shared x h traverse-code))
	  (else			(void))))

  (define (traverse-noop x h)
    (void))

  (define (traverse-pair x h)
    (traverse (car x) h)
    (traverse (cdr x) h))

  (define (traverse-code x h)
    (cond (($code-annotation x)
	   => (lambda (ann)
		(traverse ann h)))
	  (else
	   (void))))

  (define (traverse-vector x h)
    (let f ((i 0) (n (vector-length x)))
      (unless (fx=? i n)
        (traverse (vector-ref x i) h)
        (f (fx+ i 1) n))))

  (module (traverse-struct)
    ;;This module processes: Vicare's structs; Vicare's struct-type descriptors; R6RS
    ;;records; R6RS record-type descriptors.
    ;;
    (define (traverse-struct x h)
      (let ((printer (struct-printer x)))
	(if (procedure? printer)
	    (%traverse-custom-struct x h printer)
	  (%traverse-vanilla-struct x h))))

    (define (%traverse-vanilla-struct x h)
      ;;Traverse a struct object that is meant to use the built-in printer function.
      ;;
      (let ((rtd ($struct-rtd x)))
	(unless (and (record-type-descriptor? rtd)
		     (record-type-opaque? rtd))
	  (traverse (struct-name x) h)
	  (let ((n (struct-length x)))
	    (let f ((idx 0))
	      (unless (fx= idx n)
		(traverse (struct-ref x idx) h)
		(f (fxadd1 idx))))))))

    (define (%traverse-custom-struct stru h printer)
      ;;Traverse a struct object with a custom printer function.
      ;;
      ;;The custom printer is  used to print the struct in a  string output port; the
      ;;resulting string is cached
      ;;
      (receive (port extract)
	  (open-string-output-port)
	(let* ((cache        #f)
	       (sub-printer  (lambda (sub-object)
			       (let ((str (extract)))
				 (set! cache (make-cache str sub-object cache))
				 (traverse sub-object h)))))
	  (printer stru port sub-printer)
	  (let ((cache (cons (extract) cache))
		(b (hashtable-ref h stru #f)))
	    (if (fixnum? b)
		(hashtable-set! h stru (cons b cache))
	      (%cannot-happen))))))

    #| end of module: TRAVERSE-STRUCT |# )

  (define (traverse-shared x h kont)
    (cond ((hashtable-ref h x #f)
	   => (lambda (b)
		(cond ((fixnum? b)
		       (hashtable-set! h x (fxior b shared-bit)))
		      (else
		       (set-car! b (fxior (car b) shared-bit))))))
	  (else
	   (hashtable-set! h x 0)
	   (kont x h)
	   (let ((b (hashtable-ref h x #f)))
	     (cond ((fixnum? b)
		    (when (shared-set? b)
		      (hashtable-set! h x (fxior b cyclic-bit))))
		   (else
		    (let ((a (car b)))
		      (when (shared-set? a)
			(set-car! b (fxior a cyclic-bit))))))))))

  (define (%cannot-happen)
    (error 'vicare-writer "vicare: internal error"))

  #| end of module |# )


(module (write-object)
  ;;All the function whose name starts  with "write-object-" are writers for specific
  ;;object types.
  ;;
  (import TRAVERSAL-HELPERS)

  (define (write-object x p m h i)
    (cond ((pair? x)
	   (write-shared x p m h i write-object-pair))

	  ((symbol? x)
	   (if (gensym? x)
	       (write-shared x p m h i write-object-gensym)
	     (begin
	       (write-object-symbol x p m)
	       i)))

	  ((fixnum? x)
	   (write-object-fixnum x p)
	   i)

	  ((string? x)
	   (write-object-string x p m)
	   i)

	  ((boolean? x)
	   (write-char #\# p)
	   (write-char (if x #\t #\f) p)
	   i)

	  ((char? x)
	   (write-object-character x p m)
	   i)

	  ((null? x)
	   (write-char #\( p)
	   (write-char #\) p)
	   i)

	  ((number? x)
	   (write-char* (if (or (fixnum? x)
				(bignum? x))
			    (number->string x (printer-integer-radix))
			  (number->string x))
			p)
	   i)

	  ((vector? x)
	   (write-shared x p m h i write-object-vector))

	  ((bytevector? x)
	   (write-shared x p m h i write-object-bytevector))

	  ((procedure? x)
	   (write-object-procedure x p)
	   i)

	  ((port? x)
	   (write-object-port x p h i)
	   i)

	  ((eq? x (void))
	   (write-char* "#!void" p)
	   i)

	  ((eof-object? x)
	   (write-char* "#!eof" p)
	   i)

	  ((bwp-object? x)
	   (write-char* "#!bwp" p)
	   i)

	  ((would-block-object? x)
	   (write-char* "#!would-block" p)
	   i)

	  ((transcoder? x)
	   (write-char* (string-append "#<transcoder"
				       " codec="		(symbol->string (transcoder-codec x))
				       " eol-style="		(symbol->string (transcoder-eol-style x))
				       " error-handling-mode="	(symbol->string (transcoder-error-handling-mode x))
				       ">")
			p)
	   i)

	  ((struct? x)
	   (write-shared x p m h i write-object-struct))

	  ((code? x)
	   (write-object-code x p m h i))

	  ((pointer? x)
	   (write-object-pointer x p)
	   i)

	  (($unbound-object? x)
	   (write-char* "#!unbound-object" p)
	   i)

	  (else
	   (write-char* "#<unknown>" p)
	   i)))

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

  (define (write-object-fixnum x p)
    (define (loop x p)
      (unless (fxzero? x)
	(loop (fxquotient x 10) p)
	(write-char (integer->char (fx+ (fxremainder x 10)
					(char->integer #\0)))
		    p)))
    (let ((radix (printer-integer-radix)))
      (if (fx=? 10 radix)
	  (cond ((fxzero? x)
		 (write-char #\0 p))
		((fx< x 0)
		 (write-char* (fixnum->string x) p))
		(else
		 (loop x p)))
	(begin
	  (write-char #\# p)
	  (case radix
	    ((2)	(write-char #\b p))
	    ((8)	(write-char #\o p))
	    ((16)	(write-char #\x p)))
	  (write-char* (number->string x radix) p)))))

;;; --------------------------------------------------------------------

  (define (write-object-pair x p m h i)
    (define (macro x h)
      (and (pair? x)
	   (let ((a (car x)))
	     (and (symbol? a)
		  (let ((d (cdr x)))
		    (and (pair? d)
			 (null? (cdr d))
			 (not (shared? d h))))
		  (let ((p ((pretty-format a))))
		    (and (pair? p)
			 (eq? (car p) 'read-macro)
			 (let ((d (cdr p)))
			   (and (string? d) d))))))))
    (define (f d i)
      (cond ((null? d)
	     i)
	    ((not (pair? d))
	     (write-char #\space p)
	     (write-char #\. p)
	     (write-char #\space p)
	     (write-object d p m h i))
	    ((shared? d h)
	     (write-char #\space p)
	     (when (print-graph)
	       (write-char #\. p)
	       (write-char #\space p))
	     (write-object d p m h i))
	    (else
	     (write-char #\space p)
	     (let ((i (write-object (car d) p m h i)))
	       (f (cdr d) i)))))
    (cond ((macro x h)
	   => (lambda (a)
		(write-object-string a p #f)
		(write-object (cadr x) p m h i)))
	  (else
	   (write-char #\( p)
	   (let ((i (f (cdr x) (write-object (car x) p m h i))))
	     (write-char #\) p)
	     i))))

;;; --------------------------------------------------------------------

  (define (write-object-vector x p m h i)
    (define (f x p m h i idx n)
      (cond
       ((fx= idx n) i)
       (else
	(write-char #\space p)
	(let ((i (write-object (vector-ref x idx) p m h i)))
	  (f x p m h i (fx+ idx 1) n)))))
    (write-char #\# p)
    (let ((n (vector-length x)))
      (cond ((fxzero? n)
	     (write-char #\( p)
	     (write-char #\) p)
	     i)
	    (else
	     (write-char #\( p)
	     (let ((i (write-object (vector-ref x 0) p m h i)))
	       (f x p m h i 1 n)
	       (write-char #\) p)
	       i)))))

;;; --------------------------------------------------------------------

  (define (write-object-bytevector x p m h i)
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
    i)

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

  (define (write-object-character x p m)
    (define char-table ; first nonprintable chars
      '#("nul" "x1" "x2" "x3" "x4" "x5" "x6" "alarm"
         "backspace" "tab" "linefeed" "vtab" "page" "return" "xE" "xF"
         "x10" "x11" "x12" "x13" "x14" "x15" "x16" "x17"
         "x18" "x19" "x1A" "esc" "x1C" "x1D" "x1E" "x1F"
         "space"))
    (if m
        (let ((i (char->integer x)))
          (write-char #\# p)
          (cond
           ((fx< i (vector-length char-table))
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

  (define (write-object-string x p m)
    (define (write-string-escape x p)
;;; commonize with write-symbol-bar-escape
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
    (if m
        (write-string-escape x p)
      (write-char* x p)))

;;; --------------------------------------------------------------------

  (module (write-object-gensym write-object-symbol)

    (define (write-object-symbol x p m)
      (write-symbol-string (symbol->string x) p m))

    (define (write-object-gensym x p m h i)
      (cond ((and m (print-gensym))
	     =>	(lambda (gensym-how)
		  (case gensym-how
		    ((pretty)
		     (let ((str (symbol->string x)))
		       (write-char #\# p)
		       (write-char #\: p)
		       (write-symbol-string str p m)))
		    (else
		     (let ((str (symbol->string x))
			   (ustr (gensym->unique-string x)))
		       (write-char #\# p)
		       (write-char #\{ p)
		       (write-symbol-string str p m)
		       (write-char #\space p)
		       (write-symbol-bar-esc ustr p)
		       (write-char #\} p))))
		  i))
	    (else
	     (write-object-symbol x p m)
	     i)))

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

    (define (write-symbol-string str p m)
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
      (if m
	  (if (peculiar-symbol-string? str)
	      (write-peculiar str p)
	    (write-symbol-hex-esc str p))
	(write-char* str p)))

    #| end of module: WRITE-OBJECT-SYMBOL, WRITE-OBJECT-GENSYM |# )

;;; --------------------------------------------------------------------

  (module (write-object-struct)

    (define (write-object-struct x p m h i)
      (let ((b (hashtable-ref h x #f)))
	(cond ((pair? b)
	       (write-custom-struct (cdr b) p m h i))
	      (else
	       (write-vanilla-struct x p m h i)))))

    (define (write-custom-struct out p m h i)
      ;;Write a struct having a custom printer function.
      ;;
      (begin0
	  (let recur ((cache (cdr out)))
	    (if (not cache)
		i
	      (let ((i (recur (cache-next cache))))
		(write-char*  (cache-string cache) p)
		(write-object (cache-object cache) p m h i))))
	(write-char* (car out) p)))

    (define (write-vanilla-struct x p m h i)
      ;;Write a struct that is meant to use the built-in printer function.
      ;;
      (cond ((record-type-descriptor? ($struct-rtd x))
	     (%write-r6rs-record ($struct-rtd x) x p m h i))

	    ;;We do not handle opaque records specially.
	    ;; ((let ((rtd ($struct-rtd x)))
	    ;;    (and (record-type-descriptor? rtd)
	    ;; 	    (record-type-opaque? rtd)))
	    ;;  (write-char* "#<unknown>" p)
	    ;;  i)

	    ((keyword? x)
	     (write-char #\# p)
	     (write-char #\: p)
	     (write-object (struct-ref x 0) p m h i))

	    (else
	     (%write-vicare-struct x p m h i))))

    (define (%write-vicare-struct stru port m h i)
      (write-char #\# port)
      (write-char #\[ port)
      (let ((i        (write-object (struct-name stru) port m h i))
	    (stru.len (struct-length stru)))
	(let next-field ((stru.idx  0)
			 (i         i))
	  (cond ((fx=? stru.idx stru.len)
		 (write-char #\] port)
		 i)
		(else
		 (write-char #\space port)
		 (next-field (fxadd1 stru.idx)
			     (write-object (struct-ref stru stru.idx) port m h i)))))))

    (module (%write-r6rs-record)

      (define (%write-r6rs-record rtd record port m h i)
	(write-char* (if (record-type-opaque? rtd)
			 "#[opaque-r6rs-record: "
		       "#[r6rs-record: ")
		     port)
	(write-char* (symbol->string (record-type-name rtd))
		     port)
	(receive (i record.idx)
	    (let upper-rtd ((rtd rtd))
	      (cond ((record-type-parent rtd)
		     => (lambda (prtd)
			  (receive (i record.idx)
			      (upper-rtd prtd)
			    (%print-record-fields prtd record.idx record port m h i))))
		    (else
		     (values i 0))))
	  (%print-record-fields rtd record.idx record port m h i)
	  (write-char #\] port)
	  i))

      (define (%print-record-fields rtd next-record.idx record port m h i)
	(let* ((vec      (record-type-field-names rtd))
	       (vec.len  (vector-length vec)))
	  (do ((vec.idx    0               (fxadd1 vec.idx))
	       (record.idx next-record.idx (fxadd1 record.idx)))
	      ((fx=? vec.idx vec.len)
	       (values i record.idx))
	    (let* ((field-nam  (vector-ref vec vec.idx))
		   (field-val  (struct-ref record record.idx)))
	      (write-char #\space port)
	      (let ((i (write-object field-nam port m h i)))
		(write-char #\= port)
		(set! i (write-object field-val port m h i)))))))

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

  (define (write-object-port x p h i)
    (write-char* "#<" p)
    (write-char* (cond ((input/output-port? x)	"input/output")
		       ((input-port? x)		"input")
		       (else			"output"))
		 p)
    (write-char* "-port " p)
    (write-char* (if (binary-port? x) "(binary) " "(textual) ") p)
    (let ((i (write-object (port-id x) p #t h i)))
      (write-char #\> p)
      i))

;;; --------------------------------------------------------------------

  (define (write-object-code x port m h i)
    (cond (($code-annotation x)
	   => (lambda (ann)
		(write-char* "#<code annotation=" port)
		(begin0
		    (write-object ann port m h i)
		  (write-char #\> port))))
	  (else
	   (write-char* "#<code>" port)
	   i)))

;;; --------------------------------------------------------------------

  (define* (write-shared x port m h i writer-kont)
    ;;Takes care of printing shared structures  with "#n=" and "#n#" elements, rather
    ;;than  going  into  infinite  recursion.   For  this  function  to  work:  every
    ;;interesting sub-object of the object X must have been previously visited by the
    ;;function TRAVERSE, so that the hashtable H knows about it.
    ;;
    (let ((b (hashtable-ref h x #f)))
      (let ((b (cond ((fixnum? b)
		      b)
		     ((pair? b)
		      (car b))
		     (else
		      (assertion-violation __who__
			"sub-object has not been processed correctly to handle shared structure"
			b)))))
        (cond ((mark-set? b)
	       (write-char #\# port)
	       (write-object-fixnum (fxsra b mark-shift) port)
	       (write-char #\# port)
	       i)
	      ((or (cyclic-set? b)
		   (and (shared-set? b) (print-graph)))
	       (let ((n i))
		 (set-mark! x h n)
		 (write-char #\# port)
		 (write-object-fixnum n port)
		 (write-char #\= port)
		 (writer-kont x port m h (add1 i))))
	      (else
	       (writer-kont x port m h i))))))

;;; --------------------------------------------------------------------
;;; helpers

  (define (write-char* x p)
    (let f ((x x) (p p) (i 0) (n (string-length x)))
      (unless (fx=? i n)
        (write-char (string-ref x i) p)
        (f x p (fx+ i 1) n))))

  (define (write-hex x n p)
    (define s "0123456789ABCDEF")
    (unless (zero? n)
      (write-hex (sra x 4) (- n 1) p)
      (write-char (string-ref s (bitwise-and x #xF)) p)))

  #| end of module |# )


;;;; public API

(define print-graph
  (make-parameter #f))

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

(module (put-datum
	 write		display
	 printf		fprintf
	 format		debug-print)

  (define (%write-to-port x p)
    (let ((h (make-eq-hashtable)))
      (traverse x h)
      (write-object x p #t h 0)
      (void)))

  (define (%display-to-port x p)
    (let ((h (make-eq-hashtable)))
      (traverse x h)
      (write-object x p #f h 0)
      (void)))

  (define (formatter who p fmt args)
;;; first check
    (let f ((i 0) (args args))
      (cond
       ((fx= i (string-length fmt))
	(unless (null? args)
	  (die who
	       (format
		   "extra arguments given for format string \x2036;~a\x2033;"
		 fmt))))
       (else
	(let ((c (string-ref fmt i)))
	  (cond
	   ((eqv? c #\~)
	    (let ((i (fxadd1 i)))
	      (when (fx= i (string-length fmt))
		(die who "invalid ~ at end of format string" fmt))
	      (let ((c (string-ref fmt i)))
		(cond
		 ((memv c '(#\~ #\%)) (f (fxadd1 i) args))
		 ((memv c '(#\a #\s))
		  (when (null? args)
		    (die who "insufficient arguments"))
		  (f (fxadd1 i) (cdr args)))
		 ((memv c '(#\b #\o #\x #\d))
		  (when (null? args)
		    (die who "insufficient arguments"))
		  (let ((a (car args)))
		    (unless (number? a) (die who "not a number" a))
		    (unless (or (eqv? c #\d) (exact? a))
		      (die who
			   (format "inexact numbers cannot be \
                                     printed with ~~~a" c)
			   a)))
		  (f (fxadd1 i) (cdr args)))
		 (else
		  (die who "invalid sequence character after ~" c))))))
	   (else (f (fxadd1 i) args)))))))
;;; then format
    (let f ((i 0) (args args))
      (unless (fx= i (string-length fmt))
	(let ((c (string-ref fmt i)))
	  (cond
	   ((eqv? c #\~)
	    (let ((i (fxadd1 i)))
	      (let ((c (string-ref fmt i)))
		(cond
		 ((eqv? c #\~)
		  (write-char #\~ p)
		  (f (fxadd1 i) args))
		 ((eqv? c #\%)
		  (write-char #\newline p)
		  (f (fxadd1 i) args))
		 ((eqv? c #\a)
		  (%display-to-port (car args) p)
		  (f (fxadd1 i) (cdr args)))
		 ((eqv? c #\s)
		  (%write-to-port (car args) p)
		  (f (fxadd1 i) (cdr args)))
		 ((assv c '((#\b . 2) (#\o . 8) (#\x . 16) (#\d . 10)))
		  =>
		  (lambda (x)
		    (let ((a (car args)))
		      (%display-to-port (number->string a (cdr x)) p))
		    (f (fxadd1 i) (cdr args))))
		 (else (die who "BUG" c))))))
	   (else
	    (write-char c p)
	    (f (fxadd1 i) args)))))))

  (define* (fprintf p {fmt string?} . args)
    (assert-open-textual-output-port p __who__)
    (formatter __who__ p fmt args)
    (void))

  (define* (format {fmt string?} . args)
    (receive (port extract)
	(open-string-output-port)
      (formatter __who__ port fmt args)
      (extract)))

  (define* (printf {fmt string?} . args)
    (formatter __who__ (current-output-port) fmt args)
    (void))

  (case-define* write
    ((x)
     (%write-to-port x (current-output-port))
     (void))
    ((x p)
     (assert-open-textual-output-port p __who__)
     (%write-to-port x p)
     (void)))

  (define* (put-datum p x)
    (assert-open-textual-output-port p __who__)
    (%write-to-port x p)
    (void))

  (case-define* display
    ((x)
     (%display-to-port x (current-output-port))
     (void))
    ((x p)
     (assert-open-textual-output-port p __who__)
     (%display-to-port x p)
     (void)))

  (define (assert-open-textual-output-port p who)
    (unless (output-port? p)
      (error who "not an output port" p))
    (unless (textual-port? p)
      (error who "not a textual port" p))
    (when (port-closed? p)
      (error who "port is closed" p)))

  (define (debug-print . args)
    ;;Print arguments for debugging purposes.
    ;;
    (pretty-print args (current-error-port))
    (newline (current-error-port))
    (newline (current-error-port))
    (when (pair? args)
      (car args)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
