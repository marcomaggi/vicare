;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus writer)
  (export
    write		display
    format
    printf		fprintf
    print-error
    print-unicode	print-graph
    put-datum

    ;;The following are not in "makefile.sps".
    traverse		traversal-helpers)
  (import (except (ikarus)
		  write		display
		  format
		  printf	fprintf
		  print-error
		  print-unicode	print-graph
		  put-datum)
    (only (ikarus system $symbols)
	  $unbound-object?)
    (only (ikarus.pretty-formats)
	  get-fmt)
    (only (ikarus records procedural)
	  print-r6rs-record-instance))


(define print-unicode
  (make-parameter #f))

(module traversal-helpers
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
  (define-struct cache (string object next))
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


(import traversal-helpers)

(define (cannot-happen)
  (error 'vicare-writer "*** Vicare: internal error"))

(define (traverse x h)
  (define (traverse-pair x h)
    (traverse (car x) h)
    (traverse (cdr x) h))
  (define (traverse-vector x h)
    (let f ((i 0) (n (vector-length x)))
      (unless (fx=? i n)
        (traverse (vector-ref x i) h)
        (f (fx+ i 1) n))))
  (define (traverse-noop x h) (void))
  (define (traverse-struct x h)
    (define (traverse-vanilla-struct x h)
      (let ((rtd (struct-type-descriptor x)))
        (unless
	    (and (record-type-descriptor? rtd)
		 (record-type-opaque? rtd))
          (traverse (struct-name x) h)
          (let ((n (struct-length x)))
            (let f ((idx 0))
              (unless (fx= idx n)
                (traverse (struct-ref x idx) h)
                (f (fxadd1 idx))))))))
    (define (traverse-custom-struct x h printer)
      (let-values (((p e) (open-string-output-port)))
        (let ((cache #f))
          (printer x p
		   (lambda (v)
		     (let ((str (e)))
		       (set! cache (make-cache str v cache))
		       (traverse v h))))
          (let ((cache (cons (e) cache))
                (b (hashtable-ref h x #f)))
            (if (fixnum? b)
                (hashtable-set! h x (cons b cache))
	      (cannot-happen))))))
    (let ((printer (struct-printer x)))
      (if (procedure? printer)
          (traverse-custom-struct x h printer)
	(traverse-vanilla-struct x h))))
  (define (traverse-shared x h k)
    (cond
     ((hashtable-ref h x #f) =>
      (lambda (b)
	(cond
	 ((fixnum? b)
	  (hashtable-set! h x (fxior b shared-bit)))
	 (else
	  (set-car! b (fxior (car b) shared-bit))))))
     (else
      (hashtable-set! h x 0)
      (k x h)
      (let ((b (hashtable-ref h x #f)))
	(cond
	 ((fixnum? b)
	  (when (shared-set? b)
	    (hashtable-set! h x (fxior b cyclic-bit))))
	 (else
	  (let ((a (car b)))
	    (when (shared-set? a)
	      (set-car! b (fxior a cyclic-bit))))))))))
  (define (traverse x h)
    (cond
     ((pair? x)       (traverse-shared x h traverse-pair))
     ((vector? x)     (traverse-shared x h traverse-vector))
     ((struct? x)     (traverse-shared x h traverse-struct))
     ((bytevector? x) (traverse-shared x h traverse-noop))
     ((gensym? x)     (traverse-shared x h traverse-noop))
     (else (void))))
  (traverse x h))


(define (wr x p m h i)
  (define (write-fixnum x p)
    (define loop
      (lambda (x p)
        (unless (fxzero? x)
          (loop (fxquotient x 10) p)
          (write-char
	   (integer->char
	    (fx+ (fxremainder x 10)
		 (char->integer #\0)))
	   p))))
    (cond
     ((fxzero? x) (write-char #\0 p))
     ((fx< x 0)
      (write-char* (fixnum->string x) p))
     (else (loop x p))))
  (define (write-pair x p m h i)
    (define (macro x h)
      (and
       (pair? x)
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
      (cond
       ((null? d) i)
       ((not (pair? d))
	(write-char #\space p)
	(write-char #\. p)
	(write-char #\space p)
	(wr d p m h i))
       ((shared? d h)
	(write-char #\space p)
	(when (print-graph)
	  (write-char #\. p)
	  (write-char #\space p))
	(wr d p m h i))
       (else
	(write-char #\space p)
	(let ((i (wr (car d) p m h i)))
	  (f (cdr d) i)))))
    (cond
     ((macro x h) =>
      (lambda (a)
	(write-string a p #f)
	(wr (cadr x) p m h i)))
     (else
      (write-char #\( p)
      (let ((i (f (cdr x) (wr (car x) p m h i))))
	(write-char #\) p)
	i))))
  (define (write-vector x p m h i)
    (define (f x p m h i idx n)
      (cond
       ((fx= idx n) i)
       (else
	(write-char #\space p)
	(let ((i (wr (vector-ref x idx) p m h i)))
	  (f x p m h i (fx+ idx 1) n)))))
    (write-char #\# p)
    (let ((n (vector-length x)))
      (cond
       ((fx=? n 0)
	(write-char #\( p)
	(write-char #\) p)
	i)
       (else
	(write-char #\( p)
	(let ((i (wr (vector-ref x 0) p m h i)))
	  (f x p m h i 1 n)
	  (write-char #\) p)
	  i)))))
  (define (write-bytevector x p m h i)
    (write-char #\# p)
    (write-char #\v p)
    (write-char #\u p)
    (write-char #\8 p)
    (write-char #\( p)
    (let ((n (bytevector-length x)))
      (when (fx> n 0)
        (write-fixnum (bytevector-u8-ref x 0) p)
        (let f ((idx 1) (n n) (x x) (p p))
          (unless (fx= idx n)
            (write-char #\space p)
            (write-fixnum (bytevector-u8-ref x idx) p)
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
  (define (write-character x p m)
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
           ((and (print-unicode) (unicode-printable-char? x))
            (write-char #\\ p)
            (write-char x p))
           (else
            (write-char #\\ p)
            (write-char #\x p)
            (write-positive-hex-fx i p))))
      (write-char x p)))
  (define (write-string x p m)
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
	     ((or (fx= byte 127)	;this is the #\delete char
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
  (module (write-gensym write-symbol)
    (define (write-gensym x p m h i)
      (cond
       ((and m (print-gensym)) =>
	(lambda (gensym-how)
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
	(write-symbol x p m)
	i)))
    (define write-symbol-bar-esc
      (lambda (x p)
        (define write-symbol-bar-esc-loop
          (lambda (x i n p)
            (unless (fx= i n)
              (let* ((c (string-ref x i))
                     (b (char->integer c)))
                (cond
		 ((fx< b 32)
		  (cond
		   ((fx< b 7)
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
              (write-symbol-bar-esc-loop x (fxadd1 i) n p))))
        (write-char #\| p)
        (write-symbol-bar-esc-loop x 0 (string-length x) p)
        (write-char #\| p)))
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
    (define (write-symbol x p m)
      (write-symbol-string (symbol->string x) p m)))
  (define (write-struct x p m h i)
    (define (write-vanilla-struct x p m h i)
      (cond
       ((let ((rtd (struct-type-descriptor x)))
	  (and (record-type-descriptor? rtd)
	       (record-type-opaque? rtd)))
	(write-char* "#<unknown>" p)
	i)
       (else
	(cond ((record-type-descriptor? (struct-type-descriptor x))
	       (print-r6rs-record-instance x p)
	       i)
	      ((keyword? x)
	       (write-char #\# p)
	       (write-char #\: p)
	       (wr (struct-ref x 0) p m h i))
	      (else ;it is a Vicare's struct
	       (write-char #\# p)
	       (write-char #\[ p)
	       (let ((i (wr (struct-name x) p m h i)))
		 (let ((n (struct-length x)))
		   (let f ((idx 0) (i i))
		     (cond
		      ((fx= idx n)
		       (write-char #\] p)
		       i)
		      (else
		       (write-char #\space p)
		       (f (fxadd1 idx)
			  (wr (struct-ref x idx) p m h i)))))))))
	)))
    (define (write-custom-struct out p m h i)
      (let ((i
             (let f ((cache (cdr out)))
               (cond
		((not cache) i)
		(else
		 (let ((i (f (cache-next cache))))
		   (write-char* (cache-string cache) p)
		   (wr (cache-object cache) p m h i)))))))
        (write-char* (car out) p)
        i))
    (let ((b (hashtable-ref h x #f)))
      (cond
       ((pair? b)
	(write-custom-struct (cdr b) p m h i))
       (else (write-vanilla-struct x p m h i)))))
  (define (write-char* x p)
    (let f ((x x) (p p) (i 0) (n (string-length x)))
      (unless (fx=? i n)
        (write-char (string-ref x i) p)
        (f x p (fx+ i 1) n))))
  (define (write-procedure x p)
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
  (define (write-port x p)
    (write-char* "#<" p)
    (write-char* (cond ((input/output-port? x)	"input/output")
		       ((input-port? x)		"input")
		       (else			"output"))
		 p)
    (write-char* "-port " p)
    (write-char* (if (binary-port? x) "(binary) " "(textual) ") p)
    (let ((i (wr (port-id x) p #t h i)))
      (write-char #\> p)
      i))
  (define (write-hex x n p)
    (define s "0123456789ABCDEF")
    (unless (zero? n)
      (write-hex (sra x 4) (- n 1) p)
      (write-char (string-ref s (bitwise-and x #xF)) p)))
  (define (write-shared x p m h i k)
    (let ((b (hashtable-ref h x #f)))
      (let ((b (if (fixnum? b) b (car b))))
        (cond
	 ((mark-set? b)
	  (write-char #\# p)
	  (write-fixnum (fxsra b mark-shift) p)
	  (write-char #\# p)
	  i)
	 ((or (cyclic-set? b)
	      (and (shared-set? b) (print-graph)))
	  (let ((n i))
	    (set-mark! x h n)
	    (write-char #\# p)
	    (write-fixnum n p)
	    (write-char #\= p)
	    (k x p m h (+ i 1))))
	 (else
	  (k x p m h i))))))
  (define (wr x p m h i)
    (cond
     ((pair? x)   (write-shared x p m h i write-pair))
     ((symbol? x)
      (if (gensym? x)
	  (write-shared x p m h i write-gensym)
	(begin (write-symbol x p m) i)))
     ((fixnum? x) (write-fixnum x p) i)
     ((string? x) (write-string x p m) i)
     ((boolean? x)
      (write-char #\# p)
      (write-char (if x #\t #\f) p)
      i)
     ((char? x) (write-character x p m) i)
     ((null? x) (write-char #\( p) (write-char #\) p) i)
     ((number? x) (write-char* (number->string x) p) i)
     ((vector? x) (write-shared x p m h i write-vector))
     ((bytevector? x) (write-shared x p m h i write-bytevector))
     ((procedure? x) (write-procedure x p) i)
     ((port? x) (write-port x p) i)
     ((eq? x (void)) (write-char* "#<void>" p) i)
     ((eof-object? x) (write-char* "#!eof" p) i)
     ((bwp-object? x) (write-char* "#!bwp" p) i)
     ((transcoder? x) (write-char* "#<transcoder>" p) i)
     ((struct? x) (write-shared x p m h i write-struct))
     ((code? x) (write-char* "#<code>" p) i)
     ((pointer? x)
      (write-char* "#<pointer #x" p)
      (write-hex (pointer->integer x)
		 (if (<= (fixnum-width) 32) 8 16)
		 p)
      (write-char* ">" p))
     (($unbound-object? x) (write-char* "#<unbound-object>" p) i)
     (else (write-char* "#<unknown>" p) i)))
  (wr x p m h i))


(define print-graph (make-parameter #f))

(define (write-to-port x p)
  (let ((h (make-eq-hashtable)))
    (traverse x h)
    (wr x p #t h 0)
    (void)))

(define (display-to-port x p)
  (let ((h (make-eq-hashtable)))
    (traverse x h)
    (wr x p #f h 0)
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
		(display-to-port (car args) p)
		(f (fxadd1 i) (cdr args)))
	       ((eqv? c #\s)
		(write-to-port (car args) p)
		(f (fxadd1 i) (cdr args)))
	       ((assv c '((#\b . 2) (#\o . 8) (#\x . 16) (#\d . 10)))
		=>
		(lambda (x)
		  (let ((a (car args)))
		    (display-to-port (number->string a (cdr x)) p))
		  (f (fxadd1 i) (cdr args))))
	       (else (die who "BUG" c))))))
	 (else
	  (write-char c p)
	  (f (fxadd1 i) args)))))))

(define (fprintf p fmt . args)
  (assert-open-textual-output-port p 'fprintf)
  (unless (string? fmt)
    (die 'fprintf "not a string" fmt))
  (formatter 'fprintf p fmt args)
  (void))

(define (display-error errname who fmt args)
  (unless (string? fmt)
    (die 'print-error "not a string" fmt))
  (let ((p (standard-error-port)))
    (if who
	(fprintf p "~a in ~a: " errname who)
      (fprintf p "~a: " errname))
    (formatter 'print-error p fmt args)
    (write-char #\. p)
    (newline p)
    (void)))

(define (format fmt . args)
  (unless (string? fmt)
    (die 'format "not a string" fmt))
  (let-values (((p e) (open-string-output-port)))
    (formatter 'format p fmt args)
    (e)))

(define (printf fmt . args)
  (unless (string? fmt)
    (die 'printf "not a string" fmt))
  (formatter 'printf (current-output-port) fmt args)
  (void))

(define write
  (case-lambda
   ((x)
    (write-to-port x (current-output-port))
    (void))
   ((x p)
    (assert-open-textual-output-port p 'write)
    (write-to-port x p)
    (void))))

(define (put-datum p x)
  (assert-open-textual-output-port p 'put-datum)
  (write-to-port x p)
  (void))

(define display
  (case-lambda
   ((x)
    (display-to-port x (current-output-port))
    (void))
   ((x p)
    (assert-open-textual-output-port p 'display)
    (display-to-port x p)
    (void))))

(define (print-error who fmt . args)
  (display-error "Error" who fmt args)
  (void))

(define (assert-open-textual-output-port p who)
  (unless (output-port? p)
    (die who "not an output port" p))
  (unless (textual-port? p)
    (die who "not a textual port" p))
  (when (port-closed? p)
    (die who "port is closed" p)))


;;;; done

)

;;; end of file
