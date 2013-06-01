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


(library (ikarus fasl read)
  (export fasl-read)
  (import (except (ikarus)
		  fixnum-width
		  greatest-fixnum		least-fixnum
		  fasl-read)
    (except (ikarus.code-objects)
	    procedure-annotation)
    (only (vicare.foreign-libraries)
	  autoload-filename-foreign-library)
    (vicare unsafe operations)
    (except (vicare language-extensions syntaxes)
	    case-word-size)
    (vicare arguments validation))

  (include "ikarus.wordsize.scm")


;;;; main functions

(define who 'fasl-read)

(define (fasl-read port)
  ;;Read and  validate the  FASL header,  then load  the whole  file and
  ;;return the result.
  ;;
  (define (%assert x y)
    (unless (eq? x y)
      (assertion-violation who
	(format "while reading fasl header expected ~s, got ~s\n" y x))))
  (with-arguments-validation (who)
      ((input-port	port))
    (%assert (read-u8-as-char port) #\#)
    (%assert (read-u8-as-char port) #\@)
    (%assert (read-u8-as-char port) #\I)
    (%assert (read-u8-as-char port) #\K)
    (%assert (read-u8-as-char port) #\0)
    (case-word-size
     ((32)
      (%assert (read-u8-as-char port) #\1))
     ((64)
      (%assert (read-u8-as-char port) #\2)))
    (let ((v (%do-read port)))
      (if (port-eof? port)
	  v
	(assertion-violation who "port did not reach EOF at the end of fasl file")))))


(define (%do-read port)
  ;;Actually read a fasl file from the input PORT.
  ;;
  (define MARKS
    (make-vector 1 #f))

  (define-syntax MARKS.len
    (identifier-syntax ($vector-length MARKS)))

  (define (%put-mark m obj)
    ;;Mark object OBJ  with the fixnum M; that is: store  OBJ at index M
    ;;in the vector MARKS.  If  MARKS is not wide enough: reallocate it.
    ;;It is an error if the same mark is defined twice.
    ;;
    (if ($fx< m MARKS.len)
	;;FIXME For  reasons still  to be understood,  if we  remove the
	;;variable GOOD and  just put the expression in as  IF test: the
	;;result is  wrong.  This  happens only when  we use  the unsafe
	;;$VECTOR-REF   operation,  when   using  the   safe  VECTOR-REF
	;;everything  works.   It  may  be that  the  implementation  of
	;;$VECTOR-REF is partly wrong, or the compilers makes some error
	;;in generating the code.  (Marco Maggi; Oct 7, 2012)
	(let ((good ($vector-ref MARKS m)))
	  (if good
	      (assertion-violation who "mark set twice" m port)
	    ($vector-set! MARKS m obj)))
      (let* ((n MARKS.len)
	     (v (make-vector ($fxmax ($fx* n 2) ($fxadd1 m)) #f)))
	(let loop ((i 0))
	  (if ($fx= i n)
	      (begin
		(set! MARKS v)
		($vector-set! MARKS m obj))
	    (let ((m ($vector-ref MARKS i)))
	      ($vector-set! v i m)
	      (loop ($fxadd1 i))))))))

  (define (%read-without-mark)
    ;;Read and return the next object; it will have no mark.
    ;;
    ;;This procedure is used both to start reading an object and to read
    ;;subobjects:  objects that  are  components of  other objects;  for
    ;;example the car and cdr or a pair are subobjects.
    ;;
    ;;Such subobjects can  be marked, but not with the  same mark of the
    ;;superobject.
    ;;
    (%read/mark #f))

  (define (%read/mark m)
    ;;Read  and return the  next object.   Unless M  is false:  mark the
    ;;object with M.
    ;;
    (let ((ch (read-u8-as-char port)))
      (case ch
	((#\I) ;fixnum in host byte order
	 (read-fixnum port))
	((#\P) ;pair
	 (if m
	     (let ((x (cons #f #f)))
	       (%put-mark m x)
	       ($set-car! x (%read-without-mark))
	       ($set-cdr! x (%read-without-mark))
	       x)
	   (let ((a (%read-without-mark)))
	     (cons a (%read-without-mark)))))
	((#\N) '())
	((#\T) #t)
	((#\F) #f)
	((#\E) (eof-object))
	((#\U) (void))
	((#\s) ;ASCII string
	 (let* ((len (read-integer-word port))
		(str (make-string len)))
	   (let next-char ((i 0))
	     (unless ($fx= i len)
	       ($string-set! str i (read-u8-as-char port))
	       (next-char ($fxadd1 i))))
	   (when m (%put-mark m str))
	   str))
	((#\S) ;Unicode string
	 (let* ((len (read-integer-word port))
		(str (make-string len)))
	   (let next-char ((i 0))
	     (unless ($fx= i len)
	       ($string-set! str i (integer->char (read-u32 port)))
	       (next-char ($fxadd1 i))))
	   (when m (%put-mark m str))
	   str))
	((#\M) ;symbol
	 (let ((sym (string->symbol (%read-without-mark))))
	   (when m (%put-mark m sym))
	   sym))
	((#\G) ;generated symbol
	 (let* ((pretty (%read-without-mark))
		(unique (%read-without-mark))
		(g      (foreign-call "ikrt_strings_to_gensym" pretty unique)))
	   (when m (%put-mark m g))
	   g))
	((#\V) ;vector
	 (let* ((len (read-integer-word port))
		(vec (make-vector len)))
	   (when m (%put-mark m vec))
	   (let next-object ((i 0))
	     (unless ($fx= i len)
	       ($vector-set! vec i (%read-without-mark))
	       (next-object ($fxadd1 i))))
	   vec))
	((#\v) ;bytevector
	 (let* ((len (read-integer-word port))
		(bv  (make-bytevector len)))
	   (when m (%put-mark m bv))
	   (let next-octet ((i 0))
	     (unless ($fx= i len)
	       (bytevector-u8-set! bv i (read-u8 port))
	       (next-octet ($fxadd1 i))))
	   bv))
	((#\x) ;code
	 (%read-code m #f))
	((#\Q) ;procedure
	 (%read-procedure m))
	((#\R) ;struct type descriptor
	 (let* ((rtd-name	(%read-without-mark))
		(rtd-symbol	(%read-without-mark))
		(field-count	(read-integer-word port))
		(fields		(let recur ((i 0))
				  (if ($fx= i field-count)
				      '()
				    (let ((a (%read-without-mark)))
				      (cons a (recur ($fxadd1 i)))))))
		(rtd		(make-struct-type rtd-name fields rtd-symbol)))
	   (when m (%put-mark m rtd))
	   rtd))
	((#\{) ;struct instance
	 (let* ((field-count	(read-integer-word port))
		(rtd		(%read-without-mark))
		(struct		(make-struct rtd field-count)))
	   (when m (%put-mark m struct))
	   (let next-field ((i 0))
	     (unless ($fx= i field-count)
	       ($struct-set! struct i (%read-without-mark))
	       (next-field ($fxadd1 i))))
	   struct))
	((#\C) ;Unicode char
	 (integer->char (read-u32 port)))
	((#\c) ;char in the ASCII range
	 (read-u8-as-char port))
	((#\>) ;mark for the next object
	 (let ((m (read-u32 port)))
	   (%read/mark m)))
	((#\<) ;reference to a previously read mark
	 (let ((m (read-u32 port)))
	   (if ($fx< m MARKS.len)
	       (or ($vector-ref MARKS m)
		   (error who "uninitialized mark" m))
	     (assertion-violation who "invalid mark" m))))
	((#\l) ;list of length <= 255
	 (%read-list (read-u8 port) m))
	((#\L) ;list of length > 255
	 (%read-list (read-integer-word port) m))
	((#\W) ;R6RS record type descriptor
	 (let* ((name		(%read-without-mark))
		(parent		(%read-without-mark))
		(uid		(%read-without-mark))
		(sealed?	(%read-without-mark))
		(opaque?	(%read-without-mark))
		(field-count	(%read-without-mark))
		(fields		(make-vector field-count)))
	   (let next-field ((i 0))
	     (if ($fx= i field-count)
		 (let ((rtd (make-record-type-descriptor name parent uid
							 sealed? opaque? fields)))
		   (when m (%put-mark m rtd))
		   rtd)
	       (let* ((field-mutable? (%read-without-mark))
		      (field-name     (%read-without-mark)))
		 ($vector-set! fields i (list (if field-mutable? 'mutable 'immutable)
						    field-name))
		 (next-field ($fxadd1 i)))))))
	((#\b) ;bignum
	 (let* ((i	(read-integer-word port))
		(len	(if ($fx< i 0) ($fx- 0 i) i))
		(bv	(get-bytevector-n port len))
		(bignum	(bytevector-uint-ref bv 0 'little len))
		(bignum	(if ($fx< i 0) (- bignum) bignum)))
	   (when m (%put-mark m bignum))
	   bignum))
	((#\f) ;flonum
	 (let ((fl ($make-flonum)))
	   ($flonum-set! fl 7 (get-u8 port))
	   ($flonum-set! fl 6 (get-u8 port))
	   ($flonum-set! fl 5 (get-u8 port))
	   ($flonum-set! fl 4 (get-u8 port))
	   ($flonum-set! fl 3 (get-u8 port))
	   ($flonum-set! fl 2 (get-u8 port))
	   ($flonum-set! fl 1 (get-u8 port))
	   ($flonum-set! fl 0 (get-u8 port))
	   (when m (%put-mark m fl))
	   fl))
	((#\r) ;ratnum
	 (let* ((den (%read-without-mark))
		(num (%read-without-mark))
		(x   (/ num den)))
	   (when m (%put-mark m x))
	   x))
	((#\i) ;;; compnum
	 (let* ((real (%read-without-mark))
		(imag (%read-without-mark)))
	   (let ((x (make-rectangular real imag)))
	     (when m (%put-mark m x))
	     x)))
	((#\h #\H) ;;; EQ? or EQV? hashtable
	 (let ((x (if ($char= ch #\h)
		      (make-eq-hashtable)
		    (make-eqv-hashtable))))
	   (when m (%put-mark m x))
	   (let* ((keys (%read-without-mark))
		  (vals (%read-without-mark)))
	     (vector-for-each
		 (lambda (k v)
		   (hashtable-set! x k v))
	       keys vals))
	   x))
	((#\O) ;autoload foreign library
	 (autoload-filename-foreign-library (%read-without-mark))
	 ;;recurse to satisfy the request to return an object
	 (%read/mark m))
	(else
	 (assertion-violation who "unexpected char as fasl object header" ch port)))))

  (define (%read-code code-mark closure-mark)
    ;;Read and  return a  code object.  Unless  CODE-MARK is  false: the
    ;;code  object is  marked  with CODE-MARK.   Unless CLOSURE-MARK  is
    ;;false:  a  closure  is  built  with the  object  and  marked  with
    ;;CLOSURE-MARK.
    ;;
    ;;The format is:
    ;;
    ;; int	: exact integer representing code size in bytes
    ;; fixnum	: fixnum representing number of free variables
    ;; object	: code annotation
    ;; byte ...	: the actual code
    ;; vector	: code relocation vector
    ;;
    (let* ((code-size (read-integer-word port))
	   (freevars  (read-fixnum       port))
	   (code      (make-code code-size freevars)))
      (when code-mark (%put-mark code-mark code))
      (let ((annotation (%read-without-mark)))
	(set-code-annotation! code annotation))
      ;;Read the actual code one byte at a time.
      (let loop ((i 0))
	(unless ($fx= i code-size)
	  (code-set! code i (char->int (read-u8-as-char port)))
	  (loop ($fxadd1 i))))
      (if closure-mark
	  ;;First  build the  closure and  mark it,  then read  the code
	  ;;relocation vector.
	  (let ((closure ($code->closure code)))
	    (%put-mark closure-mark closure)
	    ;;Setting the code reloc vector also process it.
	    (set-code-reloc-vector! code (%read-without-mark))
	    code)
	(begin
	  (set-code-reloc-vector! code (%read-without-mark))
	  code))))

  (define (%read-procedure mark)
    ;;Read a procedure.
    ;;
    ;;FIXME Understand and document the expected format.
    ;;
    (let ((ch (read-u8-as-char port)))
      (case ch
	((#\x)
	 (let ((code (%read-code #f mark)))
	   (if mark
	       ($vector-ref MARKS mark)
	     ($code->closure code))))
	((#\<)
	 (let ((closure-mark (read-u32 port)))
	   (unless ($fx< closure-mark MARKS.len)
	     (assertion-violation who "invalid mark" mark))
	   (let* ((code ($vector-ref MARKS closure-mark))
		  (proc ($code->closure code)))
	     (when mark (%put-mark mark proc))
	     proc)))
	((#\>)
	 (let ((closure-mark (read-u32 port))
	       (ch           (read-u8-as-char port)))
	   (unless ($char= ch #\x)
	     (assertion-violation who "expected char \"x\"" ch))
	   (let ((code (%read-code closure-mark mark)))
	     (if mark
		 ($vector-ref MARKS mark)
	       ($code->closure code)))))
	(else
	 (assertion-violation who "invalid code header" ch)))))

  (define (%read-list len mark)
    ;;Read and  return a list  of LEN  elements.  Unless MARK  is false:
    ;;mark the first pair of the list with MARK.
    ;;
    (let ((ls (make-list (+ 1 len))))
      (when mark (%put-mark mark ls))
      (let loop ((ls ls))
	($set-car! ls (%read-without-mark))
	(let ((d ($cdr ls)))
	  (if (null? d)
	      ($set-cdr! ls (%read-without-mark))
	    (loop d))))
      ls))

  (%read-without-mark))


;;;; utilities

(define (make-struct rtd n)
  ;;Build  and return  a new  struct  object of  type RTD  and having  N
  ;;fields.  Initialise all the fields to the fixnum 0.
  ;;
  (let loop ((i 0) (n n) (s ($make-struct rtd n)))
    (if ($fx= i n)
	s
      (begin
	($struct-set! s i 0)
	(loop ($fxadd1 i) n s)))))

(define (read-u8 port)
  (let ((byte (get-u8 port)))
    (if (eof-object? byte)
	(error who "invalid eof encountered" port)
      byte)))

(define (read-u8-as-char port)
  (integer->char (read-u8 port)))

(define (char->int x)
  (if (char? x)
      (char->integer x)
    (assertion-violation who "unexpected EOF inside a fasl object")))

(define (read-u32 port)
  ;;Read from  the input PORT 4  bytes representing an  exact integer in
  ;;big-endian format:
  ;;
  ;;   byte3 byte2 byte1 byte0
  ;;  |-----|-----|-----|-----| 32-bit
  ;;     ^                 ^
  ;; most significant      |
  ;;             least significant
  ;;
  (let* ((c0 (read-u8 port))
	 (c1 (read-u8 port))
	 (c2 (read-u8 port))
	 (c3 (read-u8 port)))
    (bitwise-ior c0 (sll c1 8) (sll c2 16) (sll c3 24))))

(define (read-fixnum port)
  ;;Read from  the input PORT a  fixnum represented as  32-bit or 64-bit
  ;;value depending on the underlying platform's word size.
  ;;
  (case-word-size
    ((32)
     (let* ((c0 (read-u8 port))
	    (c1 (read-u8 port))
	    (c2 (read-u8 port))
	    (c3 (read-u8 port)))
       (cond
	((fx<= c3 127)
	 (fxlogor (fxlogor (fxsra c0 2) (fxsll c1 6))
		  (fxlogor (fxsll c2 14) (fxsll c3 22))))
	(else
	 (let ((c0 (fxlogand #xFF (fxlognot c0)))
	       (c1 (fxlogand #xFF (fxlognot c1)))
	       (c2 (fxlogand #xFF (fxlognot c2)))
	       (c3 (fxlogand #xFF (fxlognot c3))))
	   (fx- -1
		(fxlogor (fxlogor (fxsra c0 2)
				  (fxsll c1 6))
			 (fxlogor (fxsll c2 14)
				  (fxsll c3 22)))))))))
    ((64)
     (let* ((u0 (read-u32 port))
	    (u1 (read-u32 port)))
       (if (<= u1 #x7FFFFFF)
	   (sra (bitwise-ior u0 (sll u1 32)) 3)
	 (let ((u0 (fxlogand #xFFFFFFFF (fxlognot u0)))
	       (u1 (fxlogand #xFFFFFFFF (fxlognot u1))))
	   (fx- -1
		(fxlogor (fxsra u0 3) (fxsll u1 29)))))))))

(define (read-integer-word port)
  ;;Read from the  input PORT an exact integer  represented as 32-bit or
  ;;64-bit value depending on the underlying platform's word size.
  ;;
  (case-word-size
    ((32)	;32-bit platform
     (let* ((c0 (char->int (read-u8-as-char port)))
	    (c1 (char->int (read-u8-as-char port)))
	    (c2 (char->int (read-u8-as-char port)))
	    (c3 (char->int (read-u8-as-char port))))
       (cond
	((fx<= c3 127)
	 (fxlogor (fxlogor c0 (fxsll c1 8))
		  (fxlogor (fxsll c2 16) (fxsll c3 24))))
	(else
	 (let ((c0 (fxlogand #xFF (fxlognot c0)))
	       (c1 (fxlogand #xFF (fxlognot c1)))
	       (c2 (fxlogand #xFF (fxlognot c2)))
	       (c3 (fxlogand #xFF (fxlognot c3))))
	   (fx- -1
		(fxlogor (fxlogor c0
				  (fxsll c1 8))
			 (fxlogor (fxsll c2 16)
				  (fxsll c3 24)))))))))
    ((64)	;64-bit platform
     (let* ((u0 (read-u32 port))
	    (u1 (read-u32 port)))
       (if (<= u1 #x7FFFFFF)
	   (bitwise-ior u0 (sll u1 32))
	 (let ((u0 (fxlogand #xFFFFFFFF (fxlognot u0)))
	       (u1 (fxlogand #xFFFFFFFF (fxlognot u1))))
	   (- -1 (bitwise-ior u0 (sll u1 32)))))))))


;;;;done

)

;;; end of file
