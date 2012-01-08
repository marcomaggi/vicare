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
		  fasl-read)
    (except (ikarus.code-objects)
	    procedure-annotation)
    (ikarus system $codes)
    (ikarus system $structs)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (vicare syntactic-extensions))


;;;; main functions

(define who 'fasl-read)

(define (fasl-read port)
  (if (input-port? port)
      ($fasl-read port)
    (assertion-violation who "expected input port as argument" port)))

(define ($fasl-read port)
  ;;Read  and validate the  FASL header,  then load  the whole  file and
  ;;return the result.
  ;;
  (define (%assert x y)
    (unless (eq? x y)
      (assertion-violation who
	(format "while reading fasl header expected ~s, got ~s\n" y x))))
  (%assert (read-u8-as-char port) #\#)
  (%assert (read-u8-as-char port) #\@)
  (%assert (read-u8-as-char port) #\I)
  (%assert (read-u8-as-char port) #\K)
  (%assert (read-u8-as-char port) #\0)
  (case (fixnum-width)
    ((30)
     (%assert (read-u8-as-char port) #\1))
    (else
     (%assert (read-u8-as-char port) #\2)))
  (let ((v (%do-read port)))
    (if (port-eof? port)
	v
      (assertion-violation who "port did not reach EOF at the end of fasl file"))))


(define (%do-read port)
  ;;Actually read a fasl file from the input PORT.
  ;;
  (define-inline (fxmax x y)
    (if (unsafe.fx> x y) x y))

  (define MARKS
    (make-vector 1 #f))
  (define-syntax MARKS.len
    (identifier-syntax (vector-length MARKS)))
  (define (put-mark m obj)
    ;;Mark object OBJ  with the fixnum M; that is: store  OBJ at index M
    ;;in the vector MARKS.  If  MARKS is not wide enough: reallocate it.
    ;;It is an error if the same mark is defined twice.
    ;;
    (if (unsafe.fx< m MARKS.len)
	(if (vector-ref MARKS m)
	    (assertion-violation who "mark set twice" m)
	  (vector-set! MARKS m obj))
      (let* ((n MARKS.len)
	     (v (make-vector (fxmax (unsafe.fx* n 2) (unsafe.fxadd1 m)) #f)))
	(let loop ((i 0))
	  (if (unsafe.fx= i n)
	      (begin
		(set! MARKS v)
		(vector-set! MARKS m obj))
	    (begin
	      (vector-set! v i (vector-ref MARKS i))
	      (loop (unsafe.fxadd1 i))))))))

  (define (read)
    ;;Read and return the next object; it will have not mark.
    ;;
    (read/mark #f))

  (define (read/mark m)
    ;;Read  and return the  next object.   Unless M  is false:  mark the
    ;;object with M.
    ;;
    (define (nom)
      (when m (assertion-violation who "unhandled mark")))
    (let ((ch (read-u8-as-char port)))
      (case ch
	((#\I)
	 (read-fixnum port))
	((#\P)
	 (if m
	     (let ((x (cons #f #f)))
	       (put-mark m x)
	       (set-car! x (read))
	       (set-cdr! x (read))
	       x)
	   (let ((a (read)))
	     (cons a (read)))))
	((#\N) '())
	((#\T) #t)
	((#\F) #f)
	((#\E) (eof-object))
	((#\U) (void))
	((#\s) ;;; string
	 (let ((n (read-int port)))
	   (let ((str (make-string n)))
	     (let f ((i 0))
	       (unless (unsafe.fx= i n)
		 (let ((c (read-u8-as-char port)))
		   (string-set! str i c)
		   (f (unsafe.fxadd1 i)))))
	     (when m (put-mark m str))
	     str)))
	((#\S) ;;; unicode string
	 (let ((n (read-int port)))
	   (let ((str (make-string n)))
	     (let f ((i 0))
	       (unless (unsafe.fx= i n)
		 (let ((c (integer->char (read-u32 port))))
		   (string-set! str i c)
		   (f (unsafe.fxadd1 i)))))
	     (when m (put-mark m str))
	     str)))
	((#\M) ;;; symbol
	 (let ((str (read)))
	   (let ((sym (string->symbol str)))
	     (when m (put-mark m sym))
	     sym)))
	((#\G)
	 (let* ((pretty (read))
		(unique (read)))
	   (let ((g (foreign-call "ikrt_strings_to_gensym" pretty unique)))
	     (when m (put-mark m g))
	     g)))
	((#\V) ;;; vector
	 (let ((n (read-int port)))
	   (let ((v (make-vector n)))
	     (when m (put-mark m v))
	     (let f ((i 0))
	       (unless (unsafe.fx= i n)
		 (vector-set! v i (read))
		 (f (unsafe.fxadd1 i))))
	     v)))
	((#\v) ;;; bytevector
	 (let ((n (read-int port)))
	   (let ((v (make-bytevector n)))
	     (when m (put-mark m v))
	     (let f ((i 0))
	       (unless (unsafe.fx= i n)
		 (bytevector-u8-set! v i (read-u8 port))
		 (f (unsafe.fxadd1 i))))
	     v)))
	((#\x) ;;; code
	 (read-code m #f))
	((#\Q) ;;; procedure
	 (read-procedure m))
	((#\R)
	 (let* ((rtd-name (read))
		(rtd-symbol (read))
		(field-count (read-int port)))
	   (let ((fields
		  (let f ((i 0))
		    (cond
		     ((unsafe.fx= i field-count) '())
		     (else
		      (let ((a (read)))
			(cons a (f (unsafe.fxadd1 i)))))))))
	     (let ((rtd (make-struct-type
			 rtd-name fields rtd-symbol)))
	       (when m (put-mark m rtd))
	       rtd))))
	((#\{)
	 (let ((n (read-int port)))
	   (let ((rtd (read)))
	     (let ((x (make-struct rtd n)))
	       (when m (put-mark m x))
	       (let f ((i 0))
		 (unless (unsafe.fx= i n)
		   ($struct-set! x i (read))
		   (f (unsafe.fxadd1 i))))
	       x))))
	((#\C) (integer->char (read-u32 port)))
	((#\c) (read-u8-as-char port))
	((#\>)
	 (let ((m (read-u32 port)))
	   (read/mark m)))
	((#\<)
	 (let ((m (read-u32 port)))
	   (unless (unsafe.fx< m MARKS.len)
	     (assertion-violation who "invalid mark" m))
	   (or (vector-ref MARKS m)
	       (error who "uninitialized mark" m))))
	((#\l) ;;; list of length <= 255
	 (read-list (read-u8 port) m))
	((#\L) ;;; list of length > 255
	 (read-list (read-int port) m))
	((#\W) ;;; r6rs record type descriptor
	 (let* ((name    (read))
		(parent  (read))
		(uid     (read))
		(sealed? (read))
		(opaque? (read))
		(n       (read))
		(fields  (make-vector n)))
	   (let f ((i 0))
	     (cond
	      ((= i n)
	       (let ((rtd (make-record-type-descriptor
			   name parent uid sealed? opaque?
			   fields)))
		 (when m (put-mark m rtd))
		 rtd))
	      (else
	       (let* ((field-mutable? (read))
		      (field-name (read)))
		 (vector-set! fields i
			      (list (if field-mutable? 'mutable 'immutable) field-name))
		 (f (+ i 1))))))))
	((#\b) ;;; bignum
	 (let ((i (read-int port)))
	   (let ((bytes (if (< i 0) (- i) i)))
	     (let ((bv (get-bytevector-n port bytes)))
	       (let ((n (bytevector-uint-ref bv 0 'little bytes)))
		 (let ((n (if (< i 0) (- n) n)))
		   (when m (put-mark m n))
		   n))))))
	((#\f) ;;; flonum
	 (let ()
	   (import (ikarus system $flonums))
	   (let ((x ($make-flonum)))
	     ($flonum-set! x 7 (get-u8 port))
	     ($flonum-set! x 6 (get-u8 port))
	     ($flonum-set! x 5 (get-u8 port))
	     ($flonum-set! x 4 (get-u8 port))
	     ($flonum-set! x 3 (get-u8 port))
	     ($flonum-set! x 2 (get-u8 port))
	     ($flonum-set! x 1 (get-u8 port))
	     ($flonum-set! x 0 (get-u8 port))
	     (when m (put-mark m x))
	     x)))
	((#\r) ;;; ratnum
	 (let* ((den (read))
		(num (read)))
	   (let ((x (/ num den)))
	     (when m (put-mark m x))
	     x)))
	((#\i) ;;; compnum
	 (let* ((real (read))
		(imag (read)))
	   (let ((x (make-rectangular real imag)))
	     (when m (put-mark m x))
	     x)))
	((#\h #\H)	;;; EQ? or EQV? hashtable
	 (let ((x (if (unsafe.char= ch #\h)
		      (make-eq-hashtable)
		    (make-eqv-hashtable))))
	   (when m (put-mark m x))
	   (let* ((keys (read)) (vals (read)))
	     (vector-for-each
                 (lambda (k v) (hashtable-set! x k v))
	       keys vals))
	   x))
	(else
	 (assertion-violation who "unexpected char as fasl object header" ch port)))))

  (define (read-code code-mark closure-mark)
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
    (let* ((code-size (read-int    port))
	   (freevars  (read-fixnum port))
	   (code      (make-code code-size freevars)))
      (when code-mark (put-mark code-mark code))
      (let ((annotation (read)))
	(set-code-annotation! code annotation))
      ;;Read the actual code one byte at a time.
      (let loop ((i 0))
	(unless (unsafe.fx= i code-size)
	  (code-set! code i (char->int (read-u8-as-char port)))
	  (loop (unsafe.fxadd1 i))))
      (if closure-mark
	  ;;First  build the  closure and  mark it,  then read  the code
	  ;;relocation vector.
	  (let ((closure ($code->closure code)))
	    (put-mark closure-mark closure)
	    (set-code-reloc-vector! code (read))
	    code)
       (begin
	 (set-code-reloc-vector! code (read))
	 code))))

  (define (read-procedure mark)
    ;;Read a procedure.
    ;;
    ;;FIXME Understand and document the expected format.
    ;;
    (let ((ch (read-u8-as-char port)))
      (case ch
	((#\x)
	 (let ((code (read-code #f mark)))
	   (if mark
	       (vector-ref MARKS mark)
	     ($code->closure code))))
	((#\<)
	 (let ((closure-mark (read-u32 port)))
	   (unless (unsafe.fx< closure-mark MARKS.len)
	     (assertion-violation who "invalid mark" mark))
	   (let* ((code (vector-ref MARKS closure-mark))
		  (proc ($code->closure code)))
	     (when mark (put-mark mark proc))
	     proc)))
	((#\>)
	 (let ((closure-mark (read-u32 port))
	       (ch           (read-u8-as-char port)))
	   (unless (unsafe.char= ch #\x)
	     (assertion-violation who "expected char \"x\"" ch))
	   (let ((code (read-code closure-mark mark)))
	     (if mark
		 (vector-ref MARKS mark)
	       ($code->closure code)))))
	(else
	 (assertion-violation who "invalid code header" ch)))))

  (define (read-list len mark)
    ;;Read and  return a  list of LEN  elements.  Unless MARK  is false:
    ;;mark the list with MARK.
    ;;
    (let ((ls (make-list (+ 1 len))))
      (when mark (put-mark mark ls))
      (let loop ((ls ls))
	(set-car! ls (read))
	(let ((d (cdr ls)))
	  (if (null? d)
	      (set-cdr! ls (read))
	    (loop d))))
      ls))

  (read))


;;;; utilities

(define (make-struct rtd n)
  ;;Build  and return  a new  struct  object of  type RTD  and having  N
  ;;fields.  Initialise all the fields to the fixnum 0.
  ;;
  (let loop ((i 0) (n n) (s ($make-struct rtd n)))
    (if (unsafe.fx= i n)
	s
      (begin
	($struct-set! s i 0)
	(loop (unsafe.fxadd1 i) n s)))))

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
  (case (fixnum-width)
    ((30)
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
    (else
     (let* ((u0 (read-u32 port))
	    (u1 (read-u32 port)))
       (if (<= u1 #x7FFFFFF)
	   (sra (bitwise-ior u0 (sll u1 32)) 3)
	 (let ((u0 (fxlogand #xFFFFFFFF (fxlognot u0)))
	       (u1 (fxlogand #xFFFFFFFF (fxlognot u1))))
	   (fx- -1
		(fxlogor (fxsra u0 3) (fxsll u1 29)))))))))

(define (read-int port)
  ;;Read from the  input PORT an exact integer  represented as 32-bit or
  ;;64-bit value depending on the underlying platform's word size.
  ;;
  (case (fixnum-width)
    ((30)	;32-bit platform
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
    (else	;64-bit platform
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
