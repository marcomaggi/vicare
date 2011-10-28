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


(library (ikarus strings)
  (export string-length string-ref string-set! make-string string->list
          string-append substring string list->string uuid
          string-copy string-for-each string-fill!
          string=? string<? string<=? string>? string>=?
          string-copy!

	  ;; Vicare specific
	  string->latin1	latin1->string)
  (import (except (ikarus) string-length string-ref string-set! make-string
		  string->list string-append substring string
		  list->string uuid string-copy string-for-each
		  string=? string<? string<=? string>? string>=?
		  string-fill! string-copy!

		  ;; Vicare specific
		  string->latin1	latin1->string)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (ikarus system $strings)
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $bytevectors)
    (ikarus system $pairs)
    )


;;;; arguments validation

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (char who obj)
  (char? obj)
  (assertion-violation who "expected character as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (length who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as string length argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as string index argument" obj))

(define-argument-validation (index-for who idx str)
  (unsafe.fx< idx (unsafe.string-length str))
  (assertion-violation who "index is out of range for string" idx str))

;;; --------------------------------------------------------------------

(define-argument-validation (latin1 who code-point str)
  (unsafe.fx< code-point 256)
  (assertion-violation who
    "expected only Latin-1 characters in string argument"
    (unsafe.fixnum->char code-point) str))


(define (string-length x)
  (define who 'string-length)
  (with-arguments-validation (who)
      ((string x))
    (unsafe.string-length x)))

(define (string-ref s i)
  (define who 'string-ref)
  (with-arguments-validation (who)
      ((string		s)
       (index		i)
       (index-for	i s))
    (unsafe.string-ref s i)))

(define (string-set! s i c)
  (define who 'string-set!)
  (with-arguments-validation (who)
      ((string		s)
       (index		i)
       (index-for	i s)
       (char		c))
    (unsafe.string-set! s i c)))


(define make-string
  (case-lambda
   ((len)
    (make-string len #\x0))
   ((len fill)
    (define who 'make-string)
    (with-arguments-validation (who)
	((length len)
	 (char   fill))
      (let loop ((str (unsafe.make-string len))
		 (idx 0)
		 (len len))
	(if (unsafe.fx= idx len)
	    str
	  (begin
	    (unsafe.string-set! str idx fill)
	    (loop str (unsafe.fxadd1 idx) len))))))))

(define string
  (case-lambda
   (() "")
   ((one)
    (with-arguments-validation (string)
	((char one))
      (let ((str (unsafe.make-string 1)))
	(unsafe.string-set! str 0 one)
	str)))

   ((one two)
    (with-arguments-validation (string)
	((char one)
	 (char two))
      (let ((str (unsafe.make-string 2)))
	(unsafe.string-set! str 0 one)
	(unsafe.string-set! str 1 two)
	str)))

   ((one two three)
    (with-arguments-validation (string)
	((char one)
	 (char two)
	 (char three))
      (let ((str (unsafe.make-string 3)))
	(unsafe.string-set! str 0 one)
	(unsafe.string-set! str 1 two)
	(unsafe.string-set! str 2 three)
	str)))

   ((one two three four)
    (with-arguments-validation (string)
	((char one)
	 (char two)
	 (char three)
	 (char four))
      (let ((str (unsafe.make-string 4)))
	(unsafe.string-set! str 0 one)
	(unsafe.string-set! str 1 two)
	(unsafe.string-set! str 2 three)
	(unsafe.string-set! str 3 four)
	str)))

   ((one . chars)
    (define who 'string)
    (define (%length-and-validation chars len)
      (if (null? chars)
	  len
	(let ((ch (unsafe.car chars)))
	  (with-arguments-validation (who)
	      ((char ch))
	    (%length-and-validation (unsafe.cdr chars) (unsafe.fxadd1 len))))))
    (let* ((chars (cons one chars))
	   (len   (%length-and-validation chars 0)))
      (with-arguments-validation (who)
	  ((length len))
	(let loop ((str   (unsafe.make-string len))
		   (idx   0)
		   (chars chars))
	  (if (null? chars)
	      str
	    (begin
	      (unsafe.string-set! str idx (unsafe.car chars))
	      (loop str (unsafe.fxadd1 idx) (unsafe.cdr chars))))))))))


(module (substring)
  (define fill
    (lambda (s d si sj di)
      (cond
       ((unsafe.fx= si sj) d)
       (else
	(unsafe.string-set! d di (unsafe.string-ref s si))
	(fill s d (unsafe.fxadd1 si) sj (unsafe.fxadd1 di))))))
  (define substring
    (lambda (s n m)
      (unless (string? s)
	(die 'substring "not a string" s))
      (let ((len (unsafe.string-length s)))
	(unless (and (fixnum? n)
		     (unsafe.fx>= n 0)
		     (unsafe.fx<= n len))
	  (die 'substring "not a valid start index" n s))
	(unless (and (fixnum? m)
		     (unsafe.fx>= m 0)
		     (unsafe.fx<= m len))
	  (die 'substring "not a valid end index" m s))
	(unless (unsafe.fx<= n m)
	  (die 'substring "indices are in decreasing order" n m))
	(let ((len (unsafe.fx- m n)))
	  (if (unsafe.fx> len 0)
	      (fill s (unsafe.make-string len) n m 0)
	    ""))))))


(define string-copy
  (lambda (s)
    (if (string? s)
	(substring s 0 (string-length s))
      (die 'string-copy "not a string" s))))

(module (string=?)
  (define bstring=?
    (lambda (s1 s2 i j)
      (or (unsafe.fx= i j)
	  (and (unsafe.char= (unsafe.string-ref s1 i) (unsafe.string-ref s2 i))
	       (bstring=? s1 s2 (unsafe.fxadd1 i) j)))))
  (define check-strings-and-return-false
    (lambda (s*)
      (cond
       ((null? s*) #f)
       ((string? (unsafe.car s*))
	(check-strings-and-return-false (unsafe.cdr s*)))
       (else (err (unsafe.car s*))))))
  (define strings=?
    (lambda (s s* n)
      (or (null? s*)
	  (let ((a (unsafe.car s*)))
	    (unless (string? a)
	      (die 'string=? "not a string" a))
	    (if (unsafe.fx= n (unsafe.string-length a))
		(and (strings=? s (unsafe.cdr s*) n)
		     (bstring=? s a 0 n))
	      (check-strings-and-return-false (unsafe.cdr s*)))))))
  (define (err x)
    (die 'string=? "not a string" x))
  (define string=?
    (case-lambda
     ((s s1)
      (if (string? s)
	  (if (string? s1)
	      (let ((n (unsafe.string-length s)))
		(and (unsafe.fx= n (unsafe.string-length s1))
		     (bstring=? s s1 0 n)))
	    (err s1))
	(err s)))
     ((s . s*)
      (if (string? s)
	  (strings=? s s* (unsafe.string-length s))
	(err s))))))


(define string-cmp
  (lambda (who cmp s1 s*)
    (if (string? s1)
	(let f ((s1 s1) (s* s*))
	  (cond
	   ((null? s*) #t)
	   (else
	    (let ((s2 (car s*)))
	      (if (string? s2)
		  (if (cmp s1 s2)
		      (f s2 (cdr s*))
		    (let f ((s* (cdr s*)))
		      (cond
		       ((null? s*) #f)
		       ((string? (car s*))
			(f (cdr s*)))
		       (else
			(die who "not a string"
			     (car s*))))))
		(die who "not a string" s2))))))
      (die who "not a string" s1))))

(define (unsafe.string<? s1 s2)
  (let ((n1 (unsafe.string-length s1))
	(n2 (unsafe.string-length s2)))
    (if (unsafe.fx< n1 n2)
	(let f ((i 0) (n n1) (s1 s1) (s2 s2))
	  (if (unsafe.fx= i n)
	      #t
	    (let ((c1 (unsafe.string-ref s1 i))
		  (c2 (unsafe.string-ref s2 i)))
	      (if (unsafe.char< c1 c2)
		  #t
		(if (unsafe.char= c1 c2)
		    (f (unsafe.fxadd1 i) n s1 s2)
		  #f)))))
      (let f ((i 0) (n n2) (s1 s1) (s2 s2))
	(if (unsafe.fx= i n)
	    #f
	  (let ((c1 (unsafe.string-ref s1 i))
		(c2 (unsafe.string-ref s2 i)))
	    (if (unsafe.char< c1 c2)
		#t
	      (if (unsafe.char= c1 c2)
		  (f (unsafe.fxadd1 i) n s1 s2)
		#f))))))))

(define (unsafe.string<=? s1 s2)
  (let ((n1 (unsafe.string-length s1))
	(n2 (unsafe.string-length s2)))
    (if (unsafe.fx<= n1 n2)
	(let f ((i 0) (n n1) (s1 s1) (s2 s2))
	  (if (unsafe.fx= i n)
	      #t
	    (let ((c1 (unsafe.string-ref s1 i))
		  (c2 (unsafe.string-ref s2 i)))
	      (if (unsafe.char< c1 c2)
		  #t
		(if (unsafe.char= c1 c2)
		    (f (unsafe.fxadd1 i) n s1 s2)
		  #f)))))
      (let f ((i 0) (n n2) (s1 s1) (s2 s2))
	(if (unsafe.fx= i n)
	    #f
	  (let ((c1 (unsafe.string-ref s1 i))
		(c2 (unsafe.string-ref s2 i)))
	    (if (unsafe.char< c1 c2)
		#t
	      (if (unsafe.char= c1 c2)
		  (f (unsafe.fxadd1 i) n s1 s2)
		#f))))))))

(define (unsafe.string>? s1 s2)
  (unsafe.string<? s2 s1))

(define (unsafe.string>=? s1 s2)
  (unsafe.string<=? s2 s1))

(define string<?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    (unsafe.string<? s1 s2)
	  (die 'string<? "not a string" s2))
      (die 'string<? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string<? unsafe.string<? s s*))))

(define string<=?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    (unsafe.string<=? s1 s2)
	  (die 'string<=? "not a string" s2))
      (die 'string<=? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string<=? unsafe.string<=? s s*))))

(define string>?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    (unsafe.string>? s1 s2)
	  (die 'string>? "not a string" s2))
      (die 'string>? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string>? unsafe.string>? s s*))))

(define string>=?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    (unsafe.string>=? s1 s2)
	  (die 'string>=? "not a string" s2))
      (die 'string>=? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string>=? unsafe.string>=? s s*))))

(define string->list
  (lambda (x)
    (unless (string? x)
      (die 'string->list "not a string" x))
    (let f ((x x) (i (unsafe.string-length x)) (ac '()))
      (cond
       ((unsafe.fxzero? i) ac)
       (else
	(let ((i (unsafe.fxsub1 i)))
	  (f x i (cons (unsafe.string-ref x i) ac))))))))


(define list->string
  (letrec ((race
	    (lambda (h t ls n)
	      (if (pair? h)
		  (let ((h (unsafe.cdr h)))
		    (if (pair? h)
			(if (not (eq? h t))
			    (race (unsafe.cdr h) (unsafe.cdr t) ls (unsafe.fx+ n 2))
			  (die 'reverse "circular list" ls))
		      (if (null? h)
			  (unsafe.fx+ n 1)
			(die 'reverse "not a proper list" ls))))
		(if (null? h)
		    n
		  (die 'reverse "not a proper list" ls)))))
	   (fill
	    (lambda (s i ls)
	      (cond
	       ((null? ls) s)
	       (else
		(let ((c (unsafe.car ls)))
		  (unless (char? c)
		    (die 'list->string "not a character" c))
		  (unsafe.string-set! s i c)
		  (fill s (unsafe.fxadd1 i) (cdr ls))))))))
    (lambda (ls)
      (let ((n (race ls ls ls 0)))
	(let ((s (unsafe.make-string n)))
	  (fill s 0 ls))))))

(module (string-append)
  ;; FIXME: make nonconsing on 0,1,2, and 3 args
  (define length*
    (lambda (s* n)
      (cond
       ((null? s*) n)
       (else
	(let ((a (unsafe.car s*)))
	  (unless (string? a)
	    (die 'string-append "not a string" a))
	  (length* (unsafe.cdr s*) (unsafe.fx+ n (unsafe.string-length a))))))))
  (define fill-string
    (lambda (s a si sj ai)
      (unless (unsafe.fx= si sj)
	(unsafe.string-set! s si (unsafe.string-ref a ai))
	(fill-string s a (unsafe.fxadd1 si) sj (unsafe.fxadd1 ai)))))
  (define fill-strings
    (lambda (s s* i)
      (cond
       ((null? s*) s)
       (else
	(let ((a (unsafe.car s*)))
	  (let ((n (unsafe.string-length a)))
	    (let ((j (unsafe.fx+ i n)))
	      (fill-string s a i j 0)
	      (fill-strings s (unsafe.cdr s*) j))))))))
  (define string-append
    (lambda s*
      (let ((n (length* s* 0)))
	(let ((s (unsafe.make-string n)))
	  (fill-strings s s* 0))))))


(module (string-for-each)
  (define who 'string-for-each)
  (define string-for-each
    (case-lambda
     ((p v)
      (unless (procedure? p)
	(die who "not a procedure" p))
      (unless (string? v)
	(die who "not a string" v))
      (let f ((p p) (v v) (i 0) (n (string-length v)))
	(cond
	 ((unsafe.fx= i n) (void))
	 (else
	  (p (string-ref v i))
	  (f p v (unsafe.fxadd1 i) n)))))
     ((p v0 v1)
      (unless (procedure? p)
	(die who "not a procedure" p))
      (unless (string? v0)
	(die who "not a string" v0))
      (unless (string? v1)
	(die who "not a string" v1))
      (let ((n (string-length v0)))
	(unless (unsafe.fx= n (unsafe.string-length v1))
	  (die who "length mismatch" v0 v1))
	(let f ((p p) (v0 v0) (v1 v1) (i 0) (n n))
	  (cond
	   ((unsafe.fx= i n) (void))
	   (else
	    (p (unsafe.string-ref v0 i) (unsafe.string-ref v1 i))
	    (f p v0 v1 (unsafe.fxadd1 i) n))))))
     ((p v0 v1 . v*)
      (unless (procedure? p)
	(die who "not a procedure" p))
      (unless (string? v0)
	(die who "not a string" v0))
      (unless (string? v1)
	(die who "not a string" v1))
      (let ((n (string-length v0)))
	(unless (unsafe.fx= n (unsafe.string-length v1))
	  (die who "length mismatch" v0 v1))
	(let f ((v* v*) (n n))
	  (unless (null? v*)
	    (let ((a (unsafe.car v*)))
	      (unless (string? a)
		(die who "not a string" a))
	      (unless (unsafe.fx= (unsafe.string-length a) n)
		(die who "length mismatch")))
	    (f (unsafe.cdr v*) n)))
	(let f ((p p) (v0 v0) (v1 v1) (v* v*) (i 0) (n n))
	  (cond
	   ((unsafe.fx= i n) (void))
	   (else
	    (apply p (unsafe.string-ref v0 i) (unsafe.string-ref v1 i)
		   (let f ((i i) (v* v*))
		     (if (null? v*)
			 '()
		       (cons (unsafe.string-ref (unsafe.car v*) i)
			     (f i (unsafe.cdr v*))))))
	    (f p v0 v1 v* (unsafe.fxadd1 i) n)))))))))

(define (string-fill! v fill)
  (unless (string? v)
    (die 'string-fill! "not a vector" v))
  (unless (char? fill)
    (die 'string-fill! "not a character" fill))
  (let f ((v v) (i 0) (n (unsafe.string-length v)) (fill fill))
    (unless (unsafe.fx= i n)
      (unsafe.string-set! v i fill)
      (f v (unsafe.fxadd1 i) n fill))))


(define string-copy!
  (lambda (src src-start dst dst-start k)
    (cond
     ((or (not (fixnum? src-start)) (unsafe.fx< src-start 0))
      (die 'string-copy! "not a valid starting index" src-start))
     ((or (not (fixnum? dst-start)) (unsafe.fx< dst-start 0))
      (die 'string-copy! "not a valid starting index" dst-start))
     ((or (not (fixnum? k)) (unsafe.fx< k 0))
      (die 'string-copy! "not a valid length" k))
     ((not (string? src))
      (die 'string-copy! "not a string" src))
     ((not (string? dst))
      (die 'string-copy! "not a string" dst))
     ((let ((n (unsafe.fx+ src-start k)))
	(or (unsafe.fx< n 0) (unsafe.fx> n (unsafe.string-length src))))
      (die 'string-copy! "out of range" src-start k))
     ((let ((n (unsafe.fx+ dst-start k)))
	(or (unsafe.fx< n 0) (unsafe.fx> n (unsafe.string-length dst))))
      (die 'string-copy! "out of range" dst-start k))
     ((eq? src dst)
      (cond
       ((unsafe.fx< dst-start src-start)
	(let f ((src src) (si src-start) (di dst-start) (sj (unsafe.fx+ src-start k)))
	  (unless (unsafe.fx= si sj)
	    (unsafe.string-set! src di (unsafe.string-ref src si))
	    (f src (unsafe.fxadd1 si) (unsafe.fxadd1 di) sj))))
       ((unsafe.fx< src-start dst-start)
	(let f ((src src) (si (unsafe.fx+ src-start k)) (di (unsafe.fx+ dst-start k)) (sj src-start))
	  (unless (unsafe.fx= si sj)
	    (let ((si (unsafe.fxsub1 si)) (di (unsafe.fxsub1 di)))
	      (unsafe.string-set! src di (unsafe.string-ref src si))
	      (f src si di sj)))))
       (else (void))))
     (else
      (let f ((src src) (si src-start) (dst dst) (di dst-start) (sj (unsafe.fx+ src-start k)))
	(unless (unsafe.fx= si sj)
	  (unsafe.string-set! dst di (unsafe.string-ref src si))
	  (f src (unsafe.fxadd1 si) dst (unsafe.fxadd1 di) sj)))))))

(define uuid
  (lambda ()
    (let ((s (unsafe.make-bytevector 16)))
      (let ((r (foreign-call "ik_uuid" s)))
	(if (bytevector? r)
	    (utf8->string r)
	  (error 'uuid "cannot obtain unique id"))))))


(define (string->latin1 str)
  ;;Defined by Vicare.  Convert the string STR into a bytevector holding
  ;;octects representing the character's Latin-1 code points.
  ;;
  (define who 'latin1->string)
  (with-arguments-validation (who)
      ((string str))
    ;;Both strings and bytevectors have length representable as fixnum.
    (let* ((bv.len (unsafe.string-length str))
	   (bv	 (unsafe.make-bytevector bv.len)))
      (do ((i 0 (unsafe.fxadd1 i)))
	  ((unsafe.fx= i bv.len)
	   bv)
	(let ((code-point (unsafe.char->fixnum (unsafe.string-ref str i))))
	  (with-arguments-validation (who)
	      ((latin1 code-point str))
	    (unsafe.bytevector-u8-set! bv i code-point)))))))

(define (latin1->string bv)
  ;;Defined by Vicare.  Convert the  bytevector BV into a string holding
  ;;characters representing bytes interpreted as Latin-1 code points.
  ;;
  (define who 'latin1->string)
  (with-arguments-validation (who)
      ((bytevector bv))
    ;;Both strings and bytevectors have length representable as fixnum.
    (let* ((str.len (unsafe.bytevector-length bv))
	   (str     (unsafe.make-string str.len)))
      (do ((i 0 (unsafe.fxadd1 i)))
	  ((unsafe.fx= i str.len)
	   str)
	(unsafe.string-set! str i (unsafe.fixnum->char (unsafe.bytevector-u8-ref bv i)))))))


;;;; done

)


(library (ikarus system strings)
  (export $make-string
	  $string-length
	  $string-ref
	  $string-set!)
  (import (ikarus))
  (define $make-string		make-string)
  (define $string-length	string-length)
  (define $string-ref		string-ref)
  (define $string-set!		string-set!))

;;; end of file
