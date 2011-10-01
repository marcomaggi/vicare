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
  (import
    (ikarus system $strings)
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $bytevectors)
    (ikarus system $pairs)
    (except (ikarus) string-length string-ref string-set! make-string
            string->list string-append substring string
            list->string uuid string-copy string-for-each
            string=? string<? string<=? string>? string>=?
            string-fill! string-copy!

	    ;; Vicare specific
	    string->latin1	latin1->string))


(define string-length
  (lambda (x)
    (unless (string? x)
      (die 'string-length "not a string" x))
    ($string-length x)))


(define (string-ref s i)
  (unless (string? s)
    (die 'string-ref "not a string" s))
  (unless (fixnum? i)
    (die 'string-ref "not a valid index" i))
  (unless (and ($fx< i ($string-length s))
	       ($fx<= 0 i))
    (die 'string-ref "index is out of range" i s))
  (let ((c ($string-ref s i)))
    (unless (char? c)
      (die 'string-ref "BUG: got a non-char"))
    c))


(define string-set!
  (lambda (s i c)
    (unless (string? s)
      (die 'string-set! "not a string" s))
    (unless (fixnum? i)
      (die 'string-set! "not a valid index" i))
    (unless (and ($fx< i ($string-length s))
		 ($fx>= i 0))
      (die 'string-set! "index is out of range" i s))
    (unless (char? c)
      (die 'string-set! "not a character" c))
    ($string-set! s i c)))

(define make-string
  (let ()
    (define fill!
      (lambda (s i n c)
	(cond
	 (($fx= i n) s)
	 (else
	  ($string-set! s i c)
	  (fill! s ($fx+ i 1) n c)))))
    (define (make-string* n c)
      (unless (fixnum? n)
	(die 'make-string "length is not a fixnum" n))
      (unless (eqv? 0 (fxsra n (fx- (fixnum-width) 2)))
	(die 'make-string "length is out of range" n))
      (fill! ($make-string n) 0 n c))
    (define make-string
      (case-lambda
       ((n) (make-string* n (integer->char 0)))
       ((n c)
	(if (char? c)
	    (make-string* n c)
	  (die 'make-string "not a character" c)))))
    make-string))


(define string
    ;;; FIXME: add case-lambda
  (letrec ((length
	    (lambda (ls n)
	      (cond
	       ((null? ls) n)
	       ((char? ($car ls)) (length ($cdr ls) ($fx+ n 1)))
	       (else (die 'string "not a character" ($car ls))))))
	   (loop
	    (lambda (s ls i n)
	      (cond
	       (($fx= i n) s)
	       (else
		($string-set! s i ($car ls))
		(loop s ($cdr ls) ($fx+ i 1) n))))))
    (lambda ls
      (let ((n (length ls 0)))
	(let ((s (make-string n)))
	  (loop s ls 0 n))))))

(module (substring)
  (define fill
    (lambda (s d si sj di)
      (cond
       (($fx= si sj) d)
       (else
	($string-set! d di ($string-ref s si))
	(fill s d ($fxadd1 si) sj ($fxadd1 di))))))
  (define substring
    (lambda (s n m)
      (unless (string? s)
	(die 'substring "not a string" s))
      (let ((len ($string-length s)))
	(unless (and (fixnum? n)
		     ($fx>= n 0)
		     ($fx<= n len))
	  (die 'substring "not a valid start index" n s))
	(unless (and (fixnum? m)
		     ($fx>= m 0)
		     ($fx<= m len))
	  (die 'substring "not a valid end index" m s))
	(unless ($fx<= n m)
	  (die 'substring "indices are in decreasing order" n m))
	(let ((len ($fx- m n)))
	  (if ($fx> len 0)
	      (fill s ($make-string len) n m 0)
	    ""))))))

(define string-copy
  (lambda (s)
    (if (string? s)
	(substring s 0 (string-length s))
      (die 'string-copy "not a string" s))))

(module (string=?)
  (define bstring=?
    (lambda (s1 s2 i j)
      (or ($fx= i j)
	  (and ($char= ($string-ref s1 i) ($string-ref s2 i))
	       (bstring=? s1 s2 ($fxadd1 i) j)))))
  (define check-strings-and-return-false
    (lambda (s*)
      (cond
       ((null? s*) #f)
       ((string? ($car s*))
	(check-strings-and-return-false ($cdr s*)))
       (else (err ($car s*))))))
  (define strings=?
    (lambda (s s* n)
      (or (null? s*)
	  (let ((a ($car s*)))
	    (unless (string? a)
	      (die 'string=? "not a string" a))
	    (if ($fx= n ($string-length a))
		(and (strings=? s ($cdr s*) n)
		     (bstring=? s a 0 n))
	      (check-strings-and-return-false ($cdr s*)))))))
  (define (err x)
    (die 'string=? "not a string" x))
  (define string=?
    (case-lambda
     ((s s1)
      (if (string? s)
	  (if (string? s1)
	      (let ((n ($string-length s)))
		(and ($fx= n ($string-length s1))
		     (bstring=? s s1 0 n)))
	    (err s1))
	(err s)))
     ((s . s*)
      (if (string? s)
	  (strings=? s s* ($string-length s))
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

(define ($string<? s1 s2)
  (let ((n1 ($string-length s1))
	(n2 ($string-length s2)))
    (if ($fx< n1 n2)
	(let f ((i 0) (n n1) (s1 s1) (s2 s2))
	  (if ($fx= i n)
	      #t
	    (let ((c1 ($string-ref s1 i))
		  (c2 ($string-ref s2 i)))
	      (if ($char< c1 c2)
		  #t
		(if ($char= c1 c2)
		    (f ($fxadd1 i) n s1 s2)
		  #f)))))
      (let f ((i 0) (n n2) (s1 s1) (s2 s2))
	(if ($fx= i n)
	    #f
	  (let ((c1 ($string-ref s1 i))
		(c2 ($string-ref s2 i)))
	    (if ($char< c1 c2)
		#t
	      (if ($char= c1 c2)
		  (f ($fxadd1 i) n s1 s2)
		#f))))))))

(define ($string<=? s1 s2)
  (let ((n1 ($string-length s1))
	(n2 ($string-length s2)))
    (if ($fx<= n1 n2)
	(let f ((i 0) (n n1) (s1 s1) (s2 s2))
	  (if ($fx= i n)
	      #t
	    (let ((c1 ($string-ref s1 i))
		  (c2 ($string-ref s2 i)))
	      (if ($char< c1 c2)
		  #t
		(if ($char= c1 c2)
		    (f ($fxadd1 i) n s1 s2)
		  #f)))))
      (let f ((i 0) (n n2) (s1 s1) (s2 s2))
	(if ($fx= i n)
	    #f
	  (let ((c1 ($string-ref s1 i))
		(c2 ($string-ref s2 i)))
	    (if ($char< c1 c2)
		#t
	      (if ($char= c1 c2)
		  (f ($fxadd1 i) n s1 s2)
		#f))))))))

(define ($string>? s1 s2)
  ($string<? s2 s1))

(define ($string>=? s1 s2)
  ($string<=? s2 s1))

(define string<?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    ($string<? s1 s2)
	  (die 'string<? "not a string" s2))
      (die 'string<? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string<? $string<? s s*))))

(define string<=?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    ($string<=? s1 s2)
	  (die 'string<=? "not a string" s2))
      (die 'string<=? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string<=? $string<=? s s*))))

(define string>?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    ($string>? s1 s2)
	  (die 'string>? "not a string" s2))
      (die 'string>? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string>? $string>? s s*))))

(define string>=?
  (case-lambda
   ((s1 s2)
    (if (string? s1)
	(if (string? s2)
	    ($string>=? s1 s2)
	  (die 'string>=? "not a string" s2))
      (die 'string>=? "not a string" s2)))
   ((s . s*)
    (string-cmp 'string>=? $string>=? s s*))))

(define string->list
  (lambda (x)
    (unless (string? x)
      (die 'string->list "not a string" x))
    (let f ((x x) (i ($string-length x)) (ac '()))
      (cond
       (($fxzero? i) ac)
       (else
	(let ((i ($fxsub1 i)))
	  (f x i (cons ($string-ref x i) ac))))))))


(define list->string
  (letrec ((race
	    (lambda (h t ls n)
	      (if (pair? h)
		  (let ((h ($cdr h)))
		    (if (pair? h)
			(if (not (eq? h t))
			    (race ($cdr h) ($cdr t) ls ($fx+ n 2))
			  (die 'reverse "circular list" ls))
		      (if (null? h)
			  ($fx+ n 1)
			(die 'reverse "not a proper list" ls))))
		(if (null? h)
		    n
		  (die 'reverse "not a proper list" ls)))))
	   (fill
	    (lambda (s i ls)
	      (cond
	       ((null? ls) s)
	       (else
		(let ((c ($car ls)))
		  (unless (char? c)
		    (die 'list->string "not a character" c))
		  ($string-set! s i c)
		  (fill s ($fxadd1 i) (cdr ls))))))))
    (lambda (ls)
      (let ((n (race ls ls ls 0)))
	(let ((s ($make-string n)))
	  (fill s 0 ls))))))

(module (string-append)
  ;; FIXME: make nonconsing on 0,1,2, and 3 args
  (define length*
    (lambda (s* n)
      (cond
       ((null? s*) n)
       (else
	(let ((a ($car s*)))
	  (unless (string? a)
	    (die 'string-append "not a string" a))
	  (length* ($cdr s*) ($fx+ n ($string-length a))))))))
  (define fill-string
    (lambda (s a si sj ai)
      (unless ($fx= si sj)
	($string-set! s si ($string-ref a ai))
	(fill-string s a ($fxadd1 si) sj ($fxadd1 ai)))))
  (define fill-strings
    (lambda (s s* i)
      (cond
       ((null? s*) s)
       (else
	(let ((a ($car s*)))
	  (let ((n ($string-length a)))
	    (let ((j ($fx+ i n)))
	      (fill-string s a i j 0)
	      (fill-strings s ($cdr s*) j))))))))
  (define string-append
    (lambda s*
      (let ((n (length* s* 0)))
	(let ((s ($make-string n)))
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
	 (($fx= i n) (void))
	 (else
	  (p (string-ref v i))
	  (f p v ($fxadd1 i) n)))))
     ((p v0 v1)
      (unless (procedure? p)
	(die who "not a procedure" p))
      (unless (string? v0)
	(die who "not a string" v0))
      (unless (string? v1)
	(die who "not a string" v1))
      (let ((n (string-length v0)))
	(unless ($fx= n ($string-length v1))
	  (die who "length mismatch" v0 v1))
	(let f ((p p) (v0 v0) (v1 v1) (i 0) (n n))
	  (cond
	   (($fx= i n) (void))
	   (else
	    (p ($string-ref v0 i) ($string-ref v1 i))
	    (f p v0 v1 ($fxadd1 i) n))))))
     ((p v0 v1 . v*)
      (unless (procedure? p)
	(die who "not a procedure" p))
      (unless (string? v0)
	(die who "not a string" v0))
      (unless (string? v1)
	(die who "not a string" v1))
      (let ((n (string-length v0)))
	(unless ($fx= n ($string-length v1))
	  (die who "length mismatch" v0 v1))
	(let f ((v* v*) (n n))
	  (unless (null? v*)
	    (let ((a ($car v*)))
	      (unless (string? a)
		(die who "not a string" a))
	      (unless ($fx= ($string-length a) n)
		(die who "length mismatch")))
	    (f ($cdr v*) n)))
	(let f ((p p) (v0 v0) (v1 v1) (v* v*) (i 0) (n n))
	  (cond
	   (($fx= i n) (void))
	   (else
	    (apply p ($string-ref v0 i) ($string-ref v1 i)
		   (let f ((i i) (v* v*))
		     (if (null? v*)
			 '()
		       (cons ($string-ref ($car v*) i)
			     (f i ($cdr v*))))))
	    (f p v0 v1 v* ($fxadd1 i) n)))))))))

(define (string-fill! v fill)
  (unless (string? v)
    (die 'string-fill! "not a vector" v))
  (unless (char? fill)
    (die 'string-fill! "not a character" fill))
  (let f ((v v) (i 0) (n ($string-length v)) (fill fill))
    (unless ($fx= i n)
      ($string-set! v i fill)
      (f v ($fxadd1 i) n fill))))


(define string-copy!
  (lambda (src src-start dst dst-start k)
    (cond
     ((or (not (fixnum? src-start)) ($fx< src-start 0))
      (die 'string-copy! "not a valid starting index" src-start))
     ((or (not (fixnum? dst-start)) ($fx< dst-start 0))
      (die 'string-copy! "not a valid starting index" dst-start))
     ((or (not (fixnum? k)) ($fx< k 0))
      (die 'string-copy! "not a valid length" k))
     ((not (string? src))
      (die 'string-copy! "not a string" src))
     ((not (string? dst))
      (die 'string-copy! "not a string" dst))
     ((let ((n ($fx+ src-start k)))
	(or ($fx< n 0) ($fx> n ($string-length src))))
      (die 'string-copy! "out of range" src-start k))
     ((let ((n ($fx+ dst-start k)))
	(or ($fx< n 0) ($fx> n ($string-length dst))))
      (die 'string-copy! "out of range" dst-start k))
     ((eq? src dst)
      (cond
       (($fx< dst-start src-start)
	(let f ((src src) (si src-start) (di dst-start) (sj ($fx+ src-start k)))
	  (unless ($fx= si sj)
	    ($string-set! src di ($string-ref src si))
	    (f src ($fxadd1 si) ($fxadd1 di) sj))))
       (($fx< src-start dst-start)
	(let f ((src src) (si ($fx+ src-start k)) (di ($fx+ dst-start k)) (sj src-start))
	  (unless ($fx= si sj)
	    (let ((si ($fxsub1 si)) (di ($fxsub1 di)))
	      ($string-set! src di ($string-ref src si))
	      (f src si di sj)))))
       (else (void))))
     (else
      (let f ((src src) (si src-start) (dst dst) (di dst-start) (sj ($fx+ src-start k)))
	(unless ($fx= si sj)
	  ($string-set! dst di ($string-ref src si))
	  (f src ($fxadd1 si) dst ($fxadd1 di) sj)))))))

(define uuid
  (lambda ()
    (let ((s ($make-bytevector 16)))
      (let ((r (foreign-call "ik_uuid" s)))
	(if (bytevector? r)
	    (utf8->string r)
	  (error 'uuid "cannot obtain unique id"))))))


(define (string->latin1 str)
  ;;Defined by Vicare.  Convert the string STR into a bytevector holding
  ;;octects representing the character's Latin-1 code points.
  ;;
  (define who 'latin1->string)
  (unless (string? str)
    (assertion-violation who "expected string as argument" str))
  ;;Both strings and bytevectors have length representable as fixnum.
  (let* ((bv.len ($string-length str))
	 (bv	 ($make-bytevector bv.len)))
    (do ((i 0 ($fxadd1 i)))
	(($fx= i bv.len)
	 bv)
      ($bytevector-set! bv i ($char->fixnum ($string-ref str i))))))

(define (latin1->string bv)
  ;;Defined by Vicare.  Convert the  bytevector BV into a string holding
  ;;characters representing bytes interpreted as Latin-1 code points.
  ;;
  (define who 'latin1->string)
  (unless (bytevector? bv)
    (assertion-violation who "expected bytevector as argument" bv))
  ;;Both strings and bytevectors have length representable as fixnum.
  (let* ((str.len ($bytevector-length bv))
	 (str     ($make-string str.len)))
    (do ((i 0 ($fxadd1 i)))
	(($fx= i str.len)
	 str)
      ($string-set! str i ($fixnum->char ($bytevector-u8-ref bv i))))))


;;;; done

)

;;; end of file
