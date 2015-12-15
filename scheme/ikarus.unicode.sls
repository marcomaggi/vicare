;;;Copyright (C) 2008  Abdulaziz Ghuloum, R. Kent Dybvig
;;;Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;;Modified by Marco Maggi.
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#!vicare
(library (ikarus.unicode)
  (export
    unicode-printable-char?

    char-upcase			char-downcase
    char-titlecase		char-foldcase
    char-whitespace?		char-title-case?
    char-lower-case?		char-upper-case?
    char-numeric?		char-alphabetic?
    char-general-category
    char-ci=?			char-ci!=?
    char-ci<?			char-ci<=?
    char-ci>?			char-ci>=?

    string-upcase		string-downcase
    string-foldcase		string-titlecase
    string-ci=?
    string-ci<?			string-ci<=?
    string-ci>?			string-ci>=?
    string-normalize-nfd	string-normalize-nfkd
    string-normalize-nfc	string-normalize-nfkc)
  (import (except (vicare)
		  unicode-printable-char?

		  char-upcase			char-downcase
		  char-titlecase		char-foldcase
		  char-whitespace?		char-title-case?
		  char-lower-case?		char-upper-case?
		  char-numeric?			char-alphabetic?
		  char-general-category
		  char-ci=?			char-ci!=?
		  char-ci<?			char-ci<=?
		  char-ci>?			char-ci>=?

		  string-upcase			string-downcase
		  string-foldcase		string-titlecase
		  string-ci=?
		  string-ci<?			string-ci<=?
		  string-ci>?			string-ci>=?
		  string-normalize-nfd		string-normalize-nfkd
		  string-normalize-nfc		string-normalize-nfkc)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $strings)
    (vicare system $vectors)
    (vicare system $chars))


;;;; helper

(define ($char-fixnum-minus x y)
  ($fx- ($char->fixnum x) ($char->fixnum y)))

;;This is used in the include files!!!
;;
(define (fxlogtest x y)
  (not (fxzero? (fxand x y))))


#;(module UNSAFE
  (fx< fx<= fx> fx>= fx= fx+ fx-
   fxior fxand fxsra fxsll fxzero?
   integer->char $char->fixnum
   char<? char<=? char=? char>? char>=?
   string-ref string-set! string-length
   vector-ref vector-set! vector-length)
  (import
    (rename (vicare system $strings)
      ($string-length string-length)
      ($string-ref    string-ref)
      ($string-set!   string-set!))
    (rename (vicare system $vectors)
      ($vector-length vector-length)
      ($vector-ref    vector-ref)
      ($vector-set!   vector-set!))
    (rename (vicare system $chars)
      ($char->fixnum $char->fixnum)
      ($fixnum->char integer->char)
      ($char< char<?)
      ($char<= char<=?)
      ($char= char=?)
      ($char> char>?)
      ($char>= char>=?))
    (rename (vicare system $fx)
      ($fxzero?    fxzero?)
      ($fxsra    fxsra)
      ($fxsll    fxsll)
      ($fxlogor  fxior)
      ($fxlogand fxand)
      ($fx+      fx+)
      ($fx-      fx-)
      ($fx<      fx<)
      ($fx>      fx>)
      ($fx>=     fx>=)
      ($fx<=     fx<=)
      ($fx=      fx=))))

#;(import UNSAFE)


;;;; include files

(include "unicode/unicode-char-cases.ss" #t)
(include "unicode/unicode-charinfo.ss"   #t)


;;;; char operations

(let-syntax
    ((define-char-op (syntax-rules ()
		       ((_ ?who ?unsafe-op)
			(define* (?who {c char?})
			  (?unsafe-op c)))
		       )))
  (define-char-op char-upcase			$char-upcase)
  (define-char-op char-downcase			$char-downcase)
  (define-char-op char-titlecase		$char-titlecase)
  (define-char-op char-foldcase			$char-foldcase)
  (define-char-op char-whitespace?		$char-whitespace?)
  (define-char-op char-lower-case?		$char-lower-case?)
  (define-char-op char-upper-case?		$char-upper-case?)
  (define-char-op char-title-case?		$char-title-case?)
  (define-char-op char-numeric?			$char-numeric?)
  (define-char-op unicode-printable-char?	$char-constituent?)
  (define-char-op char-alphabetic?		$char-alphabetic?)
  (define-char-op char-general-category		$char-category)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((define-char-cmp (syntax-rules ()
			((_ ?who ?case-insensitive-cmp)
			 (case-define* ?who
			   (({c1 char?} {c2 char?})
			    (?case-insensitive-cmp ($char-foldcase c1) ($char-foldcase c2)))
			   (({ch char?} . {ch* char?})
			    (%do-char-cmp ch ch* (lambda (x y)
						   (?case-insensitive-cmp x y))))
			   #| end of CASE-DEFINE* |# ))
			)))
  (define-char-cmp char-ci=?		$char=)
  (define-char-cmp char-ci<?		$char<)
  (define-char-cmp char-ci>?		$char>)
  (define-char-cmp char-ci<=?		$char<=)
  (define-char-cmp char-ci>=?		$char>=)
  #| end of LET-SYNTAX |# )

(case-define* char-ci!=?
  (({ch1 char?} {ch2 char?})
   ($char!= ($char-foldcase ch1) ($char-foldcase ch2)))

  (({ch1 char?} {ch2 char?} {ch3 char?})
   (let ((ch1 ($char-foldcase ch1))
	 (ch2 ($char-foldcase ch2))
	 (ch3 ($char-foldcase ch3)))
     (and ($char!= ch1 ch2)
	  ($char!= ch2 ch3)
	  ($char!= ch3 ch1))))

  (({ch1 char?} {ch2 char?} {ch3 char?} {ch4 char?} . {char* char?})
   ;;We must compare every argument to all the other arguments.
   (let outer-loop ((chX    ($char-foldcase ch1))
		    (char*  (cons* ($char-foldcase ch2)
				   ($char-foldcase ch3)
				   ($char-foldcase ch4)
				   (map $char-foldcase char*))))
     (let inner-loop ((chX    chX)
		      (char^* char*))
       (cond ((pair? char^*)
	      (and ($char!= chX ($car char^*))
		   (inner-loop chX ($cdr char^*))))
	     ((pair? char*)
	      (outer-loop (car char*) (cdr char*)))
	     (else #t)))))

  #| end of CASE-DEFINE* |# )

(define (%do-char-cmp a ls cmp)
  (let loop ((a  ($char-foldcase a))
	     (ls ls))
    (if (pair? ls)
	(let ((b ($char-foldcase (car ls))))
	  (if (cmp a b)
	      (loop b (cdr ls))
	    #f))
      #t)))


;;;; string operations

(define-syntax define-string-op
  (syntax-rules ()
    ((_ ?who ?unsafe-op)
     (define* (?who {str string?})
       (?unsafe-op str)))
    ))

;;; --------------------------------------------------------------------

(define-string-op string-upcase
  (lambda (s) ($string-change-case s $str-upcase)))

(define-string-op string-foldcase
  (lambda (s) ($string-change-case s $str-foldcase)))

(define-string-op string-downcase
  (lambda (s) ($string-change-case s $str-downcase)))

(define-string-op string-titlecase
  (lambda (str)
    (let* ((n (string-length str)) (dst (make-string n)))
      (define (trans s i c seen-cased? ac)
	(define (trans1 s i c/ls seen-cased? ac)
	  (define (trans2 s i seen-cased? ac)
	    (if (fx= i n)
		(handle-special dst ac)
	      (s i seen-cased? ac)))
	  (cond ((char? c/ls)
		 (string-set! dst i c/ls)
		 (trans2 s (fx+ i 1) seen-cased? ac))
		(else
		 (trans2 s (fx+ i 1) seen-cased? (cons (cons i c/ls) ac)))))
        (if seen-cased?
            (trans1 s i ($str-downcase c) #t ac)
	  (if ($char-cased? c)
	      (trans1 s i ($str-titlecase c) #t ac)
	    (trans1 s i c #f ac))))
      (define (s0 i ac)
        (let ((c (string-ref str i)))
          (cond
	   (($wb-aletter? c) (trans sAletter i c #f ac))
	   (($wb-numeric? c) (trans sNumeric i c #f ac))
	   (($wb-katakana? c) (trans sKatakana i c #f ac))
	   (($wb-extendnumlet? c) (trans sExtendnumlet i c #f ac))
	   (else (string-set! dst i c)
		 (let ((i (fx+ i 1)))
		   (if (fx= i n)
		       (handle-special dst ac)
		     (s0 i ac)))))))
      (define (sAletter i seen-cased? ac)
        (let ((c (string-ref str i)))
          (cond
	   ((or ($wb-aletter? c) ($wb-extend? c) ($wb-format? c))
	    (trans sAletter i c seen-cased? ac))
	   ((or ($wb-midletter? c) ($wb-midnumlet? c))
	    (trans sAletterMid i c seen-cased? ac))
	   (($wb-numeric? c) (trans sNumeric i c seen-cased? ac))
	   (($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac))
	   (else (s0 i ac)))))
      (define (sAletterMid i seen-cased? ac)
        (let ((c (string-ref str i)))
          (cond
	   ((or ($wb-extend? c) ($wb-format? c))
	    (trans sAletterMid i c seen-cased? ac))
	   (($wb-aletter? c) (trans sAletter i c seen-cased? ac))
	   (else (s0 i ac)))))
      (define (sNumeric i seen-cased? ac)
        (let ((c (string-ref str i)))
          (cond
	   ((or ($wb-numeric? c) ($wb-extend? c) ($wb-format? c))
	    (trans sNumeric      i c seen-cased? ac))
	   ((or ($wb-midnum? c) ($wb-midnumlet? c))
	    (trans sNumericMid   i c seen-cased? ac))
	   (($wb-aletter? c)
	    (trans sAletter      i c seen-cased? ac))
	   (($wb-extendnumlet? c)
	    (trans sExtendnumlet i c seen-cased? ac))
	   (else (s0 i ac)))))
      (define (sNumericMid i seen-cased? ac)
        (let ((c (string-ref str i)))
          (cond
	   ((or ($wb-extend? c) ($wb-format? c))
	    (trans sNumericMid i c seen-cased? ac))
	   (($wb-numeric? c) (trans sNumeric i c seen-cased? ac))
	   (else (s0 i ac)))))
      (define (sKatakana i seen-cased? ac)
        (let ((c (string-ref str i)))
          (cond
	   ((or ($wb-katakana? c) ($wb-extend? c) ($wb-format? c))
	    (trans sKatakana i c seen-cased? ac))
	   (($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac))
	   (else (s0 i ac)))))
      (define (sExtendnumlet i seen-cased? ac)
        (let ((c (string-ref str i)))
          (cond
	   ((or ($wb-extendnumlet? c) ($wb-extend? c) ($wb-format? c))
	    (trans sExtendnumlet i c seen-cased? ac))
	   (($wb-aletter? c) (trans sAletter i c seen-cased? ac))
	   (($wb-numeric? c) (trans sNumeric i c seen-cased? ac))
	   (($wb-katakana? c) (trans sKatakana i c seen-cased? ac))
	   (else (s0 i ac)))))
      (if (fx= n 0) dst (s0 0 '())))))

;;; --------------------------------------------------------------------

(let-syntax
    ((define-string-cmp (syntax-rules ()
			  ((_ ?who ?cmp)
			   (case-define* ?who
			     (({s1 string?} {s2 string?})
			      (?cmp s1 s2))
			     (({s1 string?} . {str* list-of-strings?})
			      (%do-string-cmp s1 str* ?cmp))
			     #| end of CASE-DEFINE* |# ))
			  )))

  (define-string-cmp string-ci=?
    $string-ci=?)

  (define-string-cmp string-ci<?
    (lambda (s1 s2) ($string-ci<? s1 s2)))

  (define-string-cmp string-ci<=?
    (lambda (s1 s2) (not ($string-ci<? s2 s1))))

  (define-string-cmp string-ci>=?
    (lambda (s1 s2) (not ($string-ci<? s1 s2))))

  (define-string-cmp string-ci>?
    (lambda (s1 s2) ($string-ci<? s2 s1)))

  #| end of LET-SYNTAX |# )

(define (%do-string-cmp a ls cmp)
  (let loop ((a a) (ls ls))
    (if (pair? ls)
	(let ((b (car ls)))
	  (and (cmp a b)
	       (loop b (cdr ls))))
      #t)))

;;; --------------------------------------------------------------------

(define-string-op string-normalize-nfd
  (lambda (s) ($decompose s #t)))

(define-string-op string-normalize-nfkd
  (lambda (s) ($decompose s #f)))

(define-string-op string-normalize-nfc
  (lambda (s) ($compose ($decompose s #t))))

(define-string-op string-normalize-nfkc
  (lambda (s) ($compose ($decompose s #f))))


;;;; unsafe string operations

(define ($string-ci=? s1 s2)
  (define str1.len ($string-length s1))
  (define str2.len ($string-length s2))
  (cond (($fxzero? str1.len)
	 ($fxzero? str2.len))
	(($fxzero? str2.len)
	 #f)
	(else
	 (let loop ((i1  1)
		    (i2  1)
		    (ch1 ($str-foldcase ($string-ref s1 0)))
		    (ch2 ($str-foldcase ($string-ref s2 0))))
	   ;;CH1 and CH2 can be: characters; lists holding a single character.
	   (cond ((char? ch1)
		  (if (char? ch2)
		      (and ($char= ch1 ch2)
			   (if ($fx= i1 str1.len)
			       ($fx= i2 str2.len)
			     (and (not ($fx= i2 str2.len))
				  (loop ($fxadd1 i1)
					($fxadd1 i2)
					($str-foldcase ($string-ref s1 i1))
					($str-foldcase ($string-ref s2 i2))))))
		    (and ($char= ch1 (car ch2))
			 (not ($fx= i1 str1.len))
			 (loop ($fxadd1 i1)
			       i2
			       ($str-foldcase ($string-ref s1 i1))
			       (cdr ch2)))))
		 ((char? ch2)
		  (and ($char= (car ch1) ch2)
		       (not ($fx= i2 str2.len))
		       (loop i1
			     ($fxadd1 i2)
			     (cdr ch1)
			     ($str-foldcase ($string-ref s2 i2)))))
		 (else
		  (and (char=? (car ch1) (car ch2))
		       (loop i1 i2 (cdr ch1) (cdr ch2)))))))))

(define ($string-ci<? s1 s2)
  (define str1.len ($string-length s1))
  (define str2.len ($string-length s2))
  (and (not ($fxzero? str2.len))
       (or ($fxzero? str1.len)
	   (let loop ((i1  1)
		      (i2  1)
		      (ch1 ($str-foldcase ($string-ref s1 0)))
		      (ch2 ($str-foldcase ($string-ref s2 0))))
	     ;;CH1 and CH2 can be: characters; lists holding a single character.
	     (cond ((char? ch1)
		    (if (char? ch2)
			(or ($char< ch1 ch2)
			    (and ($char= ch1 ch2)
				 (not ($fx= i2 str2.len))
				 (or ($fx= i1 str1.len)
				     (loop ($fxadd1 i1)
					   ($fxadd1 i2)
					   ($str-foldcase ($string-ref s1 i1))
					   ($str-foldcase ($string-ref s2 i2))))))
		      (or ($char< ch1 (car ch2))
			  (and ($char= ch1 (car ch2))
			       (or ($fx= i1 str1.len)
				   (loop ($fxadd1 i1)
					 i2
					 ($str-foldcase ($string-ref s1 i1))
					 (cdr ch2)))))))
		   ((char? ch2)
		    (or ($char< (car ch1) ch2)
			(and ($char= (car ch1) ch2)
			     (not ($fx= i2 str2.len))
			     (loop i1
				   ($fxadd1 i2)
				   (cdr ch1)
				   ($str-foldcase ($string-ref s2 i2))))))
		   (else
		    (or ($char< (car ch1) (car ch2))
			(and ($char= (car ch1) (car ch2))
			     (loop i1 i2 (cdr ch1) (cdr ch2))))))))))

(define ($string-change-case str cvt-char)
  (let ((n (string-length str)))
    (let f ((str str) (dst (make-string n)) (i 0) (n n) (ac '()))
      (cond
        ((fx= i n)
         (if (null? ac)
             dst
             (handle-special dst ac)))
        (else
         (let ((c/ls (cvt-char (string-ref str i))))
           (cond
             ((char? c/ls)
              (string-set! dst i c/ls)
              (f str dst (fx+ i 1) n ac))
             (else
              (f str dst (fx+ i 1) n
                 (cons (cons i c/ls) ac))))))))))


;;;; Unicode handling

(module (handle-special $compose $decompose)

  (define (handle-special str ac)

    (define (chars ac n)
      (cond
       ((null? ac) n)
       (else
	(chars (cdr ac)
	       (let f ((p (cdar ac)) (n n))
		 (cond
		  ((pair? p) (f (cdr p) (fx+ n 1)))
		  (else n)))))))

    (define (extend src ac src-len dst-len)
      (let f ((str str) (dst (make-string dst-len)) (i 0) (j 0) (ac (reverse ac)) (sigma* '()))
	(cond
	 ((null? ac)
	  (string-copy! str i dst j (fx- src-len i))
	  (do-sigmas dst sigma*))
	 (else
	  (let ((idx (caar ac)) (c* (cdar ac)) (ac (cdr ac)))
	    (let ((cnt (fx- idx i)))
	      (string-copy! str i dst j cnt)
	      (let g ((str str)       (dst dst)
		      (i (fx+ i cnt)) (j (fx+ j cnt))
		      (ac ac)         (c* c*))
		(cond
		 ((pair? c*)
		  (string-set! dst j (car c*))
		  (g str dst i (fx+ j 1) ac (cdr c*)))
		 ((char? c*)
		  (string-set! dst j c*)
		  (f str dst (fx+ i 1) (fx+ j 1) ac sigma*))
		 (else ; assume c* = sigma
		  (f str dst (fx+ i 1) (fx+ j 1) ac (cons j sigma*)))))))))))

    (define (do-sigmas str sigma*)
      (define nonfinal-sigma #\x3c3)
      (define final-sigma #\x3c2)
      (define (final? i)
	(define (scan i incr n)
	  (and (not (fx= i n))
	       (or ($char-cased? (string-ref str i))
		   (and ($char-case-ignorable? (string-ref str i))
			(scan (fx+ i incr) incr n)))))
	(and (scan (fx- i 1) -1 -1) (not (scan (fx+ i 1) +1 (string-length str)))))
		; scanning requires we have some character in place...guess nonfinal sigma
      (for-each (lambda (i) (string-set! str i nonfinal-sigma)) sigma*)
      (for-each (lambda (i) (when (final? i) (string-set! str i final-sigma))) sigma*)
      str)

    (let* ((src-len (string-length str))
	   (dst-len (chars ac src-len)))
      (if (fx= dst-len src-len)
	  (do-sigmas str (map car ac))
	(extend str ac src-len dst-len))))

;;; --------------------------------------------------------------------

  (module (hangul-sbase hangul-slimit $hangul-decomp
			hangul-lbase hangul-llimit
			hangul-vbase hangul-vlimit
			hangul-tbase hangul-tlimit
			hangul-vcount hangul-tcount)

    ;; adapted from UAX #15
    (define SBase		#xAC00)
    (define LBase		#x1100)
    (define VBase		#x1161)
    (define TBase		#x11A7)
    (define LCount		19)
    (define VCount		21)
    (define TCount		28)
    (define NCount		($fx* VCount TCount))
    (define SCount		($fx* LCount NCount))
    (define hangul-sbase	($fixnum->char SBase))
    (define hangul-slimit	($fixnum->char (+ SBase SCount -1)))
    (define hangul-lbase	($fixnum->char LBase))
    (define hangul-llimit	($fixnum->char (+ LBase LCount -1)))
    (define hangul-vbase	($fixnum->char VBase))
    (define hangul-vlimit	($fixnum->char (+ VBase VCount -1)))
    (define hangul-tbase	($fixnum->char TBase))
    (define hangul-tlimit	($fixnum->char (+ TBase TCount -1)))
    (define hangul-vcount	VCount)
    (define hangul-tcount	TCount)

    (define ($hangul-decomp c)
      (let ((SIndex ($char-fixnum-minus c hangul-sbase)))
	(let ((L ($fixnum->char (fx+ LBase (fxdiv SIndex NCount))))
	      (V ($fixnum->char (fx+ VBase (fxdiv (fxmod SIndex NCount) TCount))))
	      (adj (fxmod SIndex TCount)))
	  (if (fx= adj 0)
	      (cons* L V)
	    (cons* L V ($fixnum->char (fx+ TBase adj)))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define ($decompose s canonical?)
    ;;Should optimize for sequences of ascii characters.
    (let ((n (string-length s)) (ac '()))

      (define (canonical>? c1 c2)
	(fx> ($char-combining-class c1) ($char-combining-class c2)))

      (define (sort-and-flush comb*)
	(unless (null? comb*)
	  (set! ac (append (list-sort canonical>? comb*) ac))))

      (define ($char-decomp c)
	(if (and (char<=? hangul-sbase c) (char<=? c hangul-slimit))
	    ($hangul-decomp c)
	  (if canonical?
	      ($str-decomp-canon c)
	    ($str-decomp-compat c))))

      (define (push-and-go c* c** i comb*)
	(if (char? c*)
	    (go c* c** i comb*)
	  (go (car c*) (cons (cdr c*) c**) i comb*)))

      (define (pop-and-go c** i comb*)
	(if (null? c**)
	    (if (fx= i n)
		(sort-and-flush comb*)
	      (go (string-ref s i) '() (fx+ i 1) comb*))
	  (push-and-go (car c**) (cdr c**) i comb*)))

      (define (go c c** i comb*)
	(let ((c* ($char-decomp c)))
	  (if (eq? c c*) ; should be eqv?
	      (if (fxzero? ($char-combining-class c))
		  (begin
		    (sort-and-flush comb*)
		    (set! ac (cons c ac))
		    (pop-and-go c** i '()))
		(pop-and-go c** i (cons c comb*)))
	    (push-and-go c* c** i comb*))))

      (pop-and-go '() 0 '())
      (list->string (reverse ac))))

;;; --------------------------------------------------------------------

  (define $compose
    (let ((comp-table #f))

      (define (lookup-composite c1 c2)
	(hashtable-ref comp-table (cons c1 c2) #f))

      (define (init!)
	(set! comp-table
	      (make-hashtable (lambda (x)
				(fxxor (fxsll ($char->fixnum (car x))
					      7)
				       ($char->fixnum (cdr x))))
			      (lambda (x y)
				(and (char=? (car x) (car y))
				     (char=? (cdr x) (cdr y))))))
	(vector-for-each
	    (lambda (c* c) (hashtable-set! comp-table c* c))
	  (car ($composition-pairs))
	  (cdr ($composition-pairs))))

      (lambda (s)
	(unless comp-table
	  (init!))
	(let ((ac '())
	      (n  (string-length s)))

	  (define (dump c acc)
	    (set! ac (cons c ac))
	    (unless (null? acc) (set! ac (append acc ac))))

	  (define (s0 i)
	    (unless (fx= i n)
	      (let ((c (string-ref s i)))
		(if (fxzero? ($char-combining-class c))
		    (s1 (fx+ i 1) c)
                  (begin (set! ac (cons c ac)) (s0 (fx+ i 1)))))))

	  (define (s1 i c)
	    (if (fx= i n)
		(set! ac (cons c ac))
              (let ((c1 (string-ref s i)))
                (cond
		 ((and (and (char<=? hangul-lbase c)
			    (char<=? c hangul-llimit))
		       (and (char<=? hangul-vbase c1)
			    (char<=? c1 hangul-vlimit)))
		  (s1 (fx+ i 1)
		      (let ((lindex ($char-fixnum-minus c hangul-lbase))
			    (vindex ($char-fixnum-minus c1 hangul-vbase)))
			($fixnum->char
			 (fx+ ($char->fixnum hangul-sbase)
			      (fx* (fx+ (fx* lindex hangul-vcount) vindex)
				   hangul-tcount))))))
		 ((and (and (char<=? hangul-sbase c)
			    (char<=? c hangul-slimit))
		       (and (char<=? hangul-tbase c1)
			    (char<=? c1 hangul-tlimit))
		       (let ((sindex ($char-fixnum-minus c hangul-sbase)))
			 (fxzero? (fxmod sindex hangul-tcount))))
		  (let ((tindex ($char-fixnum-minus c1 hangul-tbase)))
		    (s1 (fx+ i 1) ($fixnum->char (fx+ ($char->fixnum c) tindex)))))
		 (else (s2 i c -1 '()))))))

	  (define (s2 i c class acc)
	    (if (fx= i n)
		(dump c acc)
              (let ((c1 (string-ref s i)))
                (let ((class1 ($char-combining-class c1)))
                  (cond ((and (fx< class class1) (lookup-composite c c1))
			 => (lambda (c)
			      (s2 (fx+ i 1) c class acc)))
			((fx= class1 0)
			 (dump c acc)
			 (s1 (fx+ i 1) c1))
			(else
			 (s2 (fx+ i 1) c class1 (cons c1 acc))))))))

	  (s0 0)
	  (list->string (reverse ac))))))

  #| end of module |# )


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.unicode end"))

#| end of library |# )

;;; end of file
