;;; Copyright (C) 2008  Abdulaziz Ghuloum, R. Kent Dybvig
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; Modified by Marco Maggi.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (ikarus.unicode)
  (export
    unicode-printable-char?
    char-upcase char-downcase char-titlecase char-foldcase
    char-whitespace? char-lower-case? char-upper-case?
    char-title-case?  char-numeric?
    char-alphabetic? char-general-category char-ci<? char-ci<=?
    char-ci=? char-ci>? char-ci>=? string-upcase string-downcase
    string-foldcase string-titlecase  string-ci<? string-ci<=?
    string-ci=? string-ci>? string-ci>=? string-normalize-nfd
    string-normalize-nfkd string-normalize-nfc string-normalize-nfkc )
  (import (except (ikarus)
		  unicode-printable-char?
		  char-upcase char-downcase char-titlecase char-foldcase
		  char-whitespace? char-lower-case? char-upper-case?
		  char-title-case?  char-numeric?
		  char-alphabetic? char-general-category char-ci<? char-ci<=?
		  char-ci=? char-ci>? char-ci>=? string-upcase string-downcase
		  string-foldcase string-titlecase  string-ci<? string-ci<=?
		  string-ci=? string-ci>? string-ci>=? string-normalize-nfd
		  string-normalize-nfkd string-normalize-nfc string-normalize-nfkc))



(module UNSAFE
  (fx< fx<= fx> fx>= fx= fx+ fx-
   fxior fxand fxsra fxsll fxzero?
   integer->char char->integer
   char<? char<=? char=? char>? char>=?
   string-ref string-set! string-length
   vector-ref vector-set! vector-length)
  (import
    (rename (ikarus system $strings)
      ($string-length string-length)
      ($string-ref    string-ref)
      ($string-set!   string-set!))
    (rename (ikarus system $vectors)
      ($vector-length vector-length)
      ($vector-ref    vector-ref)
      ($vector-set!   vector-set!))
    (rename (ikarus system $chars)
      ($char->fixnum char->integer)
      ($fixnum->char integer->char)
      ($char< char<?)
      ($char<= char<=?)
      ($char= char=?)
      ($char> char>?)
      ($char>= char>=?))
    (rename (ikarus system $fx)
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

(module
  (unicode-printable-char?
   char-upcase char-downcase char-titlecase char-foldcase
   char-whitespace? char-lower-case? char-upper-case?
   char-title-case?  char-numeric?
   char-alphabetic? char-general-category char-ci<? char-ci<=?
   char-ci=? char-ci>? char-ci>=? string-upcase string-downcase
   string-foldcase string-titlecase  string-ci<? string-ci<=?
   string-ci=? string-ci>? string-ci>=? string-normalize-nfd
   string-normalize-nfkd string-normalize-nfc string-normalize-nfkc)

(import UNSAFE)
(define (fxlogtest x y)
  (not (fxzero? (fxand x y))))
(define (char- x y)
  (fx- (char->integer x) (char->integer y)))

(include "unicode/unicode-char-cases.ss" #t)
(include "unicode/unicode-charinfo.ss"   #t)

(define-syntax define-char-op
  (syntax-rules ()
    [(_ name unsafe-op)
     (define name
       (lambda (c)
         (if (char? c)
             (unsafe-op c)
             (assertion-violation 'name "not a char" c))))]))

(define-char-op char-upcase $char-upcase)
(define-char-op char-downcase $char-downcase)
(define-char-op char-titlecase $char-titlecase)
(define-char-op char-foldcase $char-foldcase)
(define-char-op char-whitespace? $char-whitespace?)
(define-char-op char-lower-case? $char-lower-case?)
(define-char-op char-upper-case? $char-upper-case?)
(define-char-op char-title-case? $char-title-case?)
(define-char-op char-numeric? $char-numeric?)
(define-char-op unicode-printable-char? $char-constituent?)
(define-char-op char-alphabetic? $char-alphabetic?)
(define-char-op char-general-category $char-category)

(define (do-char-cmp a ls cmp who)
  (if (char? a)
      (let f ([a ($char-foldcase a)] [ls ls])
        (cond
          [(null? ls) #t]
          [else
           (let ([b (car ls)])
             (if (char? b)
                 (let ([b ($char-foldcase b)])
                   (if (cmp a b)
                       (f b (cdr ls))
                       (let f ([ls (cdr ls)])
                         (if (null? ls)
                             #f
                             (if (char? (car ls))
                                 (f (cdr ls))
                                 (assertion-violation who
                                   "not a char" (car ls)))))))
                 (assertion-violation who "not a char" b)))]))
      (assertion-violation who "not a char" a)))

(define-syntax define-char-cmp
  (syntax-rules ()
    [(_ name cmp)
     (define name
       (case-lambda
         [(c1 c2)
          (if (char? c1)
              (if (char? c2)
                  (cmp ($char-foldcase c1) ($char-foldcase c2))
                  (assertion-violation 'name "not a char" c2))
              (assertion-violation 'name "not a char" c1))]
         [(c1 . rest)
          (do-char-cmp c1 rest (lambda (x y) (cmp x y)) 'name)]))]))

(define-char-cmp char-ci<? char<?)
(define-char-cmp char-ci<=? char<=?)
(define-char-cmp char-ci=? char=?)
(define-char-cmp char-ci>? char>?)
(define-char-cmp char-ci>=? char>=?)

(define (handle-special str ac)
  (define (chars ac n)
    (cond
      [(null? ac) n]
      [else
       (chars (cdr ac)
         (let f ([p (cdar ac)] [n n])
           (cond
             [(pair? p) (f (cdr p) (fx+ n 1))]
             [else n])))]))
  (define (extend src ac src-len dst-len)
    (let f ([str str] [dst (make-string dst-len)] [i 0] [j 0] [ac (reverse ac)] [sigma* '()])
      (cond
        [(null? ac)
         (string-copy! str i dst j (fx- src-len i))
         (do-sigmas dst sigma*)]
        [else
         (let ([idx (caar ac)] [c* (cdar ac)] [ac (cdr ac)])
           (let ([cnt (fx- idx i)])
             (string-copy! str i dst j cnt)
             (let g ([str str]       [dst dst]
                     [i (fx+ i cnt)] [j (fx+ j cnt)]
                     [ac ac]         [c* c*])
               (cond
                 [(pair? c*)
                  (string-set! dst j (car c*))
                  (g str dst i (fx+ j 1) ac (cdr c*))]
                 [(char? c*)
                  (string-set! dst j c*)
                  (f str dst (fx+ i 1) (fx+ j 1) ac sigma*)]
                 [else ; assume c* = sigma
                  (f str dst (fx+ i 1) (fx+ j 1) ac (cons j sigma*))]))))])))
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
  (let* ([src-len (string-length str)]
         [dst-len (chars ac src-len)])
     (if (fx= dst-len src-len)
         (do-sigmas str (map car ac))
         (extend str ac src-len dst-len))))

(define ($string-change-case str cvt-char)
  (let ([n (string-length str)])
    (let f ([str str] [dst (make-string n)] [i 0] [n n] [ac '()])
      (cond
        [(fx= i n)
         (if (null? ac)
             dst
             (handle-special dst ac))]
        [else
         (let ([c/ls (cvt-char (string-ref str i))])
           (cond
             [(char? c/ls)
              (string-set! dst i c/ls)
              (f str dst (fx+ i 1) n ac)]
             [else
              (f str dst (fx+ i 1) n
                 (cons (cons i c/ls) ac))]))]))))



(define-syntax define-string-op
  (syntax-rules ()
    [(_ name unsafe-op)
     (define name
       (lambda (s)
         (if (string? s)
             (unsafe-op s)
             (assertion-violation 'name "not a string" s))))]))

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


(define $string-ci=? ; two arguments, no string? checks
  (lambda (s1 s2)
    (let ([n1 (string-length s1)] [n2 (string-length s2)])
      (if (fx= n1 0)
          (fx= n2 0)
          (and (not (fx= n2 0))
            (let f ([i1 1]
                    [i2 1]
                    [c1* ($str-foldcase (string-ref s1 0))]
                    [c2* ($str-foldcase (string-ref s2 0))])
              (if (char? c1*)
                  (if (char? c2*)
                      (and (char=? c1* c2*)
                           (if (fx= i1 n1)
                               (fx= i2 n2)
                               (and (not (fx= i2 n2))
                                    (f (fx+ i1 1) (fx+ i2 1)
                                       ($str-foldcase (string-ref s1 i1))
                                       ($str-foldcase (string-ref s2 i2))))))
                      (and (char=? c1* (car c2*))
                           (not (fx= i1 n1))
                           (f (fx+ i1 1) i2
                              ($str-foldcase (string-ref s1 i1))
                              (cdr c2*))))
                  (if (char? c2*)
                      (and (char=? (car c1*) c2*)
                           (not (fx= i2 n2))
                           (f i1 (fx+ i2 1) (cdr c1*)
                              ($str-foldcase (string-ref s2 i2))))
                      (and (char=? (car c1*) (car c2*))
                           (f i1 i2 (cdr c1*) (cdr c2*)))))))))))

(define $string-ci<? ; two arguments, no string? checks
  (lambda (s1 s2)
    (let ([n1 (string-length s1)] [n2 (string-length s2)])
      (and (not (fx= n2 0))
           (or (fx= n1 0)
               (let f ([i1 1]
                       [i2 1]
                       [c1* ($str-foldcase (string-ref s1 0))]
                       [c2* ($str-foldcase (string-ref s2 0))])
                 (if (char? c1*)
                     (if (char? c2*)
                         (or (char<? c1* c2*)
                             (and (char=? c1* c2*)
                                  (not (fx= i2 n2))
                                  (or (fx= i1 n1)
                                      (f (fx+ i1 1) (fx+ i2 1)
                                         ($str-foldcase (string-ref s1 i1))
                                         ($str-foldcase (string-ref s2 i2))))))
                         (or (char<? c1* (car c2*))
                             (and (char=? c1* (car c2*))
                                  (or (fx= i1 n1)
                                      (f (fx+ i1 1) i2
                                         ($str-foldcase (string-ref s1 i1))
                                         (cdr c2*))))))
                     (if (char? c2*)
                         (or (char<? (car c1*) c2*)
                             (and (char=? (car c1*) c2*)
                                  (not (fx= i2 n2))
                                  (f i1 (fx+ i2 1) (cdr c1*)
                                     ($str-foldcase (string-ref s2 i2)))))
                         (or (char<? (car c1*) (car c2*))
                             (and (char=? (car c1*) (car c2*))
                                  (f i1 i2 (cdr c1*) (cdr c2*))))))))))))



(define (do-string-cmp a ls cmp who)
  (if (string? a)
      (let f ([a a] [ls ls])
        (cond
          [(null? ls) #t]
          [else
           (let ([b (car ls)])
             (if (string? b)
                 (if (cmp a b)
                     (f b (cdr ls))
                     (let f ([ls (cdr ls)])
                       (if (null? ls)
                           #f
                           (if (string? (car ls))
                               (f (cdr ls))
                               (assertion-violation who
                                 "not a string" (car ls)))))))
                 (assertion-violation who "not a string" b))]))
      (assertion-violation who "not a string" a)))

(define-syntax define-string-cmp
  (syntax-rules ()
    [(_ name cmp)
     (define name
       (case-lambda
         [(s1 s2)
          (if (string? s1)
              (if (string? s2)
                  (cmp s1 s2)
                  (assertion-violation 'name "not a string" s2))
              (assertion-violation 'name "not a string" s2))]
         [(s1 . rest)
          (do-string-cmp s1 rest cmp 'name)]))]))

(define-string-cmp string-ci=? $string-ci=?)
(define-string-cmp string-ci<?
  (lambda (s1 s2) ($string-ci<? s1 s2)))
(define-string-cmp string-ci<=?
  (lambda (s1 s2) (not ($string-ci<? s2 s1))))
(define-string-cmp string-ci>=?
  (lambda (s1 s2) (not ($string-ci<? s1 s2))))
(define-string-cmp string-ci>?
  (lambda (s1 s2) ($string-ci<? s2 s1)))

(module (hangul-sbase hangul-slimit $hangul-decomp
         hangul-lbase hangul-llimit
         hangul-vbase hangul-vlimit
         hangul-tbase hangul-tlimit
         hangul-vcount hangul-tcount)
 ; adapted from UAX #15
  (define SBase #xAC00)
  (define LBase #x1100)
  (define VBase #x1161)
  (define TBase #x11A7)
  (define LCount 19)
  (define VCount 21)
  (define TCount 28)
  (define NCount (* VCount TCount))
  (define SCount (* LCount NCount))
  (define hangul-sbase (integer->char SBase))
  (define hangul-slimit (integer->char (+ SBase SCount -1)))
  (define hangul-lbase (integer->char LBase))
  (define hangul-llimit (integer->char (+ LBase LCount -1)))
  (define hangul-vbase (integer->char VBase))
  (define hangul-vlimit (integer->char (+ VBase VCount -1)))
  (define hangul-tbase (integer->char TBase))
  (define hangul-tlimit (integer->char (+ TBase TCount -1)))
  (define hangul-vcount VCount)
  (define hangul-tcount TCount)
  (define ($hangul-decomp c)
    (let ([SIndex (char- c hangul-sbase)])
      (let ([L (integer->char (fx+ LBase (fxdiv SIndex NCount)))]
            [V (integer->char (fx+ VBase (fxdiv (fxmod SIndex NCount) TCount)))]
            [adj (fxmod SIndex TCount)])
        (if (fx= adj 0)
            (cons* L V)
            (cons* L V (integer->char (fx+ TBase adj))))))))

(define $decompose
 ; might should optimize for sequences of ascii characters
  (lambda (s canonical?)
    (let ([n (string-length s)] [ac '()])
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
        (let ([c* ($char-decomp c)])
          (if (eq? c c*) ; should be eqv?
              (if (fxzero? ($char-combining-class c))
                  (begin
                    (sort-and-flush comb*)
                    (set! ac (cons c ac))
                    (pop-and-go c** i '()))
                  (pop-and-go c** i (cons c comb*)))
              (push-and-go c* c** i comb*))))
      (pop-and-go '() 0 '())
      (list->string (reverse ac)))))

(define $compose
  (let ([comp-table #f])
    (define (lookup-composite c1 c2)
      (hashtable-ref comp-table (cons c1 c2) #f))
    (define (init!)
      (set! comp-table
        (make-hashtable
          (lambda (x)
            (fxxor
              (fxsll (char->integer (car x)) 7)
              (char->integer (cdr x))))
          (lambda (x y)
            (and (char=? (car x) (car y))
                 (char=? (cdr x) (cdr y))))))
      (vector-for-each
        (lambda (c* c) (hashtable-set! comp-table c* c))
        (car ($composition-pairs))
        (cdr ($composition-pairs))))
    (lambda (s)
      (unless comp-table (init!))
      (let ([ac '()] [n (string-length s)])
        (define (dump c acc)
          (set! ac (cons c ac))
          (unless (null? acc) (set! ac (append acc ac))))
        (define (s0 i)
          (unless (fx= i n)
            (let ([c (string-ref s i)])
              (if (fxzero? ($char-combining-class c))
                  (s1 (fx+ i 1) c)
                  (begin (set! ac (cons c ac)) (s0 (fx+ i 1)))))))
        (define (s1 i c)
          (if (fx= i n)
              (set! ac (cons c ac))
              (let ([c1 (string-ref s i)])
                (cond
                  [(and (and (char<=? hangul-lbase c)
                             (char<=? c hangul-llimit))
                        (and (char<=? hangul-vbase c1)
                             (char<=? c1 hangul-vlimit)))
                   (s1 (fx+ i 1)
                       (let ([lindex (char- c hangul-lbase)]
                             [vindex (char- c1 hangul-vbase)])
                         (integer->char
                           (fx+ (char->integer hangul-sbase)
                                (fx* (fx+ (fx* lindex hangul-vcount) vindex)
                                     hangul-tcount)))))]
                  [(and (and (char<=? hangul-sbase c)
                             (char<=? c hangul-slimit))
                        (and (char<=? hangul-tbase c1)
                             (char<=? c1 hangul-tlimit))
                        (let ([sindex (char- c hangul-sbase)])
                          (fxzero? (fxmod sindex hangul-tcount))))
                   (let ([tindex (char- c1 hangul-tbase)])
                     (s1 (fx+ i 1) (integer->char (fx+ (char->integer c) tindex))))]
                  [else (s2 i c -1 '())]))))
        (define (s2 i c class acc)
          (if (fx= i n)
              (dump c acc)
              (let ([c1 (string-ref s i)])
                (let ([class1 ($char-combining-class c1)])
                  (cond
                    [(and (fx< class class1) (lookup-composite c c1)) =>
                     (lambda (c) (s2 (fx+ i 1) c class acc))]
                    [(fx= class1 0)
                     (dump c acc)
                     (s1 (fx+ i 1) c1)]
                    [else (s2 (fx+ i 1) c class1 (cons c1 acc))])))))
        (s0 0)
        (list->string (reverse ac))))))

(define-string-op string-normalize-nfd
  (lambda (s) ($decompose s #t)))

(define-string-op string-normalize-nfkd
  (lambda (s) ($decompose s #f)))

(define-string-op string-normalize-nfc
  (lambda (s) ($compose ($decompose s #t))))

(define-string-op string-normalize-nfkc
  (lambda (s) ($compose ($decompose s #f))))

))
