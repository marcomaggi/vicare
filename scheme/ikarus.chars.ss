;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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


(library (ikarus chars)
  (export char=? char<? char<=? char>? char>=? char->integer integer->char)
  (import 
    (except (ikarus)
      char=? char<? char<=? char>? char>=?  integer->char char->integer)
    (ikarus system $pairs)
    (ikarus system $chars)
    (ikarus system $fx))

  (define integer->char
    (lambda (n)
      (cond
        [(not (fixnum? n)) (die 'integer->char "invalid argument" n)]
        [($fx< n 0) (die 'integer->char "negative" n)]
        [($fx<= n #xD7FF) ($fixnum->char n)]
        [($fx< n #xE000)
         (die 'integer->char "integer does not have a unicode representation" n)]
        [($fx<= n #x10FFFF) ($fixnum->char n)]
        [else (die 'integer->char 
                "integer does not have a unicode representation" n)])))
  
  (define char->integer 
    (lambda (x) 
      (unless (char? x)
        (die 'char->integer "not a character" x))
      ($char->fixnum x)))

  ;;; FIXME: this file is embarrasing
  (define char=?
    (let ()
      (define (err x)
        (die 'char=? "not a character" x))
      (case-lambda
        [(c1 c2)
         (if (char? c1)
             (if (char? c2)
                 ($char= c1 c2)
                 (err c2))
             (err c1))]
        [(c1 c2 c3)
         (if (char? c1)
             (if (char? c2)
                 (if (char? c3)
                     (and ($char= c1 c2)
                          ($char= c2 c3))
                     (err c3))
                 (err c2))
             (err c1))]
        [(c1 . c*)
         (if (char? c1)
             (let f ([c* c*])
               (or (null? c*) 
                   (let ([c2 ($car c*)])
                     (if (char? c2)
                         (if ($char= c1 c2)
                             (f ($cdr c*))
                             (let g ([c* ($cdr c*)])
                               (if (null? c*)
                                   #f
                                   (if (char? ($car c*))
                                       (g ($cdr c*))
                                       (err ($car c*))))))
                         (err c2)))))
             (err c1))])))

  (define char<?
    (let ()
      (define (err x)
        (die 'char<? "not a character" x))
      (case-lambda
        [(c1 c2)
         (if (char? c1)
             (if (char? c2)
                 ($char< c1 c2)
                 (err c2))
             (err c1))]
        [(c1 c2 c3)
         (if (char? c1)
             (if (char? c2)
                 (if (char? c3)
                     (and ($char< c1 c2)
                          ($char< c2 c3))
                     (err c3))
                 (err c2))
             (err c1))]
        [(c1 . c*)
         (if (char? c1)
             (let f ([c1 c1] [c* c*])
               (or (null? c*) 
                   (let ([c2 ($car c*)])
                     (if (char? c2)
                         (if ($char< c1 c2)
                             (f c2 ($cdr c*))
                             (let g ([c* ($cdr c*)])
                               (if (null? c*)
                                   #f
                                   (if (char? ($car c*))
                                       (g ($cdr c*))
                                       (err ($car c*))))))
                         (err c2)))))
             (err c1))])))

  (define char<=?
    (let ()
      (define (err x)
        (die 'char<=? "not a character" x))
      (case-lambda
        [(c1 c2)
         (if (char? c1)
             (if (char? c2)
                 ($char<= c1 c2)
                 (err c2))
             (err c1))]
        [(c1 c2 c3)
         (if (char? c1)
             (if (char? c2)
                 (if (char? c3)
                     (and ($char<= c1 c2)
                          ($char<= c2 c3))
                     (err c3))
                 (err c2))
             (err c1))]
        [(c1 . c*)
         (if (char? c1)
             (let f ([c1 c1] [c* c*])
               (or (null? c*) 
                   (let ([c2 ($car c*)])
                     (if (char? c2)
                         (if ($char<= c1 c2)
                             (f c2 ($cdr c*))
                             (let g ([c* ($cdr c*)])
                               (if (null? c*)
                                   #f
                                   (if (char? ($car c*))
                                       (g ($cdr c*))
                                       (err ($car c*))))))
                         (err c2)))))
             (err c1))])))

  (define char>?
    (let ()
      (define (err x)
        (die 'char>? "not a character" x))
      (case-lambda
        [(c1 c2)
         (if (char? c1)
             (if (char? c2)
                 ($char> c1 c2)
                 (err c2))
             (err c1))]
        [(c1 c2 c3)
         (if (char? c1)
             (if (char? c2)
                 (if (char? c3)
                     (and ($char> c1 c2)
                          ($char> c2 c3))
                     (err c3))
                 (err c2))
             (err c1))]
        [(c1 . c*)
         (if (char? c1)
             (let f ([c1 c1] [c* c*])
               (or (null? c*) 
                   (let ([c2 ($car c*)])
                     (if (char? c2)
                         (if ($char> c1 c2)
                             (f c2 ($cdr c*))
                             (let g ([c* ($cdr c*)])
                               (if (null? c*)
                                   #f
                                   (if (char? ($car c*))
                                       (g ($cdr c*))
                                       (err ($car c*))))))
                         (err c2)))))
             (err c1))])))

  (define char>=?
    (let ()
      (define (err x)
        (die 'char>=? "not a character" x))
      (case-lambda
        [(c1 c2)
         (if (char? c1)
             (if (char? c2)
                 ($char>= c1 c2)
                 (err c2))
             (err c1))]
        [(c1 c2 c3)
         (if (char? c1)
             (if (char? c2)
                 (if (char? c3)
                     (and ($char>= c1 c2)
                          ($char>= c2 c3))
                     (err c3))
                 (err c2))
             (err c1))]
        [(c1 . c*)
         (if (char? c1)
             (let f ([c1 c1] [c* c*])
               (or (null? c*) 
                   (let ([c2 ($car c*)])
                     (if (char? c2)
                         (if ($char>= c1 c2)
                             (f c2 ($cdr c*))
                             (let g ([c* ($cdr c*)])
                               (if (null? c*)
                                   #f
                                   (if (char? ($car c*))
                                       (g ($cdr c*))
                                       (err ($car c*))))))
                         (err c2)))))
             (err c1))])))
)

(library (ikarus system chars)
  (export $char->fixnum $fixnum->char)
  (import (ikarus))
  (define $char->fixnum char->integer)
  (define $fixnum->char integer->char))

