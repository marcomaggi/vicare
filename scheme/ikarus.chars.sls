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

#!vicare
(library (ikarus chars)
  (export
    char->integer		integer->char
    char=?			char!=?
    char<?			char<=?
    char>?			char>=?

    chmax			chmin

    char-in-ascii-range?	list-of-chars?

    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Sat Nov 22,
    ;;2014)
    $char!=


    ;; unsafe operations for (vicare system $chars)
    $chmax			$chmin)
  (import (except (vicare)
		  char->integer		integer->char
		  char=?		char!=?
		  char<?		char<=?
		  char>?		char>=?
		  chmax			chmin
		  char-in-ascii-range?	list-of-chars?)
    (vicare system $fx)
    (except (vicare system $chars)
	    $chmax
	    $chmin)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate))


;;;; predicates

(define-list-of-type-predicate list-of-chars? char?)


(define* (integer->char {fx fixnum-in-character-range?})
  ;;Defined by R6RS.   N must be a  Unicode scalar value, i.e.,  a non-negative exact
  ;;integer object in [0, #xD7FF] union [#xE000, #x10FFFF].
  ;;
  ;;For a Unicode scalar value N, INTEGER->CHAR returns its associated character.
  ;;
  ($fixnum->char fx))

(define* (char->integer {ch char?})
  ;;Defined by  R6RS.  Given  a character, CHAR->INTEGER  returns its  Unicode scalar
  ;;value as an exact integer object.
  ;;
  ($char->fixnum ch))


;;;; comparison

(define-equality/sorting-predicate char=?	$char=	char? list-of-chars?)
(define-equality/sorting-predicate char<?	$char<	char? list-of-chars?)
(define-equality/sorting-predicate char<=?	$char<=	char? list-of-chars?)
(define-equality/sorting-predicate char>?	$char>	char? list-of-chars?)
(define-equality/sorting-predicate char>=?	$char>=	char? list-of-chars?)
(define-inequality-predicate       char!=?	$char!=	char? list-of-chars?)

(define ($char!= ch1 ch2)
  ;;FIXME This is  also a primitive operation.   At the next boot  image rotation the
  ;;implementation must be changed to:
  ;;
  ;;   (import (prefix (vicare system $chars) sys.))
  ;;   (sys.$char!= ch1 ch2)
  ;;
  ;;(Marco Maggi; Wed Mar 25, 2015)
  (not ($char= ch1 ch2)))


;;;; min max

(define-min/max-comparison chmax $chmax char? list-of-chars?)
(define-min/max-comparison chmin $chmin char? list-of-chars?)

;;FIXME This should be a proper primitive operation.  (Marco Maggi; Fri Mar 27, 2015)
;;
(define ($chmin ch1 ch2)
  (if ($char< ch1 ch2) ch1 ch2))

;;FIXME This should be a proper primitive operation.  (Marco Maggi; Fri Mar 27, 2015)
;;
(define ($chmax ch1 ch2)
  (if ($char< ch1 ch2) ch2 ch1))


;;;; miscellaneous functions

(define (char-in-ascii-range? obj)
  ;;Defined by Vicare.  Return #t if OBJ is a character and its Unicode code point is
  ;;in the range [0, 127]; otherwise return #f.
  ;;
  (and (char? obj)
       (let ((chi ($char->fixnum obj)))
	 (and ($fx>= chi 0)
	      ($fx<= chi 127)))))


;;;; done

#| end of library |# )


(library (vicare system chars)
  (export $char= $char->fixnum $fixnum->char)
  (import (vicare))
  (define $char=	char=?)
  (define $char->fixnum char->integer)
  (define $fixnum->char integer->char))

;;; end of file
