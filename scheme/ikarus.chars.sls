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
(library (ikarus.chars)
  (options typed-language)
  (export
    char->integer		;;~char->integer
    integer->char		;;~integer->char
    char=?			;;~char=?
    char!=?			;;~char!=?
    char<?			;;~char<?
    char>?			;;~char>?
    char<=?			;;~char<=?
    char>=?			;;~char>=?

    chmax			;;~chmax
    chmin			;;~chmin

    char-in-ascii-range?	list-of-chars?

    ;; unsafe operations for (vicare system $chars)
    $char=			$char!=
    $char<			$char>
    $char<=			$char>=
    $chmax			$chmin
    #| end of EXPORT |# )
  (import (except (vicare)
		  char->integer		integer->char
		  char=?		char!=?
		  char<?		char<=?
		  char>?		char>=?
		  chmax			chmin
		  char-in-ascii-range?	list-of-chars?)
    (vicare system $fx)
    (prefix (vicare system $chars)
	    sys::)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define/checked-min/max-comparison
	  define/checked-equality/sorting-predicate
	  define/checked-inequality-predicate
	  define/typed-unsafe-equality/sorting-predicate
	  define/typed-unsafe-inequality-predicate
	  define/typed-unsafe-min/max-comparison))


;;;; predicates

(define-list-of-type-predicate list-of-chars? char?)


(define (integer->char {fx <non-negative-fixnum>})
  ;;Defined by R6RS.   N must be a  Unicode scalar value, i.e.,  a non-negative exact
  ;;integer object in [0, #xD7FF] union [#xE000, #x10FFFF].
  ;;
  ;;For a Unicode scalar value N, INTEGER->CHAR returns its associated character.
  ;;
  (if (fixnum-in-character-range? fx)
      (sys::$fixnum->char fx)
    (procedure-argument-violation __who__ "fixnum outside of character range" fx)))

(define ({char->integer <non-negative-fixnum>} {ch <char>})
  ;;Defined by  R6RS.  Given  a character, CHAR->INTEGER  returns its  Unicode scalar
  ;;value as an exact integer object.
  ;;
  (sys::$char->fixnum ch))


;;;; comparison

(define/checked-equality/sorting-predicate char=?	sys::$char=	<char>)
(define/checked-equality/sorting-predicate char<?	sys::$char<	<char>)
(define/checked-equality/sorting-predicate char<=?	sys::$char<=	<char>)
(define/checked-equality/sorting-predicate char>?	sys::$char>	<char>)
(define/checked-equality/sorting-predicate char>=?	sys::$char>=	<char>)
(define/checked-inequality-predicate       char!=?	sys::$char!=	<char>)

(define/typed-unsafe-equality/sorting-predicate $char=	sys::$char=	<char>)
(define/typed-unsafe-equality/sorting-predicate $char<	sys::$char<	<char>)
(define/typed-unsafe-equality/sorting-predicate $char>	sys::$char>	<char>)
(define/typed-unsafe-equality/sorting-predicate $char<=	sys::$char<=	<char>)
(define/typed-unsafe-equality/sorting-predicate $char>=	sys::$char>=	<char>)
(define/typed-unsafe-inequality-predicate $char!=	sys::$char!=	<char>)


;;;; min max

(define/checked-min/max-comparison chmax $chmax <char>)
(define/checked-min/max-comparison chmin $chmin <char>)

(define/typed-unsafe-min/max-comparison $chmin	unsafe-chmin	<char>)
(define/typed-unsafe-min/max-comparison $chmax	unsafe-chmax	<char>)

;;FIXME This should be a proper primitive operation.  (Marco Maggi; Fri Mar 27, 2015)
;;
(define-syntax-rule (unsafe-chmin ch1 ch2)
  (if (sys::$char< ch1 ch2) ch1 ch2))

;;FIXME This should be a proper primitive operation.  (Marco Maggi; Fri Mar 27, 2015)
;;
(define-syntax-rule (unsafe-chmax ch1 ch2)
  (if (sys::$char< ch1 ch2) ch2 ch1))


;;;; miscellaneous functions

(define/typed ({char-in-ascii-range? <boolean>} obj)
  ;;Defined by Vicare.  Return #t if OBJ is a character and its Unicode code point is
  ;;in the range [0, 127]; otherwise return #f.
  ;;
  (and (char? obj)
       (let ((chi (sys::$char->fixnum obj)))
	 (and ($fx>= chi 0)
	      ($fx<= chi 127)))))


;;;; done

#| end of library |# )

;;; end of file
