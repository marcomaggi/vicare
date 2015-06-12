;;;
;;;Part of: Vicare Scheme
;;;Contents: char-sets library
;;;Date: Thu Jun 11, 2015
;;;
;;;Abstract
;;;
;;;	Implementation of SRFI 14 on top of (vicare containers char-sets).
;;;
;;;Copyright (c) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (srfi :14 char-sets)
  (export
    ->char-set
    char-set
    char-set->list
    char-set->string
    char-set-adjoin
    char-set-adjoin!
    char-set-any
    char-set-complement
    char-set-complement!
    char-set-contains?
    char-set-copy
    char-set-count
    char-set-cursor
    char-set-cursor-next
    char-set-delete
    char-set-delete!
    char-set-diff+intersection
    char-set-diff+intersection!
    char-set-difference
    char-set-difference!
    char-set-every
    char-set-filter
    char-set-filter!
    char-set-fold
    char-set-for-each
    char-set-hash
    char-set-intersection
    char-set-intersection!
    char-set-map
    char-set-ref
    char-set-size
    char-set-unfold
    char-set-unfold!
    char-set-union
    char-set-union!
    char-set-xor
    char-set-xor!
    char-set:ascii
    char-set:empty
    char-set:full
    (rename (char-set:ascii/hex-digit			char-set:hex-digit)
	    (char-set:ascii/blank			char-set:blank)
	    (char-set:ascii/digit			char-set:digit)
	    (char-set:ascii/graphic			char-set:graphic)
	    (char-set:ascii/control			char-set:iso-control)
	    (char-set:ascii/letter			char-set:letter)
	    (char-set:ascii/letter+digit		char-set:letter+digit)
	    (char-set:ascii/lower-case			char-set:lower-case)
	    (char-set:ascii/upper-case			char-set:upper-case)
	    (char-set:ascii/printable			char-set:printing)
	    (char-set:ascii/punctuation			char-set:punctuation)
	    (char-set:ascii/symbol			char-set:symbol)
	    (char-set:category/letter-titlecase		char-set:title-case)
	    (char-set:ascii/whitespace			char-set:whitespace))
    (rename (char-set=?		char-set=)
	    (char-set-subset?	char-set<=))
    char-set?
    end-of-char-set?
    list->char-set
    list->char-set!
    string->char-set
    string->char-set!
    ucs-range->char-set
    ucs-range->char-set!)
  (import (vicare)
    (vicare system $fx)
    (vicare system $chars)
    (except (vicare containers char-sets)
	    char-set-ref
	    char-set-cursor-next)
    (prefix (only (vicare containers char-sets)
		  char-set-ref
		  char-set-cursor-next)
	    containers.)
    (only (vicare containers char-sets categories)
	  char-set:category/letter-titlecase))


;;;; helpers

(define (ucs-4-code-point? obj)
  (and (fixnum? obj)
       ($fx>= obj 0)
       ($fx<= obj #x10FFFF)
       (or ($fx<  obj #xD800)
	   ($fx>  obj #xDFFF))))


;;;; conversion to and from char-sets

(define string->char-set! string->char-set)

(define list->char-set! list->char-set)

(define* (->char-set obj)
  (cond ((string? obj)
	 (string->char-set obj))
	((char? obj)
	 (char-set obj))
	((char-set? obj)
	 obj)
	(else
	 (procedure-argument-violation __who__ "invalid argument" obj))))


;;;; UCS range to char-set

(case-define* ucs-range->char-set
  (({lower ucs-4-code-point?} {upper ucs-4-code-point?})
   ($ucs-range->char-set lower upper char-set:empty))
  (({lower ucs-4-code-point?} {upper ucs-4-code-point?} raise-error?)
   ($ucs-range->char-set lower upper char-set:empty))
  (({lower ucs-4-code-point?} {upper ucs-4-code-point?} raise-error? {base-cs char-set?})
   ($ucs-range->char-set lower upper base-cs)))

(define* ($ucs-range->char-set lower upper base-cs)
  (when (>= lower upper)
    (procedure-arguments-consistency-violation __who__
      "invalid order in UCS range limits" lower upper))
  (char-set-union base-cs
		  (char-set (cons ($fixnum->char lower)
				  ($fixnum->char ($fxsub1 upper))))))

(define ucs-range->char-set! ucs-range->char-set)


;;;; algebra

(define char-set-union!			char-set-union)
(define char-set-complement!		char-set-difference)
(define char-set-intersection!		char-set-intersection)
(define char-set-difference!		char-set-difference)
(define char-set-xor!			char-set-xor)
(define char-set-diff+intersection	char-set-difference+intersection)
(define char-set-diff+intersection!	char-set-diff+intersection)

(case-define char-set-unfold
  ((p f g seed)
   (char-set-unfold! p f g seed (char-set)))
  ((p f g seed base-cs)
   (char-set-unfold! p f g seed (char-set-copy base-cs))))

(define (char-set-unfold! p f g seed base-cs)
  (let lp ((seed seed)
	   (cs   base-cs))
    (if (p seed)
	cs
      (lp (g seed)
	  (char-set-add! cs (f seed))))))

(define char-set-filter!		char-set-filter)
(define char-set-delete!		char-set-delete)


;;;; cursors

(define* (char-set-ref {cs char-set?} {cursor char-set-cursor?})
  (containers.char-set-ref cursor))

(define* (char-set-cursor-next {cs char-set?} {cursor char-set-cursor?})
  (containers.char-set-cursor-next cursor))


;;;; done

#| end of library |# )

;;; end of file
