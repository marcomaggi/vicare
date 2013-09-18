;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: OOPP API to extended lists functions
;;;Date: Wed Sep 18, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa containers lists)
  (export <xlist>)
  (import (except (nausicaa)
		  break)
    (vicare containers lists))


(define-label <xlist>

  ;; constructors
  (method-syntax list-copy
    (syntax-rules ()
      ((_ o)
       (list-copy o))))

  (method-syntax tree-copy
    (syntax-rules ()
      ((_ o)
       (tree-copy o))))

  ;; circular lists
  (method-syntax circular-list!
    (syntax-rules ()
      ((_ o)
       (list->circular-list! o))))

  (method-syntax break-circular-list!
    (syntax-rules ()
      ((_ o)
       (circular-list->list! o))))

  (method-syntax circular-list-copy
    (syntax-rules ()
      ((_ o)
       (circular-list-copy o))))

  (method-syntax circular-list-length
    (syntax-rules ()
      ((_ o)
       (circular-list-length o))))

;;;  circular-list=

  ;; predicates
  (method-syntax circular-list?
    (syntax-rules ()
      ((_ o)
       (circular-list? o))))

  (method-syntax circular-list?/or-null
    (syntax-rules ()
      ((_ o)
       (circular-list?/or-null o))))

  (method-syntax dotted-list?
    (syntax-rules ()
      ((_ o)
       (dotted-list? o))))

  (method-syntax dotted-list?/or-null
    (syntax-rules ()
      ((_ o)
       (dotted-list?/or-null o))))


  ;; selectors
  (method-syntax car+cdr
    (syntax-rules ()
      ((_ o)
       (car+cdr o))))

  (method-syntax first
    (syntax-rules ()
      ((_ o)
       (first o))))

  (method-syntax second
    (syntax-rules ()
      ((_ o)
       (second o))))

  (method-syntax third
    (syntax-rules ()
      ((_ o)
       (third o))))

  (method-syntax fourth
    (syntax-rules ()
      ((_ o)
       (fourth o))))

  (method-syntax fifth
    (syntax-rules ()
      ((_ o)
       (fifth o))))

  (method-syntax sixth
    (syntax-rules ()
      ((_ o)
       (sixth o))))

  (method-syntax seventh
    (syntax-rules ()
      ((_ o)
       (seventh o))))

  (method-syntax eighth
    (syntax-rules ()
      ((_ o)
       (eighth o))))

  (method-syntax ninth
    (syntax-rules ()
      ((_ o)
       (ninth o))))

  (method-syntax tenth
    (syntax-rules ()
      ((_ o)
       (tenth o))))

  (method-syntax take-left
    (syntax-rules ()
      ((_ o i)
       (take-left o i))))

  (method-syntax take-right
    (syntax-rules ()
      ((_ o i)
       (take-right o i))))

  (method-syntax take-left!
    (syntax-rules ()
      ((_ o i)
       (set! o (take-left! o i)))))

  (method-syntax drop-left
    (syntax-rules ()
      ((_ o i)
       (drop-left o i))))

  (method-syntax drop-right
    (syntax-rules ()
      ((_ o i)
       (drop-right o i))))

  (method-syntax drop-right!
    (syntax-rules ()
      ((_ o i)
       (set! o (drop-right! o i)))))

  (method-syntax split-at
    (syntax-rules ()
      ((_ o i)
       (split-at o i))))

  (method-syntax split-at!
    (syntax-rules ()
      ((_ o i)
       (split-at! o i))))

  (method-syntax last
    (syntax-rules ()
      ((_ o)
       (last o))))

  (method-syntax last-pair
    (syntax-rules ()
      ((_ o)
       (last-pair o))))

  ;; misc
  (method-syntax length+
    (syntax-rules ()
      ((_ o)
       (length+ o))))

  (method-syntax append!
    (syntax-rules ()
      ((_ o . ?rest)
       (set! o (append! o . ?rest)))))

  (method-syntax reverse!
    (syntax-rules ()
      ((_ o)
       (set! o (reverse! o)))))

  (method-syntax append-reverse
    (syntax-rules ()
      ((_ o ?tail)
       (append-reverse o ?tail))))

  (method-syntax append-reverse!
    (syntax-rules ()
      ((_ o ?tail)
       (set! o (append-reverse! o ?tail)))))

  (method-syntax zip
    (syntax-rules ()
      ((_ o . ?rest)
       (zip o . ?rest))))

  (method-syntax zip*
    (syntax-rules ()
      ((_ o . ?rest)
       (zip* o . ?rest))))

  (method-syntax unzip1
    (syntax-rules ()
      ((_ o)
       (unzip1 o))))

  (method-syntax unzip2
    (syntax-rules ()
      ((_ o)
       (unzip2 o))))

  (method-syntax unzip3
    (syntax-rules ()
      ((_ o)
       (unzip3 o))))

  (method-syntax unzip4
    (syntax-rules ()
      ((_ o)
       (unzip4 o))))

  (method-syntax unzip5
    (syntax-rules ()
      ((_ o)
       (unzip5 o))))

  (method-syntax count
    (syntax-rules ()
      ((_ o ?proc)
       (count ?proc o))))

  ;; fold
  (method-syntax and-fold-left*
    (syntax-rules ()
      ((_ o ?knil ?kombine)
       (and-fold-left* ?kombine ?knil o))))

  (method-syntax and-fold-right*
    (syntax-rules ()
      ((_ o ?knil ?kombine)
       (and-fold-right* ?kombine ?knil o))))

  (method-syntax fold-left/pred
    (syntax-rules ()
      ((_ o ?knil ?pred)
       (fold-left/pred ?pred ?knil o))))

  (method-syntax pair-fold
    (syntax-rules ()
      ((_ o ?knil ?kombine)
       (pair-fold ?kombine ?knil o))))

  (method-syntax pair-fold*
    (syntax-rules ()
      ((_ o ?knil ?kombine)
       (pair-fold* ?kombine ?knil o))))

  (method-syntax reduce
    (syntax-rules ()
      ((_ o ?knil ?combine)
       (reduce ?combine ?knil o))))

  (method-syntax reduce*
    (syntax-rules ()
      ((_ o ?knil ?combine)
       (reduce* ?combine ?knil o))))

  ;; map
  (method-syntax map-in-order*
    (syntax-rules ()
      ((_ o ?proc)
       (map-in-order* ?proc o))))

  (method-syntax map!
    (syntax-rules ()
      ((_ o ?proc)
       (set! o (map! ?proc o)))))

  (method-syntax pair-for-each
    (syntax-rules ()
      ((_ o ?proc)
       (pair-for-each ?proc o))))

  (method-syntax filter-map
    (syntax-rules ()
      ((_ o ?proc)
       (filter-map ?proc o))))

  ;; filtering
  (method-syntax filter!
    (syntax-rules ()
      ((_ o ?pred)
       (set! o (filter! ?pred o)))))

  (method-syntax remove*
    (syntax-rules ()
      ((_ o ?pred)
       (remove* ?pred o))))

  (method-syntax remove*!
    (syntax-rules ()
      ((_ o ?pred)
       (set! o (remove*! ?pred o)))))

  ;;searching
  (method-syntax find-tail
    (syntax-rules ()
      ((_ o ?pred)
       (find-tail ?pred o))))

  (method-syntax take-while
    (syntax-rules ()
      ((_ o ?pred)
       (take-while ?pred o))))

  (method-syntax take-while!
    (syntax-rules ()
      ((_ o ?pred)
       (take-while! ?pred o))))

  (method-syntax drop-while
    (syntax-rules ()
      ((_ o ?pred)
       (drop-while ?pred o))))

  (method-syntax drop-while!
    (syntax-rules ()
      ((_ o ?pred)
       (set! o (drop-while ?pred o)))))

  (method-syntax span
    (syntax-rules ()
      ((_ o ?pred)
       (span ?pred o))))

  (method-syntax break
    (syntax-rules ()
      ((_ o ?pred)
       (break ?pred o))))

  (method-syntax any
    (syntax-rules ()
      ((_ o ?pred)
       (any ?pred o))))

  (method-syntax every
    (syntax-rules ()
      ((_ o ?pred)
       (every ?pred o))))

  (method-syntax list-index
    (syntax-rules ()
      ((_ o ?pred)
       (list-index ?pred o))))

  (method-syntax member*
    (syntax-rules ()
      ((_ o ?obj)
       (member* ?obj o))
      ((_ o ?obj ?item=)
       (member* ?obj o ?item=))))

  (method-syntax position
    (syntax-rules ()
      ((_ o ?obj)
       (position ?obj o))))

  ;; deletion
  (method-syntax delete
    (syntax-rules ()
      ((_ o ?obj)
       (delete ?obj o))
      ((_ o ?obj ?item=)
       (delete ?obj o ?item=))
      ))

  (method-syntax delete!
    (syntax-rules ()
      ((_ o ?obj)
       (set! o (delete! ?obj o)))
      ((_ o ?obj ?item=)
       (set! o (delete! ?obj o ?item=)))
      ))

  (method-syntax delete-duplicates
    (syntax-rules ()
      ((_ o)
       (delete-duplicates o))
      ((_ o ?item=)
       (delete-duplicates o ?item=))
      ))

  (method-syntax delete-duplicates!
    (syntax-rules ()
      ((_ o)
       (set! o (delete-duplicates! o)))
      ((_ o ?item=)
       (set! o (delete-duplicates o ?item=)))
      ))

  ;; sorted lists
  (method-syntax sorted-list-insert
    (syntax-rules ()
      ((_ o ?obj ?item>)
       (sorted-list-insert ?obj o ?item>))))

  (method-syntax sorted-list-insert!
    (syntax-rules ()
      ((_ o ?obj ?item>)
       (set! o (sorted-list-insert ?obj o ?item>)))))

  (method-syntax sorted-list-insert/uniq
    (syntax-rules ()
      ((_ o ?obj ?item< ?item>)
       (sorted-list-insert/uniq ?obj o ?item< ?item>))))

  (method-syntax sorted-list-insert/uniq!
    (syntax-rules ()
      ((_ o ?obj ?item< ?item>)
       (set! o (sorted-list-insert/uniq ?obj o ?item< ?item>)))))

  (method-syntax union-of-sorted-lists
    (syntax-rules ()
      ((_ o)
       (union-of-sorted-lists o))))

  (method-syntax union-of-sorted-lists/uniq
    (syntax-rules ()
      ((_ o)
       (union-of-sorted-lists/uniq o))))


  ;; alists
  (method-syntax assoc*
    (syntax-rules ()
      ((_ o)
       (assoc* o))))

  (method-syntax alist-cons
    (syntax-rules ()
      ((_ o)
       (alist-cons o))))

  (method-syntax alist-copy
    (syntax-rules ()
      ((_ o)
       (alist-copy o))))

  (method-syntax alist-delete
    (syntax-rules ()
      ((_ o)
       (alist-delete o))))

  (method-syntax alist-delete!
    (syntax-rules ()
      ((_ o)
       (alist-delete! o))))

  )


;;;; done

)

;;; end of file
