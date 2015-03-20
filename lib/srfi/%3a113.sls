;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 113
;;;Date: Sun Mar  8, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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

#!r6rs
(library (srfi :113)
  (export
    ;;
    set set-unfold
    ;;
    set? set-contains? set-empty? set-disjoint?
    ;;
    set-member set-element-comparator
    ;;
    set-adjoin set-adjoin! set-replace set-replace!
    set-delete set-delete! set-delete-all set-delete-all! set-search!
    ;;
    set-size set-find set-count set-any? set-every?
    ;;
    set-map set-for-each set-fold
    set-filter  set-remove  set-partition
    set-filter! set-remove! set-partition!
    ;;
    set-copy set->list list->set list->set!
    ;;
    set=? set<? set>? set<=? set>=?
    ;;
    set-union set-intersection set-difference set-xor
    set-union! set-intersection! set-difference! set-xor!
    ;;
    set-comparator

    ;;
    bag bag-unfold
    ;;
    bag? bag-contains? bag-empty? bag-disjoint?
    ;;
    bag-member bag-element-comparator
    ;;
    bag-adjoin bag-adjoin! bag-replace bag-replace!
    bag-delete bag-delete! bag-delete-all bag-delete-all! bag-search!
    ;;
    bag-size bag-find bag-count bag-any? bag-every?
    ;;
    bag-map bag-for-each bag-fold
    bag-filter bag-remove bag-partition
    bag-filter! bag-remove! bag-partition!
    ;;
    bag-copy bag->list list->bag list->bag!
    ;;
    bag=? bag<? bag>? bag<=? bag>=?
    ;;
    bag-union bag-intersection bag-difference bag-xor
    bag-union! bag-intersection! bag-difference! bag-xor!
    ;;
    bag-comparator
    ;;
    bag-sum bag-sum! bag-product bag-product!
    bag-unique-size bag-element-count bag-for-each-unique bag-fold-unique
    bag-increment! bag-decrement! bag->set set->bag set->bag!
    bag->alist alist->bag)
  (import (srfi :113 sets-and-bags)))

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
