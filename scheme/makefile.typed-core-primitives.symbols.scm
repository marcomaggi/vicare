;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for symbols core primitives
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;


;;;; core syntactic binding descriptors, typed core primitives: safe symbol primitives

(section

(declare-type-predicate symbol? <symbol>)

(declare-core-primitive symbol->string
    (safe)
  (signatures
   ((<symbol>) => (<string>))))

(declare-core-primitive symbol->keyword
    (safe)
  (signatures
   ((<symbol>)		=> (<keyword>))))

(declare-hash-function symbol-hash <symbol> safe)

(declare-core-primitive symbol-bound?
    (safe)
  (signatures
   ((<symbol>) => (<boolean>))))

(declare-core-primitive symbol-value
    (safe)
  (signatures
   ((<symbol>) => (<top>))))

(declare-core-primitive set-symbol-value!
    (safe)
  (signatures
   ((<symbol> <top>) => (<void>))))

(declare-core-primitive <symbol>-value
    (safe)
  (signatures
   ((<symbol>)		=> (<top>))
   ((<symbol> <top>)	=> (<void>))))

;;;

(declare-core-primitive putprop
    (safe)
  (signatures
   ((<symbol> <symbol> <top>)	=> (<void>))))

(declare-core-primitive getprop
    (safe)
  (signatures
   ((<symbol> <symbol>)		=> (<top>))))

(declare-core-primitive remprop
    (safe)
  (signatures
   ((<symbol> <symbol>)		=> (<void>))))

(declare-core-primitive property-list
    (safe)
  (signatures
   ((<symbol>)			=> (<list>))))

/section)


;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:
