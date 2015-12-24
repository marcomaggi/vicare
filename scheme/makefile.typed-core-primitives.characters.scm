;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for characters core primitives
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


;;;; core syntactic binding descriptors, typed core primitives: safe char primitives

(section

;;; predicates

(declare-type-predicate char? <char>)

(declare-char-predicate char-in-ascii-range?)
(declare-char-predicate char-alphabetic?)
(declare-char-predicate char-lower-case?)
(declare-char-predicate char-numeric?)
(declare-char-predicate char-title-case?)
(declare-char-predicate char-upper-case?)
(declare-char-predicate char-whitespace?)
(declare-char-predicate unicode-printable-char?)

;;; --------------------------------------------------------------------
;;; comparison

(declare-char-binary/multi-comparison char=?		(replacements $char=))
(declare-char-binary/multi-comparison char!=?		(replacements $char!=))
(declare-char-binary/multi-comparison char<?		(replacements $char<))
(declare-char-binary/multi-comparison char>?		(replacements $char>))
(declare-char-binary/multi-comparison char<=?		(replacements $char<=))
(declare-char-binary/multi-comparison char>=?		(replacements $char>=))

(declare-char-binary/multi-comparison char-ci=?)
(declare-char-binary/multi-comparison char-ci!=?)
(declare-char-binary/multi-comparison char-ci<?)
(declare-char-binary/multi-comparison char-ci>?)
(declare-char-binary/multi-comparison char-ci<=?)
(declare-char-binary/multi-comparison char-ci>=?)

;;; --------------------------------------------------------------------
;;; transformations

(declare-char-unary char-downcase)
(declare-char-unary char-foldcase)
(declare-char-unary char-titlecase)
(declare-char-unary char-upcase)
(declare-char-unary char-general-category)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive char->integer
  (safe)
  (signatures
   ((<char>) => (<non-negative-fixnum>))))

(declare-core-primitive char->fixnum
    (safe)
  (signatures
   ((<char>) => (<non-negative-fixnum>))))

(declare-hash-function char-hash <char> safe)

/section)


;;;; characters unsafe operations

(section

;;; comparison

(declare-char-binary-comparison $char=		unsafe)
(declare-char-binary-comparison $char!=		unsafe)
(declare-char-binary-comparison $char>		unsafe)
(declare-char-binary-comparison $char<		unsafe)
(declare-char-binary-comparison $char>=		unsafe)
(declare-char-binary-comparison $char<=		unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $char->fixnum
    (unsafe)
  (signatures
   ((<char>)			=> (<fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)


;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:
