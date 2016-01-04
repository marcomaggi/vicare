;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for transcoders core primitives
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; transcoders, safe primitives

(section

;;; predicates

(declare-type-predicate transcoder? <transcoder>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-transcoder
    (safe)
  (signatures
   ((<symbol>)				=> (<transcoder>))
   ((<symbol> <symbol>)			=> (<transcoder>))
   ((<symbol> <symbol> <symbol>)	=> (<transcoder>)))
  (attributes
   ;;Not foldable because transcoders are not representable in FASL files.
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-parameter native-transcoder	<transcoder>)

;;; --------------------------------------------------------------------
;;; accessors

(let-syntax
    ((declare-transcoder-accessor
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<transcoder>)	=> (<symbol>)))
	   (attributes
	    ((_)		foldable effect-free result-true))))
	)))
  (declare-transcoder-accessor transcoder-codec)
  (declare-transcoder-accessor transcoder-eol-style)
  (declare-transcoder-accessor transcoder-error-handling-mode)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; codec values

(declare-object-retriever latin-1-codec		foldable	<symbol>)
(declare-object-retriever utf-8-codec		foldable	<symbol>)
(declare-object-retriever utf-16-codec		foldable	<symbol>)
(declare-object-retriever utf-16le-codec	foldable	<symbol>)
(declare-object-retriever utf-16be-codec	foldable	<symbol>)
(declare-object-retriever utf-16n-codec		foldable	<symbol>)
(declare-object-retriever utf-bom-codec		foldable	<symbol>)

(declare-object-retriever native-eol-style	foldable	<symbol>)
(declare-object-retriever native-endianness	foldable	<symbol>)

/section)


;;;; transcoders, unsafe primitives

(section

(declare-core-primitive $data->transcoder
    (unsafe)
  (signatures
   ((<fixnum>)			=> (<transcoder>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $transcoder->data
    (unsafe)
  (signatures
   ((<transcoder>)		=> (<fixnum>)))
  (attributes
   ((_)				effect-free result-true)))

/section)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:
