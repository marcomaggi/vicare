;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for pairs and lists core primitives
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


;;;; core syntactic binding descriptors, typed safe core primitives: pairs and lists

(section

(declare-core-primitive cons
    (safe)
  (signatures
   ((<top> <list>)		=> (<nlist>))
   ((<top> <top>)		=> (<pair>))))

(declare-core-primitive list
    (safe)
  (signatures
   (()				=> (<null>))
   ((<top> . <top>)		=> (<nlist>))))

;;;

(declare-core-primitive <null>-constructor
    (safe)
  (signatures
   (()				=> (<null>))))

;;;

(declare-type-predicate pair? <pair>)

(declare-type-predicate list? <list>)

(declare-type-predicate nlist? <nlist>)

;;;

(declare-core-primitive car
    (safe)
  (signatures
   ((<pair>)		=> (<top>))
   ((<nlist>)		=> (<top>))
   ;;FIXME Strictly  speaking this is wrong:  "<list>" can also be  null, so applying
   ;;CAR is an error.   This is why the replacement is  commented out.  (Marco Maggi;
   ;;Thu Oct 22, 2015)
   ((<list>)		=> (<top>)))
  ;;(replacements $car)
  #| end of DECLARE-CORE-PRIMITIVE |# )

(declare-core-primitive cdr
    (safe)
  (signatures
   ((<pair>)		=> (<top>))
   ((<nlist>)		=> (<top>))
   ;;FIXME Strictly  speaking this is wrong:  "<list>" can also be  null, so applying
   ;;CDR is an error.   This is why the replacement is  commented out.  (Marco Maggi;
   ;;Thu Oct 22, 2015)
   ((<list>)		=> (<top>)))
  ;;(replacements $cdr)
  #| end of DECLARE-CORE-PRIMITIVE |# )

/section)


;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:
