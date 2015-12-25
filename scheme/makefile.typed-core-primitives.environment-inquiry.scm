;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for environment inquiry core primitives
;;Date: Tue Dec 25, 2015
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


;;;; environment inquiry, safe primitives

(section

(declare-core-primitive host-info
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive uname
    (safe)
  (signatures
   (()			=> (<utsname>)))
  (attributes
   (()			effect-free result-true)))

(declare-type-predicate utsname?	<utsname>)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<utsname>)	=> (<string>)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare utsname-sysname)
  (declare utsname-nodename)
  (declare utsname-release)
  (declare utsname-version)
  (declare utsname-machine)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-core-primitive implementation-name
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			foldable effect-free result-true)))

(declare-core-primitive implementation-version
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   ;;Not foldable.
   (()			effect-free result-true)))

(declare-core-primitive cpu-architecture
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive machine-name
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive os-name
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive os-version
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			effect-free result-true)))

/section)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; End:
