;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.system-value)
  (export
    system-value
    system-value-gensym)
  (import (except (vicare)
		  system-value
		  system-value-gensym)
    (except (vicare system $symbols)
	    system-value
	    system-value-gensym)
    (ikarus.compiler.helpers))


(cond-boot-expansion "system-value API definition"
  ((inclusion-in-normal-boot-image)
   (define SYSTEM-VALUE-GENSYM
     ;;Notice that this  gensym is generated a-new every time  the boot image source
     ;;code is expanded.
     (expand-time-gensym "system-value-gensym"))

   (define (system-value-gensym)
     SYSTEM-VALUE-GENSYM)

   (define* (system-value {x symbol?})
     ;;When the  boot image  is loaded,  it initialises  itself; for  every primitive
     ;;function (CONS, CAR, ...)  one of the operations is to put the actual function
     ;;(a closure object) in the "value" field  of a gensym, and then put such gensym
     ;;in the property list  of the symbol being the name of  the primitive, using an
     ;;internal gensym (bound to SYSTEM-VALUE-GENSYM) as key.
     ;;
     ;;For example, this is more or less what happens to CONS:
     ;;
     ;;   (define G-cons (gensym "cons"))
     ;;   ($set-symbol-value 'G-cons #<procedure cons>)
     ;;   (putprop 'cons SYSTEM-VALUE-GENSYM 'G-cons)
     ;;
     ;;so later we can do:
     ;;
     ;;   ($symbol-value (getprop 'G-cons SYSTEM-VALUE-GENSYM))
     ;;   => #<procedure cons>
     ;;
     ;;or use the equivalent public API:
     ;;
     ;;   (system-value 'cons)    => #<procedure cons>
     ;;
     (cond (($getprop x SYSTEM-VALUE-GENSYM)
	    => (lambda (g)
		 (receive-and-return (v)
		     ($symbol-value g)
		   (when ($unbound-object? v)
		     (procedure-argument-violation __who__ "not a system symbol" x)))))
	   (else
	    (procedure-argument-violation __who__ "not a system symbol" x))))

   #| end of branch |# )

;;; --------------------------------------------------------------------

  ((inclusion-in-rotation-boot-image)
   (define SYSTEM-VALUE-GENSYM
     ;;Notice that this  gensym is generated a-new every time  the boot image source
     ;;code is expanded.
     (expand-time-gensym "system-value-gensym"))

   (define (system-value-gensym)
     SYSTEM-VALUE-GENSYM)

   (define* (system-value {x symbol?})
     ;;When the  boot image  is loaded,  it initialises  itself; for  every primitive
     ;;function (CONS, CAR, ...)  one of the operations is to put the actual function
     ;;(a closure object) in the "value" field  of a gensym, and then put such gensym
     ;;in the property list  of the symbol being the name of  the primitive, using an
     ;;internal gensym (bound to SYSTEM-VALUE-GENSYM) as key.
     ;;
     ;;For example, this is more or less what happens to CONS:
     ;;
     ;;   (define G-cons (gensym "cons"))
     ;;   ($set-symbol-value 'G-cons #<procedure cons>)
     ;;   (putprop 'cons SYSTEM-VALUE-GENSYM 'G-cons)
     ;;
     ;;so later we can do:
     ;;
     ;;   ($symbol-value (getprop 'G-cons SYSTEM-VALUE-GENSYM))
     ;;   => #<procedure cons>
     ;;
     ;;or use the equivalent public API:
     ;;
     ;;   (system-value 'cons)    => #<procedure cons>
     ;;
     (cond (($getprop x SYSTEM-VALUE-GENSYM)
	    => (lambda (g)
		 (receive-and-return (v)
		     ($symbol-value g)
		   (when ($unbound-object? v)
		     (procedure-argument-violation __who__ "not a system symbol" x)))))
	   (else
	    (procedure-argument-violation __who__ "not a system symbol" x))))

   #| end of branch |# )

;;; --------------------------------------------------------------------

  ((bootstrapping-for-normal-boot-image)
   (import (prefix (only (vicare compiler)
			 system-value
			 system-value-gensym)
		   old-boot-image.))
   (define (system-value arg)
     (old-boot-image.system-value arg))
   (define (system-value-gensym)
     old-boot-image.system-value-gensym)
   #| end of branch |# )

;;; --------------------------------------------------------------------

  ((bootstrapping-for-rotation-boot-image)
   (import (only (vicare compiler)
		 system-value
		 system-value-gensym))
   #| end of branch |# ))


;;;; done

#| end of library |# )

;;; end of file
