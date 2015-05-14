;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.core-primitive-operation-names)
  (export CORE-PRIMITIVE-OPERATION-NAMES)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


(module CORE-PRIMITIVE-OPERATION-NAMES
  (core-primitive-operation? get-primop set-primop!)
  ;;This module  has the only  purpose of making the  binding COOKIE visible  only to
  ;;CORE-PRIMITIVE-OPERATION?, GET-PRIMOP and SET-PRIMOP!.
  ;;
  (define-constant COOKIE
    (expand-time-gensym "core-primitive-operation/integration-handler"))

  (define* (core-primitive-operation? {core-primitive-symbol-name symbol?})
    ;;Return  true  if  CORE-PRIMITIVE-SYMBOL-NAME  is  the public  name  of  a  core
    ;;primitive operation; otherwise return false.
    ;;
    (and (getprop core-primitive-symbol-name COOKIE) #t))

  (define* (get-primop {core-primitive-symbol-name symbol?})
    ;;If CORE-PRIMITIVE-SYMBOL-NAME is the public name of a core primitive operation:
    ;;return a PRIMITIVE-HANDLER struct describind  the operation; otherwise raise an
    ;;exception.
    ;;
    (or (getprop core-primitive-symbol-name COOKIE)
	(compiler-internal-error 'CORE-PRIMITIVE-OPERATION-NAMES __who__
	  "not a core primitive operation" core-primitive-symbol-name)))

  (define* (set-primop! {symbol symbol?} primitive-handler)
    ;;Associate to  SYMBOL the struct  PRIMITIVE-HANDLER, turning SYMBOL into  a core
    ;;primitive's symbol name.
    ;;
    (putprop symbol COOKIE primitive-handler))

  #| end of module CORE-PRIMITIVE-OPERATION-NAMES |# )



;;;; done

#| end of library |# )

;;; end of file
