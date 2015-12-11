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

#!r6rs
(library (ikarus.compiler.compat)
  (export
    import				module
    include
    define*				lambda*
    case-define*			case-lambda*
    case-define				define-inline
    define-syntax-rule			define-syntax*
    define-auxiliary-syntaxes		fluid-let-syntax
    brace				__who__
    define-constant			define-inline-constant
    let-constants
    receive				receive-and-return
    begin0
    parametrise				parameterize
    make-parameter
    define-struct			struct?
    type-descriptor
    annotation?
    annotation-source			annotation-stripped
    getenv
    printf				fprintf
    format
    pretty-print			debug-print
    debug-print*
    gensym				print-gensym
    gensym-prefix
    foreign-call
    fxadd1				fxsub1
    fxnonnegative?
    fx=
    fx>					fx<
    fx>=				fx<=
    fxsll				fxsra
    fxremainder				fxquotient
    fxlogor				fxlogand
    fxlognot
    immediate?				bignum?
    add1				sub1
    sll					sra
    make-list
    andmap				ormap
    set-car!				set-cdr!
    set-cons!
    vector-exists			vector-for-all
    getprop				putprop
    bwp-object?				void-object?
    void
    reset-symbol-proc!
    procedure-argument-violation
    expression-return-value-violation)
  (import (vicare))
  #| end of library |# )

;;; end of file
