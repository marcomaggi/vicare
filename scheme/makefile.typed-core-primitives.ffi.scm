;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for ffi core primitives
;;Date: Tue Dec 28, 2015
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


;;;; system interface and foreign functions interface

(section

(declare-core-primitive errno
    (safe)
  (signatures
   (()				=> (<fixnum>))
   ((<fixnum>)			=> (<void>)))
  (attributes
   (()			effect-free result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive free
    (safe)
  (signatures
   ((<pointer>)				=> (<void>))
   ((<memory-block>)			=> (<void>))))

(declare-core-primitive malloc
    (safe)
  (signatures
   ((<exact-integer>)		=> (<pointer>))))

(declare-core-primitive realloc
    (safe)
  (signatures
   ((<pointer> <exact-integer>)	=> (<pointer>))))

(declare-core-primitive calloc
    (safe)
  (signatures
   ((<exact-integer> <exact-integer>)	=> (<pointer>))))

;;; --------------------------------------------------------------------

(declare-core-primitive guarded-malloc
    (safe)
  (signatures
   ((<exact-integer>)		=> (<pointer>))))

(declare-core-primitive guarded-realloc
    (safe)
  (signatures
   ((<pointer> <exact-integer>)	=> (<pointer>))))

(declare-core-primitive guarded-calloc
    (safe)
  (signatures
   ((<exact-integer> <exact-integer>)	=> (<pointer>))))


;;; --------------------------------------------------------------------

(declare-core-primitive memcpy
    (safe)
  (signatures
   ((<pointer> <pointer> <exact-integer>)	=> (<void>))))

(declare-core-primitive memmove
    (safe)
  (signatures
   ((<pointer> <pointer> <exact-integer>)	=> (<void>))))

(declare-core-primitive memcmp
    (safe)
  (signatures
   ((<pointer> <pointer> <exact-integer>)	=> (<fixnum>))))

(declare-core-primitive memset
    (safe)
  (signatures
   ((<pointer> <exact-integer> <exact-integer>)	=> (<void>))))

;;; --------------------------------------------------------------------

(declare-core-primitive make-memory-block
    (safe)
  (signatures
   ((<pointer> <exact-integer>)			=> (<memory-block>))))

(declare-core-primitive make-memory-block/guarded
    (safe)
  (signatures
   ((<pointer> <exact-integer>)			=> (<memory-block>))))

(declare-core-primitive null-memory-block
    (safe)
  (signatures
   (()						=> (<memory-block>)))
  (attributes
   (()						effect-free result-true)))

(declare-core-primitive memory-block?
    (safe)
  (signatures
   ((<memory-block>)				=> (<boolean>))
   ((<top>)					=> (<false>)))
  (attributes
   ((_)						effect-free)))

(declare-core-primitive memory-block?/not-null
    (safe)
  (signatures
   ((<memory-block>)			=> (<boolean>))
   ((<top>)				=> (<false>)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive memory-block?/non-null
    (safe)
  (signatures
   ((<memory-block>)			=> (<boolean>))
   ((<top>)				=> (<false>)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive memory-block-pointer
    (safe)
  (signatures
   ((<memory-block>)			=> (<pointer>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive memory-block-size
    (safe)
  (signatures
   ((<memory-block>)			=> (<non-negative-exact-integer>)))
  (attributes
   ((_)					effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-out-of-memory-error
    (safe)
  (signatures
   (()					=> (&out-of-memory-error)))
  (attributes
   (()					effect-free result-true)))

(declare-condition-type-predicate out-of-memory-error?	&out-of-memory-error)

;;; --------------------------------------------------------------------

(declare-core-primitive memory-copy
    (safe)
  (signatures
   ((<pointer>    <fixnum> <pointer>     <fixnum> <fixnum>)	=> (<void>))
   ((<bytevector> <fixnum> <bytevector>  <fixnum> <fixnum>)	=> (<void>))
   ((<pointer>    <fixnum> <bytevector> <fixnum> <fixnum>)	=> (<void>))
   ((<bytevector> <fixnum> <pointer>    <fixnum> <fixnum>)	=> (<void>))
   ))

(declare-core-primitive memory->bytevector
    (safe)
  (signatures
   ((<pointer> <fixnum>)		=> (<bytevector>)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive bytevector->memory
    (safe)
  (signatures
   #;((<bytevector>)			=> (<pointer>/false <fixnum>/false))
   ((<bytevector>)			=> (<top> <top>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory
    (safe)
  (signatures
   #;((<bytevector>)			=> (<pointer>/false <fixnum>/false))
   ((<bytevector>)			=> (<top> <top>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->memory*
    (safe)
  (signatures
   ((<bytevector>)			=> (<pointer> <fixnum>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory*
    (safe)
  (signatures
   ((<bytevector>)			=> (<pointer> <fixnum>)))
  (attributes
   ((_ _)				effect-free)))


;;; --------------------------------------------------------------------
;;; cstrings

(declare-core-primitive bytevector->cstring
    (safe)
  (signatures
   #;((<bytevector>)		=> ((or <false> <pointer>)))
   ((<bytevector>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive bytevector->guarded-cstring
    (safe)
  (signatures
   #;((<bytevector>)		=> ((or <false> <pointer>)))
   ((<bytevector>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive bytevector->cstring*
    (safe)
  (signatures
   ((<bytevector>)		=> (<pointer>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive bytevector->guarded-cstring*
    (safe)
  (signatures
   ((<bytevector>)		=> (<pointer>)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive string->cstring
    (safe)
  (signatures
   #;((<string>)			=> ([or <false> <pointer>]))
   ((<string>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive string->guarded-cstring
    (safe)
  (signatures
   #;((<string>)			=> ([or <false> <pointer>]))
   ((<string>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive string->cstring*
    (safe)
  (signatures
   ((<string>)			=> (<pointer>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive string->guarded-cstring*
    (safe)
  (signatures
   ((<string>)			=> (<pointer>)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive cstring->bytevector
    (safe)
  (signatures
   ((<pointer>)				=> (<bytevector>))
   ((<pointer> <non-negative-fixnum>)	=> (<bytevector>)))
  (attributes
   ((_)					effect-free result-true)
   ((_ _)				effect-free result-true)))

(declare-core-primitive cstring16->bytevector
    (safe)
  (signatures
   ((<pointer>)				=> (<bytevector>)))
  (attributes
   ((_)					effect-free result-true)))

;;;

(declare-core-primitive cstring->string
    (safe)
  (signatures
   ((<pointer>)				=> (<string>))
   ((<pointer> <non-negative-fixnum>)	=> (<string>)))
  (attributes
   ((_)					effect-free result-true)
   ((_ _)				effect-free result-true)))

(declare-core-primitive cstring16n->string
    (safe)
  (signatures
   ((<pointer>)				=> (<string>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive cstring16le->string
    (safe)
  (signatures
   ((<pointer>)				=> (<string>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive cstring16be->string
    (safe)
  (signatures
   ((<pointer>)				=> (<string>)))
  (attributes
   ((_)					effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive strlen
    (safe)
  (signatures
   ((<pointer>)			=> (<exact-integer>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive strcmp
    (safe)
  (signatures
   ((<pointer> <pointer>)	=> (<fixnum>)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive strncmp
    (safe)
  (signatures
   ((<pointer> <pointer> <non-negative-fixnum>)		=> (<fixnum>)))
  (attributes
   ((_ _ _)			effect-free result-true)))

;;;

(declare-core-primitive strdup
    (safe)
  (signatures
   #;((<pointer>)			=> ([or <false> <pointer>]))
   ((<pointer>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive guarded-strdup
    (safe)
  (signatures
   #;((<pointer>)			=> ([or <false> <pointer>]))
   ((<pointer>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive strdup*
    (safe)
  (signatures
   ((<pointer>)			=> (<pointer>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive guarded-strdup*
    (safe)
  (signatures
   ((<pointer>)			=> (<pointer>)))
  (attributes
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive strndup
    (safe)
  (signatures
   #;((<pointer> <exact-integer>)		=> ([or <false> <pointer>]))
   ((<pointer> <exact-integer>)		=> (<top>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive guarded-strndup
    (safe)
  (signatures
   #;((<pointer> <exact-integer>)		=> ([or <false> <pointer>]))
   ((<pointer> <exact-integer>)		=> (<top>)))
  (attributes
   ((_ _)				effect-free)))

;;;

(declare-core-primitive strndup*
    (safe)
  (signatures
   ((<pointer> <exact-integer>)		=> (<pointer>)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive guarded-strndup*
    (safe)
  (signatures
   ((<pointer> <exact-integer>)		=> (<pointer>)))
  (attributes
   ((_ _)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bytevectors->argv
    (safe)
  (signatures
   #;((<list>)		=> ([or <false> <pointer>]))
   ((<list>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->guarded-argv
    (safe)
  (signatures
   #;((<list>)		=> ([or <false> <pointer>]))
   ((<list>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->argv*
    (safe)
  (signatures
   ((<list>)		=> (<pointer>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->guarded-argv*
    (safe)
  (signatures
   ((<list>)		=> (<pointer>)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive strings->argv
    (safe)
  (signatures
   #;((<list>)		=> ([or <false> <pointer>]))
   ((<list>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->guarded-argv
    (safe)
  (signatures
   #;((<list>)		=> ([or <false> <pointer>]))
   ((<list>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->argv*
    (safe)
  (signatures
   ((<list>)		=> (<pointer>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->guarded-argv*
    (safe)
  (signatures
   ((<list>)		=> (<pointer>)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive argv->bytevectors
    (safe)
  (signatures
   ((<pointer>)			=> (<list>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive argv->strings
    (safe)
  (signatures
   ((<pointer>)			=> (<list>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive argv-length
    (safe)
  (signatures
   ((<pointer>)			=> (<exact-integer>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive dlopen
    (safe)
  (signatures
   ;; (()					=> ([or <false> <pointer>]))
   ;; (([or <bytevector> <string>])		=> ([or <false> <pointer>]))
   ;; (([or <bytevector> <string>] _ _)		=> ([or <false> <pointer>]))
   (()						=> (<top>))
   ((<bytevector>)				=> (<top>))
   ((<string>)					=> (<top>))
   ((<bytevector> _ _)				=> (<top>))
   ((<string> _ _)				=> (<top>))))

(declare-core-primitive dlclose
    (safe)
  (signatures
   ((<pointer>)			=> (<boolean>))))

(declare-core-primitive dlerror
    (safe)
  (signatures
   #;(()			=> ([or <false> <string>]))
   (()				=> (<top>))))

(declare-core-primitive dlsym
    (safe)
  (signatures
   #;((<pointer> <string>)	=> ([or <false> <pointer>]))
   ((<pointer> <string>)	=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive make-c-callout-maker
    (safe)
  (signatures
   ((<symbol> <list>)		=> (<procedure>))))

(declare-core-primitive make-c-callout-maker/with-errno
    (safe)
  (signatures
   ((<symbol> <list>)		=> (<procedure>))))

(declare-core-primitive make-c-callback-maker
    (safe)
  (signatures
   ((<symbol> <list>)		=> (<procedure>))))

(declare-core-primitive free-c-callback
    (safe)
  (signatures
   ((<pointer>)				=> (<void>))))


;;;; foreign functions interface: raw memory accessors and mutators, safe procedures

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<pointer> <non-negative-fixnum>)	=> (?return-value-tag))
		    ((<pointer> <positive-bignum>)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare pointer-ref-c-uint8			<non-negative-fixnum>)
  (declare pointer-ref-c-sint8			<fixnum>)
  (declare pointer-ref-c-uint16			<non-negative-fixnum>)
  (declare pointer-ref-c-sint16			<fixnum>)
  (declare pointer-ref-c-uint32			<non-negative-exact-integer>)
  (declare pointer-ref-c-sint32			<exact-integer>)
  (declare pointer-ref-c-uint64			<non-negative-exact-integer>)
  (declare pointer-ref-c-sint64			<exact-integer>)

  (declare pointer-ref-c-signed-char		<fixnum>)
  (declare pointer-ref-c-signed-short		<fixnum>)
  (declare pointer-ref-c-signed-int		<exact-integer>)
  (declare pointer-ref-c-signed-long		<exact-integer>)
  (declare pointer-ref-c-signed-long-long	<exact-integer>)

  (declare pointer-ref-c-unsigned-char		<non-negative-exact-integer>)
  (declare pointer-ref-c-unsigned-short		<non-negative-exact-integer>)
  (declare pointer-ref-c-unsigned-int		<non-negative-exact-integer>)
  (declare pointer-ref-c-unsigned-long		<non-negative-exact-integer>)
  (declare pointer-ref-c-unsigned-long-long	<non-negative-exact-integer>)

  (declare pointer-ref-c-float			<flonum>)
  (declare pointer-ref-c-double			<flonum>)
  (declare pointer-ref-c-pointer		<pointer>)

  (declare pointer-ref-c-size_t			<non-negative-exact-integer>)
  (declare pointer-ref-c-ssize_t		<exact-integer>)
  (declare pointer-ref-c-off_t			<exact-integer>)
  (declare pointer-ref-c-ptrdiff_t		<exact-integer>)

  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<pointer> <non-negative-exact-integer> ?new-value-tag)	=> (<void>)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare pointer-set-c-uint8!			<non-negative-fixnum>)
  (declare pointer-set-c-sint8!			<fixnum>)
  (declare pointer-set-c-uint16!		<non-negative-fixnum>)
  (declare pointer-set-c-sint16!		<fixnum>)
  (declare pointer-set-c-uint32!		<non-negative-exact-integer>)
  (declare pointer-set-c-sint32!		<exact-integer>)
  (declare pointer-set-c-uint64!		<non-negative-exact-integer>)
  (declare pointer-set-c-sint64!		<exact-integer>)

  (declare pointer-set-c-signed-char!		<fixnum>)
  (declare pointer-set-c-signed-short!		<fixnum>)
  (declare pointer-set-c-signed-int!		<exact-integer>)
  (declare pointer-set-c-signed-long!		<exact-integer>)
  (declare pointer-set-c-signed-long-long!	<exact-integer>)

  (declare pointer-set-c-unsigned-char!		<non-negative-exact-integer>)
  (declare pointer-set-c-unsigned-short!	<non-negative-exact-integer>)
  (declare pointer-set-c-unsigned-int!		<non-negative-exact-integer>)
  (declare pointer-set-c-unsigned-long!		<non-negative-exact-integer>)
  (declare pointer-set-c-unsigned-long-long!	<non-negative-exact-integer>)

  (declare pointer-set-c-float!			<flonum>)
  (declare pointer-set-c-double!		<flonum>)
  (declare pointer-set-c-pointer!		<pointer>)

  (declare pointer-set-c-size_t!		<non-negative-exact-integer>)
  (declare pointer-set-c-ssize_t!		<exact-integer>)
  (declare pointer-set-c-off_t!			<exact-integer>)
  (declare pointer-set-c-ptrdiff_t!		<exact-integer>)

  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<pointer> <non-negative-exact-integer>)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare array-ref-c-uint8			<non-negative-fixnum>)
  (declare array-ref-c-sint8			<fixnum>)
  (declare array-ref-c-uint16			<non-negative-fixnum>)
  (declare array-ref-c-sint16			<fixnum>)
  (declare array-ref-c-uint32			<non-negative-exact-integer>)
  (declare array-ref-c-sint32			<exact-integer>)
  (declare array-ref-c-uint64			<non-negative-exact-integer>)
  (declare array-ref-c-sint64			<exact-integer>)

  (declare array-ref-c-signed-char		<fixnum>)
  (declare array-ref-c-signed-short		<fixnum>)
  (declare array-ref-c-signed-int		<exact-integer>)
  (declare array-ref-c-signed-long		<exact-integer>)
  (declare array-ref-c-signed-long-long		<exact-integer>)

  (declare array-ref-c-unsigned-char		<non-negative-fixnum>)
  (declare array-ref-c-unsigned-short		<non-negative-fixnum>)
  (declare array-ref-c-unsigned-int		<non-negative-exact-integer>)
  (declare array-ref-c-unsigned-long		<non-negative-exact-integer>)
  (declare array-ref-c-unsigned-long-long	<non-negative-exact-integer>)

  (declare array-ref-c-float			<flonum>)
  (declare array-ref-c-double			<flonum>)
  (declare array-ref-c-pointer			<pointer>)

  (declare array-ref-c-size_t			<non-negative-exact-integer>)
  (declare array-ref-c-ssize_t			<exact-integer>)
  (declare array-ref-c-off_t			<exact-integer>)
  (declare array-ref-c-ptrdiff_t		<exact-integer>)

  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<pointer> <non-negative-exact-integer> ?new-value-tag)	=> (<void>)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare array-set-c-uint8!			<non-negative-fixnum>)
  (declare array-set-c-sint8!			<fixnum>)
  (declare array-set-c-uint16!			<non-negative-fixnum>)
  (declare array-set-c-sint16!			<fixnum>)
  (declare array-set-c-uint32!			<non-negative-exact-integer>)
  (declare array-set-c-sint32!			<exact-integer>)
  (declare array-set-c-uint64!			<non-negative-exact-integer>)
  (declare array-set-c-sint64!			<exact-integer>)

  (declare array-set-c-signed-char!		<fixnum>)
  (declare array-set-c-signed-short!		<fixnum>)
  (declare array-set-c-signed-int!		<exact-integer>)
  (declare array-set-c-signed-long!		<exact-integer>)
  (declare array-set-c-signed-long-long!	<exact-integer>)

  (declare array-set-c-unsigned-char!		<non-negative-exact-integer>)
  (declare array-set-c-unsigned-short!		<non-negative-exact-integer>)
  (declare array-set-c-unsigned-int!		<non-negative-exact-integer>)
  (declare array-set-c-unsigned-long!		<non-negative-exact-integer>)
  (declare array-set-c-unsigned-long-long!	<non-negative-exact-integer>)

  (declare array-set-c-float!			<flonum>)
  (declare array-set-c-double!			<flonum>)
  (declare array-set-c-pointer!			<pointer>)

  (declare array-set-c-size_t!			<non-negative-exact-integer>)
  (declare array-set-c-ssize_t!			<exact-integer>)
  (declare array-set-c-off_t!			<exact-integer>)
  (declare array-set-c-ptrdiff_t!		<exact-integer>)

  #| end of LET-SYNTAX |# )

/section)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; End:
