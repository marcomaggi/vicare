/*
  Part of: Vicare Scheme
  Contents: external header file
  Date: Wed Jan 11, 2012

  Abstract

	This  file contains  external  definitions for  the public  API.
	Many  of  the  definitions  in  this file  are  duplicated  from
	"internals.h", which defines  the internal API; some definitions
	are modified to keep them opaque to external code.

  Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006-2008  Abdulaziz Ghuloum

  This program is  free software: you can redistribute	it and/or modify
  it under the	terms of the GNU General Public	 License as published by
  the Free Software Foundation, either	version 3 of the License, or (at
  your option) any later version.

  This program	is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY	 WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See	 the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VICARE_H
#  define VICARE_H


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <vicare-platform.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h> /* for off_t */


/** --------------------------------------------------------------------
 ** Helper macros.
 ** ----------------------------------------------------------------- */

/* The macro  IK_UNUSED indicates that a function,  function argument or
   variable may potentially be unused.	Usage examples:

   static int unused_function (char arg) IK_UNUSED;
   int foo (char unused_argument IK_UNUSED);
   int unused_variable IK_UNUSED;
*/
#ifdef __GNUC__
#  define IK_UNUSED		__attribute__((unused))
#else
#  define IK_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif

/* This directive is to be used  in the public header files installed by
   Vicare Scheme or a Vicare extension.   It is the declaration type for
   public functions exported by Vicare's runtime or a Vicare extension's
   shared library. */
#if ((defined _WIN32) || (defined __CYGWIN__))
#  ifdef __GNUC__
#    define ik_api_decl		__attribute__((dllimport)) extern
#  else
#    define ik_api_decl		__declspec(dllimport) extern
#  endif
#else
#  define ik_api_decl		extern
#endif

/* This directive  is to  be used  in the private  header files  used to
   build  a Vicare  extension.  It  is the  declaration type  for public
   functions exported by a Vicare extension's shared library. */
#if ((defined _WIN32) || (defined __CYGWIN__))
#  ifdef __GNUC__
#    define ik_decl		__attribute__((dllexport))
#  else
#    define ik_decl		__declspec(dllexport)
#  endif
#else
#  if __GNUC__ >= 4
#    define ik_decl		__attribute__((visibility ("default"))) extern
#  else
#    define ik_decl		extern
#  endif
#endif

/* This directive  is to  be used  in the private  header files  used to
   build a  Vicare extension.   It is the  declaration type  for private
   functions defined by a Vicare extension for internal usage. */
#if ((defined _WIN32) || (defined __CYGWIN__))
#  ifdef __GNUC__
#    define ik_private_decl	extern
#  else
#    define ik_private_decl	extern
#  endif
#else
#  if __GNUC__ >= 4
#    define ik_private_decl	__attribute__((visibility ("hidden"))) extern
#  else
#    define ik_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Global constants.
 ** ----------------------------------------------------------------- */

#define IK_ASS(LEFT,RIGHT)	\
  do { ikptr_t s_tmp = (RIGHT); (LEFT) = s_tmp; } while (0);


/** --------------------------------------------------------------------
 ** Global data types.
 ** ----------------------------------------------------------------- */

/* These aliases have  the only purpose of making  the code shorter here
   and there.  Unfortunately  we have to use the  cast operator often in
   the code... */
typedef signed int		ik_int;
typedef signed long		ik_long;
typedef signed long long	ik_llong;
typedef unsigned int		ik_uint;
typedef unsigned long		ik_ulong;
typedef unsigned long long	ik_ullong;

#if   (4 == SIZEOF_VOID_P)
typedef uint32_t		ikptr_t;
typedef int32_t			iksword_t;
typedef uint32_t		ikuword_t;
#elif (8 == SIZEOF_VOID_P)
typedef uint64_t		ikptr_t;
typedef int64_t			iksword_t;
typedef uint64_t		ikuword_t;
#else
typedef unsigned long		ikptr_t;
typedef signed long		iksword_t;
typedef unsigned long		ikuword_t;
#endif

typedef struct ikpcb_t {
  ikptr_t		dummy0;		/* ikptr_t allocation_pointer; */
  ikptr_t		dummy1;		/* ikptr_t allocation_redline; */
  ikptr_t		dummy2;		/* ikptr_t frame_pointer; */
  ikptr_t		dummy3;		/* ikptr_t frame_base; */
  ikptr_t		dummy4;		/* ikptr_t frame_redline; */
  ikptr_t		dummy5;		/* ikptr_t next_k; */
  ikptr_t		dummy6;		/* ikptr_t system_stack; */
  ikptr_t		dummy7;		/* ikptr_t dirty_vector; */
  ikptr_t		dummy8;		/* ikptr_t arg_list; */
  ikptr_t		dummy9;		/* ikptr_t engine_counter; */
  ikptr_t		dummy10;	/* ikptr_t interrupted; */
  ikptr_t		dummy11;	/* ikptr_t base_rtd; */
  ikptr_t		dummy12;	/* ikptr_t collect_key; */

  /* Additional roots for the garbage collector.  They are used to avoid
     collecting objects still in use while they are in use by C code. */
  ikptr_t*		root0;
  ikptr_t*		root1;
  ikptr_t*		root2;
  ikptr_t*		root3;
  ikptr_t*		root4;
  ikptr_t*		root5;
  ikptr_t*		root6;
  ikptr_t*		root7;
  ikptr_t*		root8;
  ikptr_t*		root9;

  /* Other fields not useful in the public API. */
} ikpcb_t;

/* NOTE Some day in a far,  far future these aliases will be deprecated.
   (Marco Maggi; Mon May 25, 2015) */
typedef ikptr_t			ikptr;
typedef ikpcb_t			ikpcb;


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

ik_api_decl ikpcb_t *	ik_the_pcb (void);
ik_api_decl void	ik_signal_dirt_in_page_of_pointer (ikpcb_t* pcb, ikptr_t s_pointer);
#define IK_SIGNAL_DIRT(PCB,PTR)		ik_signal_dirt_in_page_of_pointer((PCB),(PTR))

ik_api_decl int		ik_abort		(const char * error_message, ...);
ik_api_decl void	ik_error		(ikptr_t args);
ik_api_decl void	ik_debug_message	(const char * error_message, ...);

ik_api_decl ikptr_t	ik_unsafe_alloc		(ikpcb_t* pcb, ikuword_t size);
ik_api_decl ikptr_t	ik_safe_alloc		(ikpcb_t* pcb, ikuword_t size);

ik_api_decl void	ik_print		(ikptr_t x);
ik_api_decl void	ik_print_no_newline	(ikptr_t x);
ik_api_decl void	ik_fprint		(FILE*, ikptr_t x);


/** --------------------------------------------------------------------
 ** Basic object related macros.
 ** ----------------------------------------------------------------- */

#define IK_ALIGN_SHIFT	(1 + wordshift)
#define IK_ALIGN_SIZE	(2 * wordsize)
#define immediate_tag	7

#define IK_TAGOF(X)	(((ikuword_t)(X)) & 7)

#define IK_PTR(X,N)	((ikptr_t*)(((ikuword_t)(X)) + ((iksword_t)(N))))
#define IK_REF(X,N)	(IK_PTR(X,N)[0])

/* The smallest multiple of the wordsize which is greater than N. */
#define IK_ALIGN(N) \
  ((((N) + IK_ALIGN_SIZE - 1) >>  IK_ALIGN_SHIFT) << IK_ALIGN_SHIFT)

#define IK_FALSE_OBJECT		((ikptr_t)0x2F)
#define IK_TRUE_OBJECT		((ikptr_t)0x3F)
#define IK_NULL_OBJECT		((ikptr_t)0x4F)
#define IK_EOF_OBJECT		((ikptr_t)0x5F)
#define IK_VOID_OBJECT		((ikptr_t)0x7F)

/* Special machine word value stored in locations that used to hold weak
   references to values which have been already garbage collected. */
#define IK_BWP_OBJECT		((ikptr_t)0x8F)

/* Special machine word value stored  in the "value" and "proc" field of
   Scheme symbol memory blocks to signal that these fields are unset. */
#define IK_UNBOUND_OBJECT	((ikptr_t)0x6F)

#define IK_FALSE		IK_FALSE_OBJECT
#define IK_TRUE			IK_TRUE_OBJECT
#define IK_NULL			IK_NULL_OBJECT
#define IK_EOF			IK_EOF_OBJECT
#define IK_VOID			IK_VOID_OBJECT
#define IK_BWP			IK_BWP_OBJECT
#define IK_UNBOUND		IK_UNBOUND_OBJECT


/** --------------------------------------------------------------------
 ** Fixnum objects.
 ** ----------------------------------------------------------------- */

#define fx_tag		0
#define fx_shift	wordshift
#define fx_mask		(wordsize - 1)

#define most_positive_fixnum	(((ikuword_t)-1) >> (fx_shift+1))
#define most_negative_fixnum	(most_positive_fixnum+1)
#define IK_GREATEST_FIXNUM	most_positive_fixnum
#define IK_LEAST_FIXNUM		(-most_negative_fixnum)

#define IK_FIX(X)	((ikptr_t)(((ikuword_t)(X)) << fx_shift))
#define IK_UNFIX(X)	(((ikuword_t)(X)) >> fx_shift)
#define IK_IS_FIXNUM(X)	((((ikuword_t)(X)) & fx_mask) == fx_tag)

ik_api_decl ikptr_t	ikrt_fxrandom		(ikptr_t x);


/** --------------------------------------------------------------------
 ** Pair and list objects.
 ** ----------------------------------------------------------------- */

#define pair_size	(2 * wordsize)
#define pair_mask	7 /* #b111 */
#define pair_tag	1
#define disp_car	0
#define disp_cdr	wordsize
#define off_car		(disp_car - pair_tag)
#define off_cdr		(disp_cdr - pair_tag)

#define IK_IS_PAIR(X)	(pair_tag == (((ikuword_t)(X)) & pair_mask))

#define IK_CAR(PAIR)		IK_REF((PAIR), off_car)
#define IK_CDR(PAIR)		IK_REF((PAIR), off_cdr)
#define IK_CAAR(PAIR)		IK_CAR(IK_CAR(PAIR))
#define IK_CDAR(PAIR)		IK_CDR(IK_CAR(PAIR))
#define IK_CADR(PAIR)		IK_CAR(IK_CDR(PAIR))
#define IK_CDDR(PAIR)		IK_CDR(IK_CDR(PAIR))

#define IK_CAR_PTR(PAIR)	IK_PTR((PAIR), off_car)
#define IK_CDR_PTR(PAIR)	IK_PTR((PAIR), off_cdr)
#define IK_CAAR_PTR(PAIR)	IK_CAR_PTR(IK_CAR(PAIR))
#define IK_CDAR_PTR(PAIR)	IK_CDR_PTR(IK_CAR(PAIR))
#define IK_CADR_PTR(PAIR)	IK_CAR_PTR(IK_CDR(PAIR))
#define IK_CDDR_PTR(PAIR)	IK_CDR_PTR(IK_CDR(PAIR))

#define IKA_PAIR_ALLOC(PCB)	(ik_safe_alloc((PCB),  pair_size) | pair_tag)
#define IKU_PAIR_ALLOC(PCB)	(ik_unsafe_alloc((PCB),pair_size) | pair_tag)

ik_api_decl ikptr_t ika_pair_alloc		(ikpcb_t * pcb);
ik_api_decl ikptr_t iku_pair_alloc		(ikpcb_t * pcb);
ik_api_decl ikuword_t ik_list_length		(ikptr_t x);
ik_api_decl void ik_list_to_argv		(ikptr_t x, char **argv);
ik_api_decl void ik_list_to_argv_and_argc	(ikptr_t x, char **argv, long *argc);

ik_api_decl ikptr_t ika_list_from_argv	(ikpcb_t * pcb, char ** argv);
ik_api_decl ikptr_t ika_list_from_argv_and_argc(ikpcb_t * pcb, char ** argv, long argc);


/** --------------------------------------------------------------------
 ** Character objects.
 ** ----------------------------------------------------------------- */

typedef uint32_t	ikchar;

#define char_tag	0x0F
#define char_mask	0xFF
#define char_shift	8

#define IK_IS_CHAR(X)		(char_tag == (char_mask & (ikptr_t)(X)))

#define IK_CHAR_FROM_INTEGER(X) \
  ((ikptr_t)((((ikuword_t)(X)) << char_shift) | char_tag))

#define IK_CHAR32_FROM_INTEGER(X) \
  ((ikchar)((((ikuword_t)(X)) << char_shift) | char_tag))

#define IK_CHAR_TO_INTEGER(X) \
  ((ikuword_t)(((ikptr_t)(X)) >> char_shift))

#define IK_CHAR32_TO_INTEGER(X)		((uint32_t)(((ikchar)(X)) >> char_shift))

#define IK_UNICODE_FROM_ASCII(ASCII)	((ikuword_t)(ASCII))


/** --------------------------------------------------------------------
 ** String objects.
 ** ----------------------------------------------------------------- */

#define IK_STRING_CHAR_SIZE	4
#define string_mask		7
#define string_tag		6
#define disp_string_length	0
#define disp_string_data	wordsize
#define off_string_length	(disp_string_length - string_tag)
#define off_string_data		(disp_string_data   - string_tag)

#define IK_IS_STRING(X)			(string_tag == (string_mask & (ikptr_t)(X)))
#define IK_STRING_LENGTH_FX(STR)	IK_REF((STR), off_string_length)
#define IK_STRING_LENGTH(STR)		IK_UNFIX(IK_REF((STR), off_string_length))
#define IK_CHAR32(STR,IDX)		(((ikchar*)(((ikptr_t)(STR)) + off_string_data))[IDX])

#define IK_STRING_DATA_VOIDP(STR)	((void*)(((ikptr_t)(STR)) + off_string_data))
#define IK_STRING_DATA_IKCHARP(STR)	((ikchar*)(((ikptr_t)(STR)) + off_string_data))

ik_api_decl ikptr_t ika_string_alloc		(ikpcb_t * pcb, ikuword_t number_of_chars);
ik_api_decl ikptr_t ika_string_from_cstring	(ikpcb_t * pcb, const char * cstr);

ik_api_decl ikptr_t iku_string_alloc		(ikpcb_t * pcb, ikuword_t number_of_chars);
ik_api_decl ikptr_t iku_string_from_cstring	(ikpcb_t * pcb, const char * cstr);
ik_api_decl ikptr_t iku_string_to_symbol		(ikpcb_t * pcb, ikptr_t s_str);

ik_api_decl ikptr_t ikrt_string_to_symbol	(ikptr_t, ikpcb_t* pcb);
ik_api_decl ikptr_t ikrt_strings_to_gensym	(ikptr_t, ikptr_t,	ikpcb_t* pcb);


/** --------------------------------------------------------------------
 ** Symbol objects.
 ** ----------------------------------------------------------------- */

#define symbol_tag			((ikptr_t) 0x5F)
#define symbol_mask			((ikptr_t) 0xFF)
#define disp_symbol_record_tag		0
#define disp_symbol_record_string	(1 * wordsize)
#define disp_symbol_record_ustring	(2 * wordsize)
#define disp_symbol_record_value	(3 * wordsize)
#define disp_symbol_record_proc		(4 * wordsize)
#define disp_symbol_record_plist	(5 * wordsize)
#define symbol_record_size		(6 * wordsize)

#define off_symbol_record_tag		(disp_symbol_record_tag	    - vector_tag)
#define off_symbol_record_string	(disp_symbol_record_string  - vector_tag)
#define off_symbol_record_ustring	(disp_symbol_record_ustring - vector_tag)
#define off_symbol_record_value		(disp_symbol_record_value   - vector_tag)
#define off_symbol_record_proc		(disp_symbol_record_proc    - vector_tag)
#define off_symbol_record_plist		(disp_symbol_record_plist   - vector_tag)

ik_api_decl int   ik_is_symbol			(ikptr_t obj);
ik_api_decl ikptr_t iku_symbol_from_string	(ikpcb_t * pcb, ikptr_t s_str);


/** --------------------------------------------------------------------
 ** Bignum objects.
 ** ----------------------------------------------------------------- */

#define bignum_mask		0x7
#define bignum_tag		0x3
#define bignum_sign_mask	0x8
#define bignum_sign_shift	3
#define bignum_nlimbs_shift	4
#define disp_bignum_tag		0
#define disp_bignum_data	wordsize
#define off_bignum_tag		(disp_bignum_tag  - vector_tag)
#define off_bignum_data		(disp_bignum_data - vector_tag)

#define IK_BNFST_NEGATIVE(X)		(((ikuword_t)(X)) & bignum_sign_mask)
#define IK_BNFST_POSITIVE(X)		(!IK_BNFST_NEGATIVE(X))
#define IK_BNFST_LIMB_COUNT(X)		(((ikuword_t)(X)) >> bignum_nlimbs_shift)

#define IK_BIGNUM_ALLOC_SIZE(NUMBER_OF_LIMBS)			\
  IK_ALIGN(disp_bignum_data + (NUMBER_OF_LIMBS) * wordsize)

#define IKA_BIGNUM_ALLOC(PCB,LIMB_COUNT)	\
  (ik_safe_alloc((PCB), IK_BIGNUM_ALLOC_SIZE(LIMB_COUNT)) | vector_tag)

#define IK_COMPOSE_BIGNUM_FIRST_WORD(LIMB_COUNT,SIGN)		\
  ((ikptr_t)(((LIMB_COUNT) << bignum_nlimbs_shift) | (SIGN) | bignum_tag))

#define IK_POSITIVE_BIGNUM_FIRST_WORD(LIMB_COUNT)		\
  IK_COMPOSE_BIGNUM_FIRST_WORD((LIMB_COUNT),((0)<<bignum_sign_shift))

#define IK_NEGATIVE_BIGNUM_FIRST_WORD(LIMB_COUNT)		\
  IK_COMPOSE_BIGNUM_FIRST_WORD((LIMB_COUNT),((1)<<bignum_sign_shift))

#define IK_BIGNUM_DATA_LIMBP(X)					\
  ((mp_limb_t*)(ikuword_t)((X) + off_bignum_data))

#define IK_BIGNUM_DATA_VOIDP(X)					\
  ((void *)(ikuword_t)((X) + off_bignum_data))

#define IK_BIGNUM_FIRST_LIMB(X)					\
  ((mp_limb_t)IK_REF((X), off_bignum_data))

#define IK_BIGNUM_LAST_LIMB(X,LIMB_COUNT)			\
  ((mp_limb_t)IK_REF((X), off_bignum_data+((LIMB_COUNT)-1)*wordsize))

#define IK_BIGNUM_FIRST(X)	IK_REF((X), off_bignum_tag)
#define IK_LIMB(X,IDX)		IK_REF((X), off_bignum_data + (IDX)*wordsize)

ik_api_decl int	ik_is_bignum		(ikptr_t x);

ik_api_decl ikptr_t	ika_integer_from_int	(ikpcb_t* pcb, int N);
ik_api_decl ikptr_t	ika_integer_from_long	(ikpcb_t* pcb, long N);
ik_api_decl ikptr_t	ika_integer_from_llong	(ikpcb_t* pcb, ik_llong N);
ik_api_decl ikptr_t	ika_integer_from_uint	(ikpcb_t* pcb, ik_uint N);
ik_api_decl ikptr_t	ika_integer_from_ulong	(ikpcb_t* pcb, ik_ulong N);
ik_api_decl ikptr_t	ika_integer_from_ullong	(ikpcb_t* pcb, ik_ullong N);

ik_api_decl ikptr_t	ika_integer_from_sint8	(ikpcb_t* pcb, int8_t N);
ik_api_decl ikptr_t	ika_integer_from_sint16	(ikpcb_t* pcb, int16_t N);
ik_api_decl ikptr_t	ika_integer_from_sint32	(ikpcb_t* pcb, int32_t N);
ik_api_decl ikptr_t	ika_integer_from_sint64	(ikpcb_t* pcb, int64_t N);
ik_api_decl ikptr_t	ika_integer_from_uint8	(ikpcb_t* pcb, uint8_t N);
ik_api_decl ikptr_t	ika_integer_from_uint16	(ikpcb_t* pcb, uint16_t N);
ik_api_decl ikptr_t	ika_integer_from_uint32	(ikpcb_t* pcb, uint32_t N);
ik_api_decl ikptr_t	ika_integer_from_uint64	(ikpcb_t* pcb, uint64_t N);

ik_api_decl ikptr_t	ika_integer_from_off_t	(ikpcb_t * pcb, off_t N);
ik_api_decl ikptr_t	ika_integer_from_ssize_t(ikpcb_t * pcb, ssize_t N);
ik_api_decl ikptr_t	ika_integer_from_size_t	(ikpcb_t * pcb, size_t N);
ik_api_decl ikptr_t	ika_integer_from_ptrdiff_t (ikpcb_t * pcb, ptrdiff_t N);

ik_api_decl ikptr_t	ika_integer_from_sword	(ikpcb_t* pcb, iksword_t N);
ik_api_decl ikptr_t	ika_integer_from_uword	(ikpcb_t* pcb, ikuword_t N);

ik_api_decl int8_t	ik_integer_to_sint8	(ikptr_t x);
ik_api_decl int16_t	ik_integer_to_sint16	(ikptr_t x);
ik_api_decl int32_t	ik_integer_to_sint32	(ikptr_t x);
ik_api_decl int64_t	ik_integer_to_sint64	(ikptr_t x);
ik_api_decl uint8_t	ik_integer_to_uint8	(ikptr_t x);
ik_api_decl uint16_t	ik_integer_to_uint16	(ikptr_t x);
ik_api_decl uint32_t	ik_integer_to_uint32	(ikptr_t x);
ik_api_decl uint64_t	ik_integer_to_uint64	(ikptr_t x);

ik_api_decl int		ik_integer_to_int	(ikptr_t x);
ik_api_decl long	ik_integer_to_long	(ikptr_t x);
ik_api_decl ik_llong	ik_integer_to_llong	(ikptr_t x);
ik_api_decl ik_uint	ik_integer_to_uint	(ikptr_t x);
ik_api_decl ik_ulong	ik_integer_to_ulong	(ikptr_t x);
ik_api_decl ik_ullong	ik_integer_to_ullong	(ikptr_t x);

ik_api_decl off_t	ik_integer_to_off_t	(ikptr_t x);
ik_api_decl size_t	ik_integer_to_size_t	(ikptr_t x);
ik_api_decl ssize_t	ik_integer_to_ssize_t	(ikptr_t x);
ik_api_decl ptrdiff_t ik_integer_to_ptrdiff_t (ikptr_t x);

ik_api_decl iksword_t	ika_integer_to_sword	(ikpcb_t* pcb, ikptr_t X);
ik_api_decl ikuword_t	ika_integer_to_uword	(ikpcb_t* pcb, ikptr_t X);

/* inspection */
ik_api_decl ikptr_t	ikrt_positive_bn	(ikptr_t x);
ik_api_decl ikptr_t	ikrt_even_bn		(ikptr_t x);

/* arithmetics */
ik_api_decl ikptr_t	ikrt_fxfxplus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_fxbnplus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnbnplus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);

ik_api_decl ikptr_t	ikrt_fxfxminus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_fxbnminus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnfxminus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnbnminus		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);

ik_api_decl ikptr_t	ikrt_bnnegate		(ikptr_t x, ikpcb_t* pcb);

ik_api_decl ikptr_t	ikrt_fxfxmult		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_fxbnmult		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnbnmult		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);

ik_api_decl ikptr_t	ikrt_bnbncomp		(ikptr_t bn1, ikptr_t bn2);

ik_api_decl ikptr_t	ikrt_bnlognot		(ikptr_t x, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_fxbnlogand		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnbnlogand		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_fxbnlogor		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnbnlogor		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bignum_shift_right	(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_fixnum_shift_left	(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bignum_shift_left	(ikptr_t x, ikptr_t y, ikpcb_t* pcb);

ik_api_decl ikptr_t	ikrt_bnbndivrem		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnfxdivrem		(ikptr_t x, ikptr_t y, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bnfx_modulo	(ikptr_t x, ikptr_t y /*, ikpcb_t* pcb */);
ik_api_decl ikptr_t	ikrt_bignum_length	(ikptr_t x);

ik_api_decl ikptr_t	ikrt_exact_fixnum_sqrt	(ikptr_t fx /*, ikpcb_t* pcb*/);
ik_api_decl ikptr_t	ikrt_exact_bignum_sqrt	(ikptr_t bn, ikpcb_t* pcb);

ik_api_decl ikptr_t	ikrt_bignum_to_bytevector (ikptr_t x, ikpcb_t* pcb);
ik_api_decl ikptr_t	ikrt_bignum_to_flonum	(ikptr_t bn, ikptr_t more_bits, ikptr_t fl);

ik_api_decl ikptr_t	ikrt_bignum_hash	(ikptr_t bn /*, ikpcb_t* pcb */);


/** --------------------------------------------------------------------
 ** Ratnum objects.
 ** ----------------------------------------------------------------- */

#define ratnum_tag		((ikptr_t) 0x27)
#define disp_ratnum_tag		0
#define disp_ratnum_num		(1 * wordsize)
#define disp_ratnum_den		(2 * wordsize)
#define disp_ratnum_unused	(3 * wordsize)
#define ratnum_size		(4 * wordsize)

#define off_ratnum_tag		(disp_ratnum_tag    - vector_tag)
#define off_ratnum_num		(disp_ratnum_num    - vector_tag)
#define off_ratnum_den		(disp_ratnum_den    - vector_tag)
#define off_ratnum_unused	(disp_ratnum_unused - vector_tag)

#define IK_IS_RATNUM(X)		((vector_tag == IK_TAGOF(X)) && \
				 (ratnum_tag == IK_REF(X, off_ratnum_tag)))

#define IK_RATNUM_TAG(X)	IK_REF((X), off_ratnum_tag)
#define IK_RATNUM_NUM(X)	IK_REF((X), off_ratnum_num)
#define IK_RATNUM_DEN(X)	IK_REF((X), off_ratnum_den)

#define IK_RATNUM_NUM_PTR(X)	IK_PTR((X), off_ratnum_num)
#define IK_RATNUM_DEN_PTR(X)	IK_PTR((X), off_ratnum_den)

/* deprecated */
#define IK_NUMERATOR(X)		IK_RATNUM_NUM(X)
#define IK_DENOMINATOR(X)	IK_RATNUM_DEN(X)

ik_api_decl int		ik_is_ratnum			(ikptr_t X);
ik_api_decl ikptr_t	ika_ratnum_alloc_no_init	(ikpcb_t * pcb);
ik_api_decl ikptr_t	ika_ratnum_alloc_and_init	(ikpcb_t * pcb);


/** --------------------------------------------------------------------
 ** Compnum objects.
 ** ----------------------------------------------------------------- */

#define compnum_tag		((ikptr_t) 0x37)
#define disp_compnum_tag	0
#define disp_compnum_real	(1 * wordsize)
#define disp_compnum_imag	(2 * wordsize)
#define disp_compnum_unused	(3 * wordsize)
#define compnum_size		(4 * wordsize)

#define off_compnum_tag		(disp_compnum_tag    - vector_tag)
#define off_compnum_real	(disp_compnum_real   - vector_tag)
#define off_compnum_imag	(disp_compnum_imag   - vector_tag)
#define off_compnum_unused	(disp_compnum_unused - vector_tag)

#define IK_IS_COMPNUM(X)	((vector_tag  == IK_TAGOF(X)) && \
				 (compnum_tag == IK_COMPNUM_TAG(X)))

#define IK_COMPNUM_TAG(X)	IK_REF((X), off_compnum_tag)
#define IK_COMPNUM_REAL(X)	IK_REF((X), off_compnum_real)
#define IK_COMPNUM_IMAG(X)	IK_REF((X), off_compnum_imag)
#define IK_COMPNUM_REP(X)	IK_REF((X), off_compnum_real)
#define IK_COMPNUM_IMP(X)	IK_REF((X), off_compnum_imag)

#define IK_COMPNUM_REAL_PTR(X)	IK_PTR((X), off_compnum_real)
#define IK_COMPNUM_IMAG_PTR(X)	IK_PTR((X), off_compnum_imag)
#define IK_COMPNUM_REP_PTR(X)	IK_PTR((X), off_compnum_real)
#define IK_COMPNUM_IMP_PTR(X)	IK_PTR((X), off_compnum_imag)

ik_api_decl int		ik_is_compnum			(ikptr_t X);
ik_api_decl ikptr_t	ika_compnum_alloc_no_init	(ikpcb_t * pcb);
ik_api_decl ikptr_t	ika_compnum_alloc_and_init	(ikpcb_t * pcb);


/** --------------------------------------------------------------------
 ** Flonum objects.
 ** ----------------------------------------------------------------- */

#define flonum_tag		((ikptr_t)0x17)
#define flonum_size		16 /* four 32-bit words, two 64-bit words */
#define disp_flonum_tag		0 /* not f(wordsize) */
#define disp_flonum_data	8 /* not f(wordsize) */
#define off_flonum_tag		(disp_flonum_tag  - vector_tag)
#define off_flonum_data		(disp_flonum_data - vector_tag)

#define IKU_DEFINE_AND_ALLOC_FLONUM(VARNAME)				\
  ikptr_t VARNAME = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;	\
  IK_REF(VARNAME, off_flonum_tag) = (ikptr_t)flonum_tag

#define IK_FLONUM_TAG(X)	IK_REF((X), off_flonum_tag)
#define IK_FLONUM_DATA(X)	(*((double*)(((ikuword_t)(X))+off_flonum_data)))
#define IK_FLONUM_VOIDP(X)	((void*)(((ikuword_t)(X))+((iksword_t)off_flonum_data)))

#define IK_IS_FLONUM(X)		((vector_tag == IK_TAGOF(X)) && (flonum_tag == IK_FLONUM_TAG(X)))

ik_api_decl int     ik_is_flonum		(ikptr_t obj);
ik_api_decl ikptr_t iku_flonum_alloc		(ikpcb_t * pcb, double fl);
ik_api_decl ikptr_t ika_flonum_from_double	(ikpcb_t* pcb, double N);
ik_api_decl ikptr_t ikrt_flonum_hash		(ikptr_t x /*, ikpcb_t* pcb */);


/** --------------------------------------------------------------------
 ** Cflonum objects.
 ** ----------------------------------------------------------------- */

#define cflonum_tag		((ikptr_t) 0x47)
#define disp_cflonum_tag	0
#define disp_cflonum_real	(1 * wordsize)
#define disp_cflonum_imag	(2 * wordsize)
#define disp_cflonum_unused	(3 * wordsize)
#define cflonum_size		(4 * wordsize)

#define off_cflonum_tag		(disp_cflonum_tag    - vector_tag)
#define off_cflonum_real	(disp_cflonum_real   - vector_tag)
#define off_cflonum_imag	(disp_cflonum_imag   - vector_tag)
#define off_cflonum_unused	(disp_cflonum_unused - vector_tag)

#define IK_IS_CFLONUM(X)	((vector_tag  == IK_TAGOF(X)) && \
				 (cflonum_tag == IK_CFLONUM_TAG(X)))

#define IKU_DEFINE_AND_ALLOC_CFLONUM(VARNAME)				\
    ikptr_t VARNAME = ik_unsafe_alloc(pcb, cflonum_size) | vector_tag;	\
    IK_REF(VARNAME, off_cflonum_tag) = (ikptr_t)cflonum_tag;

#define IK_CFLONUM_TAG(X)	IK_REF((X), off_cflonum_tag)
#define IK_CFLONUM_REAL(X)	IK_REF((X), off_cflonum_real)
#define IK_CFLONUM_IMAG(X)	IK_REF((X), off_cflonum_imag)
#define IK_CFLONUM_REP(X)	IK_REF((X), off_cflonum_real)
#define IK_CFLONUM_IMP(X)	IK_REF((X), off_cflonum_imag)

#define IK_CFLONUM_REAL_PTR(X)	IK_PTR((X), off_cflonum_real)
#define IK_CFLONUM_IMAG_PTR(X)	IK_PTR((X), off_cflonum_imag)
#define IK_CFLONUM_REP_PTR(X)	IK_PTR((X), off_cflonum_real)
#define IK_CFLONUM_IMP_PTR(X)	IK_PTR((X), off_cflonum_imag)

#define IK_CFLONUM_REAL_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_REAL(X))
#define IK_CFLONUM_IMAG_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_IMAG(X))
#define IK_CFLONUM_REP_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_REAL(X))
#define IK_CFLONUM_IMP_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_IMAG(X))

ik_api_decl int		ik_is_cflonum			(ikptr_t X);
ik_api_decl ikptr_t	iku_cflonum_alloc_and_init	(ikpcb_t * pcb, double re, double im);
ik_api_decl ikptr_t	ika_cflonum_from_doubles	(ikpcb_t * pcb, double re, double im);


/** --------------------------------------------------------------------
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

#define pointer_tag		((ikptr_t) 0x107)
#define disp_pointer_tag	0
#define disp_pointer_data	(1 * wordsize)
#define pointer_size		(2 * wordsize)
#define off_pointer_tag		(disp_pointer_tag  - vector_tag)
#define off_pointer_data	(disp_pointer_data - vector_tag)

ik_api_decl ikptr_t	ika_pointer_alloc	(ikpcb_t* pcb, ikuword_t memory);
ik_api_decl ikptr_t	iku_pointer_alloc	(ikpcb_t* pcb, ikuword_t memory);
ik_api_decl ikptr_t	ikrt_is_pointer		(ikptr_t X);
ik_api_decl int		ik_is_pointer		(ikptr_t X);

#define IK_IS_POINTER(X)		((vector_tag  == IK_TAGOF(X)) && \
					 (pointer_tag == IK_POINTER_TAG(X)))

#define IK_POINTER_TAG(X)		IK_REF((X), off_pointer_tag)

#define IK_POINTER_DATA(X)		IK_REF((X), off_pointer_data)
#define IK_POINTER_DATA_VOIDP(X)	((void *)   IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_CHARP(X)	((char *)   IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_UINT8P(X)	((uint8_t *)IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_LONG(X)		((long)	    IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_LLONG(X)	((ik_llong) IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_ULONG(X)	((ik_ulong) IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_ULLONG(X)	((ik_ullong)IK_REF((X), off_pointer_data))

#define IK_POINTER_SET(X,P)	(IK_REF((X), off_pointer_data) = (ikptr_t)((void*)(P)))
#define IK_POINTER_SET_NULL(X)	(IK_REF((X), off_pointer_data) = 0)
#define IK_POINTER_IS_NULL(X)	(0 == IK_POINTER_DATA(X))


/** --------------------------------------------------------------------
 ** Vector objects.
 ** ----------------------------------------------------------------- */

#define vector_mask		7
#define vector_tag		5
#define disp_vector_length	0
#define disp_vector_data	wordsize
#define off_vector_length	(disp_vector_length - vector_tag)
#define off_vector_data		(disp_vector_data   - vector_tag)

ik_api_decl ikptr_t ika_vector_alloc_no_init	(ikpcb_t * pcb, ikuword_t number_of_items);
ik_api_decl ikptr_t ika_vector_alloc_and_init	(ikpcb_t * pcb, ikuword_t number_of_items);

ik_api_decl ikptr_t iku_vector_alloc_no_init	(ikpcb_t * pcb, ikuword_t number_of_items);
ik_api_decl ikptr_t iku_vector_alloc_and_init (ikpcb_t * pcb, ikuword_t number_of_items);

ik_api_decl int   ik_is_vector		(ikptr_t s_vec);
ik_api_decl ikptr_t ikrt_vector_clean	(ikptr_t s_vec);
ik_api_decl ikptr_t ikrt_vector_copy	(ikptr_t s_dst, ikptr_t s_dst_start,
					 ikptr_t s_src, ikptr_t s_src_start,
					 ikptr_t s_count, ikpcb_t * pcb);

#define IK_IS_VECTOR(OBJ)		((vector_tag == ((OBJ) & vector_mask)) && IK_IS_FIXNUM(IK_REF((OBJ), off_vector_length)))

#define IK_VECTOR_LENGTH_FX(VEC)	IK_REF((VEC), off_vector_length)
#define IK_VECTOR_LENGTH(VEC)		IK_UNFIX(IK_VECTOR_LENGTH_FX(VEC))
#define IK_ITEM(VEC,IDX)		IK_REF((VEC), off_vector_data + (IDX) * wordsize)
#define IK_VECTOR_DATA_VOIDP(VEC)	((void*)((ikptr_t)((VEC)+off_vector_data)))

#define IK_ITEM_PTR(VEC,IDX)		IK_PTR((VEC), off_vector_data + (IDX) * wordsize)


/** --------------------------------------------------------------------
 ** Bytevector objects.
 ** ----------------------------------------------------------------- */

#define bytevector_mask		7
#define bytevector_tag		2
#define disp_bytevector_length	0
#define disp_bytevector_data	8 /* not f(wordsize) */
#define off_bytevector_length	(disp_bytevector_length - bytevector_tag)
#define off_bytevector_data	(disp_bytevector_data	- bytevector_tag)

#define IK_IS_BYTEVECTOR(X)	(bytevector_tag == (((ikuword_t)(X)) & bytevector_mask))

ik_api_decl ikptr_t ika_bytevector_alloc		(ikpcb_t * pcb, ikuword_t requested_number_of_bytes);
ik_api_decl ikptr_t ika_bytevector_from_cstring	(ikpcb_t * pcb, const char * cstr);
ik_api_decl ikptr_t ika_bytevector_from_cstring_len	(ikpcb_t * pcb, const char * cstr, size_t len);
ik_api_decl ikptr_t ika_bytevector_from_memory_block	(ikpcb_t * pcb, const void * memory,
						 size_t length);
ik_api_decl ikptr_t ika_bytevector_from_utf16z	(ikpcb_t * pcb, const void * data);
ik_api_decl ikptr_t ikrt_bytevector_copy (ikptr_t s_dst, ikptr_t s_dst_start,
				    ikptr_t s_src, ikptr_t s_src_start,
				    ikptr_t s_count);

#define IK_BYTEVECTOR_LENGTH_FX(BV)	IK_REF((BV), off_bytevector_length)
#define IK_BYTEVECTOR_LENGTH(BV)	IK_UNFIX(IK_BYTEVECTOR_LENGTH_FX(BV))

#define IK_BYTEVECTOR_DATA(BV)		((ikuword_t)((BV) + off_bytevector_data))
#define IK_BYTEVECTOR_DATA_VOIDP(BV)	((void*)   IK_BYTEVECTOR_DATA(BV))
#define IK_BYTEVECTOR_DATA_CHARP(BV)	((char*)   IK_BYTEVECTOR_DATA(BV))
#define IK_BYTEVECTOR_DATA_UINT8P(BV)	((uint8_t*)IK_BYTEVECTOR_DATA(BV))


/** --------------------------------------------------------------------
 ** Struct objects.
 ** ----------------------------------------------------------------- */

#define record_mask		7
#define record_tag		vector_tag
#define disp_record_rtd		0
#define disp_record_data	wordsize
#define off_record_rtd		(disp_record_rtd  - record_tag)
#define off_record_data		(disp_record_data - record_tag)

#define rtd_tag			record_tag
#define disp_rtd_rtd		0
#define disp_rtd_name		(1 * wordsize)
#define disp_rtd_length		(2 * wordsize)
#define disp_rtd_fields		(3 * wordsize)
#define disp_rtd_printer	(4 * wordsize)
#define disp_rtd_symbol		(5 * wordsize)
#define disp_rtd_destructor	(6 * wordsize)
#define rtd_size		(7 * wordsize)

#define off_rtd_rtd		(disp_rtd_rtd	  - rtd_tag)
#define off_rtd_name		(disp_rtd_name	  - rtd_tag)
#define off_rtd_length		(disp_rtd_length  - rtd_tag)
#define off_rtd_fields		(disp_rtd_fields  - rtd_tag)
#define off_rtd_printer		(disp_rtd_printer - rtd_tag)
#define off_rtd_symbol		(disp_rtd_symbol  - rtd_tag)
#define off_rtd_destructor	(disp_rtd_destructor - rtd_tag)

ik_api_decl ikptr_t ika_struct_alloc_and_init	(ikpcb_t * pcb, ikptr_t rtd);
ik_api_decl ikptr_t ika_struct_alloc_no_init	(ikpcb_t * pcb, ikptr_t rtd);
ik_api_decl int     ik_is_struct		(ikptr_t R);

#define IK_IS_STRUCT(OBJ)		((record_tag == (record_mask & (OBJ))) && \
					 (record_tag == (record_mask & IK_STRUCT_STD(OBJ))))

#define IK_STD_STD(STD)			IK_REF((STD), off_rtd_rtd)
#define IK_STD_NAME(STD)		IK_REF((STD), off_rtd_name)
#define IK_STD_LENGTH(STD)		IK_REF((STD), off_rtd_length)
#define IK_STD_FIELDS(STD)		IK_REF((STD), off_rtd_fields)
#define IK_STD_PRINTER(STD)		IK_REF((STD), off_rtd_printer)
#define IK_STD_SYMBOL(STD)		IK_REF((STD), off_rtd_symbol)
#define IK_STD_DESTRUCTOR(STD)		IK_REF((STD), off_rtd_destructor)

#define IK_STRUCT_RTD(STRUCT)		IK_REF((STRUCT), off_record_rtd)
#define IK_STRUCT_STD(STRUCT)		IK_REF((STRUCT), off_record_rtd)
#define IK_STRUCT_RTD_PTR(STRUCT)	IK_PTR((STRUCT), off_record_rtd)
#define IK_STRUCT_STD_PTR(STRUCT)	IK_PTR((STRUCT), off_record_rtd)

#define IK_FIELD(STRUCT,FIELD)		IK_REF((STRUCT), (off_record_data+(FIELD)*wordsize))
#define IK_FIELD_PTR(STRUCT,FIELD)	IK_PTR((STRUCT), (off_record_data+(FIELD)*wordsize))

#define IK_STRUCT_FIELDS_VOIDP(STRU)	((void *)((STRU) + off_record_data))


/** --------------------------------------------------------------------
 ** Port objects.
 ** ----------------------------------------------------------------- */

#define port_tag		0x3F
#define port_mask		0x3F
#define disp_port_attrs		0)
#define disp_port_index		(1 * wordsize)
#define disp_port_size		(2 * wordsize)
#define disp_port_buffer	(3 * wordsize)
#define disp_port_transcoder	(4 * wordsize)
#define disp_port_id		(5 * wordsize)
#define disp_port_read		(6 * wordsize)
#define disp_port_write		(7 * wordsize)
#define disp_port_get_position	(8 * wordsize)
#define disp_port_set_position	(9 * wordsize)
#define disp_port_close		(10 * wordsize)
#define disp_port_cookie	(11 * wordsize)
#define disp_port_unused1	(12 * wordsize)
#define disp_port_unused2	(13 * wordsize)
#define port_size		(14 * wordsize)

#define off_port_attrs		(disp_port_attrs	- vector_tag)
#define off_port_index		(disp_port_index	- vector_tag)
#define off_port_size		(disp_port_size		- vector_tag)
#define off_port_buffer		(disp_port_buffer	- vector_tag)
#define off_port_transcoder	(disp_port_transcoder	- vector_tag)
#define off_port_id		(disp_port_id		- vector_tag)
#define off_port_read		(disp_port_read		- vector_tag)
#define off_port_write		(disp_port_write	- vector_tag)
#define off_port_get_position	(disp_port_get_position	- vector_tag)
#define off_port_set_position	(disp_port_set_position	- vector_tag)
#define off_port_close		(disp_port_close	- vector_tag)
#define off_port_cookie		(disp_port_cookie	- vector_tag)
#define off_port_unused1	(disp_port_unused1	- vector_tag)
#define off_port_unused2	(disp_port_unused2	- vector_tag)


/** --------------------------------------------------------------------
 ** Code objects.
 ** ----------------------------------------------------------------- */

/* To assert  that an object  reference X (tagged pointer)  references a
   code object we do:

     assert(code_primary_tag == (code_primary_mask & X));
     assert(code_tag         == IK_REF(X, off_code_tag));
*/
#define code_primary_mask	vector_mask
#define code_primary_tag	vector_tag
#define code_tag		((ikptr_t)0x2F)

#define disp_code_tag		0
#define disp_code_code_size	(1 * wordsize)
#define disp_code_reloc_vector	(2 * wordsize)
#define disp_code_freevars	(3 * wordsize)
#define disp_code_annotation	(4 * wordsize)
#define disp_code_unused	(5 * wordsize)
#define disp_code_data		(6 * wordsize)
#define off_code_tag		(disp_code_tag		- code_primary_tag)
#define off_code_annotation	(disp_code_annotation	- code_primary_tag)
#define off_code_data		(disp_code_data		- code_primary_tag)
#define off_code_reloc_vector	(disp_code_reloc_vector - code_primary_tag)

#define IK_IS_CODE(X)		\
     ((code_primary_tag == (code_primary_mask & X)) && \
      (code_tag         == IK_REF(X, off_code_tag)))

/* Given a reference  to code object: return a raw  pointer to the entry
   point in the code, as "ikptr_t". */
#define IK_CODE_ENTRY_POINT(CODE)	(((ikptr_t)(CODE)) + ((ikptr_t)off_code_data))


/** --------------------------------------------------------------------
 ** Closure objects.
 ** ----------------------------------------------------------------- */

#define closure_tag		3
#define closure_mask		7
#define disp_closure_code	0
#define disp_closure_data	wordsize
#define off_closure_code	(disp_closure_code - closure_tag)
#define off_closure_data	(disp_closure_data - closure_tag)

#define IK_IS_CLOSURE(X)	((((ikuword_t)(X)) & closure_mask) == closure_tag)

#define IK_CLOSURE_ENTRY_POINT(X)	IK_REF((X),off_closure_code)
#define IK_CLOSURE_CODE_OBJECT(X)	(IK_CLOSURE_ENTRY_POINT(X)-off_code_data)
#define IK_CLOSURE_NUMBER_OF_FREE_VARS(X)	\
  IK_UNFIX(IK_REF(IK_CLOSURE_CODE_OBJECT(X), off_code_freevars))
#define IK_CLOSURE_FREE_VAR(X,IDX)	IK_REF((X),off_closure_data+wordsize*(IDX))


/** --------------------------------------------------------------------
 ** Continuation objects.
 ** ----------------------------------------------------------------- */

#define continuation_primary_mask	vector_mask
#define continuation_primary_tag	vector_tag

/* ------------------------------------------------------------------ */

#define continuation_tag		((ikptr_t)0x1F)
#define disp_continuation_tag		0
#define disp_continuation_top		(1 * wordsize)
#define disp_continuation_size		(2 * wordsize)
#define disp_continuation_next		(3 * wordsize)
#define continuation_size		(4 * wordsize)

#define off_continuation_tag		(disp_continuation_tag	- vector_tag)
#define off_continuation_top		(disp_continuation_top	- vector_tag)
#define off_continuation_size		(disp_continuation_size - vector_tag)
#define off_continuation_next		(disp_continuation_next - vector_tag)

#define IK_IS_CONTINUATION(X)		\
   ((continuation_primary_tag == (continuation_primary_mask & (X))) &&	\
    (continuation_tag         == IK_REF((X), off_continuation_tag)))

/* ------------------------------------------------------------------ */

#define system_continuation_tag		((ikptr_t) 0x11F)
#define disp_system_continuation_tag	0
#define disp_system_continuation_top	(1 * wordsize)
#define disp_system_continuation_next	(2 * wordsize)
#define disp_system_continuation_unused (3 * wordsize)
#define system_continuation_size	(4 * wordsize)

#define off_system_continuation_tag	(disp_system_continuation_tag	 - vector_tag)
#define off_system_continuation_top	(disp_system_continuation_top	 - vector_tag)
#define off_system_continuation_next	(disp_system_continuation_next	 - vector_tag)
#define off_system_continuation_unused	(disp_system_continuation_unused - vector_tag)

#define IK_IS_SYSTEM_CONTINUATION(X)	\
   ((continuation_primary_tag == (continuation_primary_mask & (X))) &&	\
    (system_continuation_tag  == IK_REF((X), off_system_continuation_tag)))

/* ------------------------------------------------------------------ */

#define IK_IS_ANY_CONTINUATION(X)	\
   (IK_IS_CONTINUATION(X) || IK_IS_SYSTEM_CONTINUATION(X))


/** --------------------------------------------------------------------
 ** Tail-conc bucket (tcbucket) objects.
 ** ----------------------------------------------------------------- */

#define disp_tcbucket_tconc	(0 * wordsize)
#define disp_tcbucket_key	(1 * wordsize)
#define disp_tcbucket_val	(2 * wordsize)
#define disp_tcbucket_next	(3 * wordsize)
#define tcbucket_size		(4 * wordsize)

#define off_tcbucket_tconc	(disp_tcbucket_tconc - vector_tag)
#define off_tcbucket_key	(disp_tcbucket_key   - vector_tag)
#define off_tcbucket_val	(disp_tcbucket_val   - vector_tag)
#define off_tcbucket_next	(disp_tcbucket_next  - vector_tag)


/** --------------------------------------------------------------------
 ** Miscellanous functions.
 ** ----------------------------------------------------------------- */

ik_api_decl ikptr_t ikrt_general_copy (ikptr_t s_dst, ikptr_t s_dst_start,
				     ikptr_t s_src, ikptr_t s_src_start,
				     ikptr_t s_count, ikpcb_t * pcb);

ik_api_decl void ik_enter_c_function (ikpcb_t* pcb);
ik_api_decl void ik_leave_c_function (ikpcb_t* pcb);


/** --------------------------------------------------------------------
 ** Special exact integer object macros.
 ** ----------------------------------------------------------------- */

#define IK_IS_INTEGER(OBJ)	(IK_IS_FIXNUM(OBJ)||ik_is_bignum(OBJ))


/** --------------------------------------------------------------------
 ** Special boolean object macros.
 ** ----------------------------------------------------------------- */

#define IK_IS_BOOLEAN(OBJ)		((IK_FALSE == (OBJ)) || (IK_TRUE == (OBJ)))
#define IK_BOOLEAN_TO_INT(OBJ)		(!(IK_FALSE == (OBJ)))
#define IK_BOOLEAN_FROM_INT(INT)	((INT)? IK_TRUE : IK_FALSE)


/** --------------------------------------------------------------------
 ** Special memory-block object macros.
 ** ----------------------------------------------------------------- */

#define IK_MBLOCK_POINTER(OBJ)		IK_FIELD(OBJ, 0)
#define IK_MBLOCK_SIZE(OBJ)		IK_FIELD(OBJ, 1)
#define IK_MBLOCK_DATA_VOIDP(OBJ)	IK_POINTER_DATA_VOIDP(IK_MBLOCK_POINTER(OBJ))
#define IK_MBLOCK_DATA_CHARP(OBJ)	IK_POINTER_DATA_CHARP(IK_MBLOCK_POINTER(OBJ))
#define IK_MBLOCK_SIZE_T(OBJ)		ik_integer_to_size_t(IK_MBLOCK_SIZE(OBJ))


/** --------------------------------------------------------------------
 ** Special macros extracting "void *" pointers from objects.
 ** ----------------------------------------------------------------- */

/* pointer, false */
#define IK_POINTER_FROM_POINTER_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_POINTER_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_POINTER_DATA_VOIDP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, false */
#define IK_POINTER_FROM_BYTEVECTOR_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_BYTEVECTOR_DATA_VOIDP(OBJ))

/* ------------------------------------------------------------------ */

/* mblock, false */
#define IK_POINTER_FROM_MBLOCK_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_MBLOCK_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_MBLOCK_DATA_VOIDP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(OBJ) \
  ((IK_IS_BYTEVECTOR(OBJ))? IK_BYTEVECTOR_DATA_VOIDP(OBJ) : IK_POINTER_DATA_VOIDP(OBJ))

/* bytevector, pointer, false */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(OBJ))

/* ------------------------------------------------------------------ */

/* pointer, mblock */
#define IK_POINTER_FROM_POINTER_OR_MBLOCK(OBJ) \
          IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ)
#define   IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_POINTER(OBJ)? IK_POINTER_DATA_VOIDP(OBJ) : IK_MBLOCK_DATA_VOIDP(OBJ))

/* pointer, mblock, false */
#define IK_POINTER_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)	\
          IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)	\
  ((IK_FALSE == (OBJ))? NULL : IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer, mblock */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_BYTEVECTOR(OBJ)? IK_BYTEVECTOR_DATA_VOIDP(OBJ) : IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ))

/* bytevector, pointer, mblock, false */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ))


/** --------------------------------------------------------------------
 ** Special macros extracting "char *" pointers from objects.
 ** ----------------------------------------------------------------- */

/* pointer, false */
#define IK_CHARP_FROM_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_POINTER_DATA_CHARP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, false */
#define IK_CHARP_FROM_BYTEVECTOR_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_BYTEVECTOR_DATA_CHARP(OBJ))

/* ------------------------------------------------------------------ */

/* mblock, false */
#define IK_CHARP_FROM_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_MBLOCK_DATA_CHARP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(OBJ) \
  ((IK_IS_BYTEVECTOR(OBJ))? IK_BYTEVECTOR_DATA_CHARP(OBJ) : IK_POINTER_DATA_CHARP(OBJ))

/* bytevector, pointer, false */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(OBJ))

/* ------------------------------------------------------------------ */

/* pointer, mblock */
#define IK_CHARP_FROM_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_POINTER(OBJ)? IK_POINTER_DATA_CHARP(OBJ) : IK_MBLOCK_DATA_CHARP(OBJ))

/* pointer, mblock, false */
#define IK_CHARP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)	\
  ((IK_FALSE == (OBJ))? NULL : IK_CHARP_FROM_POINTER_OR_MBLOCK(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer, mblock */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_BYTEVECTOR(OBJ)? IK_BYTEVECTOR_DATA_CHARP(OBJ) : IK_CHARP_FROM_POINTER_OR_MBLOCK(OBJ))

/* bytevector, pointer, mblock, false */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ))


/** --------------------------------------------------------------------
 ** Generalised C buffer stuff.
 ** ----------------------------------------------------------------- */

ik_api_decl size_t ik_generalised_c_buffer_len (ikptr_t s_buffer, ikptr_t s_buffer_len);

/* ------------------------------------------------------------------ */

/* generalised C buffer */
#define IK_GENERALISED_C_BUFFER(OBJ)	\
  IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)

/* generalised C buffer or false */
#define IK_GENERALISED_C_BUFFER_OR_FALSE(OBJ)	\
  IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ)

/* ------------------------------------------------------------------ */

/* generalised sticky C buffer */
#define IK_GENERALISED_C_STICKY_BUFFER(OBJ)	\
  IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ)

/* generalised sticky C buffer or false */
#define IK_GENERALISED_C_STICKY_BUFFER_OR_FALSE(OBJ)	\
  IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)

/* ------------------------------------------------------------------ */

/* generalised C string */
#define IK_GENERALISED_C_STRING(OBJ)	\
  IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)

/* generalised C string or false */
#define IK_GENERALISED_C_STRING_OR_FALSE(OBJ)	\
  IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ)


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* ifndef VICARE_H */

/* end of file */
