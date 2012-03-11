/*
  Part of: Vicare
  Contents: internal header file
  Date: Wed Jan 11, 2012

  Abstract

	This   file  contains   internal  definitions.    Many   of  the
	definitions  in this  file are  duplicated in  "vicare.h", which
	defines the public API.

  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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

#ifndef INTERNALS_H
#define INTERNALS_H 1


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

/* I found  the following chunk on  the Net.  (Marco Maggi;  Sun Feb 26,
   2012) */
#if defined _WIN32 || defined __CYGWIN__
#  ifdef BUILDING_DLL
#    ifdef __GNUC__
#      define ik_decl		__attribute__((dllexport))
#    else
#      define ik_decl		__declspec(dllexport)
#    endif
#  else
#    ifdef __GNUC__
#      define ik_decl		__attribute__((dllimport))
#    else
#      define ik_decl		__declspec(dllimport)
#    endif
#  endif
#  define ik_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define ik_decl		__attribute__((visibility ("default")))
#    define ik_private_decl	__attribute__((visibility ("hidden")))
#  else
#    define ik_decl		extern
#    define ik_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> /* for off_t */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#ifdef HAVE_ASSERT_H
#  include <assert.h>
#endif
#ifdef HAVE_ERRNO_H
#  include <errno.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_NETDB_H
#  include <netdb.h>
#endif
#ifdef HAVE_STDARG_H
#  include <stdarg.h>
#endif
#ifdef HAVE_STDIO_H
#  include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif
#ifdef HAVE_STRING_H
#  include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#  include <strings.h>
#endif
#ifdef HAVE_ARPA_INET_H
#  include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif


/** --------------------------------------------------------------------
 ** Constants and preprocessor operations.
 ** ----------------------------------------------------------------- */

#define IK_GUARDIANS_GENERATION_NUMBER	0

#define IK_FORWARD_PTR		((ikptr)-1)
#define IK_MOST_BYTES_IN_MINOR	0x10000000

#define old_gen_mask		0x00000007
#define new_gen_mask		0x00000008
#define gen_mask		0x0000000F
#define new_gen_tag		0x00000008
#define meta_dirty_mask		0x000000F0
#define type_mask		0x00000F00
#define scannable_mask		0x0000F000
#define dealloc_mask		0x000F0000
#define large_object_mask	0x00100000
#define meta_dirty_shift	4

#define hole_type		0x00000000
#define mainheap_type		0x00000100
#define mainstack_type		0x00000200
#define pointers_type		0x00000300
#define dat_type		0x00000400
#define code_type		0x00000500
#define weak_pairs_type		0x00000600
#define symbols_type		0x00000700

#define scannable_tag		0x00001000
#define unscannable_tag		0x00000000

#define dealloc_tag_un		0x00010000
#define dealloc_tag_at		0x00020000
#define retain_tag		0x00000000

#define large_object_tag	0x00100000

#define hole_mt		(hole_type	 | unscannable_tag | retain_tag)
#define mainheap_mt	(mainheap_type	 | unscannable_tag | retain_tag)
#define mainstack_mt	(mainstack_type	 | unscannable_tag | retain_tag)
#define pointers_mt	(pointers_type	 | scannable_tag   | dealloc_tag_un)
#define symbols_mt	(symbols_type	 | scannable_tag   | dealloc_tag_un)
#define data_mt		(dat_type	 | unscannable_tag | dealloc_tag_un)
#define code_mt		(code_type	 | scannable_tag   | dealloc_tag_un)
#define weak_pairs_mt	(weak_pairs_type | scannable_tag   | dealloc_tag_un)

#define call_instruction_size	((wordsize == 4) ? 5 : 10)
#define disp_frame_size		(- (call_instruction_size + 3 * wordsize))
#define disp_frame_offset	(- (call_instruction_size + 2 * wordsize))
#define disp_multivale_rp	(- (call_instruction_size + 1 * wordsize))

/* ------------------------------------------------------------------ */

/* The IK_PAGESIZE  is used  to determine the  size of memory  blocks to
   allocate with "mmap()".

   IK_PAGESHIFT is the number of  bits to right-shift a pointer value to
   obtain the index of the page (of size IK_PAGESIZE) it is in.

       4000 >> 12 = 0
       8000 >> 12 = 1
      10000 >> 12 = 2

   The value 12 for pageshift is correct for a pagesize of 4096.

   #define IK_PAGESIZE		4096
   #define IK_PAGESHIFT		12

   These values  are determined by  the "configure" script,  because the
   page size  is platform-dependent.  In  truth we should  determine the
   page size at run time, but it would slow down computation of a lot of
   constants and structure sizes. */

#define generation_count	5  /* generations 0 (nursery), 1, 2, 3, 4 */

#define IK_HEAP_EXT_SIZE	(32 * 4096)
#define IK_HEAPSIZE		(1024 * 4096 * ((wordsize==4)?1:2)) /* 4/8 MB */

#define IK_FASL_HEADER		((sizeof(ikptr) == 4)? "#@IK01" : "#@IK02")
#define IK_FASL_HEADER_LEN	(strlen(IK_FASL_HEADER))

#define IK_PTR_PAGE_SIZE \
  ((IK_PAGESIZE - sizeof(long) - sizeof(struct ik_ptr_page*))/sizeof(ikptr))

/* Given the pointer X or tagged pointer X: evaluate to the index of the
   memory page  it is in; notice that  the tag bits of  a tagged pointer
   are not  influent.  Given a  number of bytes  X evaluate to  an index
   offset. */
#define IK_PAGE_INDEX(x)   \
  (((ik_ulong)(x)) >> IK_PAGESHIFT)

#define IK_ALIGN_TO_NEXT_PAGE(x) \
  (((IK_PAGESIZE - 1 + (ik_ulong)(x)) >> IK_PAGESHIFT) << IK_PAGESHIFT)

#define IK_ALIGN_TO_PREV_PAGE(x) \
  ((((ik_ulong)(x)) >> IK_PAGESHIFT) << IK_PAGESHIFT)

#define IK_ASS(LEFT,RIGHT)	\
  { ikptr s_tmp = (RIGHT); (LEFT) = s_tmp; }


/** --------------------------------------------------------------------
 ** Type definitions.
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

/* FIXME Should this be a "uintptr_t"? (Marco Maggi; Nov  6, 2011). */
typedef ik_ulong		ikptr;

typedef struct ikpage {
  ikptr		 base;
  struct ikpage* next;
} ikpage;

/* Node for linked list of allocated pages. */
typedef struct ikpages {
  ikptr		base;
  int		size;
  struct ikpages* next;
} ikpages;

/* Node in  a linked list  referencing all the generated  FFI callbacks.
   It is used  to allow the garbage collector not  to collect data still
   in  use by  the callbacks.	See "ikarus-ffi.c"  for details	 on this
   structure. */
typedef struct ik_callback_locative {
  void *	callable_pointer;	/* pointer to callable C function */
  void *	closure;		/* data generated by Libffi */
  ikptr		data;			/* Scheme value holding required data */
  struct ik_callback_locative * next;	/* pointer to next link */
} ik_callback_locative;

/* Node in a linked list. */
typedef struct ik_ptr_page {
  long		count;
  struct ik_ptr_page* next;
  ikptr		ptr[IK_PTR_PAGE_SIZE];
} ik_ptr_page;

typedef struct ikpcb {
  /* The  first locations  may	be  accessed by	 some  compiled code  to
     perform overflow/underflow ops. */
  ikptr	  allocation_pointer;		/* 32-bit offset =  0 */
  ikptr	  allocation_redline;		/* 32-bit offset =  4 */
  ikptr	  frame_pointer;		/* 32-bit offset =  8 */
  ikptr	  frame_base;			/* 32-bit offset = 12 */
  ikptr	  frame_redline;		/* 32-bit offset = 16 */
  ikptr	  next_k;			/* 32-bit offset = 20 */
  ikptr	  system_stack;			/* 32-bit offset = 24 */
  ikptr	  dirty_vector;			/* 32-bit offset = 28 */
  ikptr	  arg_list;			/* 32-bit offset = 32 */
  ikptr	  engine_counter;		/* 32-bit offset = 36 */
  ikptr	  interrupted;			/* 32-bit offset = 40 */
  ikptr	  base_rtd;			/* 32-bit offset = 44 */
  ikptr	  collect_key;			/* 32-bit offset = 48 */

  /* ------------------------------------------------------------------ */
  /* The  following fields are	not used  by any  scheme code  they only
     support the runtime system (GC, etc.) */

  /* Linked  list of  FFI callback  support data.   Used by  the garbage
     collector	not  to collect	 data  still  needed  by some  callbacks
     registered in data structures handled by foreign libraries. */
  ik_callback_locative * callbacks;

  /* Value of  "errno" right after the	last call to  a foreign function
     callout. */
  int			last_errno;

  /* Additional roots for the garbage collector.  They are used to avoid
     collecting objects still in use while they are in use by C code. */
  ikptr*		root0;
  ikptr*		root1;
  ikptr*		root2;
  ikptr*		root3;
  ikptr*		root4;
  ikptr*		root5;
  ikptr*		root6;
  ikptr*		root7;
  ikptr*		root8;
  ikptr*		root9;

  ik_uint *		segment_vector;
  ikptr			weak_pairs_ap;
  ikptr			weak_pairs_ep;
  /* Pointer to and number of  bytes of the current heap memory segment.
     New objects are allocated here. */
  ikptr			heap_base;
  ik_ulong		heap_size;
  /* Pointer to first node in  linked list of allocated memory segments.
     Initialised to  NULL when building	 the PCB.  Whenever  the current
     heap is full: a new node is prepended to the list, initialised with
     the fields "heap_base" and "heap_size". */
  ikpages*		heap_pages;
  /* Linked list of cached pages so that we don't map/unmap. */
  ikpage *		cached_pages;
  /* Linked list of cached ikpages so that we don't malloc/free. */
  ikpage *		uncached_pages;
  ikptr			cached_pages_base;
  int			cached_pages_size;
  ikptr			stack_base;
  ik_ulong		stack_size;
  /* The hash table holding interned symbols. */
  ikptr			symbol_table;
  /* The hash table holding interned generated symbols. */
  ikptr			gensym_table;
  /* Array of linked lists; one for each GC generation.  The linked list
     holds  references  to  Scheme  values  that  must  not  be  garbage
     collected  even   when  they   are  not  referenced,   for  example
     guardians. */
  ik_ptr_page*		protected_list[generation_count];
  ik_uint *		dirty_vector_base;
  ik_uint *		segment_vector_base;
  ikptr			memory_base;
  ikptr			memory_end;

  /* Number of garbage collections performed so far.  It is used: at the
     beginning	of a  GC ru,  to determine  which objects  generation to
     inspect; when reporting GC statistics to the user, to show how many
     GCs where performed between two timestamps. */
  int			collection_id;

  int			allocation_count_minor;
  int			allocation_count_major;

  /* Used for garbage collection statistics. */
  struct timeval	collect_utime;
  struct timeval	collect_stime;
  struct timeval	collect_rtime;

} ikpcb;

/* Node in a linked list of continuations. */
typedef struct ikcont {
  ikptr		tag;
  ikptr		top;
  long		size;
  ikptr		next;	/* pointer to next ikcont structure */
} ikcont;


/** --------------------------------------------------------------------
 ** Helper and legacy macros.
 ** ----------------------------------------------------------------- */

#define ref(X,N)	IK_REF((X),(N))


/** --------------------------------------------------------------------
 ** Internal function prototypes.
 ** ----------------------------------------------------------------- */

ik_decl ikpcb *		ik_collect		(unsigned long, ikpcb*);
ik_private_decl void	ik_verify_integrity	(ikpcb* pcb, char*);

ik_private_decl void*	ik_malloc		(int);
ik_private_decl void	ik_free			(void*, int);

ik_private_decl ikptr	ik_underflow_handler	(ikpcb*);

ik_private_decl ikptr	ik_mmap			(unsigned long);
ik_private_decl ikptr	ik_mmap_typed		(unsigned long size, unsigned type, ikpcb*);
ik_private_decl ikptr	ik_mmap_ptr		(unsigned long size, int gen, ikpcb*);
ik_private_decl ikptr	ik_mmap_data		(unsigned long size, int gen, ikpcb*);
ik_private_decl ikptr	ik_mmap_code		(unsigned long size, int gen, ikpcb*);
ik_private_decl ikptr	ik_mmap_mixed		(unsigned long size, ikpcb*);
ik_private_decl void	ik_munmap		(ikptr, unsigned long);
ik_private_decl ikpcb * ik_make_pcb		(void);
ik_private_decl void	ik_delete_pcb		(ikpcb*);
ik_private_decl void	ik_free_symbol_table	(ikpcb* pcb);

ik_private_decl void	ik_fasl_load		(ikpcb* pcb, char* filename);
ik_private_decl void	ik_relocate_code	(ikptr);

ik_private_decl ikptr	ik_exec_code		(ikpcb* pcb, ikptr code_ptr, ikptr argcount, ikptr cp);

ik_private_decl ikptr	ik_asm_enter		(ikpcb*, ikptr code_object, ikptr arg, ikptr cp);
ik_private_decl ikptr	ik_asm_reenter		(ikpcb*, ikptr code_object, ikptr val);


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

ik_decl ikpcb *	ik_the_pcb		(void);

ik_decl int	ik_abort		(const char * error_message, ...);
ik_decl void	ik_error		(ikptr args);
ik_decl void	ik_debug_message	(const char * error_message, ...);

ik_decl ikptr	ik_unsafe_alloc		(ikpcb* pcb, ik_ulong size);
ik_decl ikptr	ik_safe_alloc		(ikpcb* pcb, ik_ulong size);

ik_decl void	ik_print		(ikptr x);
ik_decl void	ik_print_no_newline	(ikptr x);
ik_decl void	ik_fprint		(FILE*, ikptr x);


/** --------------------------------------------------------------------
 ** Basic object related macros.
 ** ----------------------------------------------------------------- */

#define wordsize	((int)(sizeof(ikptr)))
/* The value of "wordshift" is selected in such a way that:

     length_in_bytes = number_of_words * wordsize
		     = number_of_words << wordshift

   this	 allows us,  for example,  to take  the fixnum	representing the
   number of items  in a vector and consider it directly  as size of the
   vector's data area in bytes. */
#define wordshift	((4 == wordsize)? 2 : 3)
#define IK_ALIGN_SHIFT	(1 + wordshift)
#define IK_ALIGN_SIZE	(2 * wordsize)
#define immediate_tag	7

#define IK_TAGOF(X)	(((int)(X)) & 7)

#define IK_REF(X,N)	(((ikptr*)(((long)(X)) + ((long)(N))))[0])

/* The smallest multiple of the wordsize which is greater than N. */
#define IK_ALIGN(N) \
  ((((N) + IK_ALIGN_SIZE - 1) >>  IK_ALIGN_SHIFT) << IK_ALIGN_SHIFT)

#define false_object		((ikptr)0x2F)
#define true_object		((ikptr)0x3F)
#define null_object		((ikptr)0x4F)
#define eof_object		((ikptr)0x5F)
#define void_object		((ikptr)0x7F)

/* Special machine word value stored in locations that used to hold weak
   references to values which have been already garbage collected. */
#define bwp_object		((ikptr)0x8F)

/* Special machine word value stored  in the "value" and "proc" field of
   Scheme symbol memory blocks to signal that these fields are unset. */
#define unbound_object		((ikptr)0x6F)


/** --------------------------------------------------------------------
 ** Code objects.
 ** ----------------------------------------------------------------- */

/* This	 is the	 primary tag,  in the  machine word  referencing  a code
   object. */
#define code_pri_tag		vector_tag
/* This is the	secondary tag, in the first word  of the referenced heap
   vector. */
#define code_tag		((ikptr)0x2F)
#define disp_code_code_tag	0
#define disp_code_code_size	(1 * wordsize)
#define disp_code_reloc_vector	(2 * wordsize)
#define disp_code_freevars	(3 * wordsize)
#define disp_code_annotation	(4 * wordsize)
#define disp_code_unused	(5 * wordsize)
#define disp_code_data		(6 * wordsize)
#define off_code_annotation	(disp_code_annotation	- code_pri_tag)
#define off_code_data		(disp_code_data		- code_pri_tag)
#define off_code_reloc_vector	(disp_code_reloc_vector - code_pri_tag)


/** --------------------------------------------------------------------
 ** Fixnum objects.
 ** ----------------------------------------------------------------- */

#define fx_tag		0
#define fx_shift	wordshift
#define fx_mask		(wordsize - 1)

#define most_positive_fixnum	(((ik_ulong)-1) >> (fx_shift+1))
#define most_negative_fixnum	(most_positive_fixnum+1)

#define IK_FIX(X)	((ikptr)(((long)(X)) << fx_shift))
#define IK_UNFIX(X)	(((long)(X)) >> fx_shift)
#define IK_IS_FIXNUM(X)	((((ik_ulong)(X)) & fx_mask) == fx_tag)

ik_decl ikptr	ikrt_fxrandom		(ikptr x);


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

#define IK_IS_PAIR(X)	(pair_tag == (((long)(X)) & pair_mask))

#define IK_CAR(PAIR)		    IK_REF((PAIR), off_car)
#define IK_CDR(PAIR)		    IK_REF((PAIR), off_cdr)
#define IK_CAAR(PAIR)		    IK_CAR(IK_CAR(PAIR))
#define IK_CDAR(PAIR)		    IK_CDR(IK_CAR(PAIR))
#define IK_CADR(PAIR)		    IK_CAR(IK_CDR(PAIR))
#define IK_CDDR(PAIR)		    IK_CDR(IK_CDR(PAIR))

#define IKA_PAIR_ALLOC(PCB)	(ik_safe_alloc((PCB),  pair_size) | pair_tag)
#define IKU_PAIR_ALLOC(PCB)	(ik_unsafe_alloc((PCB),pair_size) | pair_tag)

ik_decl ikptr ika_pair_alloc		(ikpcb * pcb);
ik_decl ikptr iku_pair_alloc		(ikpcb * pcb);
ik_decl long ik_list_length		(ikptr x);
ik_decl void ik_list_to_argv		(ikptr x, char **argv);
ik_decl void ik_list_to_argv_and_argc	(ikptr x, char **argv, long *argc);

ik_decl ikptr ika_list_from_argv	(ikpcb * pcb, char ** argv);
ik_decl ikptr ika_list_from_argv_and_argc(ikpcb * pcb, char ** argv, long argc);


/** --------------------------------------------------------------------
 ** Character objects.
 ** ----------------------------------------------------------------- */

typedef uint32_t	ikchar;

#define char_tag	0x0F
#define char_mask	0xFF
#define char_shift	8

#define IK_IS_CHAR(X)		(char_tag == (char_mask & (ikptr)(X)))

#define IK_CHAR_FROM_INTEGER(X) \
  ((ikptr)((((ik_ulong)(X)) << char_shift) | char_tag))

#define IK_CHAR32_FROM_INTEGER(X) \
  ((ikchar)((((ik_ulong)(X)) << char_shift) | char_tag))

#define IK_CHAR_TO_INTEGER(X) \
  ((ik_ulong)(((ikptr)(X)) >> char_shift))


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

#define IK_IS_STRING(X)			(string_tag == (string_mask & (ikptr)(X)))
#define IK_STRING_LENGTH_FX(STR)	IK_REF((STR), off_string_length)
#define IK_STRING_LENGTH(STR)		IK_UNFIX(IK_REF((STR), off_string_length))
#define IK_CHAR32(STR,IDX)		(((ikchar*)(((long)(STR)) + off_string_data))[IDX])

#define IK_STRING_DATA_VOIDP(STR)	((void*)(((long)(STR)) + off_string_data))

ik_decl ikptr ika_string_alloc		(ikpcb * pcb, long number_of_chars);
ik_decl ikptr ikrt_string_to_symbol	(ikptr, ikpcb* pcb);
ik_decl ikptr ikrt_strings_to_gensym	(ikptr, ikptr,	ikpcb* pcb);


/** --------------------------------------------------------------------
 ** Symbol objects.
 ** ----------------------------------------------------------------- */

#define symbol_tag			((ikptr) 0x5F)
#define symbol_mask			((ikptr) 0xFF)
#define disp_symbol_record_tag		0
#define disp_symbol_record_string	(1 * wordsize)
#define disp_symbol_record_ustring	(2 * wordsize)
#define disp_symbol_record_value	(3 * wordsize)
#define disp_symbol_record_proc		(4 * wordsize)
#define disp_symbol_record_plist	(5 * wordsize)
#define symbol_record_size		(6 * wordsize)

#define off_symbol_record_tag		(disp_symbol_record_tag	    - record_tag)
#define off_symbol_record_string	(disp_symbol_record_string  - record_tag)
#define off_symbol_record_ustring	(disp_symbol_record_ustring - record_tag)
#define off_symbol_record_value		(disp_symbol_record_value   - record_tag)
#define off_symbol_record_proc		(disp_symbol_record_proc    - record_tag)
#define off_symbol_record_plist		(disp_symbol_record_plist   - record_tag)

ik_decl int ik_is_symbol	(ikptr obj);


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

#define IK_BNFST_NEGATIVE(X)		(((ik_ulong)(X)) & bignum_sign_mask)
#define IK_BNFST_POSITIVE(X)		(!IK_BNFST_NEGATIVE(X))
#define IK_BNFST_LIMB_COUNT(X)		(((ik_ulong)(X)) >> bignum_nlimbs_shift)

#define IK_BIGNUM_ALLOC_SIZE(NUMBER_OF_LIMBS)			\
  IK_ALIGN(disp_bignum_data + (NUMBER_OF_LIMBS) * wordsize)

#define IKA_BIGNUM_ALLOC(PCB,LIMB_COUNT)	\
  (ik_safe_alloc((PCB), IK_BIGNUM_ALLOC_SIZE(LIMB_COUNT)) | vector_tag)

#define IK_COMPOSE_BIGNUM_FIRST_WORD(LIMB_COUNT,SIGN)		\
  ((ikptr)(((LIMB_COUNT) << bignum_nlimbs_shift) | (SIGN) | bignum_tag))

#define IK_POSITIVE_BIGNUM_FIRST_WORD(LIMB_COUNT)		\
  IK_COMPOSE_BIGNUM_FIRST_WORD((LIMB_COUNT),((0)<<bignum_sign_shift))

#define IK_NEGATIVE_BIGNUM_FIRST_WORD(LIMB_COUNT)		\
  IK_COMPOSE_BIGNUM_FIRST_WORD((LIMB_COUNT),((1)<<bignum_sign_shift))

#define IK_BIGNUM_DATA_LIMBP(X)					\
  ((mp_limb_t*)(long)(X + off_bignum_data))

#define IK_BIGNUM_FIRST_LIMB(X)					\
  ((mp_limb_t)IK_REF((X), off_bignum_data))

#define IK_BIGNUM_LAST_LIMB(X,LIMB_COUNT)			\
  ((mp_limb_t)IK_REF((X), off_bignum_data+((LIMB_COUNT)-1)*wordsize))

#define IK_BIGNUM_FIRST(X)	IK_REF((X), off_bignum_tag)
#define IK_LIMB(X,IDX)		IK_REF((X), off_bignum_data + (IDX)*wordsize)

ik_decl int	ik_is_bignum		(ikptr x);

ik_decl ikptr	ika_integer_from_int	(ikpcb* pcb, int N);
ik_decl ikptr	ika_integer_from_long	(ikpcb* pcb, long N);
ik_decl ikptr	ika_integer_from_llong	(ikpcb* pcb, ik_llong N);
ik_decl ikptr	ika_integer_from_uint	(ikpcb* pcb, ik_uint N);
ik_decl ikptr	ika_integer_from_ulong	(ikpcb* pcb, ik_ulong N);
ik_decl ikptr	ika_integer_from_ullong	(ikpcb* pcb, ik_ullong N);

ik_decl ikptr	ika_integer_from_sint32	(ikpcb* pcb, int32_t N);
ik_decl ikptr	ika_integer_from_sint64	(ikpcb* pcb, int64_t N);
ik_decl ikptr	ika_integer_from_uint32	(ikpcb* pcb, uint32_t N);
ik_decl ikptr	ika_integer_from_uint64	(ikpcb* pcb, uint64_t N);

ik_decl ikptr	ika_integer_from_off_t	(ikpcb * pcb, off_t N);
ik_decl ikptr	ika_integer_from_ssize_t(ikpcb * pcb, ssize_t N);
ik_decl ikptr	ika_integer_from_size_t	(ikpcb * pcb, size_t N);

ik_decl int32_t	 ik_integer_to_sint32	(ikptr x);
ik_decl int64_t	 ik_integer_to_sint64	(ikptr x);
ik_decl uint32_t ik_integer_to_uint32	(ikptr x);
ik_decl uint64_t ik_integer_to_uint64	(ikptr x);

ik_decl int	 ik_integer_to_int	(ikptr x);
ik_decl long	 ik_integer_to_long	(ikptr x);
ik_decl ik_llong ik_integer_to_llong	(ikptr x);
ik_decl ik_uint	 ik_integer_to_uint	(ikptr x);
ik_decl ik_ulong  ik_integer_to_ulong	(ikptr x);
ik_decl ik_ullong ik_integer_to_ullong	(ikptr x);

ik_decl off_t	ik_integer_to_off_t	(ikptr x);
ik_decl size_t	ik_integer_to_size_t	(ikptr x);
ik_decl ssize_t	ik_integer_to_ssize_t	(ikptr x);

/* inspection */
ik_decl ikptr	ikrt_positive_bn	(ikptr x);
ik_decl ikptr	ikrt_even_bn		(ikptr x);

/* arithmetics */
ik_decl ikptr	ikrt_fxfxplus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnplus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnplus		(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_fxfxminus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnminus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnfxminus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnminus		(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_bnnegate		(ikptr x, ikpcb* pcb);

ik_decl ikptr	ikrt_fxfxmult		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnmult		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnmult		(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_bnbncomp		(ikptr bn1, ikptr bn2);

ik_decl ikptr	ikrt_bnlognot		(ikptr x, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnlogand		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnlogand		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnlogor		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnlogor		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bignum_shift_right	(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fixnum_shift_left	(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bignum_shift_left	(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_bnbndivrem		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnfxdivrem		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnfx_modulo	(ikptr x, ikptr y /*, ikpcb* pcb */);
ik_decl ikptr	ikrt_bignum_length	(ikptr x);

ik_decl ikptr	ikrt_exact_fixnum_sqrt	(ikptr fx /*, ikpcb* pcb*/);
ik_decl ikptr	ikrt_exact_bignum_sqrt	(ikptr bn, ikpcb* pcb);

ik_decl ikptr	ikrt_bignum_to_bytevector (ikptr x, ikpcb* pcb);
ik_decl ikptr	ikrt_bignum_to_flonum	(ikptr bn, ikptr more_bits, ikptr fl);

ik_decl ikptr	ikrt_bignum_hash	(ikptr bn /*, ikpcb* pcb */);


/** --------------------------------------------------------------------
 ** Ratnum objects.
 ** ----------------------------------------------------------------- */

#define ratnum_tag		((ikptr) 0x27)
#define disp_ratnum_tag		0
#define disp_ratnum_num		(1 * wordsize)
#define disp_ratnum_den		(2 * wordsize)
#define disp_ratnum_unused	(3 * wordsize)
#define ratnum_size		(4 * wordsize)

#define off_ratnum_tag		-vector_tag
#define off_ratnum_num		(disp_ratnum_num    - vector_tag)
#define off_ratnum_den		(disp_ratnum_den    - vector_tag)
#define off_ratnum_unused	(disp_ratnum_unused - vector_tag)

#define IK_NUMERATOR(X)		IK_REF((X), off_ratnum_num)
#define IK_DENOMINATOR(X)	IK_REF((X), off_ratnum_den)

ik_decl int	ik_is_ratnum	(ikptr X);
ik_decl ikptr	ika_ratnum_alloc_no_init	(ikpcb * pcb);
ik_decl ikptr	ika_ratnum_alloc_and_init	(ikpcb * pcb);


/** --------------------------------------------------------------------
 ** Compnum objects.
 ** ----------------------------------------------------------------- */

#define compnum_tag		((ikptr) 0x37)
#define disp_compnum_tag	0
#define disp_compnum_real	(1 * wordsize)
#define disp_compnum_imag	(2 * wordsize)
#define disp_compnum_unused	(3 * wordsize)
#define compnum_size		(4 * wordsize)

#define off_compnum_tag		-vector_tag
#define off_compnum_real	(disp_compnum_real   - vector_tag)
#define off_compnum_imag	(disp_compnum_imag   - vector_tag)
#define off_compnum_unused	(disp_compnum_unused - vector_tag)

#define IK_COMPNUM_REAL(X)	IK_REF((X), off_compnum_real)
#define IK_COMPNUM_IMAG(X)	IK_REF((X), off_compnum_imag)

ik_decl int	ik_is_compnum	(ikptr X);
ik_decl ikptr	ika_compnum_alloc_no_init	(ikpcb * pcb);
ik_decl ikptr	ika_compnum_alloc_and_init	(ikpcb * pcb);


/** --------------------------------------------------------------------
 ** Flonum objects.
 ** ----------------------------------------------------------------- */

#define flonum_tag		((ikptr)0x17)
#define flonum_size		16 /* four 32-bit words, two 64-bit words */
#define disp_flonum_tag		0 /* not f(wordsize) */
#define disp_flonum_data	8 /* not f(wordsize) */
#define off_flonum_tag		(disp_flonum_tag  - vector_tag)
#define off_flonum_data		(disp_flonum_data - vector_tag)

#define IKU_DEFINE_AND_ALLOC_FLONUM(VARNAME)				\
  ikptr VARNAME = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;	\
  IK_REF(VARNAME, off_flonum_tag) = (ikptr)flonum_tag

#define IK_FLONUM_DATA(X)	(*((double*)(((long)(X))+off_flonum_data)))

ik_decl int   ik_is_flonum		(ikptr obj);
ik_decl ikptr iku_flonum_alloc		(ikpcb * pcb, double fl);
ik_decl ikptr ika_flonum_from_double	(ikpcb* pcb, double N);
ik_decl ikptr ikrt_flonum_hash		(ikptr x /*, ikpcb* pcb */);


/** --------------------------------------------------------------------
 ** Cflonum objects.
 ** ----------------------------------------------------------------- */

#define cflonum_tag		((ikptr) 0x47)
#define disp_cflonum_tag	0
#define disp_cflonum_real	(1 * wordsize)
#define disp_cflonum_imag	(2 * wordsize)
#define disp_cflonum_unused	(3 * wordsize)
#define cflonum_size		(4 * wordsize)
#define off_cflonum_tag		(disp_cflonum_tag  - vector_tag)
#define off_cflonum_real	(disp_cflonum_real - vector_tag)
#define off_cflonum_imag	(disp_cflonum_imag - vector_tag)

#define IKU_DEFINE_AND_ALLOC_CFLONUM(VARNAME)				\
  ikptr VARNAME = ik_unsafe_alloc(pcb, cflonum_size) | vector_tag;	\
  IK_REF(VARNAME, off_cflonum_tag) = (ikptr)cflonum_tag

ik_decl int   ik_is_cflonum	(ikptr X);
ik_decl ikptr iku_cflonum_alloc_and_init (ikpcb * pcb, double re, double im);

#define IK_CFLONUM_REAL(X)	IK_REF((X), off_cflonum_real)
#define IK_CFLONUM_IMAG(X)	IK_REF((X), off_cflonum_imag)
#define IK_CFLONUM_REAL_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_REAL(X))
#define IK_CFLONUM_IMAG_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_IMAG(X))


/** --------------------------------------------------------------------
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

#define pointer_tag		((ikptr) 0x107)
#define disp_pointer_tag	0
#define disp_pointer_data	(1 * wordsize)
#define pointer_size		(2 * wordsize)
#define off_pointer_tag		(disp_pointer_tag  - vector_tag)
#define off_pointer_data	(disp_pointer_data - vector_tag)

ik_decl ikptr ika_pointer_alloc	(ikpcb* pcb, ik_ulong memory);
ik_decl ikptr iku_pointer_alloc	(ikpcb* pcb, ik_ulong memory);
ik_decl ikptr ikrt_is_pointer	(ikptr X);
ik_decl int   ik_is_pointer	(ikptr X);

#define IK_POINTER_DATA(X)		IK_REF((X), off_pointer_data)
#define IK_POINTER_DATA_VOIDP(X)	((void *)   IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_CHARP(X)	((char *)   IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_UINT8P(X)	((uint8_t *)IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_LONG(X)		((long)	    IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_LLONG(X)	((ik_llong) IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_ULONG(X)	((ik_ulong) IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_ULLONG(X)	((ik_ullong)IK_REF((X), off_pointer_data))

#define IK_POINTER_SET_NULL(X)		(IK_REF((X), off_pointer_data) = 0)
#define IK_POINTER_IS_NULL(X)		(0 == IK_POINTER_DATA(X))


/** --------------------------------------------------------------------
 ** Vector objects.
 ** ----------------------------------------------------------------- */

#define vector_mask		7
#define vector_tag		5
#define disp_vector_length	0
#define disp_vector_data	wordsize
#define off_vector_length	(disp_vector_length - vector_tag)
#define off_vector_data		(disp_vector_data   - vector_tag)

ik_decl ikptr ika_vector_alloc_no_init	(ikpcb * pcb, long number_of_items);
ik_decl ikptr ika_vector_alloc_and_init	(ikpcb * pcb, long number_of_items);
ik_decl int   ik_is_vector		(ikptr s_vec);
ik_decl ikptr ikrt_vector_clean		(ikptr s_vec);
ik_decl ikptr ikrt_vector_copy		(ikptr s_dst, ikptr s_dst_start,
					 ikptr s_src, ikptr s_src_start,
					 ikptr s_count);

#define IK_VECTOR_LENGTH_FX(VEC)	IK_REF((VEC), off_vector_length)
#define IK_VECTOR_LENGTH(VEC)		IK_UNFIX(IK_VECTOR_LENGTH_FX(VEC))
#define IK_ITEM(VEC,IDX)		IK_REF((VEC), off_vector_data + (IDX) * wordsize)


/** --------------------------------------------------------------------
 ** Bytevector objects.
 ** ----------------------------------------------------------------- */

#define bytevector_mask		7
#define bytevector_tag		2
#define disp_bytevector_length	0
#define disp_bytevector_data	8 /* not f(wordsize) */
#define off_bytevector_length	(disp_bytevector_length - bytevector_tag)
#define off_bytevector_data	(disp_bytevector_data	- bytevector_tag)

#define IK_IS_BYTEVECTOR(X)	(bytevector_tag == (((long)(X)) & bytevector_mask))

ik_decl ikptr ika_bytevector_alloc		(ikpcb * pcb, long requested_number_of_bytes);
ik_decl ikptr ika_bytevector_from_cstring	(ikpcb * pcb, const char * cstr);
ik_decl ikptr ika_bytevector_from_cstring_len	(ikpcb * pcb, const char * cstr, size_t len);
ik_decl ikptr ika_bytevector_from_memory_block	(ikpcb * pcb, void * memory, size_t length);
ik_decl ikptr ikrt_bytevector_copy (ikptr s_dst, ikptr s_dst_start,
				    ikptr s_src, ikptr s_src_start,
				    ikptr s_count);

#define IK_BYTEVECTOR_LENGTH_FX(BV)	IK_REF((BV), off_bytevector_length)
#define IK_BYTEVECTOR_LENGTH(BV)	IK_UNFIX(IK_BYTEVECTOR_LENGTH_FX(BV))

#define IK_BYTEVECTOR_DATA(BV)		((long)((BV) + off_bytevector_data))
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
#define rtd_size		(6 * wordsize)

#define off_rtd_rtd		(disp_rtd_rtd	  - rtd_tag)
#define off_rtd_name		(disp_rtd_name	  - rtd_tag)
#define off_rtd_length		(disp_rtd_length  - rtd_tag)
#define off_rtd_fields		(disp_rtd_fields  - rtd_tag)
#define off_rtd_printer		(disp_rtd_printer - rtd_tag)
#define off_rtd_symbol		(disp_rtd_symbol  - rtd_tag)

ik_decl ikptr ika_struct_alloc_and_init	(ikpcb * pcb, ikptr rtd);
ik_decl ikptr ika_struct_alloc_no_init	(ikpcb * pcb, ikptr rtd);
ik_decl int   ik_is_struct	(ikptr R);

#define IK_FIELD(STRUCT,FIELD)	IK_REF((STRUCT), (off_record_data+(FIELD)*wordsize))


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
 ** Closure objects.
 ** ----------------------------------------------------------------- */

#define closure_tag		3
#define closure_mask		7
#define disp_closure_code	0
#define disp_closure_data	wordsize
#define off_closure_code	(disp_closure_code - closure_tag)
#define off_closure_data	(disp_closure_data - closure_tag)

#define IK_IS_CLOSURE(X)	((((long)(X)) & closure_mask) == closure_tag)


/** --------------------------------------------------------------------
 ** Continuation objects.
 ** ----------------------------------------------------------------- */

#define continuation_tag		((ikptr)0x1F)
#define disp_continuation_tag		0
#define disp_continuation_top		(1 * wordsize)
#define disp_continuation_size		(2 * wordsize)
#define disp_continuation_next		(3 * wordsize)
#define continuation_size		(4 * wordsize)

#define off_continuation_tag		(disp_continuation_tag	- vector_tag)
#define off_continuation_top		(disp_continuation_top	- vector_tag)
#define off_continuation_size		(disp_continuation_size - vector_tag)
#define off_continuation_next		(disp_continuation_next - vector_tag)

#define system_continuation_tag		((ikptr) 0x11F)
#define disp_system_continuation_tag	0
#define disp_system_continuation_top	(1 * wordsize)
#define disp_system_continuation_next	(2 * wordsize)
#define disp_system_continuation_unused (3 * wordsize)
#define system_continuation_size	(4 * wordsize)

#define off_system_continuation_tag	(disp_system_continuation_tag	 - vector_tag)
#define off_system_continuation_top	(disp_system_continuation_top	 - vector_tag)
#define off_system_continuation_next	(disp_system_continuation_next	 - vector_tag)
#define off_system_continuation_unused	(disp_system_continuation_unused - vector_tag)


/** --------------------------------------------------------------------
 ** Tcbucket objects.
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

ik_decl ikptr ikrt_general_copy (ikptr s_dst, ikptr s_dst_start,
				 ikptr s_src, ikptr s_src_start,
				 ikptr s_count);

ik_decl ikptr ik_enter_c_function (ikpcb* pcb);
ik_decl void  ik_leave_c_function (ikpcb* pcb, ikptr system_continuation);


/** --------------------------------------------------------------------
 ** Other objects stuff.
 ** ----------------------------------------------------------------- */

ikptr	ik_normalize_bignum	(long limbs, int sign, ikptr r);

#define max_digits_per_limb	((wordsize==4)?10:20)


/** --------------------------------------------------------------------
 ** Other prototypes and external definitions.
 ** ----------------------------------------------------------------- */

extern char **		environ;

#ifdef __CYGWIN__
void	win_munmap(char* addr, size_t size);
char*	win_mmap(size_t size);
#endif

int	ikarus_main (int argc, char** argv, char* boot_file);

ikptr	ik_errno_to_code (void);


/** --------------------------------------------------------------------
 ** Interface to "getaddrinfo()".
 ** ----------------------------------------------------------------- */

#if (!HAVE_GETADDRINFO)
#  include <sys/types.h>
#  include <netdb.h>

struct addrinfo {
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t ai_addrlen;
  struct sockaddr *ai_addr;
  struct addrinfo *ai_next;
};

extern int
getaddrinfo(const char *hostname, const char* servname,
  const struct addrinfo* hints, struct addrinfo** res);

extern void
freeaddrinfo(struct addrinfo *ai);


#ifndef EAI_SYSTEM
# define EAI_SYSTEM 11 /* same code as in glibc */
#endif

#endif /* if (!HAVE_GETADDRINFO) */


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* INTERNALS_H */

/* end of file */
