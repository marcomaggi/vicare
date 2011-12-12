/*
 * Ikarus Scheme -- A compiler for R6RS Scheme.
 * Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
 * Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 *
 * This program is free software:  you can redistribute it and/or modify
 * it under  the terms of  the GNU General  Public License version  3 as
 * published by the Free Software Foundation.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
 * General Public License for more details.
 *
 * You should  have received  a copy of  the GNU General  Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef IKARUS_H
#  define IKARUS_H


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <netdb.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/resource.h>


/** --------------------------------------------------------------------
 ** Helper macros.
 ** ----------------------------------------------------------------- */

/* The macro  IK_UNUSED indicates that a function,  function argument or
   variable may potentially be unused.  Usage examples:

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


/** --------------------------------------------------------------------
 ** Global constants.
 ** ----------------------------------------------------------------- */

extern int total_allocated_pages;
extern int total_malloced;
extern int hash_table_count;

#define cardsize 512
#define cards_per_page 8

#define most_bytes_in_minor 0x10000000

#define old_gen_mask       0x00000007
#define new_gen_mask       0x00000008
#define gen_mask           0x0000000F
#define new_gen_tag        0x00000008
#define meta_dirty_mask    0x000000F0
#define type_mask          0x00000F00
#define scannable_mask     0x0000F000
#define dealloc_mask       0x000F0000
#define large_object_mask  0x00100000
#define meta_dirty_shift 4

#define hole_type       0x00000000
#define mainheap_type   0x00000100
#define mainstack_type  0x00000200
#define pointers_type   0x00000300
#define dat_type        0x00000400
#define code_type       0x00000500
#define weak_pairs_type 0x00000600
#define symbols_type    0x00000700

#define scannable_tag   0x00001000
#define unscannable_tag 0x00000000

#define dealloc_tag_un  0x00010000
#define dealloc_tag_at  0x00020000
#define retain_tag      0x00000000

#define large_object_tag   0x00100000

#define hole_mt         (hole_type       | unscannable_tag | retain_tag)
#define mainheap_mt     (mainheap_type   | unscannable_tag | retain_tag)
#define mainstack_mt    (mainstack_type  | unscannable_tag | retain_tag)
#define pointers_mt     (pointers_type   | scannable_tag   | dealloc_tag_un)
#define symbols_mt      (symbols_type    | scannable_tag   | dealloc_tag_un)
#define data_mt         (dat_type        | unscannable_tag | dealloc_tag_un)
#define code_mt         (code_type       | scannable_tag   | dealloc_tag_un)
#define weak_pairs_mt   (weak_pairs_type | scannable_tag   | dealloc_tag_un)


#define pagesize 4096
#define generation_count 5  /* generations 0 (nursery), 1, 2, 3, 4 */

#define IK_HEAP_EXT_SIZE  (32 * 4096)
#define IK_HEAPSIZE       (1024 * ((wordsize==4)?1:2) * 4096) /* 4/8 MB */

#define IK_FASL_HEADER \
  ((sizeof(ikptr) == 4) ? "#@IK01" : "#@IK02")
#define IK_FASL_HEADER_LEN (strlen(IK_FASL_HEADER))

#define pagesize   4096
#define pageshift    12

#define page_index(x)   \
  (((unsigned long)(x)) >> pageshift)

#define IK_ALIGN_TO_NEXT_PAGE(x) \
  (((pagesize - 1 + (unsigned long)(x)) >> pageshift) << pageshift)

#define IK_ALIGN_TO_PREV_PAGE(x) \
  ((((unsigned long)(x)) >> pageshift) << pageshift)

#define call_instruction_size \
  ((wordsize == 4) ? 5 : 10)
#define disp_frame_size   (- (call_instruction_size + 3 * wordsize))
#define disp_frame_offset (- (call_instruction_size + 2 * wordsize))
#define disp_multivale_rp (- (call_instruction_size + 1 * wordsize))


/** --------------------------------------------------------------------
 ** Global data types.
 ** ----------------------------------------------------------------- */

/* FIXME Should this be a "uintptr_t"? (Marco Maggi; Nov  6, 2011). */
typedef unsigned long       ikptr;

typedef struct ikpage{
  ikptr base;
  struct ikpage* next;
} ikpage;

typedef struct ikpages{
  ikptr base;
  int size;
  struct ikpages* next;
} ikpages;

typedef struct ikdl{ /* double-link */
  struct ikdl* prev;
  struct ikdl* next;
} ikdl;

#define ik_ptr_page_size \
  ((pagesize - sizeof(long) - sizeof(struct ik_ptr_page*))/sizeof(ikptr))

typedef struct ik_ptr_page{
  long count;
  struct ik_ptr_page* next;
  ikptr ptr[ik_ptr_page_size];
} ik_ptr_page;

typedef struct ik_callback_locative {
  void *                        callable_pointer;
  void *                        closure;
  ikptr                         data;
  struct ik_callback_locative * next;
} ik_callback_locative;

typedef struct ikpcb
{
  /* the first locations may be accessed by some     */
  /* compiled code to perform overflow/underflow ops */
  ikptr   allocation_pointer;           /* offset =  0 */
  ikptr   allocation_redline;           /* offset =  4 */
  ikptr   frame_pointer;                /* offset =  8 */
  ikptr   frame_base;                   /* offset = 12 */
  ikptr   frame_redline;                /* offset = 16 */
  ikptr   next_k;                       /* offset = 20 */
  ikptr   system_stack;                 /* offset = 24 */
  ikptr   dirty_vector;                 /* offset = 28 */
  ikptr   arg_list;                     /* offset = 32 */
  ikptr   engine_counter;               /* offset = 36 */
  ikptr   interrupted;                  /* offset = 40 */
  ikptr   base_rtd;                     /* offset = 44 */
  ikptr   collect_key;                  /* offset = 48 */

/* ------------------------------------------------------------------ */
  /* The  following fields are  not used  by any  scheme code  they only
     support the runtime system (gc, etc.) */

  /* Linked  list of  FFI callback  support data.   Used by  the garbage
     collector  not  to collect  data  still  needed  by some  callbacks
     registered in data structures handled by foreign libraries. */
  ik_callback_locative * callbacks;

  ikptr* root0;
  ikptr* root1;
  unsigned int* segment_vector;
  ikptr weak_pairs_ap;
  ikptr weak_pairs_ep;
  ikptr   heap_base;
  unsigned long   heap_size;
  ikpages* heap_pages;
  ikpage* cached_pages; /* pages cached so that we don't map/unmap */
  ikpage* uncached_pages; /* ikpages cached so that we don't malloc/free */
  ikptr cached_pages_base;
  int cached_pages_size;
  ikptr   stack_base;
  unsigned long   stack_size;
  ikptr   symbol_table;
  ikptr   gensym_table;
  ik_ptr_page* protected_list[generation_count];
  unsigned int* dirty_vector_base;
  unsigned int* segment_vector_base;
  ikptr memory_base;
  ikptr memory_end;
  int collection_id;
  int allocation_count_minor;
  int allocation_count_major;
  struct timeval collect_utime;
  struct timeval collect_stime;
  struct timeval collect_rtime;
  int last_errno;
} ikpcb;

typedef struct {
  ikptr tag;
  ikptr top;
  long size;
  ikptr next;
} cont;


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

void    ik_abort                (const char * error_message, ...);
void    ik_error                (ikptr args);

ikpcb * ik_collect              (unsigned long, ikpcb*);

void*   ik_malloc               (int);
void    ik_free                 (void*, int);

ikptr   ik_underflow_handler    (ikpcb*);
ikptr   ik_unsafe_alloc         (ikpcb* pcb, unsigned long size);
ikptr   ik_safe_alloc           (ikpcb* pcb, unsigned long size);

ikptr   ik_mmap                 (unsigned long);
ikptr   ik_mmap_typed           (unsigned long size, unsigned type, ikpcb*);
ikptr   ik_mmap_ptr             (unsigned long size, int gen, ikpcb*);
ikptr   ik_mmap_data            (unsigned long size, int gen, ikpcb*);
ikptr   ik_mmap_code            (unsigned long size, int gen, ikpcb*);
ikptr   ik_mmap_mixed           (unsigned long size, ikpcb*);
void    ik_munmap               (ikptr, unsigned long);
void    ik_munmap_from_segment  (ikptr, unsigned long, ikpcb*);
ikpcb * ik_make_pcb             (void);
void    ik_delete_pcb           (ikpcb*);
void    ik_free_symbol_table    (ikpcb* pcb);

void    ik_fasl_load            (ikpcb* pcb, char* filename);
void    ik_relocate_code        (ikptr);

ikptr   ik_exec_code            (ikpcb* pcb, ikptr code_ptr, ikptr argcount, ikptr cp);
void    ik_print                (ikptr x);
void    ik_fprint               (FILE*, ikptr x);

ikptr   ik_asm_enter            (ikpcb*, ikptr code_object, ikptr arg, ikptr cp);
ikptr   ik_asm_reenter          (ikpcb*, ikptr code_object, ikptr val);


/** --------------------------------------------------------------------
 ** Basic object related macros.
 ** ----------------------------------------------------------------- */

#define wordsize        ((int)(sizeof(ikptr)))
#define wordshift       ((4 == wordsize)? 2 : 3)
#define align_shift     (1 + wordshift)
#define align_size      (2 * wordsize)
#define immediate_tag   7

#define IK_TAGOF(X)        (((int)(X)) & 7)

#define ref(X,N) \
  (((ikptr*)(((long)(X)) + ((long)(N))))[0])

/* The least multiple of the wordsize which is greater than N. */
#define IK_ALIGN(N) \
  ((((N) + align_size - 1) >>  align_shift) << align_shift)

#define false_object            ((ikptr)0x2F)
#define true_object             ((ikptr)0x3F)
#define null_object             ((ikptr)0x4F)
#define eof_object              ((ikptr)0x5F)
#define void_object             ((ikptr)0x7F)

/* Special machine word value stored in locations that used to hold weak
   references to values which have been already garbage collected. */
#define bwp_object              ((ikptr)0x8F)

/* Special machine word value stored  in the "value" and "proc" field of
   Scheme symbol memory blocks to signal that these fields are unset. */
#define unbound_object          ((ikptr)0x6F)


/** --------------------------------------------------------------------
 ** Code objects.
 ** ----------------------------------------------------------------- */

/* This  is the  primary tag,  in the  machine word  referencing  a code
   object. */
#define code_pri_tag            vector_tag
/* This is the  secondary tag, in the first word  of the referenced heap
   vector. */
#define code_tag                ((ikptr)0x2F)
#define disp_code_code_size     (1 * wordsize)
#define disp_code_reloc_vector  (2 * wordsize)
#define disp_code_freevars      (3 * wordsize)
#define disp_code_annotation    (4 * wordsize)
#define disp_code_unused        (5 * wordsize)
#define disp_code_data          (6 * wordsize)
#define off_code_annotation     (disp_code_annotation   - code_pri_tag)
#define off_code_data           (disp_code_data         - code_pri_tag)
#define off_code_reloc_vector   (disp_code_reloc_vector - code_pri_tag)


/** --------------------------------------------------------------------
 ** Fixnum objects.
 ** ----------------------------------------------------------------- */

#define fx_tag          0
#define fx_shift        wordshift
#define fx_mask         (wordsize - 1)

#define most_positive_fixnum    (((unsigned long int)-1) >> (fx_shift+1))
#define most_negative_fixnum    (most_positive_fixnum+1)

#define IK_FIX(X)       ((ikptr)(((long)(X)) << fx_shift))
#define fix(X)          IK_FIX(X)

#define IK_UNFIX(X)     (((long)(X)) >> fx_shift)
#define unfix(X)        IK_UNFIX(X)

#define IK_IS_FIXNUM(X)    \
  ((((unsigned long)(X)) & fx_mask) == fx_tag)


/** --------------------------------------------------------------------
 ** Pair and list objects.
 ** ----------------------------------------------------------------- */

#define pair_size       (2 * wordsize)
#define pair_mask       7 /* #b111 */
#define pair_tag        1
#define disp_car        0
#define disp_cdr        wordsize
#define off_car         (disp_car - pair_tag)
#define off_cdr         (disp_cdr - pair_tag)

#define IK_IS_PAIR(X)   (pair_tag == (((long)(X)) & pair_mask))

#define IK_CAR(PAIR)                ref((PAIR), off_car)
#define IK_CDR(PAIR)                ref((PAIR), off_cdr)

#define IK_DECLARE_ALLOC_AND_CONS(PAIR,LIST,PCB)    \
  ikptr PAIR = IK_PAIR_ALLOC(PCB);                  \
  IK_CDR(PAIR) = LIST;                              \
  LIST=PAIR;

#define IK_PAIR_ALLOC(PCB)      (ik_safe_alloc((PCB), IK_ALIGN(pair_size)) + pair_tag)

long    ik_list_length                  (ikptr x);
void    ik_list_to_argv                 (ikptr x, char **argv);
void    ik_list_to_argv_and_argc        (ikptr x, char **argv, long *argc);

ikptr   ik_list_from_argv               (ikpcb * pcb, char ** argv);
ikptr   ik_list_from_argv_and_argc      (ikpcb * pcb, char ** argv, long argc);


/** --------------------------------------------------------------------
 ** Character objects.
 ** ----------------------------------------------------------------- */

typedef int ikchar;

#define char_tag        0x0F
#define char_mask       0xFF
#define char_shift      8

#define is_char(X)              \
  (char_tag == (char_mask & (int)(X)))

#define int_to_scheme_char(X)   \
  ((ikptr)((((unsigned long)(X)) << char_shift) | char_tag))

#define integer_to_char(X)      \
  ((ikchar)((((int)(X)) << char_shift) + char_tag))


/** --------------------------------------------------------------------
 ** String objects.
 ** ----------------------------------------------------------------- */

#define string_char_size        4
#define string_tag              6
#define disp_string_length      0
#define disp_string_data        wordsize
#define off_string_length       (disp_string_length - string_tag)
#define off_string_data         (disp_string_data - string_tag)

#define string_set(x,i,c) \
  (((ikchar*)(((long)(x)) + off_string_data))[i] = ((ikchar)(c)))

ikptr   ikrt_string_to_symbol   (ikptr, ikpcb *);
ikptr   ikrt_strings_to_gensym  (ikptr, ikptr,  ikpcb*);


/** --------------------------------------------------------------------
 ** Symbol objects.
 ** ----------------------------------------------------------------- */

#define symbol_tag                      ((ikptr) 0x5F)
#define disp_symbol_record_string       (1 * wordsize)
#define disp_symbol_record_ustring      (2 * wordsize)
#define disp_symbol_record_value        (3 * wordsize)
#define disp_symbol_record_proc         (4 * wordsize)
#define disp_symbol_record_plist        (5 * wordsize)
#define symbol_record_size              (6 * wordsize)
#define off_symbol_record_string        (disp_symbol_record_string  - record_tag)
#define off_symbol_record_ustring       (disp_symbol_record_ustring - record_tag)
#define off_symbol_record_value         (disp_symbol_record_value   - record_tag)
#define off_symbol_record_proc          (disp_symbol_record_proc    - record_tag)
#define off_symbol_record_plist         (disp_symbol_record_plist   - record_tag)

ikptr   ik_cstring_to_symbol    (char*, ikpcb*);


/** --------------------------------------------------------------------
 ** Number objects.
 ** ----------------------------------------------------------------- */

#define bignum_mask             0x7
#define bignum_tag              0x3
#define bignum_sign_mask        0x8
#define bignum_sign_shift       3
#define bignum_length_shift     4
#define disp_bignum_data        wordsize
#define off_bignum_data         (disp_bignum_data - vector_tag)

#define bnfst_limb_count(X)     (((unsigned long)(X)) >> bignum_length_shift)
#define bnfst_negative(X)       (((unsigned long)(X)) & bignum_sign_mask)

#define max_digits_per_limb ((wordsize==4)?10:20)

#define ratnum_tag              ((ikptr) 0x27)
#define disp_ratnum_num         (1 * wordsize)
#define disp_ratnum_den         (2 * wordsize)
#define disp_ratnum_unused      (3 * wordsize)
#define ratnum_size             (4 * wordsize)

#define flonum_tag              ((ikptr)0x17)
#define flonum_size             16
#define disp_flonum_data        8 /* not f(wordsize) */
#define off_flonum_data         (disp_flonum_data - vector_tag)

#define compnum_tag             ((ikptr) 0x37)
#define disp_compnum_real       (1 * wordsize)
#define disp_compnum_imag       (2 * wordsize)
#define disp_compnum_unused     (3 * wordsize)
#define compnum_size            (4 * wordsize)

#define cflonum_tag             ((ikptr) 0x47)
#define disp_cflonum_real       (1 * wordsize)
#define disp_cflonum_imag       (2 * wordsize)
#define disp_cflonum_unused     (3 * wordsize)
#define cflonum_size            (4 * wordsize)
#define off_cflonum_real        (disp_cflonum_real - vector_tag)
#define off_cflonum_imag        (disp_cflonum_imag - vector_tag)

ikptr   ik_flonum_alloc         (ikpcb * pcb, double fl);

#define FLONUM_DATA(X)  \
  (*((double*)(((char*)(long)(X)) + off_flonum_data)))

ikptr   ik_cflonum_alloc        (ikpcb * pcb, double re, double im);

#define CFLONUM_DATA_REAL(X)    \
  (*((double*)(((char*)(long)(X)) + off_cflonum_real)))

#define CFLONUM_DATA_IMAG(X)    \
  (*((double*)(((char*)(long)(X)) + off_cflonum_imag)))


ikptr   ik_integer_from_long               (signed long x, ikpcb* pcb);
ikptr   ik_integer_from_long_long          (signed long long n, ikpcb* pcb);
ikptr   ik_integer_from_unsigned_long      (unsigned long, ikpcb*);
ikptr   ik_integer_from_unsigned_long_long (unsigned long long, ikpcb*);
ikptr   ik_flonum_from_double              (double x, ikpcb* pcb);

uint32_t  ik_integer_to_uint32 (ikptr x);
int32_t   ik_integer_to_sint32 (ikptr x);
uint64_t  ik_integer_to_uint64 (ikptr x);
int64_t   ik_integer_to_sint64 (ikptr x);


long                ik_integer_to_long                  (ikptr x);
unsigned long       ik_integer_to_unsigned_long         (ikptr x);
long long           ik_integer_to_long_long             (ikptr x);
unsigned long long  ik_integer_to_unsigned_long_long    (ikptr x);

ikptr   normalize_bignum        (long limbs, int sign, ikptr r);


/** --------------------------------------------------------------------
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

#define pointer_tag           ((ikptr) 0x107)
#define disp_pointer_data     (1 * wordsize)
#define pointer_size          (2 * wordsize)
#define off_pointer_data      (disp_pointer_data - vector_tag)

ikptr   ik_pointer_alloc        (unsigned long memory, ikpcb* pcb);
ikptr   ikrt_is_pointer         (ikptr x);

#define IK_POINTER_DATA_VOIDP(X)  \
  ((void *)ref((X), off_pointer_data))

#define IK_POINTER_DATA_CHARP(X)  \
  ((char *)ref((X), off_pointer_data))

#define IK_POINTER_DATA_UINT8P(X)  \
  ((uint8_t *)ref((X), off_pointer_data))

#define IK_POINTER_DATA_ULONG(X)  \
  ((unsigned long)ref((X), off_pointer_data))

#define IK_POINTER_DATA_ULLONG(X) \
  ((unsigned long long)ref((X), off_pointer_data))

#define IK_POINTER_DATA_LLONG(X) \
  ((long long)ref((X), off_pointer_data))


/** --------------------------------------------------------------------
 ** Vector objects.
 ** ----------------------------------------------------------------- */

#define vector_mask             7
#define vector_tag              5
#define disp_vector_length      0
#define disp_vector_data        wordsize
#define off_vector_data         (disp_vector_data   - vector_tag)
#define off_vector_length       (disp_vector_length - vector_tag)

#define IS_VECTOR(X)    \
  ((((long)(X)) & vector_mask) == vector_tag)

ikptr   ik_vector_alloc         (ikpcb * pcb, long int requested_number_of_items);

#define IK_VECTOR_LENGTH_FX(vec)                    \
  ref((vec),off_vector_length)

#define IK_VECTOR_LENGTH(vec)                       \
  unfix(ref((vec),off_vector_length))

#define IK_VECTOR_REF(vec,idx)                      \
  ref((vec),off_vector_data+(idx)*wordsize)

#define IK_VECTOR_SET(vec,idx,value)                \
  ref((vec),off_vector_data+(idx)*wordsize) = (value)

#define IK_VECTOR_SET2(vec,idx,value1,value2)       \
  ref((vec),off_vector_data+(idx)*wordsize) = (value1) = (value2)


/** --------------------------------------------------------------------
 ** Bytevector objects.
 ** ----------------------------------------------------------------- */

#define bytevector_mask         7
#define bytevector_tag          2
#define disp_bytevector_length  0
#define disp_bytevector_data    8 /* not f(wordsize) */
#define off_bytevector_length   (disp_bytevector_length - bytevector_tag)
#define off_bytevector_data     (disp_bytevector_data   - bytevector_tag)

#define IS_BYTEVECTOR(X)    \
  ((((long)(X)) & bytevector_mask) == bytevector_tag)

extern ikptr   ik_bytevector_alloc (ikpcb * pcb, long int requested_number_of_bytes);
extern ikptr   ik_bytevector_from_cstring       (ikpcb * pcb, const char * cstr);
extern ikptr   ik_bytevector_from_cstring_len   (ikpcb * pcb, const char * cstr, size_t len);
extern ikptr   ik_bytevector_from_memory_block  (ikpcb * pcb, void * memory, size_t length);

#define IK_BYTEVECTOR_LENGTH(BV)                    \
  unfix(ref((BV), off_bytevector_length))

#define IK_BYTEVECTOR_LENGTH_FX(BV)                 \
  ref((BV), off_bytevector_length)

#define IK_BYTEVECTOR_DATA_CHARP(BV)                \
  ((char*)(long)((BV) + off_bytevector_data))

#define IK_BYTEVECTOR_DATA_UINT8P(BV)               \
  ((uint8_t*)(long)((BV) + off_bytevector_data))

#define IK_BYTEVECTOR_DATA_VOIDP(BV)                \
  ((void*)(long)((BV) + off_bytevector_data))


/** --------------------------------------------------------------------
 ** Struct objects.
 ** ----------------------------------------------------------------- */

#define record_tag              vector_tag
#define disp_record_rtd         0
#define disp_record_data        wordsize
#define off_record_rtd          (disp_record_rtd  - record_tag)
#define off_record_data         (disp_record_data - record_tag)

#define rtd_tag                 record_tag
#define disp_rtd_rtd            0
#define disp_rtd_name           (1 * wordsize)
#define disp_rtd_length         (2 * wordsize)
#define disp_rtd_fields         (3 * wordsize)
#define disp_rtd_printer        (4 * wordsize)
#define disp_rtd_symbol         (5 * wordsize)
#define rtd_size                (6 * wordsize)

#define off_rtd_rtd             (disp_rtd_rtd     - rtd_tag)
#define off_rtd_name            (disp_rtd_name    - rtd_tag)
#define off_rtd_length          (disp_rtd_length  - rtd_tag)
#define off_rtd_fields          (disp_rtd_fields  - rtd_tag)
#define off_rtd_printer         (disp_rtd_printer - rtd_tag)
#define off_rtd_symbol          (disp_rtd_symbol  - rtd_tag)

ikptr   ik_struct_alloc         (ikpcb * pcb, ikptr rtd, long number_of_fields);

#define IK_STRUCT_REF(STRUCT,FIELD)         \
  ref((STRUCT), (off_record_data+(FIELD)*wordsize))

#define IK_STRUCT_SET(STRUCT,FIELD,VALUE)   \
  ref((STRUCT), (off_record_data+(FIELD)*wordsize)) = (VALUE)

#define IK_STRUCT_SET2(STRUCT,FIELD,VALUE1,VALUE2)          \
  ref((STRUCT), (off_record_data+(FIELD)*wordsize)) = (VALUE1) = (VALUE2)


/** --------------------------------------------------------------------
 ** Port objects.
 ** ----------------------------------------------------------------- */

#define port_tag        0x3F
#define port_mask       0x3F
#define port_size       (14 * wordsize)


/** --------------------------------------------------------------------
 ** Closure objects.
 ** ----------------------------------------------------------------- */

#define closure_tag             3
#define closure_mask            7
#define disp_closure_code       0
#define disp_closure_data       wordsize
#define off_closure_code        (disp_closure_code - closure_tag)
#define off_closure_data        (disp_closure_data - closure_tag)

#define is_closure(X)   \
  ((((long)(X)) & closure_mask) == closure_tag)


/** --------------------------------------------------------------------
 ** Continuation objects.
 ** ----------------------------------------------------------------- */

#define continuation_tag        ((ikptr)0x1F)
#define disp_continuation_top   (1 * wordsize)
#define disp_continuation_size  (2 * wordsize)
#define disp_continuation_next  (3 * wordsize)
#define continuation_size       (4 * wordsize)

#define system_continuation_tag         ((ikptr) 0x11F)
#define disp_system_continuation_tag    0
#define disp_system_continuation_top    (1 * wordsize)
#define disp_system_continuation_next   (2 * wordsize)
#define disp_system_continuation_unused (3 * wordsize)
#define system_continuation_size        (4 * wordsize)

#define off_continuation_top   (disp_continuation_top  - vector_tag)
#define off_continuation_size  (disp_continuation_size - vector_tag)
#define off_continuation_next  (disp_continuation_next - vector_tag)


/** --------------------------------------------------------------------
 ** Tcbucket objects.
 ** ----------------------------------------------------------------- */

#define disp_tcbucket_tconc     (0 * wordsize)
#define disp_tcbucket_key       (1 * wordsize)
#define disp_tcbucket_val       (2 * wordsize)
#define disp_tcbucket_next      (3 * wordsize)
#define tcbucket_size           (4 * wordsize)

#define off_tcbucket_tconc      (disp_tcbucket_tconc - vector_tag)
#define off_tcbucket_key        (disp_tcbucket_key   - vector_tag)
#define off_tcbucket_val        (disp_tcbucket_val   - vector_tag)
#define off_tcbucket_next       (disp_tcbucket_next  - vector_tag)


/** --------------------------------------------------------------------
 ** Prototypes and external definitions.
 ** ----------------------------------------------------------------- */

extern char **environ;

#ifdef __CYGWIN__
void    win_munmap(char* addr, size_t size);
char*   win_mmap(size_t size);
#endif

int     ikarus_main (int argc, char** argv, char* boot_file);

ikptr   ik_errno_to_code (void);


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

#endif /* ifndef IKARUS_H */

/* end of file */
