/*
  Part of: Vicare
  Contents: utilities for built in object manipulation
  Date: Tue Nov	 8, 2011

  Abstract



  Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"
#include <gmp.h>
#include <unistd.h> /* for off_t */


/** --------------------------------------------------------------------
 ** Scheme pairs utilities.
 ** ----------------------------------------------------------------- */

ikptr
ika_pair_alloc (ikpcb * pcb)
{
  ikptr	s_pair = IKA_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = IK_VOID_OBJECT;
  IK_CDR(s_pair) = IK_VOID_OBJECT;
  return s_pair;
}
ikptr
iku_pair_alloc (ikpcb * pcb)
{
  ikptr	s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = IK_VOID_OBJECT;
  IK_CDR(s_pair) = IK_VOID_OBJECT;
  return s_pair;
}
long
ik_list_length (ikptr s_list)
/* Return the  length of the list  S_LIST.  Do *not*  check for circular
   lists. */
{
  long	 length;
  for (length = 0; pair_tag == IK_TAGOF(s_list); ++length) {
    if (LONG_MAX != length)
      s_list = ref(s_list, off_cdr);
    else
      ik_abort("size of list exceeds LONG_MAX");
  }
  return length;
}

/* ------------------------------------------------------------------ */

void
ik_list_to_argv (ikptr s_list, char **argv)
/* Given a  reference S_LIST  to a list	 of bytevectors, fill  ARGV with
   pointers to the data areas, setting the last element of ARGV to NULL.
   The array referenced by ARGV must be wide enough to hold all the data
   from S_LIST plus the terminating NULL. */
{
  int	 i;
  ikptr	 bv;
  for (i=0; pair_tag == IK_TAGOF(s_list); s_list=IK_CDR(s_list), ++i) {
    bv	    = IK_CAR(s_list);
    argv[i] = IK_BYTEVECTOR_DATA_CHARP(bv);
  }
  argv[i] = NULL;
}
void
ik_list_to_argv_and_argc (ikptr s_list, char **argv, long *argc)
/* Given a  reference S_LIST  to a list	 of bytevectors: fill  ARGV with
   pointers to the data areas, setting the last element of ARGV to NULL;
   fill ARGC with the lengths  of the bytevectors.  The array referenced
   by ARGV must be wide enough to hold all the data from S_LIST plus the
   terminating NULL; the array referenced by ARGC must be wide enough to
   hold all the lengths. */
{
  int	 i;
  ikptr	 bv;
  for (i=0; pair_tag == IK_TAGOF(s_list); s_list=IK_CDR(s_list), ++i) {
    bv	    = IK_CAR(s_list);
    argv[i] = IK_BYTEVECTOR_DATA_CHARP(bv);
    argc[i] = IK_BYTEVECTOR_LENGTH(bv);
  }
  argv[i] = NULL;
}

/* ------------------------------------------------------------------ */

ikptr
ika_list_from_argv (ikpcb * pcb, char ** argv)
/* Given a  pointer ARGV  to a NULL-terminated	array of  ASCIIZ strings
   build and return  a list of bytevectors holding a  copy of the ASCIIZ
   strings.  Make use of "pcb->root8,9".  */
{
  if (! argv[0])
    return IK_NULL_OBJECT;
  else {
    ikptr	s_list, s_pair;
    s_list = s_pair = ika_pair_alloc(pcb);
    pcb->root9 = &s_list;
    pcb->root8 = &s_pair;
    {
      int		i;
      for (i=0; argv[i];) {
	IK_ASS(IK_CAR(s_pair), ika_bytevector_from_cstring(pcb, argv[i]));
	if (argv[++i]) {
	  IK_ASS(IK_CDR(s_pair), ika_pair_alloc(pcb));
	  s_pair = IK_CDR(s_pair);
	} else {
	  IK_CDR(s_pair) = IK_NULL_OBJECT;
	  break;
	}
      }
    }
    pcb->root8 = NULL;
    pcb->root9 = NULL;
    return s_list;
  }
}
ikptr
ika_list_from_argv_and_argc (ikpcb * pcb, char ** argv, long argc)
/* Given  a pointer  ARGV to  an array	of ASCIIZ  strings  holding ARGC
   pointers: build  and return a list  of bytevectors holding  a copy of
   the ASCIIZ strings.	Make use of "pcb->root8,9".  */
{
  if (! argc)
    return IK_NULL_OBJECT;
  else {
    ikptr	s_list, s_pair;
    s_list = s_pair = ika_pair_alloc(pcb);
    pcb->root9 = &s_list;
    pcb->root8 = &s_pair;
    {
      long	i;
      for (i=0; i<argc;) {
	IK_ASS(IK_CAR(s_pair), ika_bytevector_from_cstring(pcb, argv[i]));
	if (++i < argc) {
	  IK_ASS(IK_CDR(s_pair), ika_pair_alloc(pcb));
	  s_pair = IK_CDR(s_pair);
	} else {
	  IK_CDR(s_pair) = IK_NULL_OBJECT;
	  break;
	}
      }
    }
    pcb->root8 = NULL;
    pcb->root9 = NULL;
    return s_list;
  }
}


/** --------------------------------------------------------------------
 ** Scheme bytevector utilities.
 ** ----------------------------------------------------------------- */

ikptr
ika_bytevector_alloc (ikpcb * pcb, long int requested_number_of_bytes)
{
  long   aligned_size;
  ikptr	 s_bv;
  assert(requested_number_of_bytes <= most_positive_fixnum);
  aligned_size = IK_ALIGN(disp_bytevector_data + requested_number_of_bytes + 1);
  s_bv	       = ik_safe_alloc(pcb, aligned_size) | bytevector_tag;
  IK_REF(s_bv, off_bytevector_length) = IK_FIX(requested_number_of_bytes);
  IK_BYTEVECTOR_DATA_CHARP(s_bv)[requested_number_of_bytes] = '\0';
  return s_bv;
}
ikptr
ika_bytevector_from_cstring (ikpcb * pcb, const char * cstr)
{
  size_t    len	 = strlen(cstr);
  if (len > most_positive_fixnum)
    len = most_positive_fixnum;
  ikptr	    s_bv = ika_bytevector_alloc(pcb, len);
  char *    data = IK_BYTEVECTOR_DATA_CHARP(s_bv);
  memcpy(data, cstr, len);
  return s_bv;
}
ikptr
ika_bytevector_from_cstring_len (ikpcb * pcb, const char * cstr, size_t len)
{
  if (len > most_positive_fixnum)
    len = most_positive_fixnum;
  ikptr	    s_bv = ika_bytevector_alloc(pcb, len);
  char *    data = IK_BYTEVECTOR_DATA_CHARP(s_bv);
  memcpy(data, cstr, len);
  return s_bv;
}
ikptr
ika_bytevector_from_memory_block (ikpcb * pcb, const void * memory, size_t len)
{
  if (len > most_positive_fixnum)
    len = most_positive_fixnum;
  ikptr	    s_bv = ika_bytevector_alloc(pcb, len);
  void *    data = IK_BYTEVECTOR_DATA_VOIDP(s_bv);
  memcpy(data, memory, len);
  return s_bv;
}
ikptr
ikrt_bytevector_copy (ikptr s_dst, ikptr s_dst_start,
		      ikptr s_src, ikptr s_src_start,
		      ikptr s_count)
{
  long		src_start = IK_UNFIX(s_src_start);
  long		dst_start = IK_UNFIX(s_dst_start);
  size_t	count     = (size_t)IK_UNFIX(s_count);
  uint8_t *	dst = IK_BYTEVECTOR_DATA_UINT8P(s_dst) + dst_start;
  uint8_t *	src = IK_BYTEVECTOR_DATA_UINT8P(s_src) + src_start;
  memcpy(dst, src, count);
  return IK_VOID_OBJECT;
}
ikptr
ik_bytevector_from_utf16z (ikpcb * pcb, const void * _data)
/* Build and return  a new bytevector from a memory  block referencing a
   UTF-16 string terminated with two  consecutive zeros starting at even
   offset.  If the  the end of the  string is not found  before the byte
   index reaches the maximum fixnum: return the false object. */
{
  const uint8_t *	data = _data;
  int			i;
  /* Search the end of  the UTF-16 string: it is a sequence  of two 0 at
     even offset. */
  for (i=0; data[i] || data[1+i]; i+=2)
    if (most_positive_fixnum <= i)
      return IK_FALSE_OBJECT;
  return (most_positive_fixnum <= i)? IK_FALSE_OBJECT : \
    ika_bytevector_from_memory_block(pcb, data, i);
}


/** --------------------------------------------------------------------
 ** Scheme vector utilities.
 ** ----------------------------------------------------------------- */

int
ik_is_vector (ikptr s_vec)
{
  return (vector_tag == (s_vec & vector_mask)) && IK_IS_FIXNUM(ref(s_vec, -vector_tag));
}
ikptr
ika_vector_alloc_no_init (ikpcb * pcb, long number_of_items)
{
  ikptr s_len      = IK_FIX(number_of_items);
  /* Do not ask me why, but IK_ALIGN is needed here. */
  long	align_size = IK_ALIGN(disp_vector_data + s_len);
  ikptr	s_vec	   = ik_safe_alloc(pcb, align_size) | vector_tag;
  IK_REF(s_vec, off_vector_length) = s_len;
  return s_vec;
}
ikptr
ika_vector_alloc_and_init (ikpcb * pcb, long number_of_items)
{
  ikptr s_len      = IK_FIX(number_of_items);
  /* Do not ask me why, but IK_ALIGN is needed here. */
  long	align_size = IK_ALIGN(disp_vector_data + s_len);
  ikptr	s_vec	   = ik_safe_alloc(pcb, align_size) | vector_tag;
  IK_REF(s_vec, off_vector_length) = s_len;
  /* Set the data area to zero.  Remember that the machine word 0 is the
     fixnum zero. */
  memset((char*)(long)(s_vec + off_vector_data), 0, s_len);
  return s_vec;
}
ikptr
ikrt_vector_clean (ikptr s_vec)
{
  ikptr	s_len = IK_VECTOR_LENGTH_FX(s_vec);
  memset((char*)(long)(s_vec + off_vector_data), 0, s_len);
  return s_vec;
}
ikptr
ikrt_vector_copy (ikptr s_dst, ikptr s_dst_start,
		  ikptr s_src, ikptr s_src_start,
		  ikptr s_count)
{
  uint8_t *	dst = IK_BYTEVECTOR_DATA_UINT8P(s_dst) + (long)s_dst_start;
  uint8_t *	src = IK_BYTEVECTOR_DATA_UINT8P(s_src) + (long)s_src_start;
  memcpy(dst, src, (size_t)s_count);
  return IK_VOID_OBJECT;
}


/** --------------------------------------------------------------------
 ** Scheme struct utilities.
 ** ----------------------------------------------------------------- */

int
ik_is_struct (ikptr R)
{
  return ((record_tag == (record_mask & R)) &&
	  (record_tag == (record_mask & IK_REF(R, off_record_rtd))));
}
ikptr
ika_struct_alloc_no_init (ikpcb * pcb, ikptr s_rtd)
/* Allocate  and return  a new  structure instance  using S_RTD  as type
   descriptor.  All   the  fields  left  uninitialised.    Make  use  of
   "pcb->root9". */
{
  long	num_of_fields = IK_UNFIX(IK_REF(s_rtd, off_rtd_length));
  /* Do not ask me why, but IK_ALIGN is needed here. */
  long	align_size    = IK_ALIGN(disp_record_data + num_of_fields * wordsize);
  ikptr s_stru;
  pcb->root9 = &s_rtd;
  {
    s_stru = ik_safe_alloc(pcb, align_size) | record_tag;
    ref(s_stru, off_record_rtd) = s_rtd;
  }
  return s_stru;
}
ikptr
ika_struct_alloc_and_init (ikpcb * pcb, ikptr s_rtd)
/* Allocate  and return  a new  structure instance  using S_RTD  as type
   descriptor. All the fields are  initialised to the fixnum zero.  Make
   use of "pcb->root9". */
{
  ikptr	s_num_of_fields = IK_REF(s_rtd, off_rtd_length);
  /* Do not ask me why, but IK_ALIGN is needed here. */
  long	align_size      = IK_ALIGN(disp_record_data + s_num_of_fields);
  ikptr s_stru;
  pcb->root9 = &s_rtd;
  {
    s_stru = ik_safe_alloc(pcb, align_size) | record_tag;
    ref(s_stru, off_record_rtd) = s_rtd;
  }
  pcb->root9 = NULL;
  /* Set the data area to zero.  Remember that the machine word 0 is the
     fixnum zero. */
  memset((char*)(long)(s_stru + off_record_data), 0, s_num_of_fields);
  return s_stru;
}


/** --------------------------------------------------------------------
 ** Scheme string utilities.
 ** ----------------------------------------------------------------- */

ikptr
ika_string_alloc (ikpcb * pcb, long number_of_chars)
{
  long	align_size;
  ikptr s_str;
  /* Do not ask me why, but IK_ALIGN is needed here. */
  align_size = IK_ALIGN(disp_string_data + number_of_chars * sizeof(ikchar));
  s_str	     = ik_safe_alloc(pcb, align_size) | string_tag;
  ref(s_str, off_string_length) = IK_FIX(number_of_chars);
  return s_str;
}


/** --------------------------------------------------------------------
 ** Symbols.
 ** ----------------------------------------------------------------- */

int
ik_is_symbol (ikptr obj)
{
  return ((vector_tag == (vector_mask & obj)) &&
	  (symbol_tag == (symbol_mask & IK_REF(obj, off_symbol_record_tag))));
}


/** --------------------------------------------------------------------
 ** Scheme objects from C numbers.
 ** ----------------------------------------------------------------- */

ikptr
ika_integer_from_int (ikpcb * pcb, int N)
{
  return ika_integer_from_long(pcb, (long)N);
}
ikptr
ika_integer_from_long (ikpcb * pcb, long N)
{
  ikptr	s_fx = IK_FIX(N);
  if (IK_UNFIX(s_fx) == N)
    return s_fx;
  else {
#undef NUMBER_OF_WORDS
#define NUMBER_OF_WORDS		1
    /* wordsize == sizeof(long) */
    ikptr s_bn = ik_safe_alloc(pcb, IK_ALIGN(wordsize + disp_bignum_data)) | vector_tag;
    if (N > 0) { /* positive bignum */
      ref(s_bn, off_bignum_tag)	 =
	(ikptr)(bignum_tag | (NUMBER_OF_WORDS << bignum_nlimbs_shift));
      ref(s_bn, off_bignum_data) = (ikptr)+N;
    } else { /* zero or negative bignum */
      ref(s_bn, off_bignum_tag)	 =
	(ikptr)(bignum_tag | (NUMBER_OF_WORDS << bignum_nlimbs_shift) | (1 << bignum_sign_shift));
      ref(s_bn, off_bignum_data) = (ikptr)-N;
    }
    return s_bn;
  }
}
ikptr
ika_integer_from_llong (ikpcb * pcb, ik_llong N)
{
  /* If it  is in the range  of "long", use the	 appropriate function to
     allocate memory only for a "long" in the data area. */
  if (((ik_llong)(long) N) == N)
    return ika_integer_from_long(pcb, (long)N);
  else {
#undef NUMBER_OF_WORDS
#define NUMBER_OF_WORDS		sizeof(ik_llong) / sizeof(mp_limb_t)
    int	  align_size = IK_ALIGN(disp_bignum_data + sizeof(ik_llong));
    ikptr s_bn	     = ik_safe_alloc(pcb, align_size) | vector_tag;
    if (N > 0){
      ref(s_bn, off_bignum_tag) =
	(ikptr)(bignum_tag | (NUMBER_OF_WORDS << bignum_nlimbs_shift));
      *((ik_llong*)(s_bn + off_bignum_data)) = +N;
    } else {
      ref(s_bn, off_bignum_tag) =
	(ikptr)(bignum_tag | (NUMBER_OF_WORDS << bignum_nlimbs_shift) | (1 << bignum_sign_shift));
      *((ik_llong*)(s_bn + off_bignum_data)) = -N;
    }
    return s_bn;
  }
}
ikptr
ika_integer_from_uint (ikpcb * pcb, ik_uint N)
{
  return ika_integer_from_ulong(pcb, (ik_ulong)N);
}
ikptr
ika_integer_from_ulong (ikpcb * pcb, ik_ulong N)
{
  ik_ulong mxn = most_positive_fixnum;
  if (N <= mxn) {
    return IK_FIX(N);
  } else {
    /* wordsize == sizeof(unsigned long) */
    ikptr	s_bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + wordsize)) | vector_tag;
    ref(s_bn, off_bignum_tag)  = (ikptr)(bignum_tag | (1 << bignum_nlimbs_shift));
    ref(s_bn, off_bignum_data) = (ikptr)N;
    return s_bn;
  }
}
ikptr
ika_integer_from_ullong (ikpcb * pcb, ik_ullong N)
{
  /* If	 it is	in the	range of  "unsigned long",  use	 the appropriate
     function to allocate memory only for an "unsigned long" in the data
     area. */
  if (((ik_ullong)(ik_ulong) N) == N)
    return ika_integer_from_ulong(pcb, N);
  else {
#undef NUMBER_OF_WORDS
#define NUMBER_OF_WORDS		sizeof(ik_ullong) / sizeof(mp_limb_t)
    int	   align_size = IK_ALIGN(disp_bignum_data + sizeof(ik_ullong));
    ikptr  bn	      = ik_safe_alloc(pcb, align_size);
    bcopy((char*)(&N), (char*)(bn+disp_bignum_data), sizeof(ik_ullong));
    /* "ik_normalize_bignum()" wants an *untagged* pointer as argument. */
    return ik_normalize_bignum(NUMBER_OF_WORDS, 0, bn);
  }
}

/* ------------------------------------------------------------------ */

ikptr
ika_integer_from_sint32	(ikpcb* pcb, int32_t N)
{
  ikptr	s_fx = IK_FIX(N);
  if (IK_UNFIX(s_fx) == N)
    return s_fx;
  else {
    switch (sizeof(int32_t)) {
    case sizeof(int):
      return ika_integer_from_int   (pcb, (int)N);
#if 0 /* duplicate case value */
    case sizeof(long):
      return ika_integer_from_long  (pcb, (long)N);
#endif
    case sizeof(ik_llong):
      return ika_integer_from_llong (pcb, (ik_llong)N);
    default:
      ik_abort("unexpected integers size");
      return IK_VOID_OBJECT;
    }
  }
}
ikptr
ika_integer_from_sint64	(ikpcb* pcb, int64_t N)
{
  ikptr	s_fx = IK_FIX(N);
  if (IK_UNFIX(s_fx) == N)
    return s_fx;
  else {
    switch (sizeof(int64_t)) {
    case sizeof(int):
      return ika_integer_from_int   (pcb, (int)N);
#if 0 /* duplicate case value */
    case sizeof(long):
      return ika_integer_from_long  (pcb, (long)N);
#endif
    case sizeof(ik_llong):
      return ika_integer_from_llong (pcb, (ik_llong)N);
    default:
      ik_abort("unexpected integers size");
      return IK_VOID_OBJECT;
    }
  }
}
ikptr
ika_integer_from_uint32	(ikpcb* pcb, uint32_t N)
{
  uint32_t	mxn = most_positive_fixnum;
  if (N <= mxn) {
    return IK_FIX(N);
  } else {
    switch (sizeof(uint32_t)) {
    case sizeof(ik_uint):
      return ika_integer_from_uint   (pcb, (ik_uint)N);
#if 0 /* duplicate case value */
    case sizeof(ik_ulong):
      return ika_integer_from_ulong  (pcb, (ik_ulong)N);
#endif
    case sizeof(ik_ullong):
      return ika_integer_from_ullong (pcb, (ik_ullong)N);
    default:
      ik_abort("unexpected integers size");
      return IK_VOID_OBJECT;
    }
  }
}
ikptr
ika_integer_from_uint64	(ikpcb* pcb, uint64_t N)
{
  uint64_t	mxn = most_positive_fixnum;
  if (N <= mxn) {
    return IK_FIX(N);
  } else {
    switch (sizeof(uint64_t)) {
    case sizeof(ik_uint):
      return ika_integer_from_uint   (pcb, (ik_uint)N);
#if 0 /* duplicate case value */
    case sizeof(ik_ulong):
      return ika_integer_from_ulong  (pcb, (ik_ulong)N);
#endif
    case sizeof(ik_ullong):
      return ika_integer_from_ullong (pcb, (ik_ullong)N);
    default:
      ik_abort("unexpected integers size");
      return IK_VOID_OBJECT;
    }
  }
}

/* ------------------------------------------------------------------ */

ikptr
ika_integer_from_off_t (ikpcb * pcb, off_t N)
{
  switch (sizeof(off_t)) {
  case sizeof(int64_t):
    return ika_integer_from_sint64(pcb, (int64_t)N);
  case sizeof(int32_t):
    return ika_integer_from_sint32(pcb, (int32_t)N);
  default:
    ik_abort("unexpected off_t size %d", sizeof(off_t));
    return IK_VOID_OBJECT;
  }
}
ikptr
ika_integer_from_ssize_t (ikpcb * pcb, ssize_t N)
{
  switch (sizeof(ssize_t)) {
  case sizeof(int64_t):
    return ika_integer_from_sint64(pcb, (int64_t)N);
  case sizeof(int32_t):
    return ika_integer_from_sint32(pcb, (int32_t)N);
  default:
    ik_abort("unexpected ssize_t size %d", sizeof(ssize_t));
    return IK_VOID_OBJECT;
  }
}
ikptr
ika_integer_from_size_t (ikpcb * pcb, size_t N)
{
  switch (sizeof(size_t)) {
  case sizeof(uint64_t):
    return ika_integer_from_uint64(pcb, (uint64_t)N);
  case sizeof(int32_t):
    return ika_integer_from_uint32(pcb, (uint32_t)N);
  default:
    ik_abort("unexpected size_t size %d", sizeof(size_t));
    return IK_VOID_OBJECT;
  }
}
ikptr
ika_integer_from_ptrdiff_t (ikpcb * pcb, ptrdiff_t N)
{
  switch (sizeof(ptrdiff_t)) {
  case sizeof(uint64_t):
    return ika_integer_from_uint64(pcb, (uint64_t)N);
  case sizeof(int32_t):
    return ika_integer_from_uint32(pcb, (uint32_t)N);
  default:
    ik_abort("unexpected ptrdiff_t size %d", sizeof(ptrdiff_t));
    return IK_VOID_OBJECT;
  }
}

/* ------------------------------------------------------------------ */

ikptr
ika_flonum_from_double (ikpcb* pcb, double N)
{
  ikptr x = ik_safe_alloc(pcb, flonum_size) | vector_tag;
  ref(x, off_flonum_tag) = flonum_tag;
  IK_FLONUM_DATA(x) = N;
  return x;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_integer_from_machine_word (ikptr s_word, ikpcb * pcb)
{
  return ika_integer_from_ulong(pcb, (ik_ulong)s_word);
}


/** --------------------------------------------------------------------
 ** Scheme objects to C numbers.
 ** ----------------------------------------------------------------- */

int
ik_integer_to_int (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return (int)IK_UNFIX(x);
  else if (x == IK_VOID_OBJECT)
    return 0;
  else {
    if (IK_BNFST_NEGATIVE(ref(x, -vector_tag)))
      return (int)(-ref(x, off_bignum_data));
    else
      return (int)(+ref(x, off_bignum_data));
  }
}
long
ik_integer_to_long (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == IK_VOID_OBJECT)
    return 0;
  else {
    if (IK_BNFST_NEGATIVE(ref(x, -vector_tag)))
      return (long)(-ref(x, off_bignum_data));
    else
      return (long)(+ref(x, off_bignum_data));
  }
}
ik_uint
ik_integer_to_uint (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == IK_VOID_OBJECT)
    return 0;
  else {
    assert(! IK_BNFST_NEGATIVE(ref(x, -vector_tag)));
    return (ik_uint)(ref(x, off_bignum_data));
  }
}
ik_ulong
ik_integer_to_ulong (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == IK_VOID_OBJECT)
    return 0;
  else {
    assert(! IK_BNFST_NEGATIVE(ref(x, -vector_tag)));
    return (ik_ulong)(ref(x, off_bignum_data));
  }
}
ik_llong
ik_integer_to_llong (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == IK_VOID_OBJECT)
    return 0;
  else {
    ikptr fst		   = ref(x, -vector_tag);
    ikptr pos_one_limb_tag = (ikptr)(bignum_tag	      | (1 << bignum_nlimbs_shift));
    ikptr neg_one_limb_tag = (ikptr)(pos_one_limb_tag | (1 << bignum_sign_shift));
    if (fst == pos_one_limb_tag)
      return (ik_ulong)ref(x, off_bignum_data);
    else if (fst == neg_one_limb_tag)
      return -(signed long)ref(x, off_bignum_data);
    else if (IK_BNFST_NEGATIVE(fst))
      return -(*((ik_llong*)(x+off_bignum_data)));
    else
      return *((ik_llong*)(x+off_bignum_data));

  }
}
ik_ullong
ik_integer_to_ullong (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return (ik_ullong)IK_UNFIX(x);
  else if (x == IK_VOID_OBJECT)
    return 0;
  else {
    ik_ullong *	 memory = (ik_ullong *)(x + off_bignum_data);
    assert(! IK_BNFST_NEGATIVE(ref(x, -vector_tag)));
    return *memory;
  }
}

/* ------------------------------------------------------------------ */

uint32_t
ik_integer_to_uint32 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long	X = IK_UNFIX(x);
    return ((0 <= X) && (X <= UINT32_MAX))? ((uint32_t)X) : IK_FALSE_OBJECT;
  } else {
    uint32_t *	memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (IK_BNFST_NEGATIVE(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
int32_t
ik_integer_to_sint32 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long	X = IK_UNFIX(x);
    return ((INT32_MIN <= X) && (X <= INT32_MAX))? ((int32_t)X) : IK_FALSE_OBJECT;
  } else {
    int32_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (IK_BNFST_NEGATIVE(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
uint64_t
ik_integer_to_uint64 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long	X = IK_UNFIX(x);
    return ((0 <= X) && (X <= UINT64_MAX))? ((uint64_t)X) : IK_FALSE_OBJECT;
  } else {
    uint64_t *	memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (IK_BNFST_NEGATIVE(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
int64_t
ik_integer_to_sint64 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long	X = IK_UNFIX(x);
    return ((INT64_MIN <= X) && (X <= INT64_MAX))? ((int64_t)X) : IK_FALSE_OBJECT;
  } else {
    int64_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (IK_BNFST_NEGATIVE(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}

/* ------------------------------------------------------------------ */

off_t
ik_integer_to_off_t (ikptr x)
{
  if (sizeof(off_t) == sizeof(int64_t))
    return (off_t)ik_integer_to_sint64(x);
  else
    return (off_t)ik_integer_to_sint32(x);
}
size_t
ik_integer_to_size_t (ikptr x)
{
  if (sizeof(size_t) == sizeof(uint32_t))
    return (size_t)ik_integer_to_uint32(x);
  else
    return (size_t)ik_integer_to_uint64(x);
}
ssize_t
ik_integer_to_ssize_t (ikptr x)
{
  if (sizeof(ssize_t) == sizeof(int32_t))
    return (ssize_t)ik_integer_to_sint32(x);
  else
    return (ssize_t)ik_integer_to_sint64(x);
}
ptrdiff_t
ik_integer_to_ptrdiff_t (ikptr x)
{
  if (sizeof(ptrdiff_t) == sizeof(int32_t))
    return (ptrdiff_t)ik_integer_to_sint32(x);
  else
    return (ptrdiff_t)ik_integer_to_sint64(x);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_integer_to_machine_word (ikptr s_int, ikpcb * pcb)
{
  ik_ulong	word = ik_integer_to_ulong(s_int);
  return (ikptr)word;
}


/** --------------------------------------------------------------------
 ** Ratnum objects.
 ** ----------------------------------------------------------------- */

int
ik_is_ratnum (ikptr X)
{
  return ((vector_tag == IK_TAGOF(X)) &&
	  (ratnum_tag == IK_REF(X, -vector_tag)));
}
ikptr
ika_ratnum_alloc_no_init (ikpcb * pcb)
{
  ikptr	s_rn = ik_safe_alloc(pcb, ratnum_size) | vector_tag;
  IK_REF(s_rn, off_ratnum_tag) = ratnum_tag;
  return s_rn;
}
ikptr
ika_ratnum_alloc_and_init (ikpcb * pcb)
{
  ikptr	s_rn = ik_safe_alloc(pcb, ratnum_size) | vector_tag;
  IK_REF(s_rn, off_ratnum_tag) = ratnum_tag;
  memset((void *)(((long)s_rn) + off_ratnum_num), 0, 3 * wordsize);
  return s_rn;
}


/** --------------------------------------------------------------------
 ** Compnum objects.
 ** ----------------------------------------------------------------- */

int
ik_is_compnum (ikptr X)
{
  return ((vector_tag == IK_TAGOF(X)) &&
	  (compnum_tag == IK_REF(X, -vector_tag)));
}
ikptr
ika_compnum_alloc_no_init (ikpcb * pcb)
{
  ikptr	s_cn = ik_safe_alloc(pcb, compnum_size) | vector_tag;
  IK_REF(s_cn, off_compnum_tag) = compnum_tag;
  return s_cn;
}
ikptr
ika_compnum_alloc_and_init (ikpcb * pcb)
{
  ikptr	s_cn = ik_safe_alloc(pcb, compnum_size) | vector_tag;
  IK_REF(s_cn, off_compnum_tag) = compnum_tag;
  memset((void *)(((long)s_cn) + off_compnum_real), 0, 3 * wordsize);
  return s_cn;
}


/** --------------------------------------------------------------------
 ** Miscellanous functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_general_copy (ikptr s_dst, ikptr s_dst_start,
		   ikptr s_src, ikptr s_src_start,
		   ikptr s_count)
{
  long		src_start = IK_UNFIX(s_src_start);
  long		dst_start = IK_UNFIX(s_dst_start);
  size_t	count     = (size_t)IK_UNFIX(s_count);
  uint8_t *	dst = NULL;
  uint8_t *	src = NULL;

  if (IK_IS_BYTEVECTOR(s_src)) {
    src = IK_BYTEVECTOR_DATA_UINT8P(s_src) + src_start;
  } else if (ikrt_is_pointer(s_src)) {
    src = IK_POINTER_DATA_UINT8P(s_src) + src_start;
  } else if (IK_IS_STRING(s_src)) {
    src_start <<= 2; /* multiply by 4 */
    src = IK_STRING_DATA_VOIDP(s_src);
  } else
    ik_abort("%s: invalid src value, %lu", __func__, (ik_ulong)s_src);

  if (IK_IS_BYTEVECTOR(s_dst)) {
    dst = IK_BYTEVECTOR_DATA_UINT8P(s_dst) + dst_start;
  } else if (ikrt_is_pointer(s_dst)) {
    dst = IK_POINTER_DATA_UINT8P(s_dst) + dst_start;
  } else if (IK_IS_STRING(s_dst)) {
    dst_start <<= 2; /* multiply by 4 */
    dst = IK_STRING_DATA_VOIDP(s_dst);
  } else
    ik_abort("%s: invalid dst value, %lu", __func__, (ik_ulong)s_dst);

  memcpy(dst, src, count);
  return IK_VOID_OBJECT;
}

/* ------------------------------------------------------------------ */

ikptr
ik_register_to_avoid_collecting (ikptr s_obj, ikpcb * pcb)
{
  switch (s_obj) {
  case IK_FALSE:     /* Avoid registering constants. */
  case IK_TRUE:
  case IK_NULL:
  case IK_EOF:
  case IK_VOID:
  case IK_BWP:
  case IK_UNBOUND:
    return s_obj;
  default:
    if (IK_IS_FIXNUM(s_obj)) {    /* Avoid registering fixnums. */
      return s_obj;
    } else {
      pcb->root0 = &s_obj;
      {
	ikptr	s_pair = ika_pair_alloc(pcb);
	IK_CAR(s_pair) = s_obj;
	IK_CDR(s_pair) = pcb->not_to_be_collected;
	pcb->not_to_be_collected = s_pair;
      }
      pcb->root0 = NULL;
      return s_obj;
    }
  }
}
ikptr
ik_forget_to_avoid_collecting (ikptr s_obj, ikpcb * pcb)
{
  switch (s_obj) {
  case IK_FALSE:     /* Avoid searching for constants. */
  case IK_TRUE:
  case IK_NULL:
  case IK_EOF:
  case IK_VOID:
  case IK_BWP:
  case IK_UNBOUND:
    return IK_FALSE;
  default:
    if (IK_IS_FIXNUM(s_obj)) {    /* Avoid searching for fixnums. */
      return IK_FALSE;
    } else {
      ikptr s_pair = pcb->not_to_be_collected;
      /*
       *  |------------| pcb
       *       |
       *        ---> NULL = s_pair
       */
      if (IK_NULL == s_pair) {
	return IK_FALSE_OBJECT;
      } else
	/* Before:
	 *
	 *  |------------| pcb
	 *       |
	 *        --->|---|---| s_pair
	 *              |   |
	 *             OBJ   --->|---|---| CDR
	 *
	 * after:
	 *
	 *  |------------| pcb                |---|---| s_pair
	 *       |                              |
	 *        --->|---|---| CDR            OBJ
	 */
	if (IK_CAR(s_pair) == s_obj) {
	  pcb->not_to_be_collected = IK_CDR(s_pair);
	  return s_obj;
	} else {
	  ikptr	s_prev = s_pair;
	  while (IK_NULL != s_pair) {
	    /* Before:
	     *
	     *  |---|---| s_prev
	     *        |
	     *         --->|---|---| s_pair
	     *               |   |
	     *              OBJ   --->|---|---| CDR
	     *
	     * after:
	     *
	     *  |---|---| s_prev               |---|---| s_pair
	     *        |                          |
	     *         --->|---|---| CDR        OBJ
	     */
	    if (IK_CAR(s_pair) == s_obj) {
	      IK_CDR(s_prev) = IK_CDR(s_pair);
	      return s_obj;
	    } else {
	      s_prev = s_pair;
	      s_pair = IK_CDR(s_prev);
	    }
	  }
	  return IK_FALSE_OBJECT;
	}
    }
  }
}
ikptr
ik_collection_avoidance_list (ikpcb * pcb)
{
  return pcb->not_to_be_collected;
}
ikptr
ik_purge_collection_avoidance_list (ikpcb * pcb)
{
  pcb->not_to_be_collected = IK_NULL;
  return IK_VOID;
}

/* end of file */
