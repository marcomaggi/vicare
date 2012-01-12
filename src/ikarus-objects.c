/*
  Part of: Vicare
  Contents: utilities for built in object manipulation
  Date: Tue Nov  8, 2011

  Abstract



  Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"
#include <gmp.h>


/** --------------------------------------------------------------------
 ** Scheme pairs utilities.
 ** ----------------------------------------------------------------- */

long
ik_list_length (ikptr s_list)
/* Return the  length of the list  S_LIST.  Do *not*  check for circular
   lists. */
{
  long   length;
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
/* Given a  reference S_LIST  to a list  of bytevectors, fill  ARGV with
   pointers to the data areas, setting the last element of ARGV to NULL.
   The array referenced by ARGV must be wide enough to hold all the data
   from S_LIST plus the terminating NULL. */
{
  int    i;
  ikptr  bv;
  for (i=0; pair_tag == IK_TAGOF(s_list); s_list=IK_CDR(s_list), ++i) {
    bv      = IK_CAR(s_list);
    argv[i] = IK_BYTEVECTOR_DATA_CHARP(bv);
  }
  argv[i] = NULL;
}
void
ik_list_to_argv_and_argc (ikptr s_list, char **argv, long *argc)
/* Given a  reference S_LIST  to a list  of bytevectors: fill  ARGV with
   pointers to the data areas, setting the last element of ARGV to NULL;
   fill ARGC with the lengths  of the bytevectors.  The array referenced
   by ARGV must be wide enough to hold all the data from S_LIST plus the
   terminating NULL; the array referenced by ARGC must be wide enough to
   hold all the lengths. */
{
  int    i;
  ikptr  bv;
  for (i=0; pair_tag == IK_TAGOF(s_list); s_list=IK_CDR(s_list), ++i) {
    bv      = IK_CAR(s_list);
    argv[i] = IK_BYTEVECTOR_DATA_CHARP(bv);
    argc[i] = IK_BYTEVECTOR_LENGTH(bv);
  }
  argv[i] = NULL;
}
ikptr
ika_list_from_argv (ikpcb * pcb, char ** argv)
/* Given a  pointer ARGV  to a NULL-terminated  array of  ASCIIZ strings
   build and return  a list of bytevectors holding a  copy of the ASCIIZ
   strings.  Make use of PCB->ROOT1.  */
{
  ikptr         s_list, s_pair, s_new;
  int           i;
  s_list = s_pair = IKA_PAIR_ALLOC(pcb);
  pcb->root1 = &s_list;
  {
    for (i=0; argv[i];) {
      IK_CAR(s_pair) = ik_bytevector_from_cstring(pcb, argv[i]);
      if (argv[++i]) {
        s_new  = IK_CDR(s_pair) = IKA_PAIR_ALLOC(pcb);
        s_pair = s_new;
      } else
        IK_CDR(s_pair) = null_object;
    }
  }
  pcb->root1 = NULL;
  return s_list;
}
ikptr
ika_list_from_argv_and_argc (ikpcb * pcb, char ** argv, long argc)
/* Given  a pointer  ARGV to  an array  of ASCIIZ  strings  holding ARGC
   pointers: build  and return a list  of bytevectors holding  a copy of
   the ASCIIZ strings.  Make use of PCB->ROOT1.  */
{
  ikptr         s_list, s_pair, s_new;
  long          i;
  s_list = s_pair = IKA_PAIR_ALLOC(pcb);
  pcb->root1 = &s_list;
  {
    for (i=0; i<argc;) {
      IK_CAR(s_pair) = ik_bytevector_from_cstring(pcb, argv[i]);
      if (++i < argc) {
        s_new  = IK_CDR(s_pair) = IKA_PAIR_ALLOC(pcb);
        s_pair = s_new;
      } else
        IK_CDR(s_pair) = null_object;
    }
  }
  pcb->root1 = NULL;
  return s_list;
}


/** --------------------------------------------------------------------
 ** Scheme bytevector utilities.
 ** ----------------------------------------------------------------- */

ikptr
ika_bytevector_alloc (ikpcb * pcb, long int requested_number_of_bytes)
{
  long int  aligned_size;
  ikptr     bv;
  char *    data;
  aligned_size = IK_ALIGN(disp_bytevector_data
                       + requested_number_of_bytes
                       + 1);
  bv           = ik_safe_alloc(pcb, aligned_size)
                 | bytevector_tag;
  ref(bv, off_bytevector_length) = IK_FIX(requested_number_of_bytes);
  data = (char *)(long)(bv + off_bytevector_data);
  data[requested_number_of_bytes] = '\0';
  return bv;
}
ikptr
ik_bytevector_from_cstring (ikpcb * pcb, const char * cstr)
{
  size_t    len  = strlen(cstr);
  ikptr     bv   = ika_bytevector_alloc(pcb, len);
  char *    data = IK_BYTEVECTOR_DATA_CHARP(bv);
  memcpy(data, cstr, len);
  return bv;
}
ikptr
ik_bytevector_from_cstring_len (ikpcb * pcb, const char * cstr, size_t len)
{
  ikptr     bv   = ika_bytevector_alloc(pcb, len);
  char *    data = IK_BYTEVECTOR_DATA_CHARP(bv);
  memcpy(data, cstr, len);
  return bv;
}
ikptr
ik_bytevector_from_memory_block (ikpcb * pcb, void * memory, size_t length)
{
  ikptr     bv   = ika_bytevector_alloc(pcb, length);
  void *    data = IK_BYTEVECTOR_DATA_VOIDP(bv);
  memcpy(data, memory, length);
  return bv;
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
ik_vector_alloc (ikpcb * pcb, long number_of_items)
{
  long  align_size;
  ikptr vec;
  align_size = IK_ALIGN(disp_vector_data + number_of_items * wordsize);
  vec        = ik_safe_alloc(pcb, align_size) | vector_tag;
  ref(vec, off_vector_length) = IK_FIX(number_of_items);
  return vec;
}


/** --------------------------------------------------------------------
 ** Scheme struct utilities.
 ** ----------------------------------------------------------------- */

int
ik_is_struct (ikptr R)
{
  return ((record_tag == (record_mask & R)) &&
          (record_tag == (record_mask & ref(R, off_record_rtd))));
}
ikptr
ik_struct_alloc (ikpcb * pcb, ikptr s_rtd)
{
  long  num_of_fields = IK_UNFIX(ref(s_rtd, off_rtd_length));
  long  align_size    = IK_ALIGN(disp_record_data + num_of_fields * wordsize);
  ikptr s_stru        = ik_safe_alloc(pcb, align_size) | record_tag;
  ref(s_stru, off_record_rtd) = s_rtd;
  return s_stru;
}


/** --------------------------------------------------------------------
 ** Scheme string utilities.
 ** ----------------------------------------------------------------- */

ikptr
ika_string_alloc (ikpcb * pcb, long number_of_chars)
{
  long  align_size;
  ikptr s_str;
  align_size = IK_ALIGN(disp_string_data + number_of_chars * sizeof(ikchar));
  s_str        = ik_safe_alloc(pcb, align_size) | string_tag;
  ref(s_str, off_string_length) = IK_FIX(number_of_chars);
  return s_str;
}


/** --------------------------------------------------------------------
 ** Scheme objects from C numbers.
 ** ----------------------------------------------------------------- */

ikptr
ika_integer_from_int (ikpcb * pcb, signed int N)
{
  return ika_integer_from_long(pcb, (signed long)N);
}
ikptr
ika_integer_from_long (ikpcb * pcb, signed long n)
{
  ikptr fx = IK_FIX(n);
  if (IK_UNFIX(fx) == n) {
    return fx;
  }
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(wordsize+disp_bignum_data));
  if (n > 0){
    ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift));
    ref(bn, disp_bignum_data) = (ikptr)n;
  } else {
    ref(bn, 0) =
      (ikptr)(bignum_tag |
            (1 << bignum_length_shift) |
            (1 << bignum_sign_shift));
    ref(bn, disp_bignum_data) = (ikptr)-n;
  }
  return bn+vector_tag;
}
ikptr
ika_integer_from_long_long (ikpcb * pcb, ik_llong n)
{
  if (((ik_llong)(signed long) n) == n) {
    return ika_integer_from_long(pcb, n);
  }
  int len = sizeof(ik_llong) / sizeof(mp_limb_t);
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(sizeof(ik_llong)+disp_bignum_data));
  if (n > 0){
    ref(bn, 0) = (ikptr)(bignum_tag | (len << bignum_length_shift));
    *((ik_llong*)(bn+disp_bignum_data)) = n;
  } else {
    ref(bn, 0) =
      (ikptr)(bignum_tag |
            (len << bignum_length_shift) |
            (1 << bignum_sign_shift));
    *((ik_llong*)(bn+disp_bignum_data)) = -n;
  }
  return bn+vector_tag;
}
ikptr
ika_integer_from_unsigned_int (ikpcb * pcb, unsigned int N)
{
  return ika_integer_from_unsigned_long(pcb, (ik_ulong)N);
}
ikptr
ika_integer_from_unsigned_long (ikpcb * pcb, ik_ulong n)
{
  ik_ulong mxn = ((ik_ulong)-1)>>(fx_shift+1);
  if (n <= mxn) {
    return IK_FIX(n);
  }
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(wordsize+disp_bignum_data));
  ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift));
  ref(bn, disp_bignum_data) = (ikptr)n;
  return bn+vector_tag;
}
ikptr
ika_integer_from_unsigned_long_long (ikpcb * pcb, ik_ullong n)
{
  if (((ik_ullong)(ik_ulong) n) == n) {
    return ika_integer_from_unsigned_long(pcb, n);
  }
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+sizeof(ik_llong)));
  bcopy((char*)(&n), (char*)(bn+disp_bignum_data), sizeof(ik_llong));
  return normalize_bignum(sizeof(ik_llong)/sizeof(mp_limb_t), 0, bn);
}
ikptr
ik_flonum_from_double (ikpcb* pcb, double n)
{
  ikptr x = ik_safe_alloc(pcb, flonum_size) | vector_tag;
  ref(x, -vector_tag) = flonum_tag;
  IK_FLONUM_DATA(x) = n;
  return x;
}


/** --------------------------------------------------------------------
 ** Scheme objects to C numbers.
 ** ----------------------------------------------------------------- */

int
ik_integer_to_int (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == void_object)
    return 0;
  else {
    if (bnfst_negative(ref(x, -vector_tag)))
      return (int)(-ref(x, off_bignum_data));
    else
      return (int)(ref(x, off_bignum_data));
  }
}
long
ik_integer_to_long (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == void_object)
    return 0;
  else {
    if (bnfst_negative(ref(x, -vector_tag)))
      return (long)(-ref(x, off_bignum_data));
    else
      return (long)(ref(x, off_bignum_data));
  }
}
unsigned int
ik_integer_to_unsigned_int (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == void_object)
    return 0;
  else {
    assert(! bnfst_negative(ref(x, -vector_tag)));
    return (unsigned int)(ref(x, off_bignum_data));
  }
}
ik_ulong
ik_integer_to_unsigned_long (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == void_object)
    return 0;
  else {
    assert(! bnfst_negative(ref(x, -vector_tag)));
    return (ik_ulong)(ref(x, off_bignum_data));
  }
}
ik_llong
ik_integer_to_long_long (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return IK_UNFIX(x);
  else if (x == void_object)
    return 0;
  else {
    ikptr fst              = ref(x, -vector_tag);
    ikptr pos_one_limb_tag = (ikptr)(bignum_tag       | (1 << bignum_length_shift));
    ikptr neg_one_limb_tag = (ikptr)(pos_one_limb_tag | (1 << bignum_sign_shift));
    if (fst == pos_one_limb_tag)
      return (ik_ulong)ref(x, off_bignum_data);
    else if (fst == neg_one_limb_tag)
      return -(signed long)ref(x, off_bignum_data);
    else if (bnfst_negative(fst))
      return -(*((ik_llong*)(x+off_bignum_data)));
    else
      return *((ik_llong*)(x+off_bignum_data));

  }
}
ik_ullong
ik_integer_to_unsigned_long_long (ikptr x)
{
  if (IK_IS_FIXNUM(x))
    return (ik_ullong)IK_UNFIX(x);
  else if (x == void_object)
    return 0;
  else {
    ik_ullong *  memory = (ik_ullong *)(x + off_bignum_data);
    assert(! bnfst_negative(ref(x, -vector_tag)));
    return *memory;
  }
}

/* ------------------------------------------------------------------ */

uint32_t
ik_integer_to_uint32 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long        X = IK_UNFIX(x);
    return ((0 <= X) && (X <= UINT32_MAX))? ((uint32_t)X) : false_object;
  } else {
    uint32_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
int32_t
ik_integer_to_sint32 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long        X = IK_UNFIX(x);
    return ((INT32_MIN <= X) && (X <= INT32_MAX))? ((int32_t)X) : false_object;
  } else {
    int32_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
uint64_t
ik_integer_to_uint64 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long        X = IK_UNFIX(x);
    return ((0 <= X) && (X <= UINT64_MAX))? ((uint64_t)X) : false_object;
  } else {
    uint64_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
int64_t
ik_integer_to_sint64 (ikptr x)
{
  if (IK_IS_FIXNUM(x)) {
    long        X = IK_UNFIX(x);
    return ((INT64_MIN <= X) && (X <= INT64_MAX))? ((int64_t)X) : false_object;
  } else {
    int64_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}

/* end of file */
