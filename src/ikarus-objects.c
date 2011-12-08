/*
  Part of: Vicare
  Contents: utilities for built in object manipulation
  Date: Tue Nov  8, 2011

  Abstract



  Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>

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

#include "ikarus.h"
#include <gmp.h>


/** --------------------------------------------------------------------
 ** Scheme pairs utilities.
 ** ----------------------------------------------------------------- */

int
ik_list_length (ikptr x)
{
  int   n;
  for (n = 0; pair_tag == tagof(x); ++n) {
    if (INT_MAX == n) {
      fprintf(stderr, "Vicare error: size of list exceeds INT_MAX");
      exit(EXIT_FAILURE);
    } else {
      x = ref(x, off_cdr);
    }
  }
  return n;
}
void
ik_list_to_argv (ikptr x, char **argv)
/* Given a reference to a list of bytevectors, fill "argv" with pointers
   to the data areas, setting the last element to NULL. */
{
  int    i;
  ikptr  bv;
  for (i=0; pair_tag == tagof(x); x=ref(x, off_cdr), ++i) {
    bv      = ref(x, off_car);
    argv[i] = (char*)(long)(bv + off_bytevector_data);
  }
  argv[i] = NULL;
}
void
ik_list_to_argv_and_argc (ikptr x, char **argv, long *argc)
/* Given a reference to a list of bytevectors: fill "argv" with pointers
   to the data areas, setting the last element to NULL; fill "argc" with
   the lengths of the bytevectors. */
{
  int    i;
  ikptr  bv;
  for (i=0; pair_tag == tagof(x); x=ref(x, off_cdr), ++i) {
    bv      = ref(x, off_car);
    argv[i] = VICARE_BYTEVECTOR_DATA_CHARP(bv);
    argc[i] = VICARE_BYTEVECTOR_LENGTH(bv);
  }
  argv[i] = NULL;
}
char**
ik_list_to_vec (ikptr x)
{
  int n = ik_list_length(x);
  char** vec = malloc((n+1) * sizeof(char*));
  if (vec == NULL)
    exit(EXIT_FAILURE);
  int i;
  for (i=0; i<n; i++) {
    vec[i] = (char*)(long)ref(x, off_car) + off_bytevector_data;
    x = ref(x, off_cdr);
  }
  vec[n] = 0;
  return vec;
}


/** --------------------------------------------------------------------
 ** Scheme bytevector utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_bytevector_alloc (ikpcb * pcb, long int requested_number_of_bytes)
{
  long int  aligned_size;
  ikptr     bv;
  char *    data;
  aligned_size = align(disp_bytevector_data
                       + requested_number_of_bytes
                       + 1);
  bv           = ik_safe_alloc(pcb, aligned_size)
                 + bytevector_tag;
  ref(bv, off_bytevector_length) = fix(requested_number_of_bytes);
  data = (char *)(long)(bv + off_bytevector_data);
  data[requested_number_of_bytes] = '\0';
  return bv;
}
ikptr
ik_bytevector_from_cstring (ikpcb * pcb, const char * cstr)
{
  size_t    len  = strlen(cstr);
  ikptr     bv   = ik_bytevector_alloc(pcb, len);
  char *    data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
  memcpy(data, cstr, len);
  return bv;
}
ikptr
ik_bytevector_from_cstring_len (ikpcb * pcb, const char * cstr, size_t len)
{
  ikptr     bv   = ik_bytevector_alloc(pcb, len);
  char *    data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
  memcpy(data, cstr, len);
  return bv;
}
ikptr
ik_bytevector_from_memory_block (ikpcb * pcb, void * memory, size_t length)
{
  ikptr     bv   = ik_bytevector_alloc(pcb, length);
  void *    data = VICARE_BYTEVECTOR_DATA_VOIDP(bv);
  memcpy(data, memory, length);
  return bv;
}


/** --------------------------------------------------------------------
 ** Scheme vector utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_vector_alloc (ikpcb * pcb, long int requested_number_of_items)
{
  long int  aligned_size;
  ikptr     vec;
  aligned_size = align(disp_vector_data + requested_number_of_items * wordsize);
  vec          = ik_safe_alloc(pcb, aligned_size) + vector_tag;
  ref(vec, off_vector_length) = fix(requested_number_of_items);
  return vec;
}


/** --------------------------------------------------------------------
 ** Scheme struct utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_struct_alloc (ikpcb * pcb, ikptr rtd, long int number_of_fields)
{
  long  aligned_size = align(disp_record_data + number_of_fields * wordsize);
  ikptr data         = ik_safe_alloc(pcb, aligned_size) + vector_tag;
  ref(data, off_record_rtd) = rtd;
  return data;
}


/** --------------------------------------------------------------------
 ** Scheme objects from C numbers.
 ** ----------------------------------------------------------------- */

ikptr
ik_integer_from_long(signed long n, ikpcb* pcb)
{
  ikptr fx = fix(n);
  if (unfix(fx) == n) {
    return fx;
  }
  ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
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
ik_integer_from_long_long(signed long long n, ikpcb* pcb)
{
  if (((signed long long)(signed long) n) == n) {
    return ik_integer_from_long(n, pcb);
  }
  int len = sizeof(long long) / sizeof(mp_limb_t);
  ikptr bn = ik_safe_alloc(pcb, align(sizeof(long long)+disp_bignum_data));
  if (n > 0){
    ref(bn, 0) = (ikptr)(bignum_tag | (len << bignum_length_shift));
    *((long long*)(bn+disp_bignum_data)) = n;
  } else {
    ref(bn, 0) =
      (ikptr)(bignum_tag |
            (len << bignum_length_shift) |
            (1 << bignum_sign_shift));
    *((long long*)(bn+disp_bignum_data)) = -n;
  }
  return bn+vector_tag;
}
ikptr
ik_integer_from_unsigned_long(unsigned long n, ikpcb* pcb)
{
  unsigned long mxn = ((unsigned long)-1)>>(fx_shift+1);
  if (n <= mxn) {
    return fix(n);
  }
  ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
  ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift));
  ref(bn, disp_bignum_data) = (ikptr)n;
  return bn+vector_tag;
}
ikptr
ik_integer_from_unsigned_long_long(unsigned long long n, ikpcb* pcb)
{
  if (((unsigned long long)(unsigned long) n) == n) {
    return ik_integer_from_unsigned_long(n, pcb);
  }
  ikptr bn = ik_safe_alloc(pcb, align(disp_bignum_data+sizeof(long long)));
  bcopy((char*)(&n), (char*)(bn+disp_bignum_data), sizeof(long long));
  return normalize_bignum(sizeof(long long)/sizeof(mp_limb_t), 0, bn);
}
ikptr
ik_flonum_from_double (double n, ikpcb* pcb)
{
  ikptr x = ik_safe_alloc(pcb, flonum_size) + vector_tag;
  ref(x, -vector_tag) = flonum_tag;
  FLONUM_DATA(x) = n;
  return x;
}


/** --------------------------------------------------------------------
 ** Scheme objects to C numbers.
 ** ----------------------------------------------------------------- */

long
ik_integer_to_long (ikptr x)
{
  if (is_fixnum(x))
    return unfix(x);
  else if (x == void_object)
    return 0;
  else {
    if (bnfst_negative(ref(x, -vector_tag)))
      return (long)(-ref(x, off_bignum_data));
    else
      return (long)(ref(x, off_bignum_data));
  }
}
unsigned long
ik_integer_to_unsigned_long (ikptr x)
{
  if (is_fixnum(x))
    return unfix(x);
  else if (x == void_object)
    return 0;
  else {
    assert(! bnfst_negative(ref(x, -vector_tag)));
    return (unsigned long)(ref(x, off_bignum_data));
  }
}
long long
ik_integer_to_long_long (ikptr x)
{
  if (is_fixnum(x))
    return unfix(x);
  else if (x == void_object)
    return 0;
  else {
    ikptr fst              = ref(x, -vector_tag);
    ikptr pos_one_limb_tag = (ikptr)(bignum_tag       | (1 << bignum_length_shift));
    ikptr neg_one_limb_tag = (ikptr)(pos_one_limb_tag | (1 << bignum_sign_shift));
    if (fst == pos_one_limb_tag)
      return (unsigned long)ref(x, off_bignum_data);
    else if (fst == neg_one_limb_tag)
      return -(signed long)ref(x, off_bignum_data);
    else if (bnfst_negative(fst))
      return -(*((long long*)(x+off_bignum_data)));
    else
      return *((long long*)(x+off_bignum_data));

  }
}
unsigned long long
ik_integer_to_unsigned_long_long (ikptr x)
{
  if (is_fixnum(x))
    return (unsigned long long)unfix(x);
  else if (x == void_object)
    return 0;
  else {
    unsigned long long *  memory = (unsigned long long *)(x + off_bignum_data);
    assert(! bnfst_negative(ref(x, -vector_tag)));
    return *memory;
  }
}

/* ------------------------------------------------------------------ */

uint32_t
ik_integer_to_uint32 (ikptr x)
{
  if (is_fixnum(x)) {
    long        X = unfix(x);
    return ((0 <= X) && (X <= UINT32_MAX))? ((uint32_t)X) : false_object;
  } else {
    uint32_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
int32_t
ik_integer_to_sint32 (ikptr x)
{
  if (is_fixnum(x)) {
    long        X = unfix(x);
    return ((INT32_MIN <= X) && (X <= INT32_MAX))? ((int32_t)X) : false_object;
  } else {
    int32_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
uint64_t
ik_integer_to_uint64 (ikptr x)
{
  if (is_fixnum(x)) {
    long        X = unfix(x);
    return ((0 <= X) && (X <= UINT64_MAX))? ((uint64_t)X) : false_object;
  } else {
    uint64_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}
int64_t
ik_integer_to_sint64 (ikptr x)
{
  if (is_fixnum(x)) {
    long        X = unfix(x);
    return ((INT64_MIN <= X) && (X <= INT64_MAX))? ((int64_t)X) : false_object;
  } else {
    int64_t *  memory = (void *)(((uint8_t *)x) + off_bignum_data);
    return (bnfst_negative(ref(x, -vector_tag)))? -(*memory) : (*memory);
  }
}

/* end of file */
