/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        This  file is  without  license notice  in  the original  Ikarus
        distribution for no reason I can know.

  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum

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
#include <dlfcn.h>
#include <gmp.h>

#ifndef RTLD_LOCAL
#  define RTLD_LOCAL    0 /* for cygwin, possibly incorrect */
#endif


/** --------------------------------------------------------------------
 ** Shared libraries interface.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_dlerror (ikpcb* pcb)
{
  char* str = dlerror();
  if (NULL == str)
    return false_object;
  else {
    int         len  = strlen(str);
    ikptr       bv   = ik_bytevector_alloc(pcb, len);
    void *      data = VICARE_BYTEVECTOR_DATA_VOIDP(bv);
    memcpy(data, str, len);
    return bv;
  }
}
ikptr
ikrt_dlopen (ikptr library_name_bv, ikptr load_lazy, ikptr load_global, ikpcb* pcb)
{
  int           flags;
  char *        name;
  void *        memory;
  flags  =
    ((load_lazy   == false_object) ? RTLD_NOW   : RTLD_LAZY) |
    ((load_global == false_object) ? RTLD_LOCAL : RTLD_GLOBAL);
  name   = (false_object == library_name_bv)? NULL : VICARE_BYTEVECTOR_DATA_CHARP(library_name_bv);
  memory = dlopen(name, flags);
  return (NULL == memory)? false_object : ikrt_pointer_alloc((long)memory, pcb);
}
ikptr
ikrt_dlclose (ikptr x /*, ikpcb* pcb*/)
{
  int   rv = dlclose(VICARE_POINTER_DATA_VOIDP(x));
  return (0 == rv) ? true_object : false_object;
}
ikptr
ikrt_dlsym (ikptr handle, ikptr sym, ikpcb* pcb)
{
  void *  memory = dlsym(VICARE_POINTER_DATA_VOIDP(handle), VICARE_BYTEVECTOR_DATA_CHARP(sym));
  return (NULL == memory)? false_object : ikrt_pointer_alloc((long)memory, pcb);
}


/** --------------------------------------------------------------------
 ** Basic pointer object operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_pointer_alloc (long memory, ikpcb * pcb)
{
  ikptr r = ik_safe_alloc(pcb, pointer_size);
  ref(r, 0) = pointer_tag;
  ref(r, wordsize) = (ikptr)memory;
  return r+vector_tag;
}
/* FIXME  STALE To be  removed at  the next  boot image  rotation (Marco
   Maggi; Tue Nov 22, 2011). */
ikptr
make_pointer (long memory, ikpcb * pcb)
{
  return ikrt_pointer_alloc(memory, pcb);
}
ikptr
ikrt_is_pointer (ikptr x)
{
  if ((tagof(x) == vector_tag) && (ref(x, -vector_tag) == pointer_tag)) {
    return true_object;
  } else {
    return false_object;
  }
}
/* FIXME  STALE To be  removed at  the next  boot image  rotation (Marco
   Maggi; Tue Nov 22, 2011). */
ikptr
ikrt_isapointer (ikptr x, ikpcb* pcb)
{
  return ikrt_is_pointer(x);
}


/** --------------------------------------------------------------------
 ** Other pointer operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_pointer_to_int(ikptr x, ikpcb* pcb) {
  long int p = (long int) ref(x, wordsize-vector_tag);
  ikptr pfx = fix(p);
  if (unfix(pfx) == p) {
    return pfx;
  } else {
    ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
    if (p > 0){
      ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikptr)p;
    } else {
      ref(bn, 0) =
        (ikptr)(bignum_tag |
              (1 << bignum_length_shift) |
              (1 << bignum_sign_shift));
      ref(bn, disp_bignum_data) = (ikptr)-p;
    }
    return bn+vector_tag;
  }
}

#define bnfst_negative(x)       (((unsigned long int)(x)) & bignum_sign_mask)

static long
integer_to_long(ikptr x)
{
  if (is_fixnum(x)) {
    return ((long)x) >> fx_shift;
  } else {
    if(bnfst_negative(ref(x, -vector_tag))){
      return -(long)ref(x, wordsize-vector_tag);
    } else {
      return (long)ref(x, wordsize-vector_tag);
    }
  }
}
ikptr
ikrt_fx_to_pointer(ikptr x, ikpcb* pcb)
{
  return ikrt_pointer_alloc(unfix(x), pcb);
}
ikptr
ikrt_bn_to_pointer (ikptr x, ikpcb* pcb)
{
  if(bnfst_negative(ref(x, -vector_tag))){
    return ikrt_pointer_alloc(-ref(x, wordsize-vector_tag), pcb);
  } else {
    return ikrt_pointer_alloc(ref(x, wordsize-vector_tag), pcb);
  }
}

#if 0
ikptr
ikrt_pointer_null(ikptr x /*, ikpcb* pcb*/)
{
  return ref(x, off_pointer_data) ? true_object : false_object;
}
#endif


/** --------------------------------------------------------------------
 ** C language level memory operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_malloc (ikptr len, ikpcb* pcb)
{
  void* p = malloc(unfix(len));
  if (p == NULL) {
    return false_object;
  } else {
    return ikrt_pointer_alloc((long int) p, pcb);
  }
}
ikptr
ikrt_free(ikptr x)
{
  free((void*) ref(x, off_pointer_data));
  return void_object;
}
ikptr
ikrt_memcpy_to_bv(ikptr dst, ikptr dst_off, ikptr src, ikptr count /*, ikpcb* pcb */)
{
  void *src_ptr, *dst_ptr;
  src_ptr = (void *)ref(src, off_pointer_data);
  dst_ptr = (void *)(dst + off_bytevector_data + unfix(dst_off));
  memcpy(dst_ptr, src_ptr, unfix(count));
  return void_object;
}
ikptr
ikrt_memcpy_from_bv (ikptr dst, ikptr src, ikptr src_off, ikptr count /*, ikpcb* pcb */)
{
  void *src_ptr, *dst_ptr;
  src_ptr = (void *)(src + off_bytevector_data + unfix(src_off));
  dst_ptr = (void *)ref(dst, off_pointer_data);
  memcpy(dst_ptr, src_ptr, unfix(count));
  return void_object;
}


/** --------------------------------------------------------------------
 ** Scheme objects to C numbers.
 ** ----------------------------------------------------------------- */

ikptr
s_to_number(signed long n, ikpcb* pcb)
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
sll_to_number(signed long long n, ikpcb* pcb)
{
  if (((signed long long)(signed long) n) == n) {
    return s_to_number(n, pcb);
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
u_to_number(unsigned long n, ikpcb* pcb)
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
ull_to_number(unsigned long long n, ikpcb* pcb)
{
  if (((unsigned long long)(unsigned long) n) == n) {
    return u_to_number(n, pcb);
  }
  ikptr bn = ik_safe_alloc(pcb, align(disp_bignum_data+sizeof(long long)));
  bcopy((char*)(&n), (char*)(bn+disp_bignum_data), sizeof(long long));
  return normalize_bignum(sizeof(long long)/sizeof(mp_limb_t), 0, bn);
}
ikptr
d_to_number(double n, ikpcb* pcb)
{
  ikptr x = ik_safe_alloc(pcb, flonum_size) + vector_tag;
  ref(x, -vector_tag) = flonum_tag;
  flonum_data(x) = n;
  return x;
}
static ikptr
double_to_flonum (double x, ikpcb* pcb)
{
  ikptr r = ik_safe_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = flonum_tag;
  flonum_data(r) = x;
  return r;
}
long
extract_num (ikptr x)
{
  if (is_fixnum(x))
    return unfix(x);
  else if (x == void_object)
    return 0;
  else {
    if (bnfst_negative(ref(x, -vector_tag)))
      return (long)(-ref(x, wordsize-vector_tag));
    else
      return (long)(ref(x, wordsize-vector_tag));
  }
}
unsigned long
extract_unum (ikptr x)
{
  if (is_fixnum(x))
    return unfix(x);
  else if (x == void_object)
    return 0;
  else {
    assert(! bnfst_negative(ref(x, -vector_tag)));
    return (unsigned long)(ref(x, wordsize-vector_tag));
  }
}

long long
extract_num_longlong(ikptr x) {
  if (is_fixnum(x)) {
    return unfix(x);
  } else if (x == void_object) {
    return 0;
  } else {
    ikptr fst = ref(x, -vector_tag);
    ikptr pos_one_limb_tag =
        (ikptr)(bignum_tag | (1 << bignum_length_shift));
    ikptr neg_one_limb_tag =
        (ikptr)(pos_one_limb_tag | (1 << bignum_sign_shift));
    if (fst == pos_one_limb_tag) {
      return (unsigned long)ref(x, wordsize-vector_tag);
    } else if (fst == neg_one_limb_tag) {
      return -(signed long)ref(x, wordsize-vector_tag);
    } else if (bnfst_negative(fst)) {
      return -(*((long long*)(x+wordsize-vector_tag)));
    } else {
      return *((long long*)(x+wordsize-vector_tag));
    }
  }
}


/** --------------------------------------------------------------------
 ** Raw memory getters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ref_char(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return fix(*((signed char*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}
ikptr
ikrt_ref_uchar(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return fix(*((unsigned char*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}
ikptr
ikrt_ref_short(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return fix(*((signed short*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}
ikptr
ikrt_ref_ushort(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return fix(*((unsigned short*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}
ikptr
ikrt_ref_pointer(ikptr p, ikptr off, ikpcb* pcb)
{
  long idx = integer_to_long(off);
  void* ptr = (void*)ref(p, off_pointer_data);
  return ikrt_pointer_alloc(ref(ptr, idx), pcb);
}
ikptr
ikrt_ref_float(ikptr p, ikptr off, ikpcb* pcb)
{
  long idx = integer_to_long(off);
  ikptr ptr = ref(p, off_pointer_data);
  double v = *((float*)(ptr+idx));
  return double_to_flonum(v, pcb);
}
ikptr
ikrt_ref_double(ikptr p, ikptr off, ikpcb* pcb)
{
  long idx = integer_to_long(off);
  ikptr ptr = ref(p, off_pointer_data);
  double v = *((double*)(ptr+idx));
  return double_to_flonum(v, pcb);
}
ikptr
ikrt_ref_int(ikptr p, ikptr off , ikpcb* pcb) {
  signed int r =
    *((signed int*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  if (wordsize == 8) {
    return fix(r);
  } else {
    return s_to_number(r, pcb);
  }
}
ikptr
ikrt_ref_uint(ikptr p, ikptr off , ikpcb* pcb)
{
  unsigned int r =
    *((unsigned int*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  if (wordsize == 8) {
    return fix(r);
  } else {
    return u_to_number(r, pcb);
  }
}
ikptr
ikrt_ref_long(ikptr p, ikptr off , ikpcb* pcb)
{
  signed long r = *((signed long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return s_to_number(r, pcb);
}
ikptr
ikrt_ref_ulong(ikptr p, ikptr off , ikpcb* pcb)
{
  unsigned long r = *((unsigned long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return u_to_number(r, pcb);
}
ikptr
ikrt_ref_longlong(ikptr p, ikptr off , ikpcb* pcb)
{
  signed long long r = *((signed long long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return sll_to_number(r, pcb);
}
ikptr
ikrt_ref_ulonglong(ikptr p, ikptr off , ikpcb* pcb)
{
  unsigned long long r = *((unsigned long long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return ull_to_number(r, pcb);
}


/** --------------------------------------------------------------------
 ** Raw memory setters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_set_char (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((char*)memory) = extract_num(value);
  return void_object;
}
ikptr
ikrt_set_short (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((short*)memory) = extract_num(value);
  return void_object;
}
ikptr
ikrt_set_int (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((int*)memory) = extract_num(value);
  return void_object;
}
ikptr
ikrt_set_long (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((long*)memory) = extract_num(value);
  return void_object;
}
ikptr
ikrt_set_longlong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((long long*)memory) = extract_num_longlong(value);
  return void_object;
}
ikptr
ikrt_set_double (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((double*)memory) = flonum_data(value);
  return void_object;
}
ikptr
ikrt_set_float (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((float*)memory) = flonum_data(value);
  return void_object;
}
ikptr
ikrt_set_pointer (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  long  memory = VICARE_POINTER_DATA_LONG(pointer) + unfix(byte_offset);
  *((long *)memory) = VICARE_POINTER_DATA_LONG(value);
  return void_object;
}

/* end of file */
