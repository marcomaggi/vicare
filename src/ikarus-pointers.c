/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        This  file is  without  license notice  in  the original  Ikarus
        distribution  for no  reason I  can know  (Marco Maggi;  Nov 26,
        2011).

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
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_pointer_alloc (unsigned long memory, ikpcb * pcb)
{
  ikptr r = ik_safe_alloc(pcb, pointer_size);
  ref(r, 0) = pointer_tag;
  ref(r, wordsize) = (ikptr)memory;
  return r+vector_tag;
}
ikptr
ikrt_pointer_size (void)
{
  return fix(sizeof(void *));
}
ikptr
ikrt_is_pointer (ikptr x)
{
  return ((tagof(x) == vector_tag) && (ref(x, -vector_tag) == pointer_tag))? true_object : false_object;
}
ikptr
ikrt_pointer_is_null (ikptr x /*, ikpcb* pcb*/)
{
  return ref(x, off_pointer_data)? true_object : false_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_to_int (ikptr x, ikpcb* pcb)
{
  long      pointer;
  ikptr         pfx;
  pointer = (long) ref(x, off_bignum_data);
  pfx     = fix(pointer);
  if (unfix(pfx) == pointer) { /* if it fits in a fixnum ... */
    return pfx;
  } else {
    ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
    if (0 < pointer) {
      ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikptr)pointer;
    } else {
      ref(bn, 0) = (ikptr)(bignum_tag
                           | (1 << bignum_length_shift)
                           | (1 << bignum_sign_shift));
      ref(bn, disp_bignum_data) = (ikptr)(-pointer);
    }
    return bn+vector_tag;
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
    return ikrt_pointer_alloc(-ref(x, off_bignum_data), pcb);
  } else {
    return ikrt_pointer_alloc(+ref(x, off_bignum_data), pcb);
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_diff (ikptr ptr1, ikptr ptr2, ikpcb * pcb)
{
  unsigned long     memory1, memory2, diff;
  memory1 = VICARE_POINTER_DATA_ULONG(ptr1);
  memory2 = VICARE_POINTER_DATA_ULONG(ptr2);
  diff    = memory1 - memory2;
  return ik_integer_from_unsigned_long(diff, pcb);
}
ikptr
ikrt_pointer_add (ikptr ptr, ikptr delta, ikpcb * pcb)
{
  unsigned long long memory;
  long long          ptrdiff;
  memory  = VICARE_POINTER_DATA_ULLONG(ptr);
  ptrdiff = ik_integer_to_long_long(delta);
  if (0 <= ptrdiff) {
    if (ULONG_MAX - ptrdiff < memory) /* => ULONG_MAX < ptrdiff + memory */
      return false_object;
  } else {
    if (-ptrdiff > memory) /* => 0 > ptrdiff + memory */
      return false_object;
  }
  return ikrt_pointer_alloc ((unsigned long)(memory + ptrdiff), pcb);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_eq (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = VICARE_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = VICARE_POINTER_DATA_VOIDP(ptr2);
  return (memory1 == memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_neq (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = VICARE_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = VICARE_POINTER_DATA_VOIDP(ptr2);
  return (memory1 == memory2)? false_object : true_object;
}
ikptr
ikrt_pointer_lt (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = VICARE_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = VICARE_POINTER_DATA_VOIDP(ptr2);
  return (memory1 < memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_gt (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = VICARE_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = VICARE_POINTER_DATA_VOIDP(ptr2);
  return (memory1 > memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_le (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = VICARE_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = VICARE_POINTER_DATA_VOIDP(ptr2);
  return (memory1 <= memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_ge (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = VICARE_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = VICARE_POINTER_DATA_VOIDP(ptr2);
  return (memory1 >= memory2)? true_object : false_object;
}


/** --------------------------------------------------------------------
 ** C language level memory operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_malloc (ikptr len, ikpcb* pcb)
{
  void *        p = malloc(unfix(len));
  return (p)? ikrt_pointer_alloc((long) p, pcb) : false_object;
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
 ** Raw memory getters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ref_uint8 (ikptr pointer, ikptr offset)
{
  uint8_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return fix(*(memory+unfix(offset)));
}
ikptr
ikrt_ref_sint8 (ikptr pointer, ikptr offset)
{
  int8_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return fix(*(memory+unfix(offset)));
}
ikptr
ikrt_ref_uint16 (ikptr pointer, ikptr offset)
{
  uint16_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return fix(*(memory+unfix(offset)));
}
ikptr
ikrt_ref_sint16 (ikptr pointer, ikptr offset)
{
  int16_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return fix(*(memory+unfix(offset)));
}
ikptr
ikrt_ref_uint32 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint32_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return ik_integer_from_unsigned_long((unsigned long)*memory, pcb);
}
ikptr
ikrt_ref_sint32 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  int32_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return ik_integer_from_long((long)*memory, pcb);
}
ikptr
ikrt_ref_uint64 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint64_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return ik_integer_from_unsigned_long_long((unsigned long long)*memory, pcb);
}
ikptr
ikrt_ref_sint64 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  int64_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  return ik_integer_from_long_long((long long)*memory, pcb);
}

/* ------------------------------------------------------------------ */

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
  long idx = ik_integer_to_long(off);
  void* ptr = (void*)ref(p, off_pointer_data);
  return ikrt_pointer_alloc(ref(ptr, idx), pcb);
}
ikptr
ikrt_ref_float(ikptr p, ikptr off, ikpcb* pcb)
{
  long idx = ik_integer_to_long(off);
  ikptr ptr = ref(p, off_pointer_data);
  double v = *((float*)(ptr+idx));
  return ik_flonum_from_double(v, pcb);
}
ikptr
ikrt_ref_double(ikptr p, ikptr off, ikpcb* pcb)
{
  long idx = ik_integer_to_long(off);
  ikptr ptr = ref(p, off_pointer_data);
  double v = *((double*)(ptr+idx));
  return ik_flonum_from_double(v, pcb);
}
ikptr
ikrt_ref_int(ikptr p, ikptr off , ikpcb* pcb) {
  signed int r =
    *((signed int*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  if (wordsize == 8) {
    return fix(r);
  } else {
    return ik_integer_from_long(r, pcb);
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
    return ik_integer_from_unsigned_long(r, pcb);
  }
}
ikptr
ikrt_ref_long(ikptr p, ikptr off , ikpcb* pcb)
{
  signed long r = *((signed long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return ik_integer_from_long(r, pcb);
}
ikptr
ikrt_ref_ulong(ikptr p, ikptr off , ikpcb* pcb)
{
  unsigned long r = *((unsigned long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return ik_integer_from_unsigned_long(r, pcb);
}
ikptr
ikrt_ref_longlong(ikptr p, ikptr off , ikpcb* pcb)
{
  signed long long r = *((signed long long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return ik_integer_from_long_long(r, pcb);
}
ikptr
ikrt_ref_ulonglong(ikptr p, ikptr off , ikpcb* pcb)
{
  unsigned long long r = *((unsigned long long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return ik_integer_from_unsigned_long_long(r, pcb);
}


/** --------------------------------------------------------------------
 ** Raw memory setters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_set_uint8 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = unfix(value);
  return void_object;
}
ikptr
ikrt_set_sint8 (ikptr pointer, ikptr offset, ikptr value)
{
  int8_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = unfix(value);
  return void_object;
}
ikptr
ikrt_set_uint16 (ikptr pointer, ikptr offset, ikptr value)
{
  uint16_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = unfix(value);
  return void_object;
}
ikptr
ikrt_set_sint16 (ikptr pointer, ikptr offset, ikptr value)
{
  int16_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = unfix(value);
  return void_object;
}
ikptr
ikrt_set_uint32 (ikptr pointer, ikptr offset, ikptr value)
{
  uint32_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = ik_integer_to_uint32(value);
  return void_object;
}
ikptr
ikrt_set_sint32 (ikptr pointer, ikptr offset, ikptr value)
{
  int32_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = ik_integer_to_sint32(value);
  return void_object;
}
ikptr
ikrt_set_uint64 (ikptr pointer, ikptr offset, ikptr value)
{
  uint64_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = ik_integer_to_uint64(value);
  return void_object;
}
ikptr
ikrt_set_sint64 (ikptr pointer, ikptr offset, ikptr value)
{
  int64_t *     memory = VICARE_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = ik_integer_to_sint64(value);
  return void_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_set_char (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((char*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_short (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((short*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_int (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((int*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_long (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((long*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_longlong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((long long*)memory) = ik_integer_to_long_long(value);
  return void_object;
}
ikptr
ikrt_set_double (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((double*)memory) = flonum_data(value);
  return void_object;
}
ikptr
ikrt_set_float (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = VICARE_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((float*)memory) = flonum_data(value);
  return void_object;
}
ikptr
ikrt_set_pointer (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  void **  memory = VICARE_POINTER_DATA_VOIDP(pointer) + unfix(byte_offset);
  *memory = VICARE_POINTER_DATA_VOIDP(value);
  return void_object;
}

/* end of file */
