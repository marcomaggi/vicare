/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        This  file is  without  license notice  in  the original  Ikarus
        distribution  for no  reason I  can know  (Marco Maggi;  Nov 26,
        2011).

  Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
 ** Interface to "errno".
 ** ----------------------------------------------------------------- */

ikptr
ikrt_set_errno (ikptr code)
{
  if (false_object == code)
    errno = 0;
  else if (true_object == code)
    errno = EFAULT;
  else
    errno = -(fix(code));
  return void_object;
}
ikptr
ikrt_last_errno(ikpcb* pcb)
{
  int   negated_errno_code = - pcb->last_errno;
  return fix(negated_errno_code);
}


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
    void *      data = IK_BYTEVECTOR_DATA_VOIDP(bv);
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
  name   = (false_object == library_name_bv)? NULL : IK_BYTEVECTOR_DATA_CHARP(library_name_bv);
  memory = dlopen(name, flags);
  return (NULL == memory)? false_object : ik_pointer_alloc((unsigned long)memory, pcb);
}
ikptr
ikrt_dlclose (ikptr x /*, ikpcb* pcb*/)
{
  int   rv = dlclose(IK_POINTER_DATA_VOIDP(x));
  return (0 == rv) ? true_object : false_object;
}
ikptr
ikrt_dlsym (ikptr handle, ikptr sym, ikpcb* pcb)
{
  void *  memory = dlsym(IK_POINTER_DATA_VOIDP(handle), IK_BYTEVECTOR_DATA_CHARP(sym));
  return (NULL == memory)? false_object : ik_pointer_alloc((unsigned long)memory, pcb);
}


/** --------------------------------------------------------------------
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

ikptr
ik_pointer_alloc (unsigned long memory, ikpcb * pcb)
{
  ikptr r = ik_safe_alloc(pcb, pointer_size);
  ref(r, 0)        = pointer_tag;
  ref(r, wordsize) = (ikptr)memory; /* we have not yet added the tag! */
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
  return ((IK_TAGOF(x) == vector_tag) &&
          (ref(x, -vector_tag) == pointer_tag))? true_object : false_object;
}
ikptr
ikrt_pointer_is_null (ikptr x /*, ikpcb* pcb*/)
{
  return ref(x, off_pointer_data)? false_object : true_object;
}
ikptr
ikrt_pointer_set_null (ikptr pointer)
{
  ref(pointer, off_pointer_data) = (ikptr)NULL;
  return void_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_to_int (ikptr pointer, ikpcb* pcb)
{
  void *        memory;
  memory = IK_POINTER_DATA_VOIDP(pointer);
  return ik_integer_from_unsigned_long((unsigned long)memory, pcb);
}
ikptr
ikrt_fx_to_pointer(ikptr x, ikpcb* pcb)
{
  return ik_pointer_alloc(unfix(x), pcb);
}
ikptr
ikrt_bn_to_pointer (ikptr x, ikpcb* pcb)
{
  if(bnfst_negative(ref(x, -vector_tag))){
    return ik_pointer_alloc(-ref(x, off_bignum_data), pcb);
  } else {
    return ik_pointer_alloc(+ref(x, off_bignum_data), pcb);
  }
}

/* ------------------------------------------------------------------ */

/* NOTE The  Scheme function POINTER-DIFF  is implemented at  the Scheme
   level because converting pointers  to Scheme exact integer objects is
   the simplest  and safest  way to correctly  handle the full  range of
   possible pointer values. */

ikptr
ikrt_pointer_add (ikptr ptr, ikptr delta, ikpcb * pcb)
{
  unsigned long long memory;
  long long          ptrdiff;
  memory  = IK_POINTER_DATA_ULLONG(ptr);
  ptrdiff = ik_integer_to_long_long(delta);
  if (0 <= ptrdiff) {
    if (ULONG_MAX - ptrdiff < memory) /* => ULONG_MAX < ptrdiff + memory */
      return false_object;
  } else {
    if (-ptrdiff > memory) /* => 0 > ptrdiff + memory */
      return false_object;
  }
  return ik_pointer_alloc ((unsigned long)(memory + ptrdiff), pcb);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_eq (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 == memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_neq (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 == memory2)? false_object : true_object;
}
ikptr
ikrt_pointer_lt (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 < memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_gt (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 > memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_le (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 <= memory2)? true_object : false_object;
}
ikptr
ikrt_pointer_ge (ikptr ptr1, ikptr ptr2)
{
  void *        memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *        memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 >= memory2)? true_object : false_object;
}


/** --------------------------------------------------------------------
 ** C language level memory allocation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_malloc (ikptr number_of_bytes, ikpcb* pcb)
{
  void *        p = malloc(unfix(number_of_bytes));
  return (p)? ik_pointer_alloc((unsigned long) p, pcb) : false_object;
}
ikptr
ikrt_realloc (ikptr pointer, ikptr number_of_bytes, ikpcb* pcb)
{
  void *        memory = IK_POINTER_DATA_VOIDP(pointer);
  void *        new_memory;
  if (memory) {
    new_memory = realloc(memory, unfix(number_of_bytes));
    if (new_memory) {
      ref(pointer, off_pointer_data) = (ikptr)NULL;
      return ik_pointer_alloc((unsigned long)new_memory, pcb);
    } else
      return false_object;
  } else
    return false_object;
}
ikptr
ikrt_calloc (ikptr number_of_elements, ikptr element_size, ikpcb* pcb)
{
  void *        p = calloc(unfix(number_of_elements), unfix(element_size));
  return (p)? ik_pointer_alloc((unsigned long) p, pcb) : false_object;
}
ikptr
ikrt_free (ikptr pointer)
{
  void *        memory = (void*)ref(pointer, off_pointer_data);
  if (memory) {
    free(memory);
    ref(pointer, off_pointer_data) = (ikptr)NULL;
  }
  return void_object;
}


/** --------------------------------------------------------------------
 ** C language level memory operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_memcpy (ikptr dst, ikptr src, ikptr size)
{
  memcpy(IK_POINTER_DATA_VOIDP(dst),
         IK_POINTER_DATA_VOIDP(src),
         unfix(size));
  return void_object;
}
ikptr
ikrt_memmove (ikptr dst, ikptr src, ikptr size)
{
  memmove(IK_POINTER_DATA_VOIDP(dst),
          IK_POINTER_DATA_VOIDP(src),
          unfix(size));
  return void_object;
}
ikptr
ikrt_memset (ikptr ptr, ikptr byte, ikptr size)
{
  memset(IK_POINTER_DATA_VOIDP(ptr), unfix(byte), unfix(size));
  return void_object;
}
ikptr
ikrt_memcmp (ikptr pointer1, ikptr pointer2, ikptr count)
{
  int   rv;
  rv = memcmp(IK_POINTER_DATA_VOIDP(pointer1),
              IK_POINTER_DATA_VOIDP(pointer2),
              unfix(count));
  return fix(rv);
}


/** --------------------------------------------------------------------
 ** Raw memory and Scheme bytevector operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_memcpy_to_bv(ikptr dst, ikptr dst_off, ikptr src, ikptr count /*, ikpcb* pcb */)
{
  void *        src_ptr;
  void *        dst_ptr;
  src_ptr = (void *)ref(src, off_pointer_data);
  dst_ptr = (void *)(dst + off_bytevector_data + unfix(dst_off));
  memcpy(dst_ptr, src_ptr, unfix(count));
  return void_object;
}
ikptr
ikrt_memcpy_from_bv (ikptr dst, ikptr src, ikptr src_off, ikptr count /*, ikpcb* pcb */)
{
  void *        src_ptr;
  void *        dst_ptr;
  src_ptr = (void *)(src + off_bytevector_data + unfix(src_off));
  dst_ptr = (void *)ref(dst, off_pointer_data);
  memcpy(dst_ptr, src_ptr, unfix(count));
  return void_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_bytevector_from_memory (ikptr pointer, ikptr length, ikpcb * pcb)
{
  void *        memory = IK_POINTER_DATA_VOIDP(pointer);
  size_t        size   = (size_t)unfix(length);
  return ik_bytevector_from_memory_block(pcb, memory, size);
}
ikptr
ikrt_bytevector_to_memory (ikptr bv, ikpcb * pcb)
{
  size_t        length;
  void *        memory;
  length = (size_t)IK_BYTEVECTOR_LENGTH(bv);
  memory = malloc(length);
  if (memory) {
    void *      data;
    data = IK_BYTEVECTOR_DATA_VOIDP(bv);
    memcpy(memory, data, length);
    return ik_pointer_alloc((unsigned long)memory, pcb);
  } else
    return false_object;
}


/** --------------------------------------------------------------------
 ** C strings.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_bytevector_to_cstring (ikptr bv, ikpcb * pcb)
{
  char *        pointer = IK_BYTEVECTOR_DATA_CHARP(bv);
  size_t        length  = (size_t)IK_BYTEVECTOR_LENGTH(bv);
  char *        cstr;
  cstr = malloc(1+length);
  if (cstr) {
    strncpy(cstr, pointer, length);
    cstr[length] = '\0';
    return ik_pointer_alloc((unsigned long)cstr, pcb);
  } else
    return false_object;
}
ikptr
ikrt_bytevector_from_cstring (ikptr s_pointer, ikptr s_count, ikpcb * pcb)
{
  char *        pointer = IK_POINTER_DATA_VOIDP(s_pointer);
  long          count   = unfix(s_count);
  ikptr         bv      = ik_bytevector_alloc(pcb, count);
  char *        data    = IK_BYTEVECTOR_DATA_CHARP(bv);
  memcpy(data, pointer, count);
  return bv;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_strlen (ikptr s_pointer, ikpcb * pcb)
{
  return ik_integer_from_long(strlen(IK_POINTER_DATA_VOIDP(s_pointer)), pcb);
}
ikptr
ikrt_strcmp (ikptr s_pointer1, ikptr s_pointer2)
{
  char *        ptr1 = IK_POINTER_DATA_VOIDP(s_pointer1);
  char *        ptr2 = IK_POINTER_DATA_VOIDP(s_pointer2);
  return fix(strcmp(ptr1, ptr2));
}
ikptr
ikrt_strncmp (ikptr s_pointer1, ikptr s_pointer2, ikptr s_count)
{
  char *        ptr1 = IK_POINTER_DATA_VOIDP(s_pointer1);
  char *        ptr2 = IK_POINTER_DATA_VOIDP(s_pointer2);
  return fix(strncmp(ptr1, ptr2, unfix(s_count)));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_strdup (ikptr s_pointer, ikpcb * pcb)
{
  char *        src = IK_POINTER_DATA_VOIDP(s_pointer);
  char *        dst = strdup(src);
  return (dst)? ik_pointer_alloc((unsigned long)dst, pcb) : false_object;
}
ikptr
ikrt_strndup (ikptr s_pointer, ikptr s_count, ikpcb * pcb)
{
  char *        src = IK_POINTER_DATA_VOIDP(s_pointer);
  char *        dst = strndup(src, unfix(s_count));
  return (dst)? ik_pointer_alloc((unsigned long)dst, pcb) : false_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_argv_from_bytevectors (ikptr bvs, ikpcb * pcb)
/* Convert a list of bytevectors  into a NULL-terminated array of ASCIIZ
   strings.   Return a pointer referencing the malloc-ed array. */
{
  int           argc = ik_list_length(bvs);
  size_t        total_length;
  char *        bv_data[1+argc];
  long          bv_len[argc];
  char **       argv;
  int           len;
  char *        str;
  int           i;
  ik_list_to_argv_and_argc(bvs, bv_data, bv_len);
  for (i=0, total_length=0; i<argc; total_length += sizeof(char*)+1+bv_len[i++]);
  total_length += sizeof(char*);
  argv = malloc(total_length);
  if (argv) {
    str = (char*)(((uint8_t*)argv) + sizeof(char*) * (1+argc));
    for (i=0; i<argc; ++i) {
      argv[i] = str;
      len     = bv_len[i];
      strncpy(str, bv_data[i], len);
      str[len] = '\0';
      str += 1+len;
    }
    argv[argc] = NULL;
    return ik_pointer_alloc((unsigned long)argv, pcb);
  } else
    return false_object;
}
ikptr
ikrt_argv_to_bytevectors (ikptr s_pointer, ikpcb * pcb)
{
  return ik_list_from_argv(pcb, IK_POINTER_DATA_VOIDP(s_pointer));
}
ikptr
ikrt_argv_length (ikptr s_pointer)
{
  char **       argv = IK_POINTER_DATA_VOIDP(s_pointer);
  long          length;
  for (length=0; argv[length]; ++length);
  return fix(length);
}


/** --------------------------------------------------------------------
 ** Local storage.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_with_local_storage (ikptr s_lengths, ikptr s_thunk, ikpcb * pcb)
{
  ikptr         code_entry      = ref(s_thunk, off_closure_code);
  ikptr         code_ptr        = code_entry - off_code_data;
  int           arity           = IK_VECTOR_LENGTH(s_lengths);
  long          total_length    = 0;
  long          lengths[arity];
  int           i;
  for (i=0; i<arity; ++i)
    total_length += lengths[i] = (long)unfix(IK_VECTOR_REF(s_lengths, i));
  {
    uint8_t     buffer[total_length];
    long        offset;
    /* Push the arguments on the Scheme stack. */
    pcb->frame_pointer = pcb->frame_base;
    for (i=0, offset=0; i<arity; offset+=lengths[i], ++i) {
      ref(pcb->frame_pointer, -2*wordsize-i*wordsize) =
        ik_pointer_alloc((unsigned long)&(buffer[offset]), pcb);
    }
    /* Call the Scheme procedure. */
    return ik_exec_code(pcb, code_ptr, fix(-arity), s_thunk);
  }
}


/** --------------------------------------------------------------------
 ** Raw memory getters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ref_uint8 (ikptr pointer, ikptr offset)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  return fix(*(memory+unfix(offset)));
}
ikptr
ikrt_ref_sint8 (ikptr pointer, ikptr offset)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  int8_t *      data   = (int8_t *)(memory + unfix(offset));
  return fix(*data);
}
ikptr
ikrt_ref_uint16 (ikptr pointer, ikptr offset)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  uint16_t *    data   = (uint16_t *)(memory + unfix(offset));
  return fix(*data);
}
ikptr
ikrt_ref_sint16 (ikptr pointer, ikptr offset)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  int16_t *     data   = (int16_t *)(memory + unfix(offset));
  return fix(*data);
}
ikptr
ikrt_ref_uint32 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  uint32_t *    data   = (uint32_t *)(memory + unfix(offset));
  return ik_integer_from_unsigned_long((unsigned long)(*data), pcb);
}
ikptr
ikrt_ref_sint32 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  int32_t *     data   = (int32_t *)(memory + unfix(offset));
  return ik_integer_from_long((long)(*data), pcb);
}
ikptr
ikrt_ref_uint64 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  uint64_t *    data   = (uint64_t *)(memory + unfix(offset));
  return ik_integer_from_unsigned_long_long((unsigned long long)(*data), pcb);
}
ikptr
ikrt_ref_sint64 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *     memory = IK_POINTER_DATA_UINT8P(pointer);
  int64_t *     data   = (int64_t *)(memory + unfix(offset));
  return ik_integer_from_long_long((long long)(*data), pcb);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_ref_float (ikptr pointer, ikptr offset, ikpcb* pcb)
{
  long          idx = ik_integer_to_long(offset);
  ikptr         ptr = ref(pointer, off_pointer_data);
  double        val = *((float*)(ptr+idx));
  return ik_flonum_from_double(val, pcb);
}
ikptr
ikrt_ref_double (ikptr pointer, ikptr offset, ikpcb* pcb)
{
  long          idx = ik_integer_to_long(offset);
  ikptr         ptr = ref(pointer, off_pointer_data);
  double        val = *((double*)(ptr+idx));
  return ik_flonum_from_double(val, pcb);
}
ikptr
ikrt_ref_pointer (ikptr pointer, ikptr offset, ikpcb* pcb)
{
  long          idx = ik_integer_to_long(offset);
  void *        ptr = (void*)ref(pointer, off_pointer_data);
  return ik_pointer_alloc(ref(ptr, idx), pcb);
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
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  *(memory+unfix(offset)) = unfix(value);
  return void_object;
}
ikptr
ikrt_set_sint8 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  int8_t *      data   = (int8_t *)(memory + unfix(offset));
  *data = unfix(value);
  return void_object;
}
ikptr
ikrt_set_uint16 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  uint16_t *    data   = (uint16_t *)(memory + unfix(offset));
  *data = (uint16_t)unfix(value);
  return void_object;
}
ikptr
ikrt_set_sint16 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  int16_t *     data   = (int16_t *)(memory + unfix(offset));
  *data = (int16_t)unfix(value);
  return void_object;
}
ikptr
ikrt_set_uint32 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  uint32_t *    data   = (uint32_t *)(memory + unfix(offset));
  *data = ik_integer_to_uint32(value);
  return void_object;
}
ikptr
ikrt_set_sint32 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  int32_t *     data   = (int32_t *)(memory + unfix(offset));
  *data = ik_integer_to_sint32(value);
  return void_object;
}
ikptr
ikrt_set_uint64 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  uint64_t *    data   = (uint64_t *)(memory + unfix(offset));
  *data = ik_integer_to_uint64(value);
  return void_object;
}
ikptr
ikrt_set_sint64 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *     memory = IK_POINTER_DATA_VOIDP(pointer);
  int64_t *     data   = (int64_t *)(memory + unfix(offset));
  *data = ik_integer_to_sint64(value);
  return void_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_set_float (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((float*)memory) = FLONUM_DATA(value);
  return void_object;
}
ikptr
ikrt_set_double (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((double*)memory) = FLONUM_DATA(value);
  return void_object;
}
ikptr
ikrt_set_pointer (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  void **  memory = IK_POINTER_DATA_VOIDP(pointer) + unfix(byte_offset);
  *memory = IK_POINTER_DATA_VOIDP(value);
  return void_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_set_char (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((char*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_uchar (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((unsigned char*)memory) = ik_integer_to_long(value);
  return void_object;
}

ikptr
ikrt_set_short (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((short*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_ushort (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((unsigned short*)memory) = ik_integer_to_long(value);
  return void_object;
}

ikptr
ikrt_set_int (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((int*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_uint (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((unsigned int*)memory) = ik_integer_to_unsigned_long(value);
  return void_object;
}

ikptr
ikrt_set_long (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((long*)memory) = ik_integer_to_long(value);
  return void_object;
}
ikptr
ikrt_set_ulong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((unsigned long*)memory) = ik_integer_to_unsigned_long(value);
  return void_object;
}

ikptr
ikrt_set_longlong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((long long*)memory) = ik_integer_to_long_long(value);
  return void_object;
}
ikptr
ikrt_set_ulonglong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  unsigned long  memory = IK_POINTER_DATA_ULONG(pointer) + unfix(byte_offset);
  *((unsigned long long*)memory) = ik_integer_to_unsigned_long_long(value);
  return void_object;
}

/* end of file */
