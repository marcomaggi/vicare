/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov	 6, 2011

  Abstract

	This  file is  without	license notice	in  the original  Ikarus
	distribution  for no  reason I	can know  (Marco Maggi;	 Nov 26,
	2011).

  Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006,2007,2008	Abdulaziz Ghuloum

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
#include <dlfcn.h>

#ifndef RTLD_LOCAL
#  define RTLD_LOCAL	0 /* for cygwin, possibly incorrect */
#endif


/** --------------------------------------------------------------------
 ** Interface to "errno".
 ** ----------------------------------------------------------------- */

ikptr
ikrt_set_errno (ikptr code)
{
  if (IK_FALSE_OBJECT == code)
    errno = 0;
  else if (IK_TRUE_OBJECT == code)
    errno = EFAULT;
  else
    errno = -(IK_FIX(code));
  return IK_VOID_OBJECT;
}
ikptr
ikrt_last_errno(ikpcb* pcb)
{
  int	negated_errno_code = - pcb->last_errno;
  return IK_FIX(negated_errno_code);
}


/** --------------------------------------------------------------------
 ** Shared libraries interface.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_dlerror (ikpcb* pcb)
{
  char* str = dlerror();
  if (NULL == str)
    return IK_FALSE_OBJECT;
  else {
    int		len  = strlen(str);
    ikptr	bv   = ika_bytevector_alloc(pcb, len);
    void *	data = IK_BYTEVECTOR_DATA_VOIDP(bv);
    memcpy(data, str, len);
    return bv;
  }
}
ikptr
ikrt_dlopen (ikptr library_name_bv, ikptr load_lazy, ikptr load_global, ikpcb* pcb)
{
  int		flags;
  char *	name;
  void *	memory;
  flags	 =
    ((load_lazy	  == IK_FALSE_OBJECT) ? RTLD_NOW	: RTLD_LAZY) |
    ((load_global == IK_FALSE_OBJECT) ? RTLD_LOCAL : RTLD_GLOBAL);
  name	 = (IK_FALSE_OBJECT == library_name_bv)? NULL : IK_BYTEVECTOR_DATA_CHARP(library_name_bv);
  memory = dlopen(name, flags);
  return (NULL == memory)? IK_FALSE_OBJECT : ika_pointer_alloc(pcb, (ik_ulong)memory);
}
ikptr
ikrt_dlclose (ikptr x /*, ikpcb* pcb*/)
{
  int	rv = dlclose(IK_POINTER_DATA_VOIDP(x));
  return (0 == rv) ? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_dlsym (ikptr handle, ikptr sym, ikpcb* pcb)
{
  void *  memory = dlsym(IK_POINTER_DATA_VOIDP(handle), IK_BYTEVECTOR_DATA_CHARP(sym));
  return (NULL == memory)? IK_FALSE_OBJECT : ika_pointer_alloc(pcb, (ik_ulong)memory);
}


/** --------------------------------------------------------------------
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

ikptr
ika_pointer_alloc (ikpcb * pcb, ik_ulong memory)
{
  ikptr	s_pointer = ik_safe_alloc(pcb, pointer_size) | vector_tag;
  IK_REF(s_pointer, off_pointer_tag)  = pointer_tag;
  IK_REF(s_pointer, off_pointer_data) = (ikptr)memory;
  return s_pointer;
}
ikptr
iku_pointer_alloc (ikpcb * pcb, ik_ulong memory)
{
  ikptr	s_pointer = ik_unsafe_alloc(pcb, pointer_size) | vector_tag;
  IK_REF(s_pointer, off_pointer_tag)  = pointer_tag;
  IK_REF(s_pointer, off_pointer_data) = (ikptr)memory;
  return s_pointer;
}
ikptr
ikrt_pointer_size (void)
{
  return IK_FIX(sizeof(void *));
}
ikptr
ikrt_is_pointer (ikptr x)
{
  return ((IK_TAGOF(x) == vector_tag) &&
	  (IK_REF(x, -vector_tag) == pointer_tag))? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
int
ik_is_pointer (ikptr X)
{
  return ((IK_TAGOF(X) == vector_tag) &&
	  (IK_REF(X, -vector_tag) == pointer_tag));
}
ikptr
ikrt_pointer_is_null (ikptr x /*, ikpcb* pcb*/)
{
  return IK_POINTER_DATA(x)? IK_FALSE_OBJECT : IK_TRUE_OBJECT;
}
ikptr
ikrt_pointer_set_null (ikptr pointer)
{
  IK_POINTER_SET_NULL(pointer);
  return IK_VOID_OBJECT;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_from_scheme_object (ikptr s_obj, ikpcb * pcb)
{
  return ika_pointer_alloc(pcb, (ik_ulong)s_obj);
}
ikptr
ikrt_pointer_to_scheme_object (ikptr s_pointer, ikpcb * pcb)
{
  void *	pointer = IK_POINTER_DATA_VOIDP(s_pointer);
  return (ikptr)pointer;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_to_int (ikptr pointer, ikpcb* pcb)
{
  void *	memory;
  memory = IK_POINTER_DATA_VOIDP(pointer);
  return ika_integer_from_ulong(pcb, (ik_ulong)memory);
}
ikptr
ikrt_fx_to_pointer(ikptr x, ikpcb* pcb)
{
  return ika_pointer_alloc(pcb, IK_UNFIX(x));
}
ikptr
ikrt_bn_to_pointer (ikptr x, ikpcb* pcb)
{
  if (IK_BNFST_NEGATIVE(IK_REF(x, -vector_tag)))
    return ika_pointer_alloc(pcb, -IK_REF(x, off_bignum_data));
  else
    return ika_pointer_alloc(pcb, +IK_REF(x, off_bignum_data));
}

/* ------------------------------------------------------------------ */

/* NOTE The  Scheme function POINTER-DIFF  is implemented at  the Scheme
   level because converting pointers  to Scheme exact integer objects is
   the simplest	 and safest  way to correctly  handle the full	range of
   possible pointer values. */

ikptr
ikrt_pointer_add (ikptr ptr, ikptr delta, ikpcb * pcb)
{
  ik_ullong	memory;
  ik_llong	ptrdiff;
  memory  = IK_POINTER_DATA_ULLONG(ptr);
  ptrdiff = ik_integer_to_llong(delta);
  if (0 <= ptrdiff) {
    if (ULONG_MAX - ptrdiff < memory) /* => ULONG_MAX < ptrdiff + memory */
      return IK_FALSE_OBJECT;
  } else {
    if (-ptrdiff > memory) /* => 0 > ptrdiff + memory */
      return IK_FALSE_OBJECT;
  }
  return ika_pointer_alloc(pcb, (ik_ulong)(memory + ptrdiff));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_pointer_eq (ikptr ptr1, ikptr ptr2)
{
  void *	memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *	memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 == memory2)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_pointer_neq (ikptr ptr1, ikptr ptr2)
{
  void *	memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *	memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 == memory2)? IK_FALSE_OBJECT : IK_TRUE_OBJECT;
}
ikptr
ikrt_pointer_lt (ikptr ptr1, ikptr ptr2)
{
  void *	memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *	memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 < memory2)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_pointer_gt (ikptr ptr1, ikptr ptr2)
{
  void *	memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *	memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 > memory2)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_pointer_le (ikptr ptr1, ikptr ptr2)
{
  void *	memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *	memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 <= memory2)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_pointer_ge (ikptr ptr1, ikptr ptr2)
{
  void *	memory1 = IK_POINTER_DATA_VOIDP(ptr1);
  void *	memory2 = IK_POINTER_DATA_VOIDP(ptr2);
  return (memory1 >= memory2)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}


/** --------------------------------------------------------------------
 ** C language level memory allocation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_malloc (ikptr s_number_of_bytes, ikpcb* pcb)
{
  void *	p = malloc(IK_UNFIX(s_number_of_bytes));
  return (p)? ika_pointer_alloc(pcb, (ik_ulong) p) : IK_FALSE_OBJECT;
}
ikptr
ikrt_realloc (ikptr s_pointer, ikptr number_of_bytes, ikpcb* pcb)
{
  void *	memory = IK_POINTER_DATA_VOIDP(s_pointer);
  void *	new_memory;
  if (memory) {
    new_memory = realloc(memory, IK_UNFIX(number_of_bytes));
    if (new_memory) {
      IK_REF(s_pointer, off_pointer_data) = (ikptr)new_memory;
      return s_pointer;
    } else
      return IK_FALSE_OBJECT;
  } else
    return IK_FALSE_OBJECT;
}
ikptr
ikrt_calloc (ikptr s_number_of_elements, ikptr s_element_size, ikpcb* pcb)
{
  void * memory = calloc(IK_UNFIX(s_number_of_elements), IK_UNFIX(s_element_size));
  return (memory)? ika_pointer_alloc(pcb, (ik_ulong)memory) : IK_FALSE_OBJECT;
}
ikptr
ikrt_free (ikptr s_pointer)
{
  void * memory = IK_POINTER_DATA_VOIDP(s_pointer);
  if (memory) {
    free(memory);
    IK_POINTER_SET_NULL(s_pointer);
  }
  return IK_VOID_OBJECT;
}


/** --------------------------------------------------------------------
 ** C language level memory operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_memcpy (ikptr dst, ikptr src, ikptr size)
{
  memcpy(IK_POINTER_DATA_VOIDP(dst),
	 IK_POINTER_DATA_VOIDP(src),
	 IK_UNFIX(size));
  return IK_VOID_OBJECT;
}
ikptr
ikrt_memmove (ikptr dst, ikptr src, ikptr size)
{
  memmove(IK_POINTER_DATA_VOIDP(dst),
	  IK_POINTER_DATA_VOIDP(src),
	  IK_UNFIX(size));
  return IK_VOID_OBJECT;
}
ikptr
ikrt_memset (ikptr ptr, ikptr byte, ikptr size)
{
  memset(IK_POINTER_DATA_VOIDP(ptr), IK_UNFIX(byte), IK_UNFIX(size));
  return IK_VOID_OBJECT;
}
ikptr
ikrt_memcmp (ikptr pointer1, ikptr pointer2, ikptr count)
{
  int	rv;
  rv = memcmp(IK_POINTER_DATA_VOIDP(pointer1),
	      IK_POINTER_DATA_VOIDP(pointer2),
	      IK_UNFIX(count));
  return IK_FIX(rv);
}


/** --------------------------------------------------------------------
 ** Raw memory and Scheme bytevector operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_memcpy_to_bv (ikptr s_dst, ikptr s_dst_off, ikptr s_src, ikptr s_count)
{
  void * src = IK_POINTER_DATA_VOIDP(s_src);
  void * dst = (void *)(IK_BYTEVECTOR_DATA(s_dst) + IK_UNFIX(s_dst_off));
  memcpy(dst, src, IK_UNFIX(s_count));
  return IK_VOID_OBJECT;
}
ikptr
ikrt_memcpy_from_bv (ikptr s_dst, ikptr s_src, ikptr s_src_off, ikptr s_count)
{
  void * src = (void *)(IK_BYTEVECTOR_DATA(s_src) + IK_UNFIX(s_src_off));
  void * dst = IK_POINTER_DATA_VOIDP(s_dst);
  memcpy(dst, src, IK_UNFIX(s_count));
  return IK_VOID_OBJECT;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_bytevector_from_memory (ikptr s_pointer, ikptr s_length, ikpcb * pcb)
{
  void *	memory = IK_POINTER_DATA_VOIDP(s_pointer);
  size_t	size   = (size_t)IK_UNFIX(s_length);
  return ika_bytevector_from_memory_block(pcb, memory, size);
}
ikptr
ikrt_bytevector_to_memory (ikptr s_bv, ikpcb * pcb)
{
  size_t	length = (size_t)IK_BYTEVECTOR_LENGTH(s_bv);
  void *	memory = malloc(length);
  if (memory) {
    void *	data;
    data = IK_BYTEVECTOR_DATA_VOIDP(s_bv);
    memcpy(memory, data, length);
    return ika_pointer_alloc(pcb, (ik_ulong)memory);
  } else
    return IK_FALSE_OBJECT;
}


/** --------------------------------------------------------------------
 ** C strings.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_bytevector_to_cstring (ikptr bv, ikpcb * pcb)
{
  char *	pointer = IK_BYTEVECTOR_DATA_CHARP(bv);
  size_t	length	= (size_t)IK_BYTEVECTOR_LENGTH(bv);
  char *	cstr;
  int		i;
  cstr = malloc(1+length);
  if (cstr) {
    /* Notice that:

          strncpy(cstr, pointer, length);

       does the wrong thing here when there are zero bytes in the middle
       of the  bytevector: it copies the  bytes up until the  first zero
       byte, then  pads the  destination with  zeros.  We  want verbatim
       copying  here,  in  case  the   input  bytevector  is  in  UTF-16
       encoding. */
    for (i=0; i<length; ++i)
      cstr[i] = pointer[i];
    cstr[length] = '\0';
    return ika_pointer_alloc(pcb, (ik_ulong)cstr);
  } else
    return IK_FALSE_OBJECT;
}
ikptr
ikrt_bytevector_from_cstring (ikptr s_pointer, ikptr s_count, ikpcb * pcb)
{
  char *	memory	= IK_POINTER_DATA_VOIDP(s_pointer);
  long		count	= IK_UNFIX(s_count);
  ikptr		s_bv	= ika_bytevector_alloc(pcb, count);
  char *	data	= IK_BYTEVECTOR_DATA_CHARP(s_bv);
  memcpy(data, memory, count);
  return s_bv;
}
ikptr
ikrt_bytevector_from_cstring16 (ikptr s_pointer, ikpcb * pcb)
{
  return ik_bytevector_from_utf16z(pcb, IK_POINTER_DATA_VOIDP(s_pointer));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_strlen (ikptr s_pointer, ikpcb * pcb)
{
  return ika_integer_from_long(pcb, strlen(IK_POINTER_DATA_VOIDP(s_pointer)));
}
ikptr
ikrt_strcmp (ikptr s_pointer1, ikptr s_pointer2)
{
  char *	ptr1 = IK_POINTER_DATA_VOIDP(s_pointer1);
  char *	ptr2 = IK_POINTER_DATA_VOIDP(s_pointer2);
  return IK_FIX(strcmp(ptr1, ptr2));
}
ikptr
ikrt_strncmp (ikptr s_pointer1, ikptr s_pointer2, ikptr s_count)
{
  char *	ptr1 = IK_POINTER_DATA_VOIDP(s_pointer1);
  char *	ptr2 = IK_POINTER_DATA_VOIDP(s_pointer2);
  return IK_FIX(strncmp(ptr1, ptr2, IK_UNFIX(s_count)));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_strdup (ikptr s_pointer, ikpcb * pcb)
{
  char *	src = IK_POINTER_DATA_VOIDP(s_pointer);
  char *	dst = strdup(src);
  return (dst)? ika_pointer_alloc(pcb, (ik_ulong)dst) : IK_FALSE_OBJECT;
}
ikptr
ikrt_strndup (ikptr s_pointer, ikptr s_count, ikpcb * pcb)
{
  char *	src = IK_POINTER_DATA_VOIDP(s_pointer);
  char *	dst = strndup(src, IK_UNFIX(s_count));
  return (dst)? ika_pointer_alloc(pcb, (ik_ulong)dst) : IK_FALSE_OBJECT;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_argv_from_bytevectors (ikptr s_bvs, ikpcb * pcb)
/* Convert a list of bytevectors  into a NULL-terminated array of ASCIIZ
   strings.   Return a pointer referencing the malloc-ed array. */
{
  int		argc = ik_list_length(s_bvs);
  size_t	total_length;
  char *	bv_data[1+argc];
  long		bv_len[argc];
  char **	argv;
  int		len;
  char *	str;
  int		i;
  ik_list_to_argv_and_argc(s_bvs, bv_data, bv_len);
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
    return ika_pointer_alloc(pcb, (ik_ulong)argv);
  } else
    return IK_FALSE_OBJECT;
}
ikptr
ikrt_argv_to_bytevectors (ikptr s_pointer, ikpcb * pcb)
{
  return ika_list_from_argv(pcb, IK_POINTER_DATA_VOIDP(s_pointer));
}
ikptr
ikrt_argv_length (ikptr s_pointer)
{
  char **	argv = IK_POINTER_DATA_VOIDP(s_pointer);
  long		length;
  for (length=0; argv[length]; ++length);
  return IK_FIX(length);
}


/** --------------------------------------------------------------------
 ** Local storage.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_with_local_storage (ikptr s_lengths, ikptr s_thunk, ikpcb * pcb)
{
  ikptr		code_entry	= IK_REF(s_thunk, off_closure_code);
  ikptr		code_ptr	= code_entry - off_code_data;
  int		arity		= IK_VECTOR_LENGTH(s_lengths);
  long		total_length	= 0;
  long		lengths[arity];
  int		i;
  ikptr		sk;
  ikptr		s_result;
  for (i=0; i<arity; ++i)
    total_length += lengths[i] = IK_UNFIX(IK_ITEM(s_lengths, i));
  sk = ik_enter_c_function(pcb);
  {
    uint8_t	buffer[total_length];
    long	offset;
    /* Push the arguments on the Scheme stack. */
    pcb->frame_pointer = pcb->frame_base;
    for (i=0, offset=0; i<arity; offset+=lengths[i], ++i) {
      IK_REF(pcb->frame_pointer, -2*wordsize-i*wordsize) =
	/* Allocation without garbage collection!!! */
	iku_pointer_alloc(pcb, (ik_ulong)&(buffer[offset]));
    }
    /* Call the Scheme procedure. */
    s_result = ik_exec_code(pcb, code_ptr, IK_FIX(-arity), s_thunk);
  }
  ik_leave_c_function(pcb, sk);
  return s_result;
}


/** --------------------------------------------------------------------
 ** Raw memory getters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ref_uint8 (ikptr pointer, ikptr offset)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  return IK_FIX(*(memory+IK_UNFIX(offset)));
}
ikptr
ikrt_ref_sint8 (ikptr pointer, ikptr offset)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  int8_t *	data   = (int8_t *)(memory + IK_UNFIX(offset));
  return IK_FIX(*data);
}
ikptr
ikrt_ref_uint16 (ikptr pointer, ikptr offset)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  uint16_t *	data   = (uint16_t *)(memory + IK_UNFIX(offset));
  return IK_FIX(*data);
}
ikptr
ikrt_ref_sint16 (ikptr pointer, ikptr offset)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  int16_t *	data   = (int16_t *)(memory + IK_UNFIX(offset));
  return IK_FIX(*data);
}
ikptr
ikrt_ref_uint32 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  uint32_t *	data   = (uint32_t *)(memory + IK_UNFIX(offset));
  return ika_integer_from_ulong(pcb, (ik_ulong)(*data));
}
ikptr
ikrt_ref_sint32 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  int32_t *	data   = (int32_t *)(memory + IK_UNFIX(offset));
  return ika_integer_from_long(pcb, (long)(*data));
}
ikptr
ikrt_ref_uint64 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  uint64_t *	data   = (uint64_t *)(memory + IK_UNFIX(offset));
  return ika_integer_from_ullong(pcb, (ik_ullong)(*data));
}
ikptr
ikrt_ref_sint64 (ikptr pointer, ikptr offset, ikpcb * pcb)
{
  uint8_t *	memory = IK_POINTER_DATA_UINT8P(pointer);
  int64_t *	data   = (int64_t *)(memory + IK_UNFIX(offset));
  return ika_integer_from_llong(pcb, (ik_llong)(*data));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_ref_float (ikptr pointer, ikptr offset, ikpcb* pcb)
{
  long		idx = ik_integer_to_long(offset);
  ikptr		ptr = IK_REF(pointer, off_pointer_data);
  double	val = *((float*)(ptr+idx));
  return ika_flonum_from_double(pcb, val);
}
ikptr
ikrt_ref_double (ikptr pointer, ikptr offset, ikpcb* pcb)
{
  long		idx = ik_integer_to_long(offset);
  ikptr		ptr = IK_REF(pointer, off_pointer_data);
  double	val = *((double*)(ptr+idx));
  return ika_flonum_from_double(pcb, val);
}
ikptr
ikrt_ref_pointer (ikptr pointer, ikptr offset, ikpcb* pcb)
{
  long		idx = ik_integer_to_long(offset);
  void *	ptr = (void*)IK_REF(pointer, off_pointer_data);
  return ika_pointer_alloc(pcb, IK_REF(ptr, idx));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_ref_char(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return IK_FIX(*((signed char*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off))));
}
ikptr
ikrt_ref_uchar(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return IK_FIX(*((unsigned char*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off))));
}
ikptr
ikrt_ref_short(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return IK_FIX(*((signed short*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off))));
}
ikptr
ikrt_ref_ushort(ikptr p, ikptr off /*, ikpcb* pcb*/)
{
  return IK_FIX(*((unsigned short*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off))));
}
ikptr
ikrt_ref_int(ikptr p, ikptr off , ikpcb* pcb) {
  signed int r =
    *((signed int*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off)));
  if (wordsize == 8) {
    return IK_FIX(r);
  } else {
    return ika_integer_from_long(pcb, r);
  }
}
ikptr
ikrt_ref_uint(ikptr p, ikptr off , ikpcb* pcb)
{
  unsigned int r = *((unsigned int*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off)));
  return (wordsize == 8)? IK_FIX(r) : ika_integer_from_ulong(pcb, r);
}
ikptr
ikrt_ref_long(ikptr p, ikptr off , ikpcb* pcb)
{
  long r = *((long*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off)));
  return ika_integer_from_long(pcb, r);
}
ikptr
ikrt_ref_ulong(ikptr p, ikptr off , ikpcb* pcb)
{
  ik_ulong r = *((ik_ulong*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off)));
  return ika_integer_from_ulong(pcb, r);
}
ikptr
ikrt_ref_longlong(ikptr p, ikptr off , ikpcb* pcb)
{
  ik_llong r = *((ik_llong*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off)));
  return ika_integer_from_llong(pcb, r);
}
ikptr
ikrt_ref_ulonglong(ikptr p, ikptr off , ikpcb* pcb)
{
  ik_ullong r = *((ik_ullong*)(((long)IK_REF(p, off_pointer_data)) + IK_UNFIX(off)));
  return ika_integer_from_ullong(pcb, r);
}


/** --------------------------------------------------------------------
 ** Raw memory setters through pointers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_set_uint8 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  *(memory+IK_UNFIX(offset)) = IK_UNFIX(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_sint8 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  int8_t *	data   = (int8_t *)(memory + IK_UNFIX(offset));
  *data = IK_UNFIX(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_uint16 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  uint16_t *	data   = (uint16_t *)(memory + IK_UNFIX(offset));
  *data = (uint16_t)IK_UNFIX(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_sint16 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  int16_t *	data   = (int16_t *)(memory + IK_UNFIX(offset));
  *data = (int16_t)IK_UNFIX(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_uint32 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  uint32_t *	data   = (uint32_t *)(memory + IK_UNFIX(offset));
  *data = ik_integer_to_uint32(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_sint32 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  int32_t *	data   = (int32_t *)(memory + IK_UNFIX(offset));
  *data = ik_integer_to_sint32(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_uint64 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  uint64_t *	data   = (uint64_t *)(memory + IK_UNFIX(offset));
  *data = ik_integer_to_uint64(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_sint64 (ikptr pointer, ikptr offset, ikptr value)
{
  uint8_t *	memory = IK_POINTER_DATA_VOIDP(pointer);
  int64_t *	data   = (int64_t *)(memory + IK_UNFIX(offset));
  *data = ik_integer_to_sint64(value);
  return IK_VOID_OBJECT;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_set_float (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((float*)memory) = IK_FLONUM_DATA(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_double (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((double*)memory) = IK_FLONUM_DATA(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_pointer (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  void **  memory = IK_POINTER_DATA_VOIDP(pointer) + IK_UNFIX(byte_offset);
  *memory = IK_POINTER_DATA_VOIDP(value);
  return IK_VOID_OBJECT;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_set_char (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((char*)memory) = ik_integer_to_long(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_uchar (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((unsigned char*)memory) = ik_integer_to_long(value);
  return IK_VOID_OBJECT;
}

ikptr
ikrt_set_short (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((short*)memory) = ik_integer_to_long(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_ushort (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((unsigned short*)memory) = ik_integer_to_long(value);
  return IK_VOID_OBJECT;
}

ikptr
ikrt_set_int (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((int*)memory) = ik_integer_to_long(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_uint (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((unsigned int*)memory) = ik_integer_to_ulong(value);
  return IK_VOID_OBJECT;
}

ikptr
ikrt_set_long (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((long*)memory) = ik_integer_to_long(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_ulong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((ik_ulong*)memory) = ik_integer_to_ulong(value);
  return IK_VOID_OBJECT;
}

ikptr
ikrt_set_longlong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((ik_llong*)memory) = ik_integer_to_llong(value);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_ulonglong (ikptr pointer, ikptr byte_offset, ikptr value /*, ikpcb* pcb*/)
{
  ik_ulong  memory = IK_POINTER_DATA_ULONG(pointer) + IK_UNFIX(byte_offset);
  *((ik_ullong*)memory) = ik_integer_to_ullong(value);
  return IK_VOID_OBJECT;
}

/* end of file */
