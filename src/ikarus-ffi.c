/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        This module defines the interface between Vicare and Libffi.

        This  file is  without  license notice  in  the original  Ikarus
        distribution  for no  reason I  can know  (Marco Maggi;  Nov 26,
        2011).

  Copyright (C) 2011-2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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

#include "internals.h"
static ikptr    seal_scheme_stack            (ikpcb* pcb);

#ifdef HAVE_LIBFFI
#include <ffi.h>

#undef DEBUG_FFI

#ifdef LIBFFI_ON_DARWIN
#  include <sys/mman.h>         /* for "mprotect()" */
#endif


/** --------------------------------------------------------------------
 ** Constants and variables.
 ** ----------------------------------------------------------------- */

/* These  constants must  be kept  in  sync with  the ones  in the  file
   "ikarus.pointers.sls". */
typedef enum type_id_t {
  TYPE_ID_VOID          =  0,   /* &ffi_type_void    */
  TYPE_ID_UINT8         =  1,   /* &ffi_type_uint8   */
  TYPE_ID_SINT8         =  2,   /* &ffi_type_sint8   */
  TYPE_ID_UINT16        =  3,   /* &ffi_type_uint16  */
  TYPE_ID_SINT16        =  4,   /* &ffi_type_sint16  */
  TYPE_ID_UINT32        =  5,   /* &ffi_type_uint32  */
  TYPE_ID_SINT32        =  6,   /* &ffi_type_sint32  */
  TYPE_ID_UINT64        =  7,   /* &ffi_type_uint64  */
  TYPE_ID_SINT64        =  8,   /* &ffi_type_sint64  */
  TYPE_ID_FLOAT         =  9,   /* &ffi_type_float   */
  TYPE_ID_DOUBLE        = 10,   /* &ffi_type_double  */
  TYPE_ID_POINTER       = 11,   /* &ffi_type_pointer */
  TYPE_ID_UCHAR         = 12,   /* &ffi_type_uchar   */
  TYPE_ID_SCHAR         = 13,   /* &ffi_type_schar   */
  TYPE_ID_USHORT        = 14,   /* &ffi_type_ushort  */
  TYPE_ID_SSHORT        = 15,   /* &ffi_type_sshort  */
  TYPE_ID_UINT          = 16,   /* &ffi_type_uint    */
  TYPE_ID_SINT          = 17,   /* &ffi_type_sint    */
  TYPE_ID_ULONG         = 18,   /* &ffi_type_ulong   */
  TYPE_ID_SLONG         = 19,   /* &ffi_type_slong   */
  TYPE_ID_NUMBER        = 20
} type_id_t;

static ffi_type * the_ffi_types_array[TYPE_ID_NUMBER] = {
  &ffi_type_void,       /*  0 */
  &ffi_type_uint8,      /*  1 */
  &ffi_type_sint8,      /*  2 */
  &ffi_type_uint16,     /*  3 */
  &ffi_type_sint16,     /*  4 */
  &ffi_type_uint32,     /*  5 */
  &ffi_type_sint32,     /*  6 */
  &ffi_type_uint64,     /*  7 */
  &ffi_type_sint64,     /*  8 */
  &ffi_type_float,      /*  9 */
  &ffi_type_double,     /* 10 */
  &ffi_type_pointer,    /* 11 */
  &ffi_type_uchar,      /* 12 */
  &ffi_type_schar,      /* 13 */
  &ffi_type_ushort,     /* 14 */
  &ffi_type_sshort,     /* 15 */
  &ffi_type_uint,       /* 16 */
  &ffi_type_sint,       /* 17 */
  &ffi_type_ulong,      /* 18 */
  &ffi_type_slong       /* 19 */
};

static size_t the_ffi_type_sizes[TYPE_ID_NUMBER] = {
  0,                            /*  0 */
  sizeof(uint8_t),              /*  1 */
  sizeof(int8_t),               /*  2 */
  sizeof(uint16_t),             /*  3 */
  sizeof(int16_t),              /*  4 */
  sizeof(uint32_t),             /*  5 */
  sizeof(int32_t),              /*  6 */
  sizeof(uint64_t),             /*  7 */
  sizeof(int64_t),              /*  8 */
  sizeof(float),                /*  9 */
  sizeof(double),               /* 10 */
  sizeof(void *),               /* 11 */
  sizeof(unsigned char),        /* 12 */
  sizeof(char),                 /* 13 */
  sizeof(unsigned short),       /* 14 */
  sizeof(short),                /* 15 */
  sizeof(unsigned int),         /* 16 */
  sizeof(int),                  /* 17 */
  sizeof(ik_ulong),        /* 18 */
  sizeof(long)                  /* 19 */
};


/** --------------------------------------------------------------------
 ** Type definitions.
 ** ----------------------------------------------------------------- */

typedef void address_t ();

/* This structure exists to make  it easier to allocate data required by
   Libffi's Call InterFace; it wraps a Libffi's "ffi_cif" type providing
   a full description of the interface for callouts and callbacks. */
typedef struct ik_ffi_cif_stru_t {
  ffi_cif       cif;            /* Libffi's CIF structure */
  unsigned      arity;          /* number of arguments */
  size_t        args_bufsize;   /* number of  bytes to allocate  to hold
                                   the arguments as native values */
  ffi_type *    retval_type;    /* Libffi's type structure for return value */
  type_id_t     retval_type_id; /* type identifier for return value */
  ffi_type **   arg_types;      /* Libffi's type structures for arguments */
  type_id_t *   arg_type_ids;   /* type identifiers for arguments */
  uint8_t       data[0];        /* appended data */
} ik_ffi_cif_stru_t;

typedef ik_ffi_cif_stru_t *      ik_ffi_cif_t;

/* Compute the size  of memory block to hold  a full "ik_ffi_cif_stru_t"
   with array data  appended.  ARITY is the number  of arguments for the
   described call interface. */
#define IK_FFI_CIF_SIZEOF(ARITY)                \
  (sizeof(ik_ffi_cif_stru_t)+(1+(ARITY))*sizeof(ffi_type*)+(ARITY)*sizeof(type_id_t*))

/* Given a pointer  CIF of type "ik_ffi_cif_t", return  a pointer to the
   array  of  "ffy_type" structures  describing  the  type  of the  call
   arguments; such pointer must be stored the "arg_types" field. */
#define IK_FFI_CIF_ARG_TYPES_PTR(CIF,ARITY)     \
  ((ffi_type**)(((uint8_t*)cif) + sizeof(ik_ffi_cif_stru_t)))

/* Given a pointer  CIF of type "ik_ffi_cif_t", return  a pointer to the
   array  of  "type_id_t"  integers  describing  the type  of  the  call
   arguments; such pointer must be stored the "arg_type_ids" field. */
#define IK_FFI_CIF_ARG_TYPE_IDS_PTR(CIF,ARITY)  \
  ((type_id_t*)(((uint8_t*)cif) + sizeof(ik_ffi_cif_stru_t) + (1+(ARITY))*sizeof(ffi_type*)))

static void     scheme_to_native_value_cast  (type_id_t type_id, ikptr s_scheme_value, void * buffer);
static ikptr    ika_native_to_scheme_value_cast  (type_id_t type_id, void * buffer, ikpcb* pcb);
static void     generic_callback             (ffi_cif *cif, void *ret, void **args, void *user_data);


/** --------------------------------------------------------------------
 ** Helpers and miscellaneous small functions.
 ** ----------------------------------------------------------------- */

static void*
alloc (size_t n, long m)
{
  void  * ptr = calloc(n, m);
  if (!ptr)
    ik_abort("failed memory allocation with calloc(%u, %ld)", n, m);
  return ptr;
}
ikptr
ikrt_has_ffi (void)
{
  return IK_TRUE_OBJECT;
}

#ifdef DEBUG_FFI
static void
dump_stack (ikpcb* pcb, char* msg)
{
  fprintf(stderr, "====================  %s\n", msg);
  ikptr frame_base = pcb->frame_base;
  ikptr frame_pointer = pcb->frame_pointer;
  ikptr p = frame_pointer;
  fprintf(stderr, "fp=0x%016lx   base=0x%016lx\n", frame_pointer, frame_base);
  while (p < frame_base) {
    fprintf(stderr, "*0x%016lx = 0x%016lx\n", p, IK_REF(p, 0));
    p += wordsize;
  }
}
#endif


/** --------------------------------------------------------------------
 ** Call InterFace (CIF) preparation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_prep_cif (ikptr s_type_ids, ikpcb* pcb)
/* Wrapper  for   Libffi's  "ffi_prep_cif()";  prepare   a  Libffi  call
   interface (CIF) building an  appropriate data structure, whose memory
   is obtained  by "calloc()".  Return a pointer  object referencing the
   CIF structure.

   The generated CIR can be used for both callouts and callbacks.

   S_TYPE_IDS  must  be a  vector  of  fixnums  in the  set  "type_id_t"
   selecting the type  of the arguments and return  value; the fixnum at
   index 0  represents the type of  the return value,  the other fixnums
   the type of the arguments. */
{
  unsigned      arity = ((unsigned)IK_VECTOR_LENGTH(s_type_ids))-1;
  ik_ffi_cif_t  cif   = alloc(IK_FFI_CIF_SIZEOF(arity), 1);
  ffi_status    rv;
  int           i;
  cif->arg_types      = IK_FFI_CIF_ARG_TYPES_PTR(cif, arity);
  cif->arg_type_ids   = IK_FFI_CIF_ARG_TYPE_IDS_PTR(cif, arity);
  cif->arity          = arity;
  cif->retval_type_id = IK_UNFIX(IK_ITEM(s_type_ids, 0));
  cif->retval_type    = the_ffi_types_array[cif->retval_type_id];
  cif->args_bufsize   = 0;
  for (i=0; i<arity; ++i) {
    type_id_t   id       =  IK_UNFIX(IK_ITEM(s_type_ids, 1+i));
    cif->args_bufsize    += the_ffi_type_sizes[id];
    cif->arg_type_ids[i] =  id;
    cif->arg_types[i]    =  the_ffi_types_array[id];
  }
  cif->arg_types[arity] = NULL;
  rv = ffi_prep_cif(&(cif->cif), FFI_DEFAULT_ABI, arity, cif->retval_type, cif->arg_types);
  return (FFI_OK == rv)? ika_pointer_alloc(pcb, (ik_ulong)cif) : IK_FALSE_OBJECT;
}


/** --------------------------------------------------------------------
 ** Converting Scheme values to and from native values.
 ** ----------------------------------------------------------------- */

static void
scheme_to_native_value_cast (type_id_t type_id, ikptr s_scheme_value, void * buffer)
/* Convert  the S_SCHEME_VALUE to  a native  value and  store it  in the
   block of memory  referenced by BUFFER; the type  is selected TYPE_ID.
   S_SCHEME_VALUE must have been already validated.  BUFFER must be wide
   enough. */
{
  switch (type_id) {
  case TYPE_ID_VOID: return;

  case TYPE_ID_UINT8:   *((uint8_t*)         buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_SINT8:   *((int8_t*)          buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_UINT16:  *((uint16_t*)        buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_SINT16:  *((int16_t*)         buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_UINT32:  *((uint32_t*)        buffer) = ik_integer_to_uint32(s_scheme_value); return;
  case TYPE_ID_SINT32:  *((int32_t*)         buffer) = ik_integer_to_sint32(s_scheme_value); return;
  case TYPE_ID_UINT64:  *((uint64_t*)        buffer) = ik_integer_to_uint64(s_scheme_value); return;
  case TYPE_ID_SINT64:  *((int64_t*)         buffer) = ik_integer_to_sint64(s_scheme_value); return;

  case TYPE_ID_FLOAT:   *((float*)          buffer) = IK_FLONUM_DATA(s_scheme_value); return;
  case TYPE_ID_DOUBLE:  *((double*)         buffer) = IK_FLONUM_DATA(s_scheme_value); return;

  case TYPE_ID_POINTER:
    /* This  supports  bytevector  arguments  to  foreign  functions  as
       pointers.  Currently  undocumented because it  is unsafe.  (Marco
       Maggi; Jan 13, 2012) */
    if (IK_IS_BYTEVECTOR(s_scheme_value))
      *((void**)buffer) = IK_BYTEVECTOR_DATA_VOIDP(s_scheme_value);
    else
      *((void**)buffer) = IK_POINTER_DATA_VOIDP(s_scheme_value);
    return;

  case TYPE_ID_UCHAR:   *((unsigned char*)  buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_SCHAR:   *((char*)           buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_USHORT:  *((unsigned short*) buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_SSHORT:  *((signed short*)   buffer) = IK_UNFIX(s_scheme_value); return;
  case TYPE_ID_UINT:    *((unsigned int*)   buffer) = ik_integer_to_long(s_scheme_value); return;
  case TYPE_ID_SINT:    *((signed int*)     buffer) = ik_integer_to_long(s_scheme_value); return;
  case TYPE_ID_ULONG:   *((ik_ulong*)       buffer) = ik_integer_to_long(s_scheme_value); return;
  case TYPE_ID_SLONG:   *((long*)           buffer) = ik_integer_to_long(s_scheme_value); return;

  default:
    ik_abort("%s: invalid argument type selector %d", __func__, (int)type_id);
  }
}
static ikptr
ika_native_to_scheme_value_cast (type_id_t type_id, void * buffer, ikpcb* pcb)
/* Convert the native value stored  in the block of memory referenced by
   BUFFER to  a Scheme value  and return the  Scheme value; the  type is
   selected by TYPE_ID. */
{
  switch (type_id) {
  case TYPE_ID_VOID:    return IK_VOID_OBJECT;

  case TYPE_ID_UINT8:   return IK_FIX(*((uint8_t*) buffer));
  case TYPE_ID_SINT8:   return IK_FIX(*(( int8_t*) buffer));
  case TYPE_ID_UINT16:  return IK_FIX(*((uint16_t*)buffer));
  case TYPE_ID_SINT16:  return IK_FIX(*(( int16_t*)buffer));
  case TYPE_ID_UINT32:  return ika_integer_from_ulong (pcb, *((uint32_t*) buffer));
  case TYPE_ID_SINT32:  return ika_integer_from_long  (pcb, *((long*)     buffer));
  case TYPE_ID_UINT64:  return ika_integer_from_ullong(pcb, *((ik_ullong*)buffer));
  case TYPE_ID_SINT64:  return ika_integer_from_llong (pcb, *((ik_llong*) buffer));

  case TYPE_ID_FLOAT:   return ika_flonum_from_double	(pcb, *((float*)      buffer));
  case TYPE_ID_DOUBLE:  return ika_flonum_from_double	(pcb, *((double*)     buffer));
  case TYPE_ID_POINTER: return ika_pointer_alloc	(pcb, (long)*((void**)buffer));

  case TYPE_ID_UCHAR:   return ika_integer_from_ulong(pcb, *((unsigned char*) buffer));
  case TYPE_ID_SCHAR:   return ika_integer_from_long (pcb, *((signed char*)   buffer));
  case TYPE_ID_USHORT:  return ika_integer_from_ulong(pcb, *((unsigned short*)buffer));
  case TYPE_ID_SSHORT:  return ika_integer_from_long (pcb, *((signed short*)  buffer));
  case TYPE_ID_UINT:    return ika_integer_from_ulong(pcb, *((unsigned int*)  buffer));
  case TYPE_ID_SINT:    return ika_integer_from_long (pcb, *((signed int*)    buffer));
  case TYPE_ID_ULONG:   return ika_integer_from_ulong(pcb, *((ik_ulong*)      buffer));
  case TYPE_ID_SLONG:   return ika_integer_from_long (pcb, *((long*)          buffer));

  default:
    ik_abort("%s: invalid arg %d", __func__, (int)type_id);
    return IK_VOID_OBJECT;
  }
}


/** --------------------------------------------------------------------
 ** Callout: call a C function from Scheme code.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_call (ikptr s_data, ikptr s_args, ikpcb * pcb)
/* Perform a callout and return the return value of the callout.

   S_DATA  must  be  a pair  whose  car  is  a  pointer object  of  type
   "ik_ffi_cif_t"  and whose cdr  is a  pointer object  representing the
   address of the foreign function to call.

   S_ARGS must be a vector holding the call arguments.  */
{
  ikptr         return_value;
  ikptr         sk;
  size_t        args_bufsize;
  seal_scheme_stack(pcb);
  sk = ik_unsafe_alloc(pcb, system_continuation_size) | vector_tag;
  IK_REF(sk, off_system_continuation_tag)  = system_continuation_tag;
  IK_REF(sk, off_system_continuation_top)  = pcb->system_stack;
  IK_REF(sk, off_system_continuation_next) = pcb->next_k;
  pcb->next_k = sk;
  {
    ik_ffi_cif_t  cif     = IK_POINTER_DATA_VOIDP(IK_CAR(s_data));
    address_t *   address = IK_POINTER_DATA_VOIDP(IK_CDR(s_data));
    /* Prepare  memory   to  hold  native   values  representing  Scheme
       arguments and the return value */
    uint8_t     args_buffer[cif->args_bufsize];
    uint8_t *   arg_next = &(args_buffer[0]);
    uint8_t *   arg_end  = arg_next + cif->args_bufsize;
    void *      arg_value_ptrs[1+cif->arity];
    /* It seems  that Libffi expects  at least a return-value  buffer of
       size  "sizeof(uint64_t)"  even for  smaller  types,  at least  on
       64-bit platforms.  Let's play it safe and try to forget about it.
       (Marco Maggi; Aug 1, 2012) */
    uint8_t     retval_buffer[(cif->retval_type->size < sizeof(uint64_t))? \
			      sizeof(uint64_t) : cif->retval_type->size];
    /* Fill ARG_VALUE_PTRS  with pointers  to memory blocks  holding the
       native argument values. */
    int  i;
    for (i=0; i<cif->arity; i++) {
      ikptr  value = IK_ITEM(s_args, i);
      arg_value_ptrs[i] = arg_next;
      scheme_to_native_value_cast(cif->arg_type_ids[i], value, arg_next);
      arg_next += cif->arg_types[i]->size;
      if (arg_end < arg_next) {
        args_bufsize = cif->args_bufsize;
        goto too_many_args_error;
      }
    }
    arg_value_ptrs[cif->arity] = NULL;
    /* Perform the call. */
    errno = 0;
    ffi_call(&(cif->cif), address, (void *)retval_buffer, arg_value_ptrs);
    pcb->last_errno = errno;
    return_value    = ika_native_to_scheme_value_cast(cif->retval_type_id, retval_buffer, pcb);
  }
  pcb->frame_pointer = pcb->frame_base - wordsize;
  sk = pcb->next_k - vector_tag;
  if (system_continuation_tag != IK_REF(sk, disp_system_continuation_tag)) {
    ik_abort("%s: invalid system cont", __func__);
  }
  pcb->next_k       = IK_REF(sk, disp_system_continuation_next);
  pcb->system_stack = IK_REF(sk, disp_system_continuation_top);
  return return_value;

 too_many_args_error:
  ik_abort("exceeded maximum memory size (%d) reserved for callout arguments, too many arguments to callout", args_bufsize);
  return IK_VOID_OBJECT;
}


/** --------------------------------------------------------------------
 ** Callback: call a Scheme closure from C code.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_prepare_callback (ikptr s_data, ikpcb* pcb)
/* Prepare  a Libffi's  callback interface  associated  to a  CIF and  a
   Scheme function.   If successful return a  pointer object referencing
   the  callback, else  return false.   A failure  is probably  an error
   allocating memory with the system functions.

   S_DATA  must  be  a pair  whose  car  is  a  pointer object  of  type
   "ik_ffi_cif_t" and whose cdr is the Scheme function to be used by the
   callback. */
{
#if FFI_CLOSURES
  ffi_cif *                     cif;
  void *                        callable_pointer;
  ffi_closure *                 closure;
  ik_callback_locative *        callback_user_data;
  ffi_status                    st;
  cif     = IK_POINTER_DATA_VOIDP(IK_CAR(s_data));
  closure = ffi_closure_alloc(sizeof(ffi_closure), &callable_pointer);
#ifdef LIBFFI_ON_DARWIN
  { /* This is  needed on some flavors  of Darwin to  make the generated
       callback code executable. */
    long code_start = IK_ALIGN_TO_PREV_PAGE(callable_pointer);
    long code_end   = IK_ALIGN_TO_NEXT_PAGE(FFI_TRAMPOLINE_SIZE+(-1)+(long)callable_pointer);
    int rv = mprotect((void*)code_start, code_end - code_start, PROT_READ|PROT_WRITE|PROT_EXEC);
    if (rv)
      fprintf(stderr, "*** Vicare warning: error mprotecting callback code page\n");
  }
#endif
  callback_user_data = malloc(sizeof(ik_callback_locative));
  if (NULL == callback_user_data)
    return IK_FALSE_OBJECT;
  st = ffi_prep_closure_loc(closure, cif, generic_callback, callback_user_data, callable_pointer);
  if (FFI_OK != st) {
    free(callback_user_data);
    return IK_FALSE_OBJECT;
  }
  /* Prepend this callback to the linked list of callbacks registered in
     this process' PCB.  The garbage collector uses this information not
     to collect data still needed by the callbacks.  */
  callback_user_data->callable_pointer  = callable_pointer;
  callback_user_data->closure           = closure;
  callback_user_data->data              = s_data;
  callback_user_data->next              = pcb->callbacks;
  pcb->callbacks                        = callback_user_data;
  /* Return a pointer to callable code. */
  return ika_pointer_alloc(pcb, (ik_ulong)callable_pointer);
#else /* if FFI_CLOSURES */
  return IK_FALSE_OBJECT;
#endif /* if FFI_CLOSURES */
}
ikptr
ikrt_ffi_release_callback (ikptr s_callable_pointer, ikpcb * pcb)
{
  ik_callback_locative *  root;
  void *                  callable_pointer;
  root             = pcb->callbacks;
  callable_pointer = IK_POINTER_DATA_VOIDP(s_callable_pointer);
  if (root) {
    if (root->callable_pointer == callable_pointer) {
      pcb->callbacks = root->next;
      ffi_closure_free(root->closure);
      free(root);
      return IK_TRUE_OBJECT;
    } else {
      for (; root->next; root = root->next) {
        if (root->next->callable_pointer != callable_pointer)
          continue;
        else {
          ik_callback_locative *  this = root->next;
          root->next = root->next->next;
          ffi_closure_free(this->closure);
          free(this);
          return IK_TRUE_OBJECT;
        }
      }
      return IK_FALSE_OBJECT;
    }
  } else
    return IK_TRUE_OBJECT;
}
static void
generic_callback (ffi_cif * cif_, void * retval_buffer, void ** args, void * user_data)
/* Implement the  callback function used by all  the callbacks, whatever
   the CIF  and the Scheme function;  this function is  called by Libffi
   whenever   a    call   to   the   callable    pointer   returned   by
   "ikrt_prepare_callback()" is performed.

   CIF_ is a pointer to  a Libffi's call interface, which, under Vicare,
   is  also a pointer  of type  "ik_ffi_cif_t" referencing  the extended
   CIF.

   RETVAL_BUFFER is a pointer to  a memory block in which the callback's
   native return value must be stored.

   ARGS is a  pointer to an array of  pointers referencing memory blocks
   holding the native input arguments.  The arity of the callback can be
   retrieved from the CIF.

   USER_DATA is a pointer  to a structure of type "ik_callback_locative"
   whose  data  field   is  a  reference  to  the   S_DATA  argument  to
   "ikrt_prepare_callback()".

   Access the PCB through "ik_the_pcb()". */
{
  ik_ffi_cif_t  cif           = (ik_ffi_cif_t)cif_;
  ikptr         s_data        = ((ik_callback_locative*)user_data)->data;
  ikptr         s_proc        = IK_CDR(s_data);
  ikpcb *       pcb           = ik_the_pcb();
  int           i;
  ikptr         rv;
  pcb->frame_pointer = pcb->frame_base;
  pcb->root0 = &s_proc;
  { /* Push arguments on the Scheme stack. */
    for (i=0; i<cif->arity; ++i) {
      ikptr	s_value;
      s_value = ika_native_to_scheme_value_cast(cif->arg_type_ids[i], args[i], pcb);
      IK_REF(pcb->frame_pointer, -2*wordsize - i*wordsize) = s_value;
    }
  }
  pcb->root0 = NULL;
  /* Perform the call. */
  {
    ikptr         code_entry    = IK_REF(s_proc, off_closure_code);
    ikptr         code_ptr      = code_entry - off_code_data;
    rv = ik_exec_code(pcb, code_ptr, IK_FIX(-cif->arity), s_proc);
    /* Convert the Scheme return value to a native value. */
    scheme_to_native_value_cast(cif->retval_type_id, rv, retval_buffer);
  }
}


/** --------------------------------------------------------------------
 ** If libffi is not used.
 ** ----------------------------------------------------------------- */

#else

ikptr ikrt_ffi_prep_cif ()		{ return IK_FALSE_OBJECT; }
ikptr ikrt_ffi_call()			{ return IK_FALSE_OBJECT; }
ikptr ikrt_ffi_prepare_callback()	{ return IK_FALSE_OBJECT; }
ikptr ikrt_ffi_release_callback ()	{ return IK_FALSE_OBJECT; }
ikptr ikrt_has_ffi()			{ return IK_FALSE_OBJECT; }

#endif


/** --------------------------------------------------------------------
 ** Stack handling.
 ** ----------------------------------------------------------------- */

ikptr
ik_enter_c_function (ikpcb* pcb)
/* Call this function  whenever we enter a C function  that may invoke a
   Scheme callback. */
{
  ikptr		sk;
  seal_scheme_stack(pcb);
  sk = ik_unsafe_alloc(pcb, system_continuation_size) | vector_tag;
  IK_REF(sk, off_system_continuation_tag)  = system_continuation_tag;
  IK_REF(sk, off_system_continuation_top)  = pcb->system_stack;
  IK_REF(sk, off_system_continuation_next) = pcb->next_k;
  pcb->next_k = sk;
  return sk;
}
void
ik_leave_c_function (ikpcb * pcb, ikptr sk)
/* Call this  function whenever we exit  a C function that  may invoke a
   Scheme callback. */
{
  pcb->frame_pointer = pcb->frame_base - wordsize;
  sk = pcb->next_k - vector_tag;
  if (system_continuation_tag != IK_REF(sk, disp_system_continuation_tag)) {
    ik_abort("%s: invalid system cont", __func__);
  }
  pcb->next_k       = IK_REF(sk, disp_system_continuation_next);
  pcb->system_stack = IK_REF(sk, disp_system_continuation_top);
}
static ikptr
seal_scheme_stack(ikpcb* pcb)
/* FIXME Handle stack overflow.  (Abdulaziz Ghuloum) */
#ifndef DEBUG_FFI
{
  /*
   *      low memory
   *   |      .       |
   *   |      .       |
   *   |      .       |
   *   +--------------+
   *   |   underflow  |  <-- new frame pointer
   *   +--------------+
   *   | return point |  <-- old frame pointer, new frame base
   *   +--------------+
   *   |      .       |
   *   |      .       |
   *   |      .       |
   *   +--------------+
   *   |   underflow  |  <-- old frame base
   *   +--------------+
   *     high memory
   */
  ikptr		old_frame_base    = pcb->frame_base;
  ikptr		old_frame_pointer = pcb->frame_pointer;
  if ((old_frame_base - wordsize) != old_frame_pointer) {
    ikptr	underflow_handler = IK_REF(old_frame_base, -wordsize);
    ikcont *	old_next_kont     = (ikcont*) pcb->next_k;
    ikcont *	new_next_kont     = (ikcont*) ik_unsafe_alloc(pcb, sizeof(ikcont));
    new_next_kont->tag  = continuation_tag;
    new_next_kont->next = (ikptr) old_next_kont;
    new_next_kont->top  = old_frame_pointer;
    new_next_kont->size = old_frame_base - old_frame_pointer - wordsize;
    pcb->next_k         = vector_tag + (ikptr)new_next_kont;
    pcb->frame_base     = old_frame_pointer;
    pcb->frame_pointer  = pcb->frame_base - wordsize;
    IK_REF(pcb->frame_pointer, 0) = underflow_handler;
  }
  return IK_VOID_OBJECT;
}
#else
{
  ikptr		old_frame_base    = pcb->frame_base;
  ikptr		old_frame_pointer = pcb->frame_pointer;
  dump_stack(pcb, "BEFORE SEALING");
  fprintf(stderr, "old_frame_base=0x%016lx  old_frame_pointer=0x%016lx\n",
	  pcb->frame_base, pcb->frame_pointer);
  if ((old_frame_base - wordsize) != old_frame_pointer) {
    ikptr	underflow_handler = IK_REF(old_frame_base, -wordsize);
    ikcont *	old_next_kont     = (ikcont*) pcb->next_k;
    ikcont *	new_next_kont     = (ikcont*) ik_unsafe_alloc(pcb, sizeof(ikcont));
    new_next_kont->tag  = continuation_tag;
    new_next_kont->next = (ikptr) old_next_kont;
    new_next_kont->top  = old_frame_pointer;
    fprintf(stderr, "old_frame_pointer[0]=0x%016lx\n", IK_REF(old_frame_pointer, 0));
    new_next_kont->size = old_frame_base - old_frame_pointer - wordsize;
    fprintf(stderr, "new_next_kont->size=%ld\n", new_next_kont->size);
    pcb->next_k         = vector_tag + (ikptr)new_next_kont;
    pcb->frame_base     = old_frame_pointer;
    pcb->frame_pointer  = pcb->frame_base - wordsize;
    fprintf(stderr, "new_frame_base=0x%016lx  new_frame_pointer=0x%016lx\n",
	    pcb->frame_base, pcb->frame_pointer);
    fprintf(stderr, "underflow_handler=0x%016lx\n", underflow_handler);
    IK_REF(pcb->frame_pointer, 0) = underflow_handler;
  } else {
    fprintf(stderr, "already sealed\n");
  }
  dump_stack(pcb, "AFTER SEALING");
  return IK_VOID_OBJECT;
}
#endif

/* end of file */
