/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        This module defines the interface between Vicare and Libffi.

        This  file is  without  license notice  in  the original  Ikarus
        distribution  for no  reason I  can know  (Marco Maggi;  Nov 26,
        2011).

  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
  Modified by Marco Maggi <marco.maggi-ipsu@poste.it>

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
#if ENABLE_LIBFFI
#include <ffi.h>

#undef DEBUG_FFI

#ifdef LIBFFI_ON_DARWIN
#  include <sys/mman.h>         /* for "mprotect()" */
#endif


/** --------------------------------------------------------------------
 ** Constants and variables.
 ** ----------------------------------------------------------------- */

extern ikpcb* the_pcb;

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
  sizeof(unsigned long),        /* 18 */
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
static ikptr    native_to_scheme_value_cast  (type_id_t type_id, void * buffer, ikpcb* pcb);
static ikptr    seal_scheme_stack            (ikpcb* pcb);
static void     generic_callback             (ffi_cif *cif, void *ret, void **args, void *user_data);


/** --------------------------------------------------------------------
 ** Helpers and miscellaneous small functions.
 ** ----------------------------------------------------------------- */

static void*
alloc (size_t n, long m)
{
  void  * ptr = calloc(n, m);
  if (ptr)
    return ptr;
  else {
    fprintf(stderr, "*** Vicare error: failed memory allocation with calloc(%u, %ld)\n", n, m);
    exit(EXIT_FAILURE);
  }
}
ikptr
ikrt_has_ffi (void)
{
  return true_object;
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
    fprintf(stderr, "*0x%016lx = 0x%016lx\n", p, ref(p, 0));
    p += wordsize;
  }
}
#endif


/** --------------------------------------------------------------------
 ** Call InterFace (CIF) preparation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_prep_cif (ikptr s_retval_type_id, ikptr s_arg_type_ids, ikpcb* pcb)
/* Wrapper  for   Libffi's  "ffi_prep_cif()";  prepare   a  Libffi  call
   interface (CIF) building an  appropriate data structure, whose memory
   is obtained  by "calloc()".  Return a pointer  object referencing the
   CIF structure.

   The generated CIR can be used for both callouts and callbacks.

   S_RETVAL_TYPE_ID must  be a fixnum  in the set  "type_id_t" selecting
   the type of the return value.  S_ARG_TYPE_IDS must reference a vector
   holding fixnums in  the range "type_id_t" selecting the  types of the
   arguments. */
{
  unsigned      arity = (unsigned)VICARE_VECTOR_LENGTH(s_arg_type_ids);
  ik_ffi_cif_t  cif   = alloc(IK_FFI_CIF_SIZEOF(arity), 1);
  ffi_status    rv;
  int           i;
  cif->arg_types      = IK_FFI_CIF_ARG_TYPES_PTR(cif, arity);
  cif->arg_type_ids   = IK_FFI_CIF_ARG_TYPE_IDS_PTR(cif, arity);
  cif->arity          = arity;
  cif->retval_type_id = unfix(s_retval_type_id);
  cif->retval_type    = the_ffi_types_array[cif->retval_type_id];
  cif->args_bufsize   = 0;
  for (i=0; i<arity; i++) {
    type_id_t   id       =  unfix(VICARE_VECTOR_REF(s_arg_type_ids, i));
    cif->args_bufsize    += the_ffi_type_sizes[id];
    cif->arg_type_ids[i] =  id;
    cif->arg_types[i]    =  the_ffi_types_array[id];
  }
  cif->arg_types[arity] = NULL;
  rv = ffi_prep_cif(&(cif->cif), FFI_DEFAULT_ABI, arity, cif->retval_type, cif->arg_types);
  return (FFI_OK == rv)? ik_pointer_alloc((unsigned long)cif, pcb) : false_object;
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

  case TYPE_ID_UINT8:   *((uint8_t*)         buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_SINT8:   *((int8_t*)          buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_UINT16:  *((uint16_t*)        buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_SINT16:  *((int16_t*)         buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_UINT32:  *((uint32_t*)        buffer) = ik_integer_to_uint32(s_scheme_value); return;
  case TYPE_ID_SINT32:  *((int32_t*)         buffer) = ik_integer_to_sint32(s_scheme_value); return;
  case TYPE_ID_UINT64:  *((uint64_t*)        buffer) = ik_integer_to_uint64(s_scheme_value); return;
  case TYPE_ID_SINT64:  *((int64_t*)         buffer) = ik_integer_to_sint64(s_scheme_value); return;

  case TYPE_ID_FLOAT:   *((float*)          buffer) = flonum_data(s_scheme_value); return;
  case TYPE_ID_DOUBLE:  *((double*)         buffer) = flonum_data(s_scheme_value); return;
  case TYPE_ID_POINTER: *((void**)          buffer) = VICARE_POINTER_DATA_VOIDP(s_scheme_value); return;

  case TYPE_ID_UCHAR:   *((unsigned char*)  buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_SCHAR:   *((char*)           buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_USHORT:  *((unsigned short*) buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_SSHORT:  *((signed short*)   buffer) = unfix(s_scheme_value); return;
  case TYPE_ID_UINT:    *((unsigned int*)   buffer) = ik_integer_to_long(s_scheme_value); return;
  case TYPE_ID_SINT:    *((signed int*)     buffer) = ik_integer_to_long(s_scheme_value); return;
  case TYPE_ID_ULONG:   *((unsigned long*)  buffer) = ik_integer_to_long(s_scheme_value); return;
  case TYPE_ID_SLONG:   *((signed long*)    buffer) = ik_integer_to_long(s_scheme_value); return;

  default:
    fprintf(stderr, "*** Vicare FFI error: %s: invalid argument type selector %d",
            __func__, (int)type_id);
    exit(EXIT_FAILURE);
  }
}
static ikptr
native_to_scheme_value_cast (type_id_t type_id, void * buffer, ikpcb* pcb)
/* Convert the native value stored  in the block of memory referenced by
   BUFFER to  a Scheme value  and return the  Scheme value; the  type is
   selected by TYPE_ID. */
{
  switch (type_id) {
  case TYPE_ID_VOID:    return void_object;

  case TYPE_ID_UINT8:   return fix(*((uint8_t*) buffer));
  case TYPE_ID_SINT8:   return fix(*(( int8_t*) buffer));
  case TYPE_ID_UINT16:  return fix(*((uint16_t*)buffer));
  case TYPE_ID_SINT16:  return fix(*(( int16_t*)buffer));
  case TYPE_ID_UINT32:  return ik_integer_from_unsigned_long     (*((uint32_t*)          buffer), pcb);
  case TYPE_ID_SINT32:  return ik_integer_from_long              (*((signed long*)       buffer), pcb);
  case TYPE_ID_UINT64:  return ik_integer_from_unsigned_long_long(*((unsigned long long*)buffer), pcb);
  case TYPE_ID_SINT64:  return ik_integer_from_long_long         (*((signed long long*)  buffer), pcb);

  case TYPE_ID_FLOAT:   return ik_flonum_from_double             (*((float*)             buffer), pcb);
  case TYPE_ID_DOUBLE:  return ik_flonum_from_double             (*((double*)            buffer), pcb);
  case TYPE_ID_POINTER: return ik_pointer_alloc                  ((long)*((void**)       buffer), pcb);

  case TYPE_ID_UCHAR:   return ik_integer_from_unsigned_long     (*((unsigned char*)     buffer), pcb);
  case TYPE_ID_SCHAR:   return ik_integer_from_long              (*((signed char*)       buffer), pcb);
  case TYPE_ID_USHORT:  return ik_integer_from_unsigned_long     (*((unsigned short*)    buffer), pcb);
  case TYPE_ID_SSHORT:  return ik_integer_from_long              (*((signed short*)      buffer), pcb);
  case TYPE_ID_UINT:    return ik_integer_from_unsigned_long     (*((unsigned int*)      buffer), pcb);
  case TYPE_ID_SINT:    return ik_integer_from_long              (*((signed int*)        buffer), pcb);
  case TYPE_ID_ULONG:   return ik_integer_from_unsigned_long     (*((unsigned long*)     buffer), pcb);
  case TYPE_ID_SLONG:   return ik_integer_from_long              (*((signed long*)       buffer), pcb);

  default:
    fprintf(stderr, "*** Vicare error: %s: invalid arg %d", __func__, (int)type_id);
    exit(EXIT_FAILURE);
  }
}


static ikptr
seal_scheme_stack(ikpcb* pcb)
/* FIXME: handle stack overflow */
#ifndef DEBUG_FFI
{
  /*
    |              |
    |              |
    |              |
    |              |
    +--------------+
    |   underflow  |  <--------- new frame pointer
    +--------------+
    | return point |  <--------- old frame pointer, new frame base
    +--------------+
    |      .       |
    |      .       |
    |      .       |
    |              |
    +--------------+
    |   underflow  |  <--------- old frame base
    +--------------+
  */
  ikptr frame_base    = pcb->frame_base;
  ikptr frame_pointer = pcb->frame_pointer;
  if ((frame_base - wordsize) != frame_pointer) {
    ikptr underflow_handler = ref(frame_base, -wordsize);
    cont* k  = (cont*) pcb->next_k;
    cont* nk = (cont*) ik_unsafe_alloc(pcb, sizeof(cont));
    nk->tag  = continuation_tag;
    nk->next = (ikptr) k;
    nk->top  = frame_pointer;
    nk->size = frame_base - frame_pointer - wordsize;
    pcb->next_k        = vector_tag + (ikptr)nk;
    pcb->frame_base    = frame_pointer;
    pcb->frame_pointer = pcb->frame_base - wordsize;
    ref(pcb->frame_pointer, 0) = underflow_handler;
  }
  return void_object;
}
#else
{
  ikptr frame_base    = pcb->frame_base;
  ikptr frame_pointer = pcb->frame_pointer;
  dump_stack(pcb, "BEFORE SEALING");
  fprintf(stderr, "old base=0x%016lx  fp=0x%016lx\n", pcb->frame_base, pcb->frame_pointer);
  if ((frame_base - wordsize) != frame_pointer) {
    ikptr underflow_handler = ref(frame_base, -wordsize);
    cont* k = (cont*) pcb->next_k;
    cont* nk = (cont*) ik_unsafe_alloc(pcb, sizeof(cont));
    nk->tag = continuation_tag;
    nk->next = (ikptr) k;
    nk->top = frame_pointer;
    fprintf(stderr, "rp=0x%016lx\n", ref(frame_pointer, 0));
    nk->size = frame_base - frame_pointer - wordsize;
    fprintf(stderr, "frame size=%ld\n", nk->size);
    pcb->next_k        = vector_tag + (ikptr)nk;
    pcb->frame_base    = frame_pointer;
    pcb->frame_pointer = pcb->frame_base - wordsize;
    fprintf(stderr, "new base=0x%016lx  fp=0x%016lx\n", pcb->frame_base, pcb->frame_pointer);
    fprintf(stderr, "uf=0x%016lx\n", underflow_handler);
    ref(pcb->frame_pointer, 0) = underflow_handler;
  } else {
    fprintf(stderr, "already sealed\n");
  }
  dump_stack(pcb, "AFTER SEALING");
  return void_object;
}
#endif


/** --------------------------------------------------------------------
 ** Callout: call a C function from Scheme code.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_call (ikptr s_data, ikptr s_args, ikpcb * pcb)
/* Perform a callout and return the return value of the callout.

   S_DATA  must be  a vector  holding the  specification of  the foreign
   function to call and its CIF:

      S_DATA[0]: pointer  object to a  malloc-ed data structure  of type
      "ik_ffi_cif_stru_t" describing the callout interface.

      S_DATA[1]: pointer object representing  the address of the foreign
      function to call.

      S_DATA[2]:  Scheme  vector   holding  fixnum  identifiers  in  the
      enumeration "type_id_t" specifying the type of the arguments.

      S_DATA[3]:  fixnum  identifier   in  the  enumeration  "type_id_t"
      specifying the type of the return value.

   S_ARGS must be a vector holding the call arguments.  */
{
  ikptr         return_value;
  ikptr         sk;
  size_t        args_bufsize;
  seal_scheme_stack(pcb);
  sk = ik_unsafe_alloc(pcb, system_continuation_size);
  ref(sk, disp_system_continuation_tag)  = system_continuation_tag;
  ref(sk, disp_system_continuation_top)  = pcb->system_stack;
  ref(sk, disp_system_continuation_next) = pcb->next_k;
  pcb->next_k = sk + vector_tag;
  {
    ik_ffi_cif_t  cif     = VICARE_POINTER_DATA_VOIDP(VICARE_VECTOR_REF(s_data, 0));
    address_t *   address = VICARE_POINTER_DATA_VOIDP(VICARE_VECTOR_REF(s_data, 1));
    /* Prepare  memory   to  hold  native   values  representing  Scheme
       arguments and the return value */
    uint8_t     args_buffer[cif->args_bufsize];
    uint8_t *   arg_next = &(args_buffer[0]);
    uint8_t *   arg_end  = arg_next + cif->args_bufsize;
    void *      arg_value_ptrs[1+cif->arity];
    uint8_t     retval_buffer[cif->retval_type->size];
    /* Fill ARG_VALUE_PTRS  with pointers  to memory blocks  holding the
       native argument values. */
    int  i;
    for (i=0; i<cif->arity; i++) {
      ikptr  value = VICARE_VECTOR_REF(s_args, i);
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
    return_value    = native_to_scheme_value_cast(cif->retval_type_id, retval_buffer, pcb);
  }
  pcb->frame_pointer = pcb->frame_base - wordsize;
  sk = pcb->next_k - vector_tag;
  if (system_continuation_tag != ref(sk, disp_system_continuation_tag)) {
    fprintf(stderr, "vicare internal error: invalid system cont\n");
    exit(EXIT_FAILURE);
  }
  pcb->next_k       = ref(sk, disp_system_continuation_next);
  pcb->system_stack = ref(sk, disp_system_continuation_top);
  return return_value;

 too_many_args_error:
  fprintf(stderr, "*** Vicare error: exceeded maximum memory size (%d)\n\
*** reserved for callout arguments, too many arguments to callout\n",
          args_bufsize);
  exit(EXIT_FAILURE);
}


/** --------------------------------------------------------------------
 ** Callback: call a Scheme closure from C code.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_prepare_callback (ikptr s_data, ikpcb* pcb)
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
  cif     = VICARE_POINTER_DATA_VOIDP(VICARE_CAR(s_data));
  closure = ffi_closure_alloc(sizeof(ffi_closure), &callable_pointer);
#ifdef LIBFFI_ON_DARWIN
  { /* This is  needed on some flavors  of Darwin to  make the generated
       callback code executable. */
    long code_start = align_to_prev_page(callable_pointer);
    long code_end   = align_to_next_page(FFI_TRAMPOLINE_SIZE+(-1)+(long)callable_pointer);
    int rv = mprotect((void*)code_start, code_end - code_start, PROT_READ|PROT_WRITE|PROT_EXEC);
    if (rv)
      fprintf(stderr, "*** Vicare warning: error mprotecting callback code page\n");
  }
#endif
  callback_user_data = malloc(sizeof(ik_callback_locative));
  if (NULL == callback_user_data)
    return false_object;
  st = ffi_prep_closure_loc(closure, cif, generic_callback, callback_user_data, callable_pointer);
  if (FFI_OK != st) {
    free(callback_user_data);
    return false_object;
  }
  /* Prepend this callback to the linked list of callbacks registered in
     this process' PCB.  The garbage collector uses this information not
     to collect data still needed by the callbacks.  */
  callback_user_data->data = s_data;
  callback_user_data->next = pcb->callbacks;
  pcb->callbacks           = callback_user_data;
  /* Return a pointer to callable code. */
  return ik_pointer_alloc((unsigned long)callable_pointer, pcb);
#else /* if FFI_CLOSURES */
  return false_object;
#endif /* if FFI_CLOSURES */
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

   Access the PCB through the global variable "the_pcb". */
{
  ik_ffi_cif_t  cif           = (ik_ffi_cif_t)cif_;
  ikptr         s_data        = ((ik_callback_locative*)user_data)->data;
  ikptr         s_proc        = VICARE_CDR(s_data);
  ikpcb *       pcb           = the_pcb;
  ikptr         code_entry    = ref(s_proc, off_closure_code);
  ikptr         code_ptr      = code_entry - off_code_data;
  int           i;
  ikptr         rv;
  pcb->frame_pointer = pcb->frame_base;
  /* Push arguments on the Scheme stack. */
  for (i=0; i<cif->arity; ++i) {
    ref(pcb->frame_pointer, -2*wordsize - i*wordsize) =
      native_to_scheme_value_cast(cif->arg_type_ids[i], args[i], pcb);
  }
  /* Perform the call. */
  rv = ik_exec_code(pcb, code_ptr, fix(-cif->arity), s_proc);
  /* Convert the Scheme return value to a native value. */
  scheme_to_native_value_cast(cif->retval_type_id, rv, retval_buffer);
  return;
}


/** --------------------------------------------------------------------
 ** If libffi is not used.
 ** ----------------------------------------------------------------- */

#else

ikptr ikrt_ffi_prep_cif()     { return false_object; }
ikptr ikrt_ffi_call()         { return false_object; }
ikptr ikrt_prepare_callback() { return false_object; }
ikptr ikrt_has_ffi()          { return false_object; }

#endif

/* end of file */
