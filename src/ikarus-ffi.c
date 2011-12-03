/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

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
#include <stdlib.h>
#include <strings.h>

#undef DEBUG_FFI

#ifdef HACK_FFI
#  include <sys/mman.h>
#endif

/* Maximum size of memory allocated  to hold the native arguments values
   for callouts.  When exceeded the process terminates. */
#define ARGS_BUFFER_SIZE         1024

typedef void address_t ();

static void     scheme_to_native_value_cast  (ffi_type* t, ikptr nptr, ikptr p, void* r);
static ikptr    native_to_scheme_value_cast  (int n, void* p, ikpcb* pcb);
static ikptr    seal_scheme_stack            (ikpcb* pcb);

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
dump_stack(ikpcb* pcb, char* msg)
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
 ** Converting Scheme values to and from native values.
 ** ----------------------------------------------------------------- */

static void
scheme_to_native_value_cast (ffi_type* t, ikptr s_type_id, ikptr s_scheme_value, void * buffer)
/* Convert  the S_SCHEME_VALUE to  a native  value and  store it  in the
   block of memory  referenced by BUFFER; the type is  selected by the 8
   least significant bits of  the fixnum S_TYPE_ID.  S_SCHEME_VALUE must
   have been already validated.  BUFFER must be wide enough. */
{
  long  type_id = unfix(s_type_id);
  switch (type_id) {
  case TYPE_ID_VOID:
    return;

  case TYPE_ID_UINT8:
    *((uint8_t*)         buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_SINT8:
    *((int8_t*)          buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_UINT16:
    *((uint16_t*)        buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_SINT16:
    *((int16_t*)         buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_UINT32:
    *((uint32_t*)        buffer) = ik_integer_to_uint32(s_scheme_value);
    return;
  case TYPE_ID_SINT32:
    *((int32_t*)         buffer) = ik_integer_to_sint32(s_scheme_value);
    return;
  case TYPE_ID_UINT64:
    *((uint64_t*)        buffer) = ik_integer_to_uint64(s_scheme_value);
    return;
  case TYPE_ID_SINT64:
    *((int64_t*)         buffer) = ik_integer_to_sint64(s_scheme_value);
    return;

  case TYPE_ID_FLOAT:
    *((float*)          buffer) = flonum_data(s_scheme_value);
    return;
  case TYPE_ID_DOUBLE:
    *((double*)         buffer) = flonum_data(s_scheme_value);
    return;
  case TYPE_ID_POINTER:
    *((void**)          buffer) = VICARE_POINTER_DATA_VOIDP(s_scheme_value);
    return;

  case TYPE_ID_UCHAR:
    *((unsigned char*)  buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_SCHAR:
    *((char*)           buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_USHORT:
    *((unsigned short*) buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_SSHORT:
    *((signed short*)   buffer) = unfix(s_scheme_value);
    return;
  case TYPE_ID_UINT:
    *((unsigned int*)   buffer) = ik_integer_to_long(s_scheme_value);
    return;
  case TYPE_ID_SINT:
    *((signed int*)     buffer) = ik_integer_to_long(s_scheme_value);
    return;
  case TYPE_ID_ULONG:
    *((unsigned long*)  buffer) = ik_integer_to_long(s_scheme_value);
    return;
  case TYPE_ID_SLONG:
    *((signed long*)    buffer) = ik_integer_to_long(s_scheme_value);
    return;

  default:
    fprintf(stderr, "*** Vicare FFI error: %s: invalid argument type selector %ld", __func__, type_id);
    exit(EXIT_FAILURE);
  }
}
static ikptr
native_to_scheme_value_cast (int type_id, void * buffer, ikpcb* pcb)
/* Convert the native  value in BUFFER to a Scheme  value and return the
   Scheme value; the type is selected by the 8 least significant bits of
   TYPE_ID. */
{
  switch (type_id) {
  case TYPE_ID_VOID:
    return void_object;

  case TYPE_ID_UINT8:
    return fix(*((uint8_t*)buffer));
  case TYPE_ID_SINT8:
    return fix(*(( int8_t*)buffer));
  case TYPE_ID_UINT16:
    return fix(*((uint16_t*)buffer));
  case TYPE_ID_SINT16:
    return fix(*(( int16_t*)buffer));
  case TYPE_ID_UINT32:
    return ik_integer_from_unsigned_long        (*((uint32_t*)buffer), pcb);
  case TYPE_ID_SINT32:
    return ik_integer_from_long                 (*((signed long*)  buffer), pcb);
  case TYPE_ID_UINT64:
    return ik_integer_from_unsigned_long_long   (*((unsigned long long*)buffer), pcb);
  case TYPE_ID_SINT64:
    return ik_integer_from_long_long            (*((signed long long*)  buffer), pcb);

  case TYPE_ID_FLOAT:
    return ik_flonum_from_double                (*((float*)             buffer), pcb);
  case TYPE_ID_DOUBLE:
    return ik_flonum_from_double                (*((double*)            buffer), pcb);
  case TYPE_ID_POINTER:
    return ikrt_pointer_alloc                   ((long)*((void**)       buffer), pcb);

  case TYPE_ID_UCHAR:
    return ik_integer_from_unsigned_long        (*((unsigned char*)     buffer), pcb);
  case TYPE_ID_SCHAR:
    return ik_integer_from_long                 (*((signed char*)       buffer), pcb);
  case TYPE_ID_USHORT:
    return ik_integer_from_unsigned_long        (*((unsigned short*)    buffer), pcb);
  case TYPE_ID_SSHORT:
    return ik_integer_from_long                 (*((signed short*)      buffer), pcb);
  case TYPE_ID_UINT:
    return ik_integer_from_unsigned_long        (*((unsigned int*)      buffer), pcb);
  case TYPE_ID_SINT:
    return ik_integer_from_long                 (*((signed int*)        buffer), pcb);
  case TYPE_ID_ULONG:
    return ik_integer_from_unsigned_long        (*((unsigned long*)     buffer), pcb);
  case TYPE_ID_SLONG:
    return ik_integer_from_long                 (*((signed long*)       buffer), pcb);

  default:
    fprintf(stderr, "*** Vicare error: %s: invalid arg %d", __func__, type_id);
    exit(EXIT_FAILURE);
  }
}

#if 0 /* replaced by the use of the array "the_ffi_types_array" */
static ffi_type *
scheme_to_ffi_type_cast (ikptr s_type_id)
{
  long  type_id = unfix(s_type_id);
  switch (type_id) {
  case  1: return &ffi_type_void;
  case  2: return &ffi_type_uint8;
  case  3: return &ffi_type_sint8;
  case  4: return &ffi_type_uint16;
  case  5: return &ffi_type_sint16;
  case  6: return &ffi_type_uint32;
  case  7: return &ffi_type_sint32;
  case  8: return (sizeof(long)==4)? &ffi_type_uint32 : &ffi_type_uint64;
  case  9: return (sizeof(long)==4)? &ffi_type_sint32 : &ffi_type_sint64;
  case 10: return &ffi_type_uint64;
  case 11: return &ffi_type_sint64;
  case 12: return &ffi_type_float;
  case 13: return &ffi_type_double;
  case 14: return &ffi_type_pointer;
  case 15: return &ffi_type_uchar;
  case 16: return &ffi_type_schar;
  case 17: return &ffi_type_ushort;
  case 18: return &ffi_type_sshort;
  case 19: return &ffi_type_uint;
  case 20: return &ffi_type_sint;
  case 21: return &ffi_type_ulong;
  case 22: return &ffi_type_slong;
  default:
    fprintf(stderr, "*** Vicare FFI error: %s: invalid arg %ld", __func__, type_id);
    exit(EXIT_FAILURE);
  }
}
#endif


/** --------------------------------------------------------------------
 ** Call InterFace (CIF) preparation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_prep_cif (ikptr s_retval_type, ikptr s_args_types, ikpcb* pcb)
/* S_RETVAL_TYPE  must be  a fixnum  selecting  the type  of the  return
   value.   S_ARGS_TYPES   must  reference  a   vector  holding  fixnums
   selecting the types of the arguments. */
{
  ffi_cif*      cif       = alloc(sizeof(ffi_cif), 1);
  long          arity     = VICARE_VECTOR_LENGTH(s_args_types);
  ffi_type**    arg_types = alloc(sizeof(ffi_type*), 1+arity);
  int           i;
  for (i=0; i<arity; i++) {
    arg_types[i] = the_ffi_types_array[unfix(VICARE_VECTOR_REF(s_args_types, i))];
    /* arg_types[i] = scheme_to_ffi_type_cast(argt); */
  }
  arg_types[arity] = NULL;
  ffi_type* rtype = the_ffi_types_array[unfix(s_retval_type)];
  /* ffi_type* rtype = scheme_to_ffi_type_cast(s_retval_type); */
  ffi_status rv = ffi_prep_cif(cif, FFI_DEFAULT_ABI, arity, rtype, arg_types);
  if (FFI_OK == rv) {
    ikptr r = ik_safe_alloc(pcb, pointer_size);
    ref(r, 0) = pointer_tag;
    ref(r, wordsize) = (ikptr)cif;
    return r + vector_tag;
  } else {
    return false_object;
  }
}


static ikptr
seal_scheme_stack(ikpcb* pcb)
/* FIXME: handle stack overflow */
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
#ifdef DEBUG_FFI
  dump_stack(pcb, "BEFORE SEALING");
  fprintf(stderr, "old base=0x%016lx  fp=0x%016lx\n", pcb->frame_base,
          pcb->frame_pointer);
#endif
  if ((frame_base - wordsize) != frame_pointer) {
    ikptr underflow_handler = ref(frame_base, -wordsize);
    cont* k = (cont*) pcb->next_k;
    cont* nk = (cont*) ik_unsafe_alloc(pcb, sizeof(cont));
    nk->tag = continuation_tag;
    nk->next = (ikptr) k;
    nk->top = frame_pointer;
#ifdef DEBUG_FFI
    fprintf(stderr, "rp=0x%016lx\n", ref(frame_pointer, 0));
#endif
    nk->size = frame_base - frame_pointer - wordsize;
#ifdef DEBUG_FFI
    fprintf(stderr, "frame size=%ld\n", nk->size);
#endif
    pcb->next_k        = vector_tag + (ikptr)nk;
    pcb->frame_base    = frame_pointer;
    pcb->frame_pointer = pcb->frame_base - wordsize;
#ifdef DEBUG_FFI
    fprintf(stderr, "new base=0x%016lx  fp=0x%016lx\n", pcb->frame_base,
            pcb->frame_pointer);
    fprintf(stderr, "uf=0x%016lx\n", underflow_handler);
#endif
    ref(pcb->frame_pointer, 0) = underflow_handler;
  } else {
#ifdef DEBUG_FFI
    fprintf(stderr, "already sealed\n");
#endif
  }
#ifdef DEBUG_FFI
  dump_stack(pcb, "AFTER SEALING");
#endif
  return void_object;
}


/** --------------------------------------------------------------------
 ** Callout: call a C function from Scheme code.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ffi_call (ikptr data, ikptr args, ikpcb * pcb)
{
  ikptr         return_value;
  seal_scheme_stack(pcb);
  ikptr sk = ik_unsafe_alloc(pcb, system_continuation_size);
  ref(sk, disp_system_continuation_tag)  = system_continuation_tag;
  ref(sk, disp_system_continuation_top)  = pcb->system_stack;
  ref(sk, disp_system_continuation_next) = pcb->next_k;
  pcb->next_k = sk + vector_tag;
  {
    /* Unpack data describing the foreign function.

       CIFPTR:  pointer object  to a  malloc-ed data  structure  of type
       "ffi_cif" describing the callout interface.

       FUNPTR: pointer  object representing  the address of  the foreign
       function to call.

       ARG_IDS: Scheme vector  holding fixnum identifiers specifying the
       type of the arguments.

       RTYPE: fixnum identifier specifying the type of the return value.
    */
    ikptr       cifptr  = VICARE_VECTOR_REF(data, 0);
    ikptr       funptr  = VICARE_VECTOR_REF(data, 1);
    ikptr       arg_ids = VICARE_VECTOR_REF(data, 2);
    ikptr       rtype   = VICARE_VECTOR_REF(data, 3);
    /* Extract values describing the foreign function. */
    ffi_cif *   cif     = VICARE_POINTER_DATA_VOIDP(cifptr);
    address_t * address = VICARE_POINTER_DATA_VOIDP(funptr);
    int         arity   = VICARE_VECTOR_LENGTH(args);
    /* Prepare  memory   to  hold  native   values  representing  Scheme
       arguments and the return value */
    uint8_t     args_buffer[ARGS_BUFFER_SIZE];
    uint8_t *   arg_next = &(args_buffer[0]);
    uint8_t *   arg_end  = arg_next + ARGS_BUFFER_SIZE;
    void *      arg_value_ptrs[1+arity];
    uint8_t     retval_buffer[cif->rtype->size];
    /* Fill ARG_VALUE_PTRS  with pointers  to memory blocks  holding the
       native argument values. */
    int  i;
    for (i=0; i<arity; i++) {
      ffi_type *  type   = cif->arg_types[i];
      ikptr       id     = VICARE_VECTOR_REF(arg_ids, i);
      ikptr       value  = VICARE_VECTOR_REF(args,    i);
      arg_value_ptrs[i] = arg_next;
      scheme_to_native_value_cast(type, id, value, arg_next);
      arg_next += type->size;
      if (arg_end <= arg_next)
        goto too_many_args_error;
    }
    arg_value_ptrs[arity] = NULL;
    /* Perform the call. */
    errno = 0;
    ffi_call(cif, address, (void *)retval_buffer, arg_value_ptrs);
    pcb->last_errno = errno;
    return_value    = native_to_scheme_value_cast(unfix(rtype), retval_buffer, pcb);
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
          ARGS_BUFFER_SIZE);
  exit(EXIT_FAILURE);
}


/** --------------------------------------------------------------------
 ** Callback: call a Scheme closure from C code.
 ** ----------------------------------------------------------------- */

static void
generic_callback (ffi_cif *cif, void *ret, void **args, void *user_data)
{
  /* convert args according to cif to scheme values */
  /* call into scheme, get the return value */
  /* convert the return value to C */
  /* put the C return value in *ret */
  /* done */
  ikptr data = ((callback_locative*)user_data)->data;
  ikptr proc   = ref(data, off_vector_data + 1 * wordsize);
  ikptr argtypes_conv = ref(data, off_vector_data + 2 * wordsize);
  ikptr rtype_conv = ref(data, off_vector_data + 3 * wordsize);
  int n = unfix(ref(argtypes_conv, off_vector_length));

  ikpcb* pcb = the_pcb;
  ikptr code_entry = ref(proc, off_closure_code);
  ikptr code_ptr = code_entry - off_code_data;

  pcb->frame_pointer = pcb->frame_base;
  int i;
  for(i = 0; i < n; i++){
    ikptr argt = ref(argtypes_conv, off_vector_data + i*wordsize);
    void* argp = args[i];
    ref(pcb->frame_pointer, -2*wordsize - i*wordsize) =
      native_to_scheme_value_cast(unfix(argt), argp, pcb);
  }
  ikptr rv = ik_exec_code(pcb, code_ptr, fix(-n), proc);
#ifdef DEBUG_FFI
  fprintf(stderr, "and back with rv=0x%016lx!\n", rv);
#endif
  scheme_to_native_value_cast(cif->rtype, rtype_conv, rv, ret);
  return;
}
ikptr
ikrt_prepare_callback(ikptr data, ikpcb* pcb)
{
#if FFI_CLOSURES
  ikptr cifptr = ref(data, off_vector_data + 0 * wordsize);
  void* codeloc;
  ffi_closure* closure = ffi_closure_alloc(sizeof(ffi_closure), &codeloc);

#ifdef HACK_FFI
  {
    long code_start = align_to_prev_page(codeloc);
    long code_end =
      align_to_next_page(FFI_TRAMPOLINE_SIZE+(-1)+(long)codeloc);
    int rv = mprotect((void*)code_start, code_end - code_start,
                      PROT_READ|PROT_WRITE|PROT_EXEC);
    if(rv) {
      fprintf(stderr, "Error mprotecting code page!\n");
    }
  }
#endif

  ffi_cif* cif = (ffi_cif*) ref(cifptr, off_pointer_data);

  callback_locative* loc = malloc(sizeof(callback_locative));
  if(!loc) {
    fprintf(stderr, "ERROR: ikarus malloc error\n");
    exit(EXIT_FAILURE);
  }

  ffi_status st =
    ffi_prep_closure_loc(closure, cif, generic_callback, loc, codeloc);

  if (st != FFI_OK) {
    free(loc);
    return false_object;
  }

  loc->data = data;
  loc->next = pcb->callbacks;
  pcb->callbacks = loc;

  ikptr p = ik_safe_alloc(pcb, pointer_size);
  ref(p, 0) = pointer_tag;
  ref(p, wordsize) = (ikptr) codeloc;
  return p+vector_tag;
#else /* if FFI_CLOSURES */
  return false_object;
#endif /* if FFI_CLOSURES */
}
ikptr
ikrt_call_back(ikptr proc, ikpcb* pcb)
{
  seal_scheme_stack(pcb);

  ikptr sk = ik_unsafe_alloc(pcb, system_continuation_size);
  ref(sk, 0) = system_continuation_tag;
  ref(sk, disp_system_continuation_top) = pcb->system_stack;
  ref(sk, disp_system_continuation_next) = pcb->next_k;
  pcb->next_k = sk + vector_tag;
  ikptr entry_point = ref(proc, off_closure_code);
#ifdef DEBUG_FFI
  fprintf(stderr, "system_stack = 0x%016lx\n", pcb->system_stack);
#endif
  ikptr code_ptr = entry_point - off_code_data;
  pcb->frame_pointer = pcb->frame_base;
  ikptr rv = ik_exec_code(pcb, code_ptr, 0, proc);
#ifdef DEBUG_FFI
  fprintf(stderr, "system_stack = 0x%016lx\n", pcb->system_stack);
#endif
#ifdef DEBUG_FFI
  fprintf(stderr, "rv=0x%016lx\n", rv);
#endif
  sk = pcb->next_k - vector_tag;
  if (ref(sk, 0) != system_continuation_tag) {
    fprintf(stderr, "vicare internal error: invalid system cont\n");
    exit(EXIT_FAILURE);
  }
  pcb->next_k = ref(sk, disp_system_continuation_next);
  ref(sk, disp_system_continuation_next) = pcb->next_k;
  pcb->system_stack = ref(sk, disp_system_continuation_top);
  pcb->frame_pointer = pcb->frame_base - wordsize;
#ifdef DEBUG_FFI
  fprintf(stderr, "rp=0x%016lx\n", ref(pcb->frame_pointer, 0));
#endif
  return rv;
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

/* end of file */
