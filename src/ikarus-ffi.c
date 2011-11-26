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
#if ENABLE_LIBFFI

#include <ffi.h>
#include <stdlib.h>
#include <strings.h>

#undef DEBUG_FFI

#ifdef HACK_FFI
#  include <sys/mman.h>
#endif

static ffi_type *  scheme_to_ffi_type_cast         (ikptr nptr);
static void        scheme_to_ffi_value_cast        (ffi_type* t, ikptr nptr, ikptr p, void* r);
static ffi_type *  scheme_to_ffi_record_type_cast  (ikptr vec);
static void        scheme_to_ffi_record_value_cast (ffi_type* t, ikptr nptr, ikptr p, void* r);


/** --------------------------------------------------------------------
 ** Helpers.
 ** ----------------------------------------------------------------- */

static void*
alloc (size_t n, int m)
{
  void* x = calloc(n, m);
  if (x)
    return x;
  else {
    fprintf(stderr, "*** Vicare error: failed memory allocation with calloc(%u, %d)\n", n, m);
    exit(EXIT_FAILURE);
  }
}
static void *
alloc_room_for_type (ffi_type* t)
{
  return alloc(t->size, 1);
}


/** --------------------------------------------------------------------
 ** Stuff.
 ** ----------------------------------------------------------------- */

static void
scheme_to_ffi_value_cast (ffi_type* t, ikptr nptr, ikptr p, void* r)
{
  if (tagof(nptr) == vector_tag) {
    scheme_to_ffi_record_value_cast(t, nptr, p, r);
  } else if (is_fixnum(nptr)) {
    long n = unfix(nptr);
    switch (n & 0xF) {
    case  1: {  return; }
    case  2: // ffi_type_uint8;
    case  3:
      { *((char*)r) = extract_num(p); return; }
    case  4: // ffi_type_uint16;
    case  5:
      { *((short*)r) = extract_num(p); return; }
    case  6: //  ffi_type_uint32;
    case  7:
      { *((int*)r) = extract_num(p); return; }
    case  8: // ffi_type_uint64;
    case  9:
      { *((long*)r) = extract_num(p); return; }
    case 10:
    case 11:
      { *((long long*)r) = extract_num_longlong(p); return; }
    case 12: //return &ffi_type_float;
      { *((float*)r) = flonum_data(p); return; }
    case 13: //return &ffi_type_double;
      { *((double*)r) = flonum_data(p); return; }
    case 14: //return &ffi_type_pointer;
      { *((void**)r) = (void*)ref(p, off_pointer_data); return; }
    default:
      fprintf(stderr, "*** Vicare FFI error: %s: invalid arg %ld", __func__, n);
      exit(EXIT_FAILURE);
    }
  } else {
    fprintf(stderr, "*** Vicare FFI error: %s: invalid type 0x%016lx\n", __func__, nptr);
    exit(EXIT_FAILURE);
  }
}
static ffi_type *
scheme_to_ffi_type_cast (ikptr nptr)
{
  if (tagof(nptr) == vector_tag) {
    return scheme_to_ffi_record_type_cast(nptr);
  } else if (is_fixnum(nptr)) {
    long n = unfix(nptr);
    switch (n & 0xF) {
    case  1: return &ffi_type_void;
    case  2: return &ffi_type_uint8;
    case  3: return &ffi_type_sint8;
    case  4: return &ffi_type_uint16;
    case  5: return &ffi_type_sint16;
    case  6: return &ffi_type_uint32;
    case  7: return &ffi_type_sint32;
    case  8: return (sizeof(long)==4)?&ffi_type_uint32:&ffi_type_uint64;
    case  9: return (sizeof(long)==4)?&ffi_type_sint32:&ffi_type_sint64;
    case 10: return &ffi_type_uint64;
    case 11: return &ffi_type_sint64;
    case 12: return &ffi_type_float;
    case 13: return &ffi_type_double;
    case 14: return &ffi_type_pointer;
    default:
      fprintf(stderr, "*** Vicare FFI error: %s: invalid arg %ld", __func__, n);
      exit(EXIT_FAILURE);
    }
  } else {
    fprintf(stderr, "*** Vicare FFI error: %s: invalid arg %ld", __func__, nptr);
    exit(EXIT_FAILURE);
  }
}
static ffi_type *
scheme_to_ffi_record_type_cast (ikptr vec)
{
  ikptr lenptr = ref(vec, -vector_tag);
  if (! is_fixnum(lenptr)) {
    fprintf(stderr, "*** Vicare FFI error: %s: not a vector 0x%016lx\n", __func__, vec);
    exit(EXIT_FAILURE);
  }
  long n = unfix(lenptr);
  ffi_type* t = alloc(sizeof(ffi_type), 1);
  ffi_type** ts = alloc(sizeof(ffi_type*), n+1);
  t->size = 0;
  t->alignment = 0;
  t->type = FFI_TYPE_STRUCT;
  t->elements = ts;
  long i;
  for(i=0; i<n; i++){
    ts[i] = scheme_to_ffi_type_cast(ref(vec, off_vector_data + i*wordsize));
  }
  ts[n] = 0;
  return t;
}
static void
scheme_to_ffi_record_value_cast (ffi_type* t, ikptr nptr, ikptr p, void* r)
{
  if (t->type != FFI_TYPE_STRUCT) {
    fprintf(stderr, "*** Vicare error: %s: not a struct type\n", __func__);
    exit(EXIT_FAILURE);
  }
  ffi_type** ts = t->elements;
  char* buf = r;
  ikptr lenptr = ref(nptr, off_vector_length);
  int n = unfix(lenptr);
  int i;
  for(i=0; i<n; i++) {
    ffi_type* at = ts[i];
    ikptr argt = ref(nptr, off_vector_data + i*wordsize);
    ikptr arg = ref(p, off_vector_data + i*wordsize);
    scheme_to_ffi_value_cast(at, argt, arg, buf);
    buf += at->size;
  }
}
static ikptr
ffi_to_scheme_value_cast (int n, void* p, ikpcb* pcb)
{
  switch (n & 0xF) {
  case  1: return void_object;
  case  2: return u_to_number(*((unsigned char*)p), pcb);
  case  3: return s_to_number(*((signed char*)p), pcb);
  case  4: return u_to_number(*((unsigned short*)p), pcb);
  case  5: return s_to_number(*((signed short*)p), pcb);
  case  6: return u_to_number(*((unsigned int*)p), pcb);
  case  7: return s_to_number(*((signed int*)p), pcb);
  case  8: return u_to_number(*((unsigned long*)p), pcb);
  case  9: return s_to_number(*((signed long*)p), pcb);
  case 10: return ull_to_number(*((unsigned long long*)p), pcb);
  case 11: return sll_to_number(*((signed long long*)p), pcb);
  case 12: return d_to_number(*((float*)p), pcb);
  case 13: return d_to_number(*((double*)p), pcb);
  case 14: return ikrt_pointer_alloc((long)*((void**)p), pcb);
  default:
    fprintf(stderr, "*** Vicare error: %s: invalid arg %d", __func__, n);
    exit(EXIT_FAILURE);
  }
}


/** --------------------------------------------------------------------
 ** Other stuff.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_has_ffi (void)
{
  return true_object;
}
ikptr
ikrt_ffi_prep_cif (ikptr rtptr, ikptr argstptr, ikpcb* pcb)
{
  ffi_cif* cif = alloc(sizeof(ffi_cif), 1);
  ffi_abi abi = FFI_DEFAULT_ABI;
  int nargs = unfix(ref(argstptr, off_vector_length));
  ffi_type** argtypes = alloc(sizeof(ffi_type*), nargs+1);
  int i;
  for(i=0; i<nargs; i++){
    ikptr argt = ref(argstptr, off_vector_data + i*wordsize);
    argtypes[i] = scheme_to_ffi_type_cast(argt);
  }
  argtypes[nargs] = NULL;
  ffi_type* rtype = scheme_to_ffi_type_cast(rtptr);
  ffi_status s = ffi_prep_cif(cif, abi, nargs, rtype, argtypes);
  if (s == FFI_OK) {
    ikptr r = ik_safe_alloc(pcb, pointer_size);
    ref(r, 0) = pointer_tag;
    ref(r, wordsize) = (ikptr)cif;
    return r + vector_tag;
  } else {
    return false_object;
  }
}
#ifdef DEBUG_FFI
static void
dump_stack(ikpcb* pcb, char* msg) {
  fprintf(stderr, "====================  %s\n", msg);
  ikptr frame_base = pcb->frame_base;
  ikptr frame_pointer = pcb->frame_pointer;
  ikptr p = frame_pointer;
  fprintf(stderr, "fp=0x%016lx   base=0x%016lx\n", frame_pointer, frame_base);
  while(p < frame_base) {
    fprintf(stderr, "*0x%016lx = 0x%016lx\n", p, ref(p, 0));
    p += wordsize;
  }
}
#endif

ikptr
ikrt_seal_scheme_stack(ikpcb* pcb)
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
ikrt_ffi_call(ikptr data, ikptr argsvec, ikpcb* pcb)
{
  ikrt_seal_scheme_stack(pcb);
  ikptr sk = ik_unsafe_alloc(pcb, system_continuation_size);
  ref(sk, 0) = system_continuation_tag;
  ref(sk, disp_system_continuation_top) = pcb->system_stack;
  ref(sk, disp_system_continuation_next) = pcb->next_k;
  pcb->next_k = sk + vector_tag;


  ikptr cifptr  = ref(data, off_vector_data + 0 * wordsize);
  ikptr funptr  = ref(data, off_vector_data + 1 * wordsize);
  ikptr typevec = ref(data, off_vector_data + 2 * wordsize);
  ikptr rtype   = ref(data, off_vector_data + 3 * wordsize);
  ffi_cif* cif = (ffi_cif*) ref(cifptr, off_pointer_data);
  void(*fn)() = (void (*)()) ref(funptr, off_pointer_data);
  int n = unfix(ref(argsvec, off_vector_length));
  void** avalues = alloc(sizeof(void*), n+1);
  int i;
  for(i=0; i<n; i++){
    ffi_type* t = cif->arg_types[i];
    ikptr at = ref(typevec, off_vector_data + i * wordsize);
    ikptr v = ref(argsvec, off_vector_data + i * wordsize);
    void* p = alloc_room_for_type(t);
    avalues[i] = p;
    scheme_to_ffi_value_cast(t, at, v, p);
  }
  avalues[n] = NULL;
  void* rvalue = alloc_room_for_type(cif->rtype);
  ffi_call(cif, fn, rvalue, avalues);
  pcb->last_errno = errno;
  ikptr val = ffi_to_scheme_value_cast(unfix(rtype), rvalue, pcb);
  for(i=0; i<n; i++){
    free(avalues[i]);
  }
#ifdef DEBUG_FFI
  fprintf(stderr, "DONE WITH CALL, RV=0x%016lx\n", (long)val);
#endif
  free(avalues);
  free(rvalue);

  pcb->frame_pointer = pcb->frame_base - wordsize;

  sk = pcb->next_k - vector_tag;
  if (ref(sk, 0) != system_continuation_tag) {
    fprintf(stderr, "vicare internal error: invalid system cont\n");
    exit(EXIT_FAILURE);
  }
  pcb->next_k = ref(sk, disp_system_continuation_next);
  pcb->system_stack = ref(sk, disp_system_continuation_top);

  return val;
}


/** --------------------------------------------------------------------
 ** Callback: call a Scheme closure from C code.
 ** ----------------------------------------------------------------- */

extern ikpcb* the_pcb;

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
      ffi_to_scheme_value_cast(unfix(argt), argp, pcb);
  }
  ikptr rv = ik_exec_code(pcb, code_ptr, fix(-n), proc);
#ifdef DEBUG_FFI
  fprintf(stderr, "and back with rv=0x%016lx!\n", rv);
#endif
  scheme_to_ffi_value_cast(cif->rtype, rtype_conv, rv, ret);
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
  ikrt_seal_scheme_stack(pcb);

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
 ** Test functions.
 ** ----------------------------------------------------------------- */

#if 0
int ho (int(*f)(int), int n)
{
  /* fprintf(stderr, "HO HO 0x%016lx!\n", (long)f); */
  int n0 = f(n);
  /* fprintf(stderr, "GOT N0\n"); */
  return n0 + f(n);
}
int ho2 (ikptr fptr, ikptr nptr)
{
  int (*f)(int) =  (int(*)(int)) ref(fptr, off_pointer_data);
  int n = unfix(nptr);
  /* fprintf(stderr, "HO2 HO2 0x%016lx!\n", (long)f); */
  int n0 = f(n);
  /* fprintf(stderr, "GOT N0\n"); */
  return n0 + f(n);
}
int test_I_I (int(*f)(int), int n0) {
  return f(n0);
}
int test_I_II (int(*f)(int,int), int n0, int n1) {
  return f(n0,n1);
}
int test_I_III (int(*f)(int,int,int), int n0, int n1, int n2) {
  return f(n0,n1,n2);
}
int add_I_I(int n0) {
  return n0;
}
int add_I_II(int n0, int n1) {
  return n0+n1;
}
int add_I_III(int n0, int n1, int n2) {
  return n0+n1+n2;
}
struct Point{
  float x;
  float y;
};
struct Rect{
  struct Point tl;
  struct Point br;
};
float test_area_F_R(struct Rect r) {
  float dx = r.br.x - r.tl.x;
  float dy = r.br.y - r.tl.y;
  return dx * dy;
}
double test_D_D (double(*f)(double), double n0) {
  return f(n0);
}
double test_D_DD (double(*f)(double,double), double n0, double n1) {
  return f(n0,n1);
}
double test_D_DDD (double(*f)(double,double,double), double n0, double n1, double n2) {
  return f(n0,n1,n2);
}
double add_D_D(double n0) {
  return n0;
}
double add_D_DD(double n0, double n1) {
  return n0+n1;
}
double add_D_DDD(double n0, double n1, double n2) {
  return n0+n1+n2;
}
int cadd1 (int n) {
  return n+1;
}
void hello_world(int n) {
  while(n > 0) {
    fprintf(stderr, "Hello World\n");
    n--;
  }
}
#endif


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
