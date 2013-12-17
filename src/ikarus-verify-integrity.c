/*
 * Ikarus Scheme -- A compiler for R6RS Scheme.
 * Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 * Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
 *
 * This program is free software:  you can redistribute it and/or modify
 * it under  the terms of  the GNU General  Public License version  3 as
 * published by the Free Software Foundation.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
 * General Public License for more details.
 *
 * You should  have received  a copy of  the GNU General  Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "internals.h"
#define fixnum_mask 3
#define pmask 7

static long int
page_idx (void* x)
{
  ik_ulong	xi = (ik_ulong) x;
  return xi >> IK_PAGESHIFT;
}
static int
isa_fixnum (ikptr x)
{
  return ((fixnum_mask & (int)x) == 0);
}
static int
isa_vector (ikptr x)
{
  return ((IK_TAGOF(x) == vector_tag) && isa_fixnum(IK_REF(x, -vector_tag)));
}
static void
verify_code (char* x, char* base, unsigned* svec, unsigned* dvec)
{
  assert(IK_REF(x, 0) == code_tag);
  ikptr rvec = IK_REF(x, disp_code_reloc_vector);
  assert(isa_vector(rvec));
  ikptr codesize = IK_REF(x, disp_code_code_size);
  codesize += 0;
  assert(IK_UNFIX(codesize) >= 0);
  assert(isa_fixnum(codesize));
  ikptr freevars = IK_REF(x, disp_code_freevars);
  freevars += 0;
  assert(isa_fixnum(freevars));
  assert(IK_UNFIX(freevars) >= 0);

  unsigned rs = svec[page_idx((void*)(long)rvec) - page_idx(base)];
  unsigned cs = svec[page_idx(x) - page_idx(base)];
  int cgen = cs&GEN_MASK;
  int rgen = rs&GEN_MASK;
  if(rgen < cgen){
    unsigned d = dvec[page_idx(x) - page_idx(base)];
    d = d & d;
    //int off = (((int)x) - IK_ALIGN_TO_PREV_PAGE(x)) / card_size;
    //int card_mark = (d >> off) & 0xF;
    assert(d != 0);
  }
}
static void
verify_object (ikptr x IK_UNUSED, char* base IK_UNUSED,
	       unsigned* svec IK_UNUSED, unsigned* dvec IK_UNUSED)
{
  /* have the compiler shut up about unused variables */
  /* x=x; base=base; svec=svec; dvec=dvec; */
  return;
}
static char*
verify_code_small (char* p, int s IK_UNUSED, unsigned d IK_UNUSED,
		   char* base, unsigned* svec, unsigned* dvec)
{
  char* q = p + IK_PAGESIZE;
  /* have the compiler shut up about unused variables */
  /* s=s; d=d; */
  while (p < q) {
    ikptr	fst = IK_REF(p, 0);
    if (code_tag == fst) {
      ikptr	s_code_size = IK_REF(p, disp_code_code_size);
      int	code_size;
      if (!IK_IS_FIXNUM(s_code_size)) {
	ik_debug_message_no_newline("%s: expected fixnum as code object size, got: ",
				    __func__);
	ik_print(s_code_size);
	ik_debug_message_no_newline("%s: code object: ", __func__);
	ik_print(((ikptr)p) | code_primary_tag);
	ik_abort("integrity check failed");
      }
      code_size = IK_UNFIX(IK_REF(p, disp_code_code_size));
      assert(code_size >= 0);
      verify_code(p, base, svec, dvec);
      p+=IK_ALIGN(code_size + disp_code_data);
    } else {
      p = q;
    }
  }
  if (p != q) {
    ik_abort("code extended beyond a page in %p, %p\n", p, q);
  }
  return q;
}
static char *
verify_code_large (char* p, unsigned s IK_UNUSED, unsigned d IK_UNUSED,
		   char* base, unsigned* svec, unsigned* dvec)
{
  /* have the compiler shut up about unused variables */
  /* s=s; d=d; */
  ikptr fst = IK_REF(p, 0);
  fst += 0;
  assert(fst == code_tag);
  int code_size = IK_UNFIX(IK_REF(p, disp_code_code_size));
  assert(code_size >= 0);
  verify_code(p, base, svec, dvec);
  assert(IK_ALIGN(code_size+disp_code_data) >= IK_PAGESIZE);
  char* end = p + code_size + disp_code_data;
  return((char*)IK_ALIGN_TO_NEXT_PAGE(end));
}
static char*
verify_code_page (char* p, unsigned s, unsigned d,
		  char* base, unsigned* svec, unsigned* dvec)
{
  ikptr fst = IK_REF(p, 0);
  fst += 0;
  if (fst != code_tag) {
    ik_abort("non code object with tag %p found\n", (void*)(long)fst);
  }
  int code_size = IK_UNFIX(IK_REF(p, disp_code_code_size));
  assert(code_size >= 0);
  int obj_size = IK_ALIGN(code_size + disp_code_data);
  char* result;
  if (obj_size <= IK_PAGESIZE) {
    result = verify_code_small(p,s,d,base,svec,dvec);
  } else {
    result = verify_code_large(p,s,d,base,svec,dvec);
  }
  // fprintf(stderr, "code verify incomplete\n");
  return result;
}
static char *
verify_pointers_page (char* p, unsigned s IK_UNUSED, unsigned d IK_UNUSED,
		      char* base, unsigned* svec, unsigned* dvec)
{
  /* have the compiler shut up about unused variables */
  /* s=s; d=d; */
  {
    int i = 0;
    while(i < IK_PAGESIZE){
      verify_object(IK_REF(p, i), base, svec, dvec);
      i += wordsize;
    }
  }
  //fprintf(stderr, "pointers verif incomplete\n");
  return p+IK_PAGESIZE;
}
static char *
verify_page (char* p, char* base, unsigned* svec, unsigned* dvec)
{
  int idx = page_idx(p) - page_idx(base);
  unsigned s = svec[idx];
  unsigned d = dvec[idx];
  //  if(s & dealloc_mask){
  //    return p+IK_PAGESIZE;
  //  }
  int type = s & type_mask;
  if(type == hole_type){
    return p+IK_PAGESIZE;
  }
  assert((s & NEW_GEN_MASK) == 0);
  if(type == code_type){
    return verify_code_page(p,s,d,base,svec,dvec);
  }
  else if(type == pointers_type){
    return verify_pointers_page(p,s,d,base,svec,dvec);
  }
  else if(type == weak_pairs_type){
    return verify_pointers_page(p,s,d,base,svec,dvec);
  }
  else if(type == symbols_type){
    return verify_pointers_page(p,s,d,base,svec,dvec);
  }
  else if(type == dat_type){
    /* nothing to do for data */
    return p+IK_PAGESIZE;
  }
  else if(type == mainheap_type){
    /* nothing to do for main heap */
    return p+IK_PAGESIZE;
  }
  else if(type == mainstack_type){
    /* nothing to do for main stack */
    return p+IK_PAGESIZE;
  }
  ik_abort("type=0x%08x\n", type);
  return NULL;
}
void
ik_verify_integrity (ikpcb* pcb, char* where)
{
#define LOG_VERIFY	0
  if (LOG_VERIFY) {
    ik_debug_message("%s: verifying in %s...", __func__, where);
  }
  char* mem_base = (char*)(long)pcb->memory_base;
  char* mem_end = (char*)(long)pcb->memory_end;
  unsigned* seg_vec = pcb->segment_vector_base;
  unsigned* dir_vec = pcb->dirty_vector_base;
  char* mem = mem_base;
  while (mem < mem_end) {
    mem = verify_page(mem, mem_base, seg_vec, dir_vec);
  }
  if (LOG_VERIFY) {
    ik_debug_message("%s: verify_ok in %s", __func__, where);
  }
}

/* end of file */
