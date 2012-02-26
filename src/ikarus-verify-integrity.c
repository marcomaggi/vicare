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
  return xi >> pageshift;
}
static int
isa_fixnum (ikptr x)
{
  return ((fixnum_mask & (int)x) == 0);
}
static int
isa_vector (ikptr x)
{
  return ((IK_TAGOF(x) == vector_tag) && isa_fixnum(ref(x, -vector_tag)));
}
static void
verify_code (char* x, char* base, unsigned* svec, unsigned* dvec)
{
  assert(ref(x, 0) == code_tag);
  ikptr rvec = ref(x, disp_code_reloc_vector);
  assert(isa_vector(rvec));
  ikptr codesize = ref(x, disp_code_code_size);
  codesize += 0;
  assert(IK_UNFIX(codesize) >= 0);
  assert(isa_fixnum(codesize));
  ikptr freevars = ref(x, disp_code_freevars);
  freevars += 0;
  assert(isa_fixnum(freevars));
  assert(IK_UNFIX(freevars) >= 0);

  unsigned rs = svec[page_idx((void*)(long)rvec) - page_idx(base)];
  unsigned cs = svec[page_idx(x) - page_idx(base)];
  int cgen = cs&gen_mask;
  int rgen = rs&gen_mask;
  if(rgen < cgen){
    unsigned d = dvec[page_idx(x) - page_idx(base)];
    d = d & d;
    //int off = (((int)x) - IK_ALIGN_TO_PREV_PAGE(x)) / card_size;
    //int card_mark = (d >> off) & 0xF;
    assert(d != 0);
  }
}
static void
verify_object (ikptr x, char* base, unsigned* svec, unsigned* dvec)
{
  x=x; base=base; svec=svec; dvec=dvec; /* no warning */
}
static char*
verify_code_small (char* p, int s, unsigned d,
		   char* base, unsigned* svec, unsigned* dvec)
{
  char* q = p + pagesize;
  s=s; d=d; /* no warning */
  while (p < q) {
    ikptr fst = ref(p, 0);
    if(fst == code_tag){
      assert(IK_IS_FIXNUM(ref(p, disp_code_code_size)));
      int code_size = IK_UNFIX(ref(p, disp_code_code_size));
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
verify_code_large (char* p, unsigned s, unsigned d,
		   char* base, unsigned* svec, unsigned* dvec)
{
  s=s; d=d; /* no warning */
  ikptr fst = ref(p, 0);
  fst += 0;
  assert(fst == code_tag);
  int code_size = IK_UNFIX(ref(p, disp_code_code_size));
  assert(code_size >= 0);
  verify_code(p, base, svec, dvec);
  assert(IK_ALIGN(code_size+disp_code_data) >= pagesize);
  char* end = p + code_size + disp_code_data;
  return((char*)IK_ALIGN_TO_NEXT_PAGE(end));
}
static char*
verify_code_page (char* p, unsigned s, unsigned d,
		  char* base, unsigned* svec, unsigned* dvec)
{
  ikptr fst = ref(p, 0);
  fst += 0;
  if (fst != code_tag) {
    ik_abort("non code object with tag %p found\n", (void*)(long)fst);
  }
  int code_size = IK_UNFIX(ref(p, disp_code_code_size));
  assert(code_size >= 0);
  int obj_size = IK_ALIGN(code_size + disp_code_data);
  char* result;
  if (obj_size <= pagesize) {
    result = verify_code_small(p,s,d,base,svec,dvec);
  } else {
    result = verify_code_large(p,s,d,base,svec,dvec);
  }
  // fprintf(stderr, "code verify incomplete\n");
  return result;
}
static char *
verify_pointers_page (char* p, unsigned s, unsigned d,
		      char* base, unsigned* svec, unsigned* dvec)
{
  s=s; d=d; /* no warning */
  {
    int i = 0;
    while(i < pagesize){
      verify_object(ref(p, i), base, svec, dvec);
      i += wordsize;
    }
  }
  //fprintf(stderr, "pointers verif incomplete\n");
  return p+pagesize;
}
static char *
verify_page (char* p, char* base, unsigned* svec, unsigned* dvec)
{
  int idx = page_idx(p) - page_idx(base);
  unsigned s = svec[idx];
  unsigned d = dvec[idx];
  //  if(s & dealloc_mask){
  //    return p+pagesize;
  //  }
  int type = s & type_mask;
  if(type == hole_type){
    return p+pagesize;
  }
  assert((s & new_gen_mask) == 0);
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
    return p+pagesize;
  }
  else if(type == mainheap_type){
    /* nothing to do for main heap */
    return p+pagesize;
  }
  else if(type == mainstack_type){
    /* nothing to do for main stack */
    return p+pagesize;
  }
  ik_abort("type=0x%08x\n", type);
}
void
ik_verify_integrity (ikpcb* pcb, char* where)
{
  fprintf(stderr, "verifying in %s...\n", where);
  char* mem_base = (char*)(long)pcb->memory_base;
  char* mem_end = (char*)(long)pcb->memory_end;
  unsigned* seg_vec = pcb->segment_vector_base;
  unsigned* dir_vec = pcb->dirty_vector_base;
  char* mem = mem_base;
  while (mem < mem_end) {
    mem = verify_page(mem, mem_base, seg_vec, dir_vec);
  }
  fprintf(stderr, "verify_ok in %s\n", where);
}

/* end of file */
