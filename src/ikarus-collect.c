/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include "ikarus.h"
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/time.h>

#define forward_ptr ((ikptr)-1)
#define minimum_heap_size (pagesize * 1024 * 4)
#define maximum_heap_size (pagesize * 1024 * 8)
#define minimum_stack_size (pagesize * 128)

#define accounting 0

#if accounting
static int pair_count = 0;
static int symbol_count = 0;
static int closure_count = 0;
static int vector_count = 0;
static int record_count = 0;
static int continuation_count = 0;
static int string_count = 0;
static int htable_count = 0;
#endif

typedef struct qupages_t{
  ikptr p;    /* pointer to the scan start */
  ikptr q;    /* pointer to the scan end */
  struct qupages_t* next;
} qupages_t;


typedef struct{
  ikptr ap;
  ikptr aq;
  ikptr ep;
  ikptr base;
} meta_t;


#define meta_ptrs 0
#define meta_code 1
#define meta_data 2
#define meta_weak 3
#define meta_pair 4
#define meta_symbol 5
#define meta_count 6

static int extension_amount[meta_count] = {
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
};

static unsigned int meta_mt[meta_count] = {
  pointers_mt,
  code_mt,
  data_mt,
  weak_pairs_mt,
  pointers_mt,
  symbols_mt
};

typedef struct gc_t{
  meta_t meta [meta_count];
  qupages_t* queues [meta_count];
  ikpcb* pcb;
  unsigned int* segment_vector;
  int collect_gen;
  int collect_gen_tag;
  ikptr tconc_ap;
  ikptr tconc_ep;
  ikptr tconc_base;
  ikpages* tconc_queue;
  ik_ptr_page* forward_list;
} gc_t;

static void handle_guardians(gc_t* gc);
static void gc_finalize_guardians(gc_t* gc);

static unsigned int
next_gen_tag[generation_count] = {
  (4 << meta_dirty_shift) | 1 | new_gen_tag,
  (2 << meta_dirty_shift) | 2 | new_gen_tag,
  (1 << meta_dirty_shift) | 3 | new_gen_tag,
  (0 << meta_dirty_shift) | 4 | new_gen_tag,
  (0 << meta_dirty_shift) | 4 | new_gen_tag
};

static ikptr
meta_alloc_extending(long int size, gc_t* gc, int meta_id){
  long int mapsize = align_to_next_page(size);
  if(mapsize < extension_amount[meta_id]){
    mapsize = extension_amount[meta_id];
  }
  meta_t* meta = &gc->meta[meta_id];
  if((meta_id != meta_data) &&  meta->base){
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    ikptr aq = meta->aq;
    ikptr ap = meta->ap;
    ikptr ep = meta->ep;
    p->p = aq;
    p->q = ap;
    p->next = gc->queues[meta_id];
    gc->queues[meta_id] = p;
    ikptr x = ap;
    while(x < ep){
      ref(x, 0) = 0;
      x += wordsize;
    }
  }
  ikptr mem = ik_mmap_typed(
      mapsize,
      meta_mt[meta_id] | gc->collect_gen_tag,
      gc->pcb);
  gc->segment_vector = gc->pcb->segment_vector;
  meta->ap = mem + size;
  meta->aq = mem;
  meta->ep = mem + mapsize;
  meta->base = mem;
  return mem;
}




static inline ikptr
meta_alloc(long int size, gc_t* gc, int meta_id){
  assert(size == IK_ALIGN(size));
  meta_t* meta = &gc->meta[meta_id];
  ikptr ap = meta->ap;
  ikptr ep = meta->ep;
  ikptr nap = ap + size;
  if(nap > ep){
    return meta_alloc_extending(size, gc, meta_id);
  } else {
    meta->ap = nap;
    return ap;
  }
}

static inline ikptr
gc_alloc_new_ptr(int size, gc_t* gc){
  assert(size == IK_ALIGN(size));
  return meta_alloc(size, gc, meta_ptrs);
}

static inline ikptr
gc_alloc_new_large_ptr(int size, gc_t* gc){
  int memreq = align_to_next_page(size);
  ikptr mem =
      ik_mmap_typed(memreq,
        pointers_mt | large_object_tag | gc->collect_gen_tag,
        gc->pcb);
  gc->segment_vector = gc->pcb->segment_vector;
  qupages_t* p = ik_malloc(sizeof(qupages_t));
  p->p = mem;
  p->q = mem+size;
  bzero((char*)(long)(mem+size), memreq-size);
  p->next = gc->queues[meta_ptrs];
  gc->queues[meta_ptrs] = p;
  return mem;
}


static inline void
enqueue_large_ptr(ikptr mem, int size, gc_t* gc){
  long int i = page_index(mem);
  long int j = page_index(mem+size-1);
  while(i<=j){
    gc->segment_vector[i] =
      pointers_mt | large_object_tag | gc->collect_gen_tag;
    i++;
  }
  qupages_t* p = ik_malloc(sizeof(qupages_t));
  p->p = mem;
  p->q = mem+size;
  p->next = gc->queues[meta_ptrs];
  gc->queues[meta_ptrs] = p;
}


static inline ikptr
gc_alloc_new_symbol_record(gc_t* gc){
  assert(symbol_record_size == IK_ALIGN(symbol_record_size));
  return meta_alloc(symbol_record_size, gc, meta_symbol);
}




static inline ikptr
gc_alloc_new_pair(gc_t* gc){
  return meta_alloc(pair_size, gc, meta_pair);
}



static inline ikptr
gc_alloc_new_weak_pair(gc_t* gc){
  meta_t* meta = &gc->meta[meta_weak];
  ikptr ap = meta->ap;
  ikptr ep = meta->ep;
  ikptr nap = ap + pair_size;
  if(nap > ep){
      ikptr mem = ik_mmap_typed(
                   pagesize,
                   meta_mt[meta_weak] | gc->collect_gen_tag,
                   gc->pcb);
      gc->segment_vector = gc->pcb->segment_vector;
      meta->ap = mem + pair_size;
      meta->aq = mem;
      meta->ep = mem + pagesize;
      meta->base = mem;
      return mem;
  } else {
    meta->ap = nap;
    return ap;
  }
}

static inline ikptr
gc_alloc_new_data(int size, gc_t* gc){
  assert(size == IK_ALIGN(size));
  return meta_alloc(size, gc, meta_data);
}

static inline ikptr
gc_alloc_new_code(long int size, gc_t* gc){
  assert(size == IK_ALIGN(size));
  if(size < pagesize){
    return meta_alloc(size, gc, meta_code);
  } else {
    long int memreq = align_to_next_page(size);
    ikptr mem = ik_mmap_code(memreq, gc->collect_gen, gc->pcb);
    gc->segment_vector = gc->pcb->segment_vector;
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    p->p = mem;
    p->q = mem+size;
    bzero((char*)(long)(mem+size), memreq-size);
    p->next = gc->queues[meta_code];
    gc->queues[meta_code] = p;
    return mem;
  }
}

static void
add_to_collect_count(ikpcb* pcb, int bytes){
  int minor = bytes + pcb->allocation_count_minor;
  while(minor >= most_bytes_in_minor){
    minor -= most_bytes_in_minor;
    pcb->allocation_count_major++;
  }
  pcb->allocation_count_minor = minor;
}




static void
gc_tconc_push_extending(gc_t* gc, ikptr tcbucket){
  if(gc->tconc_base){
    ikpages* p = ik_malloc(sizeof(ikpages));
    p->base = gc->tconc_base;
    p->size = pagesize;
    p->next = gc->tconc_queue;
    gc->tconc_queue = p;
  }
  ikptr ap =
     ik_mmap_typed(pagesize,
        meta_mt[meta_ptrs] | gc->collect_gen_tag,
        gc->pcb);
  add_to_collect_count(gc->pcb, pagesize);
  gc->segment_vector = gc->pcb->segment_vector;
  bzero((char*)(long)ap, pagesize);
  ikptr nap = ap + 2*wordsize;
  gc->tconc_base = ap;
  gc->tconc_ap = nap;
  gc->tconc_ep = ap + pagesize;
  ref(ap,0) = tcbucket;
}


static inline void
gc_tconc_push(gc_t* gc, ikptr tcbucket){
  ikptr ap = gc->tconc_ap;
  ikptr nap = ap + 2*wordsize;
  if(nap > gc->tconc_ep){
    gc_tconc_push_extending(gc, tcbucket);
  } else {
    gc->tconc_ap = nap;
    ref(ap,0) = tcbucket;
  }
}


#ifndef NDEBUG
static ikptr add_object_proc(gc_t* gc, ikptr x, char* caller);
#define add_object(gc,x,caller) add_object_proc(gc,x,caller)
#else
static ikptr add_object_proc(gc_t* gc, ikptr x);
#define add_object(gc,x,caller) add_object_proc(gc,x)
#endif

static void collect_stack(gc_t*, ikptr top, ikptr base);
static void collect_locatives(gc_t*, ik_callback_locative*);
static void collect_loop(gc_t*);
static void fix_weak_pointers(gc_t*);
static void gc_add_tconcs(gc_t*);

/* ik_collect is called from scheme under the following conditions:
 * 1. An attempt is made to allocate a small object and the ap is above
 *    the red line.
 * 2. The current frame of the call is dead, so, upon return from ik_collect,
 *    the caller returns to its caller.
 * 3. The frame-pointer of the caller to S_collect is saved at
 *    pcb->frame_pointer.  No variables are live at that frame except for
 *    the return point (at *(pcb->frame_pointer)).
 * 4. S_collect must return a new ap (in pcb->allocation_pointer) that has
 *    at least 2 pages of memory free.
 * 5. S_collect must also update pcb->allocaton_redline to be 2 pages below
 *    the real end of heap.
 * 6. ik_collect should not move the stack.
 */



ikpcb* ik_collect_vararg(int req, ikpcb* pcb){
  return ik_collect(req, pcb);
}

static int collection_id_to_gen(int id){
  if((id & 255) == 255) { return 4; }
  if((id & 63) == 63) { return 3; }
  if((id & 15) == 15) { return 2; }
  if((id & 3) == 3) { return 1; }
  return 0;
}



static void scan_dirty_pages(gc_t*);

static void deallocate_unused_pages(gc_t*);

static void fix_new_pages(gc_t* gc);

extern void verify_integrity(ikpcb* pcb, char*);

ikptr ik_collect_check(unsigned long int req, ikpcb* pcb){
  long int bytes = ((long int)pcb->allocation_redline) -
                   ((long int)pcb->allocation_pointer);
  if (bytes >= req) {
    return true_object;
  } else {
    ik_collect(req, pcb);
    return false_object;
  }
}

ikpcb*
ik_collect(unsigned long int mem_req, ikpcb* pcb){
#ifndef NDEBUG
  verify_integrity(pcb, "entry");
#endif

  { /* ACCOUNTING */
    long int bytes = ((long int)pcb->allocation_pointer) -
                     ((long int)pcb->heap_base);
    add_to_collect_count(pcb, bytes);
  }

  struct rusage t0, t1;
  struct timeval rt0, rt1;
  gettimeofday(&rt0, 0);
  getrusage(RUSAGE_SELF, &t0);

  pcb->collect_key = false_object;
  gc_t gc;
  bzero(&gc, sizeof(gc_t));
  gc.pcb = pcb;
  gc.segment_vector = pcb->segment_vector;

  gc.collect_gen = collection_id_to_gen(pcb->collection_id);
  gc.collect_gen_tag = next_gen_tag[gc.collect_gen];
  pcb->collection_id++;
#ifndef NDEBUG
  fprintf(stderr, "ik_collect entry %ld free=%ld (collect gen=%d/id=%d)\n",
      mem_req,
       pcb->allocation_redline - pcb->allocation_pointer,
      gc.collect_gen, pcb->collection_id-1);
#endif

  /* cache heap-pages to delete later */
  ikpages* old_heap_pages = pcb->heap_pages;
  pcb->heap_pages = 0;

  /* the roots are:
   *  0. dirty pages not collected in this run
   *  1. the stack
   *  2. the next continuation
   *  3. the symbol-table
   */

  scan_dirty_pages(&gc);

  collect_stack(&gc, pcb->frame_pointer, pcb->frame_base - wordsize);
  collect_locatives(&gc, pcb->callbacks);
  pcb->next_k = add_object(&gc, pcb->next_k, "next_k");
  pcb->symbol_table = add_object(&gc, pcb->symbol_table, "symbol_table");
  pcb->gensym_table = add_object(&gc, pcb->gensym_table, "gensym_table");
  pcb->arg_list = add_object(&gc, pcb->arg_list, "args_list_foo");
  pcb->base_rtd = add_object(&gc, pcb->base_rtd, "base_rtd");
  if(pcb->root0) *(pcb->root0) = add_object(&gc, *(pcb->root0), "root0");
  if(pcb->root1) *(pcb->root1) = add_object(&gc, *(pcb->root1), "root1");

  /* now we trace all live objects */
  collect_loop(&gc);

  /* next we trace all guardian/guarded objects,
     the procedure does a collect_loop at the end */
  handle_guardians(&gc);
#ifndef NDEBUG
  fprintf(stderr, "done\n");
#endif
  collect_loop(&gc);

  /* does not allocate, only bwp's dead pointers */
  fix_weak_pointers(&gc);
  /* now deallocate all unused pages */
  deallocate_unused_pages(&gc);

  fix_new_pages(&gc);
  gc_finalize_guardians(&gc);

  pcb->allocation_pointer = pcb->heap_base;
  /* does not allocate */
  gc_add_tconcs(&gc);
  /* does not allocate */
#ifndef NDEBUG
  fprintf(stderr, "done\n");
#endif
  pcb->weak_pairs_ap = 0;
  pcb->weak_pairs_ep = 0;

#if accounting
    fprintf(stderr,
        "[%d cons|%d sym|%d cls|%d vec|%d rec|%d cck|%d str|%d htb]\n",
        pair_count,
        symbol_count,
        closure_count,
        vector_count,
        record_count,
        continuation_count,
        string_count,
        htable_count);
    pair_count = 0;
    symbol_count = 0;
    closure_count = 0;
    vector_count = 0;
    record_count = 0;
    continuation_count = 0;
    string_count = 0;
    htable_count = 0;
#endif
  //ik_dump_metatable(pcb);
#ifndef NDEBUG
  fprintf(stderr, "collect done\n");
#endif


  /* delete all old heap pages */
  if(old_heap_pages){
    ikpages* p = old_heap_pages;
    do{
      ikpages* next = p->next;
      ik_munmap_from_segment(p->base, p->size, pcb);
      ik_free(p, sizeof(ikpages));
      p=next;
    } while(p);
    old_heap_pages = 0;
  }

  unsigned long int free_space =
    ((unsigned long int)pcb->allocation_redline) -
    ((unsigned long int)pcb->allocation_pointer);
  if((free_space <= mem_req) || (pcb->heap_size < IK_HEAPSIZE)){
#ifndef NDEBUG
    fprintf(stderr, "REQ=%ld, got %ld\n", mem_req, free_space);
#endif
    long int memsize = (mem_req > IK_HEAPSIZE) ? mem_req : IK_HEAPSIZE;
    memsize = align_to_next_page(memsize);
    ik_munmap_from_segment(
        pcb->heap_base,
        pcb->heap_size,
        pcb);
    ikptr ptr = ik_mmap_mixed(memsize+2*pagesize, pcb);
    pcb->allocation_pointer = ptr;
    pcb->allocation_redline = ptr+memsize;
    pcb->heap_base = ptr;
    pcb->heap_size = memsize+2*pagesize;
  }

#ifndef NDEBUG
  ikptr x = pcb->allocation_pointer;
  while(x < pcb->allocation_redline){
    ref(x, 0) = (ikptr)(0x1234FFFF);
    x+=wordsize;
  }
#endif
#ifndef NDEBUG
  verify_integrity(pcb, "exit");
#endif

  getrusage(RUSAGE_SELF, &t1);
  gettimeofday(&rt1, 0);

  pcb->collect_utime.tv_usec += t1.ru_utime.tv_usec - t0.ru_utime.tv_usec;
  pcb->collect_utime.tv_sec += t1.ru_utime.tv_sec - t0.ru_utime.tv_sec;
  if (pcb->collect_utime.tv_usec >= 1000000){
   pcb->collect_utime.tv_usec -= 1000000;
   pcb->collect_utime.tv_sec += 1;
  }
  else if (pcb->collect_utime.tv_usec < 0){
   pcb->collect_utime.tv_usec += 1000000;
   pcb->collect_utime.tv_sec -= 1;
  }

  pcb->collect_stime.tv_usec += t1.ru_stime.tv_usec - t0.ru_stime.tv_usec;
  pcb->collect_stime.tv_sec += t1.ru_stime.tv_sec - t0.ru_stime.tv_sec;
  if (pcb->collect_stime.tv_usec >= 1000000){
   pcb->collect_stime.tv_usec -= 1000000;
   pcb->collect_stime.tv_sec += 1;
  }
  else if (pcb->collect_stime.tv_usec < 0){
   pcb->collect_stime.tv_usec += 1000000;
   pcb->collect_stime.tv_sec -= 1;
  }

  pcb->collect_rtime.tv_usec += rt1.tv_usec - rt0.tv_usec;
  pcb->collect_rtime.tv_sec += rt1.tv_sec - rt0.tv_sec;
  if (pcb->collect_rtime.tv_usec >= 1000000){
   pcb->collect_rtime.tv_usec -= 1000000;
   pcb->collect_rtime.tv_sec += 1;
  }
  else if (pcb->collect_rtime.tv_usec < 0){
   pcb->collect_rtime.tv_usec += 1000000;
   pcb->collect_rtime.tv_sec -= 1;
  }
  return pcb;
}

static inline int
is_live(ikptr x, gc_t* gc){
  if(is_fixnum(x)){
    return 1;
  }
  int tag = IK_TAGOF(x);
  if(tag == immediate_tag){
    return 1;
  }
  if(ref(x, -tag) == forward_ptr){
    return 1;
  }
  unsigned int t = gc->segment_vector[page_index(x)];
  int gen = t & gen_mask;
  if(gen > gc->collect_gen){
    return 1;
  }
  return 0;
}

static inline int
next_gen(int i){
  return ((i == (generation_count-1)) ? i : (i+1));
}


static ik_ptr_page*
move_tconc(ikptr tc, ik_ptr_page* ls){
  if((ls == NULL) || (ls->count == ik_ptr_page_size)){
    ik_ptr_page* page = (ik_ptr_page*)ik_mmap(pagesize);
    page->count = 0;
    page->next = ls;
    ls = page;
  }
  ls->ptr[ls->count++] = tc;
  return ls;
}

static void
handle_guardians(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  ik_ptr_page* pend_hold_list = 0;
  ik_ptr_page* pend_final_list = 0;
  int gen;
  /* sort protected pairs into pend_hold and pend_final lists */
  for(gen=0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* prot_list = pcb->protected_list[gen];
    pcb->protected_list[gen] = 0;
    while(prot_list){
      int i;
      for(i=0; i<prot_list->count; i++){
        ikptr p = prot_list->ptr[i];
        ikptr tc = ref(p, off_car);
        ikptr obj = ref(p, off_cdr);
        if(tc == forward_ptr){
          ikptr np = ref(p, off_cdr);
          tc = ref(np, off_car);
          obj = ref(np, off_cdr);
        }
        if(is_live(obj, gc)){
          pend_hold_list = move_tconc(p, pend_hold_list);
        } else {
          pend_final_list = move_tconc(p, pend_final_list);
        }
      }
      ik_ptr_page* next = prot_list->next;
      ik_munmap((ikptr)prot_list, pagesize);
      prot_list = next;
    }
  }
  /* move live tc pend_final_list pairs into final_list,
     the rest remain in pend_final_list,
     final_list objects are made live and collected in
     gc->forward_list */
  gc->forward_list = 0;
  int done = 0;
  while(!done){
    ik_ptr_page* final_list = 0;
    ik_ptr_page* ls = pend_final_list;
    pend_final_list = 0;
    while(ls){
      int i;
      for(i=0; i<ls->count; i++){
        ikptr p = ls->ptr[i];
        ikptr tc = ref(p, off_car);
        if(tc == forward_ptr){
          ikptr np = ref(p, off_cdr);
          tc = ref(np, off_car);
        }
        if(is_live(tc, gc)){
          final_list = move_tconc(p, final_list);
        } else {
          pend_final_list = move_tconc(p, pend_final_list);
        }
      }
      ik_ptr_page* next = ls->next;
      ik_munmap((ikptr)ls, pagesize);
      ls = next;
    }
    if(final_list == NULL){
      done = 1;
    } else {
      ls = final_list;
      while(ls){
        int i;
        for(i=0; i<ls->count; i++){
          ikptr p = ls->ptr[i];
          gc->forward_list =
            move_tconc(add_object(gc, p, "guardian"),
                gc->forward_list);
        }
        ik_ptr_page* next = ls->next;
        ik_munmap((ikptr)ls, pagesize);
        ls = next;
      }
      collect_loop(gc);
    }
  }
  /* pend_final_list now contains things that are dead and
     their tconcs are also dead, deallocate */
  while(pend_final_list){
    ik_ptr_page* next = pend_final_list->next;
    ik_munmap((ikptr)pend_final_list, pagesize);
    pend_final_list = next;
  }
  /* pend_hold_list pairs with live tconcs are moved to
     the protected list of next generation. */
  ik_ptr_page* target = pcb->protected_list[next_gen(gc->collect_gen)];
  while(pend_hold_list){
    int i;
    for(i=0; i<pend_hold_list->count; i++){
      ikptr p = pend_hold_list->ptr[i];
      ikptr tc = ref(p, off_car);
      if(tc == forward_ptr){
        ikptr np = ref(p, off_cdr);
        tc = ref(np, off_car);
      }
      if(is_live(tc, gc)){
        target = move_tconc(add_object(gc, p, "guardian"), target);
      }
    }
    ik_ptr_page* next = pend_hold_list->next;
    ik_munmap((ikptr)pend_hold_list, pagesize);
    pend_hold_list = next;
  }
  collect_loop(gc);
  pcb->protected_list[next_gen(gc->collect_gen)] = target;
}

static void
gc_finalize_guardians(gc_t* gc){
  ik_ptr_page* ls = gc->forward_list;
  int tconc_count = 0;
  unsigned int* dirty_vec = (unsigned int*)(long)gc->pcb->dirty_vector;
  while(ls){
    int i;
    for(i=0; i<ls->count; i++){
      tconc_count++;
      ikptr p = ls->ptr[i];
      ikptr tc = ref(p, off_car);
      ikptr obj = ref(p, off_cdr);
      ikptr last_pair = ref(tc, off_cdr);
      ref(last_pair, off_car) = obj;
      ref(last_pair, off_cdr) = p;
      ref(p, off_car) = false_object;
      ref(p, off_cdr) = false_object;
      ref(tc, off_cdr) = p;
      dirty_vec[page_index(tc)] = -1;
      dirty_vec[page_index(last_pair)] = -1;
    }
    ik_ptr_page* next = ls->next;
    ik_munmap((ikptr)ls, pagesize);
    ls = next;
  }
}


static int alloc_code_count = 0;


static ikptr
add_code_entry(gc_t* gc, ikptr entry){
  ikptr x = entry - disp_code_data;
  if(ref(x,0) == forward_ptr){
    return ref(x,wordsize) + off_code_data;
  }
  long int idx = page_index(x);
  unsigned int t = gc->segment_vector[idx];
  int gen = t & gen_mask;
  if(gen > gc->collect_gen){
    return entry;
  }
  long int code_size = unfix(ref(x, disp_code_code_size));
  ikptr reloc_vec = ref(x, disp_code_reloc_vector);
  ikptr freevars = ref(x, disp_code_freevars);
  ikptr annotation = ref(x, disp_code_annotation);
  long int required_mem = IK_ALIGN(disp_code_data + code_size);
  if(required_mem >= pagesize){
    int new_tag = gc->collect_gen_tag;
    long int idx = page_index(x);
    gc->segment_vector[idx] = new_tag | code_mt;
    long int i;
    for(i=pagesize, idx++; i<required_mem; i+=pagesize, idx++){
      gc->segment_vector[idx] = new_tag | data_mt;
    }
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    p->p = x;
    p->q = x+required_mem;
    p->next = gc->queues[meta_code];
    gc->queues[meta_code] = p;
    return entry;
  } else {
    ikptr y = gc_alloc_new_code(required_mem, gc);
    ref(y, 0) = code_tag;
    ref(y, disp_code_code_size) = fix(code_size);
    ref(y, disp_code_reloc_vector) = reloc_vec;
    ref(y, disp_code_freevars) = freevars;
    ref(y, disp_code_annotation) = annotation;
    memcpy((char*)(long)(y+disp_code_data),
           (char*)(long)(x+disp_code_data),
           code_size);
    ref(x, 0) = forward_ptr;
    ref(x, wordsize) = y + vector_tag;
    return y+disp_code_data;
  }
}

static void
collect_locatives(gc_t* gc, ik_callback_locative* loc) {
  while(loc) {
    loc->data = add_object(gc, loc->data, "locative");
    loc = loc->next;
  }
}

#define DEBUG_STACK 0

static void collect_stack(gc_t* gc, ikptr top, ikptr end){
  if(DEBUG_STACK){
    fprintf(stderr, "collecting stack (size=%ld) from 0x%016lx .. 0x%016lx\n",
        (long)end - (long)top, (long) top, (long) end);
  }
  while(top < end){
    if(DEBUG_STACK){
      fprintf(stderr, "collecting frame at 0x%016lx: \n", (long) top);
    }
    ikptr rp = ref(top, 0);
    long int rp_offset = unfix(ref(rp, disp_frame_offset));
    if(DEBUG_STACK){
      fprintf(stderr, "rp=0x%016lx\n", rp);
      fprintf(stderr, "rp_offset=%ld\n", rp_offset);
    }
    if(rp_offset <= 0){
      fprintf(stderr, "invalid rp_offset %ld\n", rp_offset);
      exit(EXIT_FAILURE);
    }
    /* since the return point is alive, we need to find the code
     * object containing it and mark it live as well.  the rp is
     * updated to reflect the new code object. */

    long int code_offset = rp_offset - disp_frame_offset;
    ikptr code_entry = rp - code_offset;
    ikptr new_code_entry = add_code_entry(gc, code_entry);
    ikptr new_rp = new_code_entry + code_offset;
    ref(top, 0) = new_rp;

    /* now for some livemask action.
     * every return point has a live mark above it.  the live mask
     * is a sequence of bytes (every byte for 8 frame cells).  the
     * size of the live mask is determined by the size of the frame.
     * this is how the call frame instruction sequence looks like:
     *
     *   |    ...     |
     *   | code  junk |
     *   +------------+
     *   |   byte 0   |   for fv0 .. fv7
     *   |   byte 1   |   for fv8 .. fv15
     *   |    ...     |   ...
     *   +------------+
     *   |  framesize |
     *   |    word    |
     *   +------------+
     *   | multivalue |
     *   |    word    |
     *   +------------+
     *   | frameoffst |  the frame offset determined how far its
     *   |    word    |  address is off from the start of the code
     *   +------------+
     *   |  padding   |  the size of this part is fixed so that we
     *   |  and call  |  can correlate the frame info (above) with rp
     *   +------------+
     *   | code  junk | <---- rp
     *   |    ...     |
     *
     *   WITH ONE EXCEPTION:
     *   if the framesize is 0, then the actual frame size is stored
     *   on the stack immediately below the return point.
     *   there is no live mask in this case, instead all values in the
     *   frame are live.
     */
    long int framesize =  ref(rp, disp_frame_size);
    if(DEBUG_STACK){
      fprintf(stderr, "fs=%ld\n", (long)framesize);
    }
    if(framesize < 0){
      fprintf(stderr, "invalid frame size %ld\n", (long)framesize);
      exit(EXIT_FAILURE);
    }
    else if(framesize == 0){
      framesize = ref(top, wordsize);
      if(framesize <= 0){
        fprintf(stderr, "invalid redirected framesize=%ld\n", (long)framesize);
        exit(EXIT_FAILURE);
      }
      ikptr base = top + framesize - wordsize;
      while(base > top){
        ikptr new_obj = add_object(gc,ref(base,0), "frame");
        ref(base,0) = new_obj;
        base -= wordsize;
      }
    } else {
      long int frame_cells = framesize >> fx_shift;
      long int bytes_in_mask = (frame_cells+7) >> 3;
      char* mask = (char*)(long)(rp+disp_frame_size-bytes_in_mask);

      ikptr* fp = (ikptr*)(long)(top + framesize);
      long int i;
      for(i=0; i<bytes_in_mask; i++, fp-=8){
        unsigned char m = mask[i];
#if DEBUG_STACK
        fprintf(stderr, "m[%ld]=0x%x\n", i, m);
#endif
        if(m & 0x01) { fp[-0] = add_object(gc, fp[-0], "frame0"); }
        if(m & 0x02) { fp[-1] = add_object(gc, fp[-1], "frame1"); }
        if(m & 0x04) { fp[-2] = add_object(gc, fp[-2], "frame2"); }
        if(m & 0x08) { fp[-3] = add_object(gc, fp[-3], "frame3"); }
        if(m & 0x10) { fp[-4] = add_object(gc, fp[-4], "frame4"); }
        if(m & 0x20) { fp[-5] = add_object(gc, fp[-5], "frame5"); }
        if(m & 0x40) { fp[-6] = add_object(gc, fp[-6], "frame6"); }
        if(m & 0x80) { fp[-7] = add_object(gc, fp[-7], "frame7"); }
      }
    }
    top += framesize;
  }
  if(top != end){
    fprintf(stderr, "frames did not match up 0x%016lx .. 0x%016lx\n",
        (long int) top, (long int) end);
    exit(EXIT_FAILURE);
  }
  if(DEBUG_STACK){
    fprintf(stderr, "done with stack!\n");
  }
}


static void
add_list(gc_t* gc, unsigned int t, ikptr x, ikptr* loc){
  int collect_gen = gc->collect_gen;
  while(1){
    ikptr fst = ref(x, off_car);
    ikptr snd = ref(x, off_cdr);
    ikptr y;
    if((t & type_mask) != weak_pairs_type){
      y = gc_alloc_new_pair(gc) + pair_tag;
    } else {
      y = gc_alloc_new_weak_pair(gc) + pair_tag;
    }
    *loc = y;
    ref(x,off_car) = forward_ptr;
    ref(x,off_cdr) = y;
    ref(y,off_car) = fst;
    int stag = IK_TAGOF(snd);
    if(stag == pair_tag){
      if(ref(snd, -pair_tag) == forward_ptr){
        ref(y, off_cdr) = ref(snd, wordsize-pair_tag);
        return;
      }
      else {
        t = gc->segment_vector[page_index(snd)];
        int gen = t & gen_mask;
        if(gen > collect_gen){
          ref(y, off_cdr) = snd;
          return;
        } else {
          x = snd;
          loc = (ikptr*)(long)(y + off_cdr);
          /* don't return */
        }
      }
    }
    else if(   (stag == immediate_tag)
            || (stag == 0)
            || (stag == (1<<fx_shift))) {
      ref(y,off_cdr) = snd;
      return;
    }
    else if (ref(snd, -stag) == forward_ptr){
      ref(y, off_cdr) = ref(snd, wordsize-stag);
      return;
    }
    else {
      ref(y, off_cdr) = add_object(gc, snd, "add_list");
      return;
    }
  }
}


static ikptr
#ifndef NDEBUG
add_object_proc(gc_t* gc, ikptr x, char* caller) {
  caller = caller;
#else
add_object_proc(gc_t* gc, ikptr x) {
#endif
  if(is_fixnum(x)){
    return x;
  }
  assert(x != forward_ptr);
  int tag = IK_TAGOF(x);
  if(tag == immediate_tag){
    return x;
  }
  ikptr fst = ref(x, -tag);
  if(fst == forward_ptr){
    /* already moved */
    return ref(x, wordsize-tag);
  }
  unsigned int t = gc->segment_vector[page_index(x)];
  int gen = t & gen_mask;
  if(gen > gc->collect_gen){
    return x;
  }
  if(tag == pair_tag){
    ikptr y;
    add_list(gc, t, x, &y);
    return y;
  }
#if 0
  else if(tag == symbol_tag){
    //ikptr y = gc_alloc_new_ptr(IK_ALIGN(symbol_size),gen, gc) + symbol_tag;
    ikptr y = gc_alloc_new_symbol(gen, gc) + symbol_tag;
    ref(y, off_symbol_string)       = ref(x, off_symbol_string);
    ref(y, off_symbol_ustring)      = ref(x, off_symbol_ustring);
    ref(y, off_symbol_value)        = ref(x, off_symbol_value);
    ref(y, off_symbol_plist)        = ref(x, off_symbol_plist);
    ref(y, off_symbol_system_value) = ref(x, off_symbol_system_value);
    ref(y, off_symbol_code)         = ref(x, off_symbol_code);
    ref(y, off_symbol_errcode)      = ref(x, off_symbol_errcode);
    ref(y, off_symbol_unused)       = 0;
    ref(x, -symbol_tag) = forward_ptr;
    ref(x, wordsize-symbol_tag) = y;
#if accounting
      symbol_count++;
#endif
    return y;
  }
#endif
  else if(tag == closure_tag){
    ikptr size =
      disp_closure_data +
      ref(fst, disp_code_freevars - disp_code_data);
    if(size > 1024){
      fprintf(stderr, "large closure size=0x%016lx\n", (long)size);
    }
    ikptr asize = IK_ALIGN(size);
    ikptr y = gc_alloc_new_ptr(asize, gc) + closure_tag;
    ref(y, asize-closure_tag-wordsize) = 0;
    memcpy((char*)(long)(y-closure_tag),
           (char*)(long)(x-closure_tag),
           size);
    ref(y,-closure_tag) = add_code_entry(gc, ref(y,-closure_tag));
    ref(x,-closure_tag) = forward_ptr;
    ref(x,wordsize-closure_tag) = y;
#if accounting
    closure_count++;
#endif
    return y;
  }
  else if(tag == vector_tag){
    if(is_fixnum(fst)){
      /* real vector */
      //fprintf(stderr, "X=0x%08x, FST=0x%08x\n", (int)x, (int)fst);
      ikptr size = fst;
      assert(((long)size) >= 0);
      ikptr memreq = IK_ALIGN(size + disp_vector_data);
      if(memreq >= pagesize){
        if((t & large_object_mask) == large_object_tag){
          enqueue_large_ptr(x-vector_tag, size+disp_vector_data, gc);
          return x;
        } else {
          ikptr y = gc_alloc_new_large_ptr(size+disp_vector_data, gc)
                   + vector_tag;
          ref(y, disp_vector_length-vector_tag) = fst;
          ref(y, memreq-vector_tag-wordsize) = 0;
          memcpy((char*)(long)(y+off_vector_data),
                 (char*)(long)(x+off_vector_data),
                 size);
          ref(x,-vector_tag) = forward_ptr;
          ref(x,wordsize-vector_tag) = y;
          return y;
        }
      } else {
        ikptr y = gc_alloc_new_ptr(memreq, gc) + vector_tag;
        ref(y, disp_vector_length-vector_tag) = fst;
        ref(y, memreq-vector_tag-wordsize) = 0;
        memcpy((char*)(long)(y+off_vector_data),
               (char*)(long)(x+off_vector_data),
               size);
        ref(x,-vector_tag) = forward_ptr;
        ref(x,wordsize-vector_tag) = y;
        return y;
      }
#if accounting
      vector_count++;
#endif
    }
    else if(fst == symbol_record_tag){
      ikptr y = gc_alloc_new_symbol_record(gc) + record_tag;
      ref(y, -record_tag)               = symbol_record_tag;
      ref(y, off_symbol_record_string)  = ref(x, off_symbol_record_string);
      ref(y, off_symbol_record_ustring) = ref(x, off_symbol_record_ustring);
      ref(y, off_symbol_record_value)   = ref(x, off_symbol_record_value);
      ref(y, off_symbol_record_proc)    = ref(x, off_symbol_record_proc);
      ref(y, off_symbol_record_plist)   = ref(x, off_symbol_record_plist);
      ref(x, -record_tag) = forward_ptr;
      ref(x, wordsize-record_tag) = y;
      return y;
    }
    else if(IK_TAGOF(fst) == rtd_tag){
      /* struct / record */
      ikptr size = ref(fst, off_rtd_length);
      if(size & ((1<<align_shift)-1)) {
        /* size = n * object_alignment + 4 =>
           memreq = n * object_alignment + 8
                  = (n+1) * object_alignment  => aligned */
        ikptr y = gc_alloc_new_ptr(size+wordsize, gc) + vector_tag;
        ref(y, -vector_tag) = fst;
        {
          ikptr i;
          ikptr p = y+disp_record_data-vector_tag;
          ikptr q = x+disp_record_data-vector_tag;
          ref(p, 0) = ref(q, 0);
          for(i=wordsize; i<size; i+=(2*wordsize)){
            ref(p, i) = ref(q, i);
            ref(p, i+wordsize) = ref(q, i+wordsize);
          }
        }
        ref(x,-vector_tag) = forward_ptr;
        ref(x,wordsize-vector_tag) = y;
        return y;
      } else {
        /* size = n * object_alignment =>
           memreq = n * object_alignment + 4 + 4 (pad) */
        ikptr y = gc_alloc_new_ptr(size+(2*wordsize), gc) + vector_tag;
        ref(y, -vector_tag) = fst;
        {
          ikptr i;
          ikptr p = y+disp_record_data-vector_tag;
          ikptr q = x+disp_record_data-vector_tag;
          for(i=0; i<size; i+=(2*wordsize)){
            ref(p, i) = ref(q, i);
            ref(p, i+wordsize) = ref(q, i+wordsize);
          }
        }
        ref(y, size+disp_record_data-vector_tag) = 0;
        ref(x,-vector_tag) = forward_ptr;
        ref(x,wordsize-vector_tag) = y;
        return y;
      }
    }
    else if(fst == code_tag){
      ikptr entry = x + off_code_data;
      ikptr new_entry = add_code_entry(gc, entry);
      return new_entry - off_code_data;
    }
    else if(fst == continuation_tag){
      ikptr top = ref(x, off_continuation_top);
      ikptr size =  ref(x, off_continuation_size);
#ifndef NDEBUG
      if(size > 4096){
        fprintf(stderr, "large cont size=0x%016lx\n", size);
      }
#endif
      ikptr next = ref(x, off_continuation_next);
      ikptr y = gc_alloc_new_ptr(continuation_size, gc) + vector_tag;
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ikptr new_top = gc_alloc_new_data(IK_ALIGN(size), gc);
      memcpy((char*)(long)new_top,
             (char*)(long)top,
             size);
      collect_stack(gc, new_top, new_top + size);
      ref(y, -vector_tag) = continuation_tag;
      ref(y, off_continuation_top) = new_top;
      ref(y, off_continuation_size) = (ikptr) size;
      ref(y, off_continuation_next) = next;
#if accounting
      continuation_count++;
#endif
      return y;
    }
    else if(fst == system_continuation_tag) {
      ikptr y = gc_alloc_new_data(system_continuation_size, gc) + vector_tag;
      ikptr top = ref(x, disp_system_continuation_top - vector_tag);
      ikptr next = ref(x, disp_system_continuation_next - vector_tag);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ref(y, -vector_tag) = fst;
      ref(y, disp_system_continuation_top - vector_tag) = top;
      ref(y, disp_system_continuation_next - vector_tag) =
        add_object(gc, next, "next_k");
      return y;
    }
    else if(IK_TAGOF(fst) == pair_tag){
      /* tcbucket */
      ikptr y = gc_alloc_new_ptr(tcbucket_size, gc) + vector_tag;
      ref(y,off_tcbucket_tconc) = fst;
      ikptr key = ref(x, off_tcbucket_key);
      ref(y,off_tcbucket_key) = key;
      ref(y,off_tcbucket_val) = ref(x, off_tcbucket_val);
      ref(y,off_tcbucket_next) = ref(x, off_tcbucket_next);
      if((! is_fixnum(key)) && (IK_TAGOF(key) != immediate_tag)){
        int gen = gc->segment_vector[page_index(key)] & gen_mask;
        if(gen <= gc->collect_gen){
          /* key will be moved */
          gc_tconc_push(gc, y);
        }
      }
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      return y;
    }
    else if((((long int)fst) & port_mask) == port_tag){
      ikptr y = gc_alloc_new_ptr(port_size, gc) + vector_tag;
      ref(y, -vector_tag) = fst;
      long int i;
      for(i=wordsize; i<port_size; i+=wordsize){
        ref(y, i-vector_tag) = ref(x, i-vector_tag);
      }
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      return y;
    }
    else if(fst == flonum_tag){
      ikptr new = gc_alloc_new_data(flonum_size, gc) + vector_tag;
      ref(new, -vector_tag) = flonum_tag;
      FLONUM_DATA(new) = FLONUM_DATA(x);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = new;
      return new;
    }
    else if((fst & bignum_mask) == bignum_tag){
      long int len = ((unsigned long int)fst) >> bignum_length_shift;
      long int memreq = IK_ALIGN(disp_bignum_data + len*wordsize);
      ikptr new = gc_alloc_new_data(memreq, gc) + vector_tag;
      memcpy((char*)(long)(new-vector_tag),
             (char*)(long)(x-vector_tag),
             memreq);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = new;
      return new;
    }
    else if(fst == ratnum_tag){
      ikptr y = gc_alloc_new_data(ratnum_size, gc) + vector_tag;
      ikptr num = ref(x, disp_ratnum_num-vector_tag);
      ikptr den = ref(x, disp_ratnum_den-vector_tag);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ref(y, -vector_tag) = fst;
      ref(y, disp_ratnum_num-vector_tag) = add_object(gc, num, "num");
      ref(y, disp_ratnum_den-vector_tag) = add_object(gc, den, "den");
      return y;
    }
    else if(fst == compnum_tag){
      ikptr y = gc_alloc_new_data(compnum_size, gc) + vector_tag;
      ikptr rl = ref(x, disp_compnum_real-vector_tag);
      ikptr im = ref(x, disp_compnum_imag-vector_tag);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ref(y, -vector_tag) = fst;
      ref(y, disp_compnum_real-vector_tag) = add_object(gc, rl, "real");
      ref(y, disp_compnum_imag-vector_tag) = add_object(gc, im, "imag");
      return y;
    }
    else if(fst == cflonum_tag){
      ikptr y = gc_alloc_new_data(cflonum_size, gc) + vector_tag;
      ikptr rl = ref(x, disp_cflonum_real-vector_tag);
      ikptr im = ref(x, disp_cflonum_imag-vector_tag);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ref(y, -vector_tag) = fst;
      ref(y, disp_cflonum_real-vector_tag) = add_object(gc, rl, "real");
      ref(y, disp_cflonum_imag-vector_tag) = add_object(gc, im, "imag");
      return y;
    }
    else if(fst == pointer_tag){
      ikptr y = gc_alloc_new_data(pointer_size, gc) + vector_tag;
      ref(y, -vector_tag) = pointer_tag;
      ref(y, wordsize-vector_tag) = ref(x, wordsize-vector_tag);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      return y;
    }
    else {
      fprintf(stderr, "unhandled vector with fst=0x%016lx\n",
               (long int)fst);
      assert(0);
      exit(EXIT_FAILURE);
    }
  }
  else if(tag == string_tag){
    if(is_fixnum(fst)){
      long int strlen = unfix(fst);
      long int memreq = IK_ALIGN(strlen*string_char_size + disp_string_data);
      ikptr new_str = gc_alloc_new_data(memreq, gc) + string_tag;
      ref(new_str, off_string_length) = fst;
      memcpy((char*)(long)(new_str+off_string_data),
             (char*)(long)(x + off_string_data),
             strlen*string_char_size);
      ref(x, -string_tag) = forward_ptr;
      ref(x, wordsize-string_tag) = new_str;
#if accounting
      string_count++;
#endif
      return new_str;
    }
    else {
      fprintf(stderr, "unhandled string 0x%016lx with fst=0x%016lx\n",
          (long int)x, (long int)fst);
      exit(EXIT_FAILURE);
    }
  }
  else if(tag == bytevector_tag){
    long int len = unfix(fst);
    long int memreq = IK_ALIGN(len + disp_bytevector_data + 1);
    ikptr new_bv = gc_alloc_new_data(memreq, gc) + bytevector_tag;
    ref(new_bv, off_bytevector_length) = fst;
    memcpy((char*)(long)(new_bv+off_bytevector_data),
           (char*)(long)(x + off_bytevector_data),
           len + 1);
    ref(x, -bytevector_tag) = forward_ptr;
    ref(x, wordsize-bytevector_tag) = new_bv;
    return new_bv;
  }
  fprintf(stderr, "unhandled tag: %d\n", tag);
  exit(EXIT_FAILURE);
}

static void
relocate_new_code(ikptr x, gc_t* gc){
  ikptr relocvector = ref(x, disp_code_reloc_vector);
  relocvector = add_object(gc, relocvector, "relocvec");
  ref(x, disp_code_reloc_vector) = relocvector;
  ref(x, disp_code_annotation) =
    add_object(gc, ref(x, disp_code_annotation), "annotation");
  ikptr relocsize = ref(relocvector, off_vector_length);
  ikptr p = relocvector + off_vector_data;
  ikptr q = p + relocsize;
  ikptr code = x + disp_code_data;
  while(p < q){
    long int r = unfix(ref(p, 0));
    long int tag = r & 3;
    long int code_off = r >> 2;
    if(tag == 0){
      /* undisplaced pointer */
#ifndef NDEBUG
     // fprintf(stderr, "r=0x%08x code_off=%d reloc_size=0x%08x\n",
     //     r, code_off, relocsize);
#endif
      ikptr old_object = ref(p, wordsize);
      ikptr new_object = add_object(gc, old_object, "reloc1");
      ref(code, code_off) = new_object;
      p += (2*wordsize);
    }
    else if(tag == 2){
      /* displaced pointer */
      long int obj_off = unfix(ref(p, wordsize));
      ikptr old_object = ref(p, 2*wordsize);
      ikptr new_object = add_object(gc, old_object, "reloc2");
      ref(code, code_off) = new_object + obj_off;
      p += (3 * wordsize);
    }
    else if(tag == 3){
      /* displaced relative pointer */
      long int obj_off = unfix(ref(p, wordsize));
      ikptr obj = ref(p, 2*wordsize);
#ifndef NDEBUG
      //fprintf(stderr, "obj=0x%08x, obj_off=0x%08x\n", (int)obj,
      //    obj_off);
#endif
      obj = add_object(gc, obj, "reloc3");
      ikptr displaced_object = obj + obj_off;
      long int next_word = code + code_off + 4;
      ikptr relative_distance = displaced_object - (long int)next_word;
      if(((long int)relative_distance) != ((long)((int)relative_distance))){
        fprintf(stderr, "relocation error with relative=0x%016lx\n",
            relative_distance);
        exit(EXIT_FAILURE);
      }
      *((int*)(code+code_off)) = (int)relative_distance;
      p += (3*wordsize);
    }
    else if(tag == 1){
      /* do nothing */
      p += (2 * wordsize);
    }
    else {
      fprintf(stderr, "invalid rtag %ld in 0x%016lx\n", tag, r);
      exit(EXIT_FAILURE);
    }
  }
}



static void
collect_loop(gc_t* gc){
  int done;
  do{
    done = 1;
    { /* scan the pending pairs pages */
      qupages_t* qu = gc->queues[meta_pair];
      if(qu){
        done = 0;
        gc->queues[meta_pair] = 0;
        do{
          ikptr p = qu->p;
          ikptr q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0), "loop");
            p += (2*wordsize);
          }
          qupages_t* next = qu->next;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending pointer pages */
      qupages_t* qu = gc->queues[meta_ptrs];
      if(qu){
        done = 0;
        gc->queues[meta_ptrs] = 0;
        do{
          ikptr p = qu->p;
          ikptr q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0), "pending");
            p += wordsize;
          }
          qupages_t* next = qu->next;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending symbol pages */
      qupages_t* qu = gc->queues[meta_symbol];
      if(qu){
        done = 0;
        gc->queues[meta_symbol] = 0;
        do{
          ikptr p = qu->p;
          ikptr q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0), "symbols");
            p += wordsize;
          }
          qupages_t* next = qu->next;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending code objects */
      qupages_t* codes = gc->queues[meta_code];
      if(codes){
        gc->queues[meta_code] = 0;
        done = 0;
        do{
          ikptr p = codes->p;
          ikptr q = codes->q;
          while(p < q){
            relocate_new_code(p, gc);
            alloc_code_count--;
            p += IK_ALIGN(disp_code_data + unfix(ref(p, disp_code_code_size)));
          }
          qupages_t* next = codes->next;
          ik_free(codes, sizeof(qupages_t));
          codes = next;
        } while(codes);
      }
    }
    {/* see if there are any remaining in the main ptr segment */
      {
        meta_t* meta = &gc->meta[meta_pair];
        ikptr p = meta->aq;
        ikptr q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            while(p < q){
              ref(p,0) = add_object(gc, ref(p,0), "rem");
              p += (2*wordsize);
            }
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
      {
        meta_t* meta = &gc->meta[meta_symbol];
        ikptr p = meta->aq;
        ikptr q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            while(p < q){
              ref(p,0) = add_object(gc, ref(p,0), "sym");
              p += wordsize;
            }
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
      {
        meta_t* meta = &gc->meta[meta_ptrs];
        ikptr p = meta->aq;
        ikptr q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            while(p < q){
              ref(p,0) = add_object(gc, ref(p,0), "rem2");
              p += wordsize;
            }
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
      {
        meta_t* meta = &gc->meta[meta_code];
        ikptr p = meta->aq;
        ikptr q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            do{
              alloc_code_count--;
              relocate_new_code(p, gc);
              p += IK_ALIGN(disp_code_data + unfix(ref(p, disp_code_code_size)));
            } while (p < q);
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
    }
    /* phew */
  } while (! done);
  {
    /* zero out remaining pointers */
    /* FIXME: did you hear of code reuse? */
    {
      meta_t* meta = &gc->meta[meta_pair];
      ikptr p = meta->ap;
      ikptr q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_symbol];
      ikptr p = meta->ap;
      ikptr q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_ptrs];
      ikptr p = meta->ap;
      ikptr q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_weak];
      ikptr p = meta->ap;
      ikptr q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_code];
      ikptr p = meta->ap;
      ikptr q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
  }
}

static void
fix_weak_pointers(gc_t* gc){
  unsigned int* segment_vec = gc->segment_vector;
  ikpcb* pcb = gc->pcb;
  long int lo_idx = page_index(pcb->memory_base);
  long int hi_idx = page_index(pcb->memory_end);
  long int i = lo_idx;
  int collect_gen = gc->collect_gen;
  while(i < hi_idx){
    unsigned int t = segment_vec[i];
    if((t & (type_mask|new_gen_mask)) ==
        (weak_pairs_type|new_gen_tag)){
      //int gen = t & gen_mask;
      if (1) { //(gen > collect_gen){
        ikptr p = (ikptr)(i << pageshift);
        ikptr q = p + pagesize;
        while(p < q){
          ikptr x = ref(p, 0);
          if(! is_fixnum(x)){
            int tag = IK_TAGOF(x);
            if(tag != immediate_tag){
              ikptr fst = ref(x, -tag);
              if(fst == forward_ptr){
                ref(p, 0) = ref(x, wordsize-tag);
              } else {
                int x_gen = segment_vec[page_index(x)] & gen_mask;
                if(x_gen <= collect_gen){
                  ref(p, 0) = bwp_object;
                }
              }
            }
          }
          p += (2*wordsize);
        }
      }
    }
    i++;
  }
}

static unsigned int dirty_mask[generation_count] = {
  0x88888888,
  0xCCCCCCCC,
  0xEEEEEEEE,
  0xFFFFFFFF,
  0x00000000
};


static unsigned int cleanup_mask[generation_count] = {
  0x00000000,
  0x88888888,
  0xCCCCCCCC,
  0xEEEEEEEE,
  0xFFFFFFFF
};



static void
scan_dirty_pointers_page(gc_t* gc, long int page_idx, int mask){
  unsigned int* segment_vec = (unsigned int*)(long)gc->segment_vector;
  unsigned int* dirty_vec = (unsigned int*)(long)gc->pcb->dirty_vector;
  unsigned int t = segment_vec[page_idx];
  unsigned int d = dirty_vec[page_idx];
  unsigned int masked_d = d & mask;
  ikptr p = (ikptr)(page_idx << pageshift);
  int j;
  unsigned int new_d = 0;
  for(j=0; j<cards_per_page; j++){
    if(masked_d & (0xF << (j*meta_dirty_shift))){
      /* dirty card */
      ikptr q = p + cardsize;
      unsigned int card_d = 0;
      while(p < q){
        ikptr x = ref(p, 0);
        if(is_fixnum(x) || (IK_TAGOF(x) == immediate_tag)){
          /* do nothing */
        } else {
          ikptr y = add_object(gc, x, "nothing");
          segment_vec = gc->segment_vector;
          ref(p, 0) = y;
          card_d = card_d | segment_vec[page_index(y)];
        }
        p += wordsize;
      }
      card_d = (card_d & meta_dirty_mask) >> meta_dirty_shift;
      new_d = new_d | (card_d<<(j*meta_dirty_shift));
    } else {
      p += cardsize;
      new_d = new_d | (d & (0xF << (j*meta_dirty_shift)));
    }
  }
  dirty_vec = (unsigned int*)(long)gc->pcb->dirty_vector;
  new_d = new_d & cleanup_mask[t & gen_mask];
  dirty_vec[page_idx] = new_d;
}

static void
scan_dirty_code_page(gc_t* gc, long int page_idx){
  ikptr p = (ikptr)(page_idx << pageshift);
  ikptr start = p;
  ikptr q = p + pagesize;
  unsigned int* segment_vec = (unsigned int*)(long)gc->segment_vector;
  unsigned int* dirty_vec = (unsigned int*)(long)gc->pcb->dirty_vector;
  //unsigned int d = dirty_vec[page_idx];
  unsigned int t = segment_vec[page_idx];
  //unsigned int masked_d = d & mask;
  unsigned int new_d = 0;
  while(p < q){
    if(ref(p, 0) != code_tag){
      p = q;
    }
    else {
      long int j = ((long int)p - (long int)start) / cardsize;
      long int code_size = unfix(ref(p, disp_code_code_size));
      relocate_new_code(p, gc);
      segment_vec = gc->segment_vector;
      ikptr rvec = ref(p, disp_code_reloc_vector);
      ikptr len = ref(rvec, off_vector_length);
      assert(((long)len) >= 0);
      unsigned long int i;
      unsigned long int code_d = segment_vec[page_index(rvec)];
      for(i=0; i<len; i+=wordsize){
        ikptr r = ref(rvec, i+off_vector_data);
        if(is_fixnum(r) || (IK_TAGOF(r) == immediate_tag)){
          /* do nothing */
        } else {
          r = add_object(gc, r, "nothing2");
          segment_vec = gc->segment_vector;
          code_d = code_d | segment_vec[page_index(r)];
        }
      }
      new_d = new_d | (code_d<<(j*meta_dirty_shift));
      p += IK_ALIGN(code_size + disp_code_data);
    }
  }
  dirty_vec = (unsigned int*)(long)gc->pcb->dirty_vector;
  new_d = new_d & cleanup_mask[t & gen_mask];
  dirty_vec[page_idx] = new_d;
}





static void
scan_dirty_pages(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  long int lo_idx = page_index(pcb->memory_base);
  long int hi_idx = page_index(pcb->memory_end);
  unsigned int* dirty_vec = (unsigned int*)(long)pcb->dirty_vector;
  unsigned int* segment_vec = (unsigned int*)(long)pcb->segment_vector;
  int collect_gen = gc->collect_gen;
  unsigned int mask = dirty_mask[collect_gen];
  long int i = lo_idx;
  while(i < hi_idx){
    unsigned int d = dirty_vec[i];
    if(d & mask){
      unsigned int t = segment_vec[i];
      int tgen = t & gen_mask;
      if(tgen > collect_gen){
        int type = t & type_mask;
        if(type == pointers_type){
          scan_dirty_pointers_page(gc, i, mask);
          dirty_vec = (unsigned int*)(long)pcb->dirty_vector;
          segment_vec = (unsigned int*)(long)pcb->segment_vector;
        }
        else if(type == symbols_type){
          scan_dirty_pointers_page(gc, i, mask);
          dirty_vec = (unsigned int*)(long)pcb->dirty_vector;
          segment_vec = (unsigned int*)(long)pcb->segment_vector;
        }
        else if (type == weak_pairs_type){
          scan_dirty_pointers_page(gc, i, mask);
          dirty_vec = (unsigned int*)(long)pcb->dirty_vector;
          segment_vec = (unsigned int*)(long)pcb->segment_vector;
        }
        else if (type == code_type){
          scan_dirty_code_page(gc, i);
          dirty_vec = (unsigned int*)(long)pcb->dirty_vector;
          segment_vec = (unsigned int*)(long)pcb->segment_vector;
        }
        else if (t & scannable_mask) {
          fprintf(stderr, "BUG: unhandled scan of type 0x%08x\n", t);
          exit(EXIT_FAILURE);
        }
      }
    }
    i++;
  }
}




static void
deallocate_unused_pages(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  int collect_gen =  gc->collect_gen;
  unsigned int* segment_vec = pcb->segment_vector;
  ikptr memory_base = pcb->memory_base;
  ikptr memory_end = pcb->memory_end;
  ikptr lo_idx = page_index(memory_base);
  ikptr hi_idx = page_index(memory_end);
  ikptr i = lo_idx;
  while(i < hi_idx){
    unsigned int t = segment_vec[i];
    if(t & dealloc_mask){
      int gen = t & old_gen_mask;
      if(gen <= collect_gen){
        /* we're interested */
        if(t & new_gen_mask){
          /* do nothing yet */
        } else {
          ik_munmap_from_segment((ikptr)(i<<pageshift),pagesize,pcb);
        }
      }
    }
    i++;
  }
}


static void
fix_new_pages(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  unsigned int* segment_vec = pcb->segment_vector;
  ikptr memory_base = pcb->memory_base;
  ikptr memory_end = pcb->memory_end;
  ikptr lo_idx = page_index(memory_base);
  ikptr hi_idx = page_index(memory_end);
  ikptr i = lo_idx;
  while(i < hi_idx){
    segment_vec[i] &= ~new_gen_mask;
    /*
    unsigned int t = segment_vec[i];
    if(t & new_gen_mask){
      segment_vec[i] = t & ~new_gen_mask;
    }
    */
    i++;
  }
}

static void
add_one_tconc(ikpcb* pcb, ikptr p){
  ikptr tcbucket = ref(p,0);
  ikptr tc = ref(tcbucket, off_tcbucket_tconc);
  assert(IK_TAGOF(tc) == pair_tag);
  ikptr d = ref(tc, off_cdr);
  assert(IK_TAGOF(d) == pair_tag);
  ikptr new_pair = p + pair_tag;
  ref(d, off_car) = tcbucket;
  ref(d, off_cdr) = new_pair;
  ref(new_pair, off_car) = false_object;
  ref(new_pair, off_cdr) = false_object;
  ref(tc, off_cdr) = new_pair;
  ref(tcbucket, -vector_tag) = (ikptr)(tcbucket_size - wordsize);
  ((int*)(long)pcb->dirty_vector)[page_index(tc)] = -1;
  ((int*)(long)pcb->dirty_vector)[page_index(d)] = -1;
}

static void
gc_add_tconcs(gc_t* gc){
  if(gc->tconc_base == 0){
    return;
  }
  ikpcb* pcb = gc->pcb;
  {
    ikptr p = gc->tconc_base;
    ikptr q = gc->tconc_ap;
    while(p < q){
      add_one_tconc(pcb, p);
      p += 2*wordsize;
    }
  }
  ikpages* qu = gc->tconc_queue;
  while(qu){
    ikptr p = qu->base;
    ikptr q = p + qu->size;
    while(p < q){
      add_one_tconc(pcb, p);
      p += 2*wordsize;
    }
    ikpages* next = qu->next;
    ik_free(qu, sizeof(ikpages));
    qu = next;
  }
}


