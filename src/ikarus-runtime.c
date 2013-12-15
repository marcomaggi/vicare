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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"
#include <dirent.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>


/** --------------------------------------------------------------------
 ** Prototypes and internal definitions.
 ** ----------------------------------------------------------------- */

/* Total number of Vicare pages currently allocated with "ik_mmap()". */
static int total_allocated_pages = 0;

/* Total number of bytes currently allocated with "ik_malloc()". */
static int total_malloced = 0;

static void set_page_range_type(ikptr base, ik_ulong size, uint32_t type, ikpcb* pcb);
static void extend_page_vectors_maybe (ikptr base, ik_ulong size, ikpcb* pcb);


/** --------------------------------------------------------------------
 ** C language like memory allocation.
 ** ----------------------------------------------------------------- */

void *
ik_malloc (int size)
{
  void* x = malloc(size);
  if (NULL == x)
    ik_abort("malloc failed: %s", strerror(errno));
  total_malloced += size;
  return x;
}
void
ik_free (void* x, int size)
{
  total_malloced -= size;
  free(x);
}


/** --------------------------------------------------------------------
 ** Scheme language memory allocation, basic memory mapping.
 ** ----------------------------------------------------------------- */

ikptr
ik_mmap (ik_ulong size)
/* All memory allocation is performed by this function. */
{
  ik_ulong npages        = IK_MINIMUM_PAGES_NUMBER_FOR_SIZE(size);
  ik_ulong mapsize       = IK_MMAP_ALLOCATION_SIZE_FOR_PAGES(npages);
  total_allocated_pages += npages;
  if (0) {
    ik_debug_message("%s: size=%lu, pages=%lu, mapsize=%lu, size/PGSIZE=%lu, mapsize/PGSIZE=%lu\n",
		     __func__, size, npages, mapsize, size/IK_PAGESIZE, mapsize/IK_PAGESIZE);
  }
  assert(size == mapsize);
#ifndef __CYGWIN__
  char* mem = mmap(0, mapsize, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
  /* FIXME Check if in range.  (Abdulaziz Ghuloum) */
  if (mem == MAP_FAILED)
    ik_abort("mapping (0x%lx bytes) failed: %s", size, strerror(errno));
#else
  char* mem = win_mmap(mapsize);
#endif
  memset(mem, -1, mapsize);
#ifdef VICARE_DEBUGGING
  ik_debug_message("%s: 0x%016lx .. 0x%016lx\n", __func__, (long)mem, ((long)(mem))+mapsize-1);
#endif
  return (ikptr)(long)mem;
}
void
ik_munmap (ikptr mem, ik_ulong size)
/* All memory relese is performed by this function. */
{
  ik_ulong pages   = IK_MINIMUM_PAGES_NUMBER_FOR_SIZE(size);
  ik_ulong mapsize = IK_MMAP_ALLOCATION_SIZE_FOR_PAGES(pages);
  assert(size == mapsize);
  assert(((-IK_PAGESIZE) & (int)mem) == (int)mem);
  total_allocated_pages -= pages;
#ifndef __CYGWIN__
  int err = munmap((char*)mem, mapsize);
  if (err)
    ik_abort("ik_munmap failed: %s", strerror(errno));
#else
  win_munmap((char*)mem, mapsize);
#endif
#ifdef VICARE_DEBUGGING
  ik_debug_message("%s: 0x%016lx .. 0x%016lx\n", __func__, (long)mem, ((long)(mem))+mapsize-1);
#endif
}


/** --------------------------------------------------------------------
 ** Memory mapping and tagging for garbage collection.
 ** ----------------------------------------------------------------- */

ikptr
ik_mmap_typed (ik_ulong size, uint32_t type, ikpcb* pcb)
{
  ikptr		base;
  if (size == IK_PAGESIZE) {
    /* If available, recycle a page from the cache. */
    ikpage *	pages = pcb->cached_pages;
    if (pages) {
      /* Extract the first page from the linked list of cached pages. */
      base		  = pages->base;
      pcb->cached_pages	  = pages->next;
      /* Prepend  the extracted  page  to the  linked  list of  uncached
	 pages. */
      pages->next	  = pcb->uncached_pages;
      pcb->uncached_pages = pages;
    } else {
      /* No cached page available: allocate a new page. */
      base = ik_mmap(size);
    }
  } else {
    base = ik_mmap(size);
  }
  extend_page_vectors_maybe(base, size, pcb);
  set_page_range_type(base, size, type, pcb);
  return base;
}
ikptr
ik_mmap_ptr (ik_ulong size, int gen, ikpcb* pcb)
{
  return ik_mmap_typed(size, pointers_mt|gen, pcb);
}
ikptr
ik_mmap_data (ik_ulong size, int gen, ikpcb* pcb)
{
  return ik_mmap_typed(size, data_mt|gen, pcb);
}
ikptr
ik_mmap_code (ik_ulong size, int gen, ikpcb* pcb)
{
  /* Why when allocating a code object  the first page is tagged as code
     and the subsequent pages as data?  Because we know that the all the
     slots  in  a code  object  containing  references to  other  Scheme
     objects are in  the first page, so when garbage  collecting we need
     to scan only the first page. */
  ikptr p = ik_mmap_typed(size, code_mt|gen, pcb);
  if (size > IK_PAGESIZE)
    set_page_range_type(p+IK_PAGESIZE, size-IK_PAGESIZE, data_mt|gen, pcb);
  return p;
}
ikptr
ik_mmap_mainheap (ik_ulong size, ikpcb* pcb)
/* Allocate a memory segment tagged as part of the Scheme heap. */
{
  return ik_mmap_typed(size, mainheap_mt, pcb);
}
static void
set_page_range_type (ikptr base, ik_ulong size, uint32_t type, ikpcb* pcb)
/* Set to TYPE all the entries in "pcb->segment_vector" corresponding to
   the memory block starting at BASE and SIZE bytes wide. */
{
  /* The PCB  fields "memory_base"  and "memory_end" delimit  the memory
     used by Scheme code; obviously an allocated segment must be in this
     range. */
  assert(base        >= pcb->memory_base);
  assert((base+size) <= pcb->memory_end);
  assert(size == IK_ALIGN_TO_NEXT_PAGE(size));
  uint32_t * p = pcb->segment_vector + IK_PAGE_INDEX(base);
  uint32_t * q = p                   + IK_PAGE_INDEX_RANGE(size);
  for (; p < q; ++p)
    *p = type;
}
static void
extend_page_vectors_maybe (ikptr base_ptr, ik_ulong size, ikpcb* pcb)
/* For garbage  collection purposes we  keep track of every  Vicare page
 * used by Scheme code in PCB's  dirty vector and segments vector.  When
 * a new memory block is allocated we must check if such vectors need to
 * be updated to reference it.
 *
 *   All the memory  used by Scheme code  must be in the  range from the
 * PCB members"memory_base" and "memory_end":
 *
               Scheme used memory
 *         |.......................|
 *    |--------------------------------------------| system memory
 *         ^                        ^
 *      memory_base             memory_end
 *
 * the dirty vector and segments vector  contain a slot for each page in
 * such range.
 *
 * This function only  updates the tracked range of memory,  it does NOT
 * tag the new memory in any way.
 */
{
  assert(size == IK_ALIGN_TO_NEXT_PAGE(size));
  ikptr end_ptr = base_ptr + size;
  if (base_ptr < pcb->memory_base) {
    ik_ulong new_lo_seg   = IK_SEGMENT_INDEX(base_ptr);
    ik_ulong old_lo_seg   = IK_SEGMENT_INDEX(pcb->memory_base);
    ik_ulong hi_seg       = IK_SEGMENT_INDEX(pcb->memory_end); /* unchanged */
    ik_ulong new_vec_size = (hi_seg - new_lo_seg) * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    ik_ulong old_vec_size = (hi_seg - old_lo_seg) * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    ik_ulong size_delta   = new_vec_size - old_vec_size;
    { /* Allocate a new  dirty vector.  The old slots go  to the tail of
	 the new  vector; the  head of  the new vector  is set  to zero,
	 which   means  the   corresponding   pages   are  marked   with
	 IK_PURE_WORD. */
      ikptr	new_dvec_base = ik_mmap(new_vec_size);
      bzero((char*)new_dvec_base, size_delta);
      memcpy((char*)(new_dvec_base + size_delta), (char*)pcb->dirty_vector_base, old_vec_size);
      ik_munmap((ikptr)pcb->dirty_vector_base, old_vec_size);
      pcb->dirty_vector_base = (uint32_t *)new_dvec_base;
      pcb->dirty_vector      = new_dvec_base - new_lo_seg * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    }
    { /* Allocate a new  segments vector.  The old slots go  to the tail
	 of the new vector;  the head of the new vector  is set to zero,
	 which   means  the   corresponding   pages   are  marked   with
	 "hole_mt". */
      ikptr	new_svec_base = ik_mmap(new_vec_size);
      bzero((char*)new_svec_base, size_delta);
      memcpy((char*)(new_svec_base + new_vec_size - old_vec_size), (char*)(pcb->segment_vector_base), old_vec_size);
      ik_munmap((ikptr)pcb->segment_vector_base, old_vec_size);
      pcb->segment_vector_base = (uint32_t *)new_svec_base;
      pcb->segment_vector      = (uint32_t *)(new_svec_base - new_lo_seg * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT);
    }
    pcb->memory_base = new_lo_seg * IK_SEGMENT_SIZE;
  } else if (end_ptr >= pcb->memory_end) {
    ik_ulong lo_seg       = IK_SEGMENT_INDEX(pcb->memory_base); /* unchanged */
    ik_ulong old_hi_seg   = IK_SEGMENT_INDEX(pcb->memory_end);
    ik_ulong new_hi_seg   = IK_SEGMENT_INDEX(end_ptr + IK_SEGMENT_SIZE - 1);
    ik_ulong new_vec_size = (new_hi_seg - lo_seg) * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    ik_ulong old_vec_size = (old_hi_seg - lo_seg) * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    ik_ulong size_delta   = new_vec_size - old_vec_size;
    { /* Allocate a new  dirty vector.  The old slots go  to the head of
	 the new  vector; the  tail of  the new vector  is set  to zero,
	 which   means  the   corresponding   pages   are  marked   with
	 IK_PURE_WORD. */
      ikptr new_dvec_base = ik_mmap(new_vec_size);
      memcpy((char*)new_dvec_base, (char*)pcb->dirty_vector_base, old_vec_size);
      bzero((char*)(new_dvec_base + old_vec_size), size_delta);
      ik_munmap((ikptr)pcb->dirty_vector_base, old_vec_size);
      pcb->dirty_vector_base = (uint32_t *)new_dvec_base;
      pcb->dirty_vector      = new_dvec_base - lo_seg * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    }
    { /* Allocate a new  segments vector.  The old slots go  to the head
	 of the new vector;  the tail of the new vector  is set to zero,
	 which   means  the   corresponding   pages   are  marked   with
	 "hole_mt". */
      ikptr new_svec_base = ik_mmap(new_vec_size);
      memcpy((char*)new_svec_base, (char*)pcb->segment_vector_base, old_vec_size);
      bzero((char*)(new_svec_base + old_vec_size), size_delta);
      ik_munmap((ikptr)pcb->segment_vector_base, old_vec_size);
      pcb->segment_vector_base = (uint32_t *) new_svec_base;
      pcb->segment_vector      = (uint32_t *) (new_svec_base - lo_seg * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT);
    }
    pcb->memory_end = new_hi_seg * IK_SEGMENT_SIZE;
  }
}


ikpcb*
ik_make_pcb (void)
{
  ikpcb * pcb = ik_malloc(sizeof(ikpcb));
  bzero(pcb, sizeof(ikpcb));

  /* The  Scheme heap  grows from  low memory  addresses to  high memory
   * addresses:
   *
   *     heap_base      growth         redline
   *         v         ------->           v
   *  lo mem |----------------------------+--------| hi mem
   *                       Scheme heap
   *         |.....................................| heap size
   *
   * when a Scheme  object is allocated on the heap  and its end crosses
   * the "red  line": either a garbage  collection runs, or the  heap is
   * enlarged.     See     the    functions     "ik_safe_alloc()"    and
   * "ik_unsafe_alloc()".
   */
  {
    pcb->heap_base          = ik_mmap(IK_HEAPSIZE);
    pcb->heap_size          = IK_HEAPSIZE;
    pcb->allocation_pointer = pcb->heap_base;
    pcb->allocation_redline = pcb->heap_base + IK_HEAPSIZE - IK_DOUBLE_PAGESIZE;
    /* Notice that below we will register the heap block in the segments
       vector. */
  }

  /* The Scheme  stack grows  from high memory  addresses to  low memory
   * addresses:
   *
   *    stack_base   redline    growth
   *         v          v      <-------
   *  lo mem |----------+--------------------------| hi mem
   *                       Scheme stack
   *         |.....................................| stack size
   *
   * when Scheme code execution uses  the stack crossing the "red line":
   * at the first subsequent function  call, the current Scheme stack is
   * stored away  in a Scheme continuation  and a new memory  segment is
   * allocated  and  installed as  Scheme  stack;  see for  example  the
   * "ik_stack_overflow()"  function.  When  the  function returns:  the
   * stored continuation  is reinstated  and execution continues  on the
   * old stack.
   *
   * The first stack frame starts from the end of the stack:
   *
   *    stack_base                   frame_pointer = frame_base
   *         v                                     v
   *  lo mem |-------------------------------------| hi mem
   *                       Scheme stack
   *
   * then, while nested  functions are called, new frames  are pushed on
   * the stack:
   *
   *    stack_base    frame_pointer            frame_base
   *         v             v                       v
   *  lo mem |-------------+-----------------------| hi mem
   *                  |....|
   *                           Scheme stack
   *
   * Notice how "pcb->frame_base" references a  word that is one-off the
   * end of the stack segment; so the first word in the stack is:
   *
   *    pcb->frame_base - wordsize
   *
   * Also,  when   C  code   is  running,   "pcb->frame_pointer"  always
   * references the highest  memory address in the  lowest function call
   * frame; the machine word referenced by "pcb->frame_pointer" contains
   * the return address of the last  function call.  When Scheme code is
   * entered: "pcb->frame_pointer" is stored  in the %esp register; when
   * Scheme   code  is   exited   the  %esp   register   is  stored   in
   * "pcb->frame_pointer".
   *
   * See the function "ik_exec_code()" for details about entering Scheme
   * code execution.
   */
  {
    pcb->stack_base	= ik_mmap(IK_STACKSIZE);
    pcb->stack_size	= IK_STACKSIZE;
    pcb->frame_pointer	= pcb->stack_base + pcb->stack_size;
    pcb->frame_base	= pcb->frame_pointer;
    if (IK_PROTECT_FROM_STACK_OVERFLOW) {
      /* Forbid reading  and writing in  the low-address memory  page of
       * the  stack  segment;  this  should  trigger  a  SIGSEGV  if  an
       * undetected  Scheme  stack  overflow happens.   Not  a  solution
       * against stack  overflows, but at  least it should  avoid memory
       * corruption.
       *
       *    stack_base                             frame_base
       *         v                                     v
       *  lo mem |-------------------------------------| hi mem
       *
       *         |.....|...............................|
       *       1st page         usable region
       *
       * This  configuration  must  be  repeated whenever  a  new  stack
       * segment is allocated because of detected stack overflow.
       */
      mprotect((void*)(ik_ulong)(pcb->stack_base), IK_PAGESIZE, PROT_NONE);
      pcb->frame_redline= pcb->stack_base + IK_DOUBLE_PAGESIZE + IK_PAGESIZE;
    } else {
      pcb->frame_redline= pcb->stack_base + IK_DOUBLE_PAGESIZE;
    }
    /* Notice  that  below we  will  register  the  stack block  in  the
       segments vector. */
  }

  /* Allocate and  initialise the page  cache; see the  documentation of
   * the PCB  struct for details.  We  link all the structs  in a linked
   * list, from the last slot to the first.
   *
   *          next            next
   *      -----------     -----------
   *     |           |   |           |
   *     v           |   v           |
   *   |---+---|---+---|---+---|---+---|---+---| allocated array
   *         |   ^           |   ^           |
   *         v   |           |   |           |
   *       NULL   -----------     -----------
   *                 next            next
   *   |.......|.......|.......|.......|.......|
   *    ikpage0 ikpage1 ikpage2 ikpage3 ikpage4
   */
  {
    ikpage *	cur  = (ikpage*)ik_mmap(IK_PAGE_CACHE_SIZE_IN_BYTES);
    ikpage *	past = cur + IK_PAGE_CACHE_NUM_OF_SLOTS;
    ikpage *	prev = NULL;
    pcb->cached_pages_base = (ikptr)cur;
    pcb->cached_pages_size = IK_PAGE_CACHE_SIZE_IN_BYTES;
    for (; cur < past; ++cur) {
      cur->next = prev;
      prev = cur;
    }
    pcb->cached_pages   = NULL;
    pcb->uncached_pages = prev;
  }

  /* Allocate and initialise the dirty vector and the segment vector.
   *
   * We forsee two possible scenarios:
   *
   *       Scheme heap              Scheme stack
   *    |--------------+----------+-------------| interesting memory
   *  begin              (unused?)             end
   *
   *      Scheme stack              Scheme heap
   *    |--------------+----------+-------------| interesting memory
   *  begin              (unused?)             end
   *
   * We compute two addresses: "lo_mem"  which is guaranteed to be below
   * "begin"; "hi_mem" which is guaranteed to be above "end".
   *
   * The dirty vector
   * ----------------
   *
   * The "dirty  vector" is an  array of "unsigned" integers  (which are
   * meant to  be 32-bit words), one  for each memory page  allocated by
   * Vicare; given  a memory address used  by Vicare, it is  possible to
   * compute the  index of the  corresponding slot in the  dirty vector.
   * Each slot can be one of two states:
   *
   *    0 -	The page is pure.
   *   -1 -	The page is dirty: some Scheme object in it has been
   *            mutated after the last garbage collection.
   *
   * such state is used by the garbage collector to decide which page to
   * scan, see  the function "scan_dirty_pages()".  For  example: when a
   * machine word location is  modified by "set-car!", the corresponding
   * slot in the dirty vector is set to -1.
   *
   * Indexes in  the dirty vector  are *not* zero-based.  The  fields in
   * the PCB are:
   *
   *   dirty_vector_base -
   *      Pointer to  the first byte  of memory allocated for  the dirty
   *      vector.
   *
   *   dirty_vector -
   *      Pointer to  a memory  address that  can be  used to  index the
   *      slots  in the  dirty vector,  with indexes  computed from  the
   *      actual memory addresses used by Vicare.
   *
   * it's like this:
   *
   *         base_offset            slots
   *    |...................|....................|
   *    |-------------------|--|--|--|--|--|--|--|
   *     ^                   ^
   *     dirty_vector        dirty_vector_base
   *
   * the first  slot is *not* "dirty_vector[0]",  rather some expression
   * like "dirty_vector[734]", where  734 is the value  computed here in
   * "lo_seg_idx".
   *
   * The segment vector
   * ------------------
   *
   * The "segment vector" is an  array of "unsigned" integers (which are
   * meant to  be 32-bit words), one  for each memory page  allocated by
   * Vicare; given a memory address (tagged or untagged) used by Vicare,
   * it is  possible to compute the  index of the corresponding  slot in
   * the  segment vector.   Each integer  represents the  type of  usage
   * Vicare makes  of the  page, the  garbage collection  generation the
   * page is in, and other meta informations; some of the types (defined
   * in the internal header file) are:
   *
   *   0            -	Unused memory.
   *   mainheap_mt  -	Scheme heap memory.
   *   mainstack_mt -	Scheme stack memory.
   *
   * Indexes in the segment vector  are *not* zero-based.  The fields in
   * the PCB are:
   *
   *   segment_vector_base -
   *      Pointer to the first byte  of memory allocated for the segment
   *      vector.
   *
   *   segment_vector -
   *      Pointer to  a memory  address that  can be  used to  index the
   *      slots in  the segment vector,  with indexes computed  from the
   *      actual memory addresses used by Vicare.
   *
   * it's like this:
   *
   *         base_offset            slots
   *    |...................|....................|
   *    |-------------------|--|--|--|--|--|--|--|
   *     ^                   ^
   *     segment_vector      segment_vector_base
   *
   * the first slot is *not* "segment_vector[0]", rather some expression
   * like "segment_vector[734]", where 734 is the value computed here in
   * "lo_seg_idx".
   */
  {
    ikptr	lo_mem, hi_mem;
    ik_ulong	lo_seg_idx, hi_seg_idx, vec_size, base_offset;
    if (pcb->heap_base < pcb->stack_base) {
      lo_mem = pcb->heap_base - IK_PAGESIZE;
      hi_mem = pcb->stack_base + pcb->stack_size + IK_PAGESIZE;
    } else {
      lo_mem = pcb->stack_base - IK_PAGESIZE;
      hi_mem = pcb->heap_base + pcb->heap_size + IK_PAGESIZE;
    }
    /* The segment index "lo_seg_idx" is  the index of the first segment
     * (lowest address) of used  memory.  The segment index "hi_seg_idx"
     * is the segment index of the  segment right after the last segment
     * (highest address) of used memory.
     *
     *          segment     segment     segment
     *    ---|-----------|-----------|-----------|--- used_memory
     *        ^                                   ^
     *        lo_seg_idx                          hi_seg_idx
     */
    lo_seg_idx  = IK_SEGMENT_INDEX(lo_mem);
    hi_seg_idx  = IK_SEGMENT_INDEX(hi_mem + IK_SEGMENT_SIZE - 1);
    base_offset = lo_seg_idx * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    vec_size    = (hi_seg_idx - lo_seg_idx) * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    {
      ikptr	dvec = ik_mmap(vec_size);
      bzero((char*)dvec, vec_size);
      pcb->dirty_vector_base   = (uint32_t *)dvec;
      pcb->dirty_vector        = dvec - base_offset;
    }
    {
      ikptr	svec = ik_mmap(vec_size);
      bzero((char*)(long)svec, vec_size);
      pcb->segment_vector_base = (uint32_t *)svec;
      pcb->segment_vector      = (uint32_t *)(svec - base_offset);
    }
    /* In the  whole system  memory we want  pointers to  delimiting the
       interesting memory:

          |---------------------------------------------| system_memory
	         ^                        ^
             memory_base              memory_end
    */
    pcb->memory_base = (ikptr)(lo_seg_idx * IK_SEGMENT_SIZE);
    pcb->memory_end  = (ikptr)(hi_seg_idx * IK_SEGMENT_SIZE);

    /* Register  the heap  block and  the  stack block  in the  segments
       vector.   We  do this  here,  after  having  set the  PCB  fields
       "memory_base" and "memory_end". */
    set_page_range_type(pcb->heap_base,  pcb->heap_size,  mainheap_mt,  pcb);
    set_page_range_type(pcb->stack_base, pcb->stack_size, mainstack_mt, pcb);

#if 0
    fprintf(stderr, "\n*** Vicare debug:\n");
    fprintf(stderr, "*  pcb->heap_base  = #x%lX\n", pcb->heap_base);
    fprintf(stderr, "*  pcb->heap_size  = %lu\n", pcb->heap_size);
    fprintf(stderr, "*  pcb->stack_base = #x%lX\n", pcb->stack_base);
    fprintf(stderr, "*  pcb->stack_size = %lu\n", pcb->stack_size);
    fprintf(stderr, "*  lo_mem = #x%lX, hi_mem = #x%lX\n", lo_mem, hi_mem);
    fprintf(stderr, "*  lo_seg_idx = %lu, hi_seg_idx = %lu\n", lo_seg_idx, hi_seg_idx);
    fprintf(stderr, "*  vec_size = %lu bytes, %lu 32-bit words\n",
	    vec_size, vec_size/sizeof(uint32_t));
    fprintf(stderr, "*  memory_base = #x%lX\n", pcb->memory_base);
    fprintf(stderr, "*  memory_end  = #x%lX\n", pcb->memory_end);
    fprintf(stderr, "*  first dirty   slot: dirty_vector[%lu]\n",
	    ((long)pcb->dirty_vector_base   - (long)pcb->dirty_vector)/IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT);
    fprintf(stderr, "*  first segment slot: segment_vector[%lu]\n",
	    ((long)pcb->segment_vector_base - (long)pcb->segment_vector)/IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT);
    fprintf(stderr, "\n");
#endif
  }

  /* Initialize base structure type descriptor  (STD).  This is the type
     descriptor of all the struct type descriptors; it describes itself.
     See   the  Texinfo   documentation  node   "objects  structs"   for
     details. */
  {
    ikptr s_base_rtd = ik_unsafe_alloc(pcb, IK_ALIGN(rtd_size)) | rtd_tag;
    IK_REF(s_base_rtd, off_rtd_rtd)        = s_base_rtd;
    IK_REF(s_base_rtd, off_rtd_length)     = (ikptr) (rtd_size-wordsize);
    IK_REF(s_base_rtd, off_rtd_name)       = 0; /* = the fixnum 0 */
    IK_REF(s_base_rtd, off_rtd_fields)     = 0; /* = the fixnum 0 */
    IK_REF(s_base_rtd, off_rtd_printer)    = 0; /* = the fixnum 0 */
    IK_REF(s_base_rtd, off_rtd_symbol)     = 0; /* = the fixnum 0 */
    IK_REF(s_base_rtd, off_rtd_destructor) = IK_FALSE;
    pcb->base_rtd = s_base_rtd;
  }

  /* Initialise miscellaneous fields. */
  {
    pcb->collect_key         = IK_FALSE_OBJECT;
    pcb->not_to_be_collected = NULL;
  }
  return pcb;
}


void
ik_delete_pcb (ikpcb* pcb)
{
  { /* Release the page cache. */
    ikpage *	p = pcb->cached_pages;
    for (; p; p = p->next) {
      ik_munmap(p->base, IK_PAGESIZE);
    }
    pcb->cached_pages   = NULL;
    pcb->uncached_pages = NULL;
    ik_munmap(pcb->cached_pages_base, pcb->cached_pages_size);
  }
  {
    int i;
    for(i=0; i<IK_GC_GENERATION_COUNT; i++) {
      ik_ptr_page* p = pcb->protected_list[i];
      while (p) {
        ik_ptr_page* next = p->next;
        ik_munmap((ikptr)(long)p, IK_PAGESIZE);
	p = next;
      }
    }
  }
  ikptr	base = pcb->memory_base;
  ikptr	end  = pcb->memory_end;
  { /* Release all the used pages. */
    uint32_t *	segment_vec  = pcb->segment_vector;
    long	page_idx     = IK_PAGE_INDEX(base);
    long	page_idx_end = IK_PAGE_INDEX(end);
    for (; page_idx < page_idx_end; ++page_idx) {
      if (hole_mt != segment_vec[page_idx]) {
	ik_munmap((ikptr)(page_idx << IK_PAGESHIFT), IK_PAGESIZE);
      }
    }
  }
  { /* Release the dirty vector and the segments vector. */
    ik_ulong	lo_seg   = IK_SEGMENT_INDEX(base);
    ik_ulong	hi_seg   = IK_SEGMENT_INDEX(end);
    ik_ulong	vec_size = (hi_seg - lo_seg) * IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT;
    ik_munmap((ikptr)(ik_ulong)pcb->dirty_vector_base,   vec_size);
    ik_munmap((ikptr)(ik_ulong)pcb->segment_vector_base, vec_size);
  }
  ik_free(pcb, sizeof(ikpcb));
}


/** --------------------------------------------------------------------
 ** Basic memory allocation for Scheme objects.
 ** ----------------------------------------------------------------- */

ikptr
ik_safe_alloc (ikpcb * pcb, ik_ulong size)
/* Allocate a memory block on the  Scheme heap and return a reference to
   it as an *untagged* pointer.   PCB must reference the process control
   block, SIZE  must be the  requested number of bytes  filtered through
   "IK_ALIGN()".

   If  not enough memory  is available  on the  current heap  segment, a
   garbage collection  is triggered; then allocation is  tried again: if
   it  still   fails  the  process   is  terminated  with   exit  status
   EXIT_FAILURE.  */
{
  assert(size == IK_ALIGN(size));
  ikptr		alloc_ptr;
  ikptr		end_ptr;
  ikptr		new_alloc_ptr;
  alloc_ptr	= pcb->allocation_pointer;
  end_ptr	= pcb->heap_base + pcb->heap_size;
  new_alloc_ptr	= alloc_ptr + size;
  if (new_alloc_ptr < end_ptr) {
    /* There is  room in the  current heap  segment: update the  PCB and
       return the offset. */
    pcb->allocation_pointer = new_alloc_ptr;
  } else {
    /* No room in the current heap block: run GC. */
    ik_collect(size, pcb);
    {
      alloc_ptr		= pcb->allocation_pointer;
      end_ptr		= pcb->heap_base + pcb->heap_size;
      new_alloc_ptr	= alloc_ptr + size;
      if (new_alloc_ptr < end_ptr)
	pcb->allocation_pointer = new_alloc_ptr;
      else
	ik_abort("collector did not leave enough room for %lu bytes", size);
    }
  }
  return alloc_ptr;
}
ikptr
ik_unsafe_alloc (ikpcb * pcb, ik_ulong requested_size)
/* Allocate a memory block on the  Scheme heap and return a reference to
   it as an *untagged* pointer.   PCB must reference the process control
   block, REQUESTED_SIZE must be  the requested number of bytes filtered
   through "IK_ALIGN()".

   If not enough memory is available  on the current heap segment: a new
   heap segment is  allocated; if such allocation fails:  the process is
   terminated with exit status EXIT_FAILURE.

   This  function  is  meant  to  be used  to  allocate  "small"  memory
   blocks. */
{
  assert(requested_size == IK_ALIGN(requested_size));
  ikptr alloc_ptr       = pcb->allocation_pointer;
  ikptr end_ptr         = pcb->heap_base + pcb->heap_size;
  ikptr new_alloc_ptr   = alloc_ptr + requested_size;
  if (new_alloc_ptr < end_ptr) {
    /* There is  room in the  current heap  nursery: update the  PCB and
       return the offset. */
    pcb->allocation_pointer = new_alloc_ptr;
    return alloc_ptr;
  } else {
    /* No  room  in  the  current  heap nursery:  enlarge  the  heap  by
       allocating new memory. */
    if (alloc_ptr) {
      /* This is not  the first heap block allocation, so  prepend a new
	 "ikmemblock" node  to the  linked list of  old heap  blocks and
	 initialise it with a reference to the current heap block. */
      ikmemblock *	p = ik_malloc(sizeof(ikmemblock));
      p->base = pcb->heap_base;
      p->size = pcb->heap_size;
      p->next = pcb->heap_pages;
      pcb->heap_pages = p;
    }
    { /* Accounting.  We keep  count of all the bytes  allocated for the
       * heap, so that:
       *
       *   total_allocated_bytes = \
       *     IK_MOST_BYTES_IN_MINOR * pcb->allocation_count_major
       *     + pcb->allocation_count_minor
       */
      ik_ulong bytes = ((ik_ulong)pcb->allocation_pointer) - ((ik_ulong)pcb->heap_base);
      ik_ulong minor = bytes + pcb->allocation_count_minor;
      while (minor >= IK_MOST_BYTES_IN_MINOR) {
	minor -= IK_MOST_BYTES_IN_MINOR;
	pcb->allocation_count_major++;
      }
      pcb->allocation_count_minor = minor;
    }
    { /* Allocate a  new heap  segment and register  it as  current heap
       * base.  While computing  the segment size: make  sure that there
       * is always  some room at the  end of the new  heap segment after
       * allocating the requested memory for the new object.
       *
       * Initialise it as follows:
       *
       *     heap_base                allocation_redline
       *         v                            v
       *  lo mem |----------------------------+--------| hi mem
       *                       Scheme heap
       *         |.....................................|
       *                       heap_size
       */
      ikptr	heap_ptr;
      ik_ulong	new_size = (requested_size > IK_HEAP_EXTENSION_SIZE)? \
	requested_size : IK_HEAP_EXTENSION_SIZE;
      new_size			= IK_ALIGN_TO_NEXT_PAGE(new_size + IK_DOUBLE_PAGESIZE);
      heap_ptr			= ik_mmap_mainheap(new_size, pcb);
      pcb->heap_base		= heap_ptr;
      pcb->heap_size		= new_size;
      pcb->allocation_redline	= heap_ptr + new_size - IK_DOUBLE_CHUNK_SIZE;
      pcb->allocation_pointer	= heap_ptr + requested_size;
      return heap_ptr;
    }
  }
}
void
ik_signal_dirt_in_page_of_pointer (ikpcb * pcb, ikptr s_pointer)
{
  IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, s_pointer);
}


void
ik_debug_message (const char * error_message, ...)
{
  va_list        ap;
  va_start(ap, error_message);
  fprintf(stderr, "*** Vicare debug: ");
  vfprintf(stderr, error_message, ap);
  fprintf(stderr, "\n");
  va_end(ap);
}
void
ik_debug_message_no_newline (const char * error_message, ...)
{
  va_list        ap;
  va_start(ap, error_message);
  fprintf(stderr, "*** Vicare debug: ");
  vfprintf(stderr, error_message, ap);
  va_end(ap);
}
void
ik_debug_message_start (const char * error_message, ...)
{
  va_list        ap;
  va_start(ap, error_message);
  fprintf(stderr, "\n*** Vicare debug: ");
  vfprintf(stderr, error_message, ap);
  fprintf(stderr, "\n");
  va_end(ap);
}
int
ik_abort (const char * error_message, ...)
{
  va_list        ap;
  va_start(ap, error_message);
  fprintf(stderr, "*** Vicare error: ");
  vfprintf(stderr, error_message, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(EXIT_FAILURE);
  return EXIT_FAILURE;
}
void
ik_error (ikptr args)
{
  fprintf(stderr, "*** Vicare error: ");
  ik_fprint(stderr, args);
  fprintf(stderr, "\n");
  exit(EXIT_FAILURE);
}


void
ik_stack_overflow (ikpcb* pcb)
/* Let's recall  how the  Scheme stack  is managed; at  first we  have a
 * single stack segment:
 *
 *    stack_base   redline        growth     frame_base
 *         v          v          <------        v
 *  lo mem |----------+----------------------|-| hi mem
 *                                            v
 *                                     ik_underflow_handler
 *
 * where the highest machine word is  set to the address of the assembly
 * label "ik_underflow_handler",  defined in the  file "ikarus-enter.S",
 * to  which the  execution  flow  returns after  the  last Scheme  code
 * execution completes.
 *
 * When the use  of the stack passes the redline:  this very function is
 * called;  the current  stack segment  is freezed  into a  continuation
 * object, registered in  the PCB as "next process  continuation"; a new
 * stack segment  is allocated and  initialised in  the same way  of the
 * old:
 *
 *    stack_base   redline        growth    frame_base
 *         v          v          <------        v
 *  lo mem |----------+----------------------|-| hi mem
 *                                            v
 *                                     ik_underflow_handler
 *
 * When  use of  the  new stack  segment is  finished:  the Scheme  code
 * execution returns to the  "ik_underflow_handler" label, which will do
 * what is  needed to retrieve the  freezed stack frames and  resume the
 * continuation.
 *
 * Notice that  "ik_stack_overflow()" is  always called by  the assembly
 * routine "ik_foreign_call"  with code that  does not touch  the Scheme
 * stack (because "ik_stack_overflow()" has  no Scheme arguments).  Upon
 * entering this function, assuming there are 2 frames, the situation on
 * the old Scheme stack is as follows:
 *
 *         high memory
 *   |                      | <-- pcb->frame_base
 *   |----------------------|
 *   | ik_underflow_handler |
 *   |----------------------|                         --
 *   |    local value 1     |                         .
 *   |----------------------|                         .
 *   |    local value 1     |                         . framesize 1
 *   |----------------------|                         .
 *   |   return address 1   |                         .
 *   |----------------------|                         --
 *   |    local value 0     | <-- pcb->frame_redline  .
 *   |----------------------|                         .
 *   |    local value 0     |                         . framesize 0
 *   |----------------------|                         .
 *   |   return address 0   | <-- pcb->frame_pointer  .
 *   |----------------------|                         --
 *             ...
 *   |----------------------|
 *   |                      | <-- pcb->stack_base
 *   |----------------------|
 *   |                      |
 *         low memory
 *
 * where  the frame  0  is  the one  that  crossed  the redline  causing
 * "ik_stack_overflow()" to be called.   Right after initialisation, the
 * situation of the new Scheme stack is as follows:
 *
 *         high memory
 *   |                      | <-- pcb->frame_base
 *   |----------------------|
 *   | ik_underflow_handler | <-- pcb->frame_pointer
 *   |----------------------|
 *             ...
 *   |----------------------|
 *   |                      | <-- pcb->frame_redline
 *   |----------------------|
 *             ...
 *   |----------------------|
 *   |                      | <-- pcb->stack_base
 *   |----------------------|
 *   |                      |
 *         low memory
 *
 * So  after   returning  from  this  function:   the  assembly  routine
 * "ik_foreign_call" will return to  the label "ik_underflow_handler and
 * the underflow handler will do its job.
 */
#define STACK_DEBUG	0
{
  if (0 || STACK_DEBUG) {
    ik_debug_message("%s: enter pcb=0x%016lx", __func__, (long)pcb);
  }
  assert(pcb->frame_pointer <= pcb->frame_base);
  assert(pcb->frame_pointer <= pcb->frame_redline);
  assert(IK_UNDERFLOW_HANDLER == IK_REF(pcb->frame_base, -wordsize));
  /* Freeze the  Scheme stack segment  into a continuation  and register
     the continuation object in the  PCB as "next process continuation".
     Mark the old Scheme stack segment as "data".*/
  {
    ikcont *	kont   = (ikcont*)(long)ik_unsafe_alloc(pcb, IK_ALIGN(continuation_size));
    ikptr	s_kont = ((ikptr)kont) | continuation_primary_tag;
    kont->tag  = continuation_tag;
    kont->top  = pcb->frame_pointer;
    kont->size = pcb->frame_base - pcb->frame_pointer - wordsize;
    kont->next = pcb->next_k;
    pcb->next_k = s_kont;
    set_page_range_type(pcb->stack_base, pcb->stack_size, data_mt, pcb);
    assert(0 != kont->size);
    if (IK_PROTECT_FROM_STACK_OVERFLOW) {
      /* Release the protection on the  first low-address memory page in
	 the stack  segment, which avoids  memory corruption in  case of
	 undetected Scheme stack overflow. */
      mprotect((void*)(long)(pcb->stack_base), IK_PAGESIZE, PROT_READ|PROT_WRITE);
    }
  }
  /* Allocate a  new memory segment to  be used as Scheme  stack and set
     the PCB accordingly. */
  {
    pcb->stack_base	= ik_mmap_typed(IK_STACKSIZE, mainstack_mt, pcb);
    pcb->stack_size	= IK_STACKSIZE;
    pcb->frame_base	= pcb->stack_base + IK_STACKSIZE;
    pcb->frame_pointer	= pcb->frame_base - wordsize;
    IK_REF(pcb->frame_pointer, 0) = IK_UNDERFLOW_HANDLER;
    if (IK_PROTECT_FROM_STACK_OVERFLOW) {
      /* Forbid reading  and writing in  the low-address memory  page of
       * the  stack  segment;  this  should  trigger  a  SIGSEGV  if  an
       * undetected  Scheme  stack  overflow happens.   Not  a  solution
       * against stack  overflows, but at  least it should  avoid memory
       * corruption.
       *
       *    stack_base                             frame_base
       *         v                                     v
       *  lo mem |-------------------------------------| hi mem
       *
       *         |.....|...............................|
       *       1st page         usable region
       *
       * This configuration must be performed also when first allocating
       * the stack segment.
       */
      mprotect((void*)(long)(pcb->stack_base), IK_PAGESIZE, PROT_NONE);
      pcb->frame_redline= pcb->stack_base + IK_DOUBLE_CHUNK_SIZE + IK_PAGESIZE;
    } else {
      pcb->frame_redline= pcb->stack_base + IK_DOUBLE_CHUNK_SIZE;
    }
  }
  if (0 || STACK_DEBUG) {
    ik_debug_message("%s: leave pcb=0x%016lx", __func__, (long)pcb);
  }
}


/*
char* ik_uuid(char* str) {
  assert((36 << fx_shift) == (int) ref(str, disp_string_length - string_tag));
  uuid_t u;
  uuid_clear(u);
  uuid_generate(u);
  uuid_unparse_upper(u, str + disp_string_data - string_tag);
  return str;
}
*/

static const char* uuid_chars =
  "!$%&/0123456789<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static int uuid_strlen = 1;
ikptr
ik_uuid(ikptr bv)
{
  static int fd = -1;
  if (fd == -1) {
    fd = open("/dev/urandom", O_RDONLY);
    if (fd == -1) {
      return ik_errno_to_code();
    }
    uuid_strlen = strlen(uuid_chars);
  }
  long           n    = IK_UNFIX(IK_REF(bv, off_bytevector_length));
  unsigned char* data = (unsigned char*)(long)(bv + off_bytevector_data);
  int r = read(fd, data, n);
  if (r < 0) {
    return ik_errno_to_code();
  }
  unsigned char* p = data;
  unsigned char* q = data + n;
  while (p < q) {
    *p = uuid_chars[*p % uuid_strlen];
    p++;
  }
  return bv;
}


/** --------------------------------------------------------------------
 ** Helper functions for debugging purposes.
 ** ----------------------------------------------------------------- */

static char*
mtname (unsigned n)
{
  if (n == mainheap_type)  { return "HEAP_T"; }
  if (n == mainstack_type) { return "STAK_T"; }
  if (n == pointers_type)  { return "PTER_T"; }
  if (n == dat_type)       { return "DATA_T"; }
  if (n == code_type)      { return "CODE_T"; }
  if (n == hole_type)      { return "      "; }
  return "WHAT_T";
}
ikptr
ik_dump_metatable (ikpcb* pcb)
{
  unsigned* s = pcb->segment_vector_base;
  ikptr p = pcb->memory_base;
  ikptr hi = pcb->memory_end;
  while (p < hi) {
    unsigned t = *s & type_mask;
    ikptr start = p;
    p += IK_PAGESIZE;
    s++;
    while ((p < hi) && ((*s & type_mask) == t)) {
      p += IK_PAGESIZE;
      s++;
    }
    fprintf(stderr, "0x%016lx + %5ld pages = %s\n",
	    (long) start,
	    ((long)p-(long)start)/IK_PAGESIZE,
	    mtname(t));
  }
  return IK_VOID_OBJECT;
}
ikptr
ik_dump_dirty_vector (ikpcb* pcb)
{
  unsigned* s  = pcb->dirty_vector_base;
  ikptr     p  = pcb->memory_base;
  ikptr     hi = pcb->memory_end;
  while (p < hi) {
    unsigned t     = *s;
    ikptr    start = p;
    p += IK_PAGESIZE;
    s++;
    while ((p < hi) && (*s == t)) {
      p += IK_PAGESIZE;
      s++;
    }
    fprintf(stderr, "0x%016lx + %5ld pages = 0x%08x\n",
        (long) start,
        ((long)p-(long)start)/IK_PAGESIZE,
        t);
  }
  return IK_VOID_OBJECT;
}


/** --------------------------------------------------------------------
 ** Code objects constructor and auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_make_code (ikptr s_code_size, ikptr s_freevars, ikptr s_relocation_vector, ikpcb* pcb)
/* Build a new code object and return a reference to it.

   S_CODE_SIZE is a non-negative  fixnum representing the requested code
   size.  S_FREEVARS is a non-negative fixnum representing the number of
   free variables in the code.  S_RELOCATION_VECTOR is a vector used for
   relocation; empty when handed to this function. */
{
  assert(IK_IS_FIXNUM(s_code_size));
  assert(IK_IS_FIXNUM(s_freevars));
  assert(ik_is_vector(s_relocation_vector));
  long   code_size = IK_UNFIX(s_code_size);
  /* We allocate  a number of bytes  equal to the least  number of pages
   * required to  hold CODE_SIZE.   Example: if  CODE_SIZE is  less than
   * IK_PAGESIZE:
   *
   *      IK_PAGESIZE
   *   |---------------| memreq
   *   |---------| code_size
   *
   * Example: if CODE_SIZE is greater than IK_PAGESIZE:
   *
   *      IK_PAGESIZE     IK_PAGESIZE
   *   |---------------|---------------| memreq
   *   |-------------------| code_size
   *
   */
  long   memreq    = IK_ALIGN_TO_NEXT_PAGE(disp_code_data + code_size);
  /* Here MEM is  still an untagged pointer, not really  an "ikptr" yet;
     we  tag it  later.   MEM references  the first  byte  in the  pages
     allocated with "mmap()" with execution protection. */
  ikptr  mem       = ik_mmap_code(memreq, 0, pcb);
  bzero((char*)(long)mem, memreq);
  IK_REF(mem, disp_code_tag)		= code_tag;
  IK_REF(mem, disp_code_code_size)	= s_code_size;
  IK_REF(mem, disp_code_freevars)	= s_freevars;
  IK_REF(mem, disp_code_reloc_vector)	= s_relocation_vector;
  IK_REF(mem, disp_code_annotation)	= IK_FALSE;
  /* We put  nothing in the "unused"  field of the block;  this field is
     already allocated to zeros, which means the fixnum zero. */
  /* FIXME Do we actually need to call the relocation function here?  It
     appears  that the  functions does  nothing when  the relocation  is
     empty, and it IS empty when this function is called.  (Marco Maggi;
     Oct 4, 2012) */
  ik_relocate_code(mem);
  return mem | vector_tag;
}
ikptr
ikrt_set_code_reloc_vector (ikptr s_code, ikptr s_vec, ikpcb* pcb)
{
  IK_REF(s_code, off_code_reloc_vector) = s_vec;
  ik_relocate_code(s_code - vector_tag);
  IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, s_code);
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_code_annotation (ikptr s_code, ikptr s_annot, ikpcb* pcb)
{
  IK_REF(s_code, off_code_annotation) = s_annot;
  IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, s_code);
  return IK_VOID;
}


/** --------------------------------------------------------------------
 ** Guardians handling.
 ** ----------------------------------------------------------------- */

/* The tagged  pointers referencing  guardians are  stored in  the array
   "protected_list" of the structure  "ik_ptr_page"; such structures are
   nodes   in   a  linked   list   referenced   by  the   PCB's   member
   "protected_list".
*/

ikptr
ikrt_register_guardian_pair (ikptr p0, ikpcb* pcb)
/* Register a guardian  pair in the protected list of  PCB.  If there is
   no more room in the current  protected list node: allocate a new node
   and prepend it to the linked list.  Return the void object. */
{
  /* FIRST is  a pointer  to the first  node in  a linked list.   If the
     linked list is empty or the first node is full: allocate a new node
     and prepend it to the list. */
  ik_ptr_page *	first = pcb->protected_list[IK_GUARDIANS_GENERATION_NUMBER];
  if ((NULL == first) || (IK_PTR_PAGE_NUMBER_OF_GUARDIANS_SLOTS == first->count)) {
    assert(sizeof(ik_ptr_page) == IK_PAGESIZE);
    ik_ptr_page *	new_node;
    new_node        = (ik_ptr_page*)ik_mmap(IK_PAGESIZE);
    new_node->count = 0;
    new_node->next  = first;
    first           = new_node;
    pcb->protected_list[IK_GUARDIANS_GENERATION_NUMBER] = new_node;
  }
  first->ptr[first->count++] = p0; /* store the guardian pair */
  return IK_VOID_OBJECT;
}
ikptr
ikrt_register_guardian (ikptr tc, ikptr obj, ikpcb* pcb)
{
  ikptr p0   = IKU_PAIR_ALLOC(pcb);
  IK_CAR(p0) = tc;
  IK_CDR(p0) = obj;
  return ikrt_register_guardian_pair(p0, pcb);
}


/** --------------------------------------------------------------------
 ** Garbage collections statistics.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_stats_now (ikptr t, ikpcb* pcb)
{
  struct rusage r;
  struct timeval s;
  gettimeofday(&s, 0);
  getrusage(RUSAGE_SELF, &r);
  /* Do  not change  the  order  of the  fields!!!   It  must match  the
     implementation     of     the     record    type     "stats"     in
     "scheme/ikarus.timer.sls". */
  IK_FIELD(t,  0) = IK_FIX(r.ru_utime.tv_sec);
  IK_FIELD(t,  1) = IK_FIX(r.ru_utime.tv_usec);
  IK_FIELD(t,  2) = IK_FIX(r.ru_stime.tv_sec);
  IK_FIELD(t,  3) = IK_FIX(r.ru_stime.tv_usec);
  IK_FIELD(t,  4) = IK_FIX(s.tv_sec);
  IK_FIELD(t,  5) = IK_FIX(s.tv_usec);
  IK_FIELD(t,  6) = IK_FIX(pcb->collection_id);
  IK_FIELD(t,  7) = IK_FIX(pcb->collect_utime.tv_sec);
  IK_FIELD(t,  8) = IK_FIX(pcb->collect_utime.tv_usec);
  IK_FIELD(t,  9) = IK_FIX(pcb->collect_stime.tv_sec);
  IK_FIELD(t, 10) = IK_FIX(pcb->collect_stime.tv_usec);
  IK_FIELD(t, 11) = IK_FIX(pcb->collect_rtime.tv_sec);
  IK_FIELD(t, 12) = IK_FIX(pcb->collect_rtime.tv_usec);
  { /* minor bytes */
    ik_ulong bytes_in_heap	= ((ik_ulong)pcb->allocation_pointer) - ((ik_ulong)pcb->heap_base);
    ik_ulong bytes		= bytes_in_heap + pcb->allocation_count_minor;
    IK_FIELD(t, 13)		= IK_FIX(bytes);
  }
  /* major bytes */
  IK_FIELD(t, 14) = IK_FIX(pcb->allocation_count_major);
  return IK_VOID_OBJECT;
}


/** --------------------------------------------------------------------
 ** Process termination.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_exit (ikptr status, ikpcb* pcb)
/* This function is the core of implementation of the EXIT proceure from
   "(rnrs programs (6))". */
{
  ik_delete_pcb(pcb);
  if (total_allocated_pages)
    ik_debug_message("allocated pages: %d", total_allocated_pages);
  assert(0 == total_allocated_pages);
  exit(IK_IS_FIXNUM(status)? IK_UNFIX(status) : EXIT_FAILURE);
}


/** --------------------------------------------------------------------
 ** Configuration options commands.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_vicare_built_with_srfi_enabled (ikpcb * pcb)
{
#ifdef VICARE_BUILT_WITH_SRFI_ENABLED
  return IK_TRUE;
#else
  return IK_FALSE;
#endif
}
ikptr
ikrt_vicare_built_with_iconv_enabled (ikpcb * pcb)
{
#ifdef VICARE_BUILT_WITH_ICONV_ENABLED
  return IK_TRUE;
#else
  return IK_FALSE;
#endif
}
ikptr
ikrt_vicare_built_with_ffi_enabled (ikpcb * pcb)
{
#ifdef VICARE_BUILT_WITH_FFI_ENABLED
  return IK_TRUE;
#else
  return IK_FALSE;
#endif
}
ikptr
ikrt_vicare_built_with_posix_enabled (ikpcb * pcb)
{
#ifdef VICARE_BUILT_WITH_POSIX_ENABLED
  return IK_TRUE;
#else
  return IK_FALSE;
#endif
}
ikptr
ikrt_vicare_built_with_glibc_enabled (ikpcb * pcb)
{
#ifdef VICARE_BUILT_WITH_GLIBC_ENABLED
  return IK_TRUE;
#else
  return IK_FALSE;
#endif
}
ikptr
ikrt_vicare_built_with_linux_enabled (ikpcb * pcb)
{
#ifdef VICARE_BUILT_WITH_LINUX_ENABLED
  return IK_TRUE;
#else
  return IK_FALSE;
#endif
}

/* end of file */
