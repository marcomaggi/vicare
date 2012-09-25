/*
 * Ikarus Scheme -- A compiler for R6RS Scheme.
 * Copyright (C) 2006,2007,2008,2012  Abdulaziz Ghuloum
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

static int total_allocated_pages = 0;
static int total_malloced = 0;

#define SEGMENT_SIZE		(IK_PAGESIZE * IK_PAGESIZE / 4)
#define SEGMENT_SHIFT		(IK_PAGESHIFT + IK_PAGESHIFT - 2)
#define SEGMENT_INDEX(x)	(((ik_ulong)(x)) >> SEGMENT_SHIFT)

#define CACHE_SIZE		(IK_PAGESIZE * 1) /* must be multiple of IK_PAGESIZE */


static void
extend_table_maybe (ikptr p, ik_ulong size, ikpcb* pcb)
{
  assert(size == IK_ALIGN_TO_NEXT_PAGE(size));
  ikptr q = p + size;
  if (p < pcb->memory_base) {
    ik_ulong new_lo       = SEGMENT_INDEX(p);
    ik_ulong old_lo       = SEGMENT_INDEX(pcb->memory_base);
    ik_ulong hi           = SEGMENT_INDEX(pcb->memory_end);
    ik_ulong new_vec_size = (hi - new_lo) * IK_PAGESIZE;
    ik_ulong old_vec_size = (hi - old_lo) * IK_PAGESIZE;
    ikptr v = ik_mmap(new_vec_size);
    bzero((char*)(long)v, new_vec_size - old_vec_size);
    memcpy((char*)(long)(v+new_vec_size-old_vec_size),
           (char*)(long)pcb->dirty_vector_base,
           old_vec_size);
    ik_munmap((ikptr)(long)pcb->dirty_vector_base, old_vec_size);
    pcb->dirty_vector_base = (unsigned*)(long)v;
    pcb->dirty_vector      = (v - new_lo * IK_PAGESIZE);
    ikptr s = ik_mmap(new_vec_size);
    bzero((char*)(long)s, new_vec_size - old_vec_size);
    memcpy((char*)(long)(s+new_vec_size-old_vec_size),
           (char*)(long)(pcb->segment_vector_base),
           old_vec_size);
    ik_munmap((ikptr)(long)pcb->segment_vector_base, old_vec_size);
    pcb->segment_vector_base = (unsigned*)(long)s;
    pcb->segment_vector = (unsigned*)(long)(s - new_lo * IK_PAGESIZE);
    pcb->memory_base = (new_lo * SEGMENT_SIZE);
  }
  else if (q >= pcb->memory_end) {
    ik_ulong lo           = SEGMENT_INDEX(pcb->memory_base);
    ik_ulong old_hi       = SEGMENT_INDEX(pcb->memory_end);
    ik_ulong new_hi       = SEGMENT_INDEX(q+SEGMENT_SIZE-1);
    ik_ulong new_vec_size = (new_hi - lo) * IK_PAGESIZE;
    ik_ulong old_vec_size = (old_hi - lo) * IK_PAGESIZE;
    ikptr v = ik_mmap(new_vec_size);
    memcpy((char*)(long)v,
           (char*)(long)pcb->dirty_vector_base,
           old_vec_size);
    bzero((char*)(long)(v+old_vec_size), new_vec_size - old_vec_size);
    ik_munmap((ikptr)(long)pcb->dirty_vector_base, old_vec_size);
    pcb->dirty_vector_base = (unsigned*)(long)v;
    pcb->dirty_vector      = (v - lo * IK_PAGESIZE);
    ikptr s = ik_mmap(new_vec_size);
    memcpy((char*)(long)s, pcb->segment_vector_base, old_vec_size);
    bzero((char*)(long)(s+old_vec_size), new_vec_size - old_vec_size);
    ik_munmap((ikptr)(long)pcb->segment_vector_base, old_vec_size);
    pcb->segment_vector_base = (unsigned*)(long) s;
    pcb->segment_vector      = (unsigned*)(s - lo * IK_PAGESIZE);
    pcb->memory_end          = (new_hi * SEGMENT_SIZE);
  }
}

static void
set_segment_type (ikptr base, ik_ulong size, unsigned type, ikpcb* pcb)
/* Set to TYPE all the entries in "pcb->segment_vector" corresponding to
   the memory block starting at BASE and SIZE bytes wide. */
{
  assert(base >= pcb->memory_base);
  assert((base+size) <= pcb->memory_end);
  assert(size == IK_ALIGN_TO_NEXT_PAGE(size));
  unsigned* p = pcb->segment_vector + IK_PAGE_INDEX(base);
  unsigned* q = p                   + IK_PAGE_INDEX(size);
  for (; p < q; ++p)
    *p = type;
}


ikptr
ik_mmap_typed (ik_ulong size, unsigned type, ikpcb* pcb)
{
  ikptr		segment;
  if (size == IK_PAGESIZE) {
    /* If available, recycle a page from the cache. */
    ikpage *	pages = pcb->cached_pages;
    if (pages) {
      /* Extract the first page from the linked list of cached pages. */
      segment		  = pages->base;
      pcb->cached_pages	  = pages->next;
      /* Prepend  the extracted  page  to the  linked  list of  uncached
	 pages. */
      pages->next	  = pcb->uncached_pages;
      pcb->uncached_pages = pages;
    } else
      segment = ik_mmap(size);
  } else
    segment = ik_mmap(size);
  extend_table_maybe(segment, size, pcb);
  set_segment_type(segment, size, type, pcb);
  return segment;
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
  ikptr p = ik_mmap_typed(size, code_mt|gen, pcb);
  if (size > IK_PAGESIZE)
    set_segment_type(p+IK_PAGESIZE, size-IK_PAGESIZE, data_mt|gen, pcb);
  return p;
}
ikptr
ik_mmap_mixed (ik_ulong size, ikpcb* pcb)
/* Allocate a memory segment tagged as part of the Scheme heap. */
{
  return ik_mmap_typed(size, mainheap_mt, pcb);
}


ikptr
ik_mmap (ik_ulong size)
{
  ik_ulong pages   = (size + IK_PAGESIZE - 1) / IK_PAGESIZE;
  ik_ulong mapsize = pages * IK_PAGESIZE;
  total_allocated_pages += pages;
  assert(size == mapsize);
#ifndef __CYGWIN__
  char* mem = mmap(0, mapsize, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
  /* FIXME: check if in range */
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
{
  ik_ulong pages   = (size + IK_PAGESIZE - 1) / IK_PAGESIZE;
  ik_ulong mapsize = pages * IK_PAGESIZE;
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


ikpcb*
ik_make_pcb (void)
{
  ikpcb* pcb = ik_malloc(sizeof(ikpcb));
  bzero(pcb, sizeof(ikpcb));
  pcb->collect_key = IK_FALSE_OBJECT;
#define STAKSIZE (1024 * 4096)
  //#define STAKSIZE (256 * 4096)
  pcb->heap_base = ik_mmap(IK_HEAPSIZE);
  pcb->heap_size = IK_HEAPSIZE;
  pcb->allocation_pointer = pcb->heap_base;
  pcb->allocation_redline = pcb->heap_base + IK_HEAPSIZE - 2 * 4096;

  pcb->stack_base = ik_mmap(STAKSIZE);
  pcb->stack_size = STAKSIZE;
  pcb->frame_pointer = pcb->stack_base + pcb->stack_size;
  pcb->frame_base = pcb->frame_pointer;
  pcb->frame_redline = pcb->stack_base + 2 * 4096;

  pcb->not_to_be_collected = IK_NULL_OBJECT;

  { /* make cache ikpage */
    ikpage* p = (ikpage*)(long)ik_mmap(CACHE_SIZE * sizeof(ikpage));
    pcb->cached_pages_base = (ikptr)(long)p;
    pcb->cached_pages_size = CACHE_SIZE * sizeof(ikpage);
    ikpage* q = 0;
    ikpage* e = p + CACHE_SIZE;
    while (p < e) {
      p->next = q;
      q = p;
      p++;
    }
    pcb->uncached_pages = q;
  }

  {
    /* compute extent of heap and stack */
    ikptr lo_mem;
    ikptr hi_mem;
    if (pcb->heap_base < pcb->stack_base) {
      lo_mem = pcb->heap_base - IK_PAGESIZE;
      hi_mem = pcb->stack_base + pcb->stack_size + IK_PAGESIZE;
    } else {
      lo_mem = pcb->stack_base - IK_PAGESIZE;
      hi_mem = pcb->heap_base + pcb->heap_size + IK_PAGESIZE;
    }

    ik_ulong lo_seg = SEGMENT_INDEX(lo_mem);
    ik_ulong hi_seg = SEGMENT_INDEX(hi_mem+SEGMENT_SIZE-1);
    ik_ulong vec_size = (hi_seg - lo_seg) * IK_PAGESIZE;
    ikptr dvec = ik_mmap(vec_size);
    bzero((char*)(long)dvec, vec_size);
    pcb->dirty_vector_base = (unsigned*)(long) dvec;
    pcb->dirty_vector = (dvec - lo_seg * IK_PAGESIZE);
    ikptr svec = ik_mmap(vec_size);
    bzero((char*)(long)svec, vec_size);
    pcb->segment_vector_base = (unsigned*)(long)svec;
    pcb->segment_vector = (unsigned*)(long)(svec - lo_seg * IK_PAGESIZE);
    pcb->memory_base = (ikptr)(lo_seg * SEGMENT_SIZE);
    pcb->memory_end = (ikptr)(hi_seg * SEGMENT_SIZE);
    set_segment_type(pcb->heap_base,  pcb->heap_size,  mainheap_mt,  pcb);
    set_segment_type(pcb->stack_base, pcb->stack_size, mainstack_mt, pcb);
  }
  { /* initialize base rtd */
    ikptr s_base_rtd = ik_unsafe_alloc(pcb, IK_ALIGN(rtd_size)) | rtd_tag;
    IK_REF(s_base_rtd, off_rtd_rtd)     = s_base_rtd;
    IK_REF(s_base_rtd, off_rtd_length)  = (ikptr) (rtd_size-wordsize);
    IK_REF(s_base_rtd, off_rtd_name)    = 0;
    IK_REF(s_base_rtd, off_rtd_fields)  = 0;
    IK_REF(s_base_rtd, off_rtd_printer) = 0;
    IK_REF(s_base_rtd, off_rtd_symbol)  = 0;
    pcb->base_rtd = s_base_rtd;
  }
  return pcb;
}


void
ik_delete_pcb (ikpcb* pcb)
{
  ikpage* p = pcb->cached_pages;
  pcb->cached_pages = 0;
  pcb->uncached_pages = 0;
  while (p) {
    ik_munmap(p->base, IK_PAGESIZE);
    p = p->next;
  }
  ik_munmap(pcb->cached_pages_base, pcb->cached_pages_size);
  {
    int i;
    for(i=0; i<generation_count; i++) {
      ik_ptr_page* p = pcb->protected_list[i];
      while (p) {
        ik_ptr_page* next = p->next;
        ik_munmap((ikptr)(long)p, IK_PAGESIZE);
	p = next;
      }
    }
  }
  ikptr     base        = pcb->memory_base;
  ikptr     end         = pcb->memory_end;
  unsigned* segment_vec = pcb->segment_vector;
  long i = IK_PAGE_INDEX(base);
  long j = IK_PAGE_INDEX(end);
  for (; i < j; ++i) {
    unsigned t = segment_vec[i];
    if (t != hole_mt) {
      ik_munmap((ikptr)(i<<IK_PAGESHIFT), IK_PAGESIZE);
    }
  }
  long vecsize = (SEGMENT_INDEX(end) - SEGMENT_INDEX(base)) * IK_PAGESIZE;
  ik_munmap((ikptr)(long)pcb->dirty_vector_base, vecsize);
  ik_munmap((ikptr)(long)pcb->segment_vector_base, vecsize);
  ik_free(pcb, sizeof(ikpcb));
}


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
    /* There  is room  in the  current heap  block: update  the  pcb and
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
   heap segment is  allocated; is such allocation fails:  the process is
   terminated with exit status EXIT_FAILURE.

   This  function  is  meant  to  be used  to  allocate  "small"  memory
   blocks. */
{
  assert(requested_size == IK_ALIGN(requested_size));
  ikptr alloc_ptr       = pcb->allocation_pointer;
  ikptr end_ptr         = pcb->heap_base + pcb->heap_size;
  ikptr new_alloc_ptr   = alloc_ptr + requested_size;
  /* If there  is room in the  current heap segment: update  the PCB and
     return the offset. */
  if (new_alloc_ptr < end_ptr) {
    pcb->allocation_pointer = new_alloc_ptr;
    return alloc_ptr;
  } else {
    /* No room in the current heap block: enlarge the heap by allocating
       a new segment. */
    if (alloc_ptr) {
      /* This is not the first heap segment allocation, so prepend a new
	 "ikpages"  node to  the linked  list of  old heap  segments and
	 initialise it with a reference to the current heap segment. */
      ikpages *	p = ik_malloc(sizeof(ikpages));
      p->base = pcb->heap_base;
      p->size = pcb->heap_size;
      p->next = pcb->heap_pages;
      pcb->heap_pages = p;
    }
    { /* accounting */
      long bytes = ((long)pcb->allocation_pointer) - ((long)pcb->heap_base);
      long minor = bytes + pcb->allocation_count_minor;
      while (minor >= IK_MOST_BYTES_IN_MINOR) {
	minor -= IK_MOST_BYTES_IN_MINOR;
	pcb->allocation_count_major++;
      }
      pcb->allocation_count_minor = minor;
    }
    { /* Allocate a  new heap  segment and register  it as  current heap
	 base.  While  computing the segment size: make  sure that there
	 is always  some room at the  end of the new  heap segment after
	 allocating the requested memory for the new object. */
      ik_ulong new_size = (requested_size > IK_HEAP_EXT_SIZE) ? requested_size : IK_HEAP_EXT_SIZE;
      new_size       += 2 * 4096;
      new_size       = IK_ALIGN_TO_NEXT_PAGE(new_size);
      alloc_ptr      = ik_mmap_mixed(new_size, pcb);
      pcb->heap_base = alloc_ptr;
      pcb->heap_size = new_size;
      pcb->allocation_redline = alloc_ptr + new_size - 2 * 4096;
      new_alloc_ptr = alloc_ptr + requested_size;
      pcb->allocation_pointer = new_alloc_ptr;
      return alloc_ptr;
    }
  }
}


void
ik_debug_message (const char * error_message, ...)
{
  va_list        ap;
  va_start(ap, error_message);
  fprintf(stderr, "*** Vicare debug message: ");
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
{
#ifdef VICARE_DEBUGGING
  ik_debug_message("entered ik_stack_overflow pcb=0x%016lx", (long)pcb);
#endif
  set_segment_type(pcb->stack_base, pcb->stack_size, data_mt, pcb);
  ikptr frame_base        = pcb->frame_base;
  ikptr underflow_handler = IK_REF(frame_base, -wordsize);
#ifdef VICARE_DEBUGGING
  ik_debug_message("underflow_handler = 0x%08x", (int)underflow_handler);
#endif
  /* capture continuation and set it as next_k */
  ikptr k = ik_unsafe_alloc(pcb, IK_ALIGN(continuation_size)) | vector_tag;
  IK_REF(k, -vector_tag)           = continuation_tag;
  IK_REF(k, off_continuation_top)  = pcb->frame_pointer;
  IK_REF(k, off_continuation_size) = pcb->frame_base - pcb->frame_pointer - wordsize;
  IK_REF(k, off_continuation_next) = pcb->next_k;
  pcb->next_k                      = k;

  pcb->stack_base    = (ikptr)(long)ik_mmap_typed(STAKSIZE, mainstack_mt, pcb);
  pcb->stack_size    = STAKSIZE;
  pcb->frame_base    = pcb->stack_base + pcb->stack_size;
  pcb->frame_pointer = pcb->frame_base - wordsize;
  pcb->frame_redline = pcb->stack_base + 2 * 4096;
  IK_REF(pcb->frame_pointer, 0) = underflow_handler;
  return;
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


ikptr
ikrt_make_code (ikptr codesizeptr, ikptr freevars, ikptr rvec, ikpcb* pcb)
{
  assert((fx_mask & (int)codesizeptr) == 0);
  long   code_size = IK_UNFIX(codesizeptr);
  long   memreq    = IK_ALIGN_TO_NEXT_PAGE(disp_code_data + code_size);
  ikptr  mem       = ik_mmap_code(memreq, 0, pcb);
  bzero((char*)(long)mem, memreq);
  IK_REF(mem, disp_code_code_tag)     = code_tag;
  IK_REF(mem, disp_code_code_size)    = codesizeptr;
  IK_REF(mem, disp_code_freevars)     = freevars;
  IK_REF(mem, disp_code_reloc_vector) = rvec;
  IK_REF(mem, disp_code_annotation)   = IK_FALSE_OBJECT;
  ik_relocate_code(mem);
  return mem | vector_tag;
}
ikptr
ikrt_set_code_reloc_vector (ikptr s_code, ikptr s_vec, ikpcb* pcb)
{
  IK_REF(s_code, off_code_reloc_vector) = s_vec;
  ik_relocate_code(s_code - vector_tag);
  ((unsigned*)(long)pcb->dirty_vector)[IK_PAGE_INDEX(s_code)] = -1;
  return IK_VOID_OBJECT;
}
ikptr
ikrt_set_code_annotation (ikptr s_code, ikptr s_annot, ikpcb* pcb)
{
  IK_REF(s_code, off_code_annotation) = s_annot;
  ((unsigned*)(long)pcb->dirty_vector)[IK_PAGE_INDEX(s_code)] = -1;
  return IK_VOID_OBJECT;
}


ikptr
ikrt_bvftime (ikptr outbv, ikptr fmtbv)
{
  time_t	t;
  struct tm *	tmp;
  int		rv;
  t     = time(NULL);
  errno = 0;
  tmp   = localtime(&t);
#ifdef VICARE_DEBUGGING
  if (tmp == NULL)
    ik_debug_message("error in time: %s\n", strerror(errno));
#endif
  errno = 0;
  rv    = strftime((char*)(long)(outbv + off_bytevector_data),
		   IK_UNFIX(IK_REF(outbv, off_bytevector_length)) + 1,
		   (char*)(long)(fmtbv + off_bytevector_data),
		   tmp);
#ifdef VICARE_DEBUGGING
  if (rv == 0)
    ik_debug_message("error in strftime: %s\n", strerror(errno));
#endif
  return IK_FIX(rv);
}


/** --------------------------------------------------------------------
 ** Guardians handling.
 ** ----------------------------------------------------------------- */

/* Reference words to guardians  are stored in array "protected_list" of
   the structure  "ik_ptr_page"; such structures  are nodes in  a linked
   referenced  by  "pcb->protected_list[IK_GUARDIANS_GENERATION_NUMBER].
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
  ik_ptr_page *	first;
  first = pcb->protected_list[IK_GUARDIANS_GENERATION_NUMBER];
  if ((NULL == first) || (IK_PTR_PAGE_SIZE == first->count)) {
    assert(sizeof(ik_ptr_page) == IK_PAGESIZE);
    ik_ptr_page *	new_node;
    new_node        = (ik_ptr_page*)(long)ik_mmap(IK_PAGESIZE);
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


ikptr
ikrt_stats_now (ikptr t, ikpcb* pcb)
{
  struct rusage r;
  struct timeval s;
  gettimeofday(&s, 0);
  getrusage(RUSAGE_SELF, &r);
  /* Do  not  change the  order  of the  fields!!!   It  must match  the
     implementation     of     the     record    type     "stats"     in
     "scheme/ikarus.timer.ss". */
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
    long bytes_in_heap	= ((long)pcb->allocation_pointer) - ((long)pcb->heap_base);
    long bytes		= bytes_in_heap + pcb->allocation_count_minor;
    IK_FIELD(t, 13)	= IK_FIX(bytes);
  }
  /* major bytes */
  IK_FIELD(t, 14) = IK_FIX(pcb->allocation_count_major);
  return IK_VOID_OBJECT;
}


ikptr
ikrt_make_vector1 (ikptr s_len, ikpcb* pcb)
{
  int intlen = (int)s_len;
  if (IK_IS_FIXNUM(s_len) && (intlen >= 0)) {
    ikptr s = ik_safe_alloc(pcb, IK_ALIGN(s_len + disp_vector_data));
    IK_REF(s, 0) = s_len;
    memset((char*)(long)(s+disp_vector_data), 0, s_len);
    return s | vector_tag;
  } else
    return 0;
}
#if 0
ikptr
ikrt_make_vector2 (ikptr len, ikptr obj, ikpcb* pcb)
{
  if (IK_IS_FIXNUM(len) && ((len >> 31)!=0)) {
    pcb->root0 = &obj;
    ikptr s = ik_safe_alloc(pcb, IK_ALIGN(((int)len) + disp_vector_data));
    pcb->root0 = 0;
    ref(s, 0) = len;
    memset(s+disp_vector_data, 0, (int)len);
    return s+vector_tag;
  } else {
    return IK_FALSE_OBJECT;
  }
}
#endif


/** --------------------------------------------------------------------
 ** Termination.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_exit (ikptr status, ikpcb* pcb)
/* This is nor for the public API. */
{
  ik_delete_pcb(pcb);
  assert(0 == total_allocated_pages);
  exit(IK_IS_FIXNUM(status)? IK_UNFIX(status) : EXIT_FAILURE);
}

/* end of file */
