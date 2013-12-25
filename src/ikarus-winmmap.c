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

#if ((defined __CYGWIN__) || (defined __FAKE_CYGWIN__))


/* THE PROBLEM
 * ===========
 *
 * While  on Unix  systems  (at  least those  supported  by Vicare)  the
 * allocation granularity  of "mmap()" is  2^12 = 4096 bytes,  on Cygwin
 * the granularity is 2^16 = 4096 * 2^4 = 4096 * 16.
 *
 *   This is a  "big" granularity.  While on Unix systems  we can define
 * the size of  Vicare pages IK_PAGESIZE to the granularity  4096, we do
 * not want to define IK_PAGESIZE to  2^16 on Cygwin because it probably
 * would  not be  efficient with  respect to  garbage collection  needs.
 * See:
 *
 *    R. Kent Dybvig, David Eby, Carl Bruggeman.  "Don't Stop the BIBOP:
 *    Flexible and  Efficient Storage  Management for  Dynamically Typed
 *    Languages".   Indiana  University   Computer  Science  Department.
 *    Technical Report #400.  March 1994.
 *
 *   What we do is to define wrappers for "mmap()" and "munmap()" called
 * "win_mmap()" and  "win_munmap()" and use  them on Cygwin in  place of
 * the original functions.  With such API we allocate and release memory
 * with granularity 2^16,  but split the memory blocks in  pages of 2^12
 * bytes and register them in a table:
 *
 * - When  allocating: only  pages  of  size 2^12  are  returned to  the
 *   caller, the other pages are marked as free in the table.
 *
 * - When releasing: pages of size 2^12 are marked as free in the table;
 *   when a  full block  of 2^16 bytes  is free, it  is returned  to the
 *   system.
 *
 * this API should completely shield Vicare from the underlying "mmap()"
 * behaviour.
 */


/* CYGWIN SEGMENTS
 *
 * Memory for use  by the Scheme program is allocated  on Cygwin through
 * "mmap()" in blocks  called "Cygwin segments".  A  Cygwin segment size
 * equals the  allocation granularity of "mmap()":
 *
 *    2^16 = 2^4 * 4096 = 16 * 4096 = 16 * IK_PAGESIZE
 *
 * so a Cygwin segment contains 16 contiguous Vicare pages.
 *
 *   In this  module we always  allocate memory with "mmap()"  in blocks
 * whose size is an exact multiple  of a Cygwin segment size.  We assume
 * that "mmap()" returns pointers such that:
 *
 * - The pointer references the first byte of a platform's system page.
 *
 * - When  we allocate  chunks of  size multiple  of the  Cygwin segment
 *   size, the  numeric address of the  pointer is an exact  multiple of
 *   2^16 (the 16 least significant bits in the pointer are zero).
 *
 *   Cygwin  segments are  absolute portions  of  the memory  seen by  a
 * running system process; it is natural to assign a zero-based index to
 * each Cygwin segment:
 *
 *     Cygwin segment Cygwin segment Cygwin segment Cygwin segment
 *    |--------------|--------------|--------------|--------------|
 *     ^              ^              ^              ^
 *    0x00000        0x10000        0x20000        0x30000
 *    index 0        index 1        index 2        index 3
 *
 * if we  know the  starting address  ADDR of a  Cygwin segment,  we can
 * compute its absolute index IDX as:
 *
 *    IDX = ADDR >> 16		example: 1 << 16 = 0x1000
 *
 * and if we know the absolute index of a Cygwin segment, we can compute
 * its starting address as:
 *
 *    ADDR = IDX << 16		example: 0x1000 << 16 = 1
 */


/* A DISCARDED SOLUTION
 * ====================
 *
 * It is attractive to organise the segments table in a way similar (but
 * not  equal) to  the one  used in  the PCB  structure to  organise the
 * segments vector and the dirty vector.  It would go like this:
 *
 * - We define the global variables:
 *
 *      static uint16_t * cygwin_segments    = NULL;
 *      static uint8_t *  cygwin_memory_base = NULL;
 *      static uint8_t *  cygwin_memory_end  = NULL;
 *
 *   in which "cygwin_segments"  is the vector of  Cygwin segments; each
 *   slot  represents the  state  of a  Cygwin segment;  each  bit in  a
 *   "uint16_t" slot represents the state of a Vicare page in the Cygwin
 *   segment: 0 free, 1 used.
 *
 *     The segments vector  contains a slot for every  Cygwin segment in
 *   the region of memory  delimited by the globals "cygwin_memory_base"
 *   (included) and "cygwin_memory_end" (excluded).
 *
 *     Notice that  the segments  vector *is*  itself registered  in the
 *   segments vector:  its memory falls  inside the region  delimited by
 *   "cygwin_memory_base" and  "cygwin_memory_end", and it is  marked as
 *   "used".
 *
 * - The first slot in the segments  vector holds the descriptor for the
 *   segment starting at "cygwin_memory_base".  Given a pointer ADDR the
 *   actual  index of  the slot  describing its  segment is  computed as
 *   follows:
 *
 *      base_segment_idx = cygwin_memory_base >> 16;
 *      addr_segment_idx = addr               >> 16;
 *      addr_slot_idx    = addr_segment_idx - base_segment_idx;
 *
 * - At start up we  preallocate a big slab of memory;  we might want 32
 *   MiB (where: 1 MiB = 1024 * 1024 bytes):
 *
 *      #define PREALLOCATED_MEMORY_SIZE (32 * (1024 * 1024))
 *      #define PREALLOCATED_NUMBER_OF_SEGMENTS \
 *         (PREALLOCATED_MEMORY_SIZE / CYGWIN_SEGMENT_SIZE)
 *      #define INITIAL_SEGVEC_SIZE \
 *         (sizeof(uint16_t) * PREALLOCATED_NUMBER_OF_SEGMENTS)
 *
 *      cygwin_memory_base = do_mmap(PREALLOCATED_MEMORY_SIZE);
 *      cygwin_memory_end  = cygwin_memory_base + PREALLOCATED_MEMORY_SIZE;
 *      cygwin_segments	   = (uint16_t*)cygwin_memory_base;
 *
 *   and mark all the pages as free:
 *
 *      memset(cygwin_segments, 0, INITIAL_SEGVEC_SIZE);
 *
 *   then mark as used all the pages in the segments vector itself:
 *
 *      base_idx = CYGWIN_SEGMENT_INDEX(cygwin_memory_base);
 *      range    = CYGWIN_SEGMENT_INDEX_RANGE(INITIAL_SEGVEC_SIZE);
 *      for (segment_idx=base_idx; segment_idx<range; ++segment_idx)
 *        cygwin_segments[segment_idx] = 0xFFFF;
 *
 * - We define the global variables:
 *
 *      static char *  ap = NULL;
 *      static size_t  as = 0;
 *
 *   and after preallocating memory we set them as:
 *
 *      ap = cygwin_memory_base;
 *      as = PREALLOCATED_MEMORY_SIZE - INITIAL_SEGVEC_SIZE;
 *
 *   AP references the next free word in the current allocation segment,
 *   AS is  the number  of bytes  available from  AP to  the end  of the
 *   segment:
 *
 *                               AS
 *                |................................|
 *      ----------|--------------------------------| current segment
 *                 ^
 *                 AP
 *
 * - Whenever we reserve a block of size SIZE, we do:
 *
 *      ptr  = ap;
 *      ap  += size;
 *      as  -= size;
 *
 *   and PTR references the first word in the reserved block.
 *
 * - When the current allocation segment has not enough room
 *
 *static void
 *extend_segments_vectors_maybe (uint8_t * base_ptr, ik_ulong size)
 *{
 *  uint8_t *     end_ptr = base_ptr + size;
 *  if (base_ptr < cygwin_memory_base) {
 *    ik_ulong new_lo_seg   = CYGWIN_SEGMENT_INDEX(base_ptr);
 *    ik_ulong old_lo_seg   = CYGWIN_SEGMENT_INDEX(cygwin_memory_base);
 *    ik_ulong hi_seg       = CYGWIN_SEGMENT_INDEX(cygwin_memory_end); // unchanged
 *    ik_ulong new_vec_size = (hi_seg - new_lo_seg) * sizeof(uint16_t);
 *    ik_ulong old_vec_size = (hi_seg - old_lo_seg) * sizeof(uint16_t);
 *    ik_ulong size_delta   = new_vec_size - old_vec_size;
 *    { // Allocate a new Cygwin segments vector.  The old slots go to the
 *	// tail of the  new vector; the head  of the new vector  is set to
 *	// zero.
 *      uint16_t *        new_vec_base = (uint16_t*)do_mmap(CYGWIN_ALIGN(new_vec_size));
 *      memset((char*)new_vec_base, 0, size_delta);
 *      memcpy((char*)(new_vec_base + size_delta), (char*)cygwin_segments, old_vec_size);
 *      do_munmap((char*)cygwin_segments, old_vec_size);
 *      cygwin_segments = new_vec_base;
 *    }
 *    cygwin_memory_base = (uint8_t*)(new_lo_seg * CYGWIN_SEGMENT_SIZE);
 *  } else if (end_ptr >= cygwin_memory_end) {
 *    ik_ulong lo_seg       = CYGWIN_SEGMENT_INDEX(cygwin_memory_base); // unchanged
 *    ik_ulong old_hi_seg   = CYGWIN_SEGMENT_INDEX(cygwin_memory_end);
 *    ik_ulong new_hi_seg   = CYGWIN_SEGMENT_INDEX(end_ptr + CYGWIN_SEGMENT_SIZE - 1);
 *    ik_ulong new_vec_size = (new_hi_seg - lo_seg) * sizeof(uint16_t);
 *    ik_ulong old_vec_size = (old_hi_seg - lo_seg) * sizeof(uint16_t);
 *    ik_ulong size_delta   = new_vec_size - old_vec_size;
 *    { // Allocate a new new Cygwin segments vector.  The old slots go to
 *	// the head of the  new vector; the tail of the  new vector is set
 *	// to zero.
 *      uint16_t *        new_vec_base = (uint16_t*)do_mmap(CYGWIN_ALIGN(new_vec_size));
 *      memcpy((char*)new_vec_base, (char*)cygwin_segments, old_vec_size);
 *      memset((char*)(new_vec_base + old_vec_size), 0, size_delta);
 *      do_munmap((char*)cygwin_segments, old_vec_size);
 *      cygwin_segments = new_vec_base;
 *    }
 *    cygwin_memory_end = (uint8_t*)(new_hi_seg * CYGWIN_SEGMENT_SIZE);
 *  }
 *}
 *
 *
 */


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"
#include <sys/mman.h>


/** --------------------------------------------------------------------
 ** Internal definitions: Cygwin segments.
 ** ----------------------------------------------------------------- */

/* DISCUSSION ABOUT "CYGWIN_SEGMENT_SIZE" and "CYGWIN_SEGMENT_SHIFT"
 * =================================================================
 *
 * CYGWIN_SEGMENT_SHIFT is the  number of bits to  right-shift a pointer
 * to  obtain the  index of  the Cygwin  segment containing  the pointer
 * itself; it is the number for which:
 *
 *    CYGWIN_SEGMENT_SIZE >> CYGWIN_SEGMENT_SHIFT = 1
 *    2^CYGWIN_SEGMENT_SHIFT = CYGWIN_SEGMENT_SIZE
 *
 * scenario (in this  picture 3 pages per segment, rather  than the true
 * 16 pages):
 *
 *     Cygwin segment Cygwin segment Cygwin segment
 *    |--------------|--------------|--------------|
 *     page page page page page page page page page
 *    |----|----|----|----|----|----|----|----|----|
 *                           ^
 *                           X
 *    |----|----|----|----|----|----|----|----|----| page indexes
 *      P   P+1  P+2  P+3  P+4  P+5  P+6  P+7  P+7
 *
 *    |----|----|----|----|----|----|----|----|----| segment indexes
 *      S             S+1            S+2
 *
 * remembering that IK_PAGESHIFT is defined to 12, we have:
 *
 *     X >> IK_PAGESHIFT         == IK_PAGE_INDEX(X)        == P+4
 *     X >> CYGWIN_SEGMENT_SHIFT == CYGWIN_SEGMENT_INDEX(X) == S+1
 */
#define CYGWIN_SEGMENT_SIZE	(16 * IK_PAGESIZE)
#define CYGWIN_SEGMENT_SHIFT	(4  + IK_PAGESHIFT)

#define CYGWIN_ALIGN(SIZE) \
  ((((ik_ulong)(SIZE) + CYGWIN_SEGMENT_SIZE - 1) >> CYGWIN_SEGMENT_SHIFT) << CYGWIN_SEGMENT_SHIFT)

#define CYGWIN_SEGMENT_INDEX(X)	\
  (((ik_ulong)(X)) >> CYGWIN_SEGMENT_SHIFT)

#define CYGWIN_SEGMENT_SLOT(X) \
  (CYGWIN_SEGMENT_INDEX(X)  - CYGWIN_SEGMENT_INDEX(cygwin_memory_base))

/* Given  a  number  of  bytes  SIZE  as  "ik_ulong":  evaluate  to  the
   difference between  two Cygwin segment indexes  representing a region
   big enough to hold SIZE bytes. */
#define CYGWIN_SEGMENT_INDEX_RANGE(SIZE) \
  CYGWIN_SEGMENT_INDEX(SIZE)

#define CYGWIN_SEGMENT_INDEX_FROM_PAGE_INDEX(PAGE_IDX) \
  ((PAGE_IDX) / 16)

#define CYGWIN_SEGMENT_INDEX_TO_POINTER(SEGMENT_IDX) \
  ((void*)((SEGMENT_IDX) * CYGWIN_SEGMENT_SIZE))

#define MINIMUM_CYGWIN_SEGMENTS_NUMBER_FOR_SIZE(SIZE) \
  (((SIZE) + CYGWIN_SEGMENT_SIZE - 1) >> CYGWIN_SEGMENT_SHIFT)


/** --------------------------------------------------------------------
 ** Internal definitions: preallocated memory.
 ** ----------------------------------------------------------------- */

/* Initial number of byes allocated.  We want 32 MiB, where:
 *
 *   1 MiB = 1024 * 1024 bytes
 *
 */
#ifndef PREALLOCATED_NUMBER_OF_MIBS
#  define PREALLOCATED_NUMBER_OF_MIBS	32L
#endif
#define PREALLOCATED_MEMORY_SIZE	\
  (PREALLOCATED_NUMBER_OF_MIBS * (1024L * 1024L))
#define PREALLOCATED_NUMBER_OF_SEGMENTS	\
  (PREALLOCATED_MEMORY_SIZE / CYGWIN_SEGMENT_SIZE)
#define PREALLOCATED_NUMBER_OF_PAGES	\
  (PREALLOCATED_MEMORY_SIZE / IK_PAGESIZE)

/* The number  of bytes to reserve  for the Cygwin segments  vector.  We
   want a 16-bit slot for every Cygwin segment. */
#define INITIAL_SEGMENTS_VECTOR_SIZE		\
  (sizeof(uint16_t) * PREALLOCATED_NUMBER_OF_SEGMENTS)


/** --------------------------------------------------------------------
 ** Internal definitions: global memory variables.
 ** ----------------------------------------------------------------- */

/* The vector of  Cygwin segments.  Each slot represents the  state of a
 * Cygwin segment.  Each  bit in a "uint16_t" represents the  state of a
 * Vicare page in the Cygwin segment: 0 free, 1 used.
 *
 *   The segments vector contains a slot for every Cygwin segment in the
 * region  of   memory  delimited  by  the   globals  CYGWIN_MEMORY_BASE
 * (included) and CYGWIN_MEMORY_END (excluded).
 *
 *   Notice  that the  segments  vector *is*  itself  registered in  the
 * segments  vector: its  memory falls  inside the  region delimited  by
 * CYGWIN_MEMORY_BASE and CYGWIN_MEMORY_END, and it is marked as "used".
 *
 *   The first slot in the segments  vector holds the descriptor for the
 * segment  starting at  CYGWIN_MEMORY_BASE.  Given  a pointer  ADDR the
 * actual  index of  the  slot  describing its  segment  is computed  as
 * follows:
 *
 *   base_segment_idx = CYGWIN_SEGMENT_INDEX(cygwin_memory_base);
 *   addr_segment_idx = CYGWIN_SEGMENT_INDEX(addr);
 *   addr_slot_idx    = addr_segment_idx - base_segment_idx;
 */
static uint16_t *	cygwin_segments;
static uint8_t *	cygwin_memory_base = NULL;
static uint8_t *	cygwin_memory_end  = NULL;

/* Allocation  pointer  and  size.  After  initialising  the  allocation
 * subsystem: AP references the next free word in the current allocation
 * segment, AS is te number of bytes available from AP to the end of the
 * segment.
 *
 *                               AS
 *              |................................|
 *    ----------|--------------------------------| current segment
 *               ^
 *               AP
 *
 *   Whenever we reserve a block of size SIZE, we do:
 *
 *    ptr  = ap;
 *    ap  += size;
 *    as  -= size;
 *
 * and PTR references the first word in the reserved block.
 */
static char *	ap = NULL;
static size_t	as = 0;


/** --------------------------------------------------------------------
 ** Fake memory mapped interface.
 ** ----------------------------------------------------------------- */

/* For debugging purposes only, on Unix systems we can compile with:
 *
 *   $ make CFLAGS='-D__FAKE_CYGWIN__ -pedantic -g'
 *
 * this  will enable  the use  of the  "win_*map()" API  along with  the
 * functions  below, which  fake system  memory allocation  by returning
 * pointers whose 16 least significant bits are set to zero.  This means
 * we fake allocation with granularity 2^16.
 */
#ifdef __FAKE_CYGWIN__

/* Given a  pointer PTR returned  by "mmap()": return the  least pointer
   after it that references the first word in a Cygwin segment. */
#define TOSEG(PTR) \
  ((uint8_t *)CYGWIN_ALIGN(((ik_ulong)(PTR)) + CYGWIN_SEGMENT_SIZE))

static void *
fake_mmap (void * addr IK_UNUSED, size_t length IK_UNUSED,
	   int    prot IK_UNUSED, int    flags  IK_UNUSED,
	   int    fd   IK_UNUSED, off_t  offset IK_UNUSED)
{
  size_t	mapsize = CYGWIN_ALIGN(length + CYGWIN_SEGMENT_SIZE);
  uint8_t *	base;
  uint8_t *	mem;
  base    = mmap(0, mapsize, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
  if (((void*)-1) == base)
    ik_abort("failed to fake mmap: %s", strerror(errno));
  mem = (void*)CYGWIN_ALIGN(base);
  if (0)
    ik_debug_message("%s: allocated base=0x%016lx, mem=0x%016lx, %ld bytes, %ld segments, %ld pages", __func__,
		     base, mem, mapsize, mapsize >> CYGWIN_SEGMENT_SHIFT,
		     IK_MINIMUM_PAGES_NUMBER_FOR_SIZE(mapsize));
  return mem;
}

#endif /* defined __FAKE_CYGWIN__ */


/** --------------------------------------------------------------------
 ** Low level memory mapped interface.
 ** ----------------------------------------------------------------- */

static void*
do_mmap (size_t length)
/* Actual interface to the platform's "mmap()" function. */
{
  if (0)
    ik_debug_message("%s: requested size=%ld", __func__, length);
  assert(0 == (length % CYGWIN_SEGMENT_SIZE));
#ifndef __FAKE_CYGWIN__
  void * mem =      mmap(0, length, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
#else
  void * mem = fake_mmap(0, length, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
#endif
  if (((void*)-1) == mem)
    ik_abort("failed to mmap: %s", strerror(errno));
  if (0)
    ik_debug_message("%s: allocated mem=0x%016lx, %ld bytes, %ld segments, %ld pages", __func__,
		     mem, length, length >> CYGWIN_SEGMENT_SHIFT,
		     IK_MINIMUM_PAGES_NUMBER_FOR_SIZE(length));
  /* Assert that the  16 least significant bits in MEM  are set to zero:
     this means MEM is a pointer  to the beginning of an absolute memory
     Cygwin segment. */
  assert(((-CYGWIN_SEGMENT_SIZE) & (ik_ulong)mem) == (ik_ulong)mem);
  assert(0 == (((ik_ulong)mem) & (CYGWIN_SEGMENT_SIZE-1)));
  assert(0 == ((ik_ulong)0xFFF & (ik_ulong)mem));
  return mem;
}
static void
do_munmap (void* addr, size_t size)
/* Actual interface to the platform's "munmap()" function. */
{
  int err =      munmap(addr, size);
  if (err)
    ik_abort("failed to unmap");
}


/** --------------------------------------------------------------------
 ** Public memory mapped API.
 ** ----------------------------------------------------------------- */

static void extend_segments_vectors_maybe (uint8_t * base_ptr, ik_ulong size);

char*
win_mmap (size_t size)
/* Public interface  to "mmap()".  SIZE must  be the number of  bytes to
   allocate; it  is an  exact multiple  of IK_PAGESIZE  and it  can span
   multiple Cygwin segments. */
{
  if (0)
    ik_debug_message("%s: requested size=%ld", __func__, size);
  assert(0 == (size % IK_PAGESIZE));
  if (size <= as) {
    /* There is room in the  current allocation Cygwin segment.  Reserve
       the requested size and return a pointer to it. */
    char *	x = ap;
    ap += size;
    as -= size;
    if (0)
      ik_debug_message("%s: reserved %ld bytes in segment slots beg=%ld end=%ld", __func__,
		       size, CYGWIN_SEGMENT_SLOT(x), CYGWIN_SEGMENT_SLOT(ap));
    return x;
  } else if (NULL == cygwin_segments) {
    /* Preallocate  memory and  initialise the  Cygwin segments  vector.
       The segments  vector itself  is located at  the beginning  of the
       preallocated memory; mark as used the pages in which the segments
       vector is stored. */
    cygwin_memory_base  = do_mmap(PREALLOCATED_MEMORY_SIZE);
    cygwin_memory_end	= cygwin_memory_base + PREALLOCATED_MEMORY_SIZE;
    cygwin_segments	= do_mmap(CYGWIN_ALIGN(INITIAL_SEGMENTS_VECTOR_SIZE));
    /* Mark all the pages as used. */
    memset(cygwin_segments, 0xFF, INITIAL_SEGMENTS_VECTOR_SIZE);
    /* The preallocated memory becomes the current allocation block. */
    ap = (char *)cygwin_memory_base;
    as = PREALLOCATED_MEMORY_SIZE;
    if (0)
      ik_debug_message("%s: as=%ld, ap=0x%016lx, base_idx=%ld, end_slot=%ld", __func__,
		       as, ap, CYGWIN_SEGMENT_INDEX(cygwin_memory_base),
		       CYGWIN_SEGMENT_SLOT(cygwin_memory_end));
    /* Assert that the 16 least significant  bits in AP are set to zero:
       this means AP is a pointer to the beginning of an absolute memory
       Cygwin segment. */
    assert(0 == (((ik_ulong)ap) & (CYGWIN_SEGMENT_SIZE-1)));
    {
      char *	x = ap;
      ap += size;
      as -= size;
      return x;
    }
  } else {
    /* There is no room in the current allocation block. */
    /* Mark  the unreserved  pages in  the current  allocation block  as
       free. */
    win_munmap(ap, as);
    /* Allocate  a new  block  of  memory spanning  one  or more  Cygwin
       segments. */
    {
      size_t	number_of_segments = MINIMUM_CYGWIN_SEGMENTS_NUMBER_FOR_SIZE(size);
      size_t	aligned_size       = number_of_segments << CYGWIN_SEGMENT_SHIFT;
      char *	addr               = do_mmap(aligned_size);
      extend_segments_vectors_maybe((uint8_t*)addr, aligned_size);
      /* By setting all the  bits to 1 we mark as used  all the pages in
	 the allocated Cygwin segments. */
      {
	ik_ulong addr_slot_idx    = CYGWIN_SEGMENT_SLOT(addr);
	for (ik_ulong i=0; i<number_of_segments; ++i) {
	  cygwin_segments[i + addr_slot_idx] = 0xFFFF;
	}
      }
      /* Set up the current allocation segment with the requested memory
	 reserved. */
      ap = addr         + size;
      as = aligned_size - size;
      return addr;
    }
  }
}
void
win_munmap (char* addr, size_t size)
{
  /* Assert that the 12 least significant  bits in ADDR are set to zero:
     this means ADDR is a pointer to the beginning of an absolute Vicare
     memory page. */
  assert(((-IK_PAGESIZE) & (ik_ulong)addr) == (ik_ulong)addr);
  /* Assert that the number of bytes to free is an exact multiple of the
     Vicare page size. */
  assert(0 == (size % IK_PAGESIZE));
  ik_ulong	base_segment_idx = CYGWIN_SEGMENT_INDEX(cygwin_memory_base);
  while (size) {
    ik_ulong	addr_segment_idx = CYGWIN_SEGMENT_INDEX(addr);
    ik_ulong	addr_slot_idx    = addr_segment_idx - base_segment_idx;
    uint16_t	segment_bits     = cygwin_segments[addr_slot_idx];
    /* Compute the page bit offset  by isolating the 4 least significant
       bits in the page index (15 == 0b1111 == 0xF). */
    ik_ulong	addr_page_idx        = IK_PAGE_INDEX(addr);
    ik_ulong	addr_page_bit_offset = addr_page_idx & 0xF;
    if (0)
      ik_debug_message("%s: freeing addr slot=%ld, bit offset=%ld", __func__,
		       CYGWIN_SEGMENT_SLOT(addr), addr_page_bit_offset);
    /* Assert that the page being marked as free was used before. */
    assert((segment_bits & (1<<addr_page_bit_offset)) == (1<<addr_page_bit_offset));
    /* Set to  zero the bit  representing the  state of the  page.  This
       marks the page as free. */
    uint16_t	new_bits = segment_bits & (~ (1<<addr_page_bit_offset));
    cygwin_segments[addr_slot_idx] = new_bits;
    /* If the whole Cygwin segment is free: release it to the system. */
    if (0 == new_bits)
      do_munmap(CYGWIN_SEGMENT_INDEX_TO_POINTER(addr_segment_idx), CYGWIN_SEGMENT_SIZE);
    size -= IK_PAGESIZE;
    addr += IK_PAGESIZE;
  }
}
static void
extend_segments_vectors_maybe (uint8_t * base_ptr, ik_ulong size)
{
  uint8_t *	end_ptr = base_ptr + size;
  if (base_ptr < cygwin_memory_base) {
    ik_ulong new_lo_seg   = CYGWIN_SEGMENT_INDEX(base_ptr);
    ik_ulong old_lo_seg   = CYGWIN_SEGMENT_INDEX(cygwin_memory_base);
    ik_ulong hi_seg       = CYGWIN_SEGMENT_INDEX(cygwin_memory_end); /* unchanged */
    ik_ulong new_vec_size = (hi_seg - new_lo_seg) * sizeof(uint16_t);
    ik_ulong old_vec_size = (hi_seg - old_lo_seg) * sizeof(uint16_t);
    ik_ulong size_delta   = new_vec_size - old_vec_size;
    { /* Allocate a new Cygwin segments vector.  The old slots go to the
	 tail of the  new vector; the head  of the new vector  is set to
	 zero. */
      uint16_t *	new_vec_base = (uint16_t*)do_mmap(CYGWIN_ALIGN(new_vec_size));
      memset((uint8_t*)new_vec_base, 0, size_delta);
      memcpy(((uint8_t*)new_vec_base) + size_delta, (uint8_t*)cygwin_segments, old_vec_size);
      do_munmap((uint8_t*)cygwin_segments, old_vec_size);
      cygwin_segments = new_vec_base;
    }
    cygwin_memory_base = (uint8_t*)(new_lo_seg * CYGWIN_SEGMENT_SIZE);
  } else if (end_ptr >= cygwin_memory_end) {
    ik_ulong lo_seg       = CYGWIN_SEGMENT_INDEX(cygwin_memory_base); /* unchanged */
    ik_ulong old_hi_seg   = CYGWIN_SEGMENT_INDEX(cygwin_memory_end);
    ik_ulong new_hi_seg   = CYGWIN_SEGMENT_INDEX(end_ptr + CYGWIN_SEGMENT_SIZE - 1);
    ik_ulong new_vec_size = (new_hi_seg - lo_seg) * sizeof(uint16_t);
    ik_ulong old_vec_size = (old_hi_seg - lo_seg) * sizeof(uint16_t);
    ik_ulong size_delta   = new_vec_size - old_vec_size;
    { /* Allocate a new new Cygwin segments vector.  The old slots go to
	 the head of the  new vector; the tail of the  new vector is set
	 to zero. */
      uint16_t *	new_vec_base = (uint16_t*)do_mmap(CYGWIN_ALIGN(new_vec_size));
      memcpy((uint8_t*)new_vec_base, (uint8_t*)cygwin_segments, old_vec_size);
      memset(((uint8_t*)new_vec_base) + old_vec_size, 0, size_delta);
      do_munmap((uint8_t*)cygwin_segments, old_vec_size);
      cygwin_segments = new_vec_base;
    }
    cygwin_memory_end = (uint8_t*)(new_hi_seg * CYGWIN_SEGMENT_SIZE);
  }
}

#endif

/* end of file */
