/*
  Part of: Vicare
  Contents: internal header file
  Date: Wed Jan 11, 2012

  Abstract

	This   file  contains   internal  definitions.    Many   of  the
	definitions  in this  file are  duplicated in  "vicare.h", which
	defines the public API.

  Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006-2008  Abdulaziz Ghuloum

  This program is  free software: you can redistribute	it and/or modify
  it under the	terms of the GNU General Public	 License as published by
  the Free Software Foundation, either	version 3 of the License, or (at
  your option) any later version.

  This program	is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY	 WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See	 the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef INTERNALS_H
#define INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Helper macros.
 ** ----------------------------------------------------------------- */

/* The macro  IK_UNUSED indicates that a function,  function argument or
   variable may potentially be unused.	Usage examples:

   static int unused_function (char arg) IK_UNUSED;
   int foo (char unused_argument IK_UNUSED);
   int unused_variable IK_UNUSED;
*/
#ifdef __GNUC__
#  define IK_UNUSED		__attribute__((unused))
#else
#  define IK_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif

#if (defined _WIN32 || defined __CYGWIN__)
#  ifdef __GNUC__
#    define ik_decl		__attribute__((dllexport))
#  else
#    define ik_decl		__declspec(dllexport)
#  endif
#  define ik_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define ik_decl		__attribute__((visibility ("default")))
#    define ik_private_decl	__attribute__((visibility ("hidden")))
#  else
#    define ik_decl		extern
#    define ik_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <vicare-platform.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> /* for off_t */

#ifdef HAVE_ASSERT_H
#  include <assert.h>
#endif
#ifdef HAVE_ERRNO_H
#  include <errno.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_NETDB_H
#  include <netdb.h>
#endif
#ifdef HAVE_STDARG_H
#  include <stdarg.h>
#endif
#ifdef HAVE_STDIO_H
#  include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif
#ifdef HAVE_STRING_H
#  include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#  include <strings.h>
#endif
#ifdef HAVE_ARPA_INET_H
#  include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif


/** --------------------------------------------------------------------
 ** Preprocessor definitions: memory pages and segments.
 ** ----------------------------------------------------------------- */

/* Given  a  SIZE  compute  and  return the  minimum  size  multiple  of
 * GRANULARITY that can hold it.
 *
 *    |----------------------------| size
 *    |-----------|-----------|-----------| granularity_size
 *     granularity granularity granularity
 *
 * Notice that GRANULARITY is evaluated multiple times!!!
 */
#define IK_SIZE_TO_GRANULARITY_SIZE(SIZE, GRANULARITY) \
  ((ik_ulong)(((((ik_ulong)(SIZE)) + GRANULARITY - 1) / GRANULARITY) * GRANULARITY))

/* This constant is defined as 4096 = 4 * 1024 = 4 * 2^10 = 2^12.  Never
   change it!!! */
#define IK_CHUNK_SIZE		4096
#define IK_DOUBLE_CHUNK_SIZE	(2 * IK_CHUNK_SIZE)

/* *** DISCUSSION ABOUT "IK_PAGESIZE" AND "IK_PAGESHIFT" ***
 *
 * The    preprocessor   constant    IK_MMAP_ALLOCATION_GRANULARITY   is
 * determined by the "configure" script and defined in the automatically
 * generated  header file  "config.h".   The  constants IK_PAGESIZE  and
 * IK_PAGESHIFT are hard-coded.
 *
 *   The constant  IK_MMAP_ALLOCATION_GRANULARITY represents  the memory
 * allocation  granularity used  by "mmap()":  no matter  the number  of
 * bytes we  request to "mmap()",  it will always allocate  the smallest
 * multiple of the granularity that can contain the requested bytes:
 *
 *    |----------------------------| requested_size
 *    |-----------|-----------|-----------| allocated_size
 *     granularity granularity granularity
 *
 *   On some platforms the allocation granularity equals the system page
 * size (example  GNU+Linux), on  other platforms  it does  not (example
 * Cygwin).  We assume the allocation granularity can be obtained on any
 * platform with:
 *
 *    #include <unistd.h>
 *    long granularity = sysconf(_SC_PAGESIZE);
 *
 * which should "officially"  return the system page size,  but in truth
 * it does not (see Cygwin's documentation).
 *
 *   To  mind its  own business,  Vicare defines  a "page  size" as  the
 * preprocessor symbol IK_PAGESIZE, the number of bytes in Vicare's page
 * size is 4096  = 4 * 1024 =  4 * 2^10 = 2^12 =  #x1000.  Vicare's page
 * size is not defined to be equal to the system page size, but:
 *
 * - Most likely the system page size and Vicare's page size are equal.
 *
 * - We assume that in  any case the system page size is  equal to or an
 *   exact multiple of Vicare's page size.
 *
 * - We assume  that "mmap()"  returns pointers  such that:  the pointer
 *   references the first  byte of a system page, and  so also the first
 *   byte of  a Vicare page;  the numeric address  of the pointer  is an
 *   exact multiple of 4096 (the 12 least significant bits are zero).
 *
 * it is natural to assign a zero-based index to each Vicare page:
 *
 *       page     page     page     page     page     page
 *    |--------|--------|--------|--------|--------|--------|
 *     ^        ^        ^        ^        ^        ^
 *    #x0000   #x1000   #x2000   #x3000   #x4000   #x5000
 *    index 0  index 1  index 2  index 3  index 4  index 5
 *
 *   The  preprocessor symbol  IK_PAGESHIFT  is the  number  of bits  to
 * right-shift a tagged  or untagged pointer to obtain the  index of the
 * page it is in; it is the number for which:
 *
 *    IK_PAGESIZE >> IK_PAGESHIFT = 1
 *    2^IK_PAGESHIFT = IK_PAGESIZE
 *
 * if IK_PAGESIZE is 4096, the value of IK_PAGESHIFT is 12; so we have:
 *
 *    0 * 4096 <=  4000 < 1 * 4096		 4000 >> 12 = 0
 *    1 * 4096 <=  8000 < 2 * 4096		 8000 >> 12 = 1
 *    2 * 4096 <= 10000 < 3 * 4096		10000 >> 12 = 2
 *
 */
#define IK_PAGESIZE		IK_CHUNK_SIZE
#define IK_DOUBLE_PAGESIZE	IK_DOUBLE_CHUNK_SIZE
#define IK_PAGESHIFT		12

/* Given the  tagged or untagged pointer  X as "ikptr": evaluate  to the
   index of  the memory page  it is  in; notice that  the tag bits  of a
   tagged pointer are not influent. */
#define IK_PAGE_INDEX(X)	(((ik_ulong)(X)) >> IK_PAGESHIFT)
/* Given  a  number  of  bytes  SIZE  as  "ik_ulong":  evaluate  to  the
   difference between two page indexes  representing a region big enough
   to hold X bytes. */
#define IK_PAGE_INDEX_RANGE(SIZE)	IK_PAGE_INDEX(SIZE)

/* Given a  Vicare page index: return  an untagged pointer to  the first
   word of the page. */
#define IK_PAGE_POINTER_FROM_INDEX(IDX)	\
  ((ikptr)(((ik_ulong)(IDX)) << IK_PAGESHIFT))

/* Given  a memory  SIZE in  bytes as  "ik_ulong": compute  the smallest
   number of bytes "mmap()" will allocate to hold it. */
#define IK_MMAP_ALLOCATION_SIZE(SIZE) \
  IK_SIZE_TO_GRANULARITY_SIZE((SIZE), IK_MMAP_ALLOCATION_GRANULARITY)

/* Given  a memory  SIZE in  bytes as  "ik_ulong": compute  the smallest
   number of pages of size IK_PAGESIZE needed to hold it. */
#define IK_MINIMUM_PAGES_NUMBER_FOR_SIZE(SIZE) \
  (IK_SIZE_TO_GRANULARITY_SIZE((SIZE),IK_PAGESIZE) / IK_PAGESIZE)

/* Given a  number of Vicare pages  as "ik_ulong": return the  number of
   bytes "mmap()" allocates to hold them. */
#define IK_MMAP_ALLOCATION_SIZE_FOR_PAGES(NPAGES) \
  IK_MMAP_ALLOCATION_SIZE(((ik_ulong)(NPAGES)) * IK_PAGESIZE)

/* Given  a pointer  or  tagged  pointer X  return  an untagged  pointer
 * referencing the first byte in the  page right after the one X belongs
 * to.
 *
 *      page     page     page
 *   |--------|--------|--------|
 *                  ^   ^
 *                  X   |
 *                     returned_value
 */
#define IK_ALIGN_TO_NEXT_PAGE(X) \
  ((((ik_ulong)(X) + IK_PAGESIZE - 1) >> IK_PAGESHIFT) << IK_PAGESHIFT)

/* Given  a pointer  or  tagged  pointer X  return  an untagged  pointer
 * referencing the first byte in the page X belongs to.
 *
 *      page     page     page
 *   |--------|--------|--------|
 *             ^    ^
 *             |    X
 *    returned_value
 */
#define IK_ALIGN_TO_PREV_PAGE(X) \
  ((((ik_ulong)(X)) >> IK_PAGESHIFT) << IK_PAGESHIFT)

/* *** DISCUSSION ABOUT "IK_SEGMENT_SIZE" and "IK_SEGMENT_SHIFT" ***
 *
 * Some  memory for  use  by  the Scheme  program  is allocated  through
 * "mmap()" in  blocks called "segments".   A segment's size is  a fixed
 * constant which  must be defined  as an  exact multiple of  the memory
 * allocation granularity  used by "mmap()"; we  define the preprocessor
 * macro IK_SEGMENT_SIZE to be such constant.
 *
 *   On Unix  platforms we  expect mmap's  allocation granularity  to be
 * 4096; on Windows platforms, under Cygwin, we expect mmap's allocation
 * granularity to be 2^16 = 65536 = 16 * IK_PAGESIZE.  So the allocation
 * granularity is  not always  equal to  the system  page size,  and not
 * always equal to Vicare's page size.
 *
 *   Remembering  that   we  have  defined  the   preprocessor  constant
 * IK_CHUNK_SIZE to be 4096, and assuming:
 *
 *   1 mebibyte = 1 MiB = 2^20 bytes = 1024 * 1024 bytes = 1048576 bytes
 *
 * we want the segment size to be 4 MiB:
 *
 *   4 MiB = 4 * 1024 * 1024 = 64 * 2^16 = 64 * 65536
 *         = 4096 * 1024 = 4096 * (4096 / 4) = IK_CHUNK_SIZE * 1024
 *         = 2^22 = 4194304 bytes
 *
 *   Vicare   distinguishes  among   "allocated  segments"   and  "logic
 * segments":
 *
 * - We assume  that "mmap()"  returns pointers  such that:  the pointer
 *   references the first byte of  a platform's system page; the numeric
 *   address of the  pointer is an exact multiple of  4096 (the 12 least
 *   significant bits  are zero).
 *
 * - We  request  to "mmap()"  to  allocate  memory  in sizes  that  are
 *   multiples  of  the  segment  size;   this  memory  is  composed  of
 *   "allocated segments".
 *
 * - We define  a "logic segment"  as a region  of memory whose  size is
 *   equal to  the segment size and  whose starting address is  an exact
 *   multiple of  the segment  size.  The  segment size is  4 MiB  so: a
 *   memory address  referencing the first  byte of a logic  segment has
 *   the 22 least significant bits set  to zero; for example: the memory
 *   starting at address 0 is part of the first logic segment.
 *
 * so, typically, allocated segments are displaced from logic segments:
 *
 *           alloc segment  alloc segment  alloc segment
 *    -----|--------------|--------------|--------------|----------
 *      logic segment  logic segment  logic segment  logic segment
 *    |--------------|--------------|--------------|--------------|
 *     page page page page page page page page page page page page
 *    |----|----|----|----|----|----|----|----|----|----|----|----|
 *
 * logic segments are absolute portions of  the memory seen by a running
 * system process.  It  is natural to assign a zero-based  index to each
 * logic segment:
 *
 *      logic segment  logic segment  logic segment  logic segment
 *    |--------------|--------------|--------------|--------------|
 *     ^              ^              ^              ^
 *    #x000000       #x400000       #x800000       #xC00000
 *    index 0        index 1        index 2        index 3
 *
 *   IK_SEGMENT_SHIFT is the number of  bits to right-shift a pointer or
 * tagged pointer  to obtain the  index of the logic  segment containing
 * the pointer itself; it is the number for which:
 *
 *    IK_SEGMENT_SIZE >> IK_SEGMENT_SHIFT = 1
 *    2^IK_SEGMENT_SHIFT = IK_SEGMENT_SIZE
 *
 * scenario:
 *
 *      logic segment  logic segment  logic segment
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
 *  we have:
 *
 *     X >> IK_PAGESHIFT     == IK_PAGE_INDEX(X)    == P+4
 *     X >> IK_SEGMENT_SHIFT == IK_SEGMENT_INDEX(X) == S+1
 */
#define IK_NUMBER_OF_PAGES_PER_SEGMENT	1024
#ifndef __CYGWIN__
#  define IK_SEGMENT_SIZE	(IK_CHUNK_SIZE * IK_NUMBER_OF_PAGES_PER_SEGMENT)
#  define IK_SEGMENT_SHIFT	22 /* (IK_PAGESHIFT + IK_PAGESHIFT - 2) */
#else
#  define IK_SEGMENT_SIZE	(IK_CHUNK_SIZE * IK_NUMBER_OF_PAGES_PER_SEGMENT)
#  define IK_SEGMENT_SHIFT	22 /* (IK_PAGESHIFT + IK_PAGESHIFT - 2) */
#endif
#define IK_SEGMENT_INDEX(x)	(((ik_ulong)(x)) >> IK_SEGMENT_SHIFT)

/* Slot size for both the PCB's dirty vector and the segments vector. */
#define IK_PAGE_VECTOR_SLOTS_PER_LOGIC_SEGMENT	\
  (sizeof(uint32_t) * IK_NUMBER_OF_PAGES_PER_SEGMENT)

/* On 32-bit platforms  we allocate 4 MiB as heap  size, while on 64-bit
   platforms  we  allocate  8  MiB.    On  64-bit  platforms  pairs  and
   vector-like objects have double the size.

   NOTE We  double the  heap size  on 64-bit  platforms because  of some
   criterion I am not aware of.  (Marco Maggi; Thu Dec 12, 2013) */
#define IK_HEAPSIZE		(IK_SEGMENT_SIZE * ((wordsize==4)?1:2))
/* When we  need to perform an  unsafe Scheme object allocation  and the
   Scheme heap is nearly full: the old heap is stored away in the PCB; a
   new memory block  of at least this size is  allocated and becomes the
   new heap.  This happens without garbage collections. */
#define IK_HEAP_EXTENSION_SIZE	IK_MMAP_ALLOCATION_SIZE_FOR_PAGES(32)

/* Only machine  words go on the  Scheme stack, no Scheme  objects data.
   So we are content with a single segment for the stack. */
#define IK_STACKSIZE		(IK_SEGMENT_SIZE)

/* Record in  the dirty vector the  side effect of mutating  the machine
   word at POINTER.   This will make the garbage collector  do the right
   thing when objects in an old  generation reference objects in a young
   generation. */
#define IK_PURE_WORD	0x00000000
#define IK_DIRTY_WORD	0xFFFFFFFF
#define IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(PCB,POINTER)	\
  (((uint32_t *)((PCB)->dirty_vector))[IK_PAGE_INDEX(POINTER)] = IK_DIRTY_WORD)


/** --------------------------------------------------------------------
 ** Preprocessor definitions: garbage collection stuff.
 ** ----------------------------------------------------------------- */

#define IK_GUARDIANS_GENERATION_NUMBER	0
#define IK_GC_GENERATION_COUNT		5  /* generations 0 (nursery), 1, 2, 3, 4 */

/* This  definition must  be  kept  in sync  with  the primitive  Scheme
   operation $FORWARD-PTR? */
#define IK_FORWARD_PTR		((ikptr)-1)

/* Masks for values  from the slots of PCB's segments  vector.  We logic
 * AND these masks to the segment bits to extract values.
 *
 * GEN_MASK -	Extract the page generation number.
 *
 */
#define OLD_GEN_MASK		0x00000007
#define NEW_GEN_MASK		0x00000008
#define GEN_MASK		0x0000000F
#define NEW_GEN_TAG		0x00000008
#define META_DIRTY_MASK		0x000000F0
#define TYPE_MASK		0x00000F00
#define SCANNABLE_MASK		0x0000F000
#define DEALLOC_MASK		0x000F0000
#define LARGE_OBJECT_MASK	0x00100000
#define META_DIRTY_SHIFT	4

#define HOLE_TYPE		0x00000000
#define MAINHEAP_TYPE		0x00000100
#define MAINSTACK_TYPE		0x00000200
#define POINTERS_TYPE		0x00000300
#define DATA_TYPE		0x00000400
#define CODE_TYPE		0x00000500
#define WEAK_PAIRS_TYPE		0x00000600
#define SYMBOLS_TYPE		0x00000700

#define scannable_tag		0x00001000
#define unscannable_tag		0x00000000

#define dealloc_tag_un		0x00010000
#define dealloc_tag_at		0x00020000
#define retain_tag		0x00000000

#define large_object_tag	0x00100000

/* Notice that "hole_mt" is zero. */
#define hole_mt		(HOLE_TYPE	 | unscannable_tag | retain_tag)
#define mainheap_mt	(MAINHEAP_TYPE	 | unscannable_tag | retain_tag)
#define mainstack_mt	(MAINSTACK_TYPE	 | unscannable_tag | retain_tag)
#define pointers_mt	(POINTERS_TYPE	 | scannable_tag   | dealloc_tag_un)
#define symbols_mt	(SYMBOLS_TYPE	 | scannable_tag   | dealloc_tag_un)
#define data_mt		(DATA_TYPE	 | unscannable_tag | dealloc_tag_un)
#define code_mt		(CODE_TYPE	 | scannable_tag   | dealloc_tag_un)
#define weak_pairs_mt	(WEAK_PAIRS_TYPE | scannable_tag   | dealloc_tag_un)


/** --------------------------------------------------------------------
 ** Preprocessor definitions: calling C from Scheme.
 ** ----------------------------------------------------------------- */

/*
 * When compiling Scheme code to  executable machine code: to generate a
 * "call" instruction to  a Scheme function, we need to  follow both the
 * protocol for  handling multiple  return values,  and the  protocol to
 * expose  informations  about  the  caller's stack  frame  for  garbage
 * collection purposes.
 *
 * This means generating the following chunk of pseudo-assembly:
 *
 *     jmp L0
 *     livemask-bytes		;array of bytes             |
 *     framesize		;data word, a "long"        | call
 *     rp_offset		;data word, a fixnum        | table
 *     multi-value-rp		;data word, assembly label  |
 *     pad-bytes                                            |
 *   L0:
 *     call scheme-function-address
 *   single-value-rp:		;single value return point
 *     ... instructions...
 *   multi-value-rp:		;multi value return point
 *     ... instructions...
 *
 * and  remember that  "call" pushes  on the  stack the  return address,
 * which is the label SINGLE-VALUE-RP.
 *
 * If the  callee function returns a  single value: it puts  such in the
 * CPU's  ARGC-REGISTER  and  performs  a  "ret";  this  will  make  the
 * execution flow jump back to the entry point SINGLE-VALUE-RP.
 *
 * If the callee  function wants to return zero or  2 or more arguments:
 * it retrieves the address SINGLE-VALUE-RP  from the Scheme stack, adds
 * to  it   the  constant   DISP_MULTIVALUE_RP  obtaining   the  address
 * MULTI-VALUE-RP, then  it performs a "jmp"  directly to MULTI-VALUE-RP
 * (without popping the return address from the stack).
 *
 * The constant data values (computed  at compile time) right before the
 * "call" assembly  instruction are the  "call table": a  data structure
 * representing  informations  about  this  function  call.   Given  the
 * address SINGLE-VALUE-RP  we can access  the fields of the  call table
 * using the following offsets.
 */
#define IK_CALL_INSTRUCTION_SIZE	((wordsize == 4) ? 5 : 10)
#define disp_call_table_size		(- (IK_CALL_INSTRUCTION_SIZE + 3 * wordsize))
#define disp_call_table_offset		(- (IK_CALL_INSTRUCTION_SIZE + 2 * wordsize))
#define disp_multivalue_rp		(- (IK_CALL_INSTRUCTION_SIZE + 1 * wordsize))

#define IK_CALLTABLE_FRAMESIZE(RETURN_ADDRESS)	\
		((long)IK_REF((RETURN_ADDRESS),disp_call_table_size))
#define IK_CALLTABLE_OFFSET(RETURN_ADDRESS)	\
		IK_REF((RETURN_ADDRESS),disp_call_table_offset)


/** --------------------------------------------------------------------
 ** Preprocessor definitions: miscellaneous.
 ** ----------------------------------------------------------------- */

/* Assign RIGHT to LEFT, but evaluate RIGHT first. */
#define IK_ASS(LEFT,RIGHT)	\
  { ikptr s_tmp = (RIGHT); (LEFT) = s_tmp; }

#define IK_FASL_HEADER		((sizeof(ikptr) == 4)? "#@IK01" : "#@IK02")
#define IK_FASL_HEADER_LEN	(strlen(IK_FASL_HEADER))


/** --------------------------------------------------------------------
 ** Type definitions.
 ** ----------------------------------------------------------------- */

/* These aliases have  the only purpose of making  the code shorter here
   and there.  Unfortunately  we have to use the  cast operator often in
   the code... */
typedef signed int		ik_int;
typedef signed long		ik_long;
typedef signed long long	ik_llong;
typedef unsigned int		ik_uint;
typedef unsigned long		ik_ulong;
typedef unsigned long long	ik_ullong;

/* FIXME Should this be a "uintptr_t"? (Marco Maggi; Nov  6, 2011). */
typedef ik_ulong		ikptr;

/* Node  in a  simply linked  list.  Used  to store  pointers to  memory
   blocks of size IK_PAGESIZE.  */
typedef struct ikpage {
  ikptr		 base;
  struct ikpage* next;
} ikpage;

/* Node in  a simply linked  list.  Used to  store pointers and  size of
   memory blocks. */
typedef struct ikmemblock {
  ikptr		base;
  int		size;
  struct ikmemblock* next;
} ikmemblock;

/* Node in  a linked list  referencing all the generated  FFI callbacks.
   It is used  to allow the garbage collector not  to collect data still
   in  use by  the callbacks.	See "ikarus-ffi.c"  for details	 on this
   structure. */
typedef struct ik_callback_locative {
  void *	callable_pointer;	/* pointer to callable C function */
  void *	closure;		/* data generated by Libffi */
  ikptr		data;			/* Scheme value holding required data */
  struct ik_callback_locative * next;	/* pointer to next link */
} ik_callback_locative;

/* Node in a linked list used to store tagged pointers to guardians.  We
   want  a  pointer  to  "ik_ptr_page"   to  reference  a  memory  block
   IK_PAGESIZE wide; the first words of  such page are used by the first
   members of the  data structure, while everything else is  used by the
   array "ptr". */
#define IK_PTR_PAGE_NUMBER_OF_GUARDIANS_SLOTS \
  ((IK_PAGESIZE - sizeof(long) - sizeof(struct ik_ptr_page*))/sizeof(ikptr))
typedef struct ik_ptr_page {
  long		count;
  struct ik_ptr_page* next;
  ikptr		ptr[IK_PTR_PAGE_NUMBER_OF_GUARDIANS_SLOTS];
} ik_ptr_page;

/* For  more  documentation  on  the PCB  structure:  see  the  function
   "ik_make_pcb()". */
typedef struct ikpcb {
  /* The  first locations  may	be  accessed by	 some  compiled code  to
     perform overflow/underflow ops. */
  ikptr	  allocation_pointer;	/* offset =  0 * wordsize, 32-bit offset =  0 */
  ikptr	  allocation_redline;	/* offset =  1 * wordsize, 32-bit offset =  4 */
  ikptr	  frame_pointer;	/* offset =  2 * wordsize, 32-bit offset =  8 */
  ikptr	  frame_base;		/* offset =  3 * wordsize, 32-bit offset = 12 */
  ikptr	  frame_redline;	/* offset =  4 * wordsize, 32-bit offset = 16 */
  ikptr	  next_k;		/* offset =  5 * wordsize, 32-bit offset = 20 */
  ikptr	  system_stack;		/* offset =  6 * wordsize, 32-bit offset = 24 */
  ikptr	  dirty_vector;		/* offset =  7 * wordsize, 32-bit offset = 28 */
  ikptr	  arg_list;		/* offset =  8 * wordsize, 32-bit offset = 32 */
  ikptr	  engine_counter;	/* offset =  9 * wordsize, 32-bit offset = 36 */
  ikptr	  interrupted;		/* offset = 10 * wordsize, 32-bit offset = 40 */
  ikptr	  base_rtd;		/* offset = 11 * wordsize, 32-bit offset = 44 */
  ikptr	  collect_key;		/* offset = 12 * wordsize, 32-bit offset = 48 */

  /* -------------------------------------------------------------------
   * The following  fields are  not used  by any  scheme code  they only
   * support the runtime system (GC, etc.)
   */

  /* Additional roots for the garbage collector.  They are used to avoid
     collecting objects  still in use while  they are in use  by C code.
     DO NOT MOVE THEM AROUND!!!  These fields must match the ones in the
     "ikpcb" struct defined in "vicare.h" */
  ikptr*		root0;
  ikptr*		root1;
  ikptr*		root2;
  ikptr*		root3;
  ikptr*		root4;
  ikptr*		root5;
  ikptr*		root6;
  ikptr*		root7;
  ikptr*		root8;
  ikptr*		root9;


  /* Untagged  pointers updated  (if needed)  after every  memory mapped
     allocation to  be lower  and greater  than all  the memory  used by
     Scheme programs.   They are  used for garbage  collection purposes.
     Every Vicare page between this range  of pointers is described by a
     slot in the segments vector and the dirty vector. */
  ikptr			memory_base;
  ikptr			memory_end;

  /* The segments  vector contains a slot  for every Vicare page  in the
   * region  of  memory  delimited   by  the  fields  "memory_base"  and
   * "memory_end"; it is  used to register the destination  use of every
   * page (heap, stack, unused, etc.), along with the garbage collection
   * generation the page belongs to.
   *
   *   "segment_vector_base" references the first allocated slot; access
   * to  the  vector  with   zero-based  indexes  is  performed  through
   * "segment_vector".
   *
   *   Notice that the segments vector is *not* itself registered in the
   * segments  vector and  dirty vector:  if the  segments vector  falls
   * inside the  region delimited by "memory_base"  and "memory_end", it
   * is marked as "hole" and "pure".
   */
  uint32_t *		segment_vector_base;
  uint32_t *		segment_vector;

  /* The  dirty vector  contains a  slot for  every Vicare  page in  the
   * region  of  memory  delimited   by  the  fields  "memory_base"  and
   * "memory_end"; it is  used to keep track of pages  that were mutated
   * at runtime; it allows us to do the right thing when a Scheme object
   * in an old  generation is mutated to reference a  Scheme object in a
   * new generation.
   *
   *   "dirty_vector_base" references  the first allocated  slot; access
   * to  the vector  with zero-based  indexes is  performed through  the
   * field "dirty_vector" (which is also accessible from Scheme code).
   *
   *   Notice that  the dirty vector  is *not* itself registered  in the
   * segments vector and dirty vector:  if the dirty vector falls inside
   * the  region  delimited by  "memory_base"  and  "memory_end", it  is
   * marked as "hole" and "pure".
   */
  uint32_t *		dirty_vector_base;

  /* Scheme objects  created by  a Scheme program  are allocated  on the
   * heap.  We can think of the Scheme  heap as the union of the nursery
   * and a set of generational pages.
   *
   *   The nursery is a set of memory blocks in which new Scheme objects
   * are allocated; it  is the generation 0.  The nursery  starts with a
   * single  "hot"  memory  block  in   which  new  Scheme  objects  are
   * allocated; whenever the hot block is full:
   *
   * - If  a  safe allocation  is  requested:  a garbage  collection  is
   *   performed and  all the objects are  moved from the heap  into the
   *   generational pages.
   *
   * - If an  unsafe allocation is  requested: the current hot  block is
   *   stored in  a linked list  of heap blocks and  a new hot  block is
   *   allocated.
   *
   *   The generational pages  are a set of Vicare  pages, referenced by
   * the segments vector, in which  objects are moved after they survive
   * a  garbage collection;  every generational  page is  tagged in  the
   * segments vector with the index of the generation it belongs to.
   *
   * heap_base -
   * heap_size -
   *     Pointer and  size in  bytes of the  current nursery  hot memory
   *     block; new Scheme objects are allocated here.  Initialised to a
   *     memory  mapped block  of size  IK_HEAPSIZE.
   *       When the previous block is full: to satisfy the request of an
   *     unsafe allocation  it is set to  a memory mapped block  of size
   *     IK_HEAP_EXTENSION_SIZE.
   *       After  a garbage  collection is  performed:  it is  set to  a
   *     memory mapped block of size IK_HEAPSIZE.
   *
   * allocation_pointer -
   *     Pointer to  the first word  of available  data in the  heap hot
   *     memory block; the next Scheme object to be allocated will start
   *     there.
   *
   * allocation_redline -
   *     Pointer to a word towards the end of the heap hot memory block;
   *     when the  allocation of a  Scheme object crosses  this pointer,
   *     the hot block is considered full.
   *
   * heap_pages -
   *     Pointer to  the first node  in a  linked list of  memory blocks
   *     that  once were  nursery hot  memory, and  are now  fully used;
   *     initialised to NULL when building the PCB.
   */
  ikptr			heap_base;
  ik_ulong		heap_size;
  ikmemblock *		heap_pages;

  /* Pointer to and number of bytes of the current Scheme stack memory.
   */
  ikptr			stack_base;
  ik_ulong		stack_size;

  /* Vicare pages  cache.  An array  of "ikpage" structs allocated  in a
   * single memory  block; the array  is never reallocated: its  size is
   * fixed; each struct is a node in  a simply linked list.  At run time
   * the slots  are linked in two  lists managed as stacks:  the list of
   * used nodes, each referencing a cached page; the list of free nodes,
   * currently referencing nothing.
   *
   *   At  initialisation  time:  all  the  structs  in  the  array  are
   * initialised  to reference  each other,  from the  last slot  to the
   * first slot: the last array slot is  the first node in the list, the
   * first array slot is the last node in the list.  This linked list is
   * the list of free nodes; the list of used nodes is empty.
   *
   * cached_pages_base -
   * cached_pages_size -
   *     Pointer and size-in-bytes of the array.  The pointer references
   *     the first  slot in  the array.   The size in  bytes must  be an
   *     exact multiple of a Vicare page size.
   *
   * cached_pages -
   *     Pointer to the first "ikpage" struct in the linked list of used
   *     nodes; set to NULL at PCB initialisation time; set to NULL when
   *     the cache is empty.  This pointer is the starting point when we
   *     need  to visit  all the  cached pages  (for example  to release
   *     them), or we need to pop a  cached page to be recycled for some
   *     use.
   *
   * uncached_pages -
   *     Pointer  to the  first "ikpage"  struct in  the linked  list of
   *     unused nodes;  set to reference the  last slot in the  array at
   *     PCB initialisation time; set to NULL when the cache is full.
   *
   *   When a  page needs to  be put in the  cache: the first  struct is
   * popped from  "uncached_pages", a pointer  to the page is  stored in
   * the struct, the struct pushed on "cached_pages".
   *
   *   When a cached  page needs to be used: the  first struct is popped
   * from "cached_pages",  the pointer  to the  page extracted  from the
   * struct, the struct pushed on "uncached_pages".
   *
   *   Notice that  the page cache  is *not* registered in  the segments
   * vector:  if  the  array  falls   inside  the  region  delimited  by
   * "memory_base" and "memory_end", it is marked as "hole".
   */
#define IK_PAGE_CACHE_NUM_OF_SLOTS	(IK_PAGESIZE * 1)
#define IK_PAGE_CACHE_SIZE_IN_BYTES	(IK_PAGE_CACHE_NUM_OF_SLOTS * sizeof(ikpage))
  ikptr			cached_pages_base;
  int			cached_pages_size;
  ikpage *		cached_pages;
  ikpage *		uncached_pages;

  /* The value of "argv[0]" as handed to the "main()" function. */
  char *		argv0;

  /* Linked  list of  FFI callback  support data.   Used by  the garbage
     collector	not  to collect	 data  still  needed  by some  callbacks
     registered in data structures handled by foreign libraries. */
  ik_callback_locative * callbacks;

  /* Value of  "errno" right after the	last call to  a foreign function
     callout. */
  int			last_errno;

  /* Weak pairs storage.  Weak pairs  are different from normal pairs: a
   * weak pair has a "weak" reference to  object in its car and a strong
   * reference to object  in its cdr; when an object  is referenced only
   * by the car of one or more weak pairs it can be garbage collected.
   *
   * The memory storage for weak pairs  is in Vicare pages referenced by
   * the  segments  vector, and  tagged  there  as "weak  pairs  pages".
   * Whenever such a page is full:  we just allocate a new one, register
   * it in the segments vector, and store references to it in the PCB.
   *
   * weak_pairs_ap -
   *     Pointer to the first free word  in the current weak pairs page.
   *     The next  created weak pair  object will  be stored in  the two
   *     words referenced by this pointer.
   *
   * weak_pairs_ep -
   *
   *     Pointer to  the first word right  after the end of  the current
   *     weak  pairs  page.   Whenever  "weak_pairs_ap"  surpasses  this
   *     pointer: the current page is full.
   *
   *   This is the allocation scenario, before:
   *
   *          used words          free words
   *      |...............|...................|
   *      |---|---|---|---|---|---|---|---|---|---| weak pairs page
   *                       ^                   ^
   *                  weak_pair_ap           weak_pair_ep
   *
   *   after:
   *
   *          used words           free words
   *      |.......................|...........|
   *      |---|---|---|---|---|---|---|---|---|---| weak pairs page
   *                               ^           ^
   *                          weak_pair_ap   weak_pair_ep
   */
  ikptr			weak_pairs_ap;
  ikptr			weak_pairs_ep;

  /* The hash table holding interned symbols. */
  ikptr			symbol_table;
  /* The hash table holding interned generated symbols. */
  ikptr			gensym_table;

  /* Array of linked lists; one for each GC generation.  The linked list
     holds  references  to  Scheme  values  that  must  not  be  garbage
     collected  even   when  they   are  not  referenced,   for  example
     guardians. */
  ik_ptr_page*		protected_list[IK_GC_GENERATION_COUNT];

  /* Number of garbage collections performed so far.  It is used: at the
     beginning of  a GC  run, to determine  which objects  generation to
     inspect; when reporting GC statistics to the user, to show how many
     GCs where performed between two timestamps. */
  int			collection_id;

  /* Memory  allocation accounting.   We  keep count  of  all the  bytes
   * allocated for the heap, so that:
   *
   *   total_allocated_bytes = \
   *     IK_MOST_BYTES_IN_MINOR * pcb->allocation_count_major
   *     + pcb->allocation_count_minor
   *
   * both  minor and  major  counters  must fit  into  a fixnum.   These
   * counters  are   used  by  Scheme  procedures   like  "time-it"  and
   * "time-and-gather".
   */
#define IK_MOST_BYTES_IN_MINOR	0x10000000
  int			allocation_count_minor;
  int			allocation_count_major;

  /* Used for garbage collection statistics. */
  struct timeval	collect_utime;
  struct timeval	collect_stime;
  struct timeval	collect_rtime;

  /* Collection of objects not to be collected. */
  void *		not_to_be_collected;

} ikpcb;

/* The garbage collection avoidance list  is a linked list of structures
   managed as a stack.  Allocated  structures are never released, so the
   stack continues to grow and never shrinks.

   Every structure contains  an array of machine words that  is meant to
   hold references to  "ikptr" values not to be garbage  collected; if a
   slot in the array is not IK_VOID: it contains a "ikptr" value.

   Adding values requires a linear search for a NULL slot and this sucks
   plenty; but this  way: the sweep of the garbage  collector is as fast
   as  possible, removing  values requires  only a  pointer indirection,
   memory consumption is as small as possible.

   The  arrays implicitly  associate  the memory  pointer  in which  the
   "ikptr" is  stored to  the "ikptr"  value itself;  this is  useful to
   store references to  to "ikptr" values in data  structures managed by
   foreign C language libraries. */

#define IK_GC_AVOIDANCE_ARRAY_LEN \
  ((IK_PAGESIZE - sizeof(ik_gc_avoidance_collection_t *))/sizeof(ikptr))

//  ((IK_PAGESIZE/sizeof(void *)) - 1)

typedef struct ik_gc_avoidance_collection_t	ik_gc_avoidance_collection_t;
struct ik_gc_avoidance_collection_t {
  /* NULL or  a pointer to  the next struct of  this type in  the linked
     list. */
  ik_gc_avoidance_collection_t *	next;
  /* Pointer to the first word in the free list. */
  ikptr		slots[IK_GC_AVOIDANCE_ARRAY_LEN];
};

/* The "ikcont"  data structure  is used  to access  Scheme continuation
   objects: given an "ikptr" reference to continuation, we subtract from
   it "continuation_primary_tag"  and the result is  an untagged pointer
   to "ikcont".  This struct  is a useful helper to be  used in place of
   the IK_REF getter. */
/* The  following   picture  shows  two   stack  frames  freezed   in  a
 * continuation object.  Freezed frame 0 is on the top of freezed stack.
 *
 *            high memory
 *    |                        |
 *    |------------------------|
 *    |  other return address  |
 *    |------------------------|        --             --
 *    |   local value frame 1  |        .              .
 *    |------------------------|        .              .
 *    |   local value frame 1  |        . framesize 1  .
 *    |------------------------|        .              .
 *    | return address frame 1 |        .              . continuation
 *    |------------------------|        --             . size
 *    |   local value frame 0  |        .              .
 *    |------------------------|        .              .
 *    |   local value frame 0  |        . framesize 0  .
 *    |------------------------|        .              .
 *    | return address frame 0 | <- top .              .
 *    |------------------------|        --             --
 *    |                        |
 *            low memory
 */
typedef struct ikcont {
  /* The field TAG is set to the constant value "continuation_tag". */
  ikptr		tag;
  /* The field TOP is a raw memory pointer referencing a machine word on
     the top  freezed frame; such  machine word contains the  address of
     the  code execution  return point  of this  continuation, in  other
     words: the address of the next assembly instruction to execute when
     returning to this continuation. */
  ikptr		top;
  /* The field  SIZE is  the number  of bytes in  all the  freezed stack
     frames  this continuation  references.  It  is the  sum of  all the
     freezed frame sizes. */
  long		size;
  /* Every "ikcont" struct is a node  in a linked list of continuations.
     The  field NEXT  is  0  or a  reference  (tagged  pointer) to  next
     continuation object. */
  ikptr		next;
} ikcont;


/** --------------------------------------------------------------------
 ** Internal function prototypes.
 ** ----------------------------------------------------------------- */

ik_decl ikpcb *		ik_collect		(unsigned long, ikpcb*);
ik_private_decl void	ik_verify_integrity	(ikpcb* pcb, char*);

ik_private_decl void*	ik_malloc		(int);
ik_private_decl void	ik_free			(void*, int);

ik_private_decl ikptr	ik_mmap			(unsigned long);
ik_private_decl ikptr	ik_mmap_typed		(unsigned long size, unsigned type, ikpcb*);
ik_private_decl ikptr	ik_mmap_ptr		(unsigned long size, int gen, ikpcb*);
ik_private_decl ikptr	ik_mmap_data		(unsigned long size, int gen, ikpcb*);
ik_private_decl ikptr	ik_mmap_code		(unsigned long size, int gen, ikpcb*);
ik_private_decl ikptr	ik_mmap_mainheap	(unsigned long size, ikpcb*);
ik_private_decl void	ik_munmap		(ikptr, unsigned long);
ik_private_decl ikpcb * ik_make_pcb		(void);
ik_private_decl void	ik_delete_pcb		(ikpcb*);
ik_private_decl void	ik_free_symbol_table	(ikpcb* pcb);

ik_private_decl void	ik_fasl_load		(ikpcb* pcb, char* filename);
ik_private_decl void	ik_relocate_code	(ikptr);

ik_private_decl ikptr	ik_exec_code		(ikpcb* pcb, ikptr code_ptr, ikptr argcount, ikptr cp);

ik_private_decl ikptr	ik_asm_enter		(ikpcb* pcb, ikptr code_object_entry_point,
						 ikptr s_arg_count, ikptr s_closure);
ik_private_decl ikptr	ik_asm_reenter		(ikpcb* pcb,
						 ikptr new_frame_base_pointer,
						 ikptr s_number_of_return_values);
ik_private_decl void	ik_underflow_handler	(void);
#define IK_UNDERFLOW_HANDLER		((ikptr)ik_underflow_handler)


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

ik_decl ikpcb *	ik_the_pcb		(void);
ik_decl void	ik_signal_dirt_in_page_of_pointer (ikpcb* pcb, ikptr s_pointer);

ik_decl int	ik_abort		(const char * error_message, ...);
ik_decl void	ik_error		(ikptr args);
ik_decl void	ik_debug_message	(const char * error_message, ...);
ik_decl void	ik_debug_message_start	(const char * error_message, ...);
ik_decl void	ik_debug_message_no_newline (const char * error_message, ...);

ik_decl ikptr	ik_unsafe_alloc		(ikpcb* pcb, ik_ulong size);
ik_decl ikptr	ik_safe_alloc		(ikpcb* pcb, ik_ulong size);

ik_decl void	ik_print		(ikptr x);
ik_decl void	ik_print_no_newline	(ikptr x);
ik_decl void	ik_fprint		(FILE*, ikptr x);

ik_private_decl void ik_print_stack_frame (FILE * fh, ikptr top);
ik_private_decl void ik_print_stack_frame_code_objects (FILE * fh, int max_num_of_frames,
							ikpcb * pcb);


/** --------------------------------------------------------------------
 ** Basic object related macros.
 ** ----------------------------------------------------------------- */

#define IK_ALIGN_SHIFT	(1 + wordshift)
#define IK_ALIGN_SIZE	(2 * wordsize)
#define immediate_tag	7

#define IK_TAGOF(X)	(((int)(X)) & 7)

#define IK_REF(X,N)	(((ikptr*)(((long)(X)) + ((long)(N))))[0])

/* The smallest multiple of the wordsize which is greater than N. */
#define IK_ALIGN(N) \
  ((((N) + IK_ALIGN_SIZE - 1) >>  IK_ALIGN_SHIFT) << IK_ALIGN_SHIFT)

#define IK_FALSE_OBJECT		((ikptr)0x2F)
#define IK_TRUE_OBJECT		((ikptr)0x3F)
#define IK_NULL_OBJECT		((ikptr)0x4F)
#define IK_EOF_OBJECT		((ikptr)0x5F)
#define IK_VOID_OBJECT		((ikptr)0x7F)

/* Special machine word value stored in locations that used to hold weak
   references to values which have been already garbage collected. */
#define IK_BWP_OBJECT		((ikptr)0x8F)

/* Special machine word value stored  in the "value" and "proc" field of
   Scheme symbol memory blocks to signal that these fields are unset. */
#define IK_UNBOUND_OBJECT	((ikptr)0x6F)

#define IK_FALSE		IK_FALSE_OBJECT
#define IK_TRUE			IK_TRUE_OBJECT
#define IK_NULL			IK_NULL_OBJECT
#define IK_EOF			IK_EOF_OBJECT
#define IK_VOID			IK_VOID_OBJECT
#define IK_BWP			IK_BWP_OBJECT
#define IK_UNBOUND		IK_UNBOUND_OBJECT


/** --------------------------------------------------------------------
 ** Fixnum objects.
 ** ----------------------------------------------------------------- */

#define fx_tag		0
#define fx_shift	wordshift
#define fx_mask		(wordsize - 1)

#define most_positive_fixnum	(((ik_ulong)-1) >> (fx_shift+1))
#define most_negative_fixnum	(most_positive_fixnum+1)
#define IK_GREATEST_FIXNUM	most_positive_fixnum
#define IK_LEAST_FIXNUM		(-most_negative_fixnum)

#define IK_FIX(X)	((ikptr)(((long)(X)) << fx_shift))
#define IK_UNFIX(X)	(((long)(X)) >> fx_shift)
#define IK_IS_FIXNUM(X)	((((ik_ulong)(X)) & fx_mask) == fx_tag)

ik_decl ikptr	ikrt_fxrandom		(ikptr x);


/** --------------------------------------------------------------------
 ** Pair and list objects.
 ** ----------------------------------------------------------------- */

#define pair_size	(2 * wordsize)
#define pair_mask	7 /* #b111 */
#define pair_tag	1
#define disp_car	0
#define disp_cdr	wordsize
#define off_car		(disp_car - pair_tag)
#define off_cdr		(disp_cdr - pair_tag)

#define IK_IS_PAIR(X)	(pair_tag == (((long)(X)) & pair_mask))

#define IK_CAR(PAIR)		    IK_REF((PAIR), off_car)
#define IK_CDR(PAIR)		    IK_REF((PAIR), off_cdr)
#define IK_CAAR(PAIR)		    IK_CAR(IK_CAR(PAIR))
#define IK_CDAR(PAIR)		    IK_CDR(IK_CAR(PAIR))
#define IK_CADR(PAIR)		    IK_CAR(IK_CDR(PAIR))
#define IK_CDDR(PAIR)		    IK_CDR(IK_CDR(PAIR))

#define IKA_PAIR_ALLOC(PCB)	(ik_safe_alloc((PCB),  pair_size) | pair_tag)
#define IKU_PAIR_ALLOC(PCB)	(ik_unsafe_alloc((PCB),pair_size) | pair_tag)

ik_decl ikptr ika_pair_alloc		(ikpcb * pcb);
ik_decl ikptr iku_pair_alloc		(ikpcb * pcb);
ik_decl long ik_list_length		(ikptr x);
ik_decl void ik_list_to_argv		(ikptr x, char **argv);
ik_decl void ik_list_to_argv_and_argc	(ikptr x, char **argv, long *argc);

ik_decl ikptr ika_list_from_argv	(ikpcb * pcb, char ** argv);
ik_decl ikptr ika_list_from_argv_and_argc(ikpcb * pcb, char ** argv, long argc);


/** --------------------------------------------------------------------
 ** Character objects.
 ** ----------------------------------------------------------------- */

typedef uint32_t	ikchar;

#define char_tag	0x0F
#define char_mask	0xFF
#define char_shift	8

#define IK_IS_CHAR(X)		(char_tag == (char_mask & (ikptr)(X)))

#define IK_CHAR_FROM_INTEGER(X) \
  ((ikptr)((((ik_ulong)(X)) << char_shift) | char_tag))

#define IK_CHAR32_FROM_INTEGER(X) \
  ((ikchar)((((ik_ulong)(X)) << char_shift) | char_tag))

#define IK_CHAR_TO_INTEGER(X) \
  ((ik_ulong)(((ikptr)(X)) >> char_shift))

#define IK_UNICODE_FROM_ASCII(ASCII)	((ik_ulong)(ASCII))


/** --------------------------------------------------------------------
 ** String objects.
 ** ----------------------------------------------------------------- */

#define IK_STRING_CHAR_SIZE	4
#define string_mask		7
#define string_tag		6
#define disp_string_length	0
#define disp_string_data	wordsize
#define off_string_length	(disp_string_length - string_tag)
#define off_string_data		(disp_string_data   - string_tag)

#define IK_IS_STRING(X)			(string_tag == (string_mask & (ikptr)(X)))
#define IK_STRING_LENGTH_FX(STR)	IK_REF((STR), off_string_length)
#define IK_STRING_LENGTH(STR)		IK_UNFIX(IK_REF((STR), off_string_length))
#define IK_CHAR32(STR,IDX)		(((ikchar*)(((long)(STR)) + off_string_data))[IDX])

#define IK_STRING_DATA_VOIDP(STR)	((void*)(((long)(STR)) + off_string_data))

ik_decl ikptr ika_string_alloc		(ikpcb * pcb, long number_of_chars);
ik_decl ikptr ika_string_from_cstring	(ikpcb * pcb, const char * cstr);

ik_decl ikptr iku_string_alloc		(ikpcb * pcb, long number_of_chars);
ik_decl ikptr iku_string_from_cstring	(ikpcb * pcb, const char * cstr);
ik_decl ikptr iku_string_to_symbol	(ikpcb * pcb, ikptr s_str);

ik_decl ikptr ikrt_string_to_symbol	(ikptr, ikpcb* pcb);
ik_decl ikptr ikrt_strings_to_gensym	(ikptr, ikptr,	ikpcb* pcb);


/** --------------------------------------------------------------------
 ** Symbol objects.
 ** ----------------------------------------------------------------- */

#define symbol_tag			((ikptr) 0x5F)
#define symbol_mask			((ikptr) 0xFF)
#define disp_symbol_record_tag		0
#define disp_symbol_record_string	(1 * wordsize)
#define disp_symbol_record_ustring	(2 * wordsize)
#define disp_symbol_record_value	(3 * wordsize)
#define disp_symbol_record_proc		(4 * wordsize)
#define disp_symbol_record_plist	(5 * wordsize)
#define symbol_record_size		(6 * wordsize)

#define off_symbol_record_tag		(disp_symbol_record_tag	    - record_tag)
#define off_symbol_record_string	(disp_symbol_record_string  - record_tag)
#define off_symbol_record_ustring	(disp_symbol_record_ustring - record_tag)
#define off_symbol_record_value		(disp_symbol_record_value   - record_tag)
#define off_symbol_record_proc		(disp_symbol_record_proc    - record_tag)
#define off_symbol_record_plist		(disp_symbol_record_plist   - record_tag)

ik_decl int   ik_is_symbol		(ikptr obj);
ik_decl ikptr iku_symbol_from_string	(ikpcb * pcb, ikptr s_str);


/** --------------------------------------------------------------------
 ** Bignum objects.
 ** ----------------------------------------------------------------- */

#define bignum_mask		0x7
#define bignum_tag		0x3
#define bignum_sign_mask	0x8
#define bignum_sign_shift	3
#define bignum_nlimbs_shift	4
#define disp_bignum_tag		0
#define disp_bignum_data	wordsize
#define off_bignum_tag		(disp_bignum_tag  - vector_tag)
#define off_bignum_data		(disp_bignum_data - vector_tag)

#define IK_BNFST_NEGATIVE(X)		(((ik_ulong)(X)) & bignum_sign_mask)
#define IK_BNFST_POSITIVE(X)		(!IK_BNFST_NEGATIVE(X))
#define IK_BNFST_LIMB_COUNT(X)		(((ik_ulong)(X)) >> bignum_nlimbs_shift)

#define IK_BIGNUM_ALLOC_SIZE(NUMBER_OF_LIMBS)			\
  IK_ALIGN(disp_bignum_data + (NUMBER_OF_LIMBS) * wordsize)

#define IKA_BIGNUM_ALLOC(PCB,LIMB_COUNT)	\
  (ik_safe_alloc((PCB), IK_BIGNUM_ALLOC_SIZE(LIMB_COUNT)) | vector_tag)

#define IK_COMPOSE_BIGNUM_FIRST_WORD(LIMB_COUNT,SIGN)		\
  ((ikptr)(((LIMB_COUNT) << bignum_nlimbs_shift) | (SIGN) | bignum_tag))

#define IK_POSITIVE_BIGNUM_FIRST_WORD(LIMB_COUNT)		\
  IK_COMPOSE_BIGNUM_FIRST_WORD((LIMB_COUNT),((0)<<bignum_sign_shift))

#define IK_NEGATIVE_BIGNUM_FIRST_WORD(LIMB_COUNT)		\
  IK_COMPOSE_BIGNUM_FIRST_WORD((LIMB_COUNT),((1)<<bignum_sign_shift))

#define IK_BIGNUM_DATA_LIMBP(X)					\
  ((mp_limb_t*)(long)(X + off_bignum_data))

#define IK_BIGNUM_FIRST_LIMB(X)					\
  ((mp_limb_t)IK_REF((X), off_bignum_data))

#define IK_BIGNUM_LAST_LIMB(X,LIMB_COUNT)			\
  ((mp_limb_t)IK_REF((X), off_bignum_data+((LIMB_COUNT)-1)*wordsize))

#define IK_BIGNUM_FIRST(X)	IK_REF((X), off_bignum_tag)
#define IK_LIMB(X,IDX)		IK_REF((X), off_bignum_data + (IDX)*wordsize)

ik_decl int	ik_is_bignum		(ikptr x);

ik_decl ikptr	ika_integer_from_int	(ikpcb* pcb, int N);
ik_decl ikptr	ika_integer_from_long	(ikpcb* pcb, long N);
ik_decl ikptr	ika_integer_from_llong	(ikpcb* pcb, ik_llong N);
ik_decl ikptr	ika_integer_from_uint	(ikpcb* pcb, ik_uint N);
ik_decl ikptr	ika_integer_from_ulong	(ikpcb* pcb, ik_ulong N);
ik_decl ikptr	ika_integer_from_ullong	(ikpcb* pcb, ik_ullong N);

ik_decl ikptr	ika_integer_from_sint8	(ikpcb* pcb, int8_t N);
ik_decl ikptr	ika_integer_from_sint16	(ikpcb* pcb, int16_t N);
ik_decl ikptr	ika_integer_from_sint32	(ikpcb* pcb, int32_t N);
ik_decl ikptr	ika_integer_from_sint64	(ikpcb* pcb, int64_t N);
ik_decl ikptr	ika_integer_from_uint8	(ikpcb* pcb, uint8_t N);
ik_decl ikptr	ika_integer_from_uint16	(ikpcb* pcb, uint16_t N);
ik_decl ikptr	ika_integer_from_uint32	(ikpcb* pcb, uint32_t N);
ik_decl ikptr	ika_integer_from_uint64	(ikpcb* pcb, uint64_t N);

ik_decl ikptr	ika_integer_from_off_t	(ikpcb * pcb, off_t N);
ik_decl ikptr	ika_integer_from_ssize_t(ikpcb * pcb, ssize_t N);
ik_decl ikptr	ika_integer_from_size_t	(ikpcb * pcb, size_t N);
ik_decl ikptr	ika_integer_from_ptrdiff_t(ikpcb * pcb, ptrdiff_t N);

ik_decl int8_t	 ik_integer_to_sint8	(ikptr x);
ik_decl int16_t	 ik_integer_to_sint16	(ikptr x);
ik_decl int32_t	 ik_integer_to_sint32	(ikptr x);
ik_decl int64_t	 ik_integer_to_sint64	(ikptr x);
ik_decl uint8_t	 ik_integer_to_uint8	(ikptr x);
ik_decl uint16_t ik_integer_to_uint16	(ikptr x);
ik_decl uint32_t ik_integer_to_uint32	(ikptr x);
ik_decl uint64_t ik_integer_to_uint64	(ikptr x);

ik_decl int	 ik_integer_to_int	(ikptr x);
ik_decl long	 ik_integer_to_long	(ikptr x);
ik_decl ik_llong ik_integer_to_llong	(ikptr x);
ik_decl ik_uint	 ik_integer_to_uint	(ikptr x);
ik_decl ik_ulong  ik_integer_to_ulong	(ikptr x);
ik_decl ik_ullong ik_integer_to_ullong	(ikptr x);

ik_decl off_t	ik_integer_to_off_t	(ikptr x);
ik_decl size_t	ik_integer_to_size_t	(ikptr x);
ik_decl ssize_t	ik_integer_to_ssize_t	(ikptr x);
ik_decl ptrdiff_t ik_integer_to_ptrdiff_t (ikptr x);

/* inspection */
ik_decl ikptr	ikrt_positive_bn	(ikptr x);
ik_decl ikptr	ikrt_even_bn		(ikptr x);

/* arithmetics */
ik_decl ikptr	ikrt_fxfxplus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnplus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnplus		(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_fxfxminus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnminus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnfxminus		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnminus		(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_bnnegate		(ikptr x, ikpcb* pcb);

ik_decl ikptr	ikrt_fxfxmult		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnmult		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnmult		(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_bnbncomp		(ikptr bn1, ikptr bn2);

ik_decl ikptr	ikrt_bnlognot		(ikptr x, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnlogand		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnlogand		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fxbnlogor		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnbnlogor		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bignum_shift_right	(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_fixnum_shift_left	(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bignum_shift_left	(ikptr x, ikptr y, ikpcb* pcb);

ik_decl ikptr	ikrt_bnbndivrem		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnfxdivrem		(ikptr x, ikptr y, ikpcb* pcb);
ik_decl ikptr	ikrt_bnfx_modulo	(ikptr x, ikptr y /*, ikpcb* pcb */);
ik_decl ikptr	ikrt_bignum_length	(ikptr x);

ik_decl ikptr	ikrt_exact_fixnum_sqrt	(ikptr fx /*, ikpcb* pcb*/);
ik_decl ikptr	ikrt_exact_bignum_sqrt	(ikptr bn, ikpcb* pcb);

ik_decl ikptr	ikrt_bignum_to_bytevector (ikptr x, ikpcb* pcb);
ik_decl ikptr	ikrt_bignum_to_flonum	(ikptr bn, ikptr more_bits, ikptr fl);

ik_decl ikptr	ikrt_bignum_hash	(ikptr bn /*, ikpcb* pcb */);


/** --------------------------------------------------------------------
 ** Ratnum objects.
 ** ----------------------------------------------------------------- */

#define ratnum_tag		((ikptr) 0x27)
#define disp_ratnum_tag		0
#define disp_ratnum_num		(1 * wordsize)
#define disp_ratnum_den		(2 * wordsize)
#define disp_ratnum_unused	(3 * wordsize)
#define ratnum_size		(4 * wordsize)

#define off_ratnum_tag		-vector_tag
#define off_ratnum_num		(disp_ratnum_num    - vector_tag)
#define off_ratnum_den		(disp_ratnum_den    - vector_tag)
#define off_ratnum_unused	(disp_ratnum_unused - vector_tag)

#define IK_NUMERATOR(X)		IK_REF((X), off_ratnum_num)
#define IK_DENOMINATOR(X)	IK_REF((X), off_ratnum_den)

ik_decl int	ik_is_ratnum	(ikptr X);
ik_decl ikptr	ika_ratnum_alloc_no_init	(ikpcb * pcb);
ik_decl ikptr	ika_ratnum_alloc_and_init	(ikpcb * pcb);


/** --------------------------------------------------------------------
 ** Compnum objects.
 ** ----------------------------------------------------------------- */

#define compnum_tag		((ikptr) 0x37)
#define disp_compnum_tag	0
#define disp_compnum_real	(1 * wordsize)
#define disp_compnum_imag	(2 * wordsize)
#define disp_compnum_unused	(3 * wordsize)
#define compnum_size		(4 * wordsize)

#define off_compnum_tag		-vector_tag
#define off_compnum_real	(disp_compnum_real   - vector_tag)
#define off_compnum_imag	(disp_compnum_imag   - vector_tag)
#define off_compnum_unused	(disp_compnum_unused - vector_tag)

#define IK_COMPNUM_REAL(X)	IK_REF((X), off_compnum_real)
#define IK_COMPNUM_IMAG(X)	IK_REF((X), off_compnum_imag)

ik_decl int	ik_is_compnum	(ikptr X);
ik_decl ikptr	ika_compnum_alloc_no_init	(ikpcb * pcb);
ik_decl ikptr	ika_compnum_alloc_and_init	(ikpcb * pcb);


/** --------------------------------------------------------------------
 ** Flonum objects.
 ** ----------------------------------------------------------------- */

#define flonum_tag		((ikptr)0x17)
#define flonum_size		16 /* four 32-bit words, two 64-bit words */
#define disp_flonum_tag		0 /* not f(wordsize) */
#define disp_flonum_data	8 /* not f(wordsize) */
#define off_flonum_tag		(disp_flonum_tag  - vector_tag)
#define off_flonum_data		(disp_flonum_data - vector_tag)

#define IKU_DEFINE_AND_ALLOC_FLONUM(VARNAME)				\
  ikptr VARNAME = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;	\
  IK_REF(VARNAME, off_flonum_tag) = (ikptr)flonum_tag

#define IK_FLONUM_DATA(X)	(*((double*)(((long)(X))+off_flonum_data)))

ik_decl int   ik_is_flonum		(ikptr obj);
ik_decl ikptr iku_flonum_alloc		(ikpcb * pcb, double fl);
ik_decl ikptr ika_flonum_from_double	(ikpcb* pcb, double N);
ik_decl ikptr ikrt_flonum_hash		(ikptr x /*, ikpcb* pcb */);


/** --------------------------------------------------------------------
 ** Cflonum objects.
 ** ----------------------------------------------------------------- */

#define cflonum_tag		((ikptr) 0x47)
#define disp_cflonum_tag	0
#define disp_cflonum_real	(1 * wordsize)
#define disp_cflonum_imag	(2 * wordsize)
#define disp_cflonum_unused	(3 * wordsize)
#define cflonum_size		(4 * wordsize)
#define off_cflonum_tag		(disp_cflonum_tag  - vector_tag)
#define off_cflonum_real	(disp_cflonum_real - vector_tag)
#define off_cflonum_imag	(disp_cflonum_imag - vector_tag)

#define IKU_DEFINE_AND_ALLOC_CFLONUM(VARNAME)				\
  ikptr VARNAME = ik_unsafe_alloc(pcb, cflonum_size) | vector_tag;	\
  IK_REF(VARNAME, off_cflonum_tag) = (ikptr)cflonum_tag

ik_decl int   ik_is_cflonum	(ikptr X);
ik_decl ikptr iku_cflonum_alloc_and_init (ikpcb * pcb, double re, double im);

#define IK_CFLONUM_REAL(X)	IK_REF((X), off_cflonum_real)
#define IK_CFLONUM_IMAG(X)	IK_REF((X), off_cflonum_imag)
#define IK_CFLONUM_REAL_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_REAL(X))
#define IK_CFLONUM_IMAG_DATA(X)	IK_FLONUM_DATA(IK_CFLONUM_IMAG(X))


/** --------------------------------------------------------------------
 ** Pointer objects.
 ** ----------------------------------------------------------------- */

#define pointer_tag		((ikptr) 0x107)
#define disp_pointer_tag	0
#define disp_pointer_data	(1 * wordsize)
#define pointer_size		(2 * wordsize)
#define off_pointer_tag		(disp_pointer_tag  - vector_tag)
#define off_pointer_data	(disp_pointer_data - vector_tag)

ik_decl ikptr ika_pointer_alloc	(ikpcb* pcb, ik_ulong memory);
ik_decl ikptr iku_pointer_alloc	(ikpcb* pcb, ik_ulong memory);
ik_decl ikptr ikrt_is_pointer	(ikptr X);
ik_decl int   ik_is_pointer	(ikptr X);

#define IK_IS_POINTER(X)	\
  (((vector_tag == IK_TAGOF(X)) && (pointer_tag == IK_REF(X, -vector_tag))))

#define IK_POINTER_DATA(X)		IK_REF((X), off_pointer_data)
#define IK_POINTER_DATA_VOIDP(X)	((void *)   IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_CHARP(X)	((char *)   IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_UINT8P(X)	((uint8_t *)IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_LONG(X)		((long)	    IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_LLONG(X)	((ik_llong) IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_ULONG(X)	((ik_ulong) IK_REF((X), off_pointer_data))
#define IK_POINTER_DATA_ULLONG(X)	((ik_ullong)IK_REF((X), off_pointer_data))

#define IK_POINTER_DATA_WORD(X)		((ik_uword_t)IK_REF((X), off_pointer_data))

#define IK_POINTER_SET(X,P)	(IK_REF((X), off_pointer_data) = (ikptr)((void*)(P)))
#define IK_POINTER_SET_NULL(X)	(IK_REF((X), off_pointer_data) = 0)
#define IK_POINTER_IS_NULL(X)	(0 == IK_POINTER_DATA(X))


/** --------------------------------------------------------------------
 ** Vector objects.
 ** ----------------------------------------------------------------- */

#define vector_mask		7
#define vector_tag		5
#define disp_vector_length	0
#define disp_vector_data	wordsize
#define off_vector_length	(disp_vector_length - vector_tag)
#define off_vector_data		(disp_vector_data   - vector_tag)

ik_decl ikptr ika_vector_alloc_no_init	(ikpcb * pcb, long number_of_items);
ik_decl ikptr ika_vector_alloc_and_init	(ikpcb * pcb, long number_of_items);

ik_decl ikptr iku_vector_alloc_no_init	(ikpcb * pcb, long number_of_items);
ik_decl ikptr iku_vector_alloc_and_init (ikpcb * pcb, long number_of_items);

ik_decl int   ik_is_vector		(ikptr s_vec);
ik_decl ikptr ikrt_vector_clean		(ikptr s_vec);
ik_decl ikptr ikrt_vector_copy		(ikptr s_dst, ikptr s_dst_start,
					 ikptr s_src, ikptr s_src_start,
					 ikptr s_count, ikpcb * pcb);

#define IK_VECTOR_LENGTH_FX(VEC)	IK_REF((VEC), off_vector_length)
#define IK_VECTOR_LENGTH(VEC)		IK_UNFIX(IK_VECTOR_LENGTH_FX(VEC))
#define IK_ITEM(VEC,IDX)		IK_REF((VEC), off_vector_data + (IDX) * wordsize)


/** --------------------------------------------------------------------
 ** Bytevector objects.
 ** ----------------------------------------------------------------- */

#define bytevector_mask		7
#define bytevector_tag		2
#define disp_bytevector_length	0
#define disp_bytevector_data	8 /* not f(wordsize) */
#define off_bytevector_length	(disp_bytevector_length - bytevector_tag)
#define off_bytevector_data	(disp_bytevector_data	- bytevector_tag)

#define IK_IS_BYTEVECTOR(X)	(bytevector_tag == (((long)(X)) & bytevector_mask))

ik_decl ikptr ika_bytevector_alloc		(ikpcb * pcb, long requested_number_of_bytes);
ik_decl ikptr ika_bytevector_from_cstring	(ikpcb * pcb, const char * cstr);
ik_decl ikptr ika_bytevector_from_cstring_len	(ikpcb * pcb, const char * cstr, size_t len);
ik_decl ikptr ika_bytevector_from_memory_block	(ikpcb * pcb, const void * memory,
						 size_t length);
ik_decl ikptr ika_bytevector_from_utf16z	(ikpcb * pcb, const void * data);
ik_decl ikptr ikrt_bytevector_copy (ikptr s_dst, ikptr s_dst_start,
				    ikptr s_src, ikptr s_src_start,
				    ikptr s_count);

#define IK_BYTEVECTOR_LENGTH_FX(BV)	IK_REF((BV), off_bytevector_length)
#define IK_BYTEVECTOR_LENGTH(BV)	IK_UNFIX(IK_BYTEVECTOR_LENGTH_FX(BV))

#define IK_BYTEVECTOR_DATA(BV)		((long)((BV) + off_bytevector_data))
#define IK_BYTEVECTOR_DATA_VOIDP(BV)	((void*)   IK_BYTEVECTOR_DATA(BV))
#define IK_BYTEVECTOR_DATA_CHARP(BV)	((char*)   IK_BYTEVECTOR_DATA(BV))
#define IK_BYTEVECTOR_DATA_UINT8P(BV)	((uint8_t*)IK_BYTEVECTOR_DATA(BV))


/** --------------------------------------------------------------------
 ** Struct objects.
 ** ----------------------------------------------------------------- */

#define record_mask		7
#define record_tag		vector_tag
#define disp_record_rtd		0
#define disp_record_data	wordsize
#define off_record_rtd		(disp_record_rtd  - record_tag)
#define off_record_data		(disp_record_data - record_tag)

#define rtd_tag			record_tag
#define disp_rtd_rtd		0
#define disp_rtd_name		(1 * wordsize)
#define disp_rtd_length		(2 * wordsize)
#define disp_rtd_fields		(3 * wordsize)
#define disp_rtd_printer	(4 * wordsize)
#define disp_rtd_symbol		(5 * wordsize)
#define disp_rtd_destructor	(6 * wordsize)
#define rtd_size		(7 * wordsize)
//#define rtd_size		(6 * wordsize)

#define off_rtd_rtd		(disp_rtd_rtd		- rtd_tag)
#define off_rtd_name		(disp_rtd_name		- rtd_tag)
#define off_rtd_length		(disp_rtd_length	- rtd_tag)
#define off_rtd_fields		(disp_rtd_fields	- rtd_tag)
#define off_rtd_printer		(disp_rtd_printer	- rtd_tag)
#define off_rtd_symbol		(disp_rtd_symbol	- rtd_tag)
#define off_rtd_destructor	(disp_rtd_destructor	- rtd_tag)

ik_decl ikptr ika_struct_alloc_and_init	(ikpcb * pcb, ikptr rtd);
ik_decl ikptr ika_struct_alloc_no_init	(ikpcb * pcb, ikptr rtd);
ik_decl int   ik_is_struct	(ikptr R);

#define IK_FIELD(STRUCT,FIELD)	IK_REF((STRUCT), (off_record_data+(FIELD)*wordsize))


/** --------------------------------------------------------------------
 ** Port objects.
 ** ----------------------------------------------------------------- */

#define port_tag		0x3F
#define port_mask		0x3F
#define disp_port_attrs		0)
#define disp_port_index		(1 * wordsize)
#define disp_port_size		(2 * wordsize)
#define disp_port_buffer	(3 * wordsize)
#define disp_port_transcoder	(4 * wordsize)
#define disp_port_id		(5 * wordsize)
#define disp_port_read		(6 * wordsize)
#define disp_port_write		(7 * wordsize)
#define disp_port_get_position	(8 * wordsize)
#define disp_port_set_position	(9 * wordsize)
#define disp_port_close		(10 * wordsize)
#define disp_port_cookie	(11 * wordsize)
#define disp_port_unused1	(12 * wordsize)
#define disp_port_unused2	(13 * wordsize)
#define port_size		(14 * wordsize)

#define off_port_attrs		(disp_port_attrs	- vector_tag)
#define off_port_index		(disp_port_index	- vector_tag)
#define off_port_size		(disp_port_size		- vector_tag)
#define off_port_buffer		(disp_port_buffer	- vector_tag)
#define off_port_transcoder	(disp_port_transcoder	- vector_tag)
#define off_port_id		(disp_port_id		- vector_tag)
#define off_port_read		(disp_port_read		- vector_tag)
#define off_port_write		(disp_port_write	- vector_tag)
#define off_port_get_position	(disp_port_get_position	- vector_tag)
#define off_port_set_position	(disp_port_set_position	- vector_tag)
#define off_port_close		(disp_port_close	- vector_tag)
#define off_port_cookie		(disp_port_cookie	- vector_tag)
#define off_port_unused1	(disp_port_unused1	- vector_tag)
#define off_port_unused2	(disp_port_unused2	- vector_tag)


/** --------------------------------------------------------------------
 ** Code objects.
 ** ----------------------------------------------------------------- */

/* To assert that a machine word X references a code object we do:

     ikptr	X;
     assert(code_primary_tag == (code_primary_mask & X));
     assert(code_tag         == IK_REF(X, off_code_tag));
*/
#define code_primary_mask	vector_mask
#define code_primary_tag	vector_tag
#define code_tag		((ikptr)0x2F)

#define disp_code_tag		0
#define disp_code_code_size	(1 * wordsize)
#define disp_code_reloc_vector	(2 * wordsize)
#define disp_code_freevars	(3 * wordsize)
#define disp_code_annotation	(4 * wordsize)
#define disp_code_unused	(5 * wordsize)
#define disp_code_data		(6 * wordsize)
#define off_code_tag		(disp_code_tag		- code_primary_tag)
#define off_code_freevars	(disp_code_freevars	- code_primary_tag)
#define off_code_annotation	(disp_code_annotation	- code_primary_tag)
#define off_code_data		(disp_code_data		- code_primary_tag)
#define off_code_reloc_vector	(disp_code_reloc_vector - code_primary_tag)

#define IK_IS_CODE(X)		\
     ((code_primary_tag == (code_primary_mask & X)) && \
      (code_tag         == IK_REF(X, off_code_tag)))

/* Given a reference  to code object: return a raw  pointer to the entry
   point in the code, as "ikptr". */
#define IK_CODE_ENTRY_POINT(CODE)	((CODE)+off_code_data)

ik_private_decl ikptr ik_stack_frame_top_to_code_object (ikptr top);

/* ------------------------------------------------------------------ */

/* Accessors for the words of relocation vector's records. */
#undef  IK_RELOC_RECORD_REF
#define IK_RELOC_RECORD_REF(VEC,IDX)	IK_REF((VEC),(IDX)*wordsize)
#undef  IK_RELOC_RECORD_1ST
#define IK_RELOC_RECORD_1ST(VEC)	IK_RELOC_RECORD_REF((VEC),0)
#undef  IK_RELOC_RECORD_2ND
#define IK_RELOC_RECORD_2ND(VEC)	IK_RELOC_RECORD_REF((VEC),1)
#undef  IK_RELOC_RECORD_3RD
#define IK_RELOC_RECORD_3RD(VEC)	IK_RELOC_RECORD_REF((VEC),2)

/* Least significant  bits tags  for the  first word  in records  of the
   relocation vector for code objects. */
#define IK_RELOC_RECORD_MASK_TAG		0b11
#define IK_RELOC_RECORD_VANILLA_OBJECT_TAG	0
#define IK_RELOC_RECORD_FOREIGN_ADDRESS_TAG	1
#define IK_RELOC_RECORD_DISPLACED_OBJECT_TAG	2
#define IK_RELOC_RECORD_JUMP_LABEL_TAG		3

/* Given a  machine word representing  the bits in  the first word  of a
   record in a relocation vector: evaluate to the record type tag. */
#define IK_RELOC_RECORD_1ST_BITS_TAG(WORD)	((WORD) & IK_RELOC_RECORD_MASK_TAG)

/* Given a  machine word representing  the bits in  the first word  of a
   record in a relocation vector: evaluate to the offset. */
#define IK_RELOC_RECORD_1ST_BITS_OFFSET(WORD)	((WORD) >> 2)


/** --------------------------------------------------------------------
 ** Closure objects.
 ** ----------------------------------------------------------------- */

#define closure_tag		3
#define closure_mask		7
#define disp_closure_code	0
#define disp_closure_data	wordsize
#define off_closure_code	(disp_closure_code - closure_tag)
#define off_closure_data	(disp_closure_data - closure_tag)

#define IK_IS_CLOSURE(X)	((((long)(X)) & closure_mask) == closure_tag)

#define IK_CLOSURE_ENTRY_POINT(X)	IK_REF((X),off_closure_code)
#define IK_CLOSURE_CODE_OBJECT(X)	(IK_CLOSURE_ENTRY_POINT(X)-off_code_data)
#define IK_CLOSURE_NUMBER_OF_FREE_VARS(X)	\
  IK_UNFIX(IK_REF(IK_CLOSURE_CODE_OBJECT(X), off_code_freevars))
#define IK_CLOSURE_FREE_VAR(X,IDX)	IK_REF((X),off_closure_data+wordsize*(IDX))


/** --------------------------------------------------------------------
 ** Continuation objects.
 ** ----------------------------------------------------------------- */

#define continuation_primary_mask	vector_mask
#define continuation_primary_tag	vector_tag

/* ------------------------------------------------------------------ */

#define continuation_tag		((ikptr)0x1F)
#define disp_continuation_tag		0
#define disp_continuation_top		(1 * wordsize)
#define disp_continuation_size		(2 * wordsize)
#define disp_continuation_next		(3 * wordsize)
#define continuation_size		(4 * wordsize)

#define off_continuation_tag		(disp_continuation_tag	- vector_tag)
#define off_continuation_top		(disp_continuation_top	- vector_tag)
#define off_continuation_size		(disp_continuation_size - vector_tag)
#define off_continuation_next		(disp_continuation_next - vector_tag)

#define IK_CONTINUATION_STRUCT(KONT)	((ikcont *)((long)((KONT) - vector_tag)))
#define IK_CONTINUATION_TAG(KONT)	IK_REF((KONT),off_continuation_tag)
#define IK_CONTINUATION_TOP(KONT)	IK_REF((KONT),off_continuation_top)
#define IK_CONTINUATION_SIZE(KONT)	IK_REF((KONT),off_continuation_size)
#define IK_CONTINUATION_NEXT(KONT)	IK_REF((KONT),off_continuation_next)

#define IK_IS_CONTINUATION(X)		\
   ((continuation_primary_tag == (continuation_primary_mask & (X))) &&	\
    (continuation_tag         == IK_REF((X), off_continuation_tag)))

/* ------------------------------------------------------------------ */

#define system_continuation_tag		((ikptr) 0x11F)
#define disp_system_continuation_tag	0
#define disp_system_continuation_top	(1 * wordsize)
#define disp_system_continuation_next	(2 * wordsize)
#define disp_system_continuation_unused (3 * wordsize)
#define system_continuation_size	(4 * wordsize)

#define off_system_continuation_tag	(disp_system_continuation_tag	 - vector_tag)
#define off_system_continuation_top	(disp_system_continuation_top	 - vector_tag)
#define off_system_continuation_next	(disp_system_continuation_next	 - vector_tag)
#define off_system_continuation_unused	(disp_system_continuation_unused - vector_tag)

#define IK_IS_SYSTEM_CONTINUATION(X)	\
   ((continuation_primary_tag == (continuation_primary_mask & (X))) &&	\
    (system_continuation_tag  == IK_REF((X), off_system_continuation_tag)))

/* ------------------------------------------------------------------ */

#define IK_IS_ANY_CONTINUATION(X)	\
   (IK_IS_CONTINUATION(X) || IK_IS_SYSTEM_CONTINUATION(X))


/** --------------------------------------------------------------------
 ** Tcbucket objects.
 ** ----------------------------------------------------------------- */

#define disp_tcbucket_tconc	(0 * wordsize)
#define disp_tcbucket_key	(1 * wordsize)
#define disp_tcbucket_val	(2 * wordsize)
#define disp_tcbucket_next	(3 * wordsize)
#define tcbucket_size		(4 * wordsize)

#define off_tcbucket_tconc	(disp_tcbucket_tconc - vector_tag)
#define off_tcbucket_key	(disp_tcbucket_key   - vector_tag)
#define off_tcbucket_val	(disp_tcbucket_val   - vector_tag)
#define off_tcbucket_next	(disp_tcbucket_next  - vector_tag)


/** --------------------------------------------------------------------
 ** Miscellanous functions.
 ** ----------------------------------------------------------------- */

ik_decl ikptr ikrt_general_copy (ikptr s_dst, ikptr s_dst_start,
				 ikptr s_src, ikptr s_src_start,
				 ikptr s_count, ikpcb * pcb);

ik_decl void ik_enter_c_function (ikpcb* pcb);
ik_decl void ik_leave_c_function (ikpcb* pcb);


/** --------------------------------------------------------------------
 ** Special exact integer object macros.
 ** ----------------------------------------------------------------- */

#define IK_IS_INTEGER(OBJ)	(IK_IS_FIXNUM(OBJ)||ik_is_bignum(OBJ))


/** --------------------------------------------------------------------
 ** Special boolean object macros.
 ** ----------------------------------------------------------------- */

#define IK_IS_BOOLEAN(OBJ)		((IK_FALSE == (OBJ)) || (IK_TRUE == (OBJ)))
#define IK_BOOLEAN_TO_INT(OBJ)		(!(IK_FALSE == (OBJ)))
#define IK_BOOLEAN_FROM_INT(INT)	((INT)? IK_TRUE : IK_FALSE)


/** --------------------------------------------------------------------
 ** Special memory-block object macros.
 ** ----------------------------------------------------------------- */

#define IK_MBLOCK_POINTER(OBJ)		IK_FIELD(OBJ, 0)
#define IK_MBLOCK_SIZE(OBJ)		IK_FIELD(OBJ, 1)
#define IK_MBLOCK_DATA_VOIDP(OBJ)	IK_POINTER_DATA_VOIDP(IK_MBLOCK_POINTER(OBJ))
#define IK_MBLOCK_DATA_CHARP(OBJ)	IK_POINTER_DATA_CHARP(IK_MBLOCK_POINTER(OBJ))
#define IK_MBLOCK_SIZE_T(OBJ)		ik_integer_to_size_t(IK_MBLOCK_SIZE(OBJ))


/** --------------------------------------------------------------------
 ** Special macros extracting "void *" pointers from objects.
 ** ----------------------------------------------------------------- */

/* pointer, false */
#define IK_POINTER_FROM_POINTER_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_POINTER_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_POINTER_DATA_VOIDP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, false */
#define IK_POINTER_FROM_BYTEVECTOR_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_BYTEVECTOR_DATA_VOIDP(OBJ))

/* ------------------------------------------------------------------ */

/* mblock, false */
#define IK_POINTER_FROM_MBLOCK_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_MBLOCK_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_MBLOCK_DATA_VOIDP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(OBJ) \
  ((IK_IS_BYTEVECTOR(OBJ))? IK_BYTEVECTOR_DATA_VOIDP(OBJ) : IK_POINTER_DATA_VOIDP(OBJ))

/* bytevector, pointer, false */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER(OBJ))

/* ------------------------------------------------------------------ */

/* pointer, mblock */
#define IK_POINTER_FROM_POINTER_OR_MBLOCK(OBJ) \
          IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ)
#define   IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_POINTER(OBJ)? IK_POINTER_DATA_VOIDP(OBJ) : IK_MBLOCK_DATA_VOIDP(OBJ))

/* pointer, mblock, false */
#define IK_POINTER_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)	\
          IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)	\
  ((IK_FALSE == (OBJ))? NULL : IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer, mblock */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_BYTEVECTOR(OBJ)? IK_BYTEVECTOR_DATA_VOIDP(OBJ) : IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ))

/* bytevector, pointer, mblock, false */
#define IK_POINTER_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ) \
          IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ)
#define   IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ))

/* ------------------------------------------------------------------ */

/* generalised C buffer */
#define IK_GENERALISED_C_BUFFER(OBJ)	\
  IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)

/* generalised C buffer or false */
#define IK_GENERALISED_C_BUFFER_OR_FALSE(OBJ)	\
  IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ)

/* ------------------------------------------------------------------ */

/* generalised sticky C buffer */
#define IK_GENERALISED_C_STICKY_BUFFER(OBJ)	\
  IK_VOIDP_FROM_POINTER_OR_MBLOCK(OBJ)

/* generalised sticky C buffer or false */
#define IK_GENERALISED_C_STICKY_BUFFER_OR_FALSE(OBJ)	\
  IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)


/** --------------------------------------------------------------------
 ** Special macros extracting "char *" pointers from objects.
 ** ----------------------------------------------------------------- */

/* pointer, false */
#define IK_CHARP_FROM_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_POINTER_DATA_CHARP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, false */
#define IK_CHARP_FROM_BYTEVECTOR_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_BYTEVECTOR_DATA_CHARP(OBJ))

/* ------------------------------------------------------------------ */

/* mblock, false */
#define IK_CHARP_FROM_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_MBLOCK_DATA_CHARP(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(OBJ) \
  ((IK_IS_BYTEVECTOR(OBJ))? IK_BYTEVECTOR_DATA_CHARP(OBJ) : IK_POINTER_DATA_CHARP(OBJ))

/* bytevector, pointer, false */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_CHARP_FROM_BYTEVECTOR_OR_POINTER(OBJ))

/* ------------------------------------------------------------------ */

/* pointer, mblock */
#define IK_CHARP_FROM_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_POINTER(OBJ)? IK_POINTER_DATA_CHARP(OBJ) : IK_MBLOCK_DATA_CHARP(OBJ))

/* pointer, mblock, false */
#define IK_CHARP_FROM_POINTER_OR_MBLOCK_OR_FALSE(OBJ)	\
  ((IK_FALSE == (OBJ))? NULL : IK_CHARP_FROM_POINTER_OR_MBLOCK(OBJ))

/* ------------------------------------------------------------------ */

/* bytevector, pointer, mblock */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)	\
  (IK_IS_BYTEVECTOR(OBJ)? IK_BYTEVECTOR_DATA_CHARP(OBJ) : IK_CHARP_FROM_POINTER_OR_MBLOCK(OBJ))

/* bytevector, pointer, mblock, false */
#define IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ) \
  ((IK_FALSE == (OBJ))? NULL : IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ))

/* ------------------------------------------------------------------ */

/* generalised C string */
#define IK_GENERALISED_C_STRING(OBJ)	\
  IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(OBJ)

/* generalised C string or false */
#define IK_GENERALISED_C_STRING_OR_FALSE(OBJ)	\
  IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(OBJ)


/** --------------------------------------------------------------------
 ** Other objects stuff.
 ** ----------------------------------------------------------------- */

ikptr	ik_normalize_bignum	(long limbs, int sign, ikptr r);

#define max_digits_per_limb	((wordsize==4)?10:20)

ik_decl size_t ik_generalised_c_buffer_len (ikptr s_buffer, ikptr s_buffer_len);


/** --------------------------------------------------------------------
 ** Other prototypes and external definitions.
 ** ----------------------------------------------------------------- */

extern char **		environ;

#ifdef __CYGWIN__
void	win_munmap(char* addr, size_t size);
char*	win_mmap(size_t size);
#endif

int	ikarus_main (int argc, char** argv, char* boot_file);

ikptr	ik_errno_to_code (void);


/** --------------------------------------------------------------------
 ** Interface to "getaddrinfo()".
 ** ----------------------------------------------------------------- */

#if (!HAVE_GETADDRINFO)
#  include <sys/types.h>
#  include <netdb.h>

struct addrinfo {
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t ai_addrlen;
  struct sockaddr *ai_addr;
  struct addrinfo *ai_next;
};

extern int
getaddrinfo(const char *hostname, const char* servname,
  const struct addrinfo* hints, struct addrinfo** res);

extern void
freeaddrinfo(struct addrinfo *ai);


#ifndef EAI_SYSTEM
# define EAI_SYSTEM 11 /* same code as in glibc */
#endif

#endif /* if (!HAVE_GETADDRINFO) */


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* INTERNALS_H */

/* end of file */
