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

/* Define it to true  to cause a log message printed  to stderr when the
   integrity check starts and finishes. */
#undef LOG_VERIFY
#define LOG_VERIFY	0

static ik_ulong	page_idx	  (uint8_t * x);
static uint8_t *verify_page       (uint8_t * mem,
				   uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector);

/* Scheme objects, non-code. */
static uint8_t *verify_scheme_objects_page (uint8_t * mem, uint32_t segment_bits IK_UNUSED, uint32_t dirty_bits IK_UNUSED,
					    uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector);
static void	verify_object	  (ikptr x IK_UNUSED,
				   uint8_t * mem_base IK_UNUSED, uint32_t * segment_vector IK_UNUSED, uint32_t * dirty_vector IK_UNUSED);

/* Code objects. */
static uint8_t *verify_code_page  (uint8_t * mem, uint32_t segment_bits, uint32_t dirty_bits,
				   uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector);
static uint8_t *verify_code_small_size_sequence (uint8_t * mem,
						 uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector);
static void	verify_code_object_first_word (ikptr p_code);
static ik_ulong	verify_code_object_size (ikptr p_code);
static void	verify_code_common_fields (uint8_t * mem,
					   uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector);
static void	log_invalid_code_object (ikptr X);


void
ik_verify_integrity (ikpcb* pcb, char * when_description)
/* Scan  all the  memory used  by  Vicare searching  for invalid  values
   introduced by the garbage collector.  If successful: just return.  If
   an error is found: abort the process.

   The raw pointers MEM_BASE and MEM_END select the range of memory used
   by  Vicare.  Every  Vicare page  between  this range  of pointers  is
   described by a slot in the segments vector and the dirty vector. */
{
  uint8_t *	mem_base    = (uint8_t *)(ik_ulong)pcb->memory_base;
  uint8_t *	mem_end     = (uint8_t *)(ik_ulong)pcb->memory_end;
  uint32_t *	segment_vec = pcb->segment_vector_base;
  uint32_t *	dirty_vec   = pcb->dirty_vector_base;
  if (LOG_VERIFY) {
    ik_debug_message("%s: verifying in %s...", __func__, when_description);
  }
  for (uint8_t * mem = mem_base; mem < mem_end;) {
    mem = verify_page(mem, mem_base, segment_vec, dirty_vec);
  }
  if (LOG_VERIFY) {
    ik_debug_message("%s: verify_ok in %s", __func__, when_description);
  }
}


/** --------------------------------------------------------------------
 ** Helpers.
 ** ----------------------------------------------------------------- */

static ik_ulong
page_idx (uint8_t * mem)
/* Given a raw (untagged) memory  address referencing a location used by
   Vicare: return the index of the memory page containing the location. */
{
  ik_ulong	xi = (ik_ulong) mem;
  return xi >> IK_PAGESHIFT;
}


static uint8_t *
verify_page (uint8_t * mem,
	     uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector)
/* Verify that  the memory  location referenced by  the raw  pointer MEM
   references a valid  Scheme object.  Return a  raw pointer referencing
   the next location to check. */
{
  ik_ulong	idx		= page_idx(mem) - page_idx(mem_base);
  uint32_t	segment_bits	= segment_vector[idx];
  uint32_t	dirty_bits	=   dirty_vector[idx];
  uint32_t	type		= segment_bits & TYPE_MASK;
  if (type == HOLE_TYPE) {
    /* There are no Scheme objects in this page. */
    return mem + IK_PAGESIZE;
  }
  assert(0 == (segment_bits & NEW_GEN_MASK));
  if (type == CODE_TYPE) {
    return verify_code_page(mem, segment_bits, dirty_bits, mem_base, segment_vector, dirty_vector);
  }
  else if (type == POINTERS_TYPE) {
    return verify_scheme_objects_page(mem, segment_bits, dirty_bits, mem_base, segment_vector, dirty_vector);
  }
  else if (type == WEAK_PAIRS_TYPE) {
    return verify_scheme_objects_page(mem, segment_bits, dirty_bits, mem_base, segment_vector, dirty_vector);
  }
  else if (type == SYMBOLS_TYPE) {
    return verify_scheme_objects_page(mem, segment_bits, dirty_bits, mem_base, segment_vector, dirty_vector);
  }
  else if (type == DATA_TYPE) {
    /* Nothing to do for data. */
    return mem + IK_PAGESIZE;
  }
  else if (type == MAINHEAP_TYPE) {
    /* Nothing to do for main heap. */
    return mem + IK_PAGESIZE;
  }
  else if (type == MAINSTACK_TYPE) {
    /* Nothing to do for main stack. */
    return mem + IK_PAGESIZE;
  }
  ik_abort("unknown page type bits: 0x%08x\n", type);
  return NULL;
}


static uint8_t *
verify_code_page (uint8_t * mem, uint32_t segment_bits, uint32_t dirty_bits,
		  uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector)
/* Assume that MEM references the first word of a code object.  Return a
   pointer to the next machine word to verify. */
{
  uint8_t *	first_word_of_next_page	= NULL;
  ikptr		p_code	= (ikptr)mem;
  ik_ulong	aligned_code_object_size;

  verify_code_object_first_word(p_code);
  aligned_code_object_size = verify_code_object_size(p_code);

  if (aligned_code_object_size <= IK_PAGESIZE) {
    /* This is a small Scheme code object, which fits in a single Vicare
       page.  However  this might be  the first  in a sequence  of small
       code objects  stored in  the same Vicare  page.  We  must iterate
       over them and validate them all. */
    first_word_of_next_page = verify_code_small_size_sequence(mem, mem_base, segment_vector, dirty_vector);
  } else {
    /* This is a  large Scheme code object, which fits  in a sequence of
       contiguous  Vicare pages.   The  FIRST_WORD_AFTER references  the
       machine word which starts the next page. */
    verify_code_common_fields(mem, mem_base, segment_vector, dirty_vector);
    first_word_of_next_page = ((uint8_t *)IK_ALIGN_TO_NEXT_PAGE(mem + aligned_code_object_size));
  }

  return first_word_of_next_page;
}

/* ------------------------------------------------------------------ */

static uint8_t *
verify_code_small_size_sequence (uint8_t * first_word_in_this_page,
				 uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector)
/* Assume that  FIRST_WORD_IN_THIS_PAGE references  the first word  of a
   code object whose size fits into a standalone Vicare page.  This code
   object might  be the  first in  a sequence  of contiguous  small code
   objects  fitting in  the single  page.  Verify  the code  objects and
   return a pointer  to the next machine word to  verify (which starts a
   new page).

   It does not matter if we check some fields multiple times. */
{
  /* The sequence of code objects must end before this location. */
  uint8_t *	first_word_of_next_page = first_word_in_this_page + IK_PAGESIZE;
  uint8_t *	current_code_object     = first_word_in_this_page;
  for (;;) {
    ikptr	p_code	= (ikptr)current_code_object;
    /* The ALIGNED_CODE_OBJECT_SIZE is an exact multiple of 16 bytes. */
    ik_ulong	aligned_code_object_size;
    uint8_t *	first_word_after_code_object;

    verify_code_common_fields(current_code_object, mem_base, segment_vector, dirty_vector);
    aligned_code_object_size     = verify_code_object_size(p_code);
    first_word_after_code_object = current_code_object + aligned_code_object_size;
    if ((aligned_code_object_size > IK_PAGESIZE) || (first_word_of_next_page < first_word_after_code_object)) {
      ik_debug_message("small code object should fit in a single page, but it extends beyond the page boundary:\n\
\taligned code object size                = 0x%016lx\n\
\tpage size                               = 0x%016lx\n\
\taddress of first code object in page    = 0x%016lx\n\
\taddress of first word of next page      = 0x%016lx\n\
\taddress of current code object          = 0x%016lx\n\
\taddress of first word after code object = 0x%016lx",
		       aligned_code_object_size, IK_PAGESIZE,
		       first_word_in_this_page, first_word_of_next_page,
		       current_code_object, first_word_after_code_object);
      goto integrity_error;
    }
    if (first_word_after_code_object == first_word_of_next_page) {
      /* Lucky  one.  The  current code  object's aligned  size finishes
	 exactly at the end of the page; no room wasted. */
      break;
    }
    /* Are there more code objects in the sequence? */
    else if (code_tag == IK_REF(first_word_after_code_object, disp_code_tag)) {
      /* Yes, more objects.  Verify them too. */
      current_code_object = first_word_after_code_object;
    } else {
      /* No more objects.*/
      break;
    }
  }
  return first_word_of_next_page;

 integrity_error:
  log_invalid_code_object(((ikptr)current_code_object) | code_primary_tag);
  ik_abort("integrity check failed");
  return NULL;
}

/* ------------------------------------------------------------------ */

static void
verify_code_object_first_word (ikptr p_code)
/* Verify the first word. */
{
  const ikptr	fst	= IK_REF(p_code, disp_code_tag);
  if (code_tag != fst) {
    ik_abort("expected code_tag as first word of code object memory block, found: 0x%016lx\n", (long)fst);
  }
}
static ik_ulong
verify_code_object_size (ikptr p_code)
/* Verify the binary code size.  Return the aligned code object size, an
 * exact multiple of 16.
 *
 * BINARY_CODE_SIZE is the  number of bytes actually used  by the memory
 * area holding  the executable  machine code.  CODE_OBJECT_SIZE  is the
 * number of bytes  actually used by the Scheme object  in the allocated
 * memory region.  ALIGNED_CODE_OBJECT_SIZE is  the number of bytes used
 * by the code object aligned to an exact multiple of 16 bytes.
 *
 *        meta data          executable machine code         unused
 *   |----------------|-----------------------------------|++++++++++|
 *
 *     disp_code_data          binary_code_size
 *   |................|...................................|
 *
 *                      code_object_size
 *   |....................................................|
 *
 *                      aligned_code_object_size
 *   |...............................................................|
 */
{
  const ikptr	s_binary_code_size = IK_REF(p_code, disp_code_code_size);
  ik_ulong	binary_code_size;
  ik_ulong	code_object_size;
  ik_ulong	aligned_code_object_size;
  if (! IK_IS_FIXNUM(s_binary_code_size)) {
    ik_debug_message_no_newline("%s: expected fixnum as code object size, got: ", __func__);
    ik_print(s_binary_code_size);
    goto integrity_error;
  }
  binary_code_size = IK_UNFIX(s_binary_code_size);
  if (binary_code_size < 0) {
    ik_debug_message_no_newline("%s: expected non-negative fixnum as code object size, got: ", __func__);
    ik_print(s_binary_code_size);
    goto integrity_error;
  }
  code_object_size         = disp_code_data + binary_code_size;
  aligned_code_object_size = IK_ALIGN(code_object_size);
  return aligned_code_object_size;

 integrity_error:
  log_invalid_code_object(p_code | code_primary_tag);
  ik_abort("integrity check failed");
  return 0;
}
static void
verify_code_common_fields (uint8_t * mem,
			   uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector)
{
  ikptr		p_code	= (ikptr)mem;

  verify_code_object_first_word(p_code);
  verify_code_object_size(p_code);

  /* Verify the number of free variables. */
  {
    ikptr	s_freevars = IK_REF(p_code, disp_code_freevars);
    if (!IK_IS_FIXNUM(s_freevars)) {
      ik_debug_message_no_newline("%s: expected fixnum as number of free variables in code object, got: ", __func__);
      ik_print(s_freevars);
      goto integrity_error;
    } else {
      ik_ulong	num_of_free_vars = IK_UNFIX(s_freevars);
      if (num_of_free_vars < 0) {
	ik_debug_message_no_newline("%s: expected non-negative fixnum as number of free variables in code object, got: ", __func__);
	ik_print(s_freevars);
	goto integrity_error;
      }
    }
  }

  /* Verify the annotation object. */
  {
    ikptr	s_annotation	= IK_REF(p_code, disp_code_annotation);
    verify_object(s_annotation, mem_base, segment_vector, dirty_vector);
  }

  /* Verify the unused word. */
  {
    ikptr	s_unused	= IK_REF(p_code, disp_code_unused);
    if (IK_FIX(0) != s_unused) {
      ik_debug_message_no_newline("%s: expected fixnum zero as unused word in code object, got: ", __func__);
      ik_print(s_unused);
      goto integrity_error;
    }
  }

  /* Verify the relocation vector. */
  {
    ikptr	s_relocation_vector	= IK_REF(p_code, disp_code_reloc_vector);
    assert(ik_is_vector(s_relocation_vector));
    uint32_t	rs	= segment_vector[page_idx((void*)(long)s_relocation_vector) - page_idx(mem_base)];
    uint32_t	cs	= segment_vector[page_idx(mem) - page_idx(mem_base)];
    uint32_t	cgen	= cs & GEN_MASK;
    uint32_t	rgen	= rs & GEN_MASK;
    if (rgen < cgen) {
      uint32_t dirty_bits = dirty_vector[page_idx(mem) - page_idx(mem_base)];
      dirty_bits = dirty_bits & dirty_bits;
      //int off = (((int)x) - IK_ALIGN_TO_PREV_PAGE(x)) / card_size;
      //int card_mark = (d >> off) & 0xF;
      if (0 != dirty_bits) {
	ik_debug_message("%s: expected zero as dirty bits in code object's relocation vector, got: 0x%016lx",
			 __func__, dirty_bits);
	goto integrity_error;
      }
    }
  }

  return;

 integrity_error:
  log_invalid_code_object(p_code | code_primary_tag);
  ik_abort("integrity check failed");
}
static void
log_invalid_code_object (ikptr X)
{
  ik_debug_message_no_newline("%s: code object: ", __func__);
  ik_print(X);
  ik_debug_message_no_newline("\treloc vector ref: 0x%016lx\n", IK_REF(X, off_code_reloc_vector));
  ik_debug_message_no_newline("\tfree vars: 0x%016lx\n",        IK_REF(X, off_code_freevars));
  ik_debug_message_no_newline("\tannotation: ");
  ik_print(IK_REF(X, off_code_annotation));
  ik_debug_message_no_newline("\tunused word: 0x%016lx\n",      IK_REF(X, off_code_unused));
}


static uint8_t *
verify_scheme_objects_page (uint8_t * mem, uint32_t segment_bits IK_UNUSED, uint32_t dirty_bits IK_UNUSED,
			    uint8_t * mem_base, uint32_t * segment_vector, uint32_t * dirty_vector)
/* Assume that  MEM references the first  word of a page  holding tagged
   pointers to  Scheme objects.  Verify  the page; return a  raw pointer
   referencing the next page to check. */
{
  for (int i = 0; i < IK_PAGESIZE; i += wordsize) {
    verify_object(IK_REF(mem, i), mem_base, segment_vector, dirty_vector);
  }
  return mem + IK_PAGESIZE;
}
static void
verify_object (ikptr X IK_UNUSED,
	       uint8_t * mem_base IK_UNUSED, uint32_t * segment_vector IK_UNUSED, uint32_t * dirty_vector IK_UNUSED)
{
  return;
}

/* end of file */
