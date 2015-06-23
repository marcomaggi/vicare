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
#include <dlfcn.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifndef RTLD_DEFAULT
#  define RTLD_DEFAULT 0
#endif

#define BOOT_IMAGE_CODE_OBJECTS_GENERATION	IK_GC_GENERATION_NURSERY

#define DEBUG_FASL	0

typedef struct {
  uint8_t *	membase;
  uint8_t *	memp;
  uint8_t *	memq;

  /* These  are the  "Allocation  Pointer" and  "End  Pointer" for  code
     objects  allocation.  See  the  function "alloc_code_object()"  for
     details  about   how  code   objects  from   the  boot   image  are
     allocated. */
  ikptr_t	code_ap;
  ikptr_t	code_ep;

  ikptr_t *	marks;
  int		marks_size;
} fasl_port_t;

typedef struct {
  int		code_size;
  int		reloc_size;
  ikptr_t	closure_size;
} code_header;

/* ------------------------------------------------------------------ */

static int	object_count = 0;

/* ------------------------------------------------------------------ */

static ikptr_t	fasl_read_super_code_object(ikpcb_t * pcb, fasl_port_t* p);
static ikptr_t	do_read (ikpcb_t * pcb, fasl_port_t* p);
static ikptr_t	alloc_code_object (ikuword_t scheme_object_size, ikpcb_t * pcb, fasl_port_t* p);
static uint8_t	fasl_read_byte (fasl_port_t* p);
static void	fasl_read_buf (fasl_port_t* p, void* buf, ikuword_t num_of_bytes);


void
ik_fasl_load (ikpcb_t * pcb, const char * fasl_file)
/* Load  the boot  image from  the file  whose pathname  is "fasl_file".
   Execute the initialisation code of the boot image. */
{
  int		fd;
  int		filesize;
  int		mapsize;
  uint8_t *	mem;
  fasl_port_t	port;
  if (DEBUG_FASL)
    ik_debug_message("loading boot image file: %s", fasl_file);

  /* Open the boot image file. */
  {
    fd = open(fasl_file, O_RDONLY);
    if (-1 == fd)
      ik_abort("failed to open boot file \"%s\": %s", fasl_file, strerror(errno));
    {
      struct stat	buf;
      int		err = fstat(fd, &buf);
      if (err)
	ik_abort("failed to stat \"%s\": %s", fasl_file, strerror(errno));
      filesize = buf.st_size;
    }
  }

  /* Create  a memory  mapped buffer  from  which the  file is  actually
     read. */
  {
    mapsize	= IK_MMAP_ALLOCATION_SIZE(filesize);
    if (DEBUG_FASL)
      ik_debug_message("boot image: filesize=%d, mapsize=%d, pagesize=%d", filesize, mapsize, IK_PAGESIZE);
    mem		= mmap(0, mapsize, PROT_READ, MAP_PRIVATE, fd, 0);
    if (MAP_FAILED == mem)
      ik_abort("mapping failed for %s: %s", fasl_file, strerror(errno));
  }

  /* Initialise the "fasl_port_t" struct. */
  {
    port.membase	= mem;			/* base of the input buffer */
    port.memp		= mem;			/* pointer to the next byte to read */
    port.memq		= mem + filesize;	/* one-off end pointer */
    port.marks		= 0;
    port.marks_size	= 0;
  }

  /* Read  all the  objects  from  the memory  mapped  buffer.  Run  the
     initialisation code. */
  while (port.memp < port.memq) {
    ikptr	s_code;
    port.code_ap	= 0;
    port.code_ep	= 0;

    /* Read the next super object. */
    {
      if (DEBUG_FASL)
	ik_debug_message("*** read boot image super code object");
      s_code = fasl_read_super_code_object(pcb, &port);
    }

    /* Clear table of  marks.  Every super-object in the  boot image has
       its own table. */
    if (port.marks_size) {
      ik_munmap((ikptr)(ikuword_t)port.marks, port.marks_size * sizeof(ikptr_t*));
      port.marks      = 0;
      port.marks_size = 0;
    }

    /* Check if we have reached the end  of the boot image file.  At the
       end: we unmap the mmap buffer used to read the file and close the
       file descriptor. */
    if (port.memp == port.memq) {
      int	err;
      if (DEBUG_FASL)
	ik_debug_message("finished reading all the boot image");
      err = munmap(mem, mapsize);
      if (err)
        ik_abort("failed to unmap fasl file: %s", strerror(errno));
      close(fd);
    }

    /* Execute the initialisation code. */
    if (DEBUG_FASL)
      ik_debug_message("executing boot image super code object init expressions");
    {
      ikptr s_retval = ik_exec_code(pcb, s_code, 0, 0);
      if (0) {
	/* For some reason, at some point, someone introduced this check
	   for  debugging purposes.   It  is not  really important  what
	   return value comes from  running the init expression.  (Marco
	   Maggi; Thu Apr 16, 2015) */
	if (IK_VOID_OBJECT != s_retval) {
	  ik_debug_message_no_newline("%s: code object from %s returned non-void value: ",
				      __func__, fasl_file);
	  ik_print(s_retval);
	}
      }
    }
  }
  if (port.memp != port.memq)
    ik_abort("fasl-read did not reach EOF");
}


static ikptr_t
fasl_read_super_code_object (ikpcb_t * pcb, fasl_port_t* port)
{
  int8_t	buf[IK_FASL_HEADER_LEN];
  ikptr_t	s_code;
  /* First check the header. */
  fasl_read_buf(port, buf, IK_FASL_HEADER_LEN);
  if (0 != memcmp(buf, IK_FASL_HEADER, IK_FASL_HEADER_LEN))
    ik_abort("invalid fasl header");
  if (DEBUG_FASL) ik_debug_message("start reading boot image super code object");
  s_code = do_read(pcb, port);
  if (DEBUG_FASL) ik_debug_message("done reading boot image super code object");
  return s_code;
}


static ikptr_t
do_read (ikpcb_t * pcb, fasl_port_t* p)
/* Read and return an object form a FASL port.

   This function  is used only  to load the  boot image, so it  does not
   support the "O" object field which loads foreign libraries.  */
#undef DEBUG_FASL
#define DEBUG_FASL	0
{
  unsigned char	c = fasl_read_byte(p);
  uint32_t	put_mark_index = 0;
  if (0 || DEBUG_FASL)
    ik_debug_message("reading object with header: %c", c);
  if (c == '>') {
    /* We read  a mark index  from the  port; every object  branch below
       will  fill the  slot "p->marks[put_mark_index]"  for its  object.
       Here we only make sure that the mark index is valid. */
    uint32_t idx = 0;
    fasl_read_buf(p, &idx, sizeof(uint32_t));
    put_mark_index = idx;
    /* Read the header of the next object. */
    c = fasl_read_byte(p);
    if (p->marks) {
      /* Validate the mark index. */
      if (idx >= p->marks_size)
        ik_abort("%s: mark too big: %d", __func__, idx);
      if (idx < p->marks_size) {
        if (0 != p->marks[idx])
          ik_abort("%s: mark %d already set", __func__, idx);
      }
    } else {
      /* This is the first mark read.  Allocate the marks array. */
#define NUM_OF_MARKS		(4 * IK_CHUNK_SIZE)
#define MARKS_BLOCK_SIZE	(NUM_OF_MARKS * sizeof(ikptr_t *))
      p->marks = (ikptr_t*)ik_mmap(MARKS_BLOCK_SIZE);
      bzero(p->marks, MARKS_BLOCK_SIZE);
      p->marks_size = NUM_OF_MARKS;
    }
  }
  if (c == 'x') {	/* code object */
    if (DEBUG_FASL) ik_debug_message("open %d: code object", object_count++);
    /* A Scheme code object is stored in  a memory area whose size is an
     * exact multiple of a Vicare page size.
     *
     * SCHEME_OBJECT_SIZE is  the number of  bytes actually used  by the
     * Scheme object in the allocated memory region.
     *
     * BINARY_CODE_SIZE  is the  number of  bytes actually  used by  the
     * memory area holding the executable machine code.
     *
     *      meta data        executable machine code          unused
     *   |-------------|-----------------------------------|---------|
     *
     *   |.............| disp_code_data
     *                 |...................................| binary_code_size
     *   |.................................................| scheme_object_size
     *   |...........................................................| allocated memory size
     *
     */
    ikuword_t	scheme_object_size	= 0;
    ikuword_t	binary_code_size	= 0;
    ikptr_t	s_freevars		= IK_FIX(0);
    ikptr_t	s_annotation		= IK_FALSE;
    ikptr_t	p_code			= 0;
    /* Read the binary code size. */
    {
      if (4 == wordsize) {
	/* 32-bit platform.   The binary  code size  is serialised  as a
	   big-endian raw unsigned 32-bit integer. */
	uint32_t	full_binary_code_size = 0;
	fasl_read_buf(p, &full_binary_code_size, sizeof(uint32_t));
	binary_code_size = (ikuword_t) full_binary_code_size;
      } else {
	/* 64-bit platform.   The binary  code size  is serialised  as a
	   sequence of 2 big-endian raw unsigned 32-bit integers. */
	uint32_t	lo_binary_code_size = 0;
	uint32_t	hi_binary_code_size = 0;
	fasl_read_buf(p, &lo_binary_code_size, sizeof(uint32_t));
	fasl_read_buf(p, &hi_binary_code_size, sizeof(uint32_t));
	binary_code_size = (((ikuword_t) hi_binary_code_size) << 32) | ((ikuword_t) lo_binary_code_size);
      }
    }
    /* Read the number of free variables. */
    {
      if (4 == wordsize) {
	/* 32-bit platform.  The number  of free variables is serialised
	   as  a big-endian  unsigned  32-bit  integer representing  the
	   fixnum. */
	uint32_t	full_freevars = 0;
	fasl_read_buf(p, &full_freevars, sizeof(uint32_t));
	s_freevars = (ikptr_t) full_freevars;
      } else {
	/* 64-bit platform.   The binary  code size  is serialised  as a
	   sequence   of   2   big-endian   unsigned   32-bit   integers
	   representing the fixnum. */
	uint32_t	lo_freevars = 0;
	uint32_t	hi_freevars = 0;
	fasl_read_buf(p, &lo_freevars, sizeof(uint32_t));
	fasl_read_buf(p, &hi_freevars, sizeof(uint32_t));
	s_freevars = (((ikptr_t) hi_freevars) << 32) | ((ikptr_t) lo_freevars);
      }
    }
    /* Read the annotation. */
    s_annotation = do_read(pcb, p);
    if (DEBUG_FASL) ik_print(s_annotation);
    scheme_object_size = disp_code_data + binary_code_size;
    p_code             = alloc_code_object(scheme_object_size, pcb, p);
    IK_REF(p_code, 0)			= code_tag;
    IK_REF(p_code, disp_code_code_size)	= IK_FIX(binary_code_size);
    IK_REF(p_code, disp_code_freevars)	= s_freevars;
    IK_REF(p_code, disp_code_annotation)= s_annotation;
    IK_REF(p_code, disp_code_unused)    = IK_FIX(0);
    fasl_read_buf(p, (void*)(p_code+disp_code_data), binary_code_size);
    if (put_mark_index) {
      p->marks[put_mark_index] = p_code | vector_tag;
    }
    IK_REF(p_code, disp_code_reloc_vector) = do_read(pcb, p);
    ik_relocate_code(p_code);
    if (DEBUG_FASL) ik_debug_message("close %d: code object", --object_count);
    return p_code | vector_tag;
  }
  else if (c == 'P') { /* pair object */
    if (DEBUG_FASL) ik_debug_message("open %d: pair object", object_count++);
    ikptr_t	s_pair = ik_unsafe_alloc(pcb, pair_size) | pair_tag;
    /* Mark  the  pair  object  before  reading its  car  and  cdr:  the
       structure may reference the pair itself. */
    if (put_mark_index) {
      p->marks[put_mark_index] = s_pair;
    }
    IK_CAR(s_pair) = do_read(pcb, p);
    IK_CDR(s_pair) = do_read(pcb, p);
    if (DEBUG_FASL) ik_debug_message("close %d: pair object", --object_count);
    return s_pair;
  }
  else if (c == 'M') {	/* symbol object */
    if (DEBUG_FASL) ik_debug_message("open %d: symbol object", object_count++);
    ikptr_t	s_str = do_read(pcb, p);
    ikptr_t	s_sym = ikrt_string_to_symbol(s_str, pcb);
    if (put_mark_index) {
      p->marks[put_mark_index] = s_sym;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: symbol object", --object_count);
    return s_sym;
  }
  else if (c == 's') {	/* ASCII string */
    if (DEBUG_FASL)
      ik_debug_message("open %d: ascii string object", object_count++);
    ikuword_t	num_of_chars = 0;
    ikuword_t	mem_size;
    uint8_t *	ascii_data;
    ikptr	s_str;
    fasl_read_buf(p, &num_of_chars, sizeof(ikuword_t));
    if (0 || DEBUG_FASL)
      ik_debug_message("string length: %ld", (long)num_of_chars);
    mem_size	= IK_ALIGN(num_of_chars * IK_STRING_CHAR_SIZE + disp_string_data);
    s_str	= ik_unsafe_alloc(pcb, mem_size) | string_tag;
    IK_STRING_LENGTH_FX(s_str) = IK_FIX(num_of_chars);
    ascii_data	= IK_STRING_DATA_VOIDP(s_str);
    fasl_read_buf(p, ascii_data, num_of_chars);
    if (0 || DEBUG_FASL) {
      fwrite(ascii_data, 1, num_of_chars, stderr);
      fwrite("\n", 1, 1, stderr);
    }
    /* The ASCII  characters are stored  in the leftmost portion  of the
     * data area referenced by S_STR.
     *
     *         ASCII octets
     *    |++++++++++++++++++|------------------| <- s_str
     *
     * Here we generate the Scheme chars representation as tagged 32-bit
     * Unicode code points in the same data area, starting from the end:
     *
     *         ASCII octets
     *    |++++++++++++++++++|----------------|-| <- s_str
     *                      |                  ^
     *                      |                  |
     *                       ------------------
     *                    tagged 32-bit code point
     *
     * by proceeding  from the end to  the beginning we consume  all the
     * ASCII  octets  before  overwriting them  with  the  corresponding
     * tagged code points.
     */
    {
      ikchar_t *	unicode_code_points = (ikchar_t *)ascii_data;
      for (iksword_t i=num_of_chars-1; i >= 0; --i) {
        unicode_code_points[i] = IK_CHAR32_FROM_INTEGER(ascii_data[i]);
      }
    }
    if (put_mark_index) {
      p->marks[put_mark_index] = s_str;
    }
    if (DEBUG_FASL)
      ik_debug_message("close %d: ascii string object", --object_count);
    return s_str;
  }
  else if (c == 'S') {    /* Unicode string */
    if (DEBUG_FASL) ik_debug_message("open %d: string object", object_count++);
    ikuword_t	num_of_chars = 0;
    ikuword_t	mem_size;
    ikptr_t	s_str;
    fasl_read_buf(p, &num_of_chars, sizeof(ikuword_t));
    mem_size	= IK_ALIGN(num_of_chars*IK_STRING_CHAR_SIZE + disp_string_data);
    s_str	= ik_unsafe_alloc(pcb, mem_size) | string_tag;
    IK_STRING_LENGTH_FX(s_str) = IK_FIX(num_of_chars);
    for (iksword_t i=0; i<num_of_chars; ++i) {
      ikchar_t	ch = 0;
      fasl_read_buf(p, &ch, sizeof(ikchar_t));
      IK_CHAR32(s_str, i) = IK_CHAR32_FROM_INTEGER(ch);
    }
    if (put_mark_index) {
      p->marks[put_mark_index] = s_str;
    }
    if (DEBUG_FASL)
      ik_debug_message("close %d: string object", --object_count);
    return s_str;
  }
  else if (c == 'V') {	/* vector object */
    if (DEBUG_FASL)
      ik_debug_message("open %d: vector object", object_count++);
    ikuword_t	num_of_slots = 0;
    ikuword_t	mem_size;
    ikptr_t	s_vec;
    fasl_read_buf(p, &num_of_slots, sizeof(ikuword_t));
    mem_size	= IK_ALIGN(num_of_slots * wordsize + disp_vector_data);
    s_vec	= ik_unsafe_alloc(pcb, mem_size) | vector_tag;
    IK_VECTOR_LENGTH_FX(s_vec) = IK_FIX(num_of_slots);
    /* Mark  the  vector  before  marking its  values:  the  vector  may
       reference itself. */
    if (put_mark_index) {
      p->marks[put_mark_index] = s_vec;
    }
    for (iksword_t i=0; i<num_of_slots; ++i) {
      IK_ITEM(s_vec, i) = do_read(pcb, p);
    }
    if (DEBUG_FASL)
      ik_debug_message("close %d: vector object", --object_count);
    return s_vec;
  }
  else if (c == 'I') {	/* fixnum object */
    if (DEBUG_FASL) ik_debug_message("open %d: fixnum object", object_count++);
    ikptr_t	s_fixn;
    fasl_read_buf(p, &s_fixn, sizeof(ikptr_t));
    if (0 || DEBUG_FASL) {
      ik_debug_message("close %d: fixnum object, fixnum bytes size=%d, fx=%ld",
		       --object_count, sizeof(ikptr_t), IK_UNFIX(s_fixn));
    }
    return s_fixn;
  }
  else if (c == 'F') {
    if (DEBUG_FASL) ik_debug_message("read %d: false object", object_count);
    return IK_FALSE_OBJECT;
  }
  else if (c == 'T') {
    if (DEBUG_FASL) ik_debug_message("read %d: true object", object_count);
    return IK_TRUE_OBJECT;
  }
  else if (c == 'N') {
    if (DEBUG_FASL) ik_debug_message("read %d: null object", object_count);
    return IK_NULL_OBJECT;
  }
  else if (c == 'c') {	/* ASCII char object */
    if (DEBUG_FASL) ik_debug_message("open %d: char object in ASCII encoding", object_count++);
    unsigned char x = (unsigned char) fasl_read_byte(p);
    if (DEBUG_FASL) ik_debug_message("close %d: char object in ASCII encoding", --object_count);
    return IK_CHAR_FROM_INTEGER(x);
  }
  else if (c == 'G') {	/* gensym object */
    if (DEBUG_FASL) ik_debug_message("open %d: gensym object", object_count++);
    /* G is for gensym */
    ikptr_t	s_pretty = do_read(pcb, p);
    ikptr_t	s_unique = do_read(pcb, p);
    ikptr_t	s_sym    = ikrt_strings_to_gensym(s_pretty, s_unique, pcb);
    if (put_mark_index) {
      p->marks[put_mark_index] = s_sym;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: gensym object", --object_count);
    return s_sym;
  }
  else if (c == 'R') { /* R is for RTD */
    if (DEBUG_FASL) ik_debug_message("open %d: rtd object", object_count++);
    ikptr name = do_read(pcb, p);
    ikptr symb = do_read(pcb, p);
    long i, n = 0;
    fasl_read_buf(p, &n, sizeof(long));
    ikptr fields;
    if (n == 0) {
      fields = IK_NULL_OBJECT;
    } else {
      fields = ik_unsafe_alloc(pcb, n * IK_ALIGN(pair_size)) | pair_tag;
      ikptr ptr = fields;
      for (i=0; i<n; i++) {
        IK_REF(ptr, off_car) = do_read(pcb, p);
        IK_REF(ptr, off_cdr) = ptr + IK_ALIGN(pair_size);
        ptr += IK_ALIGN(pair_size);
      }
      ptr -= pair_size;
      IK_REF(ptr, off_cdr) = IK_NULL_OBJECT;
    }
    ikptr gensym_val = IK_REF(symb, off_symbol_record_value);
    ikptr rtd;
    if (gensym_val == IK_UNBOUND_OBJECT) {
      rtd = ik_unsafe_alloc(pcb, IK_ALIGN(rtd_size)) | vector_tag;
      ikptr base_rtd = pcb->base_rtd;
      IK_REF(rtd, off_rtd_rtd)		= base_rtd;
      IK_REF(rtd, off_rtd_name)		= name;
      IK_REF(rtd, off_rtd_length)	= IK_FIX(n);
      IK_REF(rtd, off_rtd_fields)	= fields;
      IK_REF(rtd, off_rtd_printer)	= IK_FALSE_OBJECT;
      IK_REF(rtd, off_rtd_symbol)	= symb;
      IK_REF(rtd, off_rtd_destructor)	= IK_FALSE;
      IK_REF(symb, off_symbol_record_value) = rtd;
      IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, symb + off_symbol_record_value);
    } else {
      rtd = gensym_val;
    }
    if (put_mark_index) {
      p->marks[put_mark_index] = rtd;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: rtd object", --object_count);
    return rtd;
  }
  else if (c == '{') { /* { is for struct instances */
    if (DEBUG_FASL) ik_debug_message("open %d: struct instance object", object_count++);
    long	i, num_of_fields = 0, struct_size;
    ikptr	s_rtd;
    ikptr	s_struct;
    fasl_read_buf(p, &num_of_fields, sizeof(long));
    struct_size = IK_ALIGN((1 + num_of_fields) * sizeof(ikptr));
    s_struct    = ik_unsafe_alloc(pcb, struct_size) | vector_tag;
    s_rtd       = do_read(pcb, p);
    IK_REF(s_struct, 0) = s_rtd;
    for (i=0; i<num_of_fields; ++i) {
      IK_FIELD(s_struct, i) = do_read(pcb, p);
    }
    if (put_mark_index) {
      p->marks[put_mark_index] = s_struct;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: struct instance object", --object_count);
    return s_struct;
  }
  else if (c == 'W') { /* W is for R6RS record-type descriptors */
    ikptr	s_name   = do_read(pcb, p);
    ik_debug_message_no_newline("R6RS record-type descriptor in boot image: ");
    ik_print(s_name);
    ik_abort("invalid type '%c' (0x%02x), R6RS record-type descriptor, found in fasl file", c, c);
    return IK_VOID_OBJECT;
#if 0
  /* This  is currently  excluded because  there is  no way  to build  a
     record-type descriptor from C language level. */
    long	i, record_size;
    ikptr	s_name   = do_read(pcb, p);
    ikptr	s_parent = do_read(pcb, p);
    ikptr	s_uid    = do_read(pcb, p);
    ikptr	s_sealed = do_read(pcb, p);
    ikptr	s_opaque = do_read(pcb, p);
    ikptr	s_count  = do_read(pcb, p);
    long	num_of_fields = IK_UNFIX(s_count);
    ikptr	s_fields = iku_vector_alloc_and_init(pcb, num_of_fields);
    ikptr	s_record = IK_VOID;
    for (i=0; i<num_of_fields; ++i) {
      ikptr	s_field_is_mutable = do_read(pcb, p);
      ikptr	s_field_name       = do_read(pcb, p);
      ikptr	s_1st_pair = iku_pair_alloc(pcb);
      ikptr	s_2nd_pair = iku_pair_alloc(pcb);
      if (IK_TRUE == s_field_is_mutable) {
	IK_CAR(s_1st_pair) = iku_symbol_from_string(pcb, iku_string_from_cstring(pcb, "mutable"));
      } else {
	IK_CAR(s_1st_pair) = iku_symbol_from_string(pcb, iku_string_from_cstring(pcb, "immutable"));
      }
      IK_CDR(s_1st_pair) = s_2nd_pair;
      IK_CAR(s_2nd_pair) = s_field_name;
      IK_CDR(s_2nd_pair) = IK_NULL;
      IK_ITEM(s_fields, i) = s_1st_pair;
    }
    /* FIXME How do we build a record type descriptor here? */
    return s_record;
#endif
  }
  else if (c == 'Q') { /* thunk */
    if (DEBUG_FASL) ik_debug_message("open %d: thunk object", object_count++);
    ikptr s_proc = ik_unsafe_alloc(pcb, IK_ALIGN(disp_closure_data)) | closure_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = s_proc;
    }
    ikptr s_code = do_read(pcb, p);
    /* Store in  the closure's memory block  a raw pointer to  the first
       byte of the code object's data area. */
    IK_REF(s_proc, off_closure_code) = s_code + off_code_data;
    if (DEBUG_FASL) ik_debug_message("close %d: thunk object", --object_count);
    return s_proc;
  }
  else if (c == '<') {
    if (DEBUG_FASL) ik_debug_message("open %d: marked object", object_count++);
    int idx = 0;
    fasl_read_buf(p, &idx, sizeof(int));
    if ((idx <= 0) || (idx >= p->marks_size))
      ik_abort("invalid index for ref %d", idx);
    ikptr obj = p->marks[idx];
    if (obj) {
      if (DEBUG_FASL) ik_debug_message("close %d: marked object", --object_count);
      return obj;
    } else {
      ik_abort("reference to uninitialized mark %d", idx);
      return IK_VOID_OBJECT;
    }
  }
  else if (c == 'v') {
    if (DEBUG_FASL) ik_debug_message("open %d: bytevector object", object_count++);
    /* bytevector */
    long len = 0;
    fasl_read_buf(p, &len, sizeof(long));
    long  size = IK_ALIGN(len + disp_bytevector_data + 1);
    ikptr x    = ik_unsafe_alloc(pcb, size) | bytevector_tag;
    IK_REF(x, off_bytevector_length) = IK_FIX(len);
    fasl_read_buf(p, (void*)(long)(x+off_bytevector_data), len);
    ((char*)(long)x)[off_bytevector_data+len] = 0;
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: bytevector object", --object_count);
    return x;
  }
  else if (c == 'l') {
    if (DEBUG_FASL) ik_debug_message("open %d: short list object", object_count++);
    int   len  = (int) fasl_read_byte(p);
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    int i;
    ikptr pt = pair;
    for (i=0; i<len; i++) {
      IK_REF(pt, off_car) = do_read(pcb, p);
      IK_REF(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    IK_REF(pt, off_car) = do_read(pcb, p);
    IK_REF(pt, off_cdr) = do_read(pcb, p);
    if (DEBUG_FASL) ik_debug_message("close %d: short list object", --object_count);
    return pair;
  }
  else if (c == 'L') {
    if (DEBUG_FASL) ik_debug_message("open %d: long list object", object_count++);
    long len = 0;
    fasl_read_buf(p, &len, sizeof(long));
    if (len < 0)
      ik_abort("invalid len=%ld", len);
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    long i;
    ikptr pt = pair;
    for (i=0; i<len; i++) {
      IK_REF(pt, off_car) = do_read(pcb, p);
      IK_REF(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    IK_REF(pt, off_car) = do_read(pcb, p);
    IK_REF(pt, off_cdr) = do_read(pcb, p);
    if (DEBUG_FASL) ik_debug_message("close %d: long list object", --object_count);
    return pair;
  }
  else if (c == 'f') {
    if (DEBUG_FASL) ik_debug_message("open %d: flonum object", object_count++);
    ikptr x = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;
    IK_REF(x, -vector_tag) = flonum_tag;
    fasl_read_buf(p, (void*)(long)(x+disp_flonum_data-vector_tag), 8);
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: flonum object", --object_count);
    return x;
  }
  else if (c == 'C') {
    if (DEBUG_FASL) ik_debug_message("open %d: char object", object_count++);
    int n = 0;
    fasl_read_buf(p, &n, sizeof(int));
    if (DEBUG_FASL) ik_debug_message("close %d: char object", --object_count);
    return IK_CHAR_FROM_INTEGER(n);
  }
  else if (c == 'b') {
    if (DEBUG_FASL) ik_debug_message("open %d: bignum object", object_count++);
    /* The first word in the memory block of the bignum object. */
    ikuword_t	first_word;
    /* The number  of octets representing  the bignum.  If  positive the
       bignum is positive, if negative the bignum is negative. */
    long number_of_octets = 0;
    /* The number of  limbs (machine words) representing  the bignum; on
       32-bit  platforms: number_of_octets  >> 2;  on 64-bit  platforms:
       number_of_octets >> 3. */
    long nlimbs = 0;
    /* The sign bit of the bignum. */
    long sign = 0;
    /* We assume the type "long" represents a machine word. */
    fasl_read_buf(p, &number_of_octets, sizeof(long));
    if (number_of_octets < 0) {
      sign = 1;
      number_of_octets = -number_of_octets;
    }
    /* The number  of octets must  be an  exact multiple of  the machine
       word size. */
    if (number_of_octets & ((wordsize == 4)? 3 : 7))
      ik_abort("error in fasl-read: invalid bignum length %ld", number_of_octets);
    nlimbs = (number_of_octets >> ((wordsize == 4)? 2 : 3));
    first_word = bignum_tag			\
      | (sign << bignum_sign_shift)		\
      | (nlimbs << bignum_nlimbs_shift);
    ikptr x = ik_unsafe_alloc(pcb, IK_ALIGN(number_of_octets + disp_bignum_data)) | vector_tag;
    IK_REF(x, -vector_tag) = (ikptr) first_word;
    /* Read the vector of limbs as vector of octets. */
    fasl_read_buf(p, (void*)(long)(x+off_bignum_data), number_of_octets);
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: bignum object", --object_count);
    return x;
  }
  else if (c == 'i') {
    if (DEBUG_FASL) ik_debug_message("open %d: complex number object", object_count++);
    ikptr real = do_read(pcb, p);
    ikptr imag = do_read(pcb, p);
    ikptr x;
    if ((IK_TAGOF(real) == vector_tag)
	&& (IK_REF(real, -vector_tag) == flonum_tag)) {
      x = ik_unsafe_alloc(pcb, cflonum_size);
      IK_REF(x, 0) = cflonum_tag;;
      IK_REF(x, disp_cflonum_real) = real;
      IK_REF(x, disp_cflonum_imag) = imag;
    } else {
      x = ik_unsafe_alloc(pcb, compnum_size);
      IK_REF(x, 0) = compnum_tag;
      IK_REF(x, disp_compnum_real) = real;
      IK_REF(x, disp_compnum_imag) = imag;
    }
    x += vector_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    if (DEBUG_FASL) ik_debug_message("close %d: complex number object", --object_count);
    return x;
  } else {
    ik_abort("invalid type '%c' (0x%02x) found in fasl file", c, c);
    return IK_VOID_OBJECT;
  }
}


static ikptr_t
alloc_code_object (ikuword_t scheme_object_size, ikpcb_t * pcb, fasl_port_t* p)
/* Scheme code are of 2 categories:
 *
 * - Small code objects whose size fits  in a single Vicare page.  Small
 *   code objects  are allocated  in the "current  code page":  a Vicare
 *   page marked  as used "for code",  in which code objects  are stored
 *   one after  the other, with  size aligned  to exact multiples  of 16
 *   bytes:
 *
 *                         current code page
 *     |..........................................................|
 *
 *       code object   code object   code object       free
 *     |.............|.............|.............|++++++++++++++++|
 *     |----------------------------------------------------------|--
 *                                                ^                ^
 *                                             code_ap          code_ep
 *
 *   the fields CODE_AP and CODE_EP  in the FASL_PORT_T reference the free
 *   portion in the current code page.
 *
 * - Large  code objects  whose size  fits in  a sequence  of contiguous
 *   Vicare pages.
 *
 *         page        page        page        page        page
 *     |...........|...........|...........|...........|...........|
 *
 *                      large code object                     free
 *     |---------------------------------------------------|+++++++|
 *
 *   the free room at the end of the sequence of pages is lost.
 *
 * SCHEME_OBJECT_SIZE is  the non-aligned number of  bytes actually used
 * by the Scheme object in the allocated memory region.
 *
 * The ALIGNED_SCHEME_CODE_OBJECT_SIZE is an exact multiple of 16 bytes.
 *
 * NOTE When this function is called  to allocate a "super code object":
 * the fields  CODE_AP and  CODE_EP in  the FASL_PORT_T  are set  to NULL.
 * This  function behaves  correctly  in this  situation  and fills  the
 * fields with appropriate values.
 */
{
  ikuword_t	aligned_scheme_code_object_size = IK_ALIGN(scheme_object_size);
  ikptr_t	ap    = p->code_ap;
  ikptr_t	nap   = ap + aligned_scheme_code_object_size;
  if (0 && DEBUG_FASL)
    ik_debug_message("code_ap = 0x%016lx, code_ep = 0x%016lx", p->code_ap, p->code_ep);
  if (nap <= p->code_ep) {
    /* This is a small code object: it fits into a single page.
     *
     * If we are  here: we have already allocated a  page for small code
     * objects and  we are filling  it; this  code object fits  into the
     * available space.  Before the allocation:
     *
     *     code object   code object   this new code object       free
     *   |.............|.............|......................|++++++++++++++++|
     *   |-------------------------------------------------------------------|-- page
     *                                ^                                       ^
     *                             code_ap                                  code_ep
     *
     * before the allocation:
     *
     *     code object   code object   this new code object       free
     *   |.............|.............|......................|++++++++++++++++|
     *   |-------------------------------------------------------------------|-- page
     *                                                       ^                ^
     *                                                  code_ap            code_ep
     */
    p->code_ap = nap;
    return ap;
  } else if (aligned_scheme_code_object_size < IK_PAGESIZE) {
    /* This is a small code object: it fits into a single page.
     *
     * If we are here, either: CODE_AP  and CODE_EP are NULL, so we have
     * to allocate a  new page for code objects; or  there is not enough
     * room in  the page between  CODE_AP and  CODE_EP to store  the new
     * code object.
     *
     * We allocate a new page marked  in the segments vector as used for
     * code.
     *
     * After  the new  page allocation,  we might  have this  situation:
     * CODE_AP and CODE_EP  still reference the old page  with free room
     * in it; MEM references a new page with possible free room in it.
     *
     *     used by previous code objects    free
     *   |-------------------------------|++++++++|-- old page
     *                                    ^        ^
     *                                 code_ap   code_ep
     *
     *     used by this new code object     free
     *   |-------------------------------|++++++++|-- new page
     *    ^                               ^
     *   mem                mem + aligned_scheme_code_object_size
     *
     * Question: do we  adopt the new page as "current  code page" or do
     * we leave the old page alone?   Answer: if there is more free room
     * in the new  page than in the  old page, we adopt the  new page as
     * "current code page".
     */
    ikptr_t	mem		= ik_mmap_code(IK_PAGESIZE, BOOT_IMAGE_CODE_OBJECTS_GENERATION, pcb);
    ikuword_t	free_bytes_in_new_code_page = IK_PAGESIZE - aligned_scheme_code_object_size;
    ikuword_t	free_bytes_in_old_code_page = ((ikuword_t)p->code_ep) - ((ikuword_t)ap);
    if (free_bytes_in_new_code_page > free_bytes_in_old_code_page) {
      p->code_ap = mem + aligned_scheme_code_object_size;
      p->code_ep = mem + IK_PAGESIZE;
    }
    return mem;
  } else {
    /* This  is  a  large  code  object: it  fits  into  a  sequence  of
     * contiguous pages.
     *
     * We allocate a new sequence of pages marked in the segments vector
     * as  used for  code.  We  leave untouched  the page  referenced by
     * CODE_AP  and CODE_EP  as  "current code  page"  for future  small
     * objects allocations.
     */
    ikuword_t	aligned_scheme_code_object_size = IK_ALIGN_TO_NEXT_PAGE(scheme_object_size);
    ikptr_t	mem   = ik_mmap_code(aligned_scheme_code_object_size, BOOT_IMAGE_CODE_OBJECTS_GENERATION, pcb);
    return mem;
  }
}
void
ik_relocate_code (ikptr_t p_code)
/* Accept as  argument an *untagged*  pointer to a code  object; process
   the code object's relocation vector.

   This function called:

   - whenever a code  object is allocated, in this  case CODE references
     an allocated but still empty code object;

   - whenever a  code object is read  from a FASL file;

   - whenever a code object is created by the assembler. */
{
  /* The relocation vector. */
  const ikptr_t s_reloc_vec = IK_REF(p_code, disp_code_reloc_vector);
  /* The  number of  items in  the relocation  vector; it  can be  zero.
     Remember  that the  fixnum representing  the number  of items  in a
     vector, taken as  "iksword_t", also represents the  number of bytes
     in the data area of the vector. */
  const ikptr_t s_reloc_vec_len = IK_VECTOR_LENGTH_FX(s_reloc_vec);
  /* The variable P_DATA is an  *untagged* pointer referencing the first
     byte in the data area of the code object. */
  const ikptr_t p_data = p_code + disp_code_data;
  /* The variable P_RELOC_VEC_CUR is an  *untagged* pointer to the first
     word in the data area of the relocation vector RELOC_VEC. */
  ikptr_t p_reloc_vec_cur  = s_reloc_vec + off_vector_data;
  /* The variable P_RELOC_VEC_END  is an *untagged* pointer  to the word
     right after the data area of the relocation vector VEC. */
  const ikptr_t p_reloc_vec_end = p_reloc_vec_cur + s_reloc_vec_len;
  /* If the relocation vector is empty: do nothing. */
  while (p_reloc_vec_cur < p_reloc_vec_end) {
    const iksword_t	first_record_bits = IK_UNFIX(IK_RELOC_RECORD_1ST(p_reloc_vec_cur));
    if (0 == first_record_bits)
      ik_abort("invalid empty record in code object's relocation vector");
    const iksword_t	reloc_record_tag = IK_RELOC_RECORD_1ST_BITS_TAG(first_record_bits);
    const iksword_t	disp_code_word   = IK_RELOC_RECORD_1ST_BITS_OFFSET(first_record_bits);
    switch (reloc_record_tag) {
    case IK_RELOC_RECORD_VANILLA_OBJECT_TAG: {
      /* This record represents a vanilla object; this record is 2 words
	 wide.  The second word contains the reference to the object (or
	 the object itself if immediate). */
      IK_REF(p_data, disp_code_word) = IK_RELOC_RECORD_2ND(p_reloc_vec_cur);
      p_reloc_vec_cur += (2*wordsize);
      break;
    }
    case IK_RELOC_RECORD_DISPLACED_OBJECT_TAG: {
      /* This record  represents a  displaced object;  this record  is 3
	 words  wide.  The  second word  contains the  displacement, the
	 third word contains the reference to the object. */
      const iksword_t	obj_off = IK_UNFIX(IK_RELOC_RECORD_2ND(p_reloc_vec_cur));
      const ikptr_t	s_obj   =          IK_RELOC_RECORD_3RD(p_reloc_vec_cur);
      IK_REF(p_data, disp_code_word) = s_obj + obj_off;
      p_reloc_vec_cur += (3*wordsize);
      break;
    }
    case IK_RELOC_RECORD_JUMP_LABEL_TAG: {
      /* This record  represents a  jump label; this  record is  3 words
	 wide. */
      const iksword_t obj_off           = IK_UNFIX(IK_RELOC_RECORD_2ND(p_reloc_vec_cur));
      const iksword_t obj               =          IK_RELOC_RECORD_3RD(p_reloc_vec_cur);
      const iksword_t displaced_object  = obj + obj_off;
      const iksword_t next_word         = p_data + disp_code_word + 4;
      const iksword_t relative_distance = displaced_object - next_word;
#if 0
      if (wordsize == 8) {
        relative_distance += 4;
      }
#endif
      /* FIXME Why  is the target  word an  "int" rather than  a "long"?
	 (Marco Maggi; Oct 5, 2012) */
      *((int*)(p_data + disp_code_word)) = relative_distance;
      /* IK_REF(next_word, -wordsize) = relative_distance; */
      p_reloc_vec_cur += (3*wordsize);
      break;
    }
    case IK_RELOC_RECORD_FOREIGN_ADDRESS_TAG: {
      /* This record represents a foreign object; this record is 2 words
	 wide.   We store  directly the  address of  the foreign  object
	 (usually a C function) in the data area. */
      ikptr_t	s_str	= IK_RELOC_RECORD_2ND(p_reloc_vec_cur);
      char *	name	= NULL;
      if (IK_TAGOF(s_str) == bytevector_tag) {
        name = IK_BYTEVECTOR_DATA_CHARP(s_str);
      } else
        ik_abort("foreign name is not a bytevector");
      /* We call "dlerror()" here to  clean up possible previous errors.
	 (Marco Maggi; Oct 4, 2012) */
      dlerror();
      void *	sym	= dlsym(RTLD_DEFAULT, name);
      char *	err	= dlerror();
      if (err)
        ik_abort("dlsym() failed to find foreign name %s: %s", name, err);
      IK_REF(p_data, disp_code_word) = (ikptr_t)sym;
      p_reloc_vec_cur += (2*wordsize);
      break;
    }
    default:
      ik_abort("invalid first word in relocation vector's record: 0x%016lx (tag=%ld)",
	       first_record_bits, reloc_record_tag);
      break;
    } /* end of switch() */
  } /* end of while() */
}


static uint8_t
fasl_read_byte (fasl_port_t * port)
{
  int8_t	byte = 0;
  if (port->memp < port->memq) {
    byte = *(port->memp);
    port->memp++;
  } else
    ik_abort("%s: attempt to read objects from boot image file beyond EOF", __func__);
  return byte;
}
static void
fasl_read_buf (fasl_port_t * port, void * buf, ikuword_t num_of_bytes)
/* Read a block  of bytes from a FASL port.   NUM_OF_BYTES is the number
 * of bytes and BUF a pointer to the buffer that will hold them.
 *
 * Bytes are read in "big endian"  order (the most-significant byte of a
 * word is at the smallest memory address and the least significant byte
 * at the largest); for example the block from the underlying device:
 *
 *                     DD CC BB AA
 *    head of file |--|--|--|--|--|--| tail of file
 *
 * is read as 32-bit integer as:
 *
 *    #xAABBCCDD
 *       ^     ^
 *       |     least significant
 *       |
 *       most significant
 */
{
  if ((port->memp + num_of_bytes) <= port->memq) {
    memcpy(buf, port->memp, (size_t)num_of_bytes);
    port->memp += num_of_bytes;
  } else
    ik_abort("%s: attempt to read objects from boot image file beyond EOF", __func__);
}

/* end of file */
