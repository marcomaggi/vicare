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
#define RTLD_DEFAULT 0
#endif

typedef struct {
  char*		membase;
  char*		memp;
  char*		memq;
  ikptr		code_ap;
  ikptr		code_ep;
  ikptr*	marks;
  int		marks_size;
} fasl_port;

typedef struct {
  int		code_size;
  int		reloc_size;
  ikptr		closure_size;
} code_header;

static ikptr ik_fasl_read(ikpcb* pcb, fasl_port* p);


void
ik_fasl_load (ikpcb* pcb, char* fasl_file)
{
  int		fd;
  int		filesize;
  int		mapsize;
  char *	mem;
  fasl_port	p;
  fd = open(fasl_file, O_RDONLY);
  if (-1 == fd)
    ik_abort("failed to open boot file \"%s\": %s", fasl_file, strerror(errno));
  {
    struct stat buf;
    int		err = fstat(fd, &buf);
    if (err)
      ik_abort("failed to stat \"%s\": %s", fasl_file, strerror(errno));
    filesize = buf.st_size;
  }
  mapsize	= ((filesize + IK_PAGESIZE - 1) / IK_PAGESIZE) * IK_PAGESIZE;
  mem		= mmap(0, mapsize, PROT_READ, MAP_PRIVATE, fd, 0);
  if (MAP_FAILED == mem)
    ik_abort("mapping failed for %s: %s", fasl_file, strerror(errno));
  p.membase	= mem;
  p.memp	= mem;
  p.memq	= mem + filesize;
  p.marks	= 0;
  p.marks_size	= 0;
  while (p.memp < p.memq) {
    p.code_ap	= 0;
    p.code_ep	= 0;
    ikptr v = ik_fasl_read(pcb, &p);
    if (p.marks_size) {
      ik_munmap((ikptr)(long)p.marks, p.marks_size*sizeof(ikptr*));
      p.marks = 0;
      p.marks_size = 0;
    }
    if (p.memp == p.memq) {
      int err = munmap(mem, mapsize);
      if (err)
        ik_abort("failed to unmap fasl file: %s", strerror(errno));
      close(fd);
    }
    ikptr val = ik_exec_code(pcb, v, 0, 0);
    if (val != IK_VOID_OBJECT)
      ik_print(val);
  }
  if (p.memp != p.memq)
    ik_abort("fasl-read did not reach EOF");
}


static ikptr
alloc_code (long int size, ikpcb* pcb, fasl_port* p)
{
  long		asize = IK_ALIGN(size);
  ikptr		ap    = p->code_ap;
  ikptr		nap   = ap + asize;
  if (nap <= p->code_ep) {
    p->code_ap = nap;
    return ap;
  } else if (asize < IK_PAGESIZE) {
    ikptr	mem		= ik_mmap_code(IK_PAGESIZE, 0, pcb);
    long	bytes_remaining = IK_PAGESIZE - asize;
    long	previous_bytes	= ((ik_ulong)p->code_ep) - ((ik_ulong)ap);
    if (bytes_remaining <= previous_bytes) {
      return mem;
    } else {
      p->code_ap = mem+asize;
      p->code_ep = mem+IK_PAGESIZE;
      return mem;
    }
  } else {
    long	asize = IK_ALIGN_TO_NEXT_PAGE(size);
    ikptr	mem   = ik_mmap_code(asize, 0, pcb);
    return mem;
  }
}


void
ik_relocate_code (ikptr code)
/* Accept as argument an *untagged* pointer to a code object. */
{
  ikptr	vec  = IK_REF(code, disp_code_reloc_vector);
  /* Remember  that the  fixnum representing  the number  of items  in a
     vector, taken as "long", also represents the number of bytes in the
     data area of the vector. */
  ikptr size = IK_VECTOR_LENGTH_FX(vec);
  /* The variable  DATA is an  *untagged* pointer referencing  the first
     byte of binary code in the code object. */
  ikptr data = code + disp_code_data;
  /* The variable  RELOC_VEC_CUR is an  *untagged* pointer to  the first
     word in the data area of the relocation vector VEC. */
  ikptr reloc_vec_cur  = vec  + off_vector_data;
  /* The variable  RELOC_VEC_PAST is an  *untagged* pointer to  the word
     right after the data area of the relocation vector VEC. */
  ikptr reloc_vec_past = reloc_vec_cur + size;
  /* If the relocation vector is empty: do nothing. */
  while (reloc_vec_cur < reloc_vec_past) {
    long	first_record_word = IK_UNFIX(IK_REF(reloc_vec_cur, 0));
    if (0 == first_record_word)
      ik_abort("invalid empty record in code object's relocation vector");
    const long	reloc_record_tag = first_record_word & 3;
    const long	code_off	 = first_record_word >> 2;
    if (reloc_record_tag == 0) {
      /* This record represents a vanilla object; this record is 2 words
	 wide. */
      IK_REF(data, code_off) = IK_REF(reloc_vec_cur, wordsize);
      reloc_vec_cur += (2*wordsize);
    } else if (reloc_record_tag == 2) {
      /* This record  represents a  displaced object;  this record  is 3
	 words wide. */
      long	obj_off	= IK_UNFIX(IK_REF(reloc_vec_cur, wordsize));
      ikptr	obj	= IK_REF(reloc_vec_cur, 2*wordsize);
      IK_REF(data, code_off) = obj + obj_off;
      reloc_vec_cur += (3*wordsize);
    } else if (reloc_record_tag == 3) {
      /* This record  represents a  jump label; this  record is  3 words
	 wide. */
      long	obj_off			= IK_UNFIX(IK_REF(reloc_vec_cur, wordsize));
      long	obj			= IK_REF(reloc_vec_cur, 2*wordsize);
      long	displaced_object	= obj + obj_off;
      long	next_word		= data + code_off + 4;
      long	relative_distance	= displaced_object - next_word;
#if 0
      if (wordsize == 8) {
        relative_distance += 4;
      }
#endif
      *((int*)(data+code_off)) = relative_distance;
      /* IK_REF(next_word, -wordsize) = relative_distance; */
      reloc_vec_cur += (3*wordsize);
    } else if (reloc_record_tag == 1) {
      /* This record represents a foreign object; this record is 2 words
	 wide. */
      ikptr	str	= IK_REF(reloc_vec_cur, wordsize);
      char *	name	= NULL;
      if (IK_TAGOF(str) == bytevector_tag) {
        name = (char*)(long) str + off_bytevector_data;
      } else
        ik_abort("foreign name is not a bytevector");
      /* FIXME Do we call "dlerror()" here to clean up possible previous
	 errors?  (Marco Maggi; Oct 4, 2012) */
      dlerror();
      void *	sym	= dlsym(RTLD_DEFAULT, name);
      char *	err	= dlerror();
      if (err)
        ik_abort("failed to find foreign name %s: %s", name, err);
      IK_REF(data,code_off) = (ikptr)sym;
      reloc_vec_cur += (2*wordsize);
    } else
      ik_abort("invalid first word in relocation vector's record: 0x%016lx (tag=%ld)",
	       first_record_word, reloc_record_tag);
  }
}


static char
fasl_read_byte (fasl_port* p)
{
  char	c = '\0';
  if (p->memp < p->memq) {
    c = *(p->memp);
    p->memp++;
  } else
    ik_abort("%s: read beyond EOF", __func__);
  return c;
}
static void
fasl_read_buf (fasl_port* p, void* buf, int n)
{
  if ((p->memp+n) <= p->memq) {
    memcpy(buf, p->memp, n);
    p->memp += n;
  } else
    ik_abort("%s: read beyond EOF", __func__);
}


static ikptr
do_read (ikpcb* pcb, fasl_port* p)
/* Read and return an object form a FASL port.

   This function  is used only  to load the  boot image, so it  does not
   support the "O" object field which loads foreign libraries.  */
{
  char	c = fasl_read_byte(p);
  int	put_mark_index = 0;
  if (c == '>') {
    int idx = 0;
    fasl_read_buf(p, &idx, sizeof(int));
    put_mark_index = idx;
    c = fasl_read_byte(p);
    if (idx <= 0)
      ik_abort("fasl_read: invalid index %d", idx);
    if (p->marks) {
      if (idx >= p->marks_size)
        ik_abort("mark too big: %d", idx);
      if (idx < p->marks_size) {
        if (p->marks[idx] != 0)
          ik_abort("mark %d already set", idx);
      }
    }
    else {
      /* allocate marks */
      p->marks = (ikptr*)(long)ik_mmap(2*IK_PAGESIZE*sizeof(ikptr*));
      bzero(p->marks, 2*IK_PAGESIZE*sizeof(ikptr*));
      p->marks_size = 2*IK_PAGESIZE;
    }
  }
  if (c == 'x') {
    long code_size;
    ikptr freevars;
    fasl_read_buf(p, &code_size, sizeof(long));
    fasl_read_buf(p, &freevars, sizeof(ikptr));
    ikptr annotation = do_read(pcb, p);
    ikptr code = alloc_code(IK_ALIGN(code_size+disp_code_data), pcb, p);
    IK_REF(code, 0) = code_tag;
    IK_REF(code, disp_code_code_size) = IK_FIX(code_size);
    IK_REF(code, disp_code_freevars) = freevars;
    IK_REF(code, disp_code_annotation) = annotation;
    fasl_read_buf(p, (void*)(disp_code_data+(long)code), code_size);
    if (put_mark_index) {
      p->marks[put_mark_index] = code+vector_tag;
    }
    IK_REF(code, disp_code_reloc_vector) = do_read(pcb, p);
    ik_relocate_code(code);
    return code+vector_tag;
  }
  else if (c == 'P') {
    ikptr pair = ik_unsafe_alloc(pcb, pair_size) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    IK_REF(pair, off_car) = do_read(pcb, p);
    IK_REF(pair, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if (c == 'M') {
    /* symbol */
    ikptr str = do_read(pcb, p);
    ikptr sym = ikrt_string_to_symbol(str, pcb);
    if (put_mark_index) {
      p->marks[put_mark_index] = sym;
    }
    return sym;
  }
  else if (c == 's') {
    /* ascii string */
    long len;
    fasl_read_buf(p, &len, sizeof(long));
    long size = IK_ALIGN(len*IK_STRING_CHAR_SIZE + disp_string_data);
    ikptr str = ik_unsafe_alloc(pcb, size) | string_tag;
    IK_REF(str, off_string_length) = IK_FIX(len);
    fasl_read_buf(p, (char*)(long)str+off_string_data, len);
    {
      unsigned char* pi = (unsigned char*)(long)(str+off_string_data);
      ikchar* pj = (ikchar*)(long)(str+off_string_data);
      long i = len-1;
      for (i=len-1; i >= 0; i--) {
        pj[i] = IK_CHAR32_FROM_INTEGER(pi[i]);
      }
    }
    //str[off_string_data+len] = 0;
    if (put_mark_index) {
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if (c == 'S') {
    /* string */
    long len;
    fasl_read_buf(p, &len, sizeof(long));
    long size = IK_ALIGN(len*IK_STRING_CHAR_SIZE + disp_string_data);
    ikptr str = ik_unsafe_alloc(pcb, size) | string_tag;
    IK_REF(str, off_string_length) = IK_FIX(len);
    long i;
    for (i=0; i<len; i++) {
      ikchar c;
      fasl_read_buf(p, &c, sizeof(ikchar));
      IK_CHAR32(str, i) = IK_CHAR32_FROM_INTEGER(c);
    }
    //str[off_string_data+len*IK_STRING_CHAR_SIZE] = 0;
    if (put_mark_index) {
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if (c == 'V') {
    long len;
    fasl_read_buf(p, &len, sizeof(long));
    long size = IK_ALIGN(len * wordsize + disp_vector_data);
    ikptr vec = ik_unsafe_alloc(pcb, size) | vector_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = vec;
    }
    IK_REF(vec, off_vector_length) = IK_FIX(len);
    long i;
    for (i=0; i<len; i++) {
      IK_REF(vec, off_vector_data + i*wordsize) = do_read(pcb, p);
    }
    return vec;
  }
  else if (c == 'I') {
    ikptr fixn;
    fasl_read_buf(p, &fixn, sizeof(ikptr));
    return fixn;
  }
  else if (c == 'F') {
    return IK_FALSE_OBJECT;
  }
  else if (c == 'T') {
    return IK_TRUE_OBJECT;
  }
  else if (c == 'N') {
    return IK_NULL_OBJECT;
  }
  else if (c == 'c') {
    /* FIXME: sounds broken */
    unsigned char x = (unsigned char) fasl_read_byte(p);
    return IK_CHAR_FROM_INTEGER(x);
  }
  else if (c == 'G') {
    /* G is for gensym */
    ikptr pretty = do_read(pcb, p);
    ikptr unique = do_read(pcb, p);
    ikptr sym = ikrt_strings_to_gensym(pretty, unique, pcb);
    if (put_mark_index) {
      p->marks[put_mark_index] = sym;
    }
    return sym;
  }
  else if (c == 'R') { /* R is for RTD */
    ikptr name = do_read(pcb, p);
    ikptr symb = do_read(pcb, p);
    long i, n;
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
      ((unsigned int*)(long)pcb->dirty_vector)[IK_PAGE_INDEX(symb+off_symbol_record_value)] = -1;
    } else {
      rtd = gensym_val;
    }
    if (put_mark_index) {
      p->marks[put_mark_index] = rtd;
    }
    return rtd;
  }
  else if (c == 'Q') { /* thunk */
    ikptr proc = ik_unsafe_alloc(pcb, IK_ALIGN(disp_closure_data)) | closure_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = proc;
    }
    ikptr code = do_read(pcb, p);
    IK_REF(proc, -closure_tag) = code + off_code_data;
    return proc;
  }
  else if (c == '<') {
    int idx;
    fasl_read_buf(p, &idx, sizeof(int));
    if ((idx <= 0) || (idx >= p->marks_size))
      ik_abort("invalid index for ref %d", idx);
    ikptr obj = p->marks[idx];
    if (obj) {
      return obj;
    } else {
      ik_abort("reference to uninitialized mark %d", idx);
      return IK_VOID_OBJECT;
    }
  }
  else if (c == 'v') {
    /* bytevector */
    long len;
    fasl_read_buf(p, &len, sizeof(long));
    long size = IK_ALIGN(len + disp_bytevector_data + 1);
    ikptr x = ik_unsafe_alloc(pcb, size) | bytevector_tag;
    IK_REF(x, off_bytevector_length) = IK_FIX(len);
    fasl_read_buf(p, (void*)(long)(x+off_bytevector_data), len);
    ((char*)(long)x)[off_bytevector_data+len] = 0;
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if (c == 'l') {
    int len = (unsigned char) fasl_read_byte(p);
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    int i; ikptr pt = pair;
    for (i=0; i<len; i++) {
      IK_REF(pt, off_car) = do_read(pcb, p);
      IK_REF(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    IK_REF(pt, off_car) = do_read(pcb, p);
    IK_REF(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if (c == 'L') {
    long len;
    fasl_read_buf(p, &len, sizeof(long));
    if (len < 0)
      ik_abort("invalid len=%ld", len);
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    long i; ikptr pt = pair;
    for (i=0; i<len; i++) {
      IK_REF(pt, off_car) = do_read(pcb, p);
      IK_REF(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    IK_REF(pt, off_car) = do_read(pcb, p);
    IK_REF(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if (c == 'f') {
    ikptr x = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;
    IK_REF(x, -vector_tag) = flonum_tag;
    fasl_read_buf(p, (void*)(long)(x+disp_flonum_data-vector_tag), 8);
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if (c == 'C') {
    int n;
    fasl_read_buf(p, &n, sizeof(int));
    return IK_CHAR_FROM_INTEGER(n);
  }
  else if (c == 'b') {
    long len;
    long sign = 0;
    fasl_read_buf(p, &len, sizeof(long));
    if (len < 0) {
      sign = 1;
      len = -len;
    }
    if (len & 3)
      ik_abort("error in fasl-read: invalid bignum length %ld", len);
    ik_ulong tag = bignum_tag | (sign << bignum_sign_shift) |
      ((len >> 2) << bignum_nlimbs_shift);
    ikptr x = ik_unsafe_alloc(pcb, IK_ALIGN(len + disp_bignum_data)) | vector_tag;
    IK_REF(x, -vector_tag) = (ikptr) tag;
    fasl_read_buf(p, (void*)(long)(x+off_bignum_data), len);
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if (c == 'i') {
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
    return x;
  } else {
    ik_abort("invalid type '%c' (0x%02x) found in fasl file", c, c);
    return IK_VOID_OBJECT;
  }
}


static ikptr
ik_fasl_read (ikpcb* pcb, fasl_port* p)
{
  /* first check the header */
  char buf[IK_FASL_HEADER_LEN];
  fasl_read_buf(p, buf, IK_FASL_HEADER_LEN);
  if (0 != strncmp(buf, IK_FASL_HEADER, IK_FASL_HEADER_LEN))
    ik_abort("invalid fasl header");
  return do_read(pcb, p);
}

/* end of file */
