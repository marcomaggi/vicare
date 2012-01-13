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
  mapsize	= ((filesize + pagesize - 1) / pagesize) * pagesize;
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
    if (val != void_object)
      ik_print(val);
  }
  if (p.memp != p.memq)
    ik_abort("fasl-read did not reach EOF");
}

static ikptr
alloc_code (long int size, ikpcb* pcb, fasl_port* p)
{
  long int asize = IK_ALIGN(size);
  ikptr ap = p->code_ap;
  ikptr nap = ap + asize;
  if (nap <= p->code_ep) {
    p->code_ap = nap;
    return ap;
  } else if (asize < pagesize) {
    ikptr mem = ik_mmap_code(pagesize, 0, pcb);
    long int bytes_remaining = pagesize - asize;
    long int previous_bytes =
      ((unsigned long int)p->code_ep) - ((unsigned long int)ap);
    if (bytes_remaining <= previous_bytes) {
      return mem;
    } else {
      p->code_ap = mem+asize;
      p->code_ep = mem+pagesize;
      return mem;
    }
  } else {
    long int asize = IK_ALIGN_TO_NEXT_PAGE(size);
    ikptr mem = ik_mmap_code(asize, 0, pcb);
    return mem;
  }
}
void
ik_relocate_code (ikptr code)
{
  ikptr vec = ref(code, disp_code_reloc_vector);
  ikptr size = ref(vec, off_vector_length);
  ikptr data = code + disp_code_data;
  ikptr p = vec + off_vector_data;
  ikptr q = p + size;
  while(p < q) {
    long	r = unfix(ref(p, 0));
    if (0 == r)
      ik_abort("unset reloc!");
    long int tag = r & 3;
    long int code_off = r >> 2;
    if (tag == 0) {
      /* vanilla object */
      ref(data, code_off) = ref(p, wordsize);
      p += (2*wordsize);
    }
    else if (tag == 2) {
      /* displaced object */
      long int obj_off = unfix(ref(p, wordsize));
      ikptr obj = ref(p, 2*wordsize);
      ref(data, code_off) = obj + obj_off;
      p += (3*wordsize);
    }
    else if (tag == 3) {
      /* jump label */
      long int obj_off = unfix(ref(p, wordsize));
      long int obj = ref(p, 2*wordsize);
      long int displaced_object = obj + obj_off;
      long int next_word = data + code_off + 4;
      long int relative_distance = displaced_object - next_word;
#if 0
      if (wordsize == 8) {
        relative_distance += 4;
      }
#endif
      *((int*)(data+code_off)) = relative_distance;
      //      ref(next_word, -wordsize) = relative_distance;
      p += (3*wordsize);
    }
    else if (tag == 1) {
      /* foreign object */
      ikptr str = ref(p, wordsize);
      char* name = NULL;
      if (IK_TAGOF(str) == bytevector_tag) {
        name = (char*)(long) str + off_bytevector_data;
      } else
        ik_abort("foreign name is not a bytevector");
      dlerror();
      void* sym = dlsym(RTLD_DEFAULT, name);
      char* err = dlerror();
      if (err)
        ik_abort("failed to find foreign name %s: %s", name, err);
      ref(data,code_off) = (ikptr)sym;
      p += (2*wordsize);
    } else
      ik_abort("invalid reloc 0x%016lx (tag=%ld)", r, tag);
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
      p->marks = (ikptr*)(long)ik_mmap(2*pagesize*sizeof(ikptr*));
      bzero(p->marks, 2*pagesize*sizeof(ikptr*));
      p->marks_size = 2*pagesize;
    }
  }
  if (c == 'x') {
    long int code_size;
    ikptr freevars;
    fasl_read_buf(p, &code_size, sizeof(long int));
    fasl_read_buf(p, &freevars, sizeof(ikptr));
    ikptr annotation = do_read(pcb, p);
    ikptr code = alloc_code(IK_ALIGN(code_size+disp_code_data), pcb, p);
    ref(code, 0) = code_tag;
    ref(code, disp_code_code_size) = fix(code_size);
    ref(code, disp_code_freevars) = freevars;
    ref(code, disp_code_annotation) = annotation;
    fasl_read_buf(p, (void*)(disp_code_data+(long)code), code_size);
    if (put_mark_index) {
      p->marks[put_mark_index] = code+vector_tag;
    }
    ref(code, disp_code_reloc_vector) = do_read(pcb, p);
    ik_relocate_code(code);
    return code+vector_tag;
  }
  else if (c == 'P') {
    ikptr pair = ik_unsafe_alloc(pcb, pair_size) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    ref(pair, off_car) = do_read(pcb, p);
    ref(pair, off_cdr) = do_read(pcb, p);
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
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = IK_ALIGN(len*string_char_size + disp_string_data);
    ikptr str = ik_unsafe_alloc(pcb, size) | string_tag;
    ref(str, off_string_length) = fix(len);
    fasl_read_buf(p, (char*)(long)str+off_string_data, len);
    {
      unsigned char* pi = (unsigned char*)(long)(str+off_string_data);
      ikchar* pj = (ikchar*)(long)(str+off_string_data);
      long int i = len-1;
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
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = IK_ALIGN(len*string_char_size + disp_string_data);
    ikptr str = ik_unsafe_alloc(pcb, size) | string_tag;
    ref(str, off_string_length) = fix(len);
    long int i;
    for (i=0; i<len; i++) {
      ikchar c;
      fasl_read_buf(p, &c, sizeof(ikchar));
      IK_CHAR32(str, i) = IK_CHAR32_FROM_INTEGER(c);
    }
    //str[off_string_data+len*string_char_size] = 0;
    if (put_mark_index) {
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if (c == 'V') {
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = IK_ALIGN(len * wordsize + disp_vector_data);
    ikptr vec = ik_unsafe_alloc(pcb, size) | vector_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = vec;
    }
    ref(vec, off_vector_length) = fix(len);
    long int i;
    for (i=0; i<len; i++) {
      ref(vec, off_vector_data + i*wordsize) = do_read(pcb, p);
    }
    return vec;
  }
  else if (c == 'I') {
    ikptr fixn;
    fasl_read_buf(p, &fixn, sizeof(ikptr));
    return fixn;
  }
  else if (c == 'F') {
    return false_object;
  }
  else if (c == 'T') {
    return true_object;
  }
  else if (c == 'N') {
    return null_object;
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
    long int i, n;
    fasl_read_buf(p, &n, sizeof(long int));
    ikptr fields;
    if (n == 0) {
      fields = null_object;
    } else {
      fields = ik_unsafe_alloc(pcb, n * IK_ALIGN(pair_size)) | pair_tag;
      ikptr ptr = fields;
      for (i=0; i<n; i++) {
        ref(ptr, off_car) = do_read(pcb, p);
        ref(ptr, off_cdr) = ptr + IK_ALIGN(pair_size);
        ptr += IK_ALIGN(pair_size);
      }
      ptr -= pair_size;
      ref(ptr, off_cdr) = null_object;
    }
    ikptr gensym_val = ref(symb, off_symbol_record_value);
    ikptr rtd;
    if (gensym_val == unbound_object) {
      rtd = ik_unsafe_alloc(pcb, IK_ALIGN(rtd_size)) | vector_tag;
      ikptr base_rtd = pcb->base_rtd;
      ref(rtd, off_rtd_rtd) = base_rtd;
      ref(rtd, off_rtd_name) = name;
      ref(rtd, off_rtd_length) = fix(n);
      ref(rtd, off_rtd_fields) = fields;
      ref(rtd, off_rtd_printer) = false_object;
      ref(rtd, off_rtd_symbol) = symb;
      ref(symb, off_symbol_record_value) = rtd;
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
    ref(proc, -closure_tag) = code + off_code_data;
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
      return void_object;
    }
  }
  else if (c == 'v') {
    /* bytevector */
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = IK_ALIGN(len + disp_bytevector_data + 1);
    ikptr x = ik_unsafe_alloc(pcb, size) | bytevector_tag;
    ref(x, off_bytevector_length) = fix(len);
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
      ref(pt, off_car) = do_read(pcb, p);
      ref(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    ref(pt, off_car) = do_read(pcb, p);
    ref(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if (c == 'L') {
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    if (len < 0)
      ik_abort("invalid len=%ld", len);
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) | pair_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = pair;
    }
    long int i; ikptr pt = pair;
    for (i=0; i<len; i++) {
      ref(pt, off_car) = do_read(pcb, p);
      ref(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    ref(pt, off_car) = do_read(pcb, p);
    ref(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if (c == 'f') {
    ikptr x = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;
    ref(x, -vector_tag) = flonum_tag;
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
    long int len;
    long int sign = 0;
    fasl_read_buf(p, &len, sizeof(long int));
    if (len < 0) {
      sign = 1;
      len = -len;
    }
    if (len & 3)
      ik_abort("error in fasl-read: invalid bignum length %ld", len);
    unsigned long int tag = bignum_tag | (sign << bignum_sign_shift) |
      ((len >> 2) << bignum_length_shift);
    ikptr x = ik_unsafe_alloc(pcb, IK_ALIGN(len + disp_bignum_data)) | vector_tag;
    ref(x, -vector_tag) = (ikptr) tag;
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
	&& (ref(real, -vector_tag) == flonum_tag)) {
      x = ik_unsafe_alloc(pcb, cflonum_size);
      ref(x, 0) = cflonum_tag;;
      ref(x, disp_cflonum_real) = real;
      ref(x, disp_cflonum_imag) = imag;
    } else {
      x = ik_unsafe_alloc(pcb, compnum_size);
      ref(x, 0) = compnum_tag;
      ref(x, disp_compnum_real) = real;
      ref(x, disp_compnum_imag) = imag;
    }
    x += vector_tag;
    if (put_mark_index) {
      p->marks[put_mark_index] = x;
    }
    return x;
  } else {
    ik_abort("invalid type '%c' (0x%02x) found in fasl file", c, c);
    return void_object;
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
