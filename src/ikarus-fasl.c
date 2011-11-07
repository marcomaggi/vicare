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




#include "ikarus-data.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <sys/mman.h>
#include <dlfcn.h>


#ifndef RTLD_DEFAULT
#define RTLD_DEFAULT 0
#endif


typedef struct {
  char* membase;
  char* memp;
  char* memq;
  ikptr code_ap;
  ikptr code_ep;
  ikptr* marks;
  int marks_size;
} fasl_port;

static ikptr ik_fasl_read(ikpcb* pcb, fasl_port* p);

void ik_fasl_load(ikpcb* pcb, char* fasl_file){
  int fd = open(fasl_file, O_RDONLY);
  if(fd == -1){
    fprintf(stderr,
            "ikarus: failed to open boot file \"%s\": %s\n",
            fasl_file,
            strerror(errno));
    ikarus_usage_short();
    exit(EXIT_FAILURE);
  }
  int filesize;
  {
    struct stat buf;
    int err = fstat(fd, &buf);
    if(err != 0){
      fprintf(stderr,
              "ikarus: failed to stat \"%s\": %s\n",
              fasl_file,
              strerror(errno));
      exit(EXIT_FAILURE);
    }
    filesize = buf.st_size;
  }
  int mapsize = ((filesize + pagesize - 1) / pagesize) * pagesize;
  char* mem = mmap(
      0,
      mapsize,
      PROT_READ,
      MAP_PRIVATE,
      fd,
      0);
  if(mem == MAP_FAILED){
    fprintf(stderr,
            "ikarus: mapping failed for %s: %s\n",
            fasl_file,
            strerror(errno));
    exit(EXIT_FAILURE);
  }
  fasl_port p;
  p.membase = mem;
  p.memp = mem;
  p.memq = mem + filesize;
  p.marks = 0;
  p.marks_size = 0;
  while(p.memp < p.memq){
    p.code_ap = 0;
    p.code_ep = 0;
    ikptr v = ik_fasl_read(pcb, &p);
    if(p.marks_size){
      ik_munmap((ikptr)(long)p.marks, p.marks_size*sizeof(ikptr*));
      p.marks = 0;
      p.marks_size = 0;
    }
    if(p.memp == p.memq){
      int err = munmap(mem, mapsize);
      if(err != 0){
        fprintf(stderr, "Failed to unmap fasl file: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
      }
      close(fd);
    }
    ikptr val = ik_exec_code(pcb, v, 0, 0);
    if(val != void_object){
      ik_print(val);
    }
  }
  if(p.memp != p.memq){
    fprintf(stderr, "fasl-read did not reach eof!\n");
    exit(-10);
  }
}

static ikptr
alloc_code(long int size, ikpcb* pcb, fasl_port* p){
  long int asize = align(size);
  ikptr ap = p->code_ap;
  ikptr nap = ap + asize;
  if(nap <= p->code_ep){
    p->code_ap = nap;
    return ap;
  } else if (asize < pagesize){
    ikptr mem = ik_mmap_code(pagesize, 0, pcb);
    long int bytes_remaining = pagesize - asize;
    long int previous_bytes =
      ((unsigned long int)p->code_ep) - ((unsigned long int)ap);
    if(bytes_remaining <= previous_bytes){
      return mem;
    } else {
      p->code_ap = mem+asize;
      p->code_ep = mem+pagesize;
      return mem;
    }
  } else {
    long int asize = align_to_next_page(size);
    ikptr mem = ik_mmap_code(asize, 0, pcb);
    return mem;
  }
}


void
ik_relocate_code(ikptr code){
  ikptr vec = ref(code, disp_code_reloc_vector);
  ikptr size = ref(vec, off_vector_length);
  ikptr data = code + disp_code_data;
  ikptr p = vec + off_vector_data;
  ikptr q = p + size;
  while(p < q){
    long int r = unfix(ref(p, 0));
    if(r == 0){
      fprintf(stderr, "unset reloc!\n");
      exit(EXIT_FAILURE);
    }
    long int tag = r & 3;
    long int code_off = r >> 2;
    if(tag == 0){
      /* vanilla object */
      ref(data, code_off) = ref(p, wordsize);
      p += (2*wordsize);
    }
    else if(tag == 2){
      /* displaced object */
      long int obj_off = unfix(ref(p, wordsize));
      ikptr obj = ref(p, 2*wordsize);
      ref(data, code_off) = obj + obj_off;
      p += (3*wordsize);
    }
    else if(tag == 3){
      /* jump label */
      long int obj_off = unfix(ref(p, wordsize));
      long int obj = ref(p, 2*wordsize);
      long int displaced_object = obj + obj_off;
      long int next_word = data + code_off + 4;
      long int relative_distance = displaced_object - next_word;
#if 0
      if(wordsize == 8){
        relative_distance += 4;
      }
#endif
      *((int*)(data+code_off)) = relative_distance;
//      ref(next_word, -wordsize) = relative_distance;
      p += (3*wordsize);
    }
    else if(tag == 1){
      /* foreign object */
      ikptr str = ref(p, wordsize);
      char* name;
      if(tagof(str) == bytevector_tag){
        name = (char*)(long) str + off_bytevector_data;
      } else {
        fprintf(stderr, "foreign name is not a bytevector\n");
        exit(EXIT_FAILURE);
      }
      dlerror();
      void* sym = dlsym(RTLD_DEFAULT, name);
      char* err = dlerror();
      if(err){
        fprintf(stderr, "failed to find foreign name %s: %s\n", name, err);
        exit(EXIT_FAILURE);
      }
      ref(data,code_off) = (ikptr)sym;
      p += (2*wordsize);
    }
    else {
      fprintf(stderr, "invalid reloc 0x%016lx (tag=%ld)\n", r, tag);
      exit(EXIT_FAILURE);
    }
  }
}


static char fasl_read_byte(fasl_port* p){
  if(p->memp < p->memq){
    char c = *(p->memp);
    p->memp++;
    return c;
  } else {
    fprintf(stderr, "fasl_read_byte: read beyond eof\n");
    exit(EXIT_FAILURE);
  }
}

static void fasl_read_buf(fasl_port* p, void* buf, int n){
  if((p->memp+n) <= p->memq){
    memcpy(buf, p->memp, n);
    p->memp += n;
  } else {
    fprintf(stderr, "fasl_read_buf: read beyond eof\n");
    exit(EXIT_FAILURE);
  }
}
typedef struct{
  int code_size;
  int reloc_size;
  ikptr closure_size;
} code_header;



static ikptr do_read(ikpcb* pcb, fasl_port* p){
  char c = fasl_read_byte(p);
  int put_mark_index = 0;
  if(c == '>'){
    int idx = 0;
    fasl_read_buf(p, &idx, sizeof(int));
    put_mark_index = idx;
    c = fasl_read_byte(p);
    if(idx <= 0){
      fprintf(stderr, "fasl_read: invalid index %d\n", idx);
      exit(EXIT_FAILURE);
    }
    if(p->marks){
      if(idx >= p->marks_size){
        fprintf(stderr, "BUG: mark too big: %d\n", idx);
        exit(EXIT_FAILURE);
      }
      if(idx < p->marks_size){
        if(p->marks[idx] != 0){
          fprintf(stderr, "mark %d already set\n", idx);
          ik_print(p->marks[idx]);
          exit(EXIT_FAILURE);
        }
      }
    }
    else {
      /* allocate marks */
      p->marks = (ikptr*)(long)ik_mmap(2*pagesize*sizeof(ikptr*));
      bzero(p->marks, 2*pagesize*sizeof(ikptr*));
      p->marks_size = 2*pagesize;
    }
  }
  if(c == 'x'){
    long int code_size;
    ikptr freevars;
    fasl_read_buf(p, &code_size, sizeof(long int));
    fasl_read_buf(p, &freevars, sizeof(ikptr));
    ikptr annotation = do_read(pcb, p);
    ikptr code = alloc_code(align(code_size+disp_code_data), pcb, p);
    ref(code, 0) = code_tag;
    ref(code, disp_code_code_size) = fix(code_size);
    ref(code, disp_code_freevars) = freevars;
    ref(code, disp_code_annotation) = annotation;
    fasl_read_buf(p, (void*)(disp_code_data+(long)code), code_size);
    if(put_mark_index){
      p->marks[put_mark_index] = code+vector_tag;
    }
    ref(code, disp_code_reloc_vector) = do_read(pcb, p);
    ik_relocate_code(code);
    return code+vector_tag;
  }
  else if(c == 'P'){
    ikptr pair = ik_unsafe_alloc(pcb, pair_size) + pair_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = pair;
    }
    ref(pair, off_car) = do_read(pcb, p);
    ref(pair, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if(c == 'M'){
    /* symbol */
    ikptr str = do_read(pcb, p);
    ikptr sym = ikrt_string_to_symbol(str, pcb);
    if(put_mark_index){
      p->marks[put_mark_index] = sym;
    }
    return sym;
  }
  else if(c == 's'){
    /* ascii string */
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = align(len*string_char_size + disp_string_data);
    ikptr str = ik_unsafe_alloc(pcb, size) + string_tag;
    ref(str, off_string_length) = fix(len);
    fasl_read_buf(p, (char*)(long)str+off_string_data, len);
    {
      unsigned char* pi = (unsigned char*)(long)(str+off_string_data);
      ikchar* pj = (ikchar*)(long)(str+off_string_data);
      long int i = len-1;
      for(i=len-1; i >= 0; i--){
        pj[i] = integer_to_char(pi[i]);
      }
    }
    //str[off_string_data+len] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if(c == 'S'){
    /* string */
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = align(len*string_char_size + disp_string_data);
    ikptr str = ik_unsafe_alloc(pcb, size) + string_tag;
    ref(str, off_string_length) = fix(len);
    long int i;
    for(i=0; i<len; i++){
      ikchar c;
      fasl_read_buf(p, &c, sizeof(ikchar));
      string_set(str, i, integer_to_char(c));
    }
    //str[off_string_data+len*string_char_size] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if(c == 'V'){
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = align(len * wordsize + disp_vector_data);
    ikptr vec = ik_unsafe_alloc(pcb, size) + vector_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = vec;
    }
    ref(vec, off_vector_length) = fix(len);
    long int i;
    for(i=0; i<len; i++){
      ref(vec, off_vector_data + i*wordsize) = do_read(pcb, p);
    }
    return vec;
  }
  else if(c == 'I'){
    ikptr fixn;
    fasl_read_buf(p, &fixn, sizeof(ikptr));
    return fixn;
  }
  else if(c == 'F'){
    return false_object;
  }
  else if(c == 'T'){
    return true_object;
  }
  else if(c == 'N'){
    return null_object;
  }
  else if(c == 'c'){
    /* FIXME: sounds broken */
    unsigned char x = (unsigned char) fasl_read_byte(p);
    return int_to_scheme_char(x);
  }
  else if(c == 'G'){
    /* G is for gensym */
    ikptr pretty = do_read(pcb, p);
    ikptr unique = do_read(pcb, p);
    ikptr sym = ikrt_strings_to_gensym(pretty, unique, pcb);
    if(put_mark_index){
      p->marks[put_mark_index] = sym;
    }
    return sym;
  }
  else if(c == 'R'){ /* R is for RTD */
    ikptr name = do_read(pcb, p);
    ikptr symb = do_read(pcb, p);
    long int i, n;
    fasl_read_buf(p, &n, sizeof(long int));
    ikptr fields;
    if(n == 0){
      fields = null_object;
    } else {
      fields = ik_unsafe_alloc(pcb, n * align(pair_size)) + pair_tag;
      ikptr ptr = fields;
      for(i=0; i<n; i++){
        ref(ptr, off_car) = do_read(pcb, p);
        ref(ptr, off_cdr) = ptr + align(pair_size);
        ptr += align(pair_size);
      }
      ptr -= pair_size;
      ref(ptr, off_cdr) = null_object;
    }
    ikptr gensym_val = ref(symb, off_symbol_record_value);
    ikptr rtd;
    if(gensym_val == unbound_object){
      rtd = ik_unsafe_alloc(pcb, align(rtd_size)) + vector_tag;
      ikptr base_rtd = pcb->base_rtd;
      ref(rtd, off_rtd_rtd) = base_rtd;
      ref(rtd, off_rtd_name) = name;
      ref(rtd, off_rtd_length) = fix(n);
      ref(rtd, off_rtd_fields) = fields;
      ref(rtd, off_rtd_printer) = false_object;
      ref(rtd, off_rtd_symbol) = symb;
      ref(symb, off_symbol_record_value) = rtd;
      ((unsigned int*)(long)pcb->dirty_vector)[page_index(symb+off_symbol_record_value)] = -1;
    } else {
      rtd = gensym_val;
    }
    if(put_mark_index){
      p->marks[put_mark_index] = rtd;
    }
    return rtd;
  }
  else if(c == 'Q'){ /* thunk */
    ikptr proc = ik_unsafe_alloc(pcb, align(disp_closure_data)) + closure_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = proc;
    }
    ikptr code = do_read(pcb, p);
    ref(proc, -closure_tag) = code + off_code_data;
    return proc;
  }
  else if(c == '<'){
    int idx;
    fasl_read_buf(p, &idx, sizeof(int));
    if(idx <= 0){
      fprintf(stderr, "invalid index for ref %d\n", idx);
      exit(EXIT_FAILURE);
    }
    if(idx >= p->marks_size){
      fprintf(stderr, "invalid index for ref %d\n", idx);
      exit(EXIT_FAILURE);
    }
    ikptr obj = p->marks[idx];
    if(obj){
      return obj;
    } else {
      fprintf(stderr, "reference to uninitialized mark %d\n", idx);
      exit(EXIT_FAILURE);
    }
  }
  else if(c == 'v'){
    /* bytevector */
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    long int size = align(len + disp_bytevector_data + 1);
    ikptr x = ik_unsafe_alloc(pcb, size) + bytevector_tag;
    ref(x, off_bytevector_length) = fix(len);
    fasl_read_buf(p, (void*)(long)(x+off_bytevector_data), len);
    ((char*)(long)x)[off_bytevector_data+len] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if(c == 'l'){
    int len = (unsigned char) fasl_read_byte(p);
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) + pair_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = pair;
    }
    int i; ikptr pt = pair;
    for(i=0; i<len; i++){
      ref(pt, off_car) = do_read(pcb, p);
      ref(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    ref(pt, off_car) = do_read(pcb, p);
    ref(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if(c == 'L'){
    long int len;
    fasl_read_buf(p, &len, sizeof(long int));
    if(len < 0){
      fprintf(stderr, "invalid len=%ld\n", len);
      exit(EXIT_FAILURE);
    }
    ikptr pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) + pair_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = pair;
    }
    long int i; ikptr pt = pair;
    for(i=0; i<len; i++){
      ref(pt, off_car) = do_read(pcb, p);
      ref(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    ref(pt, off_car) = do_read(pcb, p);
    ref(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if(c == 'f'){
    ikptr x = ik_unsafe_alloc(pcb, flonum_size) + vector_tag;
    ref(x, -vector_tag) = flonum_tag;
    fasl_read_buf(p, (void*)(long)(x+disp_flonum_data-vector_tag), 8);
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if(c == 'C'){
    int n;
    fasl_read_buf(p, &n, sizeof(int));
    return int_to_scheme_char(n);
  }
  else if(c == 'b'){
    long int len;
    long int sign = 0;
    fasl_read_buf(p, &len, sizeof(long int));
    if(len < 0) {
      sign = 1;
      len = -len;
    }
    if(len & 3){
      fprintf(stderr, "Error in fasl-read: invalid bignum length %ld\n", len);
      exit(EXIT_FAILURE);
    }
    unsigned long int tag = bignum_tag | (sign << bignum_sign_shift) |
      ((len >> 2) << bignum_length_shift);
    ikptr x = ik_unsafe_alloc(pcb, align(len + disp_bignum_data)) + vector_tag;
    ref(x, -vector_tag) = (ikptr) tag;
    fasl_read_buf(p, (void*)(long)(x+off_bignum_data), len);
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if(c == 'i'){
    ikptr real = do_read(pcb, p);
    ikptr imag = do_read(pcb, p);
    ikptr x;
    if ((tagof(real) == vector_tag)
         && (ref(real, -vector_tag) == flonum_tag)){
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
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else {
    fprintf(stderr, "invalid type '%c' (0x%02x) found in fasl file\n", c, c);
    exit(EXIT_FAILURE);
  }
}


static ikptr ik_fasl_read(ikpcb* pcb, fasl_port* p){
  /* first check the header */
  char buf[IK_FASL_HEADER_LEN];
  fasl_read_buf(p, buf, IK_FASL_HEADER_LEN);
  if(strncmp(buf, IK_FASL_HEADER, IK_FASL_HEADER_LEN) != 0){
    fprintf(stderr, "invalid fasl header\n");
    exit(EXIT_FAILURE);
  }
  return do_read(pcb, p);
}
