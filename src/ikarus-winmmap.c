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

#ifdef __CYGWIN__


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "ikarus.h"
#include <sys/mman.h>
#include <stdio.h>
#include <strings.h>
#include <errno.h>
#include <assert.h>


/** --------------------------------------------------------------------
 ** Internal definitions.
 ** ----------------------------------------------------------------- */

#define pagesize 4096
#define pageshift 12
#define segment_size (16*pagesize)
#define segment_shift (4+pageshift)

static unsigned short* table = 0;
static char* ap = 0;
static size_t as = 0;


static void*
do_mmap(size_t n){
  void* x = mmap(0, n, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
  if(x == (void*)-1){
    fprintf(stderr, "failed to mmap: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }
  assert((((unsigned int)x) & (segment_size-1)) == 0);
  return x;
}

static void
do_munmap(void* addr, size_t size){
  int err = munmap(addr, size);
  if(err){
   fprintf(stderr, "failed to unmap\n");
   exit(EXIT_FAILURE);
  }
}


static void
init_table(){
  assert(sizeof(unsigned short) == 2);
  table = do_mmap(32*pagesize);
  bzero(table, 32*pagesize);
}

void
win_munmap(char* addr, size_t size){
  while(size){
    unsigned int page = (((unsigned int) addr) >> pageshift);
    unsigned int segment_index = page / 16;
    unsigned int page_index = page & 15;
    unsigned short alloc_bits = table[segment_index];
    assert((alloc_bits & (1<<page_index)) == (1<<page_index));
    unsigned int new_bits = alloc_bits & ~ (1 << page_index);
    table[segment_index] = new_bits;
    if(new_bits == 0){
      do_munmap((void*)(segment_index*segment_size), segment_size);
    }
    size -= pagesize;
    addr += pagesize;
  }
}


char*
win_mmap(size_t size){
  if(size <= as){
    char* x = ap;
    ap += size;
    as -= size;
    return x;
  }
  if(table == 0) init_table();
  win_munmap(ap, as);
  size_t segments = ((size+segment_size-1) >> segment_shift);
  size_t aligned_size = segments << segment_shift;
  char* addr = do_mmap(aligned_size);
  unsigned int page = (((unsigned int) addr) >> pageshift);
  unsigned int segment_index = page / 16;
  int i;
  for(i=0; i<segments; i++){
    table[segment_index+i] = -1;
  }
  ap = addr + size;
  as = aligned_size - size;
  return addr;
}

#endif

/* end of file */
