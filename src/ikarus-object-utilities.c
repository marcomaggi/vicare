/*
  Part of: Vicare
  Contents: utilities for built in object manipulation
  Date: Tue Nov  8, 2011

  Abstract



  Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "ikarus.h"


/** --------------------------------------------------------------------
 ** Scheme pairs utilities.
 ** ----------------------------------------------------------------- */

int
ik_list_length (ikptr x)
{
  int   n;
  for (n = 0; pair_tag == tagof(x); ++n) {
    if (INT_MAX == n) {
      fprintf(stderr, "Vicare error: size of list exceeds INT_MAX");
      exit(EXIT_FAILURE);
    } else {
      x = ref(x, off_cdr);
    }
  }
  return n;
}
void
ik_list_to_argv (ikptr x, char **argv)
/* Given a reference to a list of bytevectors, fill "argv" with pointers
   to the data areas, setting the last element to NULL. */
{
  int    i;
  ikptr  bv;
  for (i=0; pair_tag == tagof(x); x=ref(x, off_cdr), ++i) {
    bv      = ref(x, off_car);
    argv[i] = (char*)(long)(bv + off_bytevector_data);
  }
  argv[i] = NULL;
}
char**
ik_list_to_vec (ikptr x)
{
  int n = ik_list_length(x);
  char** vec = malloc((n+1) * sizeof(char*));
  if (vec == NULL)
    exit(EXIT_FAILURE);
  int i;
  for (i=0; i<n; i++) {
    vec[i] = (char*)(long)ref(x, off_car) + off_bytevector_data;
    x = ref(x, off_cdr);
  }
  vec[n] = 0;
  return vec;
}


/** --------------------------------------------------------------------
 ** Scheme bytevector utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_bytevector_alloc (ikpcb * pcb, long int requested_number_of_bytes)
{
  long int  aligned_size;
  ikptr     bv;
  char *    data;
  aligned_size = align(disp_bytevector_data
                       + requested_number_of_bytes
                       + 1);
  bv           = ik_safe_alloc(pcb, aligned_size)
                 + bytevector_tag;
  ref(bv, off_bytevector_length) = fix(requested_number_of_bytes);
  data = (char *)(long)(bv + off_bytevector_data);
  data[requested_number_of_bytes] = '\0';
  return bv;
}


/** --------------------------------------------------------------------
 ** Scheme vector utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_vector_alloc (ikpcb * pcb, long int requested_number_of_items)
{
  long int  aligned_size;
  ikptr     vec;
  aligned_size = align(disp_vector_data + requested_number_of_items * wordsize);
  vec          = ik_safe_alloc(pcb, aligned_size) + vector_tag;
  ref(vec, off_vector_length) = fix(requested_number_of_items);
  return vec;
}


/** --------------------------------------------------------------------
 ** Scheme struct utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_struct_alloc (ikpcb * pcb, ikptr rtd, long int number_of_fields)
{
  long  aligned_size = align(disp_record_data + number_of_fields * wordsize);
  ikptr data         = ik_safe_alloc(pcb, aligned_size) + vector_tag;
  ref(data, off_record_rtd) = rtd;
  return data;
}


/** --------------------------------------------------------------------
 ** Specific data utilities.
 ** ----------------------------------------------------------------- */

ikptr
ik_hostent_to_struct (ikptr rtd, struct hostent * src, ikpcb * pcb)
/* Makes use of "pcb->root1" only,  so that "pcb->root0" is available to
   the caller. */
{
  ikptr dst = ik_struct_alloc(pcb, rtd, 6);
  pcb->root1 = &dst;
#if 0
  ref(dst, off_record_data+0*wordsize) = false_object;
  ref(dst, off_record_data+1*wordsize) = false_object;
  ref(dst, off_record_data+2*wordsize) = false_object;
  ref(dst, off_record_data+3*wordsize) = false_object;
#else
  { /* store the official host name */
    long        name_len = strlen(src->h_name);
    ikptr       name_bv  = ik_bytevector_alloc(pcb, name_len);
    char *      name     = VICARE_BYTEVECTOR_DATA_CHARP(name_bv);
    memcpy(name, src->h_name, name_len+1);
    VICARE_STRUCT_SET(dst, 0, name_bv);
  }
  { /* store the list of aliases */
    ikptr       list_of_aliases = null_object;
    int         i;
    ref(dst, off_record_data+1*wordsize) = list_of_aliases;
    for (i=0; NULL!=src->h_aliases[i]; ++i) {
      ikptr     pair      = ik_pair_alloc(pcb);
      ref(pair, off_cdr)  = list_of_aliases;
      ref(dst, off_record_data+1*wordsize) = list_of_aliases = pair;
      long      alias_len = strlen(src->h_aliases[i]);
      ikptr     alias_bv  = ik_bytevector_alloc(pcb, alias_len);
      char *    alias     = VICARE_BYTEVECTOR_DATA_CHARP(alias_bv);
      memcpy(alias, src->h_aliases[i], alias_len);
      ref(pair, off_car) = alias_bv;
    }
  }
  { /* store the host address type */
    ref(dst, off_record_data+2*wordsize) = fix(src->h_addrtype);
  }
  { /* store the host address structure length */
    ref(dst, off_record_data+3*wordsize) = fix(src->h_length);
  }
  ikptr first_addr = false_object;
  { /* store the reversed list of addresses */
    ikptr       list_of_addrs = null_object;
    int         i;
    ref(dst, off_record_data+4*wordsize) = list_of_addrs;
    for (i=0; NULL!=src->h_addr_list[i]; ++i) {
      ikptr     pair     = ik_pair_alloc(pcb);
      ref(pair, off_cdr) = list_of_addrs;
      ref(dst, off_record_data+4*wordsize) = list_of_addrs = pair;
      ikptr     addr_bv  = ik_bytevector_alloc(pcb, src->h_length);
      char *    addr     = VICARE_BYTEVECTOR_DATA_CHARP(addr_bv);
      memcpy(addr, src->h_addr_list[i], src->h_length);
      ref(pair, off_car) = addr_bv;
      if (0 == i)
        first_addr = addr_bv;
    }
  }
  { /* store the first in the list of addresses */
    ref(dst, off_record_data+5*wordsize) = first_addr;
  }
#endif
  pcb->root1 = NULL;
  return dst;
}

/* end of file */
