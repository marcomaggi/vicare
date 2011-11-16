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

/* end of file */
