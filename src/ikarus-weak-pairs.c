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

#include "internals.h"

ikptr
ikrt_weak_cons (ikptr a, ikptr d, ikpcb* pcb)
{
  ikptr ap  = pcb->weak_pairs_ap;
  ikptr nap = ap + pair_size;
  ikptr p;
  if (nap > pcb->weak_pairs_ep) {
    /* There is  NOT enough room  for a new  pair object in  the current
       page storage  for weak pairs.   Allocate a new  page (registering
       its use destination  in the segments vector) and  use it.  Notice
       that the old page is already referenced by a slot in the segments
       vector. */
    ikptr mem = ik_mmap_typed(IK_PAGESIZE, weak_pairs_mt, pcb);
    pcb->weak_pairs_ap = mem + pair_size;
    pcb->weak_pairs_ep = mem + IK_PAGESIZE;
    p = mem | pair_tag;
  } else {
    /* There is  enough room for a  new pair object in  the current page
       storage for weak pairs. */
    pcb->weak_pairs_ap = nap;
    p = ap | pair_tag;
  }
  /* There is no  need to update the dirty vector  aboud "p" because the
     values are older. */
  IK_CAR(p) = a;
  IK_CDR(p) = d;
  return p;
}
ikptr
ikrt_is_weak_pair (ikptr x, ikpcb* pcb)
{
  if (IK_TAGOF(x) != pair_tag)
    return IK_FALSE_OBJECT;
  else {
    uint32_t tag = pcb->segment_vector[IK_PAGE_INDEX(x)];
    return ((tag & type_mask) == weak_pairs_type)? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
  }
}

/* end of file */
