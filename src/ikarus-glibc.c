/*
  Part of: Vicare
  Contents: interface to GNU C Library functions
  Date: Wed Nov  9, 2011

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
#include <dirent.h>
#include <unistd.h>
#include <net/if.h>
#include <sys/types.h>

static VICARE_UNUSED void
feature_failure_ (const char * funcname)
{
  fprintf(stderr, "Vicare error: called GNU C Library specific function, %s\n", funcname);
  exit(EXIT_FAILURE);
}

#define feature_failure(FN)     { feature_failure_(FN); return void_object; }


/** --------------------------------------------------------------------
 ** Operative system environment variables.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_clearenv (void)
{
#ifdef HAVE_CLEARENV
  clearenv();
  return void_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Inspecting file system directories.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_dirfd (ikptr pointer)
{
#ifdef HAVE_DIRFD
  DIR *  stream = (DIR *)ref(pointer, off_pointer_data);
  int    rv;
  errno = 0;
  rv    = dirfd(stream);
  if (-1 == rv)
    return ik_errno_to_code();
  else
    return fix(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Temporary files and directories.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_mkstemp (ikptr template_bv, ikpcb * pcb)
{
#ifdef HAVE_MKSTEMP
  char *        template;
  int           rv;
  template = VICARE_BYTEVECTOR_DATA_CHARP(template_bv);
  errno    = 0;
  rv       = mkstemp(template);
  if (-1 == rv)
    return ik_errno_to_code();
  else
    return fix(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_mkdtemp (ikptr template_bv, ikpcb * pcb)
{
#ifdef HAVE_MKDTEMP
  char *        template;
  char *        rv;
  template = VICARE_BYTEVECTOR_DATA_CHARP(template_bv);
  errno    = 0;
  rv       = mkdtemp(template);
  if (NULL == rv)
    return ik_errno_to_code();
  else
    return template_bv;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** File system synchronisation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_sync (void)
{
#ifdef HAVE_SYNC
  /* On Linux there  is no return value, despite what  the GNU C Library
     documentation states. */
  sync();
  return fix(0);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_fsync (ikptr fd)
{
#ifdef HAVE_FSYNC
  int   rv;
  errno = 0;
  rv    = fsync(unfix(fd));
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_fdatasync (ikptr fd)
{
#ifdef HAVE_FDATASYNC
  int   rv;
  errno = 0;
  rv    = fdatasync(unfix(fd));
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Sockets.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_if_nametoindex (ikptr name_bv)
{
  char *        name;
  unsigned int  rv;
  name  = VICARE_BYTEVECTOR_DATA_CHARP(name_bv);
  rv    = if_nametoindex(name);
  if (0 == rv)
    return false_object;
  else
    return fix((long)rv);
}
ikptr
ikrt_glibc_if_indextoname (ikptr index, ikpcb * pcb)
{
  char          buffer[1+IFNAMSIZ];
  unsigned      i = (unsigned)unfix(index);
  char *        rv;
  rv = if_indextoname(i, buffer);
  if (NULL == rv) {
    return false_object;
  } else {
    long        len  = strlen(buffer);
    ikptr       bv   = ik_bytevector_alloc(pcb, len);
    char *      data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
    memcpy(data, buffer, len+1);
    return bv;
  }
}
ikptr
ikrt_glibc_if_nameindex (ikpcb * pcb) {
  struct if_nameindex * arry;           /* the list of interfaces */
  ikptr         alist = null_object;    /* the result alist */
  ikptr         spine;                  /* the next pair to prepend to the alist */
  ikptr         entry;                  /* the next alist entry */
  long          len;                    /* the length of interface name */
  ikptr         bv;                     /* bytevector holding the interface name */
  char *        data;                   /* data area in the bytevector */
  int           i;
  pcb->root0 = &alist;
  arry = if_nameindex();
  for (i=0; 0 != arry[i].if_index; ++i) {
    /* Add a pair to the alist spine. */
    spine = ik_safe_alloc(pcb, pair_size) + pair_tag;
    ref(spine, off_cdr) = alist;
    alist               = spine;
    /* Add an entry to the alist. */
    entry = ik_safe_alloc(pcb, pair_size) + pair_tag;
    ref(spine, off_car) = entry;
    /* Fill the entry. */
    len  = strlen(arry[i].if_name);
    bv   = ik_bytevector_alloc(pcb, len);
    data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
    memcpy(data, arry[i].if_name, len);
    ref(entry, off_car) = fix(arry[i].if_index);
    ref(entry, off_cdr) = bv;
  }
  if_freenameindex(arry);
  pcb->root0 = NULL;
  return alist;
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */



/* end of file */
