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
#include <sys/types.h>

static void
feature_failure (const char * funcname)
{
  fprintf(stderr, "Vicare error: called GNU C Library specific function, %s\n", funcname);
  exit(EXIT_FAILURE);
}


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

/* end of file */
