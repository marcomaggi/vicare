/*
  Part of: Vicare
  Contents: interface to Linux functions
  Date: Mon Nov  7, 2011

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
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  fprintf(stderr, "Vicare error: called GNU+Linux specific function, %s\n", funcname);
  exit(EXIT_FAILURE);
}

#define feature_failure(FN)     { feature_failure_(FN); return void_object; }


/** --------------------------------------------------------------------
 ** Process exit status.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_linux_WIFCONTINUED (ikptr fx_status)
{
#ifdef HAVE_WIFCONTINUED
  int   status = unfix(fx_status);
  return (WIFCONTINUED(status))? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_linux_waitid (ikptr fx_idtype, ikptr fx_id, ikptr struct_info, ikptr fx_options)
{
#ifdef HAVE_WAITID
  idtype_t  idtype  = unfix(fx_idtype);
  id_t      id      = unfix(fx_id);
  siginfo_t info;
  int       options = unfix(fx_options);
  int       retval;
  errno  = 0;
  retval = waitid(idtype, id, &info, options);
  if (0 <= retval) {
    ref(struct_info, off_record_data+0*wordsize) = fix(info.si_pid);
    ref(struct_info, off_record_data+1*wordsize) = fix(info.si_uid);
    ref(struct_info, off_record_data+2*wordsize) = fix(info.si_signo);
    ref(struct_info, off_record_data+3*wordsize) = fix(info.si_status);
    ref(struct_info, off_record_data+4*wordsize) = fix(info.si_code);
    return struct_info;
  } else {
    return ik_errno_to_code();
  }
#else
  feature_failure(__func__);
#endif
}

/* end of file */
