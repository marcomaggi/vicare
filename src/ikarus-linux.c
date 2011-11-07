/*
  Part of: Vicare
  Contents: interface to Linux functions
  Date: Mon Nov  7, 2011

  Abstract



  Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>

  This  program  is free  software:  you  can redistribute  it
  and/or modify it  under the terms of the  GNU General Public
  License as published by the Free Software Foundation, either
  version  3 of  the License,  or (at  your option)  any later
  version.

  This  program is  distributed in  the hope  that it  will be
  useful, but  WITHOUT ANY WARRANTY; without  even the implied
  warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
  PURPOSE.   See  the  GNU  General Public  License  for  more
  details.

  You should  have received a  copy of the GNU  General Public
  License   along   with    this   program.    If   not,   see
  <http://www.gnu.org/licenses/>.

*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "config.h"
#include "ikarus-data.h"
#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>


/** --------------------------------------------------------------------
 ** Prototypes and external definitions.
 ** ----------------------------------------------------------------- */

ikptr ik_errno_to_code (void);


/** --------------------------------------------------------------------
 ** Process exit status.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_linux_WIFCONTINUED (ikptr fx_status)
{
#if (1 == HAVE_WIFCONTINUED)
  int   status = unfix(fx_status);
  return (WIFCONTINUED(status))? true_object : false_object;
#else
  fprintf(stderr, "Vicare error: called Linux specific function, %s\n", __func__);
  exit(EXIT_FAILURE);
#endif
}


/* end of file */
