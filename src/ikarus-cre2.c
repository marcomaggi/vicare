/*
  Part of: Vicare
  Contents: built in binding to CRE2
  Date: Fri Jan  6, 2012

  Abstract

	Built in  binding to the CRE2  library: a C wrapper  for the RE2
	regular expressions library from Google.

  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

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

#if (1 == ENABLE_CRE2)
#  include <cre2.h>
#else

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  fprintf(stderr, "Vicare error: called CRE2 specific function, %s\n", funcname);
  exit(EXIT_FAILURE);
}
#define feature_failure(FN)     { feature_failure_(FN); return void_object; }

#endif


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_cre2_enabled (void)
{
#if (1 == ENABLE_CRE2)
  return true_object;
#else
  return false_object;
#endif
}
ikptr
ikrt_cre2_version_interface_current (void)
{
#if (1 == ENABLE_CRE2)
  return fix(cre2_version_interface_current());
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_cre2_version_interface_revision (void)
{
#if (1 == ENABLE_CRE2)
  return fix(cre2_version_interface_revision());
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_cre2_version_interface_age (void)
{
#if (1 == ENABLE_CRE2)
  return fix(cre2_version_interface_age());
#else
  feature_failure(__func__);
#endif
}



/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
