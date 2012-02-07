/*
  Part of: Vicare Scheme
  Contents: optional support for GNU Readline
  Date: Tue Feb  7, 2012

  Abstract



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

#include "internals.h"
#if (1 == ENABLE_READLINE)
#  include <readline.h>
#endif

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called GNU Readline library specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return void_object; }


/** --------------------------------------------------------------------
 ** Simple support.
 ** ----------------------------------------------------------------- */

ikptr
ik_readline_readline (ikptr s_prompt, ikpcb * pcb)
{
#if (1 == ENABLE_READLINE)
  char *	prompt = IK_BYTEVECTOR_DATA_CHARP(s_prompt);
  char *	line;
  line = readline(prompt);
  return ika_bytevector_from_cstring(line);
#else
  feature_failure(__func__);
#endif
}


/* end of file */
