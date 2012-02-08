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
#ifdef HAVE_LIBREADLINE
#  if defined(HAVE_READLINE_READLINE_H)
#    include <readline/readline.h>
#  elif defined(HAVE_READLINE_H)
#    include <readline.h>
#  else /* !defined(HAVE_READLINE_H) */
extern char *readline ();
#  endif /* !defined(HAVE_READLINE_H) */
char *cmdline = NULL;
#else /* !defined(HAVE_READLINE_READLINE_H) */
   /* no readline */
#endif /* HAVE_LIBREADLINE */

#ifdef HAVE_READLINE_HISTORY
#  if defined(HAVE_READLINE_HISTORY_H)
#    include <readline/history.h>
#  elif defined(HAVE_HISTORY_H)
#    include <history.h>
#  else /* !defined(HAVE_HISTORY_H) */
extern void add_history ();
extern int write_history ();
extern int read_history ();
#  endif /* defined(HAVE_READLINE_HISTORY_H) */
   /* no history */
#endif /* HAVE_READLINE_HISTORY */

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
ik_readline_enabled (void)
{
#ifdef HAVE_LIBREADLINE
  return true_object;
#else
  return false_object;
#endif
}
ikptr
ik_readline_readline (ikptr s_prompt, ikpcb * pcb)
{
#ifdef HAVE_LIBREADLINE
  char *	prompt = (false_object == s_prompt)? NULL : IK_BYTEVECTOR_DATA_CHARP(s_prompt);
  char *	line;
  ikptr		rv;
  line = readline(prompt);
  if (line) {
#ifdef HAVE_READLINE_HISTORY
    /* Lines are added to the history only if they are not empty. */
    if (*line)
      add_history(line);
#endif
    rv = ika_bytevector_from_cstring(pcb, line);
    free(line);
    return rv;
  } else
    return false_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** GNU Readline specific interface.
 ** ----------------------------------------------------------------- */

ikptr
ik_readline_rl_version (ikpcb * pcb)
{
#if ((defined HAVE_LIBREADLINE) && (defined RL_READLINE_VERSION))
  return ika_integer_from_int(pcb, (int)RL_READLINE_VERSION);
#else
  return false_object;
#endif
}


/* end of file */
