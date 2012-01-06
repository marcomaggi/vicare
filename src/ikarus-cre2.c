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
 ** Precompiled regular expression objects.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_cre2_new (ikptr s_pattern, ikptr s_options, ikpcb * pcb)
/* Build a new precompiled regular expression object.  S_PATTERN must be
   a bytevector holding the regexp pattern.  S_OPTIONS must be a pointer
   to a "cre2_options_t" value or false if the regexp must be built with
   the default options.

   If  successful:  return  a  pointer  object  referencing  the  regexp
   structure.  If  an error occurs allocating memory:  return false.  If
   an error  occurs building the  object: return a  pair whose car  is a
   fixnum  representing the  error code  and whose  cdr is  a bytevector
   representing the error string in ASCII encoding.
*/
{
#if (1 == ENABLE_CRE2)
  const char *		pattern;
  int			pattern_len;
  cre2_regexp_t *	rex;
  cre2_options_t *	options;
  pattern     = IK_BYTEVECTOR_DATA_CHARP(s_pattern);
  pattern_len = IK_BYTEVECTOR_LENGTH(s_pattern);
  options     = (false_object == s_options)? NULL : IK_POINTER_DATA_VOIDP(s_options);
  rex         = cre2_new(pattern, pattern_len, options);
  if (NULL == rex)
    return false_object; /* error allocating memory */
  else {
    int  errcode = cre2_error_code(rex);
    if (errcode) {
      ikptr	s_pair = IK_PAIR_ALLOC(pcb);
      IK_CAR(s_pair) = fix(errcode);
      IK_CDR(s_pair) = ik_bytevector_from_cstring(pcb, cre2_error_string(rex));
      cre2_delete(rex);
      return s_pair;
    } else
      return ik_pointer_alloc((unsigned long)rex, pcb);
  }
#else
  return feature_failure(__func__);
#endif
}
ikptr
ikrt_cre2_delete (ikptr s_rex)
/* Finalise  a precompiled regular  expression releasing  the associated
   resources.   Finalisation  takes place  only  if  S_REX references  a
   non-NULL  pointer.  After the  context has  been finalised:  S_REX is
   mutated to reference a NULL pointer. */
{
#if (1 == ENABLE_CRE2)
  cre2_regexp_t *	rex;
  rex = IK_POINTER_DATA_VOIDP(s_rex);
  if (rex) {
    cre2_delete(rex);
    ref(s_rex, off_pointer_data) = 0;
  }
  return void_object;
#else
  return feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Configuration options.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_cre2_opt_new (ikpcb * pcb)
/* Build a new configuration options object.

   If  successful:  return  a  pointer object  referencing  the  options
   structure.  If an error occurs allocating memory: return false. */
{
#if (1 == ENABLE_CRE2)
  cre2_options_t *	options;
  options = cre2_opt_new();
  if (NULL == options)
    return false_object; /* error allocating memory */
  else
    return ik_pointer_alloc((unsigned long)options, pcb);
#else
  return feature_failure(__func__);
#endif
}
ikptr
ikrt_cre2_opt_delete (ikptr s_opt)
/* Finalise  a  configuration options  object  releasing the  associated
   resources.   Finalisation  takes place  only  if  S_OPT references  a
   non-NULL  pointer.  After the  context has  been finalised:  S_OPT is
   mutated to reference a NULL pointer. */
{
#if (1 == ENABLE_CRE2)
  cre2_options_t *	opt;
  opt = IK_POINTER_DATA_VOIDP(s_opt);
  if (opt) {
    cre2_opt_delete(opt);
    ref(s_opt, off_pointer_data) = 0;
  }
  return void_object;
#else
  return feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
