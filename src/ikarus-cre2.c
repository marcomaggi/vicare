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

#include "vicare.h"
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#if (1 == ENABLE_CRE2)
#  include <cre2.h>
#else
static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called CRE2 specific function, %s", funcname);
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
  return IK_FIX(cre2_version_interface_current());
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_cre2_version_interface_revision (void)
{
#if (1 == ENABLE_CRE2)
  return IK_FIX(cre2_version_interface_revision());
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_cre2_version_interface_age (void)
{
#if (1 == ENABLE_CRE2)
  return IK_FIX(cre2_version_interface_age());
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
      ikptr	s_pair = IKA_PAIR_ALLOC(pcb);
      pcb->root0 = &s_pair;
      {
	IK_CAR(s_pair) = IK_FIX(errcode);
	IK_ASS(IK_CDR(s_pair), ika_bytevector_from_cstring(pcb, cre2_error_string(rex)));
      }
      pcb->root0 = NULL;
      cre2_delete(rex);
      return s_pair;
    } else
      return ika_pointer_alloc(pcb, (ik_ulong)rex);
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
    IK_POINTER_SET_NULL(s_rex);
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
  cre2_options_t *	opt;
  opt = cre2_opt_new();
  if (opt) {
    /* UTF8 is  the default in the  current release but I  set it anyway
       (Marco Maggi; Jan 12, 2012). */
    cre2_opt_set_encoding(opt, CRE2_UTF8);
    return ika_pointer_alloc(pcb, (ik_ulong)opt);
  } else
    return false_object; /* error allocating memory */
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
    IK_POINTER_SET_NULL(s_opt);
  }
  return void_object;
#else
  return feature_failure(__func__);
#endif
}

#define DEFINE_OPTION_SETTER_AND_GETTER(NAME)				\
  ikptr									\
  ikrt_cre2_opt_set_##NAME (ikptr s_opt, ikptr s_bool)			\
  {									\
    cre2_options_t *	opt;						\
    opt = IK_POINTER_DATA_VOIDP(s_opt);					\
    cre2_opt_set_##NAME(opt, (false_object == s_bool)? 0 : 1);		\
    return void_object;							\
  }									\
  ikptr									\
  ikrt_cre2_opt_##NAME (ikptr s_opt)					\
  {									\
    cre2_options_t *	opt;						\
    opt = IK_POINTER_DATA_VOIDP(s_opt);					\
    return (cre2_opt_##NAME(opt))? true_object : false_object;		\
  }

DEFINE_OPTION_SETTER_AND_GETTER(posix_syntax)
DEFINE_OPTION_SETTER_AND_GETTER(longest_match)
DEFINE_OPTION_SETTER_AND_GETTER(log_errors)
DEFINE_OPTION_SETTER_AND_GETTER(literal)
DEFINE_OPTION_SETTER_AND_GETTER(never_nl)
DEFINE_OPTION_SETTER_AND_GETTER(case_sensitive)
DEFINE_OPTION_SETTER_AND_GETTER(perl_classes)
DEFINE_OPTION_SETTER_AND_GETTER(word_boundary)
DEFINE_OPTION_SETTER_AND_GETTER(one_line)

ikptr
ikrt_cre2_opt_set_max_mem (ikptr s_opt, ikptr s_dim)
{
  cre2_options_t *	opt;
  long			dim;
  opt = IK_POINTER_DATA_VOIDP(s_opt);
  dim = ik_integer_to_long(s_dim);
  cre2_opt_set_max_mem(opt, (int)dim);
  return void_object;
}
ikptr
ikrt_cre2_opt_max_mem (ikptr s_opt, ikpcb * pcb)
{
  cre2_options_t *	opt;
  long			dim;
  opt = IK_POINTER_DATA_VOIDP(s_opt);
  dim = (long)cre2_opt_max_mem(opt);
  return ika_integer_from_long(pcb, dim);
}


/** --------------------------------------------------------------------
 ** Matching.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_cre2_match (ikptr s_rex, ikptr s_text, ikptr s_start, ikptr s_end, ikptr s_anchor, ikpcb * pcb)
/* Match  a substring of  S_TEXT against  the regular  expression object
   S_REX.   Return false  if there  is no  match, else  return  a vector
   representing the matching portions.

   S_TEXT must be a UTF-8 bytevector.

   The  zero--based indices  S_START (inclusive)  and  S_END (exclusive)
   select the range of S_TEXT to be examined.

   S_ANCHOR selects the anchor point for the matching operation, it must
   be one of the fixnum: 0 for unanchored, 1 for start, 2 for both.

   Data  about the matching  groups is  returned in  a vector  of pairs,
   which  will  have   a  number  of  slots  equal   to  the  number  of
   parenthetical subexpressions in S_REX  plus one.  Each pair selects a
   range  of  bytes  in  S_TEXT:   the  car  is  a  non-negative  fixnum
   representing  the inclusive start  index, the  cdr is  a non-negative
   fixnum representing the exclusive end index.

   The first element  of the match vector (index  0) references the full
   portion of the  substring of S_TEXT matching the  pattern; the second
   element of the match vector  (index 1) references the portion of text
   matching the first parenthetical  subexpression, the third element of
   the match  vector (index 2)  references the portion of  text matching
   the second parenthetical subexpression; and so on.  */
{
#if (1 == ENABLE_CRE2)
  cre2_regexp_t *	rex;
  const char *		text_data;
  int			text_len;
  int			start, end, anchor;
  int			nmatch;
  int			retval;
  rex		= IK_POINTER_DATA_VOIDP(s_rex);
  text_data	= IK_BYTEVECTOR_DATA_CHARP(s_text);
  text_len	= IK_BYTEVECTOR_LENGTH(s_text);
  start		= IK_UNFIX(s_start);
  end		= IK_UNFIX(s_end);
  anchor	= IK_UNFIX(s_anchor);
  nmatch	= 1 + cre2_num_capturing_groups(rex);
  switch (anchor) {
  case 0: anchor = CRE2_UNANCHORED;	break;
  case 1: anchor = CRE2_ANCHOR_START;	break;
  case 2: anchor = CRE2_ANCHOR_BOTH;	break;
  default: /* should never happen */
    anchor = CRE2_UNANCHORED;
  }
  cre2_string_t		strings[nmatch];
  retval = cre2_match(rex, text_data, text_len, start, end, anchor, strings, nmatch);
  if (retval) {
    cre2_range_t	ranges[nmatch];
    ikptr		s_match;
    int			i;
    cre2_strings_to_ranges(text_data, ranges, strings, nmatch);
    s_match = ika_vector_alloc(pcb, nmatch);
    pcb->root0 = &s_match;
    {
      for (i=0; i<nmatch; ++i) {
	ikptr	s_pair = IKA_PAIR_ALLOC(pcb);	/* first alloc... */
	IK_ITEM(s_match, i) = s_pair;		/* ...then store */
	IK_CAR(s_pair)      = IK_FIX(ranges[i].start);
	IK_CDR(s_pair)      = IK_FIX(ranges[i].past);
      }
    }
    pcb->root0 = NULL;
    return s_match;
  } else
    return false_object; /* no match */
#else
  return feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
