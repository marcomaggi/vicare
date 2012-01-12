/*
  Part of: Vicare
  Contents: interface to GNU C Library functions
  Date: Wed Nov  9, 2011

  Abstract

	Interface to GNU C Library functions.

  Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

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

#define _GNU_SOURCE	1	/* to include "clog10()" */
#include "internals.h"
#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif
#ifdef HAVE_COMPLEX_H
#  include <complex.h>
#endif
#ifdef HAVE_FNMATCH_H
#  include <fnmatch.h>
#endif
#ifdef HAVE_GLOB_H
#  include <glob.h>
#endif
#ifdef HAVE_MATH_H
#  include <math.h>
#endif
#ifdef HAVE_NET_IF_H
#  include <net/if.h>
#endif
#ifdef HAVE_REGEX_H
#  include <regex.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_WORDEXP_H
#  include <wordexp.h>
#endif

/* Iconv usage has its own configuration option. */
#ifdef ENABLE_ICONV
#  include <iconv.h>
#endif

static IK_UNUSED void
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
  return (-1 == rv)? ik_errno_to_code() : IK_FIX(rv);
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
  template = IK_BYTEVECTOR_DATA_CHARP(template_bv);
  errno    = 0;
  rv       = mkstemp(template);
  return (-1 == rv)? ik_errno_to_code() : IK_FIX(rv);
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
  template = IK_BYTEVECTOR_DATA_CHARP(template_bv);
  errno    = 0;
  rv       = mkdtemp(template);
  return (NULL == rv)? ik_errno_to_code() : template_bv;
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
  return IK_FIX(0);
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
  rv    = fsync(IK_UNFIX(fd));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
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
  rv    = fdatasync(IK_UNFIX(fd));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
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
#ifdef HAVE_IF_NAMETOINDEX
  char *        name;
  unsigned int  rv;
  name  = IK_BYTEVECTOR_DATA_CHARP(name_bv);
  rv    = if_nametoindex(name);
  return (0 == rv)? false_object : IK_FIX((long)rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_if_indextoname (ikptr index, ikpcb * pcb)
{
#ifdef HAVE_IF_INDEXTONAME
  char          buffer[1+IFNAMSIZ];
  unsigned      i = (unsigned)IK_UNFIX(index);
  char *        rv;
  rv = if_indextoname(i, buffer);
  return (rv)? ik_bytevector_from_cstring(pcb, buffer) : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_if_nameindex (ikpcb * pcb)
{
#ifdef HAVE_IF_NAMEINDEX
  struct if_nameindex * arry;
  ikptr         s_alist;	/* the first pair in the alist's spine */
  ikptr         s_spine;        /* the current pair in the alist's spine */
  int           i;
  arry = if_nameindex();
  {
    s_alist    = s_spine = IKA_PAIR_ALLOC(pcb);
    pcb->root0 = &s_alist;
    pcb->root1 = &s_spine;
    {
      for (i=0; arry[i].if_index;) {
	IK_CAR(s_spine)  = IKA_PAIR_ALLOC(pcb);
	IK_CAAR(s_spine) = IK_FIX(arry[i].if_index);
	ikptr	s_bv     = ik_bytevector_from_cstring(pcb, arry[i].if_name);
	IK_CDAR(s_spine) = s_bv;
	if (arry[++i].if_index) {
	  IK_CDR(s_spine) = IKA_PAIR_ALLOC(pcb);
	  s_spine = IK_CDR(s_spine);
	} else
	  IK_CDR(s_spine) = null_object;
      }
    }
    pcb->root0 = NULL;
    pcb->root1 = NULL;
  }
  if_freenameindex(arry);
  return s_alist;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Mathematics.
 ** ----------------------------------------------------------------- */

#undef RE
#undef IM
#define RE(X)			IK_CFLONUM_REAL_DATA(X)
#define IM(X)			IK_CFLONUM_IMAG_DATA(X)
#define MAKE_CDOUBLE(CFLO)	(RE(CFLO) + IM(CFLO) * _Complex_I)

ikptr
ikrt_glibc_csin (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CSIN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = csin(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ccos (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CCOS
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ccos(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ctan (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CTAN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ctan(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_casin (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CASIN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = casin(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cacos (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CACOS
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = cacos(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_catan (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CATAN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = catan(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_cexp (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CEXP
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = cexp(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_clog (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CLOG
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = clog(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_clog10 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CLOG10
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = clog10(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_csqrt (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CSQRT
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = csqrt(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cpow (ikptr s_base, ikptr s_power, ikpcb * pcb)
{
#ifdef HAVE_CPOW
  complex double  base  = MAKE_CDOUBLE(s_base);
  complex double  power = MAKE_CDOUBLE(s_power);
  complex double  Y     = cpow(base, power);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_sinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_SINH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = sinh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_COSH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = cosh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_tanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_TANH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = tanh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_csinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CSINH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = csinh(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ccosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CCOSH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ccosh(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ctanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CTANH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ctanh(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_asinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ASINH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = asinh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_acosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ACOSH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = acosh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_atanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ATANH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = atanh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_casinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CASINH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = casinh(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cacosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CACOSH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = cacosh(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_catanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CATANH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = catanh(X);
  return iku_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_erf (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ERF
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = erf(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_erfc (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ERFC
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = erfc(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_lgamma (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_LGAMMA_R
  double        X   = IK_FLONUM_DATA(s_X);
  int           sgn;
  double        Y   = lgamma_r(X, &sgn);
  ikptr         s_pair = IKA_PAIR_ALLOC(pcb);
  pcb->root0 = &s_pair;
  {
    IK_CAR(s_pair) = iku_flonum_alloc(pcb, Y);
    IK_CDR(s_pair) = IK_FIX(sgn);
  }
  pcb->root0 = NULL;
  return s_pair;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_tgamma (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_TGAMMA
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = tgamma(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_y0 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_Y0
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = y0(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_y1 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_Y1
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = y1(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_j0 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_J0
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = j0(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_j1 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_J1
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = j1(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_yn (ikptr s_N, ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_YN
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = yn((int)IK_UNFIX(s_N), X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_jn (ikptr s_N, ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_JN
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = jn((int)IK_UNFIX(s_N), X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Random numbers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_rand (ikpcb * pcb)
{
#ifdef HAVE_RAND
  return ika_integer_from_long(pcb, (long)rand());
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_srand (ikptr s_seed)
{
#ifdef HAVE_RAND
  srand(ik_integer_to_uint(s_seed));
  return void_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Pattern matching, globbing, regular expressions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_fnmatch (ikptr s_pattern, ikptr s_string, ikptr s_flags)
{
#ifdef HAVE_FNMATCH
  return fnmatch(IK_BYTEVECTOR_DATA_CHARP(s_pattern),
                 IK_BYTEVECTOR_DATA_CHARP(s_string),
                 IK_UNFIX(s_flags))? false_object : true_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_glob (ikptr s_pattern, ikptr s_flags, ikptr s_error_handler, ikpcb * pcb)
{
#ifdef HAVE_GLOB
  typedef int handler_t (const char * filename, int error_code);
  glob_t        G;
  int           rv;
  handler_t     * handler;
  handler = (false_object == s_error_handler)? NULL : IK_POINTER_DATA_VOIDP(s_error_handler);
  G.gl_pathc    = 0;
  G.gl_pathv    = NULL;
  G.gl_offs     = 0;
  G.gl_opendir  = NULL;
  G.gl_readdir  = NULL;
  G.gl_closedir = NULL;
  G.gl_stat     = NULL;
  G.gl_lstat    = NULL;
  rv = glob(IK_BYTEVECTOR_DATA_CHARP(s_pattern), IK_UNFIX(s_flags), handler, &G);
  if (0 == rv) {
    ikptr       s_list = ika_list_from_argv_and_argc(pcb, G.gl_pathv, G.gl_pathc);
    globfree(&G);
    return s_list;
  } else
    return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_regcomp (ikptr s_pattern, ikptr s_flags, ikpcb *pcb)
{
#ifdef HAVE_REGCOMP
  ikptr         s_compiled_regex = ika_bytevector_alloc(pcb, sizeof(regex_t));
  regex_t *     compiled_regex   = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
  char *        pattern          = IK_BYTEVECTOR_DATA_CHARP(s_pattern);
  int           rv;
  pcb->root0 = &s_compiled_regex;
  rv = regcomp(compiled_regex, pattern, IK_UNFIX(s_flags));
  if (0 == rv) {
    pcb->root0 = NULL;
    return s_compiled_regex;
  } else {
    ikptr       s_pair = IKA_PAIR_ALLOC(pcb);
    char *      error_message;
    size_t      error_message_len;
    pcb->root1 = &s_pair;
    {
      compiled_regex    = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
      error_message_len = regerror(rv, compiled_regex, NULL, 0);
      IK_CAR(s_pair)    = IK_FIX(rv);
      IK_CDR(s_pair)    = ika_bytevector_alloc(pcb, (long)error_message_len-1);
      error_message     = IK_BYTEVECTOR_DATA_CHARP(IK_CDR(s_pair));
      compiled_regex    = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
      regerror(rv, compiled_regex, error_message, error_message_len);
    }
    pcb->root1 = NULL;
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_regexec (ikptr s_compiled_regex, ikptr s_string, ikptr s_flags, ikpcb *pcb)
{
#ifdef HAVE_REGCOMP
  regex_t *     compiled_regex = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
  char *        string         = IK_BYTEVECTOR_DATA_CHARP(s_string);
  size_t        nmatch         = compiled_regex->re_nsub;
  regmatch_t    match[1+nmatch];
  int           rv;
  rv = regexec(compiled_regex, string, 1+nmatch, match, IK_UNFIX(s_flags));
  switch (rv) {
  case 0:
    {
      size_t      i;
      ikptr       s_match_vector = ik_vector_alloc(pcb, 1+nmatch);
      ikptr       s_pair;
      pcb->root0 = &s_match_vector;
      {
        for (i=0; i<1+nmatch; ++i) {
          s_pair = IK_ITEM(s_match_vector, i) = IKA_PAIR_ALLOC(pcb);
          IK_CAR(s_pair) = IK_FIX(match[i].rm_so);
          IK_CDR(s_pair) = IK_FIX(match[i].rm_eo);
        }
      }
      pcb->root0 = NULL;
      return s_match_vector;
    }
  case REG_NOMATCH:
    return false_object;
  default:
    {
      ikptr       s_pair = IKA_PAIR_ALLOC(pcb);
      pcb->root0 = &s_pair;
      {
        char *          errmsg;
        size_t          errmsg_len_including_zero;
        compiled_regex            = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
        errmsg_len_including_zero = regerror(rv, compiled_regex, NULL, 0);
        IK_CAR(s_pair)            = IK_FIX(rv);
        IK_CDR(s_pair)            = ika_bytevector_alloc(pcb, (long)errmsg_len_including_zero-1);
        errmsg                    = IK_BYTEVECTOR_DATA_CHARP(IK_CDR(s_pair));
        compiled_regex            = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
        regerror(rv, compiled_regex, errmsg, errmsg_len_including_zero);
      }
      pcb->root0 = NULL;
      return s_pair;
    }
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_regfree (ikptr s_compiled_regex)
/* Free the compiled regex in the given bytevector, but only if at least
   one byte in  the bytevector is non-zero, else it  means that the data
   structure has already been freed. */
{
#ifdef HAVE_REGCOMP
  uint8_t *     data = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
  long          len  = IK_BYTEVECTOR_LENGTH(s_compiled_regex);
  long          i;
  int           clean = 0;
  for (i=0; i<len; ++i) {
    if (data[i]) {
      clean=1;
      break;
    }
  }
  if (clean) {
    regfree((regex_t*)data);
    for (i=0; i<len; ++i)
      data[i] = 0;
  }
  return void_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Performing word expansion.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_wordexp (ikptr s_words, ikptr s_flags, ikpcb * pcb)
{
#ifdef HAVE_WORDEXP
  char *        word = IK_BYTEVECTOR_DATA_VOIDP(s_words);
  wordexp_t     W;
  int           rv;
  W.we_wordc    = 0;
  W.we_wordv    = NULL;
  W.we_offs     = 0;
  rv = wordexp(word, &W, IK_UNFIX(s_flags));
  if (0 == rv) {
    ikptr       s_words = ik_vector_alloc(pcb, (long)W.we_wordc);
    pcb->root0 = &s_words;
    {
      int         i;
      for (i=0; i<W.we_wordc; ++i) {
        IK_ITEM(s_words, i) = ik_bytevector_from_cstring(pcb, W.we_wordv[i]);
      }
    }
    pcb->root0 = NULL;
    wordfree(&W);
    return s_words;
  } else
    return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** System configuration.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_sysconf (ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_SYSCONF
  long  parameter = ik_integer_to_long(s_parameter);
  long  value;
  errno = 0;
  value = sysconf((int)parameter);
  if (-1 == value)
    return (errno)? ik_errno_to_code() : false_object;
  else
    return ika_integer_from_long(pcb, value);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_pathconf (ikptr s_pathname, ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_PATHCONF
  char *pathname  = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  long  parameter = ik_integer_to_long(s_parameter);
  long  value;
  errno = 0;
  value = pathconf(pathname, (int)parameter);
  if (-1 == value)
    return (errno)? ik_errno_to_code() : false_object;
  else
    return ika_integer_from_long(pcb, value);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_fpathconf (ikptr s_fd, ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_FPATHCONF
  long  parameter = ik_integer_to_long(s_parameter);
  long  value;
  errno = 0;
  value = fpathconf(IK_UNFIX(s_fd), (int)parameter);
  if (-1 == value)
    return (errno)? ik_errno_to_code() : false_object;
  else
    return ika_integer_from_long(pcb, value);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_confstr (ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_CONFSTR
  long          parameter = ik_integer_to_long(s_parameter);
  size_t        length_including_zero;
  errno = 0;
  length_including_zero = confstr((int)parameter, NULL, 0);
  if (length_including_zero) {
    ikptr       s_result = ika_bytevector_alloc(pcb, (long)length_including_zero-1);
    char *      result   = IK_BYTEVECTOR_DATA_CHARP(s_result);
    confstr((int)parameter, result, length_including_zero);
    return s_result;
  } else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Generic character set conversion.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_iconv_open (ikptr s_from_code, ikptr s_to_code, ikpcb * pcb)
/* Open  a  new  conversion  handle.   S_TO_CODE must  be  a  bytevector
   representing the name of the  output encoding.  S_FROM_CODE must be a
   bytevector  representing   the  name  of  the   input  encoding.   If
   successful:  the return  value is  a pointer  object  referencing the
   state structure; if  an error occurs: the return  value is an encoded
   "errno" value. */
{
#if (ENABLE_ICONV && (defined (HAVE_ICONV_OPEN)))
  char *	to_code   = IK_BYTEVECTOR_DATA_CHARP(s_to_code);
  char *	from_code = IK_BYTEVECTOR_DATA_CHARP(s_from_code);
  /* Glibc documentation  states that we must not  assume anything about
     the "iconv_t" type, but we known  that it is a pointer.  It is very
     unlikely that  its implementation is  changed, so we take  the risk
     here. */
  iconv_t	handle;
  errno  = 0;
  /* Beware of the order of the arguments!!!  Vicare's API has different
     order than the Libiconv API. */
  handle = iconv_open(to_code, from_code);
  if (((iconv_t)-1) != handle)
    return ik_pointer_alloc((unsigned long)handle, pcb);
  else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_iconv_close (ikptr s_handle)
/* Close a  conversion handle,  releasing all the  associated resources.
   the handle is finalised only if S_HANDLE is a non-NULL pointer; if it
   is  a   NULL  pointer  nothing  happens  and   the  function  returns
   successfully.  If successful: the return value is the fixnum zero and
   S_HANLE is mutated to hold the  NULL pointer; if an error occurs: the
   return value is an encoded "errno" value. */
{
#if (ENABLE_ICONV && (defined (HAVE_ICONV_CLOSE)))
  iconv_t	handle = (iconv_t)IK_POINTER_DATA_VOIDP(s_handle);
  if (handle) {
    int		retval;
    errno  = 0;
    retval = iconv_close(handle);
    if (-1 == retval)
      return ik_errno_to_code();
    ref(s_handle, off_pointer_data) = 0;
  }
  return IK_FIX(0);
#else
  feature_failure(__func__);
#endif
}
/* Convert a  range of bytes from  the bytevector S_IN_BV  and store the
   result into a range of bytes in the bytevector S_OUT_BV, according to
   the context specified by S_HANDLE.

     S_IN_START  is  a fixnum  representing  the  inclusive input  start
   index;  S_IN_PAST is a  fixnum representing  the exclusive  input end
   index;  S_OUT_START is  a  fixnum representing  the inclusive  output
   start index; S_OUT_PAST is a fixnum representing the exclusive output
   end index.  They must be such that:

	0 <= S_IN_START  <= S_IN_PAST  <= length(S_IN_BV)
	0 <= S_OUT_START <= S_OUT_PAST <= length(S_OUT_BV)

     As special  cases: if S_IN_PAST is  false, the input  past index is
   the length of S_IN_BV; if  S_OUT_PAST is false, the output past index
   is the length of S_OUT_BV.

     If the  operation is successful: the  return value is  a pair whose
   car is a  fixnum representing the index of the  first byte in S_IN_BV
   that  was not consumed  and whose  cdr is  a fixnum  representing the
   index of the  first byte in S_OUT_BV that was  not filled with output
   data.  If  all the  input range  was processed: the  car of  the pair
   equals S_IN_PAST.   If all the  output range was filled  with output:
   the cdr of the pair equals S_OUT_PAST.

     If  an  error  occurs:  the  return value  is  an  encoded  "errno"
   value. */
ikptr
ikrt_glibc_iconv (ikptr s_handle,
		  ikptr s_in_bv,  ikptr s_in_start,  ikptr s_in_past,
		  ikptr s_out_bv, ikptr s_out_start, ikptr s_out_past,
		  ikpcb * pcb)
{
#if (ENABLE_ICONV && (defined (HAVE_ICONV)))
  iconv_t	handle = (iconv_t)IK_POINTER_DATA_VOIDP(s_handle);
  size_t	istart = IK_UNFIX(s_in_start);
  size_t	ipast  = (false_object == s_in_past)? \
    IK_BYTEVECTOR_LENGTH(s_in_bv) : IK_UNFIX(s_in_past);
  size_t	ostart = IK_UNFIX(s_out_start);
  size_t	opast  =  (false_object == s_out_past)? \
    IK_BYTEVECTOR_LENGTH(s_out_bv) : IK_UNFIX(s_out_past);
  char *	input  = istart + IK_BYTEVECTOR_DATA_CHARP(s_in_bv);
  char *	output = ostart + IK_BYTEVECTOR_DATA_CHARP(s_out_bv);
  size_t	isize  = ipast - istart;
  size_t	osize  = opast - ostart;
  size_t	retval;
  errno  = 0;
  retval = iconv(handle, &input, &isize, &output, &osize);
#undef ERR
#define ERR	((size_t)-1)
  if ((0 == retval) || ((ERR == retval) && (E2BIG == errno))) {
    ikptr	s_pair = IKA_PAIR_ALLOC(pcb);
    istart = ipast - isize;
    ostart = opast - osize;
    IK_CAR(s_pair) = IK_FIX(istart);
    IK_CDR(s_pair) = IK_FIX(ostart);
    return s_pair;
  } else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */



/* end of file */
