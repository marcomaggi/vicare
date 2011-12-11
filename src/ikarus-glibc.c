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
  template = IK_BYTEVECTOR_DATA_CHARP(template_bv);
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
  template = IK_BYTEVECTOR_DATA_CHARP(template_bv);
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
  name  = IK_BYTEVECTOR_DATA_CHARP(name_bv);
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
    char *      data = IK_BYTEVECTOR_DATA_CHARP(bv);
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
    spine = ik_pair_alloc(pcb);
    ref(spine, off_cdr) = alist;
    alist               = spine;
    /* Add an entry to the alist. */
    entry = ik_pair_alloc(pcb);
    ref(spine, off_car) = entry;
    /* Fill the entry. */
    len  = strlen(arry[i].if_name);
    bv   = ik_bytevector_alloc(pcb, len);
    data = IK_BYTEVECTOR_DATA_CHARP(bv);
    memcpy(data, arry[i].if_name, len);
    ref(entry, off_car) = fix(arry[i].if_index);
    ref(entry, off_cdr) = bv;
  }
  if_freenameindex(arry);
  pcb->root0 = NULL;
  return alist;
}


/** --------------------------------------------------------------------
 ** Mathematics.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_glibc_csin (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CSIN
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = csin(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ccos (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CCOS
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = ccos(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ctan (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CTAN
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = ctan(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_casin (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CASIN
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = casin(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cacos (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CACOS
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = cacos(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_catan (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CATAN
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = catan(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_cexp (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CEXP
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = cexp(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_clog (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CLOG
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = clog(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_clog10 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CLOG10
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = clog10(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_csqrt (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CSQRT
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = csqrt(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cpow (ikptr s_base, ikptr s_power, ikpcb * pcb)
{
#ifdef HAVE_CPOW
  complex double  base  = CFLONUM_DATA_REAL(s_base)  + CFLONUM_DATA_REAL(s_base)  * _Complex_I;
  complex double  power = CFLONUM_DATA_REAL(s_power) + CFLONUM_DATA_REAL(s_power) * _Complex_I;
  complex double  Y     = cpow(base, power);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_sinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_SINH
  double        X   = FLONUM_DATA(s_X);
  double        Y   = sinh(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_COSH
  double        X   = FLONUM_DATA(s_X);
  double        Y   = cosh(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_tanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_TANH
  double        X   = FLONUM_DATA(s_X);
  double        Y   = tanh(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_csinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CSINH
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = csinh(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ccosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CCOSH
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = ccosh(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_ctanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CTANH
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = ctanh(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_asinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ASINH
  double        X   = FLONUM_DATA(s_X);
  double        Y   = asinh(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_acosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ACOSH
  double        X   = FLONUM_DATA(s_X);
  double        Y   = acosh(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_atanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ATANH
  double        X   = FLONUM_DATA(s_X);
  double        Y   = atanh(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_casinh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CASINH
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = casinh(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_cacosh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CACOSH
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = cacosh(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_catanh (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_CATANH
  complex double        X   = CFLONUM_DATA_REAL(s_X) + CFLONUM_DATA_REAL(s_X) * _Complex_I;
  complex double        Y   = catanh(X);
  return ik_cflonum_alloc(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_erf (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ERF
  double        X   = FLONUM_DATA(s_X);
  double        Y   = erf(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_erfc (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_ERFC
  double        X   = FLONUM_DATA(s_X);
  double        Y   = erfc(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_lgamma (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_LGAMMA_R
  double        X   = FLONUM_DATA(s_X);
  int           sgn;
  double        Y   = lgamma_r(X, &sgn);
  ikptr         s_pair;
  s_pair = ik_pair_alloc(pcb);
  IK_SET_CAR(s_pair, ik_flonum_alloc(pcb, Y));
  IK_SET_CDR(s_pair, fix(sgn));
  return s_pair;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_tgamma (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_TGAMMA
  double        X   = FLONUM_DATA(s_X);
  double        Y   = tgamma(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_y0 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_Y0
  double        X   = FLONUM_DATA(s_X);
  double        Y   = y0(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_y1 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_Y1
  double        X   = FLONUM_DATA(s_X);
  double        Y   = y1(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_j0 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_J0
  double        X   = FLONUM_DATA(s_X);
  double        Y   = j0(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_j1 (ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_J1
  double        X   = FLONUM_DATA(s_X);
  double        Y   = j1(X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_yn (ikptr s_N, ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_YN
  double        X   = FLONUM_DATA(s_X);
  double        Y   = yn((int)unfix(s_N), X);
  return ik_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_jn (ikptr s_N, ikptr s_X, ikpcb * pcb)
{
#ifdef HAVE_JN
  double        X   = FLONUM_DATA(s_X);
  double        Y   = jn((int)unfix(s_N), X);
  return ik_flonum_alloc(pcb, Y);
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
  return ik_integer_from_long((long)rand(), pcb);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_glibc_srand (ikptr s_seed)
{
#ifdef HAVE_RAND
  srand((unsigned int)ik_integer_to_unsigned_long(s_seed));
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
                 unfix(s_flags))? false_object : true_object;
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
  rv = glob(IK_BYTEVECTOR_DATA_CHARP(s_pattern), unfix(s_flags), handler, &G);
  if (0 == rv) {
    ikptr       s_list = ik_list_from_argv_and_argc(G.gl_pathv, G.gl_pathc, pcb);
    globfree(&G);
    return s_list;
  } else {
    return fix(rv);
  }
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_glibc_regcomp (ikptr s_pattern, ikptr s_flags, ikpcb *pcb)
{
#ifdef HAVE_REGCOMP
  ikptr         s_compiled_regex = ik_bytevector_alloc(pcb, sizeof(regex_t));
  regex_t *     compiled_regex   = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
  char *        pattern          = IK_BYTEVECTOR_DATA_CHARP(s_pattern);
  int           rv;
  pcb->root0 = &s_compiled_regex;
  rv = regcomp(compiled_regex, pattern, unfix(s_flags));
  if (0 == rv) {
    pcb->root0 = NULL;
    return s_compiled_regex;
  } else {
    ikptr       s_pair = ik_pair_alloc(pcb);
    char *      error_message;
    size_t      error_message_len;
    /* After allocating memory  we need to extract again  the pointer to
       data,  because  the  bytevector  may have  been  moved  somewhere
       else. */
    compiled_regex    = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
    error_message_len = regerror(rv, compiled_regex, NULL, 0);
    IK_SET_CAR(s_pair, fix(rv));
    IK_SET_CDR(s_pair, ik_bytevector_alloc(pcb, (long)error_message_len-1));
    error_message     = IK_BYTEVECTOR_DATA_CHARP(IK_CDR(s_pair));
    compiled_regex    = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
    regerror(rv, compiled_regex, error_message, error_message_len);
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
  rv = regexec(compiled_regex, string, 1+nmatch, match, unfix(s_flags));
  switch (rv) {
  case 0:
    {
      size_t      i;
      ikptr       s_match = ik_vector_alloc(pcb, 1+nmatch);
      ikptr       s_pair;
      pcb->root0 = &s_match;
      {
        for (i=0; i<1+nmatch; ++i) {
          s_pair = ik_pair_alloc(pcb);
          IK_VECTOR_SET(s_match, i, s_pair);
          IK_SET_CAR(s_pair, fix(match[i].rm_so));
          IK_SET_CDR(s_pair, fix(match[i].rm_eo));
        }
      }
      pcb->root0 = NULL;
      return s_match;
    }
  case REG_NOMATCH:
    return false_object;
  default:
    {
      ikptr       s_pair = ik_pair_alloc(pcb);
      char *      error_message;
      size_t      error_message_len;
      /* After allocating memory  we need to extract again  the pointer to
         data,  because  the  bytevector  may have  been  moved  somewhere
         else. */
      compiled_regex    = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
      error_message_len = regerror(rv, compiled_regex, NULL, 0);
      IK_SET_CAR(s_pair, fix(rv));
      IK_SET_CDR(s_pair, ik_bytevector_alloc(pcb, (long)error_message_len-1));
      error_message     = IK_BYTEVECTOR_DATA_CHARP(IK_CDR(s_pair));
      compiled_regex    = IK_BYTEVECTOR_DATA_VOIDP(s_compiled_regex);
      regerror(rv, compiled_regex, error_message, error_message_len);
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
  rv = wordexp(word, &W, unfix(s_flags));
  if (0 == rv) {
    ikptr       s_words = ik_vector_alloc(pcb, (long)W.we_wordc);
    int         i;
    for (i=0; i<W.we_wordc; ++i) {
      IK_VECTOR_SET(s_words, i, ik_bytevector_from_cstring(pcb, W.we_wordv[i]));
    }
    wordfree(&W);
    return s_words;
  } else
    return fix(rv);
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
    return ik_integer_from_long(value, pcb);
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
    return ik_integer_from_long(value, pcb);
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
  value = fpathconf(unfix(s_fd), (int)parameter);
  if (-1 == value)
    return (errno)? ik_errno_to_code() : false_object;
  else
    return ik_integer_from_long(value, pcb);
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
    ikptr       s_result = ik_bytevector_alloc(pcb, (long)length_including_zero-1);
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
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */



/* end of file */
