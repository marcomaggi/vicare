/*
  Part of: Vicare
  Contents: interface to GNU C Library functions
  Date: Wed Nov  9, 2011

  Abstract

	Interface to GNU C Library functions.

  Copyright (C) 2011, 2012, 2013, 2015, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>

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
#if ((defined HAVE_ICONV) && (1 == VICARE_HAVE_ICONV))
#  include <iconv.h>
#endif

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called GNU C Library specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return IK_VOID_OBJECT; }

/* ------------------------------------------------------------------ */

/* file descriptors */
#define IK_FD_TO_NUM(fd)		IK_FIX(fd)
#define IK_NUM_TO_FD(fd)		IK_UNFIX(fd)


/** --------------------------------------------------------------------
 ** Operative system environment variables.
 ** ----------------------------------------------------------------- */

ikptr_t
ikrt_glibc_clearenv (void)
{
#ifdef HAVE_CLEARENV
  clearenv();
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Inspecting file system directories.
 ** ----------------------------------------------------------------- */

ikptr_t
ikrt_glibc_dirfd (ikptr_t pointer)
{
#ifdef HAVE_DIRFD
  DIR *  stream = (DIR *)IK_POINTER_DATA(pointer);
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

ikptr_t
ikrt_glibc_mkstemp (ikptr_t template_bv, ikpcb_t * pcb)
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
ikptr_t
ikrt_glibc_mkdtemp (ikptr_t template_bv, ikpcb_t * pcb)
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

ikptr_t
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
ikptr_t
ikrt_glibc_fsync (ikptr_t fd)
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
ikptr_t
ikrt_glibc_fdatasync (ikptr_t fd)
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

ikptr_t
ikrt_glibc_if_nametoindex (ikptr_t name_bv)
{
#ifdef HAVE_IF_NAMETOINDEX
  char *        name;
  unsigned int  rv;
  name  = IK_BYTEVECTOR_DATA_CHARP(name_bv);
  rv    = if_nametoindex(name);
  return (0 == rv)? IK_FALSE_OBJECT : IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_if_indextoname (ikptr_t index, ikpcb_t * pcb)
{
#ifdef HAVE_IF_INDEXTONAME
  char          buffer[1+IFNAMSIZ];
  unsigned      i = (unsigned)IK_UNFIX(index);
  char *        rv;
  rv = if_indextoname(i, buffer);
  return (rv)? ika_bytevector_from_cstring(pcb, buffer) : IK_FALSE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_if_nameindex (ikpcb_t * pcb)
{
#ifdef HAVE_IF_NAMEINDEX
  struct if_nameindex * arry;
  ikptr_t         s_alist;	/* the first pair in the alist's spine */
  ikptr_t         s_spine;        /* the current pair in the alist's spine */
  int           i;
  arry = if_nameindex();
  {
    s_alist    = s_spine = ika_pair_alloc(pcb);
    pcb->root0 = &s_alist;
    pcb->root1 = &s_spine;
    {
      for (i=0; arry[i].if_index;) {
	IK_ASS(IK_CAR(s_spine), ika_pair_alloc(pcb));
	IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_CAR_PTR(s_spine));
	{
	  ikptr_t	s_entry = IK_CAR(s_spine);
	  pcb->root2 = &s_entry;
	  {
	    IK_CAR(s_entry) = IK_FIX(arry[i].if_index);
	    IK_ASS(IK_CDR(s_entry), ika_bytevector_from_cstring(pcb, arry[i].if_name));
	    IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_CDR_PTR(s_entry));
	  }
	  pcb->root2 = NULL;
	}
	if (arry[++i].if_index) {
	  IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	  IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_CDR_PTR(s_spine));
	  s_spine = IK_CDR(s_spine);
	} else {
	  IK_CDR(s_spine) = IK_NULL_OBJECT;
	}
      }
    }
    pcb->root1 = NULL;
    pcb->root0 = NULL;
  }
  if_freenameindex(arry);
  return s_alist;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Networking.
 ** ----------------------------------------------------------------- */

ikptr_t
ikrt_glibc_IN_CLASSA (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_CLASSA
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_CLASSA(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN_CLASSB (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_CLASSB
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_CLASSB(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN_CLASSC (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_CLASSC
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_CLASSC(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN_CLASSD (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_CLASSD
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_CLASSD(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN_MULTICAST (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_MULTICAST
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_MULTICAST(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN_EXPERIMENTAL (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_EXPERIMENTAL
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_EXPERIMENTAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN_BADCLASS (ikptr_t s_addr, ikpcb_t * pcb)
{
#ifdef HAVE_IN_BADCLASS
  uint32_t	addr = ik_integer_to_uint32(s_addr);
  return IK_BOOLEAN_FROM_INT(IN_BADCLASS(addr));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_IN6_IS_ADDR_UNSPECIFIED (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_UNSPECIFIED
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_UNSPECIFIED(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_LOOPBACK (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_LOOPBACK
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_LOOPBACK(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_LINKLOCAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_LINKLOCAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_LINKLOCAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_SITELOCAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_SITELOCAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_SITELOCAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_V4MAPPED (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_V4MAPPED
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_V4MAPPED(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_V4COMPAT (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_V4COMPAT
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_V4COMPAT(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_MULTICAST (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_MULTICAST
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_MULTICAST(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_MC_NODELOCAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_MC_NODELOCAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_MC_NODELOCAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_MC_LINKLOCAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_MC_LINKLOCAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_MC_LINKLOCAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_MC_SITELOCAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_MC_SITELOCAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_MC_SITELOCAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_MC_ORGLOCAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_MC_ORGLOCAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_MC_ORGLOCAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_IS_ADDR_MC_GLOBAL (ikptr_t s_addr_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_IS_ADDR_MC_GLOBAL
  struct in6_addr *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr_bv);
  return IK_BOOLEAN_FROM_INT(IN6_IS_ADDR_MC_GLOBAL(addr));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_IN6_ARE_ADDR_EQUAL (ikptr_t s_addr1_bv, ikptr_t s_addr2_bv, ikpcb_t * pcb)
{
#ifdef HAVE_IN6_ARE_ADDR_EQUAL
  struct in6_addr *	addr1 = IK_BYTEVECTOR_DATA_VOIDP(s_addr1_bv);
  struct in6_addr *	addr2 = IK_BYTEVECTOR_DATA_VOIDP(s_addr2_bv);
  return IK_BOOLEAN_FROM_INT(IN6_ARE_ADDR_EQUAL(addr1, addr2));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_bindresvport (ikptr_t s_fd, ikptr_t s_sockaddr_in, ikpcb_t * pcb)
{
#ifdef HAVE_BINDRESVPORT
  int			fd	= IK_NUM_TO_FD(s_fd);
  struct sockaddr_in *	addr	= IK_BYTEVECTOR_DATA_VOIDP(s_sockaddr_in);
  int			rv;
  rv = bindresvport(fd, addr);
  return (0 == rv)? IK_FALSE : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_bindresvport6 (ikptr_t s_fd, ikptr_t s_sockaddr_in, ikpcb_t * pcb)
{
#ifdef HAVE_BINDRESVPORT6
  int			fd	= IK_NUM_TO_FD(s_fd);
  struct sockaddr_in6 *	addr	= IK_BYTEVECTOR_DATA_VOIDP(s_sockaddr_in);
  int			rv;
  rv = bindresvport6(fd, addr);
  return (0 == rv)? IK_FALSE : ik_errno_to_code();
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

ikptr_t
ikrt_glibc_csin (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CSIN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = csin(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_ccos (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CCOS
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ccos(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_ctan (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CTAN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ctan(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_casin (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CASIN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = casin(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_cacos (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CACOS
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = cacos(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_catan (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CATAN
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = catan(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_cexp (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CEXP
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = cexp(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_clog (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CLOG
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = clog(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_clog10 (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CLOG10
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = clog10(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_csqrt (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CSQRT
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = csqrt(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_cpow (ikptr_t s_base, ikptr_t s_power, ikpcb_t * pcb)
{
#ifdef HAVE_CPOW
  complex double  base  = MAKE_CDOUBLE(s_base);
  complex double  power = MAKE_CDOUBLE(s_power);
  complex double  Y     = cpow(base, power);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_sinh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_SINH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = sinh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_cosh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_COSH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = cosh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_tanh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_TANH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = tanh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_csinh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CSINH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = csinh(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_ccosh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CCOSH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ccosh(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_ctanh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CTANH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = ctanh(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_asinh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_ASINH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = asinh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_acosh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_ACOSH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = acosh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_atanh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_ATANH
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = atanh(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_casinh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CASINH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = casinh(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_cacosh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CACOSH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = cacosh(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_catanh (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_CATANH
  complex double        X   = MAKE_CDOUBLE(s_X);
  complex double        Y   = catanh(X);
  return iku_cflonum_alloc_and_init(pcb, creal(Y), cimag(Y));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_erf (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_ERF
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = erf(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_erfc (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_ERFC
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = erfc(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_lgamma (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_LGAMMA_R
  double        X   = IK_FLONUM_DATA(s_X);
  int           sgn;
  double        Y   = lgamma_r(X, &sgn);
  ikptr_t         s_pair = ika_pair_alloc(pcb);
  pcb->root0 = &s_pair;
  {
    IK_CAR(s_pair) = iku_flonum_alloc(pcb, Y);
    IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_CAR_PTR(s_pair));
    IK_CDR(s_pair) = IK_FIX(sgn);
  }
  pcb->root0 = NULL;
  return s_pair;
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_tgamma (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_TGAMMA
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = tgamma(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_y0 (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_Y0
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = y0(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_y1 (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_Y1
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = y1(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_j0 (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_J0
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = j0(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_j1 (ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_J1
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = j1(X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_yn (ikptr_t s_N, ikptr_t s_X, ikpcb_t * pcb)
{
#ifdef HAVE_YN
  double        X   = IK_FLONUM_DATA(s_X);
  double        Y   = yn((int)IK_UNFIX(s_N), X);
  return iku_flonum_alloc(pcb, Y);
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_jn (ikptr_t s_N, ikptr_t s_X, ikpcb_t * pcb)
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

ikptr_t
ikrt_glibc_rand (ikpcb_t * pcb)
{
#ifdef HAVE_RAND
  return ika_integer_from_long(pcb, (long)rand());
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_srand (ikptr_t s_seed)
{
#ifdef HAVE_RAND
  srand(ik_integer_to_uint(s_seed));
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Pattern matching, globbing, regular expressions.
 ** ----------------------------------------------------------------- */

ikptr_t
ikrt_glibc_fnmatch (ikptr_t s_pattern, ikptr_t s_string, ikptr_t s_flags)
{
#ifdef HAVE_FNMATCH
  int	rv = fnmatch(IK_BYTEVECTOR_DATA_CHARP(s_pattern),
		     IK_BYTEVECTOR_DATA_CHARP(s_string),
		     IK_UNFIX(s_flags));
  return (rv)? IK_FALSE_OBJECT : IK_TRUE_OBJECT;
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_glob (ikptr_t s_pattern, ikptr_t s_flags, ikptr_t s_error_handler, ikpcb_t * pcb)
{
#ifdef HAVE_GLOB
  typedef int handler_t (const char * filename, int error_code);
  glob_t        G;
  int           rv;
  handler_t     * handler;
  handler = (IK_FALSE_OBJECT == s_error_handler)? NULL : IK_POINTER_DATA_VOIDP(s_error_handler);
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
    ikptr_t       s_list = ika_list_from_argv_and_argc(pcb, G.gl_pathv, G.gl_pathc);
    globfree(&G);
    return s_list;
  } else
    return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_glibc_regcomp (ikptr_t s_pattern, ikptr_t s_flags, ikpcb_t *pcb)
/* Interface  to  the  C  function  "regcomp()".   Compile  the  regular
   expression in S_PATTERN accoding  to S_FLAGS.  If successful return a
   pointer object  referencing the compiled regexp.  If  an error occurs
   allocating memory:  return false.  If  an error occurs  compiling the
   pattern: return  a pair whose car  is a fixnum  representing an error
   code and whose  cdr is a bytevector representing  an error message in
   ASCII encoding.

   S_PATTERN must  be a bytevector representing  the regular expression.
   S_FLAGS must  be a fixnum  resulting from the bitwise  combination of
   REG_ constants.

   The pointer returned  in case of success references  a "regex_t" data
   structure whose  fields must be released explicitly  by "regfree", or
   they are automatically released by the garbage collector whenever the
   pointer itself is collected.  */
{
#ifdef HAVE_REGCOMP
  ikptr_t		s_retval = IK_VOID_OBJECT;
  regex_t *     rex;
  char *        pattern;
  char *	error_message;
  size_t	error_message_len;
  int           rv;
  pcb->root0 = &s_pattern;
  pcb->root1 = &s_retval;
  {
    rex = (regex_t *)malloc(sizeof(regex_t));
    if (rex) {
      pattern = IK_BYTEVECTOR_DATA_CHARP(s_pattern);
      rv      = regcomp(rex, pattern, IK_UNFIX(s_flags));
      if (0 == rv) {
	s_retval = ika_pointer_alloc(pcb, (ikuword_t)rex);
      } else {
	s_retval	  = ika_pair_alloc(pcb);
	error_message_len = regerror(rv, rex, NULL, 0);
	IK_CAR(s_retval)  = IK_FIX(rv);
	IK_ASS(IK_CDR(s_retval), ika_bytevector_alloc(pcb, (ikuword_t)(error_message_len-1)));
	IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_CDR_PTR(s_retval));
	error_message     = IK_BYTEVECTOR_DATA_CHARP(IK_CDR(s_retval));
	regerror(rv, rex, error_message, error_message_len);
	regfree(rex);
	free(rex);
      }
    } else
      s_retval = IK_FALSE_OBJECT; /* error allocating memory */
  }
  pcb->root1 = NULL;
  pcb->root0 = NULL;
  return s_retval;
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_regexec (ikptr_t s_rex, ikptr_t s_string, ikptr_t s_flags, ikpcb_t *pcb)
/* Interface to  the C function "regexec()".  Attempt  to match S_STRING
   against  the  precompiled   regular  expression  S_REX  according  to
   S_FLAGS.

   If one or more matches occur return a vector holding pairs describing
   the portions  of S_STRING that did  match; if no  match occurs return
   false;  if an  error occurs:  return  a pair  whose car  is a  fixnum
   representing an error code and whose cdr is a bytevector representing
   an error message in ASCII encoding.

   The vector returned in case  of success contains pairs: the car being
   a fixnum representing  the starting offset of a  match substring, the
   cdr  being  a  fixnum  representing  the ending  offset  of  a  match
   substring.

   The  vector element  at index  0 represents  the portion  of S_STRING
   which  matched the whole  regular expression;  the vector  element at
   index 1  represents the portion  of S_STRING which matched  the first
   parenthetical subexpression, the vector element at index 2 represents
   the  portion  of  S_STRING  which matched  the  second  parenthetical
   subexpression, and  so on.  If S_STRING matches:  the returned vector
   has at least one element. */
{
#ifdef HAVE_REGCOMP
  regex_t *     rex	= IK_POINTER_DATA_VOIDP(s_rex);
  char *        string	= IK_BYTEVECTOR_DATA_CHARP(s_string);
  size_t        nmatch	= rex->re_nsub;
  regmatch_t    match[1+nmatch];
  int           rv;
  rv = regexec(rex, string, 1+nmatch, match, IK_UNFIX(s_flags));
  switch (rv) {
  case 0:
    {
      size_t      i;
      ikptr_t       s_pair = IK_VOID_OBJECT;
      ikptr_t       s_match_vector = ika_vector_alloc_and_init(pcb, 1+nmatch);
      pcb->root0 = &s_match_vector;
      pcb->root1 = &s_pair;
      {
        for (i=0; i<1+nmatch; ++i) {
	  s_pair         = IKA_PAIR_ALLOC(pcb);
	  /* No need to  update the dirty vector  about "s_pair" because
	     the values are fixnums. */
          IK_CAR(s_pair) = IK_FIX(match[i].rm_so);
          IK_CDR(s_pair) = IK_FIX(match[i].rm_eo);
          IK_ITEM(s_match_vector, i) = s_pair;
	  IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_ITEM_PTR(s_match_vector, i));
        }
      }
      pcb->root1 = NULL;
      pcb->root0 = NULL;
      return s_match_vector;
    }
  case REG_NOMATCH:
    return IK_FALSE_OBJECT;
  default:
    {
      ikptr_t	s_pair, s_error_code, s_error_msg;
      char *	errmsg;
      size_t	errmsg_len_including_zero;
      errmsg_len_including_zero = regerror(rv, rex, NULL, 0);
      s_error_code = IK_FIX(rv);
      s_error_msg  = ika_bytevector_alloc(pcb, (ikuword_t)(errmsg_len_including_zero-1));
      errmsg       = IK_BYTEVECTOR_DATA_CHARP(s_error_msg);
      regerror(rv, rex, errmsg, errmsg_len_including_zero);
      pcb->root0 = &s_error_msg;
      {
	s_pair = IKA_PAIR_ALLOC(pcb);
	/* There  should be  no need  to update  the dirty  vector about
	   "s_pair" here, because we have allocated "s_error_msg" first.
	   But  I want  to  be supercareful  until  I really  understand
	   everything about  garbage collection.  (Marco Maggi;  Sun Dec
	   15, 2013) */
	IK_CAR(s_pair) = s_error_code;
	IK_CDR(s_pair) = s_error_msg;
	IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_CDR_PTR(s_pair));
      }
      pcb->root0 = NULL;
      return s_pair;
    }
  }
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_regfree (ikptr_t s_rex)
/* Free the compiled regex in the given bytevector, but only if at least
   one byte in  the bytevector is non-zero, else it  means that the data
   structure has already been freed. */
{
#ifdef HAVE_REGCOMP
  regex_t *	rex = IK_POINTER_DATA_VOIDP(s_rex);
  if (rex) {
    regfree(rex);
    free(rex);
    IK_POINTER_SET_NULL(s_rex);
  }
  return IK_VOID_OBJECT;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Performing word expansion.
 ** ----------------------------------------------------------------- */

ikptr_t
ikrt_glibc_wordexp (ikptr_t s_pattern, ikptr_t s_flags, ikpcb_t * pcb)
{
#ifdef HAVE_WORDEXP
  ikptr_t		s_words;
  char *        pattern = IK_BYTEVECTOR_DATA_VOIDP(s_pattern);
  wordexp_t     W;
  int		i;
  int           rv;
  W.we_wordc    = 0;
  W.we_wordv    = NULL;
  W.we_offs     = 0;
  rv = wordexp(pattern, &W, IK_UNFIX(s_flags));
  if (0 == rv) {
    s_words    = ika_vector_alloc_and_init(pcb, (ikuword_t)W.we_wordc);
    pcb->root0 = &s_words;
    {
      for (i=0; i<W.we_wordc; ++i) {
        IK_ASS(IK_ITEM(s_words, i), ika_bytevector_from_cstring(pcb, W.we_wordv[i]));
	IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, IK_ITEM_PTR(s_words, i));
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
 ** Generic character set conversion.
 ** ----------------------------------------------------------------- */

ikptr_t
ikrt_glibc_iconv_open (ikptr_t s_from_code, ikptr_t s_to_code, ikpcb_t * pcb)
/* Open  a  new  conversion  handle.   S_TO_CODE must  be  a  bytevector
   representing the name of the  output encoding.  S_FROM_CODE must be a
   bytevector  representing   the  name  of  the   input  encoding.   If
   successful:  the return  value is  a pointer  object  referencing the
   state structure; if  an error occurs: the return  value is an encoded
   "errno" value. */
{
#if ((defined HAVE_ICONV) && (1 == VICARE_HAVE_ICONV))
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
    return ika_pointer_alloc(pcb, (ikuword_t)handle);
  else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr_t
ikrt_glibc_iconv_close (ikptr_t s_handle)
/* Close a  conversion handle,  releasing all the  associated resources.
   the handle is finalised only if S_HANDLE is a non-NULL pointer; if it
   is  a   NULL  pointer  nothing  happens  and   the  function  returns
   successfully.  If successful: the return value is the fixnum zero and
   S_HANLE is mutated to hold the  NULL pointer; if an error occurs: the
   return value is an encoded "errno" value. */
{
#if ((defined HAVE_ICONV) && (1 == VICARE_HAVE_ICONV))
  iconv_t	handle = (iconv_t)IK_POINTER_DATA_VOIDP(s_handle);
  if (handle) {
    int		retval;
    errno  = 0;
    retval = iconv_close(handle);
    if (-1 == retval)
      return ik_errno_to_code();
    IK_POINTER_SET_NULL(s_handle);
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
ikptr_t
ikrt_glibc_iconv (ikptr_t s_handle,
		  ikptr_t s_in_bv,  ikptr_t s_in_start,  ikptr_t s_in_past,
		  ikptr_t s_out_bv, ikptr_t s_out_start, ikptr_t s_out_past,
		  ikpcb_t * pcb)
{
#if ((defined HAVE_ICONV) && (1 == VICARE_HAVE_ICONV))
  iconv_t	handle = (iconv_t)IK_POINTER_DATA_VOIDP(s_handle);
  size_t	istart = IK_UNFIX(s_in_start);
  size_t	ipast  = (IK_FALSE_OBJECT == s_in_past)? \
    IK_BYTEVECTOR_LENGTH(s_in_bv) : IK_UNFIX(s_in_past);
  size_t	ostart = IK_UNFIX(s_out_start);
  size_t	opast  =  (IK_FALSE_OBJECT == s_out_past)? \
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
    ikptr_t	s_pair = IKA_PAIR_ALLOC(pcb);
    istart = ipast - isize;
    ostart = opast - osize;
    /* No need  to update  the dirty vector  about "s_pair"  because the
       values are fixnums. */
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
