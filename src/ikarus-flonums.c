/*
 * Ikarus Scheme -- A compiler for R6RS Scheme.
 * Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 * Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
 *
 * This program is free software:  you can redistribute it and/or modify
 * it under  the terms of  the GNU General  Public License version  3 as
 * published by the Free Software Foundation.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
 * General Public License for more details.
 *
 * You should  have received  a copy of  the GNU General  Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"
#include <math.h>


/** --------------------------------------------------------------------
 ** Allocating flonums and cflonums.
 ** ----------------------------------------------------------------- */

ikptr
iku_flonum_alloc (ikpcb * pcb, double fl)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(F);
  IK_FLONUM_DATA(F) = fl;
  return F;
}
ikptr
iku_cflonum_alloc_and_init (ikpcb * pcb, double re, double im)
{
  IKU_DEFINE_AND_ALLOC_CFLONUM(F);
  IK_CFLONUM_REAL(F) = iku_flonum_alloc(pcb, re);
  IK_CFLONUM_IMAG(F) = iku_flonum_alloc(pcb, im);
  return F;
}

/* ------------------------------------------------------------------ */

int
ik_is_flonum (ikptr obj)
{
  return ((vector_tag == IK_TAGOF(obj)) &&
	  (flonum_tag == IK_REF(obj, -vector_tag)));
}
int
ik_is_cflonum (ikptr X)
{
  return ((vector_tag == IK_TAGOF(X)) &&
	  (cflonum_tag == IK_REF(X, -vector_tag)));
}


/** --------------------------------------------------------------------
 ** Flonum functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_fl_round (ikptr x, ikptr y)
{
  /* To avoid a warning with GCC one must either
     - invoke with -std=c99, or
     - declare "extern double round(double);", or
     - use the pre-C99 floor() and ceil(),

     double xx = IK_FLONUM_DATA(x);
     IK_FLONUM_DATA(y) = (xx>=0) ? floor(xx+0.5) : ceil(xx-0.5);

     The last of these seems most portable. (Barak A. Pearlmutter) */
#if 1
  /* IK_FLONUM_DATA(y) = rint(IK_FLONUM_DATA(x)); */
  IK_FLONUM_DATA(y) = round(IK_FLONUM_DATA(x));
#else
  double xx = IK_FLONUM_DATA(x);
  IK_FLONUM_DATA(y) = (xx>=0) ? floor(xx+0.5) : ceil(xx-0.5);
#endif
  return y;
}
ikptr
ikrt_fl_exp (ikptr x, ikptr y)
{
  IK_FLONUM_DATA(y) = exp(IK_FLONUM_DATA(x));
  return y;
}
ikptr
ikrt_fl_expm1 (ikptr x, ikptr y)
{
  IK_FLONUM_DATA(y) = expm1(IK_FLONUM_DATA(x));
  return y;
}
ikptr
ikrt_flfl_expt (ikptr a, ikptr b, ikptr z)
{
  IK_FLONUM_DATA(z) = exp(IK_FLONUM_DATA(b) * log(IK_FLONUM_DATA(a)));
  return z;
}
ikptr
ikrt_bytevector_to_flonum (ikptr x, ikpcb* pcb)
{
  char *        data = IK_BYTEVECTOR_DATA_CHARP(x);
  double        v    = strtod(data, NULL);
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = v;
  return r;
}
ikptr
ikrt_fl_plus (ikptr x, ikptr y, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = IK_FLONUM_DATA(x) + IK_FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_minus (ikptr x, ikptr y, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = IK_FLONUM_DATA(x) - IK_FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_times (ikptr x, ikptr y, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = IK_FLONUM_DATA(x) * IK_FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_div (ikptr x, ikptr y, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = IK_FLONUM_DATA(x) / IK_FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_invert (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = 1.0 / IK_FLONUM_DATA(x);
  return r;
}
ikptr
ikrt_fl_sin (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = sin(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_cos (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = cos(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_tan (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = tan(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_asin (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = asin(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_acos (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = acos(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_atan (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = atan(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_atan2 (ikptr y, ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = atan2(IK_FLONUM_DATA(y), IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_sqrt (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = sqrt(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_log (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = log(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_log1p (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = log1p(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fx_sin (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = sin(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fx_cos (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = cos(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fx_tan (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = tan(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fx_asin (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = asin(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fx_acos (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = acos(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fx_atan (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = atan(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fl_sinh (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = sinh(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_cosh (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = cosh(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_tanh (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = tanh(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_asinh (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = asinh(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_acosh (ikptr x, ikpcb* pcb) {
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = acosh(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_atanh (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = atanh(IK_FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fx_sqrt (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = sqrt(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fx_log (ikptr x, ikpcb* pcb)
{
  IKU_DEFINE_AND_ALLOC_FLONUM(r);
  IK_FLONUM_DATA(r) = log(IK_UNFIX(x));
  return r;
}
ikptr
ikrt_fixnum_to_flonum (ikptr x, ikptr r)
{
  IK_FLONUM_DATA(r) = IK_UNFIX(x);
  return r;
}
ikptr
ikrt_fl_equal (ikptr x, ikptr y)
{
  return (IK_FLONUM_DATA(x) == IK_FLONUM_DATA(y))? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_fl_less_or_equal (ikptr x, ikptr y)
{
  return (IK_FLONUM_DATA(x) <= IK_FLONUM_DATA(y))? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}
ikptr
ikrt_fl_less (ikptr x, ikptr y) {
  return (IK_FLONUM_DATA(x) < IK_FLONUM_DATA(y))? IK_TRUE_OBJECT : IK_FALSE_OBJECT;
}

/* end of file */
