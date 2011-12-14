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

#define _ISOC99_SOURCE
#include "ikarus.h"
#include <math.h>


/** --------------------------------------------------------------------
 ** Allocating flonums and cflonums.
 ** ----------------------------------------------------------------- */

#define DEFINE_AND_ALLOC_FLONUM(r)                              \
  ikptr r = ik_unsafe_alloc(pcb, flonum_size) | vector_tag;     \
  ref(r, -vector_tag) = (ikptr)flonum_tag

ikptr
ik_flonum_alloc (ikpcb * pcb, double fl)
{
  DEFINE_AND_ALLOC_FLONUM(F);
  FLONUM_DATA(F) = fl;
  return F;
}

/* ------------------------------------------------------------------ */

#define DEFINE_AND_ALLOC_CFLONUM(r)                              \
  ikptr r = ik_unsafe_alloc(pcb, cflonum_size) | vector_tag;     \
  ref(r, -vector_tag) = (ikptr)cflonum_tag

ikptr
ik_cflonum_alloc (ikpcb * pcb, double re, double im)
{
  DEFINE_AND_ALLOC_CFLONUM(F);
  CFLONUM_DATA_REAL(F) = re;
  CFLONUM_DATA_IMAG(F) = im;
  return F;
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

     double xx = FLONUM_DATA(x);
     FLONUM_DATA(y) = (xx>=0) ? floor(xx+0.5) : ceil(xx-0.5);

     The last of these seems most portable. (Barak A. Pearlmutter) */
#if 1
  /* FLONUM_DATA(y) = rint(FLONUM_DATA(x)); */
  FLONUM_DATA(y) = round(FLONUM_DATA(x));
#else
  double xx = FLONUM_DATA(x);
  FLONUM_DATA(y) = (xx>=0) ? floor(xx+0.5) : ceil(xx-0.5);
#endif
  return y;
}
ikptr
ikrt_fl_exp (ikptr x, ikptr y)
{
  FLONUM_DATA(y) = exp(FLONUM_DATA(x));
  return y;
}
ikptr
ikrt_fl_expm1 (ikptr x, ikptr y)
{
  FLONUM_DATA(y) = expm1(FLONUM_DATA(x));
  return y;
}
ikptr
ikrt_flfl_expt (ikptr a, ikptr b, ikptr z)
{
  FLONUM_DATA(z) = exp(FLONUM_DATA(b) * log(FLONUM_DATA(a)));
  return z;
}
ikptr
ikrt_bytevector_to_flonum (ikptr x, ikpcb* pcb)
{
  char *        data = IK_BYTEVECTOR_DATA_CHARP(x);
  double        v    = strtod(data, NULL);
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = v;
  return r;
}
ikptr
ikrt_fl_plus (ikptr x, ikptr y,ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = FLONUM_DATA(x) + FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_minus (ikptr x, ikptr y,ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = FLONUM_DATA(x) - FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_times (ikptr x, ikptr y,ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = FLONUM_DATA(x) * FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_div (ikptr x, ikptr y,ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = FLONUM_DATA(x) / FLONUM_DATA(y);
  return r;
}
ikptr
ikrt_fl_invert (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = 1.0 / FLONUM_DATA(x);
  return r;
}
ikptr
ikrt_fl_sin (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = sin(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_cos (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = cos(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_tan (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = tan(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_asin (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = asin(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_acos (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = acos(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_atan (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = atan(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_atan2 (ikptr y, ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = atan2(FLONUM_DATA(y), FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_sqrt (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = sqrt(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_log (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = log(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_log1p (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = log1p(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fx_sin (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = sin(unfix(x));
  return r;
}
ikptr
ikrt_fx_cos (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = cos(unfix(x));
  return r;
}
ikptr
ikrt_fx_tan (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = tan(unfix(x));
  return r;
}
ikptr
ikrt_fx_asin (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = asin(unfix(x));
  return r;
}
ikptr
ikrt_fx_acos (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = acos(unfix(x));
  return r;
}
ikptr
ikrt_fx_atan (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = atan(unfix(x));
  return r;
}
ikptr
ikrt_fl_sinh (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = sinh(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_cosh (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = cosh(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_tanh (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = tanh(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_asinh (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = asinh(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_acosh (ikptr x, ikpcb* pcb) {
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = acosh(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fl_atanh (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = atanh(FLONUM_DATA(x));
  return r;
}
ikptr
ikrt_fx_sqrt (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = sqrt(unfix(x));
  return r;
}
ikptr
ikrt_fx_log (ikptr x, ikpcb* pcb)
{
  DEFINE_AND_ALLOC_FLONUM(r);
  FLONUM_DATA(r) = log(unfix(x));
  return r;
}
ikptr
ikrt_fixnum_to_flonum (ikptr x, ikptr r)
{
  FLONUM_DATA(r) = unfix(x);
  return r;
}
ikptr
ikrt_fl_equal (ikptr x, ikptr y)
{
  return (FLONUM_DATA(x) == FLONUM_DATA(y))? true_object : false_object;
}
ikptr
ikrt_fl_less_or_equal (ikptr x, ikptr y)
{
  return (FLONUM_DATA(x) <= FLONUM_DATA(y))? true_object : false_object;
}
ikptr
ikrt_fl_less (ikptr x, ikptr y) {
  return (FLONUM_DATA(x) < FLONUM_DATA(y))? true_object : false_object;
}

/* end of file */
