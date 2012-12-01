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
#include <gmp.h>

#define BN(x)		DEBUG_VERIFY_BIGNUM(x,"BN")


/** --------------------------------------------------------------------
 ** Debugging helpers.
 ** ----------------------------------------------------------------- */

#if 0
#define DEBUG_VERIFY_BIGNUM(x,caller)		(x)
#else
static ikptr
DEBUG_VERIFY_BIGNUM (ikptr x, char* caller)
/* Validate the bignum X, which must be a tagged reference. */
{
  ikptr		first_word;
  long		limb_count;
  int		is_positive;
  mp_limb_t	last_limb;
  if (IK_TAGOF(x) != vector_tag)
    ik_abort("error in (%s) invalid primary tag 0x%016lx", caller, x);
  first_word = IK_REF(x, off_bignum_tag);
  limb_count = IK_BNFST_LIMB_COUNT(first_word);
  if (limb_count <= 0)
    ik_abort("error in (%s) invalid limb count in first_word=0x%016lx", caller, (long)first_word);
  is_positive = IK_BNFST_NEGATIVE(first_word)? 0 : 1;
  last_limb   = IK_BIGNUM_LAST_LIMB(x, limb_count);
  if (last_limb == 0)
    ik_abort("error in (%s) invalid last limb = 0x%016lx", caller, last_limb);
  if (limb_count == 1) {
    if (is_positive) {
      if (last_limb <= most_positive_fixnum)
	ik_abort("error in (%s) should be a positive fixnum: 0x%016lx", caller, last_limb);
    } else {
      if (last_limb <= most_negative_fixnum)
	ik_abort("error in (%s) should be a negative fixnum: 0x%016lx", caller, last_limb);
    }
  }
  /* ok */
  return x;
}
#endif


/** --------------------------------------------------------------------
 ** Inspection.
 ** ----------------------------------------------------------------- */

int
ik_is_bignum (ikptr x)
{
  return ((vector_tag == IK_TAGOF(x)) &&
	  (bignum_tag == (bignum_mask & (int)IK_REF(x, -vector_tag))));
}
ikptr
ikrt_positive_bn (ikptr x)
{
  ikptr first_word = ref(x, -vector_tag);
  return (IK_BNFST_NEGATIVE(first_word))? IK_FALSE_OBJECT : IK_TRUE_OBJECT;
}
ikptr
ikrt_even_bn (ikptr x)
{
  mp_limb_t first_limb = IK_BIGNUM_FIRST_LIMB(x);
  return (first_limb & 1)? IK_FALSE_OBJECT : IK_TRUE_OBJECT;
}


/** --------------------------------------------------------------------
 ** Arithmetics: addition.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_fxfxplus (ikptr x, ikptr y, ikpcb* pcb)
{
  long	n1 = IK_UNFIX(x);
  long	n2 = IK_UNFIX(y);
  long	R  = n1 + n2;
  ikptr	Q  = IK_FIX(R);
  if (R == IK_UNFIX(Q)) {
    return Q;
  } else {
    ikptr s_bn = IKA_BIGNUM_ALLOC(pcb, 1);
    if (R > 0) {
      IK_REF(s_bn, off_bignum_tag)  = IK_POSITIVE_BIGNUM_FIRST_WORD(1);
      IK_REF(s_bn, off_bignum_data) = (ikptr)+R;
    } else {
      IK_REF(s_bn, off_bignum_tag)  = IK_NEGATIVE_BIGNUM_FIRST_WORD(1);
      IK_REF(s_bn, off_bignum_data) = (ikptr)-R;
    }
    return DEBUG_VERIFY_BIGNUM(s_bn, "fxfx+");
  }
}
ikptr
ikrt_fxbnplus (ikptr x, ikptr y, ikpcb* pcb)
{
  /* If X is the fixnum zero: just return Y. */
  if (x == 0) {
    return y ;
  }
  ikptr	first_word	= IK_REF(y, -vector_tag);
  long	limb_count	= IK_BNFST_LIMB_COUNT(first_word);
  long	intx		= IK_UNFIX(x);
  if (intx > 0) {
    if (IK_BNFST_POSITIVE(first_word)) {
      /* positive fx + positive bn = even bigger positive */
      ikptr	r;
      mp_limb_t	carry;
      pcb->root0 = &y;
      {
	/* We may allocate one limb more than needed here if CARRY below
	   results  zero.  We  accept  it because  we  must perform  the
	   operation before knowing if the CARRY is non-zero. */
	r = IKA_BIGNUM_ALLOC(pcb, limb_count + 1);
      }
      pcb->root0 = 0;
      carry = mpn_add_1(IK_BIGNUM_DATA_LIMBP(r), IK_BIGNUM_DATA_LIMBP(y), limb_count, intx);
      if (carry) {
	IK_LIMB(r, limb_count) = (ikptr)1;
	IK_BIGNUM_FIRST(r)            = IK_POSITIVE_BIGNUM_FIRST_WORD(limb_count + 1);
	return DEBUG_VERIFY_BIGNUM(r, "fxbn+1");
      } else {
	IK_BIGNUM_FIRST(r) = IK_POSITIVE_BIGNUM_FIRST_WORD(limb_count);
	return DEBUG_VERIFY_BIGNUM(r, "fxbn+2");
      }
    } else {
      /* positive fx + negative bn = smaller negative bn */
      ikptr	r;
      mp_limb_t borrow;
      long	result_size;
      pcb->root0 = &y;
      {
	r = IKA_BIGNUM_ALLOC(pcb, limb_count);
      }
      pcb->root0 = 0;
      borrow = mpn_sub_1(IK_BIGNUM_DATA_LIMBP(r), IK_BIGNUM_DATA_LIMBP(y), limb_count, intx);
      if (borrow)
	ik_abort("BUG in borrow1 %ld", borrow);
      result_size = IK_BIGNUM_LAST_LIMB(r, limb_count)? limb_count : (limb_count - 1);
      if (0 == result_size) {
	return 0; /* the fixnum zero */
      } else {
	if (1 == result_size) {
	  mp_limb_t last = IK_BIGNUM_LAST_LIMB(r, result_size);
	  if (last <= most_negative_fixnum)
	    return IK_FIX(-(long)last);
	}
	IK_BIGNUM_FIRST(r) = IK_NEGATIVE_BIGNUM_FIRST_WORD(result_size);
	return DEBUG_VERIFY_BIGNUM(r, "fxbn+3");
      }
    }
  } else {
    if (IK_BNFST_POSITIVE(first_word)) {
      /* negative fx + positive bn = smaller positive fx or bn */
      ikptr	r;
      mp_limb_t borrow;
      long	result_size;
      pcb->root0 = &y;
      {
	r = IKA_BIGNUM_ALLOC(pcb, limb_count);
      }
      pcb->root0 = 0;
      borrow = mpn_sub_1(IK_BIGNUM_DATA_LIMBP(r), IK_BIGNUM_DATA_LIMBP(y), limb_count, -intx);
      if (borrow)
	ik_abort("BUG in borrow2\n");
      result_size = (0 == IK_BIGNUM_LAST_LIMB(r, limb_count))? (limb_count - 1) : limb_count;
      if (result_size == 0) {
	return 0;
      } else {
	if (1 == result_size) {
	  mp_limb_t last = IK_BIGNUM_LAST_LIMB(r, result_size);
	  if (last <= most_positive_fixnum)
	    return IK_FIX(last);
	}
	IK_BIGNUM_FIRST(r) = IK_POSITIVE_BIGNUM_FIRST_WORD(result_size);
	return DEBUG_VERIFY_BIGNUM(r, "fxbn+4");
      }
    } else {
      /* negative fx + negative bn = larger negative bn */
      ikptr	r;
      mp_limb_t carry;
      pcb->root0 = &y;
      {
	r = IKA_BIGNUM_ALLOC(pcb, 1 + limb_count);
      }
      pcb->root0 = 0;
      carry = mpn_add_1(IK_BIGNUM_DATA_LIMBP(r), IK_BIGNUM_DATA_LIMBP(y), limb_count, -intx);
      if (carry) {
	IK_LIMB(r, limb_count) = (ikptr)1;
	IK_BIGNUM_FIRST(r) = IK_NEGATIVE_BIGNUM_FIRST_WORD(limb_count + 1);
	return DEBUG_VERIFY_BIGNUM(r, "fxbn+5");
      } else {
	IK_BIGNUM_FIRST(r) = IK_NEGATIVE_BIGNUM_FIRST_WORD(limb_count);
	return DEBUG_VERIFY_BIGNUM(r, "fxbn+6");
      }
    }
  }
}
ikptr
ikrt_bnbnplus (ikptr x, ikptr y, ikpcb* pcb)
/* Depending on the sign of the operands we do a different operation:

   X>0 Y>0 => RES = X + Y
   X<0 Y<0 => RES = -(|X| + |Y|)
   X>0 Y<0 => RES = X - |Y|
   X<0 Y>0 => RES = Y - |X|

*/
{
  ik_ulong	xfst   = (ik_ulong)IK_BIGNUM_FIRST(x);
  ik_ulong	yfst   = (ik_ulong)IK_BIGNUM_FIRST(y);
  long		xsign  = xfst & bignum_sign_mask;
  long		ysign  = yfst & bignum_sign_mask;
  long		xlimbs = xfst >> bignum_nlimbs_shift;
  long		ylimbs = yfst >> bignum_nlimbs_shift;
  if (xsign == ysign) { /* bignums of equal sign */
    ikptr	res;	/* return value */
    ikptr	bn1;	/* bignum with greater number of limbs */
    ikptr	bn2;	/* bignum with lesser number of limbs */
    long	nlimb1;	/* number of limbs in BN1 */
    long	nlimb2;	/* number of limbs in BN2 */
    mp_limb_t	carry;
    if (xlimbs > ylimbs) {
      nlimb1 = xlimbs;
      nlimb2 = ylimbs;
      bn1 = x;
      bn2 = y;
    } else {
      nlimb1 = ylimbs;
      nlimb2 = xlimbs;
      bn1 = y;
      bn2 = x;
    }
    pcb->root0 = &bn1;
    pcb->root1 = &bn2;
    {
      res = IKA_BIGNUM_ALLOC(pcb, 1 + nlimb1);
    }
    pcb->root1 = NULL;
    pcb->root0 = NULL;
    carry = mpn_add(IK_BIGNUM_DATA_LIMBP(res),
		    IK_BIGNUM_DATA_LIMBP(bn1), nlimb1,
		    IK_BIGNUM_DATA_LIMBP(bn2), nlimb2);
    if (carry) {
      IK_LIMB(res, xlimbs) = (ikptr)1;
      IK_BIGNUM_FIRST(res) = IK_COMPOSE_BIGNUM_FIRST_WORD(1 + nlimb1, xsign);
      return DEBUG_VERIFY_BIGNUM(res, "bnbn+1");
    } else {
      IK_BIGNUM_FIRST(res) = IK_COMPOSE_BIGNUM_FIRST_WORD(nlimb1, xsign);
      return DEBUG_VERIFY_BIGNUM(res, "bnbn+2");
    }
  } else { /* bignums of different sign */
    ikptr	res;				/* the return value */
    ikptr	bn1		= x;
    ikptr	bn2		= y;
    long	nlimb1		= xlimbs;
    long	nlimb2		= ylimbs;
    long	len;
    long	result_sign	= xsign;
    mp_limb_t	burrow;
    /* If the limbs are equal ther result is zero. */
    while ((xlimbs == ylimbs) && (IK_LIMB(x, xlimbs - 1) == IK_LIMB(y, xlimbs - 1))) {
      xlimbs -= 1;
      ylimbs -= 1;
      if (0 == xlimbs)
	return 0; /* the fixnum zero */
    }
    /* |x| != |y| */
    if (xlimbs <= ylimbs) {
      if (xlimbs == ylimbs) {
	if (IK_LIMB(y, xlimbs - 1) > IK_LIMB(x, xlimbs - 1)) {
	  bn1		= y;
	  nlimb1	= ylimbs;
	  bn2		= x;
	  nlimb2	= xlimbs;
	  result_sign	= ysign;
	}
      } else {
	bn1		= y;
	nlimb1		= ylimbs;
	bn2		= x;
	nlimb2		= xlimbs;
	result_sign	= ysign;
      }
    }
    /* |bn1| > |bn2| */
    pcb->root0 = &bn1;
    pcb->root1 = &bn2;
    {
      res = IKA_BIGNUM_ALLOC(pcb, nlimb1);
    }
    pcb->root0 = 0;
    pcb->root1 = 0;
    burrow = mpn_sub(IK_BIGNUM_DATA_LIMBP(res),
		     IK_BIGNUM_DATA_LIMBP(bn1), nlimb1,
		     IK_BIGNUM_DATA_LIMBP(bn2), nlimb2);
    if (burrow)
      ik_abort("bug: burrow error in bnbn+");
    for (len = nlimb1; 0 == IK_LIMB(res, len - 1);) {
      --len;
      if (0 == len)
	return 0; /* the fixnum zero */
    }
    if (0 == result_sign) {
      /* positive result */
      if (1 == len) {
	mp_limb_t first_limb = IK_BIGNUM_FIRST_LIMB(res);
	if (first_limb <= most_positive_fixnum) {
	  return IK_FIX((long)first_limb);
	}
      }
      IK_BIGNUM_FIRST(res) = IK_COMPOSE_BIGNUM_FIRST_WORD(len, result_sign);
      return DEBUG_VERIFY_BIGNUM(res, "bnbn+3");
    } else {
      /* negative result */
      if (len == 1) {
	mp_limb_t first_limb = IK_BIGNUM_FIRST_LIMB(res);
	if (first_limb <= most_negative_fixnum)
	  return IK_FIX(-(long)first_limb);
      }
      IK_BIGNUM_FIRST(res) = IK_COMPOSE_BIGNUM_FIRST_WORD(len, result_sign);
      return DEBUG_VERIFY_BIGNUM(res, "bnbn+4");
    }
  }
}


/** --------------------------------------------------------------------
 ** Arithmetics: subtraction.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_fxfxminus (ikptr x, ikptr y, ikpcb* pcb)
{
  long n1 = IK_UNFIX(x);
  long n2 = IK_UNFIX(y);
  long r = n1 - n2;
  if (r >= 0) {
    if (((ik_ulong)r) <= most_positive_fixnum) {
      return IK_FIX(r);
    } else {
      ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikptr) (bignum_tag | (1 << bignum_nlimbs_shift));
      ref(bn, disp_bignum_data) = (ikptr)r;
      return DEBUG_VERIFY_BIGNUM(bn+vector_tag,"fxfx-1");
    }
  } else {
    ikptr fxr = IK_FIX(r);
    if (IK_UNFIX(fxr) == r) {
      return fxr;
    } else {
      ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikptr)
	(bignum_tag |
	 (1 << bignum_sign_shift) |
	 (1 << bignum_nlimbs_shift));
      ref(bn, disp_bignum_data) = (ikptr)(-r);
      return DEBUG_VERIFY_BIGNUM(bn+vector_tag, "fxfx-2");
    }
  }
}
ikptr
ikrt_fxbnminus (ikptr x, ikptr y, ikpcb* pcb)
{
  /* If the fixnum X is zero: just return Y negated. */
  if (0 == x) {
    return ikrt_bnnegate(y, pcb);
  }
  ikptr	first_word	= ref(y, -vector_tag);
  long	limb_count	= IK_BNFST_LIMB_COUNT(first_word);
  long	intx		= IK_UNFIX(x);
  if (intx > 0) {
    if (IK_BNFST_NEGATIVE(first_word)) {
      ikptr	r;
      long	carry;
      /* positive fx - negative bn = positive bn */
      pcb->root0 = &y;
      {
	r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      }
      pcb->root0 = 0;
      carry = mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
			(mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
			limb_count, intx);
      if (carry) {
	ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
	ref(r, 0) = IK_POSITIVE_BIGNUM_FIRST_WORD((limb_count + 1));
	return DEBUG_VERIFY_BIGNUM(r|vector_tag, "fxbn-1");
      } else {
	ref(r, 0) = IK_POSITIVE_BIGNUM_FIRST_WORD(limb_count);
	return DEBUG_VERIFY_BIGNUM(r|vector_tag, "fxbn-2");
      }
    } else {
      ikptr	r;
      long	borrow;
      long	result_size;
      /* positive fx - positive bn = smaller negative bn/fx */
      pcb->root0 = &y;
      {
	r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      }
      pcb->root0 = 0;
      borrow = mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
			 (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
			 limb_count, intx);
      if (borrow)
	ik_abort("BUG in borrow3\n");
      result_size = (ref(r, disp_bignum_data + (limb_count-1)*wordsize))?
	limb_count : (limb_count - 1);
      if (result_size == 0) {
	return 0; /* the fixnum zero */
      } else {
	if (1 == result_size) {
	  ik_ulong last = (ik_ulong) ref(r, disp_bignum_data + (result_size-1)*wordsize);
	  if (last <= most_negative_fixnum)
	    return IK_FIX(-(long)last);
	}
	ref(r, 0) = (ikptr) ((result_size << bignum_nlimbs_shift)
			     | (1 << bignum_sign_shift)
			     | bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag, "fxbn-");
      }
    }
  } else {
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* negative fx - negative bn = smaller positive */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long borrow =
	mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
		  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
		  limb_count,
		  - intx);
      if (borrow)
	ik_abort("BUG in borrow4");
      long result_size =
	(ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0)
	? (limb_count - 1)
	: limb_count;
      if (result_size == 0) {
	return 0;
      }
      if (1 == result_size) {
	ik_ulong last =
	  (ik_ulong) ref(r, disp_bignum_data + (result_size-1)*wordsize);
	if (last <= most_positive_fixnum) {
	  return IK_FIX((long)last);
	}
      }
      ref(r, 0) = (ikptr)
	((result_size << bignum_nlimbs_shift) |
	 (0 << bignum_sign_shift) |
	 bignum_tag);
      return DEBUG_VERIFY_BIGNUM(r+vector_tag,"fxbn-");
    } else {
      /* negative fx - positive bn = larger negative */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long carry =
	mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
		  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
		  limb_count,
		  -intx);
      if (carry) {
	ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
	ref(r, 0) = (ikptr)
	  (((limb_count + 1) << bignum_nlimbs_shift) |
	   (1 << bignum_sign_shift) |
	   bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag, "fxbn-");
      } else {
	ref(r, 0) = (ikptr)
	  ((limb_count << bignum_nlimbs_shift) |
	   (1 << bignum_sign_shift) |
	   bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag, "fxbn-");
      }
    }
  }
}
ikptr
ikrt_bnfxminus (ikptr x, ikptr y, ikpcb* pcb)
{
  if (y == 0) { return x; }
  ikptr first_word = ref(x, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  long inty = IK_UNFIX(y);
  if (inty < 0) {
    if (!IK_BNFST_NEGATIVE(first_word)) {
      /* - negative fx + positive bn = positive bn */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long carry =
	mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
		  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
		  limb_count,
		  -inty);
      if (carry) {
	ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
	ref(r, 0) = (ikptr)
	     (((limb_count + 1) << bignum_nlimbs_shift) |
	      (0 << bignum_sign_shift) |
	      bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag,"bnfx-");
      } else {
	ref(r, 0) = (ikptr)
	  ((limb_count << bignum_nlimbs_shift) |
	   (0 << bignum_sign_shift) |
	   bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag,"bnfx-");
      }
    }
    else {
      /* - negative fx + negative bn = smaller negative bn/fx */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long borrow =
	mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
		  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
		  limb_count,
		  -inty);
      if (borrow)
	ik_abort("BUG in borrow5\n");
      long result_size =
	(ref(r, disp_bignum_data + (limb_count-1)*wordsize))
	? limb_count
	: (limb_count - 1);
      if (result_size == 0) {
	return 0;
      }
      if (1 == result_size) {
	ik_ulong last =
	  (ik_ulong) ref(r, disp_bignum_data + (result_size-1)*wordsize);
	if (last <= most_negative_fixnum) {
	  return IK_FIX(-(long)last);
	}
      }
      ref(r, 0) = (ikptr)
	((result_size << bignum_nlimbs_shift) |
	 (1 << bignum_sign_shift) |
	 bignum_tag);
      return DEBUG_VERIFY_BIGNUM(r+vector_tag,"bnfx-");
    }
  }
  else {
    if ((bignum_sign_mask & (long)first_word) == 0) {
      /* - positive fx + positive bn = smaller positive */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long borrow =
	mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
		  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
		  limb_count,
		  inty);
      if (borrow)
	ik_abort("BUG in borrow6\n");
      long result_size =
	(ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0)
	? (limb_count - 1)
	: limb_count;
      if (result_size == 0) {
	return 0;
      }
      if (1 == result_size) {
	ik_ulong last =
	  (ik_ulong) ref(r, disp_bignum_data + (result_size-1)*wordsize);
	if (last <= most_positive_fixnum) {
	  return IK_FIX((long)last);
	}
      }
      ref(r, 0) = (ikptr)
	((result_size << bignum_nlimbs_shift) |
	 (0 << bignum_sign_shift) |
	 bignum_tag);
      return DEBUG_VERIFY_BIGNUM(r+vector_tag, "bnfx-");
    } else {
      /* - positive fx + negative bn = larger negative */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long carry =
	mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
		  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
		  limb_count,
		  inty);
      if (carry) {
	ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
	ref(r, 0) = (ikptr)
	     (((limb_count + 1) << bignum_nlimbs_shift) |
	      (1 << bignum_sign_shift) |
	      bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag, "bnfx-");
      } else {
	ref(r, 0) = (ikptr)
	  ((limb_count << bignum_nlimbs_shift) |
	   (1 << bignum_sign_shift) |
	   bignum_tag);
	return DEBUG_VERIFY_BIGNUM(r+vector_tag, "bnfx-");
      }
    }
  }
}
ikptr
ikrt_bnbnminus(ikptr x, ikptr y, ikpcb* pcb)
{
  if (x == y) { return 0; }
  ik_ulong xfst = (ik_ulong)ref(x, -vector_tag);
  ik_ulong yfst = (ik_ulong)ref(y, -vector_tag);
  long xsign = xfst & bignum_sign_mask;
  long ysign = yfst & bignum_sign_mask;
  long xlimbs = xfst >> bignum_nlimbs_shift;
  long ylimbs = yfst >> bignum_nlimbs_shift;
  if (xsign != ysign) {
    long n1,n2;
    ikptr s1,s2;
    if (xlimbs >= ylimbs) {
      n1 = xlimbs; n2 = ylimbs; s1 = x; s2 = y;
    } else {
      n1 = ylimbs; n2 = xlimbs; s1 = y; s2 = x;
    }
    pcb->root0 = &s1;
    pcb->root1 = &s2;
    ikptr res = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (n1+1)*wordsize));
    pcb->root0 = 0;
    pcb->root1 = 0;
    mp_limb_t carry =
      mpn_add((mp_limb_t*)(long)(res+disp_bignum_data),
	      (mp_limb_t*)(long)(s1-vector_tag+disp_bignum_data),
	      n1,
	      (mp_limb_t*)(long)(s2-vector_tag+disp_bignum_data),
	      n2);
    if (carry) {
      ref(res, disp_vector_data + xlimbs*wordsize) = (ikptr)1;
      ref(res, 0) = (ikptr)
		    (((n1+1) << bignum_nlimbs_shift) |
		     xsign |
		     bignum_tag);
      return DEBUG_VERIFY_BIGNUM(res+vector_tag, "bnbn-");
    } else {
      ref(res, 0) = (ikptr)
		    ((n1 << bignum_nlimbs_shift) |
		     xsign |
		     bignum_tag);
      return DEBUG_VERIFY_BIGNUM(res+vector_tag, "bnbn-");
    }
  }
  else {
    /* same sign */
    if (xlimbs == ylimbs) {
      while((ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) ==
	     ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))) {
	xlimbs -= 1;
	if (xlimbs == 0) { return 0; }
      }
      ylimbs = xlimbs;
    }
    ikptr s1=x, s2=y;
    long n1=xlimbs, n2=ylimbs;
    long result_sign = xsign;
    /* |x| != |y| */
    if (xlimbs <= ylimbs) {
      if (xlimbs == ylimbs) {
	if ((ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) >
	    ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))) {
	  s1 = y; n1 = ylimbs;
	  s2 = x; n2 = xlimbs;
	  result_sign = (1 << bignum_sign_shift) - ysign;
	}
      } else {
	s1 = y; n1 = ylimbs;
	s2 = x; n2 = xlimbs;
	result_sign = (1 << bignum_sign_shift) - ysign;
      }
    }
    /* |s1| > |s2| */
    pcb->root0 = &s1;
    pcb->root1 = &s2;
    ikptr res = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n1 * wordsize));
    bzero((void*)(res+disp_bignum_data), n1*wordsize);
    pcb->root0 = 0;
    pcb->root1 = 0;
    long burrow =
      mpn_sub((mp_limb_t*)(long)(res + disp_bignum_data),
	      (mp_limb_t*)(long)(s1 - vector_tag + disp_bignum_data),
	      n1,
	      (mp_limb_t*)(long)(s2 - vector_tag + disp_bignum_data),
	      n2);
    if (burrow)
      ik_abort("BUG: burrow error in bnbn-");
    long len = n1;
    while(ref(res, disp_bignum_data + (len-1)*wordsize) == 0) {
      len--;
      if (len == 0) {
	return 0;
      }
    }
    if (result_sign == 0) {
      /* positive result */
      if (len == 1) {
	ik_ulong fst_limb =
	  (ik_ulong) ref(res, disp_bignum_data);
	if (fst_limb <= most_positive_fixnum) {
	  return IK_FIX((long)fst_limb);
	}
      }
      ref(res, 0) = (ikptr)
		    ((len << bignum_nlimbs_shift) |
		     result_sign |
		     bignum_tag);
      return DEBUG_VERIFY_BIGNUM(res+vector_tag, "bnbn-");
    } else {
      /* negative result */
      if (len == 1) {
	ik_ulong fst_limb =
	  (ik_ulong) ref(res, disp_bignum_data);
	if (fst_limb <= most_negative_fixnum) {
	  return IK_FIX(-(long)fst_limb);
	}
      }
      ref(res, 0) = (ikptr)
		    ((len << bignum_nlimbs_shift) |
		     result_sign |
		     bignum_tag);
      return DEBUG_VERIFY_BIGNUM(res+vector_tag, "bnbn-");
    }
  }
}

ikptr
ikrt_bnnegate (ikptr x, ikpcb* pcb)
{
  ikptr first_word = ref(x, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  if (limb_count == 1) {
    if (! IK_BNFST_NEGATIVE(first_word)) {
      /* positive bignum */
      mp_limb_t limb =
	(mp_limb_t) ref(x, disp_bignum_data - vector_tag);
      if (limb == (most_positive_fixnum + 1)) {
	return IK_FIX(-(long)limb);
      }
    }
  }
  pcb->root0 = &x;
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + limb_count * wordsize));
  pcb->root0 = 0;
  memcpy((char*)(long)bn+disp_bignum_data,
	 (char*)(long)x-vector_tag+disp_bignum_data,
	 limb_count*wordsize);
  ref(bn, 0) = (ikptr)
    (bignum_tag |
     ((1 << bignum_sign_shift) - (bignum_sign_mask & (long)first_word)) |
     (limb_count << bignum_nlimbs_shift));
  return DEBUG_VERIFY_BIGNUM(bn+vector_tag, "bnneg");
}



ikptr
ikrt_fxfxmult(ikptr x, ikptr y, ikpcb* pcb) {
  long n1 = IK_UNFIX(x);
  long n2 = IK_UNFIX(y);
  mp_limb_t lo = 0;
  mp_limb_t s1 = n1;
  mp_limb_t s2 = n2;
  long sign = 0;
  if (n1 < 0) {
    s1 = -n1;
    sign = 1 - sign;
  }
  if (n2 < 0) {
    s2 = -n2;
    sign = 1 - sign;
  }
  mp_limb_t hi = mpn_mul_1(&lo, &s1, 1, s2);
  if (hi == 0) {
    if (sign) {
      if (lo <= most_negative_fixnum) {
	return IK_FIX(-((long)lo));
      }
    } else {
      if (lo <= most_positive_fixnum) {
	return IK_FIX((long)lo);
      }
    }
    ikptr r = ik_safe_alloc(pcb, disp_bignum_data + wordsize);
    ref(r, 0) = (ikptr)
      (bignum_tag |
       (sign << bignum_sign_shift) |
       (1 << bignum_nlimbs_shift));
    ref(r, disp_bignum_data) = (ikptr)lo;
    return BN(r+vector_tag);
  } else {
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + 2*wordsize));
    ref(r, 0) = (ikptr)
      (bignum_tag |
       (sign << bignum_sign_shift) |
       (2 << bignum_nlimbs_shift));
    ref(r, disp_bignum_data) = (ikptr)lo;
    ref(r, disp_bignum_data+wordsize) = (ikptr)hi;
    return BN(r+vector_tag);
  }
}

ikptr
ik_normalize_bignum(long limbs, int sign, ikptr r) {
  while(ref(r, disp_bignum_data + (limbs-1)*wordsize) == 0) {
    limbs--;
    if (limbs == 0) { return 0;}
  }
  if (limbs == 1) {
    mp_limb_t last = (mp_limb_t) ref(r, disp_bignum_data);
    if (sign == 0) {
      if (last <= most_positive_fixnum) {
	return IK_FIX(last);
      }
    } else {
      if (last <= most_negative_fixnum) {
	return IK_FIX(-(last));
      }
    }
  }
  ref(r, 0) = (ikptr) (bignum_tag | sign | (limbs << bignum_nlimbs_shift));
  return BN(r+vector_tag);
}


ikptr
ikrt_fxbnmult(ikptr x, ikptr y, ikpcb* pcb) {
  long n2 = IK_UNFIX(x);
  if (n2 == 0) { return 0; }
  mp_limb_t s2 = (n2>0) ? n2 : (- n2);
  ikptr first_word = ref(y, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  pcb->root0 = &y;
  ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (limb_count+1)*wordsize));
  pcb->root0 = 0;
  mp_limb_t hi = mpn_mul_1((mp_limb_t*)(long)(r+disp_bignum_data),
			   (mp_limb_t*)(long)(y-vector_tag+disp_bignum_data),
			   limb_count,
			   s2);
  ref(r, disp_bignum_data + limb_count * wordsize) = (ikptr)hi;
  long sign =
    ((n2 > 0) ?
     (bignum_sign_mask & (long)first_word) :
     ((1 << bignum_sign_shift) - (bignum_sign_mask&(long)first_word)));
  return ik_normalize_bignum(limb_count+1, sign, r);
}

ikptr
ikrt_bnbnmult(ikptr x, ikptr y, ikpcb* pcb) {
  long f1 = (long)ref(x, -vector_tag);
  long f2 = (long)ref(y, -vector_tag);
  long n1 = IK_BNFST_LIMB_COUNT(f1);
  long n2 = IK_BNFST_LIMB_COUNT(f2);
  long nr = n1 + n2;
  pcb->root0 = &x;
  pcb->root1 = &y;
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + nr*wordsize));
  pcb->root0 = 0;
  pcb->root1 = 0;
  if (n1 >= n2) {
    mpn_mul((mp_limb_t*)(long)(bn+disp_bignum_data),
	    (mp_limb_t*)(long)(x-vector_tag+disp_bignum_data),
	    n1,
	    (mp_limb_t*)(long)(y-vector_tag+disp_bignum_data),
	    n2);
  } else {
    mpn_mul((mp_limb_t*)(long)(bn+disp_bignum_data),
	    (mp_limb_t*)(long)(y-vector_tag+disp_bignum_data),
	    n2,
	    (mp_limb_t*)(long)(x-vector_tag+disp_bignum_data),
	    n1);
  }
  long sign =
    ((bignum_sign_mask & f1) ?
     ((1 << bignum_sign_shift) - (bignum_sign_mask & f2)) :
     (bignum_sign_mask & f2));
  return ik_normalize_bignum(nr, sign, bn);
}




ikptr
ikrt_bnbncomp(ikptr bn1, ikptr bn2) {
  ikptr f1 = ref(bn1, -vector_tag);
  ikptr f2 = ref(bn2, -vector_tag);
  if (IK_BNFST_NEGATIVE(f1)) {
    if (IK_BNFST_NEGATIVE(f2)) {
      /* both negative */
      long n1 = ((mp_limb_t) f1) >> bignum_nlimbs_shift;
      long n2 = ((mp_limb_t) f2) >> bignum_nlimbs_shift;
      if (n1 < n2) {
	return IK_FIX(1);
      } else if (n1 > n2) {
	return IK_FIX(-1);
      } else {
	long i;
	for(i=(n1-1); i>=0; i--) {
	  mp_limb_t t1 =
	    (mp_limb_t) ref(bn1,disp_bignum_data-vector_tag+i*wordsize);
	  mp_limb_t t2 =
	    (mp_limb_t) ref(bn2,disp_bignum_data-vector_tag+i*wordsize);
	  if (t1 < t2) {
	    return IK_FIX(1);
	  } else if (t1 > t2) {
	    return IK_FIX(-1);
	  }
	}
      }
      return 0;
    } else {
      /* n1 negative, n2 positive */
      return IK_FIX(-1);
    }
  } else {
    if (IK_BNFST_NEGATIVE(f2)) {
      /* n1 positive, n2 negative */
      return IK_FIX(1);
    } else {
      /* both positive */
      long n1 = ((mp_limb_t) f1) >> bignum_nlimbs_shift;
      long n2 = ((mp_limb_t) f2) >> bignum_nlimbs_shift;
      if (n1 < n2) {
	return IK_FIX(-1);
      } else if (n1 > n2) {
	return IK_FIX(1);
      } else {
	long i;
	for(i=(n1-1); i>=0; i--) {
	  mp_limb_t t1 =
	   (mp_limb_t) ref(bn1,disp_bignum_data-vector_tag+i*wordsize);
	  mp_limb_t t2 =
	    (mp_limb_t) ref(bn2,disp_bignum_data-vector_tag+i*wordsize);
	  if (t1 < t2) {
	    return IK_FIX(-1);
	  } else if (t1 > t2) {
	    return IK_FIX(1);
	  }
	}
      }
      return 0;
    }
  }
}


static inline int
count_leading_ffs(int n, mp_limb_t* x) {
  int idx;
  for(idx=0; idx<n; idx++) {
    if (x[idx] != (mp_limb_t)-1) {
      return idx;
    }
  }
  return n;
}


static void
copy_limbs(mp_limb_t* src, mp_limb_t* dst, int n1, int n2) {
  while(n1 < n2) {
    dst[n1] = src[n1];
    n1++;
  }
}

static void
bits_compliment(mp_limb_t* src, mp_limb_t* dst, long n) {
  mp_limb_t carry = 1;
  long i;
  for(i=0; i<n; i++) {
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static void
bits_compliment2(mp_limb_t* src, mp_limb_t* dst, int n1, int n2) {
  mp_limb_t carry = 1;
  int i;
  for(i=0; i<n1; i++) {
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
  for(i=n1; i<n2; i++) {
    mp_limb_t d = 0;
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static int
bits_compliment_carry(mp_limb_t* src, mp_limb_t* dst, int n1, int n2, mp_limb_t carry) {
  int i;
  for(i=n1; i<n2; i++) {
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
  return carry;
}




static void
bits_compliment_with_carry(mp_limb_t* src, mp_limb_t* dst, long n, long carry) {
  long i;
  for(i=0; i<n; i++) {
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static void
bits_compliment_logand(mp_limb_t* s1, mp_limb_t* s2, mp_limb_t* dst, int n) {
  int carry = 1;
  int i;
  for(i=0; i<n; i++) {
    mp_limb_t d = s1[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c & s2[i];
    carry = (carry && ! d);
  }
}



static int
bits_compliment_logor(mp_limb_t* s1, mp_limb_t* s2, mp_limb_t* dst, int n) {
  int carry = 1;
  int i;
  for(i=0; i<n; i++) {
    mp_limb_t d = s1[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c | s2[i];
    carry = (carry && ! d);
  }
  return carry;
}


static long
bits_carry(mp_limb_t* s,  int n) {
  /*
  int carry = 1;
  int i;
  for(i=0; i<n; i++) {
    mp_limb_t d = s[i];
    carry = (carry && ! d);
  }
  return carry;
  */
  int i;
  for(i=0; i<n; i++) {
    if (s[i] != 0) {
      return 0;
    }
  }
  return 1;
}

ikptr
ikrt_bnlognot(ikptr x, ikpcb* pcb) {
  ikptr first_word = ref(x, -vector_tag);
  long n = IK_BNFST_LIMB_COUNT(first_word);
  if (IK_BNFST_NEGATIVE(first_word)) {
    /* negative */
    pcb->root0 = &x;
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n*wordsize));
    pcb->root0 = 0;
    mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
    mp_limb_t* rd = (mp_limb_t*)(long)(r+disp_bignum_data);
    int i;
    for(i=0; (i<n) && (s1[i] == 0); i++) {
      rd[i] = -1;
    }
    rd[i] = s1[i] - 1;
    for(i++; i<n; i++) {
      rd[i] = s1[i];
    }
    return ik_normalize_bignum(n, 0, r);
  } else {
    /* positive */
    long i;
    mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
    for(i=0; (i<n) && (s1[i] == (mp_limb_t)-1); i++) {/*nothing*/}
    if (i==n) {
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (n+1)*wordsize));
      pcb->root0 = 0;
      bzero((char*)(long)r+disp_bignum_data, n*wordsize);
      ((mp_limb_t*)(long)(r+disp_bignum_data))[n] = 1;
      ref(r, 0) = (ikptr)
	(bignum_tag | (1<<bignum_sign_shift) | ((n+1) << bignum_nlimbs_shift));
      return r+vector_tag;
    } else {
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n*wordsize));
      pcb->root0 = 0;
      mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
      mp_limb_t* rd = (mp_limb_t*)(long)(r+disp_bignum_data);
      int j;
      for(j=0; j<i; j++) { rd[j] = 0; }
      rd[i] = s1[i] + 1;
      for(j=i+1; j<n; j++) { rd[j] = s1[j]; }
      ref(r, 0) = (ikptr)
	(bignum_tag | (1<<bignum_sign_shift) | (n << bignum_nlimbs_shift));
      return r+vector_tag;
    }
  }
}


ikptr
ikrt_fxbnlogand(ikptr x, ikptr y, ikpcb* pcb) {
  long n1 = IK_UNFIX(x);
  ikptr first_word = ref(y, -vector_tag);
  if (n1 >= 0) {
    /* x is positive */
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* y is negative */
      return IK_FIX(n1 & (1+~(long)ref(y, disp_vector_data-vector_tag)));
    } else {
      /* y is positive */
      return IK_FIX(n1 & (long)ref(y, disp_vector_data-vector_tag));
    }
  } else {
    /* x is negative */
    if (n1 == -1) { return y; }
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* y is negative */
      long len = IK_BNFST_LIMB_COUNT(first_word);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (len+1)*wordsize));
      pcb->root0 = 0;
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      bits_compliment2(s2, s, len, len+1);
      s[0] = s[0] & n1;
      bits_compliment2(s, s, len+1, len+1);
      return ik_normalize_bignum(len+1, 1<<bignum_sign_shift, r);
    } else {
      /* y is positive */
      long len = IK_BNFST_LIMB_COUNT(first_word);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + len * wordsize));
      pcb->root0 = 0;
      ref(r, 0) = first_word;
      ref(r, disp_bignum_data) = (ikptr)
	(((long)ref(y, disp_bignum_data - vector_tag)) & n1);
      int i;
      for(i=1; i<len; i++) {
	ref(r, disp_bignum_data+i*wordsize) =
	  ref(y, disp_bignum_data-vector_tag+i*wordsize);
      }
      return BN(r+vector_tag);
    }
  }
}

ikptr
ikrt_bnbnlogand(ikptr x, ikptr y, ikpcb* pcb) {
  ikptr xfst = ref(x, -vector_tag);
  ikptr yfst = ref(y, -vector_tag);
  long n1 = IK_BNFST_LIMB_COUNT(xfst);
  long n2 = IK_BNFST_LIMB_COUNT(yfst);
  if (IK_BNFST_NEGATIVE(xfst)) {
    if (IK_BNFST_NEGATIVE(yfst)) {
      if (n1 >= n2) {
	pcb->root0 = &x;
	pcb->root1 = &y;
	ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (n1+1)*wordsize));
	pcb->root0 = 0;
	pcb->root1 = 0;
	mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
	mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
	mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
	bits_compliment2(s1, s, n1, n1+1);
	bits_compliment_logand(s2, s, s, n2);
	bits_compliment2(s, s, n1+1, n1+1);
	return ik_normalize_bignum(n1+1, 1<<bignum_sign_shift, r);
      } else {
	return ikrt_bnbnlogand(y,x,pcb);
      }
    } else {
      return ikrt_bnbnlogand(y,x,pcb);
    }
  } else {
    if (IK_BNFST_NEGATIVE(yfst)) {
      /* x positive, y negative */
      /*  the result is at most n1 words long */
      pcb->root0 = &x;
      pcb->root1 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n1*wordsize));
      pcb->root0 = 0;
      pcb->root1 = 0;
      mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      if (n1 <= n2) {
	bits_compliment_logand(s2, s1, s, n1);
      } else {
	bits_compliment_logand(s2, s1, s, n2);
	copy_limbs(s1, s, n2, n1);
      }
      return ik_normalize_bignum(n1, 0, r);
    } else {
      /* both positive */
      int n = (n1<n2)?n1:n2;
      long i;
      for(i=n-1; i>=0; i--) {
	long l1 =
	  (long) ref(x, disp_bignum_data-vector_tag+i*wordsize);
	long l2 =
	  (long) ref(y, disp_bignum_data-vector_tag+i*wordsize);
	ik_ulong last = l1 & l2;
	if (last) {
	  if ((i == 0) && (last < most_positive_fixnum)) {
	    return IK_FIX(last);
	  }
	  pcb->root0 = &x;
	  pcb->root1 = &y;
	  ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(i+1)*wordsize));
	  pcb->root0 = 0;
	  pcb->root1 = 0;
	  ref(r, 0) = (ikptr) (bignum_tag | ((i+1)<<bignum_nlimbs_shift));
	  ref(r, disp_bignum_data + i*wordsize) = (ikptr)last;
	  int j;
	  for(j=0; j<i; j++) {
	    ref(r, disp_bignum_data + j*wordsize) = (ikptr)
	      (((long)ref(x, disp_bignum_data-vector_tag+j*wordsize))
	       &
	       ((long)ref(y, disp_bignum_data-vector_tag+j*wordsize)));
	  }
	  return r+vector_tag;
	}
      }
      return 0;
    }
  }
}


ikptr
ikrt_fxbnlogor(ikptr x, ikptr y, ikpcb* pcb) {
  long n1 = IK_UNFIX(x);
  ikptr first_word = ref(y, -vector_tag);
  if (n1 < 0) {
    /* x is negative */
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* y is negative */
      return IK_FIX(n1 | (1+~(long)ref(y, disp_vector_data-vector_tag)));
    } else {
      /* y is positive */
      return IK_FIX(n1 | (long)ref(y, disp_vector_data-vector_tag));
    }
  } else {
    /* x is non negative */
    if (n1 == 0) { return y; }
    /* x is positive */
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* y is negative */
      long len = IK_BNFST_LIMB_COUNT(first_word);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (len+1)*wordsize));
      pcb->root0 = 0;
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      bits_compliment2(s2, s, len, len+1);
      s[0] = s[0] | n1;
      bits_compliment2(s, s, len+1, len+1);
      return ik_normalize_bignum(len+1, 1<<bignum_sign_shift, r);
    } else {
      /* y is positive */
      long len = IK_BNFST_LIMB_COUNT(first_word);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + len * wordsize));
      pcb->root0 = 0;
      ref(r, 0) = first_word;
      ref(r, disp_bignum_data) = (ikptr)
	(((long)ref(y, disp_bignum_data - vector_tag)) | n1);
      int i;
      for(i=1; i<len; i++) {
	ref(r, disp_bignum_data+i*wordsize) =
	  ref(y, disp_bignum_data-vector_tag+i*wordsize);
      }
      return BN(r+vector_tag);
    }
  }
}

ikptr
ikrt_bnbnlogor(ikptr x, ikptr y, ikpcb* pcb) {
  ikptr xfst = ref(x, -vector_tag);
  ikptr yfst = ref(y, -vector_tag);
  long n1 = IK_BNFST_LIMB_COUNT(xfst);
  long n2 = IK_BNFST_LIMB_COUNT(yfst);
  if (IK_BNFST_NEGATIVE(xfst)) {
    if (IK_BNFST_NEGATIVE(yfst)) {
      if (n1 >= n2) {
	pcb->root0 = &x;
	pcb->root1 = &y;
	ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n1*wordsize));
	pcb->root0 = 0;
	pcb->root1 = 0;
	mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
	mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
	mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
	bits_compliment2(s2, s, n2, n1);
	int carry = bits_compliment_logor(s1, s, s, n1);
	bits_compliment_carry(s,s,n1,n1,carry);
	bits_compliment2(s, s, n1, n1);
	return ik_normalize_bignum(n1, 1<<bignum_sign_shift, r);
      } else {
	return ikrt_bnbnlogor(y,x,pcb);
      }
    } else {
      return ikrt_bnbnlogor(y,x,pcb);
    }
  } else {
    if (IK_BNFST_NEGATIVE(yfst)) {
      /* x positive, y negative */
      /*  the result is at most n2 words long */
      pcb->root0 = &x;
      pcb->root1 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n2*wordsize));
      pcb->root0 = 0;
      pcb->root1 = 0;
      mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      if (n2 <= n1) {
	bits_compliment_logor(s2, s1, s, n2);
	bits_compliment2(s, s, n2, n2);
      } else {
	int carry = bits_compliment_logor(s2, s1, s, n1);
	bits_compliment_carry(s2, s, n1, n2, carry);
	bits_compliment_carry(s, s, 0, n2, 1);
      }
      return ik_normalize_bignum(n2, 1<<bignum_sign_shift, r);
    } else {
      /* both positive */
      int n = (n1>n2)?n1:n2;
      pcb->root0 = &x;
      pcb->root1 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+n*wordsize));
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      pcb->root0 = 0;
      pcb->root1 = 0;
      long i;
      if (n == n1) {
	for(i=0; i<n2; i++) {
	  s[i] = s1[i] | s2[i];
	}
	for(i=n2; i<n1; i++) {
	  s[i] = s1[i];
	}
      } else {
	for(i=0; i<n1; i++) {
	  s[i] = s1[i] | s2[i];
	}
	for(i=n1; i<n2; i++) {
	  s[i] = s2[i];
	}
      }
      return ik_normalize_bignum(n, 0, r);
    }
  }
}

static void
copy_bits_shifting_right(mp_limb_t* src, mp_limb_t* dst, int n, int m) {
  mp_limb_t carry = src[0] >> m;
  int i;
  for(i=1; i<n; i++) {
    mp_limb_t b = src[i];
    dst[i-1] = (b << (mp_bits_per_limb-m)) | carry;
    carry = b >> m;
  }
  dst[n-1] = carry;
}

static void
copy_bits_shifting_left(mp_limb_t* src, mp_limb_t* dst, int n, int m) {
  mp_limb_t carry = 0;
  int i;
  for(i=0; i<n; i++) {
    mp_limb_t b = src[i];
    dst[i] = (b << m) | carry;
    carry = b >> (mp_bits_per_limb-m);
  }
  dst[n] = carry;
}





ikptr
ikrt_bignum_shift_right(ikptr x, ikptr y, ikpcb* pcb) {
  int limb_shift = (wordsize == 4 ? 5 : 6);
  long m = IK_UNFIX(y);
  ikptr first_word = ref(x, -vector_tag);
  long n = IK_BNFST_LIMB_COUNT(first_word);
  long whole_limb_shift = m >> limb_shift;
  long bit_shift = m & (mp_bits_per_limb-1);
  long new_limb_count = n - whole_limb_shift;
  if (IK_BNFST_NEGATIVE(first_word)) {
    if (new_limb_count <= 0) {
      return IK_FIX(-1);
    }
    if (bit_shift == 0) {
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + new_limb_count * wordsize));
      pcb->root0 = 0;
      bits_compliment_with_carry(
	  (mp_limb_t*)(long)(x+off_bignum_data+whole_limb_shift*wordsize),
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  new_limb_count,
	  bits_carry((mp_limb_t*)(long)(x+off_bignum_data), whole_limb_shift));
      bits_compliment(
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  new_limb_count);
      return ik_normalize_bignum(new_limb_count, 1 << bignum_sign_shift, r);
    } else {
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + new_limb_count * wordsize));
      pcb->root0 = 0;
      bits_compliment_with_carry(
	  (mp_limb_t*)(long)(x+off_bignum_data+whole_limb_shift*wordsize),
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  new_limb_count,
	  bits_carry((mp_limb_t*)(long)(x+off_bignum_data), whole_limb_shift));
      copy_bits_shifting_right(
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  new_limb_count,
	  bit_shift);
      *((mp_limb_t*)(r+disp_bignum_data+(new_limb_count-1)*wordsize))
	  |= (-1L << (mp_bits_per_limb - bit_shift));
      bits_compliment(
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  new_limb_count);
      return ik_normalize_bignum(new_limb_count, 1 << bignum_sign_shift, r);
      ik_abort("not yet for negative bignum_shift");
    }
  } else {
    if (new_limb_count <= 0) {
      return 0;
    }
    pcb->root0 = &x;
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + new_limb_count * wordsize));
    pcb->root0 = 0;
    if (bit_shift == 0) {
      memcpy((char*)(long)r+disp_bignum_data,
	     (char*)(long)x+off_bignum_data+whole_limb_shift*wordsize,
	     new_limb_count * wordsize);
      return ik_normalize_bignum(new_limb_count, 0, r);
    } else {
      copy_bits_shifting_right(
	  (mp_limb_t*)(long)(x+off_bignum_data+whole_limb_shift*wordsize),
	  (mp_limb_t*)(long)(r+disp_bignum_data),
	  new_limb_count,
	  bit_shift);
      return ik_normalize_bignum(new_limb_count, 0, r);
    }
  }
}


ikptr
ikrt_fixnum_shift_left(ikptr x, ikptr y, ikpcb* pcb) {
  int limb_shift = (wordsize == 4 ? 5 : 6);
  long m = IK_UNFIX(y);
  long n = IK_UNFIX(x);
  long limb_count = (m >> limb_shift) + 2;
  long bit_shift = m & (mp_bits_per_limb-1);
  ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + limb_count * wordsize));
  ik_ulong* s = (ik_ulong*)(long)(r+disp_bignum_data);
  bzero(s, limb_count * wordsize);
  if (n >= 0) {
    if (bit_shift) {
      s[limb_count-1] = n >> (mp_bits_per_limb - bit_shift);
    }
    s[limb_count-2] = n << bit_shift;
  } else {
    if (bit_shift) {
      s[limb_count-1] = (-n) >> (mp_bits_per_limb - bit_shift);
    }
    s[limb_count-2] = (-n) << bit_shift;
  }
  return ik_normalize_bignum(limb_count, (n>=0)?(0):(1<<bignum_sign_shift), r);
}


ikptr
ikrt_bignum_shift_left(ikptr x, ikptr y, ikpcb* pcb) {
  int limb_shift = (wordsize == 4 ? 5 : 6);
  long m = IK_UNFIX(y);
  ikptr first_word = ref(x, -vector_tag);
  long n = IK_BNFST_LIMB_COUNT(first_word);
  long whole_limb_shift = m >> limb_shift;
  long bit_shift = m & (mp_bits_per_limb-1);
  if (bit_shift == 0) {
    long limb_count = n + whole_limb_shift;
    pcb->root0 = &x;
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + limb_count * wordsize));
    pcb->root0 = 0;
    unsigned int* s = (unsigned int*)(long)(r+disp_bignum_data);
    bzero(s, whole_limb_shift*wordsize);
    memcpy(((char*)s) + whole_limb_shift*wordsize,
	   ((char*)x) + off_bignum_data,
	   n*wordsize);
    return ik_normalize_bignum(limb_count, IK_BNFST_NEGATIVE(first_word), r);
  } else {
    int limb_count = n + whole_limb_shift + 1;
    pcb->root0 = &x;
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + limb_count * wordsize));
    pcb->root0 = 0;
    mp_limb_t* s = (mp_limb_t*)(r+disp_bignum_data);
    bzero(s, whole_limb_shift*wordsize);
    copy_bits_shifting_left(
	(mp_limb_t*)(x+off_bignum_data),
	(mp_limb_t*)(s+whole_limb_shift),
	n,
	bit_shift);
    return ik_normalize_bignum(limb_count, IK_BNFST_NEGATIVE(first_word), r);
  }
}


#if 0
From TFM:
void
mpn_tdiv_qr (
  mp limb t *qp,	/* quotient placed here */
  mp limb t *rp,	/* remainder placed here */
  mp size t qxn,	/* must be zero! */
  const mp limb t *np,	/* first number	 */
  mp size t nn,		/* its length	 */
  const mp limb t *dp,	/* second number */
  mp size t dn		/* its length	 */
)

Divide {np, nn} by {dp, dn} and put the quotient at {qp,nn-dn+1}
and the remainder at {rp, dn}. The quotient is rounded towards 0.
No overlap is permitted between arguments. nn must be greater than
or equal to dn. The most significant limb of dp must be non-zero.
The qxn operand must be zero.
#endif

ikptr
ikrt_bnbndivrem(ikptr x, ikptr y, ikpcb* pcb) {
  ikptr xfst = ref(x, -vector_tag);
  ikptr yfst = ref(y, -vector_tag);
  mp_size_t xn = IK_BNFST_LIMB_COUNT(xfst);
  mp_size_t yn = IK_BNFST_LIMB_COUNT(yfst);
  if (xn < yn) {
    /* quotient is zero, remainder is x */
    pcb->root0 = &x;
    pcb->root1 = &y;
    ikptr rv = ik_safe_alloc(pcb, pair_size);
    pcb->root0 = 0;
    pcb->root1 = 0;
    ref(rv, disp_car) = 0;
    ref(rv, disp_cdr) = x;
    return rv+pair_tag;
  }
  mp_size_t qn = xn - yn + 1;
  mp_size_t rn = yn;
  /*
  ikptr q = ik_unsafe_alloc(pcb, IK_ALIGN(disp_bignum_data + qn*wordsize));
  ikptr r = ik_unsafe_alloc(pcb, IK_ALIGN(disp_bignum_data + rn*wordsize));
  */
  pcb->root0 = &x;
  pcb->root1 = &y;
  ikptr q = ik_safe_alloc(pcb,
	    IK_ALIGN(disp_bignum_data + qn*wordsize) +
	    IK_ALIGN(disp_bignum_data + rn*wordsize));
  ikptr r = q + IK_ALIGN(disp_bignum_data + qn*wordsize);
  pcb->root0 = 0;
  pcb->root1 = 0;
  mpn_tdiv_qr (
      (mp_limb_t*)(long)(q+disp_bignum_data),
      (mp_limb_t*)(long)(r+disp_bignum_data),
      0,
      (mp_limb_t*)(long)(x+off_bignum_data),
      xn,
      (mp_limb_t*)(long)(y+off_bignum_data),
      yn);

  if (IK_BNFST_NEGATIVE(xfst)) {
    /* x is negative => remainder is negative */
    r = ik_normalize_bignum(rn, 1 << bignum_sign_shift, r);
  } else {
    r = ik_normalize_bignum(rn, 0, r);
  }

  if (IK_BNFST_NEGATIVE(yfst)) {
    /* y is negative => quotient is opposite of x */
    long sign = bignum_sign_mask - IK_BNFST_NEGATIVE(xfst);
    q = ik_normalize_bignum(qn, sign, q);
  } else {
    /* y is positive => quotient is same as x */
    long sign = IK_BNFST_NEGATIVE(xfst);
    q = ik_normalize_bignum(qn, sign, q);
  }
  pcb->root0 = &q;
  pcb->root1 = &r;
  ikptr rv = ik_safe_alloc(pcb, pair_size);
  pcb->root0 = 0;
  pcb->root1 = 0;
  ref(rv, disp_car) = q;
  ref(rv, disp_cdr) = r;
  return rv+pair_tag;
}


#if 0
[Function]

mp_limb_t
mpn_divrem_1 (
  mp limb t *r1p,
  mp size t qxn,
  mp limb t *s2p,
  mp size t s2n,
  mp limb t s3limb
)

Divide {s2p, s2n} by s3limb, and write the quotient at r1p. Return the remainder.
The integer quotient is written to {r1p+qxn, s2n} and in addition qxn fraction limbs are
developed and written to {r1p, qxn}. Either or both s2n and qxn can be zero. For most
usages, qxn will be zero.
#endif

ikptr
ikrt_bnfxdivrem(ikptr x, ikptr y, ikpcb* pcb) {
  long yint = IK_UNFIX(y);
  ikptr first_word = ref(x, -vector_tag);
  mp_size_t s2n = IK_BNFST_LIMB_COUNT(first_word);
  pcb->root0 = &x;
  ikptr quot = ik_safe_alloc(pcb, IK_ALIGN(s2n*wordsize + disp_bignum_data));
  pcb->root0 = 0;
  mp_limb_t* s2p = (mp_limb_t*)(long)(x+off_bignum_data);
  mp_limb_t rv = mpn_divrem_1(
      (mp_limb_t*)(long)(quot+disp_bignum_data),
      0,
      s2p,
      s2n,
      labs(yint));

  ikptr rem;

  if (yint < 0) {
    /* y is negative => quotient is opposite of x */
    long sign = bignum_sign_mask - IK_BNFST_NEGATIVE(first_word);
    quot = ik_normalize_bignum(s2n, sign, quot);
  } else {
    /* y is positive => quotient is same as x */
    long sign = IK_BNFST_NEGATIVE(first_word);
    quot = ik_normalize_bignum(s2n, sign, quot);
  }

  /* the remainder is always less than |y|, so it will
     always be a fixnum.  (if y == most_negative_fixnum,
     then |remainder| will be at most most_positive_fixnum). */
  if (IK_BNFST_NEGATIVE(first_word)) {
    /* x is negative => remainder is negative */
    rem = (ikptr) -(rv << fx_shift);
  } else {
    rem = IK_FIX(rv);
  }
  pcb->root0 = &quot;
  pcb->root1 = &rem;
  ikptr p = ik_safe_alloc(pcb, pair_size);
  pcb->root0 = 0;
  pcb->root1 = 0;
  ref(p, disp_car) = quot;
  ref(p, disp_cdr) = rem;
  return p+pair_tag;
}


ikptr
ikrt_bnfx_modulo (ikptr x, ikptr y /*, ikpcb* pcb */)
{
  long		yint		= IK_UNFIX(y);
  mp_limb_t*	s2p		= (mp_limb_t*)(long)(x + off_bignum_data);
  ikptr		first_word	= IK_REF(x, off_bignum_tag);
  mp_size_t	s2n		= IK_BNFST_LIMB_COUNT(first_word);
  if (yint < 0) {
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* x negative, y negative */
      mp_limb_t m = mpn_mod_1(s2p, s2n, -yint);
      return IK_FIX(-m);
    } else {
      /* x positive, y negative */
      mp_limb_t m = mpn_mod_1(s2p, s2n, -yint);
      return IK_FIX(yint+m);
    }
  } else {
    if (IK_BNFST_NEGATIVE(first_word)) {
      /* x negative, y positive */
      mp_limb_t m = mpn_mod_1(s2p, s2n, yint);
      return IK_FIX(yint-m);
    } else {
      /* x positive, y positive */
      mp_limb_t m = mpn_mod_1(s2p, s2n, yint);
      return IK_FIX(m);
    }
  }
}


static int
limb_length(ik_ulong n) {
  int i=0;
  while(n != 0) {
    n = n >> 1;
    i++;
  }
  return i;
}


ikptr
ikrt_bignum_length(ikptr x) {
  ikptr first_word = ref(x, -vector_tag);
  mp_limb_t* sp = (mp_limb_t*)(long)(x+off_bignum_data);
  mp_size_t sn = IK_BNFST_LIMB_COUNT(first_word);
  mp_limb_t last = sp[sn-1];
  int n0 = limb_length(last);
  if (((ik_ulong) first_word) & bignum_sign_mask) {
    /* negative */
    if (last == (mp_limb_t)(1L<<(n0-1))) {
      /* single bit set in last limb */
      int i;
      for(i=0; i<(sn-1); i++) {
	if (sp[i] != 0) {
	  /* another bit set */
	  return IK_FIX((sn-1)*mp_bits_per_limb + n0);
	}
      }
      /* number is - #b100000000000000000000000000 */
      /* fxnot(n) =  #b011111111111111111111111111 */
      /* so, subtract 1. */
      return IK_FIX((sn-1)*mp_bits_per_limb + n0 - 1);
    } else {
      return IK_FIX((sn-1)*mp_bits_per_limb + n0);
    }
  } else {
    return IK_FIX((sn-1)*mp_bits_per_limb + n0);
  }
}


ikptr
ikrt_bignum_to_bytevector(ikptr x, ikpcb* pcb) {
  /* FIXME: avoid calling malloc, instead, use the heap pointer itself
   * as a buffer to hold the temporary data after ensuring that it has enough
   * space */
  ikptr first_word = ref(x, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  if (limb_count <= 0)
    ik_abort("BUG: nbtostring: invalid length %ld", limb_count);
  long sign_bit = bignum_sign_mask & (long) first_word;
  long nbsize = limb_count * sizeof(mp_limb_t);
  long strsize = limb_count * max_digits_per_limb;
  long mem_req = nbsize + strsize + 1;
  unsigned char* mem = malloc(mem_req);
  if (! mem)
    ik_abort("error allocating space for bignum");
  memcpy((char*)(long)mem,
	 (char*)(long)x - vector_tag + disp_bignum_data,
	 nbsize);
  mp_size_t bytes =
    mpn_get_str(mem+nbsize,	  /* output string */
		10,		  /* base */
		(mp_limb_t*) mem, /* limb */
		limb_count	  /* number of limbs */
	);
  unsigned char* string_start = mem + nbsize;
  while(*string_start == 0) {
    string_start++;
    bytes--;
  }
  ikptr bv = ik_safe_alloc(pcb, IK_ALIGN(bytes + disp_bytevector_data + (sign_bit?1:0)));
  ref(bv, 0) = IK_FIX(bytes + (sign_bit?1:0));
  char* dest = (char*)(long)(bv + disp_bytevector_data);
  if (sign_bit) {
    *dest = '-';
    dest++;
  }
  {
    long i = 0;
    while(i < bytes) {
      dest[i] = string_start[i] + '0';
      i++;
    }
    dest[bytes] = 0;
  }
  free(mem);
  return bv | bytevector_tag;
}


ikptr
ikrt_fxrandom(ikptr x) {
  long mask = 1;
  long n = IK_UNFIX(x);
  {
    while(mask < n) {
      mask = (mask << 1) | 1;
    }
  }
  while(1) {
    long r = random() & mask;
    if (r < n) {
      return IK_FIX(r);
    }
  }
}

static int
limb_size(mp_limb_t x) {
  int i = 0;
  while(x) {
    i++;
    x = x>>1;
  }
  return i;
}

static int
all_zeros(mp_limb_t* start, mp_limb_t* end) {
  while(start <= end) {
    if (*end) return 0;
    end--;
  }
  return 1;
}

#define PRECISION 53

static ikptr
ikrt_bignum_to_flonum64(ikptr bn, ikptr more_bits, ikptr fl) {
  ikptr first_word = ref(bn, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  mp_limb_t* sp = (mp_limb_t*)(long)(bn+off_bignum_data);
  double pos_result;
  if (limb_count == 1) {
    pos_result = sp[0];
  } else {
    mp_limb_t hi = sp[limb_count-1];
    int bc = limb_size(hi);
    if (bc < 64) {
      mp_limb_t mi = sp[limb_count-2];
      hi = (hi << (64-bc)) | (mi >> bc);
    }
    /* now hi has 64 full bits */
    mp_limb_t mask = ((1L<<(64-PRECISION)) - 1);
    if ((hi & mask) == ((mask+1)>>1)) {
      /* exactly at break point */
      if (((sp[limb_count-2] << (64-bc)) == 0) &&
	  all_zeros(sp, sp+limb_count-3) &&
	  (more_bits == 0)) {
	if (hi & (1L<<(64-PRECISION))) {
	  /* odd number, round to even */
	  hi = hi | mask;
	}
      } else {
	/* round up */
	hi = hi | mask;
      }
    } else if ((hi & mask) > ((mask+1)>>1)) {
      /* also round up */
      hi = hi | mask;
    } else {
      /* keep it to round down */
    }
    pos_result = hi;
    int bignum_bits = bc + (mp_bits_per_limb * (limb_count-1));
    int exponent = bignum_bits - mp_bits_per_limb;
    while(exponent) {
      pos_result *= 2.0;
      exponent -= 1;
    }
  }
  if (IK_BNFST_NEGATIVE(first_word)) {
    IK_FLONUM_DATA(fl)	= - pos_result;
  } else {
    IK_FLONUM_DATA(fl) = pos_result;
  }
  return fl;
}

ikptr
ikrt_bignum_to_flonum(ikptr bn, ikptr more_bits, ikptr fl) {
  if (mp_bits_per_limb == 64) {
    return ikrt_bignum_to_flonum64(bn, more_bits, fl);
  }
  ikptr first_word = ref(bn, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  mp_limb_t* sp = (mp_limb_t*)(long)(bn+off_bignum_data);
  double pos_result;
  if (limb_count == 1) {
    pos_result = sp[0];
  } else if (limb_count == 2) {
    mp_limb_t lo = sp[0];
    mp_limb_t hi = sp[1];
    pos_result = hi;
    pos_result = pos_result * 4294967296.0;
    pos_result = pos_result + lo;
  } else {
    mp_limb_t hi = sp[limb_count-1];
    mp_limb_t mi = sp[limb_count-2];
    int bc = limb_size(hi);
    if (bc < 32) {
      mp_limb_t lo = sp[limb_count-3];
      hi = (hi << (32-bc)) | (mi >> bc);
      mi = (mi << (32-bc)) | (lo >> bc);
    }
    /* now hi has 32 full bits, and mi has 32 full bits */
    mp_limb_t mask = ((1<<(64-PRECISION)) - 1);
    if ((mi & mask) == ((mask+1)>>1)) {
      /* exactly at break point */
      if (((sp[limb_count-3] << (32-bc)) == 0) &&
	  all_zeros(sp, sp+limb_count-4) &&
	  (more_bits == 0)) {
	if (mi & (1<<(64-PRECISION))) {
	  /* odd number, round to even */
	  mi = mi | mask;
	}
      } else {
	/* round up */
	mi = mi | mask;
      }
    } else if ((mi & mask) > ((mask+1)>>1)) {
      /* also round up */
      mi = mi | mask;
    } else {
      /* keep it to round down */
    }
    pos_result = hi;
    pos_result = pos_result * 4294967296.0;
    pos_result = pos_result + mi;
    int bignum_bits = bc + (mp_bits_per_limb * (limb_count-1));
    int exponent = bignum_bits - (2 * mp_bits_per_limb);
    while(exponent) {
      pos_result *= 2.0;
      exponent -= 1;
    }
  }
  if (IK_BNFST_NEGATIVE(first_word)) {
    IK_FLONUM_DATA(fl)	= - pos_result;
  } else {
    IK_FLONUM_DATA(fl) = pos_result;
  }
  return fl;
}

ikptr
ikrt_exact_fixnum_sqrt(ikptr fx /*, ikpcb* pcb*/) {
  mp_limb_t x = IK_UNFIX(fx);
  mp_limb_t s;
  mp_limb_t r;
  mpn_sqrtrem(&s, &r, &x, 1);
  return IK_FIX(s);
}

ikptr
ikrt_exact_bignum_sqrt(ikptr bn, ikpcb* pcb) {
  ikptr first_word = ref(bn, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  long result_limb_count = (limb_count + 1)/2;
  pcb->root0 = &bn;
  ikptr s = ik_safe_alloc(pcb,
	    IK_ALIGN(disp_bignum_data+result_limb_count*wordsize))
	  | vector_tag;
  ref(s, -vector_tag) =
    (ikptr) (bignum_tag | (result_limb_count << bignum_nlimbs_shift));
  pcb->root1 = &s;
  ikptr r = ik_safe_alloc(pcb,
	      IK_ALIGN(disp_bignum_data+limb_count*wordsize))
	  | vector_tag;
  ref(r, -vector_tag) =
    (ikptr) (bignum_tag | (limb_count << bignum_nlimbs_shift));
  pcb->root0 = &r;
  ikptr pair = ik_safe_alloc(pcb, pair_size) | pair_tag;
  pcb->root0 = 0;
  pcb->root1 = 0;
  mp_size_t r_actual_limbs = mpn_sqrtrem(
      (mp_limb_t*) (s+off_bignum_data),
      (mp_limb_t*) (r+off_bignum_data),
      (mp_limb_t*) (bn+off_bignum_data),
      limb_count);
  ref(pair, off_car) = ik_normalize_bignum(result_limb_count, 0, s-vector_tag);
  if (r_actual_limbs == 0) {
    /* perfect square */
    ref(pair, off_cdr) = 0;
  } else {
    ref(pair, off_cdr) = ik_normalize_bignum(r_actual_limbs, 0, r-vector_tag);
  }
  return pair;
}


ikptr
ikrt_flonum_hash(ikptr x /*, ikpcb* pcb */) {
  short* buf = (short*)(x+off_flonum_data);
  return IK_FIX(((long)buf[0]) ^
	     ((long)buf[1] << 3) ^
	     ((long)buf[3] << 7) ^
	     ((long)buf[2] << 11));
}
ikptr
ikrt_bignum_hash(ikptr bn /*, ikpcb* pcb */) {
  ikptr first_word = ref(bn, -vector_tag);
  long limb_count = IK_BNFST_LIMB_COUNT(first_word);
  long h = (long)first_word;
  mp_limb_t* dat = (mp_limb_t*)(bn+off_bignum_data);
  long i;
  for (i=0; i<limb_count; i++) {
    h = (h^dat[i]) << 3;
  }
  return IK_FIX(h);
}

/* end of file */
