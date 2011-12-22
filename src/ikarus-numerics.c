/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "ikarus.h"
#include <gmp.h>

#ifdef NDEBUG
#define verify_bignum(x,caller) (x)
#else
static ikptr
verify_bignum(ikptr x, char* caller){
  if(IK_TAGOF(x) != vector_tag){
    fprintf(stderr, "Error in (%s) invalid primary tag 0x%016lx\n", caller, x);
    exit(EXIT_FAILURE);
  }
  ikptr fst = ref(x, -vector_tag);
  long int limb_count = ((unsigned long int) fst) >> bignum_length_shift;
  if(limb_count <= 0){
    fprintf(stderr,
        "Error in (%s) invalid limb count in fst=0x%016lx\n",
        caller, (long int)fst);
    exit(EXIT_FAILURE);
  }
  int pos;
  if((long int)fst & bignum_sign_mask){
    pos = 0;
  } else {
    pos = 1;
  }
  mp_limb_t last_limb =
    (mp_limb_t) ref(x, off_bignum_data + (limb_count - 1) * wordsize);
  if(last_limb == 0){
    fprintf(stderr,
        "Error in (%s) invalid last limb = 0x%016lx", caller, last_limb);
    exit(EXIT_FAILURE);
  }
  if(limb_count == 1){
    if(pos){
      if(last_limb <= most_positive_fixnum){
        fprintf(stderr,
                "Error in (%s) should be a positive fixnum: 0x%016lx\n",
                caller, last_limb);
        exit(EXIT_FAILURE);
      }
    } else {
      if(last_limb <= most_negative_fixnum){
        fprintf(stderr,
                "Error in (%s) should be a negative fixnum: 0x%016lx\n",
                caller, last_limb);
        exit(EXIT_FAILURE);
      }
    }
  }
  /* ok */
  return x;
}
#endif

#define BN(x) verify_bignum(x,"BN")

#if 0
ikptr
ikrt_isbignum(ikptr x){
  if(IK_TAGOF(x) == vector_tag){
    ikptr fst = ref(x, -vector_tag);
    if (bignum_tag == (bignum_mask & (int)fst)){
      return true_object;
    }
  }
  return false_object;
}
#endif

ikptr
ikrt_positive_bn(ikptr x){
  ikptr fst = ref(x, -vector_tag);
  if(bnfst_negative(fst)){
    return false_object;
  } else {
    return true_object;
  }
}

ikptr
ikrt_even_bn(ikptr x){
  long int fst = (long int)ref(x, wordsize-vector_tag);
  if(fst & 1){
    return false_object;
  } else {
    return true_object;
  }
}



ikptr
ikrt_fxfxplus(ikptr x, ikptr y, ikpcb* pcb){
  long int n1 = unfix(x);
  long int n2 = unfix(y);
  long int r = n1 + n2;
  ikptr q = fix(r);
  if(r == unfix(q)){
    return q;
  }
  else {
    ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + wordsize));
    if(r > 0){
      ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikptr)r;
    }
    else {
      ref(bn, 0) =
        (ikptr)(bignum_tag |
              (1 << bignum_length_shift) |
              (1 << bignum_sign_shift));
      ref(bn, disp_bignum_data) = (ikptr)-r;
    }
    return verify_bignum(bn+vector_tag, "fxfx+");
  }
}

ikptr
ikrt_fxbnplus(ikptr x, ikptr y, ikpcb* pcb){
  if(x == 0){ return y ; }
  ikptr fst = ref(y, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  long int intx = unfix(x);
  if(intx > 0){
    if(!bnfst_negative(fst)){
      /* positive fx + positive bn = even bigger positive */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      mp_limb_t carry =
        mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
        ref(r, 0) = (ikptr)
             (((limb_count + 1) << bignum_length_shift) |
              (0 << bignum_sign_shift) |
              bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn+1");
      } else {
        ref(r, 0) = (ikptr)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn+2");
      }
    }
    else {
      /* positive fx + negative bn = smaller negative bn */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      mp_limb_t borrow =
        mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow1 %ld\n", borrow);
        exit(EXIT_FAILURE);
      }
      long int result_size =
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize))
        ? limb_count
        : (limb_count - 1);
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        mp_limb_t last =
          (mp_limb_t) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_negative_fixnum){
          return fix(-(long int)last);
        }
      }
      ref(r, 0) = (ikptr)
        ((result_size << bignum_length_shift) |
         (1 << bignum_sign_shift) |
         bignum_tag);
      return verify_bignum(r+vector_tag, "fxbn+3");
    }
  }
  else {
    if(! bnfst_negative(fst)){
      /* negative fx + positive bn = smaller positive */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      mp_limb_t borrow =
        mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  - intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow2\n");
        exit(EXIT_FAILURE);
      }
      long int result_size =
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0)
        ? (limb_count - 1)
        : limb_count;
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        mp_limb_t last =
          (mp_limb_t) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_positive_fixnum){
          return fix((long int)last);
        }
      }
      ref(r, 0) = (ikptr)
        ((result_size << bignum_length_shift) |
         (0 << bignum_sign_shift) |
         bignum_tag);
      return verify_bignum(r+vector_tag, "fxbn+4");
    } else {
      /* negative fx + negative bn = larger negative */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      mp_limb_t carry =
        mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  -intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
        ref(r, 0) = (ikptr)
             (((limb_count + 1) << bignum_length_shift) |
              (1 << bignum_sign_shift) |
              bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn+5");
      } else {
        ref(r, 0) = (ikptr)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn+5");
      }
    }
  }
}




ikptr
ikrt_bnbnplus(ikptr x, ikptr y, ikpcb* pcb){
  unsigned long int xfst = (unsigned long int)ref(x, -vector_tag);
  unsigned long int yfst = (unsigned long int)ref(y, -vector_tag);
  long int xsign = xfst & bignum_sign_mask;
  long int ysign = yfst & bignum_sign_mask;
  long int xlimbs = xfst >> bignum_length_shift;
  long int ylimbs = yfst >> bignum_length_shift;
  if(xsign == ysign){
    long int n1,n2;
    ikptr s1,s2;
    if(xlimbs > ylimbs){
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
    if(carry){
      ref(res, disp_vector_data + xlimbs*wordsize) = (ikptr)1;
      ref(res, 0) = (ikptr)
                    (((n1+1) << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn+1");
    } else {
      ref(res, 0) = (ikptr)
                    ((n1 << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn+2");
    }
  }
  else {
    ikptr s1=x, s2=y;
    long int n1=xlimbs, n2=ylimbs;
    long int result_sign = xsign;
    while((xlimbs == ylimbs) &&
          (ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) ==
           ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
      xlimbs -= 1;
      ylimbs -= 1;
      if(xlimbs == 0){ return 0; }
    }
    /* |x| != |y| */
    if(xlimbs <= ylimbs){
      if(xlimbs == ylimbs){
        if((ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) >
            ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
          s1 = y; n1 = ylimbs;
          s2 = x; n2 = xlimbs;
          result_sign = ysign;
        }
      } else {
        s1 = y; n1 = ylimbs;
        s2 = x; n2 = xlimbs;
        result_sign = ysign;
      }
    }
    /* |s1| > |s2| */
    pcb->root0 = &s1;
    pcb->root1 = &s2;
    ikptr res = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n1 * wordsize));
    pcb->root0 = 0;
    pcb->root1 = 0;
    mp_limb_t burrow =
      mpn_sub((mp_limb_t*)(long)(res + disp_bignum_data),
              (mp_limb_t*)(long)(s1 - vector_tag + disp_bignum_data),
              n1,
              (mp_limb_t*)(long)(s2 - vector_tag + disp_bignum_data),
              n2);
    if(burrow){
      fprintf(stderr, "BUG: Burrow error in bnbn+\n");
      exit(EXIT_FAILURE);
    }
    long int len = n1;
    while(ref(res, disp_bignum_data + (len-1)*wordsize) == 0){
      len--;
      if(len == 0){
        return 0;
      }
    }
    if(result_sign == 0){
      /* positive result */
      if(len == 1){
        mp_limb_t fst_limb = (mp_limb_t) ref(res, disp_bignum_data);
        if(fst_limb <= most_positive_fixnum){
          return fix((long int)fst_limb);
        }
      }
      ref(res, 0) = (ikptr)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn+3");
    } else {
      /* negative result */
      if(len == 1){
        mp_limb_t fst_limb = (mp_limb_t) ref(res, disp_bignum_data);
        if(fst_limb <= most_negative_fixnum){
          return fix(-(long int)fst_limb);
        }
      }
      ref(res, 0) = (ikptr)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn+4");
    }
  }
}




ikptr
ikrt_fxfxminus(ikptr x, ikptr y, ikpcb* pcb){
  long int n1 = unfix(x);
  long int n2 = unfix(y);
  long int r = n1 - n2;
  if(r >= 0){
    if(((unsigned long int)r) <= most_positive_fixnum){
      return fix(r);
    } else {
      ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikptr) (bignum_tag | (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikptr)r;
      return verify_bignum(bn+vector_tag,"fxfx-1");
    }
  } else {
    ikptr fxr = fix(r);
    if(unfix(fxr) == r){
      return fxr;
    } else {
      ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikptr)
        (bignum_tag |
         (1 << bignum_sign_shift) |
         (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikptr)(-r);
      return verify_bignum(bn+vector_tag, "fxfx-2");
    }
  }
}


ikptr
ikrt_bnnegate(ikptr x, ikpcb* pcb){
  ikptr fst = ref(x, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  if(limb_count == 1){
    if(! bnfst_negative(fst)){
      /* positive bignum */
      mp_limb_t limb =
        (mp_limb_t) ref(x, disp_bignum_data - vector_tag);
      if(limb == (most_positive_fixnum + 1)){
        return fix(-(long int)limb);
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
     ((1 << bignum_sign_shift) - (bignum_sign_mask & (long int)fst)) |
     (limb_count << bignum_length_shift));
  return verify_bignum(bn+vector_tag, "bnneg");
}

ikptr
ikrt_fxbnminus(ikptr x, ikptr y, ikpcb* pcb){
  if(x == 0){ return ikrt_bnnegate(y, pcb) ; }
  ikptr fst = ref(y, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  long int intx = unfix(x);
  if(intx > 0){
    if(bnfst_negative(fst)){
      /* positive fx - negative bn = positive bn */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long int carry =
        mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
        ref(r, 0) = (ikptr)
             (((limb_count + 1) << bignum_length_shift) |
              (0 << bignum_sign_shift) |
              bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn-1");
      } else {
        ref(r, 0) = (ikptr)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn-2");
      }
    }
    else {
      /* positive fx - positive bn = smaller negative bn/fx */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long int borrow =
        mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow3\n");
        exit(EXIT_FAILURE);
      }
      long int result_size =
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize))
        ? limb_count
        : (limb_count - 1);
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned long int last =
          (unsigned long int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_negative_fixnum){
          return fix(-(long int)last);
        }
      }
      ref(r, 0) = (ikptr)
        ((result_size << bignum_length_shift) |
         (1 << bignum_sign_shift) |
         bignum_tag);
      return verify_bignum(r+vector_tag, "fxbn-");
    }
  }
  else {
    if(bnfst_negative(fst)){
      /* negative fx - negative bn = smaller positive */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long int borrow =
        mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  - intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow4\n");
        exit(EXIT_FAILURE);
      }
      long int result_size =
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0)
        ? (limb_count - 1)
        : limb_count;
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned long int last =
          (unsigned long int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_positive_fixnum){
          return fix((long int)last);
        }
      }
      ref(r, 0) = (ikptr)
        ((result_size << bignum_length_shift) |
         (0 << bignum_sign_shift) |
         bignum_tag);
      return verify_bignum(r+vector_tag,"fxbn-");
    } else {
      /* negative fx - positive bn = larger negative */
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long int carry =
        mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(y - vector_tag + disp_bignum_data),
                  limb_count,
                  -intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
        ref(r, 0) = (ikptr)
             (((limb_count + 1) << bignum_length_shift) |
              (1 << bignum_sign_shift) |
              bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn-");
      } else {
        ref(r, 0) = (ikptr)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn-");
      }
    }
  }
}

ikptr
ikrt_bnfxminus(ikptr x, ikptr y, ikpcb* pcb){
  if(y == 0){ return x; }
  ikptr fst = ref(x, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  long int inty = unfix(y);
  if(inty < 0){
    if(!bnfst_negative(fst)){
      /* - negative fx + positive bn = positive bn */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long int carry =
        mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
                  limb_count,
                  -inty);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
        ref(r, 0) = (ikptr)
             (((limb_count + 1) << bignum_length_shift) |
              (0 << bignum_sign_shift) |
              bignum_tag);
        return verify_bignum(r+vector_tag,"bnfx-");
      } else {
        ref(r, 0) = (ikptr)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag,"bnfx-");
      }
    }
    else {
      /* - negative fx + negative bn = smaller negative bn/fx */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long int borrow =
        mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
                  limb_count,
                  -inty);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow5\n");
        exit(EXIT_FAILURE);
      }
      long int result_size =
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize))
        ? limb_count
        : (limb_count - 1);
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned long int last =
          (unsigned long int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_negative_fixnum){
          return fix(-(long int)last);
        }
      }
      ref(r, 0) = (ikptr)
        ((result_size << bignum_length_shift) |
         (1 << bignum_sign_shift) |
         bignum_tag);
      return verify_bignum(r+vector_tag,"bnfx-");
    }
  }
  else {
    if((bignum_sign_mask & (long int)fst) == 0){
      /* - positive fx + positive bn = smaller positive */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+limb_count*wordsize));
      pcb->root0 = 0;
      long int borrow =
        mpn_sub_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
                  limb_count,
                  inty);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow6\n");
        exit(EXIT_FAILURE);
      }
      long int result_size =
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0)
        ? (limb_count - 1)
        : limb_count;
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned long int last =
          (unsigned long int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_positive_fixnum){
          return fix((long int)last);
        }
      }
      ref(r, 0) = (ikptr)
        ((result_size << bignum_length_shift) |
         (0 << bignum_sign_shift) |
         bignum_tag);
      return verify_bignum(r+vector_tag, "bnfx-");
    } else {
      /* - positive fx + negative bn = larger negative */
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(limb_count+1)*wordsize));
      pcb->root0 = 0;
      long int carry =
        mpn_add_1((mp_limb_t*)(long)(r+disp_bignum_data),
                  (mp_limb_t*)(long)(x - vector_tag + disp_bignum_data),
                  limb_count,
                  inty);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikptr)1;
        ref(r, 0) = (ikptr)
             (((limb_count + 1) << bignum_length_shift) |
              (1 << bignum_sign_shift) |
              bignum_tag);
        return verify_bignum(r+vector_tag, "bnfx-");
      } else {
        ref(r, 0) = (ikptr)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "bnfx-");
      }
    }
  }
}



ikptr
ikrt_bnbnminus(ikptr x, ikptr y, ikpcb* pcb){
  if(x == y) { return 0; }
  unsigned long int xfst = (unsigned long int)ref(x, -vector_tag);
  unsigned long int yfst = (unsigned long int)ref(y, -vector_tag);
  long int xsign = xfst & bignum_sign_mask;
  long int ysign = yfst & bignum_sign_mask;
  long int xlimbs = xfst >> bignum_length_shift;
  long int ylimbs = yfst >> bignum_length_shift;
  if(xsign != ysign){
    long int n1,n2;
    ikptr s1,s2;
    if(xlimbs >= ylimbs){
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
    if(carry){
      ref(res, disp_vector_data + xlimbs*wordsize) = (ikptr)1;
      ref(res, 0) = (ikptr)
                    (((n1+1) << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn-");
    } else {
      ref(res, 0) = (ikptr)
                    ((n1 << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn-");
    }
  }
  else {
    /* same sign */
    if(xlimbs == ylimbs){
      while((ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) ==
             ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
        xlimbs -= 1;
        if(xlimbs == 0){ return 0; }
      }
      ylimbs = xlimbs;
    }
    ikptr s1=x, s2=y;
    long int n1=xlimbs, n2=ylimbs;
    long int result_sign = xsign;
    /* |x| != |y| */
    if(xlimbs <= ylimbs){
      if(xlimbs == ylimbs){
        if((ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) >
            ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
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
    long int burrow =
      mpn_sub((mp_limb_t*)(long)(res + disp_bignum_data),
              (mp_limb_t*)(long)(s1 - vector_tag + disp_bignum_data),
              n1,
              (mp_limb_t*)(long)(s2 - vector_tag + disp_bignum_data),
              n2);
    if(burrow){
      fprintf(stderr, "BUG: Burrow error in bnbn-\n");
      exit(EXIT_FAILURE);
    }
    long int len = n1;
    while(ref(res, disp_bignum_data + (len-1)*wordsize) == 0){
      len--;
      if(len == 0){
        return 0;
      }
    }
    if(result_sign == 0){
      /* positive result */
      if(len == 1){
        unsigned long int fst_limb =
          (unsigned long int) ref(res, disp_bignum_data);
        if(fst_limb <= most_positive_fixnum){
          return fix((long int)fst_limb);
        }
      }
      ref(res, 0) = (ikptr)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn-");
    } else {
      /* negative result */
      if(len == 1){
        unsigned long int fst_limb =
          (unsigned long int) ref(res, disp_bignum_data);
        if(fst_limb <= most_negative_fixnum){
          return fix(-(long int)fst_limb);
        }
      }
      ref(res, 0) = (ikptr)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn-");
    }
  }
}


ikptr
ikrt_fxfxmult(ikptr x, ikptr y, ikpcb* pcb){
  long int n1 = unfix(x);
  long int n2 = unfix(y);
  mp_limb_t lo = 0;
  mp_limb_t s1 = n1;
  mp_limb_t s2 = n2;
  long int sign = 0;
  if(n1 < 0){
    s1 = -n1;
    sign = 1 - sign;
  }
  if(n2 < 0){
    s2 = -n2;
    sign = 1 - sign;
  }
  mp_limb_t hi = mpn_mul_1(&lo, &s1, 1, s2);
  if(hi == 0){
    if(sign){
      if(lo <= most_negative_fixnum){
        return fix(-((long int)lo));
      }
    } else {
      if(lo <= most_positive_fixnum){
        return fix((long int)lo);
      }
    }
    ikptr r = ik_safe_alloc(pcb, disp_bignum_data + wordsize);
    ref(r, 0) = (ikptr)
      (bignum_tag |
       (sign << bignum_sign_shift) |
       (1 << bignum_length_shift));
    ref(r, disp_bignum_data) = (ikptr)lo;
    return BN(r+vector_tag);
  } else {
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + 2*wordsize));
    ref(r, 0) = (ikptr)
      (bignum_tag |
       (sign << bignum_sign_shift) |
       (2 << bignum_length_shift));
    ref(r, disp_bignum_data) = (ikptr)lo;
    ref(r, disp_bignum_data+wordsize) = (ikptr)hi;
    return BN(r+vector_tag);
  }
}

ikptr
normalize_bignum(long int limbs, int sign, ikptr r){
  while(ref(r, disp_bignum_data + (limbs-1)*wordsize) == 0){
    limbs--;
    if(limbs == 0){ return 0;}
  }
  if(limbs == 1){
    mp_limb_t last = (mp_limb_t) ref(r, disp_bignum_data);
    if(sign == 0){
      if(last <= most_positive_fixnum){
        return fix(last);
      }
    } else {
      if(last <= most_negative_fixnum){
        return fix(-(last));
      }
    }
  }
  ref(r, 0) = (ikptr) (bignum_tag | sign | (limbs << bignum_length_shift));
  return BN(r+vector_tag);
}


ikptr
ikrt_fxbnmult(ikptr x, ikptr y, ikpcb* pcb){
  long int n2 = unfix(x);
  if(n2 == 0) { return 0; }
  mp_limb_t s2 = (n2>0) ? n2 : (- n2);
  ikptr fst = ref(y, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  pcb->root0 = &y;
  ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (limb_count+1)*wordsize));
  pcb->root0 = 0;
  mp_limb_t hi = mpn_mul_1((mp_limb_t*)(long)(r+disp_bignum_data),
                           (mp_limb_t*)(long)(y-vector_tag+disp_bignum_data),
                           limb_count,
                           s2);
  ref(r, disp_bignum_data + limb_count * wordsize) = (ikptr)hi;
  long int sign =
    ((n2 > 0) ?
     (bignum_sign_mask & (long int)fst) :
     ((1 << bignum_sign_shift) - (bignum_sign_mask&(long int)fst)));
  return normalize_bignum(limb_count+1, sign, r);
}

ikptr
ikrt_bnbnmult(ikptr x, ikptr y, ikpcb* pcb){
  long int f1 = (long int)ref(x, -vector_tag);
  long int f2 = (long int)ref(y, -vector_tag);
  long int n1 = bnfst_limb_count(f1);
  long int n2 = bnfst_limb_count(f2);
  long int nr = n1 + n2;
  pcb->root0 = &x;
  pcb->root1 = &y;
  ikptr bn = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + nr*wordsize));
  pcb->root0 = 0;
  pcb->root1 = 0;
  if(n1 >= n2){
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
  long int sign =
    ((bignum_sign_mask & f1) ?
     ((1 << bignum_sign_shift) - (bignum_sign_mask & f2)) :
     (bignum_sign_mask & f2));
  return normalize_bignum(nr, sign, bn);
}




ikptr
ikrt_bnbncomp(ikptr bn1, ikptr bn2){
  ikptr f1 = ref(bn1, -vector_tag);
  ikptr f2 = ref(bn2, -vector_tag);
  if(bnfst_negative(f1)){
    if(bnfst_negative(f2)){
      /* both negative */
      long int n1 = ((mp_limb_t) f1) >> bignum_length_shift;
      long int n2 = ((mp_limb_t) f2) >> bignum_length_shift;
      if(n1 < n2) {
        return fix(1);
      } else if(n1 > n2){
        return fix(-1);
      } else {
        long int i;
        for(i=(n1-1); i>=0; i--){
          mp_limb_t t1 =
            (mp_limb_t) ref(bn1,disp_bignum_data-vector_tag+i*wordsize);
          mp_limb_t t2 =
            (mp_limb_t) ref(bn2,disp_bignum_data-vector_tag+i*wordsize);
          if(t1 < t2){
            return fix(1);
          } else if(t1 > t2){
            return fix(-1);
          }
        }
      }
      return 0;
    } else {
      /* n1 negative, n2 positive */
      return fix(-1);
    }
  } else {
    if(bnfst_negative(f2)){
      /* n1 positive, n2 negative */
      return fix(1);
    } else {
      /* both positive */
      long int n1 = ((mp_limb_t) f1) >> bignum_length_shift;
      long int n2 = ((mp_limb_t) f2) >> bignum_length_shift;
      if(n1 < n2) {
        return fix(-1);
      } else if(n1 > n2){
        return fix(1);
      } else {
        long int i;
        for(i=(n1-1); i>=0; i--){
          mp_limb_t t1 =
           (mp_limb_t) ref(bn1,disp_bignum_data-vector_tag+i*wordsize);
          mp_limb_t t2 =
            (mp_limb_t) ref(bn2,disp_bignum_data-vector_tag+i*wordsize);
          if(t1 < t2){
            return fix(-1);
          } else if(t1 > t2){
            return fix(1);
          }
        }
      }
      return 0;
    }
  }
}


static inline int
count_leading_ffs(int n, mp_limb_t* x){
  int idx;
  for(idx=0; idx<n; idx++){
    if(x[idx] != (mp_limb_t)-1){
      return idx;
    }
  }
  return n;
}


static void
copy_limbs(mp_limb_t* src, mp_limb_t* dst, int n1, int n2){
  while(n1 < n2){
    dst[n1] = src[n1];
    n1++;
  }
}

static void
bits_compliment(mp_limb_t* src, mp_limb_t* dst, long int n){
  mp_limb_t carry = 1;
  long int i;
  for(i=0; i<n; i++){
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static void
bits_compliment2(mp_limb_t* src, mp_limb_t* dst, int n1, int n2){
  mp_limb_t carry = 1;
  int i;
  for(i=0; i<n1; i++){
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
  for(i=n1; i<n2; i++){
    mp_limb_t d = 0;
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static int
bits_compliment_carry(mp_limb_t* src, mp_limb_t* dst, int n1, int n2, mp_limb_t carry){
  int i;
  for(i=n1; i<n2; i++){
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
  return carry;
}




static void
bits_compliment_with_carry(mp_limb_t* src, mp_limb_t* dst, long int n, long int carry){
  long int i;
  for(i=0; i<n; i++){
    mp_limb_t d = src[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static void
bits_compliment_logand(mp_limb_t* s1, mp_limb_t* s2, mp_limb_t* dst, int n){
  int carry = 1;
  int i;
  for(i=0; i<n; i++){
    mp_limb_t d = s1[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c & s2[i];
    carry = (carry && ! d);
  }
}



static int
bits_compliment_logor(mp_limb_t* s1, mp_limb_t* s2, mp_limb_t* dst, int n){
  int carry = 1;
  int i;
  for(i=0; i<n; i++){
    mp_limb_t d = s1[i];
    mp_limb_t c = carry + ~ d;
    dst[i] = c | s2[i];
    carry = (carry && ! d);
  }
  return carry;
}


static long int
bits_carry(mp_limb_t* s,  int n){
  /*
  int carry = 1;
  int i;
  for(i=0; i<n; i++){
    mp_limb_t d = s[i];
    carry = (carry && ! d);
  }
  return carry;
  */
  int i;
  for(i=0; i<n; i++){
    if (s[i] != 0){
      return 0;
    }
  }
  return 1;
}

ikptr
ikrt_bnlognot(ikptr x, ikpcb* pcb){
  ikptr fst = ref(x, -vector_tag);
  long int n = bnfst_limb_count(fst);
  if(bnfst_negative(fst)){
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
    for(i++; i<n; i++){
      rd[i] = s1[i];
    }
    return normalize_bignum(n, 0, r);
  } else {
    /* positive */
    long int i;
    mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
    for(i=0; (i<n) && (s1[i] == (mp_limb_t)-1); i++) {/*nothing*/}
    if(i==n){
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (n+1)*wordsize));
      pcb->root0 = 0;
      bzero((char*)(long)r+disp_bignum_data, n*wordsize);
      ((mp_limb_t*)(long)(r+disp_bignum_data))[n] = 1;
      ref(r, 0) = (ikptr)
        (bignum_tag | (1<<bignum_sign_shift) | ((n+1) << bignum_length_shift));
      return r+vector_tag;
    } else {
      pcb->root0 = &x;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + n*wordsize));
      pcb->root0 = 0;
      mp_limb_t* s1 = (mp_limb_t*)(long)(x+disp_bignum_data-vector_tag);
      mp_limb_t* rd = (mp_limb_t*)(long)(r+disp_bignum_data);
      int j;
      for(j=0; j<i; j++){ rd[j] = 0; }
      rd[i] = s1[i] + 1;
      for(j=i+1; j<n; j++){ rd[j] = s1[j]; }
      ref(r, 0) = (ikptr)
        (bignum_tag | (1<<bignum_sign_shift) | (n << bignum_length_shift));
      return r+vector_tag;
    }
  }
}


ikptr
ikrt_fxbnlogand(ikptr x, ikptr y, ikpcb* pcb){
  long int n1 = unfix(x);
  ikptr fst = ref(y, -vector_tag);
  if(n1 >= 0){
    /* x is positive */
    if(bnfst_negative(fst)){
      /* y is negative */
      return fix(n1 & (1+~(long int)ref(y, disp_vector_data-vector_tag)));
    } else {
      /* y is positive */
      return fix(n1 & (long int)ref(y, disp_vector_data-vector_tag));
    }
  } else {
    /* x is negative */
    if(n1 == -1){ return y; }
    if(bnfst_negative(fst)){
      /* y is negative */
      long int len = bnfst_limb_count(fst);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (len+1)*wordsize));
      pcb->root0 = 0;
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      bits_compliment2(s2, s, len, len+1);
      s[0] = s[0] & n1;
      bits_compliment2(s, s, len+1, len+1);
      return normalize_bignum(len+1, 1<<bignum_sign_shift, r);
    } else {
      /* y is positive */
      long int len = bnfst_limb_count(fst);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + len * wordsize));
      pcb->root0 = 0;
      ref(r, 0) = fst;
      ref(r, disp_bignum_data) = (ikptr)
        (((long int)ref(y, disp_bignum_data - vector_tag)) & n1);
      int i;
      for(i=1; i<len; i++){
        ref(r, disp_bignum_data+i*wordsize) =
          ref(y, disp_bignum_data-vector_tag+i*wordsize);
      }
      return BN(r+vector_tag);
    }
  }
}

ikptr
ikrt_bnbnlogand(ikptr x, ikptr y, ikpcb* pcb){
  ikptr xfst = ref(x, -vector_tag);
  ikptr yfst = ref(y, -vector_tag);
  long int n1 = bnfst_limb_count(xfst);
  long int n2 = bnfst_limb_count(yfst);
  if(bnfst_negative(xfst)){
    if(bnfst_negative(yfst)){
      if(n1 >= n2){
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
        return normalize_bignum(n1+1, 1<<bignum_sign_shift, r);
      } else {
        return ikrt_bnbnlogand(y,x,pcb);
      }
    } else {
      return ikrt_bnbnlogand(y,x,pcb);
    }
  } else {
    if(bnfst_negative(yfst)){
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
      if(n1 <= n2){
        bits_compliment_logand(s2, s1, s, n1);
      } else {
        bits_compliment_logand(s2, s1, s, n2);
        copy_limbs(s1, s, n2, n1);
      }
      return normalize_bignum(n1, 0, r);
    } else {
      /* both positive */
      int n = (n1<n2)?n1:n2;
      long int i;
      for(i=n-1; i>=0; i--){
        long int l1 =
          (long int) ref(x, disp_bignum_data-vector_tag+i*wordsize);
        long int l2 =
          (long int) ref(y, disp_bignum_data-vector_tag+i*wordsize);
        unsigned long int last = l1 & l2;
        if(last){
          if((i == 0) && (last < most_positive_fixnum)){
            return fix(last);
          }
          pcb->root0 = &x;
          pcb->root1 = &y;
          ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data+(i+1)*wordsize));
          pcb->root0 = 0;
          pcb->root1 = 0;
          ref(r, 0) = (ikptr) (bignum_tag | ((i+1)<<bignum_length_shift));
          ref(r, disp_bignum_data + i*wordsize) = (ikptr)last;
          int j;
          for(j=0; j<i; j++){
            ref(r, disp_bignum_data + j*wordsize) = (ikptr)
              (((long int)ref(x, disp_bignum_data-vector_tag+j*wordsize))
               &
               ((long int)ref(y, disp_bignum_data-vector_tag+j*wordsize)));
          }
          return r+vector_tag;
        }
      }
      return 0;
    }
  }
}


ikptr
ikrt_fxbnlogor(ikptr x, ikptr y, ikpcb* pcb){
  long int n1 = unfix(x);
  ikptr fst = ref(y, -vector_tag);
  if(n1 < 0){
    /* x is negative */
    if(bnfst_negative(fst)){
      /* y is negative */
      return fix(n1 | (1+~(long int)ref(y, disp_vector_data-vector_tag)));
    } else {
      /* y is positive */
      return fix(n1 | (long int)ref(y, disp_vector_data-vector_tag));
    }
  } else {
    /* x is non negative */
    if(n1 == 0){ return y; }
    /* x is positive */
    if(bnfst_negative(fst)){
      /* y is negative */
      long int len = bnfst_limb_count(fst);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + (len+1)*wordsize));
      pcb->root0 = 0;
      mp_limb_t* s2 = (mp_limb_t*)(long)(y+disp_bignum_data-vector_tag);
      mp_limb_t* s = (mp_limb_t*)(long)(r+disp_bignum_data);
      bits_compliment2(s2, s, len, len+1);
      s[0] = s[0] | n1;
      bits_compliment2(s, s, len+1, len+1);
      return normalize_bignum(len+1, 1<<bignum_sign_shift, r);
    } else {
      /* y is positive */
      long int len = bnfst_limb_count(fst);
      pcb->root0 = &y;
      ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + len * wordsize));
      pcb->root0 = 0;
      ref(r, 0) = fst;
      ref(r, disp_bignum_data) = (ikptr)
        (((long int)ref(y, disp_bignum_data - vector_tag)) | n1);
      int i;
      for(i=1; i<len; i++){
        ref(r, disp_bignum_data+i*wordsize) =
          ref(y, disp_bignum_data-vector_tag+i*wordsize);
      }
      return BN(r+vector_tag);
    }
  }
}

ikptr
ikrt_bnbnlogor(ikptr x, ikptr y, ikpcb* pcb){
  ikptr xfst = ref(x, -vector_tag);
  ikptr yfst = ref(y, -vector_tag);
  long int n1 = bnfst_limb_count(xfst);
  long int n2 = bnfst_limb_count(yfst);
  if(bnfst_negative(xfst)){
    if(bnfst_negative(yfst)){
      if(n1 >= n2){
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
        return normalize_bignum(n1, 1<<bignum_sign_shift, r);
      } else {
        return ikrt_bnbnlogor(y,x,pcb);
      }
    } else {
      return ikrt_bnbnlogor(y,x,pcb);
    }
  } else {
    if(bnfst_negative(yfst)){
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
      if(n2 <= n1){
        bits_compliment_logor(s2, s1, s, n2);
        bits_compliment2(s, s, n2, n2);
      } else {
        int carry = bits_compliment_logor(s2, s1, s, n1);
        bits_compliment_carry(s2, s, n1, n2, carry);
        bits_compliment_carry(s, s, 0, n2, 1);
      }
      return normalize_bignum(n2, 1<<bignum_sign_shift, r);
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
      long int i;
      if(n == n1){
        for(i=0; i<n2; i++){
          s[i] = s1[i] | s2[i];
        }
        for(i=n2; i<n1; i++){
          s[i] = s1[i];
        }
      } else {
        for(i=0; i<n1; i++){
          s[i] = s1[i] | s2[i];
        }
        for(i=n1; i<n2; i++){
          s[i] = s2[i];
        }
      }
      return normalize_bignum(n, 0, r);
    }
  }
}

static void
copy_bits_shifting_right(mp_limb_t* src, mp_limb_t* dst, int n, int m){
  mp_limb_t carry = src[0] >> m;
  int i;
  for(i=1; i<n; i++){
    mp_limb_t b = src[i];
    dst[i-1] = (b << (mp_bits_per_limb-m)) | carry;
    carry = b >> m;
  }
  dst[n-1] = carry;
}

static void
copy_bits_shifting_left(mp_limb_t* src, mp_limb_t* dst, int n, int m){
  mp_limb_t carry = 0;
  int i;
  for(i=0; i<n; i++){
    mp_limb_t b = src[i];
    dst[i] = (b << m) | carry;
    carry = b >> (mp_bits_per_limb-m);
  }
  dst[n] = carry;
}





ikptr
ikrt_bignum_shift_right(ikptr x, ikptr y, ikpcb* pcb){
  int limb_shift = (wordsize == 4 ? 5 : 6);
  long int m = unfix(y);
  ikptr fst = ref(x, -vector_tag);
  long int n = bnfst_limb_count(fst);
  long int whole_limb_shift = m >> limb_shift;
  long int bit_shift = m & (mp_bits_per_limb-1);
  long int new_limb_count = n - whole_limb_shift;
  if(bnfst_negative(fst)){
    if(new_limb_count <= 0){
      return fix(-1);
    }
    if(bit_shift == 0){
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
      return normalize_bignum(new_limb_count, 1 << bignum_sign_shift, r);
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
      return normalize_bignum(new_limb_count, 1 << bignum_sign_shift, r);
      fprintf(stderr, "not yet for negative bignum_shift\n");
      exit(EXIT_FAILURE);
    }
  } else {
    if(new_limb_count <= 0){
      return 0;
    }
    pcb->root0 = &x;
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + new_limb_count * wordsize));
    pcb->root0 = 0;
    if(bit_shift == 0){
      memcpy((char*)(long)r+disp_bignum_data,
             (char*)(long)x+off_bignum_data+whole_limb_shift*wordsize,
             new_limb_count * wordsize);
      return normalize_bignum(new_limb_count, 0, r);
    } else {
      copy_bits_shifting_right(
          (mp_limb_t*)(long)(x+off_bignum_data+whole_limb_shift*wordsize),
          (mp_limb_t*)(long)(r+disp_bignum_data),
          new_limb_count,
          bit_shift);
      return normalize_bignum(new_limb_count, 0, r);
    }
  }
}


ikptr
ikrt_fixnum_shift_left(ikptr x, ikptr y, ikpcb* pcb){
  int limb_shift = (wordsize == 4 ? 5 : 6);
  long int m = unfix(y);
  long int n = unfix(x);
  long int limb_count = (m >> limb_shift) + 2;
  long int bit_shift = m & (mp_bits_per_limb-1);
  ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + limb_count * wordsize));
  unsigned long int* s = (unsigned long int*)(long)(r+disp_bignum_data);
  bzero(s, limb_count * wordsize);
  if(n >= 0){
    if(bit_shift){
      s[limb_count-1] = n >> (mp_bits_per_limb - bit_shift);
    }
    s[limb_count-2] = n << bit_shift;
  } else {
    if(bit_shift){
      s[limb_count-1] = (-n) >> (mp_bits_per_limb - bit_shift);
    }
    s[limb_count-2] = (-n) << bit_shift;
  }
  return normalize_bignum(limb_count, (n>=0)?(0):(1<<bignum_sign_shift), r);
}


ikptr
ikrt_bignum_shift_left(ikptr x, ikptr y, ikpcb* pcb){
  int limb_shift = (wordsize == 4 ? 5 : 6);
  long int m = unfix(y);
  ikptr fst = ref(x, -vector_tag);
  long int n = bnfst_limb_count(fst);
  long int whole_limb_shift = m >> limb_shift;
  long int bit_shift = m & (mp_bits_per_limb-1);
  if(bit_shift == 0){
    long int limb_count = n + whole_limb_shift;
    pcb->root0 = &x;
    ikptr r = ik_safe_alloc(pcb, IK_ALIGN(disp_bignum_data + limb_count * wordsize));
    pcb->root0 = 0;
    unsigned int* s = (unsigned int*)(long)(r+disp_bignum_data);
    bzero(s, whole_limb_shift*wordsize);
    memcpy(((char*)s) + whole_limb_shift*wordsize,
           ((char*)x) + off_bignum_data,
           n*wordsize);
    return normalize_bignum(limb_count, bnfst_negative(fst), r);
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
    return normalize_bignum(limb_count, bnfst_negative(fst), r);
  }
}


#if 0
From TFM:
void
mpn_tdiv_qr (
  mp limb t *qp,        /* quotient placed here */
  mp limb t *rp,        /* remainder placed here */
  mp size t qxn,        /* must be zero! */
  const mp limb t *np,  /* first number  */
  mp size t nn,         /* its length    */
  const mp limb t *dp,  /* second number */
  mp size t dn          /* its length    */
)

Divide {np, nn} by {dp, dn} and put the quotient at {qp,nn-dn+1}
and the remainder at {rp, dn}. The quotient is rounded towards 0.
No overlap is permitted between arguments. nn must be greater than
or equal to dn. The most significant limb of dp must be non-zero.
The qxn operand must be zero.
#endif

ikptr
ikrt_bnbndivrem(ikptr x, ikptr y, ikpcb* pcb){
  ikptr xfst = ref(x, -vector_tag);
  ikptr yfst = ref(y, -vector_tag);
  mp_size_t xn = bnfst_limb_count(xfst);
  mp_size_t yn = bnfst_limb_count(yfst);
  if(xn < yn){
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

  if(bnfst_negative(xfst)){
    /* x is negative => remainder is negative */
    r = normalize_bignum(rn, 1 << bignum_sign_shift, r);
  } else {
    r = normalize_bignum(rn, 0, r);
  }

  if(bnfst_negative(yfst)){
    /* y is negative => quotient is opposite of x */
    long int sign = bignum_sign_mask - bnfst_negative(xfst);
    q = normalize_bignum(qn, sign, q);
  } else {
    /* y is positive => quotient is same as x */
    long int sign = bnfst_negative(xfst);
    q = normalize_bignum(qn, sign, q);
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
ikrt_bnfxdivrem(ikptr x, ikptr y, ikpcb* pcb){
  long int yint = unfix(y);
  ikptr fst = ref(x, -vector_tag);
  mp_size_t s2n = bnfst_limb_count(fst);
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

  if(yint < 0){
    /* y is negative => quotient is opposite of x */
    long int sign = bignum_sign_mask - bnfst_negative(fst);
    quot = normalize_bignum(s2n, sign, quot);
  } else {
    /* y is positive => quotient is same as x */
    long int sign = bnfst_negative(fst);
    quot = normalize_bignum(s2n, sign, quot);
  }

  /* the remainder is always less than |y|, so it will
     always be a fixnum.  (if y == most_negative_fixnum,
     then |remainder| will be at most most_positive_fixnum). */
  if(bnfst_negative(fst)){
    /* x is negative => remainder is negative */
    rem = (ikptr) -(rv << fx_shift);
  } else {
    rem = fix(rv);
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
ikrt_bnfx_modulo(ikptr x, ikptr y /*, ikpcb* pcb */){
  long int yint = unfix(y);
  mp_limb_t* s2p = (mp_limb_t*)(long)(x+off_bignum_data);
  ikptr fst = ref(x, -vector_tag);
  mp_size_t s2n = bnfst_limb_count(fst);
  if(yint < 0){
    if(bnfst_negative(fst)){
      /* x negative, y negative */
      mp_limb_t m = mpn_mod_1(s2p, s2n, -yint);
      return fix(-m);
    } else {
      /* x positive, y negative */
      mp_limb_t m = mpn_mod_1(s2p, s2n, -yint);
      return fix(yint+m);
    }
  } else {
    if(bnfst_negative(fst)){
      /* x negative, y positive */
      mp_limb_t m = mpn_mod_1(s2p, s2n, yint);
      return fix(yint-m);
    } else {
      /* x positive, y positive */
      mp_limb_t m = mpn_mod_1(s2p, s2n, yint);
      return fix(m);
    }
  }
}


static int
limb_length(unsigned long int n){
  int i=0;
  while(n != 0){
    n = n >> 1;
    i++;
  }
  return i;
}


ikptr
ikrt_bignum_length(ikptr x){
  ikptr fst = ref(x, -vector_tag);
  mp_limb_t* sp = (mp_limb_t*)(long)(x+off_bignum_data);
  mp_size_t sn = bnfst_limb_count(fst);
  mp_limb_t last = sp[sn-1];
  int n0 = limb_length(last);
  if(((unsigned long int) fst) & bignum_sign_mask){
    /* negative */
    if (last == (mp_limb_t)(1L<<(n0-1))){
      /* single bit set in last limb */
      int i;
      for(i=0; i<(sn-1); i++){
        if(sp[i] != 0){
          /* another bit set */
          return fix((sn-1)*mp_bits_per_limb + n0);
        }
      }
      /* number is - #b100000000000000000000000000 */
      /* fxnot(n) =  #b011111111111111111111111111 */
      /* so, subtract 1. */
      return fix((sn-1)*mp_bits_per_limb + n0 - 1);
    } else {
      return fix((sn-1)*mp_bits_per_limb + n0);
    }
  } else {
    return fix((sn-1)*mp_bits_per_limb + n0);
  }
}


ikptr
ikrt_bignum_to_bytevector(ikptr x, ikpcb* pcb){
  /* FIXME: avoid calling malloc, instead, use the heap pointer itself
   * as a buffer to hold the temporary data after ensuring that it has enough
   * space */
  ikptr fst = ref(x, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  if(limb_count <= 0){
    fprintf(stderr, "BUG: nbtostring: invalid length %ld\n", limb_count);
    exit(EXIT_FAILURE);
  }
  long int sign_bit = bignum_sign_mask & (long int) fst;
  long int nbsize = limb_count * sizeof(mp_limb_t);
  long int strsize = limb_count * max_digits_per_limb;
  long int mem_req = nbsize + strsize + 1;
  unsigned char* mem = malloc(mem_req);
  if(! mem){
    fprintf(stderr, "Error allocating space for bignum\n");
    exit(EXIT_FAILURE);
  }
  memcpy((char*)(long)mem,
         (char*)(long)x - vector_tag + disp_bignum_data,
         nbsize);
  mp_size_t bytes =
    mpn_get_str(mem+nbsize,       /* output string */
                10,               /* base */
                (mp_limb_t*) mem, /* limb */
                limb_count        /* number of limbs */
        );
  unsigned char* string_start = mem + nbsize;
  while(*string_start == 0){
    string_start++;
    bytes--;
  }
  ikptr bv = ik_safe_alloc(pcb, IK_ALIGN(bytes + disp_bytevector_data + (sign_bit?1:0)));
  ref(bv, 0) = fix(bytes + (sign_bit?1:0));
  char* dest = (char*)(long)(bv + disp_bytevector_data);
  if(sign_bit){
    *dest = '-';
    dest++;
  }
  {
    long int i = 0;
    while(i < bytes){
      dest[i] = string_start[i] + '0';
      i++;
    }
    dest[bytes] = 0;
  }
  free(mem);
  return bv | bytevector_tag;
}


ikptr
ikrt_fxrandom(ikptr x){
  long int mask = 1;
  long int n = unfix(x);
  {
    while(mask < n){
      mask = (mask << 1) | 1;
    }
  }
  while(1){
    long r = random() & mask;
    if(r < n){
      return fix(r);
    }
  }
}

static int
limb_size(mp_limb_t x){
  int i = 0;
  while(x){
    i++;
    x = x>>1;
  }
  return i;
}

static int
all_zeros(mp_limb_t* start, mp_limb_t* end){
  while(start <= end){
    if(*end) return 0;
    end--;
  }
  return 1;
}

#define PRECISION 53

static ikptr
ikrt_bignum_to_flonum64(ikptr bn, ikptr more_bits, ikptr fl){
  ikptr fst = ref(bn, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  mp_limb_t* sp = (mp_limb_t*)(long)(bn+off_bignum_data);
  double pos_result;
  if(limb_count == 1){
    pos_result = sp[0];
  } else {
    mp_limb_t hi = sp[limb_count-1];
    int bc = limb_size(hi);
    if(bc < 64){
      mp_limb_t mi = sp[limb_count-2];
      hi = (hi << (64-bc)) | (mi >> bc);
    }
    /* now hi has 64 full bits */
    mp_limb_t mask = ((1L<<(64-PRECISION)) - 1);
    if((hi & mask) == ((mask+1)>>1)){
      /* exactly at break point */
      if(((sp[limb_count-2] << (64-bc)) == 0) &&
          all_zeros(sp, sp+limb_count-3) &&
          (more_bits == 0)){
        if(hi & (1L<<(64-PRECISION))){
          /* odd number, round to even */
          hi = hi | mask;
        }
      } else {
        /* round up */
        hi = hi | mask;
      }
    } else if ((hi & mask) > ((mask+1)>>1)){
      /* also round up */
      hi = hi | mask;
    } else {
      /* keep it to round down */
    }
    pos_result = hi;
    int bignum_bits = bc + (mp_bits_per_limb * (limb_count-1));
    int exponent = bignum_bits - mp_bits_per_limb;
    while(exponent){
      pos_result *= 2.0;
      exponent -= 1;
    }
  }
  if(bnfst_negative(fst)){
    IK_FLONUM_DATA(fl)  = - pos_result;
  } else {
    IK_FLONUM_DATA(fl) = pos_result;
  }
  return fl;
}

ikptr
ikrt_bignum_to_flonum(ikptr bn, ikptr more_bits, ikptr fl){
  if(mp_bits_per_limb == 64){
    return ikrt_bignum_to_flonum64(bn, more_bits, fl);
  }
  ikptr fst = ref(bn, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  mp_limb_t* sp = (mp_limb_t*)(long)(bn+off_bignum_data);
  double pos_result;
  if(limb_count == 1){
    pos_result = sp[0];
  } else if (limb_count == 2){
    mp_limb_t lo = sp[0];
    mp_limb_t hi = sp[1];
    pos_result = hi;
    pos_result = pos_result * 4294967296.0;
    pos_result = pos_result + lo;
  } else {
    mp_limb_t hi = sp[limb_count-1];
    mp_limb_t mi = sp[limb_count-2];
    int bc = limb_size(hi);
    if(bc < 32){
      mp_limb_t lo = sp[limb_count-3];
      hi = (hi << (32-bc)) | (mi >> bc);
      mi = (mi << (32-bc)) | (lo >> bc);
    }
    /* now hi has 32 full bits, and mi has 32 full bits */
    mp_limb_t mask = ((1<<(64-PRECISION)) - 1);
    if((mi & mask) == ((mask+1)>>1)){
      /* exactly at break point */
      if(((sp[limb_count-3] << (32-bc)) == 0) &&
          all_zeros(sp, sp+limb_count-4) &&
          (more_bits == 0)){
        if(mi & (1<<(64-PRECISION))){
          /* odd number, round to even */
          mi = mi | mask;
        }
      } else {
        /* round up */
        mi = mi | mask;
      }
    } else if ((mi & mask) > ((mask+1)>>1)){
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
    while(exponent){
      pos_result *= 2.0;
      exponent -= 1;
    }
  }
  if(bnfst_negative(fst)){
    IK_FLONUM_DATA(fl)  = - pos_result;
  } else {
    IK_FLONUM_DATA(fl) = pos_result;
  }
  return fl;
}

ikptr
ikrt_exact_fixnum_sqrt(ikptr fx /*, ikpcb* pcb*/){
  mp_limb_t x = unfix(fx);
  mp_limb_t s;
  mp_limb_t r;
  mpn_sqrtrem(&s, &r, &x, 1);
  return fix(s);
}

ikptr
ikrt_exact_bignum_sqrt(ikptr bn, ikpcb* pcb){
  ikptr fst = ref(bn, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  long int result_limb_count = (limb_count + 1)/2;
  pcb->root0 = &bn;
  ikptr s = ik_safe_alloc(pcb,
            IK_ALIGN(disp_bignum_data+result_limb_count*wordsize))
          | vector_tag;
  ref(s, -vector_tag) =
    (ikptr) (bignum_tag | (result_limb_count << bignum_length_shift));
  pcb->root1 = &s;
  ikptr r = ik_safe_alloc(pcb,
              IK_ALIGN(disp_bignum_data+limb_count*wordsize))
          | vector_tag;
  ref(r, -vector_tag) =
    (ikptr) (bignum_tag | (limb_count << bignum_length_shift));
  pcb->root0 = &r;
  ikptr pair = ik_safe_alloc(pcb, pair_size) | pair_tag;
  pcb->root0 = 0;
  pcb->root1 = 0;
  mp_size_t r_actual_limbs = mpn_sqrtrem(
      (mp_limb_t*) (s+off_bignum_data),
      (mp_limb_t*) (r+off_bignum_data),
      (mp_limb_t*) (bn+off_bignum_data),
      limb_count);
  ref(pair, off_car) = normalize_bignum(result_limb_count, 0, s-vector_tag);
  if(r_actual_limbs == 0) {
    /* perfect square */
    ref(pair, off_cdr) = 0;
  } else {
    ref(pair, off_cdr) = normalize_bignum(r_actual_limbs, 0, r-vector_tag);
  }
  return pair;
}


ikptr
ikrt_flonum_hash(ikptr x /*, ikpcb* pcb */) {
  short* buf = (short*)(x+off_flonum_data);
  return fix(((long)buf[0]) ^
             ((long)buf[1] << 3) ^
             ((long)buf[3] << 7) ^
             ((long)buf[2] << 11));
}
ikptr
ikrt_bignum_hash(ikptr bn /*, ikpcb* pcb */) {
  ikptr fst = ref(bn, -vector_tag);
  long int limb_count = bnfst_limb_count(fst);
  long h = (long)fst;
  mp_limb_t* dat = (mp_limb_t*)(bn+off_bignum_data);
  long i;
  for (i=0; i<limb_count; i++){
    h = (h^dat[i]) << 3;
  }
  return fix(h);
}

/* end of file */
