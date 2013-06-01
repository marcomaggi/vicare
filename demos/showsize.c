/* showsize.c --

   Part of: Vicare Scheme
   Contents: prints interesting sizes on stdout
   Date: Sat Jun  1, 2013

   Abstract

	Compile and run with:

	   $ gcc -Wall -o showsize showsize.c && ./showsize

   Copyright (c) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

   This is free software you can  redistribute it and/or modify it under
   the terms of the GNU General  Public License as published by the Free
   Software Foundation; either version 2,  or (at your option) any later
   version.

   This file  is distributed  in the  hope that it  will be  useful, but
   WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
   MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
   General Public License for more details.

   You should  have received a  copy of  the GNU General  Public License
   along with  this file; see  the file COPYING.   If not, write  to the
   Free Software Foundation, Inc., 59  Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.
*/


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <limits.h>

typedef unsigned long		ikptr;
#define wordsize		((int)(sizeof(ikptr)))
#define wordshift		((4 == wordsize)? 2 : 3)

#define fx_shift		wordshift
#define fx_mask			(wordsize - 1)

#define fixnum_bits_number_32	((8 * 4) - 2)
#define fixnum_bits_number_64	((8 * 8) - 3)
#define fixnum_bits_number	((8 * wordsize) - fx_shift)

#define most_positive_fixnum	(((unsigned long)-1) >> (fx_shift+1))
#define most_negative_fixnum	(most_positive_fixnum+1)
#define IK_GREATEST_FIXNUM	most_positive_fixnum
#define IK_LEAST_FIXNUM		(-most_negative_fixnum)

/* ------------------------------------------------------------------ */

#define wordsize_32		((int)4)
#define wordshift_32		2
#define fx_shift_32		wordshift_32
#define fx_mask_32		(wordsize_32 - 1)

/* this should be: +536870911 */
#define IK_GREATEST_FIXNUM_32	(((uint32_t)-1) >> (fx_shift_32 + 1))
/* this should be: -536870912 */
#define IK_LEAST_FIXNUM_32	(- IK_GREATEST_FIXNUM_32 - 1)

/* ------------------------------------------------------------------ */

#define wordsize_64		((int)8)
#define wordshift_64		3
#define fx_shift_64		wordshift_64
#define fx_mask_64		(wordsize_64 - 1)

/* this should be: +1152921504606846975 */
#define IK_GREATEST_FIXNUM_64	(((uint64_t)-1) >> (fx_shift_64 + 1))
/* this should be: -1152921504606846976 */
#define IK_LEAST_FIXNUM_64	(- IK_GREATEST_FIXNUM_64 - 1)


int
main (void)
{
  printf("\nSize of common data types (number of bytes):\n\n");

  printf("char\t\t%ld :)\n",	sizeof(char));
  printf("void *\t\t%ld\n",	sizeof(void *));
  printf("int\t\t%ld\n",		sizeof(int));
  printf("short int\t%ld\n",	sizeof(short int));
  printf("long int\t%ld\n",	sizeof(long));
  printf("long long\t%ld\n",	sizeof(long long));
  printf("float\t\t%ld\n",	sizeof(float));
  printf("double\t\t%ld\n",	sizeof(double));
#if 1
  printf("long double\t%ld\n",	sizeof(long double));
#endif
  printf("size_t\t\t%ld\n",	sizeof(size_t));
  printf("ssize_t\t\t%ld\n",	sizeof(ssize_t));
  printf("ptrdiff_t\t%ld\n",	sizeof(ptrdiff_t));
  printf("intptr_t\t%ld\n",	sizeof(intptr_t));	/* this is a signed integer */
  printf("uintptr_t\t%ld\n",	sizeof(uintptr_t));	/* this is an unsigned integer */

  printf("\nLimits:\n\n");
  {
    printf("short int (SHRT_MIN, SHRT_MAX)    %d %d\n",		SHRT_MIN, SHRT_MAX);
    printf("unsigned short int (USHRT_MAX)    %d\n",		USHRT_MAX);
    printf("int (INT_MIN, INT_MAX)            %d %d\n",		INT_MIN, INT_MAX);
    printf("unsigned int (UINT_MAX)           %u\n",		UINT_MAX);
    printf("long (LONG_MIN, LONG_MAX)         %ld %ld\n",	LONG_MIN, LONG_MAX);
    printf("unsigned long (ULONG_MAX)         %lu\n",		ULONG_MAX);
  }

  printf("\nSize of Vicare Scheme data:\n\n");
  {
    printf("wordsize=%d, wordshift=%d\n",	wordsize, wordshift);
    printf("fx_shift=%d, fx_mask=%d\n",		fx_shift, fx_mask);

    printf("fixnum bits number on 32-bit platforms=%d\n", fixnum_bits_number_32);
    printf("fixnum bits number on 64-bit platforms=%d\n", fixnum_bits_number_64);
    printf("fixnum bits number on this platform=%d\n", fixnum_bits_number);

    printf("on this 32-bit platforms: IK_GREATEST_FIXNUM=+%d\n",	(int32_t)IK_GREATEST_FIXNUM_32);
    printf("on this 32-bit platforms:    IK_LEAST_FIXNUM=%d\n",		(int32_t)IK_LEAST_FIXNUM_32);

    printf("on this 64-bit platforms: IK_GREATEST_FIXNUM=+%ld\n",	(int64_t)IK_GREATEST_FIXNUM_64);
    printf("on this 64-bit platforms:    IK_LEAST_FIXNUM=%ld\n",	(int64_t)IK_LEAST_FIXNUM_64);

    printf("on this platform:         IK_GREATEST_FIXNUM=+%ld\n",	(int64_t)IK_GREATEST_FIXNUM);
    printf("on this platform:            IK_LEAST_FIXNUM=%ld\n",	(int64_t)IK_LEAST_FIXNUM);
  }
  exit(EXIT_SUCCESS);
}

/* end of file */
