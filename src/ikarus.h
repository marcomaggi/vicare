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

#ifndef IKARUS_H
#  define IKARUS_H


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <limits.h>
#include <errno.h>
#include "ikarus-data.h"


/** --------------------------------------------------------------------
 ** Helper macros.
 ** ----------------------------------------------------------------- */

/* The macro VICARE_UNUSED indicates  that a function, function argument
   or variable may potentially be unused.  Usage examples:

   static int unused_function (char arg) VICARE_UNUSED;
   int foo (char unused_argument VICARE_UNUSED);
   int unused_variable VICARE_UNUSED;
*/
#ifdef __GNUC__
#  define VICARE_UNUSED		__attribute__((unused))
#else
#  define VICARE_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif


/** --------------------------------------------------------------------
 ** Utility macros.
 ** ----------------------------------------------------------------- */

#define VICARE_BYTEVECTOR_LENGTH_FX(BV)                 \
  ref((BV), off_bytevector_length)

#define VICARE_BYTEVECTOR_DATA_CHARP(BV)                \
  ((char*)(long)((BV) + off_bytevector_data))

#define VICARE_BYTEVECTOR_DATA_UINT8P(BV)               \
  ((uint8_t*)(long)((BV) + off_bytevector_data))

#define VICARE_BYTEVECTOR_DATA_VOIDP(BV)                \
  ((void*)(long)((BV) + off_bytevector_data))


/** --------------------------------------------------------------------
 ** Prototypes and external definitions.
 ** ----------------------------------------------------------------- */

extern char **environ;

#ifdef __CYGWIN__
void    win_munmap(char* addr, size_t size);
char*   win_mmap(size_t size);
#endif

int     ikarus_main (int argc, char** argv, char* boot_file);

ikptr   ik_errno_to_code (void);

/* object utilities */
int     ik_list_length (ikptr x);
void    ik_list_to_argv (ikptr x, char **argv);
char**  ik_list_to_vec (ikptr x);

ikptr   ik_bytevector_alloc (ikpcb * pcb, long int requested_number_of_bytes);



/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* ifndef IKARUS_H */

/* end of file */
