/*
  Part of: Vicare
  Contents: internal header file
  Date: Wed Jan 11, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006-2008  Abdulaziz Ghuloum

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

#ifndef INTERNALS_H
#define INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "vicare.h"
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <netdb.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/resource.h>


/** --------------------------------------------------------------------
 ** Constants.
 ** ----------------------------------------------------------------- */

#define IK_FORWARD_PTR		((ikptr)-1)
#define IK_MOST_BYTES_IN_MINOR	0x10000000

#define old_gen_mask		0x00000007
#define new_gen_mask		0x00000008
#define gen_mask		0x0000000F
#define new_gen_tag		0x00000008
#define meta_dirty_mask		0x000000F0
#define type_mask		0x00000F00
#define scannable_mask		0x0000F000
#define dealloc_mask		0x000F0000
#define large_object_mask	0x00100000
#define meta_dirty_shift	4

#define hole_type		0x00000000
#define mainheap_type		0x00000100
#define mainstack_type		0x00000200
#define pointers_type		0x00000300
#define dat_type		0x00000400
#define code_type		0x00000500
#define weak_pairs_type		0x00000600
#define symbols_type		0x00000700

#define scannable_tag		0x00001000
#define unscannable_tag		0x00000000

#define dealloc_tag_un		0x00010000
#define dealloc_tag_at		0x00020000
#define retain_tag		0x00000000

#define large_object_tag	0x00100000

#define hole_mt         (hole_type       | unscannable_tag | retain_tag)
#define mainheap_mt     (mainheap_type   | unscannable_tag | retain_tag)
#define mainstack_mt    (mainstack_type  | unscannable_tag | retain_tag)
#define pointers_mt     (pointers_type   | scannable_tag   | dealloc_tag_un)
#define symbols_mt      (symbols_type    | scannable_tag   | dealloc_tag_un)
#define data_mt         (dat_type        | unscannable_tag | dealloc_tag_un)
#define code_mt         (code_type       | scannable_tag   | dealloc_tag_un)
#define weak_pairs_mt   (weak_pairs_type | scannable_tag   | dealloc_tag_un)

#define call_instruction_size	((wordsize == 4) ? 5 : 10)
#define disp_frame_size		(- (call_instruction_size + 3 * wordsize))
#define disp_frame_offset	(- (call_instruction_size + 2 * wordsize))
#define disp_multivale_rp	(- (call_instruction_size + 1 * wordsize))


/** --------------------------------------------------------------------
 ** Type definitions.
 ** ----------------------------------------------------------------- */



/** --------------------------------------------------------------------
 ** Helper and legacy macros.
 ** ----------------------------------------------------------------- */

#define ref(X,N)        IK_REF((X),(N))

#define fix(X)          IK_FIX(X)
#define unfix(X)        IK_UNFIX(X)


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

ikpcb * ik_collect              (unsigned long, ikpcb*);

void*   ik_malloc               (int);
void    ik_free                 (void*, int);

ikptr   ik_underflow_handler    (ikpcb*);

ikptr   ik_mmap                 (unsigned long);
ikptr   ik_mmap_typed           (unsigned long size, unsigned type, ikpcb*);
ikptr   ik_mmap_ptr             (unsigned long size, int gen, ikpcb*);
ikptr   ik_mmap_data            (unsigned long size, int gen, ikpcb*);
ikptr   ik_mmap_code            (unsigned long size, int gen, ikpcb*);
ikptr   ik_mmap_mixed           (unsigned long size, ikpcb*);
void    ik_munmap               (ikptr, unsigned long);
void    ik_munmap_from_segment  (ikptr, unsigned long, ikpcb*);
ikpcb * ik_make_pcb             (void);
void    ik_delete_pcb           (ikpcb*);
void    ik_free_symbol_table    (ikpcb* pcb);

void    ik_fasl_load            (ikpcb* pcb, char* filename);
void    ik_relocate_code        (ikptr);

ikptr   ik_exec_code            (ikpcb* pcb, ikptr code_ptr, ikptr argcount, ikptr cp);

ikptr   ik_asm_enter            (ikpcb*, ikptr code_object, ikptr arg, ikptr cp);
ikptr   ik_asm_reenter          (ikpcb*, ikptr code_object, ikptr val);


/** --------------------------------------------------------------------
 ** Objects stuff.
 ** ----------------------------------------------------------------- */

ikptr   normalize_bignum        (long limbs, int sign, ikptr r);
#define bnfst_limb_count(X)     (((unsigned long)(X)) >> bignum_length_shift)
#define bnfst_negative(X)       (((unsigned long)(X)) & bignum_sign_mask)

#define max_digits_per_limb	((wordsize==4)?10:20)


/** --------------------------------------------------------------------
 ** Other prototypes and external definitions.
 ** ----------------------------------------------------------------- */

extern char **		environ;
extern ikpcb *		the_pcb;

#ifdef __CYGWIN__
void    win_munmap(char* addr, size_t size);
char*   win_mmap(size_t size);
#endif

int     ikarus_main (int argc, char** argv, char* boot_file);

ikptr   ik_errno_to_code (void);


/** --------------------------------------------------------------------
 ** Interface to "getaddrinfo()".
 ** ----------------------------------------------------------------- */

#if (!HAVE_GETADDRINFO)
#  include <sys/types.h>
#  include <netdb.h>

struct addrinfo {
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t ai_addrlen;
  struct sockaddr *ai_addr;
  struct addrinfo *ai_next;
};

extern int
getaddrinfo(const char *hostname, const char* servname,
  const struct addrinfo* hints, struct addrinfo** res);

extern void
freeaddrinfo(struct addrinfo *ai);


#ifndef EAI_SYSTEM
# define EAI_SYSTEM 11 /* same code as in glibc */
#endif

#endif /* if (!HAVE_GETADDRINFO) */


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* INTERNALS_H */

/* end of file */
