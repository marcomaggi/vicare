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
 ** Prototypes and external definitions.
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
