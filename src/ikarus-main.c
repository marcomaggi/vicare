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

#include "bootfileloc.h"
#include "internals.h"
#include <fcntl.h>
#include <gmp.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

extern int cpu_has_sse2();
static void register_handlers();
static void register_alt_stack();

ikpcb* the_pcb;

ikpcb *
ik_the_pcb (void)
{
  return the_pcb;
}


int
ikarus_main (int argc, char** argv, char* boot_file)
/* Setup  global variables  and handlers,  then load  the boot  file and
   evaluate it.  This  function is meant to be  called from "main()" and
   its return value becomes the return value of "main()".

   "argc" and "argv" must reference arguments on the command line of the
   "vicare" executable,  with the command  name and the  option "--boot"
   removed.

   "boot_file" must  be a string  representing the filename of  the boot
   file to use. */
{
  ikpcb *	pcb;
  int		repl_on_sigint	= 0;
  if (! cpu_has_sse2()) {
    fprintf(stderr, "Vicare Scheme cannot run on your computer because\n");
    fprintf(stderr, "your CPU does not support the SSE2 instruction set.\n");
    fprintf(stderr, "Refer to the Vicare Scheme User's Guide for the\n");
    fprintf(stderr, "minimum hardware requirements.\n");
    exit(EXIT_FAILURE);
  }
  if (sizeof(mp_limb_t) != sizeof(long int))
    ik_abort("limb size does not match");
  if (mp_bits_per_limb != (8*sizeof(long int)))
    ik_abort("invalid bits_per_limb=%d\n", mp_bits_per_limb);
  the_pcb = pcb = ik_make_pcb();
  { /* Set up arg_list from the  last "argv" to the first; the resulting
       list will end in COMMAND-LINE. */

    ikptr	arg_list	= IK_NULL_OBJECT;
    int		i		= argc-1;
    for (; i > 0; --i) {
      if (0 == strcmp(argv[i], "--repl-on-sigint")) {
	repl_on_sigint = 1;
      } else {
	char *	s = argv[i];
	int	n = strlen(s);
	ikptr	bv = ik_unsafe_alloc(pcb, IK_ALIGN(disp_bytevector_data+n+1)) | bytevector_tag;
	IK_REF(bv, off_bytevector_length) = IK_FIX(n);
	/* copy the bytes and the terminating zero */
	memcpy((char*)(bv+off_bytevector_data), s, n+1);
	ikptr p = ik_unsafe_alloc(pcb, pair_size);
	ref(p, disp_car) = bv;
	ref(p, disp_cdr) = arg_list;
	arg_list = p+pair_tag;
      }
    }
    pcb->argv0    = argv[0];
    pcb->arg_list = arg_list;
  }
  register_handlers(repl_on_sigint);
  register_alt_stack();
  ik_fasl_load(pcb, boot_file);
  ik_delete_pcb(pcb);
  return 0;
}


/** --------------------------------------------------------------------
 ** Special functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_get_argv0_bytevector (ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, pcb->argv0);
}
ikptr
ikrt_get_argv0_string (ikpcb * pcb)
{
  return ika_string_from_cstring(pcb, pcb->argv0);
}
ikptr
ikrt_get_last_revision (ikpcb * pcb)
{
#include "last-revision.h"
  return ika_bytevector_from_cstring(pcb, LAST_REVISION);
}


/* Notice how the BSD manpages have incorrect type for the handler.

     #include <signal.h>

     struct  sigaction {
             union {
                     void    (*__sa_handler)(int);
                     void    (*__sa_sigaction)(int, struct __siginfo *, void *);
             } __sigaction_u;
             int     sa_flags;
             sigset_t sa_mask;
     };
     #define sa_handler      __sigaction_u.__sa_handler
     #define sa_sigaction    __sigaction_u.__sa_sigaction
     int
     sigaction(int sig, const struct sigaction * restrict act, struct sigaction * restrict oact);
*/

static void
handler (int signo IK_UNUSED, siginfo_t* info IK_UNUSED, void* uap)
{
  ikpcb *	pcb = ik_the_pcb();
  /* avoid compiler warnings on unused arguments */
  /* signo=signo; info=info; uap=uap; */
  pcb->engine_counter = IK_FIX(-1);
  pcb->interrupted    = 1;
}
static void
register_handlers (int repl_on_sigint)
{
  if (repl_on_sigint) {
    /* Enter REPL on SIGINT. */
    struct sigaction	sa;
    int			rv;
    sa.sa_sigaction = handler;
#ifdef __CYGWIN__
    sa.sa_flags = SA_SIGINFO;
#else
    sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
#endif
    sigemptyset(&sa.sa_mask);
    rv = sigaction(SIGINT, &sa, 0);
    if (rv)
      ik_abort("sigaction failed: %s", strerror(errno));
  }
  /* ignore sigpipes */
  {
#if 1
    struct sigaction	sa;
    int			rv;
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    rv = sigaction(SIGPIPE, &sa, NULL);
    if (rv)
      ik_abort("sigaction failed while trying to ignore SIGPIPE: %s", strerror(errno));
#else
    sigset_t	set;
    int		rv;
    sigprocmask(0, 0, &set); /* get the set */
    sigaddset(&set, SIGPIPE);
    rv = sigprocmask(SIG_SETMASK, &set, NULL);
    if (rv)
      ik_abort("sigprocmask failed: %s", strerror(errno));
#endif
  }
}

/*
SYNOPSIS
     #include <sys/types.h>
     #include <signal.h>

     struct sigaltstack {
             char   *ss_sp;
             int     ss_size;
             int     ss_flags;
     };

     int
     sigaltstack(const struct sigaltstack *ss, struct sigaltstack *oss);
*/

static void
register_alt_stack (void)
{
#if HAVE_SIGALTSTACK
  char* stk = mmap(0, SIGSTKSZ, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
  //  char* stk = ik_mmap(SIGSTKSZ);
  if (stk == (char*)-1)
    ik_abort("cannot allocate an alternate stack for interprocess signals");
  stack_t sa;
  sa.ss_sp = stk;
  sa.ss_size = SIGSTKSZ;
  sa.ss_flags = 0;
  int err = sigaltstack(&sa, 0);
  if (err)
    ik_abort("cannot set alternate stack for interprocess signals: %s\n", strerror(errno));
#endif
}

/* end of file */
