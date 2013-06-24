/*
  Part of: Vicare Scheme
  Contents: debugging facilities
  Date: Mon Jun 24, 2013

  Abstract



  Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <internals.h>


/** --------------------------------------------------------------------
 ** Dummy functions.
 ** ----------------------------------------------------------------- */

/* These functions  have the purpose  to allow testing of  Assembly code
   calling C functions from Scheme code through FOREIGN-CALL. */

ikptr
ikrt_dummy_arg_0 (ikpcb * pcb)
{
  assert(pcb == ik_the_pcb());
  return IK_TRUE;
}
ikptr
ikrt_dummy_arg_1 (ikptr arg1, ikpcb * pcb)
{
  assert(pcb == ik_the_pcb());
  return IK_FIX(1000 + IK_UNFIX(arg1));
}
ikptr
ikrt_dummy_arg_2 (ikptr arg1, ikptr arg2, ikpcb * pcb)
{
  assert(pcb == ik_the_pcb());
  return IK_FIX(1000 + IK_UNFIX(arg1) + IK_UNFIX(arg2));
}
ikptr
ikrt_dummy_arg_3 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikpcb * pcb)
{
  assert(pcb == ik_the_pcb());
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3));
}
ikptr
ikrt_dummy_arg_4 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikptr arg4, ikpcb * pcb)
{
  assert(pcb == ik_the_pcb());
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3)
		+ IK_UNFIX(arg4));
}
ikptr
ikrt_dummy_arg_5 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikptr arg4, ikptr arg5, ikpcb * pcb)
{
  ik_debug_message("%s: pcb=0x%016lx, the_pcb=0x%016lx", __func__, (long)pcb, (long)ik_the_pcb());
  assert(pcb == ik_the_pcb());
  ik_debug_message("%s: arg1=%ld, arg2=%ld, arg3=%ld, arg4=%ld, arg5=%ld", __func__,
		   IK_UNFIX(arg1), IK_UNFIX(arg2), IK_UNFIX(arg3),
		   IK_UNFIX(arg4), IK_UNFIX(arg5));
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3)
		+ IK_UNFIX(arg4) + IK_UNFIX(arg5));
}
ikptr
ikrt_dummy_arg_6 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikptr arg4, ikptr arg5, ikptr arg6,
		  ikpcb * pcb)
{
  ik_debug_message("%s: pcb=0x%016lx, the_pcb=0x%016lx", __func__, (long)pcb, (long)ik_the_pcb());
  assert(pcb == ik_the_pcb());
  ik_debug_message("%s: arg1=%ld, arg2=%ld, arg3=%ld, arg4=%ld, arg5=%ld, arg6=%ld", __func__,
		   IK_UNFIX(arg1), IK_UNFIX(arg2), IK_UNFIX(arg3),
		   IK_UNFIX(arg4), IK_UNFIX(arg5), IK_UNFIX(arg6));
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3)
		+ IK_UNFIX(arg4) + IK_UNFIX(arg5) + IK_UNFIX(arg6));
}
ikptr
ikrt_dummy_arg_7 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikptr arg4, ikptr arg5, ikptr arg6,
		  ikptr arg7, ikpcb * pcb)
{
  ik_debug_message("%s: pcb=0x%016lx, the_pcb=0x%016lx", __func__, (long)pcb, (long)ik_the_pcb());
  assert(pcb == ik_the_pcb());
  ik_debug_message("%s: arg1=%ld, arg2=%ld, arg3=%ld, arg4=%ld, arg5=%ld, arg6=%ld, arg7=%ld", __func__,
		   IK_UNFIX(arg1), IK_UNFIX(arg2), IK_UNFIX(arg3),
		   IK_UNFIX(arg4), IK_UNFIX(arg5), IK_UNFIX(arg6),
		   IK_UNFIX(arg7));
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3)
		+ IK_UNFIX(arg4) + IK_UNFIX(arg5) + IK_UNFIX(arg6)
		+ IK_UNFIX(arg7));
}
ikptr
ikrt_dummy_arg_8 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikptr arg4, ikptr arg5, ikptr arg6,
		  ikptr arg7, ikptr arg8, ikpcb * pcb)
{
  ik_debug_message("%s: pcb=0x%016lx, the_pcb=0x%016lx", __func__, (long)pcb, (long)ik_the_pcb());
  assert(pcb == ik_the_pcb());
  ik_debug_message("%s: arg1=%ld, arg2=%ld, arg3=%ld, arg4=%ld, arg5=%ld, arg6=%ld, arg7=%ld, arg8=%ld", __func__,
		   IK_UNFIX(arg1), IK_UNFIX(arg2), IK_UNFIX(arg3),
		   IK_UNFIX(arg4), IK_UNFIX(arg5), IK_UNFIX(arg6),
		   IK_UNFIX(arg7), IK_UNFIX(arg8));
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3)
		+ IK_UNFIX(arg4) + IK_UNFIX(arg5) + IK_UNFIX(arg6)
		+ IK_UNFIX(arg7) + IK_UNFIX(arg8));
}
ikptr
ikrt_dummy_arg_9 (ikptr arg1, ikptr arg2, ikptr arg3,
		  ikptr arg4, ikptr arg5, ikptr arg6,
		  ikptr arg7, ikptr arg8, ikptr arg9,
		  ikpcb * pcb)
{
  ik_debug_message("%s: pcb=0x%016lx, the_pcb=0x%016lx", __func__, (long)pcb, (long)ik_the_pcb());
  assert(pcb == ik_the_pcb());
  ik_debug_message("%s: arg1=%ld, arg2=%ld, arg3=%ld, arg4=%ld, arg5=%ld, arg6=%ld, arg7=%ld, arg8=%ld, arg9=%ld", __func__,
		   IK_UNFIX(arg1), IK_UNFIX(arg2), IK_UNFIX(arg3),
		   IK_UNFIX(arg4), IK_UNFIX(arg5), IK_UNFIX(arg6),
		   IK_UNFIX(arg7), IK_UNFIX(arg8), IK_UNFIX(arg9));
  return IK_FIX(1000
		+ IK_UNFIX(arg1) + IK_UNFIX(arg2) + IK_UNFIX(arg3)
		+ IK_UNFIX(arg4) + IK_UNFIX(arg5) + IK_UNFIX(arg6)
		+ IK_UNFIX(arg7) + IK_UNFIX(arg8) + IK_UNFIX(arg9));
}


/* end of file */
