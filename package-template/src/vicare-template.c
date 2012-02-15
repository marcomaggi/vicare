/*
  Part of: Vicare/Template
  Contents: Template for Vicare
  Date: Sat Jan 21, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
// #include <template.h>


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_template_version_interface_current (void)
{
  return IK_FIX(vicare_template_VERSION_INTERFACE_CURRENT);
}
ikptr
ikrt_template_version_interface_revision (void)
{
  return IK_FIX(vicare_template_VERSION_INTERFACE_REVISION);
}
ikptr
ikrt_template_version_interface_age (void)
{
  return IK_FIX(vicare_template_VERSION_INTERFACE_AGE);
}
ikptr
ikrt_template_version (ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, vicare_template_VERSION_INTERFACE_STRING);
}

/* end of file */
