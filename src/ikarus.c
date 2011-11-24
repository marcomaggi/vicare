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

#include "ikarus.h"
#include "bootfileloc.h"
#include <locale.h>


int
main (int argc, char** argv)
{
  char *        boot_file = NULL;
  int           i, j;
  /* Filter out the "-b" and  "--boot" options because we need them here
     to load  the boot file.   Shift the other arguments  accordingly in
     "argv". */
  for (i=1, j=1; i<argc; ++i) {
    if ((0 == strcmp(argv[i], "-b")) ||
        (0 == strcmp(argv[i], "--boot"))) {
      if (i+1 < argc) {
        if (boot_file) {
          fprintf(stderr, "*** %s error: option -b or --boot used multiple times\n", argv[0]);
          exit(2);
        } else {
          boot_file = argv[++i];
        }
      } else {
        fprintf(stderr, "*** %s error: option %s needs the boot filename as argument\n",
                argv[0], argv[i]);
        exit(2);
      }
    } else {
      argv[j] = argv[i];
      ++j;
    }
  }
  setlocale(LC_ALL, "");
  if (NULL == boot_file)
    boot_file = BOOTFILE;
  return ikarus_main(j, argv, boot_file);
}

/* end of file */
