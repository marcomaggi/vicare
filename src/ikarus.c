/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 *  Modified by Marco Maggi
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



#include "ikarus-main.h"
#include "bootfileloc.h"
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void ikarus_usage_short(){
  fprintf(stderr, "vicare -h for more help\n");
}

void ikarus_usage(){
  static char* helpstring =
"\n\
Options for running Vicare Scheme:\n\
\n  vicare -h\n\
    Prints this help message then exits.\n\
\n  vicare [-b <bootfile>] --r6rs-script <scriptfile> opts ...\n\
    Starts Vicare in R6RS-script mode.  The script file is treated\n\
    as an R6RS-script.  The options opts ... can be obtained using\n\
    the \"command-line\" procedure in the (rnrs programs) library.\n\
\n  vicare [-b <bootfile>] <file> ... [-- opts ...]\n\
    Starts Vicare in interactive mode.  Each of the files is first\n\
    loaded into the interaction environment before the interactive\n\
    repl is started.  The options opts can be obtained using the\n\
    \"command-line\" procedure.\n\
  \n\
  If the option [-b <bootfile>] is provided, the bootfile is used\n\
  as the system's initial boot file from which the environment is\n\
  initialized.  If the -b option is not supplied, the default boot\n\
  file is used.  The current default boot file location is\n\
  \"%s\".\n\
  Consult the Vicare Scheme User's Guide for more details.\n\n";
  fprintf(stderr, helpstring, BOOTFILE);
}

int main(int argc, char** argv){
  char* boot_file = BOOTFILE;
  if (((argc >= 2) && (strcmp(argv[1], "-h") == 0)) ||
      ((argc == 2) && (strcmp(argv[1], "-b") == 0))) {
    ikarus_usage();
    exit(0);
  }

  if ((argc >= 3) && (strcmp(argv[1], "-b") == 0)){
    boot_file = argv[2];
    int i;
    for(i=3; i<=argc; i++){
      argv[i-2] = argv[i];
    }
    argc -= 2;
  }
  return ikarus_main(argc, argv, boot_file);
}


