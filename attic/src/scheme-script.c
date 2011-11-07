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



#include "ikarus-main.h"
#include "bootfileloc.h"
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void ikarus_usage_short(){
  fprintf(stderr, "scheme-script <script-name> arguments ...\n");
}

void ikarus_usage(){
  static char* helpstring =
  "Usage: \n\
  scheme-script <script-name> arguments ...\n\
  \n\
  Runs the file <script-name> as a Scheme script, passing\n\
  arguments ... as (command-line)\n\
  \n\
  Consult the Ikarus Scheme User's Guide for more details.\n\n";
  fprintf(stderr, "%s", helpstring);
}

int main(int argc, char** argv){
  if(argc < 2) {
    ikarus_usage();
    exit(EXIT_FAILURE);
  }
  char* boot_file = BOOTFILE;
  char** args = calloc(sizeof(char*), argc+1);
  args[0] = argv[0];
  args[1] = "--r6rs-script";
  int i;
  for(i=1; i<argc; i++){
    args[i+1] = argv[i];
  }
  return ikarus_main(argc+1, args, boot_file);
}


