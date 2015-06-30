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

#include "internals.h"
#include "bootfileloc.h"
#include <locale.h>

extern int		ik_enabled_runtime_messages;
extern int		ik_garbage_collection_is_forbidden;
extern ikuword_t	ik_customisable_heap_nursery_size;
extern ikuword_t	ik_customisable_stack_size;

static ikuword_t	normalise_number_of_bytes_argument (const char * argument_description,
							    int i, int argc, char** argv);


int
main (int argc, char** argv)
{
  char *        boot_file = NULL;
  int		j=1;

  /* Filter  out  the command  line  arguments:
   *
   *    -b, --boot
   *    --scheme-heap-nursery-size
   *    --scheme-stack-size
   *
   * Shift the other arguments accordingly in "argv".
   */
  for (int i=1; i<argc; ++i) {
    if (0 == strcmp(argv[i], "--")) {
      /* End of options marker. */
      for (; i<argc; ++i, ++j) {
	argv[j] = argv[i];
      }
      break;
    }
    else if ((0 == strcmp(argv[i], "-b")) || (0 == strcmp(argv[i], "--boot"))) {
      if (i+1 < argc) {
        if (boot_file) {
          fprintf(stderr, "%s: error: option -b or --boot used multiple times\n", argv[0]);
          exit(2);
        } else {
          boot_file = argv[++i];
        }
      } else {
        fprintf(stderr, "%s: error: option %s needs the boot filename as argument\n",
                argv[0], argv[i]);
        exit(2);
      }
    }
    else if (0 == strcmp(argv[i], "--scheme-heap-nursery-size")) {
      ikuword_t	num_of_bytes = normalise_number_of_bytes_argument("customisable Scheme heap nursery size", i, argc, argv);
      ik_customisable_heap_nursery_size = IK_ALIGN_TO_NEXT_PAGE(num_of_bytes);
      ++i;
    }
    else if (0 == strcmp(argv[i], "--scheme-stack-size")) {
      ikuword_t	num_of_bytes = normalise_number_of_bytes_argument("customisable Scheme stack size", i, argc, argv);
      ik_customisable_stack_size = IK_ALIGN_TO_NEXT_PAGE(num_of_bytes);
      ++i;
    }
    else if (0 == strcmp(argv[i], "--option")) {
      if (1+i < argc) {
	if      (0 == strcmp(argv[1+i], "enable-automatic-gc")) {
	  IK_RUNTIME_MESSAGE("automatic garbage collection is enabled");
	  ik_garbage_collection_is_forbidden = 0;
	  ++i;
	}
	else if (0 == strcmp(argv[1+i], "disable-automatic-gc")) {
	  IK_RUNTIME_MESSAGE("automatic garbage collection is disabled");
	  ik_garbage_collection_is_forbidden = 1;
	  ++i;
	}
	else if (0 == strcmp(argv[1+i], "enable-runtime-messages")) {
	  ik_enabled_runtime_messages = 1;
	  ++i;
	}
	else if (0 == strcmp(argv[1+i], "disable-runtime-messages")) {
	  ik_enabled_runtime_messages = 0;
	  ++i;
	}
	else {
	  argv[j] = argv[i];
	  ++j;
	}
      } else {
	fprintf(stderr, "%s: error: option %s needs an argument\n", argv[0], argv[i]);
	exit(2);
      }
    }
    else {
      argv[j] = argv[i];
      ++j;
    }
  }
  /* FIXME  This is  commented  out  because, at  present,  there is  no
     support for  locales whose associated  encoding is not  ASCII; such
     missing  support causes  errors, for  example, when  handling error
     messages  from "dlerror()".   This  must be  fixed  in the  future.
     (Marco Maggi; Sat Mar 22, 2014) */
  if (0) {
    setlocale(LC_ALL, "");
  }
  if (NULL == boot_file)
    boot_file = BOOTFILE;
  return ikarus_main(j, argv, boot_file);
}


static ikuword_t
normalise_number_of_bytes_argument (const char * argument_description,
				    int i, int argc, char** argv)
{
  if (1+i < argc) {
    ik_long	num_of_bytes;
    ikuword_t	normalised_num_of_bytes;
    char *	tail_ptr;
    errno = 0;
    num_of_bytes = strtol(argv[1+i], &tail_ptr, 10);
    IK_RUNTIME_MESSAGE("argument to command line option for %s: %ld bytes",
			 argument_description, num_of_bytes);
    if (errno) {
      fprintf(stderr, "%s: error: invalid argument to option %s: %s, %s\n",
	      argv[0], argv[i], argv[1+i], strerror(errno));
      exit(2);
    } else if (tail_ptr == argv[1+i]) {
      fprintf(stderr, "%s: error: invalid argument to option %s: %s\n",
	      argv[0], argv[i], argv[1+i]);
      exit(2);
    } else if (num_of_bytes < 0) {
      fprintf(stderr, "%s: error: invalid negative argument to option %s: %s\n",
	      argv[0], argv[i], argv[1+i]);
      exit(2);
    } else if (num_of_bytes < (3 * IK_PAGESIZE)) {
      fprintf(stderr, "%s: error: invalid argument to option %s: %s, it must be at least 3 pages (%d bytes)\n",
	      argv[0], argv[i], argv[1+i], (3 * IK_PAGESIZE));
      exit(2);
    } else {
      normalised_num_of_bytes = IK_ALIGN_TO_NEXT_PAGE(num_of_bytes);
      IK_RUNTIME_MESSAGE("%s set to %lu bytes", argument_description, normalised_num_of_bytes);
      return normalised_num_of_bytes;
    }
  } else {
    fprintf(stderr, "%s: error: option %s needs an argument\n", argv[0], argv[i]);
    exit(2);
  }
}

/* end of file */
