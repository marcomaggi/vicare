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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"
#if (!HAVE_GETADDRINFO)
#include <netdb.h>
#include <sys/socket.h>
#include <cygwin/in.h>


int
getaddrinfo (const char *hostname, const char* servname,
	     const struct addrinfo* hints, struct addrinfo** res)
{
  /* hints not used */
  struct servent* sent = getservbyname(servname, "tcp");
  if (sent == 0) return -1;
  struct hostent* hent = gethostbyname(hostname);
  if (!hent){
    return -1;
  }
  struct addrinfo* r =  malloc(sizeof(struct addrinfo));
  if(r == 0) return -1;
  r->ai_family = hent->h_addrtype;
  r->ai_socktype = SOCK_STREAM;
  r->ai_protocol = 0;
  r->ai_addrlen = sizeof(struct sockaddr_in);
  r->ai_addr = malloc(r->ai_addrlen);
  if (r->ai_addr == 0){
    free(r);
    return -1;
  }
  struct sockaddr_in* sa_in = (struct sockaddr_in *)r->ai_addr;
  memset(sa_in, 0, sizeof(struct sockaddr_in));
  sa_in->sin_family = PF_INET;
  sa_in->sin_port = sent->s_port;
  struct in_addr** ap = (struct in_addr **)hent->h_addr_list;
  memcpy(&sa_in->sin_addr, *ap, sizeof(struct in_addr));
  r->ai_next = NULL;
  *res = r;
  return 0;
}
void
freeaddrinfo (struct addrinfo *ai)
{
  free(ai->ai_addr);
  free(ai);
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* if (!HAVE_GETADDRINFO) */

/* end of file */
