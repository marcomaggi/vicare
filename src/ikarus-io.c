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
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>


/** --------------------------------------------------------------------
 ** File descriptors handling for Scheme ports.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_close_fd (ikptr fd /*, ikpcb* pcb */)
{
  int   rv;
  errno = 0;
  rv    = close(unfix(fd));
  return (-1 != rv)? false_object : ik_errno_to_code();
}
ikptr
ikrt_set_position (ikptr fd, ikptr pos /*, ikpcb* pcb */)
{
  off_t         offset;
  off_t         rv;
  offset = extract_num_longlong(pos);
  errno  = 0;
  rv     = lseek(unfix(fd), offset, SEEK_SET);
  return (-1 != rv)? false_object : ik_errno_to_code();
}
ikptr
ikrt_open_input_fd (ikptr pathname_bv, ikptr ikopts /*, ikpcb* pcb */)
{
  const char *  pathname;
  VICARE_UNUSED int opts  = unfix(ikopts);
  int           flags = O_RDONLY; /* no special flags supported at present */
  /* The "mode" value is used  only when creating the file, which should
     not happen here. */
  int           mode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  int           fd;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  fd       = open(pathname, flags, mode);
  return (0 <= fd)? fix(fd) : ik_errno_to_code();
}
ikptr
ikrt_open_output_fd (ikptr pathname_bv, ikptr ikopts /*, ikpcb* pcb */)
{
  const char *  pathname;
  int           opts  = unfix(ikopts);
  int           mode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  int           flags = 0;
  int           fd;
  /*
   * File options:
   *
   *   (file-options)                                    => 0
   *   (file-options no-create)                          => #b001 = 1
   *   (file-options no-fail)                            => #b010 = 2
   *   (file-options no-create no-fail)                  => #b011 = 3
   *   (file-options no-truncate)                        => #b100 = 4
   *   (file-options no-create no-truncate)              => #b101 = 5
   *   (file-options no-fail no-truncate)                => #b110 = 6
   *   (file-options no-create no-fail no-truncate)      => #b111 = 7
   *
   * According to R6RS:
   *
   *   When supplied  to an operation that  opens a file  for output, the
   *   file-options object returned by (FILE-OPTIONS) (without arguments)
   *   specifies that  the file is  created if it  does not exist  and an
   *   exception with condition type "&i/o-file-already-exists" is raised
   *   if it does exist.
   *
   * According to the GNU C Library documentation:
   *
   * O_CREAT
   *   If set the file will be created if it does not exists.
   *
   * O_CREAT | O_EXCL
   *   "open()" fails if the specified file already exists.
   *
   * O_TRUNC
   *   Truncate the file to zero length.
   */
  switch (opts){
  case 0: flags = O_WRONLY | O_CREAT | O_EXCL ; /* (file-options) */
    break;
  case 1: flags = O_WRONLY | O_TRUNC          ; /* (file-options no-create) */
    break;
  case 2: flags = O_WRONLY | O_TRUNC | O_CREAT; /* (file-options no-fail) */
    break;
  case 3: flags = O_WRONLY | O_TRUNC          ; /* (file-options no-create no-fail) */
    break;
  case 4: flags = O_WRONLY | O_CREAT | O_EXCL ; /* (file-options no-truncate) */
    break;
  case 5: flags = O_WRONLY                    ; /* (file-options no-create no-truncate) */
    break;
  case 6: flags = O_WRONLY | O_CREAT          ; /* (file-options no-fail no-truncate) */
    break;
  case 7: flags = O_WRONLY                    ; /* (file-options no-create no-fail no-truncate) */
    break;
  }
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  fd       = open(pathname, flags, mode);
  return (0 <= fd)? fix(fd) : ik_errno_to_code();
}
ikptr
ikrt_open_input_output_fd (ikptr pathname_bv, ikptr ikopts /*, ikpcb* pcb */)
{
  const char *  pathname;
  int           opts  = unfix(ikopts);
  int           mode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  int           flags = 0;
  int           fd;
  /* With   the   exception  of   O_RDWR,   these   are   the  same   of
     "ikrt_open_output_fd()". */
  switch (opts) {
  case 0: flags = O_RDWR | O_CREAT | O_EXCL ; /* (file-options) */
    break;
  case 1: flags = O_RDWR | O_TRUNC          ; /* (file-options no-create) */
    break;
  case 2: flags = O_RDWR | O_TRUNC | O_CREAT; /* (file-options no-fail) */
    break;
  case 3: flags = O_RDWR | O_TRUNC          ; /* (file-options no-create no-fail) */
    break;
  case 4: flags = O_RDWR | O_CREAT | O_EXCL ; /* (file-options no-truncate) */
    break;
  case 5: flags = O_RDWR                    ; /* (file-options no-create no-truncate) */
    break;
  case 6: flags = O_RDWR | O_CREAT          ; /* (file-options no-fail no-truncate) */
    break;
  case 7: flags = O_RDWR                    ; /* (file-options no-create no-fail no-truncate) */
    break;
  }
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno = 0;
  fd    = open(pathname, flags, mode);
  return (0 <= fd)? fix(fd) : ik_errno_to_code();
}
ikptr
ikrt_read_fd (ikptr fd, ikptr buffer_bv, ikptr buffer_offset, ikptr requested_count /*, ikpcb* pcb */)
{
  ssize_t       rv;
  uint8_t *     buffer;
  buffer = VICARE_BYTEVECTOR_DATA_VOIDP(buffer_bv) + unfix(buffer_offset);
  errno  = 0;
  rv     = read(unfix(fd), buffer, unfix(requested_count));
  return (0 <= rv)? fix(rv) : ik_errno_to_code();
}
ikptr
ikrt_write_fd (ikptr fd, ikptr buffer_bv, ikptr buffer_offset, ikptr requested_count /*, ikpcb* pcb */)
{
  ssize_t       rv;
  uint8_t *     buffer;
  buffer = VICARE_BYTEVECTOR_DATA_VOIDP(buffer_bv) + unfix(buffer_offset);
  errno  = 0;
  rv     = write(unfix(fd), buffer, unfix(requested_count));
  return (0 <= rv)? fix(rv) : ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Original Ikarus networking interface.
 ** ----------------------------------------------------------------- */

/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_tcp_connect(ikptr host, ikptr srvc /*, ikpcb* pcb */){
  return false_object;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_udp_connect(ikptr host, ikptr srvc /*, ikpcb* pcb */){
  return false_object;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_make_fd_nonblocking(ikptr fdptr /*, ikpcb* pcb */){
  return false_object;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_select(ikptr fds, ikptr rfds, ikptr wfds, ikptr xfds /*, ikpcb* pcb */){
  return false_object;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_listen(ikptr port /*, ikpcb* pcb */){
  return false_object;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_accept(ikptr s, ikptr bv /*, ikpcb* pcb */){
  return false_object;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_shutdown(ikptr s /*, ikpcb* pcb*/){
  return false_object;
}

#if 0
static ikptr
do_connect(ikptr host, ikptr srvc, int socket_type){
  struct addrinfo* info;
  int err = getaddrinfo((char*)(long)(host+off_bytevector_data),
                        (char*)(long)(srvc+off_bytevector_data),
                        0,
                        &info);
  if(err){
    switch(err){
      case EAI_SYSTEM: return ik_errno_to_code();
      default: return false_object;
    }
  }
  struct addrinfo* i = info;
  ikptr sock = false_object;
  while(i){
    if(i->ai_socktype != socket_type){
      i = i->ai_next;
    } else {
      int s = socket(i->ai_family, i->ai_socktype, i->ai_protocol);
      if(s < 0){
        sock = ik_errno_to_code();
        i = i->ai_next;
      } else {
        int err = connect(s, i->ai_addr, i->ai_addrlen);
        if(err < 0){
          sock = ik_errno_to_code();
          i = i->ai_next;
        } else {
          sock = fix(s);
          i = 0;
        }
      }
    }
  }
  freeaddrinfo(info);
  return sock;
}

ikptr
ikrt_tcp_connect(ikptr host, ikptr srvc /*, ikpcb* pcb */){
  return do_connect(host, srvc, SOCK_STREAM);
}

ikptr
ikrt_udp_connect(ikptr host, ikptr srvc /*, ikpcb* pcb */){
  return do_connect(host, srvc, SOCK_DGRAM);
}

ikptr
ikrt_make_fd_nonblocking(ikptr fdptr /*, ikpcb* pcb */){
  int fd = unfix(fdptr);
  int err = fcntl(fd, F_SETFL, O_NONBLOCK);
  if(err == -1){
    return ik_errno_to_code();
  }
  return 0;
}

ikptr
ikrt_select(ikptr fds, ikptr rfds, ikptr wfds, ikptr xfds /*, ikpcb* pcb */){
  int rv = select(unfix(fds),
                  (fd_set*)(rfds + off_bytevector_data),
                  (fd_set*)(wfds + off_bytevector_data),
                  (fd_set*)(xfds + off_bytevector_data),
                  NULL);
  if(rv < 0){
    return ik_errno_to_code();
  }
  return fix(rv);
}

ikptr
ikrt_listen(ikptr port /*, ikpcb* pcb */){

  int sock = socket(AF_INET, SOCK_STREAM, 0);
  if(sock < 0){
    return ik_errno_to_code();
  }

  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(struct sockaddr_in));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  servaddr.sin_port = htons(unfix(port));

  int err;

  int reuse = 1;
  err = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(int));
  if(err < 0){
    return ik_errno_to_code();
  }


  err = bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr));
  if(err < 0){
    return ik_errno_to_code();
  }

  err = listen(sock, 1024);
  if(err < 0){
    return ik_errno_to_code();
  }
  return fix(sock);
}

#if 0
not used
ikptr
ikrt_getsockname(ikptr s, ikpcb* pcb){
  socklen_t size = sizeof(struct sockaddr);
  ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data+size))
             + bytevector_tag;
  int r = getsockname(unfix(s),
                     (struct sockaddr*)(bv+off_bytevector_data),
                     &size);
  if(r == 0){
    ref(bv, off_bytevector_length) = fix(size);
    return bv;
  } else {
    return ik_errno_to_code();
  }
}
#endif



ikptr
ikrt_accept(ikptr s, ikptr bv /*, ikpcb* pcb */){
  socklen_t addrlen = unfix(ref(bv, off_bytevector_length));
  int sock = accept(unfix(s),
                    (struct sockaddr*) (bv+off_bytevector_data),
                    &addrlen);
  if(sock < 0){
    return ik_errno_to_code();
  }
  ref(bv, off_bytevector_length) = fix(addrlen);
  return fix(sock);
}

ikptr
ikrt_shutdown(ikptr s /*, ikpcb* pcb*/){
#ifdef __CYGWIN__
  int err = close(unfix(s));
#else
  int err = shutdown(unfix(s), SHUT_RDWR);
#endif
  if(err < 0){
    return ik_errno_to_code();
  }
  return 0;
}
#endif

/* end of file */
