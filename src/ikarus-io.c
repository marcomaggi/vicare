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


#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <netinet/in.h>
#include <dirent.h>
#include "ikarus-data.h"

extern ikptr ik_errno_to_code();

ikptr
ikrt_close_fd(ikptr fd /*, ikpcb* pcb */){
  int err = close(unfix(fd));
  if(err == -1){
    return ik_errno_to_code();
  } else {
    return false_object;;
  }
}

ikptr
ikrt_set_position(ikptr fd, ikptr pos /*, ikpcb* pcb */){
  off_t offset = extract_num_longlong(pos);
  off_t err = lseek(unfix(fd), offset, SEEK_SET);
  if(err == -1){
    return ik_errno_to_code();
  } else {
    return false_object;;
  }
}


ikptr
ikrt_open_input_fd (ikptr fn, ikptr ikopts /*, ikpcb* pcb */)
{
  int opts  = unfix(ikopts);
  /* no special flags supported at present */
  int flags = O_RDONLY;
  /* the "mode" value is used  only when creating the file, which should
     not happen here */
  int mode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  int fh = open((char*)(long)(fn+off_bytevector_data), flags, mode);
  if (fh >= 0){
    return fix(fh);
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_open_output_fd (ikptr fn, ikptr ikopts /*, ikpcb* pcb */)
{
  int opts  = unfix(ikopts);
  int mode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  int flags = 0;
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
  int fh = open((char*)(long)(fn+off_bytevector_data), flags, mode);
  if(fh >= 0){
    return fix(fh);
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_open_input_output_fd(ikptr fn, ikptr ikopts /*, ikpcb* pcb */){
  int opts  = unfix(ikopts);
  int mode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  int flags = 0;
  /* these are the same of "ikrt_open_output_fd()" */
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
  int fh = open((char*)(long)(fn+off_bytevector_data), flags, mode);
  if(fh >= 0){
    return fix(fh);
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_read_fd(ikptr fd, ikptr bv, ikptr off, ikptr cnt /*, ikpcb* pcb */){
#if 0
  fprintf(stderr, "READ: %d\n", unfix(fd));
#endif
  ssize_t bytes =
   read(unfix(fd),
        (char*)(long)(bv+off_bytevector_data+unfix(off)),
        unfix(cnt));
#if 0
  fprintf(stderr, "BYTES: %d\n", bytes);
#endif
  if(bytes >= 0){
    return fix(bytes);
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_write_fd(ikptr fd, ikptr bv, ikptr off, ikptr cnt /*, ikpcb* pcb */){
#if 0
  if (0) {
    fprintf(stderr, "WRITE %d, %p %d %d %d\n",
         unfix(fd),
         bv,
         unfix(ref(bv, off_bytevector_length)),
         unfix(off),
        unfix(cnt));
    int i;
    for(i=0; i<100; i++){
      fprintf(stderr, "bv[%d]=0x%02x ", i,
             ((char*)(bv+off_bytevector_data))[i]);
    }
    fprintf(stderr, "\n");
  }
#endif
  ssize_t bytes =
   write(unfix(fd),
         (char*)(long)(bv+off_bytevector_data+unfix(off)),
         unfix(cnt));
  if(bytes >= 0){
    return fix(bytes);
  } else {
    return ik_errno_to_code();
  }
}



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


static ikptr
timespec_bytevector(struct timespec* s, ikpcb* pcb) {
  int len = sizeof(struct timespec);
  ikptr r = ik_safe_alloc(pcb, align(disp_bytevector_data+len+3));
  ref(r, 0) = fix(len+2);
  *((char*)(r+disp_bytevector_data+0)) = sizeof(s->tv_sec);
  *((char*)(r+disp_bytevector_data+1)) = sizeof(s->tv_nsec);
  memcpy((char*)(r+disp_bytevector_data+2), s, len);
  *((char*)(r+disp_bytevector_data+len+2)) = 0;
  return r + bytevector_tag;
}


ikptr
ikrt_file_ctime2(ikptr filename, ikpcb* pcb){
  struct stat s;
  int err = stat((char*)(filename + off_bytevector_data), &s);
  if(err) {
    return ik_errno_to_code();
  }
#if HAVE_STAT_ST_CTIMESPEC
  return timespec_bytevector(&s.st_ctimespec, pcb);
#elif HAVE_STAT_ST_CTIM
  return timespec_bytevector(&s.st_ctim, pcb);
#else
  struct timespec ts;
  ts.tv_sec = s.st_ctime;
  ts.tv_nsec = 0;
  return timespec_bytevector(&ts, pcb);
#endif
}

ikptr
ikrt_file_mtime2(ikptr filename, ikpcb* pcb){
  struct stat s;
  int err = stat((char*)(filename + off_bytevector_data), &s);
  if(err) {
    return ik_errno_to_code();
  }
#if HAVE_STAT_ST_MTIMESPEC
  return timespec_bytevector(&s.st_mtimespec, pcb);
#elif HAVE_STAT_ST_MTIM
  return timespec_bytevector(&s.st_mtim, pcb);
#else
  struct timespec ts;
  ts.tv_sec = s.st_mtime;
  ts.tv_nsec = 0;
  return timespec_bytevector(&ts, pcb);
#endif
}




ikptr
ikrt_file_ctime(ikptr filename, ikptr res){
  struct stat s;
  int err = stat((char*)(filename + off_bytevector_data), &s);
  if(err) {
    return ik_errno_to_code();
  }

  ref(res, off_car) = fix(s.st_ctime);
  ref(res, off_cdr) = 0;
  return fix(0);
}

ikptr
ikrt_file_mtime(ikptr filename, ikptr res){
  struct stat s;
  int err = stat((char*)(filename + off_bytevector_data), &s);
  if(err) {
    return ik_errno_to_code();
  }

  ref(res, off_car) = fix(s.st_mtime);
  ref(res, off_cdr) = 0;
  return fix(0);
}


ikptr
ikrt_opendir(ikptr dirname, ikpcb* pcb){
  DIR* d = opendir((char*)(dirname+off_bytevector_data));
  if(d == NULL){
    return ik_errno_to_code();
  }
  return(make_pointer((long)d, pcb));
}

ikptr
ikrt_readdir(ikptr ptr, ikpcb* pcb){
  DIR* d = (DIR*) ref(ptr, off_pointer_data);
  errno = 0;
  struct dirent* ent = readdir(d);
  if (ent == NULL){
    return (errno ? ik_errno_to_code() : false_object);
  }
  int len = strlen(ent->d_name);
  ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1))
             + bytevector_tag;
  ref(bv, -bytevector_tag) = fix(len);
  memcpy((char*)(bv+off_bytevector_data), ent->d_name, len+1);
  return bv;
}

ikptr
ikrt_closedir(ikptr ptr, ikpcb* pcb){
  DIR* d = (DIR*) ref(ptr, off_pointer_data);
  int rv = closedir(d);
  if (rv == -1){
    return ik_errno_to_code();
  }
  return 0;
}
