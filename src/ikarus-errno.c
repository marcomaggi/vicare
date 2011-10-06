/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 *  Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "ikarus-data.h"

typedef struct errno_info {
  int n;
  char* s;
  ikptr c;
} errno_info;

#ifndef ECANCELED
#define ECANCELED 140
#endif

#ifndef ENODATA
#define ENODATA 96
#endif

#ifndef ENOSR
#define ENOSR 98
#endif

#ifndef ENOSTR
#define ENOSTR 99
#endif

#ifndef ETIME
#define ETIME 101
#endif

static errno_info errno_table[] = {
  /* errnos from POSIX IEEE Std 1003.1 2004 Edition */
  {E2BIG,            "E2BIG",            fix(-1)},
  {EACCES,           "EACCES",           fix(-2)},
  {EADDRINUSE,       "EADDRINUSE",       fix(-3)},
  {EADDRNOTAVAIL,    "EADDRNOTAVAIL",    fix(-4)},
  {EAFNOSUPPORT,     "EAFNOSUPPORT",     fix(-5)},
  {EAGAIN,           "EAGAIN",           fix(-6)},
  {EALREADY,         "EALREADY",         fix(-7)},
  {EBADF,            "EBADF",            fix(-8)},
  {EBADMSG,          "EBADMSG",          fix(-9)},
  {EBUSY,            "EBUSY",            fix(-10)},
  {ECANCELED,        "ECANCELED",        fix(-11)},
  {ECHILD,           "ECHILD",           fix(-12)},
  {ECONNABORTED,     "ECONNABORTED",     fix(-13)},
  {ECONNREFUSED,     "ECONNREFUSED",     fix(-14)},
  {ECONNRESET,       "ECONNRESET",       fix(-15)},
  {EDEADLK,          "EDEADLK",          fix(-16)},
  {EDESTADDRREQ,     "EDESTADDRREQ",     fix(-17)},
  {EDOM,             "EDOM",             fix(-18)},
  {EDQUOT,           "EDQUOT",           fix(-19)},
  {EEXIST,           "EEXIST",           fix(-20)},
  {EFAULT,           "EFAULT",           fix(-21)},
  {EFBIG,            "EFBIG",            fix(-22)},
  {EHOSTUNREACH,     "EHOSTUNREACH",     fix(-23)},
  {EIDRM,            "EIDRM",            fix(-24)},
  {EILSEQ,           "EILSEQ",           fix(-25)},
  {EINPROGRESS,      "EINPROGRESS",      fix(-26)},
  {EINTR,            "EINTR",            fix(-27)},
  {EINVAL,           "EINVAL",           fix(-28)},
  {EIO,              "EIO",              fix(-29)},
  {EISCONN,          "EISCONN",          fix(-30)},
  {EISDIR,           "EISDIR",           fix(-31)},
  {ELOOP,            "ELOOP",            fix(-32)},
  {EMFILE,           "EMFILE",           fix(-33)},
  {EMLINK,           "EMLINK",           fix(-34)},
  {EMSGSIZE,         "EMSGSIZE",         fix(-35)},
  {EMULTIHOP,        "EMULTIHOP",        fix(-36)},
  {ENAMETOOLONG,     "ENAMETOOLONG",     fix(-37)},
  {ENETDOWN,         "ENETDOWN",         fix(-38)},
  {ENETRESET,        "ENETRESET",        fix(-39)},
  {ENETUNREACH,      "ENETUNREACH",      fix(-40)},
  {ENFILE,           "ENFILE",           fix(-41)},
  {ENOBUFS,          "ENOBUFS",          fix(-42)},
  {ENODATA,          "ENODATA",          fix(-43)},
  {ENODEV,           "ENODEV",           fix(-44)},
  {ENOENT,           "ENOENT",           fix(-45)},
  {ENOEXEC,          "ENOEXEC",          fix(-46)},
  {ENOLCK,           "ENOLCK",           fix(-47)},
  {ENOLINK,          "ENOLINK",          fix(-48)},
  {ENOMEM,           "ENOMEM",           fix(-49)},
  {ENOMSG,           "ENOMSG",           fix(-50)},
  {ENOPROTOOPT,      "ENOPROTOOPT",      fix(-51)},
  {ENOSPC,           "ENOSPC",           fix(-52)},
  {ENOSR,            "ENOSR",            fix(-53)},
  {ENOSTR,           "ENOSTR",           fix(-54)},
  {ENOSYS,           "ENOSYS",           fix(-55)},
  {ENOTCONN,         "ENOTCONN",         fix(-56)},
  {ENOTDIR,          "ENOTDIR",          fix(-57)},
  {ENOTEMPTY,        "ENOTEMPTY",        fix(-58)},
  {ENOTSOCK,         "ENOTSOCK",         fix(-59)},
  {ENOTSUP,          "ENOTSUP",          fix(-60)},
  {ENOTTY,           "ENOTTY",           fix(-61)},
  {ENXIO,            "ENXIO",            fix(-62)},
  {EOPNOTSUPP,       "EOPNOTSUPP",       fix(-63)},
  {EOVERFLOW,        "EOVERFLOW",        fix(-64)},
  {EPERM,            "EPERM",            fix(-65)},
  {EPIPE,            "EPIPE",            fix(-66)},
  {EPROTO,           "EPROTO",           fix(-67)},
  {EPROTONOSUPPORT,  "EPROTONOSUPPORT",  fix(-68)},
  {EPROTOTYPE,       "EPROTOTYPE",       fix(-69)},
  {ERANGE,           "ERANGE",           fix(-70)},
  {EROFS,            "EROFS",            fix(-71)},
  {ESPIPE,           "ESPIPE",           fix(-72)},
  {ESRCH,            "ESRCH",            fix(-73)},
  {ESTALE,           "ESTALE",           fix(-74)},
  {ETIME,            "ETIME",            fix(-75)},
  {ETIMEDOUT,        "ETIMEDOUT",        fix(-76)},
  {ETXTBSY,          "ETXTBSY",          fix(-77)},
  {EWOULDBLOCK,      "EWOULDBLOCK",      fix(-78)},
  {EXDEV,            "EXDEV",            fix(-79)}
};

#define errno_table_length 79

ikptr
ik_errno_num_to_code(int en){
  /* Used by C to return Ikarus errno code to Scheme. */
  errno_info* ei;
  int i;
  for(i=0; i < errno_table_length; i++){
    ei = &errno_table[i];
    if(ei->n == en){
      return ei->c;
    }
  }
  fprintf(stderr, "\n*** ikarus-errno.c: Don't know errno %d ***\n\n", en);
  return fix(-99999);  /* should always be less than last errno code */
}

ikptr
ik_errno_to_code(){
  int en = errno;
  return ik_errno_num_to_code(en);
}
ikptr
ik_errno_EAGAIN (void)
{
  return ik_errno_num_to_code(EAGAIN);
}
ikptr
ik_errno_EACCES (void)
{
  return ik_errno_num_to_code(EACCES);
}
ikptr
ik_errno_EFAULT (void)
{
  return ik_errno_num_to_code(EFAULT);
}
ikptr
ik_errno_EROFS (void)
{
  return ik_errno_num_to_code(EROFS);
}
ikptr
ik_errno_EEXIST (void)
{
  return ik_errno_num_to_code(EEXIST);
}
ikptr
ik_errno_EIO (void)
{
  return ik_errno_num_to_code(EIO);
}
ikptr
ik_errno_ENOENT (void)
{
  return ik_errno_num_to_code(ENOENT);
}


ikptr
ikrt_errno_code_to_name(ikptr ec, ikpcb* pcb){
  /* Used by Scheme to get the name of the errno corresponding
     to the Ikarus errno code. */
  errno_info* ei;
  int i;
  for(i=0; i < errno_table_length; i++){
    ei = &errno_table[i];
    if(ei->c == ec){
      int len = strlen(ei->s);
      ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1))
                 + bytevector_tag;
      ref(bv, off_bytevector_length) = fix(len);
      memcpy((char*)(bv+off_bytevector_data), ei->s, len+1);
      return bv;
    }
  }
  return false_object;
}

ikptr
ikrt_strerror(ikptr ec, ikpcb* pcb){
  /* Used by Scheme to get the error message of the errno corresponding
     to the Ikarus errno code. */
  errno_info* ei;
  int i;
  for(i=0; i < errno_table_length; i++){
    ei = &errno_table[i];
    if(ei->c == ec){
      errno = 0;
      char* es = strerror(ei->n);
      if(errno){
        perror("ikrt_strerror: strerror failed");
        exit(EXIT_FAILURE);
      }
      int len = strlen(es);
      ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1))
                 + bytevector_tag;
      ref(bv, off_bytevector_length) = fix(len);
      memcpy((char*)(bv+off_bytevector_data), es, len+1);
      return bv;
    }
  }
  return false_object;
}

