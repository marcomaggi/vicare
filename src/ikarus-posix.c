/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract



  Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum

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


/* headers */

#include "ikarus-data.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <fcntl.h>
#include <string.h>
#include <strings.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <dirent.h>
#ifdef __CYGWIN__
#include "ikarus-winmmap.h"
#endif

ikptr ik_errno_to_code (void);

extern char **environ;


ikptr
ikrt_stat (ikptr filename, ikptr follow /*, ikpcb* pcb */)
{
  char* fn = (char*)(filename + off_bytevector_data);
  struct stat s;
  int r;
  if (false_object == follow) {
    r = lstat(fn, &s);
  } else{
    r = stat(fn, &s);
  }
  if (0 == r) {
    if (S_ISREG(s.st_mode)) {
      return fix(1);
    } else if(S_ISDIR(s.st_mode)) {
      return fix(2);
    } else if(S_ISLNK(s.st_mode)){
      return fix(3);
    } else {
      return fix(0);
    }
  }
  return ik_errno_to_code();
}

ikptr
ikrt_delete_file(ikptr filename){
  char* str;
  if(tagof(filename) == bytevector_tag){
    str = (char*)(long)(filename + off_bytevector_data);
  } else {
    fprintf(stderr, "bug in ikrt_delete_file\n");
    exit(-1);
  }
  int err = unlink(str);
  if(err == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_directory_list(ikptr filename, ikpcb* pcb){
  DIR* dir;
  struct dirent* de;
  if((dir = opendir((char*)(filename + off_bytevector_data))) == NULL){
    return ik_errno_to_code();
  }
  ikptr ac = null_object;
  pcb->root0 = &ac;
  while(1){
    errno = 0;
    de = readdir(dir);
    if(de == NULL){
      pcb->root0 = 0;
      ikptr retval = (errno ? ik_errno_to_code() : ac);
      closedir(dir);
      return retval;
    }
    int len = strlen(de->d_name);
    ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1))
               + bytevector_tag;
    ref(bv, off_bytevector_length) = fix(len);
    memcpy((char*)(bv+off_bytevector_data), de->d_name, len+1);
    pcb->root1 = &bv;
    ikptr p = ik_safe_alloc(pcb, pair_size) + pair_tag;
    pcb->root1 = 0;
    ref(p, off_car) = bv;
    ref(p, off_cdr) = ac;
    ac = p;
  }
}

ikptr
ikrt_mkdir(ikptr path, ikptr mode /*, ikpcb* pcb */){
  int r = mkdir((char*)(path+off_bytevector_data), unfix(mode));
  if(r == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_rmdir(ikptr path /*, ikpcb* pcb */){
  int r = rmdir((char*)(path+off_bytevector_data));
  if(r == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_chmod(ikptr path, ikptr mode /*, ikpcb* pcb */){
  int r = chmod((char*)(path+off_bytevector_data), (mode_t)unfix(mode));
  if(r == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_symlink(ikptr to, ikptr path /*, ikpcb* pcb */){
  int r = symlink((char*)(to+off_bytevector_data), (char*)(path+off_bytevector_data));
  if(r == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_link(ikptr to, ikptr path /*, ikpcb* pcb */){
  int r = link((char*)(to+off_bytevector_data), (char*)(path+off_bytevector_data));
  if(r == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_realpath(ikptr bv, ikpcb* pcb){
  char buff[PATH_MAX];
  char* p = realpath((char*)(bv+off_bytevector_data), buff);
  if(p == NULL){
    return ik_errno_to_code();
  }
  int n = strlen(p);
  ikptr r = ik_safe_alloc(pcb, align(disp_bytevector_data+n+1));
  ref(r, 0) = fix(n);
  memcpy((char*)(r+disp_bytevector_data), p, n+1);
  return r+bytevector_tag;
}

ikptr
ikrt_system(ikptr str){
  if(tagof(str) == bytevector_tag){
    int r = system((char*)(long)(str+off_bytevector_data));
    if(r >= 0) {
      return fix(r);
    } else {
      return ik_errno_to_code();
    }
  } else {
    fprintf(stderr, "bug in ikrt_system\n");
    exit(-1);
  }
}
/* FIXME Stale.  To be removed at the next boot image rotation. */
ikptr
ik_system(ikptr str) {
  return ikrt_system(str);
}

ikptr
ikrt_current_time(ikptr t){
  struct timeval s;
  gettimeofday(&s, 0);
  /* this will break in 8,727,224 years if we stay in 32-bit ptrs */
  ref(t, off_record_data + 0*wordsize) = fix(s.tv_sec / 1000000);
  ref(t, off_record_data + 1*wordsize) = fix(s.tv_sec % 1000000);
  ref(t, off_record_data + 2*wordsize) = fix(s.tv_usec);
  return t;
}

ikptr
ikrt_gmt_offset(ikptr t){
  time_t clock =
    unfix(ref(t, off_record_data + 0*wordsize)) * 1000000
    + unfix(ref(t, off_record_data + 1*wordsize));
  struct tm* m = gmtime(&clock);
  time_t gmtclock = mktime(m);
  return fix(clock - gmtclock);
  /*
  struct tm* m = localtime(&clock);
  ikptr r = fix(m->tm_gmtoff);
  return r;
  */
}

ikptr
ikrt_fork(){
  int pid = fork();
  if(pid >= 0){
    return fix(pid);
  } else {
    return ik_errno_to_code();
  }
}



ikptr
ikrt_getenv(ikptr bv, ikpcb* pcb){
  char* v = getenv((char*)(long)(bv + off_bytevector_data));
  if(v){
    long int n = strlen(v);
    ikptr s = ik_safe_alloc(pcb, align(n+disp_bytevector_data+1))
              + bytevector_tag;
    ref(s, -bytevector_tag) = fix(n);
    memcpy((char*)(long)(s+off_bytevector_data), v, n+1);
    return s;
  }
  else {
    return false_object;
  }
}

ikptr
ikrt_setenv(ikptr key, ikptr val, ikptr overwrite){
  int err = setenv((char*)(key+off_bytevector_data),
                   (char*)(val+off_bytevector_data),
                   overwrite!=false_object);
  if(err){
    return false_object;
  } else {
    return true_object;
  }
}

ikptr
ikrt_unsetenv(ikptr key){
  unsetenv((char*)(key+off_bytevector_data));
  return void_object;
}



ikptr
ikrt_environ(ikpcb* pcb){
  char** es = environ;
  int i; char* e;
  ikptr ac = null_object;
  pcb->root0 = &ac;
  for(i=0; (e=es[i]); i++){
    long int n = strlen(e);
    ikptr s = ik_safe_alloc(pcb, align(n+disp_bytevector_data+1))
      + bytevector_tag;
    ref(s, -bytevector_tag) = fix(n);
    memcpy((char*)(long)(s+off_bytevector_data), e, n+1);
    pcb->root1 = &s;
    ikptr p = ik_safe_alloc(pcb, pair_size) + pair_tag;
    pcb->root1 = 0;
    ref(p, off_cdr) = ac;
    ref(p, off_car) = s;
    ac = p;
  }
  pcb->root0 = 0;
  return ac;
}

ikptr
ikrt_exit(ikptr status, ikpcb* pcb){
  ik_delete_pcb(pcb);
  assert(total_allocated_pages == 0);
  if(is_fixnum(status)){
    exit(unfix(status));
  } else {
    exit(EXIT_FAILURE);
  }
}

ikptr
ikrt_nanosleep(ikptr secs, ikptr nsecs /*, ikpcb* pcb */){
  struct timespec t;
  t.tv_sec =
    is_fixnum(secs)
      ? (unsigned long) unfix(secs)
      : ref(secs, off_bignum_data);
  t.tv_nsec =
    is_fixnum(nsecs)
      ? (unsigned long) unfix(nsecs)
      : ref(nsecs, off_bignum_data);
  return fix(nanosleep(&t, NULL));
}

ikptr
ikrt_chdir(ikptr pathbv /*, ikpcb* pcb */){
  int err = chdir(off_bytevector_data+(char*)pathbv);
  if(err == 0){
    return true_object;
  }
  return ik_errno_to_code();
}

ikptr
ikrt_getcwd(ikpcb* pcb){
  char buff[MAXPATHLEN+1];
  char* path = getcwd(buff, MAXPATHLEN);
  if(! path){
    return ik_errno_to_code();
  }
  int len = strlen(path);
  ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1));
  ref(bv,0) = fix(len);
  memcpy(disp_bytevector_data+(char*)(bv), path, len+1);
  return bv+bytevector_tag;
}

ikptr
ikrt_access(ikptr filename, ikptr how /*, ikpcb* pcb */){
  char* fn = (char*)(filename + off_bytevector_data);
  int r;
  int ik_how;
  int c_how;

  ik_how = unfix(how);
  if (ik_how == 0) {
    c_how = F_OK;
  } else {
    c_how = 0;
    if (ik_how & 1) c_how |= R_OK;
    if (ik_how & 2) c_how |= W_OK;
    if (ik_how & 4) c_how |= X_OK;
  }

  r = access(fn, c_how);
  if (r == 0) {
    return true_object;
  } else if ((errno == EACCES) ||
             (errno == EROFS) ||
             (errno == ETXTBSY)) {
    return false_object;
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_file_size(ikptr filename, ikpcb* pcb){
  char* fn = (char*)(filename + off_bytevector_data);
  struct stat s;
  int r = stat(fn, &s);
  if (r == 0) {
    if (sizeof(off_t) == sizeof(long)) {
      return u_to_number(s.st_size, pcb);
    } else if (sizeof(off_t) == sizeof(long long)) {
      return ull_to_number(s.st_size, pcb);
    } else {
      fprintf(stderr, "vicare internal error: invalid off_t size\n");
      exit(-1);
    }
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_rename_file(ikptr src, ikptr dst /* ikpcb* pcb */){
  int err = rename((char*)(src + off_bytevector_data),
                   (char*)(dst + off_bytevector_data));
  if (err == 0) {
    return true_object;
  } else {
    return ik_errno_to_code();
  }
}

/* end of file */
