/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        Interface to POSIX functions.  For the full documentation of the
        functions in this module,  see the official Vicare documentation
        in Texinfo format.

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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "ikarus.h"
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <time.h>
#include <utime.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>


/** --------------------------------------------------------------------
 ** Errno handling.
 ** ----------------------------------------------------------------- */

ikptr
ik_errno_to_code (void)
/* Negate the current errno value and convert it into a fixnum. */
{
  int en = - errno;
  return fix(en);
}
ikptr
ikrt_posix_strerror (ikptr negated_errno_code, ikpcb* pcb)
{
  int   code = - unfix(negated_errno_code);
  errno = 0;
  char* es = strerror(code);
  if (errno) {
    return false_object;
  } else {
    int   len = strlen(es);
    ikptr bv  = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1))
      + bytevector_tag;
    ref(bv, off_bytevector_length) = fix(len);
    memcpy((char*)(bv+off_bytevector_data), es, len+1);
    return bv;
  }
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_strerror (ikptr negated_errno_code, ikpcb* pcb)
{
  return ikrt_posix_strerror(negated_errno_code, pcb);
}


/** --------------------------------------------------------------------
 ** Operating system environment variables.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getenv (ikptr bv, ikpcb* pcb)
{
  char * v = getenv((char*)(long)(bv + off_bytevector_data));
  if (v) {
    long int n = strlen(v);
    ikptr s = ik_safe_alloc(pcb, align(n+disp_bytevector_data+1))
      + bytevector_tag;
    ref(s, -bytevector_tag) = fix(n);
    memcpy((char*)(long)(s+off_bytevector_data), v, n+1);
    return s;
  } else {
    return false_object;
  }
}
ikptr
ikrt_posix_setenv (ikptr key, ikptr val, ikptr overwrite)
{
  int err = setenv((char*)(key+off_bytevector_data),
                   (char*)(val+off_bytevector_data),
                   overwrite!=false_object);
  return (err)? false_object : true_object;
}
ikptr
ikrt_posix_unsetenv (ikptr key)
{
#if (1 == UNSETENV_HAS_RETURN_VALUE)
  int rv = unsetenv((char*)(key+off_bytevector_data));
  return (0 == rv)? true_object : false_object;
#else
  unsetenv((char*)(key+off_bytevector_data));
  return true_object;
#endif
}
ikptr
ikrt_posix_environ (ikpcb* pcb)
{
  char **       es = environ;
  int           i;
  char *        e;
  ikptr         ac = null_object;
  pcb->root0 = &ac;
  for (i=0; (e=es[i]); i++) {
    long int bv_len = strlen(e);
    ikptr    s = ik_safe_alloc(pcb, align(bv_len+disp_bytevector_data+1)) + bytevector_tag;
    ref(s, off_bytevector_length) = fix(bv_len);
    memcpy((char*)(long)(s+off_bytevector_data), e, bv_len+1);
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

/* FIXME STALE To be removed after the next boot image rotation. */
ikptr
ikrt_getenv (ikptr bv, ikpcb* pcb)
{
  return ikrt_posix_getenv (bv, pcb);
}
ikptr
ikrt_setenv (ikptr key, ikptr val, ikptr overwrite)
{
  return ikrt_posix_setenv (key, val, overwrite);
}
ikptr
ikrt_unsetenv (ikptr key)
{
  return ikrt_posix_unsetenv (key);
}
ikptr
ikrt_environ (ikpcb* pcb)
{
  return ikrt_posix_environ (pcb);
}


/** --------------------------------------------------------------------
 ** Process identifiers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getpid(void)
{
  return fix(getpid());
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_getpid(void)
{
  return ikrt_getpid();
}

ikptr
ikrt_posix_getppid(void)
{
  return fix(getppid());
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_getppid(void)
{
  return ikrt_getppid();
}


/** --------------------------------------------------------------------
 ** Executing processes.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_system (ikptr str)
/* Interface to "system()". */
{
  assert(bytevector_tag == tagof(str));
  errno = 0;
  int retval = system((char*)(long)(str+off_bytevector_data));
  if (retval >= 0)
    return fix(retval);
  else
    return ik_errno_to_code();
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ik_system (ikptr str) {
  return ikrt_posix_system(str);
}

ikptr
ikrt_posix_fork (void)
{
  errno = 0;
  int pid = fork();
  if (pid >= 0) {
    return fix(pid);
  } else {
    return ik_errno_to_code();
  }
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_fork (void)
{
  return ikrt_posix_fork();
}
ikptr
ikrt_posix_execv (ikptr bv_filename, ikptr list_argv)
{
  char *  filename = (char *)(long)(bv_filename + off_bytevector_data);
  int     argc     = ik_list_length(list_argv);
  char *  argv[1+argc];
  ik_list_to_argv(list_argv, argv);
  errno   = 0;
  execv(filename, argv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}
ikptr
ikrt_posix_execve (ikptr bv_filename, ikptr list_argv, ikptr list_envv)
{
  char *  filename = (char *)(long)(bv_filename + off_bytevector_data);
  int     argc = ik_list_length(list_argv);
  char *  argv[1+argc];
  int     envc = ik_list_length(list_envv);
  char *  envv[1+envc];
  ik_list_to_argv(list_argv, argv);
  ik_list_to_argv(list_envv, envv);
  errno  = 0;
  execve(filename, argv, envv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}
ikptr
ikrt_posix_execvp (ikptr bv_filename, ikptr list_argv)
{
  char *  filename = (char *)(long)(bv_filename + off_bytevector_data);
  int     argc = ik_list_length(list_argv);
  char *  argv[1+argc];
  ik_list_to_argv(list_argv, argv);
  errno  = 0;
  execvp(filename, argv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Process exit status.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_waitpid (ikptr pid, ikptr options)
{
  int   status;
  pid_t retval;
  errno  = 0;
  retval = waitpid(unfix(pid), &status, unfix(options));
  if (0 <= retval)
    return fix(status);
  else
    return ik_errno_to_code();
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_waitpid(ikptr rvec, ikptr pid, ikptr block /*, ikpcb* pcb */) {
  return false_object;
}
ikptr
ikrt_posix_wait (void)
{
  int   status;
  pid_t retval;
  errno  = 0;
  retval = wait(&status);
  if (0 <= retval)
    return fix(status);
  else
    return ik_errno_to_code();
}

ikptr
ikrt_posix_WIFEXITED (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WIFEXITED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WEXITSTATUS (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return fix(WEXITSTATUS(status));
}
ikptr
ikrt_posix_WIFSIGNALED (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WIFSIGNALED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WTERMSIG (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return fix(WTERMSIG(status));
}
ikptr
ikrt_posix_WCOREDUMP (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WCOREDUMP(status))? true_object : false_object;
}
ikptr
ikrt_posix_WIFSTOPPED (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WIFSTOPPED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WSTOPSIG (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return fix(WSTOPSIG(status));
}


/** --------------------------------------------------------------------
 ** Delivering interprocess signals.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_raise (ikptr fx_signum)
{
  int r = raise(unfix(fx_signum));
  return (0 == r)? fix(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_kill (ikptr fx_pid, ikptr fx_signum)
{
  pid_t pid    = unfix(fx_pid);
  int   signum = unfix(fx_signum);
  int   r = kill(pid, signum);
  return (0 == r)? fix(0) : ik_errno_to_code();
}
/* FIXME STALE To be removed at the next boot image rotation */
ikptr
ikrt_kill (ikptr pid, ikptr sigcode /*, ikpcb* pcb */)
{
  return ikrt_posix_kill(pid, sigcode);
}
ikptr
ikrt_posix_pause (void)
{
  pause();
  return void_object;
}


/** --------------------------------------------------------------------
 ** Miscellaneous stat functions.
 ** ----------------------------------------------------------------- */

static ikptr
fill_stat_struct (struct stat * S, ikptr D, ikpcb* pcb)
{
#if (4 == VICARE_SIZE_OF_VOIDP)
  /* 32-bit platforms */
  ref(D, off_record_data+0*wordsize) = u_to_number((unsigned long)S->st_mode, pcb);
  ref(D, off_record_data+1*wordsize) = u_to_number((unsigned long)S->st_ino, pcb);
  ref(D, off_record_data+2*wordsize) = s_to_number((long)S->st_dev, pcb);
  ref(D, off_record_data+3*wordsize) = u_to_number((long)S->st_nlink, pcb);

  ref(D, off_record_data+4*wordsize) = u_to_number((unsigned long)S->st_uid, pcb);
  ref(D, off_record_data+5*wordsize) = u_to_number((unsigned long)S->st_gid, pcb);
  ref(D, off_record_data+6*wordsize) = ull_to_number((unsigned long long)S->st_size, pcb);

  ref(D, off_record_data+7*wordsize) = s_to_number((long)S->st_atime, pcb);
#ifdef HAVE_STAT_ST_ATIME_USEC
  ref(D, off_record_data+8*wordsize) = u_to_number((unsigned long)S->st_atime_usec, pcb);
#else
  ref(D, off_record_data+8*wordsize) = false_object;
#endif

  ref(D, off_record_data+9*wordsize)  = s_to_number((long)S->st_mtime, pcb);
#ifdef HAVE_STAT_ST_MTIME_USEC
  ref(D, off_record_data+10*wordsize) = u_to_number((unsigned long)S->st_mtime_usec, pcb);
#else
  ref(D, off_record_data+10*wordsize) = false_object;
#endif

  ref(D, off_record_data+11*wordsize) = s_to_number((long)S->st_ctime, pcb);
#ifdef HAVE_STAT_ST_CTIME_USEC
  ref(D, off_record_data+12*wordsize) = u_to_number((unsigned long)S->st_ctime_usec, pcb);
#else
  ref(D, off_record_data+12*wordsize) = false_object;
#endif

  ref(D, off_record_data+13*wordsize) = ull_to_number((unsigned long long)S->st_blocks, pcb);
  ref(D, off_record_data+14*wordsize) = ull_to_number((unsigned long long)S->st_blksize, pcb);
#else
  /* 64-bit platforms */
  ref(D, off_record_data+0*wordsize) = ull_to_number((unsigned long long)S->st_mode, pcb);
  ref(D, off_record_data+1*wordsize) = ull_to_number((unsigned long long)S->st_ino, pcb);
  ref(D, off_record_data+2*wordsize) = sll_to_number((long)S->st_dev, pcb);
  ref(D, off_record_data+3*wordsize) = ull_to_number((long)S->st_nlink, pcb);

  ref(D, off_record_data+4*wordsize) = ull_to_number((unsigned long long)S->st_uid, pcb);
  ref(D, off_record_data+5*wordsize) = ull_to_number((unsigned long long)S->st_gid, pcb);
  ref(D, off_record_data+6*wordsize) = ull_to_number((unsigned long long)S->st_size, pcb);

  ref(D, off_record_data+7*wordsize) = sll_to_number((long)S->st_atime, pcb);
#ifdef HAVE_STAT_ST_ATIME_USEC
  ref(D, off_record_data+8*wordsize) = ull_to_number((unsigned long long)S->st_atime_usec, pcb);
#else
  ref(D, off_record_data+8*wordsize) = false_object;
#endif

  ref(D, off_record_data+9*wordsize)  = sll_to_number((long)S->st_mtime, pcb);
#ifdef HAVE_STAT_ST_MTIME_USEC
  ref(D, off_record_data+10*wordsize) = ull_to_number((unsigned long long)S->st_mtime_usec, pcb);
#else
  ref(D, off_record_data+10*wordsize) = false_object;
#endif

  ref(D, off_record_data+11*wordsize) = sll_to_number((long)S->st_ctime, pcb);
#ifdef HAVE_STAT_ST_CTIME_USEC
  ref(D, off_record_data+12*wordsize) = ull_to_number((unsigned long long)S->st_ctime_usec, pcb);
#else
  ref(D, off_record_data+12*wordsize) = false_object;
#endif

  ref(D, off_record_data+13*wordsize) = ull_to_number((unsigned long long)S->st_blocks, pcb);
  ref(D, off_record_data+14*wordsize) = ull_to_number((unsigned long long)S->st_blksize, pcb);
#endif
  return 0;
}
static ikptr
posix_stat (ikptr filename_bv, ikptr stat_struct, int follow_symlinks, ikpcb* pcb)
{
  char *        filename;
  struct stat   S;
  int           rv;
  filename = (char*)(long)(filename_bv + off_bytevector_data);
  errno    = 0;
  if (follow_symlinks) {
    rv = stat(filename, &S);
  } else {
    rv = lstat(filename, &S);
  }
  if (0 == rv) {
    return fill_stat_struct(&S, stat_struct, pcb);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_stat (ikptr filename_bv, ikptr stat_struct, ikpcb* pcb)
{
  return posix_stat(filename_bv, stat_struct, 1, pcb);
}
ikptr
ikrt_posix_lstat (ikptr filename_bv, ikptr stat_struct, ikpcb* pcb)
{
  return posix_stat(filename_bv, stat_struct, 0, pcb);
}
ikptr
ikrt_posix_fstat (ikptr fd_fx, ikptr stat_struct, ikpcb* pcb)
{
  struct stat   S;
  int           rv;
  errno = 0;
  rv = fstat(unfix(fd_fx), &S);
  if (0 == rv) {
    return fill_stat_struct(&S, stat_struct, pcb);
  } else {
    return ik_errno_to_code();
  }
}

/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_stat (ikptr filename, ikptr follow /*, ikpcb* pcb */)
{
  char* fn = (char*)(long)(filename + off_bytevector_data);
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

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_file_size(ikptr filename, ikpcb* pcb) {
  char *        pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(filename + off_bytevector_data);
  errno    = 0;
  rv       = stat(pathname, &S);
  if (0 == rv) {
    if (sizeof(off_t) == sizeof(long)) {
      return u_to_number(S.st_size, pcb);
    } else if (sizeof(off_t) == sizeof(long long)) {
      return ull_to_number(S.st_size, pcb);
    } else {
      fprintf(stderr, "vicare internal error: invalid off_t size\n");
      exit(EXIT_FAILURE);
    }
  } else {
    return ik_errno_to_code();
  }
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_file_size(ikptr filename, ikpcb* pcb) {
  return ikrt_posix_file_size(filename, pcb);
}


/** --------------------------------------------------------------------
 ** File type predicates.
 ** ----------------------------------------------------------------- */

static ikptr
file_is_p (ikptr pathname_bv, ikptr follow_symlinks, int flag)
{
  char *        pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno    = 0;
  if (false_object == follow_symlinks) {
    rv = lstat(pathname, &S);
  } else {
    rv = stat(pathname, &S);
  }
  if (0 == rv) {
    /* NOTE It is not enough to do "S.st_mode & flag", we really have to
       do "flag == (S.st_mode & flag)". */
    return (flag == (S.st_mode & flag))? true_object : false_object;
  } else {
    return ik_errno_to_code();
  }
}

#define FILE_IS_P(WHO,FLAG)                                     \
  ikptr WHO (ikptr pathname_bv, ikptr follow_symlinks)          \
  { return file_is_p(pathname_bv, follow_symlinks, FLAG); }

FILE_IS_P(ikrt_file_is_directory,       S_IFDIR)
FILE_IS_P(ikrt_file_is_char_device,     S_IFCHR)
FILE_IS_P(ikrt_file_is_block_device,    S_IFBLK)
FILE_IS_P(ikrt_file_is_regular_file,    S_IFREG)
FILE_IS_P(ikrt_file_is_symbolic_link,   S_IFLNK)
FILE_IS_P(ikrt_file_is_socket,          S_IFSOCK)
FILE_IS_P(ikrt_file_is_fifo,            S_IFIFO)

#define SPECIAL_FILE_IS_P(WHO,PRED)                                     \
   ikptr                                                                \
   WHO (ikptr pathname_bv, ikptr follow_symlinks)                       \
   {                                                                    \
     char *        pathname;                                            \
     struct stat   S;                                                   \
     int           rv;                                                  \
     pathname = (char*)(long)(pathname_bv + off_bytevector_data);       \
     errno    = 0;                                                      \
     if (false_object == follow_symlinks) {                             \
       rv = lstat(pathname, &S);                                        \
     } else {                                                           \
       rv = stat(pathname, &S);                                         \
     }                                                                  \
     if (0 == rv) {                                                     \
       return (PRED(&S))? true_object : false_object;                   \
     } else {                                                           \
       return ik_errno_to_code();                                       \
     }                                                                  \
   }

SPECIAL_FILE_IS_P(ikrt_file_is_message_queue,S_TYPEISMQ)
SPECIAL_FILE_IS_P(ikrt_file_is_semaphore,S_TYPEISSEM)
SPECIAL_FILE_IS_P(ikrt_file_is_shared_memory,S_TYPEISSHM)


/** --------------------------------------------------------------------
 ** Testing file access.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_access (ikptr filename, ikptr how)
{
  char* pathname = (char*)(filename + off_bytevector_data);
  int   rv;
  errno = 0;
  rv    = access(pathname, unfix(how));
  if (0 == rv) {
    return true_object;
  } else if ((errno == EACCES) ||
             (errno == EROFS)  ||
             (errno == ETXTBSY)) {
    return false_object;
  } else {
    return ik_errno_to_code();
  }
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_access (ikptr filename, ikptr how)
{
  char* pathname = (char*)(filename + off_bytevector_data);
  int   r;
  int   ik_how;
  int   c_how;
  ik_how = unfix(how);
  if (ik_how == 0) {
    c_how = F_OK;
  } else {
    c_how = 0;
    if (ik_how & 1) c_how |= R_OK;
    if (ik_how & 2) c_how |= W_OK;
    if (ik_how & 4) c_how |= X_OK;
  }
  r = access(pathname, c_how);
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
ikrt_posix_file_exists (ikptr pathname_bv)
{
  char *        pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  errno    = 0;
  rv = stat(pathname, &S);
  if (0 == rv) {
    return true_object;
  } else if ((ENOENT == errno) || (ENOTDIR == errno)) {
    return false_object;
  } else {
    return ik_errno_to_code();
  }
}


/** --------------------------------------------------------------------
 ** File times.
 ** ----------------------------------------------------------------- */

static ikptr
timespec_vector (struct timespec * T, ikptr vector, ikpcb* pcb)
{
  ref(vector, off_vector_data+0*wordsize) = s_to_number((long)(T->tv_sec),  pcb);
  ref(vector, off_vector_data+1*wordsize) = s_to_number((long)(T->tv_nsec), pcb);
  return fix(0);
}
ikptr
ikrt_posix_file_ctime (ikptr pathname_bv, ikptr vector, ikpcb* pcb) {
  char*         pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno = 0;
  rv    = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_CTIMESPEC
    return timespec_vector(&S.st_ctimespec, vector, pcb);
#elif HAVE_STAT_ST_CTIM
    return timespec_vector(&S.st_ctim, vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_ctime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, vector, pcb);
#endif
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_posix_file_mtime (ikptr pathname_bv, ikptr vector, ikpcb* pcb) {
  char*         pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno = 0;
  rv    = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_MTIMESPEC
    return timespec_vector(&S.st_mtimespec, vector, pcb);
#elif HAVE_STAT_ST_MTIM
    return timespec_vector(&S.st_mtim, vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_mtime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, vector, pcb);
#endif
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_posix_file_atime (ikptr pathname_bv, ikptr vector, ikpcb* pcb) {
  char*         pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno = 0;
  rv    = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_ATIMESPEC
    return timespec_vector(&S.st_atimespec, vector, pcb);
#elif HAVE_STAT_ST_ATIM
    return timespec_vector(&S.st_atim, vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_atime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, vector, pcb);
#endif
  } else {
    return ik_errno_to_code();
  }
}


/* FIXME STALE To be removed at the next boot image rotation. */
static ikptr
timespec_bytevector (struct timespec* s, ikpcb* pcb) {
  int len = sizeof(struct timespec);
  ikptr r = ik_safe_alloc(pcb, align(disp_bytevector_data+len+3));
  ref(r, 0) = fix(len+2);
  *((char*)(r+disp_bytevector_data+0)) = sizeof(s->tv_sec);
  *((char*)(r+disp_bytevector_data+1)) = sizeof(s->tv_nsec);
  memcpy((char*)(r+disp_bytevector_data+2), s, len);
  *((char*)(r+disp_bytevector_data+len+2)) = 0;
  return r + bytevector_tag;
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_file_ctime2 (ikptr filename, ikpcb* pcb) {
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
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_file_mtime2 (ikptr filename, ikpcb* pcb) {
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


/** --------------------------------------------------------------------
 ** Symbolic links.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_realpath (ikptr pathname_bv, ikpcb* pcb)
{
  char *        pathname;
  char          buff[PATH_MAX];
  char *        rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  errno    = 0;
  rv       = realpath(pathname, buff);
  if (NULL == rv) {
    return ik_errno_to_code();
  } else {
    int         n = strlen(rv);
    uint8_t *   r = (uint8_t *)ik_safe_alloc(pcb, align(disp_bytevector_data+n+1));
    ref(r, 0) = fix(n);
    memcpy((r+disp_bytevector_data), rv, n+1);
    return (ikptr)(r+bytevector_tag);
  }
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_realpath (ikptr pathname_bv, ikpcb* pcb)
{
  return ikrt_posix_realpath(pathname_bv, pcb);
}


/** --------------------------------------------------------------------
 ** Setting onwers, permissions, times.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_chown (ikptr pathname_bv, ikptr owner_fx, ikptr group_fx)
{
  char *  pathname;
  int     rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  errno    = 0;
  rv       = chown(pathname, unfix(owner_fx), unfix(group_fx));
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_fchown (ikptr fd, ikptr owner_fx, ikptr group_fx)
{
  int     rv;
  errno    = 0;
  rv       = fchown(unfix(fd), unfix(owner_fx), unfix(group_fx));
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_chmod (ikptr pathname_bv, ikptr mode_fx)
{
  char *        pathname;
  int           rv;
  mode_t        mode;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  mode     = unfix(mode_fx);
  errno    = 0;
  rv       = chmod(pathname, mode);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
/* FIXME STALE To be removed at the next boot image rotation. */
ikptr
ikrt_chmod (ikptr path, ikptr mode)
{
  return ikrt_chmod(path, mode);
}
ikptr
ikrt_posix_fchmod (ikptr fd, ikptr mode_fx)
{
  int           rv;
  mode_t        mode;
  mode     = unfix(mode_fx);
  errno    = 0;
  rv       = fchmod(unfix(fd), mode);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_umask (ikptr mask_fx)
{
  mode_t  mask = unfix(mask_fx);
  mode_t  rv   = umask(mask);
  return fix((long) rv);
}
ikptr
ikrt_posix_getumask (void)
{
  mode_t  mask = umask(0);
  umask(mask);
  return fix((long) mask);
}
ikptr
ikrt_posix_utime (ikptr pathname_bv, ikptr atime_sec_fx, ikptr mtime_sec_fx)
{
  char *          pathname;
  struct utimbuf  T;
  int             rv;
  pathname  = (char*)(long)(pathname_bv+off_bytevector_data);
  T.actime  = unfix(atime_sec_fx);
  T.modtime = unfix(mtime_sec_fx);
  errno     = 0;
  rv        = utime(pathname, &T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_utimes (ikptr pathname_bv,
                   ikptr atime_sec_fx, ikptr atime_usec_fx,
                   ikptr mtime_sec_fx, ikptr mtime_usec_fx)
{
  char *          pathname;
  struct timeval  T[2];
  int             rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  T[0].tv_sec  = unfix(atime_sec_fx);
  T[0].tv_usec = unfix(atime_usec_fx);
  T[1].tv_sec  = unfix(mtime_sec_fx);
  T[1].tv_usec = unfix(mtime_usec_fx);
  errno        = 0;
  rv           = utimes(pathname, T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_lutimes (ikptr pathname_bv,
                    ikptr atime_sec_fx, ikptr atime_usec_fx,
                    ikptr mtime_sec_fx, ikptr mtime_usec_fx)
{
  char *          pathname;
  struct timeval  T[2];
  int             rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  T[0].tv_sec  = unfix(atime_sec_fx);
  T[0].tv_usec = unfix(atime_usec_fx);
  T[1].tv_sec  = unfix(mtime_sec_fx);
  T[1].tv_usec = unfix(mtime_usec_fx);
  errno        = 0;
  rv           = lutimes(pathname, T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_futimes (ikptr fd,
                    ikptr atime_sec_fx, ikptr atime_usec_fx,
                    ikptr mtime_sec_fx, ikptr mtime_usec_fx)
{
  struct timeval  T[2];
  int             rv;
  T[0].tv_sec  = unfix(atime_sec_fx);
  T[0].tv_usec = unfix(atime_usec_fx);
  T[1].tv_sec  = unfix(mtime_sec_fx);
  T[1].tv_usec = unfix(mtime_usec_fx);
  errno        = 0;
  rv           = futimes(unfix(fd), T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}


ikptr
ikrt_delete_file (ikptr pathname)
/* Interface to "unlink()". */
{
  assert(bytevector_tag == tagof(pathname));
  char* str = (char*)(long)(pathname + off_bytevector_data);
  int   err = unlink(str);
  if (0 == err)
    return true_object;
  else
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
ikrt_rename_file(ikptr src, ikptr dst /* ikpcb* pcb */){
  int err = rename((char*)(src + off_bytevector_data),
                   (char*)(dst + off_bytevector_data));
  if (err == 0) {
    return true_object;
  } else {
    return ik_errno_to_code();
  }
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



static int
execvpe_(const char *cmd, char *const *argv, char *const *envp){
  char *path = NULL;
  const char *searchpath;
  const char *sep;
  size_t cmd_len;

  if (cmd[0] == '/')
    execve(cmd, argv, envp);

  searchpath = getenv("PATH");
  if (searchpath == NULL)
    searchpath = "/bin:/usr/bin";

  cmd_len = strlen(cmd);

  sep = NULL;
  do {
    size_t prefix_len, path_len;

    sep = strchr(searchpath, ':');
    if (sep == NULL) {
      sep = searchpath + strlen(searchpath);
    }

    prefix_len = (sep - searchpath);
    path_len = prefix_len + cmd_len + 2;
    path = realloc(path, path_len);
    if (path == NULL) {
      errno = ENOMEM;
      return -1;
    }
    memcpy(path, searchpath, prefix_len);
    if (prefix_len == 0 || searchpath[prefix_len - 1] == '/') {
      memcpy(path + prefix_len, cmd, cmd_len + 1);
    } else {
      path[prefix_len] = '/';
      memcpy(path + prefix_len + 1, cmd, cmd_len + 1);
    }

    execve(path, argv, envp);
    switch (errno) {
    case E2BIG:
    case ENOEXEC:
    case ENOMEM:
    case ETXTBSY:
      break; /* these are treated as error, abort search */
    }

    searchpath = sep + 1;
  } while (sep[0] != '\0');

  if (path) free(path);

  return -1;
}

ikptr
ikrt_process(ikptr rvec, ikptr env, ikptr cmd, ikptr argv /*, ikpcb* pcb */){
  int infds[2];
  int outfds[2];
  int errfds[2];
  int search_p = ref(rvec, off_vector_data+0*wordsize) != false_object;
  int stdin_fd = unfix(ref(rvec, off_vector_data+1*wordsize));
  int stdout_fd = unfix(ref(rvec, off_vector_data+2*wordsize));
  int stderr_fd = unfix(ref(rvec, off_vector_data+3*wordsize));

  if(stdin_fd < 0 && pipe(infds))  return ik_errno_to_code();
  if(stdout_fd < 0 && pipe(outfds)) return ik_errno_to_code();
  if(stderr_fd < 0 && pipe(errfds)) return ik_errno_to_code();
  pid_t pid = fork();
  if(pid == 0){
    /* child */
    if (stdin_fd < 0){
      if(close(infds[1]))  exit(1);
      stdin_fd = infds[0];
    }
    if (stdout_fd < 0){
      if(close(outfds[0])) exit(1);
      stdout_fd = outfds[1];
    }
    if (stderr_fd < 0){
        if(close(errfds[0])) exit(1);
        stderr_fd = errfds[1];
    }
    if (stdin_fd != 0){
      if(close(0))             exit(1);
      if(dup(stdin_fd) == -1)  exit(1);
    }
    if (stdout_fd != 1){
      if(close(1))             exit(1);
      if(dup(stdout_fd) == -1) exit(1);
    }
    if (stderr_fd != 2){
      if(close(2))             exit(2);
      if(dup(stderr_fd) == -1) exit(1);
    }
    char *cmd_str = (char*)(long)(cmd+off_bytevector_data);
    char **env_strs = env == false_object ? 0 : ik_list_to_vec(env);
    char **argv_strs = ik_list_to_vec(argv);
    if (env_strs && search_p)
      execvpe_(cmd_str, argv_strs, env_strs);
    else if (env_strs)
      execve(cmd_str, argv_strs, env_strs);
    else if (search_p)
      execvp(cmd_str, argv_strs);
    else
      execv(cmd_str, argv_strs);
    fprintf(stderr, "failed to exec %s: %s\n",
        (char*)(long)(cmd+off_bytevector_data),
        strerror(errno));
    exit(EXIT_FAILURE);
  } else if(pid > 0){
    /* parent */
    ref(rvec,off_vector_data+0*wordsize) = fix(pid);

    if (stdin_fd < 0) {
      close(infds[0]); /* ignore errors */
      ref(rvec,off_vector_data+1*wordsize) = fix(infds[1]);
    }
    if (stdout_fd < 0) {
      close(outfds[1]);
      ref(rvec,off_vector_data+2*wordsize) = fix(outfds[0]);
    }
    if (stderr_fd < 0) {
      close(errfds[1]);
      ref(rvec,off_vector_data+3*wordsize) = fix(errfds[0]);
    }
    return rvec;
  } else {
    return ik_errno_to_code();
  }
}

/* end of file */
