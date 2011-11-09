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
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifdef __CYGWIN__
#include "ikarus-winmmap.h"
#endif


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
 ** Operative system environment variables.
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
ikrt_delete_file (ikptr filename)
/* Interface to "unlink()". */
{
  assert(bytevector_tag == tagof(filename));
  char* str = (char*)(long)(filename + off_bytevector_data);
  int   err = unlink(str);
  if (0 == err)
    return true_object;
  else
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
      exit(EXIT_FAILURE);
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
