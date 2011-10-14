#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>

extern void inf_loop();
void register_handlers();
void register_alt_stack();

int main(int argc, char** argv){
  fprintf(stderr, "Entering ... \n");
  register_handlers();
  register_alt_stack();
  inf_loop();
  fprintf(stderr, "Done\n");
  return 0;
}

#if 0
     #include <signal.h>

     struct  sigaction {
             union {
                     void    (*__sa_handler)(int);
                     void    (*__sa_sigaction)(int, struct __siginfo *, void *);
             } __sigaction_u;                /* signal handler */
             int     sa_flags;               /* see signal options below */
             sigset_t sa_mask;               /* signal mask to apply */
     };

     #define sa_handler      __sigaction_u.__sa_handler
     #define sa_sigaction    __sigaction_u.__sa_sigaction

     int
     sigaction(int sig, const struct sigaction * restrict act,
         struct sigaction * restrict oact);
#endif

void handler(int signo, struct __siginfo* info, ucontext_t* uap){
  fprintf(stderr, "Handler Called!\n");
}

void
register_handlers(){
  struct sigaction sa;
  sa.sa_sigaction = (void(*)(int,struct __siginfo*,void*)) handler;
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sa.sa_mask = 0;
  int err = sigaction(SIGINT, &sa, 0);
  if(err){
    fprintf(stderr, "Sigaction Failed: %s\n", strerror(errno));
    exit(-1);
  }
}


#if 0
SYNOPSIS
     #include <sys/types.h>
     #include <signal.h>

     struct sigaltstack {
             char   *ss_sp;
             int     ss_size;
             int     ss_flags;
     };

     int
     sigaltstack(const struct sigaltstack *ss, struct sigaltstack *oss);
#endif

void
register_alt_stack(){
  char* stk = malloc(SIGSTKSZ);
  if(stk == 0){
    fprintf(stderr, "Cannot maloc an alt stack\n");
    exit(-1);
  }

  struct sigaltstack sa;
  sa.ss_sp = stk;
  sa.ss_size = SIGSTKSZ;
  sa.ss_flags = 0;
  int err = sigaltstack(&sa, 0);
  if(err){
    fprintf(stderr, "Cannot set alt stack: %s\n", strerror(errno));
    exit(-1);
  }
  fprintf(stderr, "alt-stack of size %d set\n", SIGSTKSZ);
}
