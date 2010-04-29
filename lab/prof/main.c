
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct pcb{
  int counter;
} pcb;

extern void interrupt_mem(pcb*);
extern void interrupt_reg(int);

void usage(char* name){
  fprintf(stderr, "Usage: %s [mem|reg]\n", name);
  exit(-1);
}

int main(int argc, char** argv){
  int most_positive_number = 0x7FFFFFFF;
  if(argc != 2) usage(argv[0]);
  if(strcmp(argv[1], "reg") == 0){
    interrupt_reg(most_positive_number);
  } else if(strcmp(argv[1], "mem") == 0){
    pcb x;
    x.counter = most_positive_number;
    interrupt_mem(&x);
  } else {
    usage(argv[0]);
  }
  return 0;
}
