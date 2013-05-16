#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

int main(int argc, char *argv[]) {
  // run with no resource or time limits
  return execvp(argv[1], &argv[1]);
}
