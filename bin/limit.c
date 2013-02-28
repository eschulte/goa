#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

int main(int argc, char *argv[]) {

  // resource/time limits
  struct rlimit limit;
  limit.rlim_cur = 720; limit.rlim_max = 720;
  setrlimit(RLIMIT_CPU, &limit);       // cpu seconds
  limit.rlim_cur = 1024; limit.rlim_max = 1024;
  setrlimit(RLIMIT_NPROC, &limit);     // number of spawned processes
  limit.rlim_cur = 1024; limit.rlim_max = 1024;
  setrlimit(RLIMIT_NOFILE, &limit);    // number of open files
  limit.rlim_cur = 17179607040; limit.rlim_max = 17179607040;
  setrlimit(RLIMIT_FSIZE, &limit);     // max file size (bytes)
  setrlimit(RLIMIT_MEMLOCK, &limit);   // max memory locked into RAM (bytes)
  setrlimit(RLIMIT_STACK, &limit);     // max stack size (bytes)
  alarm(1440);                         // wall clock seconds
                                       // need 600 when modeling power
                                       // otherwise only need 360

  // run
  execvp(argv[1], &argv[1]);

  // if this runs, then it is only because the call to execvp failed
  return 1;
}
