#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

int main(int argc, char *argv[]) {

  // resource/time limits
  struct rlimit limit;
  limit.rlim_cur = 8; limit.rlim_max = 8;
  setrlimit(RLIMIT_CPU, &limit);       // cpu seconds
  limit.rlim_cur = 16384; limit.rlim_max = 16384;
  setrlimit(RLIMIT_NPROC, &limit);     // number of spawned processes
  limit.rlim_cur = 16384; limit.rlim_max = 16384;
  setrlimit(RLIMIT_NOFILE, &limit);    // number of open files
  limit.rlim_cur = 17179607040; limit.rlim_max = 17179607040;
  setrlimit(RLIMIT_FSIZE, &limit);     // max file size (bytes)
  setrlimit(RLIMIT_MEMLOCK, &limit);   // max memory locked into RAM (bytes)

  alarm(12);                           // wall clock seconds
                                       // scaled back w/o graphite

  // run
  return execvp(argv[1], &argv[1]);
}
