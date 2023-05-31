#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#undef errno
extern int errno;

char *__env[] = { "HOME=.", 0 };
char **environ = __env;

int _execve(const char *name, char * const *argv, char * const *env) {
  errno = ENOMEM;
  return -1;
}

int _fork(void) {
  errno = EAGAIN;
  return -1;
}

int _fstat(int file, struct stat *st) {
  st->st_mode = S_IFCHR;
  return 0;
}

int _getpid(void) {
  return 1;
}

int _isatty(int file) {
  return 1;
}

int _kill(int pid, int sig) {
  errno = EINVAL;
  return -1;
}

int _link(const char *old, const char *new) {
  errno = EMLINK;
  return -1;
}

void* _sbrk(int incr) {
  extern char _end;		/* Defined by the linker */
  static char *heap_end;
  char *prev_heap_end;
 
  if (heap_end == 0) {
    heap_end = &_end;
  }
  prev_heap_end = heap_end;
  /*if (heap_end + incr > stack_ptr) {
    write (1, "Heap and stack collision\n", 25);
    abort ();
  }*/

  heap_end += incr;
  return (void*) prev_heap_end;
}

int _times(struct tms *buf) {
  return -1;
}

int _unlink(const char *name) {
  errno = ENOENT;
  return -1; 
}

int _wait(int *status) {
  errno = ECHILD;
  return -1;
}

int mkdir(const char *path, mode_t mode) {
  errno = EACCES;
  return -1;
}

extern unsigned int epoch();
int _gettimeofday(struct timeval *tv, struct timezone *tz) {
    tv->tv_sec = epoch(&tv->tv_usec);
    return 0;
}
