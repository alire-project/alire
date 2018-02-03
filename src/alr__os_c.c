#include <unistd.h>

void own_exec (size_t buflen, char *buffer, size_t *len) {
    *len = readlink ("/proc/self/exe", buffer, buflen);
}
