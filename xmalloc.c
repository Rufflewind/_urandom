#include <limits.h>
#include <stdlib.h>
#include <stdio.h>

void *xmalloc(size_t size)
{
#ifdef ULLONG_MAX
    typedef unsigned long long uintmax;
    static const uintmax max = ULLONG_MAX;
    static const char *fmt = "%s (requested %s%llu B)\n";
#else
    typedef unsigned long uintmax;
    static const uintmax max = ULONG_MAX;
    static const char *fmt = "%s (requested %s%lu B)\n";
#endif
    void *ptr = malloc(size);
    if (size && !ptr) {
        const char *prefix = "";
        if ((size_t)(uintmax)size != size) {
            size = max;
            prefix = "more than ";
        }
        fprintf(stderr, fmt, "xmalloc: out of memory", prefix, (uintmax)size);
        fflush(stderr);
        abort();
    }
    return ptr;
}
