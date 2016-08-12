#include <stdlib.h>
#include <stdio.h>

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif

EXPORT void seed(int i)
{
    srand(i);
}

EXPORT void test(void)
{
    printf("%i ", rand());
}

#ifdef __cplusplus
}
#endif
