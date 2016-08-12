#include <stdlib.h>
#include <stdio.h>

#ifdef _WIN32
__declspec(dllexport)
#endif
void test(void)
{
    printf("%i ", rand());
}
