#include <stdio.h>
#include <stdlib.h>
#include <QLibrary>

int main(void)
{
    typedef void (*testtype)(void);
    typedef void (*seedtype)(int);

#ifdef _WIN32
#define NAME "rndmod.dll"
#else
#define NAME "./librndmod.so"
#endif
    printf("If the last 2 rows are identical, then the test is working correctly.\n");
    printf("If you see 5 identical rows then the srand() seed is SHARED.\n");
    printf("Otherwise, the srand() seed is LOCAL to each library module.\n");
    printf("\n");
    {
        QLibrary lib(NAME);
        lib.setLoadHints(QLibrary::ExportExternalSymbolsHint);
        lib.load();
        testtype test = (testtype)lib.resolve("test");

        srand(5);
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        printf("\n");

        srand(5);
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        printf("\n");

        srand(5);
        (*test)();
        (*test)();
        (*test)();
    }
    {
        QLibrary lib(NAME);
        lib.setLoadHints(QLibrary::ExportExternalSymbolsHint);
        lib.load();
        testtype test = (testtype)lib.resolve("test");
        seedtype seed = (seedtype)lib.resolve("seed");

        (*test)();
        (*test)();
        printf("\n");

        (*seed)(5);
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        printf("\n");

        (*seed)(5);
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        (*test)();
        printf("\n");
    }
    return 0;
}
