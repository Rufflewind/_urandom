#include <stdio.h>
#include <stdlib.h>
#include <QLibrary>

int main(void)
{
    typedef void (*testtype)(void);

#ifdef _WIN32
#define NAME "rndmod.dll"
#else
#define NAME "./librndmod.so"
#endif
    printf("If you see 5 identical rows of numbers then the srand() seed is SHARED.\n");
    printf("Otherwise, the srand() seed is LOCAL to each library module.\n");
    {
        QLibrary lib(NAME);
        lib.setLoadHints(QLibrary::ExportExternalSymbolsHint);
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
    }
    {
        QLibrary lib(NAME);
        lib.setLoadHints(QLibrary::ExportExternalSymbolsHint);
        testtype test = (testtype)lib.resolve("test");

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
    }
    return 0;
}
