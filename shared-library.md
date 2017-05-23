Suppose you have a semantically versioned library `foo-1.2.3`.

Make sure all object files are compiled with `-fPIC`.

## Linux (and ELF platforms)

    # linking
    cc -o libfoo.so.1.2.3 -shared -soname=libfoo.so.1 ...
    ln -fs libfoo.so.1.2.3 libfoo.so

## Mac OS X / Mach

    # linking
    cc -o libfoo.1.dylib -dynamiclib -current_version 2.3 -compatibility_version 2.0 ...

## Windows (MinGW)

    # linking
    cc -o foo.dll -shared -Wl,--out-implib,foo.lib
