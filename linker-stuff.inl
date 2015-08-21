/* make sure to compile with -fvisibility=hidden on gcc;
   modify the name of "BUILD_SHARED" (and of "EXPORT" if needed)
   to ensure uniqueness */
#undef EXPORT
#if defined _WIN32 || defined __CYGWIN__
# ifdef BUILD_SHARED
#  define EXPORT __declspec(dllexport)
# else
#  define EXPORT __declspec(dllimport)
# endif
#else
# if __GNUC__ >= 4
#  define EXPORT __attribute__ ((visibility ("default")))
# else
#  define EXPORT
# endif
#endif
/* ... */
#undef EXPORT
