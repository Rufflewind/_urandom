#define _POSIX_C_SOURCE 199309L


#ifndef G_5FP4P4EX8CFF5QNJGX7WJ5L9QH6QS
#define G_5FP4P4EX8CFF5QNJGX7WJ5L9QH6QS
#if _POSIX_C_SOURCE < 199309L
# error Define _POSIX_C_SOURCE to at least 199309L before including this header
#endif
#if defined _WIN32
union _LARGE_INTEGER;
#elif defined __MACH__
# include <mach/mach_time.h>
#else
# include <time.h>
# include <sys/resource.h>
#endif
#include <math.h>
#ifndef NAN
# define NAN (0./0.)
#endif
#ifdef __cplusplus
extern "C" {
#endif
/** @file

    Functions for accessing a monotonic wall clock.

    Note: on systems other than Windows or Mac OS X, it is strongly
    recommended to define `_POSIX_C_SOURCE` to a value of at least `199309L`
    before the inclusion of this header or any system header.

*/

typedef struct { double _data; } wclock;

/** Initialize a value needed to access the monotonic wall clock.  The clock
    does not need to be deinitialized.

    @param clock
    An existing `wclock` value to be initialized.

    @return
    Zero on success, nonzero on failure.
*/
static
int wclock_init(wclock *clock);

/** Retrieve the time from a monotonic wall clock in seconds.

    @param clock
    An `wclock` value previously initialized by `init_wclock`.

    @return
    Duration relative to some unspecified reference time in seconds.
    If an error occurs, `NAN` is returned.

    Due to the use of double-precision floating point numbers, the precision
    is at worst (for an ideal system with hundreds of years in uptime) limited
    to about a few microseconds.  In practice, it is usually much less.
*/
static
double wclock_get(const wclock *clock);

int wclock_init(wclock *self)
{
#if defined _WIN32
    int __stdcall QueryPerformanceFrequency(union _LARGE_INTEGER *);
    __int64 freq;
    if (!QueryPerformanceFrequency((union _LARGE_INTEGER *)&freq)) {
        return 1;
    }
    self->_data = 1. / (double)freq;
#elif defined __MACH__
    mach_timebase_info_data_t base;
    if (mach_timebase_info(&base)) {
        return 1;
    }
    self->_data = 1e-9 * base.numer / base.denom;
#else
    (void)self;
#endif
    return 0;
}


double wclock_get(const wclock *self)
{
#if defined _WIN32
    int __stdcall QueryPerformanceCounter(union _LARGE_INTEGER *);
    __int64 count;
    if (!QueryPerformanceCounter((union _LARGE_INTEGER *)&count)) {
        return NAN;
    }
    return (double)count * self->_data;
#elif defined __MACH__
    return mach_absolute_time() * self->_data;
#else
    struct timespec t;
    (void)self;
    if (clock_gettime(CLOCK_MONOTONIC, &t)) {
        return NAN;
    }
    return (double)t.tv_sec + (double)t.tv_nsec * 1e-9;
#endif
}

#ifdef __cplusplus
}
#endif
#endif


#include <stdio.h>
#include <vector>
#include <unistd.h>
#include <mpi.h>

wclock clk;

const bool cancel = false;

void bench(int src) {
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Request req;
    std::vector<double> timings;
    MPI_Barrier(MPI_COMM_WORLD);
    if (rank == 0) {
        char buf[10000] = {0};
        fprintf(stderr, "# sleeping...\n");
        usleep(1000);
        fprintf(stderr, "# sending...\n");
        MPI_Isend(buf, sizeof(buf), MPI_CHAR, size / 2, 0, MPI_COMM_WORLD, &req);
        fprintf(stderr, "# sent.\n");
        if (cancel) {
            MPI_Cancel(&req);
        }
        MPI_Wait(&req, MPI_STATUS_IGNORE);
        fprintf(stderr, "# done.\n");
    } else if (rank == size / 2) {
        int flag = 0;
        MPI_Message msg;
        MPI_Status stat;
        while (flag == 0) {
            double start = wclock_get(&clk);
            MPI_Improbe(src, 0, MPI_COMM_WORLD, &flag, &msg, &stat);
            timings.push_back(wclock_get(&clk) - start);
        }
        char buf[10000];
        fprintf(stderr, "# recving.\n");
        MPI_Imrecv(buf, sizeof(buf), MPI_CHAR, &msg, &req);
        fprintf(stderr, "# recved.\n");
        if (cancel) {
            MPI_Cancel(&req);
        }
        MPI_Wait(&req, MPI_STATUS_IGNORE);
        for (auto t : timings) {
           printf("%.9f ", t);
        }
        printf("\n");
    }
}

int main(void) {
    MPI_Init(NULL, NULL);
    wclock_init(&clk);
    bench(0);
    bench(MPI_ANY_SOURCE);
    bench(0);
    bench(MPI_ANY_SOURCE);
    bench(0);
    bench(MPI_ANY_SOURCE);
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
}
