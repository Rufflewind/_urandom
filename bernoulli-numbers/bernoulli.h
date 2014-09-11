#ifndef KVBBDRCQRWSNNMYUWPQXSZJMZFWPLGYMEPLZRIST
#define KVBBDRCQRWSNNMYUWPQXSZJMZFWPLGYMEPLZRIST
/* Calculates Bernoulli numbers using Euler zigzag (up/down) numbers via the
   Seidel algorithm. */

/* By Rufflewind.  No rights reserved (public domain / CC0).
   https://github.com/Rufflewind/_urandom/tree/master/bernoulli-numbers */

#ifdef __cplusplus
extern "C" {
#endif

/**
    Returns an iterator that generates the first `count` Bernoulli numbers
    (sequence A027641 divided by sequence A027642).  The convention used here
    sets the second Bernoulli number to -1/2.

    @param[in] count
      The number of elements to be generated.

    @return
      An iterator, or `NULL` if an error occurs.
 */
void *bernoulli_create(unsigned count);

/**
    Destroys the iterator.

    @param[in] iter
      An iterator, which may be `NULL`.
 */
void bernoulli_destroy(void *iter);

/**
    Returns the next number in the sequence.  The variable pointed to by `out`
    is modified only if the operation succeeds.

    @param[in]  iter
      An iterator.  Must not be `NULL`.

    @param[out] out
      Pointer to a variable to which the output is stored.  If the pointer is
      `NULL`, then the output is ignored.

    @return
      `0`  if the number has been successfully generated.
      `1`  if there are no more numbers in the sequence.
      `22` if there are any invalid arguments.
 */
int bernoulli_next(void *iter, double *out);

/**
    Returns an iterator that generates the first `count` Euler zigzag numbers
    (sequence A000111, also known as up/down numbers).

    @param[in] count
      The number of elements to be generated.

    @return
      An iterator, or `NULL` if an error occurs.
 */
void *euler_zigzag_create(unsigned count);

/**
    Destroys the iterator.

    @param[in] iter
      An iterator, which may be `NULL`.
 */
void euler_zigzag_destroy(void *iter);

/**
    Returns the next number in the sequence.  The variable pointed to by `out`
    is modified only if the operation succeeds.

    @param[in]  iter
      An iterator.  Must not be `NULL`.

    @param[out] out
      Pointer to a variable to which the output is stored.  If the pointer is
      `NULL`, then the output is ignored.

    @return
      `0`  if the number has been successfully generated.
      `1`  if there are no more numbers in the sequence.
      `22` if there are any invalid arguments.
 */
int euler_zigzag_next(void *iter, double *out);

#ifdef __cplusplus
}
#endif
#endif
