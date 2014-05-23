#!/usr/bin/env python
from fractions import Fraction

def bernoulli(n):
    '''
    Returns an iterator that generates the first `n` Bernoulli numbers
    (sequence A027641 divided by sequence A027642) as `Fraction`s.  The
    convention used here sets the second Bernoulli number to -1/2.

    The sequence is computed using Euler zigzag numbers.
    '''
    if n <= 0:
        return
    yield Fraction(1, 1)
    if n == 1:
        return
    yield Fraction(-1, 2)
    seq = enumerate(euler_zigzag(n - 1))
    next(seq)
    for i, e in seq:
        j = i + 1
        if j % 2 == 1:
            yield 0
        else:
            yield Fraction(e * j * (-1) ** (j // 2), 2 ** j - 4 ** j)

def euler_zigzag(n):
    '''
    Returns an iterator that generates the first `n` Euler zigzag numbers
    (sequence A000111, also known as up/down numbers) as `int`s.

    The sequence is computed using the Seidal triangle method.
    '''
    if n <= 0:
        return
    center = (n - 3) // 2 + 1
    row = [0] * n
    row[center] = 1
    for i in range(-1, n - 1):
        if i % 2 == 1:
            begin = center - i // 2
            end = begin + i + 1
            for j in range(begin, end):
                row[j] += row[j - 1]
            yield row[end - 1]
        else:
            begin = center + i // 2
            end = begin - i - 1
            for j in range(begin, end, -1):
                row[j] += row[j + 1]
            yield row[end + 1]

# example usage
if __name__ == "__main__":
    for i in range(10):
        print(list(euler_zigzag(i)))
    for i in range(10):
        print(list(map(str, bernoulli(i))))
