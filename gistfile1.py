from fractions import Fraction

# generates Euler zigzag numbers (A000111) using the Seidel triangle.
def euler_zigzag(n):
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

# generates Bernoulli numbers (A027641 divided by A027642) using Euler zigzag numbers.
def bernoulli(n):
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

for i in range(10):
    print(list(euler_zigzag(i)))

for i in range(10):
    print(list(map(str, bernoulli(i))))
