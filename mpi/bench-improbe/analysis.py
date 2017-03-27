import numpy as np
import matplotlib.pyplot as plt

dd = []
for i, line in enumerate(open("timings.txt", "rb")):
    dd.append(np.loadtxt([line]))
for i, d in enumerate(dd):
    plt.semilogy(range(len(d)), d, ".",
                 label=("0" if i % 2 == 0 else "MPI_ANY_SOURCE"),
                 color=("#4186f4" if i % 2 == 0 else "#f44242"))
plt.legend()
plt.show()
