#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt

TH = 2 * np.pi / 5.                     # 5-fold symmetry

def gamm(c):
    # simplification of |r1 - rot^3 r0|
    return 2 * np.sin(1.5 * TH) - c * np.cos(1.5 * TH)

c = 0.2 * (np.tan(np.pi / 5.) * 2)      # distance AB
rot = np.array([                        # rotation matrix for TH
    [np.cos(TH), -np.sin(TH)],
    [np.sin(TH), np.cos(TH)],
])

# list of vertices
r = []
r.append([1, -c / 2.])
r.append([1, c / 2.])
for i in range(10):
    r.append(rot.dot(r[-2]))

# rescale
s = np.array(r) * 34 / gamm(c)

fig, ax = plt.subplots()

# plot pentagon and print XY
ax.plot(*s.transpose())
# XY/AF = (1 - (1, 0) . rot^2 r1) / |r1 - rot^3 r0|
print(s[0][0] - s[5][0], s[0][0] - s[5][0])
for i in range(5):
    # plot chords and print their lengths (should be 34)
    ax.plot(*np.array([s[i], s[5 + i]]).transpose())
    print(np.linalg.norm(s[i] - s[5 + i]))

from numpy import cos, sin, pi
c = np.linspace(0.0, 1.0, 200)
fig, ax = plt.subplots()
ax.plot(c, (1 - cos(4*pi/5) + c/2 * sin(4*pi/5)) /
         (2 *sin(3*pi/5) - c* cos(3*pi/5)))

plt.show()
