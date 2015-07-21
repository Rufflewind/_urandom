#!/usr/bin/env python
# demo: integrator for Newtonian gravity
import numpy as np
import matplotlib.pyplot as plt

def create_figure(nx=1, ny=1):
    figure = plt.figure()
    axes = [figure.add_subplot(nx, ny, i + 1) for i in range(nx * ny)]
    return figure, axes

def force(r):
    return r / -np.dot(r, r)**1.5

def energy(r, v):
    return -1. / np.sqrt(np.dot(r, r)) + np.dot(v, v) / 2.

def forward_euler(r0, v0, dt, t_end, force, energy):
    txyEs = []
    t = 0
    r = np.array(r0)
    v = np.array(v0)
    while t < t_end:
        a = force(r)
        r += v * dt
        v += a * dt
        t += dt
        E = energy(r, v)
        txyEs.append([t, r[0], r[1], E])
    return np.array(txyEs).transpose()

def leapfrog(r0, v0, dt, t_end, force, energy):
    txyEs = []
    t = 0
    r = np.array(r0)
    v = np.array(v0)
    a = force(r)
    v += a * dt / 2
    while t < t_end:
        r += v * dt
        a = force(r)
        v += a * dt
        t += dt
        E = energy(r, v)
        txyEs.append([t, r[0], r[1], E])
    return np.array(txyEs).transpose()

args = {
    "r0": [1., 0.],
    "v0": [0., .75],
    "dt": .01,
    "t_end": 10.,
    "force": force,
    "energy": energy,
}
lts, lxs, lys, lEs = forward_euler(**args)
fts, fxs, fys, fEs = leapfrog(**args)
fig1, [ax1] = create_figure()
fig2, [ax2] = create_figure()
ax1.plot(fxs, fys, label="forward Euler")
ax1.plot(lxs, lys, label="leapfrog")
ax1.legend()
ax2.plot(fts, fEs, label="forward Euler")
ax2.plot(lts, lEs, label="leapfrog")
ax2.legend()
plt.show()
