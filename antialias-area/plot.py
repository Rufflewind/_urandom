import matplotlib.cm
import matplotlib.pyplot as plt
import numpy as np
import scipy.optimize

class FakeAxis:
    def __init__(self):
        self.plots = []

    def plot(self, *args, **kwargs):
        self.plots.append((args, kwargs))

def plot_fit(ax, dist, xs, ys, label, color, f, fit_f=None,
             p0=None, p=None, expr="{p}"):
    if p is None:
        p, _ = scipy.optimize.curve_fit(fit_f or f, xs, ys, p0=p0)
    print(f"""
{label}:
  parameters = {expr.format(*p, p=p)}
  95% confidence interval = ±{2 * np.std(ys - f(xs, *p)):.4}
  max error = ±{np.max(abs(ys - f(xs, *p))):.4}
""")
    if fit_f:
        ax.plot(dist, fit_f(dist, *p), linestyle="dashed", color=color)
    ax.plot(dist, f(dist, *p), color=color, label=label)
    return p

def piecewise(f, sharpness):
    def fit_f(x, k, *args):
        return 1.0 + (-1.0 + f(x, k, *args)) / (1 + np.exp(sharpness * (x - k)))
    def pf(x, k, *args):
        return 1.0 + (-1.0 + f(x, k, *args)) * (x < k)
    return fit_f, pf

def exact(s, theta):
    theta = np.abs(theta) % np.pi
    if theta >= np.pi / 2:
        theta = np.pi - theta
    if theta >= np.pi / 4:
        theta = np.pi / 2 - theta
    s1 = 0.5 * (np.cos(theta) - np.sin(abs(theta)))
    s2 = 0.5 * (np.cos(theta) + np.sin(abs(theta)))
    return np.piecewise(s, [
        s < -s2,
        (-s2 <= s) & (s < -s1),
        (-s1 <= s) & (s < s1),
        (s1 <= s) & (s < s2),
        s2 <= s,
    ], [
        lambda s: 0.0,
        lambda s: (s2 + s)**2 / np.sin(2 * theta),
        lambda s: 0.5 + s / np.cos(theta),
        lambda s: 1.0 - (s2 - s)**2 / np.sin(2 * theta),
        lambda s: 1.0,
    ])

dist = np.linspace(0, .5**.5, 800)
fig, ax = plt.subplots()

subtract_avg = True
if subtract_avg:
    real_ax = ax
    ax = FakeAxis()

xs = []
ys = []
full_ys = []
for i in np.linspace(0.0, 1.0, 180):
    theta = i * np.pi / 4.0
    s2 = 0.5 * (np.cos(theta) + np.sin(abs(theta)))
    isubdist = dist < s2
    subdist = dist[isubdist]
    y = exact(subdist, theta)
    full_y = np.piecewise(dist, [isubdist], [y, np.nan])
    ax.plot(dist,
            full_y,
            color=matplotlib.cm.YlGn(0.3 + i * 0.2),
            linewidth=0.5)
    xs.append(subdist)
    ys.append(y)
    full_ys.append(full_y)
xs = np.concatenate(xs)
ys = np.concatenate(ys)
full_ys = np.array(full_ys)
avg = np.nanmean(full_ys, axis=0)

ax.plot(dist, avg, color="#2b6b13", label="quasi-exact average", linestyle=":")
delta = full_ys - avg
delta = delta[~np.isnan(delta)]
print(f"""
quasi-exact average:
  95% confidence interval = ±{2 * np.std(delta):.4}
  max error = ±{np.max(abs(delta)):.4}
""")

def cubic(x, a, b):
    return 0.5 + a * x + b * x ** 3
plot_fit(ax, dist, xs, ys, "full cubic", "#b853a6", cubic)

def f(x, k, a, b):
    return 0.5 + a * x + b * x ** 3
fit_f, f = piecewise(f, 2000)
plot_fit(ax, dist, xs, ys, "piecewise cubic", "#4261b7",
         f, p0=[0.5, 1.0, -1.0], fit_f=fit_f)

def f(x, k):
    u = x / (2.0 * k) + 0.5
    return 3 * u**2 - 2 * u**3
fit_f, f = piecewise(f, 2000)
plot_fit(ax, dist, xs, ys, "smoothstep", "#2a2c28", f, fit_f=fit_f)

if subtract_avg:
    for args, kwargs in ax.plots:
        x, y, *args = args
        real_ax.plot(x, y - avg, *args, **kwargs)
    ax = real_ax
    ax.set_ylabel("area(dist) − average")
else:
    ax.set_ylabel("area(dist)")

ax.legend()
ax.set_xlabel("dist")
plt.show()
