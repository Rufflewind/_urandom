Lagrange–Bürmann formula
========================

Given two analytic functions `f` and `g`, there exists a local expansion near the zero:

$$g(z) = \sum_n c_n (f(z))^n$$

provided that `f(0) = 0` and `f′(0) ≠ 0`.

---

Let `C` be a contour that encloses the local domain of analyticity:

$$
\begin{align}
g(z)
&= \frac{1}{2 \pi i} \oint\limits_{C} \frac{g(\xi) f'(\xi)}{f(\xi) - f(z)} d \xi \\
&= \frac{1}{2 \pi i} \oint\limits_{C} \frac{g(\xi) f'(\xi)}{f(\xi)} \frac{1}{1 - f(z) / f(\xi)} d \xi \\
&= \frac{1}{2 \pi i} \oint\limits_{C} \frac{g(\xi) f'(\xi)}{f(\xi)} \sum_{n = 0}^\infty \left(\frac{f(z)}{f(\xi)}\right)^n d \xi \\
&= \sum_{n = 0}^\infty \frac{1}{2 \pi i} (f(z))^n \oint\limits_{C} \frac{g(\xi) f'(\xi)}{(f(\xi))^{n + 1}} d \xi \\
&= \sum_{n = 0}^\infty (f(z))^n \frac{1}{n!} \lim_{\xi \to 0} \frac{d^n}{d \xi^n} \frac{g(\xi) f'(\xi)}{(f(\xi))^{n + 1}} \xi^n
\end{align}
$$

Going back to the integral earlier, one can also use integration by parts to obtain:

$$g(z) = g(0) + \sum_{n = 1}^\infty \frac{(f(z))^n}{n!} \lim_{\xi \to 0} \frac{d^{n - 1}}{d \xi^{n - 1}} \left(\frac{\xi}{f(\xi)}\right)^n g'(\xi)$$

(Note that the `n = 0` case requires some special handling.)

This can be generalized to the case around an arbitrary point `z0` with `f(z0) ≠ 0` and `g(z0) ≠ 0` using the substitution `f(z) ← f(z + z0) - f(z0)` and `g(z) ← g(z + z0)`:

$$g(z) = g(z_0) + \sum_{n = 1}^\infty \frac{(f(z) - f(z_0))^n}{n!} \lim_{\xi \to z_0} \frac{d^{n - 1}}{d \xi^{n - 1}} \left(\frac{\xi - z_0}{f(\xi) - f(z_0)}\right)^n g'(\xi)$$

This formula can be used to invert functions by setting `g(z) ≡ z`:

$$f^{-1}(w) = z_0 + \sum_{n = 1}^\infty \frac{(w - f(z_0))^n}{n!} \lim_{z \to z_0} \frac{d^{n - 1}}{d z^{n - 1}} \left(\frac{z - z_0}{f(z) - f(z_0)}\right)^n$$
