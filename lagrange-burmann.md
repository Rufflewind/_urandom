Lagrange–Bürmann formula
========================

Given two analytic functions `f` and `g`, there exists a local expansion near
the zero:

    g(z) = ∑[n] c[n] f(z)^n

provided that `f(0) = 0` and `f′(0) ≠ 0`.

Derivation
----------

Let `C` be a contour that encloses the local domain of analyticity:

    g(z) = 1/(2ℼ ⅈ) ∮[C] (g(u) f′(u))/(f(u) − f(z)) ⅆu
         = 1/(2ℼ ⅈ) ∮[C] (g(u) f′(u))/f(u) 1/(1 − f(z) / f(u)) ⅆu
         = 1/(2ℼ ⅈ) ∮[C] (g(u) f′(u))/f(u) ∑[n = 0; ∞] (f(z)/f(u))^n ⅆu
         = ∑[n = 0; ∞] 1/(2ℼ ⅈ) (f(z))^n ∮[C] (g(u) f′(u))/f(u)^(n + 1) ⅆu
         = ∑[n = 0; ∞] f(z)^n/n!
             lim[u → 0] ⅆ^n/ⅆu^n (u^n g(u) f′(u))/f(u)^(n + 1)

Going back to the integral earlier, one can also use integration by parts to
obtain:

    g(z) = g(0)
         + ∑[n = 1; ∞] f(z)^n/n!
             lim[u → 0] ⅆ^(n − 1)/ⅆu^(n − 1) (u/f(u))^n g′(u)

(Note that the `n = 0` case requires some special handling.)

Generalizations
---------------

This can be generalized to the case around an arbitrary point `z0` with `f(z0)
≠ 0` and `g(z0) ≠ 0` using the substitution `f(z) ← f(z + z0) − f(z0)` and
`g(z) ← g(z + z0)`:

    g(z) = g(z0)
         + ∑[n = 1; ∞] (f(z) − f(z0))^n/n!
             lim[u → z0] ⅆ^(n − 1)/ⅆu^(n − 1) ((u − z0)/(f(u) − f(z0)))^n g′(u)

This formula can be used to invert functions by setting `g(z) ≡ z`:

    f^(−1)(w) = z0
              + ∑[n = 1; ∞] (w − f(z0))^n/n!
                  lim[z → z0] ⅆ^(n − 1)/ⅆz^(n − 1) ((z − z0)/(f(z) − f(z0)))^n
