# Mittag-Leffler expansion

The [Mittag-Leffler theorem][thm] is *non-constructive* as it does not provide
a direct way to compute the expansion, so here is a more specialized version
of the theorem that produces a direct expansion for certain kinds of
meromorphic functions.

[thm]: https://en.wikipedia.org/wiki/Mittag-Leffler%27s_theorem

> Let `Γ[n]` be an infinite sequence of contours, each fully embedded in the
> next and all of them enclosing zero.  Let `s[n]` denote the length of
> contour `Γ[n]`, and let `d[n]` denote the maximum distance of any point on
> the contour `Γ[n]` from zero.  If all the following conditions are
> satisfied:
>
>   - `∃ C ∈ ℝ . ∀ n ∈ ℕ .             |s[n] / d[n]| < C`;
>   - `∃ M ∈ ℝ . ∀ n ∈ ℕ, z ∈ Γ[n] .   |f(z)| ≤ M`;
>   - `f` is a meromorphic function that is not singular at zero;
>
> then:
>
>     `f(z) = f(0) + ∑[w ∈ Poles f] (1 / (z − w) + 1 / w) Res[w] f
>
> where the summation is performed over all poles, starting at the ones
> nearest to the origin.  The order important as the series may diverge if
> summed in the wrong order.  The expansion is valid only within the union of
> all contours.

*Note:* If the function is singular at zero, one can either shift the origin or
turn the singularity into a removable singular by adding another meromorphic
function.

*Remark:* The second condition can be weakened to

  - `∃ M ∈ ℝ, ∀ p ∈ ℕ . ∀ n ∈ ℕ, z ∈ Γ[n] . |f(z)| ≤ M z^p`;

## Proof

Let

    g(z)(u) = (z f(u))/(u (w − z))
    Γ[n] = ∂ Ω[n]

and consider the following integral,

    1/(2 ℼ ⅈ) ∮[Γ[n]] g(u) ⅆ u
      = Res[0] g(z) + Res[z] g(z) + ∑[w ∈ Poles f] Res[w] g(z)
      = −f(0) + f(z) + ∑[w ∈ Poles f ∩ Ω[n]] z/(w (w - z)) Res[w] f

Note that,

    |1/(2 ℼ ⅈ) ∮[Γ[n]] g(u) ⅆ u|
      ≤ |z|/(2 ℼ) ∮[Γ[n] (|f(u)| |ⅆ u|)/(|u| |u - z|)
      ≤ (|z| M)/(2 ℼ) ∮[Γ[n] (|f(u)| |ⅆ u|)/(|u| |u - z|)
      ≤ (|z| M)/(2 ℼ) s[n]/(d[n] (d[n] - R))
      ≤ (|z| C M)/(2 ℼ) 1/(d[n] - R)

where `|z| ≤ R`.  Thus, in the limit

    lim[n → ∞] |1/(2 ℼ ⅈ) ∮[Γ[n]] g(u) ⅆ u|
      ≤ (|z| C M)/(2 ℼ) lim[n → ∞] 1/(d[n] - R)
      = 0

as long as `z` lies within the radius `R`.

## Logarithmic version (infinite product expansion)

If `F(z) = f′(z)/f(z)` is a meromorphic function satisfying the same
conditions as before, then:

    f(z) = f(0) ⅇ^(f′(0) z / f(0)) ∏[w ∈ Roots f] (1 - z/w) ⅇ^(z / w)

The proof is a simple application of the previous expansion to `F(z)`,
followed by integration and exponentiation.  The residues are computed using
the [argument principle](https://en.wikipedia.org/wiki/Argument_principle).

## Weierstrass factorization

Let `f` be an entire function with infinite number of roots at locations given
by the sequence `w`.  If `w[0] = 0` is a root of multiplicity `r` and `w[n]`
grows to infinity as `n` tends to infinity.  Then,

    f(z) = z^r ⅇ^(h(z)) ∏[n = 1; ∞] (1 - z/w[n])
                                    ⅇ^(∑[k = 1; K[n]] 1/k (z/w[n])^k)

for some entire function `h` and some sequence of positive integers `K`.  The
sequence `K` can be chosen arbitrarily as long as the series

    ∑[n = 1; ∞] (z/w[n])^(K[n] + 1)

is uniformly convergent on every compact subset of `ℂ`.
