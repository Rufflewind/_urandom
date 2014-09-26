# Mittag-Leffler expansion

The [Mittag-Leffler theorem][thm] is *non-constructive* as it does not provide
a direct way to compute the expansion, so here is a more specialized version
of the theorem that produces a direct expansion for certain kinds of
meromorphic functions.

> Let `Γ[n]` be an infinite sequence of contours, each fully embedded in the
> next and all of them enclosing zero.  Let `s[n]` denote the length of
> contour `Γ[n]`, and let `d[n]` denote the maximum distance of any point on
> the contour `Γ[n]` from zero.  If all the following conditions are
> satisfied:
>
>   - `∃ C ∈ ℝ . ∀ n ∈ ℕ .             |s[n] / d[n]| < C`;
>   - `∃ M ∈ ℝ . ∀ n ∈ ℕ; z ∈ Γ[n] .   |f(z)| ≤ M`;
>   - `f` is a meromorphic function that is not singular at zero;
>
> then:
>
>     `f(z) = f(0) + ∑[w ∈ Poles[f]] (1 / (z − w) + 1 / w) Res[w] f

> where the summation is performed over all poles, starting at the ones
> nearest to the origin.  The order important as the series may diverge if
> summed in the wrong order.

Note: If the function is singular at zero, one can either shift the origin or
turn the singularity into a removable singular by adding another meromorphic
function.

## Proof

Let

    g(z)(u) = (z f(u))/(u (w − z))
    Γ[n] = ∂ Ω[n]

and consider the following integral,

    1/(2 ℼ ⅈ) ∮[Γ[n]] g(u) ⅆ u
      = Res[0] g(z) + Res[z] g(z) + ∑[w ∈ Poles[f]] Res[w] g(z)
      = −f(0) + f(z) + ∑[w ∈ Poles[f] ∩ Ω[n]] z/(w (w - z)) Res[w] f

Note that,

    |1/(2 ℼ ⅈ) ∮[Γ[n]] g(u) ⅆ u|
      ≤ |z|/(2 ℼ) ∮[Γ[n] (|f(u)| |ⅆ u|)/(|u| |u - z|)
      ≤ (|z| M)/(2 ℼ) ∮[Γ[n] (|f(u)| |ⅆ u|)/(|u| |u - z|)
      ≤ (|z| M)/(2 ℼ) s[n]/(d[n] (d[n] - R[n]))
      ≤ (|z| C M)/(2 ℼ) 1/(d[n] - R[n])

where `R[n]` is the largest possible value of `z` within `Ω`.  Thus, in the
limit

    lim[n → ∞] |1/(2 ℼ ⅈ) ∮[Γ[n]] g(u) ⅆ u|
      ≤ (|z| C M)/(2 ℼ) lim[n → ∞] 1/(d[n] - R)
      = 0

[thm]: https://en.wikipedia.org/wiki/Mittag-Leffler%27s_theorem
