Curves
======

Curves are a fundamental ingredient in line integrals.  Here we consider a
specific class of curves that are *piecewise smooth*, i.e. infinitely
differentiable except at a finite set of points.


```agda
data Curve s where
  Curve : List (ℝ, s, Interval 0 1 -> s) -> Curve s
```

It consists of a finite list of *curve segments*, each of which is smooth
piece originating at a well-defined point in the target space `s` and has a
well-defined length.  Hence, curves are all compact.  The curve may have
multiple representations in this form, however (because you can concatenate
two adjacent smooth curves to form another smooth curve.  This can be done
recursively to obtain a canonical representation where curves can no longer be
concatenated.

Curves form a group:

```agda
mempty : Curve s
mempty = Curve []

_<>_   : Curve s -> Curve s -> Curve s

invert : Curve s -> Curve s
```

The zero is defined to be the empty curve, while the group operation is
concatenation.  Inversion flips the direction of the curve.
