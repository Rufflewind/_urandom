# Area of a square cut by a line

[Rendered](https://rufflewind.com/_urandom/antialias-area)

## Definition

Suppose we have a 1 × 1 square and a line that cuts across the square.  The line is angled $\theta$ with respect to one of the sides of the square, and is at a distance $s$ from the center of the square.

 1. Calculate the area $A(s, \theta)$ enclosed by the square and one side of the line.  We choose $A(s, \theta) > \frac{1}{2}$ if $s > 0$.
 2. Calculate the area $\bar{A}(s)$ averaged over all angles $\theta$ at which the line touches the square.

## Formula

For any $|\theta| \le \frac{\pi}{4}$, the area is given by
$$A(s, \theta) = \frac{1}{2} + F(|s|, |\theta|) \operatorname{sgn} s$$
where
$$\begin{aligned}
F(s, \theta) &= \begin{cases}
   s \sec\theta & \text{if } 0 \le s < S_-(\theta) \\
   \frac{1}{2} - (S_+(\theta) - s)^2 \csc(2 \theta) & \text{if } S_-(\theta) \le s < S_+(\theta) \\
   \frac{1}{2} & \text{if } s \ge S_+(\theta) \\
\end{cases} \\
S_\pm(\theta) &= \frac{\cos\theta \pm \sin|\theta|}{2}
\end{aligned}$$
The formula for other values of $\theta$ may be deduced from symmetries of the square.

The average area over all angles at which the line touches the square is given by
$$\bar{A}(s) = \frac{1}{2} + \bar{F}(|s|) \operatorname{sgn} s$$
where
$$\begin{aligned}
\bar{F}(s) &= \begin{cases}
   G(s, T(s))  & \text{if } 0 \le s < \frac{1}{2} \\
   H(s, -T(s)) & \text{if } \frac{1}{2} \le s < \frac{1}{\sqrt{2}} \\
   \frac{1}{2} & \text{if } s \ge \frac{1}{\sqrt{2}} \\
\end{cases} \\
G(s, t) &= \frac{4}{\pi} \left(s \ln\frac{1 + t}{1 - t} + I(s, t)\right) \\
H(s, t) &= \frac{I(s, t)}{\Phi(t)} \\
I(s, t) &= \frac{1}{4} \left(\Phi(t)
    + 2 s \ln\frac{1 - t}{t + t^2}
    - \left(\frac{1}{2} + 2 s^2\right) \ln\frac{1 - t^2}{2 t}
  \right) \\
\Phi(t) &= \frac{\pi}{4} - 2 \arctan t \\
T(s) &= \frac{\sqrt{2 - 4 s^2} - 1}{1 + 2 s}
\end{aligned}$$

## Derivation

### Area for a specific angle

To derive this, we first establish a coordinate system.  Define a square of area 4 by the vertices (1, 1), (−1, 1), (−1, −1), and (1, −1).  This means we have scaled up our problem by a factor of two and would be calculating $4 A(2 s, \theta)$ instead.

Define a line by the equation $y = m (x - a)$.  The parameters $(a, m)$ are related to $(2 s, \theta)$ by
$$m = \tan\theta \qquad \text{and} \qquad a = 2 s \csc\theta$$

It is adequate to consider just the following case and derive others from symmetry:
$$a \ge 0 \qquad \text{and} \qquad 1 \le m < \infty$$
Equivalently:
$$s \ge 0 \qquad \text{and} \qquad \frac{\pi}{4} \le \theta < \frac{\pi}{2}$$

Firstly, we compute the maximum value of $a$ at which the line touch the square:
$$a \le a_+ = 1 + \frac{1}{m} \qquad \Leftrightarrow \qquad s \le s_+ = \frac{1}{2}(\sin\theta + \cos\theta)$$

Below that, there are two interesting cases to consider, depending on whether $a$ is lesser or greater than this threshold:
$$a_- = 1 - \frac{1}{m} \qquad \Leftrightarrow \qquad s_- = \frac{1}{2}(\sin\theta - \cos\theta)$$

In the first case where $0 \le a \le a_-$, the area is given by
$$4 A(s, \theta) = 2 + 2 a = 2 + 4 s \csc\theta$$

In the second case where $a_- \le a \le a_+$, the area is given by
$$4 A(s, \theta) = 4 - \frac{1}{2} \left(1 - a + \frac{1}{m}\right) (1 + m (1 - a)) = 4 - 4 (s_+ - s)^2 \csc(2\theta)$$
where a double-angle formula has been used.

Then, using the symmetries of the square and of trigonometric functions, we can remap $\theta$ into the range $|\theta| < \frac{\pi}{4}$.  This turns $\csc\theta$ into $\sec\theta$, $\sin\theta$ into $\cos\theta$, and $\cos\theta$ into $\sin\theta$.

### Averaged area over all allowed angles

To find the average area, it suffices to consider only $0 \le \theta \le \frac{\pi}{4}$.  The other 8 sectors are identical by symmetry.  As before, we will only analyze the case where $s \ge 0$, hence $A(s, \theta) \ge \frac{1}{2}$.

We need to first determine the range of angles at which the line touches the square.

  - If $0 \le s \le \frac{1}{2}$, then the line will certainly make contact with the square for any $0 \le \theta \le \frac{\pi}{4}$.
  - If $\frac{1}{2} < s \le \frac{1}{\sqrt{2}}$, then the line will touch the square only if
    $$0 < -2 \arctan T(s) \le \theta < \pi / 4$$
    where
    $$T(s) = \frac{\sqrt{2 - 4 s^2} - 1}{1 + 2 s}$$
    Note that this quantity satisfies $S_\pm(\mp 2 \arctan T(s)) = s$.
  - If $s > \frac{1}{\sqrt{2}}$, the line will never touch the square.  For convenience, will simply define the averaged area to be 1.

Let us denote the lower bound of $\theta$ by
$$\Theta(s) = \max\{0, -2 \arctan T(s)\}$$

To find the average, we need to compute the integral:
$$\bar{A}(s) = \frac{\int_{\Theta(s)}^{\pi / 4} A(s, \theta) \, \mathrm{d} \theta}{\pi / 4 - \Theta(s)}$$
Due to the piecewise nature of $A(s, \theta)$, it is necessary to split the integral into several pieces:
$$\bar{A}(s) = \begin{cases}
  \frac{4}{\pi} \left(\int_0^{2 \arctan T(s)} A(s, \theta) \, \mathrm{d} \theta + \int_{2 \arctan T(s)}^{\pi / 4} A(s, \theta) \, \mathrm{d} \theta\right) & \text{if } 0 \le s < \frac{1}{2} \\
  \left(\frac{\pi}{4} + 2 \arctan T(s)\right)^{-1} \int_{-2 \arctan T(s)}^{\pi / 4} A(s, \theta) \, \mathrm{d} \theta & \text{if } \frac{1}{2} \le s < \frac{1}{\sqrt{2}} \\
\end{cases}$$

The rest of the calculation is straightforward but very tedious.  Use of a computer algebra system is recommended.
