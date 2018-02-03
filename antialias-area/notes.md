# Area of a square cut by a line

[Rendered](https://rufflewind.com/_urandom/antialias-area)

## Definition

Suppose we have a 1 × 1 square and a line that cuts across the square.  The line is angled $\theta$ with respect to one of the sides of the square, and is at a distance $s$ from the center of the square.  We want to calculate the area $A(s, \theta)$ enclosed by the square and one side of the line.  We choose $A(s, \theta) > \frac{1}{2}$ if $s > 0$.

## Formula

For any $|\theta| \le \frac{\pi}{4}$,
$$A(s, \theta) = \frac{1}{2} + \operatorname{sgn} s \cdot \begin{cases}
   |s| \sec\theta & \text{if } |s| < s_-(\theta) \\
   \frac{1}{2} - (s_+(\theta) - |s|)^2 \csc|2 \theta| & \text{if } s_-(\theta) \le |s| < s_+(\theta) \\
   \frac{1}{2} & \text{if } s_+(\theta) \le |s| \\
\end{cases}$$
where
$$s_\pm(\theta) = \frac{\cos\theta \pm \sin|\theta|}{2}$$
The formula for other values of $\theta$ may be deduced from symmetries of the square.

## Derivation

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
