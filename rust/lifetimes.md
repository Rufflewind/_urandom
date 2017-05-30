# Notes on lifetimes in Rust

A lifetime is a *contiguous span of time* during the execution of a program.  More rigorously, a lifetime is a [set of points on the control-flow graph (CFG)](http://smallcultfollowing.com/babysteps/blog/2016/05/04/non-lexical-lifetimes-based-on-liveness/) satisfying the contiguity requirement.

The contiguity requirement is important because a lot of existing Rust abstractions implicitly assume this requirement.  See the section on “Contiguity of lifetimes” for its definition and explanations of when it's needed.

## Special lifetimes

There are many possible lifetimes in any given program.  Among these there is a distinguished lifetime: `'static`.  Objects with `'static` lifetime are known to be valid for the entire duration of the program.

## Lifetime constraints

Lifetimes are constrained through the "outlives" constraint.

> We say `'a` **strictly outlives** `'b` if `'a` fully encloses `'b` and `'a` is not equal to `'b`.
>
> We say `'a` **outlives** `'b` if either `'a` strictly outlives `'b` or `'a` is equal to `'b`.  This is denoted:
>
>     'a: 'b

Intuitively, one can think of "outlives" as the `⊇` set operator (assuming the "set of CFG points" interpretation here) and "strictly outlives" as `⊃`.

When multiple constraints are combined (in the logical AND sense), they are separated by commas:

```rust
'a: 'b,   'a: 'c,   'd: 'e,   'e: 'f
```

There are some shorthands borrowed from Rust and mathematical notation:

```rust
'a: 'b + 'c     /* is a shorthand for */     'a: 'b,   'a: 'c

'd: 'e: 'f      /* is a shorthand for */     'd: 'e,   'e: 'f
```

We can also think of `'b + 'c` as a lifetime in of itself, as the **contiguous union** of `'b` and `'c` (i.e. if there is any gap between the two, the gap must be filled).

We can also define `'b * 'c`, the **intersection** of `'b` and `'c`.  Unlike unions, the contiguity of `'b` and `'c` alone is sufficient to ensure the contiguity of `'b * 'c`.

```rust
'a * 'b: 'c     /* is a shorthand for */     'a: 'c,   'b: 'c
```

It's interesting how there lifetimes are dual-purpose: they can serve as either concrete objects or as constraints.  That is, there is an isomorphism between the lifetimes themselves and lifetime constraints: a lifetime `'a` can always be reinterpreted as a constraint `: 'a` (must outlive `'a`).

Lifetimes can be universally quantified over:

```rust
'a: for<'b> 'b
```

This means `'a` must outlive every possible lifetime `'b`.  (As we'll see shortly, this implies `'a == 'static`.)

One can also existentially quantify over lifetimes:

```rust
'a: exist<'b> 'b
```

This means `'a` must outlive some lifetime `'b`.  Of course, this is a vacuous statement since `'a` always outlives itself.

## Hierachy of lifetimes

The hierarchy of lifetimes is symbolized by the relation:

```rust
for<'a>   'static: 'a
```

In other words:

  - `'static` lifetimes outlives all lifetimes (including itself).

Another way to write this is:

```rust
'static == for<'a> 'a
```

## Variance of lifetimes

Given a type constructor `F` with a single lifetime parameter:

  - We say `F` is **(co)variant** if `F<'a> -> F<'b> where 'a: 'b`.
  - We say `F` is **contravariant** if `F<'b> -> F<'a> where 'a: 'b`.

## Lifetime of a type

The lifetime of a type is the intersection of all its lifetime arguments.  We will denote this using `lifetimeof(T)`.

For example, the lifetime of the type `&'a fn(Box<Foo<'b> + 'c>` is the intersection of all three: `'a * 'b * 'c`.

One can specify outlives constraints on a lifetime of a type, such as:

```rust
lifetimeof(T): 'a
```

This is usually abbreviated as:

```rust
T: 'a
```

## Lifetime of a variable

Every variable is associated with a lifetime, which specifies how long the object will remain valid.  (This is *not to be confused* with any lifetimes that might be mentioned in its type.)

The lifetime of a `static` or `const` variable is always `'static`.  For other kinds of variables, their lifetimes are defined by their scopes.  We will write `lifetimeof(v)` to denote the lifetime of a variable `v`.

For example, consider the following code:

```rust
fn f(s: &'static str) {
    let x = 0;
    let y = &x;
    static PI = 3.14;
    println!("{} {} {} {}", s, x, y, PI);
}
```

To identify the lifetimes in this code, we must first make the scope of each variable explicit:

```rust
fn f(s: &'static str) /* 'F */ {
    /* 'X */ { let x = 0;
    /* 'Y */ { let y = &x;
    /* 'PI */ { static PI = 3.14;
    println!("{} {} {} {}", s, x, y, PI); } } }
}
```

  - `lifetimeof(s) == 'F` (it's *not* `'static`!)
  - `lifetimeof(x) == 'X`
  - `lifetimeof(y) == 'Y`
  - `lifetimeof(PI) == 'static`

### Well-formedness of variables

```rust
T: lifetimeof(r)  |-  WF(v: T)
```

A variable is well-formed only if the lifetime of its type outlives the lifetime of the variable.  This is why you can't store a reference in a variable that strictly outlives the reference.

## References

```rust
&'a ~m T
```

This denotes a reference to an object of type `T` with mutability `~m`, subject to the constraint that the reference is valid only during the lifetime `'a`.  Note that `'a` restricts the validity of the reference, not the original object `T`!

For more information on mutability, see the Mutability section.

References satisfy the following laws:

```rust
// well-formedness
T: 'a  |-  WF(&'a ~m T)

// type covariance for shared references
&'a T -> &'a U
  where T: U

// mutability covariance
&'a ~m T -> &'a T

// lifetime covariance
&'a ~m T -> &'b ~m T
  where 'a: 'b

// reborrowing
&'b ~m &'a ~m T -> &'b ~m T

// sharability
&'b ~m &'a T -> &'a T
```

### Well-formedness

```rust
T: 'a  |-  WF(&'a ~m T)
```

A reference is well-formed only if the original object outlives the duration of the borrow.

### Type covariance for shared references

```rust
&'a T -> &'a U
  where T: U
```

A shared reference can be downcast if the underlying type can be downcast.

This does not hold for mutable references, which are invariant with respect to the underlying type.

### Mutability covariance

```rust
&'a ~m T -> &'a T
```

A mutable reference can be downcast to a shared reference, but not the other way around.

### Lifetime covariance

```rust
&'a ~m T -> &'b ~m T
  where 'a: 'b
```

The lifetime of a reference can always be shrunk, but not extended.

### Reborrowing

```rust
&'b ~m &'a ~m T -> &'b ~m T
```

A reference can always be reborrowed for a shorter duration.

### Sharability

```rust
&'b ~m &'a T -> &'a T
```

Shared references can be cloned (duh).

## Contiguity of lifetimes

A lifetime `'a` is said to be contiguous if and only if, given any statements `A` and `C` included in `'a` and another statement `B` that happens after `A` and happens before `C`,  lifetime `'a`, then `B` is included in the lifetime `'a`.

What's wrong with non-contiguous lifetimes?

At first glance, code such as the one below look perfectly safe even with non-contiguous lifetimes.

```rust
let r1 = &mut x;
let r2 = &mut x;
*r1 += 1;
*r2 += 2;
```

But that's not true in general.   For example:

```rust
// this is really bad!
let r1 = &mut v;
let r2 = &mut v[0];
r1.push(...); // this will likely invalidate r2
*r2 = 0;
```

Gaps in the lifetime allow someone else to take exclusive control and then modify the object, thus invalidating associated references.

But not all references are invalidated.  The first example shows an example of that.  If you have a reference to some struct and extract a reference of one of its members, the lifetime *can* be non-contiguous as the reference cannot be invalidated without moving the struct.

This is not true for enums, or any pointer indirections (`Box`), however.  So non-contiguous lifetimes are probably very limited in usefulness.

## Mutability

The mutability qualifier `~m` can be either `shared` or `mut`:

```rust
~m == shared   ==>   &'a T
~m == mut      ==>   &'a mut T
```

We don't promote mutability to constraints directly, because mutability has two separate aspects:

  - `mut` indicates something is definitely under exclusive control
  - `shared` indicates something can be shared with others



Mutability has the following algebra:

```rust
shared + shared == shared
shared + mut == shared
mut + mut == mut
```

We could also promote mutability to constraints similar to lifetimes, but the algebra above leads to a trivial yet rather counterintuitive subtyping relation

```rust
shared: mut
```

where

  - a `shared` constraint indicates that the variable may be shared (akin to `Copy`).
  - a `mut` constraint is equivalent to no constraints at all

This would imply that references are *contravariant*, which is confusing.  Therefore, we do *not* interpret mutability in this way and simply *define* references to be covariant in mutability.

## The unsafe lifetime

One may be tempted to define an [`'unsafe` lifetime](https://github.com/rust-lang/rfcs/pull/1918/) in opposition to `'static`.  Objects with `'unsafe` lifetime are not known to be valid for any duration.  Hence, `'unsafe` lifetimes are outlived by all lifetimes (including itself).  `'unsafe`, when promoted as a constraint, is totally vacuous:

```rust
'unsafe == exists<'a> 'a == ()   // the vacuous constraint
```

There are problems with this approach, however.  One of the biggest issues is that it interacts poorly with the well-formed rules.  Consider any object `F<'unsafe>`.  If you want to use a variable of this type at all, it must satisfy `'unsafe: lifetimeof(variable)`, which is impossible.  Thus, functions can never manipulate objects where `'unsafe` appears.

Moreover, if an object contains `'unsafe`, is the lifetime of the type `'unsafe` or something else?  If we think of structs as bare, exposed tuples, then the answer would suggest `'unsafe`.  But after wrapping it inside a struct, the `'unsafe` is no longer visible, so is its lifetime `'unsafe` or unconstrained?  After all, the main motivation for `'unsafe` is to actually *disguise* lifetime parameters, which otherwise have a tendency to infect everything.

[[Unresolved]]
