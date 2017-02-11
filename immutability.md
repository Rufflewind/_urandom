# What does immutability mean in Rust?

Informally, we can say an object is immutable if no operation can change its value.  We then say a type `&T` is immutable if this is true for all objects of type `&T`.

However, this is kind of hand-wavy.  What operations?  What is the meaning of “value”?  What does “all objects of type `&T`” really mean?

## Operations

Let's start with the first: operations.  Naturally, we want to exclude any unsafe operations because they can wreak havoc on encapsulation.  Moreover, it is usually more interesting to talk about it from the *user*'s perspective, so only *public* entities are permissible.  So if `T` is part of library `lib_t`, then we must consider the public interface `lib_t`, and probably the standard library too.

But what if `lib_t` uses another `lib_u` that allows modification of `T` only after adding `lib_u` as a dependency?  Perhaps, a more natural context is to consider all transitive dependencies of `lib_t`, in addition to `lib_t` itself.  For brevity, we'll call this `interface(T)`.

OK then, let's allow any safe operation from `interface(T)` and all compositions of such.  This is still a mouthful, so let's shorten that to “`Safe`”.

## Value

Next: the meaning of value.  This one is tough.  There are *many* possible meanings!  For example, if you have two slices where the contents are equal, but the pointers are not, are they equal?  Or you have two `Vec`s whose contents are equal, but their capacities are not?

It's tempting to consider `PartialEq` the *de facto* arbiter of equality, but is that always the useful choice?  If a program *does* depend on the pointer value of a slice, or the capacity of a `Vec`, then `PartialEq` would mislead us into thinking that something hadn't changed when it really did.

Let's try the same trick as the last section and consider `Safe`.  The “value” of an object is essentially the combined result (an infinite tuple) of *all* such operations: if any of them changes, then the “value” has changed.  Each operation acts as a *projection* function for a *component* of the object's value.

But then we find that this is *too* restrictive, because if I *didn't* care about the capacity of a vector, then this would introduce spurious “changes in value” that I didn't care about.

The better solution is treat the meaning of value as *context-dependent*.  Instead of asking “is `&T` immutable?” we can “is `&T` immutable *with respect to* [some operation(s)]”.  If you are OK with `PartialEq`, we can talk about immutability with respect to `==`.  If you want something more refined that takes into account the capacity of a vector, we can talk about immutability with respect to `==` and `.capacity()`.  It's your choice!

## Valid objects

Lastly, when we speak of “all objects of type `&T`” we don't really want to consider *invalid* objects.  So actually we want the object to be constructed using `Safe`, not through `std::mem::uninitialized()` or something.  Moreover, the `&T` might only exist temporarily, so rather than assuming a constructor `g` of the form:

    let t = g();

we would rather assume the more general form:

    g(|t| { ... });

## Full definition

Now that we have all the pieces together, we can define immutability:

> A type `&T` is said to be (partially) **immutable** with respect to the `(n + 1)`-ary *pure* operation `f` if, for all `g` constructed using only the safe interface, if the following expression `e(f, g)` is valid, then it is possible for the result to be `true`.
>
> ~~~rust
> // e(f, g)
> g(|t: &T, x, h| { let y = t.f(x…); h(); y }) == g(|t: &T, x, h| { h(); t.f(x…) })
> ~~~
>
> Here, `t.f(x…)` is a shorthand for `t.f(x.0, …, x.(n - 1))`.

The “is possible” is to handle situations where the result could be nondeterministic.  In that case, false positives can occur, but that’s fine since there are always deterministic functions to cover us.

We can moreover state that a type `&T` is (fully) **immutable** if `&T` is immutable with respect to all safe, *pure* operations on `&T` (including compositions of such).  In this case, we could say `T` is “inorganic”, since if it uses any `Cell`-like type it would likely fail this test.

Note: I tried using a definition with arbitrary `t: T`, but that would mean that `T = &mut i32` would be considered “immutable” since the reference is exclusive and therefore changes made to its underlying value cannot be observed by `h()`.  The stricter definition proposed here avoids that problem.

## Definition of purity

We can use the same concept to define *purity*.  Loosely speaking, a function is *pure* if its execution has no observable effect.

> A `n`-ary function `f` is said to be **pure** if, for every `g` constructed using only the safe interface, if the following expression `e(f, g)` is valid, then it is possible for the result to be `true`.
>
> ~~~rust
> // e(f, g)
> g(|x, h| { let y = f(x…); h(); y }) == g(|x, h| { h(); f(x…) })
> ~~~
>
> Here, `f(x…)` is a shorthand for `f(x.0, …, x.(n - 1))`.
>
> Formally, we can define:
>
>     pure(f) ≡ ∀ g ∈ Safe . valid(e(f, g)) ⇒ ◇ (eval(e(f, g)) == true)

One weakness with this definition is that it is possible for `g` to reliably observe the presence of a pure `f` through, e.g. a CPU counter, on a single CPU computer.  OTOH, one could argue that the compiler is free to re-order *f* and *g*.
