# The meaning of safety in Rust

## Definitions

A **component** is a "building block" in Rust, which could be a function, type, trait, `impl`, `mod`, etc.  A component is **primitive** if it is provided by Rust as part of its language or through the standard library.  This set is denoted `Prim`.

A collection of fundamental rules (syntax, types, lifetimes, orphan rules, etc) govern the use of components in Rust.  They also describe how one can build new components.  Given a set of components `C`, the set of *all* components that could be built from `C` is called the **closure** of `C` and is denoted `C*`.  This is also a *closure operator* in the abstract sense.

From a set of components, one can also build executable **programs**.  The set of all programs that one can build using a set of components `C` is denoted `programs(C)`.  Every program requires **inputs** from its environment to run, which includes not only deterministic inputs such as standard input, files, mouse input, etc., but also non-deterministic inputs such as random seeds, hardware noise, thread scheduling, etc.  We expect inputs to reside within some prescribed tolerance (e.g. smashing a computer with a sledgehammer does not count as the kind of input we care about).

Since building programs may involve building new components, we expect that for any set of components `C`, we have `programs(C*) = programs(C)`.  After all, components are just incomplete programs.

Some components are **safe**, which means any use of the component does not require the `unsafe` keyword.  Others are **unsafe**, which means any use of the component would necessitate the `unsafe` keyword at least once.

We denote the set of primitive, *safe* components of Rust by `Safe`, and the set of primitive, *unsafe* components of Rust by `Unsafe`, which is the complement of `Safe` within `Prim`.

We say a program `p` is **sound** if there exists no input for which `p` has undefined behavior.  This is denoted `sound(p)`.  Otherwise, `p` is said to be **unsound**, denoted `¬sound(p)`.

We say a set of components `C` is **sound** if every program built from `C` is sound.  That is to say, `sound(C) ≡ ∀[p ∈ programs(C)] sound(p)`.  Otherwise, `c` is **unsound**, denoted `¬sound(C)`.

## The safe Rust axiom

This is a fundamental assumption of safe Rust:

> All programs built from the safe components of Rust are free of undefined behavior.
>
>     ⊢ sound(Safe)

In practice, the axiom may not be true, and I don't think anyone has yet proved it.  But it is a primary goal of Rust.  We will assume this axiom from here on.

As soon as an unsafe component is used, soundness is no longer guaranteed.  That is to say, it is possible that `∃ u ∈ Unsafe. ¬sound(Safe ∪ {u})`.  In particular, it desirable but not necessary to expect `∀ u ∈ Unsafe. ¬Sound(Safe ∪ {u})`, which *locally* maximizes the number of components in the `Safe` subset.

## Extending the boundary of safety

To a first approximation, we can think of a module as a set of components `M`.  The module can be built using only safe components, in which case the module must be sound.  In other words:

    (M ⊆ Safe*) → sound(M)

However, what is more interesting are modules that also use unsafe components.  Is such a module unsound?  Not necessarily so.  It is preferable to have a module that *is* sound even if it uses unsafe components.

Although in general `sound(Safe ∪ Unsafe)` does not hold, it is still possible to maintain soundness if we are careful in the design of `M`.  That is, there may exist some **invariants** `I` such that, as long as the invariants are upheld by the module, then any code that uses the module is sound.

    ∃ I . ∀ M ⊆ Prim* . I(M) → sound(Safe ∪ M)

Here, `I(M)` means `M` upholds the invariants `I`.  The invariants are essentially a possibly more restrictive version of `sound(Safe ∪ M)`.

Note that if `M ⊆ Safe*`, we have a trivial situation where invariants are not required: `I(M) = ⊤`.

Nonetheless, it is often convenient to have *some* components that do *not* uphold the invariants `I`.  Fortunately, this need not impact the module user's safety: one can either mark these components as either unsafe or *private*.

Hence, a module is not simply a set of components.  Rather it is more like a triplet `(M.Safe, M.Unsafe, M.Priv)` where `M.Safe` are the public safe components, `M.Unsafe` are the public unsafe components, and `M.Priv` are the private safe or unsafe components.  The user is only able to interact with `M.Safe` and `M.Unsafe`, but not `M.Priv`.

Thus, from the module user's perspective, they see:

  - `Safe` and `Unsafe` primitives
  - `M.Safe` and `M.Unsafe` from the module

The goal of the module designer is to ensure that `Safe ∪ M.Safe` always remains sound.

## Invariant encapsulation

However, the picture becomes more complicated when multiple unsafe-using modules get involved: `M1`, `M2`, `M3`, ….  In that case, one has to consider the possibility that different modules may have different invariants `I1`, `I2`, `I3`, ….  It certainly seems possible that the `I2` could be violated `M3`!

Fortunately, in practice it is not usually that bad.  Invariants are usually associated with the components themselves, thus there is an unambiguous *home* for each invariant.  Users are expected to obey the documented invariants on the unsafe components they use.

Invariants that have *global* ramifications are usually forbidden (e.g. mandating that all `i64` must be positive, or that all raw pointers must be dereferenceable).  In particular, it would be absurd to have an invariant that even the `Safe` primitives fail to satisfy.  Note that traits are considered *local* invariants, since they only affect types for which the trait is implemented.

That is not to say there *aren't* global invariants.  There are many, in fact, but I'm not aware of any comprehensive list.  Some are plain obvious facts: "don't mess with the private members of a foreign type", "don't initialize a foreign type with zeros", "don't duplicate a foreign type that doesn't implement Copy using memcpy".  These "obvious" rules are like a kind of "etiquette" that enables invariants to be *modular*.

There are also invariants that come from the primitives themselves (e.g. all mutable references must be unique, all references must be valid), which are effectively global since the primitives are pervasive.

## Beyond soundness

Although we have restricted ourselves to soundness only in the context of undefined behavior, it is totally possible to *strengthen* the notion to that of **semantic soundness**, which ensures that no unexpected behavior (panics, hangs, garbage output, etc.) occurs, in some context-dependent definition of "unexpected".  The analysis would be similar in nature, but the `safe`/`unsafe` boundary would no longer play a significant role.
