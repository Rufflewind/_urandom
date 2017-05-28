# Lifetime guarantee

Consider the following function:

```rust
fn borrow<'a>(&'a T) -> U<'a>;
```

The caller is free to choose `'a` insofar as `'a` encloses the duration of the call.

At run time, the callee’s code must perform the same actions independent of what `'a` gets chosen because lifetimes are always erased.

Therefore, `U<'a>` must remain valid for as long as the maximum duration of `'a`, even if `'a` is not what the compiler ends up picking!   That is, `U<'a>` is guaranteed remain valid as long as `T` is never destroyed or mutably borrowed.

This is true no matter what `borrow` does, as long as the type signature is “honest” (i.e. does not impose any requirements on how `U<'a>` has to be used beyond what is claimed by the type signature).

## Consequence

Consider the following example:

```rust
fn borrow<'a>(&'a Foo) -> Bar;

{
    let foo: Foo = ...
    let bar1 = borrow(&foo);
    let bar2 = borrow(&foo);
    // are bar1 and bar2 still valid here?
}
```

The answer is *yes* for both `bar1` and `bar2`.  (They need not be bitwise-equal, though.)

Even though the compiler has may choose short lifetimes for both `bar1` and `bar2`, both of them must remain valid between `{` and `}` because the compiler could just as well have chosen longer lifetimes for them!

You might try to cheat by using a `RefCell`, but it won’t work (within safe Rust, at least) because you can’t get `&'a T` from a `&'a RefCell<T>` (if you could, `RefCell`s would be horribly unsafe).
