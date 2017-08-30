# Lifetime guarantee

Consider the following function:

```rust
fn borrow<'a>(&'a T) -> U<'a>;
```

The caller is free to choose `'a` insofar as `'a` encloses the duration of the call.

At run time, the callee’s code must perform the same actions independent of what `'a` gets chosen because lifetimes are always erased.

Therefore, if this type signature is honest, i.e. does not impose any conditions beyond what can be read off from the type signature, then any property of `U<'a>` that holds for some chosen `'a` must also hold for *all* possible choices of `'a`.   That is to say, any property of `U<'a>` must hold for as long as `T` is never destroyed or mutably borrowed.  This is true no matter what `borrow` does.

You might try to cheat by using a `RefCell`, but it won’t work (within safe Rust, at least) because you can’t get `&'a T` from a `&'a RefCell<T>` (if you could, `RefCell`s would be horribly unsafe).

That being said, it *is* possible to obtain, e.g. a raw pointer or equivalent from `borrow`, which may not be dereferenceable for the duration `'a`, or even at all.  But this is fine because the dereferenceability of raw pointers is never assumed to begin with (safe functions must never assume a raw pointer is valid).  One may assert that validity (in the safe sense) is *not* equivalent to dereferenceability (raw pointers are always “valid” trivially, but may or may not be dereferenceable).

This as a consequence affects the possible semantics of `Deref`, `Borrow`, and `AsRef`.  Given an honest implementation, a pointer should always remain valid (and maintain whatever property it had) for the longest possible duration of `'a`.
