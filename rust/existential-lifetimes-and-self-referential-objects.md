# Existential lifetimes and self-referential objects

This is a somewhat rambly post about existentials and how they relate to self-referential data types.  Lots of speculative (and possibly wrong) ideas − you have been warned!

## What are existentials?

If you're familiar with higher-rank trait bounds (HRTB),

```rust
for<'a> fn(&'a str) -> i32
```

you'll recognize this as a function that takes a reference whose lifetime is fixed by the caller.  This is a universally quantified type: the user ("caller") is free to choose whatever lifetime `'a` it wants and the creator of this function ("callee") is at their mercy.

Existentially quantified types offer the opposite kind of choice: you specify the lifetime at the point of creation and when you use it you are at the mercy of the creator.  Symbolically, we will write:

```rust
exists<'a> &'a str
```

to denote existential types.  This can be read as "There exists a reference to a string with some lifetime, but we don't know what that lifetime is."

We can introduce a syntax for *creating* such objects:

```rust
let e: exists<'a> &'a str = exists { "Hello world" }
```

We can also introduce a syntax for *using* such objects:

```rust
match e {    // (you can instead use 'let')
    exists { s } => {
        println!("Hello world {}", s);
    }
}
```

The use of existential objects has one crucial subtlety: by unpacking `e` into `exists { s }`, you create a `&'a str` where `'a` is confined to this local scope.  It's very similar to how the magical built-in address-of (`&`) operator works!

This idea, at first, might seem rather problematic.  If we somehow have a reference of unknown lifetime, we could unknowingly smuggle the reference well past its expiration date!

Fortunately, we can introduce rules in the compiler to prevent that kind of crime.  To figure out what these rules should be, let's start by modelling existential types using types that we already have in Rust.

## Lazy existentials

That's right, existential types *already* exist in Rust!  This comes from a fairly well-known theorem from logic that:

```rust
exists<'a> F<'a> <=> for<R> FnOnce(for<'a> FnOnce(F<'a>) -> R) -> R
```

The right side of this equation encodes describes precisely how an existential type can be used.  In this encoded form, creation would look more like:

```rust
let e = move |f| { f("Hello world") }
// this would work had Rust supported polymorphic lambdas
```

Usage would then become:

```rust
e(|s| {
    println!("Hello world {}", s);
})
```

In any case, the point of this exercise is not to suggest that this is even remotely a sensible way to define existential types in Rust.  In fact, it's terrible because:

  - Rust doesn't have polymorphic lambdas, so defining them is rather annoying.
  - Rust doesn't support `for<R>` quantification (`R` being a type).
  - Lambdas/closures aren't types but traits, which means `exists<'a> F<'a>` is not just one type, but a whole family of them.  In practice, you probably want to box them for sanity reasons.  We'll continue to pretend they are types for the rest of this post.

But it does give us an idea of the rules that we should stick to when handling existential types in an ownership-oriented type system.  The first thing that you will note is that the creation of an existential involves a lambda.  Suppose I try to naively store a reference of a local variable inside an existential:

```rust
fn steal_ref1() -> exists<'a> &'a i32 {
    let i: i32 = 42;
    let r: &i32 = &i;
    move |f| { f(r) }
}
```

This doesn't work though!  This would require stashing `r: &i32` inside the environment of the closure, but that makes the closure non-`'static`, which the borrowchecker would complain because the thing we are returning has to be static because there are no exposed lifetimes, saving us from a potential crime scene.

If you find the `exists` too confusing, you could instead emulate this using `impl Trait`:

```rust
fn steal_ref2() -> impl FnOnce(for<'a> fn(&'a i32) -> R) -> R + 'static {
    let i: i32 = 42;
    let r: &i32 = &i;
    move |f| { f(r) }
}
```

Here I didn't bother to write `for<R>` because I can't and it's not really relevant here (I also used `fn` instead of `FnOnce` for the inner function for simplicity).  In any case, this does not type check – feel free to test it yourself.

But there *is* a way to make this type check, with one minor change:

```rust
fn take_ref1() -> impl FnOnce(for<'a> fn(&'a i32) -> R) -> R + 'static {
    let i: i32 = 42;
    move |f| { f(&i) }
}
```

This time, what gets stored in the environment is not a reference, but the integer value itself.  [It compiles!](https://play.rust-lang.org/?gist=cc9c1ad39f9197dc8162ec7fdf31e881&version=nightly)

If we translate this back into `exists`:

```rust
fn take_ref2() -> exists<'a> &'a i32 {
    let i: i32 = 42;
    exists { f(&i) }
}
```

Should we expect this to work?  The answer unfortunately depends on the how we want to treat `exists { f(&i) }`:

  - Lazy approach: we can pretend it's a lambda and defer execution until it's used.
  - Eager approach: we simply store `&i` directly inside the existential object.

If we do it lazily, we already have the tools to do that.  Here's a proper way to do it using Rust today:

```rust
struct ExistsRefI32(i32);

impl ExistsRefI32 {
    fn with<F, R>(self, f: F) -> R where F: for<'a> FnOnce(&'a i32) -> R {
        f(&self.0)
    }
}

fn take_ref3() -> ExistsRefI32 {
    let i: i32 = 42;
    ExistsRefI32(i)
}
```

This avoids the syntactic sugar of lambdas (due to lack of polymorphic lambdas), but it's effectively the same thing.  The `with` function is equivalent to matching (using) an existential, or calling the our fake closure.

Point is, why bother with existential types if they don't offer anything new to the table?

## Eager existentials

Eager existentials are really just a subset of lazy existentials, but performance-wise eager existentials are much more predictable.  Whereas lazy existentials could hoard all sorts of baggage in their closure environments, eager existentials are limited to precisely what they claim to carry.

Revisiting the `exists<'a> &'a i32` example, whereas a lazy existential could store *either* a static reference to `i32` or an `i32` value or perhaps something even more exotic, eager existentials can only store `&'a i32`.

Therefore, the memory representation of eager existentials are just dumb structs, without any of that fancy function stuff!  Let's introduce a more concrete notation for eager existentials by piggybacking on the existing `struct` syntax:

```rust
struct ExistsStr exists<'a> {
    inner: &'a str,
}
```

The rules for creating such an object are fairly straightforward.  To create such an object:

```rust
let e3: ExistsRefI32 = ExistsRefI32 { inner: "Hello world" }
```

you must ensure at the point of creation (before you forget what `'a` is) that everything within the existential struct outlives the struct itself.  In this case it's OK because `'a = 'static` and that outlives everything.

At the point of use:

```rust
match e3 {
    ExistsRefI32 { inner: s } => {
        println!("{}", s);
    }
}
```

The interior of the existential struct is given a fresh lifetime `'a` that lives for the scope of the `match` clause (in the cause of `let`, until the end of the current scope).  The variance of the existential lifetime `'a` depends on the variance of its contents.

Objects inside the existential struct will naturally contain some outlives constraints.  For example:

```rust
struct ExistsT<T> exists<'a> {
    inner: &'a T,
}
```

This implies `T: 'a`, but you can't just plop this constraint directly on the struct because the `'a` would escape.  Instead, this constraint should be *scoped* within the `exists`, roughly like this:

```rust
struct ExistsT<T> exists<'a where T: 'a> {
    inner: &'a T,
}
```

At the point of creation, the borrowchecker is responsible for making sure these "scoped constraints" hold.  At the point of use, the borrowchecker should simply assume these scoped constraints are valid.  If the creator created the existential object in honestly (without unsafe) and it was not tampered with, then the constraints ought to remain valid at the use site.

## Uses of existentials

The examples given so far have been rather boring.  An eager existential such as `exists<'a> &'a str` doesn't really admit a whole lot of possibilities: the only thing you can store in it are `&'static str`, so it seems like a rather pointless exercise.

Where this gets interesting is when you want to work with references unsafely.  In certain situations, especially when self-referential objects are involved, you want to store references, but you want ignore the lifetime entirely and force the compiler to trust your own judgment.  This is the motivation behind the [`'unsafe` lifetime RFC](https://github.com/rust-lang/rfcs/pull/1918).  Here's an example:

```rust
struct UnsafeRef<T> exists<'a> {
    inner: Ref<'a, T>,
}

pub unsafe disembodied_ref(c: RefCell<T>) -> UnsafeRef<T> {
    UnsafeRef { inner: mem::transmute(c.borrow()) }
}
```

The transmute converts `Ref<'a, T>` to `Ref<'b, T>` where:

  - `'a` is the lifetime of the local borrow, and
  - `'b` is the lifetime of `T`.

Since `'b` is already the lifetime of `T`, the `UnsafeRef` is not contaminated with any spurious `T: 'b` (or `T: 'static`) constraints.

In a way, eager existentials offer a way to "erase" the lifetime on any arbitrary reference-like object.  It's analogous to raw pointers, which could be considered lifetime-erased references.  The one difference though is that raw pointers push the responsibility onto the use site, whereas existentials by default push the responsibility (i.e. the scary transmute) onto the creation site.  In any case, it's pretty easy to override this by designing the appropriate interface.

## Existentials as self-referential types

I've already mentioned how existentials will provide a useful (albeit) unsafe way to handle self-referential types similar to `'unsafe`.  I think existentials are useful for something else as well, but this idea is a lot more speculative.

Suppose we have lazy existentials and want to implement something akin to OwningRef, where you have a top-level "owned" object and reference to one of its subobjects.  Some examples include: storing a Vec along with a (possibly mutable) slice of it, or storing a Tree along with a pointer to one of its descendants.

It is tempting to implement something like:

```rust
type OwnedSlice = exists<'a> &'a mut [u8]; // lazy existential!

fn create_owned_slice(v: Vec<u8>, i: usize, j: usize) -> OwnedSlice {
    exists { &mut v[i..j] }
}

fn slice_owned_slice(s: OwnedSlice, i: usize, j: usize) -> OwnedSlice {
    exists {
        match s {
            exists { t } => &mut t[i..j],
        }
    }
}
```

If we did it fully lazily, this would work just fine.  But it's not very efficient: the closure would store `i` and `j` and each time you ask for the slice it will have to produce that slice.  And as you keep slicing it the closure will become increasingly complicated: each step of the slicing process will still be there.

If we had an extremely smart compiler, then we would expect the compiler to optimize this code in a way that avoids this.  Something like this:

```rust
struct RawSlice exists<'a> { // eager existential!
    inner: &'a mut [u8],
}

struct OwnedSlice {
    owner: Vec<u8>,
    slice: RawSlice,
}

fn create_owned_slice(v: Vec<u8>, i: usize, j: usize) -> OwnedSlice {
    let slice = unsafe { mem::transmute(&mut v[i..j]) };
    OwnedSlice {
        owner: v,
        slice: RawSlice { inner: slice },
    }
}

fn slice_owned_slice(s: OwnedSlice, i: usize, j: usize) -> OwnedSlice {
    OwnedSlice {
        owner: s.owner,
        slice: RawSlice {
            inner: match s.slice {
                RawSlice { inner: t } => RawSlice { &mut t[i..j] },
            },
        },
    }
}
```

Now, this is obviously a bit too unrealistic to expect from a real compiler.  But we can look at the steps that allowed this to happen.

The general theme is that instead of deferring the result until a later time, we simply *cache* the resulting object in the existential directly, with little regard for lifetime concerns.  This is permissible as long as the lazy function itself is a pure (referentially transparent) function: insofar as the owner is not mutated (and certainly not deallocated), the slice reference you get would've been bitwise same no matter what.  In OwningRef, this is referred to as a "stable" reference.

To be more precise, a pure function cannot have side-effects, cannot be nondeterministic, and its result cannot depend on the address of its arguments (i.e. must be invariant with respect to moves of its arguments).

In the examples, the `create_owned_slice` function establishes a pure mapping from the owner to the slice reference, so it's perfectly fine to just store the owner and the slice reference in the existential object and disregard the `i` and `j` entirely.  The owner is stored because dropping it would trigger a side-effect and we don't want that!

In the `slice_owned_slice` function, the slice-to-slice function is also pure, so everything is alright.

The main issue with this is that Rust doesn't offer a simple way to *prove* these purity conditions to the compiler.  There's no easy way to even express the claim "this function does not invalidate that variable" in the Rust type system, let alone prove it.  You might be able to use [branding techniques](https://github.com/bluss/indexing), but that is painful to work with and introduces a bunch of fictitious lifetimes (we want *fewer* lifetimes!).

This I think is the main reason why self-referential data types are tricky to handle: there doesn't seem to be a *general* way that doesn't involving writing proofs.  You can write abstractions for some of the common cases (like OwningRef or rental) but there doesn't seem to exist a completely general and safe interface that's also easy to use (I could be wrong on this).

More complications arise when you introduce *choice* types (additive conjunction in linear type theory).  This is necessary if you want the ability to access both the owner and its mutable sub-object (not simultaneously of course).  This is not safe for shared (reference-counted) owners though, because cloning allows you to access the owner and its mutable sub-object simultaneously.
