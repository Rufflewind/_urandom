## Interpretation of lifetimes in Rust

Lifetime hierarchy:

    'static : 'a : 'dead

Haskell notation:

    Outlives a b <==> 'a: 'b

    forall a t . Ref a t -> ST a (Ref a t)
      <==>  for<'a, T> fn(&'a T) 'a -> &'a T

`'static` outlives all:

    instance Outlives S a

Everything outlives `'dead`:

    instance Outlives a D

References are covariant in their lifetimes:

    refDowncast :: Outlives a b => Ref a t -> Ref b t
                :: Outlives S a => Ref S t -> Ref a t
                :: Outlives a D => Ref a t -> Ref D t

Actions (`ST`) are covariant in their lifetimes too (but we don't really care).

References can always dereferenced in a region contained by the lifetime of the reference:

    deref: Outlives a b => Ref a t -> ST b t

Borrowing references creates a new region contained within the outer:

    borrow: t -> (forall b . Outlives a b => Ref b t -> ST b r) -> ST a (t, r)

There is no way to dereference `&'dead T`, since that would require `ST 'dead t`, which is impossible to run.  The use of a `'dead` lifetime therefore infects the function itself.

    fn foo(_x: &'dead ()) 'static { }

    fn foo(x: &'dead ()) 'dead { *x }

Any function whose lifetime is inferred as `'dead` is effectively dead code.  Currently they cause the borrow checker to fail.

More generally:

    fn foo<'a, 'b, 'c>(x: &'a (), _y: &'b (), z: &'c ()) 'a + 'c {
        *x;
        *z;
    }

Having `'dead` lifetimes might allow us to express [`drop` safely](https://github.com/rust-lang/rust/issues/26656):

~~~rust
impl<T: 'dead> Drop for Foo<T, U> {
    fn drop(&mut self) 'static {
        …
    }
}
~~~

The presence of `T: 'dead` should inform the drop checker that the implemented `drop` is blind to `T`.  Ideally there should be no way to (safely) inspect `T` directly, but still possible to work with `*mut T` or `*const T`.  However, currently Rust enforces `*mut T: 'a` well-formed (WF) only if `T: 'a`, so that doesn’t quite work.  (The WF check for lifetimes is overly simplistic IMO.)
