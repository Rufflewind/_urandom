## Idea: Discontinuous lifetimes

Is it possible to have discontinuous lifetimes?

For example:

```rust
let r1 = &mut x;
let r2 = &mut x;
*r1 += 1;
*r2 += 2;
```

This code is perfectly safe, but that's not true in general.

```rust
// this is really bad!
let r1 = &mut v;
let r2 = &mut v[0];
r1.push(...); // this will likely invalidate r2
*r2 = 0;
```

So why is the first one OK but the second one not?

As it turns out, there *is* often an implicit assumption that lifetimes are continuous.  Any gaps in the lifetime would allow someone else to modify the object, thus invalidating associated references.

But not all references are invalidated.  The first example shows an example of that.  If you have a reference to some struct and extract a reference of one of its members, the lifetime *can* be discontinuous as the reference cannot be invalidated without moving the struct.

This is not true for enums, or any pointer indirections (`Box`), however.  So I guess discontinuous lifetimes are fairly limited in usefulness.
