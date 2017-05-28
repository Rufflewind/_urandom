// A manual borrowing system in Rust
// =================================
//
// it would be nice to have:
//
// (1) type parameters in higher-ranked trait bounds
//     (e.g. with_label uses for<o> FnOnce(F<Label<o>>) -> ...)
// (2) polymorphic closures/lambdas (e.g. with_label)
// (3) higher-kinded types (e.g. hide_label uses F: type -> type)
// (4) linear (non-affine / undroppable) types
// (5) nonnegative integral parameters for generics
//
// the API can still be emulated in today's Rust to some extent, but
// it'd be even more clunky!
//
// the use of a label could be avoided at the cost of adding a dynamic
// check for object identity; this avoids the need for (1)-(3)
//
// alternatively, Rust could really use some sort of smarter built-in system
// for tracking object identity; this could avoid the need for (1)-(3) while
// also making it more ergonomic to use

fn some_label() -> SomeLabel<Id>;
fn hide_label(F<Label<o>>) -> SomeLabel<F>;
fn with_label(SomeLabel<F>, for<o> FnOnce(F<Label<o>>) -> R) -> R;

fn new_owned(Label<o>, Box<T>) -> Owned<Label<o>, T, 0>;
fn owned_into_box(Owned<o, T, 0>) -> (Label<o>, Box<T>);

fn borrow(Owned<o, T, n>) -> (Owned<o, T, n + 1>, Ref<o, T>);
fn kill(Ref<o, T>) -> DeadRef<o>;
fn restore(Owned<o, T, n + 1>, DeadRef<o, T>) -> Owned<o, T, n>;

fn borrow_mut(Owned<o, T, 0>) -> (OwnedMut<o, T>, RefMut<o, T>);
fn kill_mut(RefMut<o, T>) -> DeadRefMut<o>;
fn restore_mut(OwnedMut<o, T>, DeadRefMut<o, T>) -> Owned<o, T, 0>;

struct Owned<o, T, n>(Box<T>);
impl !Affine for Owned<o, T, n + 1> {}

struct OwnedMut<o, T>(Box<T>);
impl !Affine for OwnedMut<o, T> {}

struct Ref<o, T>(*const T);
struct DeadRef<o>;

struct RefMut<o, T>(*mut T);
struct DeadRefMut<o>;
