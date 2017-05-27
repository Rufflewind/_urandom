use std::marker::PhantomData;

// workaround for lack of HKT/ATC
pub trait TypeFn<T: ?Sized> { type Type; }

impl<T: ?Sized> TypeFn<T> for *const () { type Type = *const T; }

impl<T: ?Sized> TypeFn<T> for *mut () { type Type = *mut T; }

impl<'a, T: ?Sized> TypeFn<&'a T> for &'static () { type Type = &'a T; }

impl<'a, T: ?Sized> TypeFn<&'a T> for &'static mut () { type Type = &'a mut T; }

pub enum Const {}
pub enum Mut {}

pub trait Mutability {
    // Rust doesn't support higher-ranked trait bounds for types :(
    type Ptr: Copy; // + for<T: ?Sized> TyFn<T>
    type Ref; // : for<'a, T: ?Sized> TypeFn<&'a, T>

    // it would've been nicer to just have this automatically as part of the
    // variance rules, had Mutability been a built-in kind :D
    fn downgrade_ptr<T: ?Sized>(p: <Self::Ptr as TypeFn<T>>::Type) -> *const T
        where Self::Ptr: TypeFn<T>;
    fn downgrade_ref<'a, T: ?Sized>(r: <Self::Ref as TypeFn<&'a T>>::Type) -> &'a T
        where Self::Ref: TypeFn<&'a T>;
}

impl Mutability for Const {
    type Ptr = *const ();
    type Ref = &'static ();

    fn downgrade_ptr<T: ?Sized>(p: <Self::Ptr as TypeFn<T>>::Type) -> *const T
        where Self::Ptr: TypeFn<T> { p; panic!() }
    fn downgrade_ref<'a, T: ?Sized>(r: <Self::Ref as TypeFn<&'a T>>::Type) -> &'a T
        where Self::Ref: TypeFn<&'a T> { r; panic!() }
}

impl Mutability for Mut {
    type Ptr = *mut ();
    type Ref = &'static mut ();

    fn downgrade_ptr<T: ?Sized>(p: <Self::Ptr as TypeFn<T>>::Type) -> *const T
        where Self::Ptr: TypeFn<T> { p; panic!() }
    fn downgrade_ref<'a, T: ?Sized>(r: <Self::Ref as TypeFn<&'a T>>::Type) -> &'a T
        where Self::Ref: TypeFn<&'a T> { r; panic!() }
}

pub struct Buf<'a, M: Mutability>
    where M::Ptr: TypeFn<u8>,
          M::Ref: TypeFn<&'a u8>,
{
    ptr: <M::Ptr as TypeFn<u8>>::Type,
    len: usize,
    phantom: PhantomData<<M::Ref as TypeFn<&'a u8>>::Type>,
}

impl<'a> Clone for Buf<'a, Const> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            len: self.len,
            phantom: self.phantom,
        }
    }
}

impl<'a> Copy for Buf<'a, Const> {}

// have to manually derive Debug :(
impl<'a, M: Mutability> std::fmt::Debug for Buf<'a, M>
    where M::Ptr: TypeFn<u8>,
          M::Ref: TypeFn<&'a u8>,
          <M::Ptr as TypeFn<u8>>::Type: std::fmt::Debug,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Buf")
            .field("ptr", &self.ptr)
            .field("len", &self.len)
            .finish()
    }
}

impl<'a, M: Mutability> Buf<'a, M>
    where M::Ptr: TypeFn<u8>,
          M::Ref: TypeFn<&'a u8>,
{
    // inference won't work here because associated type projections aren't injective!
    // so we'd rather just have two separate constructors

    // pub unsafe fn from_raw(ptr: <M::Ptr as TypeFn<u8>>::Type, len: usize) -> Self {
    //     Self { ptr, len, phantom: PhantomData }
    // }

    pub fn len(&self) -> usize {
        self.len
    }

    // ???
    pub fn as_ptr<'b, N: Mutability>(<N::Ref as TypeFn<&'b self>>::Type)
                                     -> <M::And, N>::Ptr as TypeFn<u8>>::Type {
        M::combine_ptr(self.ptr)
    }

    // ???
    pub fn reborrow<'b, N: Mutability>(<N::Ref as TypeFn<&'b self>>::Type)
                                       -> Buf<'a, M> {
        unsafe { Buf::from_raw(self.as_ptr(), self.len()) }
    }

    // ???
    pub fn downgrade(self) -> Buf<'a> {
        unsafe { Buf::from_raw(M::downgrade_ptr(self.as_ptr()), self.len()) }
    }
}

fn main() {}