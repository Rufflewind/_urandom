use std::marker::PhantomData;

// mutability parameters:
//
// - always starts with tilde (akin to the apostrophe of lifetime parameters)
// - has only two "values": mut and const (unlike lifetimes, which are infinitely many)
// - have variance / subtyping: mut -> const
// - mut + const = const
//
// an object with unknown mutability is not copyable/cloneable, but it's also
// not mutable either

// ownership polymorphism??

// Clone and Copy will only derive for ~m = const
#[derive(Clone, Copy, Debug)]
pub struct Buf<'a, ~m> {
    ptr: *~m u8,
    len: usize,
    phantom: PhantomData<&'a ~m u8>,
}

impl<'a, ~m> Buf<'a, ~m> {
    pub unsafe fn from_raw(ptr: *~m u8, len: usize) -> Self {
        Self { ptr, len, phantom: PhantomData }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_ptr(&~m2 self) -> *~m3 u8 where ~m3: ~m + ~m2 {
        self.ptr
    }

    // this is implicit due to covariance
    //pub fn downgrade(self) -> Buf<'a, const>;

    pub fn reborrow(&~m2 self) -> Buf<~(m & m2)> {
        unsafe { Buf::from_raw(self.as_ptr(), self.len()) }
    }
}

fn main() {}
