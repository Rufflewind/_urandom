use std::marker::PhantomData;

#[derive(Copy, Clone, Debug)]
pub struct Buf<'a> {
    ptr: *const u8,
    len: usize,
    phantom: PhantomData<&'a u8>,
}

impl<'a> Buf<'a> {
    pub unsafe fn from_raw(ptr: *const u8, len: usize) -> Self {
        Self { ptr, len, phantom: PhantomData }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.ptr
    }
}

#[derive(Debug)]
pub struct BufMut<'a> {
    ptr: *mut u8,
    len: usize,
    phantom: PhantomData<&'a mut u8>,
}

impl<'a> BufMut<'a> {
    pub unsafe fn from_raw(ptr: *mut u8, len: usize) -> Self {
        Self { ptr, len, phantom: PhantomData }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.ptr
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.ptr
    }

    pub fn reborrow(&self) -> Buf {
        unsafe { Buf::from_raw(self.as_ptr(), self.len()) }
    }

    pub fn reborrow_mut(&mut self) -> BufMut {
        unsafe { BufMut::from_raw(self.as_mut_ptr(), self.len()) }
    }

    pub fn downgrade(self) -> Buf<'a> {
        unsafe { Buf::from_raw(self.as_ptr(), self.len()) }
    }
}

fn main() {}