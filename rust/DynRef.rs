use std::{cmp, fmt, hash, panic};
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(not(test))]
use std::process;

#[cfg(not(test))]
fn abort_on_panic<F: FnOnce() -> R, R>(f: F) -> R {
    panic::catch_unwind(panic::AssertUnwindSafe(f))
        .unwrap_or_else(|_| process::abort())
}

#[cfg(test)]
fn abort_on_panic<F: FnOnce() -> R, R>(f: F) -> R {
    f()
}

/// A reference to an object with dynamically checked lifetime.
///
/// If the reference escapes the scope it was created in, the program aborts.
///
/// Note that this is not copyable or clonable.  If you want that, you need
/// to wrap it inside an `Arc`.
pub struct DynRef<T: ?Sized> { value: *const T, alive: *mut AtomicBool }

unsafe impl<T: Send + ?Sized> Send for DynRef<T> {}

unsafe impl<T: Sync + ?Sized> Sync for DynRef<T> {}

impl<T: fmt::Debug + ?Sized> fmt::Debug for DynRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("DynRef")
            .field(&&**self)
            .finish()
    }
}

impl<T: PartialEq + ?Sized> PartialEq for DynRef<T> {
    fn eq(&self, other: &Self) -> bool {
        (&**self).eq(&**other)
    }
}

impl<T: Eq + ?Sized> Eq for DynRef<T> {}

impl<T: PartialOrd + ?Sized> PartialOrd for DynRef<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        (&**self).partial_cmp(&**other)
    }
}

impl<T: Ord + ?Sized> Ord for DynRef<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        (&**self).cmp(&**other)
    }
}

impl<T: hash::Hash + ?Sized> hash::Hash for DynRef<T> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        (&**self).hash(hasher)
    }
}

impl<T: ?Sized> Drop for DynRef<T> {
    fn drop(&mut self) {
        unsafe {
            (*self.alive).store(false, Ordering::Release)
        }
    }
}

impl<T: ?Sized> Deref for DynRef<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe {
            &*self.value
        }
    }
}

impl<T: ?Sized> DynRef<T> {
    pub fn with<F, R>(value: &T, f: F) -> R
        where F: FnOnce(Self) -> R
    {
        struct Canary(AtomicBool);
        impl Drop for Canary {
            fn drop(&mut self) {
                if self.0.load(Ordering::Acquire) {
                    panic!("the DynRef object has escaped");
                }
            }
        }

        abort_on_panic(move || {
            let mut canary = Canary(AtomicBool::new(true));
            f(DynRef { value, alive: &mut canary.0 })
        })
    }
}

/// A mutable reference to an object with dynamically checked lifetime.
///
/// If the reference escapes the scope it was created in, the program aborts.
pub struct DynMut<T: ?Sized> { value: *mut T, alive: *mut AtomicBool }

unsafe impl<T: Send + ?Sized> Send for DynMut<T> {}

unsafe impl<T: Sync + ?Sized> Sync for DynMut<T> {}

impl<T: fmt::Debug + ?Sized> fmt::Debug for DynMut<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("DynMut")
            .field(&&**self)
            .finish()
    }
}

impl<T: PartialEq + ?Sized> PartialEq for DynMut<T> {
    fn eq(&self, other: &Self) -> bool {
        (&**self).eq(&**other)
    }
}

impl<T: Eq + ?Sized> Eq for DynMut<T> {}

impl<T: PartialOrd + ?Sized> PartialOrd for DynMut<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        (&**self).partial_cmp(&**other)
    }
}

impl<T: Ord + ?Sized> Ord for DynMut<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        (&**self).cmp(&**other)
    }
}

impl<T: hash::Hash + ?Sized> hash::Hash for DynMut<T> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        (&**self).hash(hasher)
    }
}

impl<T: ?Sized> Drop for DynMut<T> {
    fn drop(&mut self) {
        unsafe {
            (*self.alive).store(false, Ordering::Release)
        }
    }
}

impl<T: ?Sized> Deref for DynMut<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe {
            &*self.value
        }
    }
}

impl<T: ?Sized> DerefMut for DynMut<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            &mut *self.value
        }
    }
}

impl<T: ?Sized> DynMut<T> {
    pub fn with<F, R>(value: &mut T, f: F) -> R
        where F: FnOnce(Self) -> R
    {
        struct Canary(AtomicBool);
        impl Drop for Canary {
            fn drop(&mut self) {
                if self.0.load(Ordering::Acquire) {
                    panic!("the DynMut object has escaped");
                }
            }
        }

        abort_on_panic(move || {
            let mut canary = Canary(AtomicBool::new(true));
            f(DynMut { value, alive: &mut canary.0 })
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ok() {
        DynRef::with(&[] as &[char], |obj| {
            assert_eq!(obj.len(), 0);
        });
        DynMut::with(&mut [] as &mut [char], |obj| {
            assert_eq!(obj.len(), 0);
        });
    }

    #[test]
    #[should_panic]
    fn fail_ref() {
        panic::set_hook(Box::new(|_| {}));
        DynRef::with(&[] as &[char], |x| x);
    }

    #[test]
    #[should_panic]
    fn fail_mut() {
        panic::set_hook(Box::new(|_| {}));
        DynMut::with(&mut [] as &mut [char], |x| x);
    }
}
