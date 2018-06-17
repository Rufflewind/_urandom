// A fairly easy way to declare extensible trait hierarchies in Rust with
// support for arbitrary dynamic upcasting and downcasting (just like
// traditional inheritance).  This is based on Simon Marlow's "An Extensible
// Dynamically-Typed Hierarchy of Exceptions":
// https://simonmar.github.io/bib/papers/ext-exceptions.pdf
//
// Pros: Simple, no unsafe magic other than what's encapsulated in mopa.
// Cons: Inefficient due to an indirection for each level of the hierarchy.
//
// Alternative approaches:
//
//   - https://crates.io/crates/query_interface
//   - http://idubrov.name/rust/2018/06/16/dynamic-casting-traits.html

#[macro_use]
extern crate mopa;

use mopa::Any;

// root type

pub trait Object: Any + std::fmt::Debug {
    fn upcast_object(self) -> Box<Object> where Self: Sized {
        Box::new(self)
    }
    fn downcast_object_ref(obj: &Object) -> Option<&Self> where Self: Sized {
        obj.downcast_ref()
    }
}

mopafy!(Object);

macro_rules! declare_supertype {
    ($type:ty, $supertype:ty) => {
        impl Object for $type {
            fn upcast_object(self) -> Box<Object> where Self: Sized {
                Box::<$supertype>::upcast_object(Box::new(self))
            }
            fn downcast_object_ref(obj: &Object) -> Option<&Self> where Self: Sized {
                Box::<$supertype>::downcast_object_ref(obj)?.downcast_ref()
            }
        }
    }
}

// child trait

trait Animal: Object {
    fn animal(&self);
}

mopafy!(Animal);

impl<T: Animal + ?Sized> Animal for Box<T> where Box<T>: Object {
    fn animal(&self) { (**self).animal() }
}

impl Object for Box<Animal> {}

// grandchild trait

trait Canine: Animal {
    fn canine(&self);
}

mopafy!(Canine);

impl<T: Canine + ?Sized> Canine for Box<T> where Box<T>: Object {
    fn canine(&self) { (**self).canine() }
}

declare_supertype!(Box<Canine>, Animal);

// great-grandchild type

#[derive(Debug)]
struct Wolf;

declare_supertype!(Wolf, Canine);

impl Animal for Wolf {
    fn animal(&self) { println!("I can do animal things."); }
}

impl Canine for Wolf {
    fn canine(&self) { println!("I can do canine things."); }
}

impl Wolf {
    fn wolf(&self) { println!("I can do wolf things."); }
}

fn main() {
    let wolf: Box<Object> = Wolf.upcast_object();
    Box::<Animal>::downcast_object_ref(&*wolf).unwrap().animal();
    Box::<Canine>::downcast_object_ref(&*wolf).unwrap().canine();
    Wolf::downcast_object_ref(&*wolf).unwrap().wolf();
}
