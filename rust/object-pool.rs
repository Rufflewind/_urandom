// cargo-deps: rand

// A simple allocator built using an implicit free list.
// (See also: stjepang/vec-arena)
//
// Compile this using either "rustc --test" or "cargo script".

#[cfg(not(test))]
extern crate rand;

#[derive(Clone, Debug)]
pub struct Pool<T> {
    pub slots: Vec<Result<T, usize>>,
    pub len: usize,
    pub next: usize,
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self {
            slots: Default::default(),
            len: Default::default(),
            next: Default::default(),
        }
    }
}

impl<T> Pool<T> {

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn capacity(&self) -> usize {
        self.slots.len()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        match self.slots.get(index) {
            Some(&Ok(ref item)) => Some(item),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        match self.slots.get_mut(index) {
            Some(&mut Ok(ref mut item)) => Some(item),
            _ => None,
        }
    }

    pub fn insert(&mut self, item: T) -> usize {
        let idx = self.next;
        loop {
            match self.slots.get_mut(idx) {
                None => {}
                Some(slot) => match std::mem::replace(slot, Ok(item)) {
                    Err(next) => {
                        self.next = next;
                        break;
                    },
                    Ok(_) => unreachable!(),
                }
            }
            let len = self.slots.len();
            self.slots.push(Err(len + 1));
        }
        self.len += 1;
        idx
    }

    pub fn remove(&mut self, index: usize) -> Option<T> {
        match self.slots.get_mut(index) {
            None => None,
            Some(slot) => {
                if let Err(_) = *slot {
                    return None;
                }
                let empty = Err(self.next);
                self.next = index;
                self.len -= 1;
                std::mem::replace(slot, empty).ok()
            }
        }
    }

}

#[test]
fn test() {
    let mut pool = Pool::default();
    println!("{:?}", pool);
    assert_eq!(pool.insert(":3"), 0);
    println!("{:?}", pool);
    assert_eq!(pool.insert(":D"), 1);
    println!("{:?}", pool);
    assert_eq!(pool.remove(0), Some(":3"));
    println!("{:?}", pool);
    assert_eq!(pool.remove(1), Some(":D"));
    println!("{:?}", pool);
    assert_eq!(pool.insert(":o"), 1);
    println!("{:?}", pool);
    assert_eq!(pool.insert(":P"), 0);
    println!("{:?}", pool);
}

#[cfg(not(test))]
fn main() {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let mut pool = Pool::default();
    let mut decimate = false; // every so often, decimate the entire pool
    loop {
        let mut changed = false;
        if rng.gen_range(0, pool.capacity() + 10) == 0 {
            decimate = true;
        }
        if !decimate && rng.gen_range(0, 2) == 0 {
            pool.insert("");
            changed = true;
        } else if pool.len() == 0 {
            decimate = false;
        } else {
            let i = rng.gen_range(0, pool.capacity());
            if pool.remove(i).is_some() {
                changed = true;
            }
        }
        if changed {
            if decimate {
                print!("- ")
            } else {
                print!("+ ")
            }
            print!("({:02}) ", pool.next);
            for slot in &pool.slots {
                match *slot {
                    Ok(_) => print!(" . "),
                    Err(i) => print!("{:02} ", i),
                }
            }
            println!("");
//            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }
}

// rustc -C llvm-args=-x86-asm-syntax=intel --emit asm -O pool.rs
//
// extern fn pool_remove(p: &mut Pool<*mut u8>, i: usize, r: &mut Option<*mut u8>) {
//     *r = p.remove(i)
// }
//
// extern { fn register(cb: extern fn(p: &mut Pool<*mut u8>, i: usize, r: &mut Option<*mut u8>)); }
//
// fn main() {
//     unsafe { register(pool_remove) }
// }
