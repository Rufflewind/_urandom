#![feature(test)]
extern crate test;

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use std::sync::Arc;
    use std::rc::Rc;

    // 3ns
    #[bench]
    fn bench_rc_string_clone(b: &mut Bencher) {
        let s = Rc::new(String::from("Hello"));
        b.iter(|| {
            test::black_box(s.clone());
        });
    }

    // 10ns
    #[bench]
    fn bench_arc_string_clone(b: &mut Bencher) {
        let s = Arc::new(String::from("Hello"));
        b.iter(|| {
            test::black_box(s.clone());
        });
    }

    // 23ns
    #[bench]
    fn bench_string_clone(b: &mut Bencher) {
        let s = String::from("Hello");
        b.iter(|| {
            test::black_box(s.clone());
        });
    }
}
