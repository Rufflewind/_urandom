// cargo-deps: GSL, rand, rayon

// Numerically evaluates equation (9) in:
// M. Gell-Mann and K. A. Brueckner.
// "Correlation Energy of an Electron Gas at High Density"
// https://doi.org/10.1103/PhysRev.106.364
// (mirror: http://authors.library.caltech.edu/3713/1/GELpr57b.pdf )
//
// With n = 1e9 iterations, takes about 2 minutes on an Intel i5.

extern crate rand;
extern crate rgsl;

use std::f64::consts::PI;
use rand::Rng;

fn add(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}

fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn sq(a: [f64; 3]) -> f64 {
    dot(a, a)
}

fn integrand(q: [f64; 3], p1: [f64; 3], p2: [f64; 3]) -> f64 {
    if sq(p1) < 1.0 && sq(p2) < 1.0 && sq(add(p1, q)) > 1.0 && sq(add(p2, q)) > 1.0 {
        1.0 / (sq(add(q, add(p1, p2))) * (sq(q) + dot(q, add(p1, p2))) * sq(q))
    } else {
        0.0
    }
}

fn dinfinitize(t: f64) -> f64 {
    let tsq = t.powi(2);
    (1.0 + tsq) / (1.0 - tsq).powi(2)
}

fn infinitize(t: f64) -> f64 {
    let tsq = t.powi(2);
    t / (1.0 - tsq)
}

fn main() {
    // reads from the GSL_RNG_TYPE environment variable
    // (use GSL_RNG_TYPE=taus for fastest)
    let rng_type = &rgsl::RngType::env_setup().unwrap();
    let rng = rgsl::Rng::new(rng_type).unwrap();
    rng.set(rand::OsRng::new().unwrap().gen());
    let d = 9;
    let mc = rgsl::MiserMonteCarlo::new(d).unwrap();
    let n = 1_000_000_000;
    let (result, err) = mc.integrate(
        d,
        |x| {
            let q = [infinitize(x[0]), infinitize(x[1]), infinitize(x[2])];
            let p1 = [x[3], x[4], x[5]];
            let p2 = [x[6], x[7], x[8]];
            integrand(q, p1, p2) *
                dinfinitize(x[0]) *
                dinfinitize(x[1]) *
                dinfinitize(x[2])
        },
        &[-1.0; 9], &[ 1.0; 9],
        n,
        &rng
    ).unwrap();
    let coeff = 3.0 / (16.0 * PI.powi(5));
    println!("   {}\n ± {}", coeff * result, coeff * err);
}
