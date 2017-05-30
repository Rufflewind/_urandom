const WHITE_XYZ: [f64; 3] = [0.95047, 1.0, 1.08883];
const CIELAB_D: f64 = 6.0 / 29.0;
const CIELAB_M: f64 = (29.0 / 6.0) * (29.0 / 6.0) / 3.0;
const CIELAB_C: f64 = 4.0 / 29.0;
const CIELAB_A: f64 = 3.0;
const CIELAB_MATRIX: [[f64; 3]; 3] = [
    [ 0.0 ,  1.16,  0.0 ],
    [ 5.0 , -5.0 ,  0.0 ],
    [ 0.0 ,  2.0 , -2.0 ],
];
const CIELAB_MATRIX_INV: [[f64; 3]; 3] = [
    [ 0.86206897,  0.2       ,  0.0       ],
    [ 0.86206897,  0.0       ,  0.0       ],
    [ 0.86206897,  0.0       , -0.5       ],
];
const CIELAB_OFFSET: [f64; 3] = [-0.16, 0.0, 0.0];

fn cielab_from_linear(x: f64) -> f64 {
    if x <= CIELAB_D.powf(CIELAB_A) {
        CIELAB_M * x + CIELAB_C
    } else {
        x.powf(1.0 / CIELAB_A)
    }
}

fn cielab_to_linear(y: f64) -> f64 {
    if y <= CIELAB_D  {
        (y - CIELAB_C) / CIELAB_M
    } else {
        y.powf(CIELAB_A)
    }
}

fn dot_mat_vec(m: [[f64; 3]; 3], v: [f64; 3]) -> [f64; 3] {
    [
        m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2],
        m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2],
        m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2],
    ]
}

fn map_vec<F, T, R>(v: [T; 3], mut f: F) -> [R; 3]
    where F: FnMut(T) -> R, T: Copy
{
    [
        f(v[0]),
        f(v[1]),
        f(v[2]),
    ]
}

fn zip_vec_with<F, T, U, R>(v: [T; 3], w: [U; 3], mut f: F) -> [R; 3]
    where F: FnMut(T, U) -> R, T: Copy, U: Copy
{
    [
        f(v[0], w[0]),
        f(v[1], w[1]),
        f(v[2], w[2]),
    ]
}

fn add_vec(v: [f64; 3], w: [f64; 3]) -> [f64; 3] {
    zip_vec_with(v, w, |x, y| x + y)
}

fn sub_vec(v: [f64; 3], w: [f64; 3]) -> [f64; 3] {
    zip_vec_with(v, w, |x, y| x - y)
}

fn mul_vec(v: [f64; 3], w: [f64; 3]) -> [f64; 3] {
    zip_vec_with(v, w, |x, y| x * y)
}

fn div_vec(v: [f64; 3], w: [f64; 3]) -> [f64; 3] {
    zip_vec_with(v, w, |x, y| x / y)
}

fn cielab_to_xyz(lab: [f64; 3]) -> [f64; 3] {
    let xyz = dot_mat_vec(CIELAB_MATRIX_INV, sub_vec(lab, CIELAB_OFFSET));
    mul_vec(map_vec(xyz, cielab_to_linear), WHITE_XYZ)
}

fn cielab_from_xyz(xyz: [f64; 3]) -> [f64; 3] {
    let xyz = map_vec(div_vec(xyz, WHITE_XYZ), cielab_from_linear);
    add_vec(dot_mat_vec(CIELAB_MATRIX, xyz), CIELAB_OFFSET)
}

const SRGB_D: f64 = 0.04045;
const SRGB_M: f64 = 12.92;
const SRGB_A: f64 = 2.4;
const SRGB_K: f64 = 0.055;
const SRGB_MATRIX: [[f64; 3]; 3] = [
    [ 3.2406, -1.5372, -0.4986],
    [-0.9689,  1.8758,  0.0415],
    [ 0.0557, -0.204 ,  1.057 ],
];
const SRGB_MATRIX_INV: [[f64; 3]; 3] = [
    [ 0.41239559,  0.35758343,  0.18049265],
    [ 0.21258623,  0.7151703 ,  0.0722005 ],
    [ 0.01929722,  0.11918386,  0.95049713],
];

fn srgb_from_linear(x: f64) -> f64 {
    let x = x.max(0.0).min(1.0);
    if x <= SRGB_D / SRGB_M {
        SRGB_M * x
    } else {
        (1.0 + SRGB_K) * x.powf(1.0 / SRGB_A) - SRGB_K
    }
}

fn srgb_to_linear(y: f64) -> f64 {
    let y = y.max(0.0).min(1.0);
    if y <= SRGB_D {
        y / SRGB_M
    } else {
        ((y + SRGB_K) / (1.0 + SRGB_K)).powf(SRGB_A)
    }
}

fn srgb_from_xyz(xyz: [f64; 3]) -> [f64; 3] {
    map_vec(dot_mat_vec(SRGB_MATRIX, xyz), srgb_from_linear)
}

fn srgb_to_xyz(rgb: [f64; 3]) -> [f64; 3] {
    dot_mat_vec(SRGB_MATRIX_INV, map_vec(rgb, srgb_to_linear))
}

#[no_mangle]
pub extern fn cielab_to_srgb(r: f64, g: f64, b: f64, out: &mut [f64; 3]) {
    *out = srgb_from_xyz(cielab_to_xyz([r, g, b]));
}

#[no_mangle]
pub extern fn cielab_from_srgb(rgb: [f64; 3]) -> [f64; 3] {
    cielab_from_xyz(srgb_to_xyz(rgb))
}

fn main() {}
