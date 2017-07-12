var WHITE_XYZ = [0.95047, 1.0, 1.08883];
var CIELAB_D = 6.0 / 29.0;
var CIELAB_M = Math.pow(29.0 / 6.0, 2) / 3.0;
var CIELAB_C = 4.0 / 29.0;
var CIELAB_A = 3.0;
var CIELAB_RECIP_A = 1.0 / CIELAB_A;
var CIELAB_POW_D_A = Math.pow(CIELAB_D, CIELAB_A);
var CIELAB_MATRIX = [[ 0.0 ,  1.16,  0.0 ],
                     [ 5.0 , -5.0 ,  0.0 ],
                     [ 0.0 ,  2.0 , -2.0 ]];
var CIELAB_MATRIX_INV = [[ 0.86206897,  0.2       ,  0.0       ],
                         [ 0.86206897,  0.0       ,  0.0       ],
                         [ 0.86206897,  0.0       , -0.5       ]];
var CIELAB_OFFSET = -0.16;

function cielab_from_linear(x) {
    return x <= CIELAB_POW_D_A ?
           CIELAB_M * x + CIELAB_C :
           Math.pow(x, CIELAB_RECIP_A);
}

function cielab_to_linear(y) {
    return y <= CIELAB_D ?
           (y - CIELAB_C) / CIELAB_M :
           Math.pow(y, CIELAB_A);
}

function matrix_vector_multiply(m, v, out) {
    out[0] = m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2];
    out[1] = m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2];
    out[2] = m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2];
}

function cielab_to_xyz(lab, xyz) {
    lab[0] -= CIELAB_OFFSET;
    matrix_vector_multiply(CIELAB_MATRIX_INV, lab, xyz);
    lab[0] += CIELAB_OFFSET;
    xyz[0] = cielab_to_linear(xyz[0]) * WHITE_XYZ[0];
    xyz[1] = cielab_to_linear(xyz[1]) * WHITE_XYZ[1];
    xyz[2] = cielab_to_linear(xyz[2]) * WHITE_XYZ[2];
}

function cielab_from_xyz(xyz, lab) {
    var fxyz = [
        cielab_from_linear(xyz[0] / WHITE_XYZ[0]),
        cielab_from_linear(xyz[1] / WHITE_XYZ[1]),
        cielab_from_linear(xyz[2] / WHITE_XYZ[2])
    ];
    matrix_vector_multiply(CIELAB_MATRIX, fxyz, lab);
    lab[0] += CIELAB_OFFSET;
}

var SRGB_D = 0.04045;
var SRGB_M = 12.92;
var SRGB_A = 2.4;
var SRGB_K = 0.055;
var SRGB_MATRIX = [[ 3.2406, -1.5372, -0.4986],
                   [-0.9689,  1.8758,  0.0415],
                   [ 0.0557, -0.204 ,  1.057 ]];
var SRGB_MATRIX_INV = [[ 0.41239559,  0.35758343,  0.18049265],
                       [ 0.21258623,  0.7151703 ,  0.0722005 ],
                       [ 0.01929722,  0.11918386,  0.95049713]];

function srgb_from_linear(x, bad) {
    if (x > 1.0) {
        x = 1.0;
        bad[0] = true;
    } else if (x < 0.0) {
        x = 0.0;
        bad[0] = true;
    }
    return x <= SRGB_D / SRGB_M ?
           SRGB_M * x :
           (1 + SRGB_K) * Math.pow(x, 1 / SRGB_A) - SRGB_K;
}

function srgb_to_linear(y, bad) {
    if (y > 1.0) {
        y = 1.0;
    } else if (y < 0.0) {
        y = 0.0;
    }
    return y <= SRGB_D ?
           y / SRGB_M :
           Math.pow((y + SRGB_K) / (1 + SRGB_K), SRGB_A);
}

function srgb_from_xyz(xyz, rgb, bad) {
    matrix_vector_multiply(SRGB_MATRIX, xyz, rgb);
    rgb[0] = srgb_from_linear(rgb[0], bad);
    rgb[1] = srgb_from_linear(rgb[1], bad);
    rgb[2] = srgb_from_linear(rgb[2], bad);
}

function srgb_to_xyz(rgb, xyz) {
    matrix_vector_multiply(SRGB_MATRIX_INV, [srgb_to_linear(rgb[0]),
                                             srgb_to_linear(rgb[1]),
                                             srgb_to_linear(rgb[2])], xyz);
}

exports.srgbToCielab = function(rgb) {
    var xyz = [0.0, 0.0, 0.0];
    srgb_to_xyz(rgb, xyz);
    var lab = [0.0, 0.0, 0.0];
    cielab_from_xyz(xyz, lab);
    return {value: lab, inGamut: true};
};

exports.cielabToSrgb = function(lab) {
    var xyz = [0.0, 0.0, 0.0];
    cielab_to_xyz(lab, xyz);
    var bad = [false];
    var rgb = [0.0, 0.0, 0.0];
    srgb_from_xyz(xyz, rgb, bad);
    return {value: rgb, inGamut: !bad[0]};
};
