// #version 300 es
// #define varying in
// out mediump vec4 frag_color;
// #define gl_FragColor frag_color
precision mediump float;
uniform mediump vec3 focus;
varying mediump vec4 world_pos;

const vec3 WHITE_XYZ = vec3(0.95047, 1.0, 1.08883);
const float CIELAB_D = 6.0 / 29.0;
const float CIELAB_M = pow(29.0 / 6.0, 2.0) / 3.0;
const float CIELAB_C = 4.0 / 29.0;
const float CIELAB_A = 3.0;
const float CIELAB_RECIP_A = 1.0 / CIELAB_A;
const float CIELAB_POW_D_A = pow(CIELAB_D, CIELAB_A);
const mat3 CIELAB_MATRIX = mat3(
    0.0, 5.0, 0.0,
    1.16, -5.0, 2.0,
    0.0, 0.0, -2.0);
const mat3 CIELAB_MATRIX_INV = mat3(
    0.86206897, 0.86206897, 0.86206897,
    0.2, 0.0, 0.0,
    0.0, 0.0, -0.5);
const float CIELAB_OFFSET = -0.16;

//#define VECTORIZE
#ifndef VECTORIZE
float cielab_to_linear(float y) {
    return y <= CIELAB_D
        ? (y - CIELAB_C) / CIELAB_M
        : pow(y, CIELAB_A);
}

vec3 cielab_to_xyz(vec3 lab) {
    lab[0] -= CIELAB_OFFSET;
    vec3 xyz = CIELAB_MATRIX_INV * lab;
    lab[0] += CIELAB_OFFSET;
    return vec3(
        cielab_to_linear(xyz[0]),
        cielab_to_linear(xyz[1]),
        cielab_to_linear(xyz[2])) * WHITE_XYZ;
}
#else
vec3 cielab_to_linear(vec3 y) {
    return mix(
        pow(y, vec3(CIELAB_A)),
        (y - CIELAB_C) / CIELAB_M,
        step(y, vec3(CIELAB_D)));
}

vec3 cielab_to_xyz(vec3 lab) {
    lab[0] -= CIELAB_OFFSET;
    vec3 xyz = CIELAB_MATRIX_INV * lab;
    lab[0] += CIELAB_OFFSET;
    return cielab_to_linear(xyz) * WHITE_XYZ;
}
#endif

float SRGB_D = 0.04045;
float SRGB_M = 12.92;
float SRGB_A = 2.4;
float SRGB_K = 0.055;
mat3 SRGB_MATRIX = mat3(
    3.2406, -0.9689, 0.0557,
    -1.5372, 1.8758, -0.204,
    -0.4986, 0.0415, 1.057);

// The 4th component signifies whether it is within sRGB gamut.
vec4 linear_to_srgb(vec3 t0) {
    vec3 t = clamp(t0, vec3(0.0), vec3(1.0));
    float error = distance(t, t0);
    return vec4(
        mix(
            (1.0 + SRGB_K) * pow(t, vec3(1.0 / SRGB_A)) - SRGB_K,
            SRGB_M * t,
            step(t, vec3(SRGB_D / SRGB_M))),
        step(0.0, -distance(t, t0))
      //  smoothstep(0.0, 10.0 * fwidth(error), error)
//        smoothstep(0.0, 3.0 * distance(vec2(0.0), vec2(dFdx(error), dFdy(error))), error)
    );
}

vec4 xyz_to_srgb(vec3 xyz) {
    return linear_to_srgb(SRGB_MATRIX * xyz);
}

vec4 cielab_to_srgb(vec3 lab) {
    return xyz_to_srgb(cielab_to_xyz(lab));
}

vec4 cursor_rgba(float dist) {
    const float CURSOR_RADIUS = 0.015;
    float d = dist - CURSOR_RADIUS;
    float cursorAlpha = 1.0 - smoothstep(0.0, 0.004, abs(d));
    float cursorColor = 1.0 - smoothstep(-0.001, 0.001, d);
    return vec4(vec3(cursorColor), cursorAlpha);
}

void main() {
    const float OUT_OF_GAMUT_MULTIPLIER = 0.7;
    float dist = distance(world_pos.xyz, focus);
    vec4 cursor = cursor_rgba(dist);
    vec4 rgba = vec4(world_pos.z, mix(vec2(-1.1), vec2(1.1), world_pos.xy), 0.0);
    for (int i = 0; i < 1; i++) { // DEBUG: for benchmarking
        rgba = cielab_to_srgb(rgba.xyz);
    }
    //rgba = vec4(world_pos, 1.0);
    gl_FragColor = vec4(
        mix(
            rgba.rgb * mix(mix(OUT_OF_GAMUT_MULTIPLIER, 1.0, world_pos.w), 1.0, rgba.a),
            cursor.rgb,
            cursor.a),
        1.0);
}
