// #version 300 es
// #define attribute in
// #define varying out
attribute vec4 clip_pos;
attribute vec3 model_pos;
uniform mediump vec3 focus;
varying mediump vec4 world_pos;

void main() {
    gl_Position = clip_pos;
    vec3 should_use_model = step(vec3(0.0), model_pos);
    world_pos = vec4(
        mix(focus, model_pos, should_use_model),
        step(0.0, -distance(should_use_model, vec3(0.0))));
}
