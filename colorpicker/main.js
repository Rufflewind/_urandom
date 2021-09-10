"use strict";

import vertexShaderSource from "./shader.vert";
import fragmentShaderSource from "./shader.frag";

function assertEqual(x, y) {
    console.assert(x == y, x, y);
}

function createShader(gl, shaderSpec) {
    const shader = gl.createShader(shaderSpec.type);
    gl.shaderSource(shader, shaderSpec.source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)
        && !gl.isContextLost()) {
        const log = gl.getShaderInfoLog(shader);
        gl.deleteShader(shader);
        throw new Error(`failed to compile shader: ${log}`);
    }
    return shader;
}

function createProgram(gl, shaderSpecs) {
    const program = gl.createProgram();
    const shaders = shaderSpecs.map(shaderSpec => {
        return createShader(gl, shaderSpec);
    });
    for (const shader of shaders) {
        gl.attachShader(program, shader);
    }
    gl.linkProgram(program);
    for (const shader of shaders) {
        gl.detachShader(program, shader);
        gl.deleteShader(shader);
    }
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)
        && !gl.isContextLost()) {
        const log = gl.getProgramInfoLog(program);
        gl.deleteProgram(program);
        throw new Error(`failed to link shader program: ${log}`);
    }
    return program;
}

function updateFloatBuffer(gl, program, buffer, attribValues) {
    const allValues = [];
    const attribRefs = [];
    for (const [name, {dim, values}] of Object.entries(attribValues)) {
        const index = allValues.length;
        allValues.push(...values);
        attribRefs.push({name, dim, index});
    }
    const typedArray = new Float32Array(allValues);
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, typedArray, gl.STATIC_DRAW);
    for (const {name, dim, index} of attribRefs) {
        const attrib = gl.getAttribLocation(program, name);
        const offset = index * typedArray.BYTES_PER_ELEMENT;
        gl.vertexAttribPointer(attrib, dim, gl.FLOAT, false, 0, offset);
        gl.enableVertexAttribArray(attrib);
    }
}

const DEBUG = true;

function computeViewSpec(aspectDim) {
    // When a model coordinate is set to FOLLOW_FOCUS, the world coordinate will
    // follow the cursor.  Except for CURSOR, model coordinates should normally be
    // nonnegative.
    const FOLLOW_FOCUS = -1;
    const sliderClipX = 1 - 0.2 / aspectDim[0];
    const swatchClipSize = [0.2 / aspectDim[0], 0.2 / aspectDim[1]];
    const viewSpec = {
        whichView(clipPos) {
            const viewName = clipPos[0] < sliderClipX ? "PLANE_VIEW" : "SLIDER_VIEW";
            return viewSpec.views[viewName];
        },
        views: {
            "PLANE_VIEW": {
                vertexClipPos: [
                    [-1, -1],
                    [-1, 1],
                    [sliderClipX, -1],
                    [sliderClipX, 1],
                ],
                vertexModelPos: [
                    [0, 0, FOLLOW_FOCUS],
                    [0, aspectDim[1], FOLLOW_FOCUS],
                    [aspectDim[0], 0, FOLLOW_FOCUS],
                    [aspectDim[0], aspectDim[1], FOLLOW_FOCUS],
                ],
                clipToWorld(clipPos, worldPos) {
                    worldPos[0] = (clipPos[0] + 1) / (sliderClipX + 1) * aspectDim[0];
                    worldPos[1] = (clipPos[1] + 1) / 2 * aspectDim[1];
                },
            },
            "SLIDER_VIEW": {
                vertexClipPos: [
                    [sliderClipX, -1],
                    [sliderClipX, 1],
                    [1, -1],
                    [1, 1],
                ],
                vertexModelPos: [
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, 0],
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, 1],
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, 0],
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, 1],
                ],
                clipToWorld(clipPos, worldPos) {
                    worldPos[2] = (clipPos[1] + 1) / 2;
                },
            },
            "SWATCH_VIEW": {
                vertexClipPos: [
                    [-1, -1],
                    [-1, -1 + swatchClipSize[1]],
                    [-1 + swatchClipSize[0], -1],
                    [-1 + swatchClipSize[0], -1 + swatchClipSize[1]],
                ],
                vertexModelPos: [
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, FOLLOW_FOCUS],
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, FOLLOW_FOCUS],
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, FOLLOW_FOCUS],
                    [FOLLOW_FOCUS, FOLLOW_FOCUS, FOLLOW_FOCUS],
                ],
                clipToWorld(clipPos, worldPos) {},
            },
        },
    };
    return viewSpec;
}

function buildVertexSpec(views) {
    const QUAD_TO_TRIANGLES = [
        0, 1, 2,
        1, 2, 3,
    ];
    const CLIP_DIMENSION = 2;
    const MODEL_DIMENSION = 3;
    const clipPos = [];
    const modelPos = [];
    for (const {vertexClipPos, vertexModelPos} of Object.values(views)) {
        assertEqual(vertexClipPos.length, 4);
        assertEqual(vertexModelPos.length, 4);
        for (const i of QUAD_TO_TRIANGLES) {
            assertEqual(vertexClipPos[i].length, CLIP_DIMENSION);
            assertEqual(vertexModelPos[i].length, MODEL_DIMENSION);
            clipPos.push(...vertexClipPos[i]);
            modelPos.push(...vertexModelPos[i]);
        }
    }
    return {
        numVertices: views.length * QUAD_TO_TRIANGLES.length,
        attribValues: {
            "clip_pos": {
                dim: CLIP_DIMENSION,
                values: clipPos,
            },
            "model_pos": {
                dim: MODEL_DIMENSION,
                values: modelPos,
            },
        },
    };
}

function canvasClipCoord(canvas, mouseEvent) {
    const rect = canvas.getBoundingClientRect();
    return [
        (mouseEvent.clientX - rect.left) / rect.width * 2 - 1,
        -((mouseEvent.clientY - rect.top) / rect.height * 2 - 1),
    ];
}

// Creates all the resources tied to the OpenGL context.  Don't put non-GL
// resources into this object, as Resources will be re-created when OpenGL
// context is lost and restored.
function createResources(canvas) {
    const gl = canvas.getContext(DEBUG ? "webgl2" : "webgl");
    if (gl == null) {
        return null;
    }
    const program = createProgram(gl, [
        {type: gl.VERTEX_SHADER, source: vertexShaderSource},
        {type: gl.FRAGMENT_SHADER, source: fragmentShaderSource},
    ]);
    gl.useProgram(program);
    return {
        gl,
        program,
        buffer: gl.createBuffer(),
    };
}

function updateSize(gl) {
    const width = window.devicePixelRatio * gl.canvas.clientWidth;
    const height = window.devicePixelRatio * gl.canvas.clientHeight;
    if (!(gl.canvas.width == width && gl.canvas.height == height)) {
        gl.canvas.width = width;
        gl.canvas.height = height;
        gl.viewport(0, 0, width, height);
    }
    return {width, height};
}

function renderConsole(swatchPixel, state) {
    const debugConsole = document.getElementById("debug-console");

    const hexColor = Array.from(swatchPixel.subarray(0, 3)).map(x => {
        const str = x.toString(16);
        return "0".repeat(2 - str.length) + str;
    }).join("");
    debugConsole.innerText = `focus = ${state.focus.join(", ")}
color = #${hexColor} = swatchPixel(${swatchPixel.join(", ")})`;
    debugConsole.style.border = `5px solid swatchPixel(${swatchPixel.join(", ")})`;
}

function initDrag(canvas, alpha) {
    let draggingView = null;

    function updateFocus(clipPos) {
        draggingView.clipToWorld(clipPos, alpha.state.focus);
        alpha.render();
    }

    canvas.addEventListener("pointerdown", event => {
        canvas.setPointerCapture(event.pointerId);
        const clipPos = canvasClipCoord(canvas, event);
        draggingView = alpha.viewSpec.whichView(clipPos);
        updateFocus(clipPos);
    });
    canvas.addEventListener("pointermove", event => {
        if (draggingView == null) {
            return;
        }
        const clipPos = canvasClipCoord(canvas, event);
        updateFocus(clipPos);
    });
    canvas.addEventListener("pointerup", event => {
        draggingView = null;
        canvas.releasePointerCapture(event.pointerId);
    });
}

function createPerformanceGauge(metricNames) {
    const SVG_NAMESPACE = "http://www.w3.org/2000/svg";
    const WINDOW_SIZE = 512;

    const debugGraph = document.getElementById("debug-graph");
    const debugMetric = document.getElementById("debug-metric");

    const metrics = {};
    for (const metricName of metricNames) {
        const polyline = document.createElementNS(SVG_NAMESPACE, "polyline");
        for (let x = 0; x < WINDOW_SIZE; x++) {
            const point = debugGraph.createSVGPoint();
            point.x = x;
            point.y = 1;
            polyline.points.appendItem(point);
        }
        metrics[metricNames] = {
            query: null,
            points: polyline.points,
            // cyclic buffer
            sampleBuffer: new Float64Array(50),
            sampleIndex: 0,
        };
    }

    return {
        // render needs to be sequenced before measure.
        render(gl) {
            // TODO: This render doesn't really work when multiple metrics are sampled at
            // unpredictable intervals.
            const ext = gl.getExtension("EXT_disjoint_timer_query_webgl2");
            const text = [];
            for (const [metricName, metric] of Object.entries(metrics)) {
                const query = metric.query;
                if (query == null) {
                    continue;
                }
                const available = gl.getQueryParameter(query, gl.QUERY_RESULT_AVAILABLE);
                const disjoint = gl.getParameter(ext.GPU_DISJOINT_EXT);
                if (available && !disjoint) {
                    const timeElapsed = gl.getQueryParameter(query, gl.QUERY_RESULT);
                    const {sampleBuffer, sampleIndex, points} = metric.points;
                    const n = metric.points.length;
                    for (let i = 1; i < n; i++) {
                        points.getItem(i - 1).y =
                            points.getItem(i).y;
                    }
                    points.getItem(n - 1).y = 1 - timeElapsed / 1e6;
                    sampleBuffer[sampleIndex] = timeElapsed;
                    metric.sampleIndex = (sampleIndex + 1) % n;
                    const avg = sampleBuffer.reduce((z, x) => z + x, 0) / n;
                    const std = Math.pow(sampleBuffer.reduce(
                        (z, x) => z + Math.pow(x - avg, 2), 0) / n, 0.5);
                    text.push(`${metricName}:  ${(avg / 1e3).toFixed(3)} us
${metricName}:  ${(std / 1e3).toFixed(3)} us`);
                }
                if (available || disjoint) {
                    gl.deleteQuery(query);
                    metric.query = null;
                }
            }
            debugMetric.innerText = text.join("\n");
        },
        measure(gl, metricName, measuredCallback) {
            const ext = gl.getExtension("EXT_disjoint_timer_query_webgl2");
            const metric = metrics[metricName];
            const shouldMeasure = ext != null && metric.query == null;
            if (shouldMeasure) {
                metric.query = gl.createQuery();
                gl.beginQuery(ext.TIME_ELAPSED_EXT, metric.query);
            }
            measuredCallback();
            if (shouldMeasure) {
                gl.endQuery(ext.TIME_ELAPSED_EXT);
            }
        },
    };
}

const METRICS = ["draw"];

function renderMain(performanceGauge, viewHolder, swatchPixel, resources, state) {
    const errorNoWebgl = document.getElementById("error-no-webgl");

    const gl = resources.gl;
    const errorNoWebglDisplay = gl == null ? "" : "none";
    if (errorNoWebgl.style.display != errorNoWebglDisplay) {
        errorNoWebgl.style.display = errorNoWebglDisplay;
    }

    const program = resources.program;
    const focusUniform = gl.getUniformLocation(program, "focus");

    performanceGauge.render(gl);

    const {width, height} = updateSize(gl);
    const minDim = Math.min(width, height);
    const aspectDim = [width / minDim, height / minDim];
    const viewSpec = computeViewSpec(aspectDim);
    viewHolder.viewSpec = viewSpec;
    const vertexSpec = buildVertexSpec(viewSpec.views);
    updateFloatBuffer(gl, program, resources.buffer, vertexSpec.attribValues);

    gl.uniform3fv(focusUniform, state.focus);
    performanceGauge.measure(gl, "draw", () => {
        gl.drawArrays(gl.TRIANGLES, 0, vertexSpec.numVertices);
    });
    gl.readPixels(0, 0, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, swatchPixel);
}

// Aligns rendering to animation frames and deduplicates rendering requests.
function createAlignedRenderer(render) {
    let renderPending = false;

    function alignedRender(...args) {
        if (renderPending) {
            return;
        }
        renderPending = true;
        window.requestAnimationFrame(() => {
            renderPending = false;
            render(...args);
        });
    }

    return alignedRender;
}

function maintainResources(canvas, initializeResources) {
    canvas.addEventListener("webglcontextlost", event => {
        event.preventDefault();
    }, false);
    canvas.addEventListener("webglcontextrestored", event => {
        initializeResources();
    }, false);
    initializeResources();
}

// Handles resize events.
function initResizer(canvas, requestRender) {
    const observer = new window.ResizeObserver(() => {
        requestRender();
    });
    observer.observe(canvas, {box: "content-box"});

}

function main() {
    const canvas = document.getElementById("canvas");
    const errorDisplay = document.getElementById("error-display");

    const state = {focus: [0.5, 0.5, 0.5]};
    let resources;
    maintainResources(canvas, () => {
        resources = createResources(canvas);
    });

    const alignedRender = createAlignedRenderer(renderMain);
    const performanceGauge = createPerformanceGauge(METRICS);
    const viewHolder = {viewSpec: "TODO"};
    const swatchPixel = new Uint8Array(4);
    function requestRender() {
        alignedRender(performanceGauge, viewHolder, swatchPixel, resources, state);
    }

    initResizer(canvas, requestRender);
const alpha = {};
    initDrag(canvas, alpha);
}

main();
