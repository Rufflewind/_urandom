function bytesFromInt32(i) {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    view.setUint32(0, i);
    return new Uint8Array(buffer);
}

function fnv1a32(bytes) {
    let hash = 0x811c9dc5 | 0;
    for (const byte of bytes) {
        hash = Math.imul(hash ^ byte, 0x01000193);
    }
    return bytesFromInt32(hash);
}

function fnv1a32Multi(bytes) {
    // repeat FNV-1a several times to improve randomness in the presence of
    // low-entropy inputs
    for (const _ of Array(128)) {
        bytes = fnv1a32(bytes);
    }
    return bytes;
}

function unpackPoint(dim, p) {
    const x = p % dim;
    const y = Math.floor(p / dim);
    return [x, y];
}

function encodePoint(...p) {
    return p.join(",");
}

function decodePoint(s) {
    return s.split(",").map(x => +x);
}

function immediateCells(x, gridDim) {
    return x % 2 == 0 ? [x / 2] :
        x <= 0 ? [(x + 1) / 2] :
        x >= 2 * gridDim - 1 ? [(x - 1) / 2] :
        [(x - 1) / 2, (x + 1) / 2];
}

function decodePuzzle(code) {
    const match = /^#(\d+)x(\d+):([A-Za-z]*)$/.exec(code);
    const gridWidth = +match[1];
    const gridHeight = +match[2];
    const dots = [];
    let index = -1;
    let accum = 0;
    for (const chr of match[3].toLowerCase()) {
        if (chr == 'z') {
            accum += 25;
            continue;
        }
        index += accum + chr.charCodeAt(0) - 'a'.charCodeAt(0) + 1;
        accum = 0;
        const [x, y] = unpackPoint(2 * gridWidth - 1, index);
        dots.push({x, y});
    }
    const solved = {};
    for (const [g, {x, y}] of dots.entries()) {
        for (const i of immediateCells(x, gridWidth)) {
            for (const j of immediateCells(y, gridHeight)) {
                solved[encodePoint(i, j)] = g;
            }
        }
    }
    return {gridWidth, gridHeight, dots, solved, candidates: {}};
}

function withinBounds(state, i, j) {
    return i >= 0 && i < state.gridWidth && j >= 0 && j < state.gridHeight;
}

function antipodalPoints(state, g, origI, origJ) {
    const invI = state.dots[g].x - origI;
    const invJ = state.dots[g].y - origJ;
    const points = [[origI, origJ], [invI, invJ]];
    for (const [i, j] of points) {
        if (!withinBounds(state, i, j)) {
            return null;
        }
        const gSolved = state.solved[encodePoint(i, j)];
        if (gSolved != undefined && gSolved != g) {
            return null;
        }
    }
    return points;
}

function solveSubstep(state, visited, stack, candidates) {
    const nextStack = [];
    while (stack.length != 0) {
        const {g, i, j} = stack.pop();
        if (visited.hasOwnProperty(encodePoint(g, i, j))) {
            continue;
        }
        visited[encodePoint(g, i, j)] = true;
        if (!antipodalPoints(state, g, i, j)) {
            continue;
        }
        const p = encodePoint(i, j);
        const gs = candidates[p] || {};
        candidates[p] = Object.assign(gs, {[g]: true});
        nextStack.push({g, i: i - 1, j});
        nextStack.push({g, i: i + 1, j});
        nextStack.push({g, i, j: j - 1});
        nextStack.push({g, i, j: j + 1});
    }
    stack.push(...nextStack);
}

function solveStep(state) {
    const stack = [];
    for (const [p, g] of Object.entries(state.solved)) {
        const [i, j] = decodePoint(p);
        stack.push({g, i, j});
    }
    for (const [p, gSet] of Object.entries(state.candidates)) {
        const [origI, origJ] = decodePoint(p);
        const gs = Object.keys(gSet);
        if (gs.length == 1) {
            const g = +gs[0];
            const points = antipodalPoints(state, g, origI, origJ);
            if (points) {
                for (const [i, j] of points) {
                    state.solved[encodePoint(i, j)] = g;
                }
            }
        }
    }
    const visited = {};
    const candidates = {};
    while (stack.length != 0) {
        solveSubstep(state, visited, stack, candidates);
    }
    state.candidates = candidates;
    renderPuzzle(state);
}

function cellStatus(state, i, j) {
    const p = encodePoint(i, j);
    const g = state.solved[p];
    if (g != null) {
        return {g, gs: [g]};
    }
    const gs = Object.keys(state.candidates[p] || {}).map(x => +x).sort();
    return {g: null, gs};
}

const cellSize = 40;
const visualCellSize = 80;
const margin = 10;

function renderPuzzle(state) {
    const ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    canvas.width = state.gridWidth * visualCellSize + 2 * margin;
    canvas.height = state.gridHeight * visualCellSize + 2 * margin;
    ctx.save();
    ctx.translate(margin, margin);
    ctx.scale(
        (canvas.width - margin * 2) / state.gridWidth / cellSize,
        (canvas.height - margin * 2) / state.gridHeight / cellSize,
    );
    for (const i of Array(state.gridWidth).keys()) {
        for (const j of Array(state.gridHeight).keys()) {
            const {g, gs} = cellStatus(state, i, j);
            ctx.beginPath();
            ctx.rect(i * cellSize, j * cellSize, cellSize, cellSize);
            if (g != null) {
                const rgb = fnv1a32Multi(bytesFromInt32(g)).slice(0, 3);
                ctx.fillStyle = `rgb(${rgb.map(x => x | 0x80).join(", ")})`;
                ctx.fill();
            }
            ctx.strokeStyle = "#000";
            ctx.stroke();
            ctx.font = "8px sans-serif";
            ctx.fillStyle = "#000";
            ctx.fillText(
                gs.map(x => x.toString(36)).join(" "),
                (i + 0.1) * cellSize,
                (j + 0.9) * cellSize,
                0.8 * cellSize,
            );
        }
    }
    for (const {x, y} of state.dots) {
        ctx.beginPath();
        ctx.arc(
            (x + 1) / 2 * cellSize,
            (y + 1) / 2 * cellSize,
            0.1 * cellSize,
            0,
            2 * Math.PI,
        );
        ctx.fillStyle = "#000";
        ctx.fill();
    }
    ctx.restore();
}

function onCanvasToggle(state, e) {
    const canvas = e.target;
    const rect = canvas.getBoundingClientRect();
    const x = (e.clientX - rect.left - margin) / (canvas.width - 2 * margin);
    const y = (e.clientY - rect.top - margin) / (canvas.height - 2 * margin);
    const i = Math.floor(x * state.gridWidth);
    const j = Math.floor(y * state.gridHeight);
    if (!withinBounds(state, i, j)) {
        return;
    }
    const delta = e.button == 0 ? 1 : -1;
    const p = encodePoint(i, j);
    const g = state.solved[p];
    const gSet = state.candidates[p];
    const gs = gSet
          ? Object.keys(gSet).map(x => +x)
          : [...Array(state.dots.length).keys()];
    const choices = [undefined, ...gs];
    const index = choices.indexOf(g) + delta;
    const solved = choices[(index + choices.length) % choices.length];
    if (solved == undefined) {
        delete state.solved[p];
    } else {
        state.solved[p] = solved;
    }
    renderPuzzle(state);
}

const hashInput = document.getElementById("hashInput");
hashInput.addEventListener("change", () => location.hash = hashInput.value);

let currentState;

const canvas = document.getElementById("canvas");
canvas.addEventListener("mousedown", e => onCanvasToggle(currentState, e));
canvas.addEventListener("contextmenu", e => e.preventDefault());

const solveButton = document.getElementById("solveButton");
solveButton.addEventListener("click", () => solveStep(currentState));

function onHashChange() {
    hashInput.value = location.hash;
    const state = decodePuzzle(location.hash);
    currentState = state;
    renderPuzzle(state);
}

window.addEventListener("hashchange", onHashChange);
if (!location.hash) {
    // https://puzzling.stackexchange.com/questions/125710/an-unusually-tricky-galaxy
    location.hash = "#9x9:jexfctzndtfbmvzpzgdddwf";
}
onHashChange();
