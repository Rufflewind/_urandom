function getOrInsert(object, property, callback) {
    if (Object.hasOwn(object, property)) {
        return object[property];
    }
    const value = callback();
    object[property] = value;
    return value;
}

function invertObject(object) {
    const inverted = {};
    for (const [key, value] of Object.entries(object)) {
        inverted[value] = key;
    }
    return inverted;
}

function shuffleInPlace(random, array) {
    // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
    for (let i = array.length; --i > 0;) {
        const j = Math.floor(random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
    }
}

function namespacedElement(namespaceURI, tagName, options) {
    options ??= {};
    const element = document.createElementNS(namespaceURI, tagName);
    for (const [name, value] of Object.entries(options.attributes || {})) {
        element.setAttribute(name, value);
    }
    for (const [type, listener] of Object.entries(options.listeners || {})) {
        element.addEventListener(type, listener);
    }
    for (const [_, child] of Object.entries(options.children || {})) {
        element.append(child);
    }
    return element;
}

function htmlElement(...arguments) {
    return namespacedElement("http://www.w3.org/1999/xhtml", ...arguments);
}

function svgElement(...arguments) {
    return namespacedElement("http://www.w3.org/2000/svg", ...arguments);
}

const ALPHABETIC_BASE = 0x60;
const EDGE_NO = 0;
const EDGE_UNKNOWN = 1;
const EDGE_YES = 2;
const EDGE_STATE_BY_CHAR = {"n": EDGE_NO, "u": EDGE_UNKNOWN, "y": EDGE_YES};
const EDGE_CHAR_BY_STATE = invertObject(EDGE_STATE_BY_CHAR);
const DEFAULT_HASH = "#10x10t0:a32b23b32a13a2a2a2f22a131h2a121b122c332b2211a1b3a3a1a222a2c312a3223a3c3f";
const MOVE_SEP = "";

function decodeEdgeStates(moves, edgeStates) {
    for (const match of moves.matchAll(/(\d+)([nuy])/g)) {
        edgeStates[+match[1]] = EDGE_STATE_BY_CHAR[match[2]];
    }
}

function joinMoves(moves) { return moves.join(""); }

function encodeEdgeStates(edgeStates) {
    let moves = [];
    for (const [i, edgeState] of edgeStates.entries()) {
        if (edgeState != EDGE_UNKNOWN) {
            moves.push(`${i}${EDGE_CHAR_BY_STATE[edgeState]}`);
        }
    }
    return joinMoves(moves);
}

function modifyEdgeState(edgeState, direction) {
    const result = edgeState + direction;
    if (result < EDGE_NO || result > EDGE_YES) {
        return edgeState - direction;
    }
    return result;
}

function decodeSaveFile(saveFile) {
    const entries = {};
    for (const match of [...saveFile.matchAll(/^\s*(\S+)\s*:\d+:(\S*)/gm)]) {
        const name = match[1];
        entries[name] ??= [];
        entries[name].push(match[2]);
    }
    const moves = joinMoves(entries.MOVES);
    return `#${entries.PARAMS[0]}:${entries.DESC[0]}:${moves}`;
}

// Removes the moves from the hash to obtain the "#<game-id>".
function stripMovesFromHash(hash) {
    return /[^:]*(:[^:]*)?/.exec(hash)[0];
}

function decodeHash(hash) {
    const decodedHash = window.decodeURIComponent(hash);
    const match = /#(\d+)x(\d+)t(\d+)(?:d.)?(?::([^:]*))?(?::([^:]*))?/
          .exec(decodedHash);
    const params = {
        width: +match[1],
        height: +match[2],
        type: +match[3],
    };
    const desc = match[4];
    const moves = match[5];
    if (params.type != 0) {
        throw `Unsupported grid type ${params.type}`;
    }
    const faceClues = [];
    for (const char of [...desc]) {
        const code = char.charCodeAt(0);
        if (code > ALPHABETIC_BASE) {
            faceClues.push(...Array(code - ALPHABETIC_BASE).fill(null));
        } else {
            faceClues.push(+char);
        }
    }
    const grid = buildGrid(params);
    const edgeStates = Array(grid.edges.length).fill(EDGE_UNKNOWN);
    decodeEdgeStates(moves || "", edgeStates);
    return {desc, grid, faceClues, edgeStates};
}

function percentEncodeFragment(str) {
    // https://stackoverflow.com/a/26119120
    return encodeURIComponent(str)
        .replace(/%(3A)/g, (str, hex) =>
            String.fromCharCode(parseInt(hex, 16)));
}

function encodeHash(state) {
    const moves = encodeEdgeStates(state.edgeStates);
    const {width, height, type} = state.grid.params;
    const code = `${width}x${height}t${type}:${state.desc}:${moves}`;
    return "#" + percentEncodeFragment(code);
}

function constructSquares(width, height) {
    const faceSpecs = [];
    for (let j = 0; j < height; ++j) {
        for (let i = 0; i < width; ++i) {
            faceSpecs.push({
                x: i + 0.5,
                y: j + 0.5,
                vertexSpecs: [
                    {x: i, y: j},
                    {x: i + 1, y: j},
                    {x: i + 1, y: j + 1},
                    {x: i, y: j + 1},
                ],
            });
        }
    }
    return faceSpecs;
}

function selectConstructor(gridType) {
    switch (gridType) {
    case 0:
        return constructSquares;
    default:
        throw `unsupported grid type ${gridType}`;
    }
}

function buildGrid(params) {
    const construct = selectConstructor(params.type);
    const faceSpecs = construct(params.width, params.height);
    const faces = [];
    const vertices = [];
    const vertexIxByCoord = {};
    const edges = [];
    const edgeIxByCoord = {};
    for (const faceSpec of faceSpecs) {
        const faceIx = faces.length;
        const vertexIxs = [];
        for (const {x, y} of faceSpec.vertexSpecs) {
            const coord = [x, y].join(",");
            const vertexIx = getOrInsert(vertexIxByCoord, coord, () => {
                const vertexIx = vertices.length;
                vertices.push({ix: vertexIx, x, y, faceIxs: [], edgeIxs: []});
                return vertexIx;
            });
            vertexIxs.push(vertexIx);
            vertices[vertexIx].faceIxs.push(faceIx);
        }
        const edgeIxs = [];
        for (const [i, vertexIx1] of vertexIxs.entries()) {
            const vertexIx2 = vertexIxs[(i + 1) % vertexIxs.length];
            const coord = [vertexIx1, vertexIx2].sort().join(",");
            const edgeIx = getOrInsert(edgeIxByCoord, coord, () => {
                const edgeIx = edges.length;
                // For a planar graph, faceIxs is either:
                //
                // - one element, for external edges; or
                // - two elements, for internal edges.
                edges.push({ix: edgeIx, vertexIx1, vertexIx2, faceIxs: []});
                vertices[vertexIx1].edgeIxs.push(edgeIx);
                vertices[vertexIx2].edgeIxs.push(edgeIx);
                return edgeIx;
            });
            edgeIxs.push(edgeIx);
            const edgeFaceIxs = edges[edgeIx].faceIxs;
            edgeFaceIxs.push(faceIx);
            if (edgeFaceIxs.length > 2) {
                throw "edge is nonplanar";
            }
        }
        faces.push({
            ix: faceIx,
            x: faceSpec.x,
            y: faceSpec.y,
            edgeIxs,
            vertexIxs,
        });
    }
    return {params, edges, faces, vertices};
}

function modifyEdgeInState(state, edgeIx, direction) {
    const edgeStates = state.edgeStates.slice();
    edgeStates[edgeIx] = modifyEdgeState(edgeStates[edgeIx], direction);
    return Object.assign({}, state, {edgeStates});
}

function updateEdgeInGame(game, edgeIx, direction) {
    let newState = modifyEdgeInState(game.state, edgeIx, direction);
    const autoReductionEnabled =
          document.getElementById("auto-reduction-checkbox").checked;
    if (autoReductionEnabled && findViolations(newState).valid) {
        const reducedState = reduceState(newState);
        if (reducedState.edgeStates[edgeIx] == newState.edgeStates[edgeIx]
            && findViolations(reducedState).valid) {
            newState = reducedState;
        }
    }
    updateGameState(game, newState);
}

const VISUAL_LINE_THICKNESS = 0.008;
const INTERACTIVE_LINE_THICKNESS = 0.03;

function drawGrid(game) {
    const state = game.state;
    const grid = state.grid;
    const params = grid.params;
    const gridSize = Math.min(params.width, params.height);
    function transformX(x) { return x / params.width * 0.90 + 0.05; }
    function transformY(y) { return y / params.height * 0.90 + 0.05; }
    const g = svgElement("g");
    const faceTexts = [];
    for (const face of grid.faces) {
        const clue = state.faceClues[face.ix];
        const text = svgElement("text", {
            attributes: {
                "style": `font-size: 0.05px;`,
                "text-anchor": "middle",
                "dominant-baseline": "middle",
                "x": transformX(face.x),
                "y": transformY(face.y),
            },
            children: {"text": clue == null ? "" : clue.toString()},
        });
        faceTexts.push(text);
        g.append(text);
    }
    const lowerLines = [];
    const upperLines = [];
    const interactiveLines = [];
    for (const edge of grid.edges) {
        const vertex1 = grid.vertices[edge.vertexIx1];
        const vertex2 = grid.vertices[edge.vertexIx2];
        lowerLines.push(svgElement("line", {
            attributes: {
                "x1": transformX(vertex1.x),
                "y1": transformY(vertex1.y),
                "x2": transformX(vertex2.x),
                "y2": transformY(vertex2.y),
                "stroke-linecap": "round",
                "stroke-width": VISUAL_LINE_THICKNESS,
            },
        }));
        upperLines.push(svgElement("line", {
            attributes: {
                "x1": transformX(vertex1.x),
                "y1": transformY(vertex1.y),
                "x2": transformX(vertex2.x),
                "y2": transformY(vertex2.y),
                "stroke-linecap": "round",
                "stroke-width": VISUAL_LINE_THICKNESS,
            },
        }));
        const edgeLength = Math.sqrt(
            Math.pow(transformX(vertex1.x) - transformX(vertex2.x), 2)
            + Math.pow(transformY(vertex1.y) - transformY(vertex2.y), 2));
        const mixRatio =
              (VISUAL_LINE_THICKNESS + INTERACTIVE_LINE_THICKNESS)
              / 2 / edgeLength;
        interactiveLines.push(svgElement("line", {
            attributes: {
                "x1": mixRatio * transformX(vertex1.x)
                    + (1 - mixRatio) * transformX(vertex2.x),
                "y1": mixRatio * transformY(vertex1.y)
                    + (1 - mixRatio) * transformY(vertex2.y),
                "x2": mixRatio * transformX(vertex2.x)
                    + (1 - mixRatio) * transformX(vertex1.x),
                "y2": mixRatio * transformY(vertex2.y)
                    + (1 - mixRatio) * transformY(vertex1.y),
                "class": "interactive-line",
                "stroke-linecap": "round",
                "stroke-width": INTERACTIVE_LINE_THICKNESS,
            },
            listeners: {
                "click": event => updateEdgeInGame(game, edge.ix, +1),
                "contextmenu": event => updateEdgeInGame(game, edge.ix, -1),
            },
        }));
    }
    g.append(...lowerLines, ...upperLines, ...interactiveLines);
    game.svg.replaceChildren(g);
    game.state = Object.assign({}, state, {faceTexts, upperLines, lowerLines});
}

function minEdgeStateCount(edgeState) { return edgeState == EDGE_YES; }
function maxEdgeStateCount(edgeState) { return edgeState != EDGE_NO; }

function tallyEdges(edgeStates, edgeIxs) {
    let min = 0;
    let max = 0;
    for (const edgeIx of edgeIxs) {
        min += minEdgeStateCount(edgeStates[edgeIx]);
        max += maxEdgeStateCount(edgeStates[edgeIx]);
    }
    return {min, max};
}

function findViolations(state) {
    const violatedFaceIxs = {};
    const violatedEdgeIxs = {};

    // check if solution is consistent with face clues
    for (const face of state.grid.faces) {
        const clue = state.faceClues[face.ix];
        if (clue != null) {
            const tally = tallyEdges(state.edgeStates, face.edgeIxs);
            if (!(clue >= tally.min && clue <= tally.max)) {
                violatedFaceIxs[face.ix] = true;
            }
        }
    }
    // check if solution is consistent with the vertex rules
    for (const vertex of state.grid.vertices) {
        const tally = tallyEdges(state.edgeStates, vertex.edgeIxs);
        if (!(0 >= tally.min && 0 <= tally.max)
            && !(2 >= tally.min && 2 <= tally.max)) {
            for (const edgeIx of vertex.edgeIxs) {
                violatedEdgeIxs[edgeIx] = true;
            }
        }
    }
    // check if number of populated, connected components is at most one
    const visited = {};
    const components = [];
    for (const edge0 of state.grid.edges) {
        let populated = false;          // has at least one edge set
        const component = [];
        const stack = [edge0.ix];
        while (stack.length != 0) {
            const edgeIx = stack.pop();
            if (visited[edgeIx]) {
                continue;
            }
            visited[edgeIx] = true;
            if (state.edgeStates[edgeIx] == EDGE_NO) {
                continue;
            }
            component.push(edgeIx);
            if (state.edgeStates[edgeIx] == EDGE_YES) {
                populated = true;
            }
            const edge = state.grid.edges[edgeIx];
            for (const vertexIx of [edge.vertexIx1, edge.vertexIx2]) {
                stack.push(...state.grid.vertices[vertexIx].edgeIxs);
            }
        }
        if (populated) {
            components.push(component);
        }
    }
    if (components.length > 1) {
        for (const component of components) {
            for (const edgeIx of component) {
                violatedEdgeIxs[edgeIx] = true;
            }
        }
    }
    const valid =
          Object.keys(violatedEdgeIxs).length
          + Object.keys(violatedFaceIxs).length
          == 0;
    return {valid, violatedEdgeIxs, violatedFaceIxs};
}

function applyVertexRules(grid, edgeStates) {
    let changed = false;
    for (const vertex of grid.vertices) {
        const tally = tallyEdges(edgeStates, vertex.edgeIxs);
        if (tally.min == 2 || tally.max == 1) {
            for (const edgeIx of vertex.edgeIxs) {
                if (edgeStates[edgeIx] == EDGE_UNKNOWN) {
                    edgeStates[edgeIx] = EDGE_NO;
                    changed = true;
                }
            }
        }
        if (tally.min == 1 && tally.max == 2) {
            for (const edgeIx of vertex.edgeIxs) {
                if (edgeStates[edgeIx] == EDGE_UNKNOWN) {
                    edgeStates[edgeIx] = EDGE_YES;
                    changed = true;
                }
            }
        }
    }
    return changed;
}

function reduceState(state) {
    const edgeStates = state.edgeStates.slice();
    while (true) {
        let changed = false;
        for (const face of state.grid.faces) {
            const clue = state.faceClues[face.ix];
            if (clue == null) {
                continue;
            }
            const tally = tallyEdges(edgeStates, face.edgeIxs);
            if (tally.max == clue) {
                for (const edgeIx of face.edgeIxs) {
                    if (edgeStates[edgeIx] == EDGE_UNKNOWN) {
                        edgeStates[edgeIx] = EDGE_YES;
                        changed = true;
                    }
                }
            }
            if (tally.min == clue) {
                for (const edgeIx of face.edgeIxs) {
                    if (edgeStates[edgeIx] == EDGE_UNKNOWN) {
                        edgeStates[edgeIx] = EDGE_NO;
                        changed = true;
                    }
                }
            }
        }
        changed ||= applyVertexRules(state.grid, edgeStates);
        if (!changed) {
            break;
        }
    }
    return Object.assign({}, state, {edgeStates});
}

function redrawState(state) {
    const violations = findViolations(state);
    for (const [edgeIx, edgeState] of state.edgeStates.entries()) {
        state.lowerLines[edgeIx].setAttribute(
            "class",
            edgeState == EDGE_UNKNOWN ? "edge-unknown" : "",
        );
        state.upperLines[edgeIx].setAttribute(
            "class",
            edgeState != EDGE_YES ? "" :
                violations.violatedEdgeIxs[edgeIx] ? "edge-bad" : "edge",
        );
    }
    for (const [faceIx, text] of state.faceTexts.entries()) {
        text.setAttribute(
            "class",
            violations.violatedFaceIxs[faceIx] ? "face-bad" : "face",
        );
    }
}

function updateGameState(game, state, options) {
    options ??= {};
    game.state = state;
    if (!options.skipHash) {
        game.hashLoader.update(encodeHash(state));
    }
    redrawState(state);
}

function listUnsolvedEdgeIxs(state) {
    const unsolvedEdgeIxs = [];
    for (const [edgeIx, edgeState] of state.edgeStates.entries()) {
        if (edgeState == EDGE_UNKNOWN) {
            unsolvedEdgeIxs.push(edgeIx);
        }
    }
    return unsolvedEdgeIxs;
}

function sleep(delay) {
    return new Promise((resolve, reject) => window.setTimeout(resolve, delay));
}

// Inspired by https://en.wikipedia.org/wiki/DPLL_algorithm
async function solve(ctx, state, depth) {
    const reducedState = reduceState(state);
    if (!await ctx.onProgress(reducedState)) {
        return "STOP";
    }
    if (!findViolations(reducedState).valid) {
        return false;
    }
    const unsolvedEdgeIxs = listUnsolvedEdgeIxs(state);
    const numUnsolvedEdges = unsolvedEdgeIxs.length;
    if (numUnsolvedEdges == 0) {
        return true;
    }

    // Rank the unsolved edges based on how much reduction would occur.
    const score = {};
    for (const edgeIx of unsolvedEdgeIxs) {
        const newUnsolved1 = listUnsolvedEdgeIxs(
            reduceState(modifyEdgeInState(state, edgeIx, +1))).length;
        const newUnsolved2 = listUnsolvedEdgeIxs(
            reduceState(modifyEdgeInState(state, edgeIx, -1))).length;
        score[edgeIx] = Math.min(newUnsolved1, newUnsolved2);
    }
    unsolvedEdgeIxs.sort((a, b) => score[b] - score[a]); // lower is better

    if (depth >= ctx.maxDepth) {
        return null;
    }
    while (unsolvedEdgeIxs.length > 0) {
        const edgeIx = unsolvedEdgeIxs.pop();
        const direction = Math.floor(ctx.random() * 2) * 2 - 1;
        const state1 = modifyEdgeInState(state, edgeIx, direction);
        const state2 = modifyEdgeInState(state, edgeIx, -direction);
        const status1 = await solve(ctx, state1, depth + 1);
        if (status1 == true || status1 == "STOP") {
            return status1;
        }
        const status2 = await solve(ctx, state2, depth + 1);
        if (status2 == true || status2 == "STOP") {
            return status1;
        }
        if (status2 == false && status1 == false) {
            return false;
        }
        if (depth == 0 && status1 == false) {
            ctx.bestState = state2;
            return null;
        }
        if (depth == 0 && status2 == false) {
            ctx.bestState = state1;
            return null;
        }
    }
    return null;
}

async function runSolverStep(game) {
    game.maxDepth ??= 1;
    while (true) {
        let iterations = 0;
        const ctx = {
            random: Math.random,
            maxDepth: game.maxDepth,
            bestState: null,
            async onProgress(state) {
                if (!game.solveCheckbox.checked) {
                    return false;
                }
                ++iterations;
                if (Math.random() < 0.003) {
                    redrawState(state);
                    await sleep(100);    // Let the browser breathe.
                }
                return true;
            },
        };
        const status = await solve(ctx, game.state, 0);
        console.log("Iterations ran:", iterations);
        if (status != null) {
            game.maxDepth = 1;
            return true;
        }
        if (ctx.bestState != null) {
            game.maxDepth = 1;
            updateGameState(game, reduceState(ctx.bestState));
            return false;
        }
        ++game.maxDepth;
        console.log("(!) No progress. Increasing maxDepth to:", game.maxDepth);
    }
}

class HashLoader {
    constructor(initialHash) {
        this._currentHash = null;       // for ignoring self-events
        this._initialHash = initialHash;
    }
    _load(listener) {
        const hash = location.hash || this._initialHash;
        if (this._currentHash == hash) {
            return;
        }
        this._currentHash = hash;
        listener(hash);
    }
    registerAndLoad(window, listener) {
        window.addEventListener("hashchange", event => this._load(listener));
        this._load(listener);
    }
    update(hash) {
        this._currentHash = hash;
        location.hash = hash;
    }
}

function reloadGame(game, state) {
    game.creditLink.href.replace(/#.*/, stripMovesFromHash(encodeHash(state)));
    game.state = state;
    drawGrid(game);
    redrawState(game.state);
}

function waitUntilCheckboxTrue(checkbox) {
    return new Promise(resolve => {
        async function handler() {
            if (checkbox.checked) {
                checkbox.removeEventListener("change", handler);
                resolve();
            }
        };
        checkbox.addEventListener("change", handler);
    });
}

function main() {
    const hashLoader = new HashLoader(DEFAULT_HASH);
    const game = {
        svg: document.getElementById("view"),
        creditLink: document.getElementById("credit-link"),
        hashLoader,
        state: decodeHash(DEFAULT_HASH),
        solveCheckbox: document.getElementById("solve-checkbox"),
    };
    game.svg.addEventListener("contextmenu", event => event.preventDefault());
    hashLoader.registerAndLoad(window, hash => {
        reloadGame(game, decodeHash(hash));
    });
    document.getElementById("reset-button")
        .addEventListener("click", () => {
            location.hash = stripMovesFromHash(location.hash);
            location.reload();
        });
    (async () => {
        while (true) {
            await waitUntilCheckboxTrue(game.solveCheckbox);
            while (!await runSolverStep(game)) {
                await sleep(100);
            }
            redrawState(game.state);
            game.solveCheckbox.checked = false;
        }
    })();
}

main();
