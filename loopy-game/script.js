function getOrInsert(object, property, callback) {
    if (Object.hasOwn(object, property)) {
        return object[property];
    }
    const value = callback();
    object[property] = value;
    return value;
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
const EDGE_STATES = ["n", "u", "y"];
const DEFAULT_HASH = "#10x10t0:a32b23b32a13a2a2a2f22a131h2a121b122c332b2211a1b3a3a1a222a2c312a3223a3c3f";
const MOVE_SEP = "";

function decodeSaveFile(saveFile) {
    const entries = {};
    for (const match of [...saveFile.matchAll(/^\s*(\S+)\s*:\d+:(\S*)/gm)]) {
        const name = match[1];
        entries[name] ??= [];
        entries[name].push(match[2]);
    }
    const moves = entries.MOVES.join(MOVE_SEP);
    return `#${entries.PARAMS[0]}:${entries.DESC[0]}:${moves}`;
}

function decodeEdgeStates(moves, edgeStates) {
    for (const match of moves.matchAll(/(\d+)([nuy])/g)) {
        edgeStates[+match[1]] = match[2];
    }
}

function encodeEdgeStates(edgeStates) {
    let moves = [];
    for (const [i, edgeState] of edgeStates.entries()) {
        if (edgeState != "u") {
            moves.push(`${i}${edgeState}`);
        }
    }
    return moves.join(MOVE_SEP);
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
    const edgeStates = Array(grid.edges.length).fill("u");
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

function modifyEdgeState(state, edgeIx, direction) {
    if (direction == null) {
        return state;
    }
    const edgeStates = state.edgeStates.slice();
    const oldState = edgeStates[edgeIx];
    let newState = EDGE_STATES[EDGE_STATES.indexOf(oldState) + direction];
    if (newState == undefined) {
        newState = EDGE_STATES[EDGE_STATES.indexOf(oldState) - direction];
    }
    edgeStates[edgeIx] = newState;
    return Object.assign({}, state, {edgeStates});
}

function updateEdgeState(state, edgeIx, direction) {
    if (direction == null) {
        return;
    }
    let newState = modifyEdgeState(state, edgeIx, direction);
    const autoReductionEnabled =
          document.getElementById("auto-reduction-checkbox").checked;
    if (autoReductionEnabled && findViolations(newState).valid) {
        const reducedState = reduceState(newState);
        if (reducedState.edgeStates[edgeIx] == newState.edgeStates[edgeIx]) {
            newState = reducedState;
        }
    }
    Object.assign(state, newState);
    updateHash(state);
    redrawState(state);
}

const VISUAL_LINE_THICKNESS = 0.008;
const INTERACTIVE_LINE_THICKNESS = 0.03;

function drawGrid(state, svg) {
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
                "click": event => updateEdgeState(state, edge.ix, +1),
                "contextmenu": event => updateEdgeState(state, edge.ix, -1),
            },
        }));
    }
    g.append(...lowerLines, ...upperLines, ...interactiveLines);
    svg.replaceChildren(g);
    Object.assign(state, {faceTexts, upperLines, lowerLines});
}

function minEdgeStateCount(edgeState) { return edgeState == "y"; }
function maxEdgeStateCount(edgeState) { return edgeState != "n"; }

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
            if (state.edgeStates[edgeIx] == "n") {
                continue;
            }
            component.push(edgeIx);
            if (state.edgeStates[edgeIx] == "y") {
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
                if (edgeStates[edgeIx] == "u") {
                    edgeStates[edgeIx] = "n";
                    changed = true;
                }
            }
        }
        if (tally.min == 1 && tally.max == 2) {
            for (const edgeIx of vertex.edgeIxs) {
                if (edgeStates[edgeIx] == "u") {
                    edgeStates[edgeIx] = "y";
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
                    if (edgeStates[edgeIx] == "u") {
                        edgeStates[edgeIx] = "y";
                        changed = true;
                    }
                }
            }
            if (tally.min == clue) {
                for (const edgeIx of face.edgeIxs) {
                    if (edgeStates[edgeIx] == "u") {
                        edgeStates[edgeIx] = "n";
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
    const newState = Object.assign({}, state, {edgeStates});
    if (!findViolations(newState).valid) {
        return state;
    }
    return newState;
}

function redrawState(state) {
    const violations = findViolations(state);
    for (const [edgeIx, edgeState] of state.edgeStates.entries()) {
        state.lowerLines[edgeIx].style.stroke =
            edgeState == "u" ? "skyblue" : "";
        state.upperLines[edgeIx].style.stroke =
            edgeState != "y" ? "" :
            violations.violatedEdgeIxs[edgeIx] ? "red" : "black";
    }
    for (const [faceIx, text] of state.faceTexts.entries()) {
        text.style.fill = violations.violatedFaceIxs[faceIx] ? "red" : "black";
    }
}

function listUnsolvedEdgeIxs(state) {
    const unsolvedEdgeIxs = [];
    for (const [edgeIx, edgeState] of state.edgeStates.entries()) {
        if (edgeState == "u") {
            unsolvedEdgeIxs.push(edgeIx);
        }
    }
    return unsolvedEdgeIxs;
}

let currentHash = null;                 // for ignoring events from ourself

function updateHash(state) {
    location.hash = encodeHash(state);
    currentHash = location.hash;
}

function reloadFromHash(svg) {
    let hash = location.hash || DEFAULT_HASH;
    if (currentHash == hash) {
        return;
    }
    document.getElementById("credit-link").href
        .replace(/#.*/, stripMovesFromHash(hash));
    const state = decodeHash(hash);
    drawGrid(state, svg);
    redrawState(state);
    currentHash = hash;
}

function main() {
    const svg = document.getElementById("view");
    svg.addEventListener("contextmenu", event => event.preventDefault());
    window.addEventListener("hashchange", event => reloadFromHash(svg));
    reloadFromHash(svg);
    document.getElementById("reset-button")
        .addEventListener("click", event => {
            location.hash = stripMovesFromHash(location.hash);
        });
}

main();
