"use strict"

//////////////////////////////////////////////////////////////////////////////
// Utility

function identity(x) {
    return x
}

function sgn(x) {
    return x > 0 ? 1 : x < 0 ? -1 : 0
}

// Floored modulo
function mod(x, y) {
    return (x % y + y) % y
}

function clamp(min, max, x) {
    if (x < min) {
        return min
    } else if (x > max) {
        return max
    } else {
        return x
    }
}

function round(dx, x) {
    return x + dx / 2 - mod(x + dx / 2, dx)
}

function roundIf(cond, dx, x) {
    return cond ? round(dx, x) : x
}

function sortTwo(x, y) {
    if (x < y) {
        return [x, y]
    } else {
        return [y, x]
    }
}

function intercalate(sep, xs) {
    let ys = []
    xs.forEach((x, i) => {
        if (i != 0) {
            ys.push(sep)
        }
        ys.push(x)
    })
    return ys
}

function arrayEqual(xs, ys) {
    let length = xs.length
    if (length != ys.length) {
        return false
    }
    for (let i = 0; i < length; ++i) {
        if (xs[i] != ys[i]) {
            return false
        }
    }
    return true
}

let currentId = 0
const idMap = new WeakMap()
function id(object) {
    if (!idMap.has(object)) {
        idMap.set(object, ++currentId)
    }
    return idMap.get(object)
}

function deduplicateObject(object, image) {
    const ks = Object.keys(object)
    if (ks.length != Object.keys(image).length) {
        return object
    }
    ks.forEach(k => {
        if (!(image.hasOwnProperty(k) && object[k] === image[k])) {
            return object
        }
    })
    return image
}

function arrayRemove(xs, indices) {
    xs = Array.from(xs)
    Array.from(indices)
              .sort((x, y) => y - x)
              .forEach(i => xs.splice(i, 1))
    return xs
}

function deepClone(x) {
    return JSON.parse(JSON.stringify(x))
}

// Get random integer within [min, max).
function getRandomInt(min, max) {
    min = Math.ceil(min)
    max = Math.floor(max)
    return Math.floor(Math.random() * (max - min)) + min
}

//////////////////////////////////////////////////////////////////////////////
// Geometry

function linePointDistance(x1, y1, x2, y2, x0, y0) {
    const rx = x0 - x1
    const ry = y0 - y1
    const lx = x2 - x1
    const ly = y2 - y1
    const l = Math.sqrt(lx * lx + ly * ly)
    const proj = (rx * lx + ry * ly) / l
    if (proj < l) {
        // projection is within segment
        return Math.abs(ly * x0 - lx * y0 + x2 * y1 - y2 * x1) / l
    } else if (proj < 0) {
        // projection is to the left of segment
        return Math.sqrt(rx * rx + ry * ry)
    } else {
        // projection is to the right of segment
        return Math.sqrt(Math.pow(x0 - x2, 2) + Math.pow(y0 - y2, 2))
    }
}

function arcInfo(lineLength, arcHeight) {
    if (arcHeight == 0.0) {
        return {
            inclination: 0.0,
            radius: Infinity,
            large: false,
            sweep: false,
        }
    }
    const halfLength = lineLength / 2
    const radius = (halfLength * halfLength / arcHeight + arcHeight) / 2
    const large = Math.abs(arcHeight) > halfLength
    const sweep = arcHeight < 0
    const theta = Math.asin(halfLength / radius)
    return {
        inclination: (large ? (sweep ? -1 : 1) * Math.PI - theta : theta),
        radius: radius,
        large: large,
        sweep: sweep,
    }
}

// Get the height of the arc between 1 and 2 that also passes through 0.
function threePointArc(x0, y0, x1, y1, x2, y2) {
    const ax = x1 - x2
    const ay = y1 - y2
    const bx = x2 - x0
    const by = y2 - y0
    const cx = x0 - x1
    const cy = y0 - y1
    const det = ay * cx - cy * ax
    if (det == 0) {
        return 0.0
    }
    const t = (cx * bx + cy * by) / det
    const radius = 0.5 * Math.sqrt(Math.pow(ax - ay * t, 2)
                                 + Math.pow(ay + ax * t, 2))
    const side = -sgn(ay * bx - ax * by)
    const arcHeight = side * radius - t / 2 * Math.sqrt(ax * ax + ay * ay)
    return arcHeight
}

// Choose smoothness = 1 for a Catmull-Rom spline.
function cardinalSpline(xs, ys, smoothness) {
    const length = xs.length
    if (length != ys.length) {
        throw new Error("xs and ys must have same length")
    }
    let d = ""
    let prevSecantX, prevSecantY
    for (let i = 0; i < length; ++i) {
        if (i == 0) {
            // do nothing
            d += `M ${xs[i]} ${ys[i]} `
            continue
        }
        if (i == 1 && i == length - 1) {
            d += `L ${xs[i]} ${ys[i]} `
            continue
        }
        const x01 = xs[i] - xs[i - 1]
        const y01 = ys[i] - ys[i - 1]
        const d01 = Math.sqrt(x01 * x01 + y01 * y01)
        const x12 = xs[i + 1] - xs[i]
        const y12 = ys[i + 1] - ys[i]
        const d12 = Math.sqrt(x12 * x12 + y12 * y12)
        let secantX = smoothness * (xs[i + 1] - xs[i - 1]) / (d01 + d12)
        let secantY = smoothness * (ys[i + 1] - ys[i - 1]) / (d01 + d12)
        if (i == 1) {
            // natural condition (i.e. second derivative must vanish)
            prevSecantX = (3.0 * x01 / d01 - secantX) / 2.0
            prevSecantY = (3.0 * y01 / d01 - secantY) / 2.0
        } else if (i == length - 1) {
            // natural condition (i.e. second derivative must vanish)
            secantX = (3.0 * x01 / d01 - prevSecantX) / 2.0
            secantY = (3.0 * y01 / d01 - prevSecantY) / 2.0
        }
        const control1X = xs[i - 1] + prevSecantX * d01 / 3.0
        const control1Y = ys[i - 1] + prevSecantY * d01 / 3.0
        const control2X = xs[i] - secantX * d01 / 3.0
        const control2Y = ys[i] - secantY * d01 / 3.0
        d += `C ${control1X} ${control1Y} ${control2X} ${control2Y} `
           + `${xs[i]} ${ys[i]}`
        prevSecantX = secantX
        prevSecantY = secantY
    }
    return d
}

//////////////////////////////////////////////////////////////////////////////
// DOM manipulation

function childNodesOf() {
    return this.childNodes
}

function removeChildren(element) {
    while (element.firstChild) {
        element.removeChild(element.firstChild)
    }
}

const VNODE_KEY = Symbol("VNODE_KEY")
const VNODE_SYMBOLS = Symbol("VNODE_SYMBOLS")
const VNODE_EVENT_LISTENERS = Symbol("VNODE_EVENT_LISTENERS")

function vnodeAmendAttributes(attrs, elem) {
    let listeners = elem[VNODE_EVENT_LISTENERS]
    if (listeners) {
        let i = listeners.length
        while (i--) {
            const eventListener = listeners[i]
            elem.removeEventListener(eventListener[0],
                                     eventListener[1])
        }
        listeners.length = 0
    }
    const keys = Object.keys(attrs)
    let i = keys.length
    while (i--) {
        const k = keys[i]
        const v = attrs[k]
        let m = /on(.+)/.exec(k)
        if (m) {
            const event = m[1]
            elem.addEventListener(event, v)
            if (!listeners) {
                listeners = []
                elem[VNODE_EVENT_LISTENERS] = listeners
            }
            listeners.push([event, v])
        } else if (v == null) {
            elem.removeAttribute(k, v)
        } else if (typeof(k) == "symbol") {
            let symbols = elem[VNODE_SYMBOLS]
            if (!symbols) {
                elem[VNODE_SYMBOLS] = {}
            }
            symbols[k] = v
        } else {
            if (elem.getAttribute(k) != v) {
                elem.setAttribute(k, v)
            }
        }
    }
}

function vnodeRenderAttributes(attrs, elem) {
    if (!attrs) {
        return
    }
    let oldAttrs = elem.attributes
    if (oldAttrs) {
        let i = 0
        while (i < oldAttrs.length) {
            const oldAttr = oldAttrs[i]
            const name = oldAttr.name
            if (!attrs.hasOwnProperty(name) && oldAttr.ns == null) {
                oldAttrs.removeNamedItem(name)
            } else {
                ++i
            }
        }
    }
    delete elem[VNODE_SYMBOLS]
    vnodeAmendAttributes(attrs, elem)
}

function vnodeRenderChildren(children, elem) {
    if (!children) {
        return
    }
    let oldChildren = elem.childNodes
    if (!oldChildren) {
        return
    }
    // fragment contains a queue of pending elements; if you're about
    // to increment j without removing an old node, then you should
    // flush this queue before moving on!
    let fragment = document.createDocumentFragment()
    let j = 0
    for (let i = 0; i < children.length; ++i) {
        let child = children[i]
        let oldChild = oldChildren[j]
        if (child instanceof Vnode) {
            const key = child.attributes[VNODE_KEY]
            if (key) {
                // find matching node, if any
                while (oldChild) {
                    if (oldChild instanceof Element ||
                        oldChild instanceof Text) {
                        if (oldChild[VNODE_KEY] == key) {
                            break
                        }
                        elem.removeChild(oldChild)
                    } else {
                        elem.insertBefore(fragment, oldChild)
                        ++j
                    }
                    oldChild = oldChildren[j]
                }
            } else if (oldChild && oldChild[VNODE_KEY]) {
                // not a candidate for replacement; try again later
                oldChild = null
            }
            if (oldChild) {
                // found a candiate; can we update without replacing?
                // (note: XML is case-sensitive; HTML is not)
                if (oldChild instanceof HTMLElement
                    ? (oldChild.nodeName.toLowerCase()
                        == child.name.toLowerCase())
                    : (oldChild.namespaceURI == child.namespace &&
                       oldChild.nodeName == child.name)) {
                    elem.insertBefore(fragment, oldChild)
                    ++j
                    child.renderTo(oldChild)
                    continue
                } else {
                    elem.removeChild(oldChild)
                }
            }
            child = child.create()
            child[VNODE_KEY] = key
        } else if (typeof(child) == "string") {
            if (oldChild instanceof Text) {
                elem.insertBefore(fragment, oldChild)
                ++j
                if (oldChild.nodeValue != child) {
                    oldChild.nodeValue = child
                }
                continue
            } else {
                child = document.createTextNode(child)
            }
        }
        fragment.appendChild(child)
    }
    // remove remaining nodes
    while (j < oldChildren.length) {
        let oldChild = oldChildren[j]
        if (oldChild instanceof Element ||
            oldChild instanceof Text) {
            elem.removeChild(oldChild)
        } else {
            ++j
        }
    }
    elem.appendChild(fragment)
}

class Vnode {
    constructor(namespace, name, attributes, children) {
        this.namespace = namespace
        this.name = name
        this.attributes = attributes
        this.children = children
    }

    // Copy the attributes and children to the given element.
    // Note: the behavior is unspecified if the element is of a different name.
    renderTo(elem) {
        vnodeRenderAttributes(this.attributes, elem)
        vnodeRenderChildren(this.children, elem)
    }

    create() {
        const namespace = this.namespace
        let elem = namespace
                 ? document.createElementNS(namespace, this.name)
                 : document.createElement(this.name)
        this.renderTo(elem)
        return elem
    }
}

const NAMESPACES = {
    svg: "http://www.w3.org/2000/svg",
}

function parseNamespace(name) {
    let namespace = null
    const m = /([^:]+):(.+)/.exec(name)
    if (m && NAMESPACES.hasOwnProperty(m[1])) {
        namespace = NAMESPACES[m[1]]
        name = m[2]
    }
    return [namespace, name]
}

function vnode(name, attributes, ...children) {
    let namespace
    ;[namespace, name] = parseNamespace(name)
    return new Vnode(namespace, name, attributes, children)
}

function applyRendering(rendering) {
    rendering.forEach(spec => {
        const elem = spec.element
        if (spec.attributes) {
            vnodeAmendAttributes(spec.attributes, elem)
        }
        vnodeRenderChildren(spec.children, elem)
    })
}

//////////////////////////////////////////////////////////////////////////////
// Superline manipulation

const ZERO_J = {type: "zero"}

function isZeroJ(j) {
    return typeof j == "object" && j.type == "zero"
}

const EMPTY_SUPERLINE = {
    phase: 0,
    summed: false,
    weight: 0,
}

// NOTE: must maintain invariant that terminals precede all other nodes.
// Also, the order of nodes is critical!  If you move the nodes around,
// make sure the lines are also reversed.

function availSuperlineLabels(diagram, count) {
    // avoid 0, which might get confused for j = 0
    let counter = 1
    let labels = []
    while (labels.length < count) {
        while (diagram.superlines.hasOwnProperty(counter.toString())) {
            counter += 1
        }
        labels.push(counter.toString())
        counter += 1
    }
    return labels
}

function newLabel(label) {
    const match = /^([\s\S]*?)(\d*)$/.exec(label)
    return match[1] + (Number(match[2]) + 1).toString()
}

function changePhase(superline, phase) {
    superline.phase = mod(superline.phase + phase, 4)
}

function mergeSuperlines(superline1, superline2) {
    superline1 = Object.assign({}, EMPTY_SUPERLINE, superline1)
    superline2 = Object.assign({}, EMPTY_SUPERLINE, superline2)
    changePhase(superline1, superline2.phase)
    superline1.weight += superline2.weight
    superline1.summed |= superline2.summed
    return superline1
}

function addDelta(deltas, ...entries) {
    if (entries.length < 2) {
        return deltas
    }
    deltas = Array.from(deltas)
    for (let i = 0; i < deltas.length; ++i) {
        for (let j = 0; j < entries.length; ++j) {
            if (deltas[i].includes(entries[j])) {
                deltas[i] = Array.from(deltas[i])
                entries.forEach(x => {
                    if (!deltas[i].includes(x)) {
                        deltas[i].push(x)
                    }
                })
                return deltas
            }
        }
    }
    deltas.push(entries)
    return deltas
}

//////////////////////////////////////////////////////////////////////////////
// Line manipulation

// type Line = {
//   superline: String,
//   direction: -1 (left) | 0 | 1 (right),
//   arrowPos: 0.0 (left) to 1.0 (right),
//   arcHeight: -INF to INF, (sagitta, + is downward)
//   angle: 0 to 2PI, (of the direct line, clockwise)
//   textPos: 0.0 (left) to 1.0 (right),
//   textOffset: -INF to INF, (+ is downward)
// }
//
// The angle is usually ignored, but if the arcHeight/lineLength is too
// big, then angle is used to break the degeneracy.

function reverseLine(line) {
    line = Object.assign({}, line)
    line.direction *= -1
    line.arrowPos = 1.0 - line.arrowPos
    line.arcHeight *= -1
    line.angle = mod(line.angle + Math.PI, 2 * Math.PI)
    line.textPos = 1.0 - line.textPos
    line.textOffset *= -1.0
    return line
}

function plainLine(superlineId) {
    return {
        superline: superlineId,
        direction: 0,
        arrowPos: 0.5,
        arcHeight: 0.0,
        angle: 0.0,
        textPos: 0.5,
        textOffset: 0.0,
    }
}

function canonicalizeLine(line) {
    return {
        line: Object.assign({}, line, {direction: line.direction % 2}),
        phase: mod(Math.trunc(line.direction / 2), 2) * 2,
    }
}

function joinLines(line1, reverse1, line2, reverse2) {
    if (reverse1) {
        return joinLines(reverseLine(line1), false, line2, reverse2)
    }
    if (reverse2) {
        return joinLines(line1, reverse1, reverseLine(line2), false)
    }
    const superlines = sortTwo(line1.superline, line2.superline)
    return Object.assign(canonicalizeLine({
        superline: superlines[0],
        direction: line1.direction + line2.direction,
        arrowPos: (line1.arrowPos + line2.arrowPos) / 2,
        arcHeight: (line1.arcHeight + line2.arcHeight) / 2,
        angle: Math.atan2(Math.sin(line1.angle) + Math.sin(line2.angle),
                          Math.cos(line1.angle) + Math.cos(line2.angle)),
        textPos: (line1.arrowPos + line2.arrowPos) / 2,
        textOffset: line1.textOffset + line2.textOffset,
    }), {otherSuperline: superlines[1]})
}

function getLineInfo(diagram, lineId) {
    const line = diagram.lines[lineId]
    const ends = endNodeIndices(diagram.nodes, lineId)
    let x0 = diagram.nodes[ends[0]].x
    let y0 = diagram.nodes[ends[0]].y
    let x1 = diagram.nodes[ends[1]].x
    let y1 = diagram.nodes[ends[1]].y
    const xMid = (x0 + x1) / 2
    const yMid = (y0 + y1) / 2
    let dx = x1 - x0
    let dy = y1 - y0
    let lineLength = Math.sqrt(dx * dx + dy * dy)
    const singular = lineLength == 0.0
    const trueAngle = Math.atan2(dy, dx)
    let angle = trueAngle
    if (singular) {
        // fudge numbers to avoid singularity
        // (epsilon can't be too small or the SVG rendering becomes jittery)
        const epsilon = 1e-2
        dx = epsilon * Math.cos(line.angle)
        dy = epsilon * Math.sin(line.angle)
        x0 = xMid - 0.5 * dx
        y0 = yMid - 0.5 * dy
        x1 = xMid + 0.5 * dx
        y1 = yMid + 0.5 * dy
        lineLength = epsilon
        angle = line.angle
    }
    const arc = arcInfo(lineLength, line.arcHeight)
    const c = (arc.radius - line.arcHeight) / lineLength
    const xCenter = xMid + c * dy
    const yCenter = yMid - c * dx
    const arcEx = {
        xCenter: xCenter,
        yCenter: yCenter,
        startAngle: Math.atan2(y0 - yCenter, x0 - xCenter),
    }
    return {
        x0: x0,
        y0: y0,
        x1: x1,
        y1: y1,
        xMid: xMid,
        yMid: yMid,
        dx: dx,
        dy: dy,
        angle: angle,
        trueAngle: trueAngle,
        lineLength: lineLength,
        singular: singular,
        line: line,
        arc: Object.assign(arc, arcEx),
    }
}

function findPosOnLine(lineInfo, x, y) {
    let pos, offset
    if (lineInfo.line.arcHeight == 0.0) {
        const rx = x - lineInfo.x0
        const ry = y - lineInfo.y0
        pos = (lineInfo.dx * rx + lineInfo.dy * ry)
        / Math.pow(lineInfo.lineLength, 2)
        // left-handed coordinate system!
        offset = -(rx * lineInfo.dy - ry * lineInfo.dx) / lineInfo.lineLength
    } else {
        const arc = lineInfo.arc
        const rx = x - arc.xCenter
        const ry = y - arc.yCenter
        const cycle = Math.abs(Math.PI / arc.inclination)
        const rawAngle = arc.startAngle - Math.atan2(ry, rx)
        const shift = 0.5 * cycle - 0.5 // remove the bias toward pos = 1.0
        pos = mod(rawAngle / (2 * arc.inclination) + shift, cycle) - shift
        offset = (Math.sqrt(rx * rx + ry * ry) - Math.abs(arc.radius))
        * sgn(lineInfo.line.arcHeight)
    }
    return {
        pos: pos,
        offset: offset,
    }
}

function positionOnLine(lineInfo, pos, shift) {
    const arc = lineInfo.arc
    if (lineInfo.line.arcHeight == 0.0) {
        const lineLength = lineInfo.lineLength
        const newPos = pos + shift / lineLength
        return {
            x: lineInfo.x0 + lineInfo.dx * newPos,
            y: lineInfo.y0 + lineInfo.dy * newPos,
            normalX: -lineInfo.dy / lineLength,
            normalY: lineInfo.dx / lineLength,
            tangentAngle: lineInfo.angle,
        }
    } else {
        const localAngle = 2 * arc.inclination * pos + shift / arc.radius
        const tangentAngle = lineInfo.angle + arc.inclination - localAngle
        const normalAngle = tangentAngle + Math.PI / 2
        const normalX = Math.cos(normalAngle)
        const normalY = Math.sin(normalAngle)
        return {
            x: arc.xCenter + arc.radius * normalX,
            y: arc.yCenter + arc.radius * normalY,
            normalX: normalX,
            normalY: normalY,
            tangentAngle: tangentAngle,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Node manipulation

function terminalNode(lineId, variable, x, y) {
    // every terminal has an associated name for the "m" variable;
    // because this variable is *free*, rules must never change this variable!
    // (you are free to rename the lineId whatever you want, but try to reuse
    // the user's naming if possible)
    //
    // when rendering lines connected to terminal(s), be aware that the name(s)
    // on the terminal(s) *supersede* the name of the line itself (lineId); if
    // there are two terminals, then an m-delta is implied (this is how we
    // represent m-deltas)
    return Object.freeze({
        type: "terminal",
        lines: Object.freeze([lineId]),
        variable: variable,
        x: x,
        y: y,
    })
}

function endNodeAndLineIndices(nodes, lineId) {
    let nodeAndLineIndices = []
    if (lineId === undefined || lineId === null) {
        throw new Error("lineId must not be null")
    }
    const numNodes = nodes.length
    for (let nodeIndex = 0; nodeIndex < numNodes; ++nodeIndex) {
        const lines = nodes[nodeIndex].lines
        const numLines = lines.length
        for (let lineIndex = 0; lineIndex < numLines; ++lineIndex) {
            const nodeLineId = lines[lineIndex]
            if (nodeLineId == lineId) {
                nodeAndLineIndices.push([nodeIndex, lineIndex])
            }
        }
    }
    if (nodeAndLineIndices.length != 2) {
        throw new Error("line must be connected at 2 points, "
                      + `not ${nodeAndLineIndices.length}`)
    }
    return nodeAndLineIndices
}

function endNodeIndices(nodes, lineId) {
    return endNodeAndLineIndices(nodes, lineId).map(x => x[0])
}

function otherNodeAndLineIndex(nodes, nodeIndex, lineIndex) {
    const lineId = nodes[nodeIndex].lines[lineIndex]
    if (lineId === undefined) {
        throw new Error(`cannot find line with lineIndex = ${lineIndex}`)
    }
    let nodeAndLineIndices = endNodeAndLineIndices(nodes, lineId)
    if (lexicalCmp(nodeAndLineIndices[0],
                   [nodeIndex, lineIndex],
                   defaultCmp) == 0) {
        return nodeAndLineIndices[1]
    } else {
        return nodeAndLineIndices[0]
    }
}

function otherNodeIndex(nodes, nodeIndex, lineIndex) {
    return otherNodeAndLineIndex(nodes, nodeIndex, lineIndex)[0]
}

function isLeftOfLine(nodes, nodeIndex, lineIndex) {
    return lexicalCmp(otherNodeAndLineIndex(nodes, nodeIndex, lineIndex),
                      [nodeIndex, lineIndex], defaultCmp) > 0
}

function nearestNodeIndices(nodes, count, x, y) {
    return nodes
        .map((node, nodeIndex) => ({
            distance: Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2),
            index: nodeIndex
        }))
        .sort((x, y) => x.distance - y.distance)
        .slice(0, count)
        .map(node => node.index)
}

//////////////////////////////////////////////////////////////////////////////
// Diagram manipulation

const EMPTY_DIAGRAM = Object.freeze({
    nodes: Object.freeze([]),
    superlines: Object.freeze({}),
    lines: Object.freeze({}),

    // each entry is an array of j's that are equal to each other
    deltas: Object.freeze([]),

    // each entry is an array of the three j's in ascending order
    triangles: Object.freeze([]),
})

function w3jDiagram(a, b, c, x, y) {
    if (a == b || b == c || c == a) {
        throw new Error("cannot create w3jDiagram with conflicting labels")
    }
    return Object.freeze(Object.assign({}, EMPTY_DIAGRAM, {
        nodes: Object.freeze([
            terminalNode(a, a, x - 50, y + 50),
            terminalNode(b, b, x + 50, y + 50),
            terminalNode(c, c, x, y - 70),
            {
                type: "w3j",
                lines: Object.freeze([a, b, c]),
                x: x,
                y: y,
            },
        ]),
        lines: Object.freeze({
            [a]: plainLine(a),
            [b]: plainLine(b),
            [c]: plainLine(c),
        }),
        superlines: Object.freeze({
            [a]: EMPTY_SUPERLINE,
            [b]: EMPTY_SUPERLINE,
            [c]: EMPTY_SUPERLINE,
        }),
    }))
}

function cgDiagram(a, b, c, x, y) {
    const diagram = w3jDiagram(a, b, c, x, y)
    return Object.freeze(Object.assign({}, diagram, {
        lines: Object.freeze(Object.assign({}, diagram.lines, {
            [c]: Object.freeze(Object.assign({}, diagram.lines[c], {
                direction: 1,
            })),
        })),
        superlines: Object.freeze(Object.assign({}, diagram.superlines, {
            [b]: Object.freeze(Object.assign({}, diagram.superlines[b], {
                phase: 2,
            })),
            [c]: Object.freeze(Object.assign({}, diagram.superlines[c], {
                weight: 1,
            })),
        })),
    }))
}

function setDiagramLineProps(diagram, lineId, props) {
    const lines = diagram.lines
    return Object.freeze(Object.assign({}, diagram, {
        lines: Object.freeze(Object.assign({}, lines, {
            [lineId]: Object.freeze(Object.assign({}, lines[lineId], props)),
        })),
    }))
}

function setDiagramNodeProps(diagram, nodeIndex, props) {
    const nodes = diagram.nodes
    return Object.freeze(Object.assign({}, diagram, {
        nodes: Object.freeze(Object.assign([], nodes, {
            [nodeIndex]: Object.freeze(Object.assign({}, nodes[nodeIndex],
                                                     props)),
        }))
    }))
}

function lineAngle(diagram, nodeIndex, lineIndex) {
    const node = diagram.nodes[nodeIndex]
    const lineId = node.lines[lineIndex]
    const line = diagram.lines[lineId]
    const lineInfo = getLineInfo(diagram, lineId)
    const reverse = Number(isLeftOfLine(diagram.nodes, nodeIndex, lineIndex))
    const baseAngle = (lineInfo.singular ? line.angle : lineInfo.angle)
                    + reverse * Math.PI
    const sign = reverse ? 1 : -1
    return baseAngle + sign * lineInfo.arc.inclination
}

function w3jOrientation(diagram, nodeIndex) {
    const node = diagram.nodes[nodeIndex]
    if (node.type != "w3j") {
        throw new Error("cannot get orientation of generic node")
    }
    const lines = [0, 1, 2]
        .map(function(lineIndex) {
            const angle = lineAngle(diagram, nodeIndex, lineIndex)
            return [lineIndex, mod(angle, 2 * Math.PI)]
        })
        .sort((line1, line2) => line1[1] - line2[1])
    if (lines.map(line => mod(line[0] - lines[0][0], 3)).join() == "0,1,2") {
        return 1
    } else {
        return -1
    }
}

function mergeDiagrams(diagram1, diagram2) {
    let diagram = deepClone(diagram1)
    Object.keys(diagram2.superlines).forEach(function(superlineId) {
        const self = diagram2.superlines[superlineId]
        if (diagram.superlines.hasOwnProperty(superlineId)) {
            diagram.superlines[superlineId] =
                mergeSuperlines(diagram.superlines[superlineId], self)
        } else {
            diagram.superlines[superlineId] = self
        }
    })
    let renames = {}
    Object.keys(diagram2.lines).forEach(function(lineId) {
        let newLineId = lineId
        while (diagram.lines.hasOwnProperty(newLineId)) {
            // name collision
            newLineId = newLabel(newLineId)
        }
        renames[lineId] = newLineId
        diagram.lines[newLineId] = diagram2.lines[lineId]
    })
    let terminals = []
    diagram2.nodes.forEach(function(node) {
        node = Object.assign({}, node, {
            lines: node.lines.map(lineId => renames[lineId]),
        })
        if (node.type == "terminal") {
            terminals.push(node)
        } else {
            diagram.nodes.push(node)
        }
    })
    diagram.nodes = terminals.concat(diagram.nodes)
    return diagram
}

function joinTerminals(diagram, terminalIndex1, terminalIndex2) {
    if (diagram.nodes[terminalIndex1].type != "terminal" ||
        diagram.nodes[terminalIndex2].type != "terminal") {
        throw new Error("cannot join non-terminals")
    }
    let other1 = otherNodeAndLineIndex(diagram.nodes, terminalIndex1, 0)
    let other2 = otherNodeAndLineIndex(diagram.nodes, terminalIndex2, 0)
    if (lexicalCmp(other1, other2, defaultCmp) > 0) {
        // we only handle cases where LEFT < RIGHT
        ;[terminalIndex2, terminalIndex1] = [terminalIndex1, terminalIndex2]
        ;[other2, other1] = [other1, other2]
    }
    diagram = deepClone(diagram)
    let lineId1 = diagram.nodes[terminalIndex1].lines[0]
    let lineId2 = diagram.nodes[terminalIndex2].lines[0]
    if (lineId1 == lineId2) {
        // FIXME loops are not yet implemented
        error("Cannot join terminals sharing the same line (not yet implemented)")
        return diagram
    }

    // merge the lines (be careful with orientation)
    let line1 = diagram.lines[lineId1]
    let line2 = diagram.lines[lineId2]
    line1.angle = getLineInfo(diagram, lineId1).angle
    line2.angle = getLineInfo(diagram, lineId2).angle
    let joined = joinLines(line1, other1[0] > terminalIndex1,
                           line2, terminalIndex2 > other2[0])
    let superlineId1 = joined.line.superline
    let superlineId2 = joined.otherSuperline
    if (other1[0] == other2[0] && joined.line.arcHeight == 0.0) {
        joined.line.arcHeight =
            (getLineInfo(diagram, lineId1).lineLength
           + getLineInfo(diagram, lineId2).lineLength) / 2
    }
    changePhase(diagram.superlines[superlineId1], joined.phase)
    diagram.lines[lineId1] = joined.line
    delete diagram.lines[lineId2]

    // join with other node
    diagram.nodes[other2[0]].lines[other2[1]] = lineId1

    // equate superlines and merge their factors
    diagram.superlines[superlineId1] =
        mergeSuperlines(diagram.superlines[superlineId1],
                        diagram.superlines[superlineId2])
    delete diagram.superlines[superlineId2]

    // delete the terminal nodes
    diagram.nodes = arrayRemove(diagram.nodes, [terminalIndex1, terminalIndex2])

    return diagram
}

function addW1j(diagram, lineId) {
    diagram = deepClone(diagram)
    let line = diagram.lines[lineId]
    // cycle through all possible directions
    if (line.direction > 0) {
        line.direction = -1
    } else if (line.direction < 0) {
        line.direction = 0
    } else {
        line.direction = 1
    }
    return diagram
}

function add2j(diagram, lineId) {
    diagram = deepClone(diagram)
    changePhase(diagram.superlines[diagram.lines[lineId].superline], 2)
    return diagram
}

function flipW3j(diagram, nodeIndex) {
    if (diagram.nodes[nodeIndex].type != "w3j") {
        return diagram
    }
    diagram = deepClone(diagram)
    let lines = diagram.nodes[nodeIndex].lines
    lines.reverse()
    for (let i = 0; i < 3; ++i) {
        if (lines[i] == lines[(i + 1) % 3]) {
            diagram.lines[lines[i]] = reverseLine(diagram.lines[lines[i]])
            break
        }
    }
    return diagram
}

function isDanglingSuperline(diagram, superlineId) {
    const lineIds = Object.keys(diagram.lines)
    let i = lineIds.length
    while (i--) {
        if (diagram.lines[lineIds[i]].superline == superlineId) {
            return false
        }
    }
    return true
}

function deleteNode(diagram, nodeIndex) {
    diagram = deepClone(diagram)
    const node = diagram.nodes[nodeIndex]
    if (node.type == "terminal") {
        const otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, 0)
        if (diagram.nodes[otherIndex].type != "terminal") {
            throw new Error("cannot delete terminal of node")
        }
        const superlineId = diagram.lines[node.lines[0]].superline
        delete diagram.lines[node.lines[0]]
        if (isDanglingSuperline(diagram, superlineId)) {
            delete diagram.superlines[superlineId]
        }
        diagram.nodes = arrayRemove(diagram.nodes, [nodeIndex, otherIndex])
    } else {
        let terminals = []
        node.lines.forEach(function(lineId, lineIndex) {
            const otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex)
            if (otherIndex == nodeIndex) {
                const line = diagram.lines[lineId]
                if (!line) {
                    return
                }
                delete diagram.lines[lineId]
                const superlineId = line.superline
                if (isDanglingSuperline(diagram, superlineId)) {
                    delete diagram.superlines[superlineId]
                }
                return
            }
            if (otherIndex < nodeIndex) {
                diagram.lines[lineId] = reverseLine(diagram.lines[lineId])
            }
            terminals.push(terminalNode(lineId, lineId, node.x, node.y))
        })
        diagram.nodes.splice(nodeIndex, 1)
        diagram.nodes = terminals.concat(diagram.nodes)
    }
    return diagram
}

//////////////////////////////////////////////////////////////////////////////
// Diagrammatic rules
//
// We distinguish between transformations used for editing (which generally
// do not preserve the semantics) and transformations used for derivations
// that always preserve equivalence.  The latter are called "rules".

function flipW3jRule(diagram, nodeIndex) {
    if (diagram.nodes[nodeIndex].type != "w3j") {
        return diagram
    }
    diagram = deepClone(flipW3j(diagram, nodeIndex))
    diagram.nodes[nodeIndex].lines.forEach(function(lineId) {
        const superlineId = diagram.lines[lineId].superline
        diagram.superlines[superlineId] =
            mergeSuperlines(diagram.superlines[superlineId], {
                phase: 1,
                summed: false,
                weight: 0,
            })
    })
    return diagram
}

function trianglePhaseRule(diagram, triangleIndex) {
    diagram = deepClone(diagram)
    diagram.triangles[triangleIndex].forEach(function(superlineId) {
        diagram.superlines[superlineId] =
            mergeSuperlines(diagram.superlines[superlineId], {
                phase: 2,
                summed: false,
                weight: 0,
            })
    })
    return diagram
}

function flipW1jRule(diagram, lineId) {
    if (diagram.lines[lineId].direction) {
        diagram = deepClone(diagram)
        diagram.lines[lineId].direction *= -1
        const superlineId = diagram.lines[lineId].superline
        diagram.superlines[superlineId] =
            mergeSuperlines(diagram.superlines[superlineId], {
                phase: 2,
                summed: false,
                weight: 0,
            })
    }
    return diagram
}

function threeArrowRule(diagram, nodeIndex) {
    diagram = deepClone(diagram)
    let node = diagram.nodes[nodeIndex]
    if (node.type != "w3j") {
        return diagram
    }
    let direction = 0
    // figure out the direction that would minimize the phase change
    node.lines.forEach(function(lineId, lineIndex) {
        const line = diagram.lines[lineId]
        if (isLeftOfLine(diagram.nodes, nodeIndex, lineIndex)) {
            direction -= line.direction
        } else {
            direction += line.direction
        }
    })
    if (direction == 0) {
        // we still don't have a direction, so let's just pick "outgoing"
        direction = 1
    } else if (direction == -3) {
        // if all outgoing, completely reverse the direction
        direction = -2
    } else {
        // normalize to one
        direction = direction / Math.abs(direction)
    }
    node.lines.forEach(function(lineId, lineIndex) {
        let line = diagram.lines[lineId]
        if (isLeftOfLine(diagram.nodes, nodeIndex, lineIndex)) {
            line.direction += direction
        } else {
            line.direction -= direction
        }
        const canonicalized = canonicalizeLine(line)
        diagram.lines[lineId] = canonicalized.line
        changePhase(diagram.superlines[line.superline], canonicalized.phase)
    })
    return diagram
}

function defaultCmp(x, y) {
    if (x < y) {
        return -1
    } else if (y < x) {
        return 1
    } else {
        return 0
    }
}

function lexicalCmp(xs, ys, cmp) {
    const nx = xs.length
    const ny = ys.length
    const n = nx < ny ? nx : ny
    for (let i = 0; i < n; ++i) {
        const r = cmp(xs[i], ys[i])
        if (r) {
            return r
        }
    }
    return defaultCmp(nx, ny)
}

function deltaIntroRule(diagram, nodeIndex) {
    const node = diagram.nodes[nodeIndex]
    if (node.type != "w3j") {
        return "this node is not a Wigner 3-jm symbol"
    }

    // check if the node has a loop
    const nodeLines = node.lines
    let cutIndex, loopLineIndex, expectedLoopDirection
    if (nodeLines[0] == nodeLines[1]) {
        cutIndex = 2
        expectedLoopDirection = +1
        loopLineIndex = nodeLines[0]
    } else if (nodeLines[1] == nodeLines[2]) {
        cutIndex = 0
        expectedLoopDirection = +1
        loopLineIndex = nodeLines[1]
    } else if (nodeLines[2] == nodeLines[0]) {
        cutIndex = 1
        expectedLoopDirection = -1
        loopLineIndex = nodeLines[2]
    } else {
        return "no loops found"
    }

    // make sure the loop is directed
    const loopSuperlineId = diagram.lines[loopLineIndex].superline
    let loopPhase
    switch (diagram.lines[loopLineIndex].direction * expectedLoopDirection) {
        case 1:
            loopPhase = 0
            break
        case -1:
            loopPhase = 2
            break
        default:
            return "loop must be directed"
    }

    const other = otherNodeAndLineIndex(diagram.nodes, nodeIndex, cutIndex)
    const otherNode = diagram.nodes[other[0]]
    if (otherNode.type != "w3j") {
        return "other node is not a Wigner 3-jm symbol"
    }

    // join the lines (this part is really messy)
    const joins = [0, 1, 2]
        .filter(i => i != other[1])
        .map(lineIndex => ({
            lineIndex: lineIndex,
            id: otherNode.lines[lineIndex],
            line: diagram.lines[otherNode.lines[lineIndex]],
            end: isLeftOfLine(diagram.nodes, other[0], lineIndex),
            nl: otherNodeAndLineIndex(diagram.nodes, other[0], lineIndex),
        }))
        .sort((x, y) => lexicalCmp(x.nl, y.nl, defaultCmp))
    const joined = joinLines(joins[0].line, joins[0].end,
                             joins[1].line, !joins[1].end)
    const direction = mod(joins[1].lineIndex - joins[0].lineIndex, 3) == 1
                    ? 1 : -1
    let line = joined.line
    line.direction += direction
    const canonicalized = canonicalizeLine(line)
    let lines = Object.assign({}, diagram.lines)
    delete lines[loopLineIndex]
    delete lines[node.lines[cutIndex]]
    delete lines[joins[1].id]
    lines[joins[0].id] = canonicalized.line

    // update the phase on the loop superline
    let superlines = Object.assign({}, diagram.superlines)
    superlines[loopSuperlineId] = mergeSuperlines(superlines[loopSuperlineId], {
        phase: loopPhase,
        weight: 1,
    })

    // join the other two superlines
    const superline1 = joined.line.superline
    const superline2 = joined.otherSuperline
    if (superline1 != superline2) {
        superlines[superline1] = mergeSuperlines(diagram.superlines[superline1],
                                                 diagram.superlines[superline2])
        superlines[superline2] = EMPTY_SUPERLINE
    }
    superlines[superline1] = mergeSuperlines(superlines[superline1], {
        phase: joined.phase + canonicalized.phase,
        weight: -1,
    })

    // update nodes and other things
    return Object.freeze(Object.assign({}, diagram, {
        nodes: Object.freeze(arrayRemove(Object.assign([], diagram.nodes, {
            [joins[0].nl[0]]: Object.freeze(Object.assign(
                {}, diagram.nodes[joins[0].nl[0]], {
                    lines: Object.freeze(Object.assign(
                        [], diagram.nodes[joins[0].nl[0]].lines, {
                            [joins[0].nl[1]]: joins[0].id,
                        })),
                })),
            [joins[1].nl[0]]: Object.freeze(Object.assign(
                {}, diagram.nodes[joins[1].nl[0]], {
                    lines: Object.freeze(Object.assign(
                        [], diagram.nodes[joins[1].nl[0]].lines, {
                            [joins[1].nl[1]]: joins[0].id,
                        })),
                })),
        }), [nodeIndex, other[0]])),
        lines: Object.freeze(lines),
        superlines: superlines,
        deltas: addDelta(addDelta(diagram.deltas,
                                  superline1,
                                  superline2),
                         ZERO_J,
                         node.lines[cutIndex])
    }))
}

//////////////////////////////////////////////////////////////////////////////
// Drawing

function twoJColor(diagram, lineId) {
    const superlineId = diagram.lines[lineId].superline
    return mod(diagram.superlines[superlineId].phase, 4) >= 2
}

function renderArrow(update, diagram, lineId) {
    const arrowHeadSize = 15
    const line = diagram.lines[lineId]
    if (line.direction == 0) {
        return []
    }
    if (line.arrowPos < 0.0) {
        line.arrowPos = 0.0
    } else if (line.arrowPos > 1.0) {
        line.arrowPos = 1.0
    }
    const info = getLineInfo(diagram, lineId)
    // the coordinate we need is the tip of the arrow (which follows the
    // contour of the line), but we want to try to keep the body of
    // the arrow centered
    const correction = line.direction * arrowHeadSize / 2
    const position = positionOnLine(info, line.arrowPos, correction)
    const rawPosition = positionOnLine(info, line.arrowPos, 0)
    const angle = position.tangentAngle
                + Number(line.direction < 0) * Math.PI
    if (line.direction == 0) {
        return []
    }
    return [vnode("svg:use", {
        "class": "arrow " + (twoJColor(diagram, lineId) ? "two-j " : ""),
        href: "#arrowhead",
        x: -arrowHeadSize,
        y: -arrowHeadSize / 2,
        width: arrowHeadSize,
        height: arrowHeadSize,
        transform: `translate(${position.x}, ${position.y}),`
                 + `rotate(${angle * 180 / Math.PI})`,
        onmousedown: function(e) {
            if (e.buttons == 1) {
                update(startDrag(rawPosition.x, rawPosition.y, {
                    superficial: true,
                }, (diagram, x, y, snap) => {
                    const pos = findPosOnLine(info, x, y).pos
                    let lines = Object.assign({}, diagram.lines)
                    // prevent arrows from getting stuck under nodes
                    lines[lineId] = Object.assign({}, lines[lineId], {
                        arrowPos: clamp(0.1, 0.9, roundIf(snap, 0.1, pos)),
                    })
                    return Object.assign({}, diagram, {lines: lines})
                }))
                e.preventDefault()
            } else if (e.buttons == 2) {
                update(modifyDiagram({equivalent: true}, diagram =>
                    flipW1jRule(diagram, lineId)))
            } else if (e.buttons == 4) {
                update(modifyDiagram({superficial: true}, diagram => {
                    let lines = Object.assign({}, diagram.lines)
                    lines[lineId] = Object.assign({}, lines[lineId], {
                        arrowPos: 0.5,
                    })
                    return Object.assign({}, diagram, {lines: lines})
                }))
                e.preventDefault()
            }
        },
    })]
}

function renderLine(update, diagram, lineId) {
    const line = diagram.lines[lineId]
    const minTextOffset = 20
    const info = getLineInfo(diagram, lineId)
    const position = positionOnLine(info, line.textPos, 0)
    let textOffset = line.textOffset
    if (textOffset >= 0 && textOffset < minTextOffset) {
        textOffset = minTextOffset
    } else if (textOffset < 0 && textOffset > -minTextOffset) {
        textOffset = -minTextOffset
    }
    const radius = line.arcHeight ? Math.abs(info.arc.radius) : 0.0
    const d = `M ${info.x0} ${info.y0} `
            + `A ${radius} ${radius} 0 `
            + `${Number(info.arc.large)} ${Number(info.arc.sweep)} `
            + `${info.x1} ${info.y1}`
    const textX = position.x + textOffset * position.normalX
    const textY = position.y + textOffset * position.normalY
    const twoJ = twoJColor(diagram, lineId) ? "two-j " : ""
    function onmousedown(e) {
        if (e.buttons == 1) {
            update(startDrag(e.offsetX, e.offsetY, {
                superficial: true,
            }, (diagram, x, y, snap) => {
                let change
                if (info.singular) {
                    const dx = x - info.xMid
                    const dy = y - info.yMid
                    const angle = Math.atan2(dy, dx) - Math.PI / 2
                    const height = Math.sqrt(dx * dx + dy * dy)
                    change = {
                        angle: roundIf(snap, Math.PI / 6, angle),
                        arcHeight: clamp(20.0, Infinity,
                                         roundIf(snap, 20.0, height)),
                    }
                } else {
                    change = {
                        angle: info.trueAngle,
                        arcHeight: roundIf(snap, 20.0,
                                           threePointArc(x, y,
                                                         info.x0, info.y0,
                                                         info.x1, info.y1)),
                    }
                }
                return setDiagramLineProps(diagram, lineId, change)
            }))
            e.preventDefault()
        } else if (e.buttons == 4) {
            update(modifyDiagram({superficial: true}, diagram =>
                info.singular ? diagram : setDiagramLineProps(diagram, lineId, {
                    angle: info.angle,
                    arcHeight: 0.0,
                })
            ))
            e.preventDefault()
        }
    }
    return vnode(
        "svg:g",
        {
            // prevent hover effects from sticking when nodes change
            [VNODE_KEY]: lineId,
            "class": "line",
            onmouseover: function(e) {
                update(setHover({
                    type: "line",
                    lineId: lineId,
                }))
            },
            onmouseout: function(e) {
                update(setHover({type: null}))
            },
        },
        vnode("svg:title", {}, `j[${line.superline}] m[${lineId}]`),
        // this path (1) increases hit area (2) helps delineate crossing lines
        vnode("svg:path", {
            "class": "bg",
            d: d,
            onmousedown: onmousedown,
        }),
        vnode("svg:path", {
            "class": "fg " + twoJ,
            d: d,
            onmousedown: onmousedown,
        }),
        vnode("svg:text", {
            "class": "label " + twoJ,
            x: textX,
            y: textY,
            onmousedown: function(e) {
                if (e.buttons == 1) {
                    update(startDrag(textX, textY, {
                        superficial: true,
                    }, (diagram, x, y, snap) => {
                        const where = findPosOnLine(info, x, y)
                        // prevent text from getting stuck under nodes
                        return setDiagramLineProps(diagram, lineId, {
                            textPos: clamp(0.1, 0.9,
                                           roundIf(snap, 0.1, where.pos)),
                            textOffset: roundIf(snap, 10.0, where.offset),
                        })
                    }))
                    e.preventDefault()
                } else if (e.buttons == 4) {
                    update(modifyDiagram({superficial: true}, diagram =>
                        setDiagramLineProps(diagram, lineId, {
                            textPos: 0.5,
                            textOffset: 0.0,
                        })
                    ))
                    e.preventDefault()
                }
            },
        }, line.superline),
        ...renderArrow(update, diagram, lineId)
    )
}

function renderNode(update, diagram, nodeIndex, frozen) {
    const node = diagram.nodes[nodeIndex]
    let gChildren = [vnode("svg:title", {},
                           node.type == "w3j"
                         ? `Wigner[${node.lines.join(" ")}]`
                         : node.type == "terminal"
                         ? `m[${node.variable}]`
                         : node.type)]

    if (node.type == "w3j") {
        const circularArrowSize = 30
        const orientation = w3jOrientation(diagram, nodeIndex) > 0
                          ? "flipped " : ""
        gChildren.push(vnode("svg:circle", {
            "class": orientation,
            r: 18,
        }))
        gChildren.push(vnode("svg:use", {
            "class": "arrow " + orientation,
            href: "#clockwise",
            x: -circularArrowSize / 2,
            y: -circularArrowSize / 2,
            width: circularArrowSize,
            height: circularArrowSize,
        }))

    } else if (node.type == "terminal") {
        const frozenClass = frozen ? "frozen " : ""
        gChildren.push(vnode("svg:circle", {
            "class": frozenClass,
            r: 10,
        }))

    } else {
        gChildren.push(vnode("svg:circle", {
            r: 22,
        }))
        gChildren.push(vnode("svg:text", {
            "class": "label",
        }))
    }

    return vnode("svg:g", {
        // prevent hover effects from sticking when nodes change
        [VNODE_KEY]: id(node),
        "class": "node " + node.type,
        transform: `translate(${node.x}, ${node.y})`,
        onmouseover: function(e) {
            update(setHover({
                type: "node",
                nodeIndex: nodeIndex,
            }))
        },
        onmouseout: function(e) {
            update(setHover({type: null}))
        },
        onmousedown: function(e) {
            if (e.buttons == 1) {
                update(startDrag(node.x, node.y, {
                    // moving terminals and/or custom nodes
                    // changes the semantics of the diagram
                    superficial: node.type == "w3j",
                }, (diagram, x, y, snap) =>
                    setDiagramNodeProps(diagram, nodeIndex, {
                        x: roundIf(snap, 20.0, x),
                        y: roundIf(snap, 20.0, y),
                    })
                ))
                e.preventDefault()
            } else if (e.buttons == 2) {
                if (e.shiftKey) { // do it twice!
                    update(modifyDiagram({equivalent: true}, diagram =>
                        flipW3jRule(
                            flipW3jRule(diagram, nodeIndex),
                            nodeIndex)))
                } else {
                    update(modifyDiagram({equivalent: true}, diagram =>
                        flipW3jRule(diagram, nodeIndex)))
                }
                e.preventDefault()
            } else if (e.buttons == 4) {
                if (e.shiftKey) {
                    update(modifyDiagram({equivalent: true}, diagram =>
                        deltaIntroRule(diagram, nodeIndex)))
                } else {
                    update(modifyDiagram({equivalent: true}, diagram =>
                        threeArrowRule(diagram, nodeIndex)))
                }
                e.preventDefault()
            }
        },
    }, ...gChildren)
}

function renderJTableau(update, superlines) {
    return Object.keys(superlines).sort((x, y) => {
        let d = x.length - y.length
        if (d == 0) {
            d = Number(x > y) - Number(x < y)
        }
        return d
    }).map(superlineId => {
        const superline = superlines[superlineId]
        let phase
        switch (mod(superline.phase, 4)) {
            case 0:
                phase = ["\xa0\xa0"]
                break
            case 1:
                phase = ["\xa0."]
                break
            case 2:
                phase = [vnode("span", {"class": "two-j"}, ":"), "\xa0"]
                break
            case 3:
                phase = [vnode("span", {"class": "two-j"}, ":"), "."]
                break
        }
        let weight = superline.weight
                   ? (superline.weight > 0
                    ? "+"
                    : superline.weight < 0
                    ? "\u2212"
                    : "")
                   + Math.abs(superline.weight)
                   : ""
        return vnode(
            "tr", {},
            vnode("td", {"class": "summed"}, superline.summed ? "\u2211" : ""),
            vnode("td", {"class": "name"}, superlineId),
            vnode("td", {"class": "phase"}, ...phase),
            vnode("td", {"class": "weight"}, weight),
        )
    })
}

function renderTriangleTableau(update, triangles) {
    return triangles.map((triangle, triangleIndex) => vnode("li", {
        onmousedown: function(e) {
            update(modifyDiagram({equivalent: true}, diagram =>
                trianglePhaseRule(diagram, triangleIndex)))
            e.preventDefault()
        },
    }, "{" + triangle.join(" ") + "}"))
}

function renderDeltaTableau(update, deltas) {
    return deltas.map(delta =>
        vnode("li", {},
              ...intercalate(
                  " = ",
                  Array.from(delta)
                       .map(x => isZeroJ(x)
                             ? vnode("span", {"class": "zero"}, "0")
                             : String(x)))))
}

function renderVariable(type, name) {
    if (type == "j" && isZeroJ(name)) {
        return "0"
    }
    return `${type}_{\\text{${name}}}`
}

function renderEquationLine(diagram, nodeIndex, lineIndex, summedVars) {
    const lineId = diagram.nodes[nodeIndex].lines[lineIndex]
    const line = diagram.lines[lineId]
    const otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex)
    const otherNode = diagram.nodes[otherIndex]
    // avoid redundant summation over an m-delta
    const mName = otherNode.type == "terminal"
                ? otherNode.variable
                : lineId
    const mNaked = renderVariable("m", mName)
    const jm = {
        j: renderVariable("j", diagram.lines[lineId].superline),
        m: otherIndex < nodeIndex && line.direction != 0
         ? `\\overline{${mNaked}}`
         : mNaked,
    }
    if (line.superline.summed) {
        summedVars.js[jm.j] = true
    }
    if (otherNode.type != "terminal") {
        summedVars.ms[mNaked] = true
    }
    return jm
}

function renderEquation(diagram, container) {
    const nodes = diagram.nodes
    let s = ""
    let summedVars = {js: {}, ms: {}}
    let phases = []
    let mDeltas = []
    nodes.forEach(function(node, nodeIndex) {
        if (node.type == "terminal") {
            const otherIndex = otherNodeIndex(nodes, nodeIndex, 0)
            if (otherIndex > nodeIndex) {
                return
            }
            const var1 = nodes[otherIndex].variable
            const var2 = node.variable
            if (var1 == var2) {
                return
            }
            const m1 = renderVariable("m", var1)
            const m2 = renderVariable("m", var2)
            switch (diagram.lines[node.lines[0]].direction) {
                case 0:
                    mDeltas.push(`\\delta_{${m1} ${m2}}`)
                    break
                case -1:
                    mDeltas.push(`\\delta_{${m1} \\overline{${m2}}}`)
                    break
                case 1:
                    mDeltas.push(`\\delta_{\\overline{${m1}} ${m2}}`)
                    break
                default:
                    throw new Error(`unnormalized direction: ${direction}`)
            }
        } else if (node.type == "w3j") {
            s += "\\begin{pmatrix}"
            let jRow = ""
            let mRow = ""
            node.lines.forEach(function(lineId, lineIndex) {
                if (lineIndex > 0) {
                    jRow += " & "
                    mRow += " & "
                }
                const jm = renderEquationLine(diagram, nodeIndex,
                                              lineIndex, summedVars)
                jRow += jm.j
                mRow += jm.m
            })
            s += jRow + " \\\\"
            s += mRow + " \\\\"
            s += "\\end{pmatrix}"
        } else {
            s += "\\mathtt{${node.type}}_{"
            node.lines.forEach(function(lineId, lineIndex) {
                if (lineIndex > 0) {
                    s += " "
                }
                const jm = renderEquationLine(diagram, nodeIndex,
                                              lineId, summedVars)
                s += jm[0] + " " + jm[1]
            })
            s += "}"
        }
    })
    let weights = ""
    Object.keys(diagram.superlines).forEach(function(superlineId) {
        const superline = diagram.superlines[superlineId]
        switch (mod(superline.phase, 4)) {
            case 0:
                break
            case 1:
                phases.push(`+ j_{${superlineId}}`)
                break
            case 2:
                phases.push(`+ 2 j_{${superlineId}}`)
                break
            case 3:
                phases.push(`- j_{${superlineId}}`)
                break
        }
        if (superline.weight == 0) {
        } else if (superline.weight == 2) {
            weights += ` (2 j_{${superlineId}} + 1)`
        } else if (superline.weight % 2 == 0) {
            weights += ` (2 j_{${superlineId}} + 1)^{${superline.weight / 2}}`
        } else {
            weights += ` (2 j_{${superlineId}} + 1)^{${superline.weight} / 2}`
        }
    })
    Object.keys(diagram.lines).forEach(function(lineId) {
        const line = diagram.lines[lineId]
        let name
        switch (line.direction) {
            case 0:
                return
            case -1:
                const node2 = nodes[endNodeIndices(nodes, lineId)[1]]
                if (node2.type == "terminal") {
                    name = node2.variable
                } else {
                    name = lineId
                }
                break
            case 1:
                const node1 = nodes[endNodeIndices(nodes, lineId)[0]]
                if (node1.type == "terminal") {
                    name = node1.variable
                } else {
                    name = lineId
                }
                break
            default:
                throw new Error(`unnormalized direction: ${direction}`)
        }
        const j = renderVariable("j", diagram.lines[lineId].superline)
        const m = renderVariable("m", name)
        phases.push(`+ ${j} ${line.direction < 0 ? "+" : "-"} ${m}`)
    })
    let summedVarsStr = Object.keys(summedVars.js).join(" ")
                      + " "
                      + Object.keys(summedVars.ms).join(" ")
    if (summedVarsStr != " ") {
        summedVarsStr = `\\sum_{${summedVarsStr}}`
    }
    let phasesStr = phases.join(" ")
    if (phasesStr) {
        if (phasesStr.startsWith("+ ")) {
            phasesStr = phasesStr.substr(2)
        }
        phasesStr = `(-1)^{${phasesStr}}`
    }
    const mDeltasStr = mDeltas.join(" ")
    const jDeltas = diagram.deltas.map(js => {
        if (js.length < 2) {
            return ""
        }
        const j0 = renderVariable("j", js[0])
        return js.slice(1)
                 .map(j => `\\delta_{${j0} ${renderVariable("j", j)}}`)
                 .join(" ")
    }).join(" ")
    const triangles = diagram.triangles.map(js =>
        `\\{${js.map(j => renderVariable("j", j)).join(", ")}\}`
    ).join(" ")
    container.textContent = `\\[${summedVarsStr} ${jDeltas} ${mDeltasStr} `
                          + `${weights} ${phasesStr} ${s}\\]`
    MathJax.Hub.Queue(["Typeset", MathJax.Hub])
}

//////////////////////////////////////////////////////////////////////////////
// State management
//
// The "update" function is responsible for making changes to the model (and
// updating the DOM as needed).  It has type (&mut Editor -> ()) -> ().
// In the future, "update" may ask for a Promise instead.

const ALT = 0x1
const CTRL = 0x2
const SHIFT = 0x4

const EMPTY_SNAPSHOT = {
    diagram: EMPTY_DIAGRAM,
    frozen: false,
}

function newEditor(diagram, frozen) {
    return {
        snapshot: deduplicateObject({
            diagram: diagram,
            frozen: frozen,
        }, EMPTY_SNAPSHOT),
        savedSnapshot: EMPTY_SNAPSHOT,
        savedHash: "",
        staleEquation: true,

        // controls
        hover: {type: null},
        mouseX: null,
        mouseY: null,
        dragTrail: {xs: [], ys: []},
    }
}

const INITIAL_EDITOR = newEditor(EMPTY_DIAGRAM, false)

function saveEditor(editor) {
    editor.savedSnapshot = editor.snapshot
    editor.savedHash = "#" + encodeURIComponent(JSON.stringify(editor.snapshot))
    window.location.hash = editor.savedHash
}

function loadEditor(editor) {
    const hash = window.location.hash
    // prevent hashchange listener from observing our own changes
    if (hash.length < 3) {
        Object.assign(editor, INITIAL_EDITOR)
    } else if (editor.savedHash != hash) {
        editor.snapshot = JSON.parse(decodeURIComponent(hash.substr(1)))
        editor.staleEquation = true
        editor.savedSnapshot = editor.snapshot
        editor.savedHash = hash
    }
}

function renderEditor(update, editor) {
    const diagram = editor.snapshot.diagram
    return [
        {
            element: window,
            attributes: {
                onhashchange: _ => update(loadEditor),
                onkeydown: e => update(keyDown.bind(null, e)),
                onmousemove: e => update(mouseMove(e)),
                onmouseup: e => update(mouseUp(e)),
            },
        },
        {
            element: document.getElementById("freeze"),
            attributes: {
                "class": editor.snapshot.frozen ? "active" : "",
            },
        },
        {
            element: document.getElementById("diagram"),
            attributes: {
                oncontextmenu: function(e) { e.preventDefault() },
            },
        },
        {
            element: document.getElementById("diagram-lines"),
            children: Object.keys(diagram.lines).map(lineId =>
                renderLine(update, diagram, lineId)),
        },
        {
            element: document.getElementById("diagram-nodes"),
            children: diagram.nodes.map((_, nodeIndex) =>
                renderNode(update, diagram, nodeIndex, editor.snapshot.frozen)),
        },
        {
            element: document.getElementById("diagram-drag-trail"),
            attributes: {
                d: cardinalSpline(editor.dragTrail.xs,
                                  editor.dragTrail.ys,
                                  1.0),
            },
        },

        {
            element: document.getElementById("tableau-body"),
            children: renderJTableau(update, diagram.superlines),
        },
        {
            // triangle rule relations
            element: document.getElementById("triangle-tableau"),
            children: renderTriangleTableau(update, diagram.triangles),
        },
        {
            // Kronecker delta relations
            element: document.getElementById("delta-tableau"),
            children: renderDeltaTableau(update, diagram.deltas),
        },
        {
            element: document.getElementById("equation-container"),
            attributes: {
                "class": editor.staleEquation ? "stale" : "",
            },
        },
        {
            element: document.getElementById("equation"),
            attributes: {
                onclick: e => update(freshenEquation(e.currentTarget)),
            },
        },
    ]
}

//////////////////////////////////////////////////////////////////////////////
// Actions

// supported boolean flags:
//   transient
//   equivalent
//   superficial (implies equivalent)
//   toggleFreeze (implies superficial)
function modifyDiagram(flags, diagramTransform) {
    return editor => {
        const equivalent = flags.equivalent
                        || flags.superficial
                        || flags.toggleFreeze
        if (editor.snapshot.frozen && !equivalent) {
            // nonequivalent changes are forbidden while frozen
            return
        }
        const diagram = diagramTransform(editor.snapshot.diagram)
        if (typeof(diagram) == "string") { // error?
            error(diagram)
            return
        }
        editor.snapshot = Object.assign({}, editor.snapshot, {
            diagram: diagram,
            frozen: Boolean(Number(flags.toggleFreeze)
                          ^ Number(editor.snapshot.frozen)),
        })
        if (!flags.transient) {
            saveEditor(editor)
        }
        const superficial = flags.superficial || flags.toggleFreeze
        editor.staleEquation = !superficial || editor.staleEquation
    }
}

function getModifiers(event) {
    return event.altKey | (event.ctrlKey << 1) | (event.shiftKey << 2)
}

function freshenEquation(container) {
    return editor => {
        editor.staleEquation = false
        renderEquation(editor.snapshot.diagram, container)
    }
}

function setHover(entity) {
    return editor => {
        editor.hover = entity
    }
}

function clearDragTrail(editor) {
    if (editor.dragTrail.xs) {
        editor.dragTrail.xs = []
        editor.dragTrail.ys = []
    }
}

function mouseUp(event) {
    return editor => {
        if (editor.dragger) {
            modifyDiagram(editor.draggerFlags, identity)(editor)
            editor.dragger = null
        } else {
            clearDragTrail(editor)
        }
    }
}

function mouseMove(event) {
    return editor => {
        let svg = document.getElementById("diagram")
        const rect = svg.getBoundingClientRect()
        editor.rawMouseX = event.clientX
        editor.rawMouseY = event.clientY
        editor.mouseX = event.clientX - rect.left
        editor.mouseY = event.clientY - rect.top
        if (editor.dragger) {
            modifyDiagram(
                Object.assign({}, editor.draggerFlags, {transient: true}),
                diagram => editor.dragger(
                    diagram,
                    event.clientX + editor.dragOffsetX,
                    event.clientY + editor.dragOffsetY,
                    event.ctrlKey))(editor)
        } else if (event.buttons == 1) {
            editor.dragTrail.xs.push(editor.mouseX)
            editor.dragTrail.ys.push(editor.mouseY)
        } else {
            clearDragTrail(editor)
        }
    }
}

function startDrag(x, y, flags, dragger) {
    return editor => {
        editor.dragger = dragger
        editor.draggerFlags = flags
        editor.dragOffsetX = x - editor.rawMouseX
        editor.dragOffsetY = y - editor.rawMouseY
    }
}

function keyDown(e, editor) {
    const snapshot = editor.snapshot
    const update = f => f(editor)

    // reload
    if (getModifiers(e) == 0 && e.key == "r") {
        window.location.href = ""
        e.preventDefault()
        return
    }

    // help
    if (getModifiers(e) == SHIFT && e.key == "?") {
        window.location.href = "help.txt"
        e.preventDefault()
        return
    }

    // mouse events require the position
    if (editor.mouseX === null) {
        error("Need to move the mouse before doing anything :/")
        return
    }

    // create Clebsch-Gordan coefficient
    if (getModifiers(e) == 0 && e.key == "f") {
        update(modifyDiagram({toggleFreeze: true}, identity))
        e.preventDefault()
        return
    }

    // create Clebsch-Gordan coefficient
    if (getModifiers(e) == 0 && e.key == "c") {
        update(modifyDiagram({}, diagram => {
            const labels = availSuperlineLabels(diagram, 3)
            return mergeDiagrams(diagram,
                                 cgDiagram(labels[0],
                                           labels[1],
                                           labels[2],
                                           editor.mouseX,
                                           editor.mouseY))
        }))
        e.preventDefault()
        return
    }

    // create Wigner 3-jm
    if (getModifiers(e) == 0 && e.key == "w") {
        update(modifyDiagram({}, diagram => {
            const labels = availSuperlineLabels(diagram, 3)
            return mergeDiagrams(diagram, w3jDiagram(labels[0],
                                                     labels[1],
                                                     labels[2],
                                                     editor.mouseX,
                                                     editor.mouseY))
        }))
        e.preventDefault()
        return
    }

    // attach
    if (getModifiers(e) == 0 && e.key == "a") {
        update(modifyDiagram({}, diagram => {
            const nearest = nearestNodeIndices(diagram.nodes, 2,
                                               editor.mouseX,
                                               editor.mouseY)
            if (!(nearest.length == 2 &&
                  diagram.nodes[nearest[0]].type == "terminal" &&
                  diagram.nodes[nearest[1]].type == "terminal")) {
                error("no nearby terminals found")
                return diagram
            } else {
                return joinTerminals(diagram, nearest[0], nearest[1])
            }
        }))
        e.preventDefault()
        return
    }

    // create Wigner 1-jm
    if (getModifiers(e) == 0 && e.key == "m" && editor.hover.type == "line") {
        update(modifyDiagram({}, diagram =>
            addW1j(diagram, editor.hover.lineId)))
        e.preventDefault()
        return
    }

    // add 2j phase / flip Wigner 3j node
    if (getModifiers(e) == 0 && e.key == "j") {
        if (editor.hover.type == "line") {
            update(modifyDiagram({}, diagram =>
                add2j(diagram, editor.hover.lineId)))
            e.preventDefault()
            return
        } else if (editor.hover.type == "node") {
            update(modifyDiagram({}, diagram =>
                flipW3j(diagram, editor.hover.nodeIndex)))
            e.preventDefault()
            return
        }
    }

    // delete node
    if (getModifiers(e) == 0 && e.key == "x") {
        update(modifyDiagram({}, diagram => {
            const nodes = diagram.nodes
            const nearest = nearestNodeIndices(nodes, 1,
                                               editor.mouseX,
                                               editor.mouseY)
            if (nearest.length != 1 ||
                (nodes[nearest[0]].type == "terminal" &&
                 nodes[otherNodeIndex(nodes, nearest[0], 0)].type
                    != "terminal")) {
                error("no nearby nodes found")
                return diagram
            } else {
                return deleteNode(diagram, nearest[0])
            }
        }))
        e.preventDefault()
        return
    }
}

//////////////////////////////////////////////////////////////////////////////
// Global stuff

let errorTimeout = 0
function error(msg) {
    let notice = document.getElementById("notice")
    notice.className = "warning"
    notice.textContent = msg
    if (errorTimeout) {
        window.clearTimeout(errorTimeout)
    }
    errorTimeout = window.setTimeout(function() {
        notice.textContent = "\xa0"
    }, 10000)
}

// keep the state as a global to make debugging easier
function initEditor() {
    let editor = {}
    Object.assign(editor, INITIAL_EDITOR)
    function update(...changes) {
        const n = changes.length
        for (let i = 0; i < n; ++i) {
            changes[i](editor)
        }
        applyRendering(renderEditor(update, editor))
    }
    update(loadEditor)
}

initEditor()
