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

function sortTwo(x, y) {
    if (x < y) {
        return [x, y]
    } else {
        return [y, x]
    }
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

// Choose smoothness = 1 for a Catmull–Rom spline.
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
                        elem.removeChild(oldChild)
                        if (oldChild[VNODE_KEY] == key) {
                            break
                        }
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
    superline1 = Object.assign({}, superline1)
    changePhase(superline1, superline2.phase)
    superline1.weight += superline2.weight
    superline1.summed |= superline2.summed
    return superline1
}

//////////////////////////////////////////////////////////////////////////////
// Line manipulation

// type Line = {
//   superline: String,
//   direction: -1 (left) | 0 | 1 (right),
//   arrowPos: 0.0 (left) to 1.0 (right),
//   arcHeight: -∞ to ∞, (sagitta, + is downward)
//   angle: 0 to 2π, (of the direct line, clockwise)
//   textPos: 0.0 (left) to 1.0 (right),
//   textOffset: -∞ to ∞, (+ is downward)
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

function joinLines(line1, line2) {
    const superlines = sortTwo(line1.superline, line2.superline)
    return Object.assign(canonicalizeLine({
        superline: superlines[0],
        direction: line1.direction + line2.direction,
        arrowPos: (line1.arrowPos + line2.arrowPos) / 2,
        arcHeight: (line1.arcHeight + line2.arcHeight) / 2,
        angle: 0.0, // can't take an average here
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
                nodeAndLineIndices.push({
                    node: nodeIndex,
                    lineIndex: lineIndex
                })
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
    return endNodeAndLineIndices(nodes, lineId).map(x => x.node)
}

function otherNodeAndLineIndex(nodes, nodeIndex, lineIndex) {
    const lineId = nodes[nodeIndex].lines[lineIndex]
    if (lineId === undefined) {
        throw new Error(`cannot find line with lineIndex = ${lineIndex}`)
    }
    let nodeAndLineIndices = endNodeAndLineIndices(nodes, lineId)
    if (nodeAndLineIndices[0].node == nodeIndex &&
        nodeAndLineIndices[0].lineIndex == lineIndex) {
        return nodeAndLineIndices[1]
    } else {
        return nodeAndLineIndices[0]
    }
}

function otherNodeIndex(nodes, nodeIndex, lineIndex) {
    return otherNodeAndLineIndex(nodes, nodeIndex, lineIndex).node
}

function isLeftOfLine(nodes, nodeIndex, lineIndex) {
    const other = otherNodeAndLineIndex(nodes, nodeIndex, lineIndex)
    if (other.node == nodeIndex) {
        return other.lineIndex > nodeIndex
    } else {
        return other.node > nodeIndex
    }
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

    // each entry is a "set" (object) of j's that are equal to each other
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
            {
                type: "terminal",
                lines: Object.freeze([a]),
                x: x - 50,
                y: y + 50,
            },
            {
                type: "terminal",
                lines: Object.freeze([b]),
                x: x + 50,
                y: y + 50,
            },
            {
                type: "terminal",
                lines: Object.freeze([c]),
                x: x,
                y: y - 70,
            },
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

// FIXME this is kind of broken with the introduction of arcs
// should probably just use hover or something instead
function findNearestLineId(diagram, x, y) {
    return Object
        .keys(diagram.lines)
        .map(function(lineId) {
            const nodeIndices = endNodeIndices(diagram.nodes, lineId)
            return {
                distance: linePointDistance(
                    diagram.nodes[nodeIndices[0]].x,
                    diagram.nodes[nodeIndices[0]].y,
                    diagram.nodes[nodeIndices[1]].x,
                    diagram.nodes[nodeIndices[1]].y,
                    x, y
                ),
                id: lineId
            }
        })
        .sort((x, y) => x.distance - y.distance)
        .slice(0, 1)
        .map(x => x.id)
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
        node.lines = node.lines.map(lineId => renames[lineId])
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
        if (typeof(terminalIndex1) != "number" || typeof(terminalIndex2) != "number") {
            throw "WTF"
        }
    if (diagram.nodes[terminalIndex1].type != "terminal" ||
        diagram.nodes[terminalIndex2].type != "terminal") {
        throw new Error("cannot join non-terminals")
    }
    let other1 = otherNodeAndLineIndex(diagram.nodes, terminalIndex1, 0)
    let other2 = otherNodeAndLineIndex(diagram.nodes, terminalIndex2, 0)
    if (other1.node > other2.node ||
        (other1.node == other2.node && other1.lineIndex > other1.lineIndex)) {
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

    // join with other node
    diagram.nodes[other2.node].lines[other2.lineIndex] = lineId1

    // merge the lines (be careful with orientation)
    let line1 = diagram.lines[lineId1]
    let line2 = diagram.lines[lineId2]
    if (other1.node > terminalIndex1) {
        line1 = reverseLine(line1)
    }
    if (terminalIndex2 > other2.node) {
        line2 = reverseLine(line2)
    }
    let joined = joinLines(line1, line2)
    let superlineId1 = joined.line.superline
    let superlineId2 = joined.otherSuperline
    if (other1.node == other2.node && joined.line.arcHeight == 0.0) {
        joined.line.arcHeight = 50.0
    }
    changePhase(diagram.superlines[superlineId1], joined.phase)
    diagram.lines[lineId1] = joined.line
    delete diagram.lines[lineId2]

    // equate superlines and merge their factors
    diagram.superlines[superlineId1] =
        mergeSuperlines(diagram.superlines[superlineId1],
                        diagram.superlines[superlineId2])
    delete diagram.superlines[superlineId2]

    // delete the terminal nodes
    ;[terminalIndex1, terminalIndex2]
        .sort((x, y) => y - x)
        .forEach(terminalIndex => diagram.nodes.splice(terminalIndex, 1))

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
        let danglingSuperline = true
        Object.keys(diagram.lines).forEach(function(lineId) {
            if (diagram.lines[lineId].superline == superlineId) {
                danglingSuperline = false
            }
        })
        if (danglingSuperline) {
            delete diagram.superlines[superlineId]
        }
        ;[nodeIndex, otherIndex]
            .sort((x, y) => y - x)
            .forEach(terminalIndex => diagram.nodes.splice(terminalIndex, 1))
    } else {
        let terminals = []
        node.lines.forEach(function(lineId, lineIndex) {
            const otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex)
            if (otherIndex == nodeIndex) {
                delete diagram.lines[lineIndex]
                return
            }
            if (otherIndex < nodeIndex) {
                diagram.lines[lineId] = reverseLine(diagram.lines[lineId])
            }
            terminals.push({
                type: "terminal",
                lines: [lineId],
                x: node.x,
                y: node.y
            })
        })
        diagram.nodes.splice(nodeIndex, 1)
        diagram.nodes = terminals.concat(diagram.nodes)
    }
    return diagram
}

//////////////////////////////////////////////////////////////////////////////
// Diagrammatic rules

function flipW3jRule(diagram, nodeIndex) {
    if (diagram.nodes[nodeIndex].type != "w3j") {
        return diagram
    }
    diagram = deepClone(diagram)
    diagram.nodes[nodeIndex].lines.reverse()
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

//////////////////////////////////////////////////////////////////////////////
// Drawing

function twoJColor(diagram, lineId) {
    const superlineId = diagram.lines[lineId].superline
    return mod(diagram.superlines[superlineId].phase, 4) >= 2 ?
           "#ac53b3" : "#051308"
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
    const angle = position.tangentAngle
                + Number(line.direction < 0) * Math.PI
    if (line.direction == 0) {
        return []
    }
    return [vnode("svg:use", {
        href: "#arrowhead",
        x: -arrowHeadSize,
        y: -arrowHeadSize / 2,
        width: arrowHeadSize,
        height: arrowHeadSize,
        fill: twoJColor(diagram, lineId),
        transform: `translate(${position.x}, ${position.y}),`
                 + `rotate(${angle * 180 / Math.PI})`,
        oncontextmenu: function(e) { e.preventDefault() },
        onmousedown: function(e) {
            if (e.buttons == 1) {
                update(startDrag(position.x, position.y, {
                    superficial: true,
                }, (diagram, x, y) => {
                    // prevent arrows from getting stuck under nodes
                    const pos = clamp(0.1, 0.9, findPosOnLine(info, x, y).pos)
                    let lines = Object.assign({}, diagram.lines)
                    lines[lineId] = Object.assign({}, lines[lineId], {
                        arrowPos: pos,
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
    const minTextOffset = 16
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
    const color = twoJColor(diagram, lineId)
    function onmousedown(e) {
        if (e.buttons == 1) {
            update(startDrag(e.offsetX, e.offsetY, {
                superficial: true,
            }, (diagram, x, y) => {
                let change
                if (info.singular) {
                    const dx = x - info.xMid
                    const dy = y - info.yMid
                    change = {
                        angle: Math.atan2(dy, dx) - Math.PI / 2,
                        arcHeight: Math.sqrt(dx * dx + dy * dy),
                    }
                } else {
                    change = {
                        angle: info.trueAngle,
                        arcHeight: threePointArc(x, y,
                                                 info.x0, info.y0,
                                                 info.x1, info.y1)
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
        "svg:g", {"class": "line"},
        // this path (1) increases hit area (2) helps delineate crossing lines
        vnode("svg:path", {
            fill: "none",
            stroke: "rgba(255, 255, 255, 0.7)",
            "stroke-width": "16",
            d: d,
            oncontextmenu: function(e) { e.preventDefault() },
            onmousedown: onmousedown,
        }),
        vnode("svg:path", {
            "class": "visible",
            fill: "none",
            "stroke-width": "2",
            d: d,
            stroke: color,
            oncontextmenu: function(e) { e.preventDefault() },
            onmousedown: onmousedown,
        }),
        vnode("svg:text", {
            "class": "line-label",
            "alignment-baseline": "middle",
            "text-anchor": "middle",
            "font-size": "large",
            x: textX,
            y: textY,
            fill: color,
            oncontextmenu: function(e) { e.preventDefault() },
            onmousedown: function(e) {
                if (e.buttons == 1) {
                    update(startDrag(textX, textY, {
                        superficial: true,
                    }, (diagram, x, y) => {
                        const where = findPosOnLine(info, x, y)
                        // prevent text from getting stuck under nodes
                        const pos = clamp(0.1, 0.9, where.pos)
                        return setDiagramLineProps(diagram, lineId, {
                            textPos: pos,
                            textOffset: where.offset,
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

function renderNode(update, diagram, nodeIndex) {
    const node = diagram.nodes[nodeIndex]
    let gChildren = [vnode("svg:title", {},
                           node.type == "w3j"
                         ? "Wigner 3-jm symbol"
                         : node.type == "terminal"
                         ? "Line terminal"
                         : node.type)]

    if (node.type == "w3j") {
        const circularArrowSize = 30
        const orientation = w3jOrientation(diagram, nodeIndex)
        gChildren.push(vnode("svg:circle", {
            r: 18,
            fill: orientation > 0 ? "#d98b7c" : "#2eb0ba",
        }))
        gChildren.push(vnode("svg:use", {
            href: "#clockwise",
            x: -circularArrowSize / 2,
            y: -circularArrowSize / 2,
            width: circularArrowSize,
            height: circularArrowSize,
            fill: "white",
            transform: orientation > 0 ? "scale(-1,1)" : "",
        }))

    } else if (node.type == "terminal") {
        gChildren.push(vnode("svg:circle", {
            r: 10,
            fill: "rgba(0, 0, 0, 0.2)",
        }))

    } else {
        gChildren.push(vnode("svg:circle", {
            r: 22,
            fill: "#ddd",
        }))
        gChildren.push(vnode("svg:text", {
            "class": "node-label",
            "alignment-baseline": "middle",
            "text-anchor": "middle",
            "font-size": "large",
        }))
    }

    return vnode("svg:g", {
        transform: `translate(${node.x}, ${node.y})`,
        oncontextmenu: function(e) { e.preventDefault() },
        onmousedown: function(e) {
            if (e.buttons == 1) {
                update(startDrag(node.x, node.y, {
                    // moving terminals and/or custom nodes
                    // changes the semantics of the diagram
                    superficial: node.type == "w3j",
                }, (diagram, x, y) => setDiagramNodeProps(diagram, nodeIndex, {
                    x: x,
                    y: y,
                })))
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
                update(modifyDiagram({equivalent: true}, diagram =>
                    threeArrowRule(diagram, nodeIndex)))
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
                phase = ["  "]
                break
            case 1:
                phase = [" ."]
                break
            case 2:
                phase = [vnode("span", {"class": "two-j"}, ":"), " "]
                break
            case 3:
                phase = [vnode("span", {"class": "two-j"}, ":"), "."]
                break
        }
        let weight = superline.weight
                   ? (superline.weight > 0 ? "+" : "")
                   + superline.weight
                   : ""
        return vnode(
            "tr", {},
            vnode("td", {"class": "summed"}, superline.summed ? "∑" : ""),
            vnode("td", {"class": "name"}, superlineId),
            vnode("td", {"class": "phase"}, ...phase),
            vnode("td", {"class": "weight"}, weight),
        )
    })
}

function renderTriangleTableau(update, triangles) {
    return triangles.forEach((triangle, triangleIndex) => vnode("li", {
        onmousedown: function(e) {
            update(modifyDiagram({equivalent: true}, diagram =>
                trianglePhaseRule(diagram, triangleIndex)))
            e.preventDefault()
        },
    }, "△{" + triangle.join(", ") + "}"))
}

function renderDeltaTableau(update, deltas) {
    return deltas.forEach(delta =>
        vnode("li", {}, Array.from(Object.keys(delta)).sort().join(" = ")))
}

function renderEquationLine(diagram, nodeIndex, lineIndex, summedVars) {
    const lineId = diagram.nodes[nodeIndex].lines[lineIndex]
    const mNaked = `m_{${lineId}}`
    let jm = {
        j: `j_{${diagram.lines[lineId].superline}}`,
        m: mNaked
    }
    const line = diagram.lines[lineId]
    const otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex)
    if (otherIndex < nodeIndex && line.direction != 0) {
        jm.m = `\\overline{${jm.m}}`
    }
    if (line.superline.summed) {
        summedVars.js[jm.j] = true
    }
    if (diagram.nodes[otherIndex].type != "terminal") {
        summedVars.ms[mNaked] = true
    }
    return jm
}

function renderEquation(diagram, container) {
    let s = ""
    let summedVars = {js: {}, ms: {}}
    let phases = []
    diagram.nodes.forEach(function(node, nodeIndex) {
        if (node.type == "terminal") {
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
        if (superline.weight) {
            weights += ` (2 j_{${superlineId}} + 1)^{${superline.weight} / 2}`
        }
    })
    Object.keys(diagram.lines).forEach(function(lineId) {
        const line = diagram.lines[lineId]
        if (line.direction != 0) {
            phases.push(`+ j_{${diagram.lines[lineId].superline}} `
                      + `${line.direction < 0 ? "+" : "-"} m_{${lineId}}`)
        }
    })
    let summedVarsStr = Array.from(Object.keys(summedVars.js)).join(" ")
                      + " "
                      + Array.from(Object.keys(summedVars.ms)).join(" ")
    if (summedVarsStr != " ") {
        summedVarsStr = `\\sum_{${summedVars}}`
    }
    let phasesStr = phases.join(" ")
    if (phasesStr) {
        if (phasesStr.startsWith("+ ")) {
            phasesStr = phasesStr.substr(2)
        }
        phasesStr = `(-1)^{${phasesStr}}`
    }
    container.textContent =
        `\\[${summedVarsStr} ${weights} ${phasesStr} ${s}\\]`
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
        modifiers: 0,
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
                onkeyup: e => update(changeKeyState(e)),
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
            element: document.getElementById("diagram-lines"),
            children: Object.keys(diagram.lines).map(lineId =>
                renderLine(update, diagram, lineId)),
        },
        {
            element: document.getElementById("diagram-nodes"),
            children: diagram.nodes.map((_, nodeIndex) =>
                renderNode(update, diagram, nodeIndex)),
        },
        {
            element: document.getElementById("diagram-drag-trail"),
            attributes: {
                d: cardinalSpline(editor.dragTrail.xs, editor.dragTrail.ys, 1.0),
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
//   toggleFreeze (implies equivalent)
function modifyDiagram(flags, diagramTransform) {
    return editor => {
        const equivalent = flags.equivalent
                        || flags.superficial
                        || flags.toggleFreeze
        if (editor.snapshot.frozen && !equivalent) {
            // nonequivalent changes are forbidden while frozen
            return
        }
        editor.snapshot = Object.assign({}, editor.snapshot, {
            diagram: diagramTransform(editor.snapshot.diagram),
            frozen: Boolean(Number(flags.toggleFreeze)
                          ^ Number(editor.snapshot.frozen)),
        })
        if (!flags.transient) {
            saveEditor(editor)
        }
        editor.staleEquation = !flags.superficial
                            || editor.staleEquation
    }
}

function changeKeyState(event) {
    return editor => {
        editor.modifiers = event.altKey
            | (event.ctrlKey << 1)
            | (event.shiftKey << 2)
    }
}

function freshenEquation(container) {
    return editor => {
        editor.staleEquation = false
        renderEquation(editor.snapshot.diagram, container)
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
                    event.clientY + editor.dragOffsetY))(editor)
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

function keyDown(event, editor) {
    const snapshot = editor.snapshot
    const update = f => f(editor)

    update(changeKeyState(event))

    // reload
    if (editor.modifiers == 0 && event.key == "r") {
        window.location.href = ""
        return
    }

    // mouse events require the position
    if (editor.mouseX === null) {
        error("Need to move the mouse before doing anything :/")
        return
    }

    // create Clebsch–Gordan coefficient
    if (editor.modifiers == 0 && event.key == "f") {
        update(modifyDiagram({toggleFreeze: true}, identity))
    }

    // create Clebsch–Gordan coefficient
    if (editor.modifiers == 0 && event.key == "c") {
        update(modifyDiagram({}, diagram => {
            const labels = availSuperlineLabels(diagram, 3)
            return mergeDiagrams(diagram,
                                 cgDiagram(labels[0],
                                           labels[1],
                                           labels[2],
                                           editor.mouseX,
                                           editor.mouseY))
        }))
    }

    // create Wigner 3-jm
    if (editor.modifiers == 0 && event.key == "w") {
        update(modifyDiagram({}, diagram => {
            const labels = availSuperlineLabels(diagram, 3)
            return mergeDiagrams(diagram, w3jDiagram(labels[0],
                                                     labels[1],
                                                     labels[2],
                                                     editor.mouseX,
                                                     editor.mouseY))
        }))
    }

    // attach
    if (editor.modifiers == 0 && event.key == "a") {
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
    }

    // create Wigner 1-jm
    if (editor.modifiers == 0 && event.key == "m") {
        update(modifyDiagram({}, diagram => {
            const nearest = findNearestLineId(diagram,
                                              editor.mouseX,
                                              editor.mouseY)
            if (nearest.length != 1) {
                error("no nearby line found")
                return diagram
            } else {
                return addW1j(diagram, nearest)
            }
        }))
    }

    // add 2j phase
    if (editor.modifiers == 0 && event.key == "j") {
        update(modifyDiagram({}, diagram => {
            const nearest = findNearestLineId(diagram,
                                              editor.mouseX,
                                              editor.mouseY)
            if (nearest.length != 1) {
                error("no nearby line found")
                return diagram
            } else {
                return add2j(diagram, nearest)
            }
        }))
    }

    // delete node
    if (editor.modifiers == 0 && event.key == "x") {
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
        notice.textContent = " "
    }, 10000)
}

// keep the state as a global to make debugging easier
let __editor
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
    __editor = editor
    update(loadEditor)
}

initEditor()
