"use strict"

//////////////////////////////////////////////////////////////////////////////
// Utility: Generic

function identity(x) {
    return x
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

function permutSign(xs, ys) {
    let permut = new Map()
    for (const [i, y] of ys.entries()) {
        if (permut.has(y)) {
            return 0 // not a permutation
        }
        permut.set(y, xs[i])
    }
    let visited = new Set(xs)
    let sign = 1
    while (visited.size) {
        const x0 = visited.values().next().value
        visited.delete(x0)
        let x = x0
        do {
            if (!permut.has(x)) {
                return 0 // not a permutation
            }
            x = permut.get(x)
            visited.delete(x)
            sign *= -1
        } while (x != x0)
        sign *= -1
    }
    return sign
}

//////////////////////////////////////////////////////////////////////////////
// Utility: Basic math

function sgn(x) {
    return x > 0 ? 1 : x < 0 ? -1 : 0
}

/** Floored modulo */
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
    if (!dx) {
        return x
    }
    return x + dx / 2 - mod(x + dx / 2, dx)
}

//////////////////////////////////////////////////////////////////////////////
// Utility: Iterators

function* range(start, stop) {
    for (let i = start; i < stop; ++i) {
        yield i
    }
}

function* map(f, xs, start) {
    start = start || 0
    for (const x of xs) {
        yield f(x, start)
        start += 1
    }
}

function* filter(f, xs) {
    for (const x of xs) {
        if (f(x)) {
            yield x
        }
    }
}

function* take(n, xs) {
    let i = 0
    for (const x of xs) {
        if (i >= n) {
            break
        }
        yield x
        i += 1
    }
}

function sum(xs) {
    let s = 0
    for (const x of xs) {
        s += x
    }
    return s
}

//////////////////////////////////////////////////////////////////////////////
// Utility: Arrays

function arrayEqual(xs, ys, cmp) {
    if (!cmp) {
        cmp = defaultCmp
    }
    const length = xs.length
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

function arrayRemoveMany(xs, indices) {
    xs = xs.slice()
    indices = Array.from(indices)
    // sort in reverse order
    indices.sort((x, y) => y - x)
    const n = indices.length
    for (let i = 0; i < n; ++i) {
        if (indices[i] >= xs.length) {
            throw new Error("index out of range")
        }
        xs.splice(indices[i], 1)
    }
    return xs
}

function arrayIntercalate(sep, xs) {
    const n = xs.length
    let ys = []
    for (let i = 0; i < n; ++i) {
        if (i != 0) {
            ys.push(sep)
        }
        ys.push(xs[i])
    }
    return ys
}

//////////////////////////////////////////////////////////////////////////////
// Utility: Objects

// Kind of a hack -- hopefully we won't need this in the future.
let currentId = 0
const idMap = new WeakMap()
function objectId(obj) {
    if (!idMap.has(obj)) {
        idMap.set(obj, ++currentId)
    }
    return idMap.get(obj)
}

function deduplicateObject(obj, image) {
    const ks = Object.keys(obj)
    if (ks.length != Object.keys(image).length) {
        return obj
    }
    ks.forEach(k => {
        if (!(image.hasOwnProperty(k) && obj[k] === image[k])) {
            return obj
        }
    })
    return image
}

function deepClone(x) {
    return JSON.parse(JSON.stringify(x))
}

function deepFreeze(obj) {
    if (Object.isFrozen(obj)) {
        return obj
    }
    Object.freeze(obj)
    const keys = Object.keys(obj)
    let i = keys.length
    while (i--) {
        deepFreeze(obj[keys[i]])
    }
    return obj
}

function deepDiff(x, y) {
    if (x === y) {
        return null
    }
    if (typeof x != typeof y) {
        return {type: "typeMismatch", left: x, right: y}
    }

    const xIsArray = Array.isArray(x)
    const yIsArray = Array.isArray(y)
    if (xIsArray || yIsArray) {
        if (xIsArray != yIsArray) {
            return {type: "typeMismatch", left: x, right: y}
        }
        const nx = x.length
        const ny = y.length
        if (nx != ny) {
            return {type: "lengthMismatch", left: nx, right: ny}
        }
        for (const [i, xi] of x.entries()) {
            const diff = deepDiff(xi, y[i])
            if (diff) {
                return {type: "propertyValueMismatch", prop: i, diff: diff}
            }
        }
        return null
    }

    if (typeof x == "object") {
        const xProto = Object.getPrototypeOf(x)
        const yProto = Object.getPrototypeOf(y)
        if (xProto != yProto) {
            return {type: "prototypeMismatch", left: xProto, right: yProto}
        }
        for (const k of Object.keys(x)) {
            if (!y.hasOwnProperty(k)) {
                return {type: "missingProperty", left: k}
            }
            const diff = deepDiff(x[k], y[k])
            if (diff) {
                return {type: "propertyValueMismatch", prop: k, diff: diff}
            }
        }
        for (const k of Object.keys(y)) {
            if (!x.hasOwnProperty(k)) {
                return {type: "missingProperty", right: k}
            }
        }
        return null
    }
    return {type: "notEqual", left: x, right: y}
}

function assertEq(x, y) {
    const diff = deepDiff(x, y)
    if (diff != null) {
        console.warn(x, y, diff)
    }
    console.assert(diff == null)
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

/** Get the height of the arc between 1 and 2 that also passes through 0. */
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

/** Choose smoothness = 1 for a Catmull-Rom spline. */
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
        } else {
            if (elem.getAttribute(k) != v) {
                elem.setAttribute(k, v)
            }
        }
    }
    const symKeys = Object.getOwnPropertySymbols(attrs)
    i = symKeys.length
    while (i--) {
        let symbols = elem[VNODE_SYMBOLS]
        if (!symbols) {
            symbols = {}
            elem[VNODE_SYMBOLS] = symbols
        }
        const k = symKeys[i]
        symbols[k] = attrs[k]
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
                i += 1
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
                        if ((oldChild[VNODE_SYMBOLS] || {})[VNODE_KEY] == key) {
                            break
                        }
                        elem.removeChild(oldChild)
                    } else {
                        elem.insertBefore(fragment, oldChild)
                        j += 1
                    }
                    oldChild = oldChildren[j]
                }
            } else if (oldChild && (oldChild[VNODE_SYMBOLS] || {})[VNODE_KEY]) {
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
                    j += 1
                    child.renderTo(oldChild)
                    continue
                } else {
                    elem.removeChild(oldChild)
                }
            }
            child = child.create()
        } else if (typeof child == "string") {
            if (oldChild instanceof Text) {
                elem.insertBefore(fragment, oldChild)
                j += 1
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
            j += 1
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
        if (!elem) {
            throw new Error("invalid elem")
        }
        if (spec.attributes) {
            vnodeAmendAttributes(spec.attributes, elem)
        }
        vnodeRenderChildren(spec.children, elem)
    })
}

//////////////////////////////////////////////////////////////////////////////
// Deltas
//
// Without qualification, "deltas" refer to Kronecker deltas involving
// magnitudes (j), not projections (m).
//
// Each delta is an array of at least two magnitude variables.
// Within a given list of deltas, the variables must be all distinct.

/** Merges the given deltas and simplifies the result. */
function mergeDeltas(...deltaLists) {
    let finalDeltas = []
    let finder = {} // {[entry]: [deltas]} (singleton list)
    deltaLists.forEach(deltas => deltas.forEach(delta => {
        if (delta.length < 2) {
            return
        }
        let found = null
        for (let i = 0; i < delta.length; ++i) {
            found = finder[delta[i]]
            if (found) {
                break
            }
        }
        if (found == null) {
            found = [[]]
            finalDeltas.push(found[0])
        }
        delta.forEach(x => {
            const exists = finder[x]
            if (!exists) {
                found[0].push(x)
                finder[x] = found
            } else if (exists[0] != found[0]) {
                // merge deltas
                found[0].push(...exists[0])
                exists[0].splice(0, exists[0].length)
                exists[0] = found[0]
            }
        })
    }))
    // remove the husks
    return Object.freeze(finalDeltas.map(Object.freeze)
                                    .filter(delta => delta.length > 1))
}

/** Tests whether the 'subdeltas' is a subset of (implied by) 'deltas'. */
function containsDeltas(deltas, subdeltas) {
    deltas = mergeDeltas(deltas)
    let finder = {}
    deltas.forEach(delta => delta.forEach(x =>
        finder[x] = delta
    ))
    for (let j = 0; j < subdeltas.length; ++j) {
        const subdelta = subdeltas[j]
        let found = null
        for (let i = 0; i < subdelta.length; ++i) {
            found = finder[subdelta[i]]
            if (found) {
                break
            }
        }
        if (found) {
            for (let i = 0; i < subdelta.length; ++i) {
                if (finder[subdelta[i]] != found) {
                    return false
                }
            }
        } else {
            return false
        }
    }
    return true
}

function equalDeltas(deltas1, deltas2) {
    return containsDeltas(deltas1, deltas2) && containsDeltas(deltas2, deltas1)
}

function findDeltaEntry(deltas, entry) {
    for (let j = 0; j < deltas.length; ++j) {
        for (let i = 0; i < deltas[j].length; ++i) {
            if (deltas[j][i] == entry) {
                return [j, i]
            }
        }
    }
    return null
}

function relatedDeltas(deltas, entry) {
    const ji = findDeltaEntry(deltas, entry)
    if (!ji) {
        return []
    }
    const [j, i] = ji
    return arrayRemoveMany(deltas[j], [i]).map(x => [entry, x])
}

/** Warning: this may not preserve the diagram! */
function removeDeltaEntry(deltas, entry) {
    const ji = findDeltaEntry(deltas, entry)
    if (!ji) {
        return deltas
    }
    const [j, i] = ji
    return Object.freeze(Object.assign([], deltas, {
        [j]: Object.freeze(arrayRemoveMany(deltas[j], [i])),
    })).filter(delta => delta.length > 1)
}

//////////////////////////////////////////////////////////////////////////////
// Superlines
//
// Superlines are just magnitudes (j).

const EMPTY_SUPERLINE = Object.freeze({
    phase: 0,
    summed: false,
    weight: 0,
})

/** Convert a superline-like object into a proper superline. */
function ensureSuperline(superline) {
    return Object.freeze(Object.assign({}, EMPTY_SUPERLINE, superline))
}

/** Increment the numeric suffix of a string by one.  If there is no suffix,
    `1` is appended. */
function newLabel(label) {
    const match = /^([\s\S]*?)(\d*)$/.exec(label)
    return match[1] + (Number(match[2]) + 1).toString()
}

/** Generate `count` fresh labels. */
function availLabels(obj) {
    // avoid "0", which is reserved for the actual zero
    return filter(i => !obj.hasOwnProperty(i),
                  map(i => i.toString(),
                      range(1, Number.POSITIVE_INFINITY)))
}

/** Generate `count` fresh superline labels. */
function availSuperlineLabels(diagram) {
    let labels = Object.assign({}, diagram.superlines)
    for (const delta of diagram.deltas) {
        for (const x of delta) {
            labels[x] = true
        }
    }
    return availLabels(labels)
}

function isEmptySuperline(superline) {
    return !superline.phase && !superline.summed && !superline.weight
}

/** Avoid using this function because it doesn't handle zero lines. */
function mergeSuperlines(...superlines) {
    let finalSuperline = Object.assign({}, EMPTY_SUPERLINE)
    superlines.forEach(superline => {
        finalSuperline.phase = mod(finalSuperline.phase
                                 + (superline.phase || 0), 4)
        finalSuperline.weight += superline.weight || 0
        if (superline.summed != null) {
            finalSuperline.summed = superline.summed
        }
    })
    return Object.freeze(finalSuperline)
}

function mergeSuperlineLists(...superlineLists) {
    let finalSuperlines = {}
    for (const superlines of superlineLists) {
        for (const id of Object.keys(superlines)) {
            if (id == "0") {
                finalSuperlines[id] = EMPTY_SUPERLINE
                continue
            }
            const finalSuperline = finalSuperlines[id]
            let superline = superlines[id]
            if (finalSuperline) {
                superline = mergeSuperlines(finalSuperline, superline)
            }
            finalSuperlines[id] = superline
        }
    }
    for (const id of Object.keys(finalSuperlines)) {
        if (id.phase == 0 && id.weight == 0 && !id.summed) {
            delete finalSuperlines[id]
        }
    }
    return Object.freeze(finalSuperlines)
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
// The angle is usually ignored, but if the ratio of arcHeight to lineLength
// is too big, then 'angle' is used to break the degeneracy.
//
// In general, "pos" refers to a relative position on a line, with 0.0 being
// the leftmost point and 1.0 the rightmost.

const ZERO_LINE = Object.freeze({
    superline: "0",
    direction: 0,
    arrowPos: 0.5,
    arcHeight: 0.0,
    angle: 0.0,
    textPos: 0.5,
    textOffset: 0.0,
})

function newLine(superlineId) {
    return ensureLine({superline: superlineId})
}

function ensureLine(line) {
    return Object.freeze(Object.assign({}, ZERO_LINE, line))
}

function reverseLineDirection(line) {
    return Object.freeze(Object.assign({}, line, {direction: -line.direction}))
}

function reverseLine(line) {
    line = Object.assign({}, line)
    if (line.direction != null) {
        line.direction = -line.direction
    }
    if (line.arrowPos != null) {
        line.arrowPos = 1.0 - line.arrowPos
    }
    if (line.arcHeight != null) {
        line.arcHeight = -line.arcHeight
    }
    if (line.angle != null) {
        line.angle = mod(line.angle + Math.PI, 2 * Math.PI)
    }
    if (line.textPos != null) {
        line.textPos = 1.0 - line.textPos
    }
    if (line.textOffset != null) {
        line.textOffset = -line.textOffset
    }
    return Object.freeze(line)
}

function mergeDirections(...directions) {
    return mod(sum(directions) + 1, 4) - 1
}

/** Beware: this may result in a noncanonical line!
 *
 * Because of averaging, addition isn't quite associative with respect to the
 * superficial attributes.
 *
 * All lines must agree in superline.
 */
function rawConcatLines(...lines) {
    const n = lines.length
    const avgProps = ["arrowPos", "arcHeight",
                      "textPos", "textOffset"]
    let finalLine = {}
    let sinAngle = 0.0
    let cosAngle = 0.0
    let numAngles = 0
    let counts = {}
    for (const prop of avgProps) {
        counts[prop] = 0
    }
    for (const [i, line] of lines.entries()) {
        if (i == 0) {
            finalLine.superline = line.superline
        } else if (finalLine.superline != line.superline) {
            throw new Error("cannot add lines with different superlines")
        }
        if (line.direction != null) {
            if (finalLine.direction == null) {
                finalLine.direction = 0
            }
            finalLine.direction =
                mergeDirections(finalLine.direction, line.direction)
        }
        for (const prop of avgProps) {
            if (line[prop] != null) {
                if (finalLine[prop] == null) {
                    finalLine[prop] = 0
                }
                finalLine[prop] += line[prop]
                counts[prop] += 1
            }
        }
        if (line.angle != null) {
            sinAngle += Math.sin(line.angle)
            cosAngle += Math.cos(line.angle)
            numAngles += 1
        }
    }
    for (const prop of avgProps) {
        if (counts[prop]) {
            finalLine[prop] /= counts[prop]
        }
    }
    if (numAngles) {
        finalLine.angle = Math.atan2(sinAngle, cosAngle)
    }
    return Object.freeze(finalLine)
}

/** Like `rawConcatLines`, but does canonicalization and converts conflicting
 * superlines into a delta. */
function concatLines(...lines) {
    const canonicalized = canonicalizeLine(rawConcatLines(
        ...lines.map(line => Object.assign({}, line, {
            superline: lines[0].superline,
        }))))
    return {
        line: canonicalized.line,
        phase: canonicalized.phase,
        delta: lines.map(line => line.superline), // duplicates are fine
    }
}

function canonicalizeLine(line) {
    return {
        line: Object.freeze(Object.assign({}, line, {
            direction: line.direction % 2,
        })),
        phase: mod(Math.trunc(line.direction / 2), 2) * 2,
    }
}

function joinLines(line1, reverse1, line2, reverse2) {
    line1 = (reverse1 ? reverseLine : identity)(line1)
    line2 = (reverse2 ? reverseLine : identity)(line2)
    const superlines = [line1.superline, line2.superline].sort(defaultCmp)
    line1 = Object.assign({}, line1, {superline: superlines[0]})
    line2 = Object.assign({}, line1, {superline: superlines[0]})
    return Object.assign(canonicalizeLine(rawConcatLines(line1, line2)), {
        otherSuperline: superlines[1]
    })
}

function getLineInfoBetween(x0, y0, x1, y1, line) {
    const xMid = (x0 + x1) / 2
    const yMid = (y0 + y1) / 2
    let dx = x1 - x0
    let dy = y1 - y0
    let lineLength = Math.sqrt(dx * dx + dy * dy)
    const singular = lineLength == 0.0
    const trueAngle = Math.atan2(dy, dx)
    let angle = trueAngle
    let arcHeight = line.arcHeight
    // an semiarbitrary sign used to pick a consistent side on lines even
    // when the line is reversed
    const halfDisk = mod(line.angle + Math.PI / 2, 2 * Math.PI) < Math.PI
                   ? -1 : 1
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
        arcHeight = arcHeight == 0.0
                  ? 50.0 * halfDisk
                  : sgn(arcHeight) * clamp(50.0, Infinity, Math.abs(arcHeight))
    }
    const arc = arcInfo(lineLength, arcHeight)
    const c = (arc.radius - arcHeight) / lineLength
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
        halfDisk: halfDisk,
        arcHeight: arcHeight,
        arc: Object.assign(arc, arcEx),
    }
}

/** Calculate geometric information about a line. */
function getLineInfo(diagram, lineId) {
    const line = diagram.lines[lineId]
    const ends = endNodeIndices(diagram.nodes, lineId)
    let x0 = diagram.nodes[ends[0]].x
    let y0 = diagram.nodes[ends[0]].y
    let x1 = diagram.nodes[ends[1]].x
    let y1 = diagram.nodes[ends[1]].y
    return getLineInfoBetween(x0, y0, x1, y1, line)
}

/** Find the closest "pos" on a line. */
function findPosOnLine(lineInfo, x, y) {
    let pos, offset
    if (lineInfo.arcHeight == 0.0) {
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
        * sgn(lineInfo.arcHeight)
    }
    return {
        pos: pos,
        offset: offset,
    }
}

/** Translate from a relative "pos" to absolute (x, y) coordinates.  `shift`
    specifies an extra shift in absolute coordinates along the line. */
function positionOnLine(lineInfo, pos, shift) {
    const arc = lineInfo.arc
    if (lineInfo.arcHeight == 0.0) {
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

/** Create a terminal node. */
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
    let node = {
        type: "terminal",
        lines: Object.freeze([lineId]),
        variable: variable,
    }
    if (x != null) {
        node.x = x
    }
    if (y != null) {
        node.y = y
    }
    return Object.freeze(node)
}

/** Create a Wigner 3-jm node. */
function w3jNode(a, b, c, x, y) {
    let node = {
        type: "w3j",
        lines: Object.freeze([a, b, c]),
    }
    if (x !== undefined) {
        node.x = x
    }
    if (y !== undefined) {
        node.y = y
    }
    return Object.freeze(node)
}

function endNodeAndLineIndices(nodes, lineId) {
    let nodeAndLineIndices = []
    if (lineId == null) {
        throw new Error("lineId must not be null / undefined")
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
//
// NOTE: must maintain invariant that terminals precede all other nodes.
// Also, the order of nodes is critical!  If you move the nodes around,
// make sure the lines are also reversed.

const EMPTY_DIAGRAM = Object.freeze({
    nodes: Object.freeze([]),
    superlines: Object.freeze({}),
    lines: Object.freeze({}),

    // each entry is an array of j's that are equal to each other
    deltas: Object.freeze([]),

    // each entry is an array of the three j's in ascending order
    triangles: Object.freeze([]),
})

function ensureDiagram(diagram) {
    return Object.freeze(Object.assign({}, EMPTY_DIAGRAM, diagram))
}

function w3jDiagram(a, b, c, x, y) {
    if (a == b || b == c || c == a) {
        throw new Error("cannot create w3jDiagram with conflicting labels")
    }
    return ensureDiagram({
        nodes: Object.freeze([
            terminalNode(a, a, x - 50, y + 50),
            terminalNode(b, b, x + 50, y + 50),
            terminalNode(c, c, x, y - 70),
            w3jNode(a, b, c, x, y),
        ]),
        lines: Object.freeze({
            [a]: newLine(a),
            [b]: newLine(b),
            [c]: newLine(c),
        }),
        superlines: Object.freeze({
            [a]: EMPTY_SUPERLINE,
            [b]: EMPTY_SUPERLINE,
            [c]: EMPTY_SUPERLINE,
        }),
    })
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
    const lines = node
        .lines
        .filter(x => x != "0")
        .map((_, lineIndex) => {
            const angle = lineAngle(diagram, nodeIndex, lineIndex)
            return [lineIndex, mod(angle, 2 * Math.PI)]
        })
        .sort((line1, line2) => line1[1] - line2[1])
        .map(([x, _]) => x)
    switch (lines.length) {
        case 3:
            return {
                nnz: lines.length,
                orientation: permutSign([0, 1, 2], lines),
            }
        case 2:
            const s = node.lines.indexOf("0") == 1 ? -1 : 1
            return {
                nnz: lines.length,
                orientation: permutSign([0, 1], lines) * s,
            }
        default:
            return {nnz: lines.length}
    }
}

function mergeDiagrams(diagram1, diagram2) {
    let diagram = deepClone(diagram1)
    diagram.superlines = mergeSuperlineLists(diagram1.superlines,
                                             diagram2.superlines)
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
    diagram = new Diagram(diagram)
    const terminal1 = diagram.node(terminalIndex1)
    const terminal2 = diagram.node(terminalIndex2)
    if (terminal1.type != "terminal" || terminal2.type != "terminal") {
        throw "cannot join non-terminals"
    }
    const line1 = terminal1.line(0)
    const line2 = terminal2.line(0)
    let newDiagram = deepClone(diagram.rawDiagram)
    if (line1.id == line2.id) {
        if (line1.direction != 0) {
            return "cannot form a directed loop"
        }
        newDiagram.superlines[line1.superlineId].weight += 2
    } else {
        // TODO can we abstract out this pattern? we do this a lot
        const combined = concatLines(line1.reverse().line, line2.line)
        const f = lexicalCmp([line1.node(1).index, line1.lineIndex(1)],
                             [line2.node(1).index, line2.lineIndex(1)],
                             defaultCmp) > 0
                ? reverseLine : identity
        newDiagram.lines[line1.id] = f(combined.line)
        newDiagram.superlines[line1.superlineId] = mergeSuperlines(
            line2.superline,
            line1.superline,
            {phase: combined.phase})
        line2.rawAssign(newDiagram, 1, line1.id)
        newDiagram.superlines[line2.superlineId] = EMPTY_SUPERLINE
        newDiagram.deltas = mergeDeltas(newDiagram.deltas, [combined.delta])
    }
    delete newDiagram.lines[line2.id]
    newDiagram.nodes = arrayRemoveMany(newDiagram.nodes,
                                       [terminal1.index, terminal2.index])
    return new Diagram(newDiagram).removeUnusedSuperlines().rawDiagram
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
    diagram = new Diagram(diagram)
    return diagram.substitute({}, {
        [diagram.line(lineId).superlineId]: {phase: 2}
    }).rawDiagram
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
        diagram.nodes = arrayRemoveMany(diagram.nodes, [nodeIndex, otherIndex])
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

function inferDeltas(diagram) {
    throw new Error("inferDeltas: not yet implemented")
    return []
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
    diagram.nodes[nodeIndex].lines.forEach(lineId =>
        diagram.superlines = mergeSuperlineLists(diagram.superlines, {
            [diagram.lines[lineId].superline]: {phase: 1},
        }))
    return diagram
}

function trianglePhaseRule(diagram, triangleIndex) {
    diagram = deepClone(diagram)
    diagram.nodes[nodeIndex].lines.forEach(lineId =>
        diagram.superlines = mergeSuperlineLists(diagram.superlines, {
            [diagram.lines[lineId].superline]: {phase: 2},
        }))
    return diagram
}

function flipW1jRule(diagram, lineId) {
    if (diagram.lines[lineId].superline == "0") {
        diagram = deepClone(diagram)
        diagram.lines[lineId].direction =
            (diagram.lines[lineId].direction + 2) % 3 - 1
    } else if (diagram.lines[lineId].direction) {
        diagram = deepClone(diagram)
        diagram.lines[lineId].direction *= -1
        diagram.superlines = mergeSuperlineLists(diagram.superlines, {
            [diagram.lines[lineId].superline]: {phase: 2}})
    }
    return diagram
}

function threeArrowRule(diagram, nodeIndex) {
    diagram = deepClone(diagram)
    let node = diagram.nodes[nodeIndex]
    if (node.type != "w3j") {
        return diagram
    }
    const loop = findW3jLoop(new Diagram(diagram).node(nodeIndex))
    let loopId = null
    if (typeof loop == "object") {
        loopId = loop.loopLine.id
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
        let line = Object.assign({}, diagram.lines[lineId])
        if (isLeftOfLine(diagram.nodes, nodeIndex, lineIndex)) {
            line.direction += direction
        } else {
            line.direction -= direction
        }
        diagram.lines[lineId] = line
    })
    node.lines.forEach(function(lineId, lineIndex) {
        const line = diagram.lines[lineId]
        const canonicalized = canonicalizeLine(line)
        diagram.lines[lineId] = canonicalized.line
        diagram.superlines = mergeSuperlineLists(diagram.superlines, {
            [line.superline]: {phase: canonicalized.phase},
        })
    })
    return diagram
}

/** @returns {Object<lineId, [nodeIndex1, lineIndex1,
  *                           nodeIndex2, lineIndex2]>} */
function getLineEnds(diagram) {
    let lineEnds = {}
    diagram.nodes.forEach((node, nodeIndex) => {
        node.lines.forEach((lineId, lineIndex) => {
            let ends = lineEnds[lineId]
            if (!ends) {
                lineEnds[lineId] = [nodeIndex, lineIndex]
            } else if (ends.length == 2) {
                if (lexicalCmp([ends[0], ends[1]],
                               [nodeIndex, lineIndex],
                               defaultCmp) < 0) {
                    lineEnds[lineId].push(nodeIndex, lineIndex)
                } else {
                    lineEnds[lineId].splice(0, 0, nodeIndex, lineIndex)
                }
            } else {
                throw new Error("line must be connected to exactly 2 nodes")
            }
        })
    })
    return lineEnds
}

class DiagramLine {
    constructor(diagram, id, reversed) {
        console.assert(diagram)
        this.diagram = diagram
        this.id = id
        this.reversed = Boolean(reversed)
        Object.freeze(this)
    }

    get rawLine() {
        return this.diagram.rawDiagram.lines[this.id]
    }

    get line() {
        const f = this.reversed ? reverseLine : identity
        return f(this.diagram.rawDiagram.lines[this.id])
    }

    get superline() {
        return this.diagram.superline(this.superlineId)
    }

    get superlineId() {
        return this.line.superline
    }

    get direction() {
        if (this.rawLine.direction == null) {
            return null
        }
        return this.rawLine.direction * (this.reversed * 2 - 1)
    }

    node(end) {
        const lineEnds = this.diagram._lineEnds[this.id]
        return new DiagramNode(this.diagram,
                               lineEnds[(this.reversed + end) % 2 * 2])
    }

    lineIndex(end) {
        const lineEnds = this.diagram._lineEnds[this.id]
        return lineEnds[(this.reversed + end) % 2 * 2 + 1]
    }

    cycNodeLine(end, offset) {
        return this.node(end).line((this.lineIndex(end) + offset) % 3)
    }

    /** Called automatically when used as the name of a property. */
    toString() {
        return (this.reversed ? "-" : "+") + this.id
    }

    signedRebase(diagram) {
        return diagram.signedLine(this.id, this.reversed)
    }

    reverse() {
        return new DiagramLine(this.diagram, this.id, !this.reversed)
    }

    concat(...lines) {
        return rawConcatLines(lines.map(line => line.line))
    }

    static removeSign(signedId) {
        const id = signedId.substr(1)
        let reversed = false
        if (signedId[0] == "-") {
            reversed = !reversed
        } else if (signedId[0] != "+") {
            throw new Error(`not a valid signed line ID: ${signedId}`)
        }
        return [id, reversed]
    }

    rawAssign(rawDiagram, end, lineId) {
        rawDiagram.nodes[this.node(end).index].lines[this.lineIndex(end)] = lineId
    }
}

class DiagramNode {
    constructor(diagram, index) {
        this.diagram = diagram
        this.index = index
        Object.freeze(this)
    }

    get rawNode() {
        return this.diagram.rawDiagram.nodes[this.index]
    }

    get type() {
        return this.rawNode.type
    }

    get variable() {
        if (this.type != "terminal") {
            throw new Error("node is not a terminal")
        }
        return this.rawNode.variable
    }

    get numLines() {
        return this.rawNode.lines.length
    }

    line(index) {
        const lineId = this.rawNode.lines[index]
        const lineEnd = this.diagram._lineEnds[lineId]
        const reversed = lineEnd[2] == this.index && lineEnd[3] == index
        return new DiagramLine(this.diagram, lineId, reversed)
    }

    lines() {
        return map(index => this.line(index), range(0, this.numLines))
    }
}

class Diagram {
    constructor(rawDiagram) {
        this.rawDiagram = rawDiagram || EMPTY_DIAGRAM
        this._cache = {}
        Object.freeze(this)
    }

    static deserialize(s) {
        return new Diagram(JSON.parse(s))
    }

    get serialize() {
        return JSON.stringify(this.rawDiagram)
    }

    get _lineEnds() {
        let lineEnds = this._cache.lineEnds
        if (!lineEnds) {
            lineEnds = getLineEnds(this.rawDiagram)
            this._cache.lineEnds = lineEnds
        }
        return lineEnds
    }

    get terminals() {
        let terminals = this._cache.terminals
        if (!terminals) {
            terminals = {}
            for (const node of this.nodes()) {
                if (node.type == "terminal") {
                    if (terminals[node.variable]) {
                        throw new Error("duplicate terminal variables")
                    }
                    terminals[node.variable] = node
                }
            }
            this._cache.terminals = terminals
        }
        return terminals
    }

    terminal(variable) {
        return this.terminals[variable]
    }

    get numNodes() {
        return this.rawDiagram.nodes.length
    }

    node(index) {
        return new DiagramNode(this, index)
    }

    * nodes() {
        const numNodes = this.numNodes
        for (let index = 0; index < numNodes; ++index) {
            yield this.node(index)
        }
    }

    get lineIds() {
        return Object.keys(this.rawDiagram.lines)
    }

    signedLine(signedId, reversed) {
        const [id, sign] = DiagramLine.removeSign(signedId)
        return this.line(id, sign != Boolean(reversed))
    }

    line(id, reversed) {
        return new DiagramLine(this, id, reversed)
    }

    lines() {
        return map(id => this.line(id), this.lineIds)
    }

    get superlines() {
        return this.rawDiagram.superlines
    }

    superline(superlineId) {
        return this.superlines[superlineId]
    }

    isEquallyConstrained(superlineId, deltas) {
        const related = relatedDeltas(this.rawDiagram.deltas, superlineId)
        const otherRelated = relatedDeltas(deltas, superlineId)
        return equalDeltas(related, otherRelated)
    }

    renameLines(renames) {
        let newLines = {}
        for (const lineId of Object.keys(this.rawDiagram.lines)) {
            const newLineId = renames.hasOwnProperty(lineId)
                            ? renames[lineId]
                            : lineId
            newLines[newLineId] = this.rawDiagram.lines[lineId]
        }
        return new Diagram(Object.freeze(Object.assign({}, this.rawDiagram, {
            nodes: Object.freeze(this.rawDiagram.nodes.map(node =>
                Object.freeze(Object.assign({}, node, {
                    lines: Object.freeze(node.lines.map(lineId =>
                        renames.hasOwnProperty(lineId)
                                                              ? renames[lineId]
                                                              : lineId
                    )),
                }))
            )),
            lines: Object.freeze(newLines),
        })))
    }

    removeUnusedSuperlines() {
        let marked = Object.assign({}, this.rawDiagram.superlines)
        for (const superlineId of Object.keys(marked)) {
            if (!isEmptySuperline(marked[superlineId])) {
                delete marked[superlineId]
            }
        }
        for (const line of this.lines()) {
            delete marked[line.superlineId]
        }
        for (const delta of this.rawDiagram.deltas) {
            for (const superlineId of delta) {
                delete marked[superlineId]
            }
        }
        let superlines = Object.assign({}, this.rawDiagram.superlines)
        for (const superlineId of Object.keys(marked)) {
            delete superlines[superlineId]
        }
        if (superlines["0"]) {
            superlines["0"] = EMPTY_SUPERLINE
        }
        return new Diagram(Object.freeze(Object.assign({}, this.rawDiagram, {
            superlines: superlines,
        })))
    }

    /** Substitute a portion of this diagram for another subdiagram.
     *
     * Patterns are similar to diagrams, but also slightly different:
     *
     * - lineIds must refer to existing lines and must be prefixed by `+` or `-`
     *   to indicate whether it should be treated as reversed
     * - `line.direction: MATCH_ANY` will absorb any direction
     */
    substitute(pattern, replacement) {
        // schematic idea:
        //
        // orig: --- X1 --- N1 --- N2 --- X2 ---
        // patt:     T1 --- N1 --- N2 --- T2
        // repl:     T1 ------- N3 ------ T2
        // new:  --- X1 ------- N3 ------ X2 ---
        //
        // anchor = a node that where gluing occurs;
        //   for 'pattern', all terminals are anchors
        //   for 'replacement', only terminals that match the pattern are
        //
        // the interesting/tricky parts occur on the terminals of the pattern;
        // this is where we have to glue the original diagram to the
        // replacement subdiagram.
        //
        // we have to make sure that the phases on X1-N1 are appropriately
        // merged into X1-N3 and similarly for N2-X2, and also account for
        // degenerate cases

        pattern = ensureDiagram(pattern)
        replacement = ensureDiagram(replacement)
        const rawDiagram = this.rawDiagram
        const pattDiagram = new Diagram(pattern)
        let replDiagram = new Diagram(replacement)

        // DELTAS
        // ------

        // (shared with other parts)
        let deltaMerge = [rawDiagram.deltas, replacement.deltas]

        // match deltas
        if (!containsDeltas(rawDiagram.deltas, pattern.deltas)) {
            throw new Error("mismatch in deltas")
        }

        // SHARED LINES
        // ------------

        // we allow a limited form of sharing: two pattern lines can map onto
        // the same original line as long as the orientation is different;
        // this in turn has the potential to create loops
        let sharedLines = {}
        let seenLines = {}
        for (const pattLine of pattDiagram.lines()) {
            const id = DiagramLine.removeSign(pattLine.id)[0]
            const seen = seenLines[id]
            if (seen) {
                if (!(seen.node(0).type == "terminal" ||
                      seen.node(1).type == "terminal")
                    || !(pattLine.node(0).type == "terminal" ||
                         pattLine.node(1).type == "terminal")) {
                    throw new Error("a single internal line cannot "
                                  + "match multiple lines")
                }
                if (pattLine.node(0).type == "terminal"
                    && pattLine.node(1).type == "terminal") {
                    // this causes ambiguity and breakage
                    throw new Error("a lone pattern line cannot be shared")
                }
                if (sharedLines[pattLine.id] || sharedLines[seen.id]) {
                    throw new Error("looped lines can only appear twice")
                }
                sharedLines[pattLine.id] = seen.id
                sharedLines[seen.id] = pattLine.id
                continue
            }
            seenLines[id] = pattLine
        }

        // LINE RENAMING
        // -------------

        // (shared with other parts)
        let newLines = Object.assign({}, rawDiagram.lines)

        // must remove the pattern lines first, because we want to know which
        // line IDs are available to us
        for (const pattLine of pattDiagram.lines()) {
            const lineId = DiagramLine.removeSign(pattLine.id)[0]
            console.assert(newLines[lineId] || sharedLines[pattLine.id])
            delete newLines[lineId]
        }
        // rename the replacement diagram to avoid conflicting line IDs
        // and also replace the dummy line IDs
        let freshLineIds = availLabels(newLines)
        let lineRenames = {}
        for (const line of replDiagram.lines()) {
            if (line.id.startsWith("$") || newLines.hasOwnProperty(line.id)) {
                lineRenames[line.id] = freshLineIds.next().value
            }
        }
        replDiagram = replDiagram.renameLines(lineRenames)
        replacement = replDiagram.rawDiagram

        // SUPERLINES
        // ----------

        // (shared with other parts)
        let superlineMerge = [rawDiagram.superlines]

        // match superlines and adjust phases
        for (const superlineId of Object.keys(pattern.superlines)) {
            const pattSuperline = pattern.superlines[superlineId]
            const superline = rawDiagram.superlines[superlineId]
            if (pattSuperline.summed) {
                if (!superline.summed) {
                    throw new Error("expected summed superline")
                }
                if (pattSuperline.weight != superline.weight) {
                    throw new Error("weight of summed superline must match")
                }
                if (pattSuperline.phase != superline.phase) {
                    throw new Error("phase of summed superline must match")
                }
                if (!this.isEquallyConstrained(superlineId,
                                               replacement.deltas)) {
                    throw new Error("deltas of summed superline must match")
                }
                superlineMerge.push({[superlineId]: {summed: false}})
            }
            superlineMerge.push({
                [superlineId]: {
                    weight: -pattSuperline.weight,
                    phase: -pattSuperline.phase,
                },
            })
        }
        // forbid variable capture
        for (const superlineId of Object.keys(replacement.superlines)) {
            const replSuperline = replacement.superlines[superlineId]
            if (replSuperline.summed) {
                const pattSuperline = pattern.superlines[superlineId]
                if (pattSuperline) {
                    if (!pattSuperline.summed) {
                        throw new Error("summed variable conflicts "
                                      + "with pattern")
                    }
                } else if (rawDiagram.superlines[superlineId]) {
                    throw new Error("summed variable conflicts with original")
                }
            }
        }
        superlineMerge.push(replacement.superlines)

        // NODES
        // -----

        // match nodes and adjust phases
        for (const pattNode of pattDiagram.nodes()) {
            if (pattNode.type == "terminal") {
                continue
            }
            if (pattNode.numLines == 0) {
                throw new Error("singleton nodes are not supported")
            }
            const node = pattNode.line(0).signedRebase(this).node(0)
            if (node.type != pattNode.type) {
                throw new Error("node type mismatch")
            }
            const nodeLines = Array.from(node.rawNode.lines)
            const pattNodeLines = Array.from(pattNode.rawNode.lines)
            if (node.type == "w3j") {
                const uniqueLines = permutSign(nodeLines, nodeLines)
                const sign = permutSign(
                    nodeLines,
                    pattNodeLines.map(signedId =>
                        DiagramLine.removeSign(signedId)[0]))
                if (uniqueLines && sign == 0) {
                    throw new Error("node lines mismatch")
                }
                if (sign < 0) {
                    superlineMerge.push({
                        [nodeLines[0]]: {phase: 1},
                        [nodeLines[1]]: {phase: 1},
                        [nodeLines[2]]: {phase: 1},
                    })
                }
            } else if (lexicalCmp(nodeLines, pattNodeLines, defaultCmp) != 0) {
                throw new Error("node lines mismatch")
            }
        }

        // LINES
        // -----

        // match lines and adjust phases
        for (const pattLine of pattDiagram.lines()) {
            const pattNode0 = pattLine.node(0)
            const pattNode1 = pattLine.node(1)
            const line = pattLine.signedRebase(this)
            if (line.superlineId != pattLine.superlineId) {
                throw new Error("superline ID mismatch in lines")
            }
            if (pattNode0.type != "terminal"
                && pattNode1.type != "terminal"
                && pattLine.direction != null) {
                const diffDirection = mergeDirections(line.direction,
                                                     -pattLine.direction)
                if (diffDirection % 2 != 0) {
                    throw new Error("directedness mismatch in lines")
                }
                superlineMerge.push({
                    [line.superlineId]: {phase: diffDirection},
                })
            }
        }

        let nodes = rawDiagram.nodes.map(node => Object.assign({}, node, {
            lines: node.lines.slice(),
        }))
        let replNodes = replacement.nodes.map(node => Object.assign({}, node, {
            lines: node.lines.slice(),
        }))
        let seenReplLines = {}
        // find the node that we should join to
        //
        // context: this, pattDiagram, nodes, replNodes, seenReplLines
        const findJoiningNode = replLine => {
            // There's several cases to consider here.
            //
            // In the simplest case, with no shared lines, we have:
            //
            // orig  N1 --->---
            // patt     ---<--- TP
            // repl     --->--- TR
            //
            // A shared line case might look like this:
            //
            //          -------------------------
            //         /                         \
            //         \                         /
            // orig     -->---             ------
            // patt    ---<--- TP1     TP2 --->---
            // repl    --->--- TR1     TR2 --->---
            //
            // This could go on for several jumps, if the node opposite to
            // TR2 is an anchor too.  It could even loop back to itself.
            let lines = [replLine.line]
            while (true) {
                const replNode = replLine.node(1)
                const result = {
                    type: "replacement",
                    lines: lines,
                    line: replLine,
                    lineId: replLine.id,
                    nodes: replNodes,
                }
                if (replNode.type != "terminal") {
                    return result
                }
                const pattNode = pattDiagram.terminal(replNode.variable)
                if (!pattNode) {
                    return result
                }
                seenReplLines[replLine.id] = true
                const pattLine = pattNode.line(0)
                const line = pattLine.signedRebase(this).reverse()
                // avoid consuming phases twice if the pattern line is
                // terminal-to-terminal
                if (pattLine.node(1).type != "terminal" || !pattLine.reversed) {
                    lines.push(pattLine.line, line.line)
                }
                const otherPattLineId = sharedLines[pattLine.id]
                if (!otherPattLineId) {
                    // no shared line, so joining node is from original diagram
                    return {
                        type: "original",
                        lines: lines,
                        line: line,
                        lineId: replLine.id,
                        nodes: nodes,
                    }
                }
                // we have shared line, so joining node comes from elsewhere
                const otherPattLine = pattDiagram.line(otherPattLineId,
                                                       pattLine.reversed)
                const otherPattNode = otherPattLine.node(0)
                if (otherPattNode.type != "terminal") {
                    throw new Error("expected terminal node")
                }
                lines.push(otherPattLine.reverse().line)
                replLine = replDiagram.terminal(otherPattNode.variable).line(0)
                if (seenReplLines[replLine.id]) {
                    // we found a loop
                    return {
                        type: "cycle",
                        lines: lines,
                    }
                }
                lines.push(replLine)
            }
        }
        function getPart(joining) {
            switch (joining.type) {
                case "replacement":
                    return joining.line.node(1).type == "terminal" ? -1 : 1
                case "original":
                    return 0
                default:
                    throw new Error("joining type not valid here")
            }
        }
        for (const replLine of replDiagram.lines()) {
            if (seenReplLines[replLine.id]) {
                continue // avoid double-counting
            }
            const joining0 = findJoiningNode(replLine.reverse())
            if (joining0.type == "cycle") {
                // special case: cycle
                const combined = concatLines(...joining0.lines)
                deltaMerge.push([combined.delta])
                if (combined.line.direction % 2) {
                    throw new Error("directed loops are forbidden")
                }
                superlineMerge.push({
                    [combined.line.superline]: {
                        phase: combined.phase,
                        weight: 2,
                    },
                })
                continue
            }
            const joining1 = findJoiningNode(replLine)
            const lineId = joining0.lineId
            const nodeIndex0 = joining0.line.node(1).index
            const nodeIndex1 = joining1.line.node(1).index
            const lineIndex0 = joining0.line.lineIndex(1)
            const lineIndex1 = joining1.line.lineIndex(1)
            const lines = joining0.lines
                                  .map(reverseLine)
                                  .reverse()
                                  .concat(joining1.lines.slice(1))
            const f = lexicalCmp([getPart(joining0), nodeIndex0, lineIndex0],
                                 [getPart(joining1), nodeIndex1, lineIndex1],
                                 defaultCmp) > 0
                    ? reverseLine : identity
            joining0.nodes[nodeIndex0].lines[lineIndex0] = lineId
            joining1.nodes[nodeIndex1].lines[lineIndex1] = lineId
            const combined = concatLines(...lines)
            deltaMerge.push([combined.delta])
            console.assert(!newLines[lineId])
            newLines[lineId] = ensureLine(f(combined.line))
            superlineMerge.push({
                [combined.line.superline]: {phase: combined.phase},
            })
        }

        const newNodes = Array.prototype.concat(
            replacement.nodes.filter((node, nodeIndex) =>
                node.type == "terminal"
                && !pattDiagram.terminal(node.variable)),
            arrayRemoveMany(
                nodes,
                Array.from(
                    map(node => node.line(0)
                                    .signedRebase(this)
                                    .node(0)
                                    .index,
                        filter(node => node.type != "terminal",
                               pattDiagram.nodes())))),
            replacement.nodes.filter(node => node.type != "terminal"))

        return new Diagram(Object.freeze(Object.assign({}, rawDiagram, {
            nodes: newNodes,
            lines: newLines,
            superlines: mergeSuperlineLists(...superlineMerge),
            deltas: mergeDeltas(...deltaMerge),
        }))).removeUnusedSuperlines()
    }
}

function findW3jLoop(node) {
    if (node.type != "w3j") {
        return "not a Wigner 3-jm symbol"
    }
    for (let i = 0; i < 3; ++i) {
        if (node.line(i).id == node.line((i + 1) % 3).id) {
            return {
                cutLine: node.line((i + 2) % 3),
                loopLine: node.line(i),
            }
        }
    }
    return "no loops found"
}

function loopElimRule(diagram, lineId, nodeIndex) {
    diagram = new Diagram(diagram)
    const loopNode = diagram.line(lineId).node(0)
    if (loopNode.index != diagram.line(lineId).node(1).index) {
        return "no loops found"
    }
    const loop = findW3jLoop(loopNode)
    if (typeof loop != "object") {
        return loop
    }
    if (loop.loopLine.direction == 0) {
        return "loop must be directed"
    }
    const otherNode = loop.cutLine.node(1)
    if (otherNode.type != "w3j") {
        return "other node is not a Wigner 3-jm symbol"
    }
    if (otherNode.index != nodeIndex && loopNode.index != nodeIndex) {
        return "not sure what you're trying to do"
    }
    const ld = loop.loopLine
    const lc = loop.cutLine
    const lb = loop.cutLine.cycNodeLine(1, 1).reverse()
    const la = loop.cutLine.cycNodeLine(1, 2).reverse()
    if (la.id == lb.id && la.direction == 0) {
        return "other loop must be directed too"
    }
    const md = ld.toString()
    const mc = lc.toString()
    const mb = lb.toString()
    const ma = la.toString()
    const jd = ld.superlineId
    const jc = lc.superlineId
    const jb = lb.superlineId
    const ja = la.superlineId
    return diagram.substitute({
        nodes: [
            terminalNode(ma, "a"),
            terminalNode(mb, "b"),
            w3jNode(md, md, mc),
            w3jNode(mc, mb, ma),
        ],
        lines: {
            [md]: {superline: jd, direction: +1},
            [mc]: {superline: jc, direction: null},
            [mb]: {superline: jb, direction: 0},
            [ma]: {superline: ja, direction: +1},
        },
        superlines: {
            [ja]: {weight: 1},
        },
    }, {
        nodes: [
            terminalNode("$1", "a"),
            terminalNode("$1", "b"),
        ],
        lines: {
            $1: {superline: jb, direction: 0},
        },
        superlines: {
            [jd]: {weight: 1},
            "0": {},
        },
        deltas: [
            ["0", jc],
        ],
    }).rawDiagram
}

function loopIntroRule(diagram, lineId, xy1, xy2) {
    diagram = new Diagram(diagram)
    const line = diagram.line(lineId)
    return diagram.substitute({
        nodes: [
            terminalNode(line.toString(), "a"),
            terminalNode(line.toString(), "b"),
        ],
        lines: {
            [line]: {superline: line.superlineId, direction: 0},
        },
    }, {
        nodes: [
            terminalNode("$1", "a"),
            terminalNode("$2", "b"),
            w3jNode("$4", "$4", "$3", ...xy2),
            w3jNode("$3", "$2", "$1", ...xy1),
        ],
        lines: {
            $4: {superline: "0", direction: +1, arcHeight: 50.0},
            $3: {superline: "0", direction: 0},
            $2: {superline: line.superlineId, direction: 0},
            $1: {superline: line.superlineId, direction: +1},
        },
        superlines: {
            [line.superlineId]: {weight: 1},
        },
    }).rawDiagram
}

function w3jIntroRule(diagram, lineId1, lineId2, reversed) {
    diagram = new Diagram(diagram)
    let line1 = diagram.line(lineId1)
    let line2 = diagram.line(lineId2)
    if (((line1.node(1).rawNode.x - line1.node(0).rawNode.x)
        * (line2.node(1).rawNode.x - line2.node(0).rawNode.x)
        + (line1.node(1).rawNode.y - line1.node(0).rawNode.y)
        * (line2.node(1).rawNode.y - line2.node(0).rawNode.y) < 0)
        != Boolean(reversed)) {
        line2 = line2.reverse()
    }
    const xy1 = [
        0.375 * (line1.node(0).rawNode.x + line2.node(0).rawNode.x)
        + 0.125 * (line1.node(1).rawNode.x + line2.node(1).rawNode.x),
        0.375 * (line1.node(0).rawNode.y + line2.node(0).rawNode.y)
        + 0.125 * (line1.node(1).rawNode.y + line2.node(1).rawNode.y),
    ]
    const xy2 = [
        0.375 * (line1.node(1).rawNode.x + line2.node(1).rawNode.x)
        + 0.125 * (line1.node(0).rawNode.x + line2.node(0).rawNode.x),
        0.375 * (line1.node(1).rawNode.y + line2.node(1).rawNode.y)
        + 0.125 * (line1.node(0).rawNode.y + line2.node(0).rawNode.y),
    ]
    const j = availSuperlineLabels(diagram.rawDiagram).next().value
    return diagram.substitute({
        nodes: [
            terminalNode(line1.toString(), "a"),
            terminalNode(line1.toString(), "b"),
            terminalNode(line2.toString(), "c"),
            terminalNode(line2.toString(), "d"),
        ],
        lines: {
            [line1]: {superline: line1.superlineId, direction: 0},
            [line2]: {superline: line2.superlineId, direction: 0},
        },
    }, {
        nodes: [
            terminalNode("$1", "a"),
            terminalNode("$2", "b"),
            terminalNode("$3", "c"),
            terminalNode("$4", "d"),
            w3jNode("$1", "$3", "$5", ...xy1),
            w3jNode("$2", "$4", "$5", ...xy2),
        ],
        lines: {
            $1: {superline: line1.superlineId, direction: 0},
            $2: {superline: line1.superlineId, direction: 0},
            $3: {superline: line2.superlineId, direction: 0},
            $4: {superline: line2.superlineId, direction: 0},
            $5: {superline: j, direction: 0},
        },
        superlines: {
            [j]: {phase: 0, weight: 2, summed: true},
        },
    }).rawDiagram
}

function w3jElimRule(diagram, lineId) {
    diagram = new Diagram(diagram)
    const line = diagram.line(lineId)
    const superline = line.superline
    if (!superline.summed) {
        return `j[${line.superlineId}] must be summed over`
    }
    if (superline.phase != 0) {
        return `j[${line.superlineId}] must not have any phases`
    }
    if (superline.weight != 2) {
        return `weight of j[${line.superlineId}] must be exactly 2`
    }
    if (!diagram.isEquallyConstrained(line.superlineId, [])) {
        return `j[${line.superlineId}] must not be constrained by deltas`
    }
    if (line.node(0).type != "w3j" || line.node(1).type != "w3j") {
        return "expected Wigner 3-j symbols"
    }
    const line1 = line.cycNodeLine(0, 1).reverse()
    const line2 = line.cycNodeLine(1, 1).reverse()
    const line3 = line.cycNodeLine(0, 2).reverse()
    const line4 = line.cycNodeLine(1, 2).reverse()
    if (line1.superlineId != line2.superlineId
        || line3.superlineId != line4.superlineId) {
        return "opposing j's don't match"
    }
    return diagram.substitute({
        nodes: [
            terminalNode(line1.toString(), "a"),
            terminalNode(line2.toString(), "b"),
            terminalNode(line3.toString(), "c"),
            terminalNode(line4.toString(), "d"),
            w3jNode(line1.toString(), line3.toString(), line.toString()),
            w3jNode(line2.toString(), line4.toString(), line.toString()),
        ],
        lines: {
            [line1]: {superline: line1.superlineId, direction: 0},
            [line2]: {superline: line2.superlineId, direction: 0},
            [line3]: {superline: line3.superlineId, direction: 0},
            [line4]: {superline: line4.superlineId, direction: 0},
            [line]: {superline: line.superlineId, direction: 0},
        },
        superlines: {
            [line.superlineId]: {phase: 0, weight: 2, summed: true},
        },
    }, {
        nodes: [
            terminalNode(line1.id, "a"),
            terminalNode(line1.id, "b"),
            terminalNode(line3.id, "c"),
            terminalNode(line3.id, "d"),
        ],
        lines: {
            [line1.id]: {superline: line1.superlineId, direction: 0},
            [line3.id]: {superline: line3.superlineId, direction: 0},
        },
    }).rawDiagram
}

function glueRule(diagram, lineId1, lineId2) {

}

function cutRule(diagram, lineId) {
    diagram = new Diagram(diagram)
    const line = diagram.line(lineId)
    if (line1.node(0)) {} ///??
    const line1 = line.cycNodeLine(0, 1)
    const line2 = line.cycNodeLine(1, 1)
    const line3 = line.cycNodeLine(0, 2)
    const line4 = line.cycNodeLine(1, 2)
    if (line1.superlineId != line2.superlineId
        || line3.superlineId != line4.superlineId) {
        return "opposing j's don't match"
    }
    return diagram.substitute({
        nodes: [
            terminalNode(line1.toString(), "a"),
            terminalNode(line2.toString(), "b"),
            terminalNode(line3.toString(), "c"),
            terminalNode(line4.toString(), "d"),
            w3jNode(line1.toString(), line3.toString(), line.toString()),
            w3jNode(line2.toString(), line4.toString(), line.toString()),
        ],
        lines: {
            [line1]: {superline: line1.superlineId, direction: 0},
            [line2]: {superline: line2.superlineId, direction: 0},
            [line3]: {superline: line3.superlineId, direction: 0},
            [line4]: {superline: line4.superlineId, direction: 0},
            [line]: {superline: line.superlineId, direction: 0},
        },
        superlines: {
            [line.superlineId]: {phase: 0, weight: 2, summed: true},
        },
    }, {
        nodes: [
            terminalNode(line1.id, "a"),
            terminalNode(line1.id, "b"),
            terminalNode(line3.id, "c"),
            terminalNode(line3.id, "d"),
        ],
        lines: {
            [line1.id]: {superline: line1.superlineId, direction: 0},
            [line3.id]: {superline: line3.superlineId, direction: 0},
        },
    }).rawDiagram
}

function deltaElimRule(diagram, entry) {
    const inferred = inferDeltas(diagram)
    const related = relatedDeltas(diagram.deltas, entry)
    if (!containsDeltas(inferred, related)) {
        return "cannot remove uninferrable delta entry"
    }
    return Object.freeze(Object.assign({}, diagram, {
        deltas: removeDeltaEntry(diagram.deltas, entry)
    }))
}

//////////////////////////////////////////////////////////////////////////////
// Drawing

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
    return [vnode(
        "svg:g",
        {
            "class": "arrow",
            onmousedown: function(e) {
                if (e.buttons == 1) {
                    update(startDrag(rawPosition.x, rawPosition.y, {
                        superficial: true,
                    }, (diagram, x, y, snap) => {
                        const pos = findPosOnLine(info, x, y).pos
                        let lines = Object.assign({}, diagram.lines)
                        // prevent arrows from getting stuck under nodes
                        lines[lineId] = Object.assign({}, lines[lineId], {
                            arrowPos: clamp(0.1, 0.9, round(snap && 0.1, pos)),
                        })
                        return Object.assign({}, diagram, {lines: lines})
                    }))
                    e.stopPropagation()
                } else if (e.buttons == 2 && e.ctrlKey) {
                    update(modifyDiagram({superficial: true}, diagram => {
                        let lines = Object.assign({}, diagram.lines)
                        lines[lineId] = Object.assign({}, lines[lineId], {
                            arrowPos: 0.5,
                        })
                        return Object.assign({}, diagram, {lines: lines})
                    }))
                    e.stopPropagation()
                }
            },
        },
        vnode("svg:circle", {
            "class": "hit",
            r: 15,
            cx: rawPosition.x,
            cy: rawPosition.y,
        }),
        vnode("svg:use", {
            "class": "arrow",
            href: "#arrowhead",
            x: -arrowHeadSize,
            y: -arrowHeadSize / 2,
            width: arrowHeadSize,
            height: arrowHeadSize,
            transform: `translate(${position.x}, ${position.y}),`
                     + `rotate(${angle * 180 / Math.PI})`,
        }),
    )]
}

function renderLine(update, editor, lineId) {
    const diagram = editor.snapshot.diagram
    const line = diagram.lines[lineId]
    const superline = diagram.superlines[line.superline]
    const minTextOffset = 20
    const info = getLineInfo(diagram, lineId)
    const position = positionOnLine(info, line.textPos, 0)
    let textOffset = line.textOffset
    if (textOffset == 0) {
        textOffset = minTextOffset * info.halfDisk
    } else if (textOffset > 0 && textOffset < minTextOffset) {
        textOffset = minTextOffset
    } else if (textOffset < 0 && textOffset > -minTextOffset) {
        textOffset = -minTextOffset
    }
    const radius = info.arc.radius != Infinity ? Math.abs(info.arc.radius) : 0.0
    const d = `M ${info.x0} ${info.y0} `
            + `A ${radius} ${radius} 0 `
            + `${Number(info.arc.large)} ${Number(info.arc.sweep)} `
            + `${info.x1} ${info.y1}`
    const textX = position.x + textOffset * position.normalX
    const textY = position.y + textOffset * position.normalY
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
                        angle: round(snap && Math.PI / 6, angle),
                        arcHeight: clamp(20.0, Infinity,
                                         round(snap && 20.0, height)),
                    }
                } else {
                    change = {
                        angle: info.trueAngle,
                        arcHeight: round(snap && 20.0,
                                         threePointArc(x, y,
                                                       info.x0, info.y0,
                                                       info.x1, info.y1)),
                    }
                }
                return setDiagramLineProps(diagram, lineId, change)
            }))
            e.stopPropagation()
        } else if (e.buttons == 2 && e.ctrlKey == true) {
            update(modifyDiagram({superficial: true}, diagram =>
                info.singular ? diagram : setDiagramLineProps(diagram, lineId, {
                    angle: info.angle,
                    arcHeight: 0.0,
                })
            ))
            e.stopPropagation()
        }
    }
    return vnode(
        "svg:g",
        {
            // prevent hover effects from sticking when nodes change
            [VNODE_KEY]: lineId,
            "class": "line "
                   + (line.superline == "0" ? "zero " : "")
                   + (superline.summed ? "summed " : "")
                   + (superline.phase % 2 ? "one-j " : "")
                   + (mod(superline.phase, 4) >= 2 ? "two-j " : "")
                   + ((editor.trackStart.type == "line" &&
                       editor.trackStart.lineId == lineId)
                   || (editor.trackStop.type == "line" &&
                       editor.trackStop.lineId == lineId)
                    ? (editor.trackType + " ") : ""),
            onmouseenter: function(e) {
                update(setHover({
                    type: "line",
                    lineId: lineId,
                }))
            },
            onmouseleave: function(e) {
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
            "class": "fg",
            d: d,
            onmousedown: onmousedown,
        }),
        vnode("svg:path", {
            "class": "pith",
            d: d,
            onmousedown: onmousedown,
        }),
        vnode("svg:path", {
            "class": "hit",
            d: d,
            onmousedown: onmousedown,
        }),
        vnode("svg:text", {
            "class": "label",
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
                                           round(snap && 0.1, where.pos)),
                            textOffset: round(snap && 10.0, where.offset),
                        })
                    }))
                    e.stopPropagation()
                } else if (e.buttons == 2 && e.ctrlKey == true) {
                    update(modifyDiagram({superficial: true}, diagram =>
                        setDiagramLineProps(diagram, lineId, {
                            textPos: 0.5,
                            textOffset: 0.0,
                        })
                    ))
                    e.stopPropagation()
                }
            },
        }, line.superline), // T??
        ...renderArrow(update, diagram, lineId),
    )
}

function renderNode(update, editor, nodeIndex, frozen) {
    const diagram = editor.snapshot.diagram
    const node = diagram.nodes[nodeIndex]
    let gChildren = [vnode("svg:title", {},
                           node.type == "w3j"
                         ? `Wigner[${node.lines.join(" ")}]`
                         : node.type == "terminal"
                         ? `m[${node.variable}]`
                         : node.type)]

    if (node.type == "w3j") {
        const circularArrowSize = 30
        const orientationInfo = w3jOrientation(diagram, nodeIndex)
        if (orientationInfo.nnz == 3) {
            const orientation = orientationInfo.orientation > 0 ? "flipped " : ""
            gChildren.push(vnode("svg:circle", {
                "class": "bg " + orientation,
                r: 20,
            }))
            gChildren.push(vnode("svg:circle", {
                "class": "fg " + orientation,
                r: 18,
            }))
            gChildren.push(vnode("svg:circle", {
                "class": "hit " + orientation,
                r: 25,
            }))
            gChildren.push(vnode("svg:use", {
                "class": "arrow " + orientation,
                href: "#clockwise",
                x: -circularArrowSize / 2,
                y: -circularArrowSize / 2,
                width: circularArrowSize,
                height: circularArrowSize,
            }))
        } else {
            throw new Error("degenerate Wigner 3-jm nodes are not yet implemented")
        }

    } else if (node.type == "terminal") {
        const frozenClass = frozen ? "frozen " : ""
        gChildren.push(vnode("svg:circle", {
            "class": "fg " + frozenClass,
            r: 8,
        }))
        gChildren.push(vnode("svg:circle", {
            "class": "hit",
            r: 15,
        }))

    } else {
        gChildren.push(vnode("svg:circle", {
            "class": "fg",
            r: 20,
        }))
        gChildren.push(vnode("svg:circle", {
            "class": "hit",
            r: 25,
        }))
        gChildren.push(vnode("svg:text", {
            "class": "label",
        }))
    }

    return vnode("svg:g", {
        // prevent hover effects from sticking when nodes change
        [VNODE_KEY]: objectId(node),
        "class": "node "
               + node.type + " "
               + ((editor.trackStart.type == "node" &&
                   editor.trackStart.nodeIndex == nodeIndex)
               || (editor.trackStop.type == "node" &&
                   editor.trackStop.nodeIndex == nodeIndex)
                ? (editor.trackType + " ") : ""),
        transform: `translate(${node.x}, ${node.y})`,
        onmouseenter: function(e) {
            update(setHover({
                type: "node",
                nodeIndex: nodeIndex,
            }))
        },
        onmouseleave: function(e) {
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
                        x: round(snap && 20.0, x),
                        y: round(snap && 20.0, y),
                    })
                ))
                e.stopPropagation()
            } else if (e.buttons == 2) {
                if (node.type == "terminal") {
                    return
                }
                if (e.shiftKey) { // do it twice!
                    update(modifyDiagram({equivalent: true}, diagram =>
                        flipW3jRule(
                            flipW3jRule(diagram, nodeIndex),
                            nodeIndex)))
                } else {
                    update(modifyDiagram({equivalent: true}, diagram =>
                        flipW3jRule(diagram, nodeIndex)))
                }
                e.stopPropagation()
            } else if (e.buttons == 4) {
                if (node.type == "terminal") {
                    return
                }
                update(modifyDiagram({equivalent: true}, diagram =>
                    threeArrowRule(diagram, nodeIndex)))
                e.stopPropagation()
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
            vnode("td", {"class": "name"},
                  superlineId == "0"
                ? vnode("span", {"class": "zero"}, "0")
                : superlineId),
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
            e.stopPropagation()
        },
    }, "{" + triangle.join(" ") + "}"))
}

function renderDeltaTableau(update, deltas) {
    return deltas.map(delta =>
        vnode("li", {},
              ...arrayIntercalate(
                  " = ",
                  Array.from(delta)
                       .map(x => x == "0"
                             ? vnode("span", {"class": "zero"}, "0")
                             : String(x)))))
}

function renderVariable(type, name) {
    if (name == "0") {
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
         ? `-${mNaked}`
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
                    mDeltas.push(`\\delta_{${m1}, ${m2}}`)
                    break
                case -1:
                    mDeltas.push(`\\delta_{${m1}, -${m2}}`)
                    break
                case 1:
                    mDeltas.push(`\\delta_{-${m1}, ${m2}}`)
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
    let summedVarsStr =
        Object.keys(summedVars.js)
              .concat(Object.keys(summedVars.ms)).join(", ")
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
        "\\begin{Bmatrix}"
      + js.map(j => renderVariable("j", j)).join(" & ")
      + "\\end{Bmatrix}"
    ).join(" ")
    container.textContent = `\\[${summedVarsStr} ${jDeltas} ${mDeltasStr} `
                          + `${weights} ${phasesStr} ${s}\\]`
    MathJax.Hub.Queue(["Typeset", MathJax.Hub])
}

function renderTrack(trackType, start, stop) {
    return [
        vnode("svg:circle", {
            "class": trackType,
            cx: start[0],
            cy: start[1],
            r: 8,
        }),
        vnode("svg:path", {
            "class": trackType,
            d: `M ${start[0]} ${start[1]} `
             + `L ${stop[0]} ${stop[1]} `,
        }),
        vnode("svg:circle", {
            "class": trackType,
            cx: stop[0],
            cy: stop[1],
            r: 8,
        }),
    ]
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

function newEditor() {
    return {
        snapshot: EMPTY_SNAPSHOT,
        savedSnapshot: EMPTY_SNAPSHOT,
        savedHash: "",
        staleEquation: true,

        // controls
        hover: {type: null},
        mouseX: null,
        mouseY: null,
        trackType: null,
        trackStart: {type: null, xy: null},
        trackStop: {type: null, xy: null},
    }
}

function saveEditor(editor) {
    editor.savedSnapshot = editor.snapshot
    editor.savedHash = "#" + encodeURIComponent(JSON.stringify(editor.snapshot))
    window.location.hash = editor.savedHash
}

function loadEditor(editor) {
    const hash = window.location.hash
    // prevent hashchange listener from observing our own changes
    if (hash.length < 3) {
        Object.assign(editor, newEditor())
    } else if (editor.savedHash != hash) {
        editor.snapshot = JSON.parse(decodeURIComponent(hash.substr(1)))
        editor.staleEquation = true
        editor.savedSnapshot = editor.snapshot
        editor.savedHash = hash
    }
}

function toSvgCoords(p) {
    if (!p) {
        return p
    }
    const rect = document.getElementById("diagram").getBoundingClientRect()
    return [p[0] - rect.left, p[1] - rect.top]
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
                onmousedown: e => {
                    if (e.buttons == 2) {
                        update(startTrack(e, "track1"))
                    } else if (e.buttons == 4) {
                        update(startTrack(e, "track2"))
                    }
                },
            },
        },
        {
            element: document.getElementById("diagram-lines"),
            children: Object.keys(diagram.lines).map(lineId =>
                renderLine(update, editor, lineId)),
        },
        {
            element: document.getElementById("diagram-nodes"),
            children: diagram.nodes.map((_, nodeIndex) =>
                renderNode(update, editor, nodeIndex, editor.snapshot.frozen)),
        },
        {
            element: document.getElementById("diagram-track"),
            children: editor.trackStop.xy != null
                    ? renderTrack(editor.trackType,
                                  toSvgCoords(editor.trackStart.xy),
                                  toSvgCoords(editor.trackStop.xy))
                    : [],
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
//   clearHover
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
        if (typeof diagram == "string") { // error?
            error(diagram)
            return
        }
        editor.snapshot = Object.assign({}, editor.snapshot, {
            diagram: diagram,
            frozen: Boolean(flags.toggleFreeze) != editor.snapshot.frozen,
        })
        if (!flags.transient) {
            saveEditor(editor)
        }
        if (flags.clearHover) {
            setHover({type: null})(editor)
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

function startTrack(event, trackType) {
    return editor => {
        editor.trackType = trackType
        editor.trackStart.xy = [event.clientX, event.clientY]
        if (editor.hover.type == "line") {
            editor.trackStart.type = "line"
            editor.trackStart.lineId = editor.hover.lineId
        }
    }
}

function updateTrack(event) {
    return editor => {
        if (editor.trackStart.xy == null) {
            return
        }
        editor.trackStop.xy = [event.clientX, event.clientY]
        if (editor.trackStart.type != null) {
            if (editor.hover.type == "line") {
                editor.trackStop.type = "line"
                editor.trackStop.lineId = editor.hover.lineId
            } else if (editor.hover.type == "node") {
                editor.trackStop.type = "node"
                editor.trackStop.nodeIndex = editor.hover.nodeIndex
            } else {
                editor.trackStop.type = null
            }
        }
    }
}

function finishTrack(editor, event) {
    if (editor.trackStop.xy == null ||
        Math.pow(editor.trackStart.xy[0] - editor.trackStop.xy[0], 2) +
        Math.pow(editor.trackStart.xy[1] - editor.trackStop.xy[1], 2) < 10) {
        if (editor.trackStart.type == "line") {
            modifyDiagram({equivalent: true}, diagram =>
                flipW1jRule(diagram, editor.trackStart.lineId))(editor)
            return
        }
    } else {
        const stopXy = toSvgCoords(editor.trackStop.xy)
        const startXy = toSvgCoords(editor.trackStart.xy)
        if (editor.trackType == "track1") {
            if (editor.trackStop.type == "line") {
                if (editor.trackStart.lineId == editor.trackStop.lineId) {
                    modifyDiagram({equivalent: true, clearHover: true}, diagram =>
                        w3jElimRule(diagram, editor.trackStart.lineId))(editor)
                } else {
                    modifyDiagram({equivalent: true, clearHover: true}, diagram =>
                        glueRule(diagram, editor.trackStart.lineId,
                                 editor.trackStop.lineId))(editor)
                }
            } else if (editor.trackStop.type == "node") {
                modifyDiagram({equivalent: true, clearHover: true}, diagram =>
                    loopElimRule(diagram,
                                 editor.trackStart.lineId,
                                 editor.trackStop.nodeIndex))(editor)
            }
        } else if (editor.trackType == "track2") {
            if (editor.trackStop.type == "line") {
                if (editor.trackStart.lineId == editor.trackStop.lineId) {
                    modifyDiagram({equivalent: true, clearHover: true}, diagram =>
                        cutRule(diagram, editor.trackStart.lineId))(editor)
                } else {
                    modifyDiagram({equivalent: true, clearHover: true}, diagram =>
                        w3jIntroRule(diagram,
                                     editor.trackStart.lineId,
                                     editor.trackStop.lineId,
                                     event.shiftKey))(editor)
                }
            } else if (editor.trackStart.type == "line") {
                modifyDiagram({equivalent: true, clearHover: true}, diagram =>
                    loopIntroRule(diagram,
                                  editor.trackStart.lineId,
                                  startXy,
                                  stopXy))(editor)
            }
        }
    }
}

function clearTrack(editor, event) {
    finishTrack(editor, event)
    editor.trackType = null
    editor.trackStart = {type: null, xy: null}
    editor.trackStop = {type: null, xy: null}
}

function mouseUp(event) {
    return editor => {
        if (editor.dragger) {
            modifyDiagram(editor.draggerFlags, identity)(editor)
            editor.dragger = null
        } else {
            clearTrack(editor, event)
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
        } else {
            updateTrack(event)(editor)
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
            const labels = Array.from(take(3, availSuperlineLabels(diagram)))
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
            const labels = Array.from(take(3, availSuperlineLabels(diagram)))
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
                return "no nearby terminals found"
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

function main() {
    let editor = {}
    Object.assign(editor, newEditor())
    function update(...changes) {
        const n = changes.length
        for (let i = 0; i < n; ++i) {
            changes[i](editor)
        }
        applyRendering(renderEditor(update, editor))
    }
    update(loadEditor)
}
