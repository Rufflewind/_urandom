console.assert(permutSign([2, 1, 3], [2, 1, 3]) == 1)

console.assert(JSON.stringify(mergeDeltas([
    [1, 2],
    [3, 4],
])) == JSON.stringify([
    [1, 2],
    [3, 4],
]))

console.assert(JSON.stringify(mergeDeltas([
    [1, 2],
    [3, 4],
    [2, 3],
    [5, 6],
])) == JSON.stringify([
    [1, 2, 3, 4],
    [5, 6],
]))

console.assert(containsDeltas([
    [1, 2, 3, 4],
    [5, 6],
], [[5, 6]]))

console.assert(containsDeltas([
    [1, 2, 3, 4],
    [5, 6],
], [[5, 6]]))

console.assert(JSON.stringify(removeDeltaEntry([
    [1, 2, 3, 4],
    [5, 6],
], 6)) == JSON.stringify([
    [1, 2, 3, 4],
]))

console.assert(JSON.stringify(relatedDeltas([
    [1, 2, 3, 4],
    [5, 6],
], 2)) == JSON.stringify([
    [2, 1],
    [2, 3],
    [2, 4],
]))

//////////////////////////////////////////////////////////////////////////////
// Diagram.substitute

const d1 = ensureDiagram({
    nodes: [
        terminalNode("a", "a", 0, 0),
        terminalNode("a", "b", 0, 0),
    ],
    lines: {
        a: newLine("a"),
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
})

assertEq(new Diagram(d1).renameLines({a: "b"}).rawDiagram,
         new Diagram(ensureDiagram({
             nodes: [
                 terminalNode("b", "a", 0, 0),
                 terminalNode("b", "b", 0, 0),
             ],
             lines: {
                 b: newLine("a"),
             },
             superlines: {
                 a: EMPTY_SUPERLINE,
             },
         })).rawDiagram)

assertEq(new Diagram().substitute(EMPTY_DIAGRAM, EMPTY_DIAGRAM).rawDiagram,
         EMPTY_DIAGRAM)

assertEq(new Diagram(d1).substitute(EMPTY_DIAGRAM, EMPTY_DIAGRAM).rawDiagram,
         d1)

assertEq(new Diagram(d1).substitute({
    nodes: [
        terminalNode("+a", "x"),
        terminalNode("+a", "y"),
    ],
    lines: {
        ["+a"]: {superline: "a", direction: 0},
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
}, {
    nodes: [
        terminalNode("a", "x"),
        terminalNode("a", "y"),
    ],
    lines: {
        a: {superline: "a", direction: 0},
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
}).rawDiagram, d1)

assertEq(new Diagram(d1).substitute({
    nodes: [
        terminalNode("+a", "x"),
        terminalNode("+a", "y"),
    ],
    lines: {
        "+a": {superline: "a", direction: 0},
    },
}, {
    nodes: [
        terminalNode("$1", "x"),
        terminalNode("$2", "y"),
        w3jNode("$4", "$4", "$3"),
        w3jNode("$3", "$2", "$1"),
    ],
    lines: {
        $4: {superline: "0", direction: +1},
        $3: {superline: "0", direction: 0},
        $2: {superline: "a", direction: 0},
        $1: {superline: "a", direction: +1},
    },
    superlines: {
        a: {weight: 1},
    },
}).rawDiagram, {"nodes":[{"type":"terminal","lines":["4"],"variable":"a","x":0,"y":0},{"type":"terminal","lines":["3"],"variable":"b","x":0,"y":0},{"type":"w3j","lines":["1","1","2"]},{"type":"w3j","lines":["2","3","4"]}],"superlines":{"0":{"phase":0,"summed":false,"weight":0},"a":{"phase":0,"summed":false,"weight":1}},"lines":{"1":{"superline":"0","direction":1,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0},"2":{"superline":"0","direction":0,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0},"3":{"superline":"a","direction":0,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0},"4":{"superline":"a","direction":1,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0}},"deltas":[],"triangles":[]})

document.getElementsByTagName("body")[0].style.background = "black"
