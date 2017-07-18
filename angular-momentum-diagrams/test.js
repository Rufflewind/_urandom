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

/*

assertEq(new Diagram(d1).renameLines({a: "b"}),
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
         })))

assertEq(new Diagram().substitute(EMPTY_DIAGRAM, EMPTY_DIAGRAM),
         new Diagram())

assertEq(new Diagram(d1).substitute(EMPTY_DIAGRAM, EMPTY_DIAGRAM),
         new Diagram(d1))

assertEq(new Diagram(d1).substitute(ensureDiagram({
    nodes: [
        terminalNode("+a", "a"),
        terminalNode("+a", "b"),
    ],
    lines: {
        ["+a"]: {superline: "a", direction: 0},
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
}), ensureDiagram({
    nodes: [
        terminalNode("a", "a"),
        terminalNode("a", "b"),
    ],
    lines: {
        a: {superline: "a", direction: 0},
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
})), new Diagram(d1))

*/

console.log(new Diagram(d1).substitute(ensureDiagram({
    nodes: [
        terminalNode("+a", "a"),
        terminalNode("+a", "b"),
    ],
    lines: {
        ["+a"]: {superline: "a", direction: 0},
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
}), ensureDiagram({
    nodes: [
        terminalNode("a", "a"),
        terminalNode("a", "c"),
        terminalNode("b", "d"),
        terminalNode("b", "b"),
    ],
    lines: {
        a: {superline: "a", direction: 0},
        b: {superline: "a", direction: 1},
    },
    superlines: {
        a: EMPTY_SUPERLINE,
    },
})))

// KNOWN BUGS:
//   - trimming is buggy (arrow on a,b is wrong)
//   - trimming  O---O  loop causes problems

document.getElementsByTagName("body")[0].style.background = "black"
