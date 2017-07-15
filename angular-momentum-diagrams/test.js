console.assert(JSON.stringify(mergeDeltas([
    ["a", "b"],
    ["c", "d"],
])) == JSON.stringify([
    ["a", "b"],
    ["c", "d"],
]))

console.assert(JSON.stringify(mergeDeltas([
    ["a", "b"],
    ["c", "d"],
    ["b", "c"],
    ["e", "f"],
])) == JSON.stringify([
    ["a", "b", "c", "d"],
    ["e", "f"],
]))

console.assert(containsDeltas([
    ["a", "b", "c", "d"],
    ["e", "f"],
], [["e", "f"]]))

console.assert(containsDeltas([
    ["a", "b", "c", "d"],
    ["e", "f"],
], [["e", "f"]]))

console.assert(JSON.stringify(removeDeltaEntry([
    ["a", "b", "c", "d"],
    ["e", "f"],
], "f")) == JSON.stringify([
    ["a", "b", "c", "d"],
]))

console.assert(JSON.stringify(relatedDeltas([
    ["a", "b", "c", "d"],
    ["e", "f"],
], "b")) == JSON.stringify([
    ["b", "a"],
    ["b", "c"],
    ["b", "d"],
]))
