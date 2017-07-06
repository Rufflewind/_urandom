"use strict";

// TODO: consider switching to native JS

function deepClone(x) {
    return JSON.parse(JSON.stringify(x));
}

function childNodesOf() {
    return this.childNodes;
}

function endNodeIndices(nodes, lineId) {
    var nodeIndices = [];
    if (lineId === undefined || lineId === null) {
        throw "lineId must not be null";
    }
    nodes.forEach(function (node, nodeIndex) {
        if (node.lines.includes(lineId)) {
            nodeIndices.push(nodeIndex);
        }
    });
    if (nodeIndices.length != 2) {
        throw "line is not connected to exactly 2 nodes";
    }
    return nodeIndices;
}

function otherNodeIndex(nodes, selfIndex, lineIndex) {
    var lineId = nodes[selfIndex].lines[lineIndex];
    if (lineId === undefined || lineId === null) {
        throw `cannot find line with lineIndex = ${lineIndex}`;
    }
    var nodeIndices = endNodeIndices(nodes, lineId);
    if (nodeIndices[0] == selfIndex) {
        return nodeIndices[1];
    } else {
        return nodeIndices[0];
    }
}

function angleToOtherNode(nodes, selfIndex, lineIndex) {
    var self = nodes[selfIndex];
    var other = nodes[otherNodeIndex(nodes, selfIndex, lineIndex)];
    return Math.atan2(other.y - self.y, other.x - self.x);
}

function w3jOrientation(nodes, index) {
    var node = nodes[index];
    if (node.type != "w3j") {
        throw "cannot get orientation of generic node";
    }
    var lines = [0, 1, 2].map(function(lineIndex) {
        return [lineIndex, angleToOtherNode(nodes, index, lineIndex)];
    });
    lines.sort(function (line1, line2) {
        return line1[1] - line2[1];
    });
    var lines = lines.map(function(line) {
        return (line[0] - lines[0][0] + 3) % 3;
    });
    if (lines.join() === [0, 1, 2].join()) {
        return 1;
    } else {
        return -1;
    }
}

function drawDiagramNodes(diagram, container, hist) {
    var nodes = diagram.nodes;
    var dragStarted = false; // avoid unnecessary saves
    var drag = d3.drag()
                 .on("drag", function(i) {
                     if (controls.modifers == 0) {
                         diagram = current(hist);
                         var node = diagram.nodes[i];
                         node.x = d3.event.x;
                         node.y = d3.event.y;
                         updateDiagram(diagram);
                         dragStarted = true;
                     }
                 })
                 .on("end", function(i) {
                     if (dragStarted) {
                         saveDiagram(hist, diagram);
                         dragStarted = false;
                     }
                 });
    var data = nodes.map(function(node, index) {
        return Object.assign({
            index: index,
            orientation: node.type == "w3j" ?
                         w3jOrientation(nodes, index) : 0
        }, node);
    });
    // HACK: indices aren't stable!
    container.selectAll(childNodesOf).filter("g").remove();
    var selection = container.selectAll(childNodesOf)
                             .filter("g")
                             .data(Array.from(data.keys()));
    selection.exit().remove();
    var g = selection.enter()
                     .append("g")
                     .call(drag)
                     .on("click", function(i) {
                         var diagram = flipW3jRule(current(hist), i);
                         saveDiagram(hist, diagram);
                         updateDiagram(diagram);
                     });
    g.append("circle")
     .attr("r", 16);
    g.filter(i => !["w3j", "terminal"].includes(data[i].type))
     .append("text")
     .attr("class", "node-label")
     .attr("alignment-baseline", "middle")
     .attr("text-anchor", "middle")
     .attr("font-size", "large")
    var circularArrowSize = 28;
    g.filter(i => data[i].type == "w3j")
     .append("use")
     .attr("href", "#clockwise")
     .attr("x", -circularArrowSize / 2)
     .attr("y", -circularArrowSize / 2)
     .attr("width", circularArrowSize)
     .attr("height", circularArrowSize)
     .attr("fill", "white");
    var merged = selection
        .merge(g)
        .attr("transform", i => `translate(${data[i].x}, ${data[i].y})`);
    merged.select("circle")
          .style("fill", function(i) {
              var node = data[i];
              if (node.type == "terminal") {
                  return "transparent";
              } else if (node.type == "w3j") {
                  if (node.orientation > 0) {
                      return "#d98b7c";
                  } else {
                      return "#2eb0ba";
                  }
              } else {
                  return "#ddd";
              }
          });
    merged.select("text")
          .attr("fill", function(i) {
              var node = data[i];
              if (node.type == "w3j") {
                  return "#fff";
              } else {
                  return "#000";
              }
          })
          .text(i => data[i].type);
    merged.select("use")
          .attr("transform", i => data[i].orientation > 0 ? "scale(-1,1)" : "");
}

function drawArrows(container, linesData, diagram) {
    function data(i) {
        var line = linesData[i];
        return line.arrows.map(function(arrow) {
            var nodeIndices = endNodeIndices(diagram.nodes, line.id);
            var x0 = diagram.nodes[nodeIndices[0]].x;
            var y0 = diagram.nodes[nodeIndices[0]].y;
            var x1 = diagram.nodes[nodeIndices[1]].x;
            var y1 = diagram.nodes[nodeIndices[1]].y;
            var x = x0 + (x1 - x0) * arrow.t;
            var y = y0 + (y1 - y0) * arrow.t;
            var angle = Math.atan2(y1 - y0, x1 - x0) * 180 / Math.PI;
            if (arrow.direction < 0) {
                angle += 180;
            }
            return {
                transform: `translate(${x}, ${y}),rotate(${angle})`
            };
        });
    };
    var selection = container.selectAll(childNodesOf)
                             .filter("use")
                             .data(data);
    selection.exit().remove();
    var use = selection.enter()
                       .append("use")
                       .attr("href", "#arrowhead")
                       .attr("x", -20)
                       .attr("y", -10)
                       .attr("width", 20)
                       .attr("height", 20);
    selection.merge(use)
             .attr("transform", d => d.transform);
}

function drawDiagramLines(diagram, container) {
    // HACK: indexes are used here because d3 captures the
    //       objects directly and doesn't change them :/
    var data = Object.keys(diagram.lines).map(function(id) {
        var line = diagram.lines[id];
        var nodeIndices = endNodeIndices(diagram.nodes, id);
        var x0 = diagram.nodes[nodeIndices[0]].x;
        var y0 = diagram.nodes[nodeIndices[0]].y;
        var x1 = diagram.nodes[nodeIndices[1]].x;
        var y1 = diagram.nodes[nodeIndices[1]].y;
        var textOffsetAngle = Math.atan2(y1 - y0, x1 - x0) + Math.PI / 2;
        var textOffset = 10.0;
        var d = `M ${x0} ${y0} ` +
                `L ${x1} ${y1}`;
        return Object.assign({
            id: id,
            x0: x0,
            y0: y0,
            x1: x1,
            y1: y1,
            textX: (x0 + x1) / 2 + textOffset * Math.cos(textOffsetAngle),
            textY: (y0 + y1) / 2 + textOffset * Math.sin(textOffsetAngle),
            d: d
        }, line);
    });
    var selection = container.selectAll("g.line")
                             .data(Array.from(data.keys()));
    selection.exit().remove();
    var g = selection.enter()
                     .append("g")
                     .attr("class", "line")
    var path = g.append("path")
                .attr("fill", "transparent")
                .attr("stroke", "black")
                .attr("stroke-width", "2");
    g.append("text")
     .attr("class", "line-label")
     .attr("alignment-baseline", "middle")
     .attr("text-anchor", "middle")
     .attr("font-size", "large");
    var arrowG = g.append("g");
    var merged = selection.merge(g).selectAll(childNodesOf);
    merged.filter("g").call(drawArrows, data, diagram);
    merged.filter("path")
          .attr("d", i => data[i].d);
    merged.filter("text")
          .attr("x", i => data[i].textX)
          .attr("y", i => data[i].textY)
          .text(i => data[i].superline);
}

function drawDiagram(diagram) {
    drawDiagramNodes(diagram, d3.select("#diagram-nodes"), hist);
    drawDiagramLines(diagram, d3.select("#diagram-lines"));
}

function updateDiagram(diagram) {
    drawDiagram(diagram);
    var s = "";
    var tableau = document.getElementById("tableau");
    tableau.getElementsByClassName("main")[0].remove();
    var main = document.createElement("tbody");
    main.className = "main";
    Object.keys(diagram.superlines).forEach(function(superlineId) {
        var superline = diagram.superlines[superlineId];
        var tr = document.createElement("tr");
        // name
        var td = document.createElement("td");
        td.className = "name";
        td.textContent = superlineId;
        tr.appendChild(td);
        // phase
        td = document.createElement("td");
        td.className = "phase";
        switch (superline.phase % 4) {
            case 0:
                td.textContent = "🌕";
                break;
            case 1:
                td.textContent = "🌗";
                break;
            case 2:
                td.textContent = "🌑";
                break;
            case 3:
                td.textContent = "🌓";
                break;
            default:
                throw "invalid phase: " + superline.phase;
        }
        tr.appendChild(td);
        // weight
        td = document.createElement("td");
        td.className = "weight";
        if (superline.weight) {
            td.textContent = `${superline.weight > 0 ? "+" : ""}${superline.weight}`;
        }
        tr.appendChild(td);
        main.appendChild(tr);
    });
    tableau.appendChild(main);
}

function renderNodeLine(diagram, nodeIndex, lineIndex, summedVars) {
    var lineId = diagram.nodes[nodeIndex].lines[lineIndex];
    var mNaked = `m_{${lineId}}`;
    var jm = {
        j: `j_{${diagram.lines[lineId].superline}}`,
        m: mNaked
    };
    var line = diagram.lines[lineId];
    var otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex);
    if (otherIndex < nodeIndex && line.arrows.length % 2 == 1) {
        jm.m = `\\overline{${jm.m}}`;
    }
    var summedJ = line.superline.summed;
    if (summedJ) {
        summedVars.js.push(jm.j);
    }
    var summedM = diagram.nodes[otherIndex].type == "terminal";
    if (summedM) {
        summedVars.ms.push(mNaked);
    }
    return jm;
}

function renderArrows(diagram, lineId, phases) {
    var line = diagram.lines[lineId];
    var sign = -1;
    line.arrows.forEach(function(arrow) {
        phases.push(`+ j_{${diagram.lines[lineId].superline}}`);
        phases.push(`${sign * arrow.direction > 0 ? "+" : "-"} m_{${lineId}}`);
        sign = -sign;
    });
}

function renderEquation(diagram, container) {
    var s = "";
    var summedVars = {js: [], ms: []};
    var phases = [];
    diagram.nodes.forEach(function(node, nodeIndex) {
        if (node.type == "terminal") {
        } else if (node.type == "w3j") {
            s += "\\begin{pmatrix}";
            var jRow = "";
            var mRow = "";
            node.lines.forEach(function(lineId, lineIndex) {
                if (lineIndex > 0) {
                    jRow += " & ";
                    mRow += " & ";
                }
                var jm = renderNodeLine(diagram, nodeIndex,
                                        lineIndex, summedVars);
                jRow += jm.j;
                mRow += jm.m;
            });
            s += jRow + " \\\\";
            s += mRow + " \\\\";
            s += "\\end{pmatrix}";
        } else {
            s += "\\mathtt{${node.type}}_{";
            node.lines.forEach(function(lineId, lineIndex) {
                if (lineIndex > 0) {
                    s += " ";
                }
                var jm = renderNodeLine(diagram, nodeIndex,
                                        lineId, summedVars);
                s += jm[0] + " " + jm[1];
            });
            s += "}";
        }
    });
    Object.keys(diagram.lines).forEach(function(lineId) {
        renderArrows(diagram, lineId, phases);
    });
    var weights = "";
    Object.keys(diagram.superlines).forEach(function(superlineId) {
        var superline = diagram.superlines[superlineId];
        switch (superline.phase % 4) {
            case 0:
                break;
            case 1:
                phases.push(`+ j_{${superlineId}}`);
                break;
            case 2:
                phases.push(`+ 2 j_{${superlineId}}`);
                break;
            case 3:
                phases.push(`- j_{${superlineId}}`);
                break;
        }
        if (superline.weight) {
            weights += ` (2 j_{${superlineId}} + 1)^{${superline.weight} / 2}`;
        }
    });
    summedVars = summedVars.js.join(" ") + " " + summedVars.ms.join(" ");
    phases = phases.join(" ");
    if (phases) {
        if (phases.startsWith("+ ")) {
            phases = phases.substr(2);
        }
        phases = `(-1)^{${phases}}`;
    }
    container.textContent = `\\[\\sum_{${summedVars}} ${weights} ${phases} ${s}\\]`;
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, equation]);
}

// NOTE: must maintain invariant that terminals precede all other nodes.
// Also, the order of nodes is critical!  If you move the nodes around,
// make sure the lines are also reversed.

function newLabel(label) {
    var match = /^([\s\S]*?)(\d*)$/.exec(label);
    return match[1] + (Number(match[2]) + 1).toString();
}

function mergeSuperlines(superline1, superline2) {
    superline1 = Object.assign({}, superline1);
    superline1.phase = (superline1.phase + superline2.phase) % 4;
    superline1.weight += superline2.weight;
    superline1.summed |= superline2.summed;
    return superline1;
}

function mergeDiagrams(diagram1, diagram2) {
    var diagram = deepClone(diagram1);
    Object.keys(diagram2.superlines).forEach(function(superlineId) {
        var self = diagram2.superlines[superlineId];
        if (diagram.superlines.hasOwnProperty(superlineId)) {
            diagram.superlines[superlineId] =
                mergeSuperlines(diagram.superlines[superlineId], self);
        } else {
            diagram.superlines[superlineId] = self;
        }
    });
    var existingLines = Object.assign({}, diagram.lines);
    var renames = {};
    Object.keys(diagram2.lines).forEach(function(lineId) {
        var newLineId = lineId;
        while (existingLines.hasOwnProperty(newLineId)) {
            // name collision
            newLineId = newLabel(newLineId);
        }
        renames[lineId] = newLineId;
        var line = diagram2.lines[lineId];
        diagram.lines[newLineId] = line;
    });
    var terminals = [];
    diagram2.nodes.forEach(function(node) {
        node.lines = node.lines.map(lineId => renames[lineId]);
        if (node.type == "terminal") {
            terminals.push(node);
        } else {
            diagram.nodes.push(node);
        }
    });
    diagram.nodes = terminals.concat(diagram.nodes);
    return diagram;
}

function findNearestNodeIndices(diagram, count, x, y) {
    var nodeIndices = diagram.nodes.map((node, nodeIndex) => ({
        distance: Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2),
        index: nodeIndex
    }));
    nodeIndices.sort((x, y) => x.distance - y.distance);
    return nodeIndices.slice(0, count).map(node => node.index);
}

function joinTerminals(diagram, terminalIndex1, terminalIndex2) {
    if (diagram.nodes[terminalIndex1].type != "terminal" ||
        diagram.nodes[terminalIndex2].type != "terminal") {
        throw "cannot join non-terminals";
    }
    var other1 = otherNodeIndex(diagram.nodes, terminalIndex1, 0);
    var other2 = otherNodeIndex(diagram.nodes, terminalIndex2, 0);
    if (other1 > other2) {
        // we only handle cases where LEFT < RIGHT
        return joinTerminals(diagram, terminalIndex2, terminalIndex1);
    }
    var lineId1 = diagram.nodes[terminalIndex1].lines[0];
    var lineId2 = diagram.nodes[terminalIndex2].lines[0];
    if (lineId1 == lineId2) {
        // FIXME loops are not yet implemented
        throw "cannot join terminals sharing the same line yet";
    }
    var superlineId1 = diagram.lines[lineId1].superline;
    var superlineId2 = diagram.lines[lineId2].superline;
    diagram = deepClone(diagram);

    // join with other node
    diagram.nodes[other2].lines[diagram.nodes[other2].lines.indexOf(lineId2)] =
        lineId1;

    // merge the lines (be careful with orientation)
    diagram.lines[lineId1].arrows = diagram.lines[lineId1].arrows.map(arrow => ({
        t: arrow.t * 0.5,
        direction: -arrow.direction
    })).concat(diagram.lines[lineId2].arrows.map(arrow => ({
        t: 0.5 + arrow.t * 0.5,
        direction: arrow.direction
    })));
    delete diagram.lines[lineId2];

    // equate superlines and merge their factors
    diagram.superlines[superlineId1] =
        mergeSuperlines(diagram.superlines[superlineId1],
                        diagram.superlines[superlineId2]);
    delete diagram.superlines[superlineId2];

    // delete the terminal nodes
    var terminals = [terminalIndex1, terminalIndex2];
    terminals.sort((x, y) => y - x);
    terminals.forEach(terminalIndex => {
        if (diagram.nodes[terminalIndex].type != "terminal") {
            throw "BUG: I am deleting the wrong node?";
        }
        diagram.nodes.splice(terminalIndex, 1);
    });
    return diagram;
}

function flipW3jRule(diagram, nodeIndex) {
    if (diagram.nodes[nodeIndex].type != "w3j") {
        return diagram;
    }
    diagram = deepClone(diagram);
    diagram.nodes[nodeIndex].lines.reverse();
    Object.keys(diagram.superlines).forEach(function(superlineId) {
        diagram.nodes[nodeIndex].lines.forEach(function(lineId) {
            if (diagram.lines[lineId].superline == superlineId) {
                diagram.superlines[superlineId] =
                    mergeSuperlines(diagram.superlines[superlineId], {
                        phase: 1,
                        summed: false,
                        weight: 0,
                    });
            }
        });
    });
    return diagram;
}

function w3jDiagram(a, b, c, x, y) {
    return {
        nodes: [
            {
                type: "terminal",
                lines: [a],
                x: x - 50,
                y: y + 50
            },
            {
                type: "terminal",
                lines: [b],
                x: x + 50,
                y: y + 50
            },
            {
                type: "terminal",
                lines: [c],
                x: x,
                y: y - 70
            },
            {
                type: "w3j",
                lines: [a, b, c],
                x: x,
                y: y
            },
        ],
        lines: {
            [a]: {
                superline: a,
                arrows: []
            },
            [b]: {
                superline: b,
                arrows: []
            },
            [c]: {
                superline: c,
                arrows: []
            }
        },
        superlines: {
            [a]: {
                phase: 0,
                summed: false,
                weight: 0,
            },
            [b]: {
                phase: 0,
                summed: false,
                weight: 0,
            },
            [c]: {
                phase: 0,
                summed: false,
                weight: 0,
            }
        }
    };
}

function cgDiagram(a, b, c, x, y) {
    return {
        nodes: [
            {
                type: "terminal",
                lines: [a],
                x: x - 50,
                y: y + 50
            },
            {
                type: "terminal",
                lines: [b],
                x: x + 50,
                y: y + 50
            },
            {
                type: "terminal",
                lines: [c],
                x: x,
                y: y - 70
            },
            {
                type: "w3j",
                lines: [a, b, c],
                x: x,
                y: y
            },
        ],
        lines: {
            [a]: {
                superline: a,
                arrows: []
            },
            [b]: {
                superline: b,
                arrows: []
            },
            [c]: {
                superline: c,
                arrows: [
                    {
                        t: 0.5,
                        direction: 1
                    }
                ]
            }
        },
        superlines: {
            [a]: {
                phase: 0,
                summed: false,
                weight: 0,
            },
            [b]: {
                phase: 2,
                summed: false,
                weight: 0,
            },
            [c]: {
                phase: 0,
                summed: false,
                weight: 1,
            }
        }
    };
}

var EMPTY_DIAGRAM = {
    nodes: [],
    superlines: {},
    lines: {}
};

// each time the diagram undergoes a change that is not allowed by the rules
// increase by 1
var version = 0;

var hist = {
    version: 0,
    history: [],
    undoDepth: 0
};

function saveDiagram(hist, diagram, bump) {
    hist.history.splice(hist.history.length - hist.undoDepth, hist.undoDepth);
    hist.history.push({
        version: hist.version,
        diagram: deepClone(diagram)
    });
    hist.undoDepth = 0;
    if (bump) {
        hist.version += 1;
    }
}

function current(hist) {
    var entry = hist.history[hist.history.length - 1 - hist.undoDepth];
    hist.version = entry.version;
    return deepClone(entry.diagram);
}

function undo(hist) {
    if (hist.undoDepth < hist.history.length - 1) {
        hist.undoDepth += 1;
    }
    return current(hist);
}

function redo(hist) {
    if (hist.undoDepth > 0) {
        hist.undoDepth -= 1;
    }
    return current(hist);
}

var controls = {
    modifers: 0,
    mouseX: 0,
    mouseY: 0,
};

var ALT = 0x1;
var CTRL = 0x2;
var SHIFT = 0x3;

function updateKeyState(controls, event) {
    controls.modifiers = event.altKey
                       | (event.ctrlKey << 1)
                       | (event.shiftKey << 2);
}

var counter = 0;

saveDiagram(hist, EMPTY_DIAGRAM);
updateDiagram(EMPTY_DIAGRAM);
window.addEventListener("keydown", function(event) {
    updateKeyState(controls, event);
    if (controls.modifiers == CTRL && event.key == "z") {
        updateDiagram(undo(hist));
    }
    if (controls.modifiers == CTRL && event.key == "y") {
        updateDiagram(redo(hist));
    }
    if (controls.modifiers == 0 && event.key == "c") {
        var diagram = mergeDiagrams(current(hist),
                                    cgDiagram(counter += 1,
                                              counter += 1,
                                              counter += 1,
                                              controls.mouseX,
                                              controls.mouseY));
        saveDiagram(hist, diagram, "bump");
        updateDiagram(diagram);
    }
    if (controls.modifiers == 0 && event.key == "w") {
        var diagram = mergeDiagrams(current(hist),
                                    w3jDiagram(counter += 1,
                                               counter += 1,
                                               counter += 1,
                                               controls.mouseX,
                                               controls.mouseY));
        counter += 1;
        saveDiagram(hist, diagram, "bump");
        updateDiagram(diagram);
    }
    if (controls.modifiers == 0 && event.key == "a") {
        var diagram = current(hist);
        var nearest = findNearestNodeIndices(diagram, 2,
                                             controls.mouseX,
                                             controls.mouseY);
        if (nearest.length == 2 &&
            diagram.nodes[nearest[0]].type == "terminal" &&
            diagram.nodes[nearest[1]].type == "terminal") {
            var diagram = joinTerminals(diagram, nearest[0], nearest[1]);
            saveDiagram(hist, diagram, "bump");
            updateDiagram(diagram);
        }
    }
});

window.addEventListener("keyup", function(event) {
    updateKeyState(controls, event);
});

document.getElementById("diagram").addEventListener(
    "mousemove", function(event) {
        controls.mouseX = event.clientX;
        controls.mouseY = event.clientY;
    }
);
document.getElementById("equation").addEventListener("click", function() {
    renderEquation(current(hist), document.getElementById("equation"));
});
