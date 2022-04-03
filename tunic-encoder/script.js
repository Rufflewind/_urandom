const CONSONANTS = `
w w 13
m m 46
n n 146
p p 357
b b 247
f f 3567
v v 1247

t t 1357
d d 2467
θ th 12357
ð dh 24567
s s 23567
z z 12457
ʃ sh 134567
ʒ zh 123467
ŋ ng 1234567
tʃ tsh 157
dʒ dzh 267

l l 257
r r 2357
j j 1257
k k 2347
g g 3457
h h 2457
`;

const VOWELS = `
eɪ ei 2
aɪ ai 1
ɔɪ oi 4
aʊ au 5
ʌ uh 12
ɪ ih 45
ɔ o 23
ʊ oo 34
æ a 123
ɛ e 345
ɛər ehr 35
u u 1234
i i 2345
ɪər ier 235
oʊ ou 12345
ɔr or 1235
ər er 1345
ɑr ar 1245
`;

const COMPONENT_BY_SYMBOL = {};
const COMPONENT_BY_NAME = {};

function namespacedElement(namespaceURI, name, attributes, children) {
    const element = document.createElementNS(namespaceURI, name);
    for (const [attrName, attrValue] of Object.entries(attributes)) {
        element.setAttribute(attrName, attrValue);
    }
    element.append(...children);
    return element;
}

function htmlElement(name, attributes, children) {
    return namespacedElement(
        "http://www.w3.org/1999/xhtml",
        name,
        attributes,
        children,
    );
}

function svgElement(name, attributes, children) {
    return namespacedElement(
        "http://www.w3.org/2000/svg",
        name,
        attributes,
        children,
    );
}

function arrayRange(n) {
    return new Array(n).fill();
}

const PATHS = [
    // baseline (0)
    "M -5  0 L +5  0",

    // consonant segments (1-7)
    "M  0 -3 L -5 -6",
    "M  0 -3 L  0 -9",
    "M  0 -3 L +5 -6",
    "M  0 +3 L +5 +6",
    "M  0 +3 L  0 +9",
    "M  0 +3 L -5 +6",
    "M  0 -3 L  0  0",

    // vowel segments (8-12)
    "M +5 -6 L  0 -9",
    "M  0 -9 L -5 -6",
    "M -5 -6 L -5  0  M -5 +3 L -5 +6",
    "M -5 +6 L  0 +9",
    "M  0 +9 L +5 +6",
];

function renderSymbol(symbol, x, y) {
    const consonantCommands = [];
    const vowelCommands = [];
    for (let i = 1; i < 14; ++i) {
        if (!(symbol & (1 << i))) {
            continue;
        }
        const commands = PATHS[i];
        if (i < 8) {
            consonantCommands.push(commands);
        } else {
            vowelCommands.push(commands);
        }
    }
    return svgElement(
        "g",
        {"transform": `translate(6, 12) translate(${x}, ${y})`},
        [
            svgElement("path", {
                "class": "baseline",
                "d": PATHS[0],
                "stroke": "black",
                "stroke-linecap": "round",
            }, []),
            consonantCommands.length ? svgElement("path", {
                "class": "consonant",
                "d": consonantCommands,
                "stroke": "black",
                "stroke-linecap": "round",
            }, []) : null,
            vowelCommands.length ? svgElement("path", {
                "class": "vowel",
                "d": vowelCommands,
                "stroke": "black",
                "stroke-linecap": "round",
            }, []) : null,
            symbol & (1 << 13) ? svgElement("circle", {
                "class": "inversion",
                "cx": "0",
                "cy": "+10.25",
                "r": "1.25",
                "fill": "none",
                "stroke": "black",
            }, []) : null,
        ].filter(element => element),
    );
}

function matchInput(text, i) {
    for (let j = 5; j > 0; --j) {
        const name = text.slice(i, i + j);
        if (COMPONENT_BY_NAME.hasOwnProperty(name)) {
            return {name, component: COMPONENT_BY_NAME[name]};
        }
    }
    return null;
}

function ipaFromSymbols(symbols) {
    let ipas = [];
    for (const [i, symbol] of symbols.entries()) {
        const consonant = COMPONENT_BY_SYMBOL[symbol & 0x00ff] || {};
        const vowel = COMPONENT_BY_SYMBOL[symbol & 0x1f01] || {};
        const inversion = symbol & 0x2000;
        if (inversion) {
            ipas.push(vowel.ipa, consonant.ipa);
        } else {
            ipas.push(consonant.ipa, vowel.ipa);
            if (i + 1 < symbols.length) {
                const nextConsonant = symbols[i + 1] & 0x00fe;
                const nextInversion = symbols[i + 1] & 0x2000;
                if ((!vowel.ipa && (nextInversion || !nextConsonant))
                    || (!consonant.ipa && nextConsonant && !nextInversion)) {
                    ipas.push("·");
                }
            }
        }
    }
    ipas = ipas.filter(c => c);
    const joinedIpa = ipas.join("");
    const finalIpas = [];
    let index = 0;
    for (let i = 0, j = 0; i < joinedIpa.length;) {
        finalIpas.push(ipas[j]);
        const match = matchInput(joinedIpa, i);
        if (match && match.name.length != ipas[j].length) {
            finalIpas.push("·");
        }
        i += ipas[j].length;
        ++j;
    }
    return finalIpas.join("");
}

function renderWord(symbols) {
    const scale = 0.9;
    const width = 2 + 10 * symbols.length;
    const height = 24;
    return svgElement(
        "svg",
        {
            "class": "tunic",
            "height": height * scale,
            "width": width * scale,
            "viewBox": `0 0 ${width} ${height}`,
        },
        [
            svgElement("title", {}, [ipaFromSymbols(symbols)]),
            ...symbols.map((symbol, index) =>
                renderSymbol(symbol, index * 10, 0)),
        ],
    );
}

function error(...arguments) {
    document.body.insertBefore(htmlElement("p", {}, [JSON.stringify(arguments)]), document.body.firstChild);
}

// Note: This also has the side-effect of populating the COMPONENTS_* arrays.
function initAndRenderChart(data, offset, type) {
    const trs = [];
    const seen = {};
    for (const line of data.split("\n")) {
        const fields = line.trim().split(/\s+/);
        if (fields.length <= 1) {
            continue;
        }
        const [ipa, ascii, positions] = fields;
        let symbol = 1;
        for (const posStr of positions) {
            const pos = +posStr;
            symbol |= 1 << (offset + pos);
        }
        if (seen[positions]) {
            error("duplicate", positions);
        }
        seen[positions] = true;
        if (COMPONENT_BY_NAME[ascii]) {
            error("duplicate", ascii);
        }
        if (COMPONENT_BY_NAME[ipa]) {
            error("duplicate", ipa);
        }
        const component = {type, symbol, ipa};
        COMPONENT_BY_NAME[ascii] = component;
        COMPONENT_BY_NAME[ipa] = component;
        COMPONENT_BY_SYMBOL[symbol] = component;
        trs.push(
            htmlElement("div", {"class": "chart-entry"}, [
                renderWord([symbol]),
                " ",
                htmlElement("span", {"class": "ipa"}, [ipa]),
                ...ascii != ipa ? [
                    " ",
                    htmlElement("span", {"class": "ascii"}, [ascii]),
                ] : [],
            ]),
        );
    }
    return trs;
}

function updateOutput(input) {
    const text = input.value;
    localStorage.setItem("fyl-tunic-input", text);
    const components = [];
    for (let i = 0; i < text.length;) {
        const match = matchInput(text, i);
        if (match) {
            i += match.name.length;
            components.push(match.component);
        } else {
            const c = text[i];
            ++i;
            if (c == "-") {
                continue;
            }
            if (/['·]/.test(c)) {
                components.push({type: "BREAK"});
                continue;
            }
            components.push({type: "LITERAL", text: c});
        }
    }
    components.reverse();
    const chars = [];
    while (components.length) {
        if (components[components.length - 1].type == "LITERAL") {
            chars.push(components[components.length - 1].text);
            components.pop();
            continue;
        }
        if (components[components.length - 1].type == "BREAK") {
            components.pop();
            continue;
        }
        if (components[components.length - 1].type == "VOWEL") {
            let symbol = components[components.length - 1].symbol;
            components.pop();
            if ((components[components.length - 1] || {}).type == "CONSONANT") {
                symbol |= components[components.length - 1].symbol | (1 << 13);
                components.pop();
            }
            chars.push(symbol);
            continue;
        }
        if (components[components.length - 1].type == "CONSONANT") {
            let symbol = components[components.length - 1].symbol;
            components.pop();
            if ((components[components.length - 1] || {}).type == "VOWEL") {
                symbol |= components[components.length - 1].symbol;
                components.pop();
            }
            chars.push(symbol);
            continue;
        }
        throw new Error("unknown component");
    }
    chars.reverse();
    const fragment = document.createDocumentFragment();
    const ipaFragment = document.createDocumentFragment();
    while (chars.length) {
        const word = [chars.pop()];
        while (typeof word[0] == typeof chars[chars.length - 1]) {
            word.push(chars.pop());
        }
        if (typeof word[0] == "string") {
            fragment.append(word.join(""));
            ipaFragment.append(word.join(""));
        } else {
            fragment.append(renderWord(word));
            ipaFragment.append(ipaFromSymbols(word));
        }
    }

    setChildren(document.getElementById("output"), fragment);
    setChildren(document.getElementById("output-ipa"), ipaFragment);
}

function setChildren(element, fragment) {
    while (element.firstChild) {
        element.removeChild(element.lastChild);
    }
    element.appendChild(fragment);
}

function main() {
    const fragment = document.createDocumentFragment();
    fragment.append(
        htmlElement("section", {}, initAndRenderChart(CONSONANTS, 0, "CONSONANT")),
        htmlElement("section", {}, initAndRenderChart(VOWELS, 7, "VOWEL")),
    );
    setChildren(document.getElementById("chart"), fragment);

    const input = document.getElementById("input");
    input.value = localStorage.getItem("fyl-tunic-input") || "";
    input.addEventListener("input", e => updateOutput(e.target));
    updateOutput(input);
}

main();
