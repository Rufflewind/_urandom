const SEGMENTS = [
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
    // segment 13 is the inversion circle, defined in renderSymbol
];

const COMPONENTS = [
    // consonants
    {symbol: 0b00000000001010, names: ["w"]},
    {symbol: 0b00000001010000, names: ["m"]},
    {symbol: 0b00000001010010, names: ["n"]},
    {symbol: 0b00000010101000, names: ["p"]},
    {symbol: 0b00000010010100, names: ["b"]},
    {symbol: 0b00000011101000, names: ["f"]},
    {symbol: 0b00000010010110, names: ["v"]},
    {symbol: 0b00000010101010, names: ["t"]},
    {symbol: 0b00000011010100, names: ["d"]},
    {symbol: 0b00000010101110, names: ["θ", "th"]},
    {symbol: 0b00000011110100, names: ["ð", "dh"]},
    {symbol: 0b00000011101100, names: ["s"]},
    {symbol: 0b00000010110110, names: ["z"]},
    {symbol: 0b00000011111010, names: ["ʃ", "sh"]},
    {symbol: 0b00000011011110, names: ["ʒ", "zh"]},
    {symbol: 0b00000011111110, names: ["ŋ", "ng"]},
    {symbol: 0b00000010100010, names: ["tʃ", "tsh"]},
    {symbol: 0b00000011000100, names: ["dʒ", "dzh"]},
    {symbol: 0b00000010100100, names: ["l"]},
    {symbol: 0b00000010101100, names: ["r"]},
    {symbol: 0b00000010100110, names: ["j"]},
    {symbol: 0b00000010011100, names: ["k"]},
    {symbol: 0b00000010111000, names: ["g"]},
    {symbol: 0b00000010110100, names: ["h"]},
    // vowels
    {symbol: 0b00001000000000, names: ["eɪ", "ei"]},
    {symbol: 0b00000100000000, names: ["aɪ", "ai"]},
    {symbol: 0b00100000000000, names: ["ɔɪ", "oi"]},
    {symbol: 0b01000000000000, names: ["aʊ", "au"]},
    {symbol: 0b00001100000000, names: ["ʌ", "uh"]},
    {symbol: 0b01100000000000, names: ["ɪ", "ih"]},
    {symbol: 0b00011000000000, names: ["ɔ", "o"]},
    {symbol: 0b00110000000000, names: ["ʊ", "oo"]},
    {symbol: 0b00011100000000, names: ["æ", "a"]},
    {symbol: 0b01110000000000, names: ["ɛ", "e"]},
    {symbol: 0b01010000000000, names: ["ɛər", "ehr"]},
    {symbol: 0b00111100000000, names: ["u"]},
    {symbol: 0b01111000000000, names: ["i"]},
    {symbol: 0b01011000000000, names: ["ɪər", "ier"]},
    {symbol: 0b01111100000000, names: ["oʊ", "ou"]},
    {symbol: 0b01011100000000, names: ["ɔr", "or"]},
    {symbol: 0b01110100000000, names: ["ər", "er"]},
    {symbol: 0b01101100000000, names: ["ɑr", "ar"]},
];

const MASK_BASELINE = 0x0001;
const MASK_CONSONANT = 0x00fe;
const MASK_VOWEL = 0x1f00;
const MASK_INVERSION = 0x2000;

const DEFAULT_COLORS = {
    "baseline": "black",
    "consonant": "black",
    "vowel": "black",
    "inversion": "black",
};

const CHART_COLORS = {
    "baseline": "#aaa",
    "consonant": "#f1483d",
    "vowel": "#61bb58",
    "inversion": "black",
};

const LOCAL_STORAGE_INPUT_KEY = "fyl-tunic-encoder-input";
const DEFAULT_INPUT = "uh sikriht ledzhend sez dhat uh greit trezher laiz ihn dhis far uh.wei land.";

const COMPONENT_BY_NAME = {};
const COMPONENT_BY_SYMBOL = {};

function init() {
    for (const component of COMPONENTS) {
        insertUnique(COMPONENT_BY_SYMBOL, component.symbol, component);
        for (const name of component.names) {
            insertUnique(COMPONENT_BY_NAME, name, component);
        }
    }
}

function namespacedElement(namespaceURI, name, attributes, children) {
    const element = document.createElementNS(namespaceURI, name);
    for (const [attrName, attrValue] of Object.entries(attributes)) {
        element.setAttribute(attrName, attrValue);
    }
    element.append(...children);
    return element;
}

function htmlElement(name, attributes, children) {
    const NAMESPACE = "http://www.w3.org/1999/xhtml";
    return namespacedElement(NAMESPACE, name, attributes, children);
}

function svgElement(name, attributes, children) {
    const NAMESPACE = "http://www.w3.org/2000/svg";
    return namespacedElement(NAMESPACE, name, attributes, children);
}

function setChildren(element, children) {
    while (element.firstChild) {
        element.removeChild(element.lastChild);
    }
    element.append(...children);
}

function error(...arguments) {
    let error = document.getElementById("error");
    if (error) {
        error.innerText += "\n";
    } else {
        error = document.body.insertBefore(
            htmlElement("section", {"class": "error"}, []),
            document.body.firstChild,
        );
    }
    error.innerText += `Error: ${arguments} (${JSON.stringify(arguments)})`;
}

function insertUnique(map, key, value) {
    if (map.hasOwnProperty[key]) {
        error(`duplicate key: ${key}`);
    }
    map[key] = value;
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
        const consonant = COMPONENT_BY_SYMBOL[symbol & MASK_CONSONANT] || {names: []};
        const vowel = COMPONENT_BY_SYMBOL[symbol & MASK_VOWEL] || {names: []};
        const inversion = symbol & MASK_INVERSION;
        if (inversion) {
            ipas.push(vowel.names[0], consonant.names[0]);
        } else {
            ipas.push(consonant.names[0], vowel.names[0]);
            if (i + 1 < symbols.length) {
                const nextConsonant = symbols[i + 1] & MASK_CONSONANT;
                const nextInversion = symbols[i + 1] & MASK_INVERSION;
                if ((!vowel.names[0] && (nextInversion || !nextConsonant))
                    || (!consonant.names[0] && nextConsonant && !nextInversion)) {
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

function renderSymbol(symbol, x, y, colorByClass) {
    return svgElement(
        "g",
        {"transform": `translate(6, 12) translate(${x}, ${y})`},
        [
            ...[
                ["baseline", MASK_BASELINE],
                ["consonant", MASK_CONSONANT],
                ["vowel", MASK_VOWEL],
            ].map(([componentClass, mask]) =>
                svgElement("path", {
                    "d": SEGMENTS.filter((command, i) =>
                        (MASK_BASELINE | symbol) & mask & (1 << i)
                    ).join(" "),
                    "stroke": colorByClass[componentClass],
                    "stroke-linecap": "round",
                }, []),
            ),
            symbol & MASK_INVERSION ? svgElement("circle", {
                "cx": "0",
                "cy": "+10.25",
                "r": "1.25",
                "fill": "none",
                "stroke": colorByClass["inversion"],
            }, []) : null,
        ].filter(element => element),
    );
}

function renderWord(symbols, colorByClass) {
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
                renderSymbol(symbol, index * 10, 0, colorByClass)
            ),
        ],
    );
}

function renderChart(input, mask) {
    return COMPONENTS.map(component => {
        if (!(component.symbol & mask)) {
            return null;
        }
        const [ipa, ascii] = component.names;
        const entry = htmlElement("div", {"class": "chart-entry"}, [
            renderWord([component.symbol], CHART_COLORS),
            " ",
            htmlElement("span", {"class": "ipa"}, [ipa]),
            ...ascii ? [
                " ",
                htmlElement("span", {"class": "ascii"}, [ascii]),
            ] : [],
        ]);
        entry.addEventListener("click", e => {
            input.value += ipa;
            updateOutput(input);
        });
        return entry;
    }).filter(entry => entry);
}

function normalizeIpa(text) {
    return text.replace("ɹ", "r");
}

function encodeTunic(text) {
    text = normalizeIpa(text);
    const tokens = [];
    for (let i = 0; i < text.length;) {
        const match = matchInput(text, i);
        if (match) {
            i += match.name.length;
            tokens.push(Object.assign({
                type: "COMPONENT",
                component: match.component,
            }));
            continue;
        }
        const c = text[i];
        ++i;
        if (/[ˈː]/.test(c)) {
            continue;
        }
        if (c == ".") {
            tokens.push({type: "PERIOD"});
            continue;
        }
        if (/[·]/.test(c)) {
            tokens.push({type: "BREAK"});
            continue;
        }
        tokens.push({type: "LITERAL", text: c});
    }
    tokens.reverse();
    const chars = [];
    while (tokens.length) {
        const token = tokens[tokens.length - 1];
        switch (token.type) {
        case "LITERAL":
            tokens.pop();
            chars.push(token.text);
            continue;
        case "BREAK":
            tokens.pop();
            continue;
        case "PERIOD":
            tokens.pop();
            const prevChar = chars[chars.length - 1];
            const nextType = (tokens[tokens.length - 1] || {}).type;
            if (!(typeof prevChar == "number" && nextType == "COMPONENT")) {
                chars.push(".");
            }
            continue;
        case "COMPONENT": {
            tokens.pop();
            const nextToken = tokens[tokens.length - 1] || {};
            const nextSymbol = (nextToken.component || {}).symbol;
            let symbol = token.component.symbol;
            if (((symbol & MASK_CONSONANT) && (nextSymbol & MASK_VOWEL))
                || ((symbol & MASK_VOWEL) && (nextSymbol & MASK_CONSONANT))) {
                tokens.pop();
                symbol |= nextSymbol;
                if (nextSymbol & MASK_CONSONANT) {
                    symbol |= MASK_INVERSION;
                }
            }
            chars.push(symbol);
            continue;
        }
        default:
            error(`unknown token.type: ${token.type}`);
            tokens.pop();
            return [];
        }
    }
    chars.reverse();
    const words = [];
    while (chars.length) {
        const word = [chars.pop()];
        while (typeof word[0] == typeof chars[chars.length - 1]) {
            word.push(chars.pop());
        }
        if (typeof word[0] == "number") {
            words.push(word);
        } else {
            words.push(word.join(""));
        }
    }
    return words;
}

function updateOutput(input) {
    const text = input.value;
    localStorage.setItem(LOCAL_STORAGE_INPUT_KEY, text);
    const ipaElements = [];
    const tunicElements = [];
    for (const word of encodeTunic(text)) {
        if (typeof word[0] == "number") {
            ipaElements.push(ipaFromSymbols(word));
            tunicElements.push(renderWord(word, DEFAULT_COLORS));
        } else {
            ipaElements.push(word);
            tunicElements.push(word);
        }
    }
    setChildren(document.getElementById("output-tunic"), tunicElements);
    setChildren(document.getElementById("output-ipa"), ipaElements);
}

function main() {
    init();

    const input = document.getElementById("input");
    input.value =
        localStorage.getItem(LOCAL_STORAGE_INPUT_KEY) || DEFAULT_INPUT;
    input.addEventListener("input", e => updateOutput(input));
    updateOutput(input);

    setChildren(document.getElementById("chart"), [
        htmlElement("section", {}, renderChart(input, MASK_CONSONANT)),
        htmlElement("section", {}, renderChart(input, MASK_VOWEL))
    ]);
}

main();
