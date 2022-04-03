function tokenIs(s) {
    return token => token == s
}

function tokenIsIdentifier(token) {
    return /^[-+.\d\w]/.test(token)
}

class State {
    constructor(tokens, index) {
        this.tokens = tokens
        this.index = index
    }

    copy() {
        return new State(this.tokens, this.index)
    }

    assign(state) {
        this.tokens = state.tokens
        this.index = state.index
    }

    next() {
        const result = {
            done: this.index >= this.tokens.length,
            value: this.tokens[this.index],
        }
        this.index += 1
        return result
    }

    match(matchers) {
        const state = this.copy()
        const tokens = []
        for (const matcher of matchers) {
            const result = state.next()
            if (result.done || !matcher(result.value)) {
                return null
            }
            tokens.push(result.value)
        }
        this.assign(state)
        return tokens
    }
}

function nextToken(state) {
    return state.tokens[state.index] || null
}

function parseError(state, message) {
    return new Error(message + " at token #" + state.index +
                     " (" + JSON.stringify(state.tokens[state.index]) + ")")
}

function parseDeclarationSpecifiers(state) {
    const declarationSpecifiers = []
    while (true) {
        const substate = state.copy()
        const tokens = substate.match([tokenIsIdentifier])
        if (!tokens) {
            break
        }
        state.assign(substate)
        declarationSpecifiers.push(tokens[0])
    }
    if (declarationSpecifiers.length == 0) {
        return null
    }
    if (declarationSpecifiers.length > 1 &&
        !state.copy().match([tokenIs("*")])) {
        declarationSpecifiers.pop()
        state.index -= 1
    }
    return {
        kind: "type",
        name: declarationSpecifiers.join(" "),
    }
}

function parseTypeQualifiers(state) {
    const typeQualifiers = []
    while (true) {
        const substate = state.copy()
        const tokens = substate.match([tokenIsIdentifier])
        if (!tokens) {
            break
        }
        state.assign(substate)
        typeQualifiers.push(tokens[0])
    }
    if (typeQualifiers.length > 0 && !state.copy().match([tokenIs("*")])) {
        typeQualifiers.pop()
        state.index -= 1
    }
    return typeQualifiers.join(" ")
}

function parseDirectDeclarator(state) {
    const token = nextToken(state)
    if (token == null) {
    } else if (token.startsWith("(")) {
        state.index += 1
        const params = []
        while (true) {
            const param = parseParameterDeclaration(state)
            while (nextToken(state).startsWith(",")) {
                state.index += 1
            }
            if (param == null) {
                break
            }
            params.push(param)
        }
        if (!nextToken(state).startsWith(")")) {
            throw parseError(state, "expected ')'")
        }
        state.index += 1
        const directDeclarator = parseDirectDeclarator(state)
        const variadic = params.length == 0
        if (params.length == 1 &&
            params[0].type.kind == "type" &&
            params[0].type.name == "void") {
            params.pop()
        }
        return specifier => ({
            kind: "function",
            params: params,
            ret: directDeclarator(specifier),
            variadic: variadic,
        })
    } else if (token.startsWith("[")) {
        state.index += 1
        const token = nextToken(state)
        let size = ""
        if (!token.startsWith("]")) {
            // TODO: support more complicated expressions
            if (!tokenIsIdentifier(token)) {
                throw parseError(state, "expected identifier")
            }
            state.index += 1
            size = token
            if (!nextToken(state).startsWith("]")) {
                throw parseError(state, "expected ']'")
            }
        }
        state.index += 1
        const directDeclarator = parseDirectDeclarator(state)
        return specifier => ({
            kind: "array",
            size: size,
            elem: directDeclarator(specifier),
        })
    }
    return specifier => specifier
}

function parseDeclarator(state) {
    const token = nextToken(state)
    if (token == null) {
    } else if (token.startsWith("*")) {
        state.index += 1
        const qualifiers = parseTypeQualifiers(state)
        const declarator = parseDeclarator(state)
        return specifier => declarator({
            kind: "pointer",
            target: specifier,
            qualifiers: qualifiers,
        })
    } else if (tokenIsIdentifier(token)) {
        state.index += 1
        const directDeclarator = parseDirectDeclarator(state)
        return specifier => ({
            name: token,
            type: directDeclarator(specifier),
        })
    } else if (token.startsWith("(")) {
        state.index += 1
        const substate = state.copy()
        const matched = substate.match([token => token.startsWith(")")]) ||
                        substate.match([
                            token => token.startsWith("void"),
                            token => token.startsWith(")"),
                        ])
        if (matched) { // special case
            state.assign(substate)
            const directDeclarator = parseDirectDeclarator(state)
            return specifier => ({
                name: "",
                type: {
                    kind: "function",
                    params: [],
                    ret: directDeclarator(specifier),
                    variadic: matched.length == 1,
                },
            })
        }
        const declarator = parseDeclarator(state)
        if (nextToken(state).startsWith(")")) {
            state.index += 1
            const directDeclarator = parseDirectDeclarator(state)
            return specifier => declarator(directDeclarator(specifier))
        }
        throw parseError(state, "expected ')'")
    }
    return specifier => ({
        name: "",
        type: specifier,
    })
}

function parseParameterDeclaration(state) {
    const specifier = parseDeclarationSpecifiers(state)
    if (specifier == null) {
        return null
    }
    return parseDeclarator(state)(specifier)
}

function lex(s) {
    s = /^\s*([^]*?)[\s;]*$/.exec(s)[1]
    const tokens = []
    while (s.length != 0) {
        const m = /^([-+.\d\w]+|[{}[\](),;*])\s*/.exec(s)
        if (m == null) {
            throw new Error("lexer failed on " + JSON.stringify(s))
        }
        tokens.push(m[1])
        s = s.substr(m[0].length)
    }
    return tokens
}

function parse(tokens) {
    const state = new State(tokens, 0)
    const declaration = parseParameterDeclaration(state)
    if (state.index != state.tokens.length) {
        throw parseError(state, "trailing garbage")
    }
    return declaration
}

function parensIf(cond, s) {
    if (cond) {
        return "(" + s + ")"
    } else {
        return s
    }
}

function makeText(s) {
    return document.createTextNode(s)
}

function makeElement(tagName, options, children) {
    const element = document.createElement(tagName)
    for (const child of children) {
        element.appendChild(child)
    }
    options = options || {}
    element.className = options.className || ""
    return element
}

function renderType(prec, type) {
    switch (type.kind) {
        case "array":
            const suffix = type.size == "" ? [] : [
                makeElement("span", {className: "op"}, [makeText(";")]),
                makeText(" "),
                makeElement("span", {className: " node expression"}, [
                    makeText(type.size),
                ]),
            ]
            return makeElement("span", {className: " node array"}, [].concat(
                [
                    makeElement("span", {className: "op"}, [makeText("[")]),
                    renderType(0, type.elem),
                ],
                suffix,
                [
                    makeElement("span", {className: "op"}, [makeText("]")]),
                ],
            ))
        case "function":
            const params = []
            for (const arg of type.params) {
                if (params.length > 0) {
                    params.push(
                        makeElement("span", {className: "op"}, [makeText(",")]),
                        makeText(" "),
                    )
                }
                params.push(renderDeclaration(0, arg))
            }
            if (type.variadic) {
                if (params.length > 0) {
                    params.push(
                        makeElement("span", {className: "op"}, [makeText(",")]),
                        makeText(" "),
                    )
                }
                params.push(
                    makeElement("span", {className: "op"}, [makeText("...")]),
                )
            }
            return makeElement("span", {className: " node function"}, [].concat(
                [
                    makeElement("span", {className: "op"}, [makeText("(")]),
                ],
                params,
                [
                    makeElement("span", {className: "op"}, [makeText(")")]),
                    makeText(" "),
                    makeElement("span", {className: "op"}, [makeText("->")]),
                    makeText(" "),
                    renderType(0, type.ret),
                ],
            ))
        case "pointer":
            return makeElement("span", {className: " node pointer"}, [
                makeText(type.qualifiers + (type.qualifiers ? " " : "")),
                makeElement("span", {className: "op"}, [makeText("&")]),
                renderType(0, type.target),
            ])
        case "type":
            return makeElement("span", {className: " node type"}, [
                makeText(type.name),
            ])
        default:
            throw new Error("invalid type.kind: " + type.kind)
    }
}

function renderDeclaration(prec, declaration) {
    return makeElement("span", {className: " node declaration"}, [
        makeElement("span", {className: "name"}, [
            makeText(declaration.name || "_"),
        ]),
        makeElement("span", {className: "op"}, [makeText(":")]),
        makeText(" "),
        renderType(prec, declaration.type),
    ])
}

function update(s) {
    try {
        const declaration = parse(lex(s))
        while (output.children.length) {
            output.removeChild(output.firstChild)
        }
        if (declaration) {
            output.appendChild(renderDeclaration(0, declaration))
        }
        input.setCustomValidity("")
    } catch (e) {
        input.setCustomValidity(e.toString())
    }
    window.setTimeout(() => form.reportValidity(), 0)
}

function findNode(element) {
    while (element && !(element.className || "").includes(" node ")) {
        element = element.parentNode
    }
    return element
}

const form = document.getElementById("form")
const input = document.getElementById("input")
const output = document.getElementById("output")
output.addEventListener("mouseover", event => {
    const element = findNode(event.target)
    if (findNode(element)) {
        element.className += " hover "
        event.stopPropagation()
    }
})
output.addEventListener("mouseout", event => {
    const element = findNode(event.target)
    if (findNode(element)) {
        element.className = element.className.replace(" hover ", "")
    }
})
input.addEventListener("change", event => update(event.target.value))
update(input.value)
