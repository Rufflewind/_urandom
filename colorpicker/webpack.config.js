module.exports = {
    devtool: "source-map",
    entry: "./main.js",
    output: {
        path: __dirname,
        filename: "script.js",
    },
    module: {
        rules: [
            {
                test: /\.(frag|vert)$/,
                type: "asset/source",
            },
        ]
    },
};
