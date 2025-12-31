module.exports = {
    devServer: {
        static: __dirname,
    },
    devtool: "source-map",
    entry: "./main.js",
    output: {
        path: __dirname,
        filename: "script.js"
    }
}
