const esbuild = require("esbuild");
const assetsManifestPlugin = require("esbuild-plugin-assets-manifest");
const postcssPlugin = require("@deanc/esbuild-plugin-postcss");
const fs = require("fs");
const chokidar = require('chokidar');
const path = require("path");
const {sassPlugin} = require("esbuild-sass-plugin")

// PostCSS plugins
const postcssImport = require("postcss-import");   
const tailwindNesting = require("@tailwindcss/nesting");
const tailwind = require("tailwindcss");      
const autoprefixer = require("autoprefixer");     
const postcssCopy = require("postcss-copy")({      
    dest: "../assets/fonts",      
});

let minify = false;
let sourcemap = true;
let entryNames = "[name]";

const watchDirectories = [  "./css", "./js"];

const mkProdPlugins = () => {
  return [
    assetsManifestPlugin({
      filename: "manifest.json",
      path: "../static",
      processOutput(assets) {
        console.log(assets);
        const orderAssets = {
          "app.js": assets.app.js,
          "livereload.js": assets.livereload.js,
          "styles.css": assets[''].css[0]
        }
        return JSON.stringify(orderAssets, null, "  ");
      }
    })
  ];
}

const pluginsList = () => {
  let plugins = [
    sassPlugin(),
    postcssPlugin({
      plugins: [
        postcssImport,
        tailwindNesting,
        tailwind,
        autoprefixer,
        postcssCopy,
      ],
    })];
  let prodPlugins = process.env.NODE_ENV === "prod" ? mkProdPlugins() : [];
  return plugins.concat(prodPlugins); 
}

if (process.env.NODE_ENV === "prod") {
  minify = true;
  sourcemap = false;
  entryNames = "[name]-[hash]";
}


const config = {
  color: true,
  entryPoints: {
    "app": "./js/app.js",
    "livereload": "./js/livereload.js",
    "styles": "./css/styles.css",
  },
  outdir: "../static",
  bundle: true,
  logLevel: "info",
  sourcemap: sourcemap,
  minify: minify,
  target: "es2018",
  entryNames: entryNames,
  plugins: pluginsList(),
  metafile: true,
  incremental: process.argv.includes("--watch"),
  watch: process.argv.includes("--watch"),
}

console.log(config.outdir);
console.log(__dirname);

if (process.argv.includes("--watch")) {
  (async () => {
    const result = await esbuild.build(config);
    chokidar.watch(watchDirectories).on('all', (event, path) => {
      console.log(`rebuilding ${path}`)
      result.rebuild()
    })
  })();

} else {
  esbuild.build(config).catch(() => process.exit(1))
}
