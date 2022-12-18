const esbuild = require("esbuild");
const assetsManifestPlugin = require("esbuild-plugin-assets-manifest");
const postcssPlugin = require("@baurine/esbuild-plugin-postcss3");
const fs = require("fs");
const chokidar = require('chokidar');
const path = require("path");

// PostCSS plugins
const postcssImport = require("postcss-import");   
const tailwindNesting = require("@tailwindcss/nesting");
const tailwind = require("tailwindcss");      
const autoprefixer = require("autoprefixer");     

let minify = false;
let sourcemap = true;
let entryNames = "[name]";

const mkProdPlugins = () => {
  return [
    assetsManifestPlugin({
      filename: "manifest.json",
      path: "../static",
      processOutput(assets) {
        console.log(assets);
        const orderAssets = {
          "app.js": assets.app.js,
          "styles.css": assets[''].css[0]
        }
        return JSON.stringify(orderAssets, null, "  ");
      }
    })
  ];
}

const pluginsList = () => {
  let plugins = [
    postcssPlugin.default({
      plugins: [
        postcssImport,
        tailwindNesting,
        tailwind,
        autoprefixer,
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
  entryPoints: {
    "app": "./js/app.js",
    "styles": "./css/styles.css",
  },
  outdir: "../static",
  bundle: true,
  logLevel: "verbose",
  sourcemap: sourcemap,
  minify: minify,
  target: "es2018",
  entryNames: entryNames,
  plugins: pluginsList(),
  metafile: true,
  incremental: process.argv.includes("--watch"),
}

if (process.argv.includes("--watch")) {
  (async () => {
    const result = await esbuild.build(config);
    chokidar.watch(["./js", "./css"]).on("all", async (event, path) => {
      if (event === "change") {
        console.log(`[esbuild] Rebuilding ${path}`);
        console.time("[esbuild] Done");
        await result.rebuild();
        console.timeEnd("[esbuild] Done");
      }
    });
  })();
} else {
  (async () => {
    const result = await esbuild.build(config);
    console.log({ result });
    fs.writeFileSync(
      path.join(__dirname, "metafile.json"),
      JSON.stringify(result.metafile)
    );
  })
}
