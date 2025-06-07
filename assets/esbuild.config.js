const esbuild = require("esbuild");
const assetsManifestPlugin = require("esbuild-plugin-assets-manifest");
const postcssPlugin = require("@deanc/esbuild-plugin-postcss");

// PostCSS plugins
const postcssImport = require("postcss-import");
const postcssNesting = require("postcss-nesting");
const postcssCustomMedia = require('postcss-custom-media');
const autoprefixer = require("autoprefixer");
const postcssCopy = require("postcss-copy")({
  dest: "../assets/fonts",
});
const postcssDesignTokenUtils = require("postcss-design-token-utils");
const designTokensConfig = require("./style-tokens/tokens.js");

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
          "styles.css": assets[''].css[0],
          "prism.js": assets.prism.js,
        }
        return JSON.stringify(orderAssets, null, "  ");
      }
    })
  ];
}

const pluginsList = () => {
  let plugins = [
    postcssPlugin({
      plugins: [
        postcssDesignTokenUtils({
          tokens: designTokensConfig,
        }),
        postcssImport,
        postcssNesting,
        postcssCustomMedia,
        autoprefixer,
        postcssCopy,
      ],
    })
  ];
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
    "styles": "./css/styles.css",
    "prism": "./js/prism.js",
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
}

esbuild.build(config).catch(() => process.exit(1));
