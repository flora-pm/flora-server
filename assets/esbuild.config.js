const esbuild = require("esbuild");
const { compressor:compressorPlugin } = require("esbuild-plugin-compressor");
const assetsManifestPlugin = require("esbuild-plugin-assets-manifest");
const postcssPlugin = require("@deanc/esbuild-plugin-postcss");

// PostCSS plugins
const postcssImport = require("postcss-import");
const postcssNesting = require("postcss-nesting");
const postcssCustomMedia = require('postcss-custom-media');
const autoprefixer = require("autoprefixer");
const postcssCopy = require("postcss-copy")({
  dest: "../static",
});
const postcssDesignTokenUtils = require("postcss-design-token-utils");
const designTokensConfig = require("./style-tokens/tokens.js");
const postcssOklchForOldWebkit = require("postcss-color-oklch-for-old-webkit");

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
          "polyfills.js": assets.polyfills.js,
          "alpine.js": assets.alpine.js,
          "htmx.js": assets.htmx.js,
          "styles.css": assets[''].css[0],
        }
        return JSON.stringify(orderAssets, null, "  ");
      }
    }),
    compressorPlugin({
      fileTypes: ['js', 'css'],
    }),
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
        postcssOklchForOldWebkit,
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
    "polyfills": "./js/polyfills.js",
    "alpine": "./js/alpine.js",
    "htmx": "./js/htmx.js",
    "styles": "./css/styles.css",
  },
  outdir: "../static",
  bundle: true,
  logLevel: "info",
  sourcemap: sourcemap,
  minify: minify,
  target: "es2018",
  format: 'esm',
  splitting: true,
  entryNames: entryNames,
  plugins: pluginsList(),
  metafile: true,
  loader:
    { '.woff2': 'file',
    '.ttf': 'file',
    '.svg': 'file',
  },
}

esbuild.build(config).catch((err) => {
  console.error(err)
  process.exit(1)
});
