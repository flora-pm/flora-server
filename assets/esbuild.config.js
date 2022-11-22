const esbuild = require("esbuild");
const assetsManifestPlugin = require("esbuild-plugin-assets-manifest");
const postCssPlugin = require("@deanc/esbuild-plugin-postcss");

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
let watch_fs = true;

let entryNames = "[name]";

const mkPlugins = () => {
  if (process.env.NODE_ENV === "prod") {
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
  } else {
    return [];
  }
}

const pluginsList = () => {
  let plugins = [
    postCssPlugin({
      plugins: [
        postcssImport,
        tailwindNesting,
        tailwind,
        autoprefixer,
        postcssCopy,
      ],
  })];
  let prodPlugins = mkPlugins();
  return plugins.concat(prodPlugins); 
}

if (process.env.NODE_ENV === "prod") {
  minify = true;
  sourcemap = false;
  watch_fs = false;
  entryNames = "[name]-[hash]";
}

esbuild.build({
  entryPoints: {
    "app": "./js/app.js",
    "styles": "./css/styles.css",
  },
  outdir: "../static",
  bundle: true,
  sourcemap: sourcemap,
  minify: minify,
  target: "es2018",
  entryNames: entryNames,
  watch: watch_fs,
  plugins: pluginsList(),
}).then(result => {
  console.log('watching...')
});
