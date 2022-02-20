const esbuild = require("esbuild");
const copyDest = "../static"

const minify = false;
const sourcemap = true;
const watch_fs = true;

if (process.env.NODE_ENV === 'prod') {
  minify =  true;
  sourcemap = false;
  watch_fs = false;
}

esbuild.build({
  entryPoints: ['./js/app.js'],
  outfile: `${copyDest}/js/app.js`,
  bundle: true,
  sourcemap: sourcemap,
  minify: minify,
  target: "es2016",
  watch: watch_fs,
})
