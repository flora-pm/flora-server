import {Parcel} from "@parcel/core";
import {fileURLToPath} from 'url';

let bundler = new Parcel({
  entries: ["./js/app.js", "./css/styles.css"],
  defaultConfig: "@parcel/config-default",
  defaultTargetOptions: {
    distDir: "../static"
  },
  additionalReporters: [{
    packageName: "parcel-reporter-bundle-manifest",
    resolveFrom: fileURLToPath(import.meta.url)
  }]

});

let subscription = await bundler.watch((err, event) => {
  if (err) {
    // fatal error
    throw err;
  }

  if (event.type === 'buildSuccess') {
    let bundles = event.bundleGraph.getBundles();
    console.log(`✨ Built ${bundles.length} bundles in ${event.buildTime}ms!`);
  } else if (event.type === 'buildFailure') {
    console.log(event.diagnostics);
  }
});
