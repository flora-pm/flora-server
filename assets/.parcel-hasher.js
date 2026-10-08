import {Namer} from '@parcel/plugin';
import path from 'path';

export default new Namer({

	name({ bundle, bundleGraph }) {

		let bundleGroup = bundleGraph.getBundleGroupsContainingBundle(bundle)[0];
		let isEntry = bundleGraph.isEntryBundleGroup(bundleGroup);
		// Add hash on entry points only (already added on others dependencies)
		if (isEntry === false) return
		let rootEntry = bundleGraph.getEntryRoot(bundle.target)
		let mainEntry = bundle.getMainEntry()
		let filePath = path.relative(rootEntry, mainEntry.filePath);

		// A 'forceStableName' meta variable can be set through a transformer to avoid hashing
		const dontHash = bundle.getEntryAssets()[0].meta.forceStableName === true || bundle.env.shouldOptimize === false
		if (dontHash) return filePath

		let extension = path.extname(filePath)
		const basename = filePath.slice(0, (-1 * extension.length))
		const hash = bundle.getContentHash()

		return basename + "-" + hash + extension;
	}

})
