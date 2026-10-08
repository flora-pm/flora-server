import {Transformer} from '@parcel/plugin';

export default new Transformer({
	async transform({asset}) {
		asset.meta.forceStableName = true;
		return [asset];
	},
});
