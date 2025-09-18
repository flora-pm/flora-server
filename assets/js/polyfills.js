// Popover API
// delete HTMLElement.prototype.popover
if (!(
	typeof HTMLElement !== 'undefined' &&
	typeof HTMLElement.prototype === 'object' &&
	'popover' in HTMLElement.prototype
)) {
	console.log('Popover API polyfill needed');
	import('@oddbird/popover-polyfill');
}
