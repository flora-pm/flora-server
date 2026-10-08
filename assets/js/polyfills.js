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
// Anchor Positioning
;(async () => {
	if (!("anchorName" in document.documentElement.style)) {
		console.log("Anchor Positioning polyfill needed")
		await import("@oddbird/css-anchor-positioning")
	}
})()
// Interest Invokers
;(async () => {
	if (!HTMLButtonElement.prototype.hasOwnProperty("interestForElement")) {
		console.log("Interest Invokers polyfill needed")
		await import("interestfor")
	}
})()
