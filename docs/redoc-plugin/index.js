const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

module.exports = function (context, options) {
  return {
    name: 'redoc-compatibility-plugin',
    configureWebpack(config, isServer, utils) {
      return {
        resolve: {
          fallback: {
            fs: false,
          },
        },
        plugins: [
          new NodePolyfillPlugin(),
        ],
      };
    },
  };
};
