module.exports = {
  plugins: [
    require("postcss-import")(),
    require("@tailwindcss/nesting"),
    require("tailwindcss")(),
    require("autoprefixer")(),
    require("postcss-copy")({
      dest: "../assets/fonts",
    }),
    ...(process.env.ENV_BUILD === "prod" ? [purgecss] : []),
    ...(process.env.ENV_BUILD === "prod" ? [require("cssnano")()] : []),
  ],
};
