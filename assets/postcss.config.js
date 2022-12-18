module.exports = {
  plugins: [
    require("postcss-import") (),
    require("@tailwindcss/nesting"),
    require("tailwindcss") (),
    require("autoprefixer") (),
    require("postcss-copy") ({
      dest: "../assets/fonts",
    }),
    ...(process.env.env_build === "prod" ? [purgecss] : []),
    ...(process.env.env_build === "prod" ? [require("cssnano") () ] : []),
  ],
};
