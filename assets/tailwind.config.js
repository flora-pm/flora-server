const defaultTheme = require("tailwindcss/defaultTheme")

module.exports = {
  mode: "jit",
  content: ["./js/**/*.js", "../src/FloraWeb/**/*.*hs"],
  darkMode: "class",
  theme: {
    extend: {
      fontFamily: {
        sans: ["Inter var", ...defaultTheme.fontFamily.sans],
      },
      colors: {
        "background": {
          "dark": "#25282a",
          "darker": "#1D2021",
          "dark-focused": "#3a3d3f",
          "DEFAULT": "#f3f4f6",
        },
        "brand-purple": {
          "DEFAULT": "#4e315e",
          "dark": "#352140"
        },
        "navbar": {
          "dark": "#1D2021",
          "darker": "#16191a"
        },
        "link": {
          "dark": "#4dabf7",
          "DEFAULT": "#1a0dab",
        },
        "admin-card": {
          "dark": "#202936",
          "DEFAULT": "#202936",
        }
      }
    },
  },
  variants: {
    display: ['responsive', 'dropdown']
  },
  plugins: [
    // require('@tailwindcss/forms'),
  ],
};
