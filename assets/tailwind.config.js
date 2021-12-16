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
          "DEFAULT": "#f3f4f6",
        },
        "brand-purple": {
          "DEFAULT": "#663399",
        },
        "link": {
          "dark": "#4dabf7",
          "DEFAULT": "#1a0dab",
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
