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
        "dark": "#0f1827",
        "dark-1": "#0f1827",
        "dark-2": "#232d3d",
        "dark-focused": "#3a3d3f",
        "brand-purple": {
          "1": "hsl(294, 37%, 27%)",
          "2": "hsl(297, 52%, 60%)"
          },
        "link": {
          "dark": "#4dabf7",
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
