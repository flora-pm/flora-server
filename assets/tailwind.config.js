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
        "dark-1": "hsl(218, 30%, 15%)",
        "dark-2": "hsl(218, 30%, 20%)",
        "dark-3": "hsl(218, 30%, 25%)",
        "dark-4": "hsl(218, 30%, 30%)",
        "dark-5": "hsl(218, 30%, 35%)",
        "dark-6": "hsl(218, 30%, 40%)",
        "dark-7": "hsl(218, 30%, 45%)",
        "dark-8": "hsl(218, 30%, 50%)",
        "dark-9": "hsl(218, 30%, 55%)",
        "dark-10": "hsl(218, 30%, 60%)",
        "purple-1": "hsl(294, 40%, 20%)",
        "purple-2": "hsl(294, 40%, 25%)",
        "purple-3": "hsl(294, 40%, 30%)",
        "purple-4": "hsl(294, 40%, 35%)",
        "purple-5": "hsl(294, 40%, 40%)",
        "purple-6": "hsl(294, 40%, 50%)",
        "purple-7": "hsl(294, 40%, 55%)",
        "purple-8": "hsl(294, 40%, 60%)",
        "purple-9": "hsl(294, 40%, 65%)",
        "purple-10": "hsl(294, 40%, 65%)",
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
