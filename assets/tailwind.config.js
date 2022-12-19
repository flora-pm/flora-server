const defaultTheme = require("tailwindcss/defaultTheme");

module.exports = {
  content: ["./js/**/*.js", "../src/FloraWeb/**/*.*hs"],
  darkMode: "class",
  theme: {
    extend: {
      fontFamily: {
        sans: ["Inter var", ...defaultTheme.fontFamily.sans],
      },
      colors: {
        "blue-1": "hsl(218, 30%, 15%)",
        "blue-2": "hsl(218, 30%, 20%)",
        "blue-3": "hsl(218, 30%, 25%)",
        "blue-4": "hsl(218, 30%, 30%)",
        "blue-5": "hsl(218, 30%, 40%)",
        "blue-6": "hsl(218, 30%, 45%)",
        "blue-7": "hsl(218, 30%, 50%)",
        "gray-1": "hsl(221, 39%, 11%)",
        "gray-2": "hsl(215, 28%, 17%)",
        "gray-3": "hsl(217, 19%, 27%)",
        "gray-4": "hsl(220, 9%, 46%)",
        "gray-5": "hsl(218, 11%, 65%)",
        "gray-6": "hsl(216, 12%, 84%)",
        "gray-7": "hsl(220, 13%, 91%)",
        "purple-1": "hsl(294, 40%, 20%)",
        "purple-2": "hsl(294, 40%, 25%)",
        "purple-3": "hsl(294, 40%, 30%)",
        "purple-4": "hsl(294, 40%, 35%)",
        "purple-5": "hsl(294, 40%, 50%)",
        "purple-6": "hsl(294, 40%, 55%)",
        "purple-7": "hsl(294, 40%, 60%)",
        link: {
          dark: "#4dabf7",
        },
        "admin-card": {
          dark: "#202936",
          DEFAULT: "#202936",
        },
      },
    },
  },
  variants: {
    display: ["responsive", "dropdown"],
  },
  plugins: [
    // require('@tailwindcss/forms'),
  ],
};
