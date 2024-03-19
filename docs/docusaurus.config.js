// @ts-check

const darkCodeTheme = require("prism-react-renderer/themes/palenight");
const lightCodeTheme = require("prism-react-renderer/themes/vsLight");
const path = require("path");

/** @type {import("@docusaurus/types").Config} */
const config = {
  title: "Flora Documentation",
  tagline: "",
  url: "https://flora.pm",
  baseUrl: "/documentation/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/favicon.png",
  organizationName: "flora-pm", // Usually your GitHub org/user name.
  projectName: "flora-server", // Usually your repo name.

  presets: [
    [
      "classic",
      /** @type {import("@docusaurus/preset-classic").Options} */
      ({
        docs: {
          sidebarPath: require.resolve("./sidebars.js"),
          routeBasePath: "/"
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import("@docusaurus/preset-classic").ThemeConfig} */
    ({
      colorMode: {
        defaultMode: "light",
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },
      navbar: {
        style: "dark",
        title: "Flora Documentation",
        logo: {
          alt: "Flora",
          src: "img/logo-dark-background.png",
          href: "https://flora.pm"
        },
        items: [
          {
            type: "doc",
            docId: "intro",
            label: "Guides",
            position: "left",
          },
          {
            to: "/api-reference",
            label: "API Reference",
            position: "left",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [],
        copyright: `Copyright © ${new Date().getFullYear()} Flora.pm. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme ,
        darkTheme: darkCodeTheme,
      },
    }),

  stylesheets: [
    "https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,400;0,700;1,400;1,700&display=swap"
  ],
  customFields: {},
  plugins: [path.resolve(__dirname, "redoc-plugin")],
};

module.exports = config;
