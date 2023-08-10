// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  someSidebar: {
    // 'Introduction': ['intro'],
    // 'Guides': [''],
    'API': [
      {
        type: 'link',
        label: 'API Documentation', // The link label
        href: '/api-reference', // The internal path
      },
    ]
  },
};

module.exports = sidebars;
