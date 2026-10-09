import { withMermaid } from 'vitepress-plugin-mermaid'
import apiSidebar from '../api/_sidebar.json'

// one sidebar for every page but the API, in reading order: the "previous" and "next" links at the bottom of a page
// follow it, so the documentation reads from the first page to the last
const docs = [
  {
    text: 'Start here',
    items: [
      { text: 'Introduction', link: '/guide/' },
      { text: 'Installation', link: '/guide/install' },
    ],
  },
  {
    text: 'Tutorial',
    items: [
      { text: 'Overview',                         link: '/manual/' },
      { text: '1. A first bar',                   link: '/manual/tutorial/01-first-bar' },
      { text: '2. The look of the bar',           link: '/manual/tutorial/02-look' },
      { text: '3. Colours and styles',            link: '/manual/tutorial/03-colours' },
      { text: '4. What the bar reports',          link: '/manual/tutorial/04-reports' },
      { text: '5. Spinners and counters',         link: '/manual/tutorial/05-spinners' },
      { text: '6. Talking while the bar runs',    link: '/manual/tutorial/06-terminal' },
      { text: '7. Nested loops',                  link: '/manual/tutorial/07-nested' },
      { text: '8. Batch jobs and logs',           link: '/manual/tutorial/08-logs' },
      { text: '9. Layout templates',              link: '/manual/tutorial/09-templates' },
      { text: '10. Unknown ends',                 link: '/manual/tutorial/10-unknown-ends' },
      { text: '11. A 1980s dashboard',            link: '/manual/tutorial/11-dashboard' },
    ],
  },
  {
    text: 'Recipes',
    items: [
      { text: 'Cookbook', link: '/manual/cookbook' },
    ],
  },
  {
    text: 'Reference',
    items: [
      { text: 'Feature map',                link: '/guide/features' },
      { text: 'The bar object',             link: '/guide/bar' },
      { text: 'Colours and styles',         link: '/guide/styling' },
      { text: 'Spinners',                   link: '/guide/spinners' },
      { text: 'Layout templates',           link: '/guide/templates' },
      { text: 'Terminals and logs',         link: '/guide/terminals' },
      { text: 'Behaviour and limitations',  link: '/guide/limitations' },
    ],
  },
  {
    text: 'Project',
    items: [
      { text: 'Changelog',    link: '/guide/changelog' },
      { text: 'Contributing', link: '/guide/contributing' },
    ],
  },
]

export default withMermaid({
  title: 'forbear',
  description: 'Fortran (progress) B(e)ar environment',
  base: '/forbear/',
  head: [['link', { rel: 'icon', type: 'image/svg+xml', href: '/forbear/logo.svg' }]],

  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      fortran: 'fortran-free-form',
      f90: 'fortran-free-form',
      f03: 'fortran-free-form',
      f08: 'fortran-free-form',
    },
  },

  themeConfig: {
    logo: '/logo.svg',
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Start here', link: '/guide/', activeMatch: '^/guide/(index|install)' },
      { text: 'Tutorial', link: '/manual/tutorial/01-first-bar', activeMatch: '^/manual/(index|tutorial/)' },
      { text: 'Cookbook', link: '/manual/cookbook', activeMatch: '^/manual/cookbook' },
      {
        text: 'Reference',
        link: '/guide/features',
        activeMatch: '^/guide/(features|bar|styling|spinners|templates|terminals|limitations)',
      },
      { text: 'API', link: '/api/' },
      {
        text: 'Project',
        items: [
          { text: 'Changelog',    link: '/guide/changelog' },
          { text: 'Contributing', link: '/guide/contributing' },
        ],
      },
    ],

    sidebar: {
      '/guide/': docs,
      '/manual/': docs,
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'Overview', link: '/api/' },
          ],
        },
        ...apiSidebar,
      ],
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/szaghi/forbear' },
    ],

    search: {
      provider: 'local',
    },

    footer: {
      message: 'Released under the GPL v3, BSD 2-Clause, BSD 3-Clause or MIT License.',
      copyright: 'Copyright © 2017-2026 Stefano Zaghi',
    },
  },

  mermaid: {},

  vite: {
    // Build with an explicit modern JS target so the docs compile regardless of
    // which mermaid/vitepress/esbuild versions npm resolves. Vite's default
    // es2020 target forces esbuild to down-level modern syntax (e.g. the
    // destructuring mermaid 11.16+ emits), which it refuses to do and the build
    // dies. es2022 needs no lowering and is within VitePress's browser floor.
    build: {
      target: 'es2022',
    },
  },
})
