# docs/ — the published web app (AlternaLiv)

Served at **https://martinisu.github.io/CounterfactMe/** by GitHub Pages
(source: `main` branch, `/docs` folder).

## Do not edit anything here by hand

Everything in `docs/` except this README is copied from `web/` by

```bash
python3 web/publish.py
```

which also rebuilds `cfm-assets.js` from `R/` and `inst/extdata/`,
writes `sitemap.xml` with today's date, and checks that the bundle's
version, R-file count and CSV count match the package, and that every
file `index.html` refers to exists. Edit in `web/`, run the script,
commit both.

Until 2026-10 this folder held a single hand-bundled `index.html`
(2.4 MB, larger than the unbundled source, and not reproducible). That
file is gone.

## What it needs from the network

- `https://webr.r-wasm.org/latest/webr.mjs`, plus `jsonlite` from webR's
  own repository on first run
- React, ReactDOM and Babel from unpkg (pinned with SRI hashes)
- Google Fonts

No user data is sent anywhere: lives are drawn in the browser.

## How it runs

1. `sampler.js`, a JavaScript approximation written against
   CounterfactMe v0.9.1, fills the screen while webR loads, and takes
   over if R fails. It is not maintained alongside the package.
2. webR loads the real package and draws every life from then on,
   **including locked draws** (`cfm_draw_locked_json()` in
   `cfm-reroll.R`). Before 2026-10 every locked draw silently went to
   the JS sampler.

The top bar says which one produced the life on screen:
"Kilde · R-pakke" or "Kilde · JS-fallback".

## Visibility

`index.html` carries a meta description, Open Graph / Twitter cards
(`og-image.png`, 1200×630), JSON-LD (`WebApplication`), a canonical URL
and an icon. The "Om" section at the bottom is static HTML on purpose:
it is what search engines, link previews and screen readers see before
R has loaded. Without it the indexable text was button labels.

`robots.txt` is not here because it would have no effect: crawlers only
read it at the host root (`martinisu.github.io/robots.txt`), which a
project page does not control. The default is "crawl everything".

`.nojekyll` stops GitHub Pages running the files through Jekyll.
