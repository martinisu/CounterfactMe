# docs/ — the published web app

`index.html` is served at
**https://martinisu.github.io/CounterfactMe/** by GitHub Pages
(source: `main` branch, `/docs` folder).

## Do not edit index.html

It is generated, not written. A single 2.3 MB file with everything
inlined: React, Babel, the app code, the JS fallback sampler, and
`cfm-assets.js` built from the R package sources — the `.R` files and
all 51 CSVs, gzipped and base64-encoded into the page.

Editing it by hand means editing base64 inside a bundler payload. Don't.
Changes are made in the source project and the file is replaced whole:

```bash
cp /path/to/AlternaLiv.html docs/index.html
git add docs/index.html && git commit -m "Update web app" && git push
```

## What it needs from the network

One thing: `https://webr.r-wasm.org/latest/webr.mjs`, plus
`webr::install("jsonlite")` from webR's own repository on first run.
Everything else is in the file.

## How it runs

Two samplers, in sequence:

1. A JavaScript approximation runs immediately, so the page is usable
   while webR downloads.
2. webR loads the real R package and takes over.

The JS sampler is described in its own source as approximating
**CounterfactMe v0.9.1** distributions. The R package it hands over to
is 0.9.55. Everything fixed between those versions — religion by country
rather than region, the age gates on survey dimensions, retirees not
described as employed, plausible immigrant arrival ages — applies only
after webR has loaded. The first few seconds show the old behaviour.

Worth knowing when someone reports something odd: ask whether the page
had finished loading.

## Change log

Only what changed between published versions, since the file is opaque
to `git diff`:

- **2026-08-18** — removed the mobile reordering in
  `@media (max-width: 700px)` that placed the action buttons above the
  passport card. Order now follows source order on phones. No other
  change: the ten code blocks, the CSVs and the R package inside are
  byte-identical to the previous build.
