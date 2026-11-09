# AlternaLiv — webgrensesnitt

Kjoerer CounterfactMe i nettleseren via webR. Ingen R-installasjon kreves.

- `index.html` — siden, med metadata for soek og deling og en statisk Om-seksjon
- `app.jsx` — grensesnittet: narrasjon, pass, filtre, laaser, sannsynlighetspanel
- `cfm-webr.js` — starter webR og eksponerer `drawLife`, `drawLockedLife`, `rerollLife`
- `cfm-bootstrap.R` — shimmer `system.file()` og sourcer pakkefilene i webR
- `cfm-reroll.R` — pin-maskineriet: betinget om-trekning og laaste trekk
- `cfm-translate.js` — R-livsobjekt til app-format
- `sampler.js` — JS-reserve mens R laster; ikke vedlikeholdt siden v0.9.1
- `cfm-assets.js` — GENERERT fra pakken (R-filer og CSV-er som JSON)
- `favicon.svg`, `apple-touch-icon.png`, `og-image.png` — ikon og delingsbilde
- `build-assets.py`, `publish.py` — byggeverktoey, publiseres ikke

## Publisere

```bash
python3 web/publish.py
```

Bygger `cfm-assets.js`, skriver `sitemap.xml`, kopierer alt nettleseren
trenger til `docs/` og verifiserer resultatet. Commit `web/` og `docs/` sammen.

`policy_parameters.csv` holdes utenfor bundelen (`SKIP_CSV` i
build-assets.py), og raden dens strippes fra den bundlede SOURCES.csv.

`tests/testthat/test-web-bridge.R` sjekker at listene her (pin-noekler,
reroll-dimensjoner, laasbare felt) stemmer med R-sida.
