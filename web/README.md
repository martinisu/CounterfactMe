# AlternaLiv — webgrensesnitt

Kjorer CounterfactMe i nettleseren via webR. Ingen R-installasjon kreves.

- `index.html` — kildeversjonen, laster de andre filene ved siden av seg
- `cfm-assets.js` — generert fra pakken (R-filer og CSV-er innebygd som JSON)
- `cfm-bootstrap.R` — shimmer `system.file()` og sourcer pakkefilene i webR
- `cfm-reroll.R` — betinget om-trekning av en dimensjon
- `app.jsx` — grensesnittet: narrasjon, pass, filtre, sannsynlighetspanel

Den publiserte versjonen ligger i `docs/index.html`: samme app bundlet til en
selvstendig fil, med alle pakkefiler og data inlinet.

## Regenerere etter en pakkeendring

`cfm-assets.js` ma bygges pa nytt fra `R/` og `inst/extdata/`, og
`docs/index.html` bundles pa nytt fra `web/index.html`.
