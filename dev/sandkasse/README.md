# Kjøre pakken uten R (webR i Node)

Brukt fra 10. november 2026 for å lese og kontrollere fortellinger i bulk
uten at noen må kjøre R lokalt. webR er den samme R-motoren som AlternaLiv
bruker i nettleseren; npm-pakken har R.wasm med seg, så ingen nedlasting
fra webr.r-wasm.org er nødvendig.

    mkdir -p /tmp/wr && cd /tmp/wr && npm init -y && npm install webr
    cp <pakke>/dev/sandkasse/*.mjs <pakke>/dev/sandkasse/*.R /tmp/wr/
    node run.mjs <pakke> audit.R   # 300 liv med fortelling og kortform
    node run.mjs <pakke> coh.R     # tests/testthat/test-coherence.R med stubber
    node run.mjs <pakke> stats.R   # barn etter alder, lønnsspredning, bolig, pensjon

run.mjs monterer R/ og inst/extdata/ i webR, shimmer system.file() og
sourcer pakkefilene. R-koden skriver resultatet til /out.txt.

Begrensninger: testthat og devtools finnes ikke her, så hele testsuiten og
R CMD check må fortsatt kjøres lokalt. 300 liv tar under ett minutt.

## Sveip (fra 27. november 2026)

    sed 's/SEED <- 1; N <- 500/SEED <- 7; N <- 750/' sweep.R > s7.R
    node run.mjs <pakke> s7.R

sweep.R teller hvor ofte sluttsjekken i counterfact_me() ville trukket
livet på nytt (`.find_implausibilities()` på førstetrekket), flagger
tekstmønstre i fortellingen og skriver hver tolvte fortelling til lesing.
750 liv tar under tre minutter. Første sveip (5 390 liv): sluttsjekken
slo til i 0,1 prosent av førstetrekkene for lønn og 2–3 prosent for
nylig innvandrede boligeiere.
