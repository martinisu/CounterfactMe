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
