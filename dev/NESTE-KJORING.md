# Neste kjøring

Kjør disse i R, i prosjektmappa. Jeg kan ikke kjøre R i sandkassa.

## 1. Testene (ny fil: test-web-bridge.R)

```r
devtools::test()
```

Ventet: de to kjente feilene i `test-counterfact-me.R:88-89`
(`expect_length(tbl$female, 6)` mot faktisk 8 — selve den hardkodede-liste-
feilen de nye testene finnes for å fange) står fortsatt. Alt annet skal passere.

De fem nye testene i `test-web-bridge.R` leser `web/` som tekst og sjekker at
nettleserlagets hardkodede lister stemmer med R-sida. De hoppes over i
`R CMD check` fordi `web/` ligger i `.Rbuildignore`.

## 2. AlternaLiv i nettleseren

`docs/` er allerede publisert (`python3 web/publish.py` er kjørt).
Åpne den lokalt med en server — `file://` virker ikke for webR:

```r
servr::httd("docs", port = 4321)
```

eller i terminalen: `python3 -m http.server 4321 --directory docs`

### Dette skal testes

Det som ble bygget nå: **låste trekk går i R, ikke i JS-sampleren.**

1. Vent til topbaren sier **«Kilde · R-pakke»**.
2. Lås ett felt (hengelås-knappen på en dimensjon) og trekk på nytt.
   - Det låste feltet skal stå uendret.
   - Topbaren skal fortsatt si «Kilde · R-pakke» — ikke «JS-fallback».
3. Lås **Nettoformue** eller **Studieretning**. Disse var ikke låsbare før
   (JS-sampleren kunne ikke pinne dem); nå skal hengelåsen dukke opp og virke.
4. Lås **Navn** og trekk. Kjønnet skal følge navnet, ikke trekkes på nytt.
5. Lås **Bosted**, og sjekk at trekket er raskt — kommunen pinnes, så
   fylkesfilterets 40 forsøk hoppes over.
6. Lås 3-4 felt samtidig og trekk flere ganger. De låste skal stå; resten
   skal variere, og variere *betinget* på de låste (lås høy utdanning →
   inntekt og yrke skal følge etter).

### Hva som skal ha blitt feil hvis noe er galt

Hvis topbaren slår over til «Kilde · JS-fallback» ved et låst trekk, feilet
R-ruten og det ligger en melding i konsollen (`[CFM] R draw failed`).

## 3. Hva som ble endret

- `web/cfm-reroll.R` — pin-maskineriet trukket ut av `cfm_reroll_json()` til
  `.cfm_apply_pins()`, og ny `cfm_draw_locked_json(ctx_id, pin, ...)`.
  Et låst trekk er samme operasjon som en reroll, bare med et annet nøkkelsett.
- `web/cfm-webr.js` — ny `drawLockedLife()`. `KEEP_KEYS` (en kopi av R-sidas
  liste) erstattet med et identifikator-regex; R gjør den egentlige kontrollen.
- `web/app.jsx` — `useR` krever ikke lenger at det ikke finnes låser.
  `Nettoformue` og `Studieretning` er låsbare. Statusindikatoren sier nå hva
  som faktisk lagde livet på skjermen, ikke bare om R er lastet.
- `web/sampler.js` — ærlig header: oppstartsreserve, ikke hovedmotor, og
  ikke oppdatert siden v0.9.1. Rammeverksvelgeren er ikke lenger et valg.
- `tests/testthat/test-web-bridge.R` — ny.

## 4. Ikke rørt (bevisst)

- `R/llm.R`
- `docs/index.html` genereres av `web/publish.py` — rediger aldri for hånd
- `policy_parameters.csv` og `dev/design/policy-mode-plan.md` skal ikke til git

## 5. Står fortsatt igjen

- Kalibreringsavvikene (`pop.txt`): verdsettingsrabatt på formue,
  `00-`-overstyringen, kapitalinntekt inn i `.cond_income()` **før** inntekt settes
- Geografisk betinging av formuehalen (44 % av topp 1 % bor i Oslo + Asker/Bærum)
- Commit + push av v0.9.73 (meldingen ligger i `dev/commit-melding.txt`)

## Neste gang (notert 2026-11-09)

- Fortelling: "Etter et samlivsbrudd har hun to barn" -> "Hun er skilt og har
  to barn" (R/narrate.R, grenen marital == "skilt").
- Synlighet: Google Search Console og Bing er satt opp 2026-11-09. Sjekk om
  "AlternaLiv" gir treff i soek etter en til to uker.
