# CounterfactMe — Prosjektbeskrivelse

## Formål

CounterfactMe er en R-pakke som genererer tilfeldige, men realistiske norske liv — navn, alder, kommune, utdanning, yrke, inntekt, sivilstand og husholdningstype — trukket fra faktiske fordelinger i norske registerdata (SSB). Tanken er enkel: **hva slags liv kunne du hatt?**

Pakken er et pedagogisk og sosiologisk verktøy. Den gjør abstrakte begreper som sosial mobilitet, ulikhet og livssjanser konkrete ved å vise frem én tilfeldig livshistorie av gangen. Når en student trekker ti kontrafaktiske liv og ser at inntekt henger tett sammen med utdanning, at deltid dominerer i omsorgsyrker, og at en 60-åring har 20 % sjanse for uføretrygd — da blir fordelingene til noe du kan føle, ikke bare lese i en tabell.

## Målgruppe

- **Sosiologistudenter og -undervisere**: som øvelse i å tenke kontrafaktisk om livssjanser og sosial struktur.
- **Forskere i sosiologi, demografi og samfunnsøkonomi**: som illustrasjonsverktøy i formidling, presentasjoner og artikler.
- **Nysgjerrige**: alle som vil leke med tanken "hva om jeg var en tilfeldig nordmann?".

## Hva pakken gjør

Pakken trekker kontrafaktiske liv der dimensjonene henger betinget sammen, slik de gjør i virkeligheten:

```
alder → utdanning → yrke → inntekt
alder → sivilstand → husholdning
alder + kjønn → navn (cohort-vektet)
```

Datagrunnlaget er hentet direkte fra SSBs åpne API (PxWebApi), og dekker:

- **Aldersfordeling** fra SSB 07459 (befolkning per 1.1, 5-årsbånd)
- **Kommuner og fylker** fra SSB 07459 (357 kommuner, 15 fylker, befolkningsvektet)
- **Fornavn** fra SSB 10467 (1 969 unike navn, cohort-vektet per tiår 1940–2020)
- **Utdanningsnivå per alder** fra SSB 08921 (5 SSB-nivåer → 10 pakkenivåer)
- **Yrke og lønn** fra SSB 11418 (407 STYRK-08 4-sifrede yrker, median-/snittlønn, heltid/deltid-andeler)
- **Uføretrygd per alder** fra SSB 11715 (andel uføretrygdede 18–67 år)

## Hva pakken ikke gjør

- Pakken simulerer ikke kausale mekanismer. Den trekker fra betingede fordelinger, ikke fra en strukturell modell.
- Pakken reproduserer ikke enkeltpersoner. Kombinasjonene er tilfeldige — det finnes ingen mapping tilbake til faktiske individer.
- Pakken gir ikke presise estimater av fellesfordelinger (f.eks. P(yrke | alder, utdanning, kjønn, bosted) samtidig). Den bruker forenklede betingelser.
- Pakken er ikke en befolkningsframskrivning eller mikrosimuleringsmodell.

## Begrensninger og antakelser

1. **Betingede fordelinger er forenklede**: Avhengighetsstrukturen er en kjede (alder → utdanning → yrke → inntekt), ikke en fullstendig DAG. Kryssinteraksjoner (f.eks. yrke × bosted, kjønn × deltid) er ikke modellert ennå.
2. **SSB-data er marginaler, ikke full krysstabulering**: Vi observerer P(utdanning | alder) og P(yrke | headcount, STYRK-major), men ikke den fullstendige felles fordelingen.
3. **Utdanning-til-yrke-koblingen** bruker en heuristisk mapping fra pakkens utdanningskode til STYRK-hovedgrupper (første siffer). Dette er en grov approksimering.
4. **Inntekt er forankret i SSBs medianlønn per yrke** med log-normal støy og aldersskalering. Den fanger ikke opp reell inntektsspredning innenfor yrker fullt ut.
5. **Deltidsandel** er yrkesspesifikk (fra SSB 11418 headcount-ratio), men deltidsinntekten bruker en enkel skaleringsmultiplikator, ikke faktisk stillingsprosent.
6. **Geografi er ikke koblet til yrke eller inntekt** — en fisker kan havne i Oslo, en PR-sjef på Røst.

## Teknisk struktur

```
CounterfactMe/
├── R/
│   ├── zzz.R              # Datalasting fra inst/extdata/ til .cfm_env
│   ├── samplers.R          # Enkeltdimensjon-samplere (navn, kommune, yrke, …)
│   ├── conditional.R       # Betingede trekninger (utdanning|alder, yrke|utdanning, …)
│   ├── counterfact_me.R    # Hovedfunksjon: counterfact_me()
│   └── print.R             # Pretty-print av counterfactme-objekter
├── inst/extdata/           # CSV-filer hentet fra SSB
├── data-raw/               # Python/R-scripts for å regenerere CSV-ene fra SSB API
├── man/                    # Roxygen-generert dokumentasjon
└── DESCRIPTION
```

## Dataproveniensregister

| Fil | SSB-tabell | Innhold |
|-----|-----------|---------|
| `age_distribution.csv` | 07459 | Nasjonale aldersandeler (5-årsbånd) |
| `municipalities.csv` | 07459 | 357 kommuner med fylke og folketall |
| `counties.csv` | 07459 | 15 fylker med folketall |
| `first_names_cohort.csv` | 10467 | 1 969 fornavn, cohort-vektet per tiår |
| `education_by_age.csv` | 08921 | Utdanningsnivå × 8 aldersbånd |
| `occupations_salary.csv` | 11418 | 407 yrker × {heltid, deltid, alle} × lønn/antall |
| `disability_by_age.csv` | 11715 | Uføreandel × 6 aldersbånd |
| `education_levels.csv` | Manuell | 10-nivå utdanningstaksonomi |
| `income_deciles.csv` | Manuell | 10 inntektsdesiler (NOK-grenser) |
| `occupations.csv` | Manuell | 7 021 STYRK-yrkesnavn |
| `household_types.csv` | Manuell | 11 husholdningstyper |
| `marital_status.csv` | Manuell | 7 sivilstandskategorier |

## Fremtidige utvidelser

Sentrale planlagte utvidelser (se `back-up/TODO.md` for full liste):

- **Foreldre-dimensjon** med utdannings- og yrkesarv (intergenerasjonell korrelasjon)
- **Arv og formue** knyttet til foreldres posisjon
- **Strukturell trekningsvariant**: la brukeren velge et teoretisk rammeverk (Bourdieu, Rawls, meritokrati osv.) som endrer koblingene mellom foreldrebakgrunn og utfall
- **Full yrke × alder × utdanning × deltid**: erfaringstillegg, aldersvektet lønn, og headcount-vekting
- **Geografi × yrke**: kommunestruktur som betinger hvilke yrker som er realistiske
