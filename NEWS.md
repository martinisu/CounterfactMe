# CounterfactMe 0.9.32

## Data provenance
- Fjernet `0-15`-radene fra `self_rated_health.csv`, `social_isolation.csv`,
  `tv_hours.csv`, `sleep_hours.csv` og `chronic_illness_prob.csv`. Ingen
  av dem hadde provenans: ingen skript i `data-raw/` henter dem, og
  verdiene var runde tall som summerte til noyaktig 1. De ble satt fordi
  aldersoppslaget trengte noe a finne.
- Tilsvarende for `.cond_disability()`, som hadde en hardkodet
  barnesannsynlighet pa 0.06 med hardkodede typevekter.
  `disability_by_age.csv` dekker ikke barn -- den gjelder uforetrygd, ikke
  funksjonsnedsettelse, og starter pa 18-24.
- `sleep_hours`, `media_tv_hours`, `has_chronic` og `has_disability` gates
  na under 16, sammen med sporreskjema-feltene fra 0.9.31.

  Barn beholder det som enten er SSB-basert (navn, alder, geografi,
  familie, husholdning, religion) eller apenbart skrevet (barnevarianter
  av yrke, inntekt og kosthold). Ingenting imellom -- ingen ukildede tall
  presentert som malinger.

## Robustness
- Oppslag som ikke finner sitt aldersband returnerer na `NA` i stedet for
  a falle tilbake pa forste rad i tabellen. Den gamle fallbacken
  (`row <- srh[1, ]`) ville gitt et barn 16-24-verdien uten a si fra.

# CounterfactMe 0.9.31

## Validity
- Sporreskjema-baserte dimensjoner undertrykkes na under undersokelsens
  egen nedre aldersgrense. En firearing fikk tidligere en generell
  tillitsscore (0-10), en podkastvane og en favorittavis -- den siste
  betinget av partiet barnet "stemmer" pa. Levekarsundersokelsen spor
  16+, sa slike verdier var ikke tilnaerminger, men oppdiktede
  observasjoner presentert med samme autoritet som registertallene.

  Grenser (`.dimension_min_age`): generell tillit, ensomhet, naere
  venner, fortrolig og selvrapportert helse fra 16; avis, podkast og
  sosiale medier fra 13.

  Bevisst IKKE gatet: kronisk sykdom og funksjonsnedsettelse er
  registerfestede forhold som gjelder barn like fullt, og TV-tid, sovn
  og kosthold har egne 0-15-band. Alkohol var allerede gatet ved 16.

# CounterfactMe 0.9.30

## Breaking
- Standardspraket i `counterfact_me()` er endret fra `"en"` til `"no"`.
  Resten av pakken (`counterfact_me_constrained()`,
  `counterfact_parallel_lives()`, `verify_consistency()`) hadde allerede
  norsk som default, sa dette gjor pakken konsistent. Engelsk finnes
  fortsatt via `lang = "en"`, men er delvis: yrkestitler kommer fra SSBs
  STYRK-98-register, som bare finnes pa norsk.

## Bug fixes
- Kjonnede yrkestitler ble tildelt uavhengig av ego sitt kjonn, sa en
  kvinne kunne fa "Fosterfar". Kjonnsvektingen i `occupations_gender.csv`
  ligger pa 4-sifret STYRK-08-niva, mens FOSTERMOR og FOSTERFAR deler
  gruppe 5311 -- valget mellom dem var derfor blindt. `.draw_detail_yrke()`
  filtrerer na pa kjonn for seks slike par.
  Merk: DAMEFRISOR/HERREFRISOR og DAMESKREDDER/HERRESKREDDER er bevisst
  holdt utenfor. De beskriver kundens kjonn, ikke arbeiderens.

# CounterfactMe 0.9.29

## Portability
- Alle norske tegn i R-strenger er escapet som `\u{00e6}` / `\u{00f8}` /
  `\u{00e5}` (208 tegn). Utskriften er uendret — dette er kun kildekode-
  representasjon, som kreves for portable pakker. Kommentarer er urort.
- `importFrom(stats, dnorm)` lagt til.
- Kolonnene `p_så_som`, `p_dårlig`, `p_meget_dårlig` i
  `self_rated_health.csv` er dopt om til `p_saa_som`, `p_daarlig`,
  `p_meget_daarlig`. Ikke-ASCII kolonnenavn kan mangles av `read.csv()` pa
  ikke-UTF-8 locale, som CI ofte kjorer.
- Feltet `bourdieu_økonomisk` heter na `bourdieu_okonomisk`, og
  listeelementet `økonomisk` fra `.cond_bourdieu()` heter `okonomisk`.
  Brytende endring, men pakken er ikke publisert enna.
- Lokal variabel `p_småhus` -> `p_smahus`.

# CounterfactMe 0.9.28

## Bug fixes
- `counterfact_me(conditional = FALSE)` feilet med "object 'age' not found":
  `.counterfact_independent()` kalte `.cond_parents_relationship()` med en
  udefinert variabel. Bruker nå `ego_age`.

## Documentation
- Dokumenterte `reject_impossible` og `max_reject_attempts` i `counterfact_me()`.
- Dokumenterte `num_predict` og `think` i `narrate_life_llm()`.
- Dokumenterte `print.counterfactme_narrative()`.

## Tests
- Rettet foreldet test som sendte `"gender"` som dimensjon (det er et
  parameter, ikke en dimensjon), og la til test for ukjente dimensjoner.
- Rettet foreldet test som forventet at `sample_municipality()` returnerer
  en data.frame; den returnerer en character-vektor, som dokumentert.

# CounterfactMe 0.9.27

## Preview-release
- Første publiserte versjon (offentlig GitHub-repo).
- Ny funksjon: `counterfact_me_constrained(givens)` — trekker et liv med
  bruker-spesifiserte constraints (age, gender, county, occupation, …).
- Ny funksjon: `counterfact_parallel_lives(givens, vary_dim, n)` — N
  parallelle liv som varierer én dimensjon.
- Ny funksjon: `narrate_life(x)` — template-basert biografi på norsk (ingen
  API, ingen nettverk).
- Ny funksjon: `narrate_life_llm(x)` + `life_factsheet(x)` +
  `ollama_available()` — valgfri lokal Ollama-narrasjon.
- Ny funksjon: `audit_plausibility()` — myk verify_consistency-utvidelse
  som rapporterer usannsynlige (men mulige) kombinasjoner.
- Ny funksjon: `find_impossibilities(x)` + rejection-lag i `counterfact_me()`
  — hard-umulige kombinasjoner filtreres bort før tilbakelevering.
- Import: `stats::` og `utils::` i NAMESPACE, `Imports:` i DESCRIPTION.

# CounterfactMe 0.7.0

## New features

- **Bourdieu kapitalprofil** dimension (`bourdieu`): tre indekser (økonomisk,
  kulturell, sosial — 0-100) og klasseposisjon (Etablert overklasse,
  Kulturell elite, Etablert middelklasse, Tradisjonell arbeiderklasse,
  Prekariat, m.fl.). Bygger på Bourdieu sin Distinction (1979) og norsk
  Bourdieu-tradisjon.
- **Inheritance flow**: når begge foreldre er døde, flyter `parents_capital`
  som arv til ego (delt med antall søsken). Norge har ikke arveavgift siden
  2014. Arven legges til `net_wealth_nok` og `financial_assets_nok`, og
  kan løfte ego inn i høyere `wealth_class`.

## Bug fixes

- `.cond_parents` brukte hardkodet kjønn ved kall til `.parent_occupation`,
  så samkjønnede foreldrepar fikk feil kjønnsbetinget yrke ("Andrius (M):
  Hjemmevaerende"). Fikset ved å trekke couple_type FØR yrkene tildeles.
- `.draw_boligtype` produserte enebolig for 22-åringer. Erstattet med
  geografi-bevisst aldersjustering: under 25 i Oslo/Akershus = kun blokk,
  under 25 i distrikt = enebolig mulig (arvet) men dempet sannsynlighet.
- `.cond_siblings` hadde sign-inversion i alder-delta (Arthur "4 aar yngre"
  enn Lukas 0 år = umulig). Fikset konvensjon og klipping mot ego_age.
- `.cond_nus_field` setter `detail_label` til NA når identisk med
  `field_of_study` (var "Allmenne fag == Allmenne fag" duplikat).

# CounterfactMe 0.6.0

## New features

- **Humoristiske labels** for religion og partistemming (~12 % rate;
  40 % for STEMTE_IKKE). 60 religion-varianter + 68 parti-varianter.

## Bug fixes

- Søsken og besteforeldre lagt til (`siblings`, `grandparents` dimensjoner).
- NEET-flag for 16-29-åringer (`neet` dimensjon).

# CounterfactMe 0.5.0

## New features

- **Same-sex couples**: kohort-betinget (~2.5 % for ego født etter 2010).
- **Mixed couples** (én norsk + én innvandret): 4-12 % avhengig av ego-bg.
- **Sexual orientation** dimension (~7-12 % LHBT+, kohort-betinget).
- **Religion** dimension (DnK 53 %, Katolsk, Islam, Humanistforbundet, etc.).
- **Party preference** dimension med valgdeltakelse (kalibrert mot SSB 2021).
- **Kommunenivå** boligpriser (urban/rural justering innen fylke).

# CounterfactMe 0.4.0

## New features

- **Innvandrerbakgrunn** dimension: majority/first_gen/second_gen, country,
  name_region, years_in_norway. Påvirker navn, geografi, inntekt, utdanning.
- **Historical immigration windows**: Afghanistan-89-åring umulig, etc.

# CounterfactMe 0.3.0

## New features

- **Bolig** dimension: type (enebolig/småhus/blokk), areal, verdi, gjeld,
  egenkapital, kjøpsår, hytte (fjell/innland/kyst/strandeiendom).
- **Formue** dimension: nettoformue (D1-D10 + topp 5/1/0.1 %),
  næringsformue, kapitalinntekt, wealth_class.
- **Foreldredimensjon v2**: kohortbetinget yrke, fødselsår + dødsår.

# CounterfactMe 0.2.0

## New features

- Conditional sampling: age → education → occupation → income.
- NUS-fagfelt og studieretning (broad + detailed).
- Husholdning matcher ego sitt kjønn.

# CounterfactMe 0.1.0

- Første versjon. Independent draws fra SSB-medianer.
