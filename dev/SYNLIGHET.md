# Synlighet for AlternaLiv — det du må gjøre selv

Filsiden er gjort (metadata, delingskort, Om-tekst, ikon, sitemap, README).
Dette er stegene som krever din innlogging. Rekkefølgen er etter effekt per minutt.

---

## 1. GitHub «About» — 2 min

Repo-siden → tannhjulet ved «About» øverst til høyre.

**Description:**

    AlternaLiv – hvem kunne du ha vært? Tilfeldige, realistiske norske liv fra SSB-data. R-pakke som kjører i nettleseren.

**Website:** `https://martinisu.github.io/CounterfactMe/`

**Topics** (skriv ett om gangen, Enter mellom):

    r  r-package  webr  sociology  demography  inequality  simulation
    counterfactual  norway  ssb  statistics-norway  open-data  social-mobility

Kryss av for «Use your GitHub Pages website».

---

## 2. Google Search Console — 10 min, deretter vente

1. Gå til https://search.google.com/search-console
2. «Legg til eiendom» → velg **URL-prefiks** (ikke «Domene») →
   `https://martinisu.github.io/CounterfactMe/`
3. Bekreftelsesmetode: **HTML-tag**. Kopier taggen
   (`<meta name="google-site-verification" content="...">`) og send den til meg.
   Jeg legger den i `web/index.html` og publiserer; du committer og pusher.
4. Når Pages har oppdatert seg (1–2 min etter push): trykk «Bekreft».
5. Venstremeny → **Nettstedskart** → skriv `sitemap.xml` → Send inn.
6. Øverst: lim inn adressen i «Inspiser URL» → **Be om indeksering**.

Regn med dager til uker før siden dukker opp i søk.
Search Console viser deretter hvilke søk folk faktisk finner den med.

## 3. Bing Webmaster Tools — 3 min

https://www.bing.com/webmasters → «Importer fra Google Search Console».
Bing driver også DuckDuckGo, Ecosia og søket i ChatGPT og Copilot.

---

## 4. Lenker — det som faktisk flytter noe

Google finner og rangerer sider etter hvem som lenker til dem. Én lenke fra
uio.no veier mer enn alt det tekniske over til sammen.

- **UiO-personprofilen:** legg AlternaLiv under prosjekter eller lenker.
- **ORCID:** «Websites & social links» → `AlternaLiv` → adressen.
- **LinkedIn / Bluesky / Facebook:** del lenka. Delingskortet med «Hvem kunne
  du ha vært?» kommer opp av seg selv nå. Test først i
  https://www.opengraph.xyz eller LinkedIns Post Inspector.
- **Titan.uio.no** eller **forskning.no:** en kort sak om verktøyet. Det er
  folkeopplysning med et klikkbart verktøy, og passer godt for begge.
- **Undervisning:** bruk det i et sosiologikurs. Studenter deler.
- **Norsk sosiologforening** (nyhetsbrev), **r/norge**, eventuelt en kronikk
  om ulikhet med lenke.
- For R-folk: **R Weekly** (rweekly.org, «Submit»).

---

## 5. r-universe — 10 min (gjør pakken søkbar for R-brukere)

1. Lag et nytt offentlig repo som heter nøyaktig `martinisu.r-universe.dev`.
2. Legg inn én fil, `packages.json`:

```json
[
  { "package": "CounterfactMe", "url": "https://github.com/martinisu/CounterfactMe" }
]
```

3. Installer r-universe-appen på GitHub-kontoen:
   https://github.com/apps/r-universe/installations/new
4. Etter en times tid: `https://martinisu.r-universe.dev`, og pakken kan
   installeres med
   `install.packages("CounterfactMe", repos = "https://martinisu.r-universe.dev")`.

## 6. Zenodo-DOI — 10 min (siterbar, havner i Google Scholar)

1. https://zenodo.org → logg inn med GitHub.
2. Profil → GitHub → slå på `martinisu/CounterfactMe`.
3. På GitHub: Releases → «Draft a new release» → tag `v0.9.73` → Publish.
4. Zenodo lager en DOI automatisk. Send den til meg, så legger jeg
   DOI-merke i README og DOI i CITATION.cff.

---

## Valgfritt: eget domene

`martinisu.github.io/CounterfactMe` sier verken «AlternaLiv» eller «norsk».
Et eget domene (f.eks. `alternaliv.no`, om det er ledig — sjekk hos
en .no-registrar, ca. 100–200 kr/år) gjør adressen lett å huske og si
høyt i et foredrag eller en avisartikkel. GitHub Pages støtter det gratis:
domenet pekes til GitHub, og jeg legger inn en `CNAME`-fil og bytter
canonical-adressen. Best å bestemme før punkt 2, så Google ikke må lære
seg to adresser.

## Senere, når det trengs

Siden oversetter fortsatt JSX i nettleseren (Babel) og laster React i
utviklingsmodus. Det gjør første lasting tregere. Det påvirker rangering
litt, men brukeropplevelsen mer. Det kan fikses ved å forhåndskompilere
`app.jsx` i `publish.py`.
