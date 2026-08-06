# CounterfactMe

> *What if you had been someone else?*

<!-- badges: start -->
[![R-CMD-check](https://github.com/martinisungset/CounterfactMe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/martinisungset/CounterfactMe/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**CounterfactMe** generates random but plausible counterfactual Norwegian lives
from open data published by Statistics Norway (SSB) and other public sources.
Each call to `counterfact_me()` returns a coherent life — name, age, education,
occupation, income, family, housing, wealth, religion, party preference, and
more — sampled from real population distributions and conditional on each
other in sociologically realistic ways.

## Språk

Standard er norsk. `lang = "en"` oversetter rammeverket, men er delvis med
vilje: yrkestitlene kommer fra SSBs STYRK-98-register, som bare finnes på
norsk, så yrker (og noen humoristiske merkelapper) forblir norske.

```r
counterfact_me()              # norsk
counterfact_me(lang = "en")   # engelsk ramme, norske yrker
```

## Installation

```r
# From a local source build
install.packages("CounterfactMe_0.9.35.tar.gz", repos = NULL, type = "source")

# From GitHub
remotes::install_github("martinisungset/CounterfactMe")
```

## Quick start

```r
library(CounterfactMe)

# A full counterfactual life
counterfact_me(lang = "no")
#> --- Ditt kontrafaktiske liv ---
#>   Navn: Aisha
#>   Kjonn: Kvinne
#>   Alder: 34
#>   Bakgrunn: Norskfodt med foreldre fra Pakistan
#>   Kommune: Drammen (Buskerud)
#>   Yrke: Sykepleiere
#>   Utdanning: Universitets- og høgskoleutdanning kort
#>   Inntekt: 540 000 kr  (D6 - over medianen)
#>   ...

# Reproducible
set.seed(1814)
counterfact_me()

# Pick specific dimensions
counterfact_me(dimensions = c("name", "age", "occupation", "wealth"))

# Run consistency verification across many draws
verify_consistency(N = 5000)
```

## Available dimensions

The package ships with **22 dimensions** that can be drawn jointly or
selectively. Most are conditioned on each other (age → education → occupation
→ income; ego background → parent names → family structure; etc.).

| Category | Dimensions |
|---|---|
| Identity | `name`, `age`, `gender`, `municipality` |
| Education & work | `education`, `field_of_study`, `field_of_study_detail`, `occupation`, `income` |
| Family | `parents`, `siblings`, `grandparents`, `marital_status`, `household` |
| Housing & wealth | `housing` (incl. hytte), `wealth` (incl. inheritance) |
| Identity & politics | `background` (immigrant), `orientation`, `religion`, `party` |
| Status flags | `neet` |
| Social position | `bourdieu` (calculated profile) |

## Data sources

The 32 CSV files in `inst/extdata/` are derived from open Norwegian data:

- **SSB (Statistics Norway)** — population, income, occupations (STYRK-08),
  education (NUS), housing prices (06035, 07230), wealth distribution
  (10318, 08589), immigrant background (09817), religious affiliation
  (06929), party preference (Stortingsvalget 2021), and more.
- **Eurostat** — for cross-validation of education-occupation mappings.
- **Bufdir** — LGBT+ statistics.
- **Norwegian valgforskning** — party preference modeling.

A complete list with table IDs is available in the package vignette.

## Sociological grounding

The model is built around three principles:

1. **Distributions, not predictions.** Every dimension is drawn from real
   population distributions, conditional on plausibility constraints. Two
   ego with the same age + background can land anywhere in the joint
   distribution — the simulator reproduces aggregate inequality, not
   individual prediction.

2. **Conditional realism.** Dimensions are drawn in a sociologically
   meaningful order (age → background → name → education → occupation →
   income → family → housing → wealth → identity markers → Bourdieu profile).
   Each subsequent draw is constrained by what came before.

3. **Honest about limits.** When SSB data is unavailable (e.g., joint
   age × edu × income tables), parameters are stylized but documented in
   the source. Calibration error against SSB benchmarks is kept under 5pp
   on all marginal distributions.

## Verification

`verify_consistency(N = 5000)` runs 22 systematic checks across the joint
distribution and reports any cross-dimensional inconsistencies (e.g.,
married 14-year-old, retiree-aged grandparents born after their kids,
Polish-origin ego with Norwegian first name, NEET person with active
occupation). Use it to quickly diagnose changes in the package.

## Status

| Version | Highlight |
|---|---|
| **0.7.0** | Bourdieu kapitalprofil + arveoppgjør |
| 0.6.x | Søsken, besteforeldre, NEET, humoristiske labels |
| 0.5.x | Religion, parti, samkjønnede par, sexual orientation |
| 0.4.x | Innvandrerbakgrunn med navn, utdanning, yrke per region |
| 0.3.x | Bolig + formue (inkl. fjellhytte og strandeiendom) |
| 0.2.x | Conditional sampling chain |
| 0.1.x | Independent draws fra SSB-medianer |

See [NEWS.md](NEWS.md) for full changelog.

## Roadmap

- **v0.8** — Pakkehygiene + utvidet test-suite + vignette
- **v0.9** — Kriminalitet (offer-/utsatthet-statistikk), Giddens Tier 3
- **v1.0** — Shiny app + CRAN-utgivelse
- **Senere** — LLM-narrert biografi som podcast (separat pakke)

## License

MIT.

## Citation

If you use CounterfactMe in research or writing, please cite:

> Isungset, M. A. (2026). *CounterfactMe: Counterfactual Norwegian Lives
> from SSB Open Data*. R package version 0.7.0.
> https://github.com/martinisungset/CounterfactMe
