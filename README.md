# CounterfactMe

> *What if you had been someone else?*

<!-- badges: start -->
[![R-CMD-check](https://github.com/martinisu/CounterfactMe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/martinisu/CounterfactMe/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**CounterfactMe** draws random but plausible alternate Norwegian lives from
open data. Each call to `counterfact_me()` returns one coherent life across
32 dimensions — name, age, education, occupation, income, family, housing,
wealth, religion, party, health, habits — sampled from population
distributions and conditioned on each other in a sociologically meaningful
order.

The dimensions are not drawn independently. Age constrains education,
education constrains occupation, occupation and geography shape income,
income and parental capital shape housing, and so on down the chain. The
point is a joint distribution that holds together, not a row of unrelated
random numbers.

## Installation

```r
# install.packages("remotes")
remotes::install_github("martinisu/CounterfactMe")
```

## Quick start

```r
library(CounterfactMe)
set.seed(42)

x <- counterfact_me()
print(x)
#> --- Ditt kontrafaktiske liv ---
#>   Navn: Aisha
#>   Kjønn: Kvinne
#>   Alder: 34
#>   Bakgrunn: Norskfødt med foreldre fra Pakistan
#>   Kommune: Drammen (Buskerud)
#>   Yrke: Sykepleier
#>   Utdanning: Universitets- og høgskoleutdanning kort
#>   Inntekt: 540 000 kr  (Desil 6 — over medianen)
#>   ...
```

## What you can do with it

**Draw a life with some things fixed.** Give it what you know and it fills in
the rest, conditionally:

```r
counterfact_me_constrained(list(age = 42, gender = "M", county = "Oslo"))
```

**Vary one dimension, hold the rest.** This is where the conditioning becomes
visible: change the county and watch party, newspaper, housing and hobbies
move with it.

```r
lives <- counterfact_parallel_lives(
  givens   = list(age = 40, gender = "F"),
  vary_dim = "county",
  n        = 5
)
print(lives)
```

`vary_dim` also accepts `"occupation"`, `"education"`, `"background"`,
`"religion"` and `"party"`.

**Turn a life into prose.** Template-based, no LLM and no network:

```r
cat(narrate_life(x))                     # a few paragraphs
cat(narrate_life(x, style = "compact"))  # one
```

An optional local-Ollama variant exists (`narrate_life_llm()`), but nothing
in the package requires an API key or sends data anywhere.

## Dimensions

Thirty-two, drawn jointly or in any subset (`available_dimensions()`):

| Category | Dimensions |
|---|---|
| Identity | `name`, `age`, `municipality`, `sentralitet`, `background` |
| Education & work | `education`, `field_of_study`, `field_of_study_detail`, `occupation`, `income`, `neet` |
| Family | `parents`, `siblings`, `grandparents`, `children`, `marital_status`, `household` |
| Housing & wealth | `housing` (incl. hytte), `wealth` (incl. inheritance), `deprivation` |
| Beliefs & politics | `religion`, `party`, `orientation` |
| Health & social | `health`, `isolation`, `crime` |
| Everyday life | `hobbies`, `media`, `sleep`, `diet`, `alcohol` |
| Derived | `bourdieu` (economic, cultural and social capital) |

Some dimensions are suppressed below the age at which the underlying survey
asks. A four-year-old has no generalized-trust score, because
Levekårsundersøkelsen does not survey children and inventing one would put a
fabricated number next to register figures.

## Data provenance

Fifty CSV files ship in `inst/extdata/`. They are not equally well traced,
and the package says which is which:

```r
data_sources()             # all 50 files
data_sources("untraced")   # those without a recorded origin
table(data_sources()$status)
```

| status | meaning | files |
|---|---|---|
| `ssb_api` | Fetched from Statistics Norway's API by a script in `data-raw/`. Reproducible. | 9 |
| `ssb_cited` | An SSB table is cited in the R source; figures transcribed by hand. | 5 |
| `untraced` | No script and no citation found in the package. | 33 |
| `authored` | Deliberately written content, such as the humorous labels. | 3 |

`untraced` does not mean invented. It means the origin has not been
established: the figure may be an accurate transcription of a published
table, or it may be an estimate. Establishing which is ongoing work, and the
manifest is what makes it possible to do incrementally rather than all at
once.

The files carrying the package's central claim — municipalities, counties,
age structure, names, education by age, occupational earnings — are all
`ssb_api`, and a test pins them there.

## Language

Output is Norwegian by default. `lang = "en"` translates the frame but is
partial by design: occupation titles come from Statistics Norway's STYRK-98
register, which exists only in Norwegian, so occupations and a few humorous
labels stay Norwegian whatever you pass.

```r
counterfact_me()             # Norwegian
counterfact_me(lang = "en")  # English frame, Norwegian occupations
```

## Sociological grounding

1. **Distributions, not predictions.** Every dimension is drawn from a
   population distribution subject to plausibility constraints. Two egos with
   the same age and background can land anywhere in the joint distribution.
   The simulator reproduces aggregate inequality; it does not predict
   individuals.

2. **Conditional realism.** Dimensions are drawn in a meaningful order, each
   constrained by what came before.

3. **Impossible versus improbable.** Only impossibilities are blocked. A
   married 14-year-old cannot occur; a farmer in Oslo can, and does, because
   Sørkedalen exists. Improbable combinations are the point rather than a
   defect.

## Verification

```r
find_impossibilities(x)        # hard contradictions in one life (empty = clean)
verify_consistency(N = 5000)   # 44 cross-dimensional checks over many draws
audit_plausibility(N = 5000)   # combinations that are possible but too frequent
```

`counterfact_me()` already rejects and redraws lives containing hard
impossibilities, so `find_impossibilities()` is normally empty; it exists to
catch regressions. The package's own test suite runs 64 tests, including property-based
invariants and regression guards for previously fixed bugs. A further
set of repository checks -- version numbers agreeing across files,
README counts matching the code, no undocumented export -- lives in
`dev/repo-tests/` and runs with `Rscript dev/run-repo-tests.R`.

## Status

Pre-release. The API is settling but not frozen, and the provenance work
above is unfinished. See [NEWS.md](NEWS.md) for the changelog.

Planned: a Shiny front-end, and a narrative layer aimed at readers rather
than R users.

## License

MIT. See [LICENSE.md](LICENSE.md).

## Citation

```r
citation("CounterfactMe")
```

> Isungset, M. A. (2026). *CounterfactMe: Generate Random Counterfactual
> Lives Based on Norwegian Open Data*. R package version 0.9.46.
> https://github.com/martinisu/CounterfactMe
