# CounterfactMe 0.9.42

## Tests
- The repository checks -- version numbers agreeing across files, README
  counts matching the code, SSB tables cited in the docs, tests that skip
  without counting -- are now opt-in behind `CFM_SOURCE_CHECKS`. They
  examine the source tree rather than the package, so they cannot run
  against an installed one, and they were the cause of the CI failures
  rather than anything wrong with the package.

  Run them with:

  ```r
  Sys.setenv(CFM_SOURCE_CHECKS = "true"); devtools::test()
  ```

- A separate `source-checks` job in CI runs them once per push, from the
  checkout, where they are meaningful.

# CounterfactMe 0.9.41

## Bug fixes
- Two tests read the package source through `../../R` and guarded that
  with `dir.exists("../../R")`. An installed package has an `R/`
  directory too, holding `.rdb` binaries rather than source, so the guard
  never fired under `R CMD check` or `covr`: `readLines()` parsed a
  database, found no table references, and the tests failed. Guards now
  require actual `.R` files, and reads are filtered to them.
- Split the SSB-citation test in two. `ssb_cited` entries claim the R
  source as their evidence; `ssb_api` entries claim a fetching script,
  whose table numbers may appear only in `data-raw/`, which
  `.Rbuildignore` strips from the built package. Checking both against
  `R/` conflated them and would have failed on 07459 regardless.

## CI
- The coverage workflow now prints `testthat.Rout.fail` and uploads it as
  an artifact when tests fail. The previous log reported only that
  `testthat.R` failed, without naming the expectation, so a CI failure
  could not be diagnosed from the log at all.

# CounterfactMe 0.9.40

## Tests
- Five tests could pass while checking nothing: they loop over draws with
  a `next`, and if the field they inspect ever went missing, every
  iteration would skip and the test would report green. Affected
  `very large fortunes do not overflow`, `wealth fields are numeric`,
  `no output string carries a gender morpheme`,
  `generational birth years are ordered` and
  `survey dimensions are suppressed below their age floor`. Each now
  counts what it inspected and asserts a floor.
- Removed an assertion that was true by construction
  (`expect_true(is.logical(seen_large))`).
- New `test-documentation.R`: version consistency across DESCRIPTION,
  CITATION.cff, inst/CITATION and README; README counts against the
  package; the provenance table against `SOURCES.csv`; no SSB table cited
  in the docs that the package cannot back; every export documented and
  defined; and no test that skips without counting coverage.

  These check the class of error that R CMD check passes over: a README
  describing an older version, a citation the package cannot support, a
  test that asserts nothing. They run only from a source checkout.

# CounterfactMe 0.9.39

## Documentation
- Rewrote the README. It still described version 0.7.0: 22 dimensions
  (now 32), 32 data files (now 50), 22 verification checks (now 44), and
  a roadmap listing v0.9 as future work. None of the functions added
  since -- constrained draws, parallel lives, narration, provenance --
  were mentioned.
- Corrected data-source claims in the README. It cited specific SSB
  tables for housing prices (06035, 07230), religious affiliation
  (06929) and party preference (Stortingsvalget 2021), all of which the
  provenance manifest records as `untraced`. It also claimed a complete
  table-ID list in the vignette, which does not exist. The section now
  reports what `data_sources()` actually shows.
- README and NEWS.md are now consistently English, matching DESCRIPTION.
  Norwegian sections added in 0.9.31 and 0.9.36 had left the file
  switching language mid-document, and one of them duplicated an
  existing English section on the same subject.

# CounterfactMe 0.9.38

## Metadata
- Added ORCID 0000-0001-9316-3279 to DESCRIPTION, CITATION.cff and
  inst/CITATION.

# CounterfactMe 0.9.37

## Fixes
- Corrected the GitHub username from `martinisungset` to `martinisu` in
  DESCRIPTION, the README badge, CITATION.cff, inst/CITATION and the
  package documentation. Every link pointed at a repository that does
  not exist.
- Removed a fabricated ORCID from CITATION.cff. An ORCID identifies one
  specific researcher, so a guessed number in a citation file could have
  attributed the work to someone else.

# CounterfactMe 0.9.36

## Data provenance
- New manifest `inst/extdata/SOURCES.csv` and new function
  `data_sources()` recording where each of the 50 data files comes from.
  Until now an estimate sat in the same directory as a register figure
  and looked exactly as authoritative.

  Of the 50 files: 9 are fetched from SSB's API by a script in
  `data-raw/` and can be reproduced, 5 cite an SSB table in the R source
  but were transcribed by hand, 3 are authored content, and 33 have
  neither a script nor a citation.

  `untraced` does not mean invented -- only that the origin has not been
  established. The figure may be an accurate transcription of a
  published table. Determining which is which remains to be done.

- Tests keep the manifest in step with the directory: a new CSV without
  provenance fails, a script named in the manifest must exist in
  `data-raw/`, and a cited table must actually appear in the source. The
  files carrying the package's central claim -- municipalities,
  counties, age structure, names, education, occupational earnings --
  are pinned to `ssb_api`.

# CounterfactMe 0.9.35

## Bug fixes
- `.cond_wealth()` crashed with "missing value where TRUE/FALSE needed"
  on the largest fortunes. NOK amounts in the top 0.1% exceed R's
  integer maximum (2,147,483,647), so `as.integer()` produced `NA` with
  a warning and the next line tested `residual < 0` on that `NA`. Wealth
  is now held as `double`. This also accounts for over a thousand
  warnings in the test run. The draw is rare enough that it took several
  hundred lives to surface.
- Field of study for pre-1980 cohorts: the cohort multiplier of 0.05
  could only shift weight onto something else, and STYRK 35 (ICT
  technicians) has no non-modern candidate at all while STYRK 25 is 90%
  modern. A 78-year-old therefore still drew an ICT degree 20% of the
  time. When every candidate is modern the detailed field is now left
  open; the broad field still shows.

## Internals
- `.modern_nus_codes` extracted as a documented constant (the code had
  `"489"` listed twice).

# CounterfactMe 0.9.34

## Tests
- New regression tests (`test-regressions.R`) for fixes that had no
  guard: cohort anachronism in field of study, occupation against
  education, name against background, years-in-Norway against age, and
  sibling birth years. Each of these fails silently -- the result is a
  plausible value, not an error.
- New contract tests (`test-api-contracts.R`) for
  `counterfact_me_constrained()`, `counterfact_parallel_lives()`,
  `narrate_life()` and `life_factsheet()`, none of which had any tests.
- Tests that skip draws now assert coverage, so they cannot pass while
  checking nothing.

# CounterfactMe 0.9.33

## Tests
- Fixed the age-floor test, which reported 240 failures against correct
  code. `has_chronic` and `has_disability` are `FALSE` for children
  rather than `NA`, and the predicate `is.null(v) || all(is.na(v))`
  counted `FALSE` as present.
- Introduced a single shared `.absent()`/`.present()` predicate. Absence
  has three shapes here -- `NULL`, `NA` and `FALSE` -- and rewriting the
  check for each test is what produced the error.

# CounterfactMe 0.9.32

## Data provenance
- Removed the `0-15` rows from `self_rated_health.csv`,
  `social_isolation.csv`, `tv_hours.csv`, `sleep_hours.csv` and
  `chronic_illness_prob.csv`. None had provenance: no script fetches
  them, and the values were round numbers summing to exactly 1. They
  existed because an age-band lookup needed something to find.
- Likewise for `.cond_disability()`, which carried a hardcoded child
  probability of 0.06 with hardcoded type weights.
  `disability_by_age.csv` does not cover children -- it measures
  disability benefit rather than functional impairment and starts at
  18-24.
- `sleep_hours`, `media_tv_hours`, `has_chronic` and `has_disability`
  are now suppressed below 16, alongside the survey fields gated in
  0.9.31.

  Children keep only what is either SSB-derived (name, age, geography,
  family, household, religion) or plainly authored (the child variants
  of occupation, income and diet). Nothing in between.

## Robustness
- A band lookup that finds no row now returns `NA` instead of falling
  back to the table's first row. The old fallback would have handed a
  child the 16-24 values without any error.

# CounterfactMe 0.9.31

## Validity
- Survey-based dimensions are now suppressed below the survey's own
  lower age limit. A four-year-old previously drew a generalized-trust
  score, a podcast habit and a favourite newspaper -- the last
  conditioned on the party the child "votes" for.
  Levekårsundersøkelsen surveys ages 16 and up, so these were not
  approximations but invented observations carrying the same authority
  as the register figures.

  Floors (`.dimension_min_age`): generalized trust, loneliness, close
  friends, confidant and self-rated health from 16; newspaper, podcast
  and social media from 13.

  Deliberately not gated at this stage: chronic illness and disability,
  and TV time, sleep and diet, which had their own 0-15 bands. Alcohol
  was already gated at 16.

# CounterfactMe 0.9.30

## Breaking
- The default language of `counterfact_me()` changed from `"en"` to
  `"no"`. The rest of the package already defaulted to Norwegian, so
  this makes it consistent. English remains available but is partial:
  occupation titles come from the Norwegian-only STYRK-98 register.

## Bug fixes
- Gendered occupation titles were assigned without regard to the ego's
  gender, so a woman could be given "Fosterfar". Gender weighting lives
  at the four-digit STYRK-08 level, but FOSTERMOR and FOSTERFAR share
  group 5311, leaving the choice between them blind.
  `.draw_detail_yrke()` now filters on gender for six such pairs.

  Note that DAMEFRISØR/HERREFRISØR and DAMESKREDDER/HERRESKREDDER are
  deliberately excluded: they describe the customer's gender, not the
  worker's.

# CounterfactMe 0.9.29

## Portability
- All Norwegian characters in R string literals are escaped as
  `\u{00e6}` / `\u{00f8}` / `\u{00e5}` (208 characters). Output is
  unchanged -- this is source representation only, required for portable
  packages. Comments are untouched.
- Added `importFrom(stats, dnorm)`.
- Renamed the columns `p_så_som`, `p_dårlig` and `p_meget_dårlig` in
  `self_rated_health.csv` to `p_saa_som`, `p_daarlig` and
  `p_meget_daarlig`. Non-ASCII column names can be mangled by
  `read.csv()` on non-UTF-8 locales, which CI often runs.
- The field `bourdieu_økonomisk` is now `bourdieu_okonomisk`, and the
  list element `økonomisk` from `.cond_bourdieu()` is `okonomisk`.
- Local variable `p_småhus` renamed to `p_smahus`.

# CounterfactMe 0.9.28

## Bug fixes
- `counterfact_me(conditional = FALSE)` failed with "object 'age' not
  found": `.counterfact_independent()` called
  `.cond_parents_relationship()` with an undefined variable.

## Documentation
- Documented `reject_impossible` and `max_reject_attempts` in
  `counterfact_me()`, `num_predict` and `think` in `narrate_life_llm()`,
  and `print.counterfactme_narrative()`.

## Tests
- Corrected a stale test passing `"gender"` as a dimension (it is a
  parameter), and one expecting `sample_municipality()` to return a data
  frame; it returns a character vector, as documented.

# CounterfactMe 0.9.27

## Preview release
- First published version.
- `counterfact_me_constrained(givens)` draws a life honouring
  user-specified constraints.
- `counterfact_parallel_lives(givens, vary_dim, n)` produces N lives
  varying one dimension.
- `narrate_life(x)` writes a template-based Norwegian biography, with no
  API and no network.
- `narrate_life_llm(x)`, `life_factsheet(x)` and `ollama_available()`
  provide optional local Ollama narration.
- `audit_plausibility()` reports combinations that are possible but
  implausibly frequent.
- `find_impossibilities(x)` plus a rejection layer in `counterfact_me()`
  filter out hard impossibilities before a life is returned.

# CounterfactMe 0.7.0

## New features

- **Bourdieu capital profile** (`bourdieu`): three indices (economic,
  cultural, social -- 0-100) and a class position (established upper
  class, cultural elite, established middle class, traditional working
  class, precariat, and others). Built on Bourdieu's *Distinction* (1979)
  and the Norwegian Bourdieu tradition.
- **Inheritance flow**: when both parents have died, `parents_capital`
  flows to the ego as inheritance, split by the number of siblings.
  Norway has had no inheritance tax since 2014. The inheritance is added
  to `net_wealth_nok` and `financial_assets_nok`, and can lift the ego
  into a higher `wealth_class`.

## Bug fixes

- `.cond_parents` passed a hardcoded gender to `.parent_occupation`, so
  same-sex parent couples got the wrong gender-conditioned occupation
  ("Andrius (M): Hjemmevaerende"). Fixed by drawing couple_type before
  assigning occupations.
- `.draw_boligtype` produced detached houses for 22-year-olds. Replaced
  with a geography-aware age adjustment: under 25 in Oslo/Akershus draws
  flats only; under 25 in rural areas can inherit a house, at a damped
  probability.
- `.cond_siblings` had a sign inversion in the age delta (Arthur "4
  years younger" than Lukas at age 0). Fixed the convention and clipped
  against the ego's age.
- `.cond_nus_field` sets `detail_label` to NA when identical to
  `field_of_study` (the "Allmenne fag == Allmenne fag" duplicate).

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
