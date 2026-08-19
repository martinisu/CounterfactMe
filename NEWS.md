# CounterfactMe 0.9.58

## Housing before 1992
- Purchase years were floored at 1992, where SSB's house price index
  (07230) begins, so a 93-year-old owner bought at 59 or later. New
  `housing_index_prewar.csv` reaches back to 1920 and the floor moves
  with it.
- The file has two components, deliberately kept apart. `cpi_index` is
  the consumer price index from SSB 08981, fetched by
  `data-raw/fetch_cpi.py` and reproducible. `real_factor` is an estimate
  of the real house price level, because Norway has no official index
  before 1992: flat-to-falling through the war, slow postwar growth, the
  deregulation boom peaking around 1987, the banking-crisis trough in
  1992.
- Deflating by CPI alone would have been wrong in a knowable direction.
  A home worth 5 MNOK today comes out around 123,000 kroner in 1965 with
  the real adjustment and 367,000 without; the 1965 figure was nearer
  100,000.

# CounterfactMe 0.9.57

## Bug fixes
Two more instances of the fault behind the parent-education bug in
0.9.56: a distribution measured on today's population applied to someone
who lived in a different Norway. Found by going through every `.cond_*`
function and asking what it does with the birth year.

- **Parent occupation.** `.cond_occupation()` weights STYRK major groups
  by today's headcounts, which is right for the ego and wrong for a
  parent who worked in 1955. Norway had roughly a quarter of employment
  in primary industry in 1950 and has about 2 % now. A parent born in
  1905 was drawing from the 2020s labour market: 1.0 % primary industry
  and 26.4 % professional occupations. With `.styrk_cohort_mult()`
  applied at the midpoint of the parent's working life, that becomes
  8.8 % and 7.2 %.

- **Religion by cohort.** Membership of the Church of Norway was close to
  universal before the war. The adjustment was `x1.2` against a ~50 %
  baseline, giving 60 % -- a 93-year-old came out looking like a 2020s
  Norwegian. Pre-1940 cohorts now reach 97.6 %, and 1940-54 about 92 %,
  against 48.6 % for those born in 2005.

## Known limitation
- `.draw_purchase_year()` floors purchase years at 1992 because
  `housing_price_index.csv` starts there. A 93-year-old owner therefore
  bought at 59 or later. Fixing it needs either pre-1992 index data or a
  decision to report the purchase price as unknown for older owners.

# CounterfactMe 0.9.56

## Bug fixes
- Parent education ignored the parent's cohort. A 93-year-old was given
  two parents with master's degrees; born in 1933, her parents arrived
  around 1905, when a few per cent of Norwegians had any higher
  education at all.

  `education_by_age.csv` tops out at "67 aar eller eldre", which
  describes everyone alive today above that age -- mostly born 1940-1958,
  27.5 % of them holding a degree. That band was applied to every parent
  regardless of birth year, and the inheritance path, which copies the
  ego's level with jitter 40 % of the time, ignored the cohort entirely.

  `.demote_edu_for_cohort()` now scales tertiary attainment against the
  birth cohort, with long degrees cut harder than short ones. Simulated
  against the shipped data, parents of a 93-year-old go from 27.5 % to
  2.6 % with any tertiary education and 7.3 % to 0.5 % with a long
  degree; parents of a 30-year-old stay near 24 %.

# CounterfactMe 0.9.55

## Bug fixes
- Constraint matching in `counterfact_me_constrained()` was
  case-sensitive despite intending otherwise. `.matches_givens()` called
  `grepl(fixed = TRUE, ignore.case = TRUE)`, and those two cannot be
  combined: R warns and drops `ignore.case`. A constraint of
  `occupation = "sykepleier"` therefore never matched "Sykepleier", the
  rejection sampler exhausted its 300 attempts, and the function fell
  back to direct override -- silently returning a life that did not meet
  the constraint.

  Both sides are now folded to lower case and the literal match kept.

  This was also the source of the warning volume: one warning per
  comparison, 20,508 from twenty `counterfact_parallel_lives()` calls,
  and the 1,562 counted in the test suite.

# CounterfactMe 0.9.54

## Tests
- Four tests that filter draws by country of origin had sample sizes
  guessed rather than derived, and two of them failed. The five refugee
  origins are 14.7 % of immigrants and immigrants 17.5 % of the
  population, so a draw yields one about 2.6 % of the time: 600 draws
  give ~15 expected against a threshold of 15, which is a coin flip
  rather than a test. Draw counts are now set from the base rate, with
  at least a threefold margin over the threshold, and the arithmetic is
  recorded next to each.

# CounterfactMe 0.9.53

## Bug fixes
- Arrival age is now drawn first and residence length derived from it,
  rather than the other way round. Drawing residence from the country's
  peak year and checking arrival age afterwards meant the check could
  only clamp: Pakistani first-generation immigrants piled up on the
  floor of the window at exactly 15, with 55 % arriving as children.
- The flow type moved from the region to the country, in a new
  `arrival_profile` column in `immigration_start_year.csv`.
  `mena_sor_asia` holds both 1960s labour migration (Pakistan, Turkey,
  Morocco) and refugee movements (Syria, Iraq, Afghanistan), so no
  single regional rule fits it.

    * `labour` -- young adults, a fifth arriving as accompanied children
    * `mixed` -- labour wave first, then family reunification, which
      brought spouses and teenagers but few small children
    * `refugee` -- whole families, infants to middle age

  Simulated against the shipped data, median arrival age is now 22-30
  across all origins, against 11-15 for Pakistan, Turkey, Morocco,
  Vietnam, Iran and Chile before.

- `peak_year` no longer shapes the draw. The hard constraint that
  matters -- nobody can have lived here longer than the flow has existed
  -- is carried by `max_botid`. The column is kept in the data.

# CounterfactMe 0.9.52

## Bug fixes
- Residence length for first-generation immigrants was centred on the
  country's peak migration year without checking how old the person
  would have been on arrival. Lithuania peaks in 2010, so a 65-year-old
  was given about 16 years of residence -- an arrival at 49. Baltic and
  Polish migration to Norway is labour migration, concentrated at 18-40;
  refugee and family flows span a wider range and include children.
  `.cond_immigrant_background()` now derives a plausible arrival-age
  window per region and clips residence length to it.
- Tightened the qualifying filter from an arrival age of 60 to 45.

# CounterfactMe 0.9.51

## Bug fixes
- Retirees were narrated as though they were in work: an 89-year-old
  "tok veien til jobben som hyttebokforfatter og har i dag en
  arsinntekt pa ...". The labels in `.PENSJONIST_LABELS` are pastimes,
  not jobs, and the income is a pension. `.narrate_education_career()`
  and the compact variant now take a retiree branch, keyed on age 67+ or
  on the label itself, so someone under 67 who draws a pastime label is
  covered too.

# CounterfactMe 0.9.50

## Tests
- The repository check that guards against tests skipping without
  counting required the counter to be named `checked` or `inspected`.
  That is a naming convention, not the property in question, and it
  failed a test in 0.9.49 which counted correctly in a variable called
  `seen`. It now matches the assertion -- `expect_gt()` / `expect_gte()`
  -- rather than the variable name.

  All five R CMD check platforms passed on 0.9.49; only this check
  failed, and on its own rule rather than on the package.

# CounterfactMe 0.9.49

## Bug fixes
- Religion is now conditioned on the country of origin when it is known,
  with the regional distribution as fallback.

  `religion_by_region.csv` groups Morocco to Nepal in one `mena_sor_asia`
  row, so its 8 % Hindu share -- which comes from India, Nepal and Sri
  Lanka -- was applied to Lebanon, Syria, Iraq and the rest. `afrika_sub`
  carries 55 % Islam, which made Ethiopia, Eritrea, Kenya, Uganda,
  Ghana, Rwanda and Angola Muslim-majority; all of them are
  Christian-majority. Algeria sits in that region too, and is not
  sub-Saharan at all.

  New `religion_by_country.csv` covers the 35 countries in those two
  regions, and `.religion_country_weights()` in `conditional.R` reads it.
  `.cond_religion()` takes a `country_label` argument and
  `counterfact_me.R` passes `bg$country_label`.

  The region taxonomy is deliberately untouched: `name_region` is the key
  into `names_by_region.csv` and drives the income, occupation and
  turnout adjustments. Only religion is conditioned on the country.

  The new figures are approximations of national religious composition,
  lightly adjusted for a Norwegian migrant context. They are recorded as
  `untraced` in the provenance manifest, because no single source was
  written down.

## Tests
- The README's data-file count is now derived in the repository check
  rather than hardcoded; the literal 50 went stale the moment a file was
  added.

# CounterfactMe 0.9.48

## Tests
- Both failures in 0.9.47 were in the new tests, not the code.
  - The pronoun check flagged `han|hun|ham|henne` alike, but only the
    subject forms are the fault being guarded against. "endte det med
    videregaende for henne" is correct -- object case after a
    preposition -- and those sentences appear at all only because
    `.mobility()` now works.
  - The abroad-marker check searched for "en bodde i", which also
    matches the corrected sentence "Den ene forelder**en bodde i**
    Serbia". It now matches the marker only where a job title was
    expected, and additionally asserts the country keeps its capital.

# CounterfactMe 0.9.47

## Bug fixes (narration)
- V2 inversion in `.narrate_cultural()`. The party sentence was built as
  "Politisk" + a ready-made verb phrase + the pronoun, giving
  "Politisk stemmer Sosialistisk Venstreparti hun." Norwegian puts the
  finite verb second and the subject straight after it. The conjoined
  form was worse: "..., og religiost tilhorer Den norske kirke" had no
  subject at all. The party clause is now the object alone, so the
  pronoun can be placed where it belongs.
- Register education terms appeared verbatim in prose. New `.edu_prose()`
  renders "Universitets- og hogskoleutdanning kort/lang" as
  bachelorgrad/mastergrad, and "Forskerutdanning" as doktorgrad, in
  narration only. It maps label to code to prose, reusing
  `.edu_level_label_no()` so there is one vocabulary rather than two.
  `education_levels.csv` and every other output are untouched.
- A parent who lived their working life abroad carries the marker
  "Bodde i <land>" in the occupation field. Narration treated it as a job
  title and lowercased it: "Hjemme var det en bodde i serbia og en bodde
  i serbia som forsorget familien." The marker is now recognised, the
  country keeps its capital, and the repeated case is said once.
- `.mobility()` read `x$edu_code`, `x$mother_edu_code` and
  `x$father_edu_code`. None of those fields exist -- the ego carries only
  the education label, and the parents' codes are nested. It therefore
  always returned NULL, and the educational-mobility strand of the
  biography ("Der foreldrene stoppet ved videregaende, gikk hun videre
  til mastergrad") had never appeared in any output. Removed the now
  unused `.edu_level()`, which was what received the missing fields.

# CounterfactMe 0.9.46

## Bug fixes
- A person could be given "no close friends" and "has a confidant" at
  once, with nothing to explain it. The combination is real -- for most
  people the confidant is a partner, a sibling or an adult child, which
  is why Levekar asks the two questions separately -- but it was drawn
  at a flat 35% whether or not anyone else lived in the household.
  `.cond_social_support()` accepted a `household` argument and never
  read it.

  With no close friends, a confidant is now likely for someone living
  with others (they have one at hand) and rare for someone living alone.

  The first fix enumerated the cohabiting household types and missed two
  of them; the test now checks only for living alone, which is the one
  unambiguous case.

# CounterfactMe 0.9.45

## Fixes
- DESCRIPTION claimed 33 dimensions; there are 32. The figure was wrong
  from 0.9.27 and went unnoticed because the repository check read only
  the Version field from DESCRIPTION, never its prose. It is the first
  sentence anyone reads, on GitHub and on CRAN.
- The repository checks now compare DESCRIPTION's dimension count against
  `available_dimensions()`.

# CounterfactMe 0.9.44

## Bug fixes
- Three faults in the repository checks, all introduced by me and all
  now visible because the checks finally ran somewhere they could report:
  - The test-count and meta-tests looked for `../testthat`, which was
    right when the file lived in `tests/testthat/` and wrong once it
    moved to `dev/repo-tests/`. They found no files and concluded the
    README should claim zero tests.
  - `nzchar(NA)` returns `TRUE`, so entries with an empty `ssb_table` --
    `material_deprivation.csv`, `styrk98_to_styrk08.csv`,
    `occupations.csv` -- survived the filter and were then searched for
    as the literal string `NA`. The filter now tests `!is.na()` first.
  - The meta-test now also scans `dev/repo-tests/` itself, and asserts
    that the file glob matched something, so it cannot again pass by
    finding nothing.

# CounterfactMe 0.9.43

## Bug fixes
- `R CMD check` failed on every platform. The test
  `every exported function is documented and defined` called
  `readLines("../../NAMESPACE")` before its skip guard, and under
  `R CMD check` that path does not exist -- so it raised an error rather
  than skipping, and an error fails the check outright.

## Tests
- Moved the repository checks out of `tests/` entirely, into
  `dev/repo-tests/`, run by `Rscript dev/run-repo-tests.R` and by a
  separate CI job. They examine the repository rather than the package
  and read the source tree, so `R CMD check` and `covr` -- which run
  against an installed package -- had no business running them. Three
  attempts to make them detect their own environment each failed in a
  new way; removing them from the test suite removes the class of
  problem rather than the instance.

  `tests/` now reads package data only through `system.file()`, and
  nothing in it depends on the source tree being present.

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
