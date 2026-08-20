# Regression tests for bugs that were fixed but never guarded.
#
# Each of these was found, diagnosed and fixed in an earlier session, and
# each could return silently: the failure is a plausible-looking value,
# not an error. The bug number refers to the project task list.

# ---------------------------------------------------------------
# #97 -- cohort anachronism in field of study
# A 73-year-old was given "IKT, annet". The mitigation is a cohort
# multiplier on modern NUS codes, not a hard block, so this asserts a
# rate rather than absence.
# ---------------------------------------------------------------

test_that("modern fields of study are rare for pre-1960 cohorts", {
  set.seed(201)
  modern <- c("IKT", "Informasjons", "Data", "Design", "Medie",
              "Milj", "Mikrobiologi")
  hits <- 0L; n <- 250L
  for (i in seq_len(n)) {
    x <- counterfact_me(min_age = 70, max_age = 95)
    f <- c(as.character(x$field_of_study %||% ""),
           as.character(x$field_of_study_detail %||% ""))
    if (any(vapply(modern, function(m) any(grepl(m, f, ignore.case = TRUE)),
                   logical(1)))) hits <- hits + 1L
  }
  # A multiplier can only shift weight onto something else. STYRK 35
  # (ICT technicians) has no non-modern candidate at all and STYRK 25 is
  # 90% modern, so x0.05 left those groups untouched and the observed rate
  # was 20%. Those two now fall back to an open field of study instead of
  # asserting a degree that did not exist, which should leave only the
  # genuinely dampened cases.
  expect_lt(hits / n, 0.05)
})

test_that("young cohorts still reach modern fields", {
  # Guards the opposite failure: a multiplier applied to everyone.
  set.seed(202)
  hits <- 0L; n <- 250L
  modern <- c("IKT", "Informasjons", "Data", "Design", "Medie", "Milj")
  for (i in seq_len(n)) {
    x <- counterfact_me(min_age = 25, max_age = 40)
    f <- c(as.character(x$field_of_study %||% ""),
           as.character(x$field_of_study_detail %||% ""))
    if (any(vapply(modern, function(m) any(grepl(m, f, ignore.case = TRUE)),
                   logical(1)))) hits <- hits + 1L
  }
  expect_gt(hits / n, 0.02)
})

# ---------------------------------------------------------------
# #94 -- occupation/education mismatch (a lorry driver with a master's)
# ---------------------------------------------------------------

test_that("manual STYRK groups do not draw highly educated egos", {
  # The output object carries labels, not codes, so both have to be
  # resolved through the shipped tables. If that resolution silently
  # failed, every draw would be skipped and the test would pass while
  # checking nothing -- hence the coverage assertion at the end.
  ed <- system.file("extdata", package = "CounterfactMe")
  occ_tbl <- utils::read.csv(file.path(ed, "occupations.csv"),
                             stringsAsFactors = FALSE, encoding = "UTF-8",
                             colClasses = "character")
  edu_tbl <- utils::read.csv(file.path(ed, "education_levels.csv"),
                             stringsAsFactors = FALSE, encoding = "UTF-8")

  styrk_of <- function(label) {
    if (is.null(label) || is.na(label)) return(NA_character_)
    hit <- occ_tbl$styrk08[toupper(occ_tbl$name) == toupper(label)]
    if (!length(hit)) NA_character_ else hit[1]
  }
  edu_code_of <- function(label) {
    if (is.null(label) || is.na(label)) return(NA_integer_)
    hit <- edu_tbl$code[edu_tbl$level_no == label | edu_tbl$level == label]
    if (!length(hit)) NA_integer_ else as.integer(hit[1])
  }

  set.seed(203)
  bad <- character(0); checked <- 0L
  for (i in 1:400) {
    x <- counterfact_me(min_age = 25, max_age = 66)
    sk <- styrk_of(x$occupation)
    ec <- edu_code_of(x$education)
    if (is.na(sk) || is.na(ec)) next
    checked <- checked + 1L
    # STYRK majors 6-9: agricultural, craft, operator, elementary.
    # A master's (7) or doctorate (8) there was the reported bug.
    if (substr(sk, 1, 1) %in% c("6", "7", "8", "9") && ec >= 7L) {
      bad <- c(bad, sprintf("%s / %s", x$occupation, x$education))
    }
  }
  expect_gt(checked, 100L)          # the test must actually have looked
  expect_equal(unique(bad), character(0))
})

# ---------------------------------------------------------------
# #55 -- name and background must agree
# "Fatima" was drawn for a majority-background ego with Norwegian
# parents, because the SSB name register contains names of immigrants
# who are, of course, registered in Norway.
# ---------------------------------------------------------------

test_that("majority-background egos draw Nordic names", {
  set.seed(204)
  checked <- 0L
  nbr <- utils::read.csv(
    system.file("extdata", "names_by_region.csv", package = "CounterfactMe"),
    stringsAsFactors = FALSE, encoding = "UTF-8")
  foreign <- unique(nbr$name[nbr$region != "norden"])
  bad <- character(0)
  for (i in 1:300) {
    x <- counterfact_me()
    if (!identical(x$background, "majority")) next
    checked <- checked + 1L
    if (x$name %in% foreign) bad <- c(bad, x$name)
  }
  expect_gt(checked, 100L)
  expect_equal(unique(bad), character(0))
})

# ---------------------------------------------------------------
# #51 -- immigration cannot predate the migration flow
# ---------------------------------------------------------------

test_that("years in Norway never exceeds age", {
  set.seed(205)
  checked <- 0L
  for (i in 1:400) {
    x <- counterfact_me()
    if (is.null(x$years_in_norway) || is.na(x$years_in_norway)) next
    checked <- checked + 1L
    expect_lte(x$years_in_norway, x$age)
  }
  expect_gt(checked, 10L)           # first-gen egos must occur at all
})

# ---------------------------------------------------------------
# #68 -- sibling age deltas had an inverted sign
# ---------------------------------------------------------------

test_that("sibling birth years are within a plausible span of ego", {
  set.seed(206)
  checked <- 0L
  for (i in 1:250) {
    x <- counterfact_me(min_age = 5)
    if (is.null(x$siblings) || !length(x$siblings)) next
    ego_birth <- 2026L - x$age
    for (s in x$siblings) {
      if (is.null(s$birth_year) || is.na(s$birth_year)) next
      checked <- checked + 1L
      expect_lte(s$birth_year, 2026L)
      # Siblings share parents, so a 40-year gap is not credible.
      expect_lt(abs(s$birth_year - ego_birth), 30L)
    }
  }
  expect_gt(checked, 50L)
})

# ---------------------------------------------------------------
# The all-modern fallback must leave the broad field intact -- it opens
# the detail, it does not blank the whole dimension.
# ---------------------------------------------------------------

test_that("an opened field of study keeps its broad field", {
  set.seed(207)
  checked <- 0L
  for (i in 1:200) {
    x <- counterfact_me(min_age = 70, max_age = 95)
    if (is.null(x$field_of_study) || is.na(x$field_of_study)) next
    checked <- checked + 1L
    # detail may be NA (opened), but broad must still be a real label
    expect_true(nzchar(as.character(x$field_of_study)))
  }
  expect_gt(checked, 40L)
})

# ---------------------------------------------------------------
# Wealth is held as double, not integer.
#
# Kroner amounts for the top 0.1% exceed R's integer maximum
# (2,147,483,647). as.integer() turned those into NA with a warning, and
# the next line -- if (residual < 0 && net_wealth > 0) -- then failed with
# "missing value where TRUE/FALSE needed". The draw is rare, so the crash
# surfaced only once the suite ran hundreds of lives.
# ---------------------------------------------------------------

test_that("very large fortunes do not overflow", {
  set.seed(208)
  checked <- 0L
  for (i in 1:400) {
    x <- counterfact_me(min_age = 40, max_age = 80)
    nw <- x$net_wealth_nok
    if (is.null(nw)) next
    checked <- checked + 1L
    expect_false(is.na(nw))
    expect_true(is.finite(nw))
  }
  expect_gt(checked, 300L)
})

test_that("wealth fields are numeric and survive above integer.max", {
  set.seed(209)
  checked <- 0L
  for (i in 1:500) {
    x <- counterfact_me(min_age = 45, max_age = 75)
    for (f in c("net_wealth_nok", "financial_assets_nok",
                "business_equity_nok", "inheritance_nok")) {
      v <- x[[f]]
      if (is.null(v) || is.na(v)) next
      checked <- checked + 1L
      expect_true(is.numeric(v))
      expect_true(is.finite(v))
    }
  }
  expect_gt(checked, 400L)
})

test_that("the wealth guard survives a forced top-tier draw", {
  # .cond_wealth is internal, so call it directly with inputs that push
  # into the Pareto tail rather than hoping the sampler gets there.
  set.seed(210)
  for (i in 1:100) {
    w <- CounterfactMe:::.cond_wealth(
      age = 65L, income_nok = 5e6, parents_capital = 2e8,
      housing_equity_nok = 5e7, hytte_value_nok = 3e7, lang = "no")
    expect_false(is.na(w$net_wealth_nok))
    expect_true(is.finite(w$net_wealth_nok))
  }
})

# ---------------------------------------------------------------
# A confidant without close friends must depend on the household.
#
# Reported from output: "Ingen naere venner" alongside "Fortrolig venn:
# ja". That combination is real -- for most people the confidant is a
# partner, a sibling or an adult child, which is why Levekar asks the
# two questions separately -- but it was drawn at a flat 35% regardless
# of whether anyone else lived there. `.cond_social_support()` accepted
# a `household` argument and never used it.
# ---------------------------------------------------------------

test_that("a confidant without close friends is rare for people living alone", {
  rate <- function(hh, n = 400L) {
    got <- vapply(seq_len(n), function(i) {
      s <- CounterfactMe:::.cond_social_support(
        age = 45L, loneliness = NA_character_, household = hh, lang = "no")
      if (!identical(s$n_band, 1L)) return(NA)
      isTRUE(s$has_confidant)
    }, logical(1))
    got <- got[!is.na(got)]
    list(rate = mean(got), n = length(got))
  }

  set.seed(401)
  alone <- rate("Aleneboende")
  with_others <- rate("Par med barn 6-17 \u{00e5}r")

  # Both arms must actually have produced draws with no close friends,
  # or the comparison below compares nothing.
  expect_gt(alone$n, 5L)
  expect_gt(with_others$n, 5L)

  expect_lt(alone$rate, 0.30)
  expect_gt(with_others$rate, alone$rate)
})

test_that("household is not ignored by social support", {
  # The parameter was declared and never read. This fails if that
  # returns.
  body_txt <- paste(deparse(CounterfactMe:::.cond_social_support),
                    collapse = "\n")
  expect_true(grepl("household", body_txt))
  expect_true(grepl("lives_with_others", body_txt))
})

# ---------------------------------------------------------------
# Narration faults reported from the webR app.
# ---------------------------------------------------------------

test_that("no narrated sentence ends with a subject pronoun", {
  # Norwegian is V2: after a fronted adverbial the verb comes second and
  # the subject follows it. The party clause was assembled as
  # "Politisk" + "stemmer SV" + pronoun, giving
  # "Politisk stemmer Sosialistisk Venstreparti hun."
  set.seed(501)
  checked <- 0L
  for (i in 1:40) {
    x <- counterfact_me(min_age = 18)
    for (st in c("biography", "compact")) {
      txt <- as.character(narrate_life(x, style = st))
      sentences <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
      sentences <- sentences[nzchar(trimws(sentences))]
      checked <- checked + length(sentences)
      # Only the subject forms. "... for henne." and "... enn ham." are
      # correct -- object case after a preposition -- and they appear in
      # the mobility sentences, which became reachable only once
      # .mobility() was fixed.
      trailing <- grepl("\\b(han|hun)\\s*[.!?]\\s*$", sentences)
      expect_equal(sentences[trailing], character(0))
    }
  }
  expect_gt(checked, 100L)
})

test_that("register education terms never reach the prose", {
  # "Universitets- og hogskoleutdanning kort/lang" is a classification,
  # not something anyone says. .edu_prose() renders it as
  # bachelorgrad/mastergrad in narration only; the labels in
  # education_levels.csv and every other output stay as they are.
  set.seed(502)
  checked <- 0L
  for (i in 1:40) {
    x <- counterfact_me(min_age = 25)
    for (st in c("biography", "compact", "obituary")) {
      txt <- tolower(as.character(narrate_life(x, style = st)))
      checked <- checked + 1L
      expect_false(grepl("utdanning kort|utdanning lang", txt))
      expect_false(grepl("h\u{00f8}gskoleutdanning", txt))
    }
  }
  expect_gt(checked, 100L)

  # and the source labels are untouched
  el <- utils::read.csv(
    system.file("extdata", "education_levels.csv", package = "CounterfactMe"),
    stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("kort$", el$level_no)))
  expect_true(any(grepl("lang$", el$level_no)))
})

test_that("the abroad marker is not narrated as a job title", {
  # .cond_parents() puts "Bodde i <land>" in the occupation field for a
  # parent who was not in Norway during their working life. Narration
  # treated it as a job title and lowercased it: "Hjemme var det en
  # bodde i serbia og en bodde i serbia som forsorget familien."
  expect_true(CounterfactMe:::.is_abroad_label("Bodde i Serbia"))
  expect_true(CounterfactMe:::.is_abroad_label("Lived in Serbia"))
  expect_false(CounterfactMe:::.is_abroad_label("Sykepleier"))

  set.seed(503)
  checked <- 0L
  for (i in 1:200) {
    x <- counterfact_me(min_age = 25)
    m <- x$mother$occupation
    m <- if (is.null(m) || is.na(m)) "" else as.character(m)
    if (!CounterfactMe:::.is_abroad_label(m)) next
    checked <- checked + 1L
    txt <- tolower(as.character(narrate_life(x, style = "biography")))
    # Match the marker only where a job title was expected. Plain
    # "en bodde i" also matches the correct sentence "Den ene
    # forelderen bodde i Serbia", which is the fix, not the fault.
    expect_false(grepl("var det en bodde i", txt))
    expect_false(grepl("og en bodde i", txt))
    expect_false(grepl("jobbet som bodde i", txt))
    expect_false(grepl("som var bodde i", txt))
    expect_false(grepl(" av bodde i", txt))
    # and the country never loses its capital
    expect_false(grepl("bodde i [a-z\u{00e6}\u{00f8}\u{00e5}]",
                       as.character(narrate_life(x, style = "biography"))))
  }
  expect_gt(checked, 3L)
})

test_that("educational mobility is actually narrated", {
  # .mobility() read x$edu_code, x$mother_edu_code and x$father_edu_code.
  # None of those fields exist, so it always returned NULL and this
  # entire strand of the biography never appeared in any output.
  set.seed(504)
  got <- 0L
  for (i in 1:120) {
    x <- counterfact_me(min_age = 30, max_age = 60)
    m <- CounterfactMe:::.mobility(x)
    if (!is.null(m)) got <- got + 1L
  }
  expect_gt(got, 40L)
})

# ---------------------------------------------------------------
# Religion conditioned on country, not region.
#
# `mena_sor_asia` spans Morocco to Nepal, so its 8% Hindu share -- which
# comes from India, Nepal and Sri Lanka -- was applied to Lebanon, giving
# "11-aring fra Libanon, tilhorer hinduisme". `afrika_sub` carries 55%
# Islam, which made Ethiopia, Eritrea, Kenya, Uganda, Ghana, Rwanda and
# Angola Muslim-majority; every one of them is Christian-majority.
#
# The region taxonomy is untouched: name_region keys names_by_region.csv
# and drives the income, occupation and turnout adjustments.
# ---------------------------------------------------------------

test_that("Muslim-majority origins do not draw Hinduism", {
  muslim_majority <- c("Libanon", "Syria", "Irak", "Iran", "Tyrkia",
                       "Marokko", "Egypt", "Afghanistan")
  for (cty in muslim_majority) {
    w <- CounterfactMe:::.religion_country_weights(cty)
    expect_false(is.null(w), label = sprintf("%s has country weights", cty))
    expect_equal(unname(w[["HIN"]]), 0,
                 label = sprintf("Hindu share for %s", cty))
  }
  # Pakistan keeps a small real Hindu minority, so it is not zero --
  # but nowhere near the regional 8%.
  wp <- CounterfactMe:::.religion_country_weights("Pakistan")
  expect_lt(wp[["HIN"]], 0.05)
})

test_that("Ethiopia and Eritrea come out Christian-majority", {
  for (cty in c("Etiopia", "Eritrea", "Kenya", "Uganda", "Ghana", "Rwanda")) {
    w <- CounterfactMe:::.religion_country_weights(cty)
    expect_false(is.null(w))
    christian <- w[["KAT"]] + w[["ANN_KRIS"]]
    expect_gt(christian, w[["ISL"]],
              label = sprintf("%s: Christian %.2f vs Muslim %.2f",
                              cty, christian, w[["ISL"]]))
  }
  # Somalia is genuinely Muslim-majority and must stay that way.
  ws <- CounterfactMe:::.religion_country_weights("Somalia")
  expect_gt(ws[["ISL"]], 0.9)
})

test_that("country weights fall back to region for unknown countries", {
  expect_null(CounterfactMe:::.religion_country_weights("Atlantis"))
  expect_null(CounterfactMe:::.religion_country_weights(NA_character_))
  expect_null(CounterfactMe:::.religion_country_weights(NULL))
  # and .cond_religion still returns something sensible without a country
  r <- CounterfactMe:::.cond_religion(40L, name_region = "mena_sor_asia",
                                      background = "first_gen", lang = "no")
  expect_true(!is.na(r$code))
})

test_that("every country weight row is a distribution", {
  rbc <- utils::read.csv(
    system.file("extdata", "religion_by_country.csv", package = "CounterfactMe"),
    stringsAsFactors = FALSE, encoding = "UTF-8")
  cols <- setdiff(names(rbc), c("code", "label"))
  for (i in seq_len(nrow(rbc))) {
    s <- sum(as.numeric(rbc[i, cols]))
    expect_equal(s, 1, tolerance = 0.01,
                 label = sprintf("%s sums to %.3f", rbc$label[i], s))
  }
  expect_gt(nrow(rbc), 30L)
})

test_that("drawn religion matches the country of origin", {
  # End to end: no Lebanese Hindus, no Muslim-majority Ethiopians.
  # 1200 draws yields ~43 from these origins; 400 yielded ~14 against a
  # threshold of 10, close enough to fail on an unlucky seed.
  # 250 full draws, not 1200: the country-level weights are asserted
  # directly in the tests above, so this only has to show that
  # country_label reaches .cond_religion at all.
  set.seed(601)
  seen <- 0L; hindu_mena <- 0L
  eth <- c(christian = 0L, muslim = 0L)
  for (i in 1:250) {
    x <- counterfact_me(min_age = 10)
    cb <- x$country_background
    if (is.null(cb) || is.na(cb)) next
    rel <- x$religion
    rel <- if (is.null(rel) || is.na(rel)) "" else tolower(as.character(rel))
    # Pakistan is deliberately absent: it has a real Hindu minority of
    # about 2 %, which the country table reproduces. Lumping it in with
    # countries that have none contradicted the design.
    if (cb %in% c("Libanon", "Syria", "Irak", "Iran", "Tyrkia", "Marokko",
                  "Egypt", "Afghanistan")) {
      seen <- seen + 1L
      if (grepl("hindu", rel)) hindu_mena <- hindu_mena + 1L
    }
    if (cb %in% c("Etiopia", "Eritrea")) {
      if (grepl("krist|katol|ortodoks", rel)) eth[["christian"]] <- eth[["christian"]] + 1L
      if (grepl("islam|muslim", rel))        eth[["muslim"]]    <- eth[["muslim"]] + 1L
    }
  }
  expect_gt(seen, 2L)
  expect_equal(hindu_mena, 0L)
  if (sum(eth) >= 6L) expect_gte(eth[["christian"]], eth[["muslim"]])
})

# ---------------------------------------------------------------
# Retirees are not described as being in work.
#
# Reported from output: an 89-year-old "tok veien til jobben som
# hyttebokforfatter og har i dag en arsinntekt pa ...". The pastime
# labels in .PENSJONIST_LABELS are not jobs, and the income is a pension.
# ---------------------------------------------------------------

test_that("retirees are narrated as retired, not employed", {
  # Word boundaries, and no "arbeider som": 643 STYRK titles end in
  # -ARBEIDER, so "Hjemme var det en industriarbeider som forsorget
  # familien" -- a sentence about the ego's father -- matched it.
  work_verbs <- c("\\bveien til jobben\\b", "\\bjobber som\\b",
                  "\\b\u{00e5}rsinntekt\\b")
  set.seed(701)
  checked <- 0L
  for (i in 1:60) {
    x <- counterfact_me(min_age = 70, max_age = 95)
    for (st in c("biography", "compact")) {
      txt <- tolower(as.character(narrate_life(x, style = st)))
      checked <- checked + 1L
      for (v in work_verbs) {
        expect_false(grepl(v, txt),
                     label = sprintf("'%s' in %s for age %d", v, st, x$age))
      }
    }
  }
  expect_gt(checked, 100L)
})

test_that("a pastime label is never called a job", {
  # Someone under 67 can still draw a pensjonist label; the branch keys
  # on the label as well as on age.
  set.seed(702)
  checked <- 0L
  for (i in 1:250) {
    x <- counterfact_me(min_age = 60)
    occ <- x$occupation
    if (is.null(occ) || is.na(occ)) next
    if (!(occ %in% CounterfactMe:::.PENSJONIST_LABELS)) next
    checked <- checked + 1L
    txt <- tolower(as.character(narrate_life(x, style = "biography")))
    expect_false(grepl(sprintf("jobben som %s", tolower(occ)), txt))
    expect_false(grepl(sprintf("jobber som %s", tolower(occ)), txt))
  }
  expect_gt(checked, 3L)
})

test_that("working-age people are still described as working", {
  # Guards the opposite failure: a branch that swallows everyone.
  set.seed(703)
  employed <- 0L
  for (i in 1:80) {
    x <- counterfact_me(min_age = 30, max_age = 55)
    txt <- tolower(as.character(narrate_life(x, style = "compact")))
    if (grepl("jobber som|er pensjonist", txt)) employed <- employed + 1L
    expect_false(grepl("er pensjonist", txt))
  }
  expect_gt(employed, 40L)
})

# ---------------------------------------------------------------
# Arrival age must be plausible, not just consistent with the country's
# migration history.
#
# Residence length was centred on the country's peak year without
# checking how old the person would have been on arrival. Lithuania
# peaks in 2010, so a 65-year-old got roughly 16 years of residence --
# an arrival at 49. Baltic and Polish migration to Norway is labour
# migration, which happens at 18-40.
# ---------------------------------------------------------------

# Drawing a whole life to find one Lithuanian is wasteful: the CI run
# went from minutes to the six-hour job limit. .cond_immigrant_background()
# returns background, country and years of residence on its own, which is
# all these tests read.
.arrivals <- function(n, countries, min_age = 20L, max_age = 85L) {
  out <- list(arrival = integer(0), country = character(0))
  for (i in seq_len(n)) {
    a <- sample(min_age:max_age, 1)
    b <- CounterfactMe:::.cond_immigrant_background(a, lang = "no")
    if (!identical(b$background, "first_gen")) next
    if (is.null(b$years_in_norway) || is.na(b$years_in_norway)) next
    cl <- as.character(b$country_label)
    if (is.na(cl) || !(cl %in% countries)) next
    out$arrival <- c(out$arrival, a - as.integer(b$years_in_norway))
    out$country <- c(out$country, cl)
  }
  out
}

test_that("eastern European first-gen arrive at working age", {
  set.seed(801)
  a <- .arrivals(6000L, c("Polen", "Litauen", "Latvia", "Estland", "Romania"))
  expect_gt(length(a$arrival), 40L)
  expect_gte(mean(a$arrival >= 18 & a$arrival <= 40), 0.75)
  expect_true(all(a$arrival >= 0))
  expect_true(all(a$arrival <= 45))
})
test_that("nobody arrives after 45, whatever the origin", {
  set.seed(802)
  checked <- 0L
  for (i in 1:4000) {
    a <- sample(18:90, 1)
    b <- CounterfactMe:::.cond_immigrant_background(a, lang = "no")
    if (!identical(b$background, "first_gen")) next
    y <- b$years_in_norway
    if (is.null(y) || is.na(y)) next
    checked <- checked + 1L
    expect_gte(a - as.integer(y), 0L)
    expect_lte(a - as.integer(y), 45L)
  }
  expect_gt(checked, 100L)
})
test_that("refugee-origin flows still admit child arrivals", {
  # The 18-40 window is for labour migration. Somali or Syrian first-gen
  # who came as children must stay possible, or it was applied too widely.
  set.seed(803)
  a <- .arrivals(6000L, c("Somalia", "Syria", "Irak", "Afghanistan", "Eritrea"),
                 min_age = 18L)
  expect_gt(length(a$arrival), 30L)
  expect_gt(sum(a$arrival < 18L), 0L)
})
test_that("arrival profile is set for every country with a start year", {
  isy <- utils::read.csv(
    system.file("extdata", "immigration_start_year.csv", package = "CounterfactMe"),
    stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_true("arrival_profile" %in% names(isy))
  expect_true(all(nzchar(isy$arrival_profile)))
  expect_true(all(isy$arrival_profile %in% c("labour", "mixed", "refugee")))

  # The three countries whose Norwegian history is labour migration
  # followed by family reunification, and which the region taxonomy got
  # wrong because they sit in mena_sor_asia.
  for (cty in c("Pakistan", "Tyrkia", "Marokko")) {
    expect_equal(isy$arrival_profile[isy$label == cty], "mixed",
                 label = sprintf("%s profile", cty))
  }
  expect_equal(isy$arrival_profile[isy$label == "Polen"], "labour")
  expect_equal(isy$arrival_profile[isy$label == "Syria"], "refugee")
})

test_that("mixed-profile origins are not dominated by child arrivals", {
  # Before the country-level profile, Pakistani first-gen arrived as
  # children in over half of draws, which erased the labour wave.
  set.seed(804)
  a <- .arrivals(9000L, c("Pakistan", "Tyrkia", "Marokko"), min_age = 25L)
  expect_gt(length(a$arrival), 20L)
  expect_lt(mean(a$arrival < 18), 0.45)
  expect_gt(stats::median(a$arrival), 17)
})
# ---------------------------------------------------------------
# Parent education follows the parent's cohort, not today's.
#
# Reported from output: Elsa, 93, with two master's-educated parents.
# Born in 1933, her parents arrived around 1905, when a few per cent of
# Norwegians had any higher education. education_by_age.csv tops out at
# "67 aar eller eldre" -- everyone alive today above that age, mostly
# born 1940-1958, of whom 27.5% hold a degree. That band was being
# applied to every parent regardless of when they were born, and the
# inheritance path ignored the cohort entirely.
# ---------------------------------------------------------------

test_that("the cohort multiplier rises with birth year and is bounded", {
  m <- CounterfactMe:::.edu_cohort_tertiary_mult
  years <- c(1900, 1925, 1940, 1950, 1960, 1975, 2000)
  vals <- vapply(years, m, numeric(1))
  expect_true(all(vals > 0 & vals <= 1))
  expect_false(is.unsorted(vals))          # monotonically non-decreasing
  expect_equal(m(1990), 1)
  expect_lt(m(1905), 0.2)
  expect_equal(m(NA_integer_), 1)          # unknown cohort: no adjustment
})

test_that("demotion never promotes, and leaves non-tertiary alone", {
  d <- CounterfactMe:::.demote_edu_for_cohort
  set.seed(1001)
  for (code in 0:5) {
    for (i in 1:20) expect_equal(d(code, 1905), code)
  }
  for (i in 1:200) {
    out <- d(7L, 1905)
    expect_lte(out, 7L)
  }
})

test_that("parents of the very old rarely hold degrees", {
  set.seed(1002)
  ter <- 0L; n <- 0L
  for (i in 1:150) {
    x <- counterfact_me(min_age = 88, max_age = 96)
    for (p in c("mother", "father")) {
      code <- x[[p]]$education_code
      if (is.null(code) || is.na(code)) next
      n <- n + 1L
      if (as.integer(code) >= 6L) ter <- ter + 1L
    }
  }
  expect_gt(n, 150L)
  # ~3% historically; the band alone would have given 27.5%
  expect_lt(ter / n, 0.12)
})

test_that("parents of the young still commonly hold degrees", {
  # The opposite failure: a cohort adjustment applied to everyone.
  set.seed(1003)
  ter <- 0L; n <- 0L
  for (i in 1:150) {
    x <- counterfact_me(min_age = 28, max_age = 38)
    for (p in c("mother", "father")) {
      code <- x[[p]]$education_code
      if (is.null(code) || is.na(code)) next
      n <- n + 1L
      if (as.integer(code) >= 6L) ter <- ter + 1L
    }
  }
  expect_gt(n, 150L)
  expect_gt(ter / n, 0.12)
})

# ---------------------------------------------------------------
# Anachronism sweep: distributions measured on today's population
# applied to people who lived in a different Norway.
#
# The parent-education bug was one instance. These cover the two others
# found by going through every .cond_* function and asking what it uses
# the birth year for.
# ---------------------------------------------------------------

test_that("the occupational-structure multiplier shifts with the era", {
  m <- CounterfactMe:::.styrk_cohort_mult
  now <- m(2020)
  expect_true(all(abs(now - 1) < 1e-9))
  old <- m(1955)
  expect_gt(old[["6"]], 4)      # primary industry far larger then
  expect_lt(old[["2"]], 0.5)    # professionals far smaller
  # monotone between the anchors
  expect_gt(m(1955)[["6"]], m(1980)[["6"]])
  expect_gt(m(1980)[["6"]], m(2005)[["6"]])
  expect_equal(unname(m(NA_integer_)), rep(1, 10))
})

test_that("parents of the very old worked in an older labour market", {
  primary <- function(min_a, max_a, n = 120L) {
    set.seed(1101 + min_a)
    hit <- 0L; tot <- 0L
    for (i in seq_len(n)) {
      x <- counterfact_me(min_age = min_a, max_age = max_a)
      for (p in c("mother", "father")) {
        sk <- x[[p]]$styrk_code
        if (is.null(sk) || is.na(sk) || !nzchar(as.character(sk))) next
        tot <- tot + 1L
        if (substr(as.character(sk), 1, 1) == "6") hit <- hit + 1L
      }
    }
    list(share = if (tot) hit / tot else NA_real_, n = tot)
  }
  old <- primary(88L, 96L)
  young <- primary(28L, 38L)
  expect_gt(old$n, 100L)
  expect_gt(young$n, 100L)
  expect_gt(old$share, young$share)
})

test_that("pre-war cohorts are overwhelmingly Church of Norway", {
  set.seed(1102)
  dnk <- 0L; n <- 0L
  for (i in 1:150) {
    x <- counterfact_me(min_age = 88, max_age = 96)
    if (!identical(x$background, "majority")) next
    rel <- x$religion
    if (is.null(rel) || is.na(rel)) next
    n <- n + 1L
    if (grepl("norske kirke|Den norske", as.character(rel))) dnk <- dnk + 1L
  }
  expect_gt(n, 40L)
  expect_gt(dnk / n, 0.65)
})

test_that("the young are not forced into the Church of Norway", {
  # Guards the opposite failure.
  set.seed(1103)
  dnk <- 0L; n <- 0L
  for (i in 1:200) {
    x <- counterfact_me(min_age = 20, max_age = 30)
    if (!identical(x$background, "majority")) next
    rel <- x$religion
    if (is.null(rel) || is.na(rel)) next
    n <- n + 1L
    if (grepl("norske kirke|Den norske", as.character(rel))) dnk <- dnk + 1L
  }
  expect_gt(n, 80L)
  expect_lt(dnk / n, 0.75)
})

# ---------------------------------------------------------------
# Housing before 1992.
#
# SSB's house price index (07230) starts in 1992, so purchase years were
# floored there: a 93-year-old "bought" at 59. housing_index_prewar.csv
# splices CPI (SSB 08981, real, from 1920) onto an estimated real house
# price level, which is enough to date and price an older purchase.
# ---------------------------------------------------------------

test_that("the pre-1992 index spans the period and is anchored at 1992", {
  pw <- utils::read.csv(
    system.file("extdata", "housing_index_prewar.csv", package = "CounterfactMe"),
    stringsAsFactors = FALSE)
  expect_true(all(c("year", "cpi_index", "real_factor",
                    "nominal_index_1992_100") %in% names(pw)))
  expect_lte(min(pw$year), 1925)
  expect_equal(max(pw$year), 1992)
  expect_equal(pw$nominal_index_1992_100[pw$year == 1992], 100)
  expect_true(all(pw$nominal_index_1992_100 > 0))
  # nominal prices rose over the century, even if real prices wobbled
  expect_lt(pw$nominal_index_1992_100[pw$year == 1950],
            pw$nominal_index_1992_100[pw$year == 1985])
})

test_that("older purchases are cheaper than the same home today", {
  # CPI alone would have put a 5 MNOK home at ~367,000 in 1965; the real
  # adjustment brings it to ~123,000, which is the right order.
  f <- CounterfactMe:::.housing_index_factor
  r1965 <- f("Oslo", "01", from_year = 1965L, to_year = 2025L)
  r1985 <- f("Oslo", "01", from_year = 1985L, to_year = 2025L)
  expect_gt(r1965, 10)     # a home is worth many times its 1965 price
  expect_gt(r1965, r1985)  # and more so the further back you go
  expect_equal(f("Oslo", "01", 2000L, 2000L), 1)
})

test_that("very old owners can have bought before 1992", {
  set.seed(1201)
  years <- integer(0)
  for (i in 1:200) {
    x <- counterfact_me(min_age = 82, max_age = 96)
    py <- x$housing_purchase_year
    if (is.null(py) || is.na(py)) next
    years <- c(years, as.integer(py))
  }
  expect_gt(length(years), 20L)
  expect_true(any(years < 1992L))
  expect_true(all(years >= 1920L))
  # and nobody bought before they were plausibly adult
  expect_true(all(years <= 2026L))
})

test_that("purchase price stays below current value", {
  set.seed(1202)
  checked <- 0L
  for (i in 1:250) {
    x <- counterfact_me(min_age = 40, max_age = 96)
    v <- x$housing_value_nok; p <- x$housing_purchase_price_nok
    if (is.null(v) || is.null(p) || is.na(v) || is.na(p) || v <= 0) next
    checked <- checked + 1L
    expect_lt(p, v * 1.5)
    expect_gt(p, 0)
  }
  expect_gt(checked, 30L)
})

# ---------------------------------------------------------------
# Comma before "men".
#
# Norwegian puts a comma before "men" when it joins two clauses. Four
# humour labels shipped without one -- "Synes Stoere er kjedelig men
# trygg" -- and they go straight into the narration, so the error showed
# up in the biography rather than in any template.
# ---------------------------------------------------------------

test_that("no shipped label joins clauses with an uncommaed men", {
  ed <- system.file("extdata", package = "CounterfactMe")
  offenders <- character(0)
  for (f in list.files(ed, pattern = "\\.csv$", full.names = TRUE)) {
    d <- utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8",
                         colClasses = "character")
    for (col in names(d)) {
      v <- d[[col]]
      v <- v[!is.na(v) & nzchar(v)]
      bad <- v[grepl("[^,]\\s+men\\s+", v)]
      if (length(bad)) offenders <- c(offenders,
                                      sprintf("%s$%s: %s", basename(f), col, bad))
    }
  }
  expect_equal(offenders, character(0))
})

test_that("narration templates put the comma in themselves", {
  skip_if_not(file.exists("../../R/narrate.R"), "not a source checkout")
  src <- paste(readLines("../../R/narrate.R", warn = FALSE), collapse = "\n")
  lits <- regmatches(src, gregexpr('"[^"]{10,200}"', src))[[1]]
  bad <- lits[grepl("[^,]\\s+men\\s+", lits)]
  expect_equal(bad, character(0))
})
