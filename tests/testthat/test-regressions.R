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
