# Property-based invariants.
#
# These test what the package promises, not what was most recently changed.
# They exist because two bugs (a female ego drawing the occupation
# "Fosterfar", and English being the default language) survived a full
# R CMD check and a hand-written smoke test, and were found by a user
# simply calling counterfact_me() with no arguments.

# One shared predicate, because getting this wrong is easy and quiet.
#
# "Absent" has three shapes in this package:
#   NULL   -- dimension not drawn at all
#   NA     -- drawn but not applicable (gated survey fields)
#   FALSE  -- prevalence flags such as has_chronic / has_disability, which
#             are FALSE rather than NA when the person does not have it
#
# An is.na() check treats FALSE as *present*, so a test written that way
# passes even when the field is fully gated. That mistake is why the
# age-floor test below reported 240 failures against correct code.
.absent <- function(v) {
  is.null(v) || identical(v, FALSE) || all(is.na(v))
}
.present <- function(v) !.absent(v)

# ---------------------------------------------------------------
# 1. Gendered labels must not contradict the drawn gender.
#    Systematic sweep over output strings, not a fixed list -- a new
#    gendered title entering the register should fail here.
# ---------------------------------------------------------------

test_that("no output string carries a gender morpheme contradicting ego", {
  # Morphemes that denote the *bearer's* gender in Norwegian job titles.
  # Deliberately excludes DAME*/HERRE* (describes the customer: a
  # damefrisor may be a man) and -MANN (raadmann, sysselmann etc. are
  # gender-neutral titles in modern usage).
  male_morph   <- c("far\\b", "gutt\\b", "bror\\b", "husfar")
  female_morph <- c("mor\\b", "pike\\b", "s\u{00f8}ster\\b", "husmor")

  # Fields that describe ego (not relatives, whose gender differs).
  ego_fields <- c("occupation", "household")

  check_one <- function(g, forbidden, n = 250L) {
    hits <- character(0)
    for (i in seq_len(n)) {
      x <- counterfact_me(gender = g, min_age = 20, max_age = 67)
      for (f in ego_fields) {
        v <- x[[f]]
        if (is.null(v) || length(v) != 1 || is.na(v)) next
        v <- as.character(v)
        for (p in forbidden) {
          if (grepl(p, tolower(v))) hits <- c(hits, sprintf("%s=%s", f, v))
        }
      }
    }
    unique(hits)
  }

  set.seed(101)
  f_hits <- check_one("F", male_morph)
  expect_equal(f_hits, character(0),
               info = paste("female ego, male-gendered label:",
                            paste(f_hits, collapse = "; ")))

  set.seed(102)
  m_hits <- check_one("M", female_morph)
  expect_equal(m_hits, character(0),
               info = paste("male ego, female-gendered label:",
                            paste(m_hits, collapse = "; ")))
})

# ---------------------------------------------------------------
# 2. No dimension may go silently empty.
#    A renamed CSV column, a moved file, or a changed header would not
#    raise an error -- the field would just quietly become NA for
#    everyone. This is the failure mode that is invisible without a test.
# ---------------------------------------------------------------

test_that("core dimensions are populated at plausible rates", {
  set.seed(103)
  n <- 300L
  draws <- lapply(seq_len(n), function(i) counterfact_me(min_age = 25, max_age = 70))

  filled <- function(field) mean(vapply(draws, function(d) .present(d[[field]]),
                                        logical(1)))

  # field -> minimum share of draws that must have it
  expectations <- list(
    name              = 1.00,
    age               = 1.00,
    gender            = 1.00,
    municipality      = 1.00,
    county            = 1.00,
    occupation        = 0.95,
    education         = 0.95,
    income_nok        = 0.90,
    household         = 0.95,
    marital_status    = 0.95,
    self_rated_health = 0.90,   # broke silently once via a CSV rename
    bourdieu_klasse   = 0.85,
    mother            = 0.95,
    father            = 0.95
  )

  for (f in names(expectations)) {
    got <- filled(f)
    expect_gte(got, expectations[[f]],
               label = sprintf("share of draws with '%s' (%.2f)", f, got))
  }
})

# ---------------------------------------------------------------
# 3. Printed output must be free of rendering artefacts.
#    Catches unescaped placeholders, stray NA/NULL, and -- since the
#    Norwegian characters in R source are \u escapes -- any failure to
#    decode them.
# ---------------------------------------------------------------

test_that("print output has no artefacts across ages and languages", {
  set.seed(104)
  for (lang in c("no", "en")) {
    for (i in 1:60) {
      x <- counterfact_me(lang = lang)
      txt <- paste(capture.output(print(x)), collapse = "\n")
      expect_false(grepl("\\\\u\\{|<U\\+", txt),
                   label = sprintf("undecoded escape (lang=%s)", lang))
      expect_false(grepl("\\bNULL\\b", txt),
                   label = sprintf("literal NULL in output (lang=%s)", lang))
      expect_false(grepl("\\bNA\\b", txt),
                   label = sprintf("literal NA in output (lang=%s)", lang))
    }
  }
})

test_that("Norwegian output actually contains Norwegian characters", {
  # If the \u escapes in the source ever stop decoding, output would be
  # ASCII-only and look subtly wrong rather than fail.
  set.seed(105)
  txt <- paste(vapply(1:40, function(i)
    paste(capture.output(print(counterfact_me(lang = "no"))), collapse = " "),
    character(1)), collapse = " ")
  expect_true(grepl("[\u{00e6}\u{00f8}\u{00e5}\u{00c6}\u{00d8}\u{00c5}]", txt))
})

# ---------------------------------------------------------------
# 4. Hard impossibilities must never survive, at any age.
# ---------------------------------------------------------------

test_that("no hard impossibilities across the full age range", {
  set.seed(106)
  bands <- list(c(0, 5), c(6, 17), c(18, 24), c(25, 44),
                c(45, 66), c(67, 79), c(80, 99))
  for (b in bands) {
    found <- character(0)
    for (i in 1:60) {
      x <- counterfact_me(min_age = b[1], max_age = b[2])
      found <- c(found, find_impossibilities(x))
    }
    expect_equal(unique(found), character(0),
                 label = sprintf("impossibilities in ages %d-%d: %s",
                                 b[1], b[2], paste(unique(found), collapse = ", ")))
  }
})

# ---------------------------------------------------------------
# 5. Language is a presentation layer, not a sampling input.
# ---------------------------------------------------------------

test_that("language does not alter the underlying draw", {
  for (s in c(1L, 7L, 42L, 2026L)) {
    set.seed(s); a <- counterfact_me(lang = "no")
    set.seed(s); b <- counterfact_me(lang = "en")
    expect_equal(a$age, b$age)
    expect_equal(a$gender, b$gender)
    expect_equal(a$municipality, b$municipality)
    expect_equal(a$income_nok, b$income_nok)
  }
})

# ---------------------------------------------------------------
# 6. Nobody may be born after their child, at any generation.
# ---------------------------------------------------------------

test_that("generational birth years are ordered", {
  set.seed(107)
  for (i in 1:120) {
    x <- counterfact_me(min_age = 18)
    ego_birth <- 2026L - x$age
    for (p in c("mother", "father")) {
      if (is.null(x[[p]]$birth_year)) next
      expect_lt(x[[p]]$birth_year, ego_birth - 13L)
    }
    for (gp in c("mormor", "morfar")) {
      if (is.null(x[[gp]]$birth_year) || is.null(x$mother$birth_year)) next
      expect_lt(x[[gp]]$birth_year, x$mother$birth_year - 13L)
    }
    for (gp in c("farmor", "farfar")) {
      if (is.null(x[[gp]]$birth_year) || is.null(x$father$birth_year)) next
      expect_lt(x[[gp]]$birth_year, x$father$birth_year - 13L)
    }
  }
})

# ---------------------------------------------------------------
# 7. Survey-based dimensions must be absent below the survey's own
#    lower age limit.
#
#    A 4-year-old was being given a generalized-trust score, a podcast
#    habit and a favourite newspaper -- the last one conditioned on the
#    party they "vote" for. These are not rounding errors: the source
#    surveys do not ask children, so any value is invented, and it looks
#    just as authoritative as the register-derived fields.
# ---------------------------------------------------------------

test_that("survey dimensions are suppressed below their age floor", {
  floors <- CounterfactMe:::.dimension_min_age
  set.seed(108)
  for (i in 1:120) {
    x <- counterfact_me(min_age = 0, max_age = 15)
    for (f in names(floors)) {
      if (x$age >= floors[[f]]) next
      v <- x[[f]]
      expect_true(.absent(v),
                  label = sprintf("%s present at age %d (floor %d): %s",
                                  f, x$age, floors[[f]],
                                  paste(as.character(v), collapse = ",")))
    }
  }
})

test_that("the same dimensions ARE present for adults", {
  # Guards against a gate that is too aggressive and silently empties a
  # field for everyone.
  #
  # Note the predicate: has_chronic and has_disability are FALSE, not NA,
  # for a healthy adult, so an is.na() check would count them as present
  # even if they were fully gated. They need a prevalence threshold drawn
  # from the underlying rates instead.
  set.seed(109)
  draws <- lapply(1:200, function(i) counterfact_me(min_age = 30, max_age = 60))

  present <- function(f) mean(vapply(draws, function(d) .present(d[[f]]), logical(1)))

  # Value-bearing fields: essentially everyone should have one
  for (f in c("trust", "loneliness", "close_friends", "self_rated_health",
              "media_paper", "media_podcast", "media_social",
              "media_tv_hours", "sleep_hours")) {
    got <- present(f)
    expect_gt(got, 0.5, label = sprintf("adults with '%s' (%.2f)", f, got))
  }

  # Prevalence fields: TRUE only for a minority, so the bar is the
  # underlying rate, not a majority.
  # chronic_illness_prob: 0.18 (25-44) to 0.36 (45-64)
  expect_gt(present("has_chronic"), 0.05)
  # .cond_disability: 0.13 (25-44) to 0.20 (45-66)
  expect_gt(present("has_disability"), 0.03)
})

test_that("children keep only sourced or plainly-written fields", {
  # Everything a child is given must be either SSB-derived (name, age,
  # geography, family, household) or obviously authored (the child
  # variants of occupation, income and diet). Nothing in between:
  # no unsourced numbers dressed as measurements.
  set.seed(110)
  draws <- lapply(1:200, function(i) counterfact_me(min_age = 0, max_age = 15))

  share <- function(f) mean(vapply(draws, function(d) .present(d[[f]]), logical(1)))

  # Estimates without provenance -- must be gone
  for (f in c("sleep_hours", "media_tv_hours", "has_chronic",
              "has_disability", "chronic_type", "disability")) {
    expect_equal(share(f), 0,
                 label = sprintf("unsourced field '%s' present for children", f))
  }

  # SSB-derived and authored fields -- must remain
  for (f in c("name", "age", "municipality", "county", "household",
              "occupation", "mother", "father")) {
    expect_gt(share(f), 0.9,
              label = sprintf("child lost sourced field '%s'", f))
  }
})

test_that("the removed 0-15 rows are really gone from the data", {
  # If a band row reappears, the gates above would still hide it, so the
  # test would pass while the unsourced numbers quietly returned.
  ed <- system.file("extdata", package = "CounterfactMe")
  for (f in c("self_rated_health", "social_isolation", "tv_hours",
              "sleep_hours", "chronic_illness_prob")) {
    d <- utils::read.csv(file.path(ed, paste0(f, ".csv")),
                         stringsAsFactors = FALSE)
    expect_false("0-15" %in% d$age_band,
                 label = sprintf("%s.csv still has a 0-15 row", f))
  }
})

test_that("a missing age band yields NA rather than an adult value", {
  # The old fallback was `row <- srh[1, ]`, which would hand a child the
  # 16-24 row without any error. That is the failure mode worth guarding:
  # silent, plausible, and wrong.
  h <- CounterfactMe:::.cond_health(age = 8L, edu_code = 1L, lang = "no")
  expect_true(is.na(h$self_rated))
  expect_false(isTRUE(h$chronic))

  iso <- CounterfactMe:::.cond_social_isolation(age = 8L, lang = "no")
  expect_true(is.na(iso$loneliness))
  expect_true(is.na(iso$trust))
})

test_that("alcohol remains gated at 16", {
  set.seed(111)
  for (i in 1:150) {
    x <- counterfact_me(min_age = 0, max_age = 15)
    expect_true(is.null(x$alcohol_pattern) || is.na(x$alcohol_pattern))
  }
})
