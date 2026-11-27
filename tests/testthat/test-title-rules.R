# The whole-register title requirements (R/title_rules.R).

test_that("every title in the register is classified, and many carry a requirement", {
  CounterfactMe:::.load_data()
  tr <- CounterfactMe:::.title_requirements()
  occ <- CounterfactMe:::.cfm_env$occupations
  expect_equal(nrow(tr), nrow(occ))
  expect_gt(sum(nzchar(tr$rule)), 1500L)
  # Every rule matches something: a rule that matches nothing is a typo.
  for (r in CounterfactMe:::.TITLE_RULES$rule)
    expect_gt(sum(grepl(r, tr$rule, fixed = TRUE)), 0L, label = r)
})

test_that("named titles get the requirement they should, and no more", {
  CounterfactMe:::.load_data()
  occ <- CounterfactMe:::.cfm_env$occupations
  fits <- function(title, ...) {
    r <- which(occ$name == title)
    expect_gt(length(r), 0L, label = title)
    CounterfactMe:::.title_fits(r[1], ...)
  }
  # Blocked
  expect_false(fits("UNIVERSITETSLEKTOR", age = 31, edu_code = 6L))
  expect_false(fits("LEKTOR (GRUNNSKOLE)", age = 40, edu_code = 6L))
  expect_false(fits("OVERLEGE", age = 50, edu_code = 6L))
  expect_false(fits("SYKEPLEIER", age = 40, edu_code = 4L))
  expect_false(fits("FAGARBEIDER (SNEKKERI)", age = 40, edu_code = 2L))
  expect_false(fits("SIVILINGENI\u00d8R (BYGG OG ANLEGG)", age = 40, edu_code = 6L))
  expect_false(fits("L\u00c6RLING (SNEKKERI)", age = 45, edu_code = 4L))
  expect_false(fits("KONSERNSJEF", age = 32, edu_code = 7L))
  expect_false(fits("SOLVERTINNE", age = 40, edu_code = 4L, gender = "M"))
  expect_false(fits("STALLGUTT", age = 20, edu_code = 2L, gender = "F"))
  # Allowed: the false positives a crude pattern would have caught
  expect_true(fits("UNIVERSITETSLEKTOR", age = 45, edu_code = 7L))
  expect_true(fits("MEDDOMMER (DOMSTOLENE)", age = 50, edu_code = 4L))
  expect_true(fits("DOMMER (IDRETT)", age = 30, edu_code = 4L))
  expect_true(fits("OVERSETTER", age = 26, edu_code = 6L))
  expect_true(fits("OPERAT\u00d8R (FARMAS\u00d8YTISKE PRODUKTER)", age = 30, edu_code = 2L))
  expect_true(fits("L\u00c6RER (SPR\u00c5KKURS)", age = 30, edu_code = 4L))
  expect_true(fits("L\u00c6RLING (SNEKKERI)", age = 18, edu_code = 2L))
  expect_true(fits("KOMMUNEJORDMOR", age = 40, edu_code = 6L, gender = "M"))
  expect_true(fits("JULENISSE", age = 40, edu_code = 2L, gender = "M"))
  expect_true(fits("PEDAGOGISK LEDER (F\u00d8RSKOLEL\u00c6RER)", age = 26, edu_code = 6L))
  expect_true(fits("REVISOR (IKKE STATSAUTORISERT)", age = 40, edu_code = 6L))
})

test_that("drawn occupations respect the requirements", {
  # The draw used to fall back to the unfiltered titles when every title
  # in a code was filtered out; now it draws another code.
  CounterfactMe:::.load_data()
  set.seed(7101)
  bad <- character(0); seen <- 0L
  people <- list(c(age = 31, edu = 6), c(age = 24, edu = 4), c(age = 45, edu = 2),
                 c(age = 55, edu = 4))
  for (p in people) for (i in 1:60) {
    g <- if (i %% 2) "F" else "M"
    o <- CounterfactMe:::.cond_occupation(age = p[["age"]], edu_code = as.integer(p[["edu"]]),
                                          mun_pop = 60000, gender = g)
    if (!identical(o$kind, "worker")) next
    seen <- seen + 1L
    if (!CounterfactMe:::.title_label_fits(o$label, p[["age"]], as.integer(p[["edu"]]), g))
      bad <- c(bad, sprintf("%s (%s, %d aar, utd %d)", o$label, g, p[["age"]], p[["edu"]]))
  }
  # A code with no fitting title after eight redraws can still leak; that
  # should be rare.
  expect_gt(seen, 150L)
  expect_lt(length(bad), 3L, label = paste(bad, collapse = "; "))
})

test_that("immigrant names come from the origin country's language", {
  # "Sergei fra Polen", "Tadas fra Serbia", "Milica fra Romania" and a
  # Hindu from India called Imran: the region pools mixed languages.
  CounterfactMe:::.load_data()
  pools <- CounterfactMe:::.NAME_POOLS
  set.seed(7103)
  seen <- 0L
  for (i in 1:400) {
    x <- counterfact_me(min_age = 20, max_age = 70)
    if (!identical(x$background, "first_gen")) next
    key <- unname(CounterfactMe:::.COUNTRY_NAME_POOL[x$country_background])
    if (is.na(key)) next
    seen <- seen + 1L
    expect_true(x$name %in% unlist(pools[[key]]),
                label = sprintf("%s fra %s", x$name, x$country_background))
  }
  expect_gte(seen, 30L)
  # Every country in the pool map exists in the country table.
  cd <- CounterfactMe:::.cfm_env$immigrant_country_dist
  expect_true(all(names(CounterfactMe:::.COUNTRY_NAME_POOL) %in% cd$label),
              label = paste(setdiff(names(CounterfactMe:::.COUNTRY_NAME_POOL), cd$label), collapse = ", "))
})
