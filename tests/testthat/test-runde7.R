# Sysselsetting 67+, bolig etter botid og fagfelt etter yrke (28. november 2026).

test_that("employment at 67-71 follows SSB 06445, not a flat 45 pct", {
  CounterfactMe:::.load_data()
  share <- CounterfactMe:::.employment_share_old
  # The curve averages to the 06445 band (67-74, 2025) by sex.
  expect_equal(mean(vapply(67:74, share, numeric(1), gender = "M")), 0.294, tolerance = 1e-6)
  expect_equal(mean(vapply(67:74, share, numeric(1), gender = "F")), 0.154, tolerance = 1e-6)
  expect_gt(share(67, "M"), share(71, "M"))
  set.seed(7301)
  for (g in c("M", "F")) for (a in c(67L, 70L)) {
    k <- vapply(1:250, function(i) {
      o <- CounterfactMe:::.cond_occupation(age = a, edu_code = 4L, mun_pop = 50000, gender = g)
      !(o$kind %in% c("pensioner", "disabled"))
    }, logical(1))
    expect_lt(abs(mean(k) - share(a, g)), 0.10, label = sprintf("%s %d: %.2f", g, a, mean(k)))
  }
})

test_that("nobody is on uforetrygd, AAP or dagpenger from 67", {
  CounterfactMe:::.load_data()
  set.seed(7302)
  lab <- vapply(1:300, function(i)
    CounterfactMe:::.cond_occupation(age = 67L, edu_code = 4L, mun_pop = 50000,
                                     gender = if (i %% 2) "F" else "M")$label, character(1))
  expect_false(any(grepl("^(Uf\u00f8retrygdet|AAP|Dagpenger)", lab)))
})

test_that("immigrants own less, more so when recently arrived", {
  f <- CounterfactMe:::.immigrant_own_factor
  expect_equal(f(NULL), 1)
  expect_lt(f(2, "mena_sor_asia"), f(15, "mena_sor_asia"))
  expect_equal(f(20, "vesteuropa"), 1)
  expect_lt(f(20, "afrika_sub"), 1)
  # Nobody bought a home in Norway before arriving.
  set.seed(7303)
  seen <- 0L
  for (i in 1:400) {
    x <- counterfact_me(min_age = 30, max_age = 75)
    if (!identical(x$background, "first_gen") || !identical(x$housing_tenure, "Eier")) next
    seen <- seen + 1L
    yin <- x$years_in_norway
    expect_gte(x$housing_purchase_year, CounterfactMe:::.cfm_env$ref_year - yin - 1L)
  }
  expect_gte(seen, 10L)
})

test_that("the field of study fits occupations defined by their training", {
  CounterfactMe:::.load_data()
  set.seed(7304)
  fld <- function(code, edu) CounterfactMe:::.cond_nus_field(40, edu, code, lang = "no")
  for (i in 1:30) {
    expect_equal(fld("2342", 6L)$detail_code, "211")
    expect_equal(fld("7411", 4L)$detail_code, "551")
    expect_equal(fld("5141", 4L)$detail_code, "831")
    # Not at videregaaende: a barnehagelaerer-titled assistant with vgs
    # has no preschool-teacher degree.
    expect_false(identical(fld("2342", 4L)$detail_code, "211"))
  }
})
