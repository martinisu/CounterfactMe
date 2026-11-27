# The final check on a whole life (R/plausibility.R).

test_that("the final check flags what it is meant to flag", {
  set.seed(7201)
  x <- counterfact_me(min_age = 30, max_age = 40)
  expect_equal(CounterfactMe:::.find_implausibilities(x), character(0))

  x2 <- x; x2$occupation <- "Butikkmedarbeider"; x2$income_nok <- 5e6; x2$neet <- FALSE
  attr(x2, "occ_styrk") <- "5223"; x2$stillingsandel <- NULL
  expect_true("lonn_langt_over_yrket" %in% CounterfactMe:::.find_implausibilities(x2))

  x3 <- x; x3$age <- 21; x3$housing_tenure <- "Eier"; x3$housing_purchase_price_nok <- 8e6
  x3$income_nok <- 300000; x3$parents_capital <- 0
  expect_true("ung_eier_uten_betalingsevne" %in% CounterfactMe:::.find_implausibilities(x3))

  x4 <- x; x4$education <- "Ungdomsskoleutdanning (8.-10. trinn)"; x4$edu_code <- NULL
  attr(x4, "occ_styrk") <- "1120"; x4$occupation <- "Konsernsjef"; x4$neet <- FALSE
  expect_true("toppleder_med_grunnskole" %in% CounterfactMe:::.find_implausibilities(x4))

  x6 <- x; x6$background <- "first_gen"; x6$years_in_norway <- 25L; x6$housing_tenure <- "Eier"
  x6$housing_value_nok <- 10.9e6; x6$income_nok <- 370000
  expect_true("innvandrer_bolig_over_evne" %in% CounterfactMe:::.find_implausibilities(x6))

  CounterfactMe:::.load_data()
  occ <- CounterfactMe:::.cfm_env$occupations
  x5 <- x; x5$gender <- "M"; attr(x5, "occ_title_row") <- which(occ$name == "SOLVERTINNE")[1]
  expect_true("tittel_passer_ikke" %in% CounterfactMe:::.find_implausibilities(x5))
})

test_that("lives returned by counterfact_me() pass the final check", {
  set.seed(7202)
  n <- 150L
  hits <- character(0)
  for (i in seq_len(n)) {
    x <- counterfact_me()
    hits <- c(hits, CounterfactMe:::.find_implausibilities(x))
  }
  expect_equal(hits, character(0))
})
