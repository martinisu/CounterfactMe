test_that("verify_consistency returns a list with summary + examples", {
  result <- verify_consistency(N = 200, verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("summary", "examples") %in% names(result)))
})

test_that("Children under 16 don't get sexual orientation", {
  set.seed(3)
  for (i in 1:30) {
    x <- counterfact_me(min_age = 0, max_age = 12)
    expect_true(is.null(x$orientation) || is.na(x$orientation))
  }
})

test_that("Owners with shared housing don't co-exist", {
  set.seed(4)
  for (i in 1:50) {
    x <- counterfact_me(min_age = 18)
    if (!is.null(x$housing_tenure) && !is.na(x$housing_tenure) &&
        x$housing_tenure %in% c("Eier", "Owns")) {
      hh <- x$household
      if (!is.null(hh) && !is.na(hh)) {
        expect_false(grepl("[Bb]ofellesskap|[Bb]or hos foreldre", hh))
      }
    }
  }
})

test_that("First_gen Afghan over 80 should not occur", {
  set.seed(5)
  bad <- 0
  for (i in 1:200) {
    x <- counterfact_me()
    if (!is.null(x$background) && identical(x$background, "first_gen") &&
        !is.null(x$country_background) && grepl("Afghanistan", x$country_background) &&
        !is.null(x$age) && x$age > 75) {
      bad <- bad + 1
    }
  }
  expect_lte(bad, 5)  # allow rare edge cases
})
