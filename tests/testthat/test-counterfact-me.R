test_that("counterfact_me() returns a counterfactme object with expected fields", {
  set.seed(1)
  x <- counterfact_me(lang = "no")
  expect_s3_class(x, "counterfactme")
  expect_true(!is.null(x$age))
  expect_true(x$age >= 0 && x$age <= 99)
  expect_true(x$gender %in% c("M", "F"))
  expect_true(!is.null(x$lang))
})

test_that("counterfact_me works with subset of dimensions", {
  # gender is a parameter, not a dimension -- it is always drawn
  x <- counterfact_me(dimensions = c("name", "age"))
  expect_true(!is.null(x$name))
  expect_true(!is.null(x$age))
  expect_true(!is.null(x$gender))
  expect_true(is.null(x$housing_value_nok))
})

test_that("unknown dimensions are rejected", {
  expect_error(counterfact_me(dimensions = c("name", "not_a_dimension")),
               "Unknown dimensions")
})

test_that("counterfact_me works in both languages", {
  set.seed(2)
  x_no <- counterfact_me(lang = "no")
  set.seed(2)
  x_en <- counterfact_me(lang = "en")
  expect_equal(x_no$age, x_en$age)
  expect_equal(x_no$gender, x_en$gender)
})

test_that("min_age and max_age are respected", {
  for (i in 1:20) {
    x <- counterfact_me(min_age = 25, max_age = 35)
    expect_true(x$age >= 25 && x$age <= 35)
  }
})

test_that("forced gender is respected", {
  for (i in 1:10) {
    x <- counterfact_me(gender = "F")
    expect_equal(x$gender, "F")
  }
})

test_that("conditional = FALSE works", {
  x <- counterfact_me(conditional = FALSE)
  expect_s3_class(x, "counterfactme")
})

test_that("print method runs without error", {
  x <- counterfact_me(lang = "no")
  expect_output(print(x), "Ditt kontrafaktiske liv")
})

test_that("default language is Norwegian", {
  x <- counterfact_me()
  expect_equal(x$lang, "no")
  expect_output(print(x), "Ditt kontrafaktiske liv")
})

test_that("gendered occupation titles match ego's gender", {
  # FOSTERMOR/FOSTERFAR and similar pairs live in the same STYRK-08 group,
  # so the detail draw used to be blind to gender.
  male_only   <- c("Fosterfar", "Internathusfar", "Byssegutt",
                   "Lugargutt", "Messegutt", "Stallgutt")
  female_only <- c("Fostermor", "Internathusmor", "Byssepike",
                   "Lugarpike", "Messepike", "Stallpike")
  set.seed(11)
  for (i in 1:150) {
    x <- counterfact_me(gender = "F", min_age = 25, max_age = 60)
    expect_false(isTRUE(x$occupation %in% male_only))
  }
  set.seed(12)
  for (i in 1:150) {
    x <- counterfact_me(gender = "M", min_age = 25, max_age = 60)
    expect_false(isTRUE(x$occupation %in% female_only))
  }
})

test_that("customer-gendered titles are NOT filtered", {
  # DAMEFRISOR/HERREFRISOR and DAMESKREDDER/HERRESKREDDER describe the
  # CUSTOMER's gender, not the worker's -- a damefrisor can be a man.
  # This guards against a future over-eager MOR/FAR sweep of the register.
  tbl <- CounterfactMe:::.gendered_yrke_titles
  expect_length(tbl$female, 6)
  expect_length(tbl$male, 6)
  expect_false(any(grepl("FRIS", c(tbl$female, tbl$male))))
  expect_false(any(grepl("SKREDDER", c(tbl$female, tbl$male))))
})
