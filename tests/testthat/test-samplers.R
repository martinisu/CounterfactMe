test_that("sample_first_name returns plausible names", {
  names <- sample_first_name(n = 10, gender = "F")
  expect_length(names, 10)
  expect_type(names, "character")
})

test_that("sample_age returns ages in [0, 99]", {
  ages <- sample_age(n = 100)
  expect_length(ages, 100)
  expect_true(all(ages >= 0 & ages <= 99))
})

test_that("sample_age respects bounds", {
  ages <- sample_age(n = 50, min_age = 25, max_age = 40)
  expect_true(all(ages >= 25 & ages <= 40))
})

test_that("sample_education returns plausible labels", {
  edus <- sample_education(n = 10)
  expect_length(edus, 10)
  expect_type(edus, "character")
})

test_that("sample_municipality returns county info", {
  mun <- sample_municipality(n = 5)
  expect_equal(nrow(mun), 5)
  expect_true(all(c("name", "county", "population") %in% names(mun)))
})

test_that("available_dimensions returns expected set", {
  dims <- available_dimensions()
  expect_true("name" %in% dims)
  expect_true("housing" %in% dims)
  expect_true("bourdieu" %in% dims)
})
