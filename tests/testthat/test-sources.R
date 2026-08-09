# The provenance manifest has to stay in step with the data directory,
# otherwise it becomes a stale document that is worse than none: it would
# imply every file has been accounted for when some had not.
test_that("every shipped CSV appears in the manifest, and vice versa", {
  ed <- system.file("extdata", package = "CounterfactMe")
  on_disk <- setdiff(list.files(ed, pattern = "\\.csv$"), "SOURCES.csv")
  listed <- data_sources()$file

  expect_setequal(on_disk, listed)
})

test_that("the filter argument works and rejects nonsense", {
  expect_true(all(data_sources("untraced")$status == "untraced"))
  expect_lt(nrow(data_sources("ssb_api")), nrow(data_sources()))
  expect_error(data_sources("ssb"), "Unknown status")
})

test_that("the register-derived core is genuinely register-derived", {
  # These carry the package's central claim -- that lives are drawn from
  # real population distributions. If one of them ever slipped to
  # untraced, the claim would quietly stop holding.
  d <- data_sources()
  core <- c("municipalities.csv", "counties.csv", "age_distribution.csv",
            "first_names_cohort.csv", "education_by_age.csv",
            "occupations_salary.csv")
  for (f in core) {
    st <- d$status[d$file == f]
    expect_equal(st, "ssb_api", label = sprintf("%s provenance", f))
  }
})
