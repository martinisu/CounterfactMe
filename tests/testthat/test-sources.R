# The provenance manifest has to stay in step with the data directory,
# otherwise it becomes a stale document that is worse than none: it would
# imply every file has been accounted for when some had not.

test_that("every shipped CSV appears in the manifest, and vice versa", {
  ed <- system.file("extdata", package = "CounterfactMe")
  on_disk <- setdiff(list.files(ed, pattern = "\\.csv$"), "SOURCES.csv")
  listed <- data_sources()$file

  expect_setequal(on_disk, listed)
})

test_that("manifest fields are internally consistent", {
  d <- data_sources()

  expect_true(all(d$status %in%
                  c("ssb_api", "ssb_cited", "untraced", "authored")))

  # A file claimed as API-fetched must name the script that fetches it,
  # or the claim cannot be checked by anyone.
  api <- d[d$status == "ssb_api", ]
  expect_true(all(nzchar(api$script)))

  # And that script must exist in the source tree. data-raw/ is excluded
  # from the built package, so this only runs from a source checkout.
  skip_if_not(dir.exists("../../data-raw"), "data-raw/ not present")
  for (s in unique(api$script)) {
    expect_true(file.exists(file.path("../../data-raw", s)),
                label = sprintf("data-raw/%s named in SOURCES.csv", s))
  }
})

test_that("cited SSB tables are actually cited somewhere in the source", {
  # Guards against a table number being recorded in the manifest but
  # nowhere in the code, which would make it unverifiable.
  skip_if_not(dir.exists("../../R"), "R/ not present")
  src <- paste(unlist(lapply(list.files("../../R", full.names = TRUE),
                             readLines, warn = FALSE)), collapse = "\n")
  cited <- data_sources()
  cited <- cited[cited$status %in% c("ssb_api", "ssb_cited") &
                 nzchar(as.character(cited$ssb_table)), ]
  for (i in seq_len(nrow(cited))) {
    tab <- as.character(cited$ssb_table[i])
    found <- grepl(tab, src, fixed = TRUE) ||
             any(vapply(list.files("../../data-raw", full.names = TRUE),
                        function(f) grepl(tab, paste(readLines(f, warn = FALSE),
                                                     collapse = "\n"),
                                          fixed = TRUE),
                        logical(1)))
    expect_true(found,
                label = sprintf("SSB %s (%s) cited in the source",
                                tab, cited$file[i]))
  }
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
