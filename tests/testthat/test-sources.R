# The provenance manifest has to stay in step with the data directory,
# otherwise it becomes a stale document that is worse than none: it would
# imply every file has been accounted for when some had not.

# Is this a source checkout, or an installed/checked package?
#
# `dir.exists("../../R")` is not the answer: an installed package has an
# R/ directory too, holding .rdb binaries rather than source. The guard
# has to look for actual .R files, or readLines() ends up parsing a
# database and every content check silently finds nothing.
.is_source_tree <- function() {
  length(list.files("../../R", pattern = "\\.[Rr]$")) > 0
}

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
  skip_if_not(.is_source_tree() && dir.exists("../../data-raw"),
              "data-raw/ not present")
  for (s in unique(api$script)) {
    expect_true(file.exists(file.path("../../data-raw", s)),
                label = sprintf("data-raw/%s named in SOURCES.csv", s))
  }
})

test_that("ssb_cited tables really are cited in the R source", {
  # Only `ssb_cited` claims the R source as its evidence. For `ssb_api`
  # the evidence is the fetching script, checked above -- and those table
  # numbers may live only in data-raw/, which .Rbuildignore strips from
  # the built package. Checking both against R/ conflated the two and
  # failed on 07459, which appears only in data-raw/fetch_pop.py.
  skip_if_not(.is_source_tree(), "not a source checkout")
  src <- paste(unlist(lapply(list.files("../../R", pattern = "\\.[Rr]$",
                                        full.names = TRUE),
                             readLines, warn = FALSE)), collapse = "\n")
  cited <- data_sources()
  cited <- cited[cited$status == "ssb_cited" &
                 nzchar(as.character(cited$ssb_table)), ]
  expect_gt(nrow(cited), 0L)
  for (i in seq_len(nrow(cited))) {
    tab <- as.character(cited$ssb_table[i])
    expect_true(grepl(tab, src, fixed = TRUE),
                label = sprintf("SSB %s (%s) cited in R/",
                                tab, cited$file[i]))
  }
})

test_that("ssb_api tables are traceable to their fetching script", {
  # data-raw/ is not in the built package, so this only runs from source.
  skip_if_not(.is_source_tree() && dir.exists("../../data-raw"),
              "data-raw/ not present")
  dr <- paste(unlist(lapply(
    Filter(function(f) !dir.exists(f),
           list.files("../../data-raw", full.names = TRUE)),
    readLines, warn = FALSE)), collapse = "\n")
  api <- data_sources()
  api <- api[api$status == "ssb_api" & nzchar(as.character(api$ssb_table)), ]
  expect_gt(nrow(api), 0L)
  for (i in seq_len(nrow(api))) {
    tab <- as.character(api$ssb_table[i])
    expect_true(grepl(tab, dr, fixed = TRUE),
                label = sprintf("SSB %s (%s) appears in data-raw/",
                                tab, api$file[i]))
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
